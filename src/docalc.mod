IMPLEMENTATION MODULE DoCalc;

        (************************************************************)
        (*                                                          *)
        (*                      PM Calculator                       *)
        (*          The module that does the calculations           *)
        (*                                                          *)
        (*    Started:        12 February 2002                      *)
        (*    Last edited:    12 August 2015                        *)
        (*    Status:         Working                               *)
        (*                                                          *)
        (************************************************************)


FROM Display IMPORT Message;       (* for debugging *)

IMPORT OS2, OS2RTL, DID, Display, Strings;

FROM SYSTEM IMPORT LOC;

FROM OurMath IMPORT
    (* const*)  PI,
    (* proc *)  ITRUNC, Round, URound, Negative, BoolNOT, atoi, sinh, cosh, tanh,
                power, sqrt, exp, ln, log10, sin, cos,
                tan, arcsin, arccos, arctan,
                cxNegative, cxBoolNOT, cxsqrt, cxexp, cxln, cxlog10,
                cxsin, cxcos, cxtan, cxarcsin, cxarccos, cxarctan,
                cxsinh, cxcosh, cxtanh, cxpower,
                reRound, cxRound, reTrunc, cxTrunc, rexor, cxxor,
                PolarToRect, RectToPolar, cxDummy;

FROM NumberToString IMPORT
    (* proc *)  LongRealToString, LongComplexToString;

FROM LowLevel IMPORT
    (* proc *)  IAND, IOR, EVAL;

FROM CircularBuffers IMPORT
    (* type *)  CircularBuffer,
    (* proc *)  CreateBuffer, GetBuffer, PutBuffer, InsertAtFront;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, CloseINIFile, INIGet, INIPut;

(************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST
    Nul = CHR(0);
    EmptyMarker = CHR(0);  EndMarker = ' ';
    Enter = CHR(0DH);  Esc = CHR(01BH);  Backspace = CHR(8);
    PostfixUnaryOperatorSet = CharSet {'%', ')', 's', 'S', ','};

        (* Remark: a comma is treated as a postfix unary operator in    *)
        (* the input, and then as a binary operator once it's on the    *)
        (* stack.                                                       *)

    UnknownOperatorPriority = 255;
    numberwidth = Display.DigitsPerNumber;
                        (* The field size for displaying numbers        *)
    DecimalDigits = CharSet {'0'..'9'};
    INIFileName = "KeyCalc.INI";

(************************************************************************)
(*                        THE CALCULATOR STATE                          *)
(************************************************************************)

CONST MaxRegisterNumber = 31;
      MaxVisibleMemoryNumber = Display.MaxVisibleMemoryNumber;
      MaxMemoryNumber = 9;
      DisplayedRegisters = Display.MaxVisibleRegisterNumber;

TYPE RegisterNumber = [0..MaxRegisterNumber];
     MemoryNumber = [0..MaxMemoryNumber];

VAR
    (* Numeric base. *)

    NumberBase: CARDINAL;

    (* The numeric digits that are legal in this base. *)

    Digits: CharSet;

    (* Mode: RPN or algebraic. *)

    RPNmode: BOOLEAN;

    (* Option to work with complex arithmetic. *)

    CxField: BOOLEAN;

    (* Option to have angles in degrees rather than in radians. *)

    AnglesInDegrees: BOOLEAN;

    (* Scale factors for degree/radian conversions. *)

    PreScale, PostScale: LONGREAL;

    (* Option to have each keystroke make a sound. *)

    BlipEnabled: BOOLEAN;

    (* Array "Register" is a combined operand and operator stack.  It   *)
    (* would be more conventional to have separate stacks for the       *)
    (* operators and operands, but we adopt this slightly unusual stack *)
    (* format because it makes it easier to maintain a user-friendly    *)
    (* screen display.                                                  *)

    Register: ARRAY RegisterNumber OF
                RECORD
                    operator: CHAR;
                    ParenCount: CARDINAL;
                    value: LONGCOMPLEX;
                    polar: BOOLEAN;
                END (*RECORD*);

    (* NumberPresent = FALSE means that Register[0].value is displayed  *)
    (* as a blank field rather than as a numeric string.                *)

    NumberPresent: BOOLEAN;

    (* In addition to the stack, there is a set of "memory" registers   *)
    (* in which the user can save calculation results.                  *)

    MemoryValue: ARRAY MemoryNumber OF
                     RECORD
                         val: LONGCOMPLEX;
                         polar: BOOLEAN;
                     END (*RECORD*);

(************************************************************************)
(*                    SOME SPECIAL-CASE FUNCTIONS                       *)
(************************************************************************)

PROCEDURE DisplayRegister (j: RegisterNumber);  FORWARD;
PROCEDURE DisplayMemory (j: MemoryNumber);  FORWARD;

(************************************************************************)

PROCEDURE Swap;

    (* Swaps x and y in RPM mode.  Swaps x and M0 in algebraic mode. *)

    VAR temp: LONGCOMPLEX;  flag: BOOLEAN;

    BEGIN
        temp := Register[0].value;  flag := Register[0].polar;
        IF RPNmode THEN
            Register[0].value := Register[1].value;
            Register[0].polar := Register[1].polar;
            Register[1].value := temp;
            Register[1].polar := flag;
            DisplayRegister(1);
        ELSE
            Register[0].value := MemoryValue[0].val;
            Register[0].polar := MemoryValue[0].polar;
            MemoryValue[0].val := temp;
            MemoryValue[0].polar := flag;
            DisplayMemory(0);
        END (*IF*);
    END Swap;

(************************************************************************)

PROCEDURE reSwap (dummy: LONGREAL): LONGREAL;

    BEGIN
        Swap;  RETURN RE(Register[0].value);
    END reSwap;

(************************************************************************)

PROCEDURE cxSwap (dummy: LONGCOMPLEX): LONGCOMPLEX;

    BEGIN
        Swap;  RETURN Register[0].value;
    END cxSwap;

(************************************************************************)
(*                      THE PREFIX UNARY FUNCTIONS                      *)
(************************************************************************)

(* To simplify the calculator logic, and to keep the display readable,  *)
(* all prefix unary functions are stored in the calculator stack in     *)
(* terms of the all-purpose binary function "v".  The first argument    *)
(* of v is the function number, and the second argument of v is the     *)
(* true argument of the original unary function.                        *)

CONST
    MaxFunctionNumber = 17;     (* number of built-in functions         *)
    functionnamewidth = 5;      (* # of characters in a function name   *)

TYPE
    FunctionType = [0..MaxFunctionNumber];
    NameText = ARRAY [0..functionnamewidth-1] OF CHAR;
    GlyphArray = ARRAY FunctionType OF CHAR;
    NameArray = ARRAY FunctionType OF NameText;

    (* A function procedure comes in two versions, one for real and     *)
    (* one for complex arithmetic.  In addition, the ConvertAngle       *)
    (* field says how to scale a number when angles are in degrees:     *)
    (*         0     do nothing                                         *)
    (*         1     convert to radians before calling function         *)
    (*         2     convert function result to degrees                 *)
    (*         3     convert both before and after function call.       *)

    MathProc = RECORD
                   re: PROCEDURE (LONGREAL): LONGREAL;
                   cx: PROCEDURE (LONGCOMPLEX): LONGCOMPLEX;
                   ConvertAngle: [0..3];
               END (*RECORD*);
    FunctionArray = ARRAY FunctionType OF MathProc;

CONST
    (* Array FunctionSymbol gives the display symbol for those prefix   *)
    (* functions that can be displayed using a single character.        *)
    (* Remark: postfix functions never need to be displayed, because    *)
    (* they are executed as soon as they are entered.                   *)

    FunctionSymbol = GlyphArray {'-', ' ', ' ', ' ', ' ',
                                 ' ', ' ', ' ', ' ', ' ',
                                 ' ', ' ', ' ', ' ', '~',
                                 ' ', ' ', ' ' };


    (* Array FunctionName gives the displayed function names for those  *)
    (* functions for which a one-character symbol does not suffice.     *)

    FunctionName = NameArray {'     ', 'sqrt ', 'exp  ', 'ln   ', 'log10',
                                'sin  ', 'cos  ', 'tan  ', 'asin ', 'acos ',
                                'atan ', 'sinh ', 'cosh ', 'tanh ', '     ',
                                'round', 'trunc', 'swap '};

    (* Array Function is the set of built-in functions. *)

    CONST Function = FunctionArray {{Negative, cxNegative, 0}, {sqrt, cxsqrt, 0},
                        {exp, cxexp, 0}, {ln, cxln, 0}, {log10, cxlog10, 0},
                        {sin, cxsin, 1}, {cos, cxcos, 1}, {tan, cxtan, 1},
                        {arcsin, cxarcsin, 2}, {arccos, cxarccos, 2},
                        {arctan, cxarctan, 2}, {sinh, cxsinh, 1}, {cosh, cxcosh, 1},
                        {tanh, cxtanh, 1}, {BoolNOT, cxBoolNOT, 0},
                        {reRound, cxRound, 0}, {reTrunc, cxTrunc, 0},
                        {reSwap, cxSwap, 0} };

(************************************************************************)
(*                     THE PREFIX BINARY FUNCTIONS                      *)
(************************************************************************)

(* To simplify the calculator logic, and to keep the display readable,  *)
(* all prefix binary functions are stored in the calculator stack in    *)
(* terms of the all-purpose binary function "w".  The first argument    *)
(* of w is the function number, and the second and third arguments of w *)
(* are the true argument of the original binary function.  More         *)
(* precisely, the stack in this case holds the sequence N 'w' y ',' x   *)
(* where N is the function number, x and y are the two arguments, and   *)
(* a special 'comma operator' is used to connect the arguments.         *)

CONST
    MaxFunction2Number = 2;     (* number (-1) of built-in functions    *)

TYPE
    Function2Type = [0..MaxFunction2Number];
    Glyph2Array = ARRAY Function2Type OF CHAR;
    Name2Array = ARRAY Function2Type OF NameText;
    Math2Proc = RECORD
                   re: PROCEDURE (VAR LONGREAL, VAR LONGREAL);
                   cx: PROCEDURE (VAR LONGCOMPLEX, VAR LONGCOMPLEX);
               END (*RECORD*);
    Function2Array = ARRAY Function2Type OF Math2Proc;

CONST
    (* Array Function2Symbol gives the display symbol for those prefix  *)
    (* functions that can be displayed using a single character.        *)
    (* Remark: postfix functions never need to be displayed, because    *)
    (* they are executed as soon as they are entered.                   *)

    Function2Symbol = Glyph2Array {' ', ' ', ' '};

    (* Array Function2Name gives the displayed function names for those *)
    (* functions for which a one-character symbol does not suffice.     *)

    Function2Name = Name2Array {'xor  ', 'polar', 'rect '};

    (* Array Function2 is the set of built-in binary functions.  These  *)
    (* are, inevitably, full of special cases, so the array doesn't     *)
    (* tell the whole story.                                            *)

    CONST Function2 = Function2Array { {rexor, cxxor},
                         {RectToPolar, cxDummy}, {PolarToRect, cxDummy} };

(************************************************************************)

VAR
    (* Keyboard input is placed into this buffer by a window procedure. *)

    InBuffer: CircularBuffer;

    (* Window handle. *)

    MainHwnd: OS2.HWND;

(************************************************************************)
(*                          AUDIBLE SIGNALS                             *)
(************************************************************************)

PROCEDURE Beep;

    CONST
        BEEP_WARN_FREQ =  60; (* frequency of warning beep *)
        BEEP_WARN_DUR  = 100; (* duration of warning beep *)

    BEGIN
        OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
    END Beep;

(************************************************************************)

PROCEDURE Blip;

    CONST
        BEEP_FREQ = 5000; (* frequency of short beep *)
        BEEP_DUR  =    2; (* duration of short beep *)

    BEGIN
        OS2.DosBeep(BEEP_FREQ, BEEP_DUR);
    END Blip;

(************************************************************************)
(*                     INTERTHREAD COMMUNICATION                        *)
(************************************************************************)

PROCEDURE NewChar (ch: CHAR);

    (* Sends ch as input to the calculator. *)

    BEGIN
        PutBuffer (InBuffer, ch);
        IF BlipEnabled THEN
            Blip;
        END (*IF*);
    END NewChar;

(************************************************************************)
(*                         DISPLAY OPERATIONS                           *)
(************************************************************************)

PROCEDURE DisplayRegister (j: RegisterNumber);

    (* Refreshes the display of the left parentheses, the value, and    *)
    (* the trailing operator for register j.                            *)

    VAR Buffer: ARRAY [0..2*numberwidth+3] OF CHAR;

    (********************************************************************)

    PROCEDURE AddParentheses (count: CARDINAL);

        (* Starting at Buffer[1], appends 'count' parentheses, or an    *)
        (* abbreviation if count is too large.                          *)

        VAR k: CARDINAL;

        BEGIN
            k := 1;
            IF count > 4 THEN
                Buffer[1] := '(';
                Buffer[2] := '.';
                Buffer[3] := '(';
                k := 4;
            ELSE
                WHILE count > 0 DO
                    Buffer[k] := '(';  INC(k);  DEC(count);
                END (*WHILE*);
            END (*IF*);
            Buffer[k] := Nul;
        END AddParentheses;

    (********************************************************************)

    VAR operator: CHAR;  f: FunctionType;  f2: Function2Type;

    BEGIN
        IF Register[j].operator = EmptyMarker THEN
            Display.WriteOperator (j, '');
            Display.WriteRegister (j, '');
        ELSE
            IF Register[j].operator = FunctionSym THEN
                operator := FunctionSymbol[Round(RE(Register[j+1].value))];
            ELSIF Register[j].operator = Function2Sym THEN
                operator := Function2Symbol[Round(RE(Register[j+1].value))];
            ELSIF Register[j].operator = Enter THEN
                operator := ' ';
            ELSE
                operator := Register[j].operator;
            END (*IF*);
            Buffer[0] := operator;
            AddParentheses (Register[j].ParenCount);
            Display.WriteOperator (j, Buffer);

            IF (j > 0) AND (Register[j-1].operator = FunctionSym) THEN
                f := VAL(FunctionType,Round(RE(Register[j].value)));
                Strings.Assign (FunctionName[f], Buffer);
            ELSIF (j > 0) AND (Register[j-1].operator = Function2Sym) THEN
                f2 := VAL(Function2Type,Round(RE(Register[j].value)));
                Strings.Assign (Function2Name[f2], Buffer);
            ELSIF (j = 0) AND NOT NumberPresent THEN
                Buffer[0] := Nul;
            ELSIF CxField THEN
                LongComplexToString (Register[j].value, Register[j].polar,
                                     AnglesInDegrees, NumberBase,
                                     Buffer, numberwidth);
            ELSE
                LongRealToString (RE(Register[j].value), NumberBase,
                                  Buffer, numberwidth);
            END (*IF*);
            Display.WriteRegister (j, Buffer);
        END (*IF*);

    END DisplayRegister;

(************************************************************************)

PROCEDURE DisplayStack;

    (* Refreshes the display of the stack of registers. *)

    VAR j: RegisterNumber;

    BEGIN
        FOR j := 0 TO DisplayedRegisters DO
            DisplayRegister (j);
        END (*FOR*);

        (* If stack is empty, display help message *)

        IF Register[1].operator = EmptyMarker THEN
            Display.WriteRegister (2, "F1: help");
            Display.WriteRegister (3, "Ctrl: menu");
        END (*IF*);

    END DisplayStack;

(************************************************************************)

PROCEDURE DisplayMemory (j: MemoryNumber);

    (* Refreshes the display of "memory" register j.    *)

    VAR Buffer: ARRAY [0..2*numberwidth+3] OF CHAR;

    BEGIN
        IF j <= MaxVisibleMemoryNumber THEN
            IF CxField THEN
                LongComplexToString (MemoryValue[j].val, MemoryValue[j].polar,
                                     AnglesInDegrees, NumberBase, Buffer,
                                     numberwidth);
                Buffer[2*numberwidth+3] := Nul;
            ELSE
                LongRealToString (RE(MemoryValue[j].val), NumberBase, Buffer, numberwidth);
                Buffer[numberwidth] := Nul;
            END (*IF*);
            Display.WriteMemory (j, Buffer);
        END (*IF*);
    END DisplayMemory;

(************************************************************************)

PROCEDURE InitialDisplay;

    (* Assumption: the calculator window is already open.  This         *)
    (* procedure puts the initial picture of the calculator onto the    *)
    (* screen.                                                          *)

    VAR mem: MemoryNumber;

    BEGIN
        (* Display the register contents.       *)

        DisplayStack;

        (* Display the visible memory values.   *)

        FOR mem := 0 TO MaxVisibleMemoryNumber DO
            DisplayMemory (mem);
        END (*FOR*);

    END InitialDisplay;

(************************************************************************)
(*                          OPTION SETTINGS                             *)
(************************************************************************)

PROCEDURE SetBase (base: CARDINAL);

    (* Sets the base for numeric entry and display. *)

    VAR j: CARDINAL;

    BEGIN
        NumberBase := base;
        IF NumberBase > 10 THEN
            Digits := DecimalDigits;
            FOR j := 0 TO NumberBase-11 DO
                INCL (Digits, CHR(ORD('A') + j));
                INCL (Digits, CHR(ORD('a') + j));
            END (*FOR*);
        ELSE
            Digits := CharSet{};
            FOR j := 0 TO NumberBase-1 DO
                INCL (Digits, CHR(ORD('0') + j));
            END (*FOR*);
        END (*IF*);

        Display.SetBase (base);
        InitialDisplay;

    END SetBase;

(************************************************************************)

PROCEDURE SetMode (RPN: BOOLEAN);

    (* Sets mode to RPN or algebraic. *)

    VAR j: RegisterNumber;

    BEGIN
        IF RPNmode <> RPN THEN
            IF RPN THEN
                j := 0;
                WHILE (j < MAX(RegisterNumber))
                            AND (Register[j].operator <> EndMarker) DO
                    Register[j].operator := Enter;
                    Register[j].ParenCount := 0;
                    INC(j);
                END (*WHILE*);
                Register[j].operator := EndMarker;
                Register[j].ParenCount := 0;
            END (*IF*);
            DisplayStack;
        END (*IF*);
        RPNmode := RPN;
        Display.SetRPN (RPNmode);
    END SetMode;

(************************************************************************)

PROCEDURE SetComplex (Cx: BOOLEAN);

    (* Sets complex calculations iff Cx = TRUE. *)

    BEGIN
        CxField := Cx;
        Display.SetComplex (Cx);
        InitialDisplay;
    END SetComplex;

(************************************************************************)

PROCEDURE SetDegrees (deg: BOOLEAN);

    (* Angles are measured in degrees iff deg = TRUE. *)

    BEGIN
        AnglesInDegrees := deg;
        Display.SetDegrees (deg);
        InitialDisplay;
    END SetDegrees;

(************************************************************************)
(*                          NUMERIC INPUT                               *)
(************************************************************************)

PROCEDURE AcceptNumber (nextchar: CHAR);

    (* Reads an unsigned number from the keyboard.  On entry, nextchar  *)
    (* holds the first digit or the decimal point.  On exit, the input  *)
    (* value is in Register[0].value.                                   *)

    TYPE BufferSubscript = [0..511];

    VAR mantissa, oldmantissa, placevalue, oldplacevalue, RBase: LONGREAL;
        BufferPos: BufferSubscript;
        Buffer: ARRAY BufferSubscript OF CHAR;
        BufferFull: BOOLEAN;
        exponent, oldexponent: CARDINAL;
        Negative, OldNegative, NegativeExponent, OldNegativeExponent,
                                                      AfterComma: BOOLEAN;
        IsPolar: BOOLEAN;

    (********************************************************************)

    PROCEDURE SetFinalResult;

        (* Combines mantissa and exponent, stores answer in accumulator. *)

        VAR result, result2: LONGREAL;

        BEGIN
            result := mantissa;
            IF exponent <> 0 THEN
                IF NegativeExponent THEN
                    result := result / atoi(RBase, exponent);
                ELSE
                    result := result * atoi(RBase, exponent);
                END (*IF*);
            END (*IF*);
            IF Negative THEN
                result := -result;
            END (*IF*);
            IF AfterComma THEN
                result2 := result;
                result := RE(Register[0].value);
                IF IsPolar THEN
                    IF AnglesInDegrees THEN
                        result2 := PreScale*result2;
                    END (*IF*);
                    PolarToRect (result, result2);
                END (*IF*);
                Register[0].value := CMPLX(result, result2);
            ELSE
                Register[0].value := CMPLX(result,0.0);
            END (*IF*);
            Register[0].polar := IsPolar;
        END SetFinalResult;

    (********************************************************************)

    PROCEDURE GetNextChar;

        (* Displays the input so far (as a text string if it will fit,  *)
        (* otherwise by a call to WriteReal), and then reads nextchar.  *)

        BEGIN
            IF BufferPos > numberwidth THEN
                SetFinalResult;
                DisplayRegister(0);
            ELSE
                Display.WriteRegister (0, Buffer);
            END (*IF*);
            nextchar := GetBuffer(InBuffer);
            IF NOT BufferFull THEN
                IF BufferPos = MAX(BufferSubscript) THEN
                    BufferFull := TRUE;
                ELSE
                    Buffer[BufferPos] := nextchar;
                    INC (BufferPos);
                    Buffer[BufferPos] := Nul;
                END (*IF*);
            END (*IF*);
        END GetNextChar;

    (********************************************************************)

    PROCEDURE NumVal (ch: CHAR): LONGREAL;

        (* Character to real digit value. *)

        VAR result: CARDINAL;

        BEGIN
            IF ch IN DecimalDigits THEN
                result := ORD(ch) - ORD('0');
            ELSE
                result := ORD(CAP(ch)) - ORD('A') + 10;
            END (*IF*);
            RETURN VAL(LONGREAL, result);
        END NumVal;

    (********************************************************************)

    VAR AfterThePoint, AfterTheE, AfterTheEsign,
        OldAfterThePoint, OldAfterTheE, OldAfterTheEsign: BOOLEAN;

    BEGIN
        mantissa := 0.0;
        exponent := 0;
        placevalue := 1.0;
        AfterThePoint := FALSE;
        AfterTheE := FALSE;
        AfterTheEsign := FALSE;
        Negative := FALSE;
        NegativeExponent := FALSE;
        IsPolar := FALSE;

        oldmantissa := 0.0;
        oldexponent := 0;
        oldplacevalue := 1.0;
        OldAfterThePoint := FALSE;
        OldAfterTheE := FALSE;
        OldAfterTheEsign := FALSE;
        OldNegative := FALSE;
        OldNegativeExponent := FALSE;

        BufferFull := FALSE;
        Buffer[0] := nextchar;
        Buffer[1] := Nul;
        BufferPos := 1;
        NumberPresent := TRUE;
        AfterComma := FALSE;
        RBase := VAL(LONGREAL, NumberBase);

        LOOP
            (* Backspace *)

            IF nextchar = Backspace THEN
                IF BufferFull OR (BufferPos < 2) THEN
                    DEC(BufferPos);  EXIT(*LOOP*);
                ELSE
                    DEC (BufferPos, 2);
                    nextchar := Buffer[BufferPos];
                    Buffer[BufferPos] := Nul;
                    IF nextchar = ' ' THEN
                        (* deleting space has no other effect *)
                    ELSIF (nextchar = ',') OR (nextchar = '<') THEN
                        mantissa := oldmantissa;
                        exponent := oldexponent;
                        placevalue := oldplacevalue;
                        Negative := OldNegative;
                        AfterThePoint := OldAfterThePoint;
                        AfterTheE := OldAfterTheE;
                        AfterTheEsign := OldAfterTheEsign;
                        NegativeExponent := OldNegativeExponent;
                        AfterComma := FALSE;
                        IsPolar := FALSE;
                    ELSIF nextchar = '.' THEN
                        AfterThePoint := FALSE;
                    ELSIF (nextchar = '+') OR (nextchar = '-') THEN
                        IF AfterTheEsign THEN
                            NegativeExponent := FALSE;
                            AfterTheEsign := FALSE;
                        ELSE
                            Negative := FALSE;
                        END (*IF*);
                    ELSIF (CAP(nextchar) = 'E') OR (CAP(nextchar) = 'G') THEN
                        AfterTheE := FALSE;
                    ELSIF AfterTheE THEN
                        exponent := exponent DIV NumberBase;
                    ELSIF AfterThePoint THEN
                        placevalue := placevalue*RBase;
                        mantissa := mantissa - placevalue*NumVal(nextchar);
                    ELSE
                        mantissa := (mantissa - NumVal(nextchar)) / RBase;
                    END (*IF*);
                    GetNextChar;
                END (*IF*);

            (* Decimal point *)

            ELSIF nextchar = '.' THEN
                IF AfterThePoint THEN
                    DEC(BufferPos);  EXIT(*LOOP*);
                ELSE
                    GetNextChar;  placevalue := 1.0/RBase;
                    AfterThePoint := TRUE;
                END (*IF*);

            (* Comma *)

            ELSIF ((nextchar = ',') OR (nextchar = '<')) AND NOT AfterComma THEN
                IF CxField THEN
                    IsPolar := nextchar = '<';
                    SetFinalResult;
                    AfterComma := TRUE;
                    oldmantissa := mantissa;
                    mantissa := 0.0;
                    oldexponent := exponent;
                    exponent := 0;
                    oldplacevalue := placevalue;
                    placevalue := 1.0;
                    OldNegative := Negative;
                    Negative := FALSE;
                    OldAfterThePoint := AfterThePoint;
                    AfterThePoint := FALSE;
                    OldAfterTheE := AfterTheE;
                    AfterTheE := FALSE;
                    OldAfterTheEsign := AfterTheEsign;
                    AfterTheEsign := FALSE;
                    OldNegativeExponent := NegativeExponent;
                    NegativeExponent := FALSE;
                    GetNextChar;
                ELSE
                    DEC(BufferPos);  EXIT(*LOOP*);
                END (*IF*);

            (* Digit *)

            ELSIF CAP(nextchar) IN Digits THEN
                IF AfterTheE THEN
                    exponent := NumberBase*exponent + (ORD(nextchar) - ORD('0'));
                ELSIF AfterThePoint THEN
                    mantissa := mantissa + placevalue*NumVal(nextchar);
                    placevalue := placevalue/RBase;
                ELSE
                    mantissa := RBase*mantissa + NumVal(nextchar);
                END (*IF*);
                GetNextChar;

            (* Exponent marker, 'E' or 'G'.  It is important to check   *)
            (* this after the digits, because in some bases 'E' can be  *)
            (* a digit.                                                 *)

            ELSIF (CAP(nextchar) = 'E') OR (CAP(nextchar) = 'G') THEN
                IF AfterTheE THEN
                    DEC(BufferPos);  EXIT(*LOOP*);
                ELSE
                    AfterTheE := TRUE;
                    GetNextChar;
                END (*IF*);

            (* Sign, legal only immediately after the 'E' or right at   *)
            (* the beginning of a number.                               *)

            ELSIF (nextchar = '+') OR (nextchar = '-') THEN
                IF AfterTheE AND NOT AfterTheEsign THEN
                    NegativeExponent := nextchar = '-';
                    AfterTheEsign := TRUE;
                    GetNextChar;
                ELSIF (BufferPos = 1) OR (Buffer[BufferPos-2] = ',')
                                      OR (Buffer[BufferPos-2] = '<') THEN
                    Negative := nextchar = '-';
                    GetNextChar;
                ELSE
                    DEC(BufferPos);  EXIT(*LOOP*);
                END (*IF*);

            (* Anything else is not to be processed here. *)

            ELSE
                IF BufferPos > 0 THEN
                    DEC(BufferPos);
                END(*IF*);
                EXIT (*LOOP*);
            END (*IF*);

        END (*LOOP*);

        InsertAtFront (InBuffer, nextchar);
        SetFinalResult;
        NumberPresent := BufferPos>0;

    END AcceptNumber;

(************************************************************************)

PROCEDURE priority (operator: CHAR): CARDINAL;

    (* Returns the priority of an operator.     *)

    BEGIN
        CASE operator OF
                EndMarker:      RETURN 0;
            |
                Enter,"=":      RETURN 1;
            |
                ',':            RETURN 2;
            |
                "+","-",'|':    RETURN 3;
            |
                "*","/",'&':    RETURN 4;
            |
                FunctionSym:    IF Register[1].value = 0.0 THEN RETURN 7
                                ELSE RETURN 5;
                                END (*IF*);
            |
                Function2Sym:   RETURN 5;
            |
                "^":            RETURN 6;
            |
                ELSE
                                RETURN UnknownOperatorPriority;
        END (*CASE*);
    END priority;

(************************************************************************)

PROCEDURE TopOperatorPriority(): CARDINAL;

    (* TopOperatorPriority is normally the priority of the operator in  *)
    (* Register[0].  However any left parenthesis in Register[0]        *)
    (* overrides this; in that case we return an answer of 0.           *)

    BEGIN
        IF Register[0].ParenCount > 0 THEN RETURN 0
        ELSE RETURN priority (Register[0].operator)
        END (*IF*);
    END TopOperatorPriority;

(************************************************************************)
(*                          STACK MANIPULATION                          *)
(************************************************************************)

PROCEDURE PushStack (LatestOperator: CHAR);

    (* Pushes the register stack, clearing the top one.  The argument   *)
    (* ends up as the operator in Register[0].  If the stack overflows  *)
    (* we give an audible alarm, but perform the push anyway.           *)

    VAR j: RegisterNumber;

    BEGIN
        IF Register[MaxRegisterNumber].operator <> EmptyMarker THEN
            Register[MaxRegisterNumber-1].operator := EndMarker;
            Beep;
        END (*IF*);
        FOR j := MaxRegisterNumber TO 1 BY -1 DO
            Register[j] := Register[j-1];
        END (*FOR*);
        WITH Register[0] DO
            operator := LatestOperator;  value := CMPLX(0.0,0.0);
            ParenCount := 0;
        END (*WITH*);
        NumberPresent := FALSE;
        DisplayStack;
    END PushStack;

(************************************************************************)

PROCEDURE PopStack;

    (* Pops the register stack, clearing the bottom register.   *)

    VAR j: RegisterNumber;  wasatend: BOOLEAN;

    BEGIN
        wasatend := Register[0].operator = EndMarker;
        FOR j := 0 TO MaxRegisterNumber-1 DO
            Register[j] := Register[j+1];
        END (*FOR*);
        WITH Register[MaxRegisterNumber] DO
            ParenCount := 0;  value := CMPLX(0.0,0.0);
            operator := EmptyMarker;
        END (*WITH*);
        IF wasatend THEN
            Register[0].operator := EndMarker;
        END (*IF*);
        NumberPresent := NOT wasatend;
        DisplayStack;
    END PopStack;

(************************************************************************)
(*                      OPERATIONS ON THE MEMORIES                      *)
(************************************************************************)

PROCEDURE GetMemoryNumber (): MemoryNumber;

    (* Returns the value of a one-digit memory number typed from the    *)
    (* keyboard.  Also wipes the "memory number" prompt on the display. *)
    (* Assumes memory number 0 (and does not consume the typed key) if  *)
    (* no valid memory number is specified.                             *)

    VAR ch: CHAR;

    BEGIN
        ch := GetBuffer(InBuffer);
        IF ch IN CharSet{"0"..CHR(ORD("0")+MaxMemoryNumber)} THEN
            RETURN ORD(ch) - ORD("0");
        ELSE
            InsertAtFront (InBuffer, ch);  RETURN 0;
        END (*IF*);
    END GetMemoryNumber;

(************************************************************************)

PROCEDURE StoreToMemory;

    (* Gets a memory number from the keyboard, stores the accumulator   *)
    (* value in that memory register.                                   *)

    VAR mem: MemoryNumber;

    BEGIN
        mem := GetMemoryNumber();
        MemoryValue[mem].val := Register[0].value;
        MemoryValue[mem].polar := Register[0].polar;
        DisplayMemory(mem);
    END StoreToMemory;

(************************************************************************)
(*                              OPERATIONS                              *)
(************************************************************************)

PROCEDURE Divide0 (first, second: LONGREAL): LONGREAL;

    (* Computes first/second, except that division by zero gives 0.0.   *)

    BEGIN
        IF second = 0.0 THEN
            Beep;  RETURN 0.0
        ELSE
            RETURN first/second;
        END (*IF*);
    END Divide0;

(************************************************************************)

PROCEDURE cxDivide0 (first, second: LONGCOMPLEX): LONGCOMPLEX;

    (* Computes first/second, except that division by zero gives 0.0.   *)

    BEGIN
        IF second = CMPLX(0.0,0.0) THEN
            Beep;  RETURN CMPLX(0.0, 0.0);
        ELSE
            RETURN first/second;
        END (*IF*);
    END cxDivide0;

(************************************************************************)

PROCEDURE BinaryOperation;

    (* Performs the binary operation requested by Register[0].operator. *)

    VAR x, y, result: LONGCOMPLEX;
        xR, yR, resultR: LONGREAL;
        command: CHAR;  f: FunctionType;  f2: Function2Type;

    BEGIN
        (* Special case for RPN mode: if one of the operands is 'empty' *)
        (* but the other is present, go ahead anyway.                   *)

        IF Register[1].operator = EmptyMarker THEN
            IF NumberPresent THEN
                Register[1].operator := Enter;
            END (*IF*);
        ELSE
            NumberPresent := TRUE;
        END (*IF*);

        (* Do the operation, leave the result in Register[1], then      *)
        (* pop the stack.                                               *)

        command := Register[0].operator;
        y := Register[1].value;  x := Register[0].value;

        (* For some operators, we can do the job in complex arithmetic  *)
        (* without caring whether we're in real or complex mode.        *)

        IF command = "+" THEN result := y + x
        ELSIF command = "-" THEN result := y - x
        ELSIF command = '&' THEN
            result := CMPLX (VAL(LONGREAL,IAND(URound(RE(x)), URound(RE(y)))),
                             VAL(LONGREAL,IAND(URound(IM(x)), URound(IM(y)))));
        ELSIF command = '|' THEN
            result := CMPLX (VAL(LONGREAL,IOR(URound(RE(x)), URound(RE(y)))),
                             VAL(LONGREAL,IOR(URound(IM(x)), URound(IM(y)))));

        (* For others, we must make the distinction.                    *)

        ELSIF CxField THEN
            IF (command = "*") OR (command = "x") THEN result := y * x
            ELSIF command = "/" THEN
                result := cxDivide0 (y, x);
            ELSIF command = "^" THEN
                result := cxpower (y, x);

            (* NOTE: the handling of functions below is only    *)
            (* for algebraic mode.  RPN functions are dealt     *)
            (* with before this procedure is called.            *)

            ELSIF command = FunctionSym THEN
                f := VAL(FunctionType, Round(RE(y)));
                IF AnglesInDegrees AND ODD(Function[f].ConvertAngle) THEN
                    x := CMPLX(PreScale,0.0)*x;
                END (*IF*);
                result := Function[f].cx (x);
                IF AnglesInDegrees AND (Function[f].ConvertAngle > 1) THEN
                    result := CMPLX(PostScale,0.0)*result;
                END (*IF*);

            ELSIF command = Function2Sym THEN

                f2 := VAL(Function2Type, Round(RE(y)));
                IF (f2 = 1) OR (f2 = 2) THEN
                    Register[1].polar := f2 = 1;
                    result := x;
                ELSE
                    (* Binary function with missing second operand. *)

                    y := CMPLX(0.0,0.0);
                    Function2[f2].cx (x, y);
                    result := x;
                END (*IF*);

            ELSIF (command = ',') AND (Register[1].ParenCount = 0)
                         AND (Register[1].operator = Function2Sym) THEN

                (* Binary function with both operands present. *)

                f2 := VAL(Function2Type, Round(RE(Register[2].value)));
                Function2[f2].cx (y, x);
                result := y;

            ELSE
                result := x;
                Beep;
            END (*IF*);
        ELSE
            xR := RE(x);  yR := RE(y);
            IF command = "*" THEN
                resultR := yR * xR
            ELSIF command = "/" THEN
                resultR := Divide0 (yR, xR);
            ELSIF command = "^" THEN
                resultR := power (yR, xR);

            (* NOTE: the handling of functions below is only    *)
            (* for algebraic mode.  RPN functions are dealt     *)
            (* with before this procedure is called.            *)

            ELSIF command = FunctionSym THEN
                f := VAL(FunctionType, Round(RE(y)));
                IF AnglesInDegrees AND ODD(Function[f].ConvertAngle) THEN
                    xR := PreScale*xR;
                END (*IF*);
                resultR := Function[f].re (xR);
                IF AnglesInDegrees AND (Function[f].ConvertAngle > 1) THEN
                    resultR := PostScale*resultR;
                END (*IF*);

            ELSIF command = Function2Sym THEN

                (* Binary function with missing second operand. *)

                f2 := VAL(Function2Type, Round(RE(y)));
                yR := 0.0;
                Function2[f2].re (xR, yR);
                resultR := xR;

            ELSIF (command = ',') AND (Register[1].ParenCount = 0)
                         AND (Register[1].operator = Function2Sym) THEN

                (* Binary function with both operands present,  *)
                (* non-RPN case.                                *)

                f2 := VAL(Function2Type, Round(RE(Register[2].value)));
                IF AnglesInDegrees AND (f2 = 2) THEN
                    xR := PreScale*xR;
                END (*IF*);
                Function2[f2].re (yR, xR);
                IF AnglesInDegrees AND (f2 = 1) THEN
                    xR := PostScale*xR;
                END (*IF*);
                resultR := yR;
                IF (f2 >= 1) AND (f2 <= 2) THEN
                    MemoryValue[0].val := CMPLX(xR, 0.0);
                    MemoryValue[0].polar := FALSE;
                    DisplayMemory(0);
                END (*IF*);

            ELSE
                Beep;
                resultR := xR;
            END (*IF*);
            result := CMPLX (resultR, 0.0);
        END (*IF*);
        Register[1].value := result;
        PopStack;
    EXCEPT
        Beep;  Beep;  Beep;
        Register[1].value := CMPLX(0.0,0.0);
        PopStack;
        RETURN;
    END BinaryOperation;

(************************************************************************)

PROCEDURE PrefixUnaryOperation (code: CHAR);

    (* Performs the unary operation requested by code.  *)

    BEGIN
        IF code = "~" THEN
            Register[0].value := CMPLX(BoolNOT (RE(Register[0].value)),
                                       BoolNOT (IM(Register[0].value)));
            NumberPresent := TRUE;
        ELSE
            Beep;
        END (*IF*);
        DisplayRegister(0);
    END PrefixUnaryOperation;

(************************************************************************)

PROCEDURE PostfixUnaryOperation (code: CHAR);

    (* Performs the unary operation requested by code.  *)

    BEGIN
        IF code = "%" THEN
            IF CxField THEN
                Register[0].value := 0.01*Register[0].value*Register[1].value;
            ELSE
                Register[0].value := CMPLX (0.01*RE(Register[0].value)
                                                *RE(Register[1].value), 0.0);
            END (*IF*);
        ELSIF code = ")" THEN
            IF Register[0].ParenCount > 0 THEN
                DEC (Register[0].ParenCount);
            ELSIF Register[0].operator <> EndMarker THEN
                BinaryOperation;
                InsertAtFront (InBuffer, ")");
            ELSE
                Beep;
            END (*IF*);
        ELSIF (code="s") OR (code="S") THEN
            StoreToMemory;
        ELSIF code = "," THEN

            (* Comma is a special case, because it's not really a       *)
            (* unary operator.  What we need to do is reduce the stack  *)
            (* down to where the visible operator is a Function2Sym, at *)
            (* which point we can push the comma.                       *)

            IF Register[0].ParenCount > 0 THEN
                Beep;
            ELSIF Register[0].operator = Function2Sym THEN
                PushStack (',');
            ELSIF Register[0].operator <> EndMarker THEN
                BinaryOperation;
                InsertAtFront (InBuffer, ",");
            ELSE
                Beep;
            END (*IF*);

        ELSE
            Beep;
        END (*IF*);
        DisplayRegister(0);

    END PostfixUnaryOperation;

(************************************************************************)
(*              GETTING A FUNCTION NAME BY MENU SELECTION               *)
(************************************************************************)

PROCEDURE ReadBuiltinFunctionName;

    (* Allows the user to select a function name from a menu.  We then  *)
    (* load the stack with the function number, and the special         *)
    (* "binary operator" v.                                             *)

    VAR function: FunctionType;

    BEGIN
        function := VAL(FunctionType, GetBuffer(InBuffer));
        Register[0].value := CMPLX(VAL(LONGREAL, function), 0.0);
        PushStack (FunctionSym);
    END ReadBuiltinFunctionName;

(************************************************************************)

PROCEDURE ReadBuiltinFunction2Name;

    (* This is for one of the two-argument functions.  We load the      *)
    (* stack with the function number, and the special "ternary         *)
    (* operator" Function2Sym.                                          *)

    VAR function: Function2Type;

    BEGIN
        function := VAL(Function2Type, GetBuffer(InBuffer));
        Register[0].value := CMPLX(VAL(LONGREAL, function), 0.0);
        PushStack (Function2Sym);
    END ReadBuiltinFunction2Name;

(************************************************************************)
(*                        THE CALCULATOR TASK                           *)
(************************************************************************)

PROCEDURE LoadAccumulator (VAR (*INOUT*) nextchar: CHAR);

    (* Loads the accumulator with a number, also accepting and keeping  *)
    (* track of any opening parentheses.  Unary operations are also     *)
    (* dealt with by this procedure; and this could lead to the         *)
    (* evaluation of entire subexpressions, because we treat a closing  *)
    (* parenthesis as a unary postfix operator.  On return, nextchar    *)
    (* holds the following keyboard character (usually an operator, but *)
    (* it could also be Esc, Return, or an illegal keystroke).  Most of *)
    (* the complexity of this procedure lies in the fact that the user  *)
    (* can also type Backspace at any time, which has the effect of     *)
    (* cancelling the latest number, left parenthesis, or unevaluated   *)
    (* operator, as appropriate.                                        *)

    (* It is possible that the user will enter no value before the      *)
    (* operator.  In this case, the previous accumulator contents are   *)
    (* retained, unless they have been wiped out by a backspace.        *)
    (* Conversely, the user can override a number which is already      *)
    (* present.  We try to give a legal meaning, wherever possible, to  *)
    (* any user input.                                                  *)

    CONST Starters = CharSet {"(", ".", "v", 'w', "m", "M",
                                "p", "P", '~'};
          Misc = CharSet {EndMarker, Backspace};
          HandledHere = Starters + Misc + PostfixUnaryOperatorSet;

    VAR N: CARDINAL;

    BEGIN
        LOOP

            (* If the input is such as to imply that a new number is    *)
            (* to be entered, discard any number already in the         *)
            (* accumulator - i.e. allow the user to override any        *)
            (* previous input.                                          *)

            IF nextchar IN Starters+Digits THEN
                NumberPresent := FALSE;
                DisplayRegister(0);
            END (*IF*);

            (* Exit if we see a character that's not handled here.      *)
            (* (The 'IF' below could be made more compact, but then it  *)
            (* would be hard to read.)  The only tricky case is where   *)
            (* a number starts with a sign.                             *)
            (* On seeing a '+' or '-', we handle it here iff it is a    *)
            (* unary operator, i.e. if the accumulator is empty.        *)

            IF (nextchar IN HandledHere+Digits) THEN
                (* don't exit *)
            ELSIF NOT NumberPresent
                      AND ((nextchar = "+") OR (nextchar = "-")) THEN
                (* don't exit *)
            ELSE
                EXIT (*LOOP*);
            END (*IF*);

            (* Any character which, by coincidence, has the     *)
            (* same character code as EndMarker is ignored.     *)

            IF nextchar = EndMarker THEN (* do nothing *)

            (* Read prefix unary operator.  We don't evaluate it here;  *)
            (* it's put on the stack to look like a binary operator.    *)

            ELSIF CAP(nextchar) = CAP(FunctionSym) THEN ReadBuiltinFunctionName;

            ELSIF nextchar = "~" THEN
                Register[0].value := CMPLX(14.0, 0.0);
                PushStack (FunctionSym);

            (* Similarly for prefix binary operator.    *)

            ELSIF CAP(nextchar) = CAP(Function2Sym) THEN ReadBuiltinFunction2Name;

            (* Handle postfix unary operator.   *)

            ELSIF nextchar IN PostfixUnaryOperatorSet THEN
                PostfixUnaryOperation (nextchar);
                NumberPresent := TRUE;

            (* Handle opening parenthesis.      *)

            ELSIF nextchar = "(" THEN
                INC (Register[0].ParenCount);
                DisplayRegister(0);

            (* P means the constant PI. *)

            ELSIF CAP(nextchar) = "P" THEN
                Register[0].value := CMPLX(PI, 0.0);
                NumberPresent := TRUE;
                DisplayRegister(0);

            (* Fetch a number.  *)

            ELSIF CAP(nextchar) IN (Digits + CharSet{'.', '+', '-'}) THEN
                AcceptNumber (nextchar);

            (* Or an operand from memory.       *)

            ELSIF CAP(nextchar) ="M" THEN
                N := GetMemoryNumber();
                Register[0].value := MemoryValue[N].val;
                Register[0].polar := MemoryValue[N].polar;
                NumberPresent := TRUE;
                DisplayRegister(0);

            (* Now the hard part: handle Backspace.     *)

            ELSIF nextchar = Backspace THEN

                (* The effect of a backspace depends on whether the     *)
                (* accumulator holds a user-supplied number at this     *)
                (* stage.  This depends on things like whether the      *)
                (* user has typed several backspaces in a row.          *)

                IF NumberPresent THEN

                    (* Delete the number in the accumulator.    *)

                    Register[0].value := CMPLX(0.0,0.0);
                    NumberPresent := FALSE;

                ELSIF Register[0].ParenCount > 0 THEN

                    (* Remove one left parenthesis.     *)

                    DEC (Register[0].ParenCount);

                ELSE    (* Delete the last outstanding operator, if any. *)

                    IF Register[0].operator = EndMarker THEN
                        Beep;
                    ELSIF (Register[0].operator = FunctionSym)
                            OR (Register[0].operator = Function2Sym) THEN
                        PopStack;
                        Register[0].value := CMPLX(0.0,0.0);
                        NumberPresent := FALSE;
                    ELSE
                        PopStack;
                        NumberPresent := TRUE;
                    END (*IF*);

                END (*IF*);

                DisplayRegister(0);

            END (*IF*);

            nextchar := GetBuffer(InBuffer);

        END (*LOOP*);

    END LoadAccumulator;

(************************************************************************)

PROCEDURE RPNStep (VAR (*INOUT*) operator: CHAR);

    (* Performs one step of an RPN calculation.  Depending on the       *)
    (* operator, either we load a number or fetch an operator and do    *)
    (* the operation.  On exit, parameter 'operator' holds the next     *)
    (* unprocessed character.                                           *)

    CONST
        ImpliedPush = CharSet { '.', Enter, 'm', 'M', 'p', 'P' };
        BinaryOperators = CharSet {'+', '-', '*', '/', '^', '&', '|' };
        PrefixUnary = CharSet { '~' };
        PostfixUnary = CharSet { '%', 's', 'S' };

    VAR f: FunctionType;  f2: Function2Type;
        x: LONGCOMPLEX;  xr, yr: LONGREAL;  N: CARDINAL;

    BEGIN
        (* Before anything else, check whether the input is such as     *)
        (* to imply a push before the operation.                        *)

        IF operator IN (Digits + ImpliedPush) THEN
            IF NumberPresent THEN
                PushStack(Enter);
            END (*IF*);
        END (*IF*);

        (* Do we have a number?  *)

        IF (CAP(operator) IN (Digits + CharSet{'.'})) OR
              (((operator = '+') OR (operator = '-')) AND NOT NumberPresent) THEN

            AcceptNumber (operator);

        (* Or an operand from memory? *)

        ELSIF CAP(operator) ="M" THEN
            N := GetMemoryNumber();
            Register[0].value := MemoryValue[N].val;
            Register[0].polar := MemoryValue[N].polar;
            NumberPresent := TRUE;
            DisplayRegister(0);

        (* P means the constant PI. *)

        ELSIF CAP(operator) = "P" THEN
            Register[0].value := CMPLX (PI, 0.0);
            NumberPresent := TRUE;
            DisplayRegister(0);

        ELSIF operator = Enter THEN

            (* Enter has already been dealt with by the 'implied push'  *)
            (* test above.                                              *)

        ELSIF operator = Backspace THEN

            IF NumberPresent THEN
                Register[0].value := CMPLX (0.0, 0.0);
                NumberPresent := FALSE;
                DisplayRegister(0);
            ELSE
                PopStack;
            END (*IF*);

        ELSIF operator IN BinaryOperators THEN

            Register[0].operator := operator;
            BinaryOperation;

        ELSIF operator = FunctionSym THEN

            f := VAL(FunctionType, GetBuffer(InBuffer));
            x := Register[0].value;
            IF AnglesInDegrees AND ODD(Function[f].ConvertAngle) THEN
                x := CMPLX(PreScale,0.0)*x;
            END (*IF*);
            IF CxField THEN
                Register[0].value := Function[f].cx (x);
            ELSE
                Register[0].value :=
                      CMPLX(Function[f].re (RE(x)), 0.0);
            END (*IF*);
            IF AnglesInDegrees AND (Function[f].ConvertAngle > 1) THEN
                Register[0].value := CMPLX(PostScale,0.0)*Register[0].value;
            END (*IF*);
            DisplayRegister(0);

        ELSIF operator = Function2Sym THEN

            (* Nearly everything here is a special case. *)

            f2 := VAL(Function2Type, GetBuffer(InBuffer));
            IF CxField THEN
                IF (f2 = 1) OR (f2 = 2) THEN
                    Register[0].polar := f2 = 1;
                ELSE
                    Function2[f2].cx (Register[1].value, Register[0].value);
                END (*IF*);
            ELSE
                xr := RE(Register[0].value);
                yr := RE(Register[1].value);
                IF AnglesInDegrees AND (f2 = 2) THEN
                    xr := PreScale*xr;
                END (*IF*);
                Function2[f2].re (yr, xr);
                IF AnglesInDegrees AND (f2 = 1) THEN
                    xr := PostScale*xr;
                END (*IF*);
                Register[1].value := CMPLX(yr, 0.0);
                Register[0].value := CMPLX(xr, 0.0);
                DisplayRegister(1);
            END (*IF*);
            IF f2 = 0 THEN
                PopStack;
            END (*IF*);
            DisplayRegister(0);

        (* A distinction between postfix unary and prefix unary makes   *)
        (* no sense in RPN, but because this calculator also supports   *)
        (* algebraic notation we already have separate procedures for   *)
        (* those two cases.                                             *)

        ELSIF operator IN PrefixUnary THEN

            PrefixUnaryOperation (operator);

        ELSIF operator IN PostfixUnary THEN

            PostfixUnaryOperation (operator);

        ELSE
            Beep;
        END (*IF*);

        operator := GetBuffer(InBuffer);

    END RPNStep;

(************************************************************************)

PROCEDURE AlgStep (VAR (*INOUT*) operator: CHAR);

    (* Performs one step of an 'algebraic notation' calculation.  To do *)
    (* this, we pick up an operand followed by an operator.  (Fetching  *)
    (* the operand, which is done by procedure LoadAccumulator, may     *)
    (* itself involve some subexpression evaluation, because the        *)
    (* operand can include opening and closing parentheses, prefix and  *)
    (* postfix functions, and the like.  Procedure LoadAccumulator also *)
    (* allows some of the preceding input to be deleted via the         *)
    (* Backspace key.)  The operator may be a binary operator,          *)
    (* or Enter, or '='.  (These last two are considered to be          *)
    (* equivalent.)  Anything else is considered to be an unknown       *)
    (* operator, and results in an audible Beep.                        *)
    (* A subcalculation, or possibly a whole sequence of them, is       *)
    (* triggered if there are more closing parentheses than opening     *)
    (* parentheses, or if the operator has lower priority than the last *)
    (* stacked operator.                                                *)

    BEGIN
        LoadAccumulator (operator);

        IF (operator <> 'o') AND (operator <> Esc) THEN

            (* Perform any pending operations. *)

            NumberPresent := TRUE;
            WHILE TopOperatorPriority() >= priority(operator) DO
                BinaryOperation;
            END (*WHILE*);

            (* Push the latest operator, unless it marks the end        *)
            (* of the calculation.                                      *)

            IF priority(operator) = UnknownOperatorPriority THEN
                Beep;
            ELSIF (operator <> Enter) AND (operator <> "=") THEN
                PushStack (operator);
            END(*IF*);

            (* Fetch the lookahead character. *)

            operator := GetBuffer(InBuffer);

        END (*IF*);

    END AlgStep;

(************************************************************************)

PROCEDURE HandleOption (opt: CARDINAL);

    (* Special options. *)

    TYPE C4 = ARRAY [3..6] OF CARDINAL;
    CONST Base = C4 { 2, 8, 10, 16 };

    BEGIN
        CASE opt OF
          | 1,2:
              (* Options 1, 2 are algebraic and RPN, respectively. *)
              SetMode (opt = 2);
          | 3..6:
              (* Options [3..6] are numeric base specifications. *)
              SetBase (Base[opt]);
          | 7,8:
              (* Options 7, 8 are real and complex, respectively. *)
              SetComplex (opt = 8);
          | 9,10:
              (* Options 9, 10 are degrees and radians, respectively. *)
              SetDegrees (opt = 9);
          | 11,12:
              (* Options 11, 12 are to disable or enable beep. *)
              BlipEnabled := opt = 12;
        ELSE
            (* Ignore unknown options. *)
        END (*CASE*);
    END HandleOption;

(************************************************************************)

PROCEDURE ToggleStatusWindow (k: CARDINAL);

    (* Rolls the status k up to the next possibility. *)

    BEGIN
        CASE k OF
          | 1:  (* Algebraic or RPN. *)

                SetMode (NOT RPNmode);

          | 2:  (* Number base. *)

                CASE NumberBase OF
                  | 2:   SetBase(8);
                  | 8:   SetBase(10);
                  | 10:  SetBase(16);
                  | 16:  SetBase(2);
                ELSE
                         SetBase((NumberBase+1) MOD 16);
                END (*CASE*);

          | 3:  (* Real or complex. *)

                SetComplex (NOT CxField);

          | 4:  (* Degrees or radians. *)

                SetDegrees (NOT AnglesInDegrees);

        ELSE
            (* Ignore unknown options. *)
        END (*CASE*);
    END ToggleStatusWindow;

(************************************************************************)

PROCEDURE PerformCalculation;

    (* This procedure consists of a loop which is repeated until an     *)
    (* Esc character is encountered.  Each time around the loop we      *)
    (* perform one basic step.  In RPN mode, the basic step consists of *)
    (* either loading a number or fetching and executing an operator.   *)
    (* In algebraic mode, the basic step is to pick up an operand       *)
    (* followed by an operator, but this might in itself involve some   *)
    (* subexpression evaluation.                                        *)

    VAR operator: CHAR;

    BEGIN
        operator := GetBuffer(InBuffer);
        LOOP
            WHILE operator = 'o' DO
                HandleOption (ORD(GetBuffer(InBuffer)));
                operator := GetBuffer(InBuffer);
            END (*WHILE*);

            (* The Esc key drops us out of the calculator.  Otherwise,  *)
            (* perform one calculation step.                            *)

            IF operator = Esc THEN
                EXIT (*LOOP*);
            ELSIF RPNmode THEN
                RPNStep (operator);
            ELSE
                AlgStep (operator);
            END (*IF*);

        END (*LOOP*);

    END PerformCalculation;

(************************************************************************)

PROCEDURE SaveINIData;

    (* Saves part of the calculator state to the INI file. *)

    VAR hini: HINI;

    (********************************************************************)

    PROCEDURE Put (app, key: ARRAY OF CHAR;
                         VAR (*OUT*) value: ARRAY OF LOC);

        BEGIN
            INIPut (hini, app, key, value);
        END Put;

    (********************************************************************)

    VAR j: CARDINAL;
        code: ARRAY [0..1] OF CHAR;
        name: ARRAY [0..255] OF CHAR;

    BEGIN
        code[1] := Nul;
        name := INIFileName;
        hini := OpenINIFile(name, FALSE);
        Put ("Options", "Base", NumberBase);
        Put ("Options", "RPN", RPNmode);
        Put ("Options", "Complex", CxField);
        Put ("Options", "Degrees", AnglesInDegrees);
        Put ("Options", "Beep", BlipEnabled);
        FOR j := 0 TO MaxMemoryNumber DO
            code[0] := CHR(ORD('0')+j);
            Put ("Memory", code, MemoryValue[j]);
        END (*FOR*);
        CloseINIFile (hini);
    END SaveINIData;

(************************************************************************)

PROCEDURE CalcTask;

    (* Here's the calculator task itself. *)

    VAR hab: OS2.HAB;  hmq: OS2.HMQ;

    BEGIN
        (* Create anchor block and message queue for this thread. *)

        hab := OS2.WinInitialize(0);
        IF hab = OS2.NULLHANDLE THEN
           Beep;
           HALT(1);
        END;
        hmq := OS2.WinCreateMsgQueue (hab, 0);
        IF hmq = OS2.NULLHANDLE THEN
            Beep;
            OS2.WinTerminate(hab);
            HALT(1);
        END (*IF*);

        (* Start the real work. *)

        SetBase (NumberBase);
        SetMode (RPNmode);
        SetComplex (CxField);
        SetDegrees (AnglesInDegrees);
        Display.AdjustScreenLayout;
        PerformCalculation;
        SaveINIData;
        OS2.WinPostMsg (MainHwnd, WM_FINISHED, NIL, NIL);

    END CalcTask;

(************************************************************************)
(*                        PRE-START OPERATIONS                          *)
(************************************************************************)

PROCEDURE Start (hwnd: OS2.HWND);

    (* Starts the calculator task running. *)

    BEGIN
        MainHwnd := hwnd;
        EVAL (CreateTask (CalcTask, 15, "KeyCalc"));
    END Start;

(************************************************************************)
(*                       MODULE INITIALISATION                          *)
(************************************************************************)

PROCEDURE ClearCalculatorState;

    (* Clears all of the working registers of the calculator.   *)

    VAR j: RegisterNumber;  mem: MemoryNumber;

    BEGIN
        FOR j := 0 TO MaxRegisterNumber DO
            WITH Register[j] DO
                ParenCount := 0;
                value := CMPLX (0.0, 0.0);
                operator := EmptyMarker;
            END (*WITH*);
        END (*FOR*);
        Register[0].operator := EndMarker;
        NumberPresent := FALSE;
        FOR mem := 0 TO MAX(MemoryNumber) DO
            MemoryValue[mem].val := CMPLX(0.0, 0.0);
            MemoryValue[mem].polar := FALSE;
        END (*FOR*);
    END ClearCalculatorState;

(************************************************************************)

PROCEDURE LoadINIData;

    (* Loads stored information from the INI file. *)

    VAR hini: HINI;

    (********************************************************************)

    PROCEDURE Get (app, key: ARRAY OF CHAR;
                         VAR (*OUT*) value: ARRAY OF LOC): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, app, key, value);
        END Get;

    (********************************************************************)

    VAR j: CARDINAL;
        code: ARRAY [0..1] OF CHAR;
        name: ARRAY [0..255] OF CHAR;

    BEGIN
        code[1] := Nul;
        name := INIFileName;
        hini := OpenINIFile(name, FALSE);
        IF NOT Get ("Options", "Base", NumberBase) THEN
            NumberBase := 10;
        END (*IF*);
        IF NOT Get ("Options", "RPN", RPNmode) THEN
            RPNmode := FALSE;
        END (*IF*);
        IF NOT Get ("Options", "Complex", CxField) THEN
            CxField := FALSE;
        END (*IF*);
        IF NOT Get ("Options", "Degrees", AnglesInDegrees) THEN
            AnglesInDegrees := FALSE;
        END (*IF*);
        IF NOT Get ("Options", "Beep", BlipEnabled) THEN
            BlipEnabled := TRUE;
        END (*IF*);
        FOR j := 0 TO MaxMemoryNumber DO
            code[0] := CHR(ORD('0')+j);
            IF NOT Get ("Memory", code, MemoryValue[j].val) THEN
                MemoryValue[j].val := CMPLX (0.0, 0.0);
            END (*IF*);
            IF NOT Get ("MemoryPolar", code, MemoryValue[j].polar) THEN
                MemoryValue[j].polar := FALSE;
            END (*IF*);
        END (*FOR*);
        CloseINIFile (hini);
    END LoadINIData;

(************************************************************************)

BEGIN
    PreScale := PI/180.0;  PostScale := 180.0/PI;
    BlipEnabled := TRUE;
    ClearCalculatorState;
    LoadINIData;
    CreateBuffer (InBuffer, 64);
END DoCalc.

