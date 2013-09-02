IMPLEMENTATION MODULE Display;

        (************************************************************)
        (*                                                          *)
        (*                      PM Calculator                       *)
        (*                Screen display operations                 *)
        (*                                                          *)
        (*    Started:        14 February 2002                      *)
        (*    Last edited:    13 March 2002                         *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT OS2, DID, PMInit, Strings, Keyboard;

FROM SYSTEM IMPORT
    (* proc *)  ADR;

FROM INIData IMPORT
    (* proc *)  SetInitialWindowPosition, StoreWindowPosition,
                OpenINIFile, INIGet, INIPut, INIGetString, INIPutString,
                CloseINIFile;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

(************************************************************************)

CONST
    Nul = CHR(0);
    INIFileName = "Keycalc.INI";
    MaxStatusNo = 3;

    DefaultStatusFontName = "8.Helv";
    DefaultMemoryFontName = "8.Helv";
    DefaultAccFontName = "12.Courier Bold";
    DefaultStackFontName = "10.Courier";
    DefaultMenuFontName = "9.WarpSans";

    DefaultFrameColour = 0CCCCCCH;
    DefaultStatusBackColour = 0FFFF00H;
    DefaultMemoryBackColour = 0CCCCCCH;
    DefaultAccBackColour = 0FFFFH;
    DefaultStackBackColour = 0CCCCCCH;
    DefaultMenuBackColour = 0CCCCCCH;

    DefaultStatusForeColour = 0FFH;
    DefaultMemoryForeColour = 0;
    DefaultAccForeColour = 0;
    DefaultStackForeColour = 0;
    DefaultMenuForeColour = 0;

    FontNameSize = 256;

TYPE
    LeftRight = (left, right);
    WindowPair = ARRAY LeftRight OF OS2.HWND;
    FontName = ARRAY [0..FontNameSize] OF CHAR;

VAR
    (* Whether we're using complex arithmetic. *)

    CxField: BOOLEAN;

    (* The frame window. *)

    frame: OS2.HWND;

    (* The windows that hold the accumulator and stack displays. *)

    StackReg: ARRAY RegisterNumber OF WindowPair;

    (* Ditto for the memory registers. *)

    Memory: ARRAY VisibleMemoryNumber OF WindowPair;

    (* Ditto for the status registers. *)

    Status: ARRAY [0..MaxStatusNo] OF OS2.HWND;

    (* The fonts for the various subwindows. *)

    StatusFontName, MemoryFontName, AccFontName,
                                  StackFontName, MenuFontName: FontName;

    (* Background colours. *)

    StatusBackColour, MemoryBackColour, AccBackColour,
            StackBackColour, MenuBackColour, GeneralBackground: INTEGER;

    (* Foreground colours. *)

    StatusForeColour, MemoryForeColour, AccForeColour,
                                StackForeColour, MenuForeColour: INTEGER;

    (* Num Lock state when we started the program, and the state the    *)
    (* user wants.                                                      *)

    OldNumLock, NumLockWanted: BOOLEAN;

(************************************************************************)

PROCEDURE Message (mess: ARRAY OF CHAR);

    (* Debugging message. *)

    BEGIN
        PMInit.MessageBox (OS2.HWND_DESKTOP, mess, 0, FALSE);
    END Message;

(************************************************************************)
(*                          NUMERIC CONVERSION                          *)
(************************************************************************)

PROCEDURE CardToString (val: CARDINAL;  VAR (*OUT*) result: ARRAY OF CHAR;
                        fieldsize: CARDINAL);

    VAR j: CARDINAL;

    BEGIN
        IF val > 9 THEN
            CardToString (val DIV 10, result, fieldsize-1);
        ELSE
            FOR j := 0 TO fieldsize-2 DO
                result[j] := ' ';
            END (*FOR*);
        END (*IF*);
        result[fieldsize-2] := CHR(ORD('0') + val MOD 10);
        result[fieldsize-1] := CHR(0);
    END CardToString;

(************************************************************************)

(*
PROCEDURE WriteMemCard (N: VisibleMemoryNumber;  value: CARDINAL);

    (* Writes to memory register N. *)

    VAR text: ARRAY [0..DigitsPerRegister] OF CHAR;

    BEGIN
        CardToString (value, text, DigitsPerRegister);
        OS2.WinSetWindowText (Memory[N][right], text);
    END WriteMemCard;
*)

(************************************************************************)
(*                          OPTIONS DISPLAY                             *)
(************************************************************************)

PROCEDURE SetBase (base: CARDINAL);

    (* Sets the base for numeric entry and display. *)

    VAR buffer: ARRAY [0..3] OF CHAR;

    BEGIN
        IF base = 2 THEN
            buffer := "Bin";
        ELSIF base = 8 THEN
            buffer := "Oct";
        ELSIF base = 10 THEN
            buffer := "Dec";
        ELSIF base = 16 THEN
            buffer := "Hex";
        ELSE
            CardToString (base, buffer, 3);
        END (*IF*);
        OS2.WinSetWindowText (Status[1], buffer);
    END SetBase;

(************************************************************************)

PROCEDURE SetRPN (RPN: BOOLEAN);

    (* Algebraic mode for FALSE, RPN mode for TRUE. *)

    BEGIN
        IF RPN THEN
            OS2.WinSetWindowText (Status[0], "RPN");
        ELSE
            OS2.WinSetWindowText (Status[0], "Alg");
        END (*IF*);
    END SetRPN;

(************************************************************************)

PROCEDURE SetComplex (Cmplx: BOOLEAN);

    (* Real arithmetic for FALSE, complex arithmetic for TRUE. *)

    BEGIN
        CxField := Cmplx;
        IF Cmplx THEN
            OS2.WinSetWindowText (Status[2], "Cx");
        ELSE
            OS2.WinSetWindowText (Status[2], "Real");
        END (*IF*);
        AdjustScreenLayout;
    END SetComplex;

(************************************************************************)

PROCEDURE SetDegrees (deg: BOOLEAN);

    (* Angles are measured in degrees iff deg = TRUE. *)

    BEGIN
        IF deg THEN
            OS2.WinSetWindowText (Status[3], "deg");
        ELSE
            OS2.WinSetWindowText (Status[3], "rad");
        END (*IF*);
    END SetDegrees;

(************************************************************************)
(*                            SCREEN LAYOUT                             *)
(************************************************************************)

PROCEDURE AdjustScreenLayout;

    (* Works out the screen layout for the current font, and sets all   *)
    (* the window sizes.                                                *)

    VAR hps: OS2.HPS;
        metrics: OS2.FONTMETRICS;
        width, height, hgap, vgap, vborder, vbase, j,
        linewidth, maxwidth, DigitsPerRegister: CARDINAL;
        Points: ARRAY [0..1] OF OS2.POINTL;

    BEGIN
        IF CxField THEN
            DigitsPerRegister := 2*DigitsPerNumber + 3;
        ELSE
            DigitsPerRegister := DigitsPerNumber;
        END (*IF*);

        (* Set size and position of the Status windows. *)

        hps := OS2.WinGetPS (Status[0]);
        OS2.GpiQueryFontMetrics (hps, SIZE(OS2.FONTMETRICS), metrics);
        OS2.WinReleasePS (hps);
        width := (metrics.lMaxCharInc + metrics.lAveCharWidth) DIV 2;
        height := metrics.lEmHeight + metrics.lInternalLeading;
        vgap := metrics.lExternalLeading;
        IF vgap = 0 THEN
            vgap := metrics.lInternalLeading;
        END (*IF*);
        hgap := width;
        vborder := height DIV 2;
        vbase := vborder;
        maxwidth := hgap;
        FOR j := 0 TO MaxStatusNo DO
            OS2.WinSetWindowPos (Status[j], 0,
                                 maxwidth, vbase,
                                  4*width, height,
                                    OS2.SWP_MOVE + OS2.SWP_SIZE);
            INC (maxwidth, hgap+4*width);
        END (*FOR*);
        INC (vbase, height + vgap);

        (* Set size and position of the memory windows. *)

        hps := OS2.WinGetPS (Memory[3][right]);
        OS2.GpiQueryFontMetrics (hps, SIZE(OS2.FONTMETRICS), metrics);
        width := (metrics.lMaxCharInc + metrics.lAveCharWidth) DIV 2;
        height := metrics.lEmHeight + metrics.lInternalLeading;
        vgap := metrics.lExternalLeading;
        Points[0].x := width;
        Points[0].y := height;
        Points[1].x := 0;
        Points[1].y := vgap;
        IF NOT OS2.GpiConvert (hps, OS2.CVTC_WORLD, OS2.CVTC_DEVICE, 2, Points) THEN
            PMInit.MessageBox (frame, "GpiConvert failed", 0, FALSE);
        END (*IF*);
        OS2.WinReleasePS (hps);
        width := Points[0].x;
        height := Points[0].y;
        vgap := Points[1].y;
        hgap := width;
        FOR j := 0 TO MaxVisibleMemoryNumber DO
            OS2.WinSetWindowPos (Memory[MaxVisibleMemoryNumber-j][left], 0,
                                 hgap, vbase,
                                  4*width, height,
                                    OS2.SWP_MOVE + OS2.SWP_SIZE);
            OS2.WinSetWindowPos (Memory[MaxVisibleMemoryNumber-j][right], 0,
                                 2*hgap + 4*width, vbase,
                                  DigitsPerRegister*width, height,
                                    OS2.SWP_MOVE + OS2.SWP_SIZE);
            INC (vbase, height + vgap);
        END (*FOR*);
        linewidth := 3*hgap + (DigitsPerRegister + 4)*width;
        IF maxwidth < linewidth THEN
            maxwidth := linewidth;
        END (*IF*);
        INC (vbase, height + vgap);

        (* Next the accumulator. *)

        hps := OS2.WinGetPS (StackReg[0][right]);
        OS2.GpiQueryFontMetrics (hps, SIZE(OS2.FONTMETRICS), metrics);
        OS2.WinReleasePS (hps);
        width := (metrics.lMaxCharInc + metrics.lAveCharWidth) DIV 2;
        height := metrics.lEmHeight + metrics.lInternalLeading;
        vgap := metrics.lExternalLeading;
        hgap := width;
        OS2.WinSetWindowPos (StackReg[0][left], 0,
                             hgap, vbase,
                              4*width, height,
                                OS2.SWP_MOVE + OS2.SWP_SIZE);
        OS2.WinSetWindowPos (StackReg[0][right], 0,
                             2*hgap + 4*width, vbase,
                              DigitsPerRegister*width, height,
                                OS2.SWP_MOVE + OS2.SWP_SIZE);
        INC (vbase, height + vgap);
        linewidth := 3*hgap + (DigitsPerRegister + 4)*width;
        IF maxwidth < linewidth THEN
            maxwidth := linewidth;
        END (*IF*);

        (* Now the same for the stack register windows. *)

        hps := OS2.WinGetPS (StackReg[1][right]);
        OS2.GpiQueryFontMetrics (hps, SIZE(OS2.FONTMETRICS), metrics);
        OS2.WinReleasePS (hps);
        width := (metrics.lMaxCharInc + metrics.lAveCharWidth) DIV 2;
        height := metrics.lEmHeight + metrics.lInternalLeading;
        vgap := metrics.lExternalLeading;
        hgap := width;
        INC (vbase, height DIV 4);
        FOR j := 1 TO MaxVisibleRegisterNumber DO
            OS2.WinSetWindowPos (StackReg[j][left], 0,
                                 hgap, vbase,
                                  4*width, height,
                                    OS2.SWP_MOVE + OS2.SWP_SIZE);
            OS2.WinSetWindowPos (StackReg[j][right], 0,
                                 2*hgap + 4*width, vbase,
                                  DigitsPerRegister*width, height,
                                    OS2.SWP_MOVE + OS2.SWP_SIZE);
            INC (vbase, height + vgap);
        END (*FOR*);
        linewidth := 3*hgap + (DigitsPerRegister + 4)*width;
        IF maxwidth < linewidth THEN
            maxwidth := linewidth;
        END (*IF*);

        (* Finally, set the overall window size, but leave the  *)
        (* position unchanged.                                  *)

        OS2.WinSetWindowPos (frame, 0, 0, 0,
                                  maxwidth, vbase + vgap + 26,
                                    OS2.SWP_SIZE);

    END AdjustScreenLayout;

(************************************************************************)

PROCEDURE SetFonts;

    (* Sets the fonts of all subwindows.  *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 0 TO MaxStatusNo DO
            OS2.WinSetPresParam (Status[j], OS2.PP_FONTNAMESIZE,
                                 FontNameSize, StatusFontName);
        END (*FOR*);
        FOR j := 0 TO MAX(VisibleMemoryNumber) DO
            OS2.WinSetPresParam (Memory[j][left], OS2.PP_FONTNAMESIZE,
                                 FontNameSize, MemoryFontName);
            OS2.WinSetPresParam (Memory[j][right], OS2.PP_FONTNAMESIZE,
                                 FontNameSize, MemoryFontName);
        END (*FOR*);
        OS2.WinSetPresParam (StackReg[0][left], OS2.PP_FONTNAMESIZE,
                             FontNameSize, AccFontName);
        OS2.WinSetPresParam (StackReg[0][right], OS2.PP_FONTNAMESIZE,
                             FontNameSize, AccFontName);
        FOR j := 1 TO MAX(RegisterNumber) DO
            OS2.WinSetPresParam (StackReg[j][left], OS2.PP_FONTNAMESIZE,
                                 FontNameSize, StackFontName);
            OS2.WinSetPresParam (StackReg[j][right], OS2.PP_FONTNAMESIZE,
                                 FontNameSize, StackFontName);
        END (*FOR*);
    END SetFonts;

(************************************************************************)

PROCEDURE SetColours;

    (* Sets the colours of all subwindows.  *)

    VAR j: CARDINAL;  side: LeftRight;

    BEGIN
        (* General background. *)

        OS2.WinSetPresParam (frame, OS2.PP_BACKGROUNDCOLOR,
                                 SIZE(INTEGER), ADR(GeneralBackground));
        FOR j := 0 TO MAX(VisibleMemoryNumber) DO
            OS2.WinSetPresParam (Memory[j][left], OS2.PP_BACKGROUNDCOLOR,
                                     SIZE(INTEGER), ADR(GeneralBackground));
        END (*FOR*);
        FOR j := 0 TO MAX(RegisterNumber) DO
            OS2.WinSetPresParam (StackReg[j][left], OS2.PP_BACKGROUNDCOLOR,
                                     SIZE(INTEGER), ADR(GeneralBackground));
        END (*FOR*);

        (* Status displays. *)

        FOR j := 0 TO MaxStatusNo DO
            OS2.WinSetPresParam (Status[j], OS2.PP_BACKGROUNDCOLOR,
                                 SIZE(INTEGER), ADR(StatusBackColour));
            OS2.WinSetPresParam (Status[j], OS2.PP_FOREGROUNDCOLOR,
                                 SIZE(INTEGER), ADR(StatusForeColour));
        END (*FOR*);

        (* Memory display. *)

        FOR j := 0 TO MAX(VisibleMemoryNumber) DO
            FOR side := left TO right DO
                OS2.WinSetPresParam (Memory[j][side], OS2.PP_FOREGROUNDCOLOR,
                                     SIZE(INTEGER), ADR(MemoryForeColour));
            END (*FOR*);
            OS2.WinSetPresParam (Memory[j][right], OS2.PP_BACKGROUNDCOLOR,
                                     SIZE(INTEGER), ADR(MemoryBackColour));
        END (*FOR*);

        (* Accumulator. *)

        FOR side := left TO right DO
            OS2.WinSetPresParam (StackReg[0][side], OS2.PP_FOREGROUNDCOLOR,
                                 SIZE(INTEGER), ADR(AccForeColour));
        END (*FOR*);
        OS2.WinSetPresParam (StackReg[0][right], OS2.PP_BACKGROUNDCOLOR,
                                 SIZE(INTEGER), ADR(AccBackColour));

        (* The stack. *)

        FOR j := 1 TO MAX(RegisterNumber) DO
            FOR side := left TO right DO
                OS2.WinSetPresParam (StackReg[j][side], OS2.PP_FOREGROUNDCOLOR,
                                     SIZE(INTEGER), ADR(StackForeColour));
            END (*FOR*);
            OS2.WinSetPresParam (StackReg[j][right], OS2.PP_BACKGROUNDCOLOR,
                                     SIZE(INTEGER), ADR(StackBackColour));
        END (*FOR*);

    END SetColours;

(************************************************************************)

PROCEDURE SetMenuPresParams (MenuWindow: OS2.HWND);

    (* Sets the menu font and colours from values stored in the INI file. *)

    BEGIN
        (* Colours. *)

        OS2.WinSetPresParam (MenuWindow, OS2.PP_FOREGROUNDCOLOR,
                                 SIZE(INTEGER), ADR(MenuForeColour));
        OS2.WinSetPresParam (MenuWindow, OS2.PP_BACKGROUNDCOLOR,
                                 SIZE(INTEGER), ADR(MenuBackColour));

        (* Font. *)

        OS2.WinSetPresParam (MenuWindow, OS2.PP_FONTNAMESIZE,
                                 FontNameSize, MenuFontName);

    END SetMenuPresParams;

(************************************************************************)

PROCEDURE CheckFontChange;

    (* Check whether any fonts or colours have changed, and if so take  *)
    (* appropriate action.                                              *)

    VAR j, length, AttrFound: CARDINAL;  side: LeftRight;
        fontchanged, anyfontchange: BOOLEAN;
        backcolourchanged, forecolourchanged, anycolourchange: BOOLEAN;
        NewFont: FontName;
        NewColour: INTEGER;

    BEGIN
        NewColour := OS2.CLR_PALEGRAY;

        (* Status windows *)

        fontchanged := FALSE;
        backcolourchanged := FALSE;
        forecolourchanged := FALSE;
        FOR j := 0 TO MaxStatusNo DO
            IF NOT fontchanged THEN
                length := OS2.WinQueryPresParam (Status[j], OS2.PP_FONTNAMESIZE, 0,
                                             AttrFound,
                                             FontNameSize, NewFont, 0);
                IF length < FontNameSize THEN
                    NewFont[length] := Nul;
                END (*IF*);
                IF NOT Strings.Equal (NewFont, StatusFontName) THEN
                    StatusFontName := NewFont;
                    fontchanged := TRUE;
                END (*IF*);
            END (*IF*);
            IF NOT backcolourchanged THEN
                OS2.WinQueryPresParam (Status[j], OS2.PP_BACKGROUNDCOLOR,
                                             OS2.PP_BACKGROUNDCOLORINDEX, AttrFound,
                                             SIZE(INTEGER), ADR(NewColour),
                                             OS2.QPF_ID2COLORINDEX);
                IF NewColour <> StatusBackColour THEN
                    StatusBackColour := NewColour;
                    backcolourchanged := TRUE;
                END (*IF*);
            END (*IF*);
            IF NOT forecolourchanged THEN
                OS2.WinQueryPresParam (Status[j], OS2.PP_FOREGROUNDCOLOR,
                                             OS2.PP_FOREGROUNDCOLORINDEX, AttrFound,
                                             SIZE(INTEGER), ADR(NewColour),
                                             OS2.QPF_ID2COLORINDEX);
                IF NewColour <> StatusForeColour THEN
                    StatusForeColour := NewColour;
                    forecolourchanged := TRUE;
                END (*IF*);
            END (*IF*);
        END (*FOR*);
        anyfontchange := fontchanged;
        anycolourchange := backcolourchanged OR forecolourchanged;

        (* Memory *)

        fontchanged := FALSE;
        backcolourchanged := FALSE;
        forecolourchanged := FALSE;
        FOR j := 0 TO MAX(VisibleMemoryNumber) DO
            FOR side := left TO right DO
                IF NOT fontchanged THEN
                    length := OS2.WinQueryPresParam (Memory[j][side], OS2.PP_FONTNAMESIZE, 0,
                                                 AttrFound,
                                                 FontNameSize, NewFont, 0);
                    IF length < FontNameSize THEN
                        NewFont[length] := Nul;
                    END (*IF*);
                    IF NOT Strings.Equal (NewFont, MemoryFontName) THEN
                        MemoryFontName := NewFont;
                        fontchanged := TRUE;
                    END (*IF*);
                END (*IF*);
                IF NOT forecolourchanged THEN
                    OS2.WinQueryPresParam (Memory[j][side], OS2.PP_FOREGROUNDCOLOR,
                                                 OS2.PP_FOREGROUNDCOLORINDEX, AttrFound,
                                                 SIZE(INTEGER), ADR(NewColour),
                                                 OS2.QPF_ID2COLORINDEX);
                    IF NewColour <> MemoryForeColour THEN
                        MemoryForeColour := NewColour;
                        forecolourchanged := TRUE;
                    END (*IF*);
                END (*IF*);
            END (*FOR*);
            IF NOT backcolourchanged THEN
                OS2.WinQueryPresParam (Memory[j][right], OS2.PP_BACKGROUNDCOLOR,
                                             OS2.PP_BACKGROUNDCOLORINDEX, AttrFound,
                                             SIZE(INTEGER), ADR(NewColour),
                                             OS2.QPF_ID2COLORINDEX);
                IF NewColour <> MemoryBackColour THEN
                    MemoryBackColour := NewColour;
                    backcolourchanged := TRUE;
                END (*IF*);
            END (*IF*);
        END (*FOR*);
        anyfontchange := anyfontchange OR fontchanged;
        anycolourchange := anycolourchange OR backcolourchanged OR forecolourchanged;

        (* Accumulator *)

        fontchanged := FALSE;
        backcolourchanged := FALSE;
        forecolourchanged := FALSE;
        FOR side := left TO right DO
            IF NOT fontchanged THEN
                length := OS2.WinQueryPresParam (StackReg[0][side], OS2.PP_FONTNAMESIZE, 0,
                                             AttrFound,
                                             FontNameSize, NewFont, 0);
                IF length < FontNameSize THEN
                    NewFont[length] := Nul;
                END (*IF*);
                IF NOT Strings.Equal (NewFont, AccFontName) THEN
                    AccFontName := NewFont;
                    fontchanged := TRUE;
                END (*IF*);
            END (*IF*);
            IF NOT forecolourchanged THEN
                OS2.WinQueryPresParam (StackReg[0][side], OS2.PP_FOREGROUNDCOLOR,
                                             OS2.PP_FOREGROUNDCOLORINDEX, AttrFound,
                                             SIZE(INTEGER), ADR(NewColour),
                                             OS2.QPF_ID2COLORINDEX);
                IF NewColour <> AccForeColour THEN
                    AccForeColour := NewColour;
                    forecolourchanged := TRUE;
                END (*IF*);
            END (*IF*);
        END (*FOR*);
        OS2.WinQueryPresParam (StackReg[0][right], OS2.PP_BACKGROUNDCOLOR,
                                     OS2.PP_BACKGROUNDCOLORINDEX, AttrFound,
                                     SIZE(INTEGER), ADR(NewColour),
                                     OS2.QPF_ID2COLORINDEX);
        IF NewColour <> AccBackColour THEN
            AccBackColour := NewColour;
            backcolourchanged := TRUE;
        END (*IF*);
        anyfontchange := anyfontchange OR fontchanged;
        anycolourchange := anycolourchange OR backcolourchanged OR forecolourchanged;

        (* Stack *)

        fontchanged := FALSE;
        backcolourchanged := FALSE;
        forecolourchanged := FALSE;
        FOR j := 1 TO MAX(RegisterNumber) DO
            FOR side := left TO right DO
                IF NOT fontchanged THEN
                    length := OS2.WinQueryPresParam (StackReg[j][side], OS2.PP_FONTNAMESIZE, 0,
                                                 AttrFound,
                                                 FontNameSize, NewFont, 0);
                    IF length < FontNameSize THEN
                        NewFont[length] := Nul;
                    END (*IF*);
                    IF NOT Strings.Equal (NewFont, StackFontName) THEN
                        StackFontName := NewFont;
                        fontchanged := TRUE;
                    END (*IF*);
                END (*IF*);
                IF NOT forecolourchanged THEN
                    OS2.WinQueryPresParam (StackReg[j][side], OS2.PP_FOREGROUNDCOLOR,
                                                 OS2.PP_FOREGROUNDCOLORINDEX, AttrFound,
                                                 SIZE(INTEGER), ADR(NewColour),
                                                 OS2.QPF_ID2COLORINDEX);
                    IF NewColour <> StackForeColour THEN
                        StackForeColour := NewColour;
                        forecolourchanged := TRUE;
                    END (*IF*);
                END (*IF*);
            END (*FOR*);
            IF NOT backcolourchanged THEN
                OS2.WinQueryPresParam (StackReg[j][right], OS2.PP_BACKGROUNDCOLOR,
                                             OS2.PP_BACKGROUNDCOLORINDEX, AttrFound,
                                             SIZE(INTEGER), ADR(NewColour),
                                             OS2.QPF_ID2COLORINDEX);
                IF NewColour <> StackBackColour THEN
                    StackBackColour := NewColour;
                    backcolourchanged := TRUE;
                END (*IF*);
            END (*IF*);
        END (*FOR*);
        anyfontchange := anyfontchange OR fontchanged;
        anycolourchange := anycolourchange OR backcolourchanged OR forecolourchanged;

        (* General background colour. *)

        backcolourchanged := FALSE;
        OS2.WinQueryPresParam (frame, OS2.PP_BACKGROUNDCOLOR,
                                     OS2.PP_BACKGROUNDCOLORINDEX, AttrFound,
                                     SIZE(INTEGER), ADR(NewColour),
                                     OS2.QPF_ID2COLORINDEX);
        IF NewColour <> GeneralBackground THEN
            GeneralBackground := NewColour;
            backcolourchanged := TRUE;
        END (*IF*);
        FOR j := 0 TO MAX(VisibleMemoryNumber) DO
            IF NOT backcolourchanged THEN
                OS2.WinQueryPresParam (Memory[j][left], OS2.PP_BACKGROUNDCOLOR,
                                             OS2.PP_BACKGROUNDCOLORINDEX, AttrFound,
                                             SIZE(INTEGER), ADR(NewColour),
                                             OS2.QPF_ID2COLORINDEX);
                IF NewColour <> GeneralBackground THEN
                    GeneralBackground := NewColour;
                    backcolourchanged := TRUE;
                END (*IF*);
            END (*IF*);
        END (*FOR*);
        FOR j := 0 TO MAX(RegisterNumber) DO
            IF NOT backcolourchanged THEN
                OS2.WinQueryPresParam (StackReg[j][left], OS2.PP_BACKGROUNDCOLOR,
                                             OS2.PP_BACKGROUNDCOLORINDEX, AttrFound,
                                             SIZE(INTEGER), ADR(NewColour),
                                             OS2.QPF_ID2COLORINDEX);
                IF NewColour <> GeneralBackground THEN
                    GeneralBackground := NewColour;
                    backcolourchanged := TRUE;
                END (*IF*);
            END (*IF*);
        END (*FOR*);
        anycolourchange := anycolourchange OR backcolourchanged;

        (* If anything has changed, update all fonts and colours. *)

        IF anycolourchange THEN
            SetColours;
            AdjustScreenLayout;
        END (*IF*);

        IF anyfontchange THEN
            SetFonts;
            AdjustScreenLayout;
        END (*IF*);

    END CheckFontChange;

(************************************************************************)

PROCEDURE SaveMenuPresParams (MenuWindow: OS2.HWND);

    (* Takes a copy of the menu font and colours. *)

    VAR length, AttrFound: CARDINAL;

    BEGIN
        (* Save font. *)

        length := OS2.WinQueryPresParam (MenuWindow, OS2.PP_FONTNAMESIZE, 0,
                                         AttrFound,
                                         FontNameSize, MenuFontName, 0);
        IF length < FontNameSize THEN
            MenuFontName[length] := Nul;
        END (*IF*);

        (* Save colours. *)

        OS2.WinQueryPresParam (MenuWindow, OS2.PP_FOREGROUNDCOLOR,
                                     OS2.PP_FOREGROUNDCOLORINDEX, AttrFound,
                                     SIZE(INTEGER), ADR(MenuForeColour),
                                     OS2.QPF_ID2COLORINDEX);
        OS2.WinQueryPresParam (MenuWindow, OS2.PP_BACKGROUNDCOLOR,
                                     OS2.PP_BACKGROUNDCOLORINDEX, AttrFound,
                                     SIZE(INTEGER), ADR(MenuBackColour),
                                     OS2.QPF_ID2COLORINDEX);

    END SaveMenuPresParams;

(************************************************************************)
(*                   WRITING TO THE REGISTER WINDOWS                    *)
(************************************************************************)

PROCEDURE WriteRegister (N: RegisterNumber;  text: ARRAY OF CHAR);

    (* Writes to calculator register N. *)

    BEGIN
        OS2.WinSetWindowText (StackReg[N][right], text);
    END WriteRegister;

(************************************************************************)

PROCEDURE WriteOperator (N: RegisterNumber;  text: ARRAY OF CHAR);

    (* Writes to operator register N. *)

    BEGIN
        OS2.WinSetWindowText (StackReg[N][left], text);
    END WriteOperator;

(************************************************************************)

PROCEDURE WriteMemory (N: VisibleMemoryNumber;  text: ARRAY OF CHAR);

    (* Writes to memory register N. *)

    BEGIN
        OS2.WinSetWindowText (Memory[N][right], text);
    END WriteMemory;

(************************************************************************)
(*                         INITIALISATION                               *)
(************************************************************************)

PROCEDURE LoadFontsAndColours;

    (* Loads fonts and colours from the INI file. *)

    VAR hini: OS2.HINI;

    BEGIN
        hini := OpenINIFile(INIFileName);

        IF NOT INIGetString (hini, "Font", "Status", StatusFontName) THEN
            StatusFontName := DefaultStatusFontName;
        END (*IF*);
        IF NOT INIGetString (hini, "Font", "Memory", MemoryFontName) THEN
            MemoryFontName := DefaultMemoryFontName;
        END (*IF*);
        IF NOT INIGetString (hini, "Font", "Acc", AccFontName) THEN
            AccFontName := DefaultAccFontName;
        END (*IF*);
        IF NOT INIGetString (hini, "Font", "Stack", StackFontName) THEN
            StackFontName := DefaultStackFontName;
        END (*IF*);
        IF NOT INIGetString (hini, "Font", "Menu", MenuFontName) THEN
            StackFontName := DefaultMenuFontName;
        END (*IF*);

        IF NOT INIGet (hini, "BackColour", "Frame", GeneralBackground) THEN
            GeneralBackground := DefaultFrameColour;
        END (*IF*);
        IF NOT INIGet (hini, "BackColour", "Status", StatusBackColour) THEN
            StatusBackColour := DefaultStatusBackColour;
        END (*IF*);
        IF NOT INIGet (hini, "BackColour", "Memory", MemoryBackColour) THEN
            MemoryBackColour := DefaultMemoryBackColour;
        END (*IF*);
        IF NOT INIGet (hini, "BackColour", "Acc", AccBackColour) THEN
            AccBackColour := DefaultAccBackColour;
        END (*IF*);
        IF NOT INIGet (hini, "BackColour", "Stack", StackBackColour) THEN
            StackBackColour := DefaultStackBackColour;
        END (*IF*);
        IF NOT INIGet (hini, "BackColour", "Menu", MenuBackColour) THEN
            MenuBackColour := DefaultMenuBackColour;
        END (*IF*);

        IF NOT INIGet (hini, "ForeColour", "Status", StatusForeColour) THEN
            StatusForeColour := DefaultStatusForeColour;
        END (*IF*);
        IF NOT INIGet (hini, "ForeColour", "Memory", MemoryForeColour) THEN
            MemoryForeColour := DefaultMemoryForeColour;
        END (*IF*);
        IF NOT INIGet (hini, "ForeColour", "Acc", AccForeColour) THEN
            AccForeColour := DefaultAccForeColour;
        END (*IF*);
        IF NOT INIGet (hini, "ForeColour", "Stack", StackForeColour) THEN
            StackForeColour := DefaultStackForeColour;
        END (*IF*);
        IF NOT INIGet (hini, "ForeColour", "Menu", MenuForeColour) THEN
            MenuForeColour := DefaultMenuForeColour;
        END (*IF*);

        IF NOT INIGet (hini, "Options", "NumLock", NumLockWanted) THEN
            NumLockWanted := TRUE;
        END (*IF*);

        CloseINIFile (hini);

    END LoadFontsAndColours;

(************************************************************************)

PROCEDURE SetupWindows (hwnd: OS2.HWND);

    (* Initialisation operation: works out which windows should receive *)
    (* the output for the various dialogue controls.                    *)

    BEGIN
        frame := hwnd;

        StackReg[0][left]  := OS2.WinWindowFromID (hwnd, DID.OpStk0);
        StackReg[0][right] := OS2.WinWindowFromID (hwnd, DID.NumStk0);
        StackReg[1][left]  := OS2.WinWindowFromID (hwnd, DID.OpStk1);
        StackReg[1][right] := OS2.WinWindowFromID (hwnd, DID.NumStk1);
        StackReg[2][left]  := OS2.WinWindowFromID (hwnd, DID.OpStk2);
        StackReg[2][right] := OS2.WinWindowFromID (hwnd, DID.NumStk2);
        StackReg[3][left]  := OS2.WinWindowFromID (hwnd, DID.OpStk3);
        StackReg[3][right] := OS2.WinWindowFromID (hwnd, DID.NumStk3);
        StackReg[4][left]  := OS2.WinWindowFromID (hwnd, DID.OpStk4);
        StackReg[4][right] := OS2.WinWindowFromID (hwnd, DID.NumStk4);

        Memory[0][left]  := OS2.WinWindowFromID (hwnd, DID.MemLabel0);
        Memory[0][right] := OS2.WinWindowFromID (hwnd, DID.Mem0);
        Memory[1][left]  := OS2.WinWindowFromID (hwnd, DID.MemLabel1);
        Memory[1][right] := OS2.WinWindowFromID (hwnd, DID.Mem1);
        Memory[2][left]  := OS2.WinWindowFromID (hwnd, DID.MemLabel2);
        Memory[2][right] := OS2.WinWindowFromID (hwnd, DID.Mem2);
        Memory[3][left]  := OS2.WinWindowFromID (hwnd, DID.MemLabel3);
        Memory[3][right] := OS2.WinWindowFromID (hwnd, DID.Mem3);

        Status[0] := OS2.WinWindowFromID (hwnd, DID.CalcMode);
        Status[1] := OS2.WinWindowFromID (hwnd, DID.NumBase);
        Status[2] := OS2.WinWindowFromID (hwnd, DID.CxField);
        Status[3] := OS2.WinWindowFromID (hwnd, DID.Degrees);

        SetInitialWindowPosition (hwnd, INIFileName, "MainFrame");
        LoadFontsAndColours;
        SetFonts;
        SetColours;
        AdjustScreenLayout;

        OldNumLock := Keyboard.NumLockStatus();
        Keyboard.SetNumLock (NumLockWanted);

    END SetupWindows;

(************************************************************************)
(*                            FINALISATION                              *)
(************************************************************************)

PROCEDURE SaveState;

    (* Save fonts and colours in the INI file. *)

    VAR hini: OS2.HINI;

    BEGIN

        NumLockWanted := Keyboard.NumLockStatus();
        Keyboard.SetNumLock (OldNumLock);

        hini := OpenINIFile(INIFileName);

        INIPut (hini, "Options", "NumLock", NumLockWanted);
        INIPutString (hini, "Font", "Status", StatusFontName);
        INIPutString (hini, "Font", "Memory", MemoryFontName);
        INIPutString (hini, "Font", "Acc", AccFontName);
        INIPutString (hini, "Font", "Stack", StackFontName);
        INIPutString (hini, "Font", "Menu", MenuFontName);

        INIPut (hini, "BackColour", "Frame", GeneralBackground);
        INIPut (hini, "BackColour", "Status", StatusBackColour);
        INIPut (hini, "BackColour", "Memory", MemoryBackColour);
        INIPut (hini, "BackColour", "Acc", AccBackColour);
        INIPut (hini, "BackColour", "Stack", StackBackColour);
        INIPut (hini, "BackColour", "Menu", MenuBackColour);

        INIPut (hini, "ForeColour", "Status", StatusForeColour);
        INIPut (hini, "ForeColour", "Memory", MemoryForeColour);
        INIPut (hini, "ForeColour", "Acc", AccForeColour);
        INIPut (hini, "ForeColour", "Stack", StackForeColour);
        INIPut (hini, "ForeColour", "Menu", MenuForeColour);

        CloseINIFile (hini);
        StoreWindowPosition (frame, INIFileName, "MainFrame");

    END SaveState;

(************************************************************************)

BEGIN
    CxField := FALSE;
END Display.

