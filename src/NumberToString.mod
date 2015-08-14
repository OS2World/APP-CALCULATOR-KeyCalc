IMPLEMENTATION MODULE NumberToString;

        (********************************************************)
        (*                                                      *)
        (*            Converting numeric to string              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        06 March 2002                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


FROM OurMath IMPORT
    (* const*)  PI,
    (* proc *)  atoi, RectToPolar;

(************************************************************************)
(*                         BUFFER MANIPULATION                          *)
(************************************************************************)

PROCEDURE ShiftRight (VAR (*INOUT*) buffer: ARRAY OF CHAR;
                                first, last, amount: CARDINAL);

    (* Moves the contents of buffer[first..last] right by the specified *)
    (* number of characters, space filling at the left and discarding   *)
    (* characters shifted out at the right.                             *)

    VAR j: CARDINAL;

    BEGIN
        IF amount > 0 THEN
            FOR j := last TO first+amount BY -1 DO
                buffer[j] := buffer[j-amount];
            END (*FOR*);
            FOR j := first TO first+amount-1 DO
                buffer[j] := " ";
            END (*FOR*);
        END (*IF*);
    END ShiftRight;

(************************************************************************)
(*                      CARDINAL-TO-STRING CONVERSIONS                  *)
(************************************************************************)

PROCEDURE AssembleLongCardinal (number, base: CARDINAL;
                                VAR (*OUT*) buffer: ARRAY OF CHAR;
                                VAR (*INOUT*) place: CARDINAL;
                                VAR (*OUT*) error: BOOLEAN);

    (* Converts number to base 'base', putting it in buffer starting at *)
    (* buffer[place].  On return, place has been updated to be just     *)
    (* beyond the last digit put in the buffer.                         *)

    BEGIN
        IF number >= base THEN
            AssembleLongCardinal (number DIV base, base, buffer, place, error);
            IF error THEN RETURN END(*IF*);
        END (*IF*);
        error := place > HIGH(buffer);
        IF NOT error THEN
            number := number MOD base;
            IF number > 9 THEN
                buffer[place] := CHR (number - 10 + ORD("a"));
            ELSE
                buffer[place] := CHR (number + ORD("0"));
            END (*IF*);
            INC (place);
        END (*IF*);
    END AssembleLongCardinal;

(************************************************************************)
(*                      REAL-TO-STRING CONVERSIONS                      *)
(************************************************************************)

PROCEDURE AssembleExponent (base: CARDINAL;  number: INTEGER;
                        VAR (*OUT*) buffer: ARRAY OF CHAR;
                        VAR (*INOUT*) position: CARDINAL;
                        VAR (*OUT*) error: BOOLEAN);

    (* Puts a field of the format Ennn or E-nnn into the buffer,        *)
    (* starting at buffer[position].  On return, position has been      *)
    (* updated so that buffer[position] is the first character not      *)
    (* altered by this procedure.                                       *)

    BEGIN
        error := FALSE;
        IF number <> 0 THEN
            error := position > HIGH(buffer);
            IF NOT error THEN
                IF base = 10 THEN
                    buffer[position] := "E";
                ELSE
                    buffer[position] := "G";
                END (*IF*);
                INC(position);
                IF number < 0 THEN
                    error := position > HIGH(buffer);
                    IF NOT error THEN
                        buffer[position] := "-";  INC(position);
                        number := -number;
                    END (*IF*);
                END (*IF*);
            END (*IF*);
            IF NOT error THEN
                AssembleLongCardinal (number, base, buffer, position, error);
            END (*IF*);
        END (*IF*);
    END AssembleExponent;

(************************************************************************)

PROCEDURE Roundup (VAR (*INOUT*) buffer: ARRAY OF CHAR;
                                        first, last, base: CARDINAL);

    (* Takes the number in buffer[first..last] and increments           *)
    (* its least significant digit, propagating the carry upwards as    *)
    (* far as necessary.                                                *)

    VAR position, pointposition: CARDINAL;  maxdigit: CHAR;
        code: CHAR;

    BEGIN
        IF base <= 10 THEN
            maxdigit := CHR(ORD('0') + base - 1);
        ELSE
            maxdigit := CHR(ORD('a') + base - 11);
        END (*IF*);
        position := last+1;  pointposition := position;
        REPEAT
            DEC (position);
            code := buffer[position];
            IF code = maxdigit THEN buffer[position] := "0"
            ELSIF code = "." THEN
                pointposition := position;  code := maxdigit;
            ELSIF code = "9" THEN
                buffer[position] := 'a';
            ELSE
                INC (buffer[position]);
            END (*IF*);
        UNTIL (code <> maxdigit) OR (position = first);

        (* The job is now done, except for one special case.  If we     *)
        (* have left the above loop after incrementing a maxdigit, the  *)
        (* carry has propagated off the left end of the number.  In     *)
        (* that case every digit must have been a maxdigit, so the      *)
        (* result is 10000... with a decimal point inserted at the      *)
        (* appropriate place.                                           *)

        IF code = maxdigit THEN
            IF pointposition <= last THEN
                buffer[pointposition] := "0";
                IF pointposition < last THEN
                    INC (pointposition);  buffer[pointposition] := ".";
                END (*IF*);
            END (*IF*);
            buffer[first] := "1";
        END (*IF*);

    END Roundup;

(************************************************************************)

PROCEDURE Fformat (number: LONGREAL;  base: CARDINAL;
                        VAR (*OUT*) buffer: ARRAY OF CHAR;
                        start: CARDINAL;  VAR (*INOUT*) finish: CARDINAL;
                        LeftJustified: BOOLEAN;  VAR (*OUT*) error: BOOLEAN);

    (* Formats the second argument as a decimal number, left or right   *)
    (* justified depending on the value of LeftJustified, in            *)
    (* buffer[start..finish].  This procedure is known to be called     *)
    (* only with start=0 or start=1 with a sign in buffer[0]; so we     *)
    (* perform the justification on all of buffer[0..finish] if right   *)
    (* justification is specified.  In the case of left justification,  *)
    (* finish is updated to show the last buffer position actually      *)
    (* used; and this character position is followed by one or more NUL *)
    (* characters, except in the case where we have used the entire     *)
    (* field to hold the result.                                        *)

    VAR position: CARDINAL;
        integerpart: CARDINAL;  nextdigit: CARDINAL;
        RBase: LONGREAL;

    BEGIN
        position := start;
        integerpart := VAL (CARDINAL, number);
        AssembleLongCardinal (integerpart, base, buffer, position, error);
        IF error THEN RETURN END(*IF*);

        IF position <= finish THEN
            buffer[position] := ".";
            INC (position);
            number := number - VAL (LONGREAL, integerpart);
            RBase := VAL (LONGREAL, base);

            WHILE (position <= finish) DO
                number := RBase*number;
                nextdigit := VAL (CARDINAL, number);
                IF nextdigit > 9 THEN
                    buffer[position] := CHR(ORD("a") + nextdigit - 10);
                ELSE
                    buffer[position] := CHR(ORD("0") + nextdigit);
                END (*IF*);
                INC (position);
                number := number - VAL (LONGREAL, nextdigit);
            END (*WHILE*);

            (* If the remainder is 0.5 or more, adjust the result by    *)
            (* rounding up.                                             *)

            IF number >= 0.5 THEN
                Roundup (buffer, start, finish, base);
            END (*IF*);

            (* Strip off the trailing zeros.    *)

            DEC (position);
            WHILE buffer[position] = '0' DO
                buffer[position] := CHR(0);
                DEC (position);
            END (*WHILE*);

            (* If we are left with a whole number, strip off the        *)
            (* decimal point.                                           *)

            IF buffer[position] = '.' THEN
                buffer[position] := CHR(0);
                DEC (position);
            END (*IF*);

            (* Right justify the result or modify finish, as specified. *)

            IF LeftJustified THEN
                finish := position;
            ELSE
                ShiftRight (buffer, 0, finish, finish-position);
            END (*IF*);

        END (*IF*);

    END Fformat;

(************************************************************************)

PROCEDURE Scale (VAR (*INOUT*) mantissa: LONGREAL;
                        VAR (*INOUT*) exponent: INTEGER;
                        power: CARDINAL;  lower, upper: LONGREAL);

    (* Adjusts mantissa so that lower <= mantissa < upper, while        *)
    (* keeping the quantity  (mantissa * base^exponent) invariant.  To  *)
    (* save us some calculation, the caller must ensure that            *)
    (* upper = base^power and lower = base^(-power).                        *)

    BEGIN
        WHILE mantissa >= upper DO
            INC (exponent, power);  mantissa := lower*mantissa;
        END (*WHILE*);

        WHILE mantissa < lower DO
            DEC (exponent, power);  mantissa := upper*mantissa;
        END (*WHILE*);
    END Scale;

(************************************************************************)

PROCEDURE Separate (base: CARDINAL;  number: LONGREAL;
                            VAR (*OUT*) mantissa: LONGREAL;
                            VAR (*OUT*) exponent: INTEGER);

    (* Separates the first argument into a mantissa and exponent part,  *)
    (* so that  number = mantissa * base^exponent.                     *)

    VAR N: CARDINAL;  upper, lower, Rbase: LONGREAL;

    BEGIN
        Rbase := VAL(LONGREAL, base);
        mantissa := number;  exponent := 0;
        IF base >= 16 THEN N := 128
        ELSIF base >= 4 THEN N := 256;
        ELSE N := 512;
        END (*IF*);
        REPEAT
            upper := atoi (Rbase, N);  lower := 1.0 / upper;
            Scale (mantissa, exponent, N, lower, upper);
            N := N DIV 4;
        UNTIL N = 0;
    END Separate;

(************************************************************************)

PROCEDURE Eformat (number: LONGREAL;  base: CARDINAL;
                                       VAR (*OUT*) buffer: ARRAY OF CHAR;
                                        start, finish: CARDINAL;
                                        VAR (*OUT*) error: BOOLEAN);

    (* Puts number into buffer[start..finish] in E format, with the     *)
    (* whole of buffer[0..finish] right justified.                      *)

    VAR mantissa: LONGREAL;  exponent: INTEGER;
        position: CARDINAL;

    BEGIN
        Separate (base, number, mantissa, exponent);

        (* Put the exponent into the buffer first, in order to find out *)
        (* how much space will be left for the mantissa.                *)

        position := start;
        AssembleExponent (base, exponent, buffer, position, error);
        error := error OR (position > finish);

        IF error THEN
            IF finish < HIGH(buffer) THEN
                buffer[finish+1] := CHR(0);
            END (*IF*);
        ELSE
            ShiftRight (buffer, start, finish, finish-position+1);

            (* Now assemble the mantissa into the buffer.       *)

            DEC (finish, position-start);
            Fformat (mantissa, base, buffer, start, finish, FALSE, error);
        END (*IF*);

    END Eformat;

(************************************************************************)
(*              CONVERSION OF REAL NUMBER TO CHARACTER STRING           *)
(************************************************************************)

PROCEDURE LongRealToString (number: LONGREAL;  base: CARDINAL;
                                        VAR (*OUT*) buffer: ARRAY OF CHAR;
                                        fieldsize: CARDINAL);

    (* Converts the number to a character string in array "buffer",     *)
    (* right-justified in a field of "fieldsize" characters.            *)

    VAR start, finish, j: CARDINAL;  RBase, small: LONGREAL;  error: BOOLEAN;

    BEGIN
        IF fieldsize = 0 THEN RETURN END(*IF*);

        start := 0;  finish := fieldsize-1;  error := FALSE;

        (* Make sure that the string will fit into the buffer, and that *)
        (* it will be properly terminated.                              *)

        IF finish > HIGH(buffer) THEN
            DEC (fieldsize, finish-HIGH(buffer));
            finish := HIGH(buffer);
        ELSIF finish < HIGH(buffer) THEN
            buffer[finish+1] := CHR(0);
        END (*IF*);

        (* For a negative number, insert a minus sign.  *)

        IF number < 0.0 THEN
            IF fieldsize <= 1 THEN
                error := TRUE;
            ELSE
                buffer[0] := "-";  start := 1;  DEC(fieldsize);
                number := -number;
            END (*IF*);
        END (*IF*);

        IF NOT error THEN

            (* Now decide on whether to use E format, based on the      *)
            (* value to be converted.                                   *)

            RBase := VAL(LONGREAL, base);
            small := 1.0 / atoi(RBase, fieldsize-2);

            IF number = 0.0 THEN
                Fformat (number, base, buffer, start, finish, FALSE, error);
            ELSIF (number >= atoi (RBase, fieldsize))
                    OR (number > VAL(LONGREAL, MAX(CARDINAL)))
                        OR (number < small) THEN
                Eformat (number, base, buffer, start, finish, error);
            ELSE
                Fformat (number, base, buffer, start, finish, FALSE, error);
            END (*IF*);

        END (*IF*);

        IF error THEN
            FOR j := 0 TO finish DO
                buffer[j] := '*';
            END (*FOR*);
        END (*IF*);

    END LongRealToString;

(************************************************************************)

PROCEDURE LongComplexToString (number: LONGCOMPLEX;  polar, degrees: BOOLEAN;
                                        base: CARDINAL;
                                        VAR (*OUT*) buffer: ARRAY OF CHAR;
                                        fieldsize: CARDINAL);

    (* Converts the number to a character string in array "buffer",     *)
    (* right-justified in a field of "2*fieldsize+3" characters.  This  *)
    (* allows fieldsize characters for each of the real and imaginary   *)
    (* parts, plus 3 characters for the separator.                      *)

    VAR ImBuffer: ARRAY [0..1024] OF CHAR;
        i0, realspace, spare: CARDINAL;
        number1, number2: LONGREAL;

    BEGIN
        number1 := RE(number);  number2 := IM(number);
        IF polar THEN
            RectToPolar (number1, number2);
            IF degrees THEN
                number2 := 180.0*number2/PI;
            END (*IF*);
        END (*IF*);

        LongRealToString (number2, base, ImBuffer, fieldsize);

        (* Skip over the leading spaces in the number2 part. *)

        i0 := 0;
        WHILE ImBuffer[i0] = ' ' DO
           INC (i0);
        END (*WHILE*);

        (* If the number2 part is the single digit 0, get rid of it   *)
        (* entirely.                                                  *)

        IF (ImBuffer[i0] = '0') AND (i0 = fieldsize-1) THEN
            INC (i0, 4);
        END (*IF*);

        spare := i0 DIV 2;
        realspace := fieldsize + i0 - spare;
        LongRealToString (number1, base, buffer, realspace);
        ShiftRight (buffer, 0, fieldsize+i0-1, spare);
        INC (realspace, spare);

        (* Put in the separator and the second part. *)

        IF i0 < fieldsize THEN
            buffer[realspace] := ' ';  INC(realspace);
            IF polar THEN
                buffer[realspace] := '<';
            ELSE
                buffer[realspace] := ',';
            END (*IF*);
            INC(realspace);
            buffer[realspace] := ' ';  INC(realspace);
            WHILE i0 < fieldsize DO
                buffer[realspace] := ImBuffer[i0];
                INC(i0);  INC(realspace);
            END (*WHILE*);
        END (*IF*);

        (* If space permits, terminate with a Nul. *)

        IF realspace <= HIGH(buffer) THEN
            buffer[realspace] := CHR(0);
        END (*IF*);

    END LongComplexToString;

(************************************************************************)

END NumberToString.

