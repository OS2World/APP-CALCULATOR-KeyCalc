IMPLEMENTATION MODULE OurMath;

    (********************************************************)
    (*                                                      *)
    (*           Mathematical functions for the             *)
    (*           types LONGREAL and LONGCOMPLEX             *)
    (*                                                      *)
    (*     Author:         Peter Moylan                     *)
    (*     Started:        16 February 2002                 *)
    (*     Last edited:    12 March 2002                    *)
    (*     Status:         OK                               *)
    (*                                                      *)
    (********************************************************)


IMPORT LongMath, LongComplexMath;

FROM SYSTEM IMPORT
    (* proc *)  CAST,
    (* type *)  CARD8;

FROM LowLevel IMPORT
    (* proc *)  IXOR;

(****************************************************************)
(*                REAL TO INTEGER CONVERSIONS                   *)
(****************************************************************)

PROCEDURE ITRUNC (val: LONGREAL): INTEGER;

    (* Truncate to signed integer part, with the result         *)
    (* saturating in the case of integer overflow.              *)

    BEGIN
        IF val >= MAX(INTEGER) THEN
            RETURN MAX(INTEGER);
        ELSIF val <= MIN(INTEGER) THEN
            RETURN MIN(INTEGER);
        ELSIF val < 0.0 THEN
            RETURN -VAL(INTEGER,-val);
        ELSE
            RETURN VAL(INTEGER,val);
        END (*IF*);
    END ITRUNC;

(****************************************************************)

(*
PROCEDURE UTRUNC (val: LONGREAL): CARDINAL;

    (* Truncate to unsigned integer part, with the result       *)
    (* saturating in the case of integer overflow.              *)

    VAR y: INTEGER;

    BEGIN
        y := ITRUNC(val);
        IF y >= 0 THEN
            RETURN y;
        ELSE
            RETURN MAX(CARDINAL) - (VAL(CARDINAL,-y) - 1);
        END (*IF*);
    END UTRUNC;
*)

(****************************************************************)

PROCEDURE reTrunc (val: LONGREAL): LONGREAL;

    (* Like ITRUNC, except for the result type. *)

    BEGIN
        RETURN VAL (LONGREAL, ITRUNC(val));
    END reTrunc;

(****************************************************************)

PROCEDURE cxTrunc (val: LONGCOMPLEX): LONGCOMPLEX;

    (* Like ITRUNC, except for the result type. *)

    BEGIN
        RETURN CMPLX (VAL (LONGREAL, ITRUNC(RE(val))),
                        VAL (LONGREAL, ITRUNC(IM(val))));
    END cxTrunc;

(****************************************************************)

PROCEDURE Round (val: LONGREAL): INTEGER;

    (* Same as ITRUNC, except that we round rather than truncate. *)

    BEGIN
        IF val >= MAX(INTEGER) THEN
            RETURN MAX(INTEGER);
        ELSIF val <= MIN(INTEGER) THEN
            RETURN MIN(INTEGER);
        ELSIF val < 0.0 THEN
            RETURN -VAL(INTEGER,-val+0.5);
        ELSE
            RETURN VAL(INTEGER,val+0.5);
        END (*IF*);
    END Round;

(****************************************************************)

PROCEDURE URound (val: LONGREAL): CARDINAL;

    (* Same as UTRUNC, except that we round rather than truncate. *)

    VAR y: INTEGER;

    BEGIN
        y := Round(val);
        IF y >= 0 THEN
            RETURN y;
        ELSIF y = MIN(INTEGER) THEN
            RETURN VAL(CARDINAL, MAX(INTEGER)) + 1;
        ELSE
            RETURN MAX(CARDINAL) - (VAL(CARDINAL,-y) - 1);
        END (*IF*);
    END URound;

(****************************************************************)

PROCEDURE reRound (val: LONGREAL): LONGREAL;

    (* Like Round, except for the result type. *)

    BEGIN
        RETURN VAL (LONGREAL, Round(val));
    END reRound;

(****************************************************************)

PROCEDURE cxRound (val: LONGCOMPLEX): LONGCOMPLEX;

    (* Like Round, except for the result type. *)

    BEGIN
        RETURN CMPLX (VAL (LONGREAL, Round(RE(val))),
                        VAL (LONGREAL, Round(IM(val))));
    END cxRound;

(****************************************************************)
(*                        MISCELLANEOUS                         *)
(****************************************************************)

PROCEDURE Negative (x: LONGREAL): LONGREAL;

    (* Unary minus. *)

    BEGIN
        RETURN -x;
    END Negative;

(****************************************************************)

PROCEDURE cxNegative (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Unary minus. *)

    BEGIN
        RETURN -x;
    END cxNegative;

(****************************************************************)

PROCEDURE BoolNOT (x: LONGREAL): LONGREAL;

    (* Boolean NOT of integer part. *)

    BEGIN
        RETURN VAL(LONGREAL, MAX(CARDINAL) - URound(x));
    END BoolNOT;

(****************************************************************)

PROCEDURE cxBoolNOT (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Boolean NOT of integer part. *)

    BEGIN
        RETURN CMPLX (BoolNOT(RE(x)), BoolNOT(IM(x)));
    END cxBoolNOT;

(****************************************************************)
(*                 REAL NUMBER TO CARDINAL POWER                *)
(****************************************************************)

PROCEDURE atoi (a: LONGREAL;  i: CARDINAL): LONGREAL;

    (* Calculates a**i, for unsigned integer i. *)

    VAR result: LONGREAL;

    BEGIN
        result := 1.0;

        (* Desired answer is result*(a)**i.  The loop below     *)
        (* keeps this quantity invariant while reducing i down  *)
        (* to zero.                                             *)

        LOOP
            IF ODD(i) THEN
                DEC(i);  result := a*result;
            END (*IF*);
            IF i=0 THEN EXIT(*LOOP*) END(*IF*);
            i := i DIV 2;  a := a*a;
        END (*LOOP*);
        RETURN result;
    END atoi;

(****************************************************************)
(*                   POWERS/EXPONENTIATION                      *)
(****************************************************************)

PROCEDURE power (x, y: LONGREAL): LONGREAL;

    (* x to the power y *)

    BEGIN
        RETURN LongMath.power(x, y);
    END power;

(****************************************************************)

PROCEDURE cxpower (x, y: LONGCOMPLEX): LONGCOMPLEX;

    (* x to the power y *)

    BEGIN
        RETURN LongComplexMath.power(x, RE(y))
                   * CMPLX (cos(IM(y)), sin(IM(y)));
    END cxpower;

(****************************************************************)

PROCEDURE sqrt (x: LONGREAL): LONGREAL;

    (* Square root. *)

    VAR y: LONGREAL;

    BEGIN
        y := LongMath.sqrt(x);

        (* For some reason the precision is not very good, so   *)
        (* do one step of iterative improvement.                *)

        y := 0.5*(y + x/y);
        RETURN y;

    END sqrt;

(****************************************************************)

PROCEDURE cxsqrt (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Square root. *)

    VAR y: LONGCOMPLEX;

    BEGIN
        y := LongComplexMath.sqrt(x);

        (* For some reason the precision is not very good, so   *)
        (* do one step of iterative improvement.                *)

        y := 0.5*(y + x/y);
        RETURN y;

    END cxsqrt;

(****************************************************************)

PROCEDURE exp (x: LONGREAL): LONGREAL;

    (* Exponential. *)

    BEGIN
        RETURN LongMath.exp(x);
    END exp;

(****************************************************************)

PROCEDURE cxexp (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Exponential. *)

    BEGIN
        RETURN LongComplexMath.exp(x);
    END cxexp;

(****************************************************************)

PROCEDURE ln (x: LONGREAL): LONGREAL;

    (* Natural logarithm. *)

    BEGIN
        IF x = 0.0 THEN
            RETURN MIN(LONGREAL);
        ELSE
            RETURN LongMath.ln(x);
        END (*IF*);
    END ln;

(****************************************************************)

PROCEDURE cxln (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Natural logarithm. *)

    BEGIN
        IF x = CMPLX(0.0,0.0) THEN
            RETURN CMPLX(MIN(LONGREAL), MIN(LONGREAL));
        ELSE
            RETURN LongComplexMath.ln(x);
        END (*IF*);
    END cxln;

(****************************************************************)

PROCEDURE log10 (x: LONGREAL): LONGREAL;

    (* Logarithm, base 10. *)

    BEGIN
        RETURN LongMath.ln(x)/LongMath.ln(10.0);
    END log10;

(****************************************************************)

PROCEDURE cxlog10 (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Logarithm, base 10. *)

    BEGIN
        RETURN LongComplexMath.ln(x)/LongComplexMath.ln(CMPLX(10.0, 0.0));
    END cxlog10;

(****************************************************************)
(*                  TRIGONOMETRIC FUNCTIONS                     *)
(****************************************************************)

PROCEDURE sin (x: LONGREAL): LONGREAL;

    (* Sine. *)

    BEGIN
        RETURN LongMath.sin(x);
    END sin;

(****************************************************************)

PROCEDURE cxsin (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Sine. *)

    BEGIN
        RETURN LongComplexMath.sin(x);
    END cxsin;

(****************************************************************)

PROCEDURE cos (x: LONGREAL): LONGREAL;

    (* Cosine. *)

    BEGIN
        RETURN LongMath.cos(x);
    END cos;

(****************************************************************)

PROCEDURE cxcos (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Cosine. *)

    BEGIN
        RETURN LongComplexMath.cos(x);
    END cxcos;

(****************************************************************)

PROCEDURE tan (x: LONGREAL): LONGREAL;

    (* Tangent. *)

    BEGIN
        RETURN LongMath.tan(x);
    END tan;

(****************************************************************)

PROCEDURE cxtan (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Tangent. *)

    BEGIN
        RETURN LongComplexMath.tan(x);
    END cxtan;

(****************************************************************)

PROCEDURE arcsin (x: LONGREAL): LONGREAL;

    (* Inverse sine. *)

    BEGIN
        RETURN LongMath.arcsin(x);
    END arcsin;

(****************************************************************)

PROCEDURE cxarcsin (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Inverse sine. *)

    BEGIN
        RETURN LongComplexMath.arcsin(x);
    END cxarcsin;

(****************************************************************)

PROCEDURE arccos (x: LONGREAL): LONGREAL;

    (* Inverse cosine. *)

    BEGIN
        RETURN LongMath.arccos(x);
    END arccos;

(****************************************************************)

PROCEDURE cxarccos (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Inverse cosine. *)

    BEGIN
        RETURN LongComplexMath.arccos(x);
    END cxarccos;

(****************************************************************)

PROCEDURE arctan (x: LONGREAL): LONGREAL;

    (* Inverse tangent. *)

    BEGIN
        RETURN LongMath.arctan(x);
    END arctan;

(****************************************************************)

PROCEDURE cxarctan (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Inverse tangent. *)

    BEGIN
        RETURN LongComplexMath.arctan(x);
    END cxarctan;

(****************************************************************)
(*                   HYPERBOLIC FUNCTIONS                       *)
(****************************************************************)

PROCEDURE sinh (x: LONGREAL): LONGREAL;

    (* Returns the hyperbolic sine of x *)

    BEGIN
        RETURN 0.5*(LongMath.exp(x) - LongMath.exp(-x));
    END sinh;

(****************************************************************)

PROCEDURE cxsinh (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Returns the hyperbolic sine of x *)

    BEGIN
        RETURN 0.5*(LongComplexMath.exp(x) - LongComplexMath.exp(-x));
    END cxsinh;

(****************************************************************)

PROCEDURE cosh (x: LONGREAL): LONGREAL;

    (* Returns the hyperbolic cosine of x *)

    BEGIN
        RETURN 0.5*(LongMath.exp(x) + LongMath.exp(-x));
    END cosh;

(****************************************************************)

PROCEDURE cxcosh (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Returns the hyperbolic cosine of x *)

    BEGIN
        RETURN 0.5*(LongComplexMath.exp(x) + LongComplexMath.exp(-x));
    END cxcosh;

(****************************************************************)

PROCEDURE tanh (x: LONGREAL): LONGREAL;

    (* Returns the hyperbolic tangent of x *)

    VAR v1, v2: LONGREAL;

    BEGIN
        v1 := LongMath.exp(x);
        v2 := LongMath.exp(-x);
        RETURN (v1 - v2) / (v1 + v2);
    END tanh;

(****************************************************************)

PROCEDURE cxtanh (x: LONGCOMPLEX): LONGCOMPLEX;

    (* Returns the hyperbolic tangent of x *)

    VAR v1, v2: LONGCOMPLEX;

    BEGIN
        v1 := LongComplexMath.exp(x);
        v2 := LongComplexMath.exp(-x);
        RETURN (v1 - v2) / (v1 + v2);
    END cxtanh;

(****************************************************************)
(*                   TWO-VARIABLE FUNCTIONS                     *)
(****************************************************************)

PROCEDURE rexor (VAR (*INOUT*) y: LONGREAL;  VAR (*IN*) x: LONGREAL);

    (* Exclusive OR of integer part.  The result is left in y,  *)
    (* and x is unchanged.                                      *)

    BEGIN
        y := VAL(LONGREAL, IXOR (URound(x), URound(y)));
    END rexor;

(****************************************************************)

PROCEDURE cxxor (VAR (*INOUT*) y: LONGCOMPLEX;
                        VAR (*IN*) x: LONGCOMPLEX);

    (* Exclusive OR of integer part.  The result is left in y,  *)
    (* and x is unchanged.                                      *)

    BEGIN
        y := CMPLX(
               VAL(LONGREAL, IXOR (URound(RE(x)), URound(RE(y)))),
               VAL(LONGREAL, IXOR (URound(IM(x)), URound(IM(y)))));
    END cxxor;

(****************************************************************)

PROCEDURE cxDummy (VAR y, x: LONGCOMPLEX);

    (* A do-nothing function. *)

    BEGIN
    END cxDummy;

(****************************************************************)
(*                  RECTANGULAR TO/FROM POLAR                   *)
(****************************************************************)

PROCEDURE RectToPolar (VAR (*INOUT*) number1, number2: LONGREAL);

    (* On entry, number1 and number2 are the real and imaginary *)
    (* parts, respectively, of a complex number.  On exit they  *)
    (* are the magnitude and phase.                             *)

    VAR temp: LONGREAL;

    BEGIN
        temp := sqrt (number1*number1 + number2*number2);
        IF number1 = 0.0 THEN
            IF number2 = 0.0 THEN
                number2 := 0.0;
            ELSIF number2 > 0.0 THEN
                number2 := 0.5*PI;
            ELSE
                number2 := -0.5*PI;
            END (*IF*);
        ELSE
            number2 := arctan (number2/number1);
        END (*IF*);
        IF number1 < 0.0 THEN
            number2 := number2 + PI;
        END (*IF*);
        number1 := temp;
    END RectToPolar;

(****************************************************************)

PROCEDURE PolarToRect (VAR (*INOUT*) number1, number2: LONGREAL);

    (* On entry, number1 and number2 are the magnitude and      *)
    (* phase, respectively, of a complex number.  On exit they  *)
    (* are the real and imaginary parts.                        *)

    VAR temp: LONGREAL;

    BEGIN
        temp := number1 * cos(number2);
        number2 := number1 * sin(number2);
        number1 := temp;
    END PolarToRect;

(****************************************************************)

TYPE LOC8 = ARRAY [0..7] OF CARD8;

BEGIN
    PI := CAST(LONGREAL, LOC8 {18H,2DH,44H,54H,0FBH,21H,09H,40H });
END OurMath.

