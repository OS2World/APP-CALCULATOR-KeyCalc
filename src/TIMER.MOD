IMPLEMENTATION MODULE Timer;

        (********************************************************)
        (*                                                      *)
        (*                 Timed operations                     *)
        (*                                                      *)
        (*      Author:         P. Moylan                       *)
        (*      Last edited:    23 October 1996                 *)
        (*      Status:         Working                         *)
        (*                                                      *)
        (********************************************************)

FROM OS2 IMPORT
    (* proc *)  DosSleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  TimedWaitT;

(************************************************************************)
(*                      "PUT-ME-TO-SLEEP" PROCEDURE                     *)
(************************************************************************)

PROCEDURE Sleep (milliseconds: CARDINAL);

    (* Puts the caller to sleep for approximately the given number of   *)
    (* milliseconds.                                                    *)

    BEGIN
        DosSleep (milliseconds);
    END Sleep;

(************************************************************************)
(*                      SEMAPHORE WAIT WITH TIMEOUT                     *)
(************************************************************************)

PROCEDURE TimedWait (s: Semaphore;  TimeLimit: INTEGER;
                                        VAR (*OUT*) TimedOut: BOOLEAN);

    (* Like a semaphore Wait, except that it returns with TimedOut TRUE *)
    (* if the corresponding Signal does not occur within TimeLimit      *)
    (* milliseconds.                                                    *)

    BEGIN
        TimedWaitT (s, TimeLimit, TimedOut);
    END TimedWait;

(************************************************************************)

END Timer.

