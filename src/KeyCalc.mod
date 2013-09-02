MODULE KeyCalc;

        (************************************************************)
        (*                                                          *)
        (*                      PM Calculator                       *)
        (*                                                          *)
        (*    Started:        12 February 2002                      *)
        (*    Last edited:    12 February 2002                      *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT OS2, OS2RTL, MainFrame;

FROM PMInit IMPORT
    (* proc *)  OurHab;

(********************************************************************************)

VAR hab: OS2.HAB;            (* anchor block handle *)
    qmsg: OS2.QMSG;          (* message structure *)

(********************************************************************************)
(*              MAIN PROGRAM: INITIALISATION AND MESSAGE DISPATCHING            *)
(********************************************************************************)

BEGIN
    hab := OurHab();

    (* NOTE:  clean up from here is handled by the DosExitList processing *)
    (* Since signal exceptions are not handled by RTS yet, using module   *)
    (* finalization for clean up is incorrect. This will be changed in the*)
    (* next release.                                                      *)

    MainFrame.OpenMainFrame;

    (* Get/Dispatch Message loop *)

    WHILE OS2.WinGetMsg (hab, qmsg, 0, 0, 0) DO
        OS2.WinDispatchMsg (hab, qmsg);
    END;

END KeyCalc.
