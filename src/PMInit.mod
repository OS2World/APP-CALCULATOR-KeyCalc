IMPLEMENTATION MODULE PMInit;

        (****************************************************************)
        (*                                                              *)
        (*        Initialisation code for a PM application              *)
        (*                                                              *)
        (*        Started:        17 January 2002                       *)
        (*        Last edited:    17 January 2002                       *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2;

(**************************************************************************)

VAR
    hab           : OS2.HAB;      (* Main anchor block handle     *)
    hmq           : OS2.HMQ;      (* Main message queue handle    *)

(**************************************************************************)

PROCEDURE OurHab(): OS2.HAB;

    (* Returns this program's anchor block handle. *)

    BEGIN
        RETURN hab;
    END OurHab;

(**************************************************************************)

PROCEDURE [OS2.APIENTRY] MessageBox(hwndOwner : OS2.HWND
                                   ;szText    : ARRAY OF CHAR
                                   ;fsStyle   : OS2.ULONG
                                   ;fBeep     : OS2.BOOL
                                   ): OS2.ULONG;

    BEGIN

        IF fBeep THEN
           OS2.WinAlarm(OS2.HWND_DESKTOP, OS2.WA_ERROR);
        END;

        RETURN OS2.WinMessageBox (OS2.HWND_DESKTOP, hwndOwner, szText,
                                  NIL, 0, fsStyle);
    END MessageBox;

(********************************************************************************)
(*                                FINALISATION                                  *)
(********************************************************************************)

PROCEDURE ["SysCall"] ExitProc (ulTermCode : OS2.ULONG);

    BEGIN
        OS2.WinDestroyMsgQueue (hmq);
        OS2.WinTerminate (hab);
        OS2.DosExitList (OS2.EXLST_EXIT, NIL);    (* termination complete *)
    END ExitProc;

(************************************************************************)
(*                             INITIALISATION                           *)
(************************************************************************)

CONST
    BEEP_WARN_FREQ =  60; (* frequency of warning beep *)
    BEEP_WARN_DUR  = 100; (* duration of warning beep *)

BEGIN
    hab := OS2.WinInitialize(0);

    IF hab = OS2.NULLHANDLE THEN
       OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
       HALT(1);
    END;

    hmq := OS2.WinCreateMsgQueue (hab, 0);

    IF hmq = OS2.NULLHANDLE THEN
        OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
        OS2.WinTerminate(hab);
        HALT(1);
    END (*IF*);

    (* Add ExitProc to the exit list to handle the exit processing.  If *)
    (* there is an error, then terminate the process since there have   *)
    (* not been any resources allocated yet.                            *)

    IF OS2.DosExitList(OS2.EXLST_ADD, ExitProc) <> 0 THEN
        MessageBox (OS2.HWND_DESKTOP ,"Cannot load exit list",
                               OS2.MB_OK + OS2.MB_ERROR, TRUE);
        OS2.DosExit(OS2.EXIT_PROCESS, 1);
    END (*IF*);

END PMInit.
