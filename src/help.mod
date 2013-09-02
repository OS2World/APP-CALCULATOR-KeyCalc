IMPLEMENTATION MODULE Help;

        (************************************************************)
        (*                                                          *)
        (*                      PM Calculator                       *)
        (*                     The help window                      *)
        (*                                                          *)
        (*    Started:        26 February 2002                      *)
        (*    Last edited:    01 March 2002                         *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT OS2, DID;

(**************************************************************************)

PROCEDURE Open;

    (* Displays the help window. *)

    VAR hwnd: OS2.HWND;

    BEGIN
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, OS2.HWND_DESKTOP,
                       OS2.WinDefDlgProc,       (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       500,                 (* dialogue ID *)
                       NIL);                (* creation parameters *)
    END Open;

(**************************************************************************)

END Help.

