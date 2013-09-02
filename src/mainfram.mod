IMPLEMENTATION MODULE MainFrame;

        (************************************************************)
        (*                                                          *)
        (*                      PM Calculator                       *)
        (*                  The main frame window                   *)
        (*                                                          *)
        (*    Started:        12 February 2002                      *)
        (*    Last edited:    16 March 2002                         *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT OS2, OS2RTL, DID, PMInit, Display, DoCalc, Help, Drag;

FROM SYSTEM IMPORT CAST, ADDRESS;

FROM INIData IMPORT
    (* proc *)  SetInitialWindowPosition, StoreWindowPosition;

FROM LowLevel IMPORT
    (* proc *)  IAND, EVAL;

(**************************************************************************)

TYPE
    SWList = ARRAY [1..4] OF CARDINAL;

CONST
    ID_FUNC_MENU = 300;
    ID_FUNC_2ARG = 330;
    ID_OPTIONS_BASE = 350;

    StatusWinList =
            SWList{ DID.CalcMode, DID.NumBase, DID.CxField, DID.Degrees };

VAR
    SwitchData : OS2.SWCNTRL;     (* switch entry data *)
    hwndFuncMenu: OS2.HWND;       (* function menu handle *)
    ChangeInProgress: BOOLEAN;    (* updating fonts/colours *)
    StatusWin: ARRAY [1..4] OF OS2.HWND;

(**************************************************************************)
(*                WINDOW PROCEDURE FOR SUBCLASSED CASE                    *)
(**************************************************************************)

PROCEDURE ["SysCall"] SubWindowProc (hwnd     : OS2.HWND;
                                     msg      : OS2.ULONG;
                                     mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Window procedure to intercept some of the things that happen in  *)
    (* the subwindows.  If it's something we don't want to deal with    *)
    (* here, we pass the message to the parent window procedure.        *)

    VAR OldWndProc: OS2.PFNWP;  j, k: CARDINAL;
        owner: OS2.HWND;

    BEGIN
        OldWndProc := CAST (OS2.PFNWP, OS2.WinQueryWindowPtr (hwnd, OS2.QWL_USER));
        owner := OS2.WinQueryWindow(hwnd,OS2.QW_OWNER);

        (* Because of the interaction between subclassing and DragText, *)
        (* some messages will go lost if we use the obvious strategy of *)
        (* sending them through to OldWndProc.  To get around this, we  *)
        (* have to send those messages directly to the target window.   *)

        IF (msg = OS2.WM_BUTTON2DOWN) OR (msg = OS2.DM_DRAGOVER)
                   OR (msg = OS2.DM_DRAGLEAVE) OR (msg = OS2.DM_DROP) THEN

            RETURN OS2.WinSendMsg (owner, msg, mp1, mp2);

        (* Check for a button1 click on one of the status windows. *)

        ELSIF msg = OS2.WM_BUTTON1DOWN THEN
            k := 0;
            FOR j := 1 TO 4 DO
                IF (k = 0) AND (hwnd = StatusWin[j]) THEN
                    k := j;
                END (*IF*);
            END (*FOR*);
            IF k = 0 THEN
                RETURN OldWndProc (hwnd, msg, mp1, mp2);
            ELSE
                DoCalc.ToggleStatusWindow(k);
                RETURN NIL;
            END (*IF*);

        (* Check for font or colour change. *)

        ELSIF (msg = OS2.WM_PRESPARAMCHANGED) AND NOT ChangeInProgress THEN
            ChangeInProgress := TRUE;
            Display.CheckFontChange;
            ChangeInProgress := FALSE;
            RETURN NIL;
        END (*IF*);

        RETURN OldWndProc (hwnd, msg, mp1, mp2);

    END SubWindowProc;

(**************************************************************************)
(*                       MAIN DIALOGUE PROCEDURE                          *)
(**************************************************************************)

PROCEDURE ["SysCall"] MainDialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    CONST Esc = CHR(01BH);  NSub = 22;
    TYPE IDList = ARRAY [1..NSub] OF CARDINAL;
    CONST subwinList =
            IDList{ DID.NumStk0, DID.OpStk0, DID.NumStk1, DID.OpStk1,
                    DID.NumStk2, DID.OpStk2, DID.NumStk3, DID.OpStk3,
                    DID.NumStk4, DID.OpStk4, DID.Mem0, DID.MemLabel0,
                    DID.Mem1, DID.MemLabel1, DID.Mem2, DID.MemLabel2,
                    DID.Mem3, DID.MemLabel3,
                    DID.CalcMode, DID.NumBase, DID.CxField, DID.Degrees };

    VAR ch: CHAR;  flags, code, j: CARDINAL;
        hwndCtrl: OS2.HWND;
        fSuccess: BOOLEAN;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   SetInitialWindowPosition (hwnd, "KeyCalc.INI", "MainFrame");
                   Display.SetupWindows (hwnd);
                   FOR j := 1 TO 4 DO
                       StatusWin[j] := OS2.WinWindowFromID (hwnd, StatusWinList[j]);
                   END (*FOR*);
                   FOR j := 1 TO NSub DO
                       hwndCtrl := OS2.WinWindowFromID (hwnd, subwinList[j]);
                       OS2.WinSetWindowPtr (hwndCtrl, OS2.QWL_USER,
                                   CAST(ADDRESS,OS2.WinSubclassWindow (hwndCtrl,
                                                               SubWindowProc)));
                   END (*FOR*);
                   DoCalc.Start (hwnd);
                   OS2.WinShowWindow (hwnd, TRUE);
                   ChangeInProgress := FALSE;
                   RETURN NIL;

           |  OS2.WM_HELP:
                   Help.Open;
                   RETURN NIL;

           |  OS2.WM_COMMAND:
                   IF OS2.SHORT1FROMMP(mp2) = OS2.CMDSRC_MENU THEN
                       Display.SaveMenuPresParams (hwndFuncMenu);
                       code := OS2.SHORT1FROMMP (mp1);
                       IF code > ID_OPTIONS_BASE THEN
                           DoCalc.NewChar('o');
                           DoCalc.NewChar(CHR(code - ID_OPTIONS_BASE));
                       ELSIF code >= ID_FUNC_2ARG THEN
                           DoCalc.NewChar(DoCalc.Function2Sym);
                           DoCalc.NewChar(CHR(code - ID_FUNC_2ARG));
                       ELSIF code >= ID_FUNC_MENU THEN
                           DoCalc.NewChar(DoCalc.FunctionSym);
                           DoCalc.NewChar(CHR(code - ID_FUNC_MENU));
                       ELSE
                           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                       END (*IF*);
                   ELSIF OS2.SHORT1FROMMP(mp1) = OS2.DID_CANCEL THEN
                       DoCalc.NewChar(Esc);
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);
                   RETURN NIL;

           |  OS2.WM_CHAR:
                   flags := OS2.SHORT1FROMMP (mp1);
                   IF (IAND(flags, OS2.KC_VIRTUALKEY) <> 0) AND
                                   (OS2.SHORT2FROMMP (mp2) = OS2.VK_CTRL) THEN
                       hwndFuncMenu := OS2.WinLoadMenu (hwnd, 0, ID_FUNC_MENU);
                       Display.SetMenuPresParams (hwndFuncMenu);
                       fSuccess := OS2.WinPopupMenu (hwnd,
                                             hwnd,
                                             hwndFuncMenu,
                                             20,
                                             50,
                                             0,
                                             OS2.PU_HCONSTRAIN
                                               + OS2.PU_VCONSTRAIN
                                               + OS2.PU_MOUSEBUTTON1
                                               + OS2.PU_KEYBOARD);
                       RETURN NIL;

                   ELSIF IAND(flags, OS2.KC_CHAR) <> 0 THEN
                       code := OS2.SHORT1FROMMP (mp2);
                       IF code < 256 THEN
                           ch := CHR(code);
                           IF (ch = 'o') OR (ch = DoCalc.FunctionSym)
                                          OR (ch = DoCalc.Function2Sym) THEN

                               (* Change reserved characters to something that  *)
                               (* will cause an error beep.                     *)

                               DoCalc.NewChar(CHR(255));

                           ELSE
                               DoCalc.NewChar(ch);
                           END (*IF*);
                       END (*IF*);
                       RETURN NIL;
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

           |  OS2.WM_BUTTON2DOWN:

                   hwndFuncMenu := OS2.WinLoadMenu (hwnd, 0, ID_FUNC_MENU);
                   Display.SetMenuPresParams (hwndFuncMenu);
                   fSuccess := OS2.WinPopupMenu (hwnd,
                                         hwnd,
                                         hwndFuncMenu,
                                         20,
                                         50,
                                         ID_FUNC_MENU+1,
                                         OS2.PU_HCONSTRAIN
                                          + OS2.PU_VCONSTRAIN
                                          + OS2.PU_MOUSEBUTTON1 + OS2.PU_KEYBOARD);
                   RETURN NIL;

           |  OS2.WM_PRESPARAMCHANGED:

                   IF ChangeInProgress THEN
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   ELSE
                       ChangeInProgress := TRUE;
                       Display.CheckFontChange;
                       ChangeInProgress := FALSE;
                       RETURN NIL;
                   END (*IF*);

           |  OS2.DM_DRAGOVER:

                   (* Turn on emphasis if it isn't already, then        *)
                   (* evaluate the item being dragged over this window. *)

                   Drag.DrawTargetEmphasis (hwnd, TRUE);
                   RETURN Drag.DragOver(hwnd, CAST(OS2.PDRAGINFO, mp1));

           |  OS2.DM_DRAGLEAVE:

                   (* Turn off emphasis because the mouse has left the window. *)

                   Drag.DrawTargetEmphasis( hwnd, FALSE);
                   RETURN NIL;

           |  OS2.DM_DROP:

                   (* Fetch the text and use it. *)

                   Drag.ReceiveDroppedData (hwnd, CAST(OS2.PDRAGINFO, mp1));
                   RETURN NIL;

           |  DoCalc.WM_FINISHED:
                   Display.SaveState;
                   OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);
                   RETURN NIL;

           |  OS2.WM_CLOSE:
                   DoCalc.NewChar(Esc);
                   RETURN NIL;

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END MainDialogueProc;

(**************************************************************************)

PROCEDURE OpenMainFrame;

    (* Creates the main dialogue box. *)

    VAR hwnd: OS2.HWND;
        pid: OS2.PID;  tid: OS2.TID;

    BEGIN
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, OS2.HWND_DESKTOP,
                       MainDialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.CalcWindow,      (* dialogue ID *)
                       NIL);                (* creation parameters *)

        (* Put us on the visible task list.  Note: my use of WinLoadPointer     *)
        (* is apparently incorrect, but I've run out of ideas on how to set     *)
        (* the hwndIcon field, and there are no examples in the toolkit of      *)
        (* using WinCreateSwitchEntry.                                          *)

        OS2.WinQueryWindowProcess (hwnd, pid, tid);
        SwitchData.hwnd := hwnd;
        WITH SwitchData DO
            (*hwndIcon      := OS2.WinLoadPointer (OS2.HWND_DESKTOP, 0, 99);*)
            (*hwndIcon := 0;*)

            (* WinLoadPointer doesn't work. *)
            (* Neither does WinLoadFileIcon. *)
            (* Neither does a simple 0, even though the EXE is built with *)
            (* an icon in it (via the resource compiler).                 *)

            hwndIcon      := OS2.WinLoadFileIcon ("keycalc.exe", FALSE);
            hprog         := 0;
            idProcess     := pid;
            idSession     := 0;
            uchVisibility := OS2.SWL_VISIBLE;
            fbJump        := OS2.SWL_JUMPABLE;
            szSwtitle     := "KeyCalc";
            bProgType     := OS2.PROG_PM;
        END (*WITH*);
        OS2.WinCreateSwitchEntry (PMInit.OurHab(), SwitchData);

        (* Now process the main dialogue until finished. *)

        OS2.WinProcessDlg(hwnd);
        OS2.WinDestroyWindow (hwndFuncMenu);
        OS2.WinDestroyWindow (hwnd);

    END OpenMainFrame;

(**************************************************************************)

BEGIN
    ChangeInProgress := TRUE;
END MainFrame.

