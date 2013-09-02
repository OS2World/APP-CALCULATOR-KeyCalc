IMPLEMENTATION MODULE Keyboard;

        (****************************************************************)
        (*                                                              *)
        (*      Keyboard status routines for the calculator program     *)
        (*                                                              *)
        (*  Programmer:         P. Moylan                               *)
        (*  Last edited:        7 March 2002                            *)
        (*  Status:             Working                                 *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD8,
    (* proc *)  ADR;

IMPORT OS2;

FROM LowLevel IMPORT
    (* proc *)  IAND, IOR, IANDB, IORB;

(************************************************************************)

CONST NumLock = 32;

(************************************************************************)

PROCEDURE SetNumLock (set: BOOLEAN);

    (* Set/clear the num lock condition.   *)

    VAR KeyState: ARRAY [0..255] OF CARD8;
        code: CARDINAL;

    (********************************************************************)

    PROCEDURE UpdateKeyState (VirtualKey: CARD8);

        (* Sets or clears the low-order bit of KeyState[VirtualKey],    *)
        (* depending on the value of code.                              *)

        BEGIN
            IF set THEN
                KeyState[VirtualKey] := IORB (KeyState[VirtualKey], 1);
            ELSE
                KeyState[VirtualKey] := IANDB (KeyState[VirtualKey], 0FEH);
            END (*IF*);
        END UpdateKeyState;

    (********************************************************************)

    CONST LockStatusMask = MAX(CARDINAL) - NumLock;

    VAR Action, Length: CARDINAL;
        ss: OS2.SHIFTSTATE;
        handle: OS2.HFILE;

    BEGIN
        IF set THEN
            code := NumLock;
        ELSE
            code := 0;
        END (*IF*);

        OS2.DosOpen("KBD$", handle, Action, 0, 0, OS2.FILE_OPEN,
               OS2.OPEN_ACCESS_READONLY + OS2.OPEN_SHARE_DENYNONE, NIL);
        OS2.DosDevIOCtl(handle, OS2.IOCTL_KEYBOARD, OS2.KBD_GETSHIFTSTATE,
                   NIL, 0, Action, ADR(ss), SIZE(ss), Length);
        ss.fsState := IOR (code, IAND (ss.fsState, LockStatusMask));
        Action := SIZE(ss);
        Length := 0;
        OS2.DosDevIOCtl(handle, OS2.IOCTL_KEYBOARD, OS2.KBD_SETSHIFTSTATE,
                   ADR(ss), SIZE(ss), Action, NIL, 0, Length);

        (* The following code handles the case of a windowed VIO        *)
        (* or PM session.                                               *)

        OS2.WinSetKeyboardStateTable (OS2.HWND_DESKTOP, KeyState, FALSE);
        UpdateKeyState (OS2.VK_NUMLOCK);
        OS2.WinSetKeyboardStateTable (OS2.HWND_DESKTOP, KeyState, TRUE);

        OS2.DosClose(handle);

    END SetNumLock;

(************************************************************************)

PROCEDURE NumLockStatus (): BOOLEAN;

    (* Returns the current state of the num lock condition.  *)

    CONST LockStatusMask = NumLock;

    VAR Action, Length, result: CARDINAL;
        ss: OS2.SHIFTSTATE;
        handle: OS2.HFILE;

    BEGIN
        OS2.DosOpen("KBD$", handle, Action, 0, 0, OS2.FILE_OPEN,
               OS2.OPEN_ACCESS_READONLY + OS2.OPEN_SHARE_DENYNONE,
               NIL);
        Action := 0;
        Length := SIZE(ss);
        OS2.DosDevIOCtl(handle, OS2.IOCTL_KEYBOARD, OS2.KBD_GETSHIFTSTATE,
                   NIL, 0, Action, ADR(ss), SIZE(ss), Length);
        result := IAND (ss.fsState, NumLock);
        OS2.DosClose(handle);
        RETURN result <> 0;
    END NumLockStatus;

(************************************************************************)

END Keyboard.

