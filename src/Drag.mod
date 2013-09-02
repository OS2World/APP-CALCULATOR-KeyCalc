IMPLEMENTATION MODULE Drag;

      (************************************************************)
      (*                                                          *)
      (*                      PM Calculator                       *)
      (*                                                          *)
      (*    Module to deal with strings that are dropped          *)
      (*    onto the calculator by DragText.  This is based on    *)
      (*    sample code supplied by Rich Walsh (thanks, Rich!),   *)
      (*    the author of DragText.                               *)
      (*                                                          *)
      (*    Started:        15 March 2002                         *)
      (*    Last edited:    16 March 2002                         *)
      (*    Status:         Working                               *)
      (*                                                          *)
      (************************************************************)


FROM LowLevel IMPORT LowWord;
IMPORT OS2, DoCalc;

(************************************************************************)

VAR
    (* Flag to keep track of whether target emphasis has been drawn. *)

    EmphasisIsOn: BOOLEAN;

(************************************************************************)
(*                    RESPONDING TO DRAG MESSAGES                       *)
(************************************************************************)

PROCEDURE DragOver (hwnd: OS2.HWND;  pdinfo: OS2.PDRAGINFO): OS2.MRESULT;

    (* Called when we see a DM_DRAGOVER window message. We are  *)
    (* supposed to say whether we can accept the data.          *)

    VAR pdi: OS2.PDRAGITEM;
        mRtn: OS2.MRESULT;
        mechanism, format: ARRAY [0..8] OF CHAR;

    BEGIN
        mRtn := OS2.MRFROM2SHORT (OS2.DOR_NEVERDROP, 0);

        (* Gain access to the draginfo structure. *)

        IF OS2.DrgAccessDraginfo (pdinfo) THEN

            (* If we can get a ptr to the dragitem struct and the   *)
            (* rendering method/format is OK, and there is          *)
            (* something in the item ID, indicate that we can       *)
            (* accept the drop.                                     *)

            pdi := OS2.DrgQueryDragitemPtr (pdinfo, 0);
            mechanism := "DRM_ATOM";
            format := "DRF_TEXT";

            IF (pdi <> NIL) AND OS2.DrgVerifyRMF (pdi^, mechanism, format)
                            AND (LowWord (pdi^.ulItemID) <> 0) THEN
                mRtn := OS2.MRFROM2SHORT(OS2.DOR_DROP, OS2.DO_COPY);
            END (*IF*);

            (* Free the draginfo structure. *)

            OS2.DrgFreeDraginfo (pdinfo);

        END (*IF*);

        RETURN mRtn;

    END DragOver;

(************************************************************************)

PROCEDURE Drop (pdinfo: OS2.PDRAGINFO;
                VAR (*OUT*) Text: ARRAY OF CHAR): CARDINAL;

    (* Here is where we accept the dropped data.  If we can receive a   *)
    (* character string, we return the string in parameter Text and     *)
    (* return its length as the function result.                        *)

    VAR length: CARDINAL;  pdi: OS2.PDRAGITEM;
        mechanism, format: ARRAY [0..8] OF CHAR;

    BEGIN
        length := 0;

        (* If we have nil pointers or can't gain access to the  *)
        (* draginfo structure, exit now.                        *)

        IF (pdinfo = NIL) OR NOT OS2.DrgAccessDraginfo(pdinfo) THEN
            RETURN 0;
        END (*IF*);

        (* If we can get a pointer to the dragitem struct, and the      *)
        (* rendering method/format are things we can handle, and        *)
        (* there's something in the item ID, retrieve the text.         *)

        pdi := OS2.DrgQueryDragitemPtr (pdinfo, 0);
        mechanism := "DRM_ATOM";
        format := "DRF_TEXT";
        IF (pdi <> NIL) AND OS2.DrgVerifyRMF (pdi^, mechanism, format)
                    AND (LowWord (pdi^.ulItemID) <> 0) THEN

            (* The dragged text is apparently in the ulItemID field.    *)

            length := OS2.DrgQueryStrName (pdi^.ulItemID, 256, Text);

        END (*IF*);

        (* As long as we can identify the source window, we have to     *)
        (* tell it we're done.  Regardless of outcome, tell it we       *)
        (* succeeded.                                                   *)

        IF pdi <> NIL THEN
            OS2.DrgSendTransferMsg (pdi^.hwndItem, OS2.DM_ENDCONVERSATION,
                                    OS2.MPFROMULONG(pdi^.ulItemID),
                                    OS2.MPFROMSHORT(OS2.DMFL_TARGETSUCCESSFUL));
        END (*IF*);

        (* The target of a successful drop has to destroy all atoms.    *)
        (* Destroy the text-bearing one as long as it doesn't match     *)
        (* the name proposed for a new DTFile.  Note:  a non-zero       *)
        (* length implies that both pdi and the atom are valid.         *)

        IF (length <> 0) AND (pdi^.ulItemID <> pdi^.hstrTargetName) THEN
            OS2.DrgDeleteStrHandle (pdi^.ulItemID);
        END (*IF*);

        (* Delete all of the atoms in the standard places,      *)
        (* then free the draginfo structure.                    *)

        OS2.DrgDeleteDraginfoStrHandles( pdinfo);
        OS2.DrgFreeDraginfo( pdinfo);

        RETURN length;

    END Drop;

(************************************************************************)

PROCEDURE ReceiveDroppedData (hwnd: OS2.HWND;  pdinfo: OS2.PDRAGINFO);

    (* Respond to a DM_DROP message by picking up, if possible, the     *)
    (* dropped string and using it.                                     *)

    CONST Nul = CHR(0);  LF = CHR(10);

    VAR j, length: CARDINAL;  ch: CHAR;
        Text: ARRAY [0..255] OF CHAR;

    BEGIN
        (* Turn off emphasis now that a drop has occurred, *)
        (* then get the text and byte count.               *)

        DrawTargetEmphasis (hwnd, FALSE);
        length := Drop (pdinfo, Text);

        (* If the text string is non-empty, use it. *)

        IF length > 0 THEN
            FOR j := 0 TO length-1 DO
                ch := Text[j];
                IF (ch <> LF) AND (ch <> Nul) THEN
                    DoCalc.NewChar (ch);
                END (*IF*);
            END (*FOR*);
        END (*IF*);

    END ReceiveDroppedData;

(************************************************************************)

PROCEDURE DrawTargetEmphasis (hwnd: OS2.HWND;  Draw: BOOLEAN);

    (* Draws the target emphasis, or undraws it if the second parameter *)
    (* is FALSE.                                                        *)

    VAR rctl: OS2.RECTL;
        hps: OS2.HPS;

    BEGIN
        (* Compare the Draw parameter with the global flag EmphasisIsOn *)
        (* to see whether we need to do anything on this call.          *)

        IF Draw <> EmphasisIsOn THEN

            (* Get the window rectangle in parent coordinates, then     *)
            (* shrink the rectangle a bit (these are only guesses).     *)

            OS2.WinQueryWindowRect (hwnd, rctl);
            WITH rctl DO
                INC (xLeft, 3);
                DEC (xRight, 4);
                INC (yBottom, 3);
                DEC (yTop, 4);
            END (*WITH*);

            (* You need a special presentation space to have your       *)
            (* output appear during a drag;  if we can get one, use an  *)
            (* xor operation to draw/undraw a border.                   *)

            hps := OS2.DrgGetPS (hwnd);
            IF hps <> 0 THEN
                OS2.WinDrawBorder (hps, rctl, 1, 1, 0, 0, OS2.DB_DESTINVERT);
                OS2.DrgReleasePS (hps);
            END (*IF*);

            (* Remember whether we currently have emphasis. *)

            EmphasisIsOn := Draw;

        END (*IF*);

    END DrawTargetEmphasis;

(************************************************************************)
(*                       MODULE INITIALISATION                          *)
(************************************************************************)

BEGIN
    EmphasisIsOn := FALSE;
END Drag.

