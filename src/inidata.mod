IMPLEMENTATION MODULE INIData;

        (************************************************************)
        (*                                                          *)
        (*               Looking after our INI file data            *)
        (*                                                          *)
        (*    Started:        30 March 2000                         *)
        (*    Last edited:    07 March 2002                         *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT SYSTEM, OS2, Strings;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);

TYPE
    FilenameString = ARRAY [0..511] OF CHAR;

    WindowPosition = RECORD
                         x, y: CARDINAL;
                     END (*RECORD*);

    CharArrayPointer = POINTER TO ARRAY [0..MAX(CARDINAL) DIV 4] OF CHAR;

    StringReadState = POINTER TO
                          RECORD
                              pos, BufferSize: CARDINAL;
                              bufptr: CharArrayPointer;
                          END (*RECORD*);

VAR
    (* Anchor block handle for this application.  *)

    hab: OS2.HAB;

    (* CurrentDir is the directory from which this program is running.     *)

    CurrentDir: FilenameString;

(************************************************************************)
(*                   READING/WRITING A LOCAL INI FILE                   *)
(************************************************************************)

PROCEDURE OpenINIFile (filename: ARRAY OF CHAR): OS2.HINI;

    (* Opens an INI file, returns its handle. *)

    BEGIN
        RETURN OS2.PrfOpenProfile (hab, filename);
    END OpenINIFile;

(************************************************************************)

PROCEDURE CloseINIFile (hini: OS2.HINI);

    (* Closes our INI file. *)

    BEGIN
        OS2.PrfCloseProfile (hini);
    END CloseINIFile;

(************************************************************************)

PROCEDURE ItemSize (hini: OS2.HINI;  application, key: ARRAY OF CHAR;
                                VAR (*OUT*) size: CARDINAL): BOOLEAN;

    (* Sets size to the size in bytes of the given INI file entry,      *)
    (* or returns FALSE if there is no such entry.                      *)

    BEGIN
        size := 0;
        IF application[0] = Nul THEN
            RETURN OS2.PrfQueryProfileSize (hini, NIL, NIL, size);
        ELSIF key[0] = Nul THEN
            RETURN OS2.PrfQueryProfileSize (hini, application, NIL, size);
        ELSE
            RETURN OS2.PrfQueryProfileSize (hini, application, key, size);
        END (*IF*);
    END ItemSize;

(********************************************************************************)

PROCEDURE INIGet (hini: OS2.HINI;  application, key: ARRAY OF CHAR;
                            VAR (*OUT*) result: ARRAY OF SYSTEM.LOC): BOOLEAN;

    (* Retrieves the value of a variable from the INI file.  Returns FALSE if   *)
    (* the variable was not found.                                              *)

    VAR size: CARDINAL;  success: BOOLEAN;

    BEGIN
        success := ItemSize (hini, application, key, size);
        IF success THEN
            IF application[0] = Nul THEN
                success := OS2.PrfQueryProfileData (hini, NIL, NIL, result, size);
            ELSIF key[0] = Nul THEN
                success := OS2.PrfQueryProfileData (hini, application, NIL, result, size);
            ELSE
                success := OS2.PrfQueryProfileData (hini, application, key, result, size);
            END (*IF*);
        END (*IF*);
        RETURN success;
    END INIGet;

(********************************************************************************)

PROCEDURE INIGetString (hini: OS2.HINI;  name1, name2: ARRAY OF CHAR;
                                    VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

    (* Like INIGet, but we accept any size data that will fit in the variable,  *)
    (* and we add a Nul terminator in the case of a size mismatch.              *)

    VAR size: CARDINAL;

    BEGIN
        IF OS2.PrfQueryProfileSize (hini, name1, name2, size)
                                   AND (size <= HIGH(variable)+1) THEN
            OS2.PrfQueryProfileData (hini, name1, name2, SYSTEM.ADR(variable), size);
            IF size <= HIGH(variable) THEN
                variable[size] := Nul;
            END (*IF*);
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END INIGetString;

(************************************************************************)

PROCEDURE INIPut (hini: OS2.HINI;  name1, name2: ARRAY OF CHAR;
                                                   variable: ARRAY OF SYSTEM.LOC);

    (* Writes data to the INI file. *)

    BEGIN
        OS2.PrfWriteProfileData (hini, name1, name2,
                                        SYSTEM.ADR(variable), HIGH(variable)+1);
    END INIPut;

(************************************************************************)

PROCEDURE INIPutBinary (hini: OS2.HINI;  name1, name2: ARRAY OF CHAR;
                        VAR (*IN*) variable: ARRAY OF SYSTEM.LOC;
                        amount: CARDINAL);

    (* Writes data to the INI file. *)

    BEGIN
        OS2.PrfWriteProfileData (hini, name1, name2,
                                        SYSTEM.ADR(variable), amount);
    END INIPutBinary;

(************************************************************************)

PROCEDURE INIPutString (hini: OS2.HINI;  name1, name2: ARRAY OF CHAR;
                                                   string: ARRAY OF CHAR);

    (* Writes a character string to the INI file. *)

    BEGIN
        OS2.PrfWriteProfileData (hini, name1, name2,
                                      SYSTEM.ADR(string), LENGTH(string));
    END INIPutString;

(************************************************************************)
(*              READING A STRING OF STRINGS FROM AN INI FILE            *)
(************************************************************************)

PROCEDURE GetStringList (hini: OS2.HINI;  app, key: ARRAY OF CHAR;
                                   VAR (*OUT*) state: StringReadState);

    (* Initialisation in preparation for a "NextString" operation. *)

    BEGIN
        NEW (state);

        (* Pick up the list of all list names. *)

        state^.bufptr := NIL;
        IF NOT ItemSize (hini, app, key, state^.BufferSize) THEN
            state^.BufferSize := 0;
        END (*IF*);
        IF state^.BufferSize > 0 THEN
            ALLOCATE (state^.bufptr, state^.BufferSize);
            IF NOT INIGet (hini, app, key, state^.bufptr^) THEN
                state^.bufptr^[0] := Nul;
            END (*IF*);
        END (*IF*);
        state^.pos := 0;

    END GetStringList;

(************************************************************************)

PROCEDURE NextString (state: StringReadState;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Reads the next character string from a string-of-strings field.  *)
    (* An empty string is returned when we have run out of strings.     *)

    VAR k: CARDINAL;

    BEGIN
        WITH state^ DO
            IF pos >= BufferSize THEN
                result[0] := Nul;
            ELSE
                k := 0;
                REPEAT
                    result[k] := bufptr^[pos];
                    INC (k);  INC (pos);
                UNTIL (pos >= BufferSize) OR (bufptr^[pos-1] = Nul)
                                        OR (k > HIGH(result));
                IF k <= HIGH(result) THEN
                    result[k] := Nul;
                END (*IF*);
            END (*IF*);
        END (*WITH*);
    END NextString;

(**************************************************************************)

PROCEDURE CloseStringList (VAR (*INOUT*) state: StringReadState);

    (* Must be called to release the memory used in fetching a  *)
    (* string of strings.                                       *)

    BEGIN
        IF state <> NIL THEN
            IF state^.BufferSize > 0 THEN
                DEALLOCATE (state^.bufptr, state^.BufferSize);
            END (*IF*);
            DISPOSE (state);
        END (*IF*);
    END CloseStringList;

(************************************************************************)
(*                      OTHER INI FILE OPERATIONS                       *)
(************************************************************************)

PROCEDURE INIDeleteApp (hini: OS2.HINI;  app: ARRAY OF CHAR);

    (* Deletes an application from the INI file. *)

    BEGIN
        OS2.PrfWriteProfileData (hini, app, NIL, NIL, 0);
    END INIDeleteApp;

(************************************************************************)

PROCEDURE INIDeleteKey (hini: OS2.HINI;  app, key: ARRAY OF CHAR);

    (* Deletes a key, and its associated data, from the INI file. *)

    BEGIN
        OS2.PrfWriteProfileData (hini, app, key, NIL, 0);
    END INIDeleteKey;

(************************************************************************)

PROCEDURE INICopyKey (hini: OS2.HINI;  oldapp, oldkey,
                                       newapp, newkey: ARRAY OF CHAR);

    (* Creates a second copy of the data for (oldapp, oldkey).  *)

    VAR size: CARDINAL;  p: CharArrayPointer;

    BEGIN
        IF NOT ItemSize (hini, oldapp, oldkey, size) THEN
            size := 0;
        END (*IF*);
        IF size = 0 THEN
            INIPutBinary (hini, newapp, newkey, p, 0);
        ELSE
            ALLOCATE (p, size);
            IF INIGet (hini, oldapp, oldkey, p^) THEN
                INIPutBinary (hini, newapp, newkey, p^, size);
            END (*IF*);
            DEALLOCATE (p, size);
        END (*IF*);
    END INICopyKey;

(************************************************************************)

PROCEDURE INICopyApp (hini: OS2.HINI;  oldapp, newapp: ARRAY OF CHAR);

    (* Creates a second copy of all data for this application.  *)

    VAR state: StringReadState;
        key: ARRAY [0..127] OF CHAR;

    BEGIN
        IF NOT Strings.Equal (oldapp, newapp) THEN
            GetStringList (hini, oldapp, "", state);
            NextString (state, key);
            WHILE key[0] <> Nul DO
                INICopyKey (hini, oldapp, key, newapp, key);
                INIDeleteKey (hini, oldapp, key);
                NextString (state, key);
            END (*WHILE*);
            CloseStringList (state);
        END (*IF*);
    END INICopyApp;

(************************************************************************)

PROCEDURE INIRenameApp (hini: OS2.HINI;  oldapp, newapp: ARRAY OF CHAR);

    (* Changes the name of an application, retaining the data. *)

    BEGIN
        IF NOT Strings.Equal (oldapp, newapp) THEN
            INICopyApp (hini, oldapp, newapp);
            INIDeleteApp (hini, oldapp);
        END (*IF*);
    END INIRenameApp;

(************************************************************************)
(*                      WHAT IS OUR DIRECTORY?                          *)
(************************************************************************)

PROCEDURE OurDirectory (VAR (*OUT*) dirname: ARRAY OF CHAR);

    (* Tells the caller the name of the program directory. *)

    BEGIN
        Strings.Assign (CurrentDir, dirname);
    END OurDirectory;

(************************************************************************)
(*                     SETUP DATA IN OUR INI FILE                       *)
(************************************************************************)

PROCEDURE SetInitialWindowPosition (hwnd: OS2.HWND;
                                    INIFileName, label: ARRAY OF CHAR);

    (* If this window has a previously stored position in our INI file, *)
    (* positions the window to that position.                           *)

    CONST bufsize = 256;

    VAR hini: OS2.HINI;  pos: WindowPosition;
        FontName: ARRAY [0..bufsize-1] OF CHAR;

    BEGIN
        hini := OpenINIFile(INIFileName);
        IF INIGet (hini, "WindowPos", label, pos) THEN
            OS2.WinSetWindowPos (hwnd, 0, pos.x, pos.y, 0, 0, OS2.SWP_MOVE);
        END (*IF*);
        IF NOT INIGetString (hini, "Font", label, FontName)
                      OR (FontName[0] = Nul) THEN
            FontName := "10.System Proportional";
        END (*IF*);
        CloseINIFile (hini);
        OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE, bufsize, FontName);
    END SetInitialWindowPosition;

(************************************************************************)

PROCEDURE StoreWindowPosition (hwnd: OS2.HWND;
                               INIFileName, label: ARRAY OF CHAR);

    (* Saves the location of this window in our INI file. *)

    CONST bufsize = 256;

    VAR hini: OS2.HINI;  swp: OS2.SWP;
        pos: WindowPosition;
        FontName: ARRAY [0..bufsize-1] OF CHAR;
        AttrFound, length: CARDINAL;

    BEGIN
        OS2.WinQueryWindowPos (hwnd, swp);
        pos.x := swp.x;  pos.y := swp.y;
        length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                     AttrFound,
                                     bufsize, FontName, 0(*OS2.QPF_NOINHERIT*));
        IF length < bufsize THEN
            FontName[length] := Nul;
        END (*IF*);
        hini := OpenINIFile(INIFileName);
        INIPut (hini, "WindowPos", label, pos);
        INIPutString (hini, "Font", label, FontName);
        CloseINIFile (hini);
    END StoreWindowPosition;

(************************************************************************)
(*                      SET CURRENT DIRECTORY                           *)
(************************************************************************)

PROCEDURE SetCurrentDirectory;

    (* Sets the CurrentDir variable to the name of the directory        *)
    (* where the executable resides.                                    *)

    VAR pPib: OS2.PPIB;  pTib: OS2.PTIB;
        j: CARDINAL;

    BEGIN
        OS2.DosGetInfoBlocks (pTib, pPib);
        IF OS2.DosQueryModuleName (pPib^.pib_hmte, OS2.CCHMAXPATH,
                                        CurrentDir) = OS2.NO_ERROR THEN

            (* Strip the string back to just before the last '\'. *)

            j := LENGTH (CurrentDir);
            WHILE (j > 0) AND (CurrentDir[j] <> '\') DO
                DEC (j);
            END (*WHILE*);
            CurrentDir[j] := CHR(0);
        ELSE
            CurrentDir := "";
        END (*IF*);

    END SetCurrentDirectory;

(************************************************************************)

BEGIN
    hab := OS2.WinInitialize (0);
    SetCurrentDirectory;
FINALLY
    IF hab <> OS2.NULLHANDLE THEN
        OS2.WinTerminate (hab);
    END (*IF*);
END INIData.

