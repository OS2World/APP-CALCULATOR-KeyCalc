IMPLEMENTATION MODULE TaskControl;

<* IF NOT multithread THEN *>
  "This module needs the multithread model"
  END TaskControl.
<* END *>

        (****************************************************************)
        (*                                                              *)
        (*   Data structures internal to the kernel of the operating    *)
        (*     system; the dispatcher of the operating system; and      *)
        (*                  related procedures.                         *)
        (*                                                              *)
        (*      Programmer:     P. Moylan                               *)
        (*      Last edited:    15 June 2001                            *)
        (*      Status:         OK                                      *)
        (*                                                              *)
        (****************************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST;

IMPORT OS2, Processes;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  Assert;

(************************************************************************)

CONST StackSize = 32768;

TYPE

    (********************************************************************)
    (*                                                                  *)
    (* Descriptor for a task.  The fields have the following meaning.   *)
    (*                                                                  *)
    (*   next        pointer to the next descriptor on the master task  *)
    (*                list                                              *)
    (*   name        identifier for testing purposes                    *)
    (*   WakeUp      event semaphore used in blocking a task            *)
    (*   threadnum   thread identifier                                  *)
    (*                                                                  *)
    (********************************************************************)

    Task = POINTER TO
               RECORD
                   next: Task;
                   name: NameString;
                   WakeUp: OS2.HEV;
                   threadnum: TaskID;
               END (*RECORD*);

(************************************************************************)

VAR
    (* The list of all tasks known to us. *)

    MasterTaskList: Task;

    (* Mutual exclusion semaphore.  We must lock this for any access to *)
    (* the task list.                                                   *)

    TaskListAccess: OS2.HMTX;

    (* A copy of the last TaskID <-> Task match made by DescriptorOf.   *)

    lastthread: TaskID;
    lastTask: Task;

(************************************************************************)
(*                         CONSISTENCY CHECK                            *)
(************************************************************************)

PROCEDURE Crash (message: ARRAY OF CHAR);

    (* Aborts the program in a way that will let us do a postmortem check. *)

    BEGIN
        Assert (FALSE);
    END Crash;

(************************************************************************)
(*                 KERNEL CRITICAL SECTION PROTECTION                   *)
(************************************************************************)

PROCEDURE LockTaskList;

    BEGIN
        OS2.DosRequestMutexSem (TaskListAccess, OS2.SEM_INDEFINITE_WAIT);
    END LockTaskList;

(************************************************************************)

PROCEDURE UnlockTaskList;

    BEGIN
        OS2.DosReleaseMutexSem (TaskListAccess);
    END UnlockTaskList;

(************************************************************************)
(*                            TASK CREATION                             *)
(************************************************************************)

PROCEDURE NewTaskDescriptor (taskname: NameString): Task;

    (* Creates a descriptor for a new task, and adds it to the master   *)
    (* task list.  Note that this does not fill in the id field of      *)
    (* the descriptor.                                                  *)

    VAR result: Task;

    BEGIN
        NEW (result);
        WITH result^ DO
            name := taskname;
            OS2.DosCreateEventSem (NIL, WakeUp, 0, FALSE);
        END (*WITH*);
        LockTaskList;
        result^.next := MasterTaskList;
        MasterTaskList := result;
        UnlockTaskList;
        RETURN result;
    END NewTaskDescriptor;

(************************************************************************)

TYPE TaskStartInfo = POINTER TO
                          RECORD
                              HasParameter: BOOLEAN;
                              TaskCode0: PROC;
                              TaskCode1: PROC1;
                              parameter: ADDRESS;
                              descriptor: Task;
                          END;

(************************************************************************)

PROCEDURE Dummy0;

    (* Dummy task, should never be run. *)

    BEGIN
        Crash ("Dummy0 entered");
    END Dummy0;

(************************************************************************)

PROCEDURE Dummy1 (param: ADDRESS);

    (* Dummy task, should never be run. *)

    BEGIN
        Crash ("Dummy1 entered");
        IF param <> NIL THEN  (* Pointless code to suppress a compiler warning *)
            HALT;
        END (*IF*)
    END Dummy1;

(************************************************************************)

PROCEDURE TaskWrapper;

    (* This is the task that runs the user's task code. *)

    VAR StartInfo: TaskStartInfo;
        UseParameter: BOOLEAN;
        Proc0: PROC;
        Proc1: PROC1;  param: ADDRESS;

    BEGIN
        StartInfo := Processes.MyParam();
        WITH StartInfo^ DO
            UseParameter := HasParameter;
            Proc0 := TaskCode0;
            Proc1 := TaskCode1;
            param := parameter;
            descriptor^.threadnum := CurrentTaskID();
        END (*WITH*);
        DISPOSE (StartInfo);

        (* Call the user's task code. *)

        IF UseParameter THEN
            Proc1 (param);
        ELSE
            Proc0;
        END (*IF*);

        TaskExit;

    END TaskWrapper;

(************************************************************************)

PROCEDURE CreateTask (StartAddress: PROC;  taskpriority: PriorityLevel;
                                                taskname: NameString);

    (* Must be called to introduce a task to the system. The first      *)
    (* parameter, which should be the name of a procedure containing    *)
    (* the task code, gives the starting address.  The second parameter *)
    (* is the task's base priority.  If this task has a higher priority *)
    (* than its creator, it will run immediately.  Otherwise, it        *)
    (* becomes ready.                                                   *)

    VAR T: Task;  StartInfo: TaskStartInfo;
        id: Processes.ProcessId;

    BEGIN
        T := NewTaskDescriptor (taskname);
        Assert (T <> NIL);
        NEW (StartInfo);
        WITH StartInfo^ DO
            HasParameter := FALSE;
            TaskCode0 := StartAddress;
            TaskCode1 := Dummy1;
            parameter := NIL;
            descriptor := T;
        END (*WITH*);

        Processes.Start (TaskWrapper, StackSize, taskpriority, StartInfo, id);

    END CreateTask;

(************************************************************************)

PROCEDURE CreateTask1 (StartAddress: PROC1;  taskpriority: PriorityLevel;
                                   taskname: NameString;  param: ADDRESS);

    (* Like CreateTask, but allows the passing of a single parameter    *)
    (* "param" to the task.                                             *)

    VAR StartInfo: TaskStartInfo;  T: Task;
        id: Processes.ProcessId;

    BEGIN
        T := NewTaskDescriptor (taskname);
        NEW (StartInfo);
        WITH StartInfo^ DO
            HasParameter := TRUE;
            TaskCode0 := Dummy0;
            TaskCode1 := StartAddress;  parameter := param;
            descriptor := T;
        END (*WITH*);

        Processes.Start (TaskWrapper, StackSize, taskpriority, StartInfo, id);

    END CreateTask1;

(************************************************************************)

PROCEDURE TaskExit;

    (* Removes the currently running task from the system, and performs *)
    (* a task switch to the next ready task.                            *)

    VAR MyID: TaskID;  previous, current: Task;

    BEGIN
        MyID := CurrentTaskID();

        LockTaskList;
        previous := NIL;  current := MasterTaskList;
        WHILE (current <> NIL) AND (current^.threadnum <> MyID) DO
            previous := current;  current := current^.next;
        END (*WHILE*);
        IF current <> NIL THEN
            IF previous = NIL THEN
                MasterTaskList := current^.next;
            ELSE
                previous^.next := current^.next;
            END (*IF*);
            OS2.DosCloseEventSem (current^.WakeUp);
            IF current = lastTask THEN
               lastthread := 0;  lastTask := NIL;
            END (*IF*);
            DISPOSE (current);
        END (*IF*);
        UnlockTaskList;

        Processes.StopMe;

    END TaskExit;

(************************************************************************)
(*                        IDENTIFYING A TASK                            *)
(************************************************************************)

PROCEDURE CurrentTaskID(): TaskID;

    (* Returns the TaskID of the calling task. *)

    VAR ptib: OS2.PTIB;  ppib: OS2.PPIB;

    BEGIN
        OS2.DosGetInfoBlocks (ptib, ppib);
        RETURN ptib^.tib_ptib2^.tib2_ultid;
    END CurrentTaskID;

(************************************************************************)

PROCEDURE DescriptorOf (thread: TaskID): Task;

    (* Returns the task descriptor corresponding to the given TaskID. *)

    VAR result: Task;

    BEGIN
        LockTaskList;
        IF thread = lastthread THEN
            result := lastTask;
        ELSE
            result := MasterTaskList;
            WHILE (result <> NIL) AND (result^.threadnum <> thread) DO
                result := result^.next;
            END (*WHILE*);
            lastthread := thread;
            lastTask := result;
        END (*IF*);
        UnlockTaskList;
        RETURN result;
    END DescriptorOf;

(************************************************************************)
(*                LOCKS FOR CRITICAL SECTION PROTECTION                 *)
(************************************************************************)

PROCEDURE CreateLock (VAR (*OUT*) L: Lock);

    (* Creates a new lock. *)

    BEGIN
        OS2.DosCreateMutexSem (NIL, L, 0, FALSE);
    END CreateLock;

(************************************************************************)

PROCEDURE DestroyLock (VAR (*INOUT*) L: Lock);

    (* Disposes of a lock. *)

    BEGIN
        OS2.DosCloseMutexSem (L);
    END DestroyLock;

(************************************************************************)

PROCEDURE Obtain (L: Lock);

    (* Obtains lock L, waiting if necessary. *)

    BEGIN
        OS2.DosRequestMutexSem (L, OS2.SEM_INDEFINITE_WAIT);
    END Obtain;

(************************************************************************)

PROCEDURE Release (L: Lock);

    (* Releases lock L - which might unblock some other task. *)

    BEGIN
        OS2.DosReleaseMutexSem (L);
    END Release;

(************************************************************************)
(*                 SUSPENDING AND RESUMING A TASK                       *)
(************************************************************************)

PROCEDURE SuspendMe (id: TaskID;  TimeLimit: CARDINAL): BOOLEAN;

    (* Suspends the caller.  A TRUE result indicates that the time      *)
    (* limit expired without the task being woken up.                   *)

    VAR T: Task;  status: CARDINAL;  PostCount: CARDINAL;
        TimedOut: BOOLEAN;

    BEGIN
        T := DescriptorOf (id);
        status := OS2.DosWaitEventSem (T^.WakeUp, TimeLimit);
        TimedOut := status = OS2.ERROR_TIMEOUT;
        IF NOT TimedOut THEN
            status := OS2.DosResetEventSem (T^.WakeUp, PostCount);
        END (*IF*);
        RETURN TimedOut;
    END SuspendMe;

(************************************************************************)

PROCEDURE ResumeTask (id: TaskID): BOOLEAN;

    (* Resumes a task specified by its thread ID.                       *)
    (* The function result is normally TRUE, but is FALSE if the task   *)
    (* couldn't be resumed (usually because that task no longer exists).*)

    VAR T: Task;  status: CARDINAL;

    BEGIN
        T := DescriptorOf (id);
        IF T = NIL THEN
            RETURN FALSE;
        END (*IF*);
        status := OS2.DosPostEventSem (T^.WakeUp);
        RETURN TRUE;
    END ResumeTask;

(************************************************************************)
(*                     MODULE INITIALISATION                            *)
(************************************************************************)

PROCEDURE CreateMainTaskDescriptor;

    VAR T: Task;

    BEGIN
        T := NewTaskDescriptor ("*MAIN*");
        T^.threadnum := CurrentTaskID();
        lastTask := T;
        lastthread := T^.threadnum;
    END CreateMainTaskDescriptor;

(************************************************************************)

BEGIN
    OS2.DosCreateMutexSem (NIL, TaskListAccess, 0, FALSE);
    MasterTaskList := NIL;
    CreateMainTaskDescriptor;
END TaskControl.

