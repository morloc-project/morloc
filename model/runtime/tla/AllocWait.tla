------------------------------ MODULE AllocWait ------------------------------
(* The allocator's locks and mappings within one process (model/runtime/shm.md *)
(* SHM-2, FORK-10). Workers allocate and free. An allocation snapshots the  *)
(* published volumes under the short process lock VOLUMES, drops it, and    *)
(* then waits for each volume's cross-process lock, which another process   *)
(* may hold for as long as it likes. When no volume has room it grows: it   *)
(* takes the growth lock, builds a new volume outside every other lock,     *)
(* and publishes it under VOLUMES. A free finds its volume in the published *)
(* range table without a lock. A fork's prepare takes VOLUMES. Recovery is  *)
(* the only unmapper: it closes the request gate and unmaps once every      *)
(* thread that may touch shared memory has stopped, workers and the other   *)
(* nexus threads that read it (a stdio connection, a spawn watcher) alike.  *)
EXTENDS Naturals, FiniteSets

CONSTANT Variant
\* "design": the design.
\* "alloc_mutex": an allocation holds a process lock across its wait on a
\*   volume's cross-process lock.
\* "grow_under_volumes": growth builds the new volume holding VOLUMES.
\* "uncounted_reader": recovery waits for the workers only.
\* "loser_unlinks": of two threads that grow at once, the loser removes the
\*   name it built, which is the winner's name too.

Workers == {"w1", "w2"}
Readers == {"stdio"}
Threads == Workers \cup Readers
Vols == {1, 2}

VARIABLES
    published,  \* volumes in the range table
    mapped,     \* volumes mapped in this process
    named,      \* volumes whose name exists
    volLock,    \* volume -> "free" | "other" | a thread
    heldVOL,    \* the thread holding VOLUMES, or "none"
    growLock,   \* the thread holding the growth lock, or "none"
    pc,         \* thread -> its step
    at,         \* thread -> the volume it is using, or 0
    built,      \* thread -> the volume it built, or 0
    gate,       \* new work is admitted
    rpc,        \* recovery: "idle" | "draining" | "unmapped"
    fpc         \* fork: "idle" | "prepared" | "done"

vars == <<published, mapped, named, volLock, heldVOL, growLock, pc, at, built, gate, rpc, fpc>>

Init ==
    /\ published = {1}
    /\ mapped = {1}
    /\ named = {1}
    /\ volLock = [v \in Vols |-> "free"]
    /\ heldVOL = "none"
    /\ growLock = "none"
    /\ pc = [t \in Threads |-> "idle"]
    /\ at = [t \in Threads |-> 0]
    /\ built = [t \in Threads |-> 0]
    /\ gate = TRUE
    /\ rpc = "idle"
    /\ fpc = "idle"

Others(t) == UNCHANGED <<published, mapped, named, volLock, heldVOL, growLock, gate, rpc, fpc>>

\* Another process takes and gives back volume 1's lock.
OtherLocks ==
    /\ volLock[1] = "free"
    /\ volLock' = [volLock EXCEPT ![1] = "other"]
    /\ UNCHANGED <<published, mapped, named, heldVOL, growLock, pc, at, built, gate, rpc, fpc>>
OtherUnlocks ==
    /\ volLock[1] = "other"
    /\ volLock' = [volLock EXCEPT ![1] = "free"]
    /\ UNCHANGED <<published, mapped, named, heldVOL, growLock, pc, at, built, gate, rpc, fpc>>

\* ---- a worker allocating ----

Begin(t) ==
    /\ t \in Workers /\ pc[t] = "idle" /\ gate
    /\ heldVOL = "none"
    /\ \E v \in published :
         /\ at' = [at EXCEPT ![t] = v]
         /\ IF Variant = "alloc_mutex"
            THEN /\ heldVOL' = t
                 /\ pc' = [pc EXCEPT ![t] = "lockvol"]
            ELSE /\ UNCHANGED heldVOL
                 /\ pc' = [pc EXCEPT ![t] = "lockvol"]
    /\ UNCHANGED <<published, mapped, named, volLock, growLock, built, gate, rpc, fpc>>

LockVol(t) ==
    /\ pc[t] = "lockvol"
    /\ volLock[at[t]] = "free"
    /\ volLock' = [volLock EXCEPT ![at[t]] = t]
    /\ pc' = [pc EXCEPT ![t] = "claim"]
    /\ UNCHANGED <<published, mapped, named, heldVOL, growLock, at, built, gate, rpc, fpc>>

\* Claim a block, or find no room and go grow.
Claim(t) ==
    /\ pc[t] = "claim"
    /\ volLock' = [volLock EXCEPT ![at[t]] = "free"]
    /\ heldVOL' = IF heldVOL = t THEN "none" ELSE heldVOL
    /\ \/ /\ pc' = [pc EXCEPT ![t] = "idle"]
          /\ at' = [at EXCEPT ![t] = 0]
       \/ /\ pc' = [pc EXCEPT ![t] = "grow"]
          /\ at' = [at EXCEPT ![t] = 0]
    /\ UNCHANGED <<published, mapped, named, growLock, built, gate, rpc, fpc>>

\* Two threads may decide to grow; the growth lock lets one build at a time,
\* and a thread that finds a volume published since it decided uses it.
Grow(t) ==
    /\ pc[t] = "grow"
    /\ growLock = "none"
    /\ IF published = Vols
       THEN /\ pc' = [pc EXCEPT ![t] = "idle"]
            /\ UNCHANGED <<growLock, heldVOL>>
       ELSE /\ Variant = "grow_under_volumes" => heldVOL = "none"
            /\ growLock' = IF Variant = "loser_unlinks" THEN "none" ELSE t
            /\ heldVOL' = IF Variant = "grow_under_volumes" THEN t ELSE heldVOL
            /\ pc' = [pc EXCEPT ![t] = "build"]
    /\ UNCHANGED <<published, mapped, named, volLock, at, built, gate, rpc, fpc>>

\* Building creates the name and maps the segment: slow, and may block.
Build(t) ==
    /\ pc[t] = "build"
    /\ built' = [built EXCEPT ![t] = 2]
    /\ named' = named \cup {2}
    /\ mapped' = mapped \cup {2}
    /\ pc' = [pc EXCEPT ![t] = "install"]
    /\ UNCHANGED <<published, volLock, heldVOL, growLock, at, gate, rpc, fpc>>

Install(t) ==
    /\ pc[t] = "install"
    /\ heldVOL \in {"none", t}
    /\ IF 2 \in published
       THEN \* lost the race: drop its own mapping
            /\ named' = IF Variant = "loser_unlinks" THEN named \ {2} ELSE named
            /\ UNCHANGED <<published, mapped>>
       ELSE /\ published' = published \cup {2}
            /\ UNCHANGED <<named, mapped>>
    /\ heldVOL' = "none"
    /\ growLock' = IF growLock = t THEN "none" ELSE growLock
    /\ built' = [built EXCEPT ![t] = 0]
    /\ pc' = [pc EXCEPT ![t] = "idle"]
    /\ UNCHANGED <<volLock, at, gate, rpc, fpc>>

\* ---- a free, and a reader thread touching shared memory ----

\* A reader thread enters the same gate as a request, except where recovery
\* does not count it.
Touch(t) ==
    /\ pc[t] = "idle"
    /\ gate \/ (t \in Readers /\ Variant = "uncounted_reader")
    /\ \E v \in published :
         /\ at' = [at EXCEPT ![t] = v]
         /\ pc' = [pc EXCEPT ![t] = "touching"]
    /\ UNCHANGED <<published, mapped, named, volLock, heldVOL, growLock, built, gate, rpc, fpc>>

Untouch(t) ==
    /\ pc[t] = "touching"
    /\ at' = [at EXCEPT ![t] = 0]
    /\ pc' = [pc EXCEPT ![t] = "idle"]
    /\ UNCHANGED <<published, mapped, named, volLock, heldVOL, growLock, built, gate, rpc, fpc>>

\* ---- fork ----

Prepare ==
    /\ fpc = "idle"
    /\ heldVOL = "none"
    /\ heldVOL' = "fork"
    /\ fpc' = "prepared"
    /\ UNCHANGED <<published, mapped, named, volLock, growLock, pc, at, built, gate, rpc>>

AfterFork ==
    /\ fpc = "prepared"
    /\ heldVOL' = "none"
    /\ fpc' = "done"
    /\ UNCHANGED <<published, mapped, named, volLock, growLock, pc, at, built, gate, rpc>>

\* ---- recovery ----

Counted == IF Variant = "uncounted_reader" THEN Workers ELSE Threads

StartRecovery ==
    /\ rpc = "idle"
    /\ gate' = FALSE
    /\ rpc' = "draining"
    /\ UNCHANGED <<published, mapped, named, volLock, heldVOL, growLock, pc, at, built, fpc>>

Unmap ==
    /\ rpc = "draining"
    /\ \A t \in Counted : pc[t] = "idle"
    /\ mapped' = {}
    /\ published' = {}
    /\ rpc' = "unmapped"
    /\ UNCHANGED <<named, volLock, heldVOL, growLock, pc, at, built, gate, fpc>>

Next ==
    \/ OtherLocks \/ OtherUnlocks
    \/ \E t \in Threads :
         Begin(t) \/ LockVol(t) \/ Claim(t) \/ Grow(t) \/ Build(t) \/ Install(t)
         \/ Touch(t) \/ Untouch(t)
    \/ Prepare \/ AfterFork
    \/ StartRecovery \/ Unmap
    \/ UNCHANGED vars

Spec ==
    /\ Init /\ [][Next]_vars
    /\ WF_vars(OtherUnlocks)
    /\ \A t \in Threads :
         WF_vars(LockVol(t)) /\ WF_vars(Claim(t)) /\ WF_vars(Grow(t))
         /\ WF_vars(Build(t)) /\ WF_vars(Install(t)) /\ WF_vars(Untouch(t))
    /\ WF_vars(Prepare) /\ WF_vars(AfterFork) /\ WF_vars(Unmap)

\* ---- properties ----

\* No thread uses a volume that is not mapped.
NeverTouchesUnmapped == \A t \in Threads : at[t] # 0 => at[t] \in mapped

\* No process lock is held while waiting on another process or while
\* building a volume, so a fork's prepare waits only for short sections.
NoProcessLockAcrossAWait ==
    \A t \in Threads : heldVOL = t => pc[t] \notin {"lockvol", "build"}

\* A published volume can still be opened by its name.
PublishedVolumesAreNamed == published \subseteq named \/ rpc = "unmapped"

TheForkCompletes == <>(fpc = "done")
================================================================================
