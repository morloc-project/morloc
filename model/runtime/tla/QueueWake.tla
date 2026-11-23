------------------------------ MODULE QueueWake ------------------------------
(* Waking across a stream's custody queue only when someone sleeps          *)
(* (model/runtime/streams.md SLOT-22). The writer pushes batches and, when  *)
(* the queue is full, waits for the custodian's done count to move; the     *)
(* custodian takes batches and, when the queue is empty, waits for the      *)
(* queue's bell to move. A push moves the push count and then rings the     *)
(* bell; a stop (from the process hosting the custodian, at any time) sets  *)
(* the custodian's stop word, rings the bell and wakes it. The custodian    *)
(* reads the bell before it checks the stop word, and ends when it finds a  *)
(* stop or the last batch done. The queue takes no lock. A side about to    *)
(* sleep registers on its word's waiter count and then sleeps only while    *)
(* the word still holds what it read (a futex wait). A side that moves a    *)
(* word moves it first and then wakes its sleepers only if the count is     *)
(* non-zero; the stopper wakes unconditionally. Every step is one           *)
(* sequentially consistent atomic action; a wake reaches only a sleeper     *)
(* already blocked, and a blocked side never times out, except a writer    *)
(* asleep after a stop: the done count no longer moves, and the writer      *)
(* sees the stop only when its slice ends, which this model allows but      *)
(* does not time. Stuttering is allowed only once both sides have ended.    *)
EXTENDS Naturals

CONSTANT Variant, Depth, Batches
\* "design": the design.
\* "check_before_bump": a side reads the waiter count before bumping the word.
\* "register_after_compare": a sleeper compares the word before registering.
\* "tail_only": no bell; the custodian sleeps on the push count, read after
\*   its stop check, and a stop moves no word.

Sides == {"w", "c"}
Other(s) == IF s = "w" THEN "c" ELSE "w"

VARIABLES
    word,      \* side -> the count that side moves: "w" moves tail, "c" moves done
    bell,      \* the custodian's bell
    waiters,   \* side -> sleepers registered on the word that side waits on
    pc,        \* side -> step
    seen,      \* side -> the word it read before sleeping
    pend,      \* side -> its wake decision, read early (check_before_bump)
    stopped,   \* the custodian's stop word is set
    spc        \* the stopper: "idle" | "ring" | "wake" | "done"

vars == <<word, bell, waiters, pc, seen, pend, stopped, spc>>

Tail == word["w"]
Done == word["c"]

Init ==
    /\ word = [s \in Sides |-> 0]
    /\ bell = 0
    /\ waiters = [s \in Sides |-> 0]
    /\ pc = [s \in Sides |-> "work"]
    /\ seen = [s \in Sides |-> 0]
    /\ pend = [s \in Sides |-> FALSE]
    /\ stopped = FALSE
    /\ spc = "idle"

CanWork(s) == IF s = "w" THEN Tail - Done < Depth ELSE Tail > Done
Finished(s) == IF s = "w" THEN Tail = Batches ELSE Done = Batches

\* The word side s sleeps on.
Waited(s) ==
    IF s = "w" THEN Done
    ELSE IF Variant = "tail_only" THEN Tail ELSE bell

\* The custodian reads the bell first, then checks its stop word.
Look ==
    /\ pc["c"] = "work"
    /\ seen' = [seen EXCEPT !["c"] = IF Variant = "tail_only" THEN @ ELSE bell]
    /\ pc' = [pc EXCEPT !["c"] = "stopchk"]
    /\ UNCHANGED <<word, bell, waiters, pend, stopped, spc>>

StopCheck ==
    /\ pc["c"] = "stopchk"
    /\ pc' = [pc EXCEPT !["c"] = IF stopped THEN "end" ELSE "take"]
    /\ UNCHANGED <<word, bell, waiters, seen, pend, stopped, spc>>

\* The writer at "work", the custodian at "take".
Work(s) ==
    /\ pc[s] = IF s = "w" THEN "work" ELSE "take"
    /\ IF Finished(s) \/ (s = "w" /\ stopped)
       THEN /\ pc' = [pc EXCEPT ![s] = "end"]
            /\ UNCHANGED <<word, waiters, seen, pend>>
       ELSE IF CanWork(s)
       THEN IF Variant = "check_before_bump"
            THEN /\ pend' = [pend EXCEPT ![s] = waiters[Other(s)] > 0]
                 /\ pc' = [pc EXCEPT ![s] = "bump"]
                 /\ UNCHANGED <<word, waiters, seen>>
            ELSE /\ word' = [word EXCEPT ![s] = @ + 1]
                 /\ pc' = [pc EXCEPT ![s] = IF s = "w" /\ Variant # "tail_only" THEN "ring" ELSE "wake"]
                 /\ UNCHANGED <<waiters, seen, pend>>
       ELSE /\ seen' = IF s = "w" \/ Variant = "tail_only"
                       THEN [seen EXCEPT ![s] = Waited(s)] ELSE seen
            /\ pc' = [pc EXCEPT ![s] = IF Variant = "register_after_compare" THEN "cmp" ELSE "reg"]
            /\ UNCHANGED <<word, waiters, pend>>
    /\ UNCHANGED <<bell, stopped, spc>>

\* check_before_bump: the bump after the early read of the waiter count.
Bump(s) ==
    /\ pc[s] = "bump"
    /\ word' = [word EXCEPT ![s] = @ + 1]
    /\ pc' = [pc EXCEPT ![s] = IF s = "w" THEN "ring" ELSE "wake"]
    /\ UNCHANGED <<bell, waiters, seen, pend, stopped, spc>>

Ring ==
    /\ pc["w"] = "ring"
    /\ bell' = bell + 1
    /\ pc' = [pc EXCEPT !["w"] = "wake"]
    /\ UNCHANGED <<word, waiters, seen, pend, stopped, spc>>

\* Wake the other side if it is registered and blocked.
Wake(s) ==
    /\ pc[s] = "wake"
    /\ LET o == Other(s)
           doit == IF Variant = "check_before_bump" THEN pend[s] ELSE waiters[o] > 0
       IN pc' = IF doit /\ pc[o] = "blocked"
                THEN [pc EXCEPT ![s] = "work", ![o] = "unreg"]
                ELSE [pc EXCEPT ![s] = "work"]
    /\ pend' = [pend EXCEPT ![s] = FALSE]
    /\ UNCHANGED <<word, bell, waiters, seen, stopped, spc>>

\* A sleeper whose wait returned ends its own registration.
Unregister(s) ==
    /\ pc[s] = "unreg"
    /\ waiters' = [waiters EXCEPT ![s] = @ - 1]
    /\ pc' = [pc EXCEPT ![s] = "work"]
    /\ UNCHANGED <<word, bell, seen, pend, stopped, spc>>

Register(s) ==
    /\ pc[s] = "reg"
    /\ waiters' = [waiters EXCEPT ![s] = @ + 1]
    /\ pc' = [pc EXCEPT ![s] = "cmp"]
    /\ UNCHANGED <<word, bell, seen, pend, stopped, spc>>

\* The futex compare: return at once if the word moved, else block.
Compare(s) ==
    /\ pc[s] = "cmp"
    /\ pc' = [pc EXCEPT ![s] =
                 IF Waited(s) # seen[s]
                 THEN (IF Variant = "register_after_compare" THEN "work" ELSE "unreg")
                 ELSE (IF Variant = "register_after_compare" THEN "lreg" ELSE "blocked")]
    /\ UNCHANGED <<word, bell, waiters, seen, pend, stopped, spc>>

\* register_after_compare: registering after the compare, then blocking.
LateRegister(s) ==
    /\ pc[s] = "lreg"
    /\ waiters' = [waiters EXCEPT ![s] = @ + 1]
    /\ pc' = [pc EXCEPT ![s] = "blocked"]
    /\ UNCHANGED <<word, bell, seen, pend, stopped, spc>>

Stop ==
    /\ spc = "idle"
    /\ stopped' = TRUE
    /\ spc' = IF Variant = "tail_only" THEN "wake" ELSE "ring"
    /\ UNCHANGED <<word, bell, waiters, pc, seen, pend>>

StopRing ==
    /\ spc = "ring"
    /\ bell' = bell + 1
    /\ spc' = "wake"
    /\ UNCHANGED <<word, waiters, pc, seen, pend, stopped>>

\* A writer asleep after a stop wakes at the end of its slice.
SliceEnds ==
    /\ stopped
    /\ pc["w"] = "blocked"
    /\ pc' = [pc EXCEPT !["w"] = "unreg"]
    /\ UNCHANGED <<word, bell, waiters, seen, pend, stopped, spc>>

StopWake ==
    /\ spc = "wake"
    /\ pc' = IF pc["c"] = "blocked" THEN [pc EXCEPT !["c"] = "unreg"] ELSE pc
    /\ spc' = "done"
    /\ UNCHANGED <<word, bell, waiters, seen, pend, stopped>>

Next ==
    \/ Look \/ StopCheck \/ Ring \/ Stop \/ StopRing \/ StopWake \/ SliceEnds
    \/ \E s \in Sides : Work(s) \/ Bump(s) \/ Wake(s) \/ Register(s) \/ Compare(s)
                       \/ LateRegister(s) \/ Unregister(s)
    \/ (pc["w"] = "end" /\ pc["c"] = "end" /\ spc \in {"idle", "done"} /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars
        /\ WF_vars(Look) /\ WF_vars(StopCheck) /\ WF_vars(Ring)
        /\ WF_vars(StopRing) /\ WF_vars(StopWake) /\ WF_vars(SliceEnds)
        /\ \A s \in Sides : WF_vars(Work(s)) /\ WF_vars(Bump(s)) /\ WF_vars(Wake(s))
                            /\ WF_vars(Register(s)) /\ WF_vars(Compare(s)) /\ WF_vars(LateRegister(s))
                            /\ WF_vars(Unregister(s))

\* ---- properties ----
EverythingIsWritten == <>(pc["c"] = "end")
BoundedQueue == Tail - Done <= Depth
\* A stop ends the custodian.
AStopEndsTheCustodian == stopped ~> (pc["c"] = "end")
================================================================================
