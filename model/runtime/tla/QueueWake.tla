------------------------------ MODULE QueueWake ------------------------------
(* Waking across a stream's custody queue only when someone sleeps          *)
(* (model/runtime/streams.md SLOT-22). The writer pushes batches and, when  *)
(* the queue is full, waits for the custodian's done count to move; the     *)
(* custodian takes batches and, when the queue is empty, waits for the push *)
(* count to move. The queue takes no lock. A side about to sleep reads the  *)
(* word it waits on, registers on that word's waiter count, and then sleeps *)
(* only while the word still holds what it read (a futex wait). A side that *)
(* moves a word bumps it first and then wakes its sleepers only if the      *)
(* count is non-zero. Every step is one sequentially consistent atomic      *)
(* action; a wake reaches only a sleeper already blocked.                    *)
EXTENDS Naturals

CONSTANT Variant, Depth, Batches
\* "design": the design.
\* "check_before_bump": a side reads the waiter count before bumping the word.
\* "register_after_compare": a sleeper compares the word before registering.

Sides == {"w", "c"}
Other(s) == IF s = "w" THEN "c" ELSE "w"

VARIABLES
    word,      \* side -> the word that side moves: "w" moves tail, "c" moves done
    waiters,   \* side -> sleepers registered on the word the OTHER side moves
    pc,        \* side -> step
    seen,      \* side -> the word it read before sleeping
    pend       \* side -> its wake decision, read early (check_before_bump)

vars == <<word, waiters, pc, seen, pend>>

Tail == word["w"]
Done == word["c"]

Init ==
    /\ word = [s \in Sides |-> 0]
    /\ waiters = [s \in Sides |-> 0]
    /\ pc = [s \in Sides |-> "work"]
    /\ seen = [s \in Sides |-> 0]
    /\ pend = [s \in Sides |-> FALSE]

\* Whether side s can make progress now, and whether it is finished.
CanWork(s) == IF s = "w" THEN Tail - Done < Depth ELSE Tail > Done
Finished(s) == IF s = "w" THEN Tail = Batches ELSE Done = Batches

\* The word side s waits on: the one the other side moves.
Waited(s) == word[Other(s)]

Work(s) ==
    /\ pc[s] = "work"
    /\ IF Finished(s)
       THEN /\ pc' = [pc EXCEPT ![s] = "end"]
            /\ UNCHANGED <<word, waiters, seen, pend>>
       ELSE IF CanWork(s)
       THEN IF Variant = "check_before_bump"
            THEN /\ pend' = [pend EXCEPT ![s] = waiters[Other(s)] > 0]
                 /\ pc' = [pc EXCEPT ![s] = "bump"]
                 /\ UNCHANGED <<word, waiters, seen>>
            ELSE /\ word' = [word EXCEPT ![s] = @ + 1]
                 /\ pc' = [pc EXCEPT ![s] = "wake"]
                 /\ UNCHANGED <<waiters, seen, pend>>
       ELSE /\ seen' = [seen EXCEPT ![s] = Waited(s)]
            /\ pc' = [pc EXCEPT ![s] = IF Variant = "register_after_compare" THEN "cmp" ELSE "reg"]
            /\ UNCHANGED <<word, waiters, pend>>

\* check_before_bump: the bump after the early read of the waiter count.
Bump(s) ==
    /\ pc[s] = "bump"
    /\ word' = [word EXCEPT ![s] = @ + 1]
    /\ pc' = [pc EXCEPT ![s] = "wake"]
    /\ UNCHANGED <<waiters, seen, pend>>

\* Wake the other side if it is registered and blocked.
Wake(s) ==
    /\ pc[s] = "wake"
    /\ LET o == Other(s)
           doit == IF Variant = "check_before_bump" THEN pend[s] ELSE waiters[o] > 0
       IN pc' = IF doit /\ pc[o] = "blocked"
                THEN [pc EXCEPT ![s] = "work", ![o] = "unreg"]
                ELSE [pc EXCEPT ![s] = "work"]
    /\ pend' = [pend EXCEPT ![s] = FALSE]
    /\ UNCHANGED <<word, waiters, seen>>

\* A sleeper whose wait returned ends its own registration.
Unregister(s) ==
    /\ pc[s] = "unreg"
    /\ waiters' = [waiters EXCEPT ![s] = @ - 1]
    /\ pc' = [pc EXCEPT ![s] = "work"]
    /\ UNCHANGED <<word, seen, pend>>

Register(s) ==
    /\ pc[s] = "reg"
    /\ waiters' = [waiters EXCEPT ![s] = @ + 1]
    /\ pc' = [pc EXCEPT ![s] = "cmp"]
    /\ UNCHANGED <<word, seen, pend>>

\* The futex compare: return at once if the word moved, else block.
Compare(s) ==
    /\ pc[s] = "cmp"
    /\ pc' = [pc EXCEPT ![s] =
                 IF Waited(s) # seen[s]
                 THEN (IF Variant = "register_after_compare" THEN "work" ELSE "unreg")
                 ELSE (IF Variant = "register_after_compare" THEN "lreg" ELSE "blocked")]
    /\ UNCHANGED <<word, waiters, seen, pend>>

\* register_after_compare: registering after the compare, then blocking.
LateRegister(s) ==
    /\ pc[s] = "lreg"
    /\ waiters' = [waiters EXCEPT ![s] = @ + 1]
    /\ pc' = [pc EXCEPT ![s] = "blocked"]
    /\ UNCHANGED <<word, seen, pend>>

Next ==
    \/ \E s \in Sides : Work(s) \/ Bump(s) \/ Wake(s) \/ Register(s) \/ Compare(s)
                       \/ LateRegister(s) \/ Unregister(s)
    \/ (pc["w"] = "end" /\ pc["c"] = "end" /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars
        /\ \A s \in Sides : WF_vars(Work(s)) /\ WF_vars(Bump(s)) /\ WF_vars(Wake(s))
                            /\ WF_vars(Register(s)) /\ WF_vars(Compare(s)) /\ WF_vars(LateRegister(s))
                            /\ WF_vars(Unregister(s))

\* ---- properties ----
EverythingIsWritten == <>(pc["c"] = "end")
BoundedQueue == Tail - Done <= Depth
================================================================================
