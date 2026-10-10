----------------------------- MODULE QueueHandoff -----------------------------
(* Who frees a stream's queue block when its slot is reopened while the     *)
(* custodian's writer for the previous stream is still leaving. Each queue  *)
(* has one state word with two flags: CONSUMER, held by the writer that     *)
(* consumes it, and ABANDONED, set by an opener that gives the slot a new   *)
(* queue. The writer leaving clears CONSUMER and frees the queue if it      *)
(* finds ABANDONED set. An opener under the slot lock reuses the queue if   *)
(* CONSUMER is clear; otherwise it sets ABANDONED, and if CONSUMER had      *)
(* cleared in the meantime it reuses the queue after all (clearing          *)
(* ABANDONED), else it installs a new queue and the writer frees the old.   *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "flag_and_pointer": the writer, leaving, frees the queue only if the slot
\*   no longer points at it, and the opener installs a new queue whenever the
\*   consumer flag is set.
\* "no_recheck": the opener installs a new queue after setting ABANDONED
\*   even when the writer had already left.

Queues == {"q1", "q2"}

VARIABLES
    custody,   \* the queue the slot points at
    consumer,  \* queue -> its writer still consumes it
    abandoned, \* queue -> an opener gave the slot another queue
    freed,     \* queue -> times freed
    allocated, \* queue -> has been allocated
    wpc,       \* the leaving writer: "consuming" | "cleared" | "done"
    opc,       \* the opener: "idle" | "saw" | "flagged" | "done"
    sawConsumer,
    prevConsumer

vars == <<custody, consumer, abandoned, freed, allocated, wpc, opc, sawConsumer, prevConsumer>>

Init ==
    /\ custody = "q1"
    /\ consumer = [q \in Queues |-> q = "q1"]
    /\ abandoned = [q \in Queues |-> FALSE]
    /\ freed = [q \in Queues |-> 0]
    /\ allocated = [q \in Queues |-> q = "q1"]
    /\ wpc = "consuming"
    /\ opc = "idle"
    /\ sawConsumer = FALSE
    /\ prevConsumer = FALSE

\* ---- the writer of the previous stream, leaving ----

\* Design: one atomic step clears CONSUMER and returns the old word.
WriterClear ==
    /\ wpc = "consuming"
    /\ consumer' = [consumer EXCEPT !["q1"] = FALSE]
    /\ IF Variant \in {"design", "no_recheck"}
       THEN IF abandoned["q1"]
            THEN /\ freed' = [freed EXCEPT !["q1"] = @ + 1]
                 /\ wpc' = "done"
            ELSE /\ wpc' = "done"
                 /\ UNCHANGED freed
       ELSE /\ wpc' = "cleared"
            /\ UNCHANGED freed
    /\ UNCHANGED <<custody, abandoned, allocated, opc, sawConsumer, prevConsumer>>

\* Variant: a separate look at the slot's pointer.
WriterCheckPointer ==
    /\ Variant = "flag_and_pointer"
    /\ wpc = "cleared"
    /\ IF custody # "q1"
       THEN freed' = [freed EXCEPT !["q1"] = @ + 1]
       ELSE UNCHANGED freed
    /\ wpc' = "done"
    /\ UNCHANGED <<custody, consumer, abandoned, allocated, opc, sawConsumer, prevConsumer>>

\* ---- the opener of the next stream, under the slot lock ----

OpenerLook ==
    /\ opc = "idle"
    /\ sawConsumer' = consumer["q1"]
    /\ IF consumer["q1"]
       THEN opc' = "saw"
       ELSE opc' = "done"   \* reuse q1
    /\ UNCHANGED <<custody, consumer, abandoned, freed, allocated, wpc, prevConsumer>>

\* Design: set ABANDONED atomically, learning whether CONSUMER was still set.
OpenerFlag ==
    /\ opc = "saw"
    /\ IF Variant \in {"design", "no_recheck"}
       THEN /\ abandoned' = [abandoned EXCEPT !["q1"] = TRUE]
            /\ prevConsumer' = consumer["q1"]
            /\ opc' = "flagged"
       ELSE /\ UNCHANGED <<abandoned, prevConsumer>>
            /\ opc' = "flagged"
    /\ UNCHANGED <<custody, consumer, freed, allocated, wpc, sawConsumer>>

OpenerInstall ==
    /\ opc = "flagged"
    /\ IF Variant = "design" /\ ~prevConsumer
       THEN \* the writer left in between: reuse q1 after all
            /\ abandoned' = [abandoned EXCEPT !["q1"] = FALSE]
            /\ UNCHANGED <<custody, allocated>>
       ELSE /\ custody' = "q2"
            /\ allocated' = [allocated EXCEPT !["q2"] = TRUE]
            /\ UNCHANGED abandoned
    /\ opc' = "done"
    /\ UNCHANGED <<consumer, freed, wpc, sawConsumer, prevConsumer>>

Next ==
    \/ WriterClear \/ WriterCheckPointer
    \/ OpenerLook \/ OpenerFlag \/ OpenerInstall
    \/ (wpc = "done" /\ opc = "done" /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars /\ WF_vars(WriterClear) /\ WF_vars(WriterCheckPointer)
        /\ WF_vars(OpenerLook) /\ WF_vars(OpenerFlag) /\ WF_vars(OpenerInstall)

\* ---- properties ----

NeverFreedTwice == \A q \in Queues : freed[q] <= 1

\* A queue is never freed while the slot uses it. Inside the opener's
\* critical section the slot lock is held, and every use of a slot's queue
\* takes that lock, so the slot's pointer is not followed there.
NeverFreedInUse == opc \in {"idle", "done"} => freed[custody] = 0

\* Once both are done, a queue the slot no longer uses has been freed.
NoLeak == (wpc = "done" /\ opc = "done") =>
            \A q \in Queues : (allocated[q] /\ q # custody) => freed[q] = 1

\* An abandoned flag never outlives a reuse of its queue.
ReusedQueueIsClean == (opc = "done" /\ custody = "q1") => ~abandoned["q1"]
================================================================================
