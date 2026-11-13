-------------------------- MODULE WriteBufferHandoff --------------------------
(* Who releases a stream's write buffer when its writer dies mid-handoff.   *)
(* Under the slot lock the writer takes a fresh buffer (the queue's spare,  *)
(* or a new block), pushes its sealed buffer onto the custody queue, then   *)
(* installs the fresh one in the slot. The custodian pops a sealed buffer,  *)
(* writes it, and keeps it as the queue's spare or frees it. The slot is    *)
(* reclaimed under its lock; a writer that died holding the lock leaves it  *)
(* to the next taker as a dead owner, which poisons the slot, and the       *)
(* reclaim of a poisoned slot frees none of its blocks. An unrelated        *)
(* allocator claims any free block, so a stale free lands on a live one.    *)
(* Ghost state `refs` names the holders that own each block's references;  *)
(* a free on behalf of a holder that owns none is a free of someone else's *)
(* reference, whatever the count says.                                     *)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANT Variant
\* "design": the design.
\* "free_on_poison": the reclaim frees a poisoned slot's blocks.
\* "unlocked_reclaim": the reclaim does not take the slot lock, so it never
\*   learns that the writer died holding it.

Blocks == {"b1", "b2", "b3", "b4"}
None == "none"

VARIABLES
    count,     \* block -> reference count
    refs,      \* block -> holders owning its references (ghost)
    wb,        \* the block the slot's write-buffer field names
    fresh,     \* the writer's fresh block, once taken
    spare,     \* the queue's spare block, or None
    queue,     \* sealed blocks queued for the custodian
    popped,    \* the block the custodian is writing, or None
    wpc,       \* writer: "take" | "push" | "install" | "done" | "dead"
    lockDead,  \* the writer died holding the slot lock
    poisoned,  \* the slot is poisoned
    rpc,       \* the reclaim: "idle" | "slot" | "spare" | "done"
    foreign,   \* a free was made for a holder owning no reference
    refused    \* a free found its block already free

vars == <<count, refs, wb, fresh, spare, queue, popped, wpc, lockDead,
          poisoned, rpc, foreign, refused>>

Init ==
    \E s \in {None, "b2"} :
        /\ spare = s
        /\ count = [b \in Blocks |-> IF b = "b1" \/ b = s THEN 1 ELSE 0]
        /\ refs = [b \in Blocks |->
                     IF b = "b1" THEN {"slot"}
                     ELSE IF b = s THEN {"spare"} ELSE {}]
        /\ wb = "b1"
        /\ fresh = None
        /\ queue = <<>>
        /\ popped = None
        /\ wpc = "take"
        /\ lockDead = FALSE
        /\ poisoned = FALSE
        /\ rpc = "idle"
        /\ foreign = FALSE
        /\ refused = FALSE

\* Release one reference to `b` on behalf of holder `h`.
Free(b, h) ==
    /\ count' = [count EXCEPT ![b] = IF @ > 0 THEN @ - 1 ELSE 0]
    /\ refs' = [refs EXCEPT ![b] = @ \ {h}]
    /\ foreign' = (foreign \/ h \notin refs[b])
    /\ refused' = (refused \/ count[b] = 0)

\* ---- the writer, holding the slot lock ----

TakeSpare ==
    /\ wpc = "take" /\ spare # None
    /\ fresh' = spare
    /\ refs' = [refs EXCEPT ![spare] = (@ \ {"spare"}) \cup {"fresh"}]
    /\ spare' = None
    /\ wpc' = "push"
    /\ UNCHANGED <<count, wb, queue, popped, lockDead, poisoned, rpc, foreign, refused>>

TakeNew ==
    /\ wpc = "take" /\ spare = None
    /\ \E b \in Blocks :
        /\ count[b] = 0
        /\ count' = [count EXCEPT ![b] = 1]
        /\ refs' = [refs EXCEPT ![b] = {"fresh"}]
        /\ fresh' = b
    /\ wpc' = "push"
    /\ UNCHANGED <<wb, spare, queue, popped, lockDead, poisoned, rpc, foreign, refused>>

\* The queue item takes the slot's reference; the slot field still names the
\* block until the install overwrites it.
Push ==
    /\ wpc = "push"
    /\ queue' = Append(queue, wb)
    /\ refs' = [refs EXCEPT ![wb] = (@ \ {"slot"}) \cup {"queue"}]
    /\ wpc' = "install"
    /\ UNCHANGED <<count, wb, fresh, spare, popped, lockDead, poisoned, rpc, foreign, refused>>

Install ==
    /\ wpc = "install"
    /\ wb' = fresh
    /\ refs' = [refs EXCEPT ![fresh] = (@ \ {"fresh"}) \cup {"slot"}]
    /\ wpc' = "done"
    /\ UNCHANGED <<count, fresh, spare, queue, popped, lockDead, poisoned, rpc, foreign, refused>>

\* Death at any step while the lock is held.
Die ==
    /\ wpc \in {"take", "push", "install"}
    /\ wpc' = "dead"
    /\ lockDead' = TRUE
    /\ UNCHANGED <<count, refs, wb, fresh, spare, queue, popped, poisoned, rpc, foreign, refused>>

\* ---- the custodian ----

Pop ==
    /\ popped = None /\ queue # <<>>
    /\ popped' = Head(queue)
    /\ queue' = Tail(queue)
    /\ refs' = [refs EXCEPT ![Head(queue)] = (@ \ {"queue"}) \cup {"custodian"}]
    /\ UNCHANGED <<count, wb, fresh, spare, wpc, lockDead, poisoned, rpc, foreign, refused>>

KeepSpare ==
    /\ popped # None /\ spare = None
    /\ spare' = popped
    /\ refs' = [refs EXCEPT ![popped] = (@ \ {"custodian"}) \cup {"spare"}]
    /\ popped' = None
    /\ UNCHANGED <<count, wb, fresh, queue, wpc, lockDead, poisoned, rpc, foreign, refused>>

Release ==
    /\ popped # None
    /\ Free(popped, "custodian")
    /\ popped' = None
    /\ UNCHANGED <<wb, fresh, spare, queue, wpc, lockDead, poisoned, rpc>>

\* ---- the reclaim, under the slot lock, once the writer is gone and the
\* queue is written ----

\* Taking the lock learns of a writer that died holding it, and poisons.
Reclaim ==
    /\ wpc \in {"done", "dead"} /\ rpc = "idle"
    /\ queue = <<>> /\ popped = None
    /\ LET sees == IF Variant = "unlocked_reclaim" THEN poisoned ELSE poisoned \/ lockDead
       IN /\ poisoned' = sees
          /\ rpc' = IF sees /\ Variant # "free_on_poison" THEN "done" ELSE "slot"
    /\ UNCHANGED <<count, refs, wb, fresh, spare, queue, popped, wpc, lockDead, foreign, refused>>

ReclaimSlot ==
    /\ rpc = "slot"
    /\ Free(wb, "slot")
    /\ rpc' = "spare"
    /\ UNCHANGED <<wb, fresh, spare, queue, popped, wpc, lockDead, poisoned>>

ReclaimSpare ==
    /\ rpc = "spare"
    /\ IF spare # None
       THEN /\ Free(spare, "spare")
            /\ spare' = None
       ELSE UNCHANGED <<count, refs, foreign, refused, spare>>
    /\ rpc' = "done"
    /\ UNCHANGED <<wb, fresh, queue, popped, wpc, lockDead, poisoned>>

\* ---- an unrelated holder claims and frees any free block ----

OtherClaim ==
    /\ \E b \in Blocks :
        /\ count[b] = 0
        /\ count' = [count EXCEPT ![b] = 1]
        /\ refs' = [refs EXCEPT ![b] = {"other"}]
    /\ UNCHANGED <<wb, fresh, spare, queue, popped, wpc, lockDead, poisoned, rpc, foreign, refused>>

OtherFree ==
    /\ \E b \in Blocks :
        /\ "other" \in refs[b]
        /\ Free(b, "other")
    /\ UNCHANGED <<wb, fresh, spare, queue, popped, wpc, lockDead, poisoned, rpc>>

Next ==
    \/ TakeSpare \/ TakeNew \/ Push \/ Install \/ Die
    \/ Pop \/ KeepSpare \/ Release
    \/ Reclaim \/ ReclaimSlot \/ ReclaimSpare
    \/ OtherClaim \/ OtherFree
    \/ (wpc \in {"done", "dead"} /\ rpc = "done" /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars

\* ---- properties ----

\* Every release is made by a holder that owns a reference to the block.
NoForeignFree == ~foreign

\* No free finds its block already free.
NoRefusedFree == ~refused

\* A block's count is the number of holders owning a reference to it.
CountsMatchHolders == \A b \in Blocks : count[b] = Cardinality(refs[b])

\* A writer that finished leaks nothing: after the reclaim, only unrelated
\* holders hold references.
NoLeakWithoutDeath ==
    (rpc = "done" /\ wpc = "done") =>
        \A b \in Blocks : refs[b] \subseteq {"other"}
================================================================================
