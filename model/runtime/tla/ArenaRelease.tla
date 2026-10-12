----------------------------- MODULE ArenaRelease -----------------------------
(* One block's references on a thread with an eval arena (model/runtime/shm.md  *)
(* SHM-7). A stream's sub-packet cache is the example holder: a miss allocates *)
(* the block (the eval's reference, recorded in the arena) and the cache takes *)
(* a reference of its own; a hit gives the eval another reference. The eval   *)
(* frees a reference it holds, which drops one arena entry for the block, or  *)
(* leaves it to the arena, which releases what it still records when it ends. *)
(* The cache releases its reference when it evicts. The eval may also hand a  *)
(* reference to a holder that releases it itself (an Arrow view adopting the  *)
(* caller's reference, a cell). All on one thread.                           *)
EXTENDS Naturals

CONSTANT Variant, Hits
\* "design": the design.
\* "evict_forgets": the cache's release also drops an arena entry.
\* "hit_unrecorded": a hit's reference is not recorded in the arena.
\* "handoff_keeps_entry": a hand-off leaves the arena's entry in place.

VARIABLES
    count,     \* the block's reference count
    entries,   \* arena entries for the block
    rec,       \* references the eval holds that an arena entry stands for
    unrec,     \* references the eval holds that no arena entry stands for
    cached,    \* the cache holds a reference
    view,      \* references handed to a holder that releases them itself
    hits,      \* hits so far
    allocated, \* the miss has happened
    ended,     \* the arena has ended
    underflow, \* a release found the count at zero
    early      \* the block was freed while a holder still held it

vars == <<count, entries, rec, unrec, cached, view, hits, allocated, ended, underflow, early>>

Init ==
    /\ count = 0 /\ entries = 0 /\ rec = 0 /\ unrec = 0
    /\ cached = FALSE /\ view = 0 /\ hits = 0 /\ allocated = FALSE /\ ended = FALSE
    /\ underflow = FALSE /\ early = FALSE

\* References someone still means to use or release, after a change.
HeldAfter(r, u, c, v) == r + u + v + (IF c THEN 1 ELSE 0)

\* Release k references; flag underflow, and a free under a live holder.
Release(k, held) ==
    /\ underflow' = (underflow \/ count < k)
    /\ count' = IF count < k THEN 0 ELSE count - k
    /\ early' = (early \/ (count >= k /\ count - k = 0 /\ held > 0))

Miss ==
    /\ ~allocated /\ ~ended
    /\ allocated' = TRUE
    /\ count' = 2 /\ entries' = 1 /\ rec' = 1 /\ cached' = TRUE
    /\ UNCHANGED <<unrec, view, hits, ended, underflow, early>>

Hit ==
    /\ cached /\ ~ended /\ hits < Hits
    /\ hits' = hits + 1
    /\ count' = count + 1
    /\ IF Variant = "hit_unrecorded"
       THEN /\ unrec' = unrec + 1 /\ UNCHANGED <<rec, entries>>
       ELSE /\ rec' = rec + 1 /\ entries' = entries + 1 /\ UNCHANGED unrec
    /\ UNCHANGED <<cached, view, allocated, ended, underflow, early>>

\* The eval frees one of its references, which drops one arena entry for
\* the block if there is one, whichever reference it frees.
EvalFree ==
    /\ ~ended
    /\ \/ /\ rec > 0 /\ rec' = rec - 1 /\ UNCHANGED unrec
       \/ /\ unrec > 0 /\ unrec' = unrec - 1 /\ UNCHANGED rec
    /\ entries' = IF entries > 0 THEN entries - 1 ELSE 0
    /\ Release(1, HeldAfter(rec', unrec', cached, view))
    /\ UNCHANGED <<cached, view, hits, allocated, ended>>

\* The eval drops a reference without freeing it: a recorded one is the
\* arena's to release; an unrecorded one is lost.
EvalLeave ==
    /\ ~ended
    /\ \/ /\ rec > 0 /\ rec' = rec - 1 /\ UNCHANGED unrec
       \/ /\ unrec > 0 /\ unrec' = unrec - 1 /\ UNCHANGED rec
    /\ UNCHANGED <<count, entries, cached, view, hits, allocated, ended, underflow, early>>

Evict ==
    /\ cached
    /\ cached' = FALSE
    /\ entries' = IF Variant = "evict_forgets" /\ entries > 0 THEN entries - 1 ELSE entries
    /\ Release(1, HeldAfter(rec, unrec, FALSE, view))
    /\ UNCHANGED <<rec, unrec, view, hits, allocated, ended>>

\* The eval hands a recorded reference to a holder, which drops its entry.
HandOff ==
    /\ rec > 0 /\ ~ended
    /\ rec' = rec - 1 /\ view' = view + 1
    /\ entries' = IF Variant = "handoff_keeps_entry" THEN entries ELSE entries - 1
    /\ UNCHANGED <<count, unrec, cached, hits, allocated, ended, underflow, early>>

\* That holder releases its reference, never touching the arena.
ViewRelease ==
    /\ view > 0
    /\ view' = view - 1
    /\ Release(1, HeldAfter(rec, unrec, cached, view - 1))
    /\ UNCHANGED <<entries, rec, unrec, cached, hits, allocated, ended>>

\* The arena ends once the eval holds nothing.
End ==
    /\ ~ended /\ allocated /\ rec = 0 /\ unrec = 0
    /\ ended' = TRUE /\ entries' = 0
    /\ Release(entries, HeldAfter(0, 0, cached, view))
    /\ UNCHANGED <<rec, unrec, cached, view, hits, allocated>>

Done == ended /\ ~cached /\ view = 0

Next == Miss \/ Hit \/ EvalFree \/ EvalLeave \/ HandOff \/ ViewRelease \/ Evict \/ End
        \/ (Done /\ UNCHANGED vars)

Spec == Init /\ [][Next]_vars

\* ---- properties ----
NoUnderflow == ~underflow
NoFreeUnderAHolder == ~early
NoLeak == Done => count = 0
================================================================================
