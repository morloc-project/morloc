------------------------------- MODULE AheadJob -------------------------------
(* Sub-packets decoded ahead by a background job (model/runtime/streams.md    *)
(* SLOT-27). The owner of a reader slot allocates the blocks of a pass,       *)
(* copies their compressed bytes out of the mapping and starts a job; the job *)
(* decodes into the blocks and finishes; the owner joins it before it serves, *)
(* releases or drops any of them. The job frees nothing and reads only what  *)
(* it was given. Blocks are the slot cache's (SHM-7).                        *)
EXTENDS Naturals, FiniteSets

CONSTANT Variant, Blocks
\* "design": the design.
\* "drop_without_join": the slot drops (releasing its blocks) while a job runs.
\* "serve_before_join": a block is served before its job is joined.

VARIABLES
    st,        \* block -> "unused", "decoding", "decoded", "cached", "served", "released"
    job,       \* "idle", "running", "finished"
    late,      \* the job wrote a block after it was released
    partial    \* a block was served before its job wrote it

vars == <<st, job, late, partial>>

Init ==
    /\ st = [b \in Blocks |-> "unused"]
    /\ job = "idle"
    /\ late = FALSE
    /\ partial = FALSE

Schedule ==
    /\ job = "idle"
    /\ \A b \in Blocks : st[b] = "unused"
    /\ st' = [b \in Blocks |-> "decoding"]
    /\ job' = "running"
    /\ UNCHANGED <<late, partial>>

Write(b) ==
    /\ job = "running"
    /\ st[b] \in {"decoding", "released"}
    /\ late' = (late \/ st[b] = "released")
    /\ st' = [st EXCEPT ![b] = IF st[b] = "decoding" THEN "decoded" ELSE "released"]
    /\ UNCHANGED <<job, partial>>

Finish ==
    /\ job = "running"
    /\ \A b \in Blocks : st[b] # "decoding"
    /\ job' = "finished"
    /\ UNCHANGED <<st, late, partial>>

Join ==
    /\ job = "finished"
    /\ job' = "idle"
    /\ st' = [b \in Blocks |-> IF st[b] = "decoded" THEN "cached" ELSE st[b]]
    /\ UNCHANGED <<late, partial>>

Serve(b) ==
    /\ IF Variant = "serve_before_join"
       THEN st[b] \in {"cached", "decoding", "decoded"}
       ELSE st[b] = "cached"
    /\ partial' = (partial \/ st[b] = "decoding")
    /\ st' = [st EXCEPT ![b] = "served"]
    /\ UNCHANGED <<job, late>>

\* The owner drops the slot: it joins a job first, then releases its blocks.
Drop ==
    /\ IF Variant = "drop_without_join" THEN TRUE ELSE job # "running"
    /\ \E b \in Blocks : st[b] \in {"decoding", "decoded", "cached"}
    /\ st' = [b \in Blocks |-> IF st[b] \in {"decoding", "decoded", "cached"} THEN "released" ELSE st[b]]
    /\ job' = IF job = "finished" THEN "idle" ELSE job
    /\ UNCHANGED <<late, partial>>

\* A pass is over once its blocks are served or released; the next reuses
\* nothing of it.
Recycle ==
    /\ job = "idle"
    /\ \A b \in Blocks : st[b] \in {"served", "released"}
    /\ st' = [b \in Blocks |-> "unused"]
    /\ UNCHANGED <<job, late, partial>>

Next ==
    \/ Schedule
    \/ Recycle
    \/ \E b \in Blocks : Write(b)
    \/ Finish
    \/ Join
    \/ \E b \in Blocks : Serve(b)
    \/ Drop

Spec == Init /\ [][Next]_vars

NoWriteAfterRelease == ~late
NoPartialServe == ~partial
================================================================================
