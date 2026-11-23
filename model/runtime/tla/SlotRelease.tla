----------------------------- MODULE SlotRelease -----------------------------
(* Who releases a written stream's registry slot once its close is queued  *)
(* (model/runtime/streams.md SLOT-19).                                    *)
(* A close, under the slot lock, checks that the slot still holds its       *)
(* stream and queues a close marker, naming itself the closer. A waiting    *)
(* closer (a user @close) waits until the custodian has written the footer  *)
(* and then releases the slot; it may die first. A detached closer (the     *)
(* sweep at the end of a call) returns at once. The custodian writes the    *)
(* footer at the marker, then releases the slot itself when the closer is   *)
(* detached or dead, and otherwise waits until the slot is released or      *)
(* reused. Every release checks, under the slot lock, that the slot still   *)
(* holds the stream it was closing. A released slot may be reopened at      *)
(* once for a new stream. The process hosting the custodian may stop it at  *)
(* any time; a custodian waiting after the footer then leaves, releasing    *)
(* the slot first if its closer is gone. (A stop that reaches the custodian *)
(* before the footer is SLOT-13's and outside this model.)                  *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "detached_waits": the custodian treats a detached closer like a live one.
\* "detached_releases": a detached closer releases the slot after queueing.
\* "stop_first": the waiting custodian leaves on a stop before it looks at
\*   its closer.
\* "closer_then_stop": it reads the stop word after it looks at its closer.

Closers == {"user", "sweep"}
Mode(c) == IF c = "user" THEN "wait" ELSE "detached"

VARIABLES
    gen,       \* the stream the slot holds; 0 when free
    queued,    \* closer -> it queued the close of stream 1
    closer,    \* the closer the queue names: "none" | "user" | "sweep"
    footer,    \* the custodian wrote stream 1's footer
    cpc,       \* closer -> "idle" | "waiting" | "done" | "dead"
    kpc,       \* custodian: "serving" | "wrote" | "check" | "stopread" | "done"
    releases,  \* releases of stream 1's slot
    reopened,
    stopped,   \* the custodian's process stopped it
    ksaw,      \* the stop word as the waiting custodian last read it
    goneAtStop \* the closer was gone when the stop came

vars == <<gen, queued, closer, footer, cpc, kpc, releases, reopened, stopped, ksaw, goneAtStop>>

Init ==
    /\ gen = 1
    /\ queued = [c \in Closers |-> FALSE]
    /\ closer = "none"
    /\ footer = FALSE
    /\ cpc = [c \in Closers |-> "idle"]
    /\ kpc = "serving"
    /\ releases = 0
    /\ reopened = FALSE
    /\ stopped = FALSE
    /\ ksaw = FALSE
    /\ goneAtStop = FALSE

\* A release by a party closing stream 1.
Release ==
    IF gen = 1
    THEN /\ gen' = 0
         /\ releases' = releases + 1
    ELSE UNCHANGED <<gen, releases>>

\* Under the slot lock: the stream is still open and no close is queued.
Close(c) ==
    /\ cpc[c] = "idle"
    /\ IF gen = 1 /\ closer = "none"
       THEN /\ queued' = [queued EXCEPT ![c] = TRUE]
            /\ closer' = c
            /\ cpc' = [cpc EXCEPT ![c] =
                         IF Mode(c) = "wait" THEN "waiting"
                         ELSE IF Variant = "detached_releases" THEN "waiting"
                         ELSE "done"]
       ELSE /\ cpc' = [cpc EXCEPT ![c] = "done"]
            /\ UNCHANGED <<queued, closer>>
    /\ UNCHANGED <<gen, footer, kpc, releases, reopened, stopped, ksaw, goneAtStop>>

\* A waiting closer releases once the footer is written; the broken detached
\* variant releases without waiting.
CloserRelease(c) ==
    /\ cpc[c] = "waiting"
    /\ footer \/ (Variant = "detached_releases" /\ Mode(c) = "detached")
    /\ Release
    /\ cpc' = [cpc EXCEPT ![c] = "done"]
    /\ UNCHANGED <<queued, closer, footer, kpc, reopened, stopped, ksaw, goneAtStop>>

CloserDies(c) ==
    /\ cpc[c] = "waiting"
    /\ Mode(c) = "wait"
    /\ cpc' = [cpc EXCEPT ![c] = "dead"]
    /\ UNCHANGED <<gen, queued, closer, footer, kpc, releases, reopened, stopped, ksaw, goneAtStop>>

WriteFooter ==
    /\ kpc = "serving"
    /\ closer # "none"
    /\ footer' = TRUE
    /\ kpc' = "wrote"
    /\ UNCHANGED <<gen, queued, closer, cpc, releases, reopened, stopped, ksaw, goneAtStop>>

CloserGone ==
    \/ cpc[closer] = "dead"
    \/ Mode(closer) = "detached" /\ Variant # "detached_waits"

\* The custodian, waiting after the footer, looks in turn at the slot, its
\* stop word and its closer: it leaves once the slot no longer holds stream
\* 1, releases for a closer that is gone or detached, and otherwise leaves if
\* the stop word it read was set. The design reads the stop word before it
\* looks at the closer.
CustodianLeaves ==
    /\ kpc = "wrote"
    /\ gen # 1
    /\ kpc' = "done"
    /\ UNCHANGED <<gen, queued, closer, footer, cpc, releases, reopened, stopped, ksaw, goneAtStop>>

CustodianLook ==
    /\ kpc = "wrote"
    /\ gen = 1
    /\ IF Variant = "stop_first" /\ stopped
       THEN /\ kpc' = "done"
            /\ UNCHANGED ksaw
       ELSE /\ kpc' = "check"
            /\ ksaw' = (Variant = "design" /\ stopped)
    /\ UNCHANGED <<gen, queued, closer, footer, cpc, releases, reopened, stopped, goneAtStop>>

CustodianRelease ==
    /\ kpc = "check"
    /\ CloserGone
    /\ Release
    /\ kpc' = "done"
    /\ UNCHANGED <<queued, closer, footer, cpc, reopened, stopped, ksaw, goneAtStop>>

CustodianWaits ==
    /\ kpc = "check"
    /\ ~CloserGone
    /\ kpc' = IF Variant = "closer_then_stop" THEN "stopread" ELSE IF ksaw THEN "done" ELSE "wrote"
    /\ UNCHANGED <<gen, queued, closer, footer, cpc, releases, reopened, stopped, ksaw, goneAtStop>>

\* closer_then_stop: the stop word, read after the look at the closer.
CustodianLateStopRead ==
    /\ kpc = "stopread"
    /\ kpc' = IF stopped THEN "done" ELSE "wrote"
    /\ UNCHANGED <<gen, queued, closer, footer, cpc, releases, reopened, stopped, ksaw, goneAtStop>>

Stop ==
    /\ ~stopped
    /\ stopped' = TRUE
    /\ goneAtStop' = (closer # "none" /\ CloserGone)
    /\ UNCHANGED <<gen, queued, closer, footer, cpc, kpc, releases, reopened, ksaw>>

Reopen ==
    /\ gen = 0
    /\ ~reopened
    /\ gen' = 2
    /\ reopened' = TRUE
    /\ UNCHANGED <<queued, closer, footer, cpc, kpc, releases, stopped, ksaw, goneAtStop>>

Next ==
    \/ \E c \in Closers : Close(c) \/ CloserRelease(c) \/ CloserDies(c)
    \/ WriteFooter \/ CustodianLeaves \/ CustodianLook \/ CustodianRelease \/ CustodianWaits
    \/ CustodianLateStopRead \/ Reopen \/ Stop
    \/ UNCHANGED vars

Spec ==
    /\ Init /\ [][Next]_vars
    /\ \A c \in Closers : WF_vars(Close(c)) /\ WF_vars(CloserRelease(c))
    /\ WF_vars(WriteFooter) /\ WF_vars(CustodianLeaves) /\ WF_vars(CustodianLook)
    /\ WF_vars(CustodianRelease) /\ WF_vars(CustodianWaits) /\ WF_vars(CustodianLateStopRead)

\* ---- properties ----

ReleasedOnce == releases <= 1
NeverReleasedBeforeTheFooter == (releases > 0) => footer
\* Once a close is queued, the slot is released, unless a stop ended the
\* custodian while its closer lived.
AQueuedCloseFreesTheSlot == (closer # "none") ~> (releases = 1 \/ (stopped /\ kpc = "done"))
\* A stop that comes after the closer is gone frees the slot.
AStopAfterTheCloserFreesTheSlot == goneAtStop ~> (releases = 1)
================================================================================
