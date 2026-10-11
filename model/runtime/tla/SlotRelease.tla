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
(* once for a new stream.                                                   *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "detached_waits": the custodian treats a detached closer like a live one.
\* "detached_releases": a detached closer releases the slot after queueing.

Closers == {"user", "sweep"}
Mode(c) == IF c = "user" THEN "wait" ELSE "detached"

VARIABLES
    gen,       \* the stream the slot holds; 0 when free
    queued,    \* closer -> it queued the close of stream 1
    closer,    \* the closer the queue names: "none" | "user" | "sweep"
    footer,    \* the custodian wrote stream 1's footer
    cpc,       \* closer -> "idle" | "waiting" | "done" | "dead"
    kpc,       \* custodian: "serving" | "wrote" | "done"
    releases,  \* releases of stream 1's slot
    reopened

vars == <<gen, queued, closer, footer, cpc, kpc, releases, reopened>>

Init ==
    /\ gen = 1
    /\ queued = [c \in Closers |-> FALSE]
    /\ closer = "none"
    /\ footer = FALSE
    /\ cpc = [c \in Closers |-> "idle"]
    /\ kpc = "serving"
    /\ releases = 0
    /\ reopened = FALSE

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
    /\ UNCHANGED <<gen, footer, kpc, releases, reopened>>

\* A waiting closer releases once the footer is written; the broken detached
\* variant releases without waiting.
CloserRelease(c) ==
    /\ cpc[c] = "waiting"
    /\ footer \/ (Variant = "detached_releases" /\ Mode(c) = "detached")
    /\ Release
    /\ cpc' = [cpc EXCEPT ![c] = "done"]
    /\ UNCHANGED <<queued, closer, footer, kpc, reopened>>

CloserDies(c) ==
    /\ cpc[c] = "waiting"
    /\ Mode(c) = "wait"
    /\ cpc' = [cpc EXCEPT ![c] = "dead"]
    /\ UNCHANGED <<gen, queued, closer, footer, kpc, releases, reopened>>

WriteFooter ==
    /\ kpc = "serving"
    /\ closer # "none"
    /\ footer' = TRUE
    /\ kpc' = "wrote"
    /\ UNCHANGED <<gen, queued, closer, cpc, releases, reopened>>

CloserGone ==
    \/ cpc[closer] = "dead"
    \/ Mode(closer) = "detached" /\ Variant # "detached_waits"

\* The custodian releases for a closer that is gone or detached, and stops
\* once the slot no longer holds stream 1.
CustodianRelease ==
    /\ kpc = "wrote"
    /\ CloserGone
    /\ Release
    /\ kpc' = "done"
    /\ UNCHANGED <<queued, closer, footer, cpc, reopened>>

CustodianLeaves ==
    /\ kpc = "wrote"
    /\ gen # 1
    /\ kpc' = "done"
    /\ UNCHANGED <<gen, queued, closer, footer, cpc, releases, reopened>>

Reopen ==
    /\ gen = 0
    /\ ~reopened
    /\ gen' = 2
    /\ reopened' = TRUE
    /\ UNCHANGED <<queued, closer, footer, cpc, kpc, releases>>

Next ==
    \/ \E c \in Closers : Close(c) \/ CloserRelease(c) \/ CloserDies(c)
    \/ WriteFooter \/ CustodianRelease \/ CustodianLeaves \/ Reopen
    \/ UNCHANGED vars

Spec ==
    /\ Init /\ [][Next]_vars
    /\ \A c \in Closers : WF_vars(Close(c)) /\ WF_vars(CloserRelease(c))
    /\ WF_vars(WriteFooter) /\ WF_vars(CustodianRelease) /\ WF_vars(CustodianLeaves)

\* ---- properties ----

ReleasedOnce == releases <= 1
NeverReleasedBeforeTheFooter == (releases > 0) => footer
\* Once a close is queued, the slot is released.
AQueuedCloseFreesTheSlot == (closer # "none") ~> (releases = 1)
================================================================================
