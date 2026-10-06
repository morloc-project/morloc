------------------------------ MODULE WorkerExit ------------------------------
(* How a pool worker's references are recovered when it ends               *)
(* (model/shm.md SHM-8). A worker holds references for the call it serves  *)
(* (released at the end of the call) and may hold long-lived ones (stream  *)
(* caches, views). It ends by retiring when idle, by crashing at any time, *)
(* or by exiting with status 0 in the middle of a call (an error escaping  *)
(* its handler, a library's exit). The coordinator treats an exit as clean *)
(* only if the worker sent a clean-exit token, which it sends only when it *)
(* holds nothing; any other exit ends the pool, and the nexus recovers by  *)
(* discarding the whole shared namespace. Shutdown signals the pool's      *)
(* whole process group at once: every process's shutdown flag is set      *)
(* before any of them can exit, and a worker that sees its flag exits with *)
(* status 0 and no token. Such an exit is not a failure, so the            *)
(* coordinator consults its flag after reaping, not only before.           *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "status_inferred": an exit with status 0 counts as clean.
\* "retire_holding": a worker retires while holding long-lived references.
\* "no_recovery": an unclean exit is logged and the worker replaced.
\* "flag_before_reap": the coordinator checks its shutdown flag only before
\*   reaping.

(* --algorithm WorkerExit
variables
    callRefs = 0,
    longRefs = 0,
    gone = FALSE,
    status = "none",
    token = FALSE,
    reaped = FALSE,
    clean = FALSE,
    poolEnded = FALSE,
    recovered = FALSE,
    shutdown = FALSE,
    byShutdown = FALSE,
    falseAlarm = FALSE;

define
    Held == callRefs + longRefs
    NoCleanExitHoldsReferences == (reaped /\ clean) => Held = 0
    AnUncleanExitIsRecovered == (reaped /\ ~clean) ~> recovered
    AShutdownExitIsNoFailure == ~falseAlarm
end define;

fair process Worker = "worker"
begin
  Serve:
    either
      callRefs := 1;
      either
        longRefs := 1;
      or
        skip;
      end either;
    or
      goto Idle;
    end either;
  InCall:
    either
      callRefs := 0;
    or
      await shutdown;
      gone := TRUE;
      status := "zero";
      byShutdown := TRUE;
      goto Done;
    or
      \* An error escapes the handler, or a library exits: status 0.
      gone := TRUE;
      status := "zero";
      goto Done;
    or
      gone := TRUE;
      status := "signal";
      goto Done;
    end either;
  Idle:
    either
      await shutdown;
      gone := TRUE;
      status := "zero";
      byShutdown := TRUE;
    or
      if longRefs = 0 \/ Variant = "retire_holding" then
        token := TRUE;
        gone := TRUE;
        status := "zero";
      end if;
    or
      gone := TRUE;
      status := "signal";
    end either;
end process;

process Signal = "signal"
begin
  Send:
    either
      shutdown := TRUE;
    or
      skip;
    end either;
end process;

fair process Coordinator = "coord"
begin
  Top:
    if shutdown then
      goto Done;
    end if;
  Reap:
    \* A worker that cannot retire stays alive, serving.
    await gone \/ pc["worker"] = "Done";
    if ~gone then
      goto Done;
    end if;
  Classify:
    if Variant = "status_inferred" then
      clean := status = "zero";
    else
      clean := token;
    end if;
    reaped := TRUE;
  Decide:
    if ~clean /\ Variant /= "no_recovery" /\ (Variant = "flag_before_reap" \/ ~shutdown) then
      poolEnded := TRUE;
      falseAlarm := byShutdown;
    end if;
end process;

fair process Nexus = "nexus"
begin
  Recover:
    await poolEnded \/ shutdown \/ (reaped /\ clean) \/ (reaped /\ Variant = "no_recovery")
          \/ pc["coord"] = "Done";
    \* Shutdown discards the namespace too.
    if poolEnded \/ shutdown then
      recovered := TRUE;
    end if;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "9cd242cb" /\ chksum(tla) = "f4226adf")
VARIABLES callRefs, longRefs, gone, status, token, reaped, clean, poolEnded, 
          recovered, shutdown, byShutdown, falseAlarm, pc

(* define statement *)
Held == callRefs + longRefs
NoCleanExitHoldsReferences == (reaped /\ clean) => Held = 0
AnUncleanExitIsRecovered == (reaped /\ ~clean) ~> recovered
AShutdownExitIsNoFailure == ~falseAlarm


vars == << callRefs, longRefs, gone, status, token, reaped, clean, poolEnded, 
           recovered, shutdown, byShutdown, falseAlarm, pc >>

ProcSet == {"worker"} \cup {"signal"} \cup {"coord"} \cup {"nexus"}

Init == (* Global variables *)
        /\ callRefs = 0
        /\ longRefs = 0
        /\ gone = FALSE
        /\ status = "none"
        /\ token = FALSE
        /\ reaped = FALSE
        /\ clean = FALSE
        /\ poolEnded = FALSE
        /\ recovered = FALSE
        /\ shutdown = FALSE
        /\ byShutdown = FALSE
        /\ falseAlarm = FALSE
        /\ pc = [self \in ProcSet |-> CASE self = "worker" -> "Serve"
                                        [] self = "signal" -> "Send"
                                        [] self = "coord" -> "Top"
                                        [] self = "nexus" -> "Recover"]

Serve == /\ pc["worker"] = "Serve"
         /\ \/ /\ callRefs' = 1
               /\ \/ /\ longRefs' = 1
                  \/ /\ TRUE
                     /\ UNCHANGED longRefs
               /\ pc' = [pc EXCEPT !["worker"] = "InCall"]
            \/ /\ pc' = [pc EXCEPT !["worker"] = "Idle"]
               /\ UNCHANGED <<callRefs, longRefs>>
         /\ UNCHANGED << gone, status, token, reaped, clean, poolEnded, 
                         recovered, shutdown, byShutdown, falseAlarm >>

InCall == /\ pc["worker"] = "InCall"
          /\ \/ /\ callRefs' = 0
                /\ pc' = [pc EXCEPT !["worker"] = "Idle"]
                /\ UNCHANGED <<gone, status, byShutdown>>
             \/ /\ shutdown
                /\ gone' = TRUE
                /\ status' = "zero"
                /\ byShutdown' = TRUE
                /\ pc' = [pc EXCEPT !["worker"] = "Done"]
                /\ UNCHANGED callRefs
             \/ /\ gone' = TRUE
                /\ status' = "zero"
                /\ pc' = [pc EXCEPT !["worker"] = "Done"]
                /\ UNCHANGED <<callRefs, byShutdown>>
             \/ /\ gone' = TRUE
                /\ status' = "signal"
                /\ pc' = [pc EXCEPT !["worker"] = "Done"]
                /\ UNCHANGED <<callRefs, byShutdown>>
          /\ UNCHANGED << longRefs, token, reaped, clean, poolEnded, recovered, 
                          shutdown, falseAlarm >>

Idle == /\ pc["worker"] = "Idle"
        /\ \/ /\ shutdown
              /\ gone' = TRUE
              /\ status' = "zero"
              /\ byShutdown' = TRUE
              /\ token' = token
           \/ /\ IF longRefs = 0 \/ Variant = "retire_holding"
                    THEN /\ token' = TRUE
                         /\ gone' = TRUE
                         /\ status' = "zero"
                    ELSE /\ TRUE
                         /\ UNCHANGED << gone, status, token >>
              /\ UNCHANGED byShutdown
           \/ /\ gone' = TRUE
              /\ status' = "signal"
              /\ UNCHANGED <<token, byShutdown>>
        /\ pc' = [pc EXCEPT !["worker"] = "Done"]
        /\ UNCHANGED << callRefs, longRefs, reaped, clean, poolEnded, 
                        recovered, shutdown, falseAlarm >>

Worker == Serve \/ InCall \/ Idle

Send == /\ pc["signal"] = "Send"
        /\ \/ /\ shutdown' = TRUE
           \/ /\ TRUE
              /\ UNCHANGED shutdown
        /\ pc' = [pc EXCEPT !["signal"] = "Done"]
        /\ UNCHANGED << callRefs, longRefs, gone, status, token, reaped, clean, 
                        poolEnded, recovered, byShutdown, falseAlarm >>

Signal == Send

Top == /\ pc["coord"] = "Top"
       /\ IF shutdown
             THEN /\ pc' = [pc EXCEPT !["coord"] = "Done"]
             ELSE /\ pc' = [pc EXCEPT !["coord"] = "Reap"]
       /\ UNCHANGED << callRefs, longRefs, gone, status, token, reaped, clean, 
                       poolEnded, recovered, shutdown, byShutdown, falseAlarm >>

Reap == /\ pc["coord"] = "Reap"
        /\ gone \/ pc["worker"] = "Done"
        /\ IF ~gone
              THEN /\ pc' = [pc EXCEPT !["coord"] = "Done"]
              ELSE /\ pc' = [pc EXCEPT !["coord"] = "Classify"]
        /\ UNCHANGED << callRefs, longRefs, gone, status, token, reaped, clean, 
                        poolEnded, recovered, shutdown, byShutdown, falseAlarm >>

Classify == /\ pc["coord"] = "Classify"
            /\ IF Variant = "status_inferred"
                  THEN /\ clean' = (status = "zero")
                  ELSE /\ clean' = token
            /\ reaped' = TRUE
            /\ pc' = [pc EXCEPT !["coord"] = "Decide"]
            /\ UNCHANGED << callRefs, longRefs, gone, status, token, poolEnded, 
                            recovered, shutdown, byShutdown, falseAlarm >>

Decide == /\ pc["coord"] = "Decide"
          /\ IF ~clean /\ Variant /= "no_recovery" /\ (Variant = "flag_before_reap" \/ ~shutdown)
                THEN /\ poolEnded' = TRUE
                     /\ falseAlarm' = byShutdown
                ELSE /\ TRUE
                     /\ UNCHANGED << poolEnded, falseAlarm >>
          /\ pc' = [pc EXCEPT !["coord"] = "Done"]
          /\ UNCHANGED << callRefs, longRefs, gone, status, token, reaped, 
                          clean, recovered, shutdown, byShutdown >>

Coordinator == Top \/ Reap \/ Classify \/ Decide

Recover == /\ pc["nexus"] = "Recover"
           /\ poolEnded \/ shutdown \/ (reaped /\ clean) \/ (reaped /\ Variant = "no_recovery")
              \/ pc["coord"] = "Done"
           /\ IF poolEnded \/ shutdown
                 THEN /\ recovered' = TRUE
                 ELSE /\ TRUE
                      /\ UNCHANGED recovered
           /\ pc' = [pc EXCEPT !["nexus"] = "Done"]
           /\ UNCHANGED << callRefs, longRefs, gone, status, token, reaped, 
                           clean, poolEnded, shutdown, byShutdown, falseAlarm >>

Nexus == Recover

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Worker \/ Signal \/ Coordinator \/ Nexus
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Worker)
        /\ WF_vars(Coordinator)
        /\ WF_vars(Nexus)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
