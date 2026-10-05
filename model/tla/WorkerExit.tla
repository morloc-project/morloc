------------------------------ MODULE WorkerExit ------------------------------
(* How a pool worker's references are recovered when it ends               *)
(* (model/shm.md SHM-8). A worker holds references for the call it serves  *)
(* (released at the end of the call) and may hold long-lived ones (stream  *)
(* caches, views). It ends by retiring when idle, by crashing at any time, *)
(* or by exiting with status 0 in the middle of a call (an error escaping  *)
(* its handler, a library's exit). The coordinator treats an exit as clean *)
(* only if the worker sent a clean-exit token, which it sends only when it *)
(* holds nothing; any other exit ends the pool, and the nexus recovers by  *)
(* discarding the whole shared namespace.                                  *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "status_inferred": an exit with status 0 counts as clean.
\* "retire_holding": a worker retires while holding long-lived references.
\* "no_recovery": an unclean exit is logged and the worker replaced.

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
    recovered = FALSE;

define
    Held == callRefs + longRefs
    NoCleanExitHoldsReferences == (reaped /\ clean) => Held = 0
    AnUncleanExitIsRecovered == (reaped /\ ~clean) ~> recovered
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

fair process Coordinator = "coord"
begin
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
    if ~clean /\ Variant /= "no_recovery" then
      poolEnded := TRUE;
    end if;
end process;

fair process Nexus = "nexus"
begin
  Recover:
    await poolEnded \/ (reaped /\ clean) \/ (reaped /\ Variant = "no_recovery")
          \/ pc["coord"] = "Done";
    if poolEnded then
      recovered := TRUE;
    end if;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "ecd2f8d6" /\ chksum(tla) = "a0d5cab4")
VARIABLES callRefs, longRefs, gone, status, token, reaped, clean, poolEnded, 
          recovered, pc

(* define statement *)
Held == callRefs + longRefs
NoCleanExitHoldsReferences == (reaped /\ clean) => Held = 0
AnUncleanExitIsRecovered == (reaped /\ ~clean) ~> recovered


vars == << callRefs, longRefs, gone, status, token, reaped, clean, poolEnded, 
           recovered, pc >>

ProcSet == {"worker"} \cup {"coord"} \cup {"nexus"}

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
        /\ pc = [self \in ProcSet |-> CASE self = "worker" -> "Serve"
                                        [] self = "coord" -> "Reap"
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
                         recovered >>

InCall == /\ pc["worker"] = "InCall"
          /\ \/ /\ callRefs' = 0
                /\ pc' = [pc EXCEPT !["worker"] = "Idle"]
                /\ UNCHANGED <<gone, status>>
             \/ /\ gone' = TRUE
                /\ status' = "zero"
                /\ pc' = [pc EXCEPT !["worker"] = "Done"]
                /\ UNCHANGED callRefs
             \/ /\ gone' = TRUE
                /\ status' = "signal"
                /\ pc' = [pc EXCEPT !["worker"] = "Done"]
                /\ UNCHANGED callRefs
          /\ UNCHANGED << longRefs, token, reaped, clean, poolEnded, recovered >>

Idle == /\ pc["worker"] = "Idle"
        /\ \/ /\ IF longRefs = 0 \/ Variant = "retire_holding"
                    THEN /\ token' = TRUE
                         /\ gone' = TRUE
                         /\ status' = "zero"
                    ELSE /\ TRUE
                         /\ UNCHANGED << gone, status, token >>
           \/ /\ gone' = TRUE
              /\ status' = "signal"
              /\ token' = token
        /\ pc' = [pc EXCEPT !["worker"] = "Done"]
        /\ UNCHANGED << callRefs, longRefs, reaped, clean, poolEnded, 
                        recovered >>

Worker == Serve \/ InCall \/ Idle

Reap == /\ pc["coord"] = "Reap"
        /\ gone \/ pc["worker"] = "Done"
        /\ IF ~gone
              THEN /\ pc' = [pc EXCEPT !["coord"] = "Done"]
              ELSE /\ pc' = [pc EXCEPT !["coord"] = "Classify"]
        /\ UNCHANGED << callRefs, longRefs, gone, status, token, reaped, clean, 
                        poolEnded, recovered >>

Classify == /\ pc["coord"] = "Classify"
            /\ IF Variant = "status_inferred"
                  THEN /\ clean' = (status = "zero")
                  ELSE /\ clean' = token
            /\ reaped' = TRUE
            /\ pc' = [pc EXCEPT !["coord"] = "Decide"]
            /\ UNCHANGED << callRefs, longRefs, gone, status, token, poolEnded, 
                            recovered >>

Decide == /\ pc["coord"] = "Decide"
          /\ IF ~clean /\ Variant /= "no_recovery"
                THEN /\ poolEnded' = TRUE
                ELSE /\ TRUE
                     /\ UNCHANGED poolEnded
          /\ pc' = [pc EXCEPT !["coord"] = "Done"]
          /\ UNCHANGED << callRefs, longRefs, gone, status, token, reaped, 
                          clean, recovered >>

Coordinator == Reap \/ Classify \/ Decide

Recover == /\ pc["nexus"] = "Recover"
           /\ poolEnded \/ (reaped /\ clean) \/ (reaped /\ Variant = "no_recovery")
              \/ pc["coord"] = "Done"
           /\ IF poolEnded
                 THEN /\ recovered' = TRUE
                 ELSE /\ TRUE
                      /\ UNCHANGED recovered
           /\ pc' = [pc EXCEPT !["nexus"] = "Done"]
           /\ UNCHANGED << callRefs, longRefs, gone, status, token, reaped, 
                           clean, poolEnded >>

Nexus == Recover

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Worker \/ Coordinator \/ Nexus
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Worker)
        /\ WF_vars(Coordinator)
        /\ WF_vars(Nexus)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
