------------------------------- MODULE PoolFork -------------------------------
(* A pool forking a worker (model/runtime/fork.md FORK-6). The coordinator takes up *)
(* the nexus's lifeline and attaches the stream registry, starting no      *)
(* thread of its own: it watches the lifeline in its own loop, and         *)
(* attaching marks the sweeper as wanted. A library thread exists from the *)
(* start and is ended by the library's own fork handler. The coordinator   *)
(* forks through a gate: the child waits while the parent counts its       *)
(* threads after the fork, and runs only if the count is one; otherwise    *)
(* the parent kills the child, which may be stuck before its gate when     *)
(* another thread existed at the fork, and the pool ends. The worker sends *)
(* a sweep request, and the sweeper starts on the first request.           *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "guard_thread": the coordinator watches the lifeline from a thread.
\* "eager_sweeper": attaching the registry starts the sweeper.
\* "wanted_by_thread": a process wants a sweeper only if its parent ran one.
\* "unpolled": the coordinator never watches the lifeline.
\* "count_before_fork": threads are counted before the fork handlers run.
\* "no_kill": with a sweeper thread at the fork, a refused child is waited
\* for but not killed.
\* "ungated": with a sweeper thread at the fork, the child runs at once.

(* --algorithm PoolFork
variables
    nexusAlive = TRUE,
    nexusSettled = FALSE,
    groupEnded = FALSE,
    libThread = TRUE,
    coordThreads = {},
    coordWants = FALSE,
    atFork = {},
    child = "none",
    refused = FALSE,
    workerWants = FALSE,
    workerSweeper = FALSE,
    requested = FALSE,
    served = FALSE;

define
    ChildRunsOnlyIfForkedAlone == child = "running" => atFork = {}
    NoSpuriousRefusal == refused => atFork /= {}
    NexusDeathEndsThePool == ~nexusAlive ~> groupEnded
    ThePoolServes == <>(served \/ ~nexusAlive)
end define;

fair process Nexus = "nexus"
begin
  Live:
    either
      nexusAlive := FALSE;
    or
      skip;
    end either;
  Settle:
    nexusSettled := TRUE;
end process;

fair process Coordinator = "coord"
variables counted = {};
begin
  Adopt:
    if Variant = "guard_thread" then
      coordThreads := coordThreads \union {"lifeline"};
    end if;
  Attach:
    coordWants := TRUE;
    if Variant \in {"eager_sweeper", "ungated", "no_kill"} then
      coordThreads := coordThreads \union {"sweeper"};
    end if;
  Prepare:
    counted := coordThreads \union (IF libThread THEN {"lib"} ELSE {});
    libThread := FALSE;
  Fork:
    atFork := coordThreads;
    if Variant = "wanted_by_thread" then
      workerWants := "sweeper" \in coordThreads;
    else
      workerWants := coordWants;
    end if;
    if Variant = "ungated" then
      child := "running";
      goto Watch;
    else
      child := "gated";
    end if;
  Count:
    if Variant /= "count_before_fork" then
      counted := coordThreads;
    end if;
    if counted = {} then
      child := "running";
    else
      refused := TRUE;
      if Variant /= "no_kill" then
        child := "killed";
      end if;
    end if;
  Reap:
    if refused then
      await child \in {"killed", "exited"};
      groupEnded := TRUE;
      goto Done;
    end if;
  Watch:
    if Variant = "unpolled" \/ Variant = "guard_thread" then
      skip;
    else
      await nexusSettled;
      if ~nexusAlive then
        groupEnded := TRUE;
      end if;
    end if;
end process;

fair process Guard = "guard"
begin
  GuardWatch:
    if Variant = "guard_thread" then
      await nexusSettled;
      if ~nexusAlive then
        groupEnded := TRUE;
      end if;
    end if;
end process;

fair process Worker = "worker"
begin
  Gate:
    await child /= "none";
    if child = "gated" /\ atFork /= {} then
      \* Stuck in a fork handler on a lock a vanished thread held.
      await child = "killed";
    else
      await child /= "gated";
    end if;
    if child /= "running" then
      goto Done;
    end if;
  Send:
    requested := TRUE;
    if workerWants then
      workerSweeper := TRUE;
    end if;
  Serve:
    await workerSweeper \/ groupEnded;
    if workerSweeper then
      served := TRUE;
    end if;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "8169f56f" /\ chksum(tla) = "47a59c5")
VARIABLES nexusAlive, nexusSettled, groupEnded, libThread, coordThreads, 
          coordWants, atFork, child, refused, workerWants, workerSweeper, 
          requested, served, pc

(* define statement *)
ChildRunsOnlyIfForkedAlone == child = "running" => atFork = {}
NoSpuriousRefusal == refused => atFork /= {}
NexusDeathEndsThePool == ~nexusAlive ~> groupEnded
ThePoolServes == <>(served \/ ~nexusAlive)

VARIABLE counted

vars == << nexusAlive, nexusSettled, groupEnded, libThread, coordThreads, 
           coordWants, atFork, child, refused, workerWants, workerSweeper, 
           requested, served, pc, counted >>

ProcSet == {"nexus"} \cup {"coord"} \cup {"guard"} \cup {"worker"}

Init == (* Global variables *)
        /\ nexusAlive = TRUE
        /\ nexusSettled = FALSE
        /\ groupEnded = FALSE
        /\ libThread = TRUE
        /\ coordThreads = {}
        /\ coordWants = FALSE
        /\ atFork = {}
        /\ child = "none"
        /\ refused = FALSE
        /\ workerWants = FALSE
        /\ workerSweeper = FALSE
        /\ requested = FALSE
        /\ served = FALSE
        (* Process Coordinator *)
        /\ counted = {}
        /\ pc = [self \in ProcSet |-> CASE self = "nexus" -> "Live"
                                        [] self = "coord" -> "Adopt"
                                        [] self = "guard" -> "GuardWatch"
                                        [] self = "worker" -> "Gate"]

Live == /\ pc["nexus"] = "Live"
        /\ \/ /\ nexusAlive' = FALSE
           \/ /\ TRUE
              /\ UNCHANGED nexusAlive
        /\ pc' = [pc EXCEPT !["nexus"] = "Settle"]
        /\ UNCHANGED << nexusSettled, groupEnded, libThread, coordThreads, 
                        coordWants, atFork, child, refused, workerWants, 
                        workerSweeper, requested, served, counted >>

Settle == /\ pc["nexus"] = "Settle"
          /\ nexusSettled' = TRUE
          /\ pc' = [pc EXCEPT !["nexus"] = "Done"]
          /\ UNCHANGED << nexusAlive, groupEnded, libThread, coordThreads, 
                          coordWants, atFork, child, refused, workerWants, 
                          workerSweeper, requested, served, counted >>

Nexus == Live \/ Settle

Adopt == /\ pc["coord"] = "Adopt"
         /\ IF Variant = "guard_thread"
               THEN /\ coordThreads' = (coordThreads \union {"lifeline"})
               ELSE /\ TRUE
                    /\ UNCHANGED coordThreads
         /\ pc' = [pc EXCEPT !["coord"] = "Attach"]
         /\ UNCHANGED << nexusAlive, nexusSettled, groupEnded, libThread, 
                         coordWants, atFork, child, refused, workerWants, 
                         workerSweeper, requested, served, counted >>

Attach == /\ pc["coord"] = "Attach"
          /\ coordWants' = TRUE
          /\ IF Variant \in {"eager_sweeper", "ungated", "no_kill"}
                THEN /\ coordThreads' = (coordThreads \union {"sweeper"})
                ELSE /\ TRUE
                     /\ UNCHANGED coordThreads
          /\ pc' = [pc EXCEPT !["coord"] = "Prepare"]
          /\ UNCHANGED << nexusAlive, nexusSettled, groupEnded, libThread, 
                          atFork, child, refused, workerWants, workerSweeper, 
                          requested, served, counted >>

Prepare == /\ pc["coord"] = "Prepare"
           /\ counted' = (coordThreads \union (IF libThread THEN {"lib"} ELSE {}))
           /\ libThread' = FALSE
           /\ pc' = [pc EXCEPT !["coord"] = "Fork"]
           /\ UNCHANGED << nexusAlive, nexusSettled, groupEnded, coordThreads, 
                           coordWants, atFork, child, refused, workerWants, 
                           workerSweeper, requested, served >>

Fork == /\ pc["coord"] = "Fork"
        /\ atFork' = coordThreads
        /\ IF Variant = "wanted_by_thread"
              THEN /\ workerWants' = ("sweeper" \in coordThreads)
              ELSE /\ workerWants' = coordWants
        /\ IF Variant = "ungated"
              THEN /\ child' = "running"
                   /\ pc' = [pc EXCEPT !["coord"] = "Watch"]
              ELSE /\ child' = "gated"
                   /\ pc' = [pc EXCEPT !["coord"] = "Count"]
        /\ UNCHANGED << nexusAlive, nexusSettled, groupEnded, libThread, 
                        coordThreads, coordWants, refused, workerSweeper, 
                        requested, served, counted >>

Count == /\ pc["coord"] = "Count"
         /\ IF Variant /= "count_before_fork"
               THEN /\ counted' = coordThreads
               ELSE /\ TRUE
                    /\ UNCHANGED counted
         /\ IF counted' = {}
               THEN /\ child' = "running"
                    /\ UNCHANGED refused
               ELSE /\ refused' = TRUE
                    /\ IF Variant /= "no_kill"
                          THEN /\ child' = "killed"
                          ELSE /\ TRUE
                               /\ child' = child
         /\ pc' = [pc EXCEPT !["coord"] = "Reap"]
         /\ UNCHANGED << nexusAlive, nexusSettled, groupEnded, libThread, 
                         coordThreads, coordWants, atFork, workerWants, 
                         workerSweeper, requested, served >>

Reap == /\ pc["coord"] = "Reap"
        /\ IF refused
              THEN /\ child \in {"killed", "exited"}
                   /\ groupEnded' = TRUE
                   /\ pc' = [pc EXCEPT !["coord"] = "Done"]
              ELSE /\ pc' = [pc EXCEPT !["coord"] = "Watch"]
                   /\ UNCHANGED groupEnded
        /\ UNCHANGED << nexusAlive, nexusSettled, libThread, coordThreads, 
                        coordWants, atFork, child, refused, workerWants, 
                        workerSweeper, requested, served, counted >>

Watch == /\ pc["coord"] = "Watch"
         /\ IF Variant = "unpolled" \/ Variant = "guard_thread"
               THEN /\ TRUE
                    /\ UNCHANGED groupEnded
               ELSE /\ nexusSettled
                    /\ IF ~nexusAlive
                          THEN /\ groupEnded' = TRUE
                          ELSE /\ TRUE
                               /\ UNCHANGED groupEnded
         /\ pc' = [pc EXCEPT !["coord"] = "Done"]
         /\ UNCHANGED << nexusAlive, nexusSettled, libThread, coordThreads, 
                         coordWants, atFork, child, refused, workerWants, 
                         workerSweeper, requested, served, counted >>

Coordinator == Adopt \/ Attach \/ Prepare \/ Fork \/ Count \/ Reap \/ Watch

GuardWatch == /\ pc["guard"] = "GuardWatch"
              /\ IF Variant = "guard_thread"
                    THEN /\ nexusSettled
                         /\ IF ~nexusAlive
                               THEN /\ groupEnded' = TRUE
                               ELSE /\ TRUE
                                    /\ UNCHANGED groupEnded
                    ELSE /\ TRUE
                         /\ UNCHANGED groupEnded
              /\ pc' = [pc EXCEPT !["guard"] = "Done"]
              /\ UNCHANGED << nexusAlive, nexusSettled, libThread, 
                              coordThreads, coordWants, atFork, child, refused, 
                              workerWants, workerSweeper, requested, served, 
                              counted >>

Guard == GuardWatch

Gate == /\ pc["worker"] = "Gate"
        /\ child /= "none"
        /\ IF child = "gated" /\ atFork /= {}
              THEN /\ child = "killed"
              ELSE /\ child /= "gated"
        /\ IF child /= "running"
              THEN /\ pc' = [pc EXCEPT !["worker"] = "Done"]
              ELSE /\ pc' = [pc EXCEPT !["worker"] = "Send"]
        /\ UNCHANGED << nexusAlive, nexusSettled, groupEnded, libThread, 
                        coordThreads, coordWants, atFork, child, refused, 
                        workerWants, workerSweeper, requested, served, counted >>

Send == /\ pc["worker"] = "Send"
        /\ requested' = TRUE
        /\ IF workerWants
              THEN /\ workerSweeper' = TRUE
              ELSE /\ TRUE
                   /\ UNCHANGED workerSweeper
        /\ pc' = [pc EXCEPT !["worker"] = "Serve"]
        /\ UNCHANGED << nexusAlive, nexusSettled, groupEnded, libThread, 
                        coordThreads, coordWants, atFork, child, refused, 
                        workerWants, served, counted >>

Serve == /\ pc["worker"] = "Serve"
         /\ workerSweeper \/ groupEnded
         /\ IF workerSweeper
               THEN /\ served' = TRUE
               ELSE /\ TRUE
                    /\ UNCHANGED served
         /\ pc' = [pc EXCEPT !["worker"] = "Done"]
         /\ UNCHANGED << nexusAlive, nexusSettled, groupEnded, libThread, 
                         coordThreads, coordWants, atFork, child, refused, 
                         workerWants, workerSweeper, requested, counted >>

Worker == Gate \/ Send \/ Serve

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Nexus \/ Coordinator \/ Guard \/ Worker
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Nexus)
        /\ WF_vars(Coordinator)
        /\ WF_vars(Guard)
        /\ WF_vars(Worker)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
