--------------------------- MODULE DaemonRecovery ---------------------------
(* Pool-crash recovery in the daemon (model/daemon.md DAEMON-1). Request   *)
(* workers read shared memory; recovery unmaps and remaps it. A pool call  *)
(* may be wedged until the pools are killed; only a pool that this        *)
(* recovery kills is modelled as wedging (unbounded calls: DAEMON-6).      *)
EXTENDS Naturals

CONSTANTS Workers,
          AtomicAdmission, \* the gate check and the count change are one step
          WaitForRequests, \* recovery waits for admitted requests to finish
          KillBeforeWait   \* pools are killed before that wait

(* --algorithm DaemonRecovery
variables
    recovering = FALSE,
    inflight = 0,
    mapped = TRUE,
    poolsUp = TRUE,
    recovered = FALSE,
    reading = [w \in Workers |-> FALSE];

define
    NoReadOfUnmappedMemory == \A w \in Workers : reading[w] => mapped
    RecoveryFinishes == recovering ~> ~recovering
end define;

fair process Worker \in Workers
variables admitted = FALSE, sawOpen = FALSE, wedged = FALSE;
begin
  Loop:
    while TRUE do
      Check:
        if AtomicAdmission then
          if ~recovering then
            inflight := inflight + 1;
            admitted := TRUE;
          else
            admitted := FALSE;
          end if;
        else
          sawOpen := ~recovering;
        end if;
      Admit:
        if ~AtomicAdmission then
          if sawOpen then
            inflight := inflight + 1;
            admitted := TRUE;
          else
            admitted := FALSE;
          end if;
        end if;
      Send:
        if admitted then
          either
            wedged := FALSE;
          or
            await ~recovered;
            wedged := TRUE;
          end either;
        end if;
      Call:
        if admitted /\ wedged then
          await ~poolsUp;
        end if;
      Read:
        if admitted then
          reading[self] := TRUE;
        end if;
      Leave:
        reading[self] := FALSE;
        if admitted then
          inflight := inflight - 1;
          admitted := FALSE;
        end if;
    end while;
end process;

fair process Recovery = "recovery"
begin
  Begin:
    recovering := TRUE;
  KillEarly:
    if KillBeforeWait then
      poolsUp := FALSE;
    end if;
  Wait:
    if WaitForRequests then
      await inflight = 0;
    end if;
  KillLate:
    poolsUp := FALSE;
  Unmap:
    mapped := FALSE;
  Remap:
    mapped := TRUE;
    poolsUp := TRUE;
  Reopen:
    recovering := FALSE;
    recovered := TRUE;
end process;

end algorithm; *)
\* BEGIN TRANSLATION
VARIABLES recovering, inflight, mapped, poolsUp, recovered, reading, pc

(* define statement *)
NoReadOfUnmappedMemory == \A w \in Workers : reading[w] => mapped
RecoveryFinishes == recovering ~> ~recovering

VARIABLES admitted, sawOpen, wedged

vars == << recovering, inflight, mapped, poolsUp, recovered, reading, pc, 
           admitted, sawOpen, wedged >>

ProcSet == (Workers) \cup {"recovery"}

Init == (* Global variables *)
        /\ recovering = FALSE
        /\ inflight = 0
        /\ mapped = TRUE
        /\ poolsUp = TRUE
        /\ recovered = FALSE
        /\ reading = [w \in Workers |-> FALSE]
        (* Process Worker *)
        /\ admitted = [self \in Workers |-> FALSE]
        /\ sawOpen = [self \in Workers |-> FALSE]
        /\ wedged = [self \in Workers |-> FALSE]
        /\ pc = [self \in ProcSet |-> CASE self \in Workers -> "Loop"
                                        [] self = "recovery" -> "Begin"]

Loop(self) == /\ pc[self] = "Loop"
              /\ pc' = [pc EXCEPT ![self] = "Check"]
              /\ UNCHANGED << recovering, inflight, mapped, poolsUp, recovered, 
                              reading, admitted, sawOpen, wedged >>

Check(self) == /\ pc[self] = "Check"
               /\ IF AtomicAdmission
                     THEN /\ IF ~recovering
                                THEN /\ inflight' = inflight + 1
                                     /\ admitted' = [admitted EXCEPT ![self] = TRUE]
                                ELSE /\ admitted' = [admitted EXCEPT ![self] = FALSE]
                                     /\ UNCHANGED inflight
                          /\ UNCHANGED sawOpen
                     ELSE /\ sawOpen' = [sawOpen EXCEPT ![self] = ~recovering]
                          /\ UNCHANGED << inflight, admitted >>
               /\ pc' = [pc EXCEPT ![self] = "Admit"]
               /\ UNCHANGED << recovering, mapped, poolsUp, recovered, reading, 
                               wedged >>

Admit(self) == /\ pc[self] = "Admit"
               /\ IF ~AtomicAdmission
                     THEN /\ IF sawOpen[self]
                                THEN /\ inflight' = inflight + 1
                                     /\ admitted' = [admitted EXCEPT ![self] = TRUE]
                                ELSE /\ admitted' = [admitted EXCEPT ![self] = FALSE]
                                     /\ UNCHANGED inflight
                     ELSE /\ TRUE
                          /\ UNCHANGED << inflight, admitted >>
               /\ pc' = [pc EXCEPT ![self] = "Send"]
               /\ UNCHANGED << recovering, mapped, poolsUp, recovered, reading, 
                               sawOpen, wedged >>

Send(self) == /\ pc[self] = "Send"
              /\ IF admitted[self]
                    THEN /\ \/ /\ wedged' = [wedged EXCEPT ![self] = FALSE]
                            \/ /\ ~recovered
                               /\ wedged' = [wedged EXCEPT ![self] = TRUE]
                    ELSE /\ TRUE
                         /\ UNCHANGED wedged
              /\ pc' = [pc EXCEPT ![self] = "Call"]
              /\ UNCHANGED << recovering, inflight, mapped, poolsUp, recovered, 
                              reading, admitted, sawOpen >>

Call(self) == /\ pc[self] = "Call"
              /\ IF admitted[self] /\ wedged[self]
                    THEN /\ ~poolsUp
                    ELSE /\ TRUE
              /\ pc' = [pc EXCEPT ![self] = "Read"]
              /\ UNCHANGED << recovering, inflight, mapped, poolsUp, recovered, 
                              reading, admitted, sawOpen, wedged >>

Read(self) == /\ pc[self] = "Read"
              /\ IF admitted[self]
                    THEN /\ reading' = [reading EXCEPT ![self] = TRUE]
                    ELSE /\ TRUE
                         /\ UNCHANGED reading
              /\ pc' = [pc EXCEPT ![self] = "Leave"]
              /\ UNCHANGED << recovering, inflight, mapped, poolsUp, recovered, 
                              admitted, sawOpen, wedged >>

Leave(self) == /\ pc[self] = "Leave"
               /\ reading' = [reading EXCEPT ![self] = FALSE]
               /\ IF admitted[self]
                     THEN /\ inflight' = inflight - 1
                          /\ admitted' = [admitted EXCEPT ![self] = FALSE]
                     ELSE /\ TRUE
                          /\ UNCHANGED << inflight, admitted >>
               /\ pc' = [pc EXCEPT ![self] = "Loop"]
               /\ UNCHANGED << recovering, mapped, poolsUp, recovered, sawOpen, 
                               wedged >>

Worker(self) == Loop(self) \/ Check(self) \/ Admit(self) \/ Send(self)
                   \/ Call(self) \/ Read(self) \/ Leave(self)

Begin == /\ pc["recovery"] = "Begin"
         /\ recovering' = TRUE
         /\ pc' = [pc EXCEPT !["recovery"] = "KillEarly"]
         /\ UNCHANGED << inflight, mapped, poolsUp, recovered, reading, 
                         admitted, sawOpen, wedged >>

KillEarly == /\ pc["recovery"] = "KillEarly"
             /\ IF KillBeforeWait
                   THEN /\ poolsUp' = FALSE
                   ELSE /\ TRUE
                        /\ UNCHANGED poolsUp
             /\ pc' = [pc EXCEPT !["recovery"] = "Wait"]
             /\ UNCHANGED << recovering, inflight, mapped, recovered, reading, 
                             admitted, sawOpen, wedged >>

Wait == /\ pc["recovery"] = "Wait"
        /\ IF WaitForRequests
              THEN /\ inflight = 0
              ELSE /\ TRUE
        /\ pc' = [pc EXCEPT !["recovery"] = "KillLate"]
        /\ UNCHANGED << recovering, inflight, mapped, poolsUp, recovered, 
                        reading, admitted, sawOpen, wedged >>

KillLate == /\ pc["recovery"] = "KillLate"
            /\ poolsUp' = FALSE
            /\ pc' = [pc EXCEPT !["recovery"] = "Unmap"]
            /\ UNCHANGED << recovering, inflight, mapped, recovered, reading, 
                            admitted, sawOpen, wedged >>

Unmap == /\ pc["recovery"] = "Unmap"
         /\ mapped' = FALSE
         /\ pc' = [pc EXCEPT !["recovery"] = "Remap"]
         /\ UNCHANGED << recovering, inflight, poolsUp, recovered, reading, 
                         admitted, sawOpen, wedged >>

Remap == /\ pc["recovery"] = "Remap"
         /\ mapped' = TRUE
         /\ poolsUp' = TRUE
         /\ pc' = [pc EXCEPT !["recovery"] = "Reopen"]
         /\ UNCHANGED << recovering, inflight, recovered, reading, admitted, 
                         sawOpen, wedged >>

Reopen == /\ pc["recovery"] = "Reopen"
          /\ recovering' = FALSE
          /\ recovered' = TRUE
          /\ pc' = [pc EXCEPT !["recovery"] = "Done"]
          /\ UNCHANGED << inflight, mapped, poolsUp, reading, admitted, 
                          sawOpen, wedged >>

Recovery == Begin \/ KillEarly \/ Wait \/ KillLate \/ Unmap \/ Remap
               \/ Reopen

Next == Recovery
           \/ (\E self \in Workers: Worker(self))

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Workers : WF_vars(Worker(self))
        /\ WF_vars(Recovery)

\* END TRANSLATION
=============================================================================
