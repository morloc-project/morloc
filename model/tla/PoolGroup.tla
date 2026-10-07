------------------------------ MODULE PoolGroup ------------------------------
(* A pool's process group (model/daemon.md DAEMON-11). The group is led by *)
(* a pin, a shell that blocks SIGTERM from birth and ends only on SIGKILL, *)
(* when the nexus closes its pipe, or when something outside the nexus     *)
(* kills it. The nexus registers the group in a slot, then starts the pool *)
(* in it. The pool may exit at any time and is reaped at once. Once no     *)
(* process holds the id as its pid or its group, the kernel may hand it to *)
(* a stranger. Every signal goes through the slot: a signaller takes the   *)
(* slot while it signals, and after SIGKILL the slot is dead. A reaper     *)
(* marks a pin's slot dead before it reaps the pin. Stopping everything    *)
(* (the handler) marks the table stopped, then kills every group. A pool   *)
(* that never exits and is never killed leaves the processes waiting; the *)
(* lifeline ends it in the code, so deadlock is not checked.              *)
EXTENDS Naturals

CONSTANT Variant
\* "pinned": the design.
\* "no_pin": the group has no pin.
\* "kill_then_clear": the slot is marked dead only after SIGKILL is sent.
\* "reap_unmarked": a pin is reaped without marking its slot dead.
\* "spawn_unheld": the pool is started without holding the slot.

(* --algorithm PoolGroup
variables
    pool = "unborn",
    pin = IF Variant = "no_pin" THEN "none" ELSE "alive",
    slot = "group",
    stopped = FALSE,
    outsideKill = FALSE,
    stranger = FALSE,
    hitStranger = FALSE;

define
    Held == pool \in {"alive", "zombie"} \/ pin \in {"alive", "zombie"}
    NoSignalReachesAStranger == ~hitStranger
    \* Without a pin's death from outside, stopping everything ends the pool.
    StopEndsThePool ==
        (pc["handler"] = "Done" /\ pc["starter"] = "Done" /\ ~outsideKill) => pool # "alive"
end define;

macro deliver(sig) begin
  if stranger then
    hitStranger := TRUE;
  elsif sig = "KILL" then
    if pool = "alive" then pool := "zombie"; end if;
    if pin = "alive" then pin := "zombie"; end if;
  end if;
end macro;

fair process Starter = "starter"
begin
  Hold:
    if Variant = "spawn_unheld" then
      if slot = "group" then goto Spawn; else goto StartDone; end if;
    else
      await slot # "busy";
      if slot = "group" then slot := "busy"; else goto StartDone; end if;
    end if;
  Spawn:
    \* Joining the group fails once its id is free.
    if Held \/ Variant = "no_pin" then pool := "alive"; end if;
  Unhold:
    if slot = "busy" then slot := "group"; end if;
  StartDone:
    skip;
end process;

fair process Pool = "pool"
begin
  Exit:
    await pool # "unborn" \/ pc["starter"] = "Done";
    either
      if pool = "alive" then pool := "zombie"; end if;
    or
      skip;
    end either;
end process;

fair process Outsider = "outsider"
begin
  Kill:
    either
      skip;
    or
      await pin = "alive";
      pin := "zombie";
      outsideKill := TRUE;
    end either;
end process;

fair process Reaper = "reaper"
begin
  ReapPool:
    await pool = "zombie" \/ (pool = "unborn" /\ pc["starter"] = "Done");
    if pool = "zombie" then pool := "reaped"; end if;
  ReapPin:
    await pin # "alive";
    if pin = "zombie" then
      if Variant # "reap_unmarked" then
        await slot # "busy";
        if slot = "group" then slot := "dead"; end if;
      end if;
      pin := "reaped";
    end if;
end process;

fair process Kernel = "kernel"
begin
  Reissue:
    either
      await ~Held;
      stranger := TRUE;
    or
      skip;
    end either;
end process;

fair process Signaller \in {"pools", "handler"}
variables sig = "TERM";
begin
  Choose:
    if self = "handler" then
      sig := "KILL";
      stopped := TRUE;
    else
      either sig := "TERM"; or sig := "KILL"; end either;
    end if;
  Take:
    await slot # "busy";
    if slot = "group" then
      if Variant = "kill_then_clear" /\ sig = "KILL" then
        deliver(sig);
      else
        slot := "busy";
      end if;
    else
      goto Finish;
    end if;
  Send:
    if Variant = "kill_then_clear" /\ sig = "KILL" then
      slot := "dead";
    else
      deliver(sig);
    end if;
  Give:
    if slot = "busy" then
      slot := IF sig = "KILL" THEN "dead" ELSE "group";
    end if;
  Finish:
    skip;
end process;

fair process Release = "release"
begin
  WaitGone:
    await pool \in {"reaped", "unborn"} /\ pc["starter"] = "Done";
  Clear:
    await slot # "busy";
    slot := "empty";
  Close:
    if pin = "alive" then pin := "zombie"; end if;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "c20bcef0" /\ chksum(tla) = "13bbbea3")
VARIABLES pool, pin, slot, stopped, outsideKill, stranger, hitStranger, pc

(* define statement *)
Held == pool \in {"alive", "zombie"} \/ pin \in {"alive", "zombie"}
NoSignalReachesAStranger == ~hitStranger

StopEndsThePool ==
    (pc["handler"] = "Done" /\ pc["starter"] = "Done" /\ ~outsideKill) => pool # "alive"

VARIABLE sig

vars == << pool, pin, slot, stopped, outsideKill, stranger, hitStranger, pc, 
           sig >>

ProcSet == {"starter"} \cup {"pool"} \cup {"outsider"} \cup {"reaper"} \cup {"kernel"} \cup ({"pools", "handler"}) \cup {"release"}

Init == (* Global variables *)
        /\ pool = "unborn"
        /\ pin = (IF Variant = "no_pin" THEN "none" ELSE "alive")
        /\ slot = "group"
        /\ stopped = FALSE
        /\ outsideKill = FALSE
        /\ stranger = FALSE
        /\ hitStranger = FALSE
        (* Process Signaller *)
        /\ sig = [self \in {"pools", "handler"} |-> "TERM"]
        /\ pc = [self \in ProcSet |-> CASE self = "starter" -> "Hold"
                                        [] self = "pool" -> "Exit"
                                        [] self = "outsider" -> "Kill"
                                        [] self = "reaper" -> "ReapPool"
                                        [] self = "kernel" -> "Reissue"
                                        [] self \in {"pools", "handler"} -> "Choose"
                                        [] self = "release" -> "WaitGone"]

Hold == /\ pc["starter"] = "Hold"
        /\ IF Variant = "spawn_unheld"
              THEN /\ IF slot = "group"
                         THEN /\ pc' = [pc EXCEPT !["starter"] = "Spawn"]
                         ELSE /\ pc' = [pc EXCEPT !["starter"] = "StartDone"]
                   /\ slot' = slot
              ELSE /\ slot # "busy"
                   /\ IF slot = "group"
                         THEN /\ slot' = "busy"
                              /\ pc' = [pc EXCEPT !["starter"] = "Spawn"]
                         ELSE /\ pc' = [pc EXCEPT !["starter"] = "StartDone"]
                              /\ slot' = slot
        /\ UNCHANGED << pool, pin, stopped, outsideKill, stranger, hitStranger, 
                        sig >>

Spawn == /\ pc["starter"] = "Spawn"
         /\ IF Held \/ Variant = "no_pin"
               THEN /\ pool' = "alive"
               ELSE /\ TRUE
                    /\ pool' = pool
         /\ pc' = [pc EXCEPT !["starter"] = "Unhold"]
         /\ UNCHANGED << pin, slot, stopped, outsideKill, stranger, 
                         hitStranger, sig >>

Unhold == /\ pc["starter"] = "Unhold"
          /\ IF slot = "busy"
                THEN /\ slot' = "group"
                ELSE /\ TRUE
                     /\ slot' = slot
          /\ pc' = [pc EXCEPT !["starter"] = "StartDone"]
          /\ UNCHANGED << pool, pin, stopped, outsideKill, stranger, 
                          hitStranger, sig >>

StartDone == /\ pc["starter"] = "StartDone"
             /\ TRUE
             /\ pc' = [pc EXCEPT !["starter"] = "Done"]
             /\ UNCHANGED << pool, pin, slot, stopped, outsideKill, stranger, 
                             hitStranger, sig >>

Starter == Hold \/ Spawn \/ Unhold \/ StartDone

Exit == /\ pc["pool"] = "Exit"
        /\ pool # "unborn" \/ pc["starter"] = "Done"
        /\ \/ /\ IF pool = "alive"
                    THEN /\ pool' = "zombie"
                    ELSE /\ TRUE
                         /\ pool' = pool
           \/ /\ TRUE
              /\ pool' = pool
        /\ pc' = [pc EXCEPT !["pool"] = "Done"]
        /\ UNCHANGED << pin, slot, stopped, outsideKill, stranger, hitStranger, 
                        sig >>

Pool == Exit

Kill == /\ pc["outsider"] = "Kill"
        /\ \/ /\ TRUE
              /\ UNCHANGED <<pin, outsideKill>>
           \/ /\ pin = "alive"
              /\ pin' = "zombie"
              /\ outsideKill' = TRUE
        /\ pc' = [pc EXCEPT !["outsider"] = "Done"]
        /\ UNCHANGED << pool, slot, stopped, stranger, hitStranger, sig >>

Outsider == Kill

ReapPool == /\ pc["reaper"] = "ReapPool"
            /\ pool = "zombie" \/ (pool = "unborn" /\ pc["starter"] = "Done")
            /\ IF pool = "zombie"
                  THEN /\ pool' = "reaped"
                  ELSE /\ TRUE
                       /\ pool' = pool
            /\ pc' = [pc EXCEPT !["reaper"] = "ReapPin"]
            /\ UNCHANGED << pin, slot, stopped, outsideKill, stranger, 
                            hitStranger, sig >>

ReapPin == /\ pc["reaper"] = "ReapPin"
           /\ pin # "alive"
           /\ IF pin = "zombie"
                 THEN /\ IF Variant # "reap_unmarked"
                            THEN /\ slot # "busy"
                                 /\ IF slot = "group"
                                       THEN /\ slot' = "dead"
                                       ELSE /\ TRUE
                                            /\ slot' = slot
                            ELSE /\ TRUE
                                 /\ slot' = slot
                      /\ pin' = "reaped"
                 ELSE /\ TRUE
                      /\ UNCHANGED << pin, slot >>
           /\ pc' = [pc EXCEPT !["reaper"] = "Done"]
           /\ UNCHANGED << pool, stopped, outsideKill, stranger, hitStranger, 
                           sig >>

Reaper == ReapPool \/ ReapPin

Reissue == /\ pc["kernel"] = "Reissue"
           /\ \/ /\ ~Held
                 /\ stranger' = TRUE
              \/ /\ TRUE
                 /\ UNCHANGED stranger
           /\ pc' = [pc EXCEPT !["kernel"] = "Done"]
           /\ UNCHANGED << pool, pin, slot, stopped, outsideKill, hitStranger, 
                           sig >>

Kernel == Reissue

Choose(self) == /\ pc[self] = "Choose"
                /\ IF self = "handler"
                      THEN /\ sig' = [sig EXCEPT ![self] = "KILL"]
                           /\ stopped' = TRUE
                      ELSE /\ \/ /\ sig' = [sig EXCEPT ![self] = "TERM"]
                              \/ /\ sig' = [sig EXCEPT ![self] = "KILL"]
                           /\ UNCHANGED stopped
                /\ pc' = [pc EXCEPT ![self] = "Take"]
                /\ UNCHANGED << pool, pin, slot, outsideKill, stranger, 
                                hitStranger >>

Take(self) == /\ pc[self] = "Take"
              /\ slot # "busy"
              /\ IF slot = "group"
                    THEN /\ IF Variant = "kill_then_clear" /\ sig[self] = "KILL"
                               THEN /\ IF stranger
                                          THEN /\ hitStranger' = TRUE
                                               /\ UNCHANGED << pool, pin >>
                                          ELSE /\ IF sig[self] = "KILL"
                                                     THEN /\ IF pool = "alive"
                                                                THEN /\ pool' = "zombie"
                                                                ELSE /\ TRUE
                                                                     /\ pool' = pool
                                                          /\ IF pin = "alive"
                                                                THEN /\ pin' = "zombie"
                                                                ELSE /\ TRUE
                                                                     /\ pin' = pin
                                                     ELSE /\ TRUE
                                                          /\ UNCHANGED << pool, 
                                                                          pin >>
                                               /\ UNCHANGED hitStranger
                                    /\ slot' = slot
                               ELSE /\ slot' = "busy"
                                    /\ UNCHANGED << pool, pin, hitStranger >>
                         /\ pc' = [pc EXCEPT ![self] = "Send"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Finish"]
                         /\ UNCHANGED << pool, pin, slot, hitStranger >>
              /\ UNCHANGED << stopped, outsideKill, stranger, sig >>

Send(self) == /\ pc[self] = "Send"
              /\ IF Variant = "kill_then_clear" /\ sig[self] = "KILL"
                    THEN /\ slot' = "dead"
                         /\ UNCHANGED << pool, pin, hitStranger >>
                    ELSE /\ IF stranger
                               THEN /\ hitStranger' = TRUE
                                    /\ UNCHANGED << pool, pin >>
                               ELSE /\ IF sig[self] = "KILL"
                                          THEN /\ IF pool = "alive"
                                                     THEN /\ pool' = "zombie"
                                                     ELSE /\ TRUE
                                                          /\ pool' = pool
                                               /\ IF pin = "alive"
                                                     THEN /\ pin' = "zombie"
                                                     ELSE /\ TRUE
                                                          /\ pin' = pin
                                          ELSE /\ TRUE
                                               /\ UNCHANGED << pool, pin >>
                                    /\ UNCHANGED hitStranger
                         /\ slot' = slot
              /\ pc' = [pc EXCEPT ![self] = "Give"]
              /\ UNCHANGED << stopped, outsideKill, stranger, sig >>

Give(self) == /\ pc[self] = "Give"
              /\ IF slot = "busy"
                    THEN /\ slot' = (IF sig[self] = "KILL" THEN "dead" ELSE "group")
                    ELSE /\ TRUE
                         /\ slot' = slot
              /\ pc' = [pc EXCEPT ![self] = "Finish"]
              /\ UNCHANGED << pool, pin, stopped, outsideKill, stranger, 
                              hitStranger, sig >>

Finish(self) == /\ pc[self] = "Finish"
                /\ TRUE
                /\ pc' = [pc EXCEPT ![self] = "Done"]
                /\ UNCHANGED << pool, pin, slot, stopped, outsideKill, 
                                stranger, hitStranger, sig >>

Signaller(self) == Choose(self) \/ Take(self) \/ Send(self) \/ Give(self)
                      \/ Finish(self)

WaitGone == /\ pc["release"] = "WaitGone"
            /\ pool \in {"reaped", "unborn"} /\ pc["starter"] = "Done"
            /\ pc' = [pc EXCEPT !["release"] = "Clear"]
            /\ UNCHANGED << pool, pin, slot, stopped, outsideKill, stranger, 
                            hitStranger, sig >>

Clear == /\ pc["release"] = "Clear"
         /\ slot # "busy"
         /\ slot' = "empty"
         /\ pc' = [pc EXCEPT !["release"] = "Close"]
         /\ UNCHANGED << pool, pin, stopped, outsideKill, stranger, 
                         hitStranger, sig >>

Close == /\ pc["release"] = "Close"
         /\ IF pin = "alive"
               THEN /\ pin' = "zombie"
               ELSE /\ TRUE
                    /\ pin' = pin
         /\ pc' = [pc EXCEPT !["release"] = "Done"]
         /\ UNCHANGED << pool, slot, stopped, outsideKill, stranger, 
                         hitStranger, sig >>

Release == WaitGone \/ Clear \/ Close

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Starter \/ Pool \/ Outsider \/ Reaper \/ Kernel \/ Release
           \/ (\E self \in {"pools", "handler"}: Signaller(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Starter)
        /\ WF_vars(Pool)
        /\ WF_vars(Outsider)
        /\ WF_vars(Reaper)
        /\ WF_vars(Kernel)
        /\ \A self \in {"pools", "handler"} : WF_vars(Signaller(self))
        /\ WF_vars(Release)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
