------------------------------ MODULE LocalSweep ------------------------------
(* A process's local slot for a stream (model/runtime/streams.md SLOT-9). The slot *)
(* holds a cache and buffers for one generation of the stream. Another      *)
(* process may end the stream, which moves its generation and rings the    *)
(* doorbell. A thread of this process takes the slot out to use it and      *)
(* gives it back, dropping it instead if the stream has ended meanwhile. A  *)
(* sweep, run when the doorbell has moved, drops every slot not taken out   *)
(* whose generation is no longer the stream's. The process may never touch *)
(* the stream again.                                                        *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "lock_holders_only": the sweep drops only slots holding the file lock,
\* so a reader's slot for an ended stream stays.
\* "drop_in_use": the sweep also drops a slot a thread has taken out.
\* "reinstall_stale": a slot given back after the stream ended is kept.

(* --algorithm LocalSweep
variables
    gen = 1,
    rung = 0,
    seen = 0,
    slot = "idle",
    cached = 1,
    holdsLock = FALSE,
    usedDropped = FALSE;

define
    NoSlotIsDroppedInUse == ~usedDropped
    AnEndedStreamsSlotIsDropped == (gen /= cached) ~> (slot = "dropped")
end define;

fair process User = "user"
begin
  Use:
    either
      await slot = "idle";
      slot := "inuse";
    or
      goto Done;
    end either;
  GiveBack:
    if slot = "dropped" then
      usedDropped := TRUE;
    elsif gen /= cached /\ Variant /= "reinstall_stale" then
      slot := "dropped";
    else
      slot := "idle";
    end if;
end process;

fair process Closer = "closer"
begin
  End:
    gen := gen + 1;
  Ring:
    rung := rung + 1;
end process;

fair process Sweeper = "sweep"
begin
  Loop:
    while TRUE do
      Wait:
        either
          await rung /= seen;
          seen := rung;
        or
          await pc["user"] = "Done" /\ pc["closer"] = "Done" /\ rung = seen;
          goto Done;
        end either;
      Sweep:
        if gen /= cached /\ (slot = "idle" \/ (slot = "inuse" /\ Variant = "drop_in_use"))
           /\ (holdsLock \/ Variant /= "lock_holders_only") then
          slot := "dropped";
        end if;
    end while;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "135e0715" /\ chksum(tla) = "431dfcc9")
VARIABLES gen, rung, seen, slot, cached, holdsLock, usedDropped, pc

(* define statement *)
NoSlotIsDroppedInUse == ~usedDropped
AnEndedStreamsSlotIsDropped == (gen /= cached) ~> (slot = "dropped")


vars == << gen, rung, seen, slot, cached, holdsLock, usedDropped, pc >>

ProcSet == {"user"} \cup {"closer"} \cup {"sweep"}

Init == (* Global variables *)
        /\ gen = 1
        /\ rung = 0
        /\ seen = 0
        /\ slot = "idle"
        /\ cached = 1
        /\ holdsLock = FALSE
        /\ usedDropped = FALSE
        /\ pc = [self \in ProcSet |-> CASE self = "user" -> "Use"
                                        [] self = "closer" -> "End"
                                        [] self = "sweep" -> "Loop"]

Use == /\ pc["user"] = "Use"
       /\ \/ /\ slot = "idle"
             /\ slot' = "inuse"
             /\ pc' = [pc EXCEPT !["user"] = "GiveBack"]
          \/ /\ pc' = [pc EXCEPT !["user"] = "Done"]
             /\ slot' = slot
       /\ UNCHANGED << gen, rung, seen, cached, holdsLock, usedDropped >>

GiveBack == /\ pc["user"] = "GiveBack"
            /\ IF slot = "dropped"
                  THEN /\ usedDropped' = TRUE
                       /\ slot' = slot
                  ELSE /\ IF gen /= cached /\ Variant /= "reinstall_stale"
                             THEN /\ slot' = "dropped"
                             ELSE /\ slot' = "idle"
                       /\ UNCHANGED usedDropped
            /\ pc' = [pc EXCEPT !["user"] = "Done"]
            /\ UNCHANGED << gen, rung, seen, cached, holdsLock >>

User == Use \/ GiveBack

End == /\ pc["closer"] = "End"
       /\ gen' = gen + 1
       /\ pc' = [pc EXCEPT !["closer"] = "Ring"]
       /\ UNCHANGED << rung, seen, slot, cached, holdsLock, usedDropped >>

Ring == /\ pc["closer"] = "Ring"
        /\ rung' = rung + 1
        /\ pc' = [pc EXCEPT !["closer"] = "Done"]
        /\ UNCHANGED << gen, seen, slot, cached, holdsLock, usedDropped >>

Closer == End \/ Ring

Loop == /\ pc["sweep"] = "Loop"
        /\ pc' = [pc EXCEPT !["sweep"] = "Wait"]
        /\ UNCHANGED << gen, rung, seen, slot, cached, holdsLock, usedDropped >>

Wait == /\ pc["sweep"] = "Wait"
        /\ \/ /\ rung /= seen
              /\ seen' = rung
              /\ pc' = [pc EXCEPT !["sweep"] = "Sweep"]
           \/ /\ pc["user"] = "Done" /\ pc["closer"] = "Done" /\ rung = seen
              /\ pc' = [pc EXCEPT !["sweep"] = "Done"]
              /\ seen' = seen
        /\ UNCHANGED << gen, rung, slot, cached, holdsLock, usedDropped >>

Sweep == /\ pc["sweep"] = "Sweep"
         /\ IF gen /= cached /\ (slot = "idle" \/ (slot = "inuse" /\ Variant = "drop_in_use"))
               /\ (holdsLock \/ Variant /= "lock_holders_only")
               THEN /\ slot' = "dropped"
               ELSE /\ TRUE
                    /\ slot' = slot
         /\ pc' = [pc EXCEPT !["sweep"] = "Loop"]
         /\ UNCHANGED << gen, rung, seen, cached, holdsLock, usedDropped >>

Sweeper == Loop \/ Wait \/ Sweep

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == User \/ Closer \/ Sweeper
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(User)
        /\ WF_vars(Closer)
        /\ WF_vars(Sweeper)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
