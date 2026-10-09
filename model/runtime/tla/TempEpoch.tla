------------------------------ MODULE TempEpoch ------------------------------
(* Reclaiming temps made on threads no dispatch owns (model/runtime/fork.md *)
(* FORK-16). A dispatch removes its own temps when it ends. A temp made on *)
(* a helper thread is assumed to belong to every dispatch running when it  *)
(* was made, so it is removed at the first dispatch end after which none   *)
(* of those is still running. In the parent, an outer dispatch's helper    *)
(* makes a temp while two other dispatches start and end, possibly always  *)
(* overlapping. A child forked during a dispatch starts with one dispatch  *)
(* that never ends, standing for the dispatches it did not inherit; its    *)
(* helper temps then live as long as the child.                            *)
EXTENDS Naturals

CONSTANTS Variant, Scenario
\* Variant "design": the design.
\* Variant "zero_rule": a helper temp is removed only when no dispatch runs.
\* Variant "no_phantom": a child starts with no running dispatch.
\* Scenario "parent" or "child".

Dispatches == {"outer", "l1", "l2", "phantom"}

(* --algorithm TempEpoch
variables
    running = [d \in Dispatches |-> d = "phantom" /\ Scenario = "child" /\ Variant /= "no_phantom"],
    older = {},
    temp = "none";

define
    Collectable ==
        IF Variant = "zero_rule"
        THEN \A d \in Dispatches : ~running[d]
        ELSE \A d \in older : ~running[d]
    \* Parent: the outer dispatch's helper uses the temp while the outer runs.
    \* Child: the helper uses it while the child lives.
    NoTempDeletedInUse ==
        temp = "deleted" => (Scenario = "parent" /\ ~running["outer"])
    HelperTempsAreReclaimed ==
        Scenario = "parent" => ((temp = "live" /\ ~running["outer"]) ~> temp = "deleted")
end define;

macro sweep() begin
  if temp = "live" /\ Collectable then
    temp := "deleted";
  end if;
end macro;

fair process Outer = "outer"
begin
  Begin:
    if Scenario = "parent" then
      running["outer"] := TRUE;
    end if;
  Helper:
    older := {d \in Dispatches : running[d]};
    temp := "live";
  End:
    if Scenario = "parent" then
      running["outer"] := FALSE;
      sweep();
    end if;
end process;

fair process Load \in {"l1", "l2"}
begin
  Loop:
    while TRUE do
      Start:
        \* A dispatch started now is younger than the temp.
        running[self] := TRUE;
        older := older \ {self};
      Stop:
        running[self] := FALSE;
        sweep();
    end while;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "8607f01a" /\ chksum(tla) = "162c94c4")
VARIABLES running, older, temp, pc

(* define statement *)
Collectable ==
    IF Variant = "zero_rule"
    THEN \A d \in Dispatches : ~running[d]
    ELSE \A d \in older : ~running[d]


NoTempDeletedInUse ==
    temp = "deleted" => (Scenario = "parent" /\ ~running["outer"])
HelperTempsAreReclaimed ==
    Scenario = "parent" => ((temp = "live" /\ ~running["outer"]) ~> temp = "deleted")


vars == << running, older, temp, pc >>

ProcSet == {"outer"} \cup ({"l1", "l2"})

Init == (* Global variables *)
        /\ running = [d \in Dispatches |-> d = "phantom" /\ Scenario = "child" /\ Variant /= "no_phantom"]
        /\ older = {}
        /\ temp = "none"
        /\ pc = [self \in ProcSet |-> CASE self = "outer" -> "Begin"
                                        [] self \in {"l1", "l2"} -> "Loop"]

Begin == /\ pc["outer"] = "Begin"
         /\ IF Scenario = "parent"
               THEN /\ running' = [running EXCEPT !["outer"] = TRUE]
               ELSE /\ TRUE
                    /\ UNCHANGED running
         /\ pc' = [pc EXCEPT !["outer"] = "Helper"]
         /\ UNCHANGED << older, temp >>

Helper == /\ pc["outer"] = "Helper"
          /\ older' = {d \in Dispatches : running[d]}
          /\ temp' = "live"
          /\ pc' = [pc EXCEPT !["outer"] = "End"]
          /\ UNCHANGED running

End == /\ pc["outer"] = "End"
       /\ IF Scenario = "parent"
             THEN /\ running' = [running EXCEPT !["outer"] = FALSE]
                  /\ IF temp = "live" /\ Collectable
                        THEN /\ temp' = "deleted"
                        ELSE /\ TRUE
                             /\ temp' = temp
             ELSE /\ TRUE
                  /\ UNCHANGED << running, temp >>
       /\ pc' = [pc EXCEPT !["outer"] = "Done"]
       /\ older' = older

Outer == Begin \/ Helper \/ End

Loop(self) == /\ pc[self] = "Loop"
              /\ pc' = [pc EXCEPT ![self] = "Start"]
              /\ UNCHANGED << running, older, temp >>

Start(self) == /\ pc[self] = "Start"
               /\ running' = [running EXCEPT ![self] = TRUE]
               /\ older' = older \ {self}
               /\ pc' = [pc EXCEPT ![self] = "Stop"]
               /\ temp' = temp

Stop(self) == /\ pc[self] = "Stop"
              /\ running' = [running EXCEPT ![self] = FALSE]
              /\ IF temp = "live" /\ Collectable
                    THEN /\ temp' = "deleted"
                    ELSE /\ TRUE
                         /\ temp' = temp
              /\ pc' = [pc EXCEPT ![self] = "Loop"]
              /\ older' = older

Load(self) == Loop(self) \/ Start(self) \/ Stop(self)

Next == Outer
           \/ (\E self \in {"l1", "l2"}: Load(self))

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Outer)
        /\ \A self \in {"l1", "l2"} : WF_vars(Load(self))

\* END TRANSLATION 
=============================================================================
