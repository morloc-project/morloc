---------------------------- MODULE ResetPublish ----------------------------
(* A reset value (model/runtime/fork.md FORK-8): a static slot holds a pointer to *)
(* the current instance, tagged with the fork generation that built it.     *)
(* A user that finds no instance of its own generation builds one and       *)
(* publishes it by compare-and-swap; the loser of a race destroys only its  *)
(* own unpublished instance and loads again. The parent's instance is never *)
(* destroyed in the child. Two parent threads and two child threads race;   *)
(* the fork copies the slot at any point.                                   *)
EXTENDS Naturals

CONSTANT Variant
\* "cas": the design.
\* "store": publish with a plain store instead of compare-and-swap.
\* "no_generation": accept any instance, whatever generation built it.
\* "free_observed": the loser destroys the instance it lost to.

Users == {"p1", "p2", "c1", "c2"}
Gen(t) == IF t \in {"c1", "c2"} THEN 1 ELSE 0
None == <<"none", 99>>

(* --algorithm ResetPublish
variables
    pslot = None,
    cslot = None,
    forked = FALSE,
    destroyed = {},
    used = [t \in Users |-> None];

define
    Slot(t) == IF Gen(t) = 1 THEN cslot ELSE pslot
    NoUseOfADestroyedValue ==
        \A t \in Users : used[t] = None \/ used[t] \notin destroyed
    OneInstancePerGeneration ==
        \A s, t \in Users :
            (used[s] /= None /\ used[t] /= None /\ Gen(s) = Gen(t)) => used[s] = used[t]
    ChildNeverUsesTheParentsInstance ==
        \A t \in Users : used[t] = None \/ used[t][2] = Gen(t)
    EveryUserFinishes == <>(\A t \in Users : used[t] /= None)
end define;

fair process User \in Users
variables seen = None, mine = None;
begin
  Start:
    if Gen(self) = 1 then
      await forked;
    end if;
  Load:
    seen := Slot(self);
  Check:
    if seen /= None /\ (Variant = "no_generation" \/ seen[2] = Gen(self)) then
      mine := seen;
      goto Use;
    end if;
  Build:
    mine := <<self, Gen(self)>>;
  Publish:
    if Variant = "store" \/ Slot(self) = seen then
      if Gen(self) = 1 then
        cslot := mine;
      else
        pslot := mine;
      end if;
      goto Use;
    else
      if Variant = "free_observed" then
        destroyed := destroyed \union {Slot(self)};
      else
        destroyed := destroyed \union {mine};
      end if;
      goto Load;
    end if;
  Use:
    used[self] := mine;
end process;

fair process Forker = "forker"
begin
  Fork:
    cslot := pslot;
    forked := TRUE;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "4e8e9f70" /\ chksum(tla) = "61b59d31")
VARIABLES pslot, cslot, forked, destroyed, used, pc

(* define statement *)
Slot(t) == IF Gen(t) = 1 THEN cslot ELSE pslot
NoUseOfADestroyedValue ==
    \A t \in Users : used[t] = None \/ used[t] \notin destroyed
OneInstancePerGeneration ==
    \A s, t \in Users :
        (used[s] /= None /\ used[t] /= None /\ Gen(s) = Gen(t)) => used[s] = used[t]
ChildNeverUsesTheParentsInstance ==
    \A t \in Users : used[t] = None \/ used[t][2] = Gen(t)
EveryUserFinishes == <>(\A t \in Users : used[t] /= None)

VARIABLES seen, mine

vars == << pslot, cslot, forked, destroyed, used, pc, seen, mine >>

ProcSet == (Users) \cup {"forker"}

Init == (* Global variables *)
        /\ pslot = None
        /\ cslot = None
        /\ forked = FALSE
        /\ destroyed = {}
        /\ used = [t \in Users |-> None]
        (* Process User *)
        /\ seen = [self \in Users |-> None]
        /\ mine = [self \in Users |-> None]
        /\ pc = [self \in ProcSet |-> CASE self \in Users -> "Start"
                                        [] self = "forker" -> "Fork"]

Start(self) == /\ pc[self] = "Start"
               /\ IF Gen(self) = 1
                     THEN /\ forked
                     ELSE /\ TRUE
               /\ pc' = [pc EXCEPT ![self] = "Load"]
               /\ UNCHANGED << pslot, cslot, forked, destroyed, used, seen, 
                               mine >>

Load(self) == /\ pc[self] = "Load"
              /\ seen' = [seen EXCEPT ![self] = Slot(self)]
              /\ pc' = [pc EXCEPT ![self] = "Check"]
              /\ UNCHANGED << pslot, cslot, forked, destroyed, used, mine >>

Check(self) == /\ pc[self] = "Check"
               /\ IF seen[self] /= None /\ (Variant = "no_generation" \/ seen[self][2] = Gen(self))
                     THEN /\ mine' = [mine EXCEPT ![self] = seen[self]]
                          /\ pc' = [pc EXCEPT ![self] = "Use"]
                     ELSE /\ pc' = [pc EXCEPT ![self] = "Build"]
                          /\ mine' = mine
               /\ UNCHANGED << pslot, cslot, forked, destroyed, used, seen >>

Build(self) == /\ pc[self] = "Build"
               /\ mine' = [mine EXCEPT ![self] = <<self, Gen(self)>>]
               /\ pc' = [pc EXCEPT ![self] = "Publish"]
               /\ UNCHANGED << pslot, cslot, forked, destroyed, used, seen >>

Publish(self) == /\ pc[self] = "Publish"
                 /\ IF Variant = "store" \/ Slot(self) = seen[self]
                       THEN /\ IF Gen(self) = 1
                                  THEN /\ cslot' = mine[self]
                                       /\ pslot' = pslot
                                  ELSE /\ pslot' = mine[self]
                                       /\ cslot' = cslot
                            /\ pc' = [pc EXCEPT ![self] = "Use"]
                            /\ UNCHANGED destroyed
                       ELSE /\ IF Variant = "free_observed"
                                  THEN /\ destroyed' = (destroyed \union {Slot(self)})
                                  ELSE /\ destroyed' = (destroyed \union {mine[self]})
                            /\ pc' = [pc EXCEPT ![self] = "Load"]
                            /\ UNCHANGED << pslot, cslot >>
                 /\ UNCHANGED << forked, used, seen, mine >>

Use(self) == /\ pc[self] = "Use"
             /\ used' = [used EXCEPT ![self] = mine[self]]
             /\ pc' = [pc EXCEPT ![self] = "Done"]
             /\ UNCHANGED << pslot, cslot, forked, destroyed, seen, mine >>

User(self) == Start(self) \/ Load(self) \/ Check(self) \/ Build(self)
                 \/ Publish(self) \/ Use(self)

Fork == /\ pc["forker"] = "Fork"
        /\ cslot' = pslot
        /\ forked' = TRUE
        /\ pc' = [pc EXCEPT !["forker"] = "Done"]
        /\ UNCHANGED << pslot, destroyed, used, seen, mine >>

Forker == Fork

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Forker
           \/ (\E self \in Users: User(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Users : WF_vars(User(self))
        /\ WF_vars(Forker)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
