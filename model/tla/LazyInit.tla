------------------------------ MODULE LazyInit ------------------------------
(* Initialising a process-wide value on first use (model/state.md INIT-1). *)
(* Two threads may make their first use at once, and another thread may     *)
(* fork at any moment. A created value is owned by one static slot;         *)
(* installing a new one there destroys the previous one.                    *)
EXTENDS Naturals

CONSTANT Variant
\* "locked": a held init lock, checked again once taken.
\* "unlocked": check, then create and install, with no lock.
\* "unheld": locked, but the lock is not held across fork.
\* "outside": create without the lock, then take it only to check again and
\* install; a thread that finds a value already installed destroys its own.
\* "outside_blind": as "outside", but installs without checking again.

Users == {"t1", "t2"}
None == "none"

(* --algorithm LazyInit
variables
    base = None,
    slot = None,
    destroyed = {},
    lockHolder = None,
    used = [t \in Users |-> None],
    childLock = None,
    childReady = FALSE,
    childDone = FALSE;

define
    NoUseOfADestroyedValue ==
        /\ \A t \in Users : used[t] = None \/ used[t] \notin destroyed
        /\ base \notin destroyed
    ChildNeverWaitsOnAThreadItLacks ==
        childReady => childLock \in {None, "forker"}
    ChildFinishes == <>childDone
    PrepareNeverWaitsOnACreation ==
        ~(pc["forker"] = "Prepare" /\ lockHolder \in Users /\ pc[lockHolder] = "Create")
end define;

fair process User \in Users
variables mine = None;
begin
  Check:
    if base /= None then
      mine := base;
      goto Use;
    end if;
  Early:
    if Variant \in {"outside", "outside_blind"} then
      mine := self;
    end if;
  Lock:
    if Variant /= "unlocked" then
      await lockHolder = None;
      lockHolder := self;
    end if;
  Recheck:
    if Variant \notin {"unlocked", "outside_blind"} /\ base /= None then
      if Variant = "outside" then
        destroyed := destroyed \union {mine};
      end if;
      mine := base;
      goto Unlock;
    elsif Variant \in {"outside", "outside_blind"} then
      goto Install;
    end if;
  Create:
    mine := self;
  Install:
    if slot /= None then
      destroyed := destroyed \union {slot};
    end if;
    slot := mine;
  Publish:
    base := mine;
  Unlock:
    if Variant /= "unlocked" then
      lockHolder := None;
    end if;
  Use:
    used[self] := mine;
end process;

fair process Forker = "forker"
begin
  Prepare:
    if Variant \in {"locked", "outside", "outside_blind"} then
      await lockHolder = None;
      lockHolder := "forker";
    end if;
  Fork:
    childLock := lockHolder;
  AfterFork:
    if Variant \in {"locked", "outside", "outside_blind"} then
      lockHolder := None;
      childLock := None;
    end if;
    childReady := TRUE;
  ChildInits:
    await childLock \in {None, "forker"};
    childDone := TRUE;
end process;

end algorithm; *)
\* BEGIN TRANSLATION
VARIABLES base, slot, destroyed, lockHolder, used, childLock, childReady, 
          childDone, pc

(* define statement *)
NoUseOfADestroyedValue ==
    /\ \A t \in Users : used[t] = None \/ used[t] \notin destroyed
    /\ base \notin destroyed
ChildNeverWaitsOnAThreadItLacks ==
    childReady => childLock \in {None, "forker"}
ChildFinishes == <>childDone
PrepareNeverWaitsOnACreation ==
    ~(pc["forker"] = "Prepare" /\ lockHolder \in Users /\ pc[lockHolder] = "Create")

VARIABLE mine

vars == << base, slot, destroyed, lockHolder, used, childLock, childReady, 
           childDone, pc, mine >>

ProcSet == (Users) \cup {"forker"}

Init == (* Global variables *)
        /\ base = None
        /\ slot = None
        /\ destroyed = {}
        /\ lockHolder = None
        /\ used = [t \in Users |-> None]
        /\ childLock = None
        /\ childReady = FALSE
        /\ childDone = FALSE
        (* Process User *)
        /\ mine = [self \in Users |-> None]
        /\ pc = [self \in ProcSet |-> CASE self \in Users -> "Check"
                                        [] self = "forker" -> "Prepare"]

Check(self) == /\ pc[self] = "Check"
               /\ IF base /= None
                     THEN /\ mine' = [mine EXCEPT ![self] = base]
                          /\ pc' = [pc EXCEPT ![self] = "Use"]
                     ELSE /\ pc' = [pc EXCEPT ![self] = "Early"]
                          /\ mine' = mine
               /\ UNCHANGED << base, slot, destroyed, lockHolder, used, 
                               childLock, childReady, childDone >>

Early(self) == /\ pc[self] = "Early"
               /\ IF Variant \in {"outside", "outside_blind"}
                     THEN /\ mine' = [mine EXCEPT ![self] = self]
                     ELSE /\ TRUE
                          /\ mine' = mine
               /\ pc' = [pc EXCEPT ![self] = "Lock"]
               /\ UNCHANGED << base, slot, destroyed, lockHolder, used, 
                               childLock, childReady, childDone >>

Lock(self) == /\ pc[self] = "Lock"
              /\ IF Variant /= "unlocked"
                    THEN /\ lockHolder = None
                         /\ lockHolder' = self
                    ELSE /\ TRUE
                         /\ UNCHANGED lockHolder
              /\ pc' = [pc EXCEPT ![self] = "Recheck"]
              /\ UNCHANGED << base, slot, destroyed, used, childLock, 
                              childReady, childDone, mine >>

Recheck(self) == /\ pc[self] = "Recheck"
                 /\ IF Variant \notin {"unlocked", "outside_blind"} /\ base /= None
                       THEN /\ IF Variant = "outside"
                                  THEN /\ destroyed' = (destroyed \union {mine[self]})
                                  ELSE /\ TRUE
                                       /\ UNCHANGED destroyed
                            /\ mine' = [mine EXCEPT ![self] = base]
                            /\ pc' = [pc EXCEPT ![self] = "Unlock"]
                       ELSE /\ IF Variant \in {"outside", "outside_blind"}
                                  THEN /\ pc' = [pc EXCEPT ![self] = "Install"]
                                  ELSE /\ pc' = [pc EXCEPT ![self] = "Create"]
                            /\ UNCHANGED << destroyed, mine >>
                 /\ UNCHANGED << base, slot, lockHolder, used, childLock, 
                                 childReady, childDone >>

Create(self) == /\ pc[self] = "Create"
                /\ mine' = [mine EXCEPT ![self] = self]
                /\ pc' = [pc EXCEPT ![self] = "Install"]
                /\ UNCHANGED << base, slot, destroyed, lockHolder, used, 
                                childLock, childReady, childDone >>

Install(self) == /\ pc[self] = "Install"
                 /\ IF slot /= None
                       THEN /\ destroyed' = (destroyed \union {slot})
                       ELSE /\ TRUE
                            /\ UNCHANGED destroyed
                 /\ slot' = mine[self]
                 /\ pc' = [pc EXCEPT ![self] = "Publish"]
                 /\ UNCHANGED << base, lockHolder, used, childLock, childReady, 
                                 childDone, mine >>

Publish(self) == /\ pc[self] = "Publish"
                 /\ base' = mine[self]
                 /\ pc' = [pc EXCEPT ![self] = "Unlock"]
                 /\ UNCHANGED << slot, destroyed, lockHolder, used, childLock, 
                                 childReady, childDone, mine >>

Unlock(self) == /\ pc[self] = "Unlock"
                /\ IF Variant /= "unlocked"
                      THEN /\ lockHolder' = None
                      ELSE /\ TRUE
                           /\ UNCHANGED lockHolder
                /\ pc' = [pc EXCEPT ![self] = "Use"]
                /\ UNCHANGED << base, slot, destroyed, used, childLock, 
                                childReady, childDone, mine >>

Use(self) == /\ pc[self] = "Use"
             /\ used' = [used EXCEPT ![self] = mine[self]]
             /\ pc' = [pc EXCEPT ![self] = "Done"]
             /\ UNCHANGED << base, slot, destroyed, lockHolder, childLock, 
                             childReady, childDone, mine >>

User(self) == Check(self) \/ Early(self) \/ Lock(self) \/ Recheck(self)
                 \/ Create(self) \/ Install(self) \/ Publish(self)
                 \/ Unlock(self) \/ Use(self)

Prepare == /\ pc["forker"] = "Prepare"
           /\ IF Variant \in {"locked", "outside", "outside_blind"}
                 THEN /\ lockHolder = None
                      /\ lockHolder' = "forker"
                 ELSE /\ TRUE
                      /\ UNCHANGED lockHolder
           /\ pc' = [pc EXCEPT !["forker"] = "Fork"]
           /\ UNCHANGED << base, slot, destroyed, used, childLock, childReady, 
                           childDone, mine >>

Fork == /\ pc["forker"] = "Fork"
        /\ childLock' = lockHolder
        /\ pc' = [pc EXCEPT !["forker"] = "AfterFork"]
        /\ UNCHANGED << base, slot, destroyed, lockHolder, used, childReady, 
                        childDone, mine >>

AfterFork == /\ pc["forker"] = "AfterFork"
             /\ IF Variant \in {"locked", "outside", "outside_blind"}
                   THEN /\ lockHolder' = None
                        /\ childLock' = None
                   ELSE /\ TRUE
                        /\ UNCHANGED << lockHolder, childLock >>
             /\ childReady' = TRUE
             /\ pc' = [pc EXCEPT !["forker"] = "ChildInits"]
             /\ UNCHANGED << base, slot, destroyed, used, childDone, mine >>

ChildInits == /\ pc["forker"] = "ChildInits"
              /\ childLock \in {None, "forker"}
              /\ childDone' = TRUE
              /\ pc' = [pc EXCEPT !["forker"] = "Done"]
              /\ UNCHANGED << base, slot, destroyed, lockHolder, used, 
                              childLock, childReady, mine >>

Forker == Prepare \/ Fork \/ AfterFork \/ ChildInits

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
