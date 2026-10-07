----------------------------- MODULE OncePublish -----------------------------
(* A process-wide value set on first use (model/fork.md FORK-9). Threads    *)
(* build it outside any lock and publish by compare-and-swap; a loser       *)
(* destroys its own value and takes the published one, and only the winner  *)
(* performs the value's side effect, after publishing. Two parent threads race; a fork copies *)
(* the parent at any point, and a child thread then makes its first use.    *)
EXTENDS Naturals

CONSTANT Variant
\* "cas": the design.
\* "oncelock": a waiter blocks while another thread initialises, as the
\* standard library's once cell does.
\* "effect_each": every builder performs the side effect.

Users == {"p1", "p2", "c1"}
Gen(t) == IF t = "c1" THEN 1 ELSE 0
None == <<"none", 99>>

(* --algorithm OncePublish
variables
    pslot = None,
    cslot = None,
    prunning = FALSE,
    crunning = FALSE,
    peffects = 0,
    ceffects = 0,
    forked = FALSE,
    destroyed = {},
    used = [t \in Users |-> None];

define
    Slot(t) == IF Gen(t) = 1 THEN cslot ELSE pslot
    Running(t) == IF Gen(t) = 1 THEN crunning ELSE prunning
    NoUseOfADestroyedValue ==
        \A t \in Users : used[t] = None \/ used[t] \notin destroyed
    OneValuePerProcess ==
        used["p1"] /= None /\ used["p2"] /= None => used["p1"] = used["p2"]
    AtMostOneEffectPerProcess == peffects <= 1 /\ ceffects <= 1
    TheChildGetsAValue == <>(used["c1"] /= None)
end define;

macro set_slot(t, v) begin
  if Gen(t) = 1 then cslot := v; else pslot := v; end if;
end macro;

macro effect(t) begin
  if Gen(t) = 1 then ceffects := ceffects + 1; else peffects := peffects + 1; end if;
end macro;

fair process User \in Users
variables mine = None;
begin
  Start:
    if Gen(self) = 1 then
      await forked;
    end if;
  Load:
    if Slot(self) /= None then
      mine := Slot(self);
      goto Use;
    end if;
  Claim:
    if Variant = "oncelock" then
      if Running(self) then
        await Slot(self) /= None;
        mine := Slot(self);
        goto Use;
      elsif Gen(self) = 1 then
        crunning := TRUE;
      else
        prunning := TRUE;
      end if;
    end if;
  Build:
    mine := <<self, Gen(self)>>;
  Publish:
    if Slot(self) = None then
      set_slot(self, mine);
    else
      destroyed := destroyed \union {mine};
      if Variant = "effect_each" then
        effect(self);
      end if;
      mine := Slot(self);
      goto Use;
    end if;
  \* The winner's side effect, a separate step: a fork can land between.
  Effect:
    effect(self);
  Use:
    used[self] := mine;
end process;

fair process Forker = "forker"
begin
  Fork:
    cslot := pslot;
    crunning := prunning;
    ceffects := peffects;
    forked := TRUE;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "ae6da616" /\ chksum(tla) = "6d173593")
VARIABLES pslot, cslot, prunning, crunning, peffects, ceffects, forked, 
          destroyed, used, pc

(* define statement *)
Slot(t) == IF Gen(t) = 1 THEN cslot ELSE pslot
Running(t) == IF Gen(t) = 1 THEN crunning ELSE prunning
NoUseOfADestroyedValue ==
    \A t \in Users : used[t] = None \/ used[t] \notin destroyed
OneValuePerProcess ==
    used["p1"] /= None /\ used["p2"] /= None => used["p1"] = used["p2"]
AtMostOneEffectPerProcess == peffects <= 1 /\ ceffects <= 1
TheChildGetsAValue == <>(used["c1"] /= None)

VARIABLE mine

vars == << pslot, cslot, prunning, crunning, peffects, ceffects, forked, 
           destroyed, used, pc, mine >>

ProcSet == (Users) \cup {"forker"}

Init == (* Global variables *)
        /\ pslot = None
        /\ cslot = None
        /\ prunning = FALSE
        /\ crunning = FALSE
        /\ peffects = 0
        /\ ceffects = 0
        /\ forked = FALSE
        /\ destroyed = {}
        /\ used = [t \in Users |-> None]
        (* Process User *)
        /\ mine = [self \in Users |-> None]
        /\ pc = [self \in ProcSet |-> CASE self \in Users -> "Start"
                                        [] self = "forker" -> "Fork"]

Start(self) == /\ pc[self] = "Start"
               /\ IF Gen(self) = 1
                     THEN /\ forked
                     ELSE /\ TRUE
               /\ pc' = [pc EXCEPT ![self] = "Load"]
               /\ UNCHANGED << pslot, cslot, prunning, crunning, peffects, 
                               ceffects, forked, destroyed, used, mine >>

Load(self) == /\ pc[self] = "Load"
              /\ IF Slot(self) /= None
                    THEN /\ mine' = [mine EXCEPT ![self] = Slot(self)]
                         /\ pc' = [pc EXCEPT ![self] = "Use"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Claim"]
                         /\ mine' = mine
              /\ UNCHANGED << pslot, cslot, prunning, crunning, peffects, 
                              ceffects, forked, destroyed, used >>

Claim(self) == /\ pc[self] = "Claim"
               /\ IF Variant = "oncelock"
                     THEN /\ IF Running(self)
                                THEN /\ Slot(self) /= None
                                     /\ mine' = [mine EXCEPT ![self] = Slot(self)]
                                     /\ pc' = [pc EXCEPT ![self] = "Use"]
                                     /\ UNCHANGED << prunning, crunning >>
                                ELSE /\ IF Gen(self) = 1
                                           THEN /\ crunning' = TRUE
                                                /\ UNCHANGED prunning
                                           ELSE /\ prunning' = TRUE
                                                /\ UNCHANGED crunning
                                     /\ pc' = [pc EXCEPT ![self] = "Build"]
                                     /\ mine' = mine
                     ELSE /\ pc' = [pc EXCEPT ![self] = "Build"]
                          /\ UNCHANGED << prunning, crunning, mine >>
               /\ UNCHANGED << pslot, cslot, peffects, ceffects, forked, 
                               destroyed, used >>

Build(self) == /\ pc[self] = "Build"
               /\ mine' = [mine EXCEPT ![self] = <<self, Gen(self)>>]
               /\ pc' = [pc EXCEPT ![self] = "Publish"]
               /\ UNCHANGED << pslot, cslot, prunning, crunning, peffects, 
                               ceffects, forked, destroyed, used >>

Publish(self) == /\ pc[self] = "Publish"
                 /\ IF Slot(self) = None
                       THEN /\ IF Gen(self) = 1
                                  THEN /\ cslot' = mine[self]
                                       /\ pslot' = pslot
                                  ELSE /\ pslot' = mine[self]
                                       /\ cslot' = cslot
                            /\ pc' = [pc EXCEPT ![self] = "Effect"]
                            /\ UNCHANGED << peffects, ceffects, destroyed, 
                                            mine >>
                       ELSE /\ destroyed' = (destroyed \union {mine[self]})
                            /\ IF Variant = "effect_each"
                                  THEN /\ IF Gen(self) = 1
                                             THEN /\ ceffects' = ceffects + 1
                                                  /\ UNCHANGED peffects
                                             ELSE /\ peffects' = peffects + 1
                                                  /\ UNCHANGED ceffects
                                  ELSE /\ TRUE
                                       /\ UNCHANGED << peffects, ceffects >>
                            /\ mine' = [mine EXCEPT ![self] = Slot(self)]
                            /\ pc' = [pc EXCEPT ![self] = "Use"]
                            /\ UNCHANGED << pslot, cslot >>
                 /\ UNCHANGED << prunning, crunning, forked, used >>

Effect(self) == /\ pc[self] = "Effect"
                /\ IF Gen(self) = 1
                      THEN /\ ceffects' = ceffects + 1
                           /\ UNCHANGED peffects
                      ELSE /\ peffects' = peffects + 1
                           /\ UNCHANGED ceffects
                /\ pc' = [pc EXCEPT ![self] = "Use"]
                /\ UNCHANGED << pslot, cslot, prunning, crunning, forked, 
                                destroyed, used, mine >>

Use(self) == /\ pc[self] = "Use"
             /\ used' = [used EXCEPT ![self] = mine[self]]
             /\ pc' = [pc EXCEPT ![self] = "Done"]
             /\ UNCHANGED << pslot, cslot, prunning, crunning, peffects, 
                             ceffects, forked, destroyed, mine >>

User(self) == Start(self) \/ Load(self) \/ Claim(self) \/ Build(self)
                 \/ Publish(self) \/ Effect(self) \/ Use(self)

Fork == /\ pc["forker"] = "Fork"
        /\ cslot' = pslot
        /\ crunning' = prunning
        /\ ceffects' = peffects
        /\ forked' = TRUE
        /\ pc' = [pc EXCEPT !["forker"] = "Done"]
        /\ UNCHANGED << pslot, prunning, peffects, destroyed, used, mine >>

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
