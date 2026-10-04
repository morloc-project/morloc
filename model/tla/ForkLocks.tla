------------------------------ MODULE ForkLocks ------------------------------
(* Process-wide locks across fork (model/fork.md FORK-5, FORK-7, FORK-8).  *)
(* Worker threads take ranked locks; the forking thread runs the prepare    *)
(* handler, forks, and the child, whose only thread is the forker, then     *)
(* takes every lock it uses.                                                *)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS Workers,  \* threads other than the forker
          Variant   \* which lock policy to check (below)

None == "none"
Locks == {"alloc", "map", "service"}
Rank == [l \in Locks |-> CASE l = "map" -> 1 [] l = "alloc" -> 2 [] l = "service" -> 3]

\* "policy": every lock is held across fork or reset in the child, and
\* prepare takes the held ones in rank order.
\* "uncovered": one lock is neither held nor reset.
\* "misordered": prepare takes the held locks against their rank.
\* "held_across_wait": a worker holds a held lock while it waits on another
\* process, which may never answer.
\* "fork_while_holding": the forking thread already holds a held lock, as
\* when user code forks from inside a callback the runtime made under it.
Held == {"map", "alloc"}
Reset == IF Variant = "uncovered" THEN {} ELSE {"service"}
PrepareOrder == IF Variant = "misordered" THEN <<"alloc", "map">> ELSE <<"map", "alloc">>

(* --algorithm ForkLocks
variables
    holder = [l \in Locks |-> None],
    childHolder = [l \in Locks |-> None],
    forked = FALSE,
    childReady = FALSE,
    childDone = FALSE;

define
    ChildNeverWaitsOnAThreadItLacks ==
        childReady => \A l \in Locks : childHolder[l] \in {None, "forker"}
    ChildFinishes == forked ~> childDone
end define;

fair process Worker \in Workers
variables want = {}, held = <<>>, stuck = FALSE;
begin
  Loop:
    while TRUE do
      Choose:
        with s \in SUBSET Locks, k \in BOOLEAN do
          want := s;
          stuck := k /\ Variant = "held_across_wait" /\ "alloc" \in s;
        end with;
      Take:
        while \E l \in want : l \notin {held[i] : i \in 1..Len(held)} do
          with l \in {m \in want : m \notin {held[i] : i \in 1..Len(held)}
                                   /\ \A n \in want : n \notin {held[i] : i \in 1..Len(held)} => Rank[m] <= Rank[n]} do
            await holder[l] = None;
            holder[l] := self;
            held := Append(held, l);
          end with;
        end while;
      Wait:
        await ~stuck;
      Drop:
        while held /= <<>> do
          holder[Head(held)] := None;
          held := Tail(held);
        end while;
    end while;
end process;

fair process Forker = "forker"
variables next = 1;
begin
  Hold:
    if Variant = "fork_while_holding" then
      await holder["alloc"] = None;
      holder["alloc"] := "forker";
    end if;
  Prepare:
    while next <= Len(PrepareOrder) do
      await holder[PrepareOrder[next]] = None;
      holder[PrepareOrder[next]] := "forker";
      next := next + 1;
    end while;
  Fork:
    childHolder := holder;
    forked := TRUE;
  AfterForkInParent:
    holder := [l \in Locks |-> IF l \in Held THEN None ELSE holder[l]];
  AfterForkInChild:
    childHolder := [l \in Locks |->
                      IF l \in Held \/ l \in Reset THEN None ELSE childHolder[l]];
    childReady := TRUE;
  ChildUsesLocks:
    await \A l \in Locks : childHolder[l] \in {None, "forker"};
    childDone := TRUE;
end process;

end algorithm; *)
\* BEGIN TRANSLATION
VARIABLES holder, childHolder, forked, childReady, childDone, pc

(* define statement *)
ChildNeverWaitsOnAThreadItLacks ==
    childReady => \A l \in Locks : childHolder[l] \in {None, "forker"}
ChildFinishes == forked ~> childDone

VARIABLES want, held, stuck, next

vars == << holder, childHolder, forked, childReady, childDone, pc, want, held, 
           stuck, next >>

ProcSet == (Workers) \cup {"forker"}

Init == (* Global variables *)
        /\ holder = [l \in Locks |-> None]
        /\ childHolder = [l \in Locks |-> None]
        /\ forked = FALSE
        /\ childReady = FALSE
        /\ childDone = FALSE
        (* Process Worker *)
        /\ want = [self \in Workers |-> {}]
        /\ held = [self \in Workers |-> <<>>]
        /\ stuck = [self \in Workers |-> FALSE]
        (* Process Forker *)
        /\ next = 1
        /\ pc = [self \in ProcSet |-> CASE self \in Workers -> "Loop"
                                        [] self = "forker" -> "Hold"]

Loop(self) == /\ pc[self] = "Loop"
              /\ pc' = [pc EXCEPT ![self] = "Choose"]
              /\ UNCHANGED << holder, childHolder, forked, childReady, 
                              childDone, want, held, stuck, next >>

Choose(self) == /\ pc[self] = "Choose"
                /\ \E s \in SUBSET Locks:
                     \E k \in BOOLEAN:
                       /\ want' = [want EXCEPT ![self] = s]
                       /\ stuck' = [stuck EXCEPT ![self] = k /\ Variant = "held_across_wait" /\ "alloc" \in s]
                /\ pc' = [pc EXCEPT ![self] = "Take"]
                /\ UNCHANGED << holder, childHolder, forked, childReady, 
                                childDone, held, next >>

Take(self) == /\ pc[self] = "Take"
              /\ IF \E l \in want[self] : l \notin {held[self][i] : i \in 1..Len(held[self])}
                    THEN /\ \E l \in {m \in want[self] : m \notin {held[self][i] : i \in 1..Len(held[self])}
                                                         /\ \A n \in want[self] : n \notin {held[self][i] : i \in 1..Len(held[self])} => Rank[m] <= Rank[n]}:
                              /\ holder[l] = None
                              /\ holder' = [holder EXCEPT ![l] = self]
                              /\ held' = [held EXCEPT ![self] = Append(held[self], l)]
                         /\ pc' = [pc EXCEPT ![self] = "Take"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Wait"]
                         /\ UNCHANGED << holder, held >>
              /\ UNCHANGED << childHolder, forked, childReady, childDone, want, 
                              stuck, next >>

Wait(self) == /\ pc[self] = "Wait"
              /\ ~stuck[self]
              /\ pc' = [pc EXCEPT ![self] = "Drop"]
              /\ UNCHANGED << holder, childHolder, forked, childReady, 
                              childDone, want, held, stuck, next >>

Drop(self) == /\ pc[self] = "Drop"
              /\ IF held[self] /= <<>>
                    THEN /\ holder' = [holder EXCEPT ![Head(held[self])] = None]
                         /\ held' = [held EXCEPT ![self] = Tail(held[self])]
                         /\ pc' = [pc EXCEPT ![self] = "Drop"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Loop"]
                         /\ UNCHANGED << holder, held >>
              /\ UNCHANGED << childHolder, forked, childReady, childDone, want, 
                              stuck, next >>

Worker(self) == Loop(self) \/ Choose(self) \/ Take(self) \/ Wait(self)
                   \/ Drop(self)

Hold == /\ pc["forker"] = "Hold"
        /\ IF Variant = "fork_while_holding"
              THEN /\ holder["alloc"] = None
                   /\ holder' = [holder EXCEPT !["alloc"] = "forker"]
              ELSE /\ TRUE
                   /\ UNCHANGED holder
        /\ pc' = [pc EXCEPT !["forker"] = "Prepare"]
        /\ UNCHANGED << childHolder, forked, childReady, childDone, want, held, 
                        stuck, next >>

Prepare == /\ pc["forker"] = "Prepare"
           /\ IF next <= Len(PrepareOrder)
                 THEN /\ holder[PrepareOrder[next]] = None
                      /\ holder' = [holder EXCEPT ![PrepareOrder[next]] = "forker"]
                      /\ next' = next + 1
                      /\ pc' = [pc EXCEPT !["forker"] = "Prepare"]
                 ELSE /\ pc' = [pc EXCEPT !["forker"] = "Fork"]
                      /\ UNCHANGED << holder, next >>
           /\ UNCHANGED << childHolder, forked, childReady, childDone, want, 
                           held, stuck >>

Fork == /\ pc["forker"] = "Fork"
        /\ childHolder' = holder
        /\ forked' = TRUE
        /\ pc' = [pc EXCEPT !["forker"] = "AfterForkInParent"]
        /\ UNCHANGED << holder, childReady, childDone, want, held, stuck, next >>

AfterForkInParent == /\ pc["forker"] = "AfterForkInParent"
                     /\ holder' = [l \in Locks |-> IF l \in Held THEN None ELSE holder[l]]
                     /\ pc' = [pc EXCEPT !["forker"] = "AfterForkInChild"]
                     /\ UNCHANGED << childHolder, forked, childReady, 
                                     childDone, want, held, stuck, next >>

AfterForkInChild == /\ pc["forker"] = "AfterForkInChild"
                    /\ childHolder' = [l \in Locks |->
                                         IF l \in Held \/ l \in Reset THEN None ELSE childHolder[l]]
                    /\ childReady' = TRUE
                    /\ pc' = [pc EXCEPT !["forker"] = "ChildUsesLocks"]
                    /\ UNCHANGED << holder, forked, childDone, want, held, 
                                    stuck, next >>

ChildUsesLocks == /\ pc["forker"] = "ChildUsesLocks"
                  /\ \A l \in Locks : childHolder[l] \in {None, "forker"}
                  /\ childDone' = TRUE
                  /\ pc' = [pc EXCEPT !["forker"] = "Done"]
                  /\ UNCHANGED << holder, childHolder, forked, childReady, 
                                  want, held, stuck, next >>

Forker == Hold \/ Prepare \/ Fork \/ AfterForkInParent \/ AfterForkInChild
             \/ ChildUsesLocks

Next == Forker
           \/ (\E self \in Workers: Worker(self))

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Workers : WF_vars(Worker(self))
        /\ WF_vars(Forker)

\* END TRANSLATION
=============================================================================
