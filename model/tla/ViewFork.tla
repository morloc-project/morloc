------------------------------- MODULE ViewFork -------------------------------
(* Views inherited across fork (model/fork.md FORK-15). A view holds a      *)
(* reference on the shared block it reads and is recorded in the process's *)
(* view registry. A fork copies the views; the child may read them after   *)
(* the parent has released its own. The fork handler holds the registry   *)
(* across the fork and takes one reference per registered view, recorded  *)
(* in the child's lease; the child releases none of them, and the lease    *)
(* gives them all back once the child is gone, including one for a view   *)
(* that was already dead at the fork. Meanwhile another parent thread      *)
(* creates view v2 and releases v1.                                        *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "unlocked": the registry is read before the fork without being held, so
\* a view registered in between reaches the child without a reference.
\* "no_child_ref": the fork takes no reference for the child.
\* "child_releases": the child also releases an inherited reference when it
\* drops the view, so the lease releases it a second time.

Views == {"v1", "v2"}

(* --algorithm ViewFork
variables
    ref = [v \in Views |-> IF v = "v1" THEN 1 ELSE 0],
    live = [v \in Views |-> v = "v1"],
    registry = {"v1"},
    lockHeld = FALSE,
    forked = FALSE,
    childRefs = {},
    childViews = {},
    childLive = [v \in Views |-> FALSE],
    freedRead = FALSE,
    childDone = FALSE;

define
    NoReadOfAFreedBlock == ~freedRead
    NothingLeaks == (childDone /\ \A p \in {"t1", "t2"} : pc[p] = "Done")
                        => \A v \in Views : ref[v] = 0
    NoReleaseOfAReleasedBlock == \A v \in Views : ref[v] >= 0
end define;

fair process Creator = "t2"
begin
  Incref:
    ref["v2"] := ref["v2"] + 1;
    live["v2"] := TRUE;
  Register:
    await ~lockHeld;
    registry := registry \union {"v2"};
  Release2:
    live["v2"] := FALSE;
  Forget2:
    await ~lockHeld;
    registry := registry \ {"v2"};
    ref["v2"] := ref["v2"] - 1;
end process;

fair process Releaser = "t1"
begin
  Dead:
    live["v1"] := FALSE;
  Forget:
    await ~lockHeld;
    registry := registry \ {"v1"};
    ref["v1"] := ref["v1"] - 1;
end process;

fair process Forker = "fork"
variables snap = {};
begin
  Prepare:
    if Variant = "unlocked" then
      snap := registry;
    else
      await ~lockHeld;
      lockHeld := TRUE;
      snap := registry;
    end if;
  TakeRefs:
    if Variant /= "no_child_ref" then
      ref := [v \in Views |-> IF v \in snap THEN ref[v] + 1 ELSE ref[v]];
      childRefs := snap;
    end if;
  Fork:
    childViews := registry;
    childLive := live;
    forked := TRUE;
    lockHeld := FALSE;
end process;

fair process Child = "child"
variables todo = {};
begin
  Start:
    \* Only a live view has a language object to read and release it.
    await forked;
    todo := {v \in childViews : childLive[v]};
  Read:
    while todo /= {} do
      with v \in todo do
        if ref[v] = 0 then
          freedRead := TRUE;
        end if;
        if Variant = "child_releases" /\ v \in childRefs then
          ref[v] := ref[v] - 1;
        end if;
        todo := todo \ {v};
      end with;
    end while;
  Reclaim:
    \* The child is gone; its lease gives back every reference taken for it.
    ref := [v \in Views |-> IF v \in childRefs THEN ref[v] - 1 ELSE ref[v]];
  Finish:
    childDone := TRUE;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "fe0c26e9" /\ chksum(tla) = "24f602c1")
VARIABLES ref, live, registry, lockHeld, forked, childRefs, childViews, 
          childLive, freedRead, childDone, pc

(* define statement *)
NoReadOfAFreedBlock == ~freedRead
NothingLeaks == (childDone /\ \A p \in {"t1", "t2"} : pc[p] = "Done")
                    => \A v \in Views : ref[v] = 0
NoReleaseOfAReleasedBlock == \A v \in Views : ref[v] >= 0

VARIABLES snap, todo

vars == << ref, live, registry, lockHeld, forked, childRefs, childViews, 
           childLive, freedRead, childDone, pc, snap, todo >>

ProcSet == {"t2"} \cup {"t1"} \cup {"fork"} \cup {"child"}

Init == (* Global variables *)
        /\ ref = [v \in Views |-> IF v = "v1" THEN 1 ELSE 0]
        /\ live = [v \in Views |-> v = "v1"]
        /\ registry = {"v1"}
        /\ lockHeld = FALSE
        /\ forked = FALSE
        /\ childRefs = {}
        /\ childViews = {}
        /\ childLive = [v \in Views |-> FALSE]
        /\ freedRead = FALSE
        /\ childDone = FALSE
        (* Process Forker *)
        /\ snap = {}
        (* Process Child *)
        /\ todo = {}
        /\ pc = [self \in ProcSet |-> CASE self = "t2" -> "Incref"
                                        [] self = "t1" -> "Dead"
                                        [] self = "fork" -> "Prepare"
                                        [] self = "child" -> "Start"]

Incref == /\ pc["t2"] = "Incref"
          /\ ref' = [ref EXCEPT !["v2"] = ref["v2"] + 1]
          /\ live' = [live EXCEPT !["v2"] = TRUE]
          /\ pc' = [pc EXCEPT !["t2"] = "Register"]
          /\ UNCHANGED << registry, lockHeld, forked, childRefs, childViews, 
                          childLive, freedRead, childDone, snap, todo >>

Register == /\ pc["t2"] = "Register"
            /\ ~lockHeld
            /\ registry' = (registry \union {"v2"})
            /\ pc' = [pc EXCEPT !["t2"] = "Release2"]
            /\ UNCHANGED << ref, live, lockHeld, forked, childRefs, childViews, 
                            childLive, freedRead, childDone, snap, todo >>

Release2 == /\ pc["t2"] = "Release2"
            /\ live' = [live EXCEPT !["v2"] = FALSE]
            /\ pc' = [pc EXCEPT !["t2"] = "Forget2"]
            /\ UNCHANGED << ref, registry, lockHeld, forked, childRefs, 
                            childViews, childLive, freedRead, childDone, snap, 
                            todo >>

Forget2 == /\ pc["t2"] = "Forget2"
           /\ ~lockHeld
           /\ registry' = registry \ {"v2"}
           /\ ref' = [ref EXCEPT !["v2"] = ref["v2"] - 1]
           /\ pc' = [pc EXCEPT !["t2"] = "Done"]
           /\ UNCHANGED << live, lockHeld, forked, childRefs, childViews, 
                           childLive, freedRead, childDone, snap, todo >>

Creator == Incref \/ Register \/ Release2 \/ Forget2

Dead == /\ pc["t1"] = "Dead"
        /\ live' = [live EXCEPT !["v1"] = FALSE]
        /\ pc' = [pc EXCEPT !["t1"] = "Forget"]
        /\ UNCHANGED << ref, registry, lockHeld, forked, childRefs, childViews, 
                        childLive, freedRead, childDone, snap, todo >>

Forget == /\ pc["t1"] = "Forget"
          /\ ~lockHeld
          /\ registry' = registry \ {"v1"}
          /\ ref' = [ref EXCEPT !["v1"] = ref["v1"] - 1]
          /\ pc' = [pc EXCEPT !["t1"] = "Done"]
          /\ UNCHANGED << live, lockHeld, forked, childRefs, childViews, 
                          childLive, freedRead, childDone, snap, todo >>

Releaser == Dead \/ Forget

Prepare == /\ pc["fork"] = "Prepare"
           /\ IF Variant = "unlocked"
                 THEN /\ snap' = registry
                      /\ UNCHANGED lockHeld
                 ELSE /\ ~lockHeld
                      /\ lockHeld' = TRUE
                      /\ snap' = registry
           /\ pc' = [pc EXCEPT !["fork"] = "TakeRefs"]
           /\ UNCHANGED << ref, live, registry, forked, childRefs, childViews, 
                           childLive, freedRead, childDone, todo >>

TakeRefs == /\ pc["fork"] = "TakeRefs"
            /\ IF Variant /= "no_child_ref"
                  THEN /\ ref' = [v \in Views |-> IF v \in snap THEN ref[v] + 1 ELSE ref[v]]
                       /\ childRefs' = snap
                  ELSE /\ TRUE
                       /\ UNCHANGED << ref, childRefs >>
            /\ pc' = [pc EXCEPT !["fork"] = "Fork"]
            /\ UNCHANGED << live, registry, lockHeld, forked, childViews, 
                            childLive, freedRead, childDone, snap, todo >>

Fork == /\ pc["fork"] = "Fork"
        /\ childViews' = registry
        /\ childLive' = live
        /\ forked' = TRUE
        /\ lockHeld' = FALSE
        /\ pc' = [pc EXCEPT !["fork"] = "Done"]
        /\ UNCHANGED << ref, live, registry, childRefs, freedRead, childDone, 
                        snap, todo >>

Forker == Prepare \/ TakeRefs \/ Fork

Start == /\ pc["child"] = "Start"
         /\ forked
         /\ todo' = {v \in childViews : childLive[v]}
         /\ pc' = [pc EXCEPT !["child"] = "Read"]
         /\ UNCHANGED << ref, live, registry, lockHeld, forked, childRefs, 
                         childViews, childLive, freedRead, childDone, snap >>

Read == /\ pc["child"] = "Read"
        /\ IF todo /= {}
              THEN /\ \E v \in todo:
                        /\ IF ref[v] = 0
                              THEN /\ freedRead' = TRUE
                              ELSE /\ TRUE
                                   /\ UNCHANGED freedRead
                        /\ IF Variant = "child_releases" /\ v \in childRefs
                              THEN /\ ref' = [ref EXCEPT ![v] = ref[v] - 1]
                              ELSE /\ TRUE
                                   /\ ref' = ref
                        /\ todo' = todo \ {v}
                   /\ pc' = [pc EXCEPT !["child"] = "Read"]
              ELSE /\ pc' = [pc EXCEPT !["child"] = "Reclaim"]
                   /\ UNCHANGED << ref, freedRead, todo >>
        /\ UNCHANGED << live, registry, lockHeld, forked, childRefs, 
                        childViews, childLive, childDone, snap >>

Reclaim == /\ pc["child"] = "Reclaim"
           /\ ref' = [v \in Views |-> IF v \in childRefs THEN ref[v] - 1 ELSE ref[v]]
           /\ pc' = [pc EXCEPT !["child"] = "Finish"]
           /\ UNCHANGED << live, registry, lockHeld, forked, childRefs, 
                           childViews, childLive, freedRead, childDone, snap, 
                           todo >>

Finish == /\ pc["child"] = "Finish"
          /\ childDone' = TRUE
          /\ pc' = [pc EXCEPT !["child"] = "Done"]
          /\ UNCHANGED << ref, live, registry, lockHeld, forked, childRefs, 
                          childViews, childLive, freedRead, snap, todo >>

Child == Start \/ Read \/ Reclaim \/ Finish

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Creator \/ Releaser \/ Forker \/ Child
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Creator)
        /\ WF_vars(Releaser)
        /\ WF_vars(Forker)
        /\ WF_vars(Child)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
