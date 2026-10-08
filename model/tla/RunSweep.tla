------------------------------ MODULE RunSweep ------------------------------
(* Removing a dead run's shared memory (model/daemon.md DAEMON-13). The    *)
(* run directory holds a marker for every object, written before the       *)
(* object is made; a create whose marker cannot be written makes nothing.  *)
(* The nexus records each pool group in the directory before starting the *)
(* pool, so once it is gone every group of the run is recorded. Each       *)
(* group's members may each make one object; a member killed between its   *)
(* marker and its object still makes the object, as a system call under   *)
(* way completes. When the nexus is gone, each group's teardown kills the  *)
(* group, and a sweeper outside the group waits until no member runs and,  *)
(* when no recorded group has a running member, sweeps the directory:      *)
(* every object a marker names, every marker, then the directory itself.   *)
(* A later nexus's startup sweep may also find the run and sweeps under    *)
(* the same condition. Each Reaper below is one group's teardown and its   *)
(* sweeper.                                                                *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the protocol above.
\* "no_wait": the sweeper does not wait for its own group to end.
\* "sweep_each": each sweeper sweeps once its own group ends, whether or
\*     not other groups run.
\* "sweeper_in_group": the sweeper is in the group the teardown kills.
\* "startup_ignores_groups": the startup sweep removes a dead run whatever
\*     its groups are doing.

Groups == {"g1", "g2"}
Seg == {"n", "g1", "g2"}

(* --algorithm RunSweep
variables
    nexus = "alive",
    member = [g \in Groups |-> "alive"],
    dir = TRUE,
    marker = [s \in Seg |-> FALSE],
    object = [s \in Seg |-> FALSE];

define
    Quiet == \A h \in Groups: member[h] = "gone"
    AllDone == \A p \in {"nexus", "r1", "r2", "startup"} \cup Groups: pc[p] = "Done"
    NothingLeft == AllDone => \A s \in Seg: ~object[s]
end define;

macro sweep() begin
  object := [s \in Seg |-> IF marker[s] THEN FALSE ELSE object[s]];
  marker := [s \in Seg |-> FALSE];
  dir := FALSE;
end macro;

fair process Nexus = "nexus"
begin
  NMark:
    either
      if dir then marker["n"] := TRUE; else goto NDie; end if;
    or
      goto NDie;
    end either;
  NMake:
    object["n"] := TRUE;
  NDie:
    nexus := "dead";
end process;

fair process Member \in Groups
begin
  MMark:
    either
      await member[self] = "alive";
      if dir then marker[self] := TRUE; else goto MDie; end if;
    or
      goto MDie;
    end either;
  MMake:
    object[self] := TRUE;
  MDie:
    await member[self] # "alive";
    member[self] := "gone";
end process;

fair process Reaper \in {"r1", "r2"}
variables grp = IF self = "r1" THEN "g1" ELSE "g2";
begin
  Await:
    await nexus = "dead";
  Kill:
    if member[grp] = "alive" then member[grp] := "killed"; end if;
    if Variant = "sweeper_in_group" then goto Finish; end if;
  WaitGone:
    if Variant # "no_wait" then await member[grp] = "gone"; end if;
  Check:
    if Variant \in {"sweep_each", "no_wait"} \/ Quiet then
      sweep();
    end if;
  Finish:
    skip;
end process;

fair process Startup = "startup"
begin
  Look:
    either
      await nexus = "dead";
      if Variant = "startup_ignores_groups" \/ Quiet then
        sweep();
      end if;
    or
      skip;
    end either;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "e7dfb116" /\ chksum(tla) = "a27ab001")
VARIABLES nexus, member, dir, marker, object, pc

(* define statement *)
Quiet == \A h \in Groups: member[h] = "gone"
AllDone == \A p \in {"nexus", "r1", "r2", "startup"} \cup Groups: pc[p] = "Done"
NothingLeft == AllDone => \A s \in Seg: ~object[s]

VARIABLE grp

vars == << nexus, member, dir, marker, object, pc, grp >>

ProcSet == {"nexus"} \cup (Groups) \cup ({"r1", "r2"}) \cup {"startup"}

Init == (* Global variables *)
        /\ nexus = "alive"
        /\ member = [g \in Groups |-> "alive"]
        /\ dir = TRUE
        /\ marker = [s \in Seg |-> FALSE]
        /\ object = [s \in Seg |-> FALSE]
        (* Process Reaper *)
        /\ grp = [self \in {"r1", "r2"} |-> IF self = "r1" THEN "g1" ELSE "g2"]
        /\ pc = [self \in ProcSet |-> CASE self = "nexus" -> "NMark"
                                        [] self \in Groups -> "MMark"
                                        [] self \in {"r1", "r2"} -> "Await"
                                        [] self = "startup" -> "Look"]

NMark == /\ pc["nexus"] = "NMark"
         /\ \/ /\ IF dir
                     THEN /\ marker' = [marker EXCEPT !["n"] = TRUE]
                          /\ pc' = [pc EXCEPT !["nexus"] = "NMake"]
                     ELSE /\ pc' = [pc EXCEPT !["nexus"] = "NDie"]
                          /\ UNCHANGED marker
            \/ /\ pc' = [pc EXCEPT !["nexus"] = "NDie"]
               /\ UNCHANGED marker
         /\ UNCHANGED << nexus, member, dir, object, grp >>

NMake == /\ pc["nexus"] = "NMake"
         /\ object' = [object EXCEPT !["n"] = TRUE]
         /\ pc' = [pc EXCEPT !["nexus"] = "NDie"]
         /\ UNCHANGED << nexus, member, dir, marker, grp >>

NDie == /\ pc["nexus"] = "NDie"
        /\ nexus' = "dead"
        /\ pc' = [pc EXCEPT !["nexus"] = "Done"]
        /\ UNCHANGED << member, dir, marker, object, grp >>

Nexus == NMark \/ NMake \/ NDie

MMark(self) == /\ pc[self] = "MMark"
               /\ \/ /\ member[self] = "alive"
                     /\ IF dir
                           THEN /\ marker' = [marker EXCEPT ![self] = TRUE]
                                /\ pc' = [pc EXCEPT ![self] = "MMake"]
                           ELSE /\ pc' = [pc EXCEPT ![self] = "MDie"]
                                /\ UNCHANGED marker
                  \/ /\ pc' = [pc EXCEPT ![self] = "MDie"]
                     /\ UNCHANGED marker
               /\ UNCHANGED << nexus, member, dir, object, grp >>

MMake(self) == /\ pc[self] = "MMake"
               /\ object' = [object EXCEPT ![self] = TRUE]
               /\ pc' = [pc EXCEPT ![self] = "MDie"]
               /\ UNCHANGED << nexus, member, dir, marker, grp >>

MDie(self) == /\ pc[self] = "MDie"
              /\ member[self] # "alive"
              /\ member' = [member EXCEPT ![self] = "gone"]
              /\ pc' = [pc EXCEPT ![self] = "Done"]
              /\ UNCHANGED << nexus, dir, marker, object, grp >>

Member(self) == MMark(self) \/ MMake(self) \/ MDie(self)

Await(self) == /\ pc[self] = "Await"
               /\ nexus = "dead"
               /\ pc' = [pc EXCEPT ![self] = "Kill"]
               /\ UNCHANGED << nexus, member, dir, marker, object, grp >>

Kill(self) == /\ pc[self] = "Kill"
              /\ IF member[grp[self]] = "alive"
                    THEN /\ member' = [member EXCEPT ![grp[self]] = "killed"]
                    ELSE /\ TRUE
                         /\ UNCHANGED member
              /\ IF Variant = "sweeper_in_group"
                    THEN /\ pc' = [pc EXCEPT ![self] = "Finish"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "WaitGone"]
              /\ UNCHANGED << nexus, dir, marker, object, grp >>

WaitGone(self) == /\ pc[self] = "WaitGone"
                  /\ IF Variant # "no_wait"
                        THEN /\ member[grp[self]] = "gone"
                        ELSE /\ TRUE
                  /\ pc' = [pc EXCEPT ![self] = "Check"]
                  /\ UNCHANGED << nexus, member, dir, marker, object, grp >>

Check(self) == /\ pc[self] = "Check"
               /\ IF Variant \in {"sweep_each", "no_wait"} \/ Quiet
                     THEN /\ object' = [s \in Seg |-> IF marker[s] THEN FALSE ELSE object[s]]
                          /\ marker' = [s \in Seg |-> FALSE]
                          /\ dir' = FALSE
                     ELSE /\ TRUE
                          /\ UNCHANGED << dir, marker, object >>
               /\ pc' = [pc EXCEPT ![self] = "Finish"]
               /\ UNCHANGED << nexus, member, grp >>

Finish(self) == /\ pc[self] = "Finish"
                /\ TRUE
                /\ pc' = [pc EXCEPT ![self] = "Done"]
                /\ UNCHANGED << nexus, member, dir, marker, object, grp >>

Reaper(self) == Await(self) \/ Kill(self) \/ WaitGone(self) \/ Check(self)
                   \/ Finish(self)

Look == /\ pc["startup"] = "Look"
        /\ \/ /\ nexus = "dead"
              /\ IF Variant = "startup_ignores_groups" \/ Quiet
                    THEN /\ object' = [s \in Seg |-> IF marker[s] THEN FALSE ELSE object[s]]
                         /\ marker' = [s \in Seg |-> FALSE]
                         /\ dir' = FALSE
                    ELSE /\ TRUE
                         /\ UNCHANGED << dir, marker, object >>
           \/ /\ TRUE
              /\ UNCHANGED <<dir, marker, object>>
        /\ pc' = [pc EXCEPT !["startup"] = "Done"]
        /\ UNCHANGED << nexus, member, grp >>

Startup == Look

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Nexus \/ Startup
           \/ (\E self \in Groups: Member(self))
           \/ (\E self \in {"r1", "r2"}: Reaper(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Nexus)
        /\ \A self \in Groups : WF_vars(Member(self))
        /\ \A self \in {"r1", "r2"} : WF_vars(Reaper(self))
        /\ WF_vars(Startup)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
