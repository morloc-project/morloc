-------------------------------- MODULE Lease --------------------------------
(* Leases (model/runtime/fork.md FORK-15). A process forking a child that inherits *)
(* views takes a reference per view and records it in a lease file the     *)
(* child holds locked. The lease is created in a staging directory no      *)
(* reclaimer reads, locked and written there, then renamed into place. A   *)
(* reclaimer that takes a lease's lock knows its holder is gone: it        *)
(* releases the recorded references and removes the file. The child reads *)
(* the block while it lives; the parent releases its own reference.        *)
EXTENDS Naturals

CONSTANT Variant
\* "design": the design.
\* "no_staging": the lease is created under its final name before it is
\* locked, so a reclaimer may take the lock first and drop the lease.
\* "no_lock_check": the reclaimer releases a lease without taking its lock.

(* --algorithm Lease
variables
    ref = 1,
    file = "none",
    locked = FALSE,
    recorded = FALSE,
    childStarted = FALSE,
    freedRead = FALSE,
    released = FALSE;

define
    NoReadOfAFreedBlock == ~freedRead
    TheReferenceIsReleased == <>[](ref = 0)
end define;

fair process Parent = "parent"
begin
  Create:
    ref := ref + 1;
    if Variant = "no_staging" then
      file := "final";
    else
      file := "staging";
    end if;
  Lock:
    locked := TRUE;
    recorded := TRUE;
  Rename:
    if Variant /= "no_staging" then
      file := "final";
    end if;
  Fork:
    childStarted := TRUE;
  ParentRelease:
    ref := ref - 1;
end process;

fair process Child = "child"
begin
  Wait:
    await childStarted;
  Read:
    if ref = 0 then
      freedRead := TRUE;
    end if;
  Die:
    locked := FALSE;
end process;

fair process Reclaimer = "reclaim"
begin
  Scan:
    while ~released do
      either
        await file = "final" /\ (~locked \/ Variant = "no_lock_check");
        if recorded then
          ref := ref - 1;
        end if;
        file := "none";
        released := TRUE;
      or
        await pc["child"] = "Done" /\ pc["parent"] = "Done" /\ file = "none";
        released := TRUE;
      end either;
    end while;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "3dfea8c7" /\ chksum(tla) = "80813368")
VARIABLES ref, file, locked, recorded, childStarted, freedRead, released, pc

(* define statement *)
NoReadOfAFreedBlock == ~freedRead
TheReferenceIsReleased == <>[](ref = 0)


vars == << ref, file, locked, recorded, childStarted, freedRead, released, pc
        >>

ProcSet == {"parent"} \cup {"child"} \cup {"reclaim"}

Init == (* Global variables *)
        /\ ref = 1
        /\ file = "none"
        /\ locked = FALSE
        /\ recorded = FALSE
        /\ childStarted = FALSE
        /\ freedRead = FALSE
        /\ released = FALSE
        /\ pc = [self \in ProcSet |-> CASE self = "parent" -> "Create"
                                        [] self = "child" -> "Wait"
                                        [] self = "reclaim" -> "Scan"]

Create == /\ pc["parent"] = "Create"
          /\ ref' = ref + 1
          /\ IF Variant = "no_staging"
                THEN /\ file' = "final"
                ELSE /\ file' = "staging"
          /\ pc' = [pc EXCEPT !["parent"] = "Lock"]
          /\ UNCHANGED << locked, recorded, childStarted, freedRead, released >>

Lock == /\ pc["parent"] = "Lock"
        /\ locked' = TRUE
        /\ recorded' = TRUE
        /\ pc' = [pc EXCEPT !["parent"] = "Rename"]
        /\ UNCHANGED << ref, file, childStarted, freedRead, released >>

Rename == /\ pc["parent"] = "Rename"
          /\ IF Variant /= "no_staging"
                THEN /\ file' = "final"
                ELSE /\ TRUE
                     /\ file' = file
          /\ pc' = [pc EXCEPT !["parent"] = "Fork"]
          /\ UNCHANGED << ref, locked, recorded, childStarted, freedRead, 
                          released >>

Fork == /\ pc["parent"] = "Fork"
        /\ childStarted' = TRUE
        /\ pc' = [pc EXCEPT !["parent"] = "ParentRelease"]
        /\ UNCHANGED << ref, file, locked, recorded, freedRead, released >>

ParentRelease == /\ pc["parent"] = "ParentRelease"
                 /\ ref' = ref - 1
                 /\ pc' = [pc EXCEPT !["parent"] = "Done"]
                 /\ UNCHANGED << file, locked, recorded, childStarted, 
                                 freedRead, released >>

Parent == Create \/ Lock \/ Rename \/ Fork \/ ParentRelease

Wait == /\ pc["child"] = "Wait"
        /\ childStarted
        /\ pc' = [pc EXCEPT !["child"] = "Read"]
        /\ UNCHANGED << ref, file, locked, recorded, childStarted, freedRead, 
                        released >>

Read == /\ pc["child"] = "Read"
        /\ IF ref = 0
              THEN /\ freedRead' = TRUE
              ELSE /\ TRUE
                   /\ UNCHANGED freedRead
        /\ pc' = [pc EXCEPT !["child"] = "Die"]
        /\ UNCHANGED << ref, file, locked, recorded, childStarted, released >>

Die == /\ pc["child"] = "Die"
       /\ locked' = FALSE
       /\ pc' = [pc EXCEPT !["child"] = "Done"]
       /\ UNCHANGED << ref, file, recorded, childStarted, freedRead, released >>

Child == Wait \/ Read \/ Die

Scan == /\ pc["reclaim"] = "Scan"
        /\ IF ~released
              THEN /\ \/ /\ file = "final" /\ (~locked \/ Variant = "no_lock_check")
                         /\ IF recorded
                               THEN /\ ref' = ref - 1
                               ELSE /\ TRUE
                                    /\ ref' = ref
                         /\ file' = "none"
                         /\ released' = TRUE
                      \/ /\ pc["child"] = "Done" /\ pc["parent"] = "Done" /\ file = "none"
                         /\ released' = TRUE
                         /\ UNCHANGED <<ref, file>>
                   /\ pc' = [pc EXCEPT !["reclaim"] = "Scan"]
              ELSE /\ pc' = [pc EXCEPT !["reclaim"] = "Done"]
                   /\ UNCHANGED << ref, file, released >>
        /\ UNCHANGED << locked, recorded, childStarted, freedRead >>

Reclaimer == Scan

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Parent \/ Child \/ Reclaimer
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Parent)
        /\ WF_vars(Child)
        /\ WF_vars(Reclaimer)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
