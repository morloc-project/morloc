---------------------------- MODULE LargeVolume ----------------------------
(* A one-block large volume (model/runtime/shm.md SHM-18, SHM-6, SHM-13).  *)
(* Page 0 holds the block's header and is always committed; pages 1..P    *)
(* hold its data and are committed (backed) or not. The file system holds  *)
(* at most Cap data pages, and an outside process can take one of them at  *)
(* any time. A store into, or a read of, an uncommitted page when the file *)
(* system is full is a fault (SIGBUS); a read of one otherwise backs it    *)
(* silently. Two allocators each reserve the block by compare-and-swap,    *)
(* commit the pages of their request one at a time, publish, use, and      *)
(* release it, keeping it committed (scrubbed) or removing its pages. A    *)
(* failed commit removes every data page and frees the block. Either can   *)
(* die at any step. A stale reader holding no reference checks the header  *)
(* and, in the bug variant, reads the block's data as a header too.        *)
EXTENDS Naturals, FiniteSets

CONSTANTS P, Cap0, Variant
\* "design": the protocol above.
\* "no_cas": reserve by a plain test and store.
\* "publish_before_commit": publish the block, then commit its pages.
\* "failed_commit_keeps_pages": a failed commit frees the block but leaves
\*   the pages it committed.
\* "punch_header_page": a release by removal also removes page 0.
\* "interior_header_read": the stale reader also reads a data page as a header.

Allocs == {"a1", "a2"}
Data == 1..P
TD == 99

(* --algorithm LargeVolume
variables
    committed = [p \in 0..P |-> p = 0],
    cap = Cap0,
    count = 0,
    owner = "none",
    size = 0,
    kept = FALSE,
    fault = FALSE,
    dead = {};

define
    Used == Cardinality({p \in Data : committed[p]})
    Full == Used >= cap
    NoFault == ~fault
    HeaderCommitted == committed[0]
    HeldBytesCommitted == (count = 1) => \A p \in 1..size : committed[p]
    OneOwner == ~(owner = "both")
    WithinCapacity == Used <= Cap0
    FreeVolumeLeaksNothing ==
        (count = 0 /\ ~kept) => \A p \in Data : ~committed[p]
end define;

macro touch(p) begin
    if ~committed[p] then
        if Full then fault := TRUE; else committed[p] := TRUE; end if;
    end if;
end macro;

fair process A \in Allocs
variables need = 0, next = 1;
begin
  Start:
    either skip; or dead := dead \union {self}; goto Done; end either;
  Reserve:
    either
      if Variant = "no_cas" then
        await count = 0;
      else
        await count = 0;
        count := TD;
      end if;
    or
      goto Done;
    end either;
  Reserve2:
    if Variant = "no_cas" then
      count := TD;
    end if;
    owner := IF owner = "none" THEN self ELSE "both";
    with n \in Data do need := n; end with;
    next := 1;
  EarlyPublish:
    if Variant = "publish_before_commit" then
      size := need;
      count := 1;
    end if;
  Commit:
    while next <= need do
      either skip; or dead := dead \union {self}; goto Done; end either;
      CommitOne:
        if ~committed[next] then
          if Full then
            if Variant /= "failed_commit_keeps_pages" then
              committed := [p \in 0..P |-> IF p = 0 THEN TRUE ELSE FALSE];
            end if;
            goto Fail;
          else
            committed[next] := TRUE;
          end if;
        end if;
      Advance:
        next := next + 1;
    end while;
  Publish:
    kept := FALSE;
    if Variant /= "publish_before_commit" then
      size := need;
      count := 1;
    end if;
  Use:
    with p \in 1..size do touch(p); end with;
  Release:
    either skip; or dead := dead \union {self}; goto Done; end either;
  Mark:
    count := TD;
  Empty:
    either
      with p \in 1..size do touch(p); end with;
      kept := TRUE;
    or
      committed := [p \in 0..P |-> IF p = 0 THEN Variant /= "punch_header_page" ELSE FALSE];
      kept := FALSE;
    end either;
  Free:
    owner := IF owner = self THEN "none" ELSE owner;
    size := 0;
    count := 0;
    goto Done;
  Fail:
    owner := IF owner = self THEN "none" ELSE owner;
    kept := FALSE;
    count := 0;
end process;

fair process Stale = "stale"
variables reads = 0;
begin
  Read:
    while reads < 3 do
      ReadHeader:
        touch(0);
      ReadInterior:
        if Variant = "interior_header_read" then
          with p \in Data do touch(p); end with;
        end if;
      Count:
        reads := reads + 1;
    end while;
end process;

fair process Thief = "thief"
begin
  Take:
    either cap := cap - 1; or skip; end either;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "9ce6c8f7" /\ chksum(tla) = "b7ebc5b4")
VARIABLES committed, cap, count, owner, size, kept, fault, dead, pc

(* define statement *)
Used == Cardinality({p \in Data : committed[p]})
Full == Used >= cap
NoFault == ~fault
HeaderCommitted == committed[0]
HeldBytesCommitted == (count = 1) => \A p \in 1..size : committed[p]
OneOwner == ~(owner = "both")
WithinCapacity == Used <= Cap0
FreeVolumeLeaksNothing ==
    (count = 0 /\ ~kept) => \A p \in Data : ~committed[p]

VARIABLES need, next, reads

vars == << committed, cap, count, owner, size, kept, fault, dead, pc, need, 
           next, reads >>

ProcSet == (Allocs) \cup {"stale"} \cup {"thief"}

Init == (* Global variables *)
        /\ committed = [p \in 0..P |-> p = 0]
        /\ cap = Cap0
        /\ count = 0
        /\ owner = "none"
        /\ size = 0
        /\ kept = FALSE
        /\ fault = FALSE
        /\ dead = {}
        (* Process A *)
        /\ need = [self \in Allocs |-> 0]
        /\ next = [self \in Allocs |-> 1]
        (* Process Stale *)
        /\ reads = 0
        /\ pc = [self \in ProcSet |-> CASE self \in Allocs -> "Start"
                                        [] self = "stale" -> "Read"
                                        [] self = "thief" -> "Take"]

Start(self) == /\ pc[self] = "Start"
               /\ \/ /\ TRUE
                     /\ pc' = [pc EXCEPT ![self] = "Reserve"]
                     /\ dead' = dead
                  \/ /\ dead' = (dead \union {self})
                     /\ pc' = [pc EXCEPT ![self] = "Done"]
               /\ UNCHANGED << committed, cap, count, owner, size, kept, fault, 
                               need, next, reads >>

Reserve(self) == /\ pc[self] = "Reserve"
                 /\ \/ /\ IF Variant = "no_cas"
                             THEN /\ count = 0
                                  /\ count' = count
                             ELSE /\ count = 0
                                  /\ count' = TD
                       /\ pc' = [pc EXCEPT ![self] = "Reserve2"]
                    \/ /\ pc' = [pc EXCEPT ![self] = "Done"]
                       /\ count' = count
                 /\ UNCHANGED << committed, cap, owner, size, kept, fault, 
                                 dead, need, next, reads >>

Reserve2(self) == /\ pc[self] = "Reserve2"
                  /\ IF Variant = "no_cas"
                        THEN /\ count' = TD
                        ELSE /\ TRUE
                             /\ count' = count
                  /\ owner' = (IF owner = "none" THEN self ELSE "both")
                  /\ \E n \in Data:
                       need' = [need EXCEPT ![self] = n]
                  /\ next' = [next EXCEPT ![self] = 1]
                  /\ pc' = [pc EXCEPT ![self] = "EarlyPublish"]
                  /\ UNCHANGED << committed, cap, size, kept, fault, dead, 
                                  reads >>

EarlyPublish(self) == /\ pc[self] = "EarlyPublish"
                      /\ IF Variant = "publish_before_commit"
                            THEN /\ size' = need[self]
                                 /\ count' = 1
                            ELSE /\ TRUE
                                 /\ UNCHANGED << count, size >>
                      /\ pc' = [pc EXCEPT ![self] = "Commit"]
                      /\ UNCHANGED << committed, cap, owner, kept, fault, dead, 
                                      need, next, reads >>

Commit(self) == /\ pc[self] = "Commit"
                /\ IF next[self] <= need[self]
                      THEN /\ \/ /\ TRUE
                                 /\ pc' = [pc EXCEPT ![self] = "CommitOne"]
                                 /\ dead' = dead
                              \/ /\ dead' = (dead \union {self})
                                 /\ pc' = [pc EXCEPT ![self] = "Done"]
                      ELSE /\ pc' = [pc EXCEPT ![self] = "Publish"]
                           /\ dead' = dead
                /\ UNCHANGED << committed, cap, count, owner, size, kept, 
                                fault, need, next, reads >>

CommitOne(self) == /\ pc[self] = "CommitOne"
                   /\ IF ~committed[next[self]]
                         THEN /\ IF Full
                                    THEN /\ IF Variant /= "failed_commit_keeps_pages"
                                               THEN /\ committed' = [p \in 0..P |-> IF p = 0 THEN TRUE ELSE FALSE]
                                               ELSE /\ TRUE
                                                    /\ UNCHANGED committed
                                         /\ pc' = [pc EXCEPT ![self] = "Fail"]
                                    ELSE /\ committed' = [committed EXCEPT ![next[self]] = TRUE]
                                         /\ pc' = [pc EXCEPT ![self] = "Advance"]
                         ELSE /\ pc' = [pc EXCEPT ![self] = "Advance"]
                              /\ UNCHANGED committed
                   /\ UNCHANGED << cap, count, owner, size, kept, fault, dead, 
                                   need, next, reads >>

Advance(self) == /\ pc[self] = "Advance"
                 /\ next' = [next EXCEPT ![self] = next[self] + 1]
                 /\ pc' = [pc EXCEPT ![self] = "Commit"]
                 /\ UNCHANGED << committed, cap, count, owner, size, kept, 
                                 fault, dead, need, reads >>

Publish(self) == /\ pc[self] = "Publish"
                 /\ kept' = FALSE
                 /\ IF Variant /= "publish_before_commit"
                       THEN /\ size' = need[self]
                            /\ count' = 1
                       ELSE /\ TRUE
                            /\ UNCHANGED << count, size >>
                 /\ pc' = [pc EXCEPT ![self] = "Use"]
                 /\ UNCHANGED << committed, cap, owner, fault, dead, need, 
                                 next, reads >>

Use(self) == /\ pc[self] = "Use"
             /\ \E p \in 1..size:
                  IF ~committed[p]
                     THEN /\ IF Full
                                THEN /\ fault' = TRUE
                                     /\ UNCHANGED committed
                                ELSE /\ committed' = [committed EXCEPT ![p] = TRUE]
                                     /\ fault' = fault
                     ELSE /\ TRUE
                          /\ UNCHANGED << committed, fault >>
             /\ pc' = [pc EXCEPT ![self] = "Release"]
             /\ UNCHANGED << cap, count, owner, size, kept, dead, need, next, 
                             reads >>

Release(self) == /\ pc[self] = "Release"
                 /\ \/ /\ TRUE
                       /\ pc' = [pc EXCEPT ![self] = "Mark"]
                       /\ dead' = dead
                    \/ /\ dead' = (dead \union {self})
                       /\ pc' = [pc EXCEPT ![self] = "Done"]
                 /\ UNCHANGED << committed, cap, count, owner, size, kept, 
                                 fault, need, next, reads >>

Mark(self) == /\ pc[self] = "Mark"
              /\ count' = TD
              /\ pc' = [pc EXCEPT ![self] = "Empty"]
              /\ UNCHANGED << committed, cap, owner, size, kept, fault, dead, 
                              need, next, reads >>

Empty(self) == /\ pc[self] = "Empty"
               /\ \/ /\ \E p \in 1..size:
                          IF ~committed[p]
                             THEN /\ IF Full
                                        THEN /\ fault' = TRUE
                                             /\ UNCHANGED committed
                                        ELSE /\ committed' = [committed EXCEPT ![p] = TRUE]
                                             /\ fault' = fault
                             ELSE /\ TRUE
                                  /\ UNCHANGED << committed, fault >>
                     /\ kept' = TRUE
                  \/ /\ committed' = [p \in 0..P |-> IF p = 0 THEN Variant /= "punch_header_page" ELSE FALSE]
                     /\ kept' = FALSE
                     /\ fault' = fault
               /\ pc' = [pc EXCEPT ![self] = "Free"]
               /\ UNCHANGED << cap, count, owner, size, dead, need, next, 
                               reads >>

Free(self) == /\ pc[self] = "Free"
              /\ owner' = (IF owner = self THEN "none" ELSE owner)
              /\ size' = 0
              /\ count' = 0
              /\ pc' = [pc EXCEPT ![self] = "Done"]
              /\ UNCHANGED << committed, cap, kept, fault, dead, need, next, 
                              reads >>

Fail(self) == /\ pc[self] = "Fail"
              /\ owner' = (IF owner = self THEN "none" ELSE owner)
              /\ kept' = FALSE
              /\ count' = 0
              /\ pc' = [pc EXCEPT ![self] = "Done"]
              /\ UNCHANGED << committed, cap, size, fault, dead, need, next, 
                              reads >>

A(self) == Start(self) \/ Reserve(self) \/ Reserve2(self)
              \/ EarlyPublish(self) \/ Commit(self) \/ CommitOne(self)
              \/ Advance(self) \/ Publish(self) \/ Use(self)
              \/ Release(self) \/ Mark(self) \/ Empty(self) \/ Free(self)
              \/ Fail(self)

Read == /\ pc["stale"] = "Read"
        /\ IF reads < 3
              THEN /\ pc' = [pc EXCEPT !["stale"] = "ReadHeader"]
              ELSE /\ pc' = [pc EXCEPT !["stale"] = "Done"]
        /\ UNCHANGED << committed, cap, count, owner, size, kept, fault, dead, 
                        need, next, reads >>

ReadHeader == /\ pc["stale"] = "ReadHeader"
              /\ IF ~committed[0]
                    THEN /\ IF Full
                               THEN /\ fault' = TRUE
                                    /\ UNCHANGED committed
                               ELSE /\ committed' = [committed EXCEPT ![0] = TRUE]
                                    /\ fault' = fault
                    ELSE /\ TRUE
                         /\ UNCHANGED << committed, fault >>
              /\ pc' = [pc EXCEPT !["stale"] = "ReadInterior"]
              /\ UNCHANGED << cap, count, owner, size, kept, dead, need, next, 
                              reads >>

ReadInterior == /\ pc["stale"] = "ReadInterior"
                /\ IF Variant = "interior_header_read"
                      THEN /\ \E p \in Data:
                                IF ~committed[p]
                                   THEN /\ IF Full
                                              THEN /\ fault' = TRUE
                                                   /\ UNCHANGED committed
                                              ELSE /\ committed' = [committed EXCEPT ![p] = TRUE]
                                                   /\ fault' = fault
                                   ELSE /\ TRUE
                                        /\ UNCHANGED << committed, fault >>
                      ELSE /\ TRUE
                           /\ UNCHANGED << committed, fault >>
                /\ pc' = [pc EXCEPT !["stale"] = "Count"]
                /\ UNCHANGED << cap, count, owner, size, kept, dead, need, 
                                next, reads >>

Count == /\ pc["stale"] = "Count"
         /\ reads' = reads + 1
         /\ pc' = [pc EXCEPT !["stale"] = "Read"]
         /\ UNCHANGED << committed, cap, count, owner, size, kept, fault, dead, 
                         need, next >>

Stale == Read \/ ReadHeader \/ ReadInterior \/ Count

Take == /\ pc["thief"] = "Take"
        /\ \/ /\ cap' = cap - 1
           \/ /\ TRUE
              /\ cap' = cap
        /\ pc' = [pc EXCEPT !["thief"] = "Done"]
        /\ UNCHANGED << committed, count, owner, size, kept, fault, dead, need, 
                        next, reads >>

Thief == Take

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Stale \/ Thief
           \/ (\E self \in Allocs: A(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Allocs : WF_vars(A(self))
        /\ WF_vars(Stale)
        /\ WF_vars(Thief)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
