---------------------------- MODULE ShmHandoff ----------------------------
(* A shared-memory block crossing from a producer pool to a consumer       *)
(* (model/runtime/shm.md SHM-3, SHM-4; model/runtime/fork.md FORK-1). Either side may die *)
(* at any step, and the consumer may fork a child that exits.              *)
EXTENDS Naturals

CONSTANTS DonateBeforeSend, \* the sender takes the recipient's reference
          ForkGuard         \* a forked child forgets what it inherited

(* --algorithm ShmHandoff
variables
    count = 0,          \* the block's reference count
    freed = FALSE,      \* scrubbed and back in the allocator
    donated = FALSE,    \* a reference was taken on the recipient's behalf
    sent = FALSE,       \* the packet naming the block is on the wire
    forked = FALSE,
    childCopy = FALSE,  \* the child inherited the consumer's holder entry
    dead = {},
    badRead = FALSE,
    underflow = FALSE;

define
    NoReadOfFreedBlock == ~badRead
    NoReleaseBelowZero == ~underflow
    NoLeakUnlessAHolderDied ==
        (\A p \in {"producer", "consumer", "child"} : pc[p] = "Done")
            /\ dead = {}
        => freed
end define;

macro Release() begin
    if count = 0 then
        underflow := TRUE;
    else
        if count = 1 then freed := TRUE; end if;
        count := count - 1;
    end if;
end macro;

process Producer = "producer"
begin
  Alloc:
    count := 1;
  Donate:
    either
      if DonateBeforeSend then
        count := count + 1;
        donated := TRUE;
      end if;
    or
      dead := dead \union {"producer"}; goto Done;
    end either;
  ReleaseOwn:
    either
      Release();
    or
      dead := dead \union {"producer"}; goto Done;
    end either;
  Send:
    sent := TRUE;
end process;

process Consumer = "consumer"
begin
  Receive:
    either
      await sent;
    or
      await "producer" \in dead /\ ~sent; goto Done;
    end either;
  Adopt:
    if ~donated then
      if freed then
        badRead := TRUE; goto Done;
      else
        count := count + 1;
      end if;
    end if;
  MaybeFork:
    either
      forked := TRUE;
      childCopy := TRUE;
    or
      skip;
    or
      dead := dead \union {"consumer"}; goto Done;
    end either;
  Read:
    either
      if freed then badRead := TRUE; end if;
    or
      dead := dead \union {"consumer"}; goto Done;
    end either;
  ReleaseRead:
    Release();
end process;

process Child = "child"
begin
  Born:
    either
      await forked;
    or
      await pc["consumer"] = "Done" /\ ~forked; goto Done;
    end either;
  Exit:
    if childCopy /\ ~ForkGuard then
      Release();
    end if;
end process;

end algorithm; *)
\* BEGIN TRANSLATION
VARIABLES count, freed, donated, sent, forked, childCopy, dead, badRead, 
          underflow, pc

(* define statement *)
NoReadOfFreedBlock == ~badRead
NoReleaseBelowZero == ~underflow
NoLeakUnlessAHolderDied ==
    (\A p \in {"producer", "consumer", "child"} : pc[p] = "Done")
        /\ dead = {}
    => freed


vars == << count, freed, donated, sent, forked, childCopy, dead, badRead, 
           underflow, pc >>

ProcSet == {"producer"} \cup {"consumer"} \cup {"child"}

Init == (* Global variables *)
        /\ count = 0
        /\ freed = FALSE
        /\ donated = FALSE
        /\ sent = FALSE
        /\ forked = FALSE
        /\ childCopy = FALSE
        /\ dead = {}
        /\ badRead = FALSE
        /\ underflow = FALSE
        /\ pc = [self \in ProcSet |-> CASE self = "producer" -> "Alloc"
                                        [] self = "consumer" -> "Receive"
                                        [] self = "child" -> "Born"]

Alloc == /\ pc["producer"] = "Alloc"
         /\ count' = 1
         /\ pc' = [pc EXCEPT !["producer"] = "Donate"]
         /\ UNCHANGED << freed, donated, sent, forked, childCopy, dead, 
                         badRead, underflow >>

Donate == /\ pc["producer"] = "Donate"
          /\ \/ /\ IF DonateBeforeSend
                      THEN /\ count' = count + 1
                           /\ donated' = TRUE
                      ELSE /\ TRUE
                           /\ UNCHANGED << count, donated >>
                /\ pc' = [pc EXCEPT !["producer"] = "ReleaseOwn"]
                /\ dead' = dead
             \/ /\ dead' = (dead \union {"producer"})
                /\ pc' = [pc EXCEPT !["producer"] = "Done"]
                /\ UNCHANGED <<count, donated>>
          /\ UNCHANGED << freed, sent, forked, childCopy, badRead, underflow >>

ReleaseOwn == /\ pc["producer"] = "ReleaseOwn"
              /\ \/ /\ IF count = 0
                          THEN /\ underflow' = TRUE
                               /\ UNCHANGED << count, freed >>
                          ELSE /\ IF count = 1
                                     THEN /\ freed' = TRUE
                                     ELSE /\ TRUE
                                          /\ freed' = freed
                               /\ count' = count - 1
                               /\ UNCHANGED underflow
                    /\ pc' = [pc EXCEPT !["producer"] = "Send"]
                    /\ dead' = dead
                 \/ /\ dead' = (dead \union {"producer"})
                    /\ pc' = [pc EXCEPT !["producer"] = "Done"]
                    /\ UNCHANGED <<count, freed, underflow>>
              /\ UNCHANGED << donated, sent, forked, childCopy, badRead >>

Send == /\ pc["producer"] = "Send"
        /\ sent' = TRUE
        /\ pc' = [pc EXCEPT !["producer"] = "Done"]
        /\ UNCHANGED << count, freed, donated, forked, childCopy, dead, 
                        badRead, underflow >>

Producer == Alloc \/ Donate \/ ReleaseOwn \/ Send

Receive == /\ pc["consumer"] = "Receive"
           /\ \/ /\ sent
                 /\ pc' = [pc EXCEPT !["consumer"] = "Adopt"]
              \/ /\ "producer" \in dead /\ ~sent
                 /\ pc' = [pc EXCEPT !["consumer"] = "Done"]
           /\ UNCHANGED << count, freed, donated, sent, forked, childCopy, 
                           dead, badRead, underflow >>

Adopt == /\ pc["consumer"] = "Adopt"
         /\ IF ~donated
               THEN /\ IF freed
                          THEN /\ badRead' = TRUE
                               /\ pc' = [pc EXCEPT !["consumer"] = "Done"]
                               /\ count' = count
                          ELSE /\ count' = count + 1
                               /\ pc' = [pc EXCEPT !["consumer"] = "MaybeFork"]
                               /\ UNCHANGED badRead
               ELSE /\ pc' = [pc EXCEPT !["consumer"] = "MaybeFork"]
                    /\ UNCHANGED << count, badRead >>
         /\ UNCHANGED << freed, donated, sent, forked, childCopy, dead, 
                         underflow >>

MaybeFork == /\ pc["consumer"] = "MaybeFork"
             /\ \/ /\ forked' = TRUE
                   /\ childCopy' = TRUE
                   /\ pc' = [pc EXCEPT !["consumer"] = "Read"]
                   /\ dead' = dead
                \/ /\ TRUE
                   /\ pc' = [pc EXCEPT !["consumer"] = "Read"]
                   /\ UNCHANGED <<forked, childCopy, dead>>
                \/ /\ dead' = (dead \union {"consumer"})
                   /\ pc' = [pc EXCEPT !["consumer"] = "Done"]
                   /\ UNCHANGED <<forked, childCopy>>
             /\ UNCHANGED << count, freed, donated, sent, badRead, underflow >>

Read == /\ pc["consumer"] = "Read"
        /\ \/ /\ IF freed
                    THEN /\ badRead' = TRUE
                    ELSE /\ TRUE
                         /\ UNCHANGED badRead
              /\ pc' = [pc EXCEPT !["consumer"] = "ReleaseRead"]
              /\ dead' = dead
           \/ /\ dead' = (dead \union {"consumer"})
              /\ pc' = [pc EXCEPT !["consumer"] = "Done"]
              /\ UNCHANGED badRead
        /\ UNCHANGED << count, freed, donated, sent, forked, childCopy, 
                        underflow >>

ReleaseRead == /\ pc["consumer"] = "ReleaseRead"
               /\ IF count = 0
                     THEN /\ underflow' = TRUE
                          /\ UNCHANGED << count, freed >>
                     ELSE /\ IF count = 1
                                THEN /\ freed' = TRUE
                                ELSE /\ TRUE
                                     /\ freed' = freed
                          /\ count' = count - 1
                          /\ UNCHANGED underflow
               /\ pc' = [pc EXCEPT !["consumer"] = "Done"]
               /\ UNCHANGED << donated, sent, forked, childCopy, dead, badRead >>

Consumer == Receive \/ Adopt \/ MaybeFork \/ Read \/ ReleaseRead

Born == /\ pc["child"] = "Born"
        /\ \/ /\ forked
              /\ pc' = [pc EXCEPT !["child"] = "Exit"]
           \/ /\ pc["consumer"] = "Done" /\ ~forked
              /\ pc' = [pc EXCEPT !["child"] = "Done"]
        /\ UNCHANGED << count, freed, donated, sent, forked, childCopy, dead, 
                        badRead, underflow >>

Exit == /\ pc["child"] = "Exit"
        /\ IF childCopy /\ ~ForkGuard
              THEN /\ IF count = 0
                         THEN /\ underflow' = TRUE
                              /\ UNCHANGED << count, freed >>
                         ELSE /\ IF count = 1
                                    THEN /\ freed' = TRUE
                                    ELSE /\ TRUE
                                         /\ freed' = freed
                              /\ count' = count - 1
                              /\ UNCHANGED underflow
              ELSE /\ TRUE
                   /\ UNCHANGED << count, freed, underflow >>
        /\ pc' = [pc EXCEPT !["child"] = "Done"]
        /\ UNCHANGED << donated, sent, forked, childCopy, dead, badRead >>

Child == Born \/ Exit

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Producer \/ Consumer \/ Child
           \/ Terminating

Spec == Init /\ [][Next]_vars

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION
=============================================================================
