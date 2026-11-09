------------------------------ MODULE ShmClaim ------------------------------
(* Claiming a free block and checking a reference packet against it        *)
(* (model/runtime/shm.md SHM-2, SHM-4, SHM-13). A block has a reference     *)
(* count (0 free, TD being released or claimed, n held) and a generation.   *)
(* An allocator claims the block under its volume's lock, makes one packet  *)
(* stamped with the new generation, and later releases the block. A stray   *)
(* claim, the allocator choosing a block that is in use, must touch         *)
(* nothing. A checker reads the count, then the generation, and accepts the *)
(* packet only if the block is held at the packet's generation. The model   *)
(* is sequentially consistent: it orders the steps, not the memory model.   *)
EXTENDS Naturals

CONSTANTS Variant, MaxClaims
\* "design": claim moves the count 0 -> TD, advances the generation, then
\*   stores 1; a stray claim's exchange fails and touches nothing.
\* "bump_then_cas": advance the generation, then exchange 0 -> 1.
\* "cas_then_bump": exchange 0 -> 1, then advance the generation.

TD == 99

(* --algorithm ShmClaim
variables
    count = 0,
    gen = 0,
    stamp = 0,
    alive = FALSE,
    falseAccept = FALSE,
    liveRefused = FALSE;

define
    Held(c) == c /= 0 /\ c /= TD
    NoFalseAccept == ~falseAccept
    NoLiveRefusal == ~liveRefused
end define;

fair process Alloc = "alloc"
variables claims = 0;
begin
  Loop:
    while claims < MaxClaims do
      Claim1:
        if Variant = "bump_then_cas" then
          gen := gen + 1;
        elsif count = 0 then
          count := IF Variant = "design" THEN TD ELSE 1;
        else
          goto Loop;
        end if;
      Claim2:
        if Variant = "bump_then_cas" then
          if count = 0 then count := 1; else goto Loop; end if;
        else
          gen := gen + 1;
        end if;
      Claim3:
        if Variant = "design" then count := 1; end if;
        claims := claims + 1;
      Make:
        if stamp = 0 then
          stamp := gen;
          alive := TRUE;
        end if;
      Release1:
        alive := FALSE;
        count := TD;
      Release2:
        count := 0;
    end while;
end process;

fair process Stray = "stray"
begin
  Try1:
    await alive \/ pc["alloc"] = "Done";
    if alive /\ Variant = "bump_then_cas" then
      gen := gen + 1;
    end if;
  Try2:
    skip;
end process;

fair process Check = "check"
variables c = 0, g = 0, before = FALSE, n = 0;
begin
  Wait:
    await stamp /= 0;
  Reads:
    while n < 3 do
      ReadCount:
        before := alive;
        c := count;
      ReadGen:
        g := gen;
      Decide:
        if Held(c) /\ g = stamp then
          if ~alive /\ ~before then falseAccept := TRUE; end if;
        elsif before /\ alive then
          liveRefused := TRUE;
        end if;
        n := n + 1;
    end while;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "6ceaf4ac" /\ chksum(tla) = "cf39d872")
VARIABLES count, gen, stamp, alive, falseAccept, liveRefused, pc

(* define statement *)
Held(c) == c /= 0 /\ c /= TD
NoFalseAccept == ~falseAccept
NoLiveRefusal == ~liveRefused

VARIABLES claims, c, g, before, n

vars == << count, gen, stamp, alive, falseAccept, liveRefused, pc, claims, c, 
           g, before, n >>

ProcSet == {"alloc"} \cup {"stray"} \cup {"check"}

Init == (* Global variables *)
        /\ count = 0
        /\ gen = 0
        /\ stamp = 0
        /\ alive = FALSE
        /\ falseAccept = FALSE
        /\ liveRefused = FALSE
        (* Process Alloc *)
        /\ claims = 0
        (* Process Check *)
        /\ c = 0
        /\ g = 0
        /\ before = FALSE
        /\ n = 0
        /\ pc = [self \in ProcSet |-> CASE self = "alloc" -> "Loop"
                                        [] self = "stray" -> "Try1"
                                        [] self = "check" -> "Wait"]

Loop == /\ pc["alloc"] = "Loop"
        /\ IF claims < MaxClaims
              THEN /\ pc' = [pc EXCEPT !["alloc"] = "Claim1"]
              ELSE /\ pc' = [pc EXCEPT !["alloc"] = "Done"]
        /\ UNCHANGED << count, gen, stamp, alive, falseAccept, liveRefused, 
                        claims, c, g, before, n >>

Claim1 == /\ pc["alloc"] = "Claim1"
          /\ IF Variant = "bump_then_cas"
                THEN /\ gen' = gen + 1
                     /\ pc' = [pc EXCEPT !["alloc"] = "Claim2"]
                     /\ count' = count
                ELSE /\ IF count = 0
                           THEN /\ count' = (IF Variant = "design" THEN TD ELSE 1)
                                /\ pc' = [pc EXCEPT !["alloc"] = "Claim2"]
                           ELSE /\ pc' = [pc EXCEPT !["alloc"] = "Loop"]
                                /\ count' = count
                     /\ gen' = gen
          /\ UNCHANGED << stamp, alive, falseAccept, liveRefused, claims, c, g, 
                          before, n >>

Claim2 == /\ pc["alloc"] = "Claim2"
          /\ IF Variant = "bump_then_cas"
                THEN /\ IF count = 0
                           THEN /\ count' = 1
                                /\ pc' = [pc EXCEPT !["alloc"] = "Claim3"]
                           ELSE /\ pc' = [pc EXCEPT !["alloc"] = "Loop"]
                                /\ count' = count
                     /\ gen' = gen
                ELSE /\ gen' = gen + 1
                     /\ pc' = [pc EXCEPT !["alloc"] = "Claim3"]
                     /\ count' = count
          /\ UNCHANGED << stamp, alive, falseAccept, liveRefused, claims, c, g, 
                          before, n >>

Claim3 == /\ pc["alloc"] = "Claim3"
          /\ IF Variant = "design"
                THEN /\ count' = 1
                ELSE /\ TRUE
                     /\ count' = count
          /\ claims' = claims + 1
          /\ pc' = [pc EXCEPT !["alloc"] = "Make"]
          /\ UNCHANGED << gen, stamp, alive, falseAccept, liveRefused, c, g, 
                          before, n >>

Make == /\ pc["alloc"] = "Make"
        /\ IF stamp = 0
              THEN /\ stamp' = gen
                   /\ alive' = TRUE
              ELSE /\ TRUE
                   /\ UNCHANGED << stamp, alive >>
        /\ pc' = [pc EXCEPT !["alloc"] = "Release1"]
        /\ UNCHANGED << count, gen, falseAccept, liveRefused, claims, c, g, 
                        before, n >>

Release1 == /\ pc["alloc"] = "Release1"
            /\ alive' = FALSE
            /\ count' = TD
            /\ pc' = [pc EXCEPT !["alloc"] = "Release2"]
            /\ UNCHANGED << gen, stamp, falseAccept, liveRefused, claims, c, g, 
                            before, n >>

Release2 == /\ pc["alloc"] = "Release2"
            /\ count' = 0
            /\ pc' = [pc EXCEPT !["alloc"] = "Loop"]
            /\ UNCHANGED << gen, stamp, alive, falseAccept, liveRefused, 
                            claims, c, g, before, n >>

Alloc == Loop \/ Claim1 \/ Claim2 \/ Claim3 \/ Make \/ Release1 \/ Release2

Try1 == /\ pc["stray"] = "Try1"
        /\ alive \/ pc["alloc"] = "Done"
        /\ IF alive /\ Variant = "bump_then_cas"
              THEN /\ gen' = gen + 1
              ELSE /\ TRUE
                   /\ gen' = gen
        /\ pc' = [pc EXCEPT !["stray"] = "Try2"]
        /\ UNCHANGED << count, stamp, alive, falseAccept, liveRefused, claims, 
                        c, g, before, n >>

Try2 == /\ pc["stray"] = "Try2"
        /\ TRUE
        /\ pc' = [pc EXCEPT !["stray"] = "Done"]
        /\ UNCHANGED << count, gen, stamp, alive, falseAccept, liveRefused, 
                        claims, c, g, before, n >>

Stray == Try1 \/ Try2

Wait == /\ pc["check"] = "Wait"
        /\ stamp /= 0
        /\ pc' = [pc EXCEPT !["check"] = "Reads"]
        /\ UNCHANGED << count, gen, stamp, alive, falseAccept, liveRefused, 
                        claims, c, g, before, n >>

Reads == /\ pc["check"] = "Reads"
         /\ IF n < 3
               THEN /\ pc' = [pc EXCEPT !["check"] = "ReadCount"]
               ELSE /\ pc' = [pc EXCEPT !["check"] = "Done"]
         /\ UNCHANGED << count, gen, stamp, alive, falseAccept, liveRefused, 
                         claims, c, g, before, n >>

ReadCount == /\ pc["check"] = "ReadCount"
             /\ before' = alive
             /\ c' = count
             /\ pc' = [pc EXCEPT !["check"] = "ReadGen"]
             /\ UNCHANGED << count, gen, stamp, alive, falseAccept, 
                             liveRefused, claims, g, n >>

ReadGen == /\ pc["check"] = "ReadGen"
           /\ g' = gen
           /\ pc' = [pc EXCEPT !["check"] = "Decide"]
           /\ UNCHANGED << count, gen, stamp, alive, falseAccept, liveRefused, 
                           claims, c, before, n >>

Decide == /\ pc["check"] = "Decide"
          /\ IF Held(c) /\ g = stamp
                THEN /\ IF ~alive /\ ~before
                           THEN /\ falseAccept' = TRUE
                           ELSE /\ TRUE
                                /\ UNCHANGED falseAccept
                     /\ UNCHANGED liveRefused
                ELSE /\ IF before /\ alive
                           THEN /\ liveRefused' = TRUE
                           ELSE /\ TRUE
                                /\ UNCHANGED liveRefused
                     /\ UNCHANGED falseAccept
          /\ n' = n + 1
          /\ pc' = [pc EXCEPT !["check"] = "Reads"]
          /\ UNCHANGED << count, gen, stamp, alive, claims, c, g, before >>

Check == Wait \/ Reads \/ ReadCount \/ ReadGen \/ Decide

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Alloc \/ Stray \/ Check
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Alloc)
        /\ WF_vars(Stray)
        /\ WF_vars(Check)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
