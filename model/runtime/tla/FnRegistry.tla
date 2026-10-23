----------------------------- MODULE FnRegistry -----------------------------
(* The Rust function-value registry (model/runtime/state.md STATE-4). A       *)
(* function value that can cross pools is an Arc whose data address keys an  *)
(* entry naming its environment. Threads build values, hand clones to each   *)
(* other, drop them and look them up. One address is modelled, so every     *)
(* value after the first is allocated where a freed one was.                *)
EXTENDS Naturals

CONSTANT Variant
\* "design": register before the value escapes; the last owner removes the
\*           entry before the allocation is freed.
\* "remove_after_free": the allocation is freed before its entry is removed.
\* "register_after_escape": a value is handed out before it is registered.

Threads == {"t1", "t2"}
Other(t) == IF t = "t1" THEN "t2" ELSE "t1"
None == 0
MaxBuilt == 2

(* --algorithm FnRegistry
variables
    rc = 0,                         \* strong count of the allocation; 0 = free
    owner = None,                   \* environment of the live allocation
    reg = None,                     \* the registry entry for the address
    next = 1,                       \* next environment id
    held = [t \in Threads |-> None];  \* the environment a thread's handle names

define
    \* Every handle a thread holds finds its own environment in the registry.
    EntriesMatch == \A t \in Threads : held[t] /= None => reg = held[t]
    \* A handle never outlives its allocation.
    HandlesAreLive == \A t \in Threads : held[t] /= None => owner = held[t] /\ rc > 0
end define;

fair process Worker \in Threads
variables built = 0, mine = None;
begin
  Loop:
    while built < MaxBuilt \/ held[self] /= None do
      either
        \* build: allocate where a freed value was, register, hand out
        await held[self] = None /\ built < MaxBuilt /\ rc = 0 /\ owner = None;
        mine := next;
        next := next + 1;
        owner := mine;
        rc := 1;
        built := built + 1;
        if Variant = "register_after_escape" then
          Escape1:
            held[self] := mine;
          Register1:
            reg := mine;
        else
          Register2:
            reg := mine;
          Escape2:
            held[self] := mine;
        end if;
      or
        \* share: a clone of my handle for the other thread
        await held[self] /= None /\ held[Other(self)] = None /\ pc[Other(self)] = "Loop";
        rc := rc + 1;
        held[Other(self)] := held[self];
      or
        \* drop my handle; the last owner removes the entry and frees
        await held[self] /= None;
        mine := held[self];
        held[self] := None;
        rc := rc - 1;
        if rc = 0 then
          if Variant = "remove_after_free" then
            Free1:
              owner := None;
            Remove1:
              reg := None;
          else
            Remove2:
              reg := None;
            Free2:
              owner := None;
          end if;
        end if;
      end either;
    end while;
end process;
end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "9991f106" /\ chksum(tla) = "ce01de09")
VARIABLES rc, owner, reg, next, held, pc

(* define statement *)
EntriesMatch == \A t \in Threads : held[t] /= None => reg = held[t]

HandlesAreLive == \A t \in Threads : held[t] /= None => owner = held[t] /\ rc > 0

VARIABLES built, mine

vars == << rc, owner, reg, next, held, pc, built, mine >>

ProcSet == (Threads)

Init == (* Global variables *)
        /\ rc = 0
        /\ owner = None
        /\ reg = None
        /\ next = 1
        /\ held = [t \in Threads |-> None]
        (* Process Worker *)
        /\ built = [self \in Threads |-> 0]
        /\ mine = [self \in Threads |-> None]
        /\ pc = [self \in ProcSet |-> "Loop"]

Loop(self) == /\ pc[self] = "Loop"
              /\ IF built[self] < MaxBuilt \/ held[self] /= None
                    THEN /\ \/ /\ held[self] = None /\ built[self] < MaxBuilt /\ rc = 0 /\ owner = None
                               /\ mine' = [mine EXCEPT ![self] = next]
                               /\ next' = next + 1
                               /\ owner' = mine'[self]
                               /\ rc' = 1
                               /\ built' = [built EXCEPT ![self] = built[self] + 1]
                               /\ IF Variant = "register_after_escape"
                                     THEN /\ pc' = [pc EXCEPT ![self] = "Escape1"]
                                     ELSE /\ pc' = [pc EXCEPT ![self] = "Register2"]
                               /\ held' = held
                            \/ /\ held[self] /= None /\ held[Other(self)] = None /\ pc[Other(self)] = "Loop"
                               /\ rc' = rc + 1
                               /\ held' = [held EXCEPT ![Other(self)] = held[self]]
                               /\ pc' = [pc EXCEPT ![self] = "Loop"]
                               /\ UNCHANGED <<owner, next, built, mine>>
                            \/ /\ held[self] /= None
                               /\ mine' = [mine EXCEPT ![self] = held[self]]
                               /\ held' = [held EXCEPT ![self] = None]
                               /\ rc' = rc - 1
                               /\ IF rc' = 0
                                     THEN /\ IF Variant = "remove_after_free"
                                                THEN /\ pc' = [pc EXCEPT ![self] = "Free1"]
                                                ELSE /\ pc' = [pc EXCEPT ![self] = "Remove2"]
                                     ELSE /\ pc' = [pc EXCEPT ![self] = "Loop"]
                               /\ UNCHANGED <<owner, next, built>>
                    ELSE /\ pc' = [pc EXCEPT ![self] = "Done"]
                         /\ UNCHANGED << rc, owner, next, held, built, mine >>
              /\ reg' = reg

Escape1(self) == /\ pc[self] = "Escape1"
                 /\ held' = [held EXCEPT ![self] = mine[self]]
                 /\ pc' = [pc EXCEPT ![self] = "Register1"]
                 /\ UNCHANGED << rc, owner, reg, next, built, mine >>

Register1(self) == /\ pc[self] = "Register1"
                   /\ reg' = mine[self]
                   /\ pc' = [pc EXCEPT ![self] = "Loop"]
                   /\ UNCHANGED << rc, owner, next, held, built, mine >>

Register2(self) == /\ pc[self] = "Register2"
                   /\ reg' = mine[self]
                   /\ pc' = [pc EXCEPT ![self] = "Escape2"]
                   /\ UNCHANGED << rc, owner, next, held, built, mine >>

Escape2(self) == /\ pc[self] = "Escape2"
                 /\ held' = [held EXCEPT ![self] = mine[self]]
                 /\ pc' = [pc EXCEPT ![self] = "Loop"]
                 /\ UNCHANGED << rc, owner, reg, next, built, mine >>

Free1(self) == /\ pc[self] = "Free1"
               /\ owner' = None
               /\ pc' = [pc EXCEPT ![self] = "Remove1"]
               /\ UNCHANGED << rc, reg, next, held, built, mine >>

Remove1(self) == /\ pc[self] = "Remove1"
                 /\ reg' = None
                 /\ pc' = [pc EXCEPT ![self] = "Loop"]
                 /\ UNCHANGED << rc, owner, next, held, built, mine >>

Remove2(self) == /\ pc[self] = "Remove2"
                 /\ reg' = None
                 /\ pc' = [pc EXCEPT ![self] = "Free2"]
                 /\ UNCHANGED << rc, owner, next, held, built, mine >>

Free2(self) == /\ pc[self] = "Free2"
               /\ owner' = None
               /\ pc' = [pc EXCEPT ![self] = "Loop"]
               /\ UNCHANGED << rc, reg, next, held, built, mine >>

Worker(self) == Loop(self) \/ Escape1(self) \/ Register1(self)
                   \/ Register2(self) \/ Escape2(self) \/ Free1(self)
                   \/ Remove1(self) \/ Remove2(self) \/ Free2(self)

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == (\E self \in Threads: Worker(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Threads : WF_vars(Worker(self))

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
