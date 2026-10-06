------------------------------ MODULE PanicExit ------------------------------
(* What a panic does (model/panic.md PANIC-1..4). Process A's request      *)
(* thread takes the shared lock, tears the data it protects, and may panic *)
(* there, inside or outside a catch scope, with or without having begun    *)
(* its reply. A's other worker and a sibling process B keep taking the     *)
(* lock. A reader that finds the lock poisoned, or its holder dead, treats *)
(* the data as damaged; reading torn data as sound is the fault.           *)
EXTENDS Naturals

CONSTANTS Variant
\* "design": the design; a lock unwound through is poisoned and released.
\* "unlock_on_unwind": a guard dropped during a panic unlocks without poisoning.
\* "hook_exits_in_scope": the hook ends the process even inside a catch scope.
\* "continue_after_catch": the catch answers and A goes on serving.
\* "reply_twice": the catch answers even when a reply was begun.

(* --algorithm PanicExit
variables
    holder = "none",
    ownerDead = FALSE,
    poisoned = FALSE,
    data = "ok",
    tornRead = FALSE,
    aAlive = TRUE,
    status = 0,
    shutdown = FALSE,
    panicked = FALSE,
    inScope = FALSE,
    replyBegun = FALSE,
    replies = 0;

define
    Free == holder = "none" \/ ownerDead
    NoTornRead == ~tornRead
    AtMostOneReply == replies <= 1
    ExitIsInternalError == ~aAlive => status = 70
    CaughtRequestIsAnswered ==
        (panicked /\ inScope /\ ~replyBegun) ~> replies = 1
    PanicEndsTheProcess == panicked ~> ~aAlive
end define;

macro take(who) begin
  if ownerDead \/ poisoned then
    \* Damaged for good: the SHM namespace is discarded, never read.
    poisoned := TRUE;
  elsif data = "torn" then
    tornRead := TRUE;
  end if;
  holder := who;
  ownerDead := FALSE;
end macro;

macro die() begin
  \* A robust lock held by a dead process reports its owner dead.
  if holder \in {"req", "worker"} then
    ownerDead := TRUE;
  end if;
  aAlive := FALSE;
  status := 70;
end macro;

fair process Request = "req"
begin
  Take:
    await Free;
    take("req");
  Tear:
    data := "torn";
    either replyBegun := TRUE; replies := 1; or skip; end either;
    either inScope := TRUE; or skip; end either;
  Act:
    either
      data := "ok";
      holder := "none";
      goto Done;
    or
      panicked := TRUE;
    end either;
  Hook:
    if aAlive then
      if inScope /\ Variant /= "hook_exits_in_scope" then
        \* Unwind: the lock guard drops.
        if Variant /= "unlock_on_unwind" then
          poisoned := TRUE;
        end if;
        holder := "none";
      else
        die();
        goto Done;
      end if;
    else
      goto Done;
    end if;
  Catch:
    if aAlive /\ (~replyBegun \/ Variant = "reply_twice") then
      replies := replies + 1;
    end if;
    if Variant /= "continue_after_catch" then
      shutdown := TRUE;
    end if;
end process;

fair process Worker = "worker"
begin
  Loop:
    while aAlive do
      W_Take:
        await Free \/ ~aAlive;
        if aAlive then
          take("worker");
        else
          goto W_Done;
        end if;
      W_Give:
        if holder = "worker" /\ aAlive then
          holder := "none";
        end if;
    end while;
  W_Done:
    skip;
end process;

fair process Exit = "exit"
begin
  E_Wait:
    await shutdown \/ ~aAlive;
  E_Exit:
    if aAlive then
      die();
    end if;
end process;

process Sibling = "b"
begin
  B_Loop:
    while TRUE do
      B_Take:
        await Free;
        take("b");
      B_Give:
        holder := "none";
    end while;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "e29cf18d" /\ chksum(tla) = "8371522c")
VARIABLES holder, ownerDead, poisoned, data, tornRead, aAlive, status, 
          shutdown, panicked, inScope, replyBegun, replies, pc

(* define statement *)
Free == holder = "none" \/ ownerDead
NoTornRead == ~tornRead
AtMostOneReply == replies <= 1
ExitIsInternalError == ~aAlive => status = 70
CaughtRequestIsAnswered ==
    (panicked /\ inScope /\ ~replyBegun) ~> replies = 1
PanicEndsTheProcess == panicked ~> ~aAlive


vars == << holder, ownerDead, poisoned, data, tornRead, aAlive, status, 
           shutdown, panicked, inScope, replyBegun, replies, pc >>

ProcSet == {"req"} \cup {"worker"} \cup {"exit"} \cup {"b"}

Init == (* Global variables *)
        /\ holder = "none"
        /\ ownerDead = FALSE
        /\ poisoned = FALSE
        /\ data = "ok"
        /\ tornRead = FALSE
        /\ aAlive = TRUE
        /\ status = 0
        /\ shutdown = FALSE
        /\ panicked = FALSE
        /\ inScope = FALSE
        /\ replyBegun = FALSE
        /\ replies = 0
        /\ pc = [self \in ProcSet |-> CASE self = "req" -> "Take"
                                        [] self = "worker" -> "Loop"
                                        [] self = "exit" -> "E_Wait"
                                        [] self = "b" -> "B_Loop"]

Take == /\ pc["req"] = "Take"
        /\ Free
        /\ IF ownerDead \/ poisoned
              THEN /\ poisoned' = TRUE
                   /\ UNCHANGED tornRead
              ELSE /\ IF data = "torn"
                         THEN /\ tornRead' = TRUE
                         ELSE /\ TRUE
                              /\ UNCHANGED tornRead
                   /\ UNCHANGED poisoned
        /\ holder' = "req"
        /\ ownerDead' = FALSE
        /\ pc' = [pc EXCEPT !["req"] = "Tear"]
        /\ UNCHANGED << data, aAlive, status, shutdown, panicked, inScope, 
                        replyBegun, replies >>

Tear == /\ pc["req"] = "Tear"
        /\ data' = "torn"
        /\ \/ /\ replyBegun' = TRUE
              /\ replies' = 1
           \/ /\ TRUE
              /\ UNCHANGED <<replyBegun, replies>>
        /\ \/ /\ inScope' = TRUE
           \/ /\ TRUE
              /\ UNCHANGED inScope
        /\ pc' = [pc EXCEPT !["req"] = "Act"]
        /\ UNCHANGED << holder, ownerDead, poisoned, tornRead, aAlive, status, 
                        shutdown, panicked >>

Act == /\ pc["req"] = "Act"
       /\ \/ /\ data' = "ok"
             /\ holder' = "none"
             /\ pc' = [pc EXCEPT !["req"] = "Done"]
             /\ UNCHANGED panicked
          \/ /\ panicked' = TRUE
             /\ pc' = [pc EXCEPT !["req"] = "Hook"]
             /\ UNCHANGED <<holder, data>>
       /\ UNCHANGED << ownerDead, poisoned, tornRead, aAlive, status, shutdown, 
                       inScope, replyBegun, replies >>

Hook == /\ pc["req"] = "Hook"
        /\ IF aAlive
              THEN /\ IF inScope /\ Variant /= "hook_exits_in_scope"
                         THEN /\ IF Variant /= "unlock_on_unwind"
                                    THEN /\ poisoned' = TRUE
                                    ELSE /\ TRUE
                                         /\ UNCHANGED poisoned
                              /\ holder' = "none"
                              /\ pc' = [pc EXCEPT !["req"] = "Catch"]
                              /\ UNCHANGED << ownerDead, aAlive, status >>
                         ELSE /\ IF holder \in {"req", "worker"}
                                    THEN /\ ownerDead' = TRUE
                                    ELSE /\ TRUE
                                         /\ UNCHANGED ownerDead
                              /\ aAlive' = FALSE
                              /\ status' = 70
                              /\ pc' = [pc EXCEPT !["req"] = "Done"]
                              /\ UNCHANGED << holder, poisoned >>
              ELSE /\ pc' = [pc EXCEPT !["req"] = "Done"]
                   /\ UNCHANGED << holder, ownerDead, poisoned, aAlive, status >>
        /\ UNCHANGED << data, tornRead, shutdown, panicked, inScope, 
                        replyBegun, replies >>

Catch == /\ pc["req"] = "Catch"
         /\ IF aAlive /\ (~replyBegun \/ Variant = "reply_twice")
               THEN /\ replies' = replies + 1
               ELSE /\ TRUE
                    /\ UNCHANGED replies
         /\ IF Variant /= "continue_after_catch"
               THEN /\ shutdown' = TRUE
               ELSE /\ TRUE
                    /\ UNCHANGED shutdown
         /\ pc' = [pc EXCEPT !["req"] = "Done"]
         /\ UNCHANGED << holder, ownerDead, poisoned, data, tornRead, aAlive, 
                         status, panicked, inScope, replyBegun >>

Request == Take \/ Tear \/ Act \/ Hook \/ Catch

Loop == /\ pc["worker"] = "Loop"
        /\ IF aAlive
              THEN /\ pc' = [pc EXCEPT !["worker"] = "W_Take"]
              ELSE /\ pc' = [pc EXCEPT !["worker"] = "W_Done"]
        /\ UNCHANGED << holder, ownerDead, poisoned, data, tornRead, aAlive, 
                        status, shutdown, panicked, inScope, replyBegun, 
                        replies >>

W_Take == /\ pc["worker"] = "W_Take"
          /\ Free \/ ~aAlive
          /\ IF aAlive
                THEN /\ IF ownerDead \/ poisoned
                           THEN /\ poisoned' = TRUE
                                /\ UNCHANGED tornRead
                           ELSE /\ IF data = "torn"
                                      THEN /\ tornRead' = TRUE
                                      ELSE /\ TRUE
                                           /\ UNCHANGED tornRead
                                /\ UNCHANGED poisoned
                     /\ holder' = "worker"
                     /\ ownerDead' = FALSE
                     /\ pc' = [pc EXCEPT !["worker"] = "W_Give"]
                ELSE /\ pc' = [pc EXCEPT !["worker"] = "W_Done"]
                     /\ UNCHANGED << holder, ownerDead, poisoned, tornRead >>
          /\ UNCHANGED << data, aAlive, status, shutdown, panicked, inScope, 
                          replyBegun, replies >>

W_Give == /\ pc["worker"] = "W_Give"
          /\ IF holder = "worker" /\ aAlive
                THEN /\ holder' = "none"
                ELSE /\ TRUE
                     /\ UNCHANGED holder
          /\ pc' = [pc EXCEPT !["worker"] = "Loop"]
          /\ UNCHANGED << ownerDead, poisoned, data, tornRead, aAlive, status, 
                          shutdown, panicked, inScope, replyBegun, replies >>

W_Done == /\ pc["worker"] = "W_Done"
          /\ TRUE
          /\ pc' = [pc EXCEPT !["worker"] = "Done"]
          /\ UNCHANGED << holder, ownerDead, poisoned, data, tornRead, aAlive, 
                          status, shutdown, panicked, inScope, replyBegun, 
                          replies >>

Worker == Loop \/ W_Take \/ W_Give \/ W_Done

E_Wait == /\ pc["exit"] = "E_Wait"
          /\ shutdown \/ ~aAlive
          /\ pc' = [pc EXCEPT !["exit"] = "E_Exit"]
          /\ UNCHANGED << holder, ownerDead, poisoned, data, tornRead, aAlive, 
                          status, shutdown, panicked, inScope, replyBegun, 
                          replies >>

E_Exit == /\ pc["exit"] = "E_Exit"
          /\ IF aAlive
                THEN /\ IF holder \in {"req", "worker"}
                           THEN /\ ownerDead' = TRUE
                           ELSE /\ TRUE
                                /\ UNCHANGED ownerDead
                     /\ aAlive' = FALSE
                     /\ status' = 70
                ELSE /\ TRUE
                     /\ UNCHANGED << ownerDead, aAlive, status >>
          /\ pc' = [pc EXCEPT !["exit"] = "Done"]
          /\ UNCHANGED << holder, poisoned, data, tornRead, shutdown, panicked, 
                          inScope, replyBegun, replies >>

Exit == E_Wait \/ E_Exit

B_Loop == /\ pc["b"] = "B_Loop"
          /\ pc' = [pc EXCEPT !["b"] = "B_Take"]
          /\ UNCHANGED << holder, ownerDead, poisoned, data, tornRead, aAlive, 
                          status, shutdown, panicked, inScope, replyBegun, 
                          replies >>

B_Take == /\ pc["b"] = "B_Take"
          /\ Free
          /\ IF ownerDead \/ poisoned
                THEN /\ poisoned' = TRUE
                     /\ UNCHANGED tornRead
                ELSE /\ IF data = "torn"
                           THEN /\ tornRead' = TRUE
                           ELSE /\ TRUE
                                /\ UNCHANGED tornRead
                     /\ UNCHANGED poisoned
          /\ holder' = "b"
          /\ ownerDead' = FALSE
          /\ pc' = [pc EXCEPT !["b"] = "B_Give"]
          /\ UNCHANGED << data, aAlive, status, shutdown, panicked, inScope, 
                          replyBegun, replies >>

B_Give == /\ pc["b"] = "B_Give"
          /\ holder' = "none"
          /\ pc' = [pc EXCEPT !["b"] = "B_Loop"]
          /\ UNCHANGED << ownerDead, poisoned, data, tornRead, aAlive, status, 
                          shutdown, panicked, inScope, replyBegun, replies >>

Sibling == B_Loop \/ B_Take \/ B_Give

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Request \/ Worker \/ Exit \/ Sibling
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Request)
        /\ WF_vars(Worker)
        /\ WF_vars(Exit)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
