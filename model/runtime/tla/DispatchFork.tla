----------------------------- MODULE DispatchFork -----------------------------
(* A dispatch reads a request, runs user code and replies on the request's  *)
(* connection (model/runtime/fork.md FORK-12). User code may fork; the child shares *)
(* the connection and the worker's state, and once user code returns it    *)
(* either replies or leaves without replying (an exit or an uncaught        *)
(* exception), and the worker's cleanup then runs. The reading thread       *)
(* records who read the request; a process that does not match the record  *)
(* exits as soon as user code returns.                                      *)
EXTENDS Naturals, FiniteSets

CONSTANT Variant
\* "guarded": the record is the fork generation, checked when user code
\* returns.
\* "unguarded": no check.
\* "by_pid": checked when user code returns, but the record is the pid,
\* which a child may share (FORK-14).
\* "reply_only": the generation, checked only before a reply.

Procs == {"parent", "child"}

(* --algorithm DispatchFork
variables
    replied = {},
    touched = {},
    exited = {},
    forked = FALSE,
    generation = [p \in Procs |-> 0],
    pid = [p \in Procs |-> 1],
    recorded = [p \in Procs |-> 99];

define
    Me(p) == IF Variant = "by_pid" THEN pid[p] ELSE generation[p]
    Matches(p) == recorded[p] = Me(p)
    CheckOnReturn == Variant \in {"guarded", "by_pid"}
    CheckOnReply == Variant = "reply_only"
    AtMostOneReply == Cardinality(replied) <= 1
    ChildNeverTouchesTheWorker == "child" \notin touched
    ParentReplies == <>("parent" \in replied)
end define;

fair process Parent = "parent"
begin
  Read:
    recorded["parent"] := Me("parent");
  UserCode:
    either
      skip;
    or
      \* Fork copies the record; the child's generation always differs and
      \* its pid may equal the parent's.
      recorded["child"] := recorded["parent"];
      generation["child"] := generation["parent"] + 1;
      with p \in {1, 2} do
        pid["child"] := p;
      end with;
      forked := TRUE;
    end either;
  Reply:
    replied := replied \union {"parent"};
end process;

fair process Child = "child"
begin
  Wait:
    await forked \/ pc["parent"] = "Done";
    if ~forked then
      goto Finish;
    end if;
  Returned:
    if CheckOnReturn /\ ~Matches("child") then
      exited := exited \union {"child"};
      goto Finish;
    end if;
  Leave:
    either
      if CheckOnReply /\ ~Matches("child") then
        exited := exited \union {"child"};
      else
        replied := replied \union {"child"};
      end if;
    or
      touched := touched \union {"child"};
    end either;
  Finish:
    skip;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "4c89306d" /\ chksum(tla) = "c2f40b00")
VARIABLES replied, touched, exited, forked, generation, pid, recorded, pc

(* define statement *)
Me(p) == IF Variant = "by_pid" THEN pid[p] ELSE generation[p]
Matches(p) == recorded[p] = Me(p)
CheckOnReturn == Variant \in {"guarded", "by_pid"}
CheckOnReply == Variant = "reply_only"
AtMostOneReply == Cardinality(replied) <= 1
ChildNeverTouchesTheWorker == "child" \notin touched
ParentReplies == <>("parent" \in replied)


vars == << replied, touched, exited, forked, generation, pid, recorded, pc >>

ProcSet == {"parent"} \cup {"child"}

Init == (* Global variables *)
        /\ replied = {}
        /\ touched = {}
        /\ exited = {}
        /\ forked = FALSE
        /\ generation = [p \in Procs |-> 0]
        /\ pid = [p \in Procs |-> 1]
        /\ recorded = [p \in Procs |-> 99]
        /\ pc = [self \in ProcSet |-> CASE self = "parent" -> "Read"
                                        [] self = "child" -> "Wait"]

Read == /\ pc["parent"] = "Read"
        /\ recorded' = [recorded EXCEPT !["parent"] = Me("parent")]
        /\ pc' = [pc EXCEPT !["parent"] = "UserCode"]
        /\ UNCHANGED << replied, touched, exited, forked, generation, pid >>

UserCode == /\ pc["parent"] = "UserCode"
            /\ \/ /\ TRUE
                  /\ UNCHANGED <<forked, generation, pid, recorded>>
               \/ /\ recorded' = [recorded EXCEPT !["child"] = recorded["parent"]]
                  /\ generation' = [generation EXCEPT !["child"] = generation["parent"] + 1]
                  /\ \E p \in {1, 2}:
                       pid' = [pid EXCEPT !["child"] = p]
                  /\ forked' = TRUE
            /\ pc' = [pc EXCEPT !["parent"] = "Reply"]
            /\ UNCHANGED << replied, touched, exited >>

Reply == /\ pc["parent"] = "Reply"
         /\ replied' = (replied \union {"parent"})
         /\ pc' = [pc EXCEPT !["parent"] = "Done"]
         /\ UNCHANGED << touched, exited, forked, generation, pid, recorded >>

Parent == Read \/ UserCode \/ Reply

Wait == /\ pc["child"] = "Wait"
        /\ forked \/ pc["parent"] = "Done"
        /\ IF ~forked
              THEN /\ pc' = [pc EXCEPT !["child"] = "Finish"]
              ELSE /\ pc' = [pc EXCEPT !["child"] = "Returned"]
        /\ UNCHANGED << replied, touched, exited, forked, generation, pid, 
                        recorded >>

Returned == /\ pc["child"] = "Returned"
            /\ IF CheckOnReturn /\ ~Matches("child")
                  THEN /\ exited' = (exited \union {"child"})
                       /\ pc' = [pc EXCEPT !["child"] = "Finish"]
                  ELSE /\ pc' = [pc EXCEPT !["child"] = "Leave"]
                       /\ UNCHANGED exited
            /\ UNCHANGED << replied, touched, forked, generation, pid, 
                            recorded >>

Leave == /\ pc["child"] = "Leave"
         /\ \/ /\ IF CheckOnReply /\ ~Matches("child")
                     THEN /\ exited' = (exited \union {"child"})
                          /\ UNCHANGED replied
                     ELSE /\ replied' = (replied \union {"child"})
                          /\ UNCHANGED exited
               /\ UNCHANGED touched
            \/ /\ touched' = (touched \union {"child"})
               /\ UNCHANGED <<replied, exited>>
         /\ pc' = [pc EXCEPT !["child"] = "Finish"]
         /\ UNCHANGED << forked, generation, pid, recorded >>

Finish == /\ pc["child"] = "Finish"
          /\ TRUE
          /\ pc' = [pc EXCEPT !["child"] = "Done"]
          /\ UNCHANGED << replied, touched, exited, forked, generation, pid, 
                          recorded >>

Child == Wait \/ Returned \/ Leave \/ Finish

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Parent \/ Child
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Parent)
        /\ WF_vars(Child)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
