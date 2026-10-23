----------------------------- MODULE SlotSeqlock -----------------------------
(* A versioned read of a stream slot (model/runtime/streams.md SLOT-8). A reader *)
(* holding a handle for generation 1 loads the generation, reads a field   *)
(* and loads the generation again, accepting the field only if both loads  *)
(* match its handle. Meanwhile the slot is released and reopened: the      *)
(* generation is bumped, the field cleared, rewritten and published under  *)
(* a new generation. Without a fence before the second load, a weakly      *)
(* ordered processor may perform the field read after it; that reordering  *)
(* is modelled explicitly.                                                 *)
EXTENDS Naturals

CONSTANT Variant
\* "fenced": bump before clearing; a fence before the second load.
\* "clear_first": the release clears the field before bumping.
\* "no_fence": no fence, so the field read may follow the second load.
\* "no_writer_fence": no fence after the release's bump, so its clearing
\* store may become visible before the bump.

(* --algorithm SlotSeqlock
variables
    gen = 1,
    field = 1,
    accepted = FALSE,
    seen = 99;

define
    AcceptsOnlyItsOwnValue == accepted => seen = 1
end define;

fair process Writer = "writer"
variables clearFirst = FALSE;
begin
  Choose:
    if Variant = "clear_first" then
      clearFirst := TRUE;
    elsif Variant = "no_writer_fence" then
      with c \in {FALSE, TRUE} do
        clearFirst := c;
      end with;
    end if;
  Release:
    if clearFirst then
      field := 0;
    else
      gen := 2;
    end if;
  Release2:
    if clearFirst then
      gen := 2;
    else
      field := 0;
    end if;
  Reopen:
    field := 3;
  Publish:
    gen := 3;
end process;

fair process Reader = "reader"
variables g0 = 0, g1 = 0, late = FALSE;
begin
  First:
    g0 := gen;
    if g0 /= 1 then
      goto Done;
    end if;
  Order:
    if Variant = "no_fence" then
      with l \in {FALSE, TRUE} do
        late := l;
      end with;
    end if;
  Early:
    if ~late then
      seen := field;
    end if;
  Second:
    g1 := gen;
  Late:
    if late then
      seen := field;
    end if;
  Check:
    accepted := g1 = 1;
end process;

end algorithm; *)
\* BEGIN TRANSLATION (chksum(pcal) = "dc4331fe" /\ chksum(tla) = "46e8e675")
VARIABLES gen, field, accepted, seen, pc

(* define statement *)
AcceptsOnlyItsOwnValue == accepted => seen = 1

VARIABLES clearFirst, g0, g1, late

vars == << gen, field, accepted, seen, pc, clearFirst, g0, g1, late >>

ProcSet == {"writer"} \cup {"reader"}

Init == (* Global variables *)
        /\ gen = 1
        /\ field = 1
        /\ accepted = FALSE
        /\ seen = 99
        (* Process Writer *)
        /\ clearFirst = FALSE
        (* Process Reader *)
        /\ g0 = 0
        /\ g1 = 0
        /\ late = FALSE
        /\ pc = [self \in ProcSet |-> CASE self = "writer" -> "Choose"
                                        [] self = "reader" -> "First"]

Choose == /\ pc["writer"] = "Choose"
          /\ IF Variant = "clear_first"
                THEN /\ clearFirst' = TRUE
                ELSE /\ IF Variant = "no_writer_fence"
                           THEN /\ \E c \in {FALSE, TRUE}:
                                     clearFirst' = c
                           ELSE /\ TRUE
                                /\ UNCHANGED clearFirst
          /\ pc' = [pc EXCEPT !["writer"] = "Release"]
          /\ UNCHANGED << gen, field, accepted, seen, g0, g1, late >>

Release == /\ pc["writer"] = "Release"
           /\ IF clearFirst
                 THEN /\ field' = 0
                      /\ gen' = gen
                 ELSE /\ gen' = 2
                      /\ field' = field
           /\ pc' = [pc EXCEPT !["writer"] = "Release2"]
           /\ UNCHANGED << accepted, seen, clearFirst, g0, g1, late >>

Release2 == /\ pc["writer"] = "Release2"
            /\ IF clearFirst
                  THEN /\ gen' = 2
                       /\ field' = field
                  ELSE /\ field' = 0
                       /\ gen' = gen
            /\ pc' = [pc EXCEPT !["writer"] = "Reopen"]
            /\ UNCHANGED << accepted, seen, clearFirst, g0, g1, late >>

Reopen == /\ pc["writer"] = "Reopen"
          /\ field' = 3
          /\ pc' = [pc EXCEPT !["writer"] = "Publish"]
          /\ UNCHANGED << gen, accepted, seen, clearFirst, g0, g1, late >>

Publish == /\ pc["writer"] = "Publish"
           /\ gen' = 3
           /\ pc' = [pc EXCEPT !["writer"] = "Done"]
           /\ UNCHANGED << field, accepted, seen, clearFirst, g0, g1, late >>

Writer == Choose \/ Release \/ Release2 \/ Reopen \/ Publish

First == /\ pc["reader"] = "First"
         /\ g0' = gen
         /\ IF g0' /= 1
               THEN /\ pc' = [pc EXCEPT !["reader"] = "Done"]
               ELSE /\ pc' = [pc EXCEPT !["reader"] = "Order"]
         /\ UNCHANGED << gen, field, accepted, seen, clearFirst, g1, late >>

Order == /\ pc["reader"] = "Order"
         /\ IF Variant = "no_fence"
               THEN /\ \E l \in {FALSE, TRUE}:
                         late' = l
               ELSE /\ TRUE
                    /\ late' = late
         /\ pc' = [pc EXCEPT !["reader"] = "Early"]
         /\ UNCHANGED << gen, field, accepted, seen, clearFirst, g0, g1 >>

Early == /\ pc["reader"] = "Early"
         /\ IF ~late
               THEN /\ seen' = field
               ELSE /\ TRUE
                    /\ seen' = seen
         /\ pc' = [pc EXCEPT !["reader"] = "Second"]
         /\ UNCHANGED << gen, field, accepted, clearFirst, g0, g1, late >>

Second == /\ pc["reader"] = "Second"
          /\ g1' = gen
          /\ pc' = [pc EXCEPT !["reader"] = "Late"]
          /\ UNCHANGED << gen, field, accepted, seen, clearFirst, g0, late >>

Late == /\ pc["reader"] = "Late"
        /\ IF late
              THEN /\ seen' = field
              ELSE /\ TRUE
                   /\ seen' = seen
        /\ pc' = [pc EXCEPT !["reader"] = "Check"]
        /\ UNCHANGED << gen, field, accepted, clearFirst, g0, g1, late >>

Check == /\ pc["reader"] = "Check"
         /\ accepted' = (g1 = 1)
         /\ pc' = [pc EXCEPT !["reader"] = "Done"]
         /\ UNCHANGED << gen, field, seen, clearFirst, g0, g1, late >>

Reader == First \/ Order \/ Early \/ Second \/ Late \/ Check

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == Writer \/ Reader
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(Writer)
        /\ WF_vars(Reader)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION 
=============================================================================
