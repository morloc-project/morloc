------------------------------ MODULE FrameStack ------------------------------
(* The frame stack of one thread (model/runtime/state.md STATE-5, STATE-6). *)
(* A program is a nest of calls: the pool's dispatch, named and unnamed    *)
(* generated functions, sourced code, closures and `@try`. A named call    *)
(* pushes its label on entry and pops it once its result is computed; a    *)
(* failure skips the pops. Dispatch, `@try` and closures are boundaries:   *)
(* each saves the depth on entry and, when a failure leaves it, takes the  *)
(* labels above the saved depth into the failure and restores the depth.   *)
(* Sourced code may catch any failure and carry on, and may enter a nested *)
(* dispatch on the same thread. A failure is a user failure, which carries *)
(* a trace, or a pass-through failure (closed pipe, infrastructure error), *)
(* which boundaries pass on unchanged. Each call has a unique id, so a     *)
(* trace that names a stale or wrong instance is seen.                     *)
EXTENDS Integers, Sequences

CONSTANTS Variant, MaxCalls, Fuel, RingSize
\* "design": the design.
\* "reset_at_dispatch": dispatch sets the depth to zero on entry.
\* "no_closure_boundary": a closure lets a failure through untouched.
\* "pass_keeps_depth": a pass-through failure leaves a boundary without
\*   restoring the depth.
\* "ring": labels kept in a ring of RingSize slots tagged with their depth.
\* "pop_before_value": the pop precedes the computation of the result.
\* "no_pop_without_return": a named function whose body is not a return
\*   statement never pops.

Kinds == {"dispatch", "named", "unnamed", "sourced", "closure", "try"}
Boundaries == {"dispatch", "closure", "try"}
Generated == {"dispatch", "named", "unnamed", "closure", "try"}
NoExc == [kind |-> "none", msg |-> <<>>, expect |-> <<>>]
Slots == 1..(MaxCalls + Fuel)

VARIABLES calls, depth, slot, exc, nextId, fuel,
          badTrace, unbalanced

vars == <<calls, depth, slot, exc, nextId, fuel, badTrace, unbalanced>>

Top == calls[Len(calls)]
Pop(s) == SubSeq(s, 1, Len(s) - 1)

\* The slot holding depth k, and what reading it for depth k returns: the
\* id it records, or Lost (no call has id 0).
Where(k) == IF Variant = "ring" THEN ((k - 1) % RingSize) + 1 ELSE k
Lost == 0
Recorded(k) ==
    IF k \notin Slots THEN Lost
    ELSE IF slot[Where(k)][1] = k THEN slot[Where(k)][2] ELSE Lost

\* The labels above `saved`, innermost first.
Lines(saved) == [i \in 1..(depth - saved) |-> Recorded(depth - i + 1)]

LiveNamed ==
    LET named == SelectSeq(calls, LAMBDA c : c.kind = "named" /\ ~c.done)
    IN [i \in 1..Len(named) |-> named[i].id]

CanCall(from, k) ==
    CASE from = "none"    -> k = "dispatch"
      [] from = "sourced" -> k \in {"closure", "sourced", "dispatch"}
      [] OTHER            -> k \in {"named", "unnamed", "sourced", "closure", "try"}

Init ==
    /\ calls = <<>>
    /\ depth = 0
    /\ slot = [k \in Slots |-> <<0, 0>>]
    /\ exc = NoExc
    /\ nextId = 1
    /\ fuel = Fuel
    /\ badTrace = FALSE
    /\ unbalanced = FALSE

Step == fuel' = fuel - 1 /\ nextId' = nextId + 1

Call(k) ==
    /\ exc.kind = "none"
    /\ Len(calls) < MaxCalls
    /\ CanCall(IF calls = <<>> THEN "none" ELSE Top.kind, k)
    /\ (calls /= <<>> => ~Top.done)
    /\ LET entry == IF k = "dispatch" /\ Variant = "reset_at_dispatch"
                    THEN 0 ELSE depth
           frame == [kind |-> k, id |-> nextId, saved |-> entry, done |-> FALSE]
       IN /\ calls' = Append(calls, frame)
          /\ IF k = "named"
             THEN /\ entry + 1 \in Slots
                  /\ depth' = entry + 1
                  /\ slot' = [slot EXCEPT ![Where(entry + 1)] = <<entry + 1, nextId>>]
             ELSE /\ depth' = entry
                  /\ UNCHANGED slot
    /\ Step
    /\ UNCHANGED <<exc, badTrace, unbalanced>>

\* A named call has computed its result.
Finish ==
    /\ exc.kind = "none"
    /\ calls /= <<>>
    /\ Top.kind = "named" /\ ~Top.done
    /\ calls' = [calls EXCEPT ![Len(calls)].done = TRUE]
    /\ \/ depth' = depth - 1
       \/ Variant = "no_pop_without_return" /\ UNCHANGED depth
    /\ Step
    /\ UNCHANGED <<slot, exc, badTrace, unbalanced>>

Return ==
    /\ exc.kind = "none"
    /\ calls /= <<>>
    /\ (Top.kind = "named" => Top.done)
    /\ calls' = Pop(calls)
    /\ IF Top.kind \in Boundaries /\ depth /= Top.saved
       THEN unbalanced' = TRUE ELSE UNCHANGED unbalanced
    /\ Step
    /\ UNCHANGED <<depth, slot, exc, badTrace>>

\* The innermost call raises a failure. A named call that has computed its
\* result raises nothing more: what is left is a move of the result and
\* the release of its locals.
Throw(kind) ==
    /\ exc.kind = "none"
    /\ calls /= <<>>
    /\ Top.kind \in {"named", "unnamed", "sourced"}
    /\ (Top.kind = "named" /\ Top.done) => Variant = "pop_before_value"
    /\ exc' = [kind |-> kind, msg |-> <<>>, expect |-> <<>>]
    /\ Step
    /\ UNCHANGED <<calls, depth, slot, badTrace, unbalanced>>

\* The failure leaves the innermost call.
Unwind ==
    /\ exc.kind /= "none"
    /\ calls /= <<>>
    /\ LET t == Top
           fail == exc.kind = "fail"
           restores == ~fail => Variant /= "pass_keeps_depth"
           traced == [exc EXCEPT !.msg = exc.msg \o Lines(t.saved)]
           exact == fail => traced.msg = exc.expect
       IN CASE t.kind = "named" ->
                 /\ calls' = Pop(calls)
                 /\ exc' = IF fail
                           THEN [exc EXCEPT !.expect = Append(exc.expect, t.id)]
                           ELSE exc
                 /\ UNCHANGED <<depth, badTrace>>
            [] t.kind = "unnamed"
               \/ (t.kind = "closure" /\ Variant = "no_closure_boundary") ->
                 /\ calls' = Pop(calls)
                 /\ UNCHANGED <<exc, depth, badTrace>>
            [] t.kind = "sourced" ->
                 \/ /\ exc' = NoExc
                    /\ UNCHANGED <<calls, depth, badTrace>>
                 \/ /\ calls' = Pop(calls)
                    /\ UNCHANGED <<exc, depth, badTrace>>
            [] t.kind = "closure" ->
                 /\ calls' = Pop(calls)
                 /\ exc' = IF fail THEN traced ELSE exc
                 /\ depth' = IF restores THEN t.saved ELSE depth
                 /\ UNCHANGED badTrace
            [] t.kind = "try" ->
                 /\ IF fail
                    THEN /\ calls' = Pop(calls)
                         /\ exc' = NoExc
                         /\ badTrace' = (badTrace \/ ~exact)
                    ELSE /\ calls' = Pop(calls)
                         /\ UNCHANGED <<exc, badTrace>>
                 /\ depth' = IF restores THEN t.saved ELSE depth
            [] t.kind = "dispatch" ->
                 /\ calls' = Pop(calls)
                 /\ exc' = NoExc
                 /\ badTrace' = (badTrace \/ ~exact)
                 /\ depth' = IF restores THEN t.saved ELSE depth
    /\ Step
    /\ UNCHANGED <<slot, unbalanced>>

\* A run ends when its fuel does.
Done == fuel = 0 /\ UNCHANGED vars

Next ==
    \/ /\ fuel > 0
       /\ \/ \E k \in Kinds : Call(k)
          \/ Finish
          \/ Return
          \/ \E kind \in {"fail", "pass"} : Throw(kind)
          \/ Unwind
    \/ Done

Spec == Init /\ [][Next]_vars

\* Every trace a boundary reports names exactly the named calls the failure
\* unwound through, innermost first.
TraceExact == ~badTrace
\* A boundary left normally finds the depth it saved.
Balanced == ~unbalanced
NoUnderflow == depth >= 0
\* With no failure in flight the stack lists exactly the named calls that
\* have not yet computed their result, outermost first; so its top is the
\* call a crash report names.
StackIsTheCalls ==
    exc.kind = "none" =>
        /\ depth = Len(LiveNamed)
        /\ \A k \in 1..depth : Recorded(k) = LiveNamed[k]
=============================================================================
