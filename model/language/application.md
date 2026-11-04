# Application

Application, partial application, arity and arrow grouping. Prefix: APP.

### APP-1 Arrow grouping is not part of a morloc type
Intent: ruled 2026-09-24
Tests: spec-app-1-1, spec-app-1-2, spec-app-1-3
Code: conforms 2026-11-04

`a -> b -> c` and `a -> (b -> c)` are one type. Neither the nesting of a
definition's lambdas nor the grouping in a general signature changes how a
value is represented. Grouping is a property of an implementation, declared
at its `source` (HOST). This holds where a type variable is instantiated to
a function: `mk :: Int -> a` at an instance `Int -> Int` is `mk :: Int ->
Int -> Int`, and `mk 1` is a partial application of it. A call that gives a
function fewer arguments than it takes is a partial application wherever
the function comes from: a parameter, a condition's arm, a list. A source
takes the arguments of its declared signature (HOST-1, HOST-2, HOST-10),
any other function those of its type. An instance whose type ends in a
variable (`instance Mk a`) covers a use at a function type.

Why: languages disagree on grouping, so a general signature cannot decide it.

### APP-2 Partial application yields a function of the remaining arguments
Intent: proposed
Code: conforms 2026-10-21

Applying a function of n arguments to k < n arguments yields a function of
the other n - k. Operators apply partially on either side (OP).

### APP-3 Pure application is eager and runs nothing suspended
Intent: proposed
Code: conforms 2026-10-21

A pure argument is evaluated once, at the application. Applying a function
whose result is a suspension builds that suspension and runs nothing (EFF-6).

### APP-4 Applying a value whose type is a variable instantiated to a function
Intent: ruled 2026-11-04
Tests: spec-app-4-1, spec-app-4-2, spec-app-4-3, spec-app-4-4
Code: conforms 2026-11-04

A call may give more arguments than its head's type shows when that
type's result is still to be solved: `f xs1 xs2` is `(f xs1) xs2`, and the
result is a function of the rest. `format "a =" 1 2` with `format :: Str ->
r` takes its numbers this way, as Haskell's printf does. A result that is a
rigid variable (`apply1 f = f 1 2` in the body of `apply1 :: (Int -> a) ->
a`) or a suspension (`<IO> a`) takes no more arguments.

### APP-5 A function exported with no arguments supplied
Intent: open
Code: unaudited

Is a term `f :: Int` distinct from a function of zero arguments, in the
language and at the command line? Candidates: (a) a constant, computed once
per run of the program; (b) the same as `Unit -> Int`.
