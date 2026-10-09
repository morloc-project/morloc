# Application

Application, partial application, arity and arrow grouping. Prefix: APP.

### APP-1 Arrow grouping is not part of a morloc type
Intent: ruled 2026-09-24
Code: unaudited

`a -> b -> c` and `a -> (b -> c)` are one type. Neither the nesting of a
definition's lambdas nor the grouping in a general signature changes how a
value is represented. Grouping is a property of an implementation, declared
at its `source` (HOST).

Why: languages disagree on grouping, so a general signature cannot decide it.

### APP-2 Partial application yields a function of the remaining arguments
Intent: proposed
Code: unaudited

Applying a function of n arguments to k < n arguments yields a function of
the other n - k. Operators apply partially on either side (OP).

### APP-3 Pure application is eager and runs nothing suspended
Intent: proposed
Code: unaudited

A pure argument is evaluated once, at the application. Applying a function
whose result is a suspension builds that suspension and runs nothing (EFF-6).

### APP-4 Applying a value whose type is a variable instantiated to a function
Intent: open
Code: unaudited

When `f :: a -> a` is instantiated at `a = Int -> Int`, may `f g 1` apply the
result to `1`? Candidates: (a) yes, an over-application is an application of
the result; (b) only through an explicit lambda.

### APP-5 A function exported with no arguments supplied
Intent: open
Code: unaudited

Is a term `f :: Int` distinct from a function of zero arguments, in the
language and at the command line? Candidates: (a) a constant, computed once
per run of the program; (b) the same as `Unit -> Int`.
