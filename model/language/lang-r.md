# R

What sourced R code must do, and what morloc does for it. These are the R
forms of rules stated once for every language; each item cites its rule.
Prefix: RLANG.

Rules for R stated elsewhere: NATIVE-5 (records), NUM-1 and NUM-3 (`Int`
and 64-bit integers), RECUR-3 (R's recursion limit).

### RLANG-1 A sourced R function never changes an argument by reference
Intent: ruled 2026-10-18
Code: unaudited

Ordinary R values copy on modify, so changing a local variable that holds
an argument is allowed. A sourced function never changes an argument
through reference semantics: an environment, a reference class or R6
object, `data.table`'s `:=` and `set*` functions, or an external pointer.
The rule is not checked; a program that breaks it has no defined result
(EVAL-7). It holds until morloc has a way to say where a value may be
changed.
