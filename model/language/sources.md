# Sources

`source` declarations: files, names, signatures, the claims a source makes. Prefix: SRC.

### SRC-1 A sourced name is a foreign symbol, checked against the language's name patterns
Intent: proposed
Code: unaudited

The name in a `source` declaration must match the language's identifier or
operator pattern. Anything else, such as raw code (`lambda x: x`), is
rejected. The patterns are per-language data, not knowledge built into the
compiler.

### SRC-2 `as` and `--' name:` rename a sourced term, and are equivalent
Intent: proposed
Code: unaudited

The parenthesized form with `as` and the block form with a `--' name:`
docstring declare the same thing. Without either, the morloc name is the
foreign name.

### SRC-3 A sourced signature is a claim the compiler does not check
Intent: proposed
Code: unaudited

The general type of a sourced term, including its effect row, is the
author's assertion about the foreign code. Effect rows are required there and
never inferred (see `EFF`).

### SRC-4 Whether a backtick name is checked
Intent: open
Code: unaudited

A backtick name (`` `and` as (&&) ``) is emitted as infix text between the
two arguments; the manual says verbatim, for any infix operator. That admits
raw code, which SRC-1 rejects. Either (a) backtick text must match a
per-language list of keyword operators; or (b) it is verbatim, as an
exception to SRC-1.

### SRC-5 When a sourced name that does not exist is reported
Intent: open
Code: unaudited

A Python source naming a builtin or a missing function builds and fails when
called. Either (a) the build checks that each sourced name exists, where the
language allows; or (b) the failure is a run-time error naming the term.
