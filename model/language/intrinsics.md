# Intrinsics

The `@` intrinsics (`@load`, `@read`, `@stdin`, ...): their types and meaning. Prefix: INTR.

### INTR-1 An intrinsic that touches the world carries `<IO>`; one that can fail returns `Try`
Intent: proposed
Code: unaudited

A missing file, a full disk, a broken pipe or a decode mismatch is an `Err`
arm, never an effect label (see `FAIL`). `@read` is fallible and pure.
`@close`, `@tell`, `@stream`, `@stdout` and `@stderr` have no failure value.
`@throw` and `@try` are `FAIL`'s.

### INTR-2 A data-reading intrinsic yields data
Intent: proposed
Code: unaudited

The result type of `@load`, `@read`, `@next`, `@open` and `@stdin` contains
no suspension and no function. If inference solves it to one, the build is
rejected and asks for an annotation.

### INTR-3 What `@save`, `@savem` or `@savej` writes, `@load` reads back equal
Intent: proposed
Code: unaudited

`@load` detects the format from the bytes and returns a value equal to the
one written, or `Err` if the file does not hold a value of the expected type.

### INTR-4 `@schema` and `@typeof` depend only on their argument's type
Intent: proposed
Code: unaudited

The argument is not evaluated. `@typeof` gives the morloc type as written in
a signature, never a language-native name.

### INTR-5 `@version`, `@compiled` and `@datafile` are fixed when the program is built
Intent: proposed
Code: unaudited

`@datafile p` is the installed location of data file `p`, or `p` unchanged
when the program is not installed.

### INTR-6 Whether `@lang` may expose the realization
Intent: open
Code: unaudited

`@lang` is the canonical identifier of the language that evaluates it
(`"morloc"` outside any pool), so a program's value can depend on which
implementation REAL picks. Either (a) allowed, and REAL's interchangeability
excludes `@lang`; or (b) `@lang` is restricted to positions that cannot reach
a result, such as logging.
