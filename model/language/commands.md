# Commands

Which exports become commands, and their names. Prefix: CMD.

### CMD-1 Every exported term is a command named after the term
Intent: proposed
Code: conforms 2026-10-21

Each term a program's main module exports becomes a subcommand. `--' @name n`
on its signature names the command `n` instead. The first line of its
docstring is its summary, and the module's docstring describes the program.

### CMD-2 A generic export is not a command
Intent: proposed
Code: conforms 2026-10-21

An exported term whose type still has a type variable has no command line
form. The build warns, naming it, and builds the other commands.

### CMD-3 `--* group:` lines in an export list group commands
Intent: proposed
Code: conforms 2026-10-21

A `--* group: g` line opens group `g`; the following `--*` lines describe it,
and each export listed after it, until the next group line, is a subcommand
of `g`.

### CMD-4 Two commands with one name
Intent: open
Code: unaudited

`@name` can give a command the name of another export, and groups add a
second level of names. Either (a) two commands with one name in one group is
a build error naming both; or (b) the explicit `@name` wins and the build
warns.

### CMD-5 How a single-export program reads its first token
Intent: open
Code: unaudited

With one export, naming the command is optional, so `./greet hello` may be
the command `hello` or the argument `"hello"`, and the manual says the name
does not end the nexus zone. Either (a) the name is optional and a first
token equal to it is the name; or (b) the name is never accepted; or (c) it
is always required.

### CMD-6 Command arguments are parsed and verified according to the statically known type
Intent: ruled 2026-11-01
Code: conforms 2026-11-01

The code that reads, parses and checks a command's arguments is part of
the runtime; a program's author writes none of it. Each argument's type,
and so its wire schema, is known at compile time (SIG-10), and that schema
directs the parsing: the input is read in the fixed order its docstring
directives set (CLI-1, CLI-2), and the loaded value must fit the wire
schema exactly; a value that does not is rejected before the command runs
(IFACE-17). Marshalling the value into each language is generated.
