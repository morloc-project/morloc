# Lexical structure

Characters, tokens, comments, indentation and layout, identifier classes. Prefix: LEX.

### LEX-1 `--` always opens a comment
Intent: proposed
Code: unaudited

`--` runs to the end of the line, whatever follows it. `--'` opens a
docstring line (DOC-1) and `--*` an export-group line. `--^` is rejected.
`{-` ... `-}` is a block comment, and block comments nest. No operator name
begins with `--` (OP-1).

### LEX-2 The case of a name's first character fixes its class
Intent: proposed
Code: unaudited

A name starts with a letter or `_` and continues with letters, digits, `_`
and `'`. Lowercase or `_` first: a term or a type variable. Uppercase
first: a type, a constructor, a class or a kind. A lone `_` is a wildcard.
`'name` is a tick name, used only in type-level label lists.

### LEX-3 Keywords are reserved
Intent: proposed
Code: unaudited

`module import export source from as where type newtype data record object
class instance effect escapable infixl infixr infix match let in do` and the
literal words `True False Null Inf NaN` cannot be used as names.

### LEX-4 Blocks are delimited by indentation or by explicit braces
Intent: proposed
Code: unaudited

The bindings after `where`, `let` and `do` form a block delimited by
indentation, or by `{` and `}` with `;` separators when `{` directly follows
the keyword.

### LEX-5 Whitespace around `@` and `-` changes the token
Intent: proposed
Code: unaudited

`x@p`, with no whitespace on either side, is a binder (BIND-5). `@name` at
the start of a line or after whitespace or a delimiter is an intrinsic.
`-` written directly against a digit is part of a numeric literal (LIT-3).

### LEX-6 Non-ASCII letters in names
Intent: open
Code: unaudited

String literals and comments may hold any Unicode text. May a name contain
non-ASCII letters? Candidates: (a) yes, any Unicode letter, with case
deciding the class as in LEX-2; (b) no, names are ASCII, which every target
language can spell.
