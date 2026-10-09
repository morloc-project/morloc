# Operators

Infix operators: declaration, fixity, precedence, sections, import. Prefix: OP.

### OP-1 An operator name is a run of operator characters
Intent: proposed
Code: unaudited

The characters are `: ! $ % & * + . / < = > ? @ \ ^ | - ~ #`. A name may
not begin with `--` (LEX-1), and the bare `|` is reserved.

### OP-2 An operator is an ordinary term
Intent: proposed
Code: unaudited

Wrapped in parentheses, an operator name is used wherever a term name is:
signatures, definitions, `source` lists, class methods, export and import
lists. `(e op)` is `\y -> e op y` and `(op e)` is `\x -> x op e`.

### OP-3 Fixity is a level from 0 to 9 and an associativity
Intent: proposed
Code: unaudited

`infixl`, `infixr` and `infix` take a level in 0..9; a higher level binds
tighter, and a level outside 0..9 is a parse error. An operator with no
fixity declaration is `infixl 9`. Application binds tighter than any operator.

### OP-4 Ambiguous chains are rejected
Intent: proposed
Code: unaudited

`a op b op c` with `op` declared `infix` is an error, and so is a chain
mixing two operators of one level with different associativity.

### OP-5 An operator has one fixity in a program
Intent: proposed
Code: unaudited

Fixity travels with the operator on import; the importer does not redeclare
it. Two different fixity declarations for one operator are an error.

### OP-6 A section of `-`
Intent: open
Code: unaudited

Is `(- e)` a right section (`\x -> x - e`) or the negation `negate e`
(LIT-3)? Candidates: (a) negation, as in Haskell, with `subtract e` for the
section; (b) a section, with negation written `negate e`.
