```
program
    = import
    | statement
    | source
    | program program

import
    = "from" package "import" [function]
    | "import" package "as" qualifier
    | "import" package

qualifier,function = identifier
package = path

statement = signature | declaration | application

signature = identifier "::" [type ","] "->" type "where" [constraint "\n"]

constraint
    = "let" identifier "=" aexpr -- assign an expression to a variable
    | bexpr                      -- boolean expression

bexpr
    = "(" bexpr ")"
    | aexpr rop aexpr     -- compare two expressions
    | bapplication        -- application that reduces to a boolean
    | bexpr bop bexpr     -- boolean binary operation
    | "not" bexpr

aexpr
    = "(" aexpr ")"
    | number
    | - aexpr
    | indexedAccess Num  -- where a number must be returned
    | aexpr aop aexpr
    | aapplication       -- application that reduces to a number
    | set 

indexedAccess
    = identifier "[" iexpr "]"        -- e.g.  x[i]
    | identifier "[" [iexpr, ","] "]" -- e.g.  a[i,j]

iexpr
    = "(" iexpr ")"
    | integer
    | indexedAccess Int  -- where an integer must be returned
    | iexpr "..." iexpr    -- e.g. 1 : 50
    | set
    
set = "[" aexpr "|" [ buildStatement "," ] "]"

buildStatement
    = identifier <- aexpr
    | bexpr

bop = "and" | "or" | "xor" | "not" | "all" | "any" | "none"

aop = *
    | /
    | // -- integer division
    | +
    | -
    | %  -- modulus
    | ^  -- exponentiation

rop = < | > | <= | >= | ==

type = typename
     | typename type'

type'
    = typename
    | "(" type ")"
    | type' type'

typename
    = capitolizedIdentifier
    | identifier "=" capitolizedIdentifier

declaration
    = name "=" constant
    | name "=" application
    | name parameters "=" application

type_declaration
    = name "::" input "->" type
    | name "::" type

input = type | input "," type

application
    = function argument
    | application argument

constant = string | number | integer | array

argument = constant | "(" application ")"

label      = string
package    = filename
identifier = function | constant
```

`[a ","]` indicates a comma separated list of `a`.

Source code from other languages can also be nested in a Morloc script, however
this will be extracted into packages in preprocessing.
