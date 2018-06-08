```
statement = import | declaration | application

import = "import" string

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
