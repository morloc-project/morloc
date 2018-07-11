A Morloc program and all imported modules and ontologies, needs to be encoded
as a RDF triple store. Here I will work out the rules for how elements of
a Morloc script should be translated into RDF.

The system needs to define the

 1. connectivity of the graph (what functions call what)
 2. language-specific type hints
 3. type ontology (including the function types)

Object
 1. WUID - workflow unique identifier, used to identify elements, such as nodes
    in the application graph, that are unique to the workflow.
 2. UUID - universally unique identifiers, such as functions or types from Morloc.IO.
```

Productions:

```
<0> :isa :Module
<0> :name "Base"
<1> :isa :Module
<1> :name "Prelude"
<1> :parent <0>

    [
        (0, IsA' Module')
      , (0, Name' "Base")
      , (1, IsA' Module')
      , (1, Name' "Prelude")
      , (1, Parent' 0)
    ]

$ source "R" "file" ("foo" as fooyolo, "bar")
<p1>  :isa :Module
<p1>  :isa :Source
<p1>  :lang "R"
<p1>  :parent <1>   -- the imported code inherits from, and may mask Prelude
<p1>  :file "file"
<p1>  :import (<id2> <id3>)
<id2> :name "foo"
<id2> :alias "fooyolo"
<id3> :name "bar"

-- more packages may be stacked

<global> :isa :Module
<global> :lang "Morloc"
<global> :parent <p1>   -- the gobal environment inherits from the last package

$ fname :: [r?] => i1 [, i?] -> o1 [, o?] where ( [constraint?] )
<id1> :isa :FunctionSignature
<id1> :parent <global>
<id1> :name "fname"
<id1> :role <id2>
<id1> :role <id3>
<id1> :args (<id4> <id5> ...)
<id1> :outs (<id6> <id7> ...)
<id1> :cons <id8>
<id1> :cons <id9>

$ typename :: [r?] => i:t1 [n?:t?] where ([constraint?])
<id2> :isa :TypeSignature
<id1> :parent <global>
<id2> :name "typename"
<id2> :role <id3>
<id2> :role <id4>
<id5> :parameters (<id6>, ...)
<id5> :name "i"
<id2> :cons <id7>
<id2> :cons <id8>

$ a operator b
<id3> <binop> <id4>
<id3> :parent <signature>  -- appear only in constraints, in signature scope

$ len xs == n
<id1> :binop :EQ
<id1> :parent <signature>
<id1> :lhs <id2>
<id1> :rhs <id3>
<id2> :application (<id4> <id5>)
<id3> :name "n"
<id4> :name "len"
<id5> :name "xs"

$ foo x y = bar (baz x y)
<id1> :isa :Declaration
<id1> :parent <global>
<id1> :name "foo"
<id1> :args (<id2> <id3>)
<id2> :name "x"
<id3> :name "y"
<id1> :value <id4>
<id4> :application (<id5> <id6>)
<id5> :name "bar"
<id6> :application (<id7> <id8> <id9>)
<id7> :name "baz"
<id8> :name "x"
<id8> :name "y"


-- primitives -- they do not inherit from anything
-- though eventually I will allow variables and function in primitive containers
-- at that point, I will need to define scopes

$ 42
<id> :type :Integer
<id> :value 42

$ 42.0
<id> :type :Double
<id> :value 42.0

$ True
<id> :type :Bool
<id> :value true

$ [1,2,3]
<id1> :type :List
<id1> :value (<id2> <id3> <id4>)
<id2> :type :Integer
<id3> :type :Integer
<id4> :type :Integer
<id2> :value 1
<id3> :value 2
<id4> :value 3

$ (1,2,3)
<id1> :type :Tuple
<id1> :value (<id2> <id3> <id4>)
<id2> :type :Integer
<id3> :type :Integer
<id4> :type :Integer
<id2> :value 1
<id3> :value 2
<id4> :value 3

$ {a :: "foo", ...}
<id1> :type :Record
<id1> :value (<id2> ...)
<id2> :type (<id3> <id4>)
<id3> :type :String
<id3> :value "a"
<id4> :type :String
<id4> :value "foo"
...

```
