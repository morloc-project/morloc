# Parts of the Morloc ecosystem

 1. semantic type system (OWL)
 2. database (SPARQL)
 3. functional scripting language
 4. generative compiler


Singular types
 - DistanceMeter
 - Index
 - SocialSecurityNum

Collections
 - tuples - (Numeric, List a)
 - parameterized types -
   - List a
   - Maybe a
 - algebraic
   - Bool :: True | False   -- enum when not parameterized
   - Tree a :: Node a (List (Tree a)) | Leaf a   -- Recursive OK
 - list
   - List a
 - table
   - TableH [(String, Type)]
   - TableHR [(String, Type)] a;
   - TableH [Type]
   - TableHR [Type] a;
 - matrix
   - Matrix a
 - tree
 - graph
 - structure
   - Record a {
        foo = List a,
        bar = Maybe a
        baz = Record a {
            bim = Numeric,
            bo  = String
        }
     }
