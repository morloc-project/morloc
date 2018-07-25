{-# LANGUAGE OverloadedStrings #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import qualified Data.RDF as DR
import qualified Data.Sort as DS
import qualified Data.Text as DT

import qualified Morloc.Parser as MP
import qualified Morloc.Triple as M3
import qualified Morloc.Data as MD
import qualified Morloc.Error as ME

main :: IO ()
main = do
  test <- testSpec "morloc" spec
  Test.Tasty.defaultMain test

rmId :: [Int] -> [DR.Triple] -> [DR.Triple]
rmId is ts = filter (rmId' is) ts
  where
    rmId' :: [Int] -> DR.Triple -> Bool 
    rmId' is' (DR.Triple s _ _) = all (\x -> (M3.asId x) /= s) is'

testRdfCodeWith :: ([DR.Triple] -> [DR.Triple]) -> String -> [DR.Triple] -> Spec 
testRdfCodeWith f s ts = case (run' f s) of
  (Right ts') -> it s $ do shouldBe (DS.sort ts) (DS.sort ts')
  (Left err) -> error (unlines ["Failure in:", s, ">>>" ++ show err])
  where
    run' f s = fmap (mapTriples f) (MP.morlocScript (s ++ ";"))
    mapTriples f (M3.TopRDF _ rdf) = f (DR.triplesOf rdf)

testRdfCode :: String -> [DR.Triple] -> Spec
testRdfCode = testRdfCodeWith id

iuu :: Int -> String -> String -> DR.Triple
iuu s p o = M3.tripleN' s p o

iui :: Int -> String -> Int -> DR.Triple
iui s p o = M3.tripleN s p (DR.UNode (DT.pack . show $ o))  

iut :: Int -> String -> String -> String -> DR.Triple
iut s p t o = M3.tripleL s p t o

spec :: Spec
spec = parallel $ do

  testRdfCode
    "source \"R\" (\"fo.o\" as foo)"
    [ iuu 0 "morloc:isa"     "morloc:script"
    , iui 0 "morloc:child_0" 1
    , iuu 1 "morloc:isa"     "morloc:source"
    , iut 1 "morloc:lang"    "morloc:string" "R"
    , iui 1 "morloc:import"  2
    , iut 2 "morloc:name"    "morloc:string" "fo.o"
    , iut 2 "morloc:alias"   "morloc:string" "foo"
    ]

  testRdfCode
    "from bob/foo import (bar, baz)"
    [ iuu 0 "morloc:isa"     "morloc:script"
    , iui 0 "morloc:child_0" 1
    , iuu 1 "morloc:isa"     "morloc:restricted_import"
    , iut 1 "morloc:name"    "morloc:string" "bob.foo"
    , iui 1 "morloc:import"  2
    , iut 2 "morloc:isa"     "morloc:name"   "bar"
    , iui 1 "morloc:import"  3
    , iut 3 "morloc:isa"     "morloc:name"   "baz"
    ]

  testRdfCode
    "import bob/foo as foo"
    [ iuu 0 "morloc:isa"       "morloc:script"
    , iui 0 "morloc:child_0"   1
    , iuu 1 "morloc:isa"       "morloc:import"
    , iut 1 "morloc:name"      "morloc:string" "bob.foo"
    , iut 1 "morloc:namespace" "morloc:string" "foo"
    ]

  testRdfCodeWith
    (rmId [0])
    "42"
    [ iut 1 "morloc:isa" "morloc:integer" "42" ]


  testRdfCodeWith
    (rmId [0])
    "-42"
    [ iut 1 "morloc:isa" "morloc:integer" "-42" ]

  testRdfCodeWith
    (rmId [0])
    "4.2"
    [ iut 1 "morloc:isa" "morloc:number" "4.2" ]

  testRdfCodeWith
    (rmId [0])
    "True"
    [ iut 1 "morloc:isa" "morloc:boolean" "True" ]

  testRdfCodeWith
    (rmId [0])
    "[42,99]"
    [ iuu 1 "morloc:isa"        "morloc:list"
    , iui 1 "morloc:contains_0" 2
    , iut 2 "morloc:isa"        "morloc:integer" "42"
    , iui 1 "morloc:contains_1" 3
    , iut 3 "morloc:isa"        "morloc:integer" "99"
    ]

  testRdfCodeWith
    (rmId [0])
    "[42,\"foo\"]"
    [ iuu 1 "morloc:isa"      "morloc:list"
    , iui 1 "morloc:contains_0" 2
    , iut 2 "morloc:isa"      "morloc:integer" "42"
    , iui 1 "morloc:contains_1" 3
    , iut 3 "morloc:isa"      "morloc:string" "foo"
    ]

  testRdfCodeWith
    (rmId [0])
    "{job = \"poopsmith\", age = 34}"
    [ iuu 1 "morloc:isa"      "morloc:record"
    , iui 1 "morloc:contains_0" 2
    , iuu 2 "morloc:isa"      "morloc:recordEntry"
    , iut 2 "morloc:lhs"      "morloc:name"    "job"
    , iui 2 "morloc:rhs"      3
    , iut 3 "morloc:isa"      "morloc:string"  "poopsmith"
    , iui 1 "morloc:contains_1" 4
    , iuu 4 "morloc:isa"      "morloc:recordEntry"
    , iut 4 "morloc:lhs"      "morloc:name"    "age"
    , iui 4 "morloc:rhs"      5
    , iut 5 "morloc:isa"      "morloc:integer" "34"
    ]

  -- testRdfCodeWith
  --   (rmId [0])
  --   "A :: Bool"
  --   [ (1, "morloc:isa", Str' "morloc:typeDeclaration"
  --   , (1, "morloc:lhs", Id' 2
  --   , (2, "morloc:isa", Str' "morloc:name"
  --   , (2, "morloc:value", Str' "A"
  --   , (1, "morloc:rhs", Id' 3
  --   , (3, "morloc:isa", Str' "morloc:atomicType"
  --   , (3, "morloc:value", Str' "Bool"
  --   ]
  --
  -- testRdfCodeWith
  --   (rmId [0])
  --   "A :: [Bool]"
  --   [ (1, "morloc:isa", Str' "morloc:typeDeclaration"
  --   , (1, "morloc:lhs", Id' 2
  --   , (2, "morloc:isa", Str' "morloc:name"
  --   , (2, "morloc:value", Str' "A"
  --   , (1, "morloc:rhs", Id' 3
  --   , (3, "morloc:isa", Str' "morloc:parameterizedType"
  --   , (3, "morloc:value", Str' "List"
  --   , (3, "morloc:parameter", Id' 4
  --   , (4, "morloc:isa", Str' "morloc:atomicType"
  --   , (4, "morloc:value", Str' "Bool"
  --   ]
  --
  -- testRdfCodeWith
  --   (rmId [0])
  --   "A :: (Bool, Fool)"
  --   [ (1, "morloc:isa", Str' "morloc:typeDeclaration"
  --   , (1, "morloc:lhs", Id' 2
  --   , (2, "morloc:isa", Str' "morloc:name"
  --   , (2, "morloc:value", Str' "A"
  --   , (1, "morloc:rhs", Id' 3
  --   , (3, "morloc:isa", Str' "morloc:parameterizedType"
  --   , (3, "morloc:value", Str' "Tuple"
  --   , (3, "morloc:parameter", Id' 4
  --   , (4, "morloc:isa", Str' "morloc:atomicType"
  --   , (4, "morloc:value", Str' "Bool"
  --   , (3, "morloc:parameter", Id' 5
  --   , (5, "morloc:isa", Str' "morloc:atomicType"
  --   , (5, "morloc:value", Str' "Fool"
  --   ]
  --
  -- testRdfCodeWith
  --   (rmId [0])
  --   "A :: {B :: Bool, C :: Fool}"
  --   [ (1, "morloc:isa", Str' "morloc:typeDeclaration"
  --   , (1, "morloc:lhs", Id' 2
  --   , (2, "morloc:isa", Str' "morloc:name"
  --   , (2, "morloc:value", Str' "A"
  --   , (1, "morloc:rhs", Id' 3
  --   , (3, "morloc:isa", Str' "morloc:parameterizedType"
  --   , (3, "morloc:value", Str' "Record"
  --   , (3, "morloc:parameter", Id' 4
  --   , (4, "morloc:isa", Str' "morloc:namedType"
  --   , (4, "morloc:name", Str' "B"
  --   , (4, "morloc:value", Id' 5
  --   , (5, "morloc:isa", Str' "morloc:atomicType"
  --   , (5, "morloc:value", Str' "Bool"
  --   , (3, "morloc:parameter", Id' 6
  --   , (6, "morloc:isa", Str' "morloc:namedType"
  --   , (6, "morloc:name", Str' "C"
  --   , (6, "morloc:value", Id' 7
  --   , (7, "morloc:isa", Str' "morloc:atomicType"
  --   , (7, "morloc:value", Str' "Fool"
  --   ]
  --
  -- testRdfCode
  --   "x = 1"
  --   [ (0, "morloc:isa",   Str' "morloc:script"
  --   , (0, "morloc:child", Id'  1
  --   , (1, "morloc:isa",   Str' "morloc:dataDeclaration"
  --   , (1, "morloc:lhs",   Id'  2
  --   , (2, "morloc:isa",   Str' "morloc:name"
  --   , (2, "morloc:value", Str' "x"
  --   , (1, "morloc:rhs",   Id'  3
  --   , (3, "morloc:isa",   Str' "morloc:integer"
  --   , (3, "morloc:value", Int' 1
  --   ]
  --
  -- testRdfCode
  --   "f x = x"
  --   [ (0, "morloc:isa",       Str' "morloc:script"
  --   , (0, "morloc:child",     Id'  1
  --   , (1, "morloc:isa",       Str' "morloc:dataDeclaration"
  --   , (1, "morloc:lhs",       Id'  2
  --   , (2, "morloc:isa",       Str' "morloc:name"
  --   , (2, "morloc:value",     Str' "f"
  --   , (1, "morloc:parameter", Id'  3
  --   , (3, "morloc:isa",       Str' "morloc:name"
  --   , (3, "morloc:value",     Str' "x"
  --   , (1, "morloc:rhs",       Id'  4
  --   , (4, "morloc:isa",       Str' "morloc:name"
  --   , (4, "morloc:value",     Str' "x"
  --   ]
  --
  -- testRdfCode
  --   "f = g 42 66"
  --   [ (0, "morloc:isa",      Str' "morloc:script"
  --   , (0, "morloc:child",    Id'  1
  --   , (1, "morloc:isa",      Str' "morloc:dataDeclaration"
  --   , (1, "morloc:lhs",      Id'  2
  --   , (2, "morloc:isa",      Str' "morloc:name"
  --   , (2, "morloc:value",    Str' "f"
  --   , (1, "morloc:rhs",      Id'  3
  --   , (3, "morloc:isa",      Str' "morloc:call"
  --   , (3, "morloc:value",    Id' 4
  --   , (4, "morloc:isa",      Str' "morloc:name"
  --   , (4, "morloc:value",    Str' "g"
  --   , (3, "morloc:argument", Id'  5
  --   , (5, "morloc:isa",      Str' "morloc:integer"
  --   , (5, "morloc:value",    Int' 42
  --   , (3, "morloc:argument", Id'  6
  --   , (6, "morloc:isa",      Str' "morloc:integer"
  --   , (6, "morloc:value",    Int' 66
  --   ]
  --
  -- it "(x = (5)) == (x = 5)" $ do
  --   shouldBe
  --     (morlocScript "x = (5);"
  --     (morlocScript "x = 5;"
  --
  -- testRdfCode
  --   "(1,\"foo\",1.1)"
  --   [ (0, "morloc:isa",      Str' "morloc:script"
  --   , (0, "morloc:child",    Id'  1
  --   , (1, "morloc:isa",      Str' "morloc:tuple"
  --   , (1, "morloc:contains", Id'  2
  --   , (2, "morloc:isa",      Str' "morloc:integer"
  --   , (2, "morloc:value",    Int' 1
  --   , (1, "morloc:contains", Id'  3
  --   , (3, "morloc:isa",      Str' "morloc:string"
  --   , (3, "morloc:value",    Str' "foo"
  --   , (1, "morloc:contains", Id'  4
  --   , (4, "morloc:isa",      Str' "morloc:number"
  --   , (4, "morloc:value",    Num' 1.1
  --   ]
  --
  -- testRdfCodeWith
  --   (rmId [0..3])
  --   "foo :: i:Int -> j:[A]"
  --   [ (4, "morloc:isa",       Str' "morloc:atomicType"
  --   , (4, "morloc:value",     Str' "Int"
  --   , (4, "morloc:label",     Str' "i"
  --   , (5, "morloc:isa",       Str' "morloc:parameterizedType"
  --   , (5, "morloc:value",     Str' "List"
  --   , (5, "morloc:label",     Str' "j"
  --   , (5, "morloc:parameter", Id'  6
  --   , (6, "morloc:isa",       Str' "morloc:atomicType"
  --   , (6, "morloc:value",     Str' "A"
  --   ]
  --
  -- testRdfCode
  --   "foo :: i:Int -> Num where (i > 0)"
  --   [ (0, "morloc:isa",        Str' "morloc:script"
  --   , (0, "morloc:child",      Id'  1
  --   , (1, "morloc:isa",        Str' "morloc:typeDeclaration"
  --   , (1, "morloc:lhs",        Id'  2
  --   , (2, "morloc:isa",        Str' "morloc:name"
  --   , (2, "morloc:value",      Str' "foo"
  --   , (1, "morloc:rhs",        Id'  3
  --   , (3, "morloc:isa",        Str' "morloc:functionType"    ) -- i:Int -> Num where (i > 0
  --   , (3, "morloc:input",      Id'  4
  --   , (4, "morloc:isa",        Str' "morloc:atomicType"      ) -- i:Int
  --   , (4, "morloc:value",      Str' "Int"
  --   , (4, "morloc:label",      Str' "i"
  --   , (3, "morloc:output",     Id'  5                  ) -- Num
  --   , (5, "morloc:isa",        Str' "morloc:atomicType"
  --   , (5, "morloc:value",      Str' "Num"
  --   , (3, "morloc:constraint", Id'  6
  --   , (6, "morloc:isa",        Str' "morloc:binop"           ) -- "i > 0"
  --   , (6, "morloc:value",      Str' "GT"
  --   , (6, "morloc:lhs",        Id'  7
  --   , (6, "morloc:rhs",        Id'  8
  --   , (7, "morloc:isa",        Str' "morloc:name"
  --   , (7, "morloc:value",      Str' "i"
  --   , (8, "morloc:isa",        Str' "morloc:integer"
  --   , (8, "morloc:value",      Int' 0
  --   ]
  --
  -- testRdfCode
  --   "foo :: Int"
  --   [ (0, "morloc:isa",        Str' "morloc:script"
  --   , (0, "morloc:child",      Id'  1
  --   , (1, "morloc:isa",        Str' "morloc:typeDeclaration"
  --   , (1, "morloc:lhs",        Id'  2
  --   , (2, "morloc:isa",        Str' "morloc:name"
  --   , (2, "morloc:value",      Str' "foo"
  --   , (1, "morloc:rhs",        Id'  3
  --   , (3, "morloc:isa",        Str' "morloc:atomicType"
  --   , (3, "morloc:value",      Str' "Int"
  --   ]
  --
  -- testRdfCodeWith
  --   (rmId [0..5])
  --   "foo :: X -> Y where (1.1 + 1.2 > 2.0)"
  --   [ (6,  "morloc:isa",   Str' "morloc:binop"
  --   , (6,  "morloc:value", Str' "GT"
  --   , (6,  "morloc:lhs",   Id'  8
  --   , (6,  "morloc:rhs",   Id'  10
  --   , (8,  "morloc:isa",   Str' "morloc:binop"
  --   , (8,  "morloc:value", Str' "Add"
  --   , (8,  "morloc:lhs",   Id'  7
  --   , (8,  "morloc:rhs",   Id'  9
  --   , (7,  "morloc:isa",   Str' "morloc:number"
  --   , (7,  "morloc:value", Num' 1.1
  --   , (9, "morloc:isa",   Str' "morloc:number"
  --   , (9, "morloc:value", Num' 1.2
  --   , (10, "morloc:isa",   Str' "morloc:number"
  --   , (10, "morloc:value", Num' 2.0
  --   ]
  --
  -- testRdfCode
  --   "foo :: a, (b -> c) -> d"
  --   [ (0, "morloc:isa",    Str' "morloc:script"
  --   , (0, "morloc:child",  Id'  1
  --   , (1, "morloc:isa",    Str' "morloc:typeDeclaration"
  --   , (1, "morloc:lhs",    Id'  2
  --   , (2, "morloc:isa",    Str' "morloc:name"
  --   , (2, "morloc:value",  Str' "foo"
  --   , (1, "morloc:rhs",    Id'  3
  --   , (3, "morloc:isa",    Str' "morloc:functionType"
  --   , (3, "morloc:input",  Id'  4
  --   , (4, "morloc:isa",    Str' "morloc:atomicGeneric"
  --   , (4, "morloc:value",  Str' "a"
  --   , (3, "morloc:input",  Id'  5
  --   , (5, "morloc:isa",    Str' "morloc:functionType"
  --   , (5, "morloc:input",  Id'  6
  --   , (6, "morloc:isa",    Str' "morloc:atomicGeneric"
  --   , (6, "morloc:value",  Str' "b"
  --   , (5, "morloc:output", Id'  7
  --   , (7, "morloc:isa",    Str' "morloc:atomicGeneric"
  --   , (7, "morloc:value",  Str' "c"
  --   , (3, "morloc:output", Id'  8
  --   , (8, "morloc:isa",    Str' "morloc:atomicGeneric"
  --   , (8, "morloc:value",  Str' "d"
  --   ]
  --
  -- testRdfCodeWith
  --   (rmId [0..2])
  --   "foo :: A B -> C D"
  --   [ (3, "morloc:isa",       Str' "morloc:functionType"
  --   , (3, "morloc:input",     Id'  4
  --   , (4, "morloc:isa",       Str' "morloc:parameterizedType"
  --   , (4, "morloc:value",     Str' "A"
  --   , (4, "morloc:parameter", Id'  5
  --   , (5, "morloc:isa",       Str' "morloc:atomicType"
  --   , (5, "morloc:value",     Str' "B"
  --   , (3, "morloc:output",    Id'  6
  --   , (6, "morloc:isa",       Str' "morloc:parameterizedType"
  --   , (6, "morloc:value",     Str' "C"
  --   , (6, "morloc:parameter", Id'  7
  --   , (7, "morloc:isa",       Str' "morloc:atomicType"
  --   , (7, "morloc:value",     Str' "D"
  --   ]
  --
  -- testRdfCodeWith
  --   (rmId [0..3])
  --   "foo :: A where ((1 == 1) and (2 == 2))"
  --   [ (4,  "morloc:isa",   Str' "morloc:binop"
  --   , (4,  "morloc:value", Str' "and"
  --   , (4,  "morloc:lhs",   Id'  5
  --   , (4,  "morloc:rhs",   Id'  8
  --   , (5,  "morloc:isa",   Str' "morloc:binop"
  --   , (5,  "morloc:value", Str' "EQ"
  --   , (5,  "morloc:lhs",   Id'  6
  --   , (5,  "morloc:rhs",   Id'  7
  --   , (6,  "morloc:isa",   Str' "morloc:integer"
  --   , (6,  "morloc:value", Int' 1
  --   , (7,  "morloc:isa",   Str' "morloc:integer"
  --   , (7,  "morloc:value", Int' 1
  --   , (8,  "morloc:isa",   Str' "morloc:binop"
  --   , (8,  "morloc:value", Str' "EQ"
  --   , (8,  "morloc:lhs",   Id'  9
  --   , (8,  "morloc:rhs",   Id'  10
  --   , (9, "morloc:isa",   Str' "morloc:integer"
  --   , (9, "morloc:value", Int' 2
  --   , (10, "morloc:isa",   Str' "morloc:integer"
  --   , (10, "morloc:value", Int' 2
  --   ]
  --
  -- testRdfCode
  --   "f . g"
  --   [ (0, "morloc:isa",   Str' "morloc:script"
  --   , (0, "morloc:child", Id'  2
  --   , (2, "morloc:isa",   Str' "morloc:composition"
  --   , (2, "morloc:lhs",   Id'  1
  --   , (2, "morloc:rhs",   Id'  3
  --   , (1, "morloc:isa",   Str' "morloc:name"
  --   , (1, "morloc:value", Str' "f"
  --   , (3, "morloc:isa",   Str' "morloc:name"
  --   , (3, "morloc:value", Str' "g"
  --   ]
  --
  -- testRdfCodeWith
  --   (rmId ([0..6] ++ [9])
  --   -- this will fail later, since x,k, and t are undefined.
  --   "X :: Y where (x^(-k) == 1)"
  --   [ (7, "morloc:isa",      Str' "Neg"
  --   , (7, "morloc:contains", Id'  8
  --   , (8, "morloc:isa",      Str' "morloc:name"
  --   , (8, "morloc:value",    Str' "k"
  --   ]
  --
  -- testRdfCodeWith
  --   (rmId ([0..3])
  --   -- this will fail later, since x,k, and t are undefined.
  --   "X :: Y where (f x (g y z))"
  --   [ (4, "morloc:isa",      Str' "morloc:call"
  --   , (4, "morloc:name",     Str' "f"
  --   , (4, "morloc:argument", Id'  5
  --   , (5, "morloc:isa",      Str' "morloc:name"
  --   , (5, "morloc:value",    Str' "x"
  --   , (4, "morloc:argument", Id'  6
  --   , (6, "morloc:isa",      Str' "morloc:call"
  --   , (6, "morloc:name",     Str' "g"
  --   , (6, "morloc:argument", Id'  7
  --   , (7, "morloc:isa",      Str' "morloc:name"
  --   , (7, "morloc:value",    Str' "y"
  --   , (6, "morloc:argument", Id'  8
  --   , (8, "morloc:isa",      Str' "morloc:name"
  --   , (8, "morloc:value",    Str' "z"
  --   ]
  --
  -- testRdfCodeWith
  --   (rmId ([0..4] ++ [8])
  --   -- this will fail later, since x,k, and t are undefined.
  --   "X :: Y where (f x y == 1)"
  --   [ (5, "morloc:isa",      Str' "morloc:call"
  --   , (5, "morloc:name",     Str' "f"
  --   , (5, "morloc:argument", Id'  6
  --   , (6, "morloc:isa",      Str' "morloc:name"
  --   , (6, "morloc:value",    Str' "x"
  --   , (5, "morloc:argument", Id'  7
  --   , (7, "morloc:isa",      Str' "morloc:name"
  --   , (7, "morloc:value",    Str' "y"
  --   ]
