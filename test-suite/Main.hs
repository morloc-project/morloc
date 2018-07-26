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


  testRdfCodeWith
    (rmId [0])
    "A :: Bool"
    [ iuu 1 "morloc:isa" "morloc:typeDeclaration"
    , iui 1 "morloc:lhs" 2
    , iut 2 "morloc:isa" "morloc:name" "A"
    , iui 1 "morloc:rhs" 3
    , iut 3 "morloc:isa" "morloc:atomicType" "Bool"
    ]

  -- testRdfCodeWith
  --   iuu rmId [0])
  --   "A :: [Bool]"
  --   [ iuu 1 "morloc:isa" "morloc:typeDeclaration"
  --   , iui 1 "morloc:lhs" 2
  --   , iuu 2 "morloc:isa" "morloc:name" "A"
  --   , iui 1 "morloc:rhs" 3
  --   , iuu 3 "morloc:isa" "morloc:parameterizedType" "List"
  --   , iui 3 "morloc:parameter" 4
  --   , iuu 4 "morloc:isa" "morloc:atomicType" "Bool"
  --   ]
  --
  -- testRdfCodeWith
  --   iuu rmId [0])
  --   "A :: iuu Bool, Fool)"
  --   [ iuu1 "morloc:isa" "morloc:typeDeclaration"
  --   , iui 1 "morloc:lhs" 2
  --   , iuu 2 "morloc:isa" "morloc:name" "A"
  --   , iui 1 "morloc:rhs" 3
  --   , iuu 3 "morloc:isa" "morloc:parameterizedType" "Tuple"
  --   , iui 3 "morloc:parameter" 4
  --   , iuu 4 "morloc:isa" "morloc:atomicType" "Bool"
  --   , iu. 3 "morloc:parameter" 5
  --   , iuu 5 "morloc:isa" "morloc:atomicType" "Fool"
  --   ]
  --
  -- testRdfCodeWith
  --   iuu rmId [0])
  --   "A :: {B :: Bool, C :: Fool}"
  --   [ iuu 1 "morloc:isa" "morloc:typeDeclaration"
  --   , iui 1 "morloc:lhs" 2
  --   , iuu 2 "morloc:isa" "morloc:name" "A"
  --   , iui 1 "morloc:rhs" 3
  --   , iuu 3 "morloc:isa" "morloc:parameterizedType" "Record"
  --   , iui 3 "morloc:parameter" 4
  --   , iuu 4 "morloc:isa" "morloc:namedType"
  --   , iuu 4 "morloc:name" Str' "B"
  --   , iui 4 "morloc:value" 5
  --   , iuu 5 "morloc:isa" "morloc:atomicType" "Bool"
  --   , iui 3 "morloc:parameter" 6
  --   , iuu 6 "morloc:isa" "morloc:namedType"
  --   , iuu 6 "morloc:name" Str' "C"
  --   , iui 6 "morloc:value" 7
  --   , iuu 7 "morloc:isa" "morloc:atomicType" "Fool"
  --   ]
  --
  -- testRdfCode
  --   "x = 1"
  --   [ iuu 0 "morloc:isa"   "morloc:script"
  --   , iui 0 "morloc:child"  1
  --   , iuu 1 "morloc:isa"   "morloc:dataDeclaration"
  --   , iui 1 "morloc:lhs"    2
  --   , iuu 2 "morloc:isa"   "morloc:name" "x"
  --   , iui 1 "morloc:rhs"    3
  --   , iut 3 "morloc:isa"   "morloc:integer" "1"
  --   ]
  --
  -- testRdfCode
  --   "f x = x"
  --   [ iuu 0 "morloc:isa"       "morloc:script"
  --   , iuu 0 "morloc:child"      1
  --   , iuu 1 "morloc:isa"       "morloc:dataDeclaration"
  --   , iuu 1 "morloc:lhs"        2
  --   , iuu 2 "morloc:isa"       "morloc:name"
  --   , iuu 2 "morloc:value"     Str' "f"
  --   , iuu 1 "morloc:parameter"  3
  --   , iuu 3 "morloc:isa"       "morloc:name"
  --   , iuu 3 "morloc:value"     Str' "x"
  --   , iuu 1 "morloc:rhs"        4
  --   , iuu 4 "morloc:isa"       "morloc:name"
  --   , iuu 4 "morloc:value"     Str' "x"
  --   ]
  --
  -- testRdfCode
  --   "f = g 42 66"
  --   [ iuu 0 "morloc:isa"      "morloc:script"
  --   , iuu 0 "morloc:child"     1
  --   , iuu 1 "morloc:isa"      "morloc:dataDeclaration"
  --   , iuu 1 "morloc:lhs"       2
  --   , iuu 2 "morloc:isa"      "morloc:name"
  --   , iuu 2 "morloc:value"    Str' "f"
  --   , iuu 1 "morloc:rhs"       3
  --   , iuu 3 "morloc:isa"      "morloc:call"
  --   , iuu 3 "morloc:value"    4
  --   , iuu 4 "morloc:isa"      "morloc:name"
  --   , iuu 4 "morloc:value"    Str' "g"
  --   , iuu 3 "morloc:argument"  5
  --   , iuu 5 "morloc:isa"      "morloc:integer"
  --   , iuu 5 "morloc:value"    Int' 42
  --   , iuu 3 "morloc:argument"  6
  --   , iuu 6 "morloc:isa"      "morloc:integer"
  --   , iuu 6 "morloc:value"    Int' 66
  --   ]
  --
  -- it "iuu x = (5)) == (x = 5)" $ do
  --   shouldBe
  --     iuu morlocScript "x = (5);"
  --     iuu morlocScript "x = 5;"
  --
  -- testRdfCode
  --   "iuu 1,\"foo\"1.1)"
  --   [ iuu 0 "morloc:isa"      "morloc:script"
  --   , iuu 0 "morloc:child"     1
  --   , iuu 1 "morloc:isa"      "morloc:tuple"
  --   , iuu 1 "morloc:contains"  2
  --   , iuu 2 "morloc:isa"      "morloc:integer"
  --   , iuu 2 "morloc:value"    Int' 1
  --   , iuu 1 "morloc:contains"  3
  --   , iuu 3 "morloc:isa"      "morloc:string"
  --   , iuu 3 "morloc:value"    Str' "foo"
  --   , iuu 1 "morloc:contains"  4
  --   , iuu 4 "morloc:isa"      "morloc:number"
  --   , iuu 4 "morloc:value"    Num' 1.1
  --   ]
  --
  -- testRdfCodeWith
  --   iuu rmId [0..3])
  --   "foo :: i:Int -> j:[A]"
  --   [ iuu 4 "morloc:isa"       "morloc:atomicType"
  --   , iuu 4 "morloc:value"     Str' "Int"
  --   , iuu 4 "morloc:label"     Str' "i"
  --   , iuu 5 "morloc:isa"       "morloc:parameterizedType"
  --   , iuu 5 "morloc:value"     Str' "List"
  --   , iuu 5 "morloc:label"     Str' "j"
  --   , iuu 5 "morloc:parameter"  6
  --   , iuu 6 "morloc:isa"       "morloc:atomicType"
  --   , iuu 6 "morloc:value"     Str' "A"
  --   ]
  --
  -- testRdfCode
  --   "foo :: i:Int -> Num where iuu i > 0)"
  --   [ iuu 0 "morloc:isa"        "morloc:script"
  --   , iuu 0 "morloc:child"       1
  --   , iuu 1 "morloc:isa"        "morloc:typeDeclaration"
  --   , iuu 1 "morloc:lhs"         2
  --   , iuu 2 "morloc:isa"        "morloc:name"
  --   , iuu 2 "morloc:value"      Str' "foo"
  --   , iuu 1 "morloc:rhs"         3
  --   , iuu 3 "morloc:isa"        "morloc:functionType"    ) -- i:Int -> Num where iuu i > 0
  --   , iuu 3 "morloc:input"       4
  --   , iuu 4 "morloc:isa"        "morloc:atomicType"      ) -- i:Int
  --   , iuu 4 "morloc:value"      Str' "Int"
  --   , iuu 4 "morloc:label"      Str' "i"
  --   , iuu 3 "morloc:output"      5                  ) -- Num
  --   , iuu 5 "morloc:isa"        "morloc:atomicType"
  --   , iuu 5 "morloc:value"      Str' "Num"
  --   , iuu 3 "morloc:constraint"  6
  --   , iuu 6 "morloc:isa"        "morloc:binop"           ) -- "i > 0"
  --   , iuu 6 "morloc:value"      Str' "GT"
  --   , iuu 6 "morloc:lhs"         7
  --   , iuu 6 "morloc:rhs"         8
  --   , iuu 7 "morloc:isa"        "morloc:name"
  --   , iuu 7 "morloc:value"      Str' "i"
  --   , iuu 8 "morloc:isa"        "morloc:integer"
  --   , iuu 8 "morloc:value"      Int' 0
  --   ]
  --
  -- testRdfCode
  --   "foo :: Int"
  --   [ iuu 0 "morloc:isa"        "morloc:script"
  --   , iuu 0 "morloc:child"       1
  --   , iuu 1 "morloc:isa"        "morloc:typeDeclaration"
  --   , iuu 1 "morloc:lhs"         2
  --   , iuu 2 "morloc:isa"        "morloc:name"
  --   , iuu 2 "morloc:value"      Str' "foo"
  --   , iuu 1 "morloc:rhs"         3
  --   , iuu 3 "morloc:isa"        "morloc:atomicType"
  --   , iuu 3 "morloc:value"      Str' "Int"
  --   ]
  --
  -- testRdfCodeWith
  --   iuu rmId [0..5])
  --   "foo :: X -> Y where iuu 1.1 + 1.2 > 2.0)"
  --   [ iuu 6,  "morloc:isa"   "morloc:binop" "GT"
  --   , iuu 6,  "morloc:lhs"    8
  --   , iuu 6,  "morloc:rhs"    10
  --   , iuu 8,  "morloc:isa"   "morloc:binop" "Add"
  --   , iuu 8,  "morloc:lhs"    7
  --   , iuu 8,  "morloc:rhs"    9
  --   , iuu 7,  "morloc:isa"   "morloc:number"
  --   , iuu 7,  "morloc:value" Num' 1.1
  --   , iuu 9 "morloc:isa"   "morloc:number"
  --   , iuu 9 "morloc:value" Num' 1.2
  --   , iuu 10 "morloc:isa"   "morloc:number"
  --   , iuu 10 "morloc:value" Num' 2.0
  --   ]
  --
  -- testRdfCode
  --   "foo :: a, iuu b -> c) -> d"
  --   [ iuu 0 "morloc:isa"    "morloc:script"
  --   , iuu 0 "morloc:child"   1
  --   , iuu 1 "morloc:isa"    "morloc:typeDeclaration"
  --   , iuu 1 "morloc:lhs"     2
  --   , iuu 2 "morloc:isa"    "morloc:name"
  --   , iuu 2 "morloc:value"  Str' "foo"
  --   , iuu 1 "morloc:rhs"     3
  --   , iuu 3 "morloc:isa"    "morloc:functionType"
  --   , iuu 3 "morloc:input"   4
  --   , iuu 4 "morloc:isa"    "morloc:atomicGeneric"
  --   , iuu 4 "morloc:value"  Str' "a"
  --   , iuu 3 "morloc:input"   5
  --   , iuu 5 "morloc:isa"    "morloc:functionType"
  --   , iuu 5 "morloc:input"   6
  --   , iuu 6 "morloc:isa"    "morloc:atomicGeneric"
  --   , iuu 6 "morloc:value"  Str' "b"
  --   , iuu 5 "morloc:output"  7
  --   , iuu 7 "morloc:isa"    "morloc:atomicGeneric"
  --   , iuu 7 "morloc:value"  Str' "c"
  --   , iuu 3 "morloc:output"  8
  --   , iuu 8 "morloc:isa"    "morloc:atomicGeneric"
  --   , iuu 8 "morloc:value"  Str' "d"
  --   ]
  --
  -- testRdfCodeWith
  --   iuu rmId [0..2])
  --   "foo :: A B -> C D"
  --   [ iuu 3 "morloc:isa"       "morloc:functionType"
  --   , iuu 3 "morloc:input"      4
  --   , iuu 4 "morloc:isa"       "morloc:parameterizedType"
  --   , iuu 4 "morloc:value"     Str' "A"
  --   , iuu 4 "morloc:parameter"  5
  --   , iuu 5 "morloc:isa"       "morloc:atomicType"
  --   , iuu 5 "morloc:value"     Str' "B"
  --   , iuu 3 "morloc:output"     6
  --   , iuu 6 "morloc:isa"       "morloc:parameterizedType"
  --   , iuu 6 "morloc:value"     Str' "C"
  --   , iuu 6 "morloc:parameter"  7
  --   , iuu 7 "morloc:isa"       "morloc:atomicType"
  --   , iuu 7 "morloc:value"     Str' "D"
  --   ]
  --
  -- testRdfCodeWith
  --   iuu rmId [0..3])
  --   "foo :: A where iuu (1 == 1) and (2 == 2))"
  --   [ iuu 4,  "morloc:isa"   "morloc:binop" "and"
  --   , iuu 4,  "morloc:lhs"    5
  --   , iuu 4,  "morloc:rhs"    8
  --   , iuu 5,  "morloc:isa"   "morloc:binop" "EQ"
  --   , iuu 5,  "morloc:lhs"    6
  --   , iuu 5,  "morloc:rhs"    7
  --   , iuu 6,  "morloc:isa"   "morloc:integer"
  --   , iuu 6,  "morloc:value" Int' 1
  --   , iuu 7,  "morloc:isa"   "morloc:integer"
  --   , iuu 7,  "morloc:value" Int' 1
  --   , iuu 8,  "morloc:isa"   "morloc:binop" "EQ"
  --   , iuu 8,  "morloc:lhs"    9
  --   , iuu 8,  "morloc:rhs"    10
  --   , iuu 9 "morloc:isa"   "morloc:integer"
  --   , iuu 9 "morloc:value" Int' 2
  --   , iuu 10 "morloc:isa"   "morloc:integer"
  --   , iuu 10 "morloc:value" Int' 2
  --   ]
  --
  -- testRdfCode
  --   "f . g"
  --   [ iuu 0 "morloc:isa"   "morloc:script"
  --   , iuu 0 "morloc:child"  2
  --   , iuu 2 "morloc:isa"   "morloc:composition"
  --   , iuu 2 "morloc:lhs"    1
  --   , iuu 2 "morloc:rhs"    3
  --   , iuu 1 "morloc:isa"   "morloc:name" "f"
  --   , iuu 3 "morloc:isa"   "morloc:name" "g"
  --   ]
  --
  -- testRdfCodeWith
  --   iuu rmId ([0..6] ++ [9])
  --   -- this will fail later, since x,k, and t are undefined.
  --   "X :: Y where iuu x^(-k) == 1)"
  --   [ iuu 7 "morloc:isa"      Str' "Neg"
  --   , iuu 7 "morloc:contains"  8
  --   , iuu 8 "morloc:isa"      "morloc:name"
  --   , iuu 8 "morloc:value"    Str' "k"
  --   ]
  --
  -- testRdfCodeWith
  --   iuu rmId ([0..3])
  --   -- this will fail later, since x,k, and t are undefined.
  --   "X :: Y where iuu f x (g y z))"
  --   [ iuu 4 "morloc:isa"      "morloc:call"
  --   , iuu 4 "morloc:name"     Str' "f"
  --   , iui 4 "morloc:argument"  5
  --   , iuu 5 "morloc:isa"      "morloc:name"
  --   , iuu 5 "morloc:value"    Str' "x"
  --   , iui 4 "morloc:argument" 6
  --   , iuu 6 "morloc:isa"      "morloc:call"
  --   , iuu 6 "morloc:name"     Str' "g"
  --   , iui 6 "morloc:argument"  7
  --   , iuu 7 "morloc:isa"      "morloc:name"
  --   , iuu 7 "morloc:value"    Str' "y"
  --   , iui 6 "morloc:argument"  8
  --   , iuu 8 "morloc:isa"      "morloc:name"
  --   , iuu 8 "morloc:value"    Str' "z"
  --   ]
  --
  -- testRdfCodeWith
  --   iuu rmId ([0..4] ++ [8])
  --   -- this will fail later, since x,k, and t are undefined.
  --   "X :: Y where iuu f x y == 1)"
  --   [ iuu 5 "morloc:isa"      "morloc:call"
  --   , iuu 5 "morloc:name"     Str' "f"
  --   , iui 5 "morloc:argument" 6
  --   , iuu 6 "morloc:isa"      "morloc:name"
  --   , iuu 6 "morloc:value"    Str' "x"
  --   , iui 5 "morloc:argument"  7
  --   , iuu 7 "morloc:isa"      "morloc:name"
  --   , iuu 7 "morloc:value"    Str' "y"
  --   ]
