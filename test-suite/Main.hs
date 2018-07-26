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
  test <- testSpec "Parser (Morloc script to RDF)" spec
  Test.Tasty.defaultMain test

rmId :: [Int] -> [DR.Triple] -> [DR.Triple]
rmId is ts = filter (rmId' is) ts
  where
    rmId' :: [Int] -> DR.Triple -> Bool 
    rmId' is' (DR.Triple s _ _) = all (\x -> (M3.idUri x) /= s) is'

testRdfCodeWith :: ([DR.Triple] -> [DR.Triple]) -> String -> [DR.Triple] -> Spec 
testRdfCodeWith f s ts = case (run' f s) of
  (Right ts') -> it s $ do shouldBe (DS.sort ts) (DS.sort ts')
  (Left err) -> error (unlines ["Failure in:", s, ">>>" ++ show err])
  where
    run' f s = fmap (mapTriples f) (MP.morlocScript (s ++ ";"))
    mapTriples f rdf = f (DR.triplesOf rdf)

testRdfCode :: String -> [DR.Triple] -> Spec
testRdfCode = testRdfCodeWith id

-- triple making convenience functions
iuu s r o = M3.uss (M3.idUri s) r o 
iui s r o = M3.usu (M3.idUri s) r (M3.idUri o)
iut s r t o = M3.ust (M3.idUri s) r t o

spec :: Spec
spec = parallel $ do

  testRdfCode
    "source \"R\" (\"fo.o\" as foo)"
    [ iuu 0 "rdf:type" "morloc:script"
    , iui 0 "morloc:child_0" 1
    , iuu 1 "rdf:type" "morloc:source"
    , iut 1 "morloc:lang" "morloc:string" "R"
    , iui 1 "morloc:import" 2
    , iut 2 "morloc:name" "morloc:string" "fo.o"
    , iut 2 "morloc:alias" "morloc:string" "foo"
    ]

  testRdfCode
    "from bob/foo import (bar, baz)"
    [ iuu 0 "rdf:type" "morloc:script"
    , iui 0 "morloc:child_0" 1
    , iuu 1 "rdf:type" "morloc:restricted_import"
    , iut 1 "morloc:name" "morloc:string" "bob.foo"
    , iui 1 "morloc:import" 2
    , iut 2 "rdf:type" "morloc:name" "bar"
    , iui 1 "morloc:import" 3
    , iut 3 "rdf:type" "morloc:name" "baz"
    ]

  testRdfCode
    "import bob/foo as foo"
    [ iuu 0 "rdf:type" "morloc:script"
    , iui 0 "morloc:child_0" 1
    , iuu 1 "rdf:type" "morloc:import"
    , iut 1 "morloc:name" "morloc:string" "bob.foo"
    , iut 1 "morloc:namespace" "morloc:string" "foo"
    ]

  testRdfCodeWith
    (rmId [0])
    "42"
    [ iut 1 "rdf:type" "morloc:integer" "42" ]


  testRdfCodeWith
    (rmId [0])
    "-42"
    [ iut 1 "rdf:type" "morloc:integer" "-42" ]

  testRdfCodeWith
    (rmId [0])
    "4.2"
    [ iut 1 "rdf:type" "morloc:number" "4.2" ]

  testRdfCodeWith
    (rmId [0])
    "True"
    [ iut 1 "rdf:type" "morloc:boolean" "True" ]

  testRdfCodeWith
    (rmId [0])
    "[42,99]"
    [ iuu 1 "rdf:type" "morloc:list"
    , iui 1 "morloc:contains_0" 2
    , iut 2 "rdf:type" "morloc:integer" "42"
    , iui 1 "morloc:contains_1" 3
    , iut 3 "rdf:type" "morloc:integer" "99"
    ]

  testRdfCodeWith
    (rmId [0])
    "[42,\"foo\"]"
    [ iuu 1 "rdf:type" "morloc:list"
    , iui 1 "morloc:contains_0" 2
    , iut 2 "rdf:type" "morloc:integer" "42"
    , iui 1 "morloc:contains_1" 3
    , iut 3 "rdf:type" "morloc:string" "foo"
    ]

  testRdfCodeWith
    (rmId [0])
    "{job = \"poopsmith\", age = 34}"
    [ iuu 1 "rdf:type" "morloc:record"
    , iui 1 "morloc:contains_0" 2
    , iuu 2 "rdf:type" "morloc:recordEntry"
    , iut 2 "morloc:lhs" "morloc:name" "job"
    , iui 2 "morloc:rhs" 3
    , iut 3 "rdf:type" "morloc:string" "poopsmith"
    , iui 1 "morloc:contains_1" 4
    , iuu 4 "rdf:type" "morloc:recordEntry"
    , iut 4 "morloc:lhs" "morloc:name" "age"
    , iui 4 "morloc:rhs" 5
    , iut 5 "rdf:type" "morloc:integer" "34"
    ]


  testRdfCodeWith
    (rmId [0])
    "A :: Bool"
    [ iuu 1 "rdf:type" "morloc:typeDeclaration"
    , iui 1 "morloc:lhs" 2
    , iut 2 "rdf:type" "morloc:name" "A"
    , iui 1 "morloc:rhs" 3
    , iut 3 "rdf:type" "morloc:atomicType" "Bool"
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: [Bool]"
    [ iuu 1 "rdf:type" "morloc:typeDeclaration"
    , iui 1 "morloc:lhs" 2
    , iut 2 "rdf:type" "morloc:name" "A"
    , iui 1 "morloc:rhs" 3
    , iut 3 "rdf:type" "morloc:parameterizedType" "List"
    , iui 3 "morloc:parameter_0" 4
    , iut 4 "rdf:type" "morloc:atomicType" "Bool"
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: (Bool, Fool)"
    [ iuu 1 "rdf:type" "morloc:typeDeclaration"
    , iui 1 "morloc:lhs" 2
    , iut 2 "rdf:type" "morloc:name" "A"
    , iui 1 "morloc:rhs" 3
    , iut 3 "rdf:type" "morloc:parameterizedType" "Tuple"
    , iui 3 "morloc:parameter_0" 4
    , iut 4 "rdf:type" "morloc:atomicType" "Bool"
    , iui 3 "morloc:parameter_1" 5
    , iut 5 "rdf:type" "morloc:atomicType" "Fool"
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: {B :: Bool, C :: Fool}"
    [ iuu 1 "rdf:type" "morloc:typeDeclaration"
    , iui 1 "morloc:lhs" 2
    , iut 2 "rdf:type" "morloc:name" "A"
    , iui 1 "morloc:rhs" 3
    , iut 3 "rdf:type" "morloc:parameterizedType" "Record"
    , iui 3 "morloc:parameter_0" 4
    , iuu 4 "rdf:type" "morloc:namedType"
    , iut 4 "morloc:key" "morloc:name" "B"
    , iui 4 "morloc:value" 5
    , iut 5 "rdf:type" "morloc:atomicType" "Bool"
    , iui 3 "morloc:parameter_1" 6
    , iuu 6 "rdf:type" "morloc:namedType"
    , iut 6 "morloc:key" "morloc:name" "C"
    , iui 6 "morloc:value" 7
    , iut 7 "rdf:type" "morloc:atomicType" "Fool"
    ]

  testRdfCode
    "x = 1"
    [ iuu 0 "rdf:type" "morloc:script"
    , iui 0 "morloc:child_0" 1
    , iuu 1 "rdf:type" "morloc:dataDeclaration"
    , iui 1 "morloc:lhs" 2
    , iut 2 "rdf:type" "morloc:name" "x"
    , iui 1 "morloc:rhs" 3
    , iut 3 "rdf:type" "morloc:integer" "1"
    ]

  testRdfCode
    "f x = x"
    [ iuu 0 "rdf:type" "morloc:script"
    , iui 0 "morloc:child_0" 1
    , iuu 1 "rdf:type" "morloc:dataDeclaration"
    , iui 1 "morloc:lhs" 2
    , iut 2 "rdf:type" "morloc:name" "f"
    , iui 1 "morloc:parameter_0" 3
    , iut 3 "rdf:type" "morloc:name" "x"
    , iui 1 "morloc:rhs" 4
    , iut 4 "rdf:type" "morloc:name" "x"
    ]

  testRdfCode
    "f = g 42 66"
    [ iuu 0 "rdf:type" "morloc:script"
    , iui 0 "morloc:child_0" 1
    , iuu 1 "rdf:type" "morloc:dataDeclaration"
    , iui 1 "morloc:lhs" 2
    , iut 2 "rdf:type" "morloc:name" "f"
    , iui 1 "morloc:rhs" 3
    , iuu 3 "rdf:type" "morloc:call"
    , iui 3 "morloc:value" 4
    , iut 4 "rdf:type" "morloc:name" "g"
    , iui 3 "morloc:argument_0" 5
    , iut 5 "rdf:type" "morloc:integer" "42"
    , iui 3 "morloc:argument_1" 6
    , iut 6 "rdf:type" "morloc:integer" "66"
    ]

  testRdfCodeWith
    (rmId [0])
    "(1, \"foo\", 1.1)"
    [ iuu 1 "rdf:type" "morloc:tuple"
    , iui 1 "morloc:contains_0" 2
    , iut 2 "rdf:type" "morloc:integer" "1"
    , iui 1 "morloc:contains_1" 3
    , iut 3 "rdf:type" "morloc:string" "foo"
    , iui 1 "morloc:contains_2" 4
    , iut 4 "rdf:type" "morloc:number" "1.1"
    ]

  testRdfCodeWith
    (rmId [0..3])
    "foo :: i:Int -> j:[A]"
    [ iut 4 "rdf:type" "morloc:atomicType" "Int"
    , iut 4 "morloc:label" "morloc:name" "i"
    , iut 5 "rdf:type" "morloc:parameterizedType" "List"
    , iut 5 "morloc:label" "morloc:name" "j"
    , iui 5 "morloc:parameter_0" 6
    , iut 6 "rdf:type" "morloc:atomicType" "A"
    ]

  testRdfCodeWith
    (rmId [0..2])
    "foo :: i:Int -> Num where (i > 0)"
    [ iuu 3 "rdf:type" "morloc:functionType"
    , iui 3 "morloc:input_0" 4
    , iut 4 "rdf:type" "morloc:atomicType" "Int"
    , iut 4 "morloc:label" "morloc:name" "i"
    , iui 3 "morloc:output" 5
    , iut 5 "rdf:type" "morloc:atomicType" "Num"
    , iui 3 "morloc:constraint_0" 6
    , iut 6 "rdf:type" "morloc:binop" "GT"
    , iui 6 "morloc:lhs" 7
    , iui 6 "morloc:rhs" 8
    , iut 7 "rdf:type" "morloc:name" "i"
    , iut 8 "rdf:type" "morloc:integer" "0"
    ]

  testRdfCodeWith
    (rmId [0])
    "foo :: Int"
    [ iuu 1 "rdf:type" "morloc:typeDeclaration"
    , iui 1 "morloc:lhs" 2
    , iut 2 "rdf:type" "morloc:name" "foo"
    , iui 1 "morloc:rhs" 3
    , iut 3 "rdf:type" "morloc:atomicType" "Int"
    ]

  testRdfCodeWith
    (rmId [0..5])
    "foo :: X -> Y where (1.1 + 1.2 > 2.0)"
    [ iut 6 "rdf:type" "morloc:binop" "GT"
    , iui 6 "morloc:lhs" 8
    , iui 6 "morloc:rhs" 10
    , iut 8 "rdf:type" "morloc:binop" "Add"
    , iui 8 "morloc:lhs" 7
    , iui 8 "morloc:rhs" 9
    , iut 7 "rdf:type" "morloc:number" "1.1"
    , iut 9 "rdf:type" "morloc:number" "1.2"
    , iut 10 "rdf:type" "morloc:number" "2.0"
    ]

  testRdfCodeWith
    (rmId [0])
    "foo :: a, (b -> c) -> d"
    [ iuu 1 "rdf:type" "morloc:typeDeclaration"
    , iui 1 "morloc:lhs" 2
    , iut 2 "rdf:type" "morloc:name" "foo"
    , iui 1 "morloc:rhs" 3
    , iuu 3 "rdf:type" "morloc:functionType"
    , iui 3 "morloc:input_0" 4
    , iut 4 "rdf:type" "morloc:atomicGeneric" "a"
    , iui 3 "morloc:input_1" 5
    , iuu 5 "rdf:type" "morloc:functionType"
    , iui 5 "morloc:input_0" 6
    , iut 6 "rdf:type" "morloc:atomicGeneric" "b"
    , iui 5 "morloc:output" 7
    , iut 7 "rdf:type" "morloc:atomicGeneric" "c"
    , iui 3 "morloc:output" 8
    , iut 8 "rdf:type" "morloc:atomicGeneric" "d"
    ]

  testRdfCodeWith
    (rmId [0..2])
    "foo :: A B -> C D"
    [ iuu 3 "rdf:type" "morloc:functionType"
    , iui 3 "morloc:input_0" 4
    , iut 4 "rdf:type" "morloc:parameterizedType" "A"
    , iui 4 "morloc:parameter_0" 5
    , iut 5 "rdf:type" "morloc:atomicType" "B"
    , iui 3 "morloc:output" 6
    , iut 6 "rdf:type" "morloc:parameterizedType" "C"
    , iui 6 "morloc:parameter_0" 7
    , iut 7 "rdf:type" "morloc:atomicType" "D"
    ]

  testRdfCodeWith
    (rmId [0..3])
    "foo :: A where ((1 == 1) and (2 == 2))"
    [ iut 4 "rdf:type" "morloc:binop" "and"
    , iui 4 "morloc:lhs" 5
    , iui 4 "morloc:rhs" 8
    , iut 5 "rdf:type" "morloc:binop" "EQ"
    , iui 5 "morloc:lhs" 6
    , iui 5 "morloc:rhs" 7
    , iut 6 "rdf:type" "morloc:integer" "1"
    , iut 7 "rdf:type" "morloc:integer" "1"
    , iut 8 "rdf:type" "morloc:binop" "EQ"
    , iui 8 "morloc:lhs" 9
    , iui 8 "morloc:rhs" 10
    , iut 9 "rdf:type" "morloc:integer" "2"
    , iut 10 "rdf:type" "morloc:integer" "2"
    ]

  testRdfCodeWith
    (rmId [0])
    "f . g"
    [ iuu 2 "rdf:type" "morloc:composition"
    , iui 2 "morloc:lhs" 1
    , iui 2 "morloc:rhs" 3
    , iut 1 "rdf:type" "morloc:name" "f"
    , iut 3 "rdf:type" "morloc:name" "g"
    ]

  testRdfCodeWith
    (rmId ([0..6] ++ [9]))
    -- this will fail later, since x,k, and t are undefined.
    "X :: Y where (x^(-k) == 1)"
    [ iut 7 "rdf:type" "morloc:unaryOp" "Neg"
    , iui 7 "morloc:contains_0" 8
    , iut 8 "rdf:type" "morloc:name" "k"
    ]

  testRdfCodeWith
    (rmId [0..3])
    -- this will fail later, since x,k, and t are undefined.
    "X :: Y where (f x (g y z))"
    [ iuu 4 "rdf:type" "morloc:call"
    , iut 4 "morloc:value" "morloc:name" "f"
    , iui 4 "morloc:argument_0" 5
    , iut 5 "rdf:type" "morloc:name" "x"
    , iui 4 "morloc:argument_1" 6
    , iuu 6 "rdf:type" "morloc:call"
    , iut 6 "morloc:value" "morloc:name" "g"
    , iui 6 "morloc:argument_0" 7
    , iut 7 "rdf:type" "morloc:name" "y"
    , iui 6 "morloc:argument_1" 8
    , iut 8 "rdf:type" "morloc:name" "z"
    ]

  testRdfCodeWith
    (rmId ([0..4] ++ [8]))
    -- this will fail later, since x,k, and t are undefined.
    "X :: Y where (f x y == 1)"
    [ iuu 5 "rdf:type" "morloc:call"
    , iut 5 "morloc:value" "morloc:name" "f"
    , iui 5 "morloc:argument_0" 6
    , iut 6 "rdf:type" "morloc:name" "x"
    , iui 5 "morloc:argument_1" 7
    , iut 7 "rdf:type" "morloc:name" "y"
    ]
