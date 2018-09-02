{-# LANGUAGE OverloadedStrings #-}

module ParserTests (testParser) where

import Test.Tasty.Hspec
import qualified Data.RDF as DR
import qualified Data.Text as DT

import qualified Morloc.Triple as M3
import qualified Morloc.Parser as MP
import Morloc.Operators
import Morloc.Util (show')

uri :: Int -> DR.Node
uri = M3.idUri Nothing

rmId :: [Int] -> [DR.Triple] -> [DR.Triple]
rmId is ts = filter (rmId' is) ts
  where
    rmId' :: [Int] -> DR.Triple -> Bool 
    rmId' is' (DR.Triple s _ _) = all (\x -> uri x /= s) is'

testRdfCodeWith :: ([DR.Triple] -> [DR.Triple]) -> DT.Text -> [DR.Triple] -> Spec 
testRdfCodeWith f s ts = case (run' f s) of
  (Right ts') -> it (DT.unpack s) $
    do
      shouldBe
        (map (DR.expandTriple M3.prefixMap) (DR.uordered ts))
        (DR.uordered ts')
  (Left err) -> error (unlines ["Failure in:", DT.unpack s, ">>>" ++ show err])
  where
    run' f' s' = fmap (mapTriples f') (MP.parseShallow Nothing (s' <> ";"))
    mapTriples f' rdf = f' (DR.triplesOf rdf)

testRdfCode :: DT.Text -> [DR.Triple] -> Spec
testRdfCode = testRdfCodeWith id

-- triple making convenience functions
isu i r o = DR.triple (M3.midPre .:. show' i) (DR.UNode r) o
iss i r o = DR.triple (M3.midPre .:. show' i) (DR.UNode r) (DR.UNode o)
isi i r j = DR.triple (M3.midPre .:. show' i) (DR.UNode r) (M3.midPre .:. show' j)

-- a little helper function for making plain nodes
plain :: DT.Text -> DR.Node
plain s = DR.LNode (DR.PlainL s)

testParser :: Spec
testParser = parallel $ do

  testRdfCodeWith
    (rmId [0])
    "42"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:number"
    , iss 1 "rdf:type" "mlc:data"
    , isu 1 "rdf:value" ("42.0" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCodeWith
    (rmId [0])
    "-42"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:number"
    , iss 1 "rdf:type" "mlc:data"
    , isu 1 "rdf:value" ("-42.0" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCodeWith
    (rmId [0])
    "4.2"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:number"
    , iss 1 "rdf:type" "mlc:data"
    , isu 1 "rdf:value" ("4.2" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCodeWith
    (rmId [0])
    "True"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:boolean"
    , iss 1 "rdf:type" "mlc:data"
    , isu 1 "rdf:value" ("True" .^^. M3.xsdPre .:. "boolean")
    ]

  testRdfCodeWith
    (rmId [0])
    "[42,99]"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:list"
    , iss 1 "rdf:type" "mlc:data"
    , isi 2 "rdf:_0" 1
    , iss 2 "rdf:type" "mlc:number"
    , iss 2 "rdf:type" "mlc:data"
    , isu 2 "rdf:value" ("42.0" .^^. M3.xsdPre .:. "decimal")
    , isi 3 "rdf:_1" 1
    , iss 3 "rdf:type" "mlc:number"
    , iss 3 "rdf:type" "mlc:data"
    , isu 3 "rdf:value" ("99.0" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCodeWith
    (rmId [0])
    "[42,\"foo\"]"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:list"
    , iss 1 "rdf:type" "mlc:data"
    , isi 2 "rdf:_0" 1
    , iss 2 "rdf:type" "mlc:number"
    , iss 2 "rdf:type" "mlc:data"
    , isu 2 "rdf:value" ("42.0" .^^. M3.xsdPre .:. "decimal")
    , isi 3 "rdf:_1" 1
    , iss 3 "rdf:type" "mlc:string"
    , iss 3 "rdf:type" "mlc:data"
    , isu 3 "rdf:value" ("foo" .^^. M3.xsdPre .:. "string")
    ]

  testRdfCodeWith
    (rmId [0])
    "{job = \"poopsmith\", age = 34}"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:record"
    , iss 1 "rdf:type" "mlc:data"
    , isi 2 "rdf:_0" 1
    , iss 2 "rdf:type" "mlc:recordEntry"
    , isu 2 "mlc:lhs" (plain "job")
    , isi 2 "mlc:rhs" 3
    , iss 3 "rdf:type" "mlc:string"
    , iss 3 "rdf:type" "mlc:data"
    , isu 3 "rdf:value" ("poopsmith" .^^. M3.xsdPre .:. "string")
    , isi 4 "rdf:_1" 1
    , iss 4 "rdf:type" "mlc:recordEntry"
    , isu 4 "mlc:lhs" (plain "age")
    , isi 4 "mlc:rhs" 5
    , iss 5 "rdf:type" "mlc:number"
    , iss 5 "rdf:type" "mlc:data"
    , isu 5 "rdf:value" ("34.0" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCode
    "source \"R\" (\"fo.o\" as foo)"
    [ iss 0 "rdf:type" "mlc:script"
    , isu 0 "rdf:value" (plain "<stdin>")
    , isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:source"
    , isu 1 "mlc:lang" (plain "R")
    , isi 1 "mlc:import" 2
    , isu 2 "mlc:name" (plain "fo.o")
    , isu 2 "mlc:alias" (plain "foo")
    ]

  testRdfCodeWith
    (rmId [0])
    "from \"bob/foo\" import (bar, baz)"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:restricted_import"
    , isu 1 "mlc:name" (plain "bob/foo.loc")
    , isi 1 "mlc:import" 2
    , iss 2 "rdf:type" "mlc:name"
    , isu 2 "rdf:value" (plain "bar")
    , isi 1 "mlc:import" 3
    , iss 3 "rdf:type" "mlc:name"
    , isu 3 "rdf:value" (plain "baz")
    ]

  testRdfCodeWith
    (rmId [0])
    "import \"bob/foo\" as foo"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:import"
    , isu 1 "mlc:name" (plain "bob/foo.loc")
    , isu 1 "mlc:namespace" (plain "foo")
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: Bool"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:typeDeclaration"
    , isu 1 "mlc:lang" (plain "Morloc")
    , isu 1 "mlc:lhs" (plain "A")
    , isi 1 "mlc:rhs" 2
    , iss 2 "rdf:type" "mlc:atomicType"
    , iss 2 "rdf:type" "mlc:type"
    , isu 2 "rdf:value" (plain "Bool")
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: [Bool]"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:typeDeclaration"
    , isu 1 "mlc:lang" (plain "Morloc")
    , isu 1 "mlc:lhs" (plain "A")
    , isi 1 "mlc:rhs" 2
    , iss 2 "rdf:type" "mlc:parameterizedType"
    , iss 2 "rdf:type" "mlc:type"
    , isu 2 "rdf:value" (plain "List")
    , isi 3 "rdf:_0" 2
    , iss 3 "rdf:type" "mlc:atomicType"
    , iss 3 "rdf:type" "mlc:type"
    , isu 3 "rdf:value" (plain "Bool")
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: (Bool, Fool)"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:typeDeclaration"
    , isu 1 "mlc:lang" (plain "Morloc")
    , isu 1 "mlc:lhs" (plain "A")
    , isi 1 "mlc:rhs" 2
    , iss 2 "rdf:type" "mlc:parameterizedType"
    , iss 2 "rdf:type" "mlc:type"
    , isu 2 "rdf:value" (plain "Tuple")
    , isi 3 "rdf:_0" 2
    , iss 3 "rdf:type" "mlc:atomicType"
    , iss 3 "rdf:type" "mlc:type"
    , isu 3 "rdf:value" (plain "Bool")
    , isi 4 "rdf:_1" 2
    , iss 4 "rdf:type" "mlc:atomicType"
    , iss 4 "rdf:type" "mlc:type"
    , isu 4 "rdf:value" (plain "Fool")
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: {B :: Bool, C :: Fool}"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:typeDeclaration"
    , isu 1 "mlc:lang" (plain "Morloc")
    , isu 1 "mlc:lhs" (plain "A")
    , isi 1 "mlc:rhs" 2
    , iss 2 "rdf:type" "mlc:parameterizedType"
    , iss 2 "rdf:type" "mlc:type"
    , isu 2 "rdf:value" (plain "Record")
    , isi 3 "rdf:_0" 2
    , iss 3 "rdf:type" "mlc:namedType"
    , isu 3 "mlc:key" (plain "B")
    , isi 3 "rdf:value" 4
    , iss 4 "rdf:type" "mlc:atomicType"
    , iss 4 "rdf:type" "mlc:type"
    , isu 4 "rdf:value" (plain "Bool")
    , isi 5 "rdf:_1" 2
    , iss 5 "rdf:type" "mlc:namedType"
    , isu 5 "mlc:key" (plain "C")
    , isi 5 "rdf:value" 6
    , iss 6 "rdf:type" "mlc:atomicType"
    , iss 6 "rdf:type" "mlc:type"
    , isu 6 "rdf:value" (plain "Fool")
    ]

  -- TODO: resolve to simple calls
  --       instead I should test RDF equality of: (g . f) x == g (f x)
  testRdfCodeWith
    (rmId [0])
    "(g . f) x"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:call"
    , isi 1 "rdf:value" 3
    , iss 3 "rdf:type" "mlc:composition"
    , isi 3 "mlc:lhs" 2
    , isi 3 "mlc:rhs" 4
    , iss 2 "rdf:type" "mlc:name"
    , isu 2 "rdf:value" (plain "g")
    , iss 4 "rdf:type" "mlc:name"
    , isu 4 "rdf:value" (plain "f")
    , isi 5 "rdf:_0" 1
    , iss 5 "rdf:type" "mlc:name"
    , isu 5 "rdf:value" (plain "x")
    ]

  testRdfCodeWith
    (rmId [0])
    "x = 1"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:dataDeclaration"
    , isu 1 "mlc:lhs" (plain "x")
    , isi 1 "mlc:rhs" 2
    , iss 2 "rdf:type" "mlc:number"
    , iss 2 "rdf:type" "mlc:data"
    , isu 2 "rdf:value" ("1.0" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCodeWith
    (rmId [0])
    "f x = x"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:dataDeclaration"
    , isu 1 "mlc:lhs" (plain "f")
    , isi 2 "rdf:_0" 1
    , iss 2 "rdf:type" "mlc:name"
    , isu 2 "rdf:value" (plain "x")
    , isi 1 "mlc:rhs" 3
    , iss 3 "rdf:type" "mlc:name"
    , isu 3 "rdf:value" (plain "x")
    ]

  testRdfCodeWith
    (rmId [0])
    "f = g 42 66"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:dataDeclaration"
    , isu 1 "mlc:lhs" (plain "f")
    , isi 1 "mlc:rhs" 2
    , iss 2 "rdf:type" "mlc:call"
    , isi 2 "rdf:value" 3
    , iss 3 "rdf:type" "mlc:name"
    , isu 3 "rdf:value" (plain "g")
    , isi 4 "rdf:_0" 2
    , iss 4 "rdf:type" "mlc:number"
    , iss 4 "rdf:type" "mlc:data"
    , isu 4 "rdf:value" ("42.0" .^^. M3.xsdPre .:. "decimal")
    , isi 5 "rdf:_1" 2
    , iss 5 "rdf:type" "mlc:number"
    , iss 5 "rdf:type" "mlc:data"
    , isu 5 "rdf:value" ("66.0" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCodeWith
    (rmId [0])
    "(1, \"foo\", 1.1)"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:tuple"
    , iss 1 "rdf:type" "mlc:data"
    , isi 2 "rdf:_0" 1
    , iss 2 "rdf:type" "mlc:number"
    , iss 2 "rdf:type" "mlc:data"
    , isu 2 "rdf:value" ("1.0" .^^. M3.xsdPre .:. "decimal")
    , isi 3 "rdf:_1" 1
    , iss 3 "rdf:type" "mlc:string"
    , iss 3 "rdf:type" "mlc:data"
    , isu 3 "rdf:value" ("foo" .^^. M3.xsdPre .:. "string")
    , isi 4 "rdf:_2" 1
    , iss 4 "rdf:type" "mlc:number"
    , iss 4 "rdf:type" "mlc:data"
    , isu 4 "rdf:value" ("1.1" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCodeWith
    (rmId [0..2])
    "foo :: i:Int -> j:[A]"
    [ isi 3 "rdf:_0" 2
    , iss 3 "rdf:type" "mlc:atomicType"
    , iss 3 "rdf:type" "mlc:type"
    , isu 3 "rdf:value" (plain "Int")
    , isu 3 "mlc:label" (plain "i")
    , iss 4 "rdf:type" "mlc:parameterizedType"
    , iss 4 "rdf:type" "mlc:type"
    , isu 4 "rdf:value" (plain "List")
    , isu 4 "mlc:label" (plain "j")
    , isi 5 "rdf:_0" 4
    , iss 5 "rdf:type" "mlc:atomicType"
    , iss 5 "rdf:type" "mlc:type"
    , isu 5 "rdf:value" (plain "A")
    ]

  testRdfCodeWith
    (rmId [0..1])
    "foo :: i:Int -> Num where (i > 0)"
    [ iss 2 "rdf:type" "mlc:functionType"
    , iss 2 "rdf:type" "mlc:type"
    , isi 3 "rdf:_0" 2
    , iss 3 "rdf:type" "mlc:atomicType"
    , iss 3 "rdf:type" "mlc:type"
    , isu 3 "rdf:value" (plain "Int")
    , isu 3 "mlc:label" (plain "i")
    , isi 2 "mlc:output" 4
    , iss 4 "rdf:type" "mlc:atomicType"
    , iss 4 "rdf:type" "mlc:type"
    , isu 4 "rdf:value" (plain "Num")
    , isi 2 "mlc:constraint" 5
    , iss 5 "rdf:type" "mlc:binop"
    , isu 5 "rdf:value" (plain "GT")
    , isi 5 "mlc:lhs" 6
    , isi 5 "mlc:rhs" 7
    , iss 6 "rdf:type" "mlc:name"
    , isu 6 "rdf:value" (plain "i")
    , iss 7 "rdf:type" "mlc:number"
    , iss 7 "rdf:type" "mlc:data"
    , isu 7 "rdf:value" ("0.0" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCodeWith
    (rmId [0])
    "foo :: Int"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:typeDeclaration"
    , isu 1 "mlc:lang" (plain "Morloc")
    , isu 1 "mlc:lhs" (plain "foo")
    , isi 1 "mlc:rhs" 2
    , iss 2 "rdf:type" "mlc:atomicType"
    , iss 2 "rdf:type" "mlc:type"
    , isu 2 "rdf:value" (plain "Int")
    ]

  testRdfCodeWith
    (rmId [0..4])
    "foo :: X -> Y where (1.1 + 1.2 > 2.0)"
    [ iss 5 "rdf:type" "mlc:binop"
    , isu 5 "rdf:value" (plain "GT")
    , isi 5 "mlc:lhs" 7
    , isi 5 "mlc:rhs" 9
    , iss 7 "rdf:type" "mlc:binop"
    , isu 7 "rdf:value" (plain "Add")
    , isi 7 "mlc:lhs" 6
    , isi 7 "mlc:rhs" 8
    , iss 6 "rdf:type" "mlc:number"
    , iss 6 "rdf:type" "mlc:data"
    , isu 6 "rdf:value" ("1.1" .^^. M3.xsdPre .:. "decimal")
    , iss 8 "rdf:type" "mlc:number"
    , iss 8 "rdf:type" "mlc:data"
    , isu 8 "rdf:value" ("1.2" .^^. M3.xsdPre .:. "decimal")
    , iss 9 "rdf:type" "mlc:number"
    , iss 9 "rdf:type" "mlc:data"
    , isu 9 "rdf:value" ("2.0" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCodeWith
    (rmId [0])
    "foo :: a, (b -> c) -> d"
    [ isi 1 "rdf:_0" 0
    , iss 1 "rdf:type" "mlc:typeDeclaration"
    , isu 1 "mlc:lang" (plain "Morloc")
    , isu 1 "mlc:lhs" (plain "foo")
    , isi 1 "mlc:rhs" 2
    , iss 2 "rdf:type" "mlc:functionType"
    , iss 2 "rdf:type" "mlc:type"
    , isi 3 "rdf:_0" 2
    , iss 3 "rdf:type" "mlc:atomicGeneric"
    , iss 3 "rdf:type" "mlc:type"
    , isu 3 "rdf:value" (plain "a")
    , isi 4 "rdf:_1" 2
    , iss 4 "rdf:type" "mlc:functionType"
    , iss 4 "rdf:type" "mlc:type"
    , isi 5 "rdf:_0" 4
    , iss 5 "rdf:type" "mlc:atomicGeneric"
    , iss 5 "rdf:type" "mlc:type"
    , isu 5 "rdf:value" (plain "b")
    , isi 4 "mlc:output" 6
    , iss 6 "rdf:type" "mlc:atomicGeneric"
    , iss 6 "rdf:type" "mlc:type"
    , isu 6 "rdf:value" (plain "c")
    , isi 2 "mlc:output" 7
    , iss 7 "rdf:type" "mlc:atomicGeneric"
    , iss 7 "rdf:type" "mlc:type"
    , isu 7 "rdf:value" (plain "d")
    ]

  testRdfCodeWith
    (rmId [0..1])
    "foo :: A B -> C D"
    [ iss 2 "rdf:type" "mlc:functionType"
    , iss 2 "rdf:type" "mlc:type"
    , isi 3 "rdf:_0" 2
    , iss 3 "rdf:type" "mlc:parameterizedType"
    , iss 3 "rdf:type" "mlc:type"
    , isu 3 "rdf:value" (plain "A")
    , isi 4 "rdf:_0" 3
    , iss 4 "rdf:type" "mlc:atomicType"
    , iss 4 "rdf:type" "mlc:type"
    , isu 4 "rdf:value" (plain "B")
    , isi 2 "mlc:output" 5
    , iss 5 "rdf:type" "mlc:parameterizedType"
    , iss 5 "rdf:type" "mlc:type"
    , isu 5 "rdf:value" (plain "C")
    , isi 6 "rdf:_0" 5
    , iss 6 "rdf:type" "mlc:atomicType"
    , iss 6 "rdf:type" "mlc:type"
    , isu 6 "rdf:value" (plain "D")
    ]

  testRdfCodeWith
    (rmId [0..1])
    "foo :: A where ((1 == 1) and (2 == 2))"
    [ iss 2 "rdf:type" "mlc:atomicType"
    , iss 2 "rdf:type" "mlc:type"
    , isu 2 "rdf:value" (plain "A")
    , isi 2 "mlc:constraint" 3
    , iss 3 "rdf:type" "mlc:binop"
    , isu 3 "rdf:value" (plain "and")
    , isi 3 "mlc:lhs" 4
    , isi 3 "mlc:rhs" 7
    , iss 4 "rdf:type" "mlc:binop"
    , isu 4 "rdf:value" (plain "EQ")
    , isi 4 "mlc:lhs" 5
    , isi 4 "mlc:rhs" 6
    , iss 5 "rdf:type" "mlc:number"
    , iss 5 "rdf:type" "mlc:data"
    , isu 5 "rdf:value" ("1.0" .^^. M3.xsdPre .:. "decimal")
    , iss 6 "rdf:type" "mlc:number"
    , iss 6 "rdf:type" "mlc:data"
    , isu 6 "rdf:value" ("1.0" .^^. M3.xsdPre .:. "decimal")
    , iss 7 "rdf:type" "mlc:binop"
    , isu 7 "rdf:value" (plain "EQ")
    , isi 7 "mlc:lhs" 8
    , isi 7 "mlc:rhs" 9
    , iss 8 "rdf:type" "mlc:number"
    , iss 8 "rdf:type" "mlc:data"
    , isu 8 "rdf:value" ("2.0" .^^. M3.xsdPre .:. "decimal")
    , iss 9 "rdf:type" "mlc:number"
    , iss 9 "rdf:type" "mlc:data"
    , isu 9 "rdf:value" ("2.0" .^^. M3.xsdPre .:. "decimal")
    ]

  testRdfCodeWith
    (rmId [0])
    "f . g"
    [ isi 2 "rdf:_0" 0
    , iss 2 "rdf:type" "mlc:composition"
    , isi 2 "mlc:lhs" 1
    , isi 2 "mlc:rhs" 3
    , iss 1 "rdf:type" "mlc:name"
    , isu 1 "rdf:value" (plain "f")
    , iss 3 "rdf:type" "mlc:name"
    , isu 3 "rdf:value" (plain "g")
    ]

  testRdfCodeWith
    (rmId ([0..5] ++ [8]))
    -- this will fail later, since x,k, and t are undefined.
    "X :: Y where (x^(-k) == 1)"
    [ iss 6 "rdf:type" "mlc:unaryOp"
    , isu 6 "rdf:value" (plain "Neg")
    , isi 7 "rdf:_0" 6
    , iss 7 "rdf:type" "mlc:name"
    , isu 7 "rdf:value" (plain "k")
    ]

  testRdfCodeWith
    (rmId [0..2])
    -- this will fail later, since x,k, and t are undefined.
    "X :: Y where (f x (g y z))"
    [ iss 3 "rdf:type" "mlc:call"
    , isi 3 "rdf:value" 4
    , iss 4 "rdf:type" "mlc:name"
    , isu 4 "rdf:value" (plain "f")
    , isi 5 "rdf:_0" 3
    , iss 5 "rdf:type" "mlc:name"
    , isu 5 "rdf:value" (plain "x")
    , isi 6 "rdf:_1" 3
    , iss 6 "rdf:type" "mlc:call"
    , isi 6 "rdf:value" 7
    , iss 7 "rdf:type" "mlc:name"
    , isu 7 "rdf:value" (plain "g")
    , isi 8 "rdf:_0" 6
    , iss 8 "rdf:type" "mlc:name"
    , isu 8 "rdf:value" (plain "y")
    , isi 9 "rdf:_1" 6
    , iss 9 "rdf:type" "mlc:name"
    , isu 9 "rdf:value" (plain "z")
    ]

  testRdfCodeWith
    (rmId ([0..3] ++ [7]))
    -- this will fail later, since x,k, and t are undefined.
    "X :: Y where (f x y == 1)"
    [ iss 4 "rdf:type" "mlc:call"
    , isi 4 "rdf:value" 5
    , iss 5 "rdf:type" "mlc:name"
    , isu 5 "rdf:value" (plain "f")
    , isi 6 "rdf:_0" 4
    , iss 6 "rdf:type" "mlc:name"
    , isu 6 "rdf:value" (plain "x")
    , iss 8 "rdf:type" "mlc:number"
    , iss 8 "rdf:type" "mlc:data"
    , isu 8 "rdf:value" ("1.0" .^^. M3.xsdPre .:. "decimal")
    ]
