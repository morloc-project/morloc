import qualified Test.Tasty
import Test.Tasty.Hspec

import Morloc.Parser (morlocScript)
import Morloc.Triple

main :: IO ()
main = do
  test <- testSpec "morloc" spec
  Test.Tasty.defaultMain test

rmId :: [Int] -> [Triple] -> [Triple]
rmId is ts = filter (rmId' is) ts where
  rmId' :: [Int] -> Triple -> Bool 
  rmId' is' (i, _, _) = all (\x -> x /= i) is'

testRdfCodeWith :: ([Triple] -> [Triple]) -> String -> [Triple] -> Spec 
testRdfCodeWith f s ts = case (run' f s) of
  (Right ts') -> it s $ do shouldBe ts ts'
  (Left err) -> error (unlines ["Failure in:", s, ">>>" ++ show err])
  where
    run' f' s' = (fmap (\(RDF _ ts') -> f' ts') (morlocScript (s' ++ ";")))

testRdfCode :: String -> [Triple] -> Spec
testRdfCode = testRdfCodeWith id


spec :: Spec
spec = parallel $ do

  testRdfCode
    "source \"R\" (\"fo.o\" as foo)"
    [ (0, ":isa",    Str' ":script" )
    , (0, ":child",  Id'  1         )
    , (1, ":isa",    Str' ":source" )
    , (1, ":lang",   Str' "R"       )
    , (1, ":import", Id'  2         )
    , (2, ":name",   Str' "fo.o"    )
    , (2, ":alias",  Str' "foo"     )
    ]

  testRdfCode
    "from bob/foo import (bar, baz)"
    [ (0, ":isa",    Str' ":script"            )
    , (0, ":child",  Id'  1                    )
    , (1, ":isa",    Str' ":restricted_import" )
    , (1, ":name",   Str' "bob.foo"            )
    , (1, ":import", Id'  2                    )
    , (2, ":isa",    Str' ":name"              )
    , (2, ":value",  Str' "bar"                )
    , (1, ":import", Id'  3                    )
    , (3, ":isa",    Str' ":name"              )
    , (3, ":value",  Str' "baz"                )
    ]

  testRdfCode
    "import bob/foo as foo"
    [ (0, ":isa",       Str' ":script" )
    , (0, ":child",     Id'  1         )
    , (1, ":isa",       Str' ":import" )
    , (1, ":name",      Str' "bob.foo" )
    , (1, ":namespace", Str' "foo"     )
    ]

  testRdfCodeWith
    (rmId [0])
    "42"
    [ (1, ":isa",   Str' ":integer" )
    , (1, ":value", Int' 42         )
    ]

  testRdfCodeWith
    (rmId [0])
    "-42"
    [ (1, ":isa",   Str' ":integer" )
    , (1, ":value", Int' (-42)      )
    ]

  testRdfCodeWith
    (rmId [0])
    "4.2"
    [ (1, ":isa",   Str' ":number" )
    , (1, ":value", Num' 4.2       )
    ]

  testRdfCodeWith
    (rmId [0])
    "True"
    [ (1, ":isa",   Str' ":boolean" )
    , (1, ":value", Log' True       )
    ]

  testRdfCodeWith
    (rmId [0])
    "[42,99]"
    [ (1, ":isa",      Str' ":list"    )
    , (1, ":contains", Id'  2          )
    , (2, ":isa",      Str' ":integer" )
    , (2, ":value",    Int' 42         )
    , (1, ":contains", Id'  3          )
    , (3, ":isa",      Str' ":integer" )
    , (3, ":value",    Int' 99         )
    ]

  testRdfCodeWith
    (rmId [0])
    "[42,\"foo\"]"
    [ (1, ":isa",      Str' ":list"    )
    , (1, ":contains", Id'  2          )
    , (2, ":isa",      Str' ":integer" )
    , (2, ":value",    Int' 42         )
    , (1, ":contains", Id'  3          )
    , (3, ":isa",      Str' ":string"  )
    , (3, ":value",    Str' "foo"      )
    ]

  testRdfCodeWith
    (rmId [0])
    "{job = \"poopsmith\", age = 34}"
    [ (1, ":isa",      Str' ":record"     )
    , (1, ":contains", Id'  2             )
    , (2, ":isa",      Str' "recordEntry" )
    , (2, ":lhs",      Str' "job"         )
    , (2, ":rhs",      Id'  3             )
    , (3, ":isa",      Str' ":string"     )
    , (3, ":value",    Str' "poopsmith"   )
    , (1, ":contains", Id'  4             )
    , (4, ":isa",      Str' "recordEntry" )
    , (4, ":lhs",      Str' "age"         )
    , (4, ":rhs",      Id'  5             )
    , (5, ":isa",      Str' ":integer"    )
    , (5, ":value",    Int' 34            )
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: Bool"
    [ (1, ":isa", Str' ":typeDeclaration" )
    , (1, ":lhs", Id' 2 )
    , (2, ":isa", Str' ":name" )
    , (2, ":value", Str' "A" )
    , (1, ":rhs", Id' 3 )
    , (3, ":isa", Str' ":atomicType" )
    , (3, ":value", Str' "Bool" )
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: [Bool]"
    [ (1, ":isa", Str' ":typeDeclaration" )
    , (1, ":lhs", Id' 2 )
    , (2, ":isa", Str' ":name" )
    , (2, ":value", Str' "A" )
    , (1, ":rhs", Id' 3 )
    , (3, ":isa", Str' ":parameterizedType" )
    , (3, ":value", Str' "List" )
    , (3, ":parameter", Id' 4 )
    , (4, ":isa", Str' ":atomicType" )
    , (4, ":value", Str' "Bool" )
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: (Bool, Fool)"
    [ (1, ":isa", Str' ":typeDeclaration" )
    , (1, ":lhs", Id' 2 )
    , (2, ":isa", Str' ":name" )
    , (2, ":value", Str' "A" )
    , (1, ":rhs", Id' 3 )
    , (3, ":isa", Str' ":parameterizedType" )
    , (3, ":value", Str' "Tuple" )
    , (3, ":parameter", Id' 4 )
    , (4, ":isa", Str' ":atomicType" )
    , (4, ":value", Str' "Bool" )
    , (3, ":parameter", Id' 5 )
    , (5, ":isa", Str' ":atomicType" )
    , (5, ":value", Str' "Fool" )
    ]

  testRdfCodeWith
    (rmId [0])
    "A :: {B :: Bool, C :: Fool}"
    [ (1, ":isa", Str' ":typeDeclaration" )
    , (1, ":lhs", Id' 2 )
    , (2, ":isa", Str' ":name" )
    , (2, ":value", Str' "A" )
    , (1, ":rhs", Id' 3 )
    , (3, ":isa", Str' ":parameterizedType" )
    , (3, ":value", Str' "Record" )
    , (3, ":parameter", Id' 4 )
    , (4, ":isa", Str' ":namedType" )
    , (4, ":name", Str' "B" )
    , (4, ":value", Id' 5 )
    , (5, ":isa", Str' ":atomicType" )
    , (5, ":value", Str' "Bool" )
    , (3, ":parameter", Id' 6 )
    , (6, ":isa", Str' ":namedType" )
    , (6, ":name", Str' "C" )
    , (6, ":value", Id' 7 )
    , (7, ":isa", Str' ":atomicType" )
    , (7, ":value", Str' "Fool" )
    ]

  testRdfCode
    "x = 1"
    [ (0, ":isa",   Str' ":script"      )
    , (0, ":child", Id'  1              )
    , (1, ":isa",   Str' ":declaration" )
    , (1, ":lhs",   Id'  2              )
    , (2, ":isa",   Str' ":name"        )
    , (2, ":value", Str' "x"            )
    , (1, ":rhs",   Id'  3              )
    , (3, ":isa",   Str' ":integer"     )
    , (3, ":value", Int' 1              )
    ]

  testRdfCode
    "f x = x"
    [ (0, ":isa",       Str' ":script"      )
    , (0, ":child",     Id'  1              )
    , (1, ":isa",       Str' ":declaration" )
    , (1, ":lhs",       Id'  2              )
    , (2, ":isa",       Str' ":name"        )
    , (2, ":value",     Str' "f"            )
    , (1, ":parameter", Id'  3              )
    , (3, ":isa",       Str' ":name"        )
    , (3, ":value",     Str' "x"            )
    , (1, ":rhs",       Id'  4              )
    , (4, ":isa",       Str' ":name"        )
    , (4, ":value",     Str' "x"            )
    ]

  testRdfCode
    "f = g 42 66"
    [ (0, ":isa",      Str' ":script"      )
    , (0, ":child",    Id'  1              )
    , (1, ":isa",      Str' ":declaration" )
    , (1, ":lhs",      Id'  2              )
    , (2, ":isa",      Str' ":name"        )
    , (2, ":value",    Str' "f"            )
    , (1, ":rhs",      Id'  3              )
    , (3, ":isa",      Str' ":application" )
    , (3, ":function", Id'  4              )
    , (4, ":isa",      Str' ":name"        )
    , (4, ":value",    Str' "g"            )
    , (3, ":argument", Id'  5              )
    , (5, ":isa",      Str' ":integer"     )
    , (5, ":value",    Int' 42             )
    , (3, ":argument", Id'  6              )
    , (6, ":isa",      Str' ":integer"     )
    , (6, ":value",    Int' 66             )
    ]

  it "(x = (5)) == (x = 5)" $ do
    shouldBe
      (morlocScript "x = (5);")
      (morlocScript "x = 5;")

  testRdfCode
    "(1,\"foo\",1.1)"
    [ (0, ":isa",      Str' ":script"  )
    , (0, ":child",    Id'  1          )
    , (1, ":isa",      Str' ":tuple"   )
    , (1, ":contains", Id'  2          )
    , (2, ":isa",      Str' ":integer" )
    , (2, ":value",    Int' 1          )
    , (1, ":contains", Id'  3          )
    , (3, ":isa",      Str' ":string"  )
    , (3, ":value",    Str' "foo"      )
    , (1, ":contains", Id'  4          )
    , (4, ":isa",      Str' ":number"  )
    , (4, ":value",    Num' 1.1        )
    ]

  testRdfCode
    "foo :: i:Int -> Num where (i > 0)"
    [ (0, ":isa",        Str' ":script"          )
    , (0, ":child",      Id'  1                  )
    , (1, ":isa",        Str' ":typeDeclaration" )
    , (1, ":lhs",        Id'  2                  )
    , (2, ":isa",        Str' ":name"            )
    , (2, ":value",      Str' "foo"              )
    , (1, ":rhs",        Id'  3                  )
    , (3, ":isa",        Str' ":functionType"    ) -- i:Int -> Num where (i > 0)
    , (3, ":input",      Id'  4                  )
    , (4, ":isa",        Str' ":atomicType"      ) -- i:Int
    , (4, ":value",      Str' "Int"              )
    , (4, ":label",      Str' "i"                )
    , (3, ":output",     Id'  5                  ) -- Num
    , (5, ":isa",        Str' ":atomicType"      )
    , (5, ":value",      Str' "Num"              )
    , (3, ":constraint", Id'  6                  )
    , (6, ":isa",        Str' ":binop"           ) -- "i > 0"
    , (6, ":value",      Str' "GT"               )
    , (6, ":lhs",        Id'  7                  )
    , (6, ":rhs",        Id'  8                  )
    , (7, ":isa",        Str' ":name"            )
    , (7, ":value",      Str' "i"                )
    , (8, ":isa",        Str' ":integer"         )
    , (8, ":value",      Int' 0                  )
    ]

  testRdfCode
    "foo :: Int"
    [ (0, ":isa",        Str' ":script"          )
    , (0, ":child",      Id'  1                  )
    , (1, ":isa",        Str' ":typeDeclaration" )
    , (1, ":lhs",        Id'  2                  )
    , (2, ":isa",        Str' ":name"            )
    , (2, ":value",      Str' "foo"              )
    , (1, ":rhs",        Id'  3                  )
    , (3, ":isa",        Str' ":atomicType"      )
    , (3, ":value",      Str' "Int"              )
    ]

  testRdfCodeWith
    (rmId [0..5])
    "foo :: X -> Y where (1.1 + 1.2 > 2.0)"
    [ (6,  ":isa",   Str' ":binop"  )
    , (6,  ":value", Str' "GT"      )
    , (6,  ":lhs",   Id'  8         )
    , (6,  ":rhs",   Id'  10        )
    , (8,  ":isa",   Str' ":binop"  )
    , (8,  ":value", Str' "Add"     )
    , (8,  ":lhs",   Id'  7         )
    , (8,  ":rhs",   Id'  9        )
    , (7,  ":isa",   Str' ":number" )
    , (7,  ":value", Num' 1.1       )
    , (9, ":isa",   Str' ":number" )
    , (9, ":value", Num' 1.2       )
    , (10, ":isa",   Str' ":number" )
    , (10, ":value", Num' 2.0       )
    ]

  testRdfCode
    "foo :: a, (b -> c) -> d"
    [ (0, ":isa",    Str' ":script"          )
    , (0, ":child",  Id'  1                  )
    , (1, ":isa",    Str' ":typeDeclaration" )
    , (1, ":lhs",    Id'  2                  )
    , (2, ":isa",    Str' ":name"            )
    , (2, ":value",  Str' "foo"              )
    , (1, ":rhs",    Id'  3                  )
    , (3, ":isa",    Str' ":functionType"    )
    , (3, ":input",  Id'  4                  )
    , (4, ":isa",    Str' ":atomicGeneric"   )
    , (4, ":value",  Str' "a"                )
    , (3, ":input",  Id'  5                  )
    , (5, ":isa",    Str' ":functionType"    )
    , (5, ":input",  Id'  6                  )
    , (6, ":isa",    Str' ":atomicGeneric"   )
    , (6, ":value",  Str' "b"                )
    , (5, ":output", Id'  7                  )
    , (7, ":isa",    Str' ":atomicGeneric"   )
    , (7, ":value",  Str' "c"                )
    , (3, ":output", Id'  8                  )
    , (8, ":isa",    Str' ":atomicGeneric"   )
    , (8, ":value",  Str' "d"                )
    ]

  testRdfCodeWith
    (rmId [0..3])
    "foo :: A where ((1 == 1) and (2 == 2))"
    [ (4,  ":isa",   Str' ":binop"   )
    , (4,  ":value", Str' "and"      )
    , (4,  ":lhs",   Id'  5          )
    , (4,  ":rhs",   Id'  8          )
    , (5,  ":isa",   Str' ":binop"   )
    , (5,  ":value", Str' "EQ"       )
    , (5,  ":lhs",   Id'  6          )
    , (5,  ":rhs",   Id'  7          )
    , (6,  ":isa",   Str' ":integer" )
    , (6,  ":value", Int' 1          )
    , (7,  ":isa",   Str' ":integer" )
    , (7,  ":value", Int' 1          )
    , (8,  ":isa",   Str' ":binop"   )
    , (8,  ":value", Str' "EQ"       )
    , (8,  ":lhs",   Id'  9         )
    , (8,  ":rhs",   Id'  10         )
    , (9, ":isa",   Str' ":integer" )
    , (9, ":value", Int' 2          )
    , (10, ":isa",   Str' ":integer" )
    , (10, ":value", Int' 2          )
    ]

  testRdfCode
    "f . g"
    [ (0, ":isa",   Str' ":script"      )
    , (0, ":child", Id'  2              )
    , (2, ":isa",   Str' ":composition" )
    , (2, ":lhs",   Id'  1              )
    , (2, ":rhs",   Id'  3              )
    , (1, ":isa",   Str' ":name"        )
    , (1, ":value", Str' "f"            )
    , (3, ":isa",   Str' ":name"        )
    , (3, ":value", Str' "g"            )
    ]

  testRdfCodeWith
    (rmId ([0..6] ++ [9]))
    -- this will fail later, since x,k, and t are undefined.
    "X :: Y where (x^(-k) == 1)"
    [ (7, ":isa",      Str' "Neg"   )
    , (7, ":contains", Id'  8       )
    , (8, ":isa",      Str' ":name" )
    , (8, ":value",    Str' "k"     )
    ]

  testRdfCodeWith
    (rmId ([0..3]))
    -- this will fail later, since x,k, and t are undefined.
    "X :: Y where (f x (g y z))"
    [ (4, ":isa",      Str' ":call" )
    , (4, ":name",     Str' "f"     )
    , (4, ":argument", Id'  5       )
    , (5, ":isa",      Str' ":name" )
    , (5, ":value",    Str' "x"     )
    , (4, ":argument", Id'  6       )
    , (6, ":isa",      Str' ":call" )
    , (6, ":name",     Str' "g"     )
    , (6, ":argument", Id'  7       )
    , (7, ":isa",      Str' ":name" )
    , (7, ":value",    Str' "y"     )
    , (6, ":argument", Id'  8       )
    , (8, ":isa",      Str' ":name" )
    , (8, ":value",    Str' "z"     )
    ]

  testRdfCodeWith
    (rmId ([0..4] ++ [8]))
    -- this will fail later, since x,k, and t are undefined.
    "X :: Y where (f x y == 1)"
    [ (5, ":isa",      Str' ":call" )
    , (5, ":name",     Str' "f"     )
    , (5, ":argument", Id'  6       )
    , (6, ":isa",      Str' ":name" )
    , (6, ":value",    Str' "x"     )
    , (5, ":argument", Id'  7       )
    , (7, ":isa",      Str' ":name" )
    , (7, ":value",    Str' "y"     )
    ]
