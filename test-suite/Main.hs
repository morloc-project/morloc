import qualified Test.Tasty
import Test.Tasty.Hspec

import Morloc.Parser (morlocScript)
import Morloc.Triple

-- TODO: test the following
-- [x] [x] arithmetic
-- [x] [x] higher order functions
-- [ ] [ ] boolean operators
-- [ ] [ ] unary operators (+/-)
-- [ ] [ ] functions in constraints
-- [ ] [ ] composition
-- [ ] [ ] each explicit data type
-- [ ] [x] source
-- [ ] [ ] simple import
-- [ ] [ ] restricted import
-- [ ] [ ] error throwing?

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
  (Left _) -> error "Compilation failed"
  where
    run' f' s' = (fmap (\(RDF _ ts') -> f' ts') (morlocScript s'))

testRdfCode :: String -> [Triple] -> Spec
testRdfCode = testRdfCodeWith id


spec :: Spec
spec = parallel $ do

  testRdfCode
    "x = 1;"
    [ (1, ":isa",   Str' ":script"      )
    , (1, ":child", Id'  2              )
    , (2, ":isa",   Str' ":declaration" )
    , (2, ":lhs",   Id'  3              )
    , (3, ":isa",   Str' ":name"        )
    , (3, ":value", Str' "x"            )
    , (2, ":rhs",   Id'  4              )
    , (4, ":isa",   Str' ":integer"     )
    , (4, ":value", Int' 1              )
    ]

  testRdfCode
    "f x = x;"
    [ (1, ":isa",       Str' ":script"      )
    , (1, ":child",     Id'  2              )
    , (2, ":isa",       Str' ":declaration" )
    , (2, ":lhs",       Id'  3              )
    , (3, ":isa",       Str' ":name"        )
    , (3, ":value",     Str' "f"            )
    , (2, ":parameter", Id'  4              )
    , (4, ":isa",       Str' ":name"        )
    , (4, ":value",     Str' "x"            )
    , (2, ":rhs",       Id'  5              )
    , (5, ":isa",       Str' ":name"        )
    , (5, ":value",     Str' "x"            )
    ]

  testRdfCode
    "f = g 42 66;"
    [ (1, ":isa",      Str' ":script"      )
    , (1, ":child",    Id'  2              )
    , (2, ":isa",      Str' ":declaration" )
    , (2, ":lhs",      Id'  3              )
    , (3, ":isa",      Str' ":name"        )
    , (3, ":value",    Str' "f"            )
    , (2, ":rhs",      Id'  4              )
    , (4, ":isa",      Str' ":application" )
    , (4, ":function", Id'  5              )
    , (5, ":isa",      Str' ":name"        )
    , (5, ":value",    Str' "g"            )
    , (4, ":argument", Id'  6              )
    , (6, ":isa",      Str' ":integer"     )
    , (6, ":value",    Int' 42             )
    , (4, ":argument", Id'  7              )
    , (7, ":isa",      Str' ":integer"     )
    , (7, ":value",    Int' 66             )
    ]

  it "(x = (5)) == (x = 5)" $ do
    shouldBe
      (morlocScript "x = (5);")
      (morlocScript "x = 5;")

  testRdfCode
    "(1,\"foo\",1.1);"
    [ (1, ":isa",   Str' ":script"  )
    , (1, ":child", Id'  2          )
    , (2, ":isa",   Str' ":tuple"   )
    , (2, ":child", Id'  3          )
    , (3, ":isa",   Str' ":integer" )
    , (3, ":value", Int' 1          )
    , (2, ":child", Id'  4          )
    , (4, ":isa",   Str' ":string"  )
    , (4, ":value", Str' "foo"      )
    , (2, ":child", Id'  5          )
    , (5, ":isa",   Str' ":number"  )
    , (5, ":value", Num' 1.1        )
    ]

  testRdfCode
    "foo :: i:Int -> Num where (i > 0);"
    [ (1, ":isa",        Str' ":script"          )
    , (1, ":child",      Id'  2                  )
    , (2, ":isa",        Str' ":typeDeclaration" )
    , (2, ":lhs",        Id'  3                  )
    , (3, ":isa",        Str' ":name"            )
    , (3, ":value",      Str' "foo"              )
    , (2, ":rhs",        Id'  4                  )
    , (4, ":isa",        Str' ":function"        ) -- i:Int -> Num where (i > 0)
    , (4, ":input",      Id'  5                  )
    , (5, ":isa",        Str' ":type"            ) -- i:Int
    , (5, ":value",      Str' "Int"              )
    , (5, ":label",      Str' "i"                )
    , (4, ":output",     Id'  6                  ) -- Num
    , (6, ":isa",        Str' ":type"            )
    , (6, ":value",      Str' "Num"              )
    , (4, ":constraint", Id'  7                  )
    , (7, ":isa",        Str' ":binop"           ) -- "i > 0"
    , (7, ":value",      Str' "GT"               )
    , (7, ":lhs",        Id'  8                  )
    , (7, ":rhs",        Id'  9                  )
    , (8, ":isa",        Str' ":name"            )
    , (8, ":value",      Str' "i"                )
    , (9, ":isa",        Str' ":integer"         )
    , (9, ":value",      Int' 0                  )
    ]

  testRdfCode
    "foo :: Int;"
    [ (1, ":isa",        Str' ":script"          )
    , (1, ":child",      Id'  2                  )
    , (2, ":isa",        Str' ":typeDeclaration" )
    , (2, ":lhs",        Id'  3                  )
    , (3, ":isa",        Str' ":name"            )
    , (3, ":value",      Str' "foo"              )
    , (2, ":rhs",        Id'  4                  )
    , (4, ":isa",        Str' ":type"            )
    , (4, ":value",      Str' "Int"              )
    ]

  testRdfCodeWith
    (rmId [1..6])
    "foo :: X -> Y where (1.1 + 1.2 > 2.0);"
    [ (7,  ":isa",   Str' ":binop"  )
    , (7,  ":value", Str' "GT"      )
    , (7,  ":lhs",   Id'  9         )
    , (7,  ":rhs",   Id'  11        )
    , (9,  ":isa",   Str' ":binop"  )
    , (9,  ":value", Str' "Add"     )
    , (9,  ":lhs",   Id'  8         )
    , (9,  ":rhs",   Id'  10        )
    , (8,  ":isa",   Str' ":number" )
    , (8,  ":value", Num' 1.1       )
    , (10, ":isa",   Str' ":number" )
    , (10, ":value", Num' 1.2       )
    , (11, ":isa",   Str' ":number" )
    , (11, ":value", Num' 2.0       )
    ]

  testRdfCode
    "foo :: a, (b -> c) -> d;"
    [ (1, ":isa",    Str' ":script"          )
    , (1, ":child",  Id'  2                  )
    , (2, ":isa",    Str' ":typeDeclaration" )
    , (2, ":lhs",    Id'  3                  )
    , (3, ":isa",    Str' ":name"            )
    , (3, ":value",  Str' "foo"              )
    , (2, ":rhs",    Id'  4                  )
    , (4, ":isa",    Str' ":function"        )
    , (4, ":input",  Id'  5                  )
    , (5, ":isa",    Str' ":generic"         )
    , (5, ":value",  Str' "a"                )
    , (4, ":input",  Id'  6                  )
    , (6, ":isa",    Str' ":function"        )
    , (6, ":input",  Id'  7                  )
    , (7, ":isa",    Str' ":generic"         )
    , (7, ":value",  Str' "b"                )
    , (6, ":output", Id'  8                  )
    , (8, ":isa",    Str' ":generic"         )
    , (8, ":value",  Str' "c"                )
    , (4, ":output", Id'  9                  )
    , (9, ":isa",    Str' ":generic"         )
    , (9, ":value",  Str' "d"                )
    ]
