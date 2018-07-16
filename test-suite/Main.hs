import qualified Test.Tasty
import Test.Tasty.Hspec

import Morloc.Parser (morlocScript)
import Morloc.Triple

-- TODO: test the following
-- [x] arithmetic
-- [ ] boolean operators
-- [ ] unary operators (+/-)
-- [ ] functions in constraints
-- [ ] composition
-- [ ] each explicit data type
-- [ ] source
-- [ ] simple import
-- [ ] restricted import
-- [ ] error throwing?

main :: IO ()
main = do
  test <- testSpec "morloc" spec
  Test.Tasty.defaultMain test

rmId :: [Int] -> RDF -> RDF
rmId is (RDF i ts) = RDF i (filter (rmId' is) ts) where
  rmId' :: [Int] -> Triple -> Bool 
  rmId' is' (i, _, _) = all (\x -> x /= i) is'

spec :: Spec
spec = parallel $ do
  it "x = 1" $ do
    shouldBe
      (morlocScript "x = 1;")
      (Right $ RDF 1
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
      )

  it "f x = x;" $ do
    shouldBe
      (morlocScript "f x = x;")
      (Right $ RDF 1
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
      )

  it "f = g 42 66;" $ do
    shouldBe
      (morlocScript "f = g 42 66;")
      (Right $ RDF 1
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
      )

  it "(x = (5)) == (x = 5)" $ do
    shouldBe
      (morlocScript "x = (5);")
      (morlocScript "x = 5;")

  it "(1,\"foo\",1.1);" $ do
    shouldBe
      (morlocScript "(1,\"foo\",1.1);")
      (Right $ RDF 1
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
      )

  it "foo :: i:Int -> Num where (i > 0);" $ do
    shouldBe
      (morlocScript "foo :: i:Int -> Num where (i > 0);")
      (Right $ RDF 1
        [ (1, ":isa",        Str' ":script"          )
        , (1, ":child",      Id'  2                  )
        , (2, ":isa",        Str' ":typeDeclaration" )
        , (2, ":lhs",        Id'  3                  )
        , (3, ":isa",        Str' ":name"            )
        , (3, ":value",      Str' "foo"              )
        , (2, ":rhs",        Id'  4                  )
        , (4, ":isa",        Str' ":type"            ) -- i:Int -> Num where (i > 0)
        , (4, ":input",      Id'  5                  )
        , (5, ":isa",        Str' ":type"            ) -- i:Int
        , (5, ":name",       Str' "Int"              )
        , (5, ":label",      Str' "i"                )
        , (4, ":output",     Id'  6                  ) -- Num
        , (6, ":isa",        Str' ":type"            )
        , (6, ":name",       Str' "Num"              )
        , (4, ":constraint", Id'  7                  )
        , (7, ":isa",        Str' ":binop"           ) -- "i > 0"
        , (7, ":name",       Str' "GT"               )
        , (7, ":lhs",        Id'  8                  )
        , (7, ":rhs",        Id'  9                  )
        , (8, ":isa",        Str' ":name"            )
        , (8, ":value",      Str' "i"                )
        , (9, ":isa",        Str' ":integer"         )
        , (9, ":value",      Int' 0                  )
        ]
      )

  it "foo :: Int where (1.1 + 1.2 > 2.0);" $ do
    shouldBe
      (fmap (rmId [1..5]) (morlocScript "foo :: Int where (1.1 + 1.2 > 2.0);"))
      (Right $ RDF 1
        [ (6,  ":isa",   Str' ":binop"  )
        , (6,  ":name",  Str' "GT"      )
        , (6,  ":lhs",   Id'  8         )
        , (6,  ":rhs",   Id'  10        )
        , (8,  ":isa",   Str' ":binop"  )
        , (8,  ":name",  Str' "Add"     )
        , (8,  ":lhs",   Id'  7         )
        , (8,  ":rhs",   Id'  9         )
        , (7,  ":isa",   Str' ":number" )
        , (7,  ":value", Num' 1.1       )
        , (9,  ":isa",   Str' ":number" )
        , (9,  ":value", Num' 1.2       )
        , (10, ":isa",   Str' ":number" )
        , (10, ":value", Num' 2.0       )
        ]
      )
