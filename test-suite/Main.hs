import qualified Test.Tasty
import Test.Tasty.Hspec

import Morloc.Parser (morlocScript)
import Morloc.Triple

main :: IO ()
main = do
  test <- testSpec "morloc" spec
  Test.Tasty.defaultMain test

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
