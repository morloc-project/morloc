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
        , (2, ":rhs",   Id'  4              )
        , (3, ":isa",   Str' ":name"        )
        , (3, ":value", Str' "x"            )
        , (4, ":isa",   Str' ":integer"     )
        , (4, ":value", Int' 1              )
        ]
      )
