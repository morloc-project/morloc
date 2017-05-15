-- This stub is from HSpec tutorial (http://hspec.github.io/)
-- I like HSpec, mostly because it is very cleanly documented

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (intercalate)
import Data.Either (either)
import Control.Applicative

import Morloc (interpret)
import Morloc.Mode (asLIL)

readLIL s = either l r (interpret asLIL s) where
  l = const Nothing
  r = Just . map words . lines

main :: IO ()
main = hspec $ do

  {- describe "parse morloc code" $ do                   -}
  {-                                                     -}
  {-   it "dies on weird input" $ do                     -}
  {-     evaluate (eval) `shouldThrow` anyException      -}
  {-                                                     -}
  {- describe "evaluate morloc code" $ do                -}
  {-                                                     -}
  {-   it "doesn't allow application to primitives" $ do -}
  {-     evaluate (eval) `shouldThrow` anyException      -}
    
  describe "interpret asLIL" $ do

    it "handles node application (a b c)" $ do
      shouldBe
        (readLIL "a b")
        (Just [["a", "0", "1", "*", "b"]])
