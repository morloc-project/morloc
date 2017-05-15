-- This stub is from HSpec tutorial (http://hspec.github.io/)
-- I like HSpec, mostly because it is very cleanly documented

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List (intercalate)
import Data.Either (either, isLeft)
import Control.Applicative

import Morloc (interpret)
import Morloc.Evaluator (eval)
import Morloc.Mode (asLIL)

main :: IO ()
main = hspec $ do

  describe "parse morloc code" $ do

    it "dies on syntax errors" $ do
      (isLeft . eval) "a b . %" `shouldBe` True
      (isLeft . eval) ". a"     `shouldBe` True

    it "doesn't allow application to primitives" $ do
      (isLeft . eval) "1 1" `shouldBe` True
      (isLeft . eval) "a . 1 1" `shouldBe` True
    
  describe "interpret asLIL" $

    it "handles node application (a b)" $
      shouldBe
        (readLIL "a b")
        (Right [["a", "0", "1", "*", "b"]])

  where
    readLIL :: String -> Either String [[String]]
    readLIL s = either l r (interpret asLIL s) where
      l = Left . show
      r = Right . map words . lines
