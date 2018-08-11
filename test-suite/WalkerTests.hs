{-# LANGUAGE OverloadedStrings #-}

module WalkerTests (testWalker) where

import Test.Tasty.Hspec
import qualified Data.RDF as DR
import qualified Morloc.Walker as MW
import qualified Morloc.Parser as MP
import qualified Morloc.Triple as M3
import Morloc.Operators ((|>>))


testIt :: (Show a, Eq a) => String -> String -> (M3.RDF -> a) -> a -> Spec
testIt desc code f exp' = case MP.parseShallow Nothing code of 
  (Right rdf) -> it desc $ do shouldBe (f rdf) exp'
  (Left err) -> error (unlines ["Failure:", ">>>" ++ show err])

testWalker :: Spec
testWalker = parallel $ do
  testIt
    "getType"
    "foo :: Int, [Bar] -> Num; bar :: GGG; foo a b = bar [12,23,45];"
    (\rdf -> MW.getType rdf "foo")
    [DR.UNode "mid:3"]

  testIt
    "getType >>= elements"
    "foo :: Int, [Bar] -> Num; bar :: GGG; foo a b = bar [12,23,45];"
    (\rdf -> MW.getType rdf "foo" >>= MW.elements rdf)
    [DR.UNode "mid:4", DR.UNode "mid:5"]

  testIt
    "map position (getType >>= elements)"
    "foo :: Int, [Bar] -> Num; bar :: GGG; foo a b = bar [12,23,45];"
    (\rdf ->  MW.getType rdf "foo"
          >>= MW.elements rdf
          |>> MW.position rdf
      )
    [Just 0, Just 1]

  testIt
    "sourceExports"
    "export foo; source \"R\" (\"f.o.o\" as foo, \"bar\");"
    (\rdf -> MW.getSources rdf >>= MW.sourceExports rdf)
    [DR.UNode "mid:3"]
