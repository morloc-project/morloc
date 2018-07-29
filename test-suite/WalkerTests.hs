{-# LANGUAGE OverloadedStrings #-}

module WalkerTests (testWalker) where

import Test.Tasty.Hspec
import qualified Data.RDF as DR
import qualified Morloc.Walker as MW
import qualified Morloc.Parser as MP
import qualified Morloc.Triple as M3
import Morloc.Operators ((|>>))


testIt :: (Show a, Eq a) => String -> String -> (M3.RDF -> a) -> a -> Spec
testIt desc code f exp = case MP.morlocScript code of 
  (Right rdf) -> it desc $ do shouldBe (f rdf) exp
  (Left err) -> error (unlines ["Failure:", ">>>" ++ show err])

testWalker :: Spec
testWalker = parallel $ do
  testIt
    "fetchType"
    "foo :: Int, [Bar] -> Num; bar :: GGG; foo a b = bar [12,23,45];"
    (\rdf -> MW.fetchType rdf "foo")
    [DR.UNode "mid:3"]

  testIt
    "fetchType >>= elements"
    "foo :: Int, [Bar] -> Num; bar :: GGG; foo a b = bar [12,23,45];"
    (\rdf -> MW.fetchType rdf "foo" >>= MW.elements rdf)
    [DR.UNode "mid:4", DR.UNode "mid:5"]

  testIt
    "map position (fetchType >>= elements)"
    "foo :: Int, [Bar] -> Num; bar :: GGG; foo a b = bar [12,23,45];"
    (\rdf ->  MW.fetchType rdf "foo"
          >>= MW.elements rdf
          |>> MW.position rdf
      )
    [Just 0, Just 1]


