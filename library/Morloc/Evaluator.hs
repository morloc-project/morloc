module Morloc.Evaluator (eval) where

import Control.Monad.Except (throwError)
import Data.Maybe (mapMaybe)
import Control.Applicative

import Morloc.Data
import qualified Morloc.Syntax as S
import qualified Morloc.Error as E

import qualified Morloc.Graph as G
import Morloc.Parser (morlocScript)

eval :: String -> E.ThrowsError Program
eval s = morlocScript s >>= script2program

script2program :: [S.Top] -> E.ThrowsError Program
script2program xs
  =   Program
  <$> workflow' xs
  <*> ontology' xs
  <*> packages' xs

ontology' :: [S.Top] -> E.ThrowsError [(String, TNode)]
ontology' xs
  = pure [
    (n, TNodeSignature i o c) |
    S.TopStatement (S.Signature n i o c) <- xs
  ]

packages' :: [S.Top] -> E.ThrowsError [S.Source]
packages' xs = pure [x | (S.TopSource x) <- xs]

workflow' :: [S.Top] -> E.ThrowsError [FunctionTree a]
workflow' xs = sequence $
  [FunctionTree <$> pure n <*> pure args <*> callTree expr |
    (S.TopStatement (S.Declaration n args expr)) <- xs]
  where

  callTree :: S.Expression -> E.ThrowsError (G.Graph WNode)

  -- TODO: currently I allow heterogenous lists and only allows primitive data
  -- in Morloc containers. For example, `{ a = foo "yolo" }`, is illegal.
  -- Having workflows nested in Morloc containers is something that should be
  -- legal. But for now I won't allow it. 
  -- Anyway, as it is, the following will never raise an error. 
  callTree (S.ExprData mdata)
    = pure $ G.Node (WNodeData mdata) []

  -- parse an application
  callTree (S.ExprApplication name tag xs)
    = G.Node <$> pure (WNodeVar name tag) <*> sequence (map callTree xs) 

  -- parse composition
  callTree (S.ExprComposition g f)
    = case (callTree g) of 
      Right (G.Node (WNodeData _) _)
        -> Left (E.BadComposition "Only functions can be composed")
      Right (G.Node v kids)
        -> G.Node <$> pure v <*> (app <$> pure kids <*> callTree f)
      err -> err

  -- add an element to the end of a list
  app :: [a] -> a -> [a]
  app xs x = xs ++ [x]
