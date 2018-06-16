module Morloc.Evaluator (eval) where

import Control.Monad.Except (throwError)
import Data.Maybe (mapMaybe)
import Control.Applicative

import Morloc.Data
import qualified Morloc.Syntax as Syntax
import qualified Morloc.EvalError as Error

import qualified Morloc.Graph as Graph
import Morloc.Parser (morlocScript)

eval :: String -> Error.ThrowsError Program
eval s = morlocScript s >>= script2program

script2program :: [Syntax.Top] -> Error.ThrowsError Program
script2program xs
  =   Program
  <$> workflow' xs
  <*> ontology' xs
  <*> packages' xs
  where

  workflow' :: [Syntax.Top] -> Error.ThrowsError (Graph.Graph WNode)
  workflow' = undefined

  ontology' :: [Syntax.Top] -> Error.ThrowsError [(String, TNode)]
  ontology' = undefined

  packages' :: [Syntax.Top] -> Error.ThrowsError [Syntax.Import]
  packages' = undefined
