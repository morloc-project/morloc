{-|
Module      : Morloc.Database.Typecheck
Description : Check the logical consistency of a program
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Database.Typecheck (typecheck) where

import Morloc.Types
import qualified Morloc.Error as ME
import qualified Morloc.Data.Text as MT
import qualified Morloc.Component.Manifold as Manifold

-- TODO: this should be wrapped in the Either monad or something else better
-- for handling Error.
typecheck :: SparqlEndPoint -> IO ()
typecheck ep = do
  putStrLn "  typechecking RDF graph ... "
  manifolds <- Manifold.fromSparqlDb ep
  isGood manifolds

isGood :: [Manifold] -> IO ()
isGood ms = mapM_ isGoodOne ms

isGoodOne :: Manifold -> IO ()
isGoodOne m = case (mAbstractType m, mArgs m) of
  (Just (MFuncType _ exps _), args) -> mapM_ compareTypes (zip exps args)
  _ -> return ()

compareTypes :: (MType, Argument) -> IO ()
compareTypes (parentType, (ArgCall c)) = case mAbstractType c of
  (Just (MFuncType _ _ childOutput)) ->
    if
      parentType == childOutput 
    then
      return () 
    else
      return ()
      -- fail (show $ ME.TypeConflict (MT.unpack $ MT.pretty parentType)
      --                              (MT.unpack $ MT.pretty childOutput))
  _ -> return ()
compareTypes _ = return ()
