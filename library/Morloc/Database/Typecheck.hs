{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Database.Typecheck
Description : Check the logical consistency of a program
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This typechecker currently only checks the case where an argument with a
concrete type receives input from a function with a concrete output. In this
case, the typechecker raises a warning (but does not die) if the types are not
identical.
-}

module Morloc.Database.Typecheck (typecheck) where

import Morloc.Types
import Morloc.Operators
import qualified Morloc.Data.Text as MT
import qualified Morloc.Component.Manifold as Manifold

-- TODO: this should be wrapped in the Either monad or something else better
-- for handling Error.
typecheck :: SparqlDatabaseLike db => db -> IO ()
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
      eqTypes parentType childOutput 
    then
      return ()
    else
      do
        MT.putStr (MT.show' $ TypeConflict (showType parentType)
                                           (showType childOutput))
        return ()
  _ -> return ()
compareTypes _ = return ()

showType :: MType -> MT.Text
showType (MConcType _ x xs) = MT.intercalate " " ([MT.pretty x] ++ map showType xs)
showType (MAbstType _ x xs) = MT.intercalate " " ([MT.pretty x] ++ map showType xs)
showType (MFuncType _ xs o) = MT.intercalate ", " (map showType xs) <> " -> " <> MT.pretty o

-- TODO: This is VERY rudimentary. I am treating generics as completely
-- unconstrainted wildcards. Instead, I should resolve all generic cases to
-- concrete cases first, raising errors for any ambiguities.
eqTypes :: MType -> MType -> Bool
eqTypes (MAbstType _ _ _) _ = True
eqTypes _ (MAbstType _ _ _) = True
eqTypes (MConcType _ x xs) (MConcType _ y ys)
  =  x == y
     && foldl (&&) True (zipWith eqTypes xs ys)  
