{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Instance
Description : Find the source of a method call built by the code generator
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Used only for method calls the code generator itself builds (such as
@__get_slice__@ or @map@ over an effect boundary). It takes the first
sourced instance that matches, without ranking by specificity, and does not
see instances written in morloc.
-}
module Morloc.CodeGenerator.Instance
  ( findInstanceByArgHead
  , findFunctorMap
  , resolveInstanceForType
  ) where

import Morloc.CodeGenerator.Infer (requireDetermined)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import Morloc.Typecheck.Internal (instanceArgHead)

-- | The source of a method in a language, for the instance whose argument at
-- @pos@ has the given head.
findInstanceByArgHead :: Int -> EVar -> Lang -> TVar -> MorlocMonad (Maybe Source)
findInstanceByArgHead pos method lang containerTv = do
  sigmap <- MM.gets stateTypeclasses
  case Map.lookup method sigmap of
    Nothing -> return Nothing
    Just inst -> return $ listToMaybe
      [ src
      | TermTypes (Just et) cs _ <- instanceTerms inst
      , instanceArgHead pos et == Just containerTv
      , (_, Idx _ src) <- cs
      , srcLang src == lang
      ]

-- | The source of @map@ for a container type; the container is argument 1.
findFunctorMap :: Lang -> TypeU -> MorlocMonad (Maybe Source)
findFunctorMap lang receiverType =
  findInstanceByArgHead 1 (EV "map") lang (extractKey receiverType)

-- | The first hit of a lookup along a type's alias chain. A type nothing
-- determined is rejected at @i@ (POLY-6).
resolveInstanceForType
  :: (Lang -> TypeU -> MorlocMonad (Maybe Source))
  -> Int
  -> Lang
  -> Type
  -> MorlocMonad (Maybe Source)
resolveInstanceForType perTypeLookup i lang originalType = do
  requireDetermined i originalType
  scope <- MM.getGeneralScope
  go (TE.aliasChain scope (type2typeu originalType))
  where
    go [] = return Nothing
    go (t : ts) = perTypeLookup lang t >>= maybe (go ts) (return . Just)
