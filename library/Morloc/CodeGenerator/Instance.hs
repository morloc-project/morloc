{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Instance
Description : Resolve a typeclass method's source binding for a type
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Instance
  ( findInstanceByArgHead
  , findFunctorMap
  , resolveInstanceForType
  ) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import Morloc.Typecheck.Internal (unqualify)

-- | Look up the per-language source binding of a class method whose
-- receiver type head occupies a known argument position.
findInstanceByArgHead :: Int -> EVar -> Lang -> TVar -> MorlocMonad (Maybe Source)
findInstanceByArgHead pos method lang containerTv = do
  sigmap <- MM.gets stateTypeclasses
  case Map.lookup method sigmap of
    Nothing -> return Nothing
    Just inst -> return $ listToMaybe
      [ src
      | TermTypes (Just et) cs _ <- instanceTerms inst
      , receiverHead et == Just containerTv
      , (_, Idx _ src) <- cs
      , srcLang src == lang
      ]
  where
    receiverHead :: EType -> Maybe TVar
    receiverHead et = case snd (unqualify (etype et)) of
      FunU args _ | length args > pos -> Just (extractKey (args !! pos))
      _                               -> Nothing

-- | Look up the @Functor@ @map@ instance for a container type in
-- language @lang@. The instance method signature is @(a -> b) -> f a
-- -> f b@; the container head sits at argument index 1.
findFunctorMap :: Lang -> TypeU -> MorlocMonad (Maybe Source)
findFunctorMap lang receiverType =
  findInstanceByArgHead 1 (EV "map") lang (extractKey receiverType)

-- | Resolve a typeclass-method instance for a value's type by walking
-- the alias chain. Tests the type's outermost head TVar against the
-- per-TVar lookup; on miss, reduces the type one alias step
-- (via 'TE.reduceType') and retries. Returns the first hit, or
-- 'Nothing' if the chain is exhausted. This is the key mechanism by
-- which @type Array a = List a@ inherits @instance Indexable List@:
-- the call site type @Array Int@ misses on @Array@, reduces to
-- @List Int@, then hits.
resolveInstanceForType
  :: (Lang -> TypeU -> MorlocMonad (Maybe Source))
  -> Lang
  -> Type
  -> MorlocMonad (Maybe Source)
resolveInstanceForType perTypeLookup lang originalType = do
  scope <- MM.getGeneralScope
  go scope (type2typeu originalType)
  where
    go scope t = do
      mSrc <- perTypeLookup lang t
      case mSrc of
        Just src -> return (Just src)
        Nothing -> case TE.reduceType scope t of
          Just t' | t' /= t -> go scope t'
          _ -> return Nothing
