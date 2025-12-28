{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.Merge
Description : Merge various things
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : Apache-2.0
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

Since morloc allows co-existence of many different implementations, the ever
new definition needs to be merged in with all the pre-existing ones. Further,
we need to avoid many identical copies of on instance. The purpose of this
module is to curate the functions for merging different terms.
-}

module Morloc.Frontend.Merge
  ( mergeTermTypes
  , weaveTermTypes
  , mergeEType
  , mergeTypeUs
  , mergeTypeclasses
  , mergeIndexedInstances
  , unionTermTypes
  , mergeSignatureSet
  , mergeFirstIndexM
  ) where


-- TODO: tighten all this up, formalize these operations and follow the
-- conventions below. Make a typeclass for these mergeable types.
--
-- union :: [a] -> [a] -> Either MorlocError [a]
-- weave :: a -> [a] -> Either MorlocError [a]
-- merge :: a -> a -> Either MorlocError a


import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM

mergeFirstIndexM :: Monad m => (a -> a -> m a) -> Indexed a -> Indexed a -> m (Indexed a)
mergeFirstIndexM f (Idx i x) (Idx _ y) = Idx i <$> f x y

mergeTermTypes :: TermTypes -> TermTypes -> MorlocMonad TermTypes
mergeTermTypes (TermTypes g1 cs1 es1) (TermTypes g2 cs2 es2)
  = TermTypes
  <$> maybeCombine mergeEType g1 g2
  <*> pure (unique (cs1 <> cs2))
  <*> pure (unique (es1 <> es2))
  where
  -- either combine terms or take the first on that is defined, or whatever
  maybeCombine :: Monad m => (a -> a -> m a) -> Maybe a -> Maybe a -> m (Maybe a)
  maybeCombine f (Just a) (Just b) = Just <$> f a b
  maybeCombine _ (Just a) _ = return $ Just a
  maybeCombine _ _ (Just b) = return $ Just b
  maybeCombine _ _ _ = return Nothing


-- Add one new TermTypes object into a list
weaveTermTypes :: TermTypes -> [TermTypes] -> [TermTypes]
weaveTermTypes t1@(TermTypes (Just gt1) srcs1 es1) (t2@(TermTypes (Just gt2) srcs2 es2):ts)
  | equivalent (etype gt1) (etype gt2) = TermTypes (Just gt1) (unique (srcs1 <> srcs2)) (es1 <> es2) : ts
  | otherwise = t2 : weaveTermTypes t1 ts
weaveTermTypes (TermTypes Nothing srcs1 es1) ((TermTypes e2 srcs2 es2):ts2) =
  weaveTermTypes (TermTypes e2 (srcs1 <> srcs2) (es1 <> es2)) ts2
weaveTermTypes TermTypes{} (TermTypes{}:_) = error "what the why?"
weaveTermTypes t1 [] = [t1]

-- | This function defines how general types are merged. There are decisions
-- encoded in this function that should be vary carefully considered.
--  * Can properties simply be concatenated?
--  * What if constraints are contradictory?
--  * Should general type merging even be possible?
mergeEType :: EType -> EType -> MorlocMonad EType
mergeEType (EType t1 ps1 cs1 edoc1) (EType t2 ps2 cs2 edoc2)
  = EType <$> mergeTypeUs t1 t2 <*> pure (ps1 <> ps2) <*> pure (cs1 <> cs2) <*> pure edocs12
  where
    edocs12 = mergeEdocs edoc1 edoc2

    -- TODO: is there a real use case where we would want to merge docstrings?
    mergeEdocs x _ = x

-- merge two general types
mergeTypeUs :: TypeU -> TypeU -> MorlocMonad TypeU
mergeTypeUs t1 t2
  | equivalent t1 t2 = return t1
  | otherwise = MM.throwError $ IncompatibleGeneralType t1 t2

mergeTypeclasses :: Instance -> Instance -> MorlocMonad Instance
mergeTypeclasses inst1@(Instance cls1 vs1 t1 ts1) inst2@(Instance cls2 vs2 t2 ts2)
  | cls1 /= cls2 = MM.throwError $ ConflictingInstances "Mismatched class names" inst1 inst2
  | not (equivalent (etype t1) (etype t2)) = MM.throwError $ ConflictingInstances "Conflicting typeclass term general type" inst1 inst2
  | length vs1 /= length vs2 = MM.throwError $ ConflictingInstances "Conflicting typeclass parameter count" inst1 inst2
  -- here I should do reciprocal subtyping
  | otherwise = return $ Instance cls1 vs1 t1 (unionTermTypes ts1 ts2)

-- Merge two indexed instances keeping the left index
mergeIndexedInstances
  :: Indexed Instance
  -> Indexed Instance
  -> MorlocMonad (Indexed Instance)
mergeIndexedInstances = mergeFirstIndexM mergeTypeclasses

mergeSignatureSet :: SignatureSet -> SignatureSet -> MorlocMonad SignatureSet
mergeSignatureSet s1@(Polymorphic cls1 v1 t1 ts1) s2@(Polymorphic cls2 v2 t2 ts2)
  | cls1 == cls2 && equivalent (etype t1) (etype t2) && v1 == v2 = return $ Polymorphic cls1 v1 t1 (unionTermTypes ts1 ts2)
  | otherwise = MM.throwError $ CannotUnifySignatures s1 s2
mergeSignatureSet (Monomorphic ts1) (Monomorphic ts2) = Monomorphic <$> mergeTermTypes ts1 ts2
mergeSignatureSet s1 s2 = MM.throwError $ CannotUnifySignatures s1 s2


unionTermTypes :: [TermTypes] -> [TermTypes] -> [TermTypes]
unionTermTypes ts1 ts2 = foldr weaveTermTypes ts2 ts1
