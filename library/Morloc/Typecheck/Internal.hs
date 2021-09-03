{-|
Module      : Morloc.Typecheck.Internal
Description : Functions for type checking and type manipulation
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This module exports any typechecking machinery that can be shared between the
general typechecker in the frontend and the language specific typechecker(s) of
the backend.

-}

module Morloc.Typecheck.Internal
  ( (+>)
  , (++>)
  , resolve
  , substituteT
  -- * accessing state
  , lookupSig
  , newvar
  , newvarRich
  -- * Typeclasses
  , Applicable(..)
  , Indexable(..)
  -- * manipulating context
  , access1
  , access2
  , lookupU
  , cut
  , substitute
  , occursCheck
  ) where

import Morloc.Namespace
import qualified Morloc.Frontend.PartialOrder as P
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Monad as Mod

import qualified Data.Set as Set


class Applicable a where
  apply :: Gamma -> a -> a

-- | Apply a context to a type (See Dunfield Figure 8).
instance Applicable UnresolvedType where
  -- [G]a = a
  apply _ a@(VarU _) = a
  -- [G](A->B) = ([G]A -> [G]B)
  apply g (FunU a b) = FunU (apply g a) (apply g b)
  -- [G]ForallU a.a = forall a. [G]a
  apply g (ForallU x a) = ForallU x (apply g a)
  -- [G[a=t]]a = [G[a=t]]t
  apply g (ExistU v ts ds) =
    case lookupU v g of
      -- FIXME: this seems problematic - do I keep the previous parameters or the new ones?
      (Just t') -> apply g t' -- reduce an existential; strictly smaller term
      Nothing -> ExistU v (map (apply g) ts) (map (apply g) ds)
  apply g (ArrU v ts) = ArrU v (map (apply g) ts)
  apply g (NamU r v ts rs) = NamU r v (map (apply g) ts) (map (\(n, t) -> (n, apply g t)) rs)

instance Applicable EType where
  apply g e = e { etype = apply g (etype e) }


class Indexable a where
  index :: a -> GammaIndex

instance Indexable GammaIndex where
  index = id

instance Indexable UnresolvedType where
  index (ExistU t ts ds) = ExistG t ts ds
  index t = error $ "Can only index ExistT, found: " <> show t


(+>) :: Indexable a => Gamma -> a -> Gamma
(+>) g x = g {gammaContext = (index x) : gammaContext g}


(++>) :: Indexable a => Gamma -> [a] -> Gamma
(++>) g xs = g {gammaContext = map index (reverse xs) <> gammaContext g }



occursCheck :: UnresolvedType -> UnresolvedType -> MT.Text -> Either TypeError ()
occursCheck t1 t2 place = do
  case Set.member t1 (P.free t2) of
    True -> Left $ OccursCheckFail t1 t2 place
    False -> Right ()



-- This functions removes qualified and existential types.
--  * all qualified terms are replaced with UnkT
--  * all existentials are replaced with default values if a possible
--    FIXME: should I really just take the first in the list???
resolve :: UnresolvedType -> Type
resolve (VarU v) = VarT v
resolve (FunU t1 t2) = FunT (resolve t1) (resolve t2)
resolve (ArrU v ts) = ArrT v (map resolve ts)
resolve (NamU r v ps rs) =
  let ts' = map (resolve . snd) rs
      ps' = map resolve ps 
  in NamT r v ps' (zip (map fst rs) ts')
resolve (ExistU v _ []) = resolve (ForallU v (VarU v)) -- whatever
resolve (ExistU _ _ (t:_)) = resolve t
resolve (ForallU v t) = substituteT v (UnkT v) (resolve t)


-- | substitute all appearances of a given variable with a given new type
substituteT :: TVar -> Type -> Type -> Type
substituteT v0 r0 t0 = sub t0
  where
    sub :: Type -> Type
    sub t@(UnkT _) = t
    sub t@(VarT v)
      | v0 == v = r0
      | otherwise = t
    sub (FunT t1 t2) = FunT (sub t1) (sub t2)
    sub (ArrT v ts) = ArrT v (map sub ts)
    sub (NamT r v ts rs) = NamT r v (map sub ts) [(x, sub t) | (x, t) <- rs]


-- | substitute all appearances of a given variable with an existential
-- [t/v]A
substitute :: TVar -> UnresolvedType -> UnresolvedType
substitute v t = P.substitute v (ExistU v [] []) t


access1 :: TVar -> [GammaIndex] -> Maybe ([GammaIndex], GammaIndex, [GammaIndex])
access1 v gs =
  case findIndex (exists v) gs of
    (Just 0) -> Just ([], head gs, tail gs)
    (Just i) -> Just (take i gs, gs !! i, drop (i + 1) gs)
    _ -> Nothing
  where
    exists :: TVar -> GammaIndex -> Bool
    exists v1 (ExistG v2 _ _) = v1 == v2
    exists _ _ = False


access2
  :: TVar
  -> TVar
  -> [GammaIndex]
  -> Maybe ([GammaIndex], GammaIndex, [GammaIndex], GammaIndex, [GammaIndex])
access2 lv rv gs =
  case access1 lv gs of
    Just (ls, x, rs) ->
      case access1 rv rs of
        Just (ls', y, rs') -> Just (ls, x, ls', y, rs')
        _ -> Nothing
    _ -> Nothing


-- | Look up a solved existential type variable
lookupU :: TVar -> Gamma -> Maybe UnresolvedType
lookupU v (gammaContext -> gs0) = f gs0 where
  f [] = Nothing
  f ((SolvedG v' t):gs)
    | v == v' = Just t
    | otherwise = f gs
  f (_:gs) = f gs



-- | remove context up to a marker
cut :: GammaIndex -> Gamma -> Either TypeError Gamma
cut i g = do
  xs1 <- f (gammaContext g)
  return $ g { gammaContext = xs1 }
  where
  f [] = Left $ EmptyCut i
  f (x:xs)
    | i == x = return xs
    | otherwise = f xs


newvar :: Maybe Lang -> Gamma -> (Gamma, UnresolvedType)
newvar = newvarRich [] []


newvarRich
  :: [UnresolvedType] -- ^ type parameters
  -> [UnresolvedType] -- ^ type defaults
  -> Maybe Lang
  -> Gamma
  -> (Gamma, UnresolvedType)
newvarRich ps ds lang g =
  let i = gammaCounter g
  in (g {gammaCounter = i + 1}, ExistU (TV lang (newvars !! i)) ps ds)
  where
    newvars =
      zipWith (\x y -> MT.pack (x ++ show y)) (repeat "t") ([0 ..] :: [Integer])

-- | create a new variable such as "a.0"
newqul :: TVar -> Gamma -> (Gamma, TVar)
newqul (TV l v) g
  = let i = gammaCounter g
    in (g {gammaCounter = i + 1}, TV l (v <> "." <> (MT.pack . show $ i)))



lookupSig :: Int -> MorlocMonad (Maybe TermTypes)
lookupSig i = do
  s <- MM.get
  case GMap.lookup i (stateSignatures s) of
    GMapNoFst -> return Nothing
    GMapNoSnd -> MM.throwError . CallTheMonkeys $ "Internal GMap key missing"
    (GMapJust t) -> return (Just t)
