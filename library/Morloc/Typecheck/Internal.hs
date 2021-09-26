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
  -- * accessing state
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
instance Applicable TypeU where
  -- [G]a = a
  apply _ a@(VarU _) = a
  -- [G](A->B) = ([G]A -> [G]B)
  apply g (FunU ts t) = FunU (map (apply g) ts) (apply g t)
  -- [G]ForallU a.a = forall a. [G]a
  apply g (ForallU x a) = ForallU x (apply g a)
  -- [G[a=t]]a = [G[a=t]]t
  apply g (ExistU v ts ds) =
    case lookupU v g of
      -- FIXME: this seems problematic - do I keep the previous parameters or the new ones?
      (Just t') -> apply g t' -- reduce an existential; strictly smaller term
      Nothing -> ExistU v (map (apply g) ts) (map (apply g) ds)
  apply g (AppU v ts) = AppU v (map (apply g) ts)
  apply g (NamU o n ps rs) = NamU o n ps [(k, apply g t) | (k, t) <- rs]

instance Applicable EType where
  apply g e = e { etype = apply g (etype e) }


class Indexable a where
  index :: a -> GammaIndex

instance Indexable GammaIndex where
  index = id

instance Indexable TypeU where
  index (ExistU t ts ds) = ExistG t ts ds
  index t = error $ "Can only index ExistT, found: " <> show t


(+>) :: Indexable a => Gamma -> a -> Gamma
(+>) g x = g {gammaContext = (index x) : gammaContext g}


(++>) :: Indexable a => Gamma -> [a] -> Gamma
(++>) g xs = g {gammaContext = map index (reverse xs) <> gammaContext g }



occursCheck :: TypeU -> TypeU -> MT.Text -> Either TypeError ()
occursCheck t1 t2 place = do
  case Set.member t1 (free t2) of
    True -> Left $ OccursCheckFail t1 t2 place
    False -> Right ()



-- | substitute all appearances of a given variable with an existential
-- [t/v]A
substitute :: TVar -> TypeU -> TypeU
substitute v t = substituteTVar v (ExistU v [] []) t


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
lookupU :: TVar -> Gamma -> Maybe TypeU
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


newvar :: Maybe Lang -> Gamma -> (Gamma, TypeU)
newvar = newvarRich [] []


newvarRich
  :: [TypeU] -- ^ type parameters
  -> [TypeU] -- ^ type defaults
  -> Maybe Lang
  -> Gamma
  -> (Gamma, TypeU)
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
