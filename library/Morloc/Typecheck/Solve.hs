{- |
Module      : Morloc.Typecheck.Solve
Description : Discharge the class constraints a definition's body owes
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Each wanted is resolved against the givens and the instances
('Morloc.Frontend.Instances.resolve'). A selected instance may fix a class
argument the others determine (CLS-20); the equations it implies are
applied to gamma, and the wanteds still waiting are tried again, until a
pass fixes nothing more. The result does not depend on the order the
wanteds were recorded in. What is left undetermined or unmet is returned
with each wanted.
-}
module Morloc.Typecheck.Solve
  ( Residual (..)
  , Solution (..)
  , solve
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Frontend.Instances as FI
import Morloc.Frontend.Namespace
import Morloc.Typecheck.Internal (apply, subtype, typeUChildren, (+>))

-- | Why a wanted is left.
data Residual
  = -- | the class arguments hold variables nothing fixed (CLS-18)
    Undecided [TVar]
  | Unmet FI.ResolveError

data Solution = Solution
  { solEvidence :: IntMap.IntMap [Evidence]
  -- ^ by the term the wanted arose at
  , solResidual :: [(Wanted, Residual)]
  }

-- | Discharge wanteds. Constraints are resolved with the signature's
-- variables renamed so that they read as fixed types (@toRigid@), and the
-- equations a selected instance or a given implies are applied in gamma's
-- names (@fromRigid@).
solve ::
  FI.ResolvePolicy ->
  Scope ->
  InstanceEnv ->
  [Constraint] ->
  (Constraint -> Constraint) ->
  (TypeU -> TypeU) ->
  Gamma ->
  [Wanted] ->
  (Gamma, Solution)
solve policy scope env givens toRigid fromRigid g0 ws0 = go g0 IntMap.empty [] (reverse ws0)
  where
    closure = FI.superClosure env givens
    go g evs failed ws =
      let (g', evs', waiting, failed', improved) = foldl step (g, evs, [], failed, False) ws
       in if improved && not (null waiting)
            then go g' evs' failed' (reverse [w | (w, _) <- waiting])
            else (g', Solution evs' (reverse failed' <> [(w, Undecided vs) | (w, vs) <- reverse waiting]))

    step (g, evs, waiting, failed, improved) w =
      let (gGiven, byGiven) = fromGiven g (apply g (wPred w))
          c = apply gGiven (wPred w)
       in case FI.resolveClosed policy scope env closure (toRigid c) of
            Right ev ->
              let (g', changed) = improve gGiven c ev
               in (g', IntMap.insertWith (flip (<>)) (wSite w) [ev] evs, waiting, failed, improved || byGiven || changed)
            Left (FI.Undetermined _ vs) -> (gGiven, evs, (w, vs) : waiting, failed, improved || byGiven)
            Left e -> (gGiven, evs, waiting, (w, Unmet e) : failed, improved || byGiven)

    -- The dependent arguments a selected instance fixes (CLS-20), solved in
    -- gamma; whether that solved anything. Selection has already found
    -- them to agree with the instance; one gamma cannot solve (a variable
    -- the checker left, not an existential) stays as it is, as at a use.
    improve g (Constraint cls args) (EvInstance key tys _)
      | not (null (Map.findWithDefault [] cls (ieFundeps env))) =
          case FI.keyConstraintAt key tys of
            Constraint _ heads -> equate g args (map fromRigid heads)
            _ -> (g, False)
    improve g _ _ = (g, False)

    -- A given that matches on every selecting argument fixes the dependent
    -- ones (CLS-20).
    fromGiven g c@(Constraint _ args) = case FI.givenFixing env closure (toRigid c) of
      Just (Constraint _ ts) -> equate g args (map fromRigid ts)
      _ -> (g, False)
    fromGiven g _ = (g, False)

    equate g args ts =
      let g1 = foldl (+>) g [v | ExistU v _ _ <- concatMap everyType args]
       in case subtype scope (FunU args BT.unitU) (FunU ts BT.unitU) g1 of
            Right g2 -> (g2, map (apply g2) args /= map (apply g) args)
            Left _ -> (g, False)

    everyType t = t : concatMap everyType (typeUChildren t)
