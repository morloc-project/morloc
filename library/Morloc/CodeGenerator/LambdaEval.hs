{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.LambdaEval
Description : Evaluate all applied lambdas
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.LambdaEval (
    applyLambdas
) where

import Morloc.CodeGenerator.Namespace

-- {- | Remove lambdas introduced through substitution
--
-- For example:
--
--  bif x = add x 10
--  bar py :: "int" -> "int"
--  bar y = add y 30
--  f z = bar (bif z)
--
-- In Treeify.hs, the morloc declarations will be substituted in as lambdas. But
-- we want to preserve the link to any annotations (in this case, the annotation
-- that `bar` should be in terms of python ints). The morloc declarations can be
-- substituted in as follows:
--
--  f z = (\y -> add y 30) ((\x -> add x 10) z)
--
-- The indices for bif and bar that link the annotations to the functions are
-- relative to the lambda expressions, so this substitution preserves the link.
-- Typechecking can proceed safely.
--
-- The expression can be simplified:
--
--  f z = (\y -> add y 30) ((\x -> add x 10) z)
--  f z = (\y -> add y 30) (add z 10)            -- [z / x]
--  f z = add (add z 10) 30                      -- [add z 10 / y]
--
-- The simplified expression is what should be written in the generated code. It
-- would also be easier to typecheck and debug. So should these substitutions be
-- done immediately after parsing? We need to preserve
--  1. links to locations in the original source code (for error messages)
--  2. type annotations.
--  3. declaration names for generated comments and subcommands
--
-- Here is the original expression again, but annotated and indexed
--
--  (\x -> add_2 x_3 10_4)_1
--  (\y -> add_6 y_7 30_8)_5
--  (\z -> bar_10 (bif_11 z_12))_9
--
--  1: name="bif"
--  5: name="bar", type="int"@py -> "int"@py
--  9: name="f"
--
-- Each add is also associated with a type defined in a signature in an
-- unmentioned imported library, but those will be looked up by the typechecker
-- and will not be affected by rewriting.
--
-- Substitution requires reindexing. A definition can be used multiple times and
-- we need to distinguish between the use cases.
--
-- Replace bif and bar with their definition and create fresh indices:
--
--  (\z -> (\y -> add_18 y_19 30_20)_17 ((\x -> add_14 x_15 10_16)_13 z_12)_9
--
--  13,1: name="bif"
--  17,5: name="bar", type="int"@py -> "int"@py
--  9: name="f"
--
-- Now we can substitute for y
--
--  (\z -> add_18 ((\x -> add_14 x_15 10_16)_13 z_12)_9 30_20)
--
-- But this destroyed index 17 and the link to the python annotation. We can
-- preserve the type by splitting the annotation of bar.
--
--  13,1: name="bif"
--  18,17,5: name="bar"
--  12: "int"@py
--  13: "int"@py
--  9: name="f"
--
-- Index 18 should be associated with the *name* "bar", but not the type, since it
-- has been applied. The type of bar is now split between indices 12 and 13.
--
-- This case works fine, but it breaks down when types are polymorphic. If the
-- annotation of bar had been `a -> a`, then how would we type 12 and 13? We can't
-- say that `12 :: forall a . a` and `13 :: forall a . a`, since this
-- eliminates the constraint that the `a`s must be the same.
--
-- If instead we rewrite lambdas after typechecking, then everything works out.
--
-- Thus applyLambdas is done here, rather than in Treeify.hs or Desugar.hs.
--
-- Lambda application can also NOT be done before collapsing from Many to One in
-- AnnoS. The reason is that in ((VarS (Many es)) 42), the values in es
-- may contain `CallS src` or `LamS vs e` types. The CallS terms cannot be
-- reduced but the lambdas can. So applying here would lead to divergence.
--
-- It also must be done BEFORE conversion to ExprM in `express`, where manifolds
-- are resolved.
-- -}
applyLambdas
  :: AnnoS (Indexed Type) One a
  -> MorlocMonad (AnnoS (Indexed Type) One a)
-- eliminate empty lambdas
applyLambdas (AnnoS g1 _ (AppS (AnnoS _ _ (LamS [] (AnnoS _ c2 e))) [])) = applyLambdas $ AnnoS g1 c2 e

-- eliminate empty applications
applyLambdas (AnnoS g1 _ (AppS (AnnoS _ c2 e) [])) = applyLambdas $ AnnoS g1 c2 e

-- substitute applied lambdas
applyLambdas
  (AnnoS i1 tb1
    ( AppS
      ( AnnoS
          (Idx i2 (FunT (_:tas) tb2))
          c
          (LamS (v:vs) e2)
      )
      ( e1:es )
    )
  ) = let e2' = substituteAnnoS v e1 e2
      in applyLambdas
          (AnnoS i1 tb1
            ( AppS
              ( AnnoS
                  (Idx i2 (FunT tas tb2))
                  c
                  (LamS vs e2')
              )
              es
            )
          )

-- propagate the changes
applyLambdas (AnnoS g c (AppS f es)) = do
  f' <- applyLambdas f
  es' <- mapM applyLambdas es
  return (AnnoS g c (AppS f' es'))
applyLambdas (AnnoS g c (LamS vs e)) = AnnoS g c . LamS vs <$> applyLambdas e
applyLambdas (AnnoS g c (LstS es)) = AnnoS g c . LstS <$> mapM applyLambdas es
applyLambdas (AnnoS g c (TupS es)) = AnnoS g c . TupS <$> mapM applyLambdas es
applyLambdas (AnnoS g c (NamS rs)) = AnnoS g c . NamS <$> mapM (secondM applyLambdas) rs
applyLambdas (AnnoS g c (VarS v (One e))) = AnnoS g c . VarS v . One <$> applyLambdas e
applyLambdas x = return x

substituteAnnoS
  :: EVar
  -> AnnoS (Indexed Type) One a
  -> AnnoS (Indexed Type) One a
  -> AnnoS (Indexed Type) One a
substituteAnnoS v r = f where
  f e@(AnnoS _ _ (BndS v'))
    | v == v' = r
    | otherwise = e
  -- propagate the changes
  f (AnnoS g c (AppS e es)) =
    let f' = f e
        es' = map f es
    in AnnoS g c (AppS f' es')
  f e0@(AnnoS g c (LamS vs e))
    | v `elem` vs = e0 -- the replacement term is shadowed
    | otherwise =
        let e' = f e
        in AnnoS g c (LamS vs e')
  f (AnnoS g c (LstS es)) =
    let es' = map f es
    in AnnoS g c (LstS es')
  f (AnnoS g c (TupS es)) =
    let es' = map f es
    in AnnoS g c (TupS es')
  f (AnnoS g c (NamS rs)) =
    let es' = map (f . snd) rs
    in AnnoS g c (NamS (zip (map fst rs) es'))
  f x = x
