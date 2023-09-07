{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.Internal
Description : Miscellaneous backend utilities
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Internal
(
    weaveTypes
  , weaveTypes'
  , weaveResolvedTypes
) where

import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import Control.Monad.Except (Except, throwError)

weaveTypes :: Maybe Type -> Type -> Except MDoc TypeP
weaveTypes g0 t0 = case (g0 >>= langOf, langOf t0) of
  (_, Nothing) -> throwError
    $ "Expected a language-specific type as the second argument, found " <> viaShow (g0, t0)
  (Just _, _) -> throwError
    $ "Expected a general type as the first argument, found" <> viaShow (g0, t0)
  (_, Just lang) -> return $ w lang g0 t0

weaveTypes' :: Type -> Type -> TypeP
weaveTypes' gt ct = w (langOf' ct) (Just gt) ct

w :: Lang -> Maybe Type -> Type -> TypeP
w lang (Just (UnkT (TV _ v1))) (UnkT (TV _ v2)) = UnkP (PV lang (Just v1) v2)
w lang (Just g) (UnkT (TV _ v)) = UnkP (PV lang (Just passthroughGen) v) where
  passthroughGen = render $ "*" <> parens (pretty g)
w lang _ (UnkT (TV _ v)) = UnkP (PV lang (Just "*1") v)
w lang (Just (VarT (TV _ v1))) (VarT (TV _ v2)) = VarP (PV lang (Just v1) v2)
w lang _ (VarT (TV _ v2)) = VarP (PV lang (Just "*2") v2)

w lang (Just (FunT ts1 t1)) (FunT ts2 t2)
  = FunP [w lang (Just gt) ct | (gt, ct) <- zip ts1 ts2] (w lang (Just t1) t2)
w lang Nothing (FunT ts t)
  = FunP (map (w lang (Just (UnkT (TV Nothing "*3")))) ts) (w lang (Just (UnkT (TV Nothing "*4"))) t)

w lang (Just (AppT v1 ts1)) (AppT v2 ts2)
  = AppP (w lang (Just v1) v2) [w lang (Just gt) gc | (gt, gc) <- zip ts1 ts2]
w lang Nothing (AppT v ts)
  = AppP (w lang (Just (UnkT (TV Nothing "*4"))) v) (map (w lang (Just (UnkT (TV Nothing "*4")))) ts)

w lang (Just (NamT o (TV _ n1) ps1 rs1)) (NamT _ (TV _ n2) ps2 rs2)
  = NamP o
         (PV lang (Just n1) n2)
         (zipWith (w lang) (map Just ps1) ps2)
         [(PV lang (Just k1) k2, w lang (Just t1) t2) | ((k1,t1),(k2,t2)) <- zip rs1 rs2]
w lang Nothing (NamT o (TV _ n) ps rs)
  = NamP o
         (PV lang Nothing n)
         (map (w lang Nothing) ps) -- (zipWith (w lang) (repeat Nothing) ps)
         [(PV lang Nothing k, w lang Nothing t) | (k,t) <- rs]
w _ _ _ = error "impossible" -- the typechecker shouldn't let this happen


weaveResolvedTypes
    :: Type -- ^ general type (optional)
    -> Type -- ^ concrete type (required)
    -> Either MT.Text TypeP
weaveResolvedTypes g0 t0 = do
  case (langOf g0, langOf t0) of
    (_, Nothing) -> Left . render
      $ "Expected a language-specific type as the second argument, found " <> viaShow (g0, t0)
    (Just _, _) -> Left . render
      $ "Expected a general type as the first argument, found" <> viaShow (g0, t0)
    (Nothing, Just lang) -> return $ f lang g0 t0
  where
    f :: Lang -> Type -> Type -> TypeP
    f lang (UnkT (TV _ v1)) (UnkT (TV _ v2)) = UnkP (PV lang (Just v1) v2)
    f lang (VarT (TV _ v1)) (VarT (TV _ v2)) = VarP (PV lang (Just v1) v2)
    f lang (FunT ts1 t1) (FunT ts2 t2)
      = FunP (zipWith (f lang) ts1 ts2) (f lang t1 t2)
    f lang (AppT v1 ts1) (AppT v2 ts2)
      = AppP (f lang v1 v2) (zipWith (f lang) ts1 ts2)
    f lang (NamT o (TV _ n1) ps1 rs1) (NamT _ (TV _ n2) ps2 rs2)
      = NamP o (PV lang (Just n1) n2)
          (zipWith (f lang) ps1 ps2)
          [(PV lang (Just k1) k2, f lang t1 t2) | ((k1, t1), (k2, t2)) <- zip rs1 rs2]
    f lang g (UnkT (TV _ v)) = UnkP (PV lang (Just passthroughGen) v) where
      passthroughGen = render $ "*" <> parens (pretty g)
    f _ t1 t2 = error . MT.unpack . render
        $ "General and concrete types are not compatible: "
        <> "\n  " <> viaShow t1
        <> "\n  " <> viaShow t2
