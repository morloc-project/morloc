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
  , typeP2typeM
) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Monad as MM
import Morloc.Data.Doc

weaveTypes :: Maybe Type -> Type -> MorlocMonad TypeP
weaveTypes g0 t0 = case (g0 >>= langOf, langOf t0) of
  (_, Nothing) -> MM.throwError . CallTheMonkeys . render
    $ "Expected a language-specific type as the second argument, found " <> viaShow (g0, t0)
  (Just _, _) -> MM.throwError . CallTheMonkeys . render
    $ "Expected a general type as the first argument, found" <> viaShow (g0, t0)
  (_, Just lang) -> return $ w lang g0 t0

weaveTypes' :: Type -> Type -> TypeP
weaveTypes' gt ct = w (langOf' ct) (Just gt) ct

w :: Lang -> Maybe Type -> Type -> TypeP
w lang (Just (UnkT (TV _ v1))) (UnkT (TV _ v2)) = UnkP (PV lang (Just v1) v2)
w lang _ (UnkT (TV _ v)) = UnkP (PV lang Nothing v)
w lang (Just (VarT (TV _ v1))) (VarT (TV _ v2)) = VarP (PV lang (Just v1) v2)
w lang _ (VarT (TV _ v2)) = VarP (PV lang Nothing v2)

w lang (Just (FunT ts1 t1)) (FunT ts2 t2)
  = FunP [w lang (Just gt) ct | (gt, ct) <- zip ts1 ts2] (w lang (Just t1) t2)
w lang Nothing (FunT ts t)
  = FunP (map (w lang Nothing) ts) (w lang Nothing t)

w lang (Just (AppT v1 ts1)) (AppT v2 ts2)
  = AppP (w lang (Just v1) v2) [w lang (Just gt) gc | (gt, gc) <- zip ts1 ts2]
w lang Nothing (AppT v ts)
  = AppP (w lang Nothing v) (map (w lang Nothing) ts)

w lang (Just (NamT o (TV _ n1) ps1 rs1)) (NamT _ (TV _ n2) ps2 rs2)
  = NamP o
         (PV lang (Just n1) n2)
         (zipWith (\(TV _ p1) (TV _ p2) -> PV lang (Just p1) p2) ps1 ps2)
         [(PV lang (Just k1) k2, w lang (Just t1) t2) | ((k1,t1),(k2,t2)) <- zip rs1 rs2]
w lang Nothing (NamT o (TV _ n) ps rs)
  = NamP o
         (PV lang Nothing n)
         [PV lang Nothing v | (TV _ v) <- ps]
         [(PV lang Nothing k, w lang Nothing t) | (k,t) <- rs]
w _ _ _ = error "impossible" -- the typechecker shouldn't let this happen


weaveResolvedTypes :: Type -> Type -> MorlocMonad TypeP
weaveResolvedTypes g0 t0 = case (langOf g0, langOf t0) of
  (_, Nothing) -> MM.throwError . CallTheMonkeys . render
    $ "Expected a language-specific type as the second argument, found " <> viaShow (g0, t0)
  (Just _, _) -> MM.throwError . CallTheMonkeys . render
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
          (zipWith (\(TV _ p1) (TV _ p2) -> PV lang (Just p1) p2) ps1 ps2)
          [(PV lang (Just k1) k2, f lang t1 t2) | ((k1, t1), (k2, t2)) <- zip rs1 rs2]
    f lang t1 t2 = error $ "General and concrete types are not compatible: " <> show (lang, t1, t2)

typeP2typeM :: TypeP -> TypeM
typeP2typeM (FunP ts t) = Function (map typeP2typeM ts) (typeP2typeM t)
typeP2typeM (UnkP _) = Passthrough
typeP2typeM t = Native t
