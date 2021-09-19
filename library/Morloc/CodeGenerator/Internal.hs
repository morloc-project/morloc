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
  , weaveTypesGCP
  , weaveTypesGCM
  , typeP2typeM
) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Monad as MM

weaveTypes :: Maybe Type -> Type -> MorlocMonad TypeP
weaveTypes g0 t0 = case (g0 >>= langOf, langOf t0) of
  (_, Nothing) -> MM.throwError . CallTheMonkeys
    $ "Expected a language-specific type as the second argument"
  (Just _, _) -> MM.throwError . CallTheMonkeys
    $ "Expected a general type as the first argument"
  (_, Just lang) -> return $ w lang g0 t0

weaveTypes' :: Type -> Type -> TypeP
weaveTypes' gt ct = w (langOf' ct) (Just gt) ct

w :: Lang -> Maybe Type -> Type -> TypeP
w lang _ NulT = NulP
w lang (Just (UnkT (TV _ v1))) (UnkT (TV _ v2)) = UnkP (PV lang (Just v1) v2)
w lang _ (UnkT (TV _ v)) = UnkP (PV lang Nothing v)
w lang (Just (VarT (TV _ v1))) (VarT (TV _ v2)) = VarP (PV lang (Just v1) v2)
w lang _ (VarT (TV _ v2)) = VarP (PV lang Nothing v2)

w lang (Just (FunT t11 t12)) (FunT t21 t22)
  = FunP (w lang (Just t11) t21) (w lang (Just t12) t22)
w lang Nothing (FunT t1 t2)
  = FunP (w lang Nothing t1) (w lang Nothing t2)

w lang (Just (AppT t11 t12)) (AppT t21 t22)
  = AppP (w lang (Just t11) t21) (w lang (Just t12) t22)
w lang Nothing (AppT t1 t2)
  = AppP (w lang Nothing t1) (w lang Nothing t22)

w lang (Just (RecT r1 t11 k1 t12)) (RecT r2 t21 k2 t22)
  = RecP r1 (w lang (Just t11) t21) (PV lang (Just k1) k2) (w lang (Just t12) t22)
w lang Nothing (RecT r2 t21 k2 t22)
  = RecP r1 (w lang Nothing t21) (PV lang Nothing k2) (w lang Nothing t22)


weaveResolvedTypes :: Type -> Type -> MorlocMonad TypeP
weaveResolvedTypes g0 t0 = case (langOf g0, langOf t0) of
  (_, Nothing) -> MM.throwError . CallTheMonkeys
    $ "Expected a language-specific type as the second argument"
  (Just _, _) -> MM.throwError . CallTheMonkeys
    $ "Expected a general type as the first argument"
  (Nothing, Just lang) -> return $ f lang g0 t0
  where
    f :: Lang -> Type -> Type -> TypeP
    f _ NulT NulT = NulP
    f lang (UnkT (TV _ v1)) (UnkT (TV _ v2)) = UnkP (PV lang (Just v1) v2)
    f lang (VarT (TV _ v1)) (VarT (TV _ v2)) = VarP (PV lang (Just v1) v2)
    f lang (FunT t11 t12) (FunT t21 t22) = FunP (f lang t11 t21) (f lang t12 t22)
    f lang (AppT t11 t12) (AppT t21 t22) = AppP (f lang t11 t21) (f lang t12 t22)
    f lang (RecT r t11 k1 t12) (RecT _ t21 k2 t22)
      = RecP r (f lang t11 t21) (PV lang (Just k1) k2) (f lang t12 t22)

weaveTypesGCP :: (Indexed Type) -> Type -> MorlocMonad TypeP
weaveTypesGCP (Idx i _) t = metaType i >>= (flip weaveTypes) t

weaveTypesGCM :: (Indexed Type) -> Type -> MorlocMonad TypeM
weaveTypesGCM (Idx i _) t = metaType i >>= (flip weaveTypes) t |>> typeP2typeM

typeP2typeM :: TypeP -> TypeM
typeP2typeM (FunP t1 t2) = case typeP2typeM t2 of
  (Function ins out) -> Function (t1:ins) out 
  out -> Function [t1] out
typeP2typeM (UnkP _) = Passthrough
typeP2typeM t = Native t
