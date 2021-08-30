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
  (_, Just lang) -> return $ f lang g0 t0
  where
    f :: Lang -> Maybe Type -> Type -> TypeP

    f lang (Just (UnkT (TV _ v1))) (UnkT (TV _ v2)) = UnkP (PV lang (Just v1) v2)
    f lang _ (UnkT (TV _ v)) = UnkP (PV lang Nothing v)

    f lang (Just (VarT (TV _ v1))) (VarT (TV _ v2)) = VarP (PV lang (Just v1) v2)
    f lang _ (VarT (TV _ v2)) = VarP (PV lang Nothing v2)

    f lang (Just (FunT t11 t12)) (FunT t21 t22)
      = FunP (f lang (Just t11) t21) (f lang (Just t12) t22)
    f lang _ (FunT t1 t2)
      = FunP (f lang Nothing t1) (f lang Nothing t2)

    f lang (Just (ArrT (TV _ v1) ts1)) (ArrT (TV _ v2) ts2)
      = ArrP (PV lang (Just v1) v2) (zipWith (f lang) (map Just ts1) ts2)
    f lang _ (ArrT (TV _ v) ts)
      = ArrP (PV lang Nothing v) (map (f lang Nothing) ts)

    f lang (Just (NamT _ (TV _ v1) ts1 rs1)) (NamT r2 (TV _ v2) ts2 rs2)
      = NamP r2 (PV lang (Just v1) v2) (zipWith (f lang) (map Just ts1) ts2)
      $ zip
        (zipWith (PV lang) (map (Just . fst) rs1) (map fst rs2))
        (zipWith (f lang) (map (Just . snd) rs1) (map snd rs2))
    f lang _ (NamT r (TV _ v) ts rs)
      = NamP r (PV lang Nothing v) (map (f lang Nothing) ts)
      $ zip
        (map (PV lang Nothing) (map fst rs))
        (map (f lang Nothing) (map snd rs))


weaveResolvedTypes :: Type -> Type -> MorlocMonad TypeP
weaveResolvedTypes g0 t0 = case (langOf g0, langOf t0) of
  (_, Nothing) -> MM.throwError . CallTheMonkeys
    $ "Expected a language-specific type as the second argument"
  (Just _, _) -> MM.throwError . CallTheMonkeys
    $ "Expected a general type as the first argument"
  (_, Just lang) -> return $ f lang g0 t0
  where
    f :: Lang -> Type -> Type -> TypeP
    f lang (UnkT (TV _ v1)) (UnkT (TV _ v2)) = UnkP (PV lang (Just v1) v2)
    f lang (VarT (TV _ v1)) (VarT (TV _ v2)) = VarP (PV lang (Just v1) v2)
    f lang (FunT t11 t12) (FunT t21 t22)
      = FunP (f lang t11 t21) (f lang t12 t22)
    f lang (ArrT (TV _ v1) ts1) (ArrT (TV _ v2) ts2)
      = ArrP (PV lang (Just v1) v2) (zipWith (f lang) ts1 ts2)
    f lang (NamT _ (TV _ v1) ts1 rs1) (NamT r2 (TV _ v2) ts2 rs2)
      = NamP r2 (PV lang (Just v1) v2) (zipWith (f lang) ts1 ts2)
      $ zip
        (zipWith (PV lang) (map (Just . fst) rs1) (map fst rs2))
        (zipWith (f lang) (map snd rs1) (map snd rs2))


weaveTypesGCP :: (Indexed Type) -> Type -> MorlocMonad TypeP
weaveTypesGCP (Idx i _) t = metaType i >>= (flip weaveTypes) t

weaveTypesGCM :: (Indexed Type) -> Type -> MorlocMonad TypeM
weaveTypesGCM (Idx i _) t = metaType i >>= (flip weaveTypes) t |>> typeP2typeM

typeP2typeM :: TypeP -> TypeM
typeP2typeM f@(FunP _ _) = case decompose f of
  (inputs, output) -> Function (map typeP2typeM inputs) (typeP2typeM output)
typeP2typeM (UnkP _) = Passthrough
typeP2typeM t = Native t
