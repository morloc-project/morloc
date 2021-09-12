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
w lang (Just (CatT gk t11 t12)) (CatT ck t21 t22)
  = CatP (mergeCat (Just gk) ck) (w lang (Just t11) t21) (w lang (Just t12) t22)
w lang Nothing (CatT k t1 t2)
  = CatP (mergeCat Nothing k) (w lang Nothing t1) (w lang Nothing t2)

mergeCat :: Maybe CatType -> CatType -> CatTypeP
mergeCat _ CatTypeFun = CatTypeFunP
mergeCat _ CatTypeArr = CatTypeArrP
mergeCat _ CatTypeEnt = CatTypeEntP
mergeCat Nothing (CatTypeRec t ps)
  = CatTypeRecP t [PV lang Nothing v | (TV (Just lang) v) <- ps]
mergeCat (Just (CatTypeRec _ psg)) (CatTypeRec t psc)
  = CatTypeRecP t [PV lang (Just vg) vc | (TV _ vg, TV (Just lang) vc) <- zip psg psc]
mergeCat _ _ = undefined

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
    f lang (CatT gk t11 t12) (CatT ck t21 t22) = CatP (mergeCat (Just gk) ck) (f lang t11 t21) (f lang t12 t22)

weaveTypesGCP :: (Indexed Type) -> Type -> MorlocMonad TypeP
weaveTypesGCP (Idx i _) t = metaType i >>= (flip weaveTypes) t

weaveTypesGCM :: (Indexed Type) -> Type -> MorlocMonad TypeM
weaveTypesGCM (Idx i _) t = metaType i >>= (flip weaveTypes) t |>> typeP2typeM

typeP2typeM :: TypeP -> TypeM
typeP2typeM f@(CatP CatTypeFunP _ _) = case decompose f of
  (inputs, output) -> Function (map typeP2typeM inputs) (typeP2typeM output)
typeP2typeM (UnkP _) = Passthrough
typeP2typeM t = Native t
