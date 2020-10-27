{-|
Module      : Morloc.CodeGenerator.Internal
Description : Short description
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Internal
(
    weaveTypes
  , weaveTypesGCP
  , weaveTypesGCM
  , typeP2typeM
  , typePartsP
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

    f lang (Just (NamT _ (TV _ v1) rs1)) (NamT r2 (TV _ v2) rs2)
      = NamP r2 (PV lang (Just v1) v2)
      $ zip
        (zipWith (PV lang) (map (Just . fst) rs1) (map fst rs2))
        (zipWith (f lang) (map (Just . snd) rs1) (map snd rs2))
    f lang _ (NamT r (TV _ v) rs)
      = NamP r (PV lang Nothing v)
      $ zip
        (map (PV lang Nothing) (map fst rs))
        (map (f lang Nothing) (map snd rs))


weaveTypesGCP :: GMeta -> CType -> MorlocMonad TypeP
weaveTypesGCP g (CType t) = weaveTypes (unGType <$> metaGType g) t

weaveTypesGCM :: GMeta -> CType -> MorlocMonad TypeM
weaveTypesGCM g (CType t) = typeP2typeM <$> weaveTypes (unGType <$> metaGType g) t

typeP2typeM :: TypeP -> TypeM
typeP2typeM f@(FunP _ _) = case typePartsP f of
  (inputs, output) -> Function (map typeP2typeM inputs) (typeP2typeM output)
typeP2typeM (UnkP _) = Passthrough
typeP2typeM t = Native t

-- get input types to a function type
typePartsP :: TypeP -> ([TypeP], TypeP)
typePartsP t0 = case reverse $ typeArgs t0 of
  [] -> error "This should not be possible"
  [t] -> ([], t)
  (t:ts) -> (reverse ts, t)
  where
    typeArgs (FunP t1 t2) = t1 : typeArgs t2
    typeArgs t = [t]
