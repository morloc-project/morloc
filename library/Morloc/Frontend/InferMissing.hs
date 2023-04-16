{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.Frontend.InferMissing
Description : Infer missing concrete types from general types
Copyright   : (c) Zebulun Arendsee, 2023
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.InferMissing (inferMissingTypes) where

import Morloc.Frontend.Namespace
import qualified Data.Map as Map
import qualified Morloc.Frontend.Lang.DefaultTypes as Def
import qualified Morloc.Monad as MM
import qualified Morloc.Data.GMap as GMap
import Morloc.Frontend.Desugar (desugarType)

inferMissingTypes :: a -> MorlocMonad a
inferMissingTypes x = do
    MM.modify infer
    return x

-- I also need to synthesize stateAnnotations

infer :: MorlocState -> MorlocState 
infer s = do
    s {stateSignatures = GMap.mapVals (processTermType (stateTypedefs s)) (stateSignatures s)}


processTermType :: GMap Int MVar (Map.Map TVar [([TVar], TypeU)]) -> TermTypes -> TermTypes
processTermType (GMap _ typedefs) (TermTypes (Just g) cs ds) =
    let cs' = [(mv, processTypes (Map.lookup mv typedefs) g es maySrc, maySrc) | (mv, es, maySrc) <- cs]
    in TermTypes (Just g) cs' ds
processTermType _ t = t 


processTypes :: Maybe (Map.Map TVar [([TVar], TypeU)]) -> EType -> [EType] -> Maybe (Indexed Source) -> [EType]
-- if there are no given concrete types, try to synthesize one from the general type
processTypes (Just typedefs) g [] (Just (Idx _ (srcLang -> lang))) =
    case synthesizeEType lang typedefs g of
        (Left _) -> []
        (Right e) -> [e]
processTypes _ _ es _ = es


synthesizeEType :: Lang -> Map.Map TVar [([TVar], TypeU)] -> EType -> Either MorlocError EType
synthesizeEType lang typedefs (EType t0 ps cs) = EType <$> desugarType typedefs (switchLang t0) <*> pure ps <*> pure cs where
    switchLang :: TypeU -> TypeU
    switchLang (VarU (TV _ v)) = VarU (TV (Just lang) v)
    switchLang (ForallU (TV _ v) t) = ForallU (TV (Just lang) v) (switchLang t)
    switchLang (FunU ts t) = FunU (map switchLang ts) (switchLang t)
    switchLang (AppU t ts) = AppU (switchLang t) (map switchLang ts)
    switchLang (ExistU (TV _ v) ts ds rs) =
        let rs' = map (switchLang . snd) rs
            ts' = map switchLang ts
            ds' = map switchLang ds
            v' = Def.generalDefaultToConcrete v (length ts) lang
        in ExistU (TV (Just lang) v') ts' ds' (zip (map fst rs) rs')
    switchLang (NamU n (TV _ v) ts rs) =
        let ts' = map switchLang ts
            rs' = map (switchLang . snd) rs
        in NamU n (TV (Just lang) v) ts' (zip (map fst rs) rs')
