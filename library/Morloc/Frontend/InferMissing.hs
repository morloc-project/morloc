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
import qualified Data.Set as Set
import Morloc.Frontend.Desugar (desugarType)

inferMissingTypes :: a -> MorlocMonad a
inferMissingTypes x = do
    MM.get >>= infer >>= MM.put
    return x

-- TODO: I also need to synthesize stateAnnotations

infer :: MorlocState -> MorlocMonad MorlocState
infer s = do
    sigs <- GMap.mapValsM (processTermType (stateTypedefs s)) (stateSignatures s)
    return $ s {stateSignatures = sigs}


processTermType :: GMap Int MVar (Map.Map TVar [([TVar], TypeU)]) -> TermTypes -> MorlocMonad TermTypes
processTermType (GMap _ typedefs) (TermTypes (Just g) cs ds) = do
    case mapM (\(mv, es, maySrc) -> processTypes (Map.lookup mv typedefs) g es maySrc) cs of
        (Left e) -> MM.throwError e
        (Right ess') -> return $ TermTypes (Just g) (zipWith (\e (m, _, s) -> (m, e, s)) ess' cs) ds 
processTermType _ t = return t 


processTypes :: Maybe (Map.Map TVar [([TVar], TypeU)]) -> EType -> [EType] -> Maybe (Indexed Source) -> Either MorlocError [EType]
processTypes Nothing g [] (Just (Idx _ src)) = Left (CannotSynthesizeConcreteType src (etype g))
-- if there are no given concrete types, try to synthesize one from the general type
processTypes (Just typedefs) g [] (Just (Idx _ src)) = return <$> synthesizeEType src typedefs g
processTypes _ _ es _ = return es


synthesizeEType :: Source -> Map.Map TVar [([TVar], TypeU)] -> EType -> Either MorlocError EType
synthesizeEType src@(srcLang -> lang) typedefs (EType t0 ps cs)
  | canSynth = EType <$> desugarType typedefs (switchLang t0) <*> pure ps <*> pure cs
  | otherwise = Left (CannotSynthesizeConcreteType src t0)
  where
    -- true if all free general variables in the type have aliases in the concrete language
    canSynth = all isJust [Map.lookup (TV (Just lang) v) typedefs | VarU (TV _ v) <- Set.toList (free t0)]

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
