{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.Classify
Description : Collect typeclasses
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Classify (linkTypeclasses) where

import Morloc.Frontend.Namespace
import Morloc.Data.Doc
import qualified Control.Monad.State as CMS
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.GMap as GMap
import Morloc.Frontend.Merge (mergeTermTypes, weaveTermTypes, mergeTypeclasses, unionTermTypes)


linkTypeclasses
  :: MVar
  -> ExprI
  -> [(m, e, Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]))]
  -> MorlocMonad (Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]))
linkTypeclasses _ e es
  -- Merge the typeclasses and instances from all imported modules
  -- These are inherited implicitly, so import terms are ignored
  = Map.unionsWithM mergeTypeclasses [x | (_,_,x) <- es]
  -- Augment the inherited map with the typeclasses and instances in this module
  >>= findTypeclasses e


findTypeclasses
  :: ExprI
  -> Map.Map EVar (Typeclass, [TVar], EType, [TermTypes])
  -> MorlocMonad (Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]))
findTypeclasses (ExprI _ (ModE moduleName es0)) priorClasses = do

  -- first we collect all typeclass definitions in this module
  -- typeclasses are defined only at the top-level, so no descent into sub-expressions
  localClasses <- Map.unionsWithM mergeTypeclasses
                . map makeClass
                $ [(cls, vs, sigs) | (ExprI _ (ClsE cls vs sigs)) <- es0]

  -- then merge them with all prior typeclasses and instances
  allClasses <- Map.unionWithM mergeTypeclasses priorClasses localClasses

  -- find instances in this module
  -- The (IstE cls ts es) terms refer to
  --   cls: typeclass, such as "Packable"
  --   ts: types, such as ["Map a b", "[(a,b)]"]
  --   es: instance definitions, such as source statements (the only ones
  --       allowed at the moment)
  let instances = [(cls, ts, es) | (ExprI _ (IstE cls ts es)) <- es0]

  -- fold the instances into the current typeclass map and return
  moduleClasses <- foldlM addInstance allClasses instances

  MM.sayVVV $ "moduleClasses:"
         <+> list (
           map ( \ (v, (cls,vs,et,ts))
                 -> pretty v <+> "="
                 <+> pretty cls
                 <+> pretty vs
                 <+> parens (pretty (etype et))
                 <+> list (map pretty ts)
              ) (Map.toList moduleClasses)
         )

  mapM_ (linkVariablesToTypeclasses moduleClasses) es0

  return moduleClasses

  where
    -- make a map of all terms that are defined in a typeclass (these will all
    -- be general term)
    makeClass :: (Typeclass, [TVar], [Signature]) -> Map.Map EVar (Typeclass, [TVar], EType, [TermTypes])
    makeClass (cls, vs, sigs) = Map.fromList $ map makeClassTerm sigs where
      makeClassTerm :: Signature -> (EVar, (Typeclass, [TVar], EType, [TermTypes]))
      makeClassTerm (Signature v _ t) = (v, (cls, vs, t, []))

    addInstance
      :: Map.Map EVar (Typeclass, [TVar], EType, [TermTypes])
      -> (Typeclass, [TypeU], [ExprI])
      -> MorlocMonad (Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]))
    addInstance clsmap (_, _, []) = return clsmap
    addInstance clsmap (cls0, ts0, es) = mapM f es |>> Map.fromListWith mergeInstances where
      f :: ExprI -> MorlocMonad (EVar, (Typeclass, [TVar], EType, [TermTypes]))
      f (ExprI srcIndex (SrcE src)) =
        case Map.lookup (srcAlias src) clsmap of
          (Just (cls1, vs, generalType, otherInstances)) -> do
            MM.sayVVV $ "Adding SrcE instance:" <+> pretty (srcAlias src) <+> pretty srcIndex
            when (cls1 /= cls0) (error "Conflicting instances")
            when (length vs /= length ts0) (error "Conflicting class and instance parameter count")
            let instanceType = generalType { etype = foldl (\t (v,r) -> substituteTVar v r t) (requalify vs (etype generalType)) (zip vs ts0) }
            let newTerm = TermTypes (Just instanceType) [(moduleName, Idx srcIndex src)] []
            let typeterms = weaveTermTypes newTerm otherInstances
            return (srcAlias src, (cls0, vs, generalType, typeterms))
          Nothing -> error "No typeclass found for instance"

      f (ExprI assIdx (AssE v e _)) =
        case Map.lookup v clsmap of
          (Just (cls1, vs, generalType, otherInstances)) -> do
            MM.sayVVV $ "Adding AssE instance:" <+> pretty v <+> pretty assIdx
            when (cls1 /= cls0) (error "Conflicting instances")
            when (length vs /= length ts0) (error "Conflicting class and instance parameter count")
            let instanceType = generalType { etype = foldl (\t (v',r) -> substituteTVar v' r t) (requalify vs (etype generalType)) (zip vs ts0) }
            let newTerm = TermTypes (Just instanceType) [] [e]
            let typeterms = weaveTermTypes newTerm otherInstances
            return (v, (cls0, vs, generalType, typeterms))
          Nothing -> error "No typeclass found for instance"

      f _ = error "Only source statements are currently allowed in instances (generalization is in development)"

      mergeInstances
        :: (Typeclass, [TVar], EType, [TermTypes])
        -> (Typeclass, [TVar], EType, [TermTypes])
        -> (Typeclass, [TVar], EType, [TermTypes])
      mergeInstances (cls1, vs1, e1, ts1) (cls2, vs2, e2, ts2)
        | cls1 == cls2, length vs1 == length vs2, equivalent (etype e1) (etype e2) = (cls1, vs1, e1, unionTermTypes ts1 ts2)
        | otherwise = error "failed to merge"

      requalify :: [TVar] -> TypeU -> TypeU
      requalify (v:vs) (ForallU v' t)
        | v == v' = requalify vs t
        | otherwise = ForallU v' (requalify vs t)
      requalify _ t = t

    linkVariablesToTypeclasses
      :: Map.Map EVar (Typeclass, [TVar], EType, [TermTypes])
      -> ExprI
      -> MorlocMonad ()
    linkVariablesToTypeclasses = link where
      link :: Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]) -> ExprI -> MorlocMonad ()
      -- The following may have terms from typeclasses
      -- 1. variables
      link m (ExprI i (VarE v)) = setClass m i v
      -- recurse into assignments, allow shadowing of typeclass functions (TODO: warn)
      link m (ExprI _ (AssE _ (ExprI _ (LamE ks e)) es)) = do
        -- shadow all terms bound under the lambda
        let m' = foldr Map.delete m ks
        mapM_ (link m') (e:es)
      link m (ExprI _ (AssE _ e es)) = mapM_ (link m) (e:es)
      -- modules currently cannot be nested (should this be allowed?)
      link _ (ExprI _ (ModE v _)) = MM.throwError $ NestedModule v
      -- everything below boilerplate
      link m (ExprI _ (AccE _ e)) = link m e
      link m (ExprI _ (LstE xs)) = mapM_ (link m) xs
      link m (ExprI _ (TupE xs)) = mapM_ (link m) xs
      link m (ExprI _ (LamE vs e)) = link (foldr Map.delete m vs) e
      link m (ExprI _ (AppE f es)) = link m f >> mapM_ (link m) es
      link m (ExprI _ (AnnE e _)) = link m e
      link m (ExprI _ (NamE rs)) = mapM_ (link m . snd) rs
      link _ _ = return ()

      setClass :: Map.Map EVar (Typeclass, [TVar], EType, [TermTypes]) -> Int -> EVar -> MorlocMonad ()
      setClass m termIndex v = case Map.lookup v m of
        (Just (cls, _, t, ts)) -> do

          MM.sayVVV $ "setClass map:" <+> viaShow m

          mapM_ (mapSources cls v t) ts
          mapM_ (mapExpressions cls v t) ts

          s <- CMS.get
          -- Yes, both indices are termIndex. After typechecking, the
          -- polymorphic type will resolve to monomorphic. Each may resolve
          -- differently, so instances must not all point to the same signature.
          newMap <- GMap.insertWithM mergeSignatureSet termIndex termIndex (Polymorphic cls v t ts) (stateSignatures s)
          CMS.put (s { stateSignatures = newMap
                     , stateName = Map.insert termIndex v (stateName s)})
          return ()
        Nothing -> return ()

      mapSources :: Typeclass -> EVar -> EType -> TermTypes -> MorlocMonad ()
      mapSources cls v gt t = mapM_ (mapSource . snd) (termConcrete t) where
        mapSource :: Indexed Source -> MorlocMonad ()
        mapSource (Idx i src) = do
          let t' = TermTypes (termGeneral t) [(mv, srcidx) | (mv, srcidx) <- termConcrete t, val srcidx == src] []
          MM.sayVVV $ "mapSource" <+> pretty i <+> pretty src
                    <> "\n  termGeneral t:" <+> pretty (termGeneral t)
                    <> "\n  termGeneral t':" <+> pretty (termGeneral t')
                    <> "\n  length (termConcrete t):" <+> pretty (length (termConcrete t))
                    <> "\n  length (termConcrete t'):" <+> pretty (length (termConcrete t'))
          s <- CMS.get
          newMap <- GMap.insertWithM mergeSignatureSet i i (Polymorphic cls v gt [t']) (stateSignatures s)
          CMS.put (s { stateSignatures = newMap })
          return ()

      mapExpressions :: Typeclass -> EVar -> EType -> TermTypes -> MorlocMonad ()
      mapExpressions cls v gt t = mapM_ mapExpression (termDecl t) where
        mapExpression :: ExprI -> MorlocMonad ()
        mapExpression (ExprI i _) = do
          MM.sayVVV $ "mapExpression" <+> pretty i
          s <- CMS.get
          let t' = TermTypes (termGeneral t) [] [e | e@(ExprI i' _) <- termDecl t, i' == i]
          newMap <- GMap.insertWithM mergeSignatureSet i i (Polymorphic cls v gt [t']) (stateSignatures s)
          CMS.put (s { stateSignatures = newMap })
          return ()

      mergeSignatureSet :: SignatureSet -> SignatureSet -> MorlocMonad SignatureSet
      mergeSignatureSet (Polymorphic cls1 v1 t1 ts1) (Polymorphic cls2 v2 t2 ts2)
        | cls1 == cls2 && equivalent (etype t1) (etype t2) && v1 == v2 = return $ Polymorphic cls1 v1 t1 (unionTermTypes ts1 ts2)
        | otherwise = error "Invalid SignatureSet merge"
      mergeSignatureSet (Monomorphic ts1) (Monomorphic ts2) = Monomorphic <$> mergeTermTypes ts1 ts2
      mergeSignatureSet _ _ = undefined
findTypeclasses _ _ = undefined
