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
import qualified Morloc.Data.Text as DT
import Morloc.Typecheck.Internal (unqualify, qualify)
import Morloc.Frontend.Merge (mergeTermTypes, weaveTermTypes, mergeTypeclasses, unionTermTypes)


linkTypeclasses
  :: MVar
  -> ExprI
  -> [(m, e, Map.Map EVar (ClassName, [TVar], EType, [TermTypes]))]
  -> MorlocMonad (Map.Map EVar (ClassName, [TVar], EType, [TermTypes]))
linkTypeclasses _ e es
  -- Merge the typeclasses and instances from all imported modules
  -- These are inherited implicitly, so import terms are ignored
  = Map.unionsWithM mergeTypeclasses [x | (_,_,x) <- es]
  -- Augment the inherited map with the typeclasses and instances in this module
  >>= findTypeclasses e

findTypeclasses
  :: ExprI
  -> Map.Map EVar (ClassName, [TVar], EType, [TermTypes])
  -> MorlocMonad (Map.Map EVar (ClassName, [TVar], EType, [TermTypes]))
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

  _ <- updateTypeclasses moduleClasses

  mapM_ (linkVariablesToTypeclasses moduleClasses) es0

  return moduleClasses

  where
    -- make a map of all terms that are defined in a typeclass (these will all
    -- be general term)
    makeClass :: (ClassName, [TVar], [Signature]) -> Map.Map EVar (ClassName, [TVar], EType, [TermTypes])
    makeClass (cls, vs, sigs) = Map.fromList $ map makeClassTerm sigs where
      makeClassTerm :: Signature -> (EVar, (ClassName, [TVar], EType, [TermTypes]))
      makeClassTerm (Signature v _ t) = (v, (cls, vs, t, []))

    addInstance
      :: Map.Map EVar (ClassName, [TVar], EType, [TermTypes])
      -> (ClassName, [TypeU], [ExprI])
      -> MorlocMonad (Map.Map EVar (ClassName, [TVar], EType, [TermTypes]))
    addInstance clsmap (_, _, []) = return clsmap
    addInstance clsmap (cls0, ts0, es) = mapM f es |>> Map.fromListWith mergeInstances where
      f :: ExprI -> MorlocMonad (EVar, (ClassName, [TVar], EType, [TermTypes]))
      f (ExprI srcIndex (SrcE src)) =
        case Map.lookup (srcAlias src) clsmap of
          (Just (cls1, vs, generalType, otherInstances)) -> do
            when (cls1 /= cls0) (MM.throwError $ ConflictingClasses cls1 cls0 (srcAlias src))
            when (length vs /= length ts0) (MM.throwError $ InstanceSizeMismatch cls1 vs ts0)
            instanceType <- substituteInstanceTypes vs (etype generalType) ts0
            let newTerm = TermTypes (Just $ generalType {etype = instanceType}) [(moduleName, Idx srcIndex src)] []
                typeterms = weaveTermTypes newTerm otherInstances

            MM.sayVVV $ "addInstance src:"
                      <> "\n    v:" <+> pretty (srcAlias src)
                      <> "\n  cls:" <+> pretty cls1
                      <> "\n  generalType:" <+> pretty generalType
                      <> "\n  ts0:" <+> encloseSep "{" "}" ";" (map pretty ts0)
                      <> "\n  instanceType:" <+> pretty instanceType
                      <> "\n  newTerm:" <+> pretty newTerm

            return (srcAlias src, (cls0, vs, generalType, typeterms))
          Nothing ->  MM.throwError $ MissingTypeclassDefinition cls0 (srcAlias src)

      f (ExprI _ (AssE v e _)) =
        case Map.lookup v clsmap of
          (Just (cls1, vs, generalType, otherInstances)) -> do
            when (cls1 /= cls0) (MM.throwError $ ConflictingClasses cls1 cls0 v)
            when (length vs /= length ts0) (MM.throwError $ InstanceSizeMismatch cls1 vs ts0)
            instanceType <- substituteInstanceTypes vs (etype generalType) ts0
            let newTerm = TermTypes (Just $ generalType {etype = instanceType}) [] [e]
                typeterms = weaveTermTypes newTerm otherInstances

            MM.sayVVV $ "addInstance decl:"
                      <> "\n    v:" <+> pretty v
                      <> "\n  cls:" <+> pretty cls1
                      <> "\n  generalType:" <+> pretty generalType
                      <> "\n  ts0:" <+> encloseSep "{" "}" ";" (map pretty ts0)
                      <> "\n  instanceType:" <+> pretty instanceType
                      <> "\n  newTerm:" <+> pretty newTerm

            return (v, (cls0, vs, generalType, typeterms))
          Nothing -> MM.throwError $ MissingTypeclassDefinition cls0 v

      f (ExprI _ e) = MM.throwError $ IllegalExpressionInInstance cls0 ts0 e

      mergeInstances
        :: (ClassName, [TVar], EType, [TermTypes])
        -> (ClassName, [TVar], EType, [TermTypes])
        -> (ClassName, [TVar], EType, [TermTypes])
      mergeInstances (cls1, vs1, e1, ts1) (cls2, vs2, e2, ts2)
        | cls1 == cls2, length vs1 == length vs2, equivalent (etype e1) (etype e2) = (cls1, vs1, e1, unionTermTypes ts1 ts2)
        | otherwise = error "failed to merge"

    linkVariablesToTypeclasses
      :: Map.Map EVar (ClassName, [TVar], EType, [TermTypes])
      -> ExprI
      -> MorlocMonad ()
    linkVariablesToTypeclasses = link where
      link :: Map.Map EVar (ClassName, [TVar], EType, [TermTypes]) -> ExprI -> MorlocMonad ()
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

      setClass :: Map.Map EVar (ClassName, [TVar], EType, [TermTypes]) -> Int -> EVar -> MorlocMonad ()
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

      mapSources :: ClassName -> EVar -> EType -> TermTypes -> MorlocMonad ()
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

      mapExpressions :: ClassName -> EVar -> EType -> TermTypes -> MorlocMonad ()
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

    updateTypeclasses :: Map.Map EVar (ClassName, [TVar], EType, [TermTypes]) -> MorlocMonad ()
    updateTypeclasses m = do
      s <- MM.get
      newMap <- Map.unionWithM mergeTypeclasses m (stateTypeclasses s)
      MM.put (s {stateTypeclasses = newMap})

findTypeclasses _ _ = undefined


{- Substitute the instance types into the class function definition

Suppose we have the following class and instances:

class Reversible a b where
  forward :: a -> b
  backward :: b -> a

instance Reversible ([a],[b]) [(a,b)] where
  ...

If we are handling the single instance above for the `forward` function:

  classVars: [a, b]
  classType: forall a b . a -> b
  instanceParameters: forall a b . ([a], [b])
                      forall a b . [(a, b)]

and the return type should be

  forall a b . ([a],[b]) -> [(a,b)]

A problem here is that the instance parameters *share* qualifiers. The `a` and `b`
in the first instance parameter are the same as those in the second. But not the
same as the `a` and `b` in the class.


-}
substituteInstanceTypes :: [TVar] -> TypeU -> [TypeU] -> MorlocMonad TypeU
substituteInstanceTypes classVars classType instanceParameters = do

      -- find all qualifiers in the instance parameter list
  let instanceQualifiers = unique $ concatMap (fst . unqualify) instanceParameters

      -- rewrite the class type such that the class qualifiers appear first and
      -- do not conflict with parameter qualifiers
      cleanClassType = replaceQualifiers instanceQualifiers (putClassVarsFirst classType)

      -- substitute in the parameter types
      finalType = qualify instanceQualifiers
                $ substituteQualifiers cleanClassType (map (snd . unqualify) instanceParameters)

  MM.sayVVV $ "substituteInstanceTypes"
            <> "\n  classVars:" <+> pretty classVars
            <> "\n  classType:" <+> pretty classType
            <> "\n  instanceParameters:" <+> pretty instanceParameters
            <> "\n  -------------------"
            <> "\n  instanceQualifiers:" <+> pretty instanceQualifiers
            <> "\n  cleanClassType:" <+> pretty cleanClassType
            <> "\n  finalType:" <+> pretty finalType

  return finalType

  where
    putClassVarsFirst :: TypeU -> TypeU
    putClassVarsFirst t =
      let (vs, t') = unqualify t
      in qualify (classVars <> filter (`notElem` classVars) vs) t'

    replaceQualifiers :: [TVar] -> TypeU -> TypeU
    replaceQualifiers vs0 t0 = f vs0 [r | r <- freshVariables, r `notElem` doNotUse] t0
      where

      -- qualifiers to avoid when replacing
      doNotUse = vs0 <> (fst . unqualify) t0

      f (v:vs) (r:rs) (ForallU v' t)
        | v == v' = ForallU r . f vs rs $ substituteTVar v' (VarU r) t
        | otherwise = ForallU v' (f (v:vs) (r:rs) t)
      f _ _ t = t

      freshVariables = [1 ..] >>= flip replicateM ['a' .. 'z'] |>> TV . DT.pack

    substituteQualifiers :: TypeU -> [TypeU] -> TypeU
    substituteQualifiers (ForallU v t) (r:rs) = substituteQualifiers (substituteTVar v r t) rs
    substituteQualifiers t _ = t
