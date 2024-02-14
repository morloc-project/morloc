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
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Text as DT
import Morloc.Typecheck.Internal (unqualify, qualify)
import Morloc.Frontend.Merge (weaveTermTypes, mergeTypeclasses, mergeSignatureSet, mergeFirstIndexM)


-- Merge two indexed instances keeping the left index
mergeIndexedInstances = mergeFirstIndexM mergeTypeclasses

-- Handle typeclasses
--
--  * Write all global typeclasses into state, this is needed currently just for
--    serialization
--
--  * Associate each variable with polymorphic
linkTypeclasses
  :: MVar
  -> ExprI
  -> [(m, [(EVar, EVar)], Map.Map EVar (Indexed Instance))]
  -> MorlocMonad (Map.Map EVar (Indexed Instance))
linkTypeclasses mv e es = do

  -- remove any instance implementations that are not imported
  let instances = [pruneInstances imports maps | (_, imports, maps) <- es]

  MM.sayVVV $ "linkTypeclasses" <+> pretty mv

  -- Merge the typeclasses and instances from all imported modules
  -- These are inherited implicitly, so import terms are ignored
  kids <- Map.unionsWithM mergeIndexedInstances instances

  -- Augment the inherited map with the typeclasses and instances in this module
  findTypeclasses e kids

pruneInstances :: [(EVar, EVar)] -> Map.Map EVar (Indexed Instance) -> Map.Map EVar (Indexed Instance) 
pruneInstances imports = Map.mapWithKey checkInstance where
  checkInstance :: EVar -> Indexed Instance -> Indexed Instance
  checkInstance v inst@(Idx i t) = case lookup v imports of
    (Just v') -> if v == v'
                 then inst
                 else error "Cannot alias polymorphic terms"
    Nothing -> Idx i $ t {instanceTerms = []}

findTypeclasses
  :: ExprI
  -> Map.Map EVar (Indexed Instance)
  -> MorlocMonad (Map.Map EVar (Indexed Instance))
findTypeclasses (ExprI _ (ModE moduleName es0)) priorClasses = do


  -- first we collect all typeclass definitions in this module
  -- typeclasses are defined only at the top-level, so no descent into sub-expressions
  localClasses <- mapM makeClass [(cls, vs, sigs) | (ExprI _ (ClsE cls vs sigs)) <- es0]
               >>= Map.unionsWithM mergeIndexedInstances

  -- then merge them with all prior typeclasses and instances
  allClasses <- Map.unionWithM mergeIndexedInstances priorClasses localClasses

  MM.sayVVV $ "findTypeclasses"
            <> "\n  allClasses keys:" <+> pretty (Map.keys allClasses)

  -- find instances in this module
  -- The (IstE cls ts es) terms refer to
  --   cls: typeclass, such as "Packable"
  --   ts: types, such as ["Map a b", "[(a,b)]"]
  --   es: instance definitions, such as source statements (the only ones
  --       allowed at the moment)
  let instances = [(cls, ts, es) | (ExprI _ (IstE cls ts es)) <- es0]

  -- fold the instances into the current typeclass map and return
  moduleClasses <- foldlM addInstance allClasses instances

  MM.sayVVV $ "moduleClasses:" <+> prettyClsmap moduleClasses

  _ <- updateTypeclasses moduleClasses

  mapM_ (linkVariablesToTypeclasses moduleClasses) es0

  return moduleClasses

  where
    -- make a map of all terms that are defined in a typeclass (these will all
    -- be general term)
    makeClass :: (ClassName, [TVar], [Signature]) -> MorlocMonad (Map.Map EVar (Indexed Instance))
    makeClass (cls, vs, sigs) = mapM makeClassTerm sigs >>= fromListWithM mergeIndexedInstances where
      makeClassTerm :: Signature -> MorlocMonad (EVar, Indexed Instance)
      makeClassTerm (Signature v _ t) = do
        i <- MM.getCounter
        return (v, Idx i (Instance cls vs t []))

    addInstance
      :: Map.Map EVar (Indexed Instance)
      -> (ClassName, [TypeU], [ExprI])
      -> MorlocMonad (Map.Map EVar (Indexed Instance))
    addInstance clsmap (cls0 , ts0, []) = do
      MM.sayVVV $ "addInstance: in module" <+> pretty moduleName <+> "empty instance" <+> pretty cls0 <+> hsep (map (parens . pretty) ts0)
      return clsmap
    addInstance clsmap (cls0, ts0, es) = do
      MM.sayVVV $ "addInstance: in module" <+> pretty moduleName <+> "adding instance" <+> pretty cls0 <+> hsep (map (parens . pretty) ts0)
      newinst <- mapM f es >>= fromListWithM mergeIndexedInstances
      Map.unionWithM mergeIndexedInstances newinst clsmap
      where
      f :: ExprI -> MorlocMonad (EVar, Indexed Instance)
      f (ExprI srcIndex (SrcE src)) = do
        MM.sayVVV $ "  src:" <+> pretty src
                  <> "\n  clsmap:" <+> prettyClsmap clsmap

        case Map.lookup (srcAlias src) clsmap of
          (Just (Idx instIdx (Instance cls1 vs generalType otherInstances))) -> do
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

            return (srcAlias src, Idx instIdx (Instance cls0 vs generalType typeterms))
          Nothing ->  MM.throwError $ MissingTypeclassDefinition cls0 (srcAlias src)

      f (ExprI _ (AssE v e _)) =
        case Map.lookup v clsmap of
          (Just (Idx instIdx (Instance cls1 vs generalType otherInstances))) -> do
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

            return (v, Idx instIdx (Instance cls0 vs generalType typeterms))
          Nothing -> MM.throwError $ MissingTypeclassDefinition cls0 v

      f (ExprI _ e) = MM.throwError $ IllegalExpressionInInstance cls0 ts0 e

    fromListWithM :: (Monad m, Ord k) => (v -> v -> m v) -> [(k,v)] -> m (Map.Map k v) 
    fromListWithM f xs = Map.unionsWithM f $ map (Map.fromList . return) xs

    linkVariablesToTypeclasses
      :: Map.Map EVar (Indexed Instance) 
      -> ExprI
      -> MorlocMonad ()
    linkVariablesToTypeclasses = link where
      link :: Map.Map EVar (Indexed Instance) -> ExprI -> MorlocMonad ()
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

      setClass :: Map.Map EVar (Indexed Instance) -> Int -> EVar -> MorlocMonad ()
      setClass m termIndex v = case Map.lookup v m of
        (Just (Idx typeIndex (Instance cls _ t ts))) -> do

          MM.sayVVV $ "setClass map:" <+> viaShow m

          mapM_ (mapSources typeIndex cls v t) ts
          mapM_ (mapExpressions typeIndex  cls v t) ts

          s <- MM.get
          -- Yes, both indices are termIndex. After typechecking, the
          -- polymorphic type will resolve to monomorphic. Each may resolve
          -- differently, so instances must not all point to the same signature.
          MM.sayVVV $ "mergeSignatureSet in setClass for"
                    <> "\n i:" <+> pretty termIndex
                    <> "\n j:" <+> pretty typeIndex
                    <> "\n cls:" <+> pretty cls
                    <> "\n v:" <+> pretty v
                    <> "\n t:" <+> pretty t
                    <> "\n ts:" <+> pretty ts
          newMap <- GMap.insertWithM mergeSignatureSet termIndex typeIndex (Polymorphic cls v t ts) (stateSignatures s)
          MM.put (s { stateSignatures = newMap
                     , stateName = Map.insert termIndex v (stateName s)})
          return ()
        Nothing -> return ()

      mapSources :: Int -> ClassName -> EVar -> EType -> TermTypes -> MorlocMonad ()
      mapSources typeIndex cls v gt t = mapM_ (mapSource . snd) (termConcrete t) where
        mapSource :: Indexed Source -> MorlocMonad ()
        mapSource (Idx i src) = do
          let t' = TermTypes (termGeneral t) [(mv, srcidx) | (mv, srcidx) <- termConcrete t, val srcidx == src] []
          MM.sayVVV $ "mapSource" <+> pretty i <+> pretty src
                    <> "\n  termGeneral t:" <+> pretty (termGeneral t)
                    <> "\n  termGeneral t':" <+> pretty (termGeneral t')
                    <> "\n  length (termConcrete t):" <+> pretty (length (termConcrete t))
                    <> "\n  length (termConcrete t'):" <+> pretty (length (termConcrete t'))
          s <- MM.get
          MM.sayVVV $ "mergeSignatureSet src for"
                    <> "\n i:" <+> pretty i
                    <> "\n j:" <+> pretty typeIndex
                    <> "\n cls:" <+> pretty cls
                    <> "\n v:" <+> pretty v
                    <> "\n t:" <+> pretty gt
                    <> "\n ts:" <+> pretty [t']
                    <> "\n statesignatures:" <+> pretty (stateSignatures s)
          newMap <- GMap.insertWithM mergeSignatureSet i typeIndex (Polymorphic cls v gt [t']) (stateSignatures s)
          MM.put (s { stateSignatures = newMap })
          return ()

      mapExpressions :: Int -> ClassName -> EVar -> EType -> TermTypes -> MorlocMonad ()
      mapExpressions typeIndex cls v gt t = mapM_ mapExpression (termDecl t) where
        mapExpression :: ExprI -> MorlocMonad ()
        mapExpression (ExprI i e) = do
          MM.sayVVV $ "mapExpression" <+> pretty i
                    <> "\n  e:" <+> pretty e
          s <- MM.get
          let t' = TermTypes (termGeneral t) [] [e' | e'@(ExprI i' _) <- termDecl t, i' == i]
          MM.sayVVV $ "mergeSignatureSet src for"
                    <> "\n  mergeSignatureSet mapExpression for"
                    <> "\n    i:" <+> pretty i
                    <> "\n    j:" <+> pretty typeIndex
                    <> "\n    cls:" <+> pretty cls
                    <> "\n    v:" <+> pretty v
                    <> "\n    t:" <+> pretty gt
                    <> "\n    ts:" <+> pretty [t']
          newMap <- GMap.insertWithM mergeSignatureSet i typeIndex (Polymorphic cls v gt [t']) (stateSignatures s)
          MM.put (s { stateSignatures = newMap })
          return ()

    updateTypeclasses :: Map.Map EVar (Indexed Instance) -> MorlocMonad ()
    updateTypeclasses m = do
      s <- MM.get
      newMap <- Map.unionWithM mergeTypeclasses (Map.map val m) (stateTypeclasses s)
      MM.put (s {stateTypeclasses = newMap})

findTypeclasses _ _ = undefined
 

prettyClsmap :: Map.Map EVar (Indexed Instance) -> Doc ann
prettyClsmap clsmap
  = list (
           map ( \ (v, inst)
                 -> pretty v <+> "=" <+> pretty inst
              ) (Map.toList clsmap)
         )

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
