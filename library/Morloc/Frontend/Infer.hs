{-|
Module      : Morloc.Frontend.Infer
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.Infer
  (
  -- * The main type checker
    typecheck
  -- * Internal functions used in testing
  , subtype
  , substitute
  , apply
  , infer
  , rename
  , unrename
  , fromType
  ) where

import Morloc.Frontend.Namespace
import Morloc.Frontend.Internal
import qualified Morloc.Language as ML
import qualified Morloc.Frontend.PartialOrder as P
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
import qualified Morloc.Data.DAG as MDD
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Control.Monad.Reader as R

import Morloc.Data.Doc hiding (putDoc)
import Morloc.Frontend.Pretty
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc, AnsiStyle)

typecheck
  :: DAG MVar [(EVar, EVar)] PreparedNode
  -> Stack (DAG MVar [(EVar, EVar)] TypedNode)
typecheck d = do
  maybeDAG <- MDD.synthesizeDAG typecheck' d
  case maybeDAG of
    Nothing -> throwError CyclicDependency
    (Just d') -> do
      d'' <- MDD.synthesizeDAG propagateConstructors d'
      case d'' of
        (Just d''') -> return d'''
        Nothing -> throwError CyclicDependency
  where
    typecheck'
      :: MVar
      -> PreparedNode
      -> [(MVar, [(EVar, EVar)], TypedNode)]
      -> Stack TypedNode
    typecheck' k n xs = do
      enter $ "entering module '" <> viaShow k <> "'"
      g0 <- importTypes xs
      (g1, es) <- typecheckExpr g0 (preparedNodeBody n)
      leave $ "module"
      return $ TypedNode
        { typedNodeModuleName = k
        , typedNodePath = preparedNodePath n
        , typedNodeBody = es
          -- the typemap is really only used when typchecking modules that
          -- import this module, so it technically could be removed deleted for
          -- being passed to the downstream generators.
        , typedNodeTypeMap = nodeTypeMapFromGamma g1
        , typedNodeSourceMap = preparedNodeSourceMap n
        , typedNodeExports = preparedNodeExports n
        , typedNodePackers = preparedNodePackers n
        , typedNodeConstructors
            = Map.fromList
            . map (\src@(Source _ lang _ alias) -> (TV (Just lang) (unEVar alias), src))
            . catMaybes
            . map ((flip Map.lookup) (preparedNodeSourceMap n))
            $ [ (EVar v, lang)
              | (TV (Just lang) v) <- unique (conmap collectConstructors es)]

        , typedNodeTypedefs = Map.map (\(t,ps) -> (resolve t, ps)) (preparedNodeTypedefs n)
        }


    collectConstructors :: Expr -> [TVar] 
    collectConstructors (AnnE e ts) = collectConstructors e ++ (conmap findTVar ts)
    collectConstructors (Declaration _ e) = collectConstructors e
    collectConstructors (ListE es) = conmap collectConstructors es
    collectConstructors (TupleE es) = conmap collectConstructors es
    collectConstructors (LamE _ e) = collectConstructors e
    collectConstructors (AppE e1 e2) = collectConstructors e1 ++ collectConstructors e2
    collectConstructors (RecE rs) = conmap (collectConstructors . snd) rs
    collectConstructors _ = []

    findTVar :: UnresolvedType -> [TVar]
    findTVar (VarU _) = []
    findTVar (ExistU _ _ _) = []
    findTVar (ForallU _ t) = findTVar t
    findTVar (FunU t1 t2) = findTVar t1 ++ findTVar t2
    findTVar (ArrU _ ts) = conmap findTVar ts
    findTVar (NamU _ v _ rs) = v : conmap (findTVar . snd) rs

    propagateConstructors
      :: MVar -- the importing module name
      -> TypedNode -- data about the importing module
      -> [(   MVar -- the name of an imported module
            , [(EVar -- the name of a term in the imported module
              , EVar -- the alias in the importing module
              )]
            , TypedNode -- data about the imported module
         )]
      -> Stack TypedNode
    propagateConstructors k n1 es = do
      let cons = Map.union (typedNodeConstructors n1)
               $ (Map.fromList . concat) [inherit n2 ps | (_, ps, n2) <- es] 
      return $ n1 { typedNodeConstructors = cons }

    inherit :: TypedNode -> [(EVar, EVar)] -> [(TVar, Source)]
    inherit ((Map.toList . typedNodeConstructors) -> ms) es =
      [ (TV lang (unEVar n'), Source n l p n')
      | (TV lang _, Source n l p a) <- ms -- information from parent
      , (a', n') <- es -- edge: a' imported term name
      , a == a']

    nodeTypeMapFromGamma :: Gamma -> Map.Map EVar TypeSet
    nodeTypeMapFromGamma g
      = Map.fromList
      $ [(e,t) | AnnG (VarE e) t <- g] ++ [(v,t) | AnnG (Declaration v _) t <- g]

    importTypes :: [(MVar, [(EVar, EVar)], TypedNode)] -> Stack Gamma
    importTypes xs
      -- [(EVar, [TypeSet])]
      = (return . groupSort . concat . map importTypes') xs
      -- [(EVar, TypeSet)]
      >>= mapM mergeManyTypeSets
      -- [GammaIndex]
      |>> map (\(v, t) -> AnnG (VarE v) t)

    importTypes' :: (MVar, [(EVar, EVar)], TypedNode) -> [(EVar, TypeSet)]
    importTypes' (_, xs, n) = mapMaybe (lookupOne (typedNodeTypeMap n)) xs

    lookupOne :: Map.Map EVar TypeSet -> (EVar, EVar) -> Maybe (EVar, TypeSet)
    lookupOne m (name, alias) = case Map.lookup name m of
      (Just t) -> return (name, t)
      Nothing -> Nothing

    -- Typecheck a set of expressions within a given context (i.e., one module).
    -- Return the modified context and a list of annotated expressions.
    typecheckExpr :: Gamma -> [Expr] -> Stack (Gamma, [Expr])
    typecheckExpr g e = do
      es <- mapM rename e
      (g', es') <- typecheckExpr' g [] es
      let es'' = concat [toExpr v t | (AnnG (VarE v) t) <- g'] ++ reverse es'
      return $ (g', map (generalizeE . unrename . apply g') es'')
      where
        toExpr :: EVar -> TypeSet -> [Expr]
        toExpr v (TypeSet (Just e) es) = [Signature v t | t <- (e : es)]
        toExpr v (TypeSet Nothing es) = [Signature v t | t <- es]

        typecheckExpr' :: Gamma -> [Expr] -> [Expr] -> Stack (Gamma, [Expr])
        typecheckExpr' g es [] = return (g, es)
        typecheckExpr' g es (x:xs) = do
          (g', _, e') <- infer Nothing g x
          case e' of
            (Signature _ _) -> typecheckExpr' g' es xs
            _ -> typecheckExpr' g' (e' : es) xs

    mergeManyTypeSets :: (EVar, [TypeSet]) -> Stack (EVar, TypeSet)
    mergeManyTypeSets (v, ts) = do
      generalType <- mergeGeneral $ catMaybes [gt | (TypeSet gt _) <- ts]
      let concreteTypes = concat [cs | (TypeSet _ cs) <- ts]
      return $ (v, TypeSet generalType concreteTypes)

    mergeGeneral :: [EType] -> Stack (Maybe EType)
    mergeGeneral [] = return Nothing
    mergeGeneral [e] = return (Just e)
    mergeGeneral [e1, e2] = fmap Just $ mergeGeneralTwo e1 e2
    mergeGeneral (e1:es) = do
      e2' <- mergeGeneral es
      case e2' of
        (Just e2) -> fmap Just $ mergeGeneralTwo e1 e2
        Nothing -> return Nothing

    mergeGeneralTwo :: EType -> EType -> Stack EType
    mergeGeneralTwo (EType t1 ps1 cs1) (EType t2 ps2 cs2) = do
      subtype t1 t2 []
      subtype t2 t1 []
      -- FIXME: implement better behavior here for joining properties
      return $ EType t1 (Set.union ps1 ps2) (Set.union cs1 cs2)


-- | type 1 is more polymorphic than type 2 (Dunfield Figure 9)
subtype :: UnresolvedType -> UnresolvedType -> Gamma -> Stack Gamma
subtype t1 t2 g = do
  enter $ prettyGreenUnresolvedType t1 <+> "<:" <+> prettyGreenUnresolvedType t2
  seeGamma g
  g' <- subtype' t1 t2 g
  leave "subtype"
  return g'

-- VarU vs VarT
subtype' t1@(VarU (TV lang1 a1)) t2@(VarU (TV lang2 a2)) g
  -- If everything is the same, do nothing
  --
  -- ----------------------------------------- <:Var
  --  G[a] |- a_l <: a_l -| G[a]
  | lang1 == lang2 && a1 == a2 = return g
  -- If languages are different, do nothing
  --  l1 != l2    b_l2 ~~> a_l1
  -- ----------------------------------------- <:Var
  --  G[a] |- a_l1 <: b_l2 -| G[a]
  | lang1 /= lang2 = serialConstraint t1 t2 >> return g
  -- If languages are same, but types are different, raise error
  | lang1 == lang2 && a1 /= a2 = throwError $ SubtypeError (unresolvedType2type t1) (unresolvedType2type t2)

subtype' a@(ExistU (TV l1 _) _ _) b@(ExistU (TV l2 _) _ _) g
  --
  -- ----------------------------------------- <:Exvar
  --  G[E.a] |- E.a <: E.a -| G[E.a]
  | a == b = return g
  --  l1 == l2
  -- ----------------------------------------- <:AlienExvar
  --  G[E.a,E.b] |- E.a <: E.b -| G[E.a,E.b], E.a ~~> E.b
  | l1 /= l2 = return $ g +> UnsolvedConstraint a b
  --
  -- ----------------------------------------- <:InstantiateL/<:InstantiateR
  --  G[E.a] |- Ea <: Ea -| G[E.a]
  | otherwise
      -- formally, an `Ea notin FV(G)` check should be done here, but since the
      -- types involved are all existentials, it will always pass, so I omit
      -- it.
   = instantiate a b g

--  g1 |- B1 <: A1 -| g2
--  g2 |- [g2]A2 <: [g2]B2 -| g3
-- ----------------------------------------- <:-->
--  g1 |- A1 -> A2 <: B1 -> B2 -| g3
subtype' (FunU a1 a2) (FunU b1 b2) g1
  -- function subtypes are *contravariant* with respect to the input, that is,
  -- the subtypes are reversed so we have b1<:a1 instead of a1<:b1.
 = do
  g2 <- subtype b1 a1 g1
  subtype (apply g2 a2) (apply g2 b2) g2

--  g1 |- A1 <: B1
-- ----------------------------------------- <:App
--  g1 |- A1 A2 <: B1 B2 -| g2
--  unparameterized types are the same as VarT, so subtype on that instead
subtype' (ArrU v1 []) (ArrU v2 []) g
  | langOf v1 == langOf v2 = subtype (VarU v1) (VarU v2) g
  | otherwise = throwError . OtherError $ "Cannot compare types between languages"
subtype' t1@(ArrU v1@(TV l1 _) vs1) t2@(ArrU v2@(TV l2 _) vs2) g
  | length vs1 /= length vs2 = throwError . OtherError
    $ "Cannot subtype types with unequal parameter count" 
  | l1 /= l2 = serialConstraint t1 t2 >> return g
  | v1 == v2 = compareArr vs1 vs2 g
  | otherwise = throwError . OtherError $ "Shit happens" 
  where
    compareArr :: [UnresolvedType] -> [UnresolvedType] -> Gamma -> Stack Gamma
    compareArr [] [] g' = return g'
    compareArr (t1':ts1') (t2':ts2') g' = do
      g'' <- subtype t1' t2' g'
      compareArr ts1' ts2' g''
    compareArr _ _ _ = throwError TypeMismatch

-- subtype unordered records
subtype' (NamU _ v1 _ rs1) (NamU _ v2 _ rs2) g = do
  g' <- subtype (VarU v1) (VarU v2) g
  compareEntry (sort rs1) (sort rs2) g'
  where
    compareEntry :: [(MT.Text, UnresolvedType)] -> [(MT.Text, UnresolvedType)] -> Gamma -> Stack Gamma
    compareEntry [] [] g2 = return g2
    compareEntry ((k1, t1):rs1') ((k2, t2):rs2') g2
      | l1 == l2 = do
          g3 <- subtype (VarU (TV l1 k1)) (VarU (TV l2 k2)) g2
          g4 <- subtype t1 t2 g3
          compareEntry rs1' rs2' g4
      | otherwise = serialConstraint t1 t2 >> return g
      where
        l1 = langOf t1
        l2 = langOf t2
    compareEntry _ _ _ = throwError TypeMismatch

--  Ea not in FV(a)
--  g1[Ea] |- A <=: Ea -| g2
-- ----------------------------------------- <:InstantiateR
--  g1[Ea] |- A <: Ea -| g2
subtype' a b@(ExistU _ [] _) g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = occursCheck a b >> instantiate a b g
--  Ea not in FV(a)
--  g1[Ea] |- Ea <=: A -| g2
-- ----------------------------------------- <:InstantiateL
--  g1[Ea] |- Ea <: A -| g2
subtype' a@(ExistU _ [] _) b g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = occursCheck b a >> instantiate a b g

subtype' a@(ArrU v1 ps1) b@(ExistU v2 ps2 _) g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = subtype' (ArrU v1 ps1) (ExistU v2 ps2 []) g
subtype' (ExistU v1 ps1 _) t@(ArrU v2 ps2) g1
  | langOf v1 /= langOf v2 = return g1 -- incomparable
  | length ps1 /= length ps2 = throwError . OtherError . render $ 
      "Expected equal number of type paramters, found:"
        <+> list (map prettyGreenUnresolvedType ps1)
        <+> list (map prettyGreenUnresolvedType ps2)
  | otherwise = do
    g2 <- foldM (\g (p1, p2) -> subtype p1 p2 g) g1 (zip ps1 ps2)
    case access1 v1 g2 of
      Just (rs, _, ls) ->
        return $ rs ++ [SolvedG v1 t] ++ ls
      Nothing -> return g2 -- it is already solved, so do nothing

--  g1,>Ea,Ea |- [Ea/x]A <: B -| g2,>Ea,g3
-- ----------------------------------------- <:ForallL
--  g1 |- Forall x . A <: B -| g2
--
subtype' (ForallU v@(TV lang _) a) b g
  | lang /= langOf b = return g
  | otherwise = do
      a' <- newvar lang
      g' <- subtype (P.substitute v a' a) b (g +> MarkG v +> a')
      cut (MarkG v) g'

--  g1,a |- A <: B -| g2,a,g3
-- ----------------------------------------- <:ForallR
--  g1 |- A <: Forall a. B -| g2
subtype' a (ForallU v@(TV lang _) b) g
  | lang /= langOf a = return g
  | otherwise = subtype a b (g +> VarG v) >>= cut (VarG v)
subtype' a b _ = throwError $ SubtypeError (unresolvedType2type a) (unresolvedType2type b)



-- | Dunfield Figure 10 -- type-level structural recursion
instantiate :: UnresolvedType -> UnresolvedType -> Gamma -> Stack Gamma
instantiate t1 t2 g1 = do
  say $ prettyGreenUnresolvedType t1 <+> "<=:" <+> prettyGreenUnresolvedType t2
  g2 <- instantiate' t1 t2 g1 
  say $ "instantiate done"
  seeGamma g2
  return g2

--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- A1 <=: Ea1 -| g2
--  g2 |- Ea2 <=: [g2]A2 -| g3
-- ----------------------------------------- InstLArr
--  g1[Ea] |- Ea <=: A1 -> A2 -| g3
instantiate' ta@(ExistU v@(TV lang _) [] _) (FunU t1 t2) g1 = do
  ea1 <- newvar lang
  ea2 <- newvar lang
  g2 <-
    case access1 v g1 of
      Just (rs, _, ls) ->
        return $ rs ++ [SolvedG v (FunU ea1 ea2), index ea1, index ea2] ++ ls
      Nothing -> throwError $ OtherError "Bad thing #2"
  g3 <- instantiate t1 ea1 g2
  g4 <- instantiate ea2 (apply g3 t2) g3
  return g4
--  g1[Ea2,Ea1,Ea=Ea1->Ea2] |- Ea1 <=: A1 -| g2
--  g2 |- [g2]A2 <=: Ea2 -| g3
-- ----------------------------------------- InstRArr
--  g1[Ea] |- A1 -> A2 <=: Ea -| g3
instantiate' t@(FunU t1 t2) tb@(ExistU v@(TV lang _) [] _) g1 = do
  ea1 <- newvar lang
  ea2 <- newvar lang
  g2 <-
    case access1 v g1 of
      Just (rs, _, ls) ->
        return $ rs ++ [SolvedG v (FunU ea1 ea2), index ea1, index ea2] ++ ls
      Nothing -> throwError $ OtherError "Bad thing #3"
  g3 <- instantiate t1 ea1 g2
  g4 <- instantiate ea2 (apply g3 t2) g3
  return g4
--
-- ----------------------------------------- InstLAllR
--
instantiate' ta@(ExistU _ _ _) tb@(ForallU v2 t2) g1
  | langOf ta /= langOf tb = return g1
  | otherwise = instantiate ta t2 (g1 +> VarG v2) >>= cut (VarG v2)
-- InstLReach or instRReach -- each rule eliminates an existential
-- Replace the rightmost with leftmost (G[a][b] --> L,a,M,b=a,R)
-- WARNING: be careful here, since the implementation adds to the front and the
-- formal syntax adds to the back. Don't change anything in the function unless
-- you really know what you are doing and have tests to confirm it.
instantiate' ta@(ExistU v1 ps1 []) tb@(ExistU v2 ps2 []) g1 = do
  g2 <- foldM (\g (t1, t2) -> subtype t1 t2 g) g1 (zip ps1 ps2)
  g3 <- case access2 v1 v2 g2 of
    -- InstLReach
    (Just (ls, _, ms, x, rs)) -> return $ ls <> (SolvedG v1 tb : ms) <> (x : rs)
    Nothing ->
      case access2 v2 v1 g2 of
      -- InstRReach
        (Just (ls, _, ms, x, rs)) ->
          return $ ls <> (SolvedG v2 ta : ms) <> (x : rs)
        Nothing -> return g2
  return g3
--  g1[Ea],>Eb,Eb |- [Eb/x]B <=: Ea -| g2,>Eb,g3
-- ----------------------------------------- InstRAllL
--  g1[Ea] |- Forall x. B <=: Ea -| g2
instantiate' ta@(ForallU x b) tb@(ExistU _ [] _) g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      instantiate
        (substitute x b) -- [Eb/x]B
        tb -- Ea
        (g1 +> MarkG x +> ExistG x [] []) -- g1[Ea],>Eb,Eb
      >>= cut (MarkG x)
--  g1 |- t
-- ----------------------------------------- InstRSolve
--  g1,Ea,g2 |- t <=: Ea -| g1,Ea=t,g2
instantiate' ta tb@(ExistU v [] []) g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      case access1 v g1 of
        (Just (ls, _, rs)) -> return $ ls ++ (SolvedG v ta) : rs
        Nothing ->
          case lookupU v g1 of
            (Just _) -> return g1
            Nothing ->
              throwError . OtherError $
              "Error in InstRSolve: ta=(" <>
              MT.show' ta <> ") tb=(" <> MT.show' tb <> ") g1=(" <> MT.show' g1 <> ")"
--  g1 |- t
-- ----------------------------------------- instLSolve
--  g1,Ea,g2 |- Ea <=: t -| g1,Ea=t,g2
instantiate' ta@(ExistU v [] []) tb g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      case access1 v g1 of
        (Just (ls, _, rs)) -> return $ ls ++ (SolvedG v tb) : rs
        Nothing ->
          case lookupU v g1 of
            (Just _) -> return g1
            Nothing -> error "error in InstLSolve"

-- if defaults are involved, no solving is done, but the subtypes of parameters
-- and defaults needs to be checked. 
instantiate' ta@(ExistU v1 ps1 ds1) tb@(ExistU v2 ps2 ds2) g1 = do
  g2 <- foldM (\g (t1, t2) -> subtype t1 t2 g) g1 (zip ps1 ps2)
  g3 <- foldM (\g d1 -> foldM (\g' d2 -> subtype d1 d2 g') g ds2) g2 ds1
  return g3

-- bad
instantiate' _ _ g = return g



infer ::
     Maybe Lang
  -> Gamma
  -> Expr -- ^ A subexpression from the original expression
  -> Stack ( Gamma
           , [UnresolvedType] -- The return types
           , Expr -- The annotated expression
           )
infer l g e = do
  enter $ "infer" <+> maybe "MLang" (viaShow . id) l <+> parens (prettyExpr e)
  seeGamma g
  o@(_, ts, _) <- infer' l g e
  leave $ "infer |-" <+> encloseSep "(" ")" ", " (map prettyGreenUnresolvedType ts)
  return o

--
-- ----------------------------------------- <primitive>
--  g |- <primitive expr> => <primitive type> -| g
--
-- Num=>
infer' Nothing g e@(NumE _) = do
  let t = head $ MLD.defaultNumber Nothing
  return (g, [t], ann e t)
infer' lang g e@(NumE _) = do
  t <- newvarRich [] [head $ MLD.defaultNumber lang] lang
  return (g +> t, [t], ann e t)

-- Str=>
infer' Nothing g e@(StrE _) = do
  let t = head $ MLD.defaultString Nothing
  return (g, [t], ann e t)
infer' lang g e@(StrE _) = do
  t <- newvarRich [] [head $ MLD.defaultString lang] lang
  return (g +> t, [t], ann e t)

-- Log=>
infer' Nothing g e@(LogE _) = do
  let t = head $ MLD.defaultBool Nothing
  return (g, [t], ann e t)
infer' lang g e@(LogE _) = do
  t <- newvarRich [] [head $ MLD.defaultBool lang] lang
  return (g +> t, [t], ann e t)

-- Src=>
-- -- FIXME: the expressions are now NOT sorted ... need to fix
-- Since the expressions in a Morloc script are sorted before being
-- evaluated, the SrcE expressions will be considered before the Signature
-- and Declaration expressions. Thus every term that originates in source
-- code will be initialized here and elaborated upon with deeper type
-- information as the signatures and declarations are parsed. 
-- -- NOTE: Keeping SrcE as an expression, rather than pulling it out of the
-- body, as is done with imports and exports, is justified since the type
-- system should know that a given term is from a given language since it may
-- be possible, in cases, to infer a type signature for the given language from
-- the general type signature.
infer' (Just _) _ (SrcE _) = throwError ToplevelStatementsHaveNoLanguage
infer' Nothing g1 s1@(SrcE srcs) = do
  let g3 = map SrcG srcs ++ g1
  return (g3, [], s1)

-- Signature=>
infer' (Just _) _ (Signature _ _) = throwError ToplevelStatementsHaveNoLanguage
infer' Nothing g (Signature v e1) = do
  g2 <- accessWith1 isAnnG (append' e1) (ifNotFound e1) g
  return (g2, [], Signature v e1)
  where

    -- find a typeset
    isAnnG :: GammaIndex -> Bool
    isAnnG (AnnG (VarE e1) _)
      | v == e1 = True
      | otherwise = False
    isAnnG _ = False

    -- update the found typeset
    append' :: EType -> GammaIndex -> Stack GammaIndex
    append' e (AnnG x@(VarE v) r2) = AnnG <$> pure x <*> appendTypeSet g v r2 e
    append' _ _ = throwError $ OtherError "Bad Gamma"

    -- create a new typeset if none was found
    ifNotFound :: EType -> Gamma -> Stack Gamma
    ifNotFound e g' = case (langOf . etype) e of
        lang@(Just _) -> return $ AnnG (VarE v) (TypeSet Nothing [e]) : g'
        Nothing       -> return $ AnnG (VarE v) (TypeSet (Just e) []) : g'

-- Declaration=>
infer' (Just _) _ (Declaration _ _) = throwError ToplevelStatementsHaveNoLanguage
infer' Nothing g1 e0@(Declaration v e1) = do
  (typeset3, g4, es4) <- case lookupE v g1 of
    -- CheckDeclaration
    (Just (_, typeset@(TypeSet t ts))) -> do
      let xs1 = map etype (maybeToList t ++ ts)
          tlangs = langsOf g1 typeset
          langs = [lang | lang <- langsOf g1 e1, not (elem lang tlangs)]
      -- Check each of the signatures against the expression.
      (g2, ts2, es2) <- foldM (foldCheck e1) (g1, [], []) xs1
      (g3, ts3, es3) <- mapM newvar langs
                     >>= foldM (foldCheckExist v e1) (g2, ts2, es2)
      typeset2 <- foldM (appendTypeSet g3 v) typeset (map (toEType g3) ts3)
      return (generalizeTypeSet typeset2, g3, es3)
    -- InferDeclaration
    Nothing -> do
      (g3, ts3, es3) <- foldM (foldInfer v e1) (g1, [], []) (langsOf g1 e1)
      let ts4 = unique ts3
      typeset2 <- typesetFromList g3 v (map generalize ts4)
      return (typeset2, g3, es3)

  e2 <- collate es4

  let e5 = Declaration v (generalizeE e2)

  return (g4 +> AnnG e0 typeset3, [], e5)
  where

    foldInfer
      :: EVar
      -> Expr
      -> (Gamma, [UnresolvedType], [Expr])
      -> Maybe Lang
      -> Stack (Gamma, [UnresolvedType], [Expr])
    foldInfer v e' (g1', ts1, es) lang = do
      (g2', ts2, e2) <- infer lang (g1' +> MarkEG v) e'
      g3' <- cut (MarkEG v) g2'
      return (g2', ts1 ++ ts2, e2:es)

    foldCheckExist
      :: EVar
      -> Expr
      -> (Gamma, [UnresolvedType], [Expr])
      -> UnresolvedType
      -> Stack (Gamma, [UnresolvedType], [Expr])
    foldCheckExist v e' (g1', ts, es) t' = do
      (g2', t2', e2') <- check (g1' +> MarkEG v +> t') e' t'
      g3' <- cut (MarkEG v) g2'
      return (g2', t2':ts, e2':es)

    foldCheck ::
         Expr
      -> (Gamma, [UnresolvedType], [Expr])
      -> UnresolvedType
      -> Stack (Gamma, [UnresolvedType], [Expr])
    foldCheck e' (g1', ts, es) t' = do
      (g2', t2', e2') <- check g1' e' t'
      say (prettyExpr e2')
      return (g2', t2':ts, e2':es)

    toEType g t = EType
      { etype = t
      , eprop = Set.empty
      , econs = Set.empty
      }

infer' lang g e@(VarE v) = do
  say $ "----------------------------------"
  say $ pretty v
  case (lang, lookupE v g) of
    (Just _, Just (VarE v', t@(TypeSet _ []))) -> 
      if v' == v
      then return (g, mapTS etype t, AnnE (VarE v') (mapTS etype t))
      else infer' lang g (VarE v')
    --  forall M . (x:A_m) not_in 
    -- ------------------------------------------- Var=>
    --  g |- x => A -| g
    (Just _, Just (e', TypeSet _ [])) -> infer lang g e'
    --  (x:A) in g
    -- ------------------------------------------- Var
    --  g |- x => A -| g
    (_, Just (_, typeset)) ->
      let ts = mapTS etype typeset
      in return (g, ts, AnnE e ts)
    (_, Nothing) -> throwError (UnboundVariable v)
  where
    mapTS :: (EType -> a) -> TypeSet -> [a]
    mapTS f (TypeSet (Just a) es) = map f (a:es)
    mapTS f (TypeSet Nothing es) = map f es

infer' lang g (AccE e k) = do
  (g', record_ts, e') <- infer lang g e
  ts <- mapM (accessRecord k) record_ts
  return (g', ts, AnnE e' ts)
  where
    accessRecord :: EVar -> UnresolvedType -> Stack UnresolvedType
    accessRecord (EVar k) (NamU _ _ _ rs) =
      case [t | (k', t) <- rs, k' == k] of
        [] -> throwError BadRecordAccess
        [t] -> return t
        _ -> throwError  BadRecordAccess

--  g1,Ea,Eb,x:Ea |- e <= Eb -| g2,x:Ea,g3
-- ----------------------------------------- -->I=>
--  g1 |- \x.e => Ea -> Eb -| g2
-- | type 1 is more polymorphic than type 2 (Dunfield Figure 9)
infer' lang g1 e0@(LamE v e2) = do
  a <- newvar lang
  b <- newvar lang
  let anng = AnnG (VarE v) (fromType lang a)
      g2 = g1 +> a +> b +> anng
  (g3, t1, e2') <- check g2 e2 b
  case fmap snd (lookupE v g3) >>= toType lang of
    (Just t2) -> do
      let t3 = FunU (apply g3 t2) t1
      g4 <- cut anng g3
      return (g4, [t3], ann (LamE v e2') t3)
    Nothing -> throwError $ OtherError "Bad thing #4"

{-  g |- e1 => A* -| d_1
 -  { d_i |- [d_i]A_i o e2 =>> C_i -| d_{i+1} } forall i in (1,2 ... k)
 - ----------------------------------------- -->E
 -  g |- e1 e2 =>> C -| d_k
 -}
infer' lang g1 (AppE e1 e2) = do
  -- Anonymous lambda functions are currently not supported. So e1 currently will
  -- be a VarE, an AppE, or an AnnE annotating a VarE or AppE. Anonymous lambdas
  -- would roughly correspond to DeclareInfer statements while adding annotated
  -- lambdas would correspond to DeclareAnnot.

  -- @as1@ will include one entry consisting of the general type `(Nothing,t)`
  -- and one or more realizatoins `(Just lang, t)`
  (d1, as1, e1') <- infer lang g1 e1

  -- Map derive over every type observed for e1, the functional element. The
  -- result is a list of the types and expressions derived from e2
  (g2, fs, es2') <- foldM deriveF (d1, [], []) as1

  let cs1 = [c | FunU _ c <- fs]

  e2' <- collate es2' 

  -- * e1' - e1 with type annotations
  -- * e2' - e2 with type annotations (after being applied to e2)
  (as2, ek') <- applyConcrete e1' e2' fs

  return (g2, as2, ek')
  where
    -- pair input and output types by language and construct the function type
    applyConcrete :: Expr -> Expr -> [UnresolvedType] ->  Stack ([UnresolvedType], Expr)
    applyConcrete (AnnE e1 f) e2 fs' = do
      let (tas, tcs) = unzip [ (FunU a c, c) | (FunU a c) <- fs' ]
      return (tcs, AnnE (AppE (AnnE e1 tas) e2) tcs)
    applyConcrete e _ _ = do
      say $ prettyScream "ERROR!!!"
      say $ "e =" <+> prettyExpr e
      throwError . OtherError $ "bad concrete"

    deriveF ::
         (Gamma, [UnresolvedType], [Expr])
      -> UnresolvedType
      -> Stack (Gamma, [UnresolvedType], [Expr])
    deriveF (g', ts, es) t' = do
      (g'', t'', e'') <- derive g' e2 t'
      return (g'', t'':ts, e'':es)

--  g1 |- A
--  g1 |- e <= A -| g2
-- ----------------------------------------- Anno
--  g1 |- (e:A) => A -| g2
infer' _ g e1@(AnnE e@(VarE v) [t]) = do
  -- FIXME - I need to distinguish between the two types of annotations. There
  -- are annotations that the user writes; these need to be checked. There are
  -- annotations that are generated by the typechecker; these are basically
  -- cached results that do not need to be checked.
  --
  -- Currently I am checking the general cases, since that is the only kind of
  -- annotation the user can make, but this still runs some unnecessary checks.
  if langOf t == Nothing
    then
      case lookupE v g of
        (Just _) -> checkup g e t
        Nothing -> return (g, [t], e1)
    else
        return (g, [t], e1)
infer' _ g (AnnE e [t]) =
  if langOf t == Nothing
    then checkup g e t
    else return (g, [t], e)
infer' _ g (AnnE e ts) = return (g, ts, e)

-- List=>
infer' lang g1 e1@(ListE xs1) = do
  (g2, pairs) <- chainInfer lang g1 xs1
  elementType <- case (P.mostSpecific . catMaybes)  (map fst pairs) of
    [] -> newvar lang
    (t:_) -> return t
  (g3, _, xs3) <- chainCheck (zip (repeat elementType) xs1) g2
  let dts = MLD.defaultList lang elementType
  containerType <-
    if lang == Nothing
    then return (head dts)
    else newvarRich [elementType] dts lang
  return (g3, [containerType], ann (ListE xs3) containerType)

-- Tuple=>
infer' _ _ (TupleE []) = throwError EmptyTuple
infer' _ _ (TupleE [_]) = throwError TupleSingleton
infer' lang g1 e@(TupleE xs1) = do
  (g2, pairs) <- chainInfer lang g1 xs1
  let (ts2may, xs2) = unzip pairs
  ts2 <- case sequence ts2may of
    Nothing -> throwError . OtherError $ "Could not infer tuple type"
    (Just ts2') -> return ts2' 
  let dts = MLD.defaultTuple lang ts2
  containerType <-
    if lang == Nothing
    then return (head dts)
    else newvarRich ts2 dts lang
  return (g2, [containerType], ann (TupleE xs2) containerType)

-- Record=>
infer' _ _ (RecE []) = throwError EmptyRecord
infer' lang g1 e@(RecE rs) = do
  (g2, pairs) <- chainInfer lang g1 (map snd rs)
  let (ts2may, xs2) = unzip pairs
      keys = map fst rs
  entries <- case sequence ts2may of
    (Just ts2) -> return $ zip (map unEVar keys) ts2
    Nothing -> throwError . OtherError $ "Could not infer record type"
  let dts = MLD.defaultRecord lang entries
  containerType <-
    if lang == Nothing
    then return (head dts)
    else newvarRich [NamU NamRecord (TV lang "__RECORD__") [] entries] dts lang -- see entry in Parser.hs
  return (g2, [containerType], ann (RecE (zip keys xs2)) containerType)



-- | Pattern matches against each type
check ::
     Gamma
  -> Expr -- ^ An expression which should be of the type given
  -> UnresolvedType -- ^ The expected type of the expression
  -> Stack ( Gamma
           , UnresolvedType -- The inferred type of the expression
           , Expr -- The annotated expression
           )
check g e t = do
  enter $ "check" <+> parens (prettyExpr e) <> "  " <> prettyGreenUnresolvedType t
  seeGamma g
  (g', t', e') <- check' g e t
  leave $ "check |-" <+> prettyGreenUnresolvedType t'
  return (g', t', e')

--  g1,x:A |- e <= B -| g2,x:A,g3
-- ----------------------------------------- -->I
--  g1 |- \x.e <= A -> B -| g2
check' g1 (LamE v e1) t1@(FunU a b) = do
  -- define x:A
  let anng = AnnG (VarE v) (fromType (langOf t1) a)
  -- check that e has the expected output type
  (g2, t2, e2) <- check (g1 +> anng) e1 b
  -- ignore the trailing context and (x:A), since it is out of scope
  g3 <- cut anng g2
  let t3 = FunU a t2
  return (g3, t3, ann (LamE v e2) t3)

--  g1,x |- e <= A -| g2,x,g3
-- ----------------------------------------- Forall.I
--  g1 |- e <= Forall x.A -| g2
check' g1 e1 t2@(ForallU x a) = do
  (g2, _, e2) <- check (g1 +> VarG x) e1 a
  g3 <- cut (VarG x) g2
  let t3 = apply g3 t2
  return (g3, t3, ann e2 t3)

--  g1 |- e => A -| g2
--  g2 |- [g2]A <: [g2]B -| g3
-- ----------------------------------------- Sub
--  g1 |- e <= B -| g3
check' g1 e1 b = do
  (g2, ts, e2) <- infer (langOf b) g1 e1
  g3 <- foldM (\g t -> subtype (apply g t) (apply g b) g) g2 ts
  return (g3, apply g3 b, anns (apply g3 e2) (map (apply g3) ts))



derive ::
     Gamma
  -> Expr -- the expression that is passed to the function
  -> UnresolvedType -- the function type
  -> Stack ( Gamma
           , UnresolvedType -- output function type
           , Expr -- @e@, with type annotation
            )
derive g e f = do
  enter $ "derive" <+> prettyExpr e <> "  " <> prettyGreenUnresolvedType f
  seeGamma g
  (g', t', e') <- derive' g e f
  leave $ "derive |-" <+> prettyGreenUnresolvedType t'
  return (g', t', e')

--  g1 |- e <= A -| g2
-- ----------------------------------------- -->App
--  g1 |- A->C o e =>> C -| g2
derive' g e (FunU a b) = do
  (g', a', e') <- check g e a
  let b' = apply g' b
  return (g', FunU a' b', apply g' e')

--  g1,Ea |- [Ea/a]A o e =>> C -| g2
-- ----------------------------------------- Forall App
--  g1 |- Forall x.A o e =>> C -| g2
derive' g e (ForallU x s) = derive (g +> ExistG x [] []) e (substitute x s)

--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- e <= Ea1 -| g2
-- ----------------------------------------- EaApp
--  g1[Ea] |- Ea o e =>> Ea2 -| g2
derive' g e t@(ExistU v@(TV lang _) [] _) =
  case access1 v g of
    -- replace <t0> with <t0>:<ea1> -> <ea2>
    Just (rs, _, ls) -> do
      ea1 <- newvar lang
      ea2 <- newvar lang
      let t' = FunU ea1 ea2
          g2 = rs ++ [SolvedG v t', index ea1, index ea2] ++ ls
      (g3, a', e2) <- check g2 e ea1
      let f' = FunU a' (apply g3 ea2)
      return (g3, f', e2)
    -- if the variable has already been solved, use solved value
    Nothing -> case lookupU v g of
      (Just (FunU t1 t2)) -> do
        (g2, _, e2) <- check g e t1
        return (g2, FunU t1 t2, e2)
      _ -> throwError . OtherError $ "Expected a function"

derive' _ e t = do
  say $ prettyScream "ERROR!!!"
  say $ "e: " <> prettyExpr e
  say $ "t: " <> prettyGreenUnresolvedType t
  throwError NonFunctionDerive



-- ----- H E L P E R S --------------------------------------------------

-- | substitute all appearances of a given variable with an existential
-- [t/v]A
substitute :: TVar -> UnresolvedType -> UnresolvedType
substitute v t = P.substitute v (ExistU v [] []) t

-- | TODO: document
occursCheck :: UnresolvedType -> UnresolvedType -> Stack ()
occursCheck t1 t2 = do
  -- say $ "occursCheck:" <+> prettyGreenUnresolvedType t1 <+> prettyGreenUnresolvedType t2
  case Set.member t1 (P.free t2) of
    True -> throwError OccursCheckFail
    False -> return ()


-- | fold a list of annotated expressions into one, preserving annotations
collate :: [Expr] -> Stack Expr
collate [] = throwError . OtherError $ "Nothing to collate"
collate [e] = return e
collate (e:es) = do
  say $ "collating" <+> (align . vsep . map prettyExpr) (e:es)
  e' <- foldM collateOne e es
  say $ "collated to:" <+> prettyExpr e'
  return e'

-- | Merge two annotated expressions into one, fail if the expressions are not
-- equivalent.
collateOne :: Expr -> Expr -> Stack Expr
collateOne (AnnE e1 ts1) (AnnE e2 ts2) = AnnE <$> collateOne e1 e2 <*> collateTypes ts1 ts2
-- 
collateOne (AppE e11 e12) (AppE e21 e22) = AppE <$> collateOne e11 e21 <*> collateOne e12 e22
collateOne (LamE v1 e1) (LamE v2 e2)
  | v1 == v2 = LamE <$> pure v1 <*> collateOne e1 e2
  | otherwise = throwError $ OtherError "collate error #1"
collateOne e@(VarE v1) (VarE v2)
  | v1 == v2 = return e
  | otherwise = throwError $ OtherError "collate error #2"
-- primitives
collateOne e@UniE UniE = return e
collateOne e@(LogE _) (LogE _) = return e
collateOne e@(NumE _) (NumE _) = return e
collateOne e@(StrE _) (StrE _) = return e
-- containers
collateOne (ListE es1) (ListE es2)
  | length es1 == length es2 = ListE <$> zipWithM collateOne es1 es2
  | otherwise = throwError $ OtherError "collate error: unequal list length"
collateOne (TupleE es1) (TupleE es2)
  | length es1 == length es2 = TupleE <$> zipWithM collateOne es1 es2
  | otherwise = throwError $ OtherError "collate error: unequal tuple length"
collateOne (RecE es1) (RecE es2)
  | length es1 == length es2 =
    RecE <$> (
          zip
      <$> zipWithM returnIfEqual (map fst es1) (map fst es2)
      <*> zipWithM collateOne (map snd es1) (map snd es2)
    )
  | otherwise = throwError $ OtherError "collate error: unequal record length"
  where
    returnIfEqual :: Eq a => a -> a -> Stack a
    returnIfEqual x y
      | x == y = return x
      | otherwise = throwError $ OtherError "expected them to be equal"
-- variable expansion
collateOne (VarE _) x = return x
collateOne x (VarE _) = return x
-- illegal
collateOne (Signature _ _) (Signature _ _) = error "the hell's a toplevel doing down here?"
collateOne (Declaration _ _) (Declaration _ _) = error "the hell's is a toplevel doing down here?"
collateOne (SrcE _) (SrcE _) = error "the hell's is a toplevel doing down here?"
collateOne e1 e2 = throwError . OtherError . render $
  nest 2 . vsep $ ["collation failure - unequal expressions:", viaShow e1, viaShow e2]

collateTypes :: [UnresolvedType] -> [UnresolvedType] -> Stack [UnresolvedType]
collateTypes ts1 ts2
  = mapM (collateByLang . snd)
  . groupSort
  $ [(langOf t, t) | t <- nub (ts1 ++ ts2)]
  where
    collateByLang :: [UnresolvedType] -> Stack UnresolvedType
    collateByLang [] = throwError . OtherError $ "This should be impossible"
    collateByLang [t] = return t
    collateByLang (t1:ts) = foldM moreSpecific t1 ts

    moreSpecific :: UnresolvedType -> UnresolvedType -> Stack UnresolvedType
    moreSpecific (FunU t11 t12) (FunU t21 t22) = FunU <$> moreSpecific t11 t21 <*> moreSpecific t12 t22
    moreSpecific (ArrU v1 ts1) (ArrU v2 ts2) = ArrU v1 <$> zipWithM moreSpecific ts1 ts2
    moreSpecific (NamU r1 v1 ps rs1) (NamU r2 v2 _ rs2)
      | v1 == v2 && r1 == r2 = NamU r1 <$> pure v1 <*> pure ps <*> zipWithM mergeEntry (sort rs1) (sort rs2)
      | otherwise = throwError . OtherError $ "Cannot collate records with unequal names/langs"
      where
      mergeEntry (k1, t1) (k2, t2)
        | k1 == k2 = (,) <$> pure k1 <*> moreSpecific t1 t2
        | otherwise = throwError . OtherError $ "Cannot collate records with unequal keys"
    moreSpecific (ExistU _ _ []) t = return t
    moreSpecific t (ExistU _ _ []) = return t
    moreSpecific (ForallU _ _) t = return t
    moreSpecific t (ForallU _ _) = return t
    moreSpecific t _ = return t


-- | merge the new data from a signature with any prior type data
appendTypeSet :: Gamma -> EVar -> TypeSet -> EType -> Stack TypeSet
appendTypeSet g v s e1 =
  case ((langOf . etype) e1, s) of
  -- if e is a general type, and there is no conflicting type, then set e
    (Nothing, TypeSet Nothing rs) -> do
      mapM_ (checkRealization e1) rs
      return $ TypeSet (Just e1) rs
  -- if e is a realization, and no general type is set, just add e to the list
    (Just lang, TypeSet Nothing rs) -> do
      return $ TypeSet Nothing (e1 : [r | r <- rs, r /= e1])
  -- if e is a realization, and a general type exists, append it and check
    (Just lang, TypeSet (Just e2) rs) -> do
      checkRealization e2 e1
      return $ TypeSet (Just e2) (e1 : [r | r <- rs, r /= e1])
  -- if e is general, and a general type exists, merge the general types
    (Nothing, TypeSet (Just e2) rs) -> do
      let e3 =
            EType
              { etype = etype e2
              , eprop = Set.union (eprop e1) (eprop e2)
              , econs = Set.union (econs e1) (econs e2)
              }
      return $ TypeSet (Just e3) rs

-- | TODO: document
checkRealization :: EType -> EType -> Stack ()
checkRealization e1 e2 = f' (etype e1) (etype e2)
  where
    f' :: UnresolvedType -> UnresolvedType -> Stack ()
    f' (FunU x1 y1) (FunU x2 y2) = f' x1 x2 >> f' y1 y2
    f' (ForallU _ x) (ForallU _ y) = f' x y
    f' (ForallU _ x) y = f' x y
    f' x (ForallU _ y) = f' x y
    f' (ExistU _ [] _) (ExistU _ [] _) = return ()
    f' (ExistU v (x:xs) ds1) (ExistU w (y:ys) ds2) = f' (ExistU v xs ds1) (ExistU w ys ds2)
    f' (ExistU _ _ _) (ExistU _ _ _) = throwError . OtherError $
      "BadRealization: unequal number of parameters"
    f' (ExistU _ _ _) _ = return ()
    f' _ (ExistU _ _ _) = return ()
    f' t1@(FunU _ _) t2 = throwError . OtherError $
      "BadRealization: Cannot compare types '" <> MT.show' t1 <> "' to '" <> MT.show' t2 <> "'"
    f' t1 t2@(FunU _ _) = throwError . OtherError $
      "BadRealization: Cannot compare types '" <> MT.show' t1 <> "' to '" <> MT.show' t2 <> "'"
    f' _ _ = return ()

checkup :: Gamma -> Expr -> UnresolvedType -> Stack (Gamma, [UnresolvedType], Expr)
checkup g e t = do
  say "checkup"
  (g', t', e') <- check g e t
  return (g', [t'], e')

typesetFromList :: Gamma -> EVar -> [UnresolvedType] -> Stack TypeSet
typesetFromList g v ts = do 
  say "typesetFromList"
  let gentype = [makeEType t | t <- ts, (isNothing . langOf) t]
      contype = [makeEType t | t <- ts, (isJust . langOf) t]
  case (gentype, contype) of
    ([x], cs) -> return $ TypeSet (Just x) cs
    ([], cs) -> return $ TypeSet Nothing cs
    _ -> throwError $ OtherError "ambiguous general type"
  where
    makeEType :: UnresolvedType -> EType
    makeEType t = EType
      { etype = t
      , eprop = Set.empty
      , econs = Set.empty
      }

-- Synthesize types for a list of expressions. Each expression is synthesized
-- independently, though context is passed along. The returned "Maybe Type" is
-- the type of the paired expression in the given language.
chainInfer :: Maybe Lang -> Gamma -> [Expr] -> Stack (Gamma, [(Maybe UnresolvedType, Expr)])
chainInfer lang g0 es0 = do
  say "chainInfer"
  chainInfer' g0 (reverse es0) []
  where
    chainInfer' ::
         Gamma -> [Expr] -> [(Maybe UnresolvedType,Expr)] -> Stack (Gamma, [(Maybe UnresolvedType, Expr)])
    chainInfer' g [] xs = return (g, xs)
    chainInfer' g (e:es) xs = do
      (g', ts, e') <- infer lang g e
      let t' = listToMaybe $ filter (\t -> langOf t == lang) ts
      chainInfer' g' es ((t', e'):xs)

chainCheck :: [(UnresolvedType, Expr)] -> Gamma -> Stack (Gamma, [UnresolvedType], [Expr])
chainCheck xs g = do
  (g, ts, es) <- foldM f (g, [], []) xs
  return (g, reverse ts, reverse es)
  where
    f :: (Gamma, [UnresolvedType], [Expr]) -> (UnresolvedType, Expr) -> Stack (Gamma, [UnresolvedType], [Expr])
    f (g', ts, es) (t', e') = do 
      (g'', t'', e'') <- check g' e' t'
      return (g'', t'':ts, e'':es)



-- ----- U T I L I T I E S ----------------------------------------------

enter :: Doc AnsiStyle -> Stack ()
enter d = do
  depth <- incDepth
  debugLog $ pretty (take depth (repeat '-')) <> ">" <+> align d <> "\n"

say :: Doc AnsiStyle -> Stack ()
say d = do
  depth <- getDepth
  debugLog $ pretty (take depth (repeat ' ')) <> ":" <+> align d <> "\n"

seeGamma :: Gamma -> Stack ()
seeGamma g = say $ nest 4 $ "Gamma:" <> line <> (vsep (map prettyGammaIndex g))

leave :: Doc AnsiStyle -> Stack ()
leave d = do
  depth <- decDepth
  debugLog $ "<" <> pretty (take depth (repeat '-')) <+> align d <> "\n"

debugLog :: Doc AnsiStyle -> Stack ()
debugLog d = do
  verbosity <- R.asks stackConfigVerbosity 
  if verbosity > 0
    then (liftIO . putDoc) d
    else return ()
