{-|
Module      : Morloc.TypeChecker.Infer
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.TypeChecker.Infer
  -- * The main type checker
  ( typecheck
  -- * Internal functions used in testing
  , subtype
  , substitute
  , apply
  , infer
  , rename
  , unrename
  , fromType
  ) where

import Morloc.Namespace
import Morloc.TypeChecker.Internal
import qualified Morloc.TypeChecker.PartialOrder as P
import qualified Morloc.Lang.DefaultTypes as MLD
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Control.Monad.Reader as R

import Morloc.Data.Doc hiding (putDoc)
import Morloc.Pretty
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc, AnsiStyle)

typecheck :: [Module] -> Stack [Module]
typecheck ms = do

  checkForMultipleDeclarations

  -- graph :: Map MVar (Set MVar)
  let graph = Map.fromList $ map mod2pair ms

  -- list modules ordered such that all dependencies of each module are defined
  -- in modules that appear earlier in the list
  -- ms' :: [Module]
  ms' <- sequence . map (\v -> find (\m -> moduleName m == v) ms) <$> path graph

  -- mods :: [Module]
  mods <- case ms' of
    (Just mods') -> typecheckModules (Map.empty) mods'
    Nothing -> throwError $ OtherError "No modules found"

  let modmap = Map.fromList [(moduleName m, m) | m <- mods]
  return (Map.elems . Map.map (addImportMap modmap) $ modmap)
  where

    checkForMultipleDeclarations :: Stack ()
    checkForMultipleDeclarations = case duplicates (map moduleName ms) of
      [] -> return ()
      mvars -> throwError $ MultipleModuleDeclarations mvars

    mod2pair :: Module -> (MVar, Set.Set MVar)
    mod2pair m = (moduleName m, Set.fromList $ map importModuleName (moduleImports m))

    addImportMap :: Map.Map MVar Module -> Module -> Module
    addImportMap ms m = m {
      moduleImportMap = Map.unions $ map (mkImportMap m ms) (moduleImports m) 
    }

    mkImportMap
      :: Module
      -> Map.Map MVar Module
      -> Import
      -> Map.Map EVar MVar
    mkImportMap m ms imp = case ( importInclude imp
                                , Map.lookup (importModuleName imp) ms
                                ) of 
      (_, Nothing) -> error "Bad import"
      -- include everything
      (Nothing, Just m') -> exportMap m' (importExclude imp)
      -- include specific selection of terms
      (Just xs, Just m') -> Map.filterWithKey (\v _ -> elem v (map snd xs))
                                              (exportMap m' (importExclude imp))

    exportMap :: Module -> [EVar] -> Map.Map EVar MVar
    exportMap m excl
      = Map.fromSet (\_ -> moduleName m)
      $ Set.difference (moduleExports m) (Set.fromList excl)

    -- typecheck a list of modules, pass context onwards.
    typecheckModules :: ModularGamma -> [Module] -> Stack [Module]
    typecheckModules _ [] = return []
    typecheckModules mg (m:ms) = do
      enter $ "entering module '" <> viaShow (moduleName m) <> "'"
      g <- importFromModularGamma mg m
      (g', exprs) <- typecheckExpr g (moduleBody m)
      (privateMap, mg') <- extendModularGamma g' m mg
      mods <- typecheckModules mg' ms
      leave $ "module"
      return (m { moduleBody = exprs
                , moduleTypeMap = privateMap
                , moduleDeclarationMap = Map.fromList [(v, e) | (Declaration v e) <- exprs]
                } : mods)

    -- produce a path from sources to pools, die on cycles
    path :: (Ord a) => Map.Map a (Set.Set a) -> Stack [a]
    path m
      | Map.size m == 0 = return []
      | otherwise =
        if Map.size rootMap == 0
          then throwError CyclicDependency
          else do
            rest <- path (Map.difference m rootMap)
            return (rest ++ Map.keys rootMap)
      where
        rootMap = Map.filterWithKey (isRoot m) m

        isRoot :: (Ord a) => Map.Map a (Set.Set a) -> a -> Set.Set a -> Bool
        isRoot m k _ = not $ Map.foldr (isChild k) False m
          where
            isChild :: (Ord a) => a -> Set.Set a -> Bool -> Bool
            isChild _ _ True = True
            isChild k' s False = Set.member k' s

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



-- | type 1 is more polymorphic than type 2 (Dunfield Figure 9)
subtype :: Type -> Type -> Gamma -> Stack Gamma
subtype t1 t2 g = do
  enter $ prettyGreenType t1 <+> "<:" <+> prettyGreenType t2
  seeGamma g
  g' <- subtype' t1 t2 g
  leave "subtype"
  return g'

-- VarT vs VarT
subtype' t1@(VarT (TV lang1 a1)) t2@(VarT (TV lang2 a2)) g
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
  | lang1 == lang2 && a1 /= a2 = throwError $ SubtypeError t1 t2

subtype' a@(ExistT (TV l1 _) _ _) b@(ExistT (TV l2 _) _ _) g
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
subtype' (FunT a1 a2) (FunT b1 b2) g1
  -- function subtypes are *contravariant* with respect to the input, that is,
  -- the subtypes are reversed so we have b1<:a1 instead of a1<:b1.
 = do
  g2 <- subtype b1 a1 g1
  subtype (apply g2 a2) (apply g2 b2) g2

--  g1 |- A1 <: B1
-- ----------------------------------------- <:App
--  g1 |- A1 A2 <: B1 B2 -| g2
--  unparameterized types are the same as VarT, so subtype on that instead
subtype' (ArrT v1 []) (ArrT v2 []) g
  | langOf v1 == langOf v2 = subtype (VarT v1) (VarT v2) g
  | otherwise = throwError . OtherError $ "Cannot compare types between languages"
subtype' t1@(ArrT v1@(TV l1 _) vs1) t2@(ArrT v2@(TV l2 _) vs2) g
  | length vs1 /= length vs2 = throwError . OtherError
    $ "Cannot subtype types with unequal parameter count" 
  | l1 /= l2 = serialConstraint t1 t2 >> return g
  | v1 == v2 = compareArr vs1 vs2 g
  | otherwise = throwError . OtherError $ "Shit happens" 
  where
    compareArr :: [Type] -> [Type] -> Gamma -> Stack Gamma
    compareArr [] [] g' = return g'
    compareArr (t1':ts1') (t2':ts2') g' = do
      g'' <- subtype t1' t2' g'
      compareArr ts1' ts2' g''
    compareArr _ _ _ = throwError TypeMismatch

-- subtype unordered records
subtype' (NamT v1 rs1) (NamT v2 rs2) g = do
  g' <- subtype (VarT v1) (VarT v2) g
  compareEntry (sort rs1) (sort rs2) g'
  where
    compareEntry :: [(MT.Text, Type)] -> [(MT.Text, Type)] -> Gamma -> Stack Gamma
    compareEntry [] [] g2 = return g2
    compareEntry ((k1, t1):rs1') ((k2, t2):rs2') g2
      | l1 == l2 = do
          g3 <- subtype (VarT (TV l1 k1)) (VarT (TV l2 k2)) g2
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
subtype' a b@(ExistT _ [] _) g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = occursCheck a b >> instantiate a b g
--  Ea not in FV(a)
--  g1[Ea] |- Ea <=: A -| g2
-- ----------------------------------------- <:InstantiateL
--  g1[Ea] |- Ea <: A -| g2
subtype' a@(ExistT _ [] _) b g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = occursCheck b a >> instantiate a b g

subtype' a@(ArrT v1 ps1) b@(ExistT v2 ps2 _) g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = subtype' (ArrT v1 ps1) (ExistT v2 ps2 []) g
subtype' (ExistT v1 ps1 _) t@(ArrT v2 ps2) g1
  | langOf v1 /= langOf v2 = return g1 -- incomparable
  | length ps1 /= length ps2 = throwError . OtherError . render $ 
      "Expected equal number of type paramters, found:"
        <+> list (map prettyGreenType ps1)
        <+> list (map prettyGreenType ps2)
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
subtype' (Forall v@(TV lang _) a) b g
  | lang /= langOf b = return g
  | otherwise = do
      a' <- newvar lang
      g' <- subtype (P.substitute v a' a) b (g +> MarkG v +> a')
      cut (MarkG v) g'

--  g1,a |- A <: B -| g2,a,g3
-- ----------------------------------------- <:ForallR
--  g1 |- A <: Forall a. B -| g2
subtype' a (Forall v@(TV lang _) b) g
  | lang /= langOf a = return g
  | otherwise = subtype a b (g +> VarG v) >>= cut (VarG v)
subtype' a b _ = throwError $ SubtypeError a b



-- | Dunfield Figure 10 -- type-level structural recursion
instantiate :: Type -> Type -> Gamma -> Stack Gamma
instantiate t1 t2 g1 = do
  say $ prettyGreenType t1 <+> "<=:" <+> prettyGreenType t2
  g2 <- instantiate' t1 t2 g1 
  say $ "instantiate done"
  seeGamma g2
  return g2

--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- A1 <=: Ea1 -| g2
--  g2 |- Ea2 <=: [g2]A2 -| g3
-- ----------------------------------------- InstLArr
--  g1[Ea] |- Ea <=: A1 -> A2 -| g3
instantiate' ta@(ExistT v@(TV lang _) [] _) (FunT t1 t2) g1 = do
  ea1 <- newvar lang
  ea2 <- newvar lang
  g2 <-
    case access1 v g1 of
      Just (rs, _, ls) ->
        return $ rs ++ [SolvedG v (FunT ea1 ea2), index ea1, index ea2] ++ ls
      Nothing -> throwError $ OtherError "Bad thing #2"
  g3 <- instantiate t1 ea1 g2
  g4 <- instantiate ea2 (apply g3 t2) g3
  return g4
--  g1[Ea2,Ea1,Ea=Ea1->Ea2] |- Ea1 <=: A1 -| g2
--  g2 |- [g2]A2 <=: Ea2 -| g3
-- ----------------------------------------- InstRArr
--  g1[Ea] |- A1 -> A2 <=: Ea -| g3
instantiate' t@(FunT t1 t2) tb@(ExistT v@(TV lang _) [] _) g1 = do
  ea1 <- newvar lang
  ea2 <- newvar lang
  g2 <-
    case access1 v g1 of
      Just (rs, _, ls) ->
        return $ rs ++ [SolvedG v (FunT ea1 ea2), index ea1, index ea2] ++ ls
      Nothing -> throwError $ OtherError "Bad thing #3"
  g3 <- instantiate t1 ea1 g2
  g4 <- instantiate ea2 (apply g3 t2) g3
  return g4
--
-- ----------------------------------------- InstLAllR
--
instantiate' ta@(ExistT _ _ _) tb@(Forall v2 t2) g1
  | langOf ta /= langOf tb = return g1
  | otherwise = instantiate ta t2 (g1 +> VarG v2) >>= cut (VarG v2)
-- InstLReach or instRReach -- each rule eliminates an existential
-- Replace the rightmost with leftmost (G[a][b] --> L,a,M,b=a,R)
-- WARNING: be careful here, since the implementation adds to the front and the
-- formal syntax adds to the back. Don't change anything in the function unless
-- you really know what you are doing and have tests to confirm it.
instantiate' ta@(ExistT v1 ps1 []) tb@(ExistT v2 ps2 []) g1 = do
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
instantiate' ta@(Forall x b) tb@(ExistT _ [] _) g1
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
instantiate' ta tb@(ExistT v [] []) g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      case access1 v g1 of
        (Just (ls, _, rs)) -> return $ ls ++ (SolvedG v ta) : rs
        Nothing ->
          case lookupT v g1 of
            (Just _) -> return g1
            Nothing ->
              throwError . OtherError $
              "Error in InstRSolve: ta=(" <>
              MT.show' ta <> ") tb=(" <> MT.show' tb <> ") g1=(" <> MT.show' g1 <> ")"
--  g1 |- t
-- ----------------------------------------- instLSolve
--  g1,Ea,g2 |- Ea <=: t -| g1,Ea=t,g2
instantiate' ta@(ExistT v [] []) tb g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      case access1 v g1 of
        (Just (ls, _, rs)) -> return $ ls ++ (SolvedG v tb) : rs
        Nothing ->
          case lookupT v g1 of
            (Just _) -> return g1
            Nothing -> error "error in InstLSolve"

-- if defaults are involved, no solving is done, but the subtypes of parameters
-- and defaults needs to be checked. 
instantiate' ta@(ExistT v1 ps1 ds1) tb@(ExistT v2 ps2 ds2) g1 = do
  g2 <- foldM (\g (t1, t2) -> subtype t1 t2 g) g1 (zip ps1 ps2)
  g3 <- foldM (\g d1 -> foldM (\g' d2 -> subtype (unDefaultType d1) (unDefaultType d2) g') g ds2) g2 ds1
  return g3

-- bad
instantiate' _ _ g = return g



infer ::
     Maybe Lang
  -> Gamma
  -> Expr -- ^ A subexpression from the original expression
  -> Stack ( Gamma
           , [Type] -- The return types
           , Expr -- The annotated expression
           )
infer l g e = do
  enter $ "infer" <+> maybe "MLang" (viaShow . id) l <+> parens (prettyExpr e)
  seeGamma g
  o@(_, ts, _) <- infer' l g e
  leave $ "infer |-" <+> encloseSep "(" ")" ", " (map prettyGreenType ts)
  return o

--
-- ----------------------------------------- <primitive>
--  g |- <primitive expr> => <primitive type> -| g
--
-- Num=>
infer' Nothing g e@(NumE _) = do
  let [t] = map unDefaultType (MLD.defaultNumber Nothing)
  return (g, [t], ann e t)
infer' lang g e@(NumE _) = do
  t <- newvarRich [] (MLD.defaultNumber lang) lang
  return (g +> t, [t], ann e t)

-- Str=>
infer' Nothing g e@(StrE _) = do
  let [t] = map unDefaultType (MLD.defaultString Nothing)
  return (g, [t], ann e t)
infer' lang g e@(StrE _) = do
  t <- newvarRich [] (MLD.defaultString lang) lang
  return (g +> t, [t], ann e t)

-- Log=>
infer' Nothing g e@(LogE _) = do
  let [t] = map unDefaultType (MLD.defaultBool Nothing)
  return (g, [t], ann e t)
infer' lang g e@(LogE _) = do
  t <- newvarRich [] (MLD.defaultBool lang) lang
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
infer' Nothing g1 (Declaration v e1) = do
  (typeset3, g4, es4) <- case lookupE (VarE v) g1 of
    -- CheckDeclaration
    (Just typeset@(TypeSet t ts)) -> do
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

  return (g4 +> AnnG (VarE v) typeset3, [], e5)
  where

    foldInfer
      :: EVar
      -> Expr
      -> (Gamma, [Type], [Expr])
      -> Maybe Lang
      -> Stack (Gamma, [Type], [Expr])
    foldInfer v e' (g1', ts1, es) lang = do
      (g2', ts2, e2) <- infer lang (g1' +> MarkEG v) e'
      g3' <- cut (MarkEG v) g2'
      return (g2', ts1 ++ ts2, e2:es)

    foldCheckExist
      :: EVar
      -> Expr
      -> (Gamma, [Type], [Expr])
      -> Type
      -> Stack (Gamma, [Type], [Expr])
    foldCheckExist v e' (g1', ts, es) t' = do
      (g2', t2', e2') <- check (g1' +> MarkEG v +> t') e' t'
      g3' <- cut (MarkEG v) g2'
      return (g2', t2':ts, e2':es)

    foldCheck ::
         Expr
      -> (Gamma, [Type], [Expr])
      -> Type
      -> Stack (Gamma, [Type], [Expr])
    foldCheck e' (g1', ts, es) t' = do
      (g2', t2', e2') <- check g1' e' t'

      say $ prettyExpr e2'

      return (g2', t2':ts, e2':es)

    toEType g t = EType
      { etype = t
      , eprop = Set.empty
      , econs = Set.empty
      }

--  (x:A) in g
-- ------------------------------------------- Var
--  g |- x => A -| g
infer' _ g e@(VarE v) = do
  case lookupE e g of
    (Just typeset) ->
      let ts = mapTS etype typeset
      in return (g, ts, AnnE e ts)
    Nothing -> throwError (UnboundVariable v)
  where
    mapTS :: (EType -> a) -> TypeSet -> [a]
    mapTS f (TypeSet (Just a) es) = map f (a:es)
    mapTS f (TypeSet Nothing es) = map f es

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
  case lookupE (VarE v) g3 >>= toType lang of
    (Just t2) -> do
      let t3 = FunT (apply g3 t2) t1
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

  let cs1 = [c | FunT _ c <- fs]

  e2' <- collate es2' 

  -- * e1' - e1 with type annotations
  -- * e2' - e2 with type annotations (after being applied to e2)
  (as2, ek') <- applyConcrete e1' e2' fs

  return (g2, as2, ek')
  where
    -- pair input and output types by language and construct the function type
    applyConcrete :: Expr -> Expr -> [Type] ->  Stack ([Type], Expr)
    applyConcrete (AnnE e1 f) e2 fs' = do
      let (tas, tcs) = unzip [ (FunT a c, c) | (FunT a c) <- fs' ]
      return (tcs, AnnE (AppE (AnnE e1 tas) e2) tcs)
    applyConcrete e _ _ = do
      say $ prettyScream "ERROR!!!"
      say $ "e =" <+> prettyExpr e
      throwError . OtherError $ "bad concrete"

    deriveF ::
         (Gamma, [Type], [Expr])
      -> Type
      -> Stack (Gamma, [Type], [Expr])
    deriveF (g', ts, es) t' = do
      (g'', t'', e'') <- derive g' e2 t'
      return (g'', t'':ts, e'':es)

--  g1 |- A
--  g1 |- e <= A -| g2
-- ----------------------------------------- Anno
--  g1 |- (e:A) => A -| g2
infer' _ g e1@(AnnE e@(VarE _) [t]) = do
  -- FIXME - I need to distinguish between the two types of annotations. There
  -- are annotations that the user writes; these need to be checked. There are
  -- annotations that are generated by the typechecker; these are basically
  -- cached results that do not need to be checked.
  --
  -- Currently I am checking the general cases, since that is the only kind of
  -- annotation the user can make, but this still runs some unnecessary checks.
  if langOf t == Nothing
    then
      case lookupE e g of
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
  elementType <- case P.mostSpecific (map fst pairs) of
    [] -> newvar lang
    (t:_) -> return t
  (g3, _, xs3) <- chainCheck (zip (repeat elementType) xs1) g2
  let dts = MLD.defaultList lang elementType
  containerType <-
    if lang == Nothing
    then return (head $ map unDefaultType dts)
    else newvarRich [elementType] dts lang
  return (g3, [containerType], ann (ListE xs3) containerType)

-- Tuple=>
infer' _ _ (TupleE []) = throwError EmptyTuple
infer' _ _ (TupleE [_]) = throwError TupleSingleton
infer' lang g1 e@(TupleE xs1) = do
  (g2, pairs) <- chainInfer lang g1 xs1
  let (ts2, xs2) = unzip pairs
      dts = MLD.defaultTuple lang ts2
  containerType <-
    if lang == Nothing
    then return (head $ map unDefaultType dts)
    else newvarRich ts2 dts lang
  return (g2, [containerType], ann (TupleE xs2) containerType)

-- Record=>
infer' _ _ (RecE []) = throwError EmptyRecord
infer' lang g1 e@(RecE rs) = do
  (g2, pairs) <- chainInfer lang g1 (map snd rs)
  let (ts2, xs2) = unzip pairs
      keys = map fst rs
      entries = zip (map unEVar keys) ts2
      dts = MLD.defaultRecord lang entries
  containerType <-
    if lang == Nothing
    then return (head $ map unDefaultType dts)
    else newvarRich [NamT (TV lang "__RECORD__") entries] dts lang -- see entry in Parser.hs
  return (g2, [containerType], ann (RecE (zip keys xs2)) containerType)



-- | Pattern matches against each type
check ::
     Gamma
  -> Expr -- ^ An expression which should be of the type given
  -> Type -- ^ The expected type of the expression
  -> Stack ( Gamma
           , Type -- The inferred type of the expression
           , Expr -- The annotated expression
           )
check g e t = do
  enter $ "check" <+> parens (prettyExpr e) <> "  " <> prettyGreenType t
  seeGamma g
  (g', t', e') <- check' g e t
  leave $ "check |-" <+> prettyGreenType t'
  return (g', t', e')

--  g1,x:A |- e <= B -| g2,x:A,g3
-- ----------------------------------------- -->I
--  g1 |- \x.e <= A -> B -| g2
check' g1 (LamE v e1) t1@(FunT a b) = do
  -- define x:A
  let anng = AnnG (VarE v) (fromType (langOf t1) a)
  -- check that e has the expected output type
  (g2, t2, e2) <- check (g1 +> anng) e1 b
  -- ignore the trailing context and (x:A), since it is out of scope
  g3 <- cut anng g2
  let t3 = FunT a t2
  return (g3, t3, ann (LamE v e2) t3)

--  g1,x |- e <= A -| g2,x,g3
-- ----------------------------------------- Forall.I
--  g1 |- e <= Forall x.A -| g2
check' g1 e1 t2@(Forall x a) = do
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
  -> Type -- the function type
  -> Stack ( Gamma
           , Type -- output function type
           , Expr -- @e@, with type annotation
            )
derive g e f = do
  enter $ "derive" <+> prettyExpr e <> "  " <> prettyGreenType f
  seeGamma g
  (g', t', e') <- derive' g e f
  leave $ "derive |-" <+> prettyGreenType t'
  return (g', t', e')

--  g1 |- e <= A -| g2
-- ----------------------------------------- -->App
--  g1 |- A->C o e =>> C -| g2
derive' g e (FunT a b) = do
  (g', a', e') <- check g e a
  let b' = apply g' b
  return (g', FunT a' b', apply g' e')

--  g1,Ea |- [Ea/a]A o e =>> C -| g2
-- ----------------------------------------- Forall App
--  g1 |- Forall x.A o e =>> C -| g2
derive' g e (Forall x s) = derive (g +> ExistG x [] []) e (substitute x s)

--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- e <= Ea1 -| g2
-- ----------------------------------------- EaApp
--  g1[Ea] |- Ea o e =>> Ea2 -| g2
derive' g e t@(ExistT v@(TV lang _) [] _) =
  case access1 v g of
    -- replace <t0> with <t0>:<ea1> -> <ea2>
    Just (rs, _, ls) -> do
      ea1 <- newvar lang
      ea2 <- newvar lang
      let t' = FunT ea1 ea2
          g2 = rs ++ [SolvedG v t', index ea1, index ea2] ++ ls
      (g3, a', e2) <- check g2 e ea1
      let f' = FunT a' (apply g3 ea2)
      return (g3, f', e2)
    -- if the variable has already been solved, use solved value
    Nothing -> case lookupT v g of
      (Just (FunT t1 t2)) -> do
        (g2, _, e2) <- check g e t1
        return (g2, FunT t1 t2, e2)
      _ -> throwError . OtherError $ "Expected a function"

derive' _ e t = do
  say $ prettyScream "ERROR!!!"
  say $ "e: " <> prettyExpr e
  say $ "t: " <> prettyGreenType t
  throwError NonFunctionDerive



-- ----- H E L P E R S --------------------------------------------------

-- | substitute all appearances of a given variable with an existential
-- [t/v]A
substitute :: TVar -> Type -> Type
substitute v t = P.substitute v (ExistT v [] []) t

-- | TODO: document
occursCheck :: Type -> Type -> Stack ()
occursCheck t1 t2 = do
  -- say $ "occursCheck:" <+> prettyGreenType t1 <+> prettyGreenType t2
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
-- illegal
collateOne (Signature _ _) (Signature _ _) = error "the hell's a toplevel doing down here?"
collateOne (Declaration _ _) (Declaration _ _) = error "the hell's is a toplevel doing down here?"
collateOne (SrcE _) (SrcE _) = error "the hell's is a toplevel doing down here?"
collateOne e1 e2 = throwError . OtherError . render $
  nest 2 . vsep $ ["collation failure - unequal expressions:", viaShow e1, viaShow e2]

collateTypes :: [Type] -> [Type] -> Stack [Type]
collateTypes ts1 ts2
  = mapM (collateByLang . snd)
  . groupSort
  $ [(langOf t, t) | t <- nub (ts1 ++ ts2)]
  where
    collateByLang :: [Type] -> Stack Type
    collateByLang [] = throwError . OtherError $ "This should be impossible"
    collateByLang [t] = return t
    collateByLang (t1:ts) = foldM moreSpecific t1 ts

    moreSpecific :: Type -> Type -> Stack Type
    moreSpecific (FunT t11 t12) (FunT t21 t22) = FunT <$> moreSpecific t11 t21 <*> moreSpecific t12 t22
    moreSpecific (ArrT v1 ts1) (ArrT v2 ts2) = ArrT v1 <$> zipWithM moreSpecific ts1 ts2
    moreSpecific (NamT v1 ts1) (NamT v2 ts2)
      | v1 == v2 = NamT <$> pure v1 <*> zipWithM mergeEntry (sort ts1) (sort ts2)
      | otherwise = throwError . OtherError $ "Cannot collate records with unequal names/langs"
      where
      mergeEntry (k1, t1) (k2, t2)
        | k1 == k2 = (,) <$> pure k1 <*> moreSpecific t1 t2
        | otherwise = throwError . OtherError $ "Cannot collate records with unequal keys"
    moreSpecific (ExistT _ _ []) t = return t
    moreSpecific t (ExistT _ _ []) = return t
    moreSpecific (Forall _ _) t = return t
    moreSpecific t (Forall _ _) = return t
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
    f' :: Type -> Type -> Stack ()
    f' (FunT x1 y1) (FunT x2 y2) = f' x1 x2 >> f' y1 y2
    f' (Forall _ x) (Forall _ y) = f' x y
    f' (Forall _ x) y = f' x y
    f' x (Forall _ y) = f' x y
    f' (ExistT _ [] _) (ExistT _ [] _) = return ()
    f' (ExistT v (x:xs) ds1) (ExistT w (y:ys) ds2) = f' (ExistT v xs ds1) (ExistT w ys ds2)
    f' (ExistT _ _ _) (ExistT _ _ _) = throwError . OtherError $
      "BadRealization: unequal number of parameters"
    f' (ExistT _ _ _) _ = return ()
    f' _ (ExistT _ _ _) = return ()
    f' t1@(FunT _ _) t2 = throwError . OtherError $
      "BadRealization: Cannot compare types '" <> MT.show' t1 <> "' to '" <> MT.show' t2 <> "'"
    f' t1 t2@(FunT _ _) = throwError . OtherError $
      "BadRealization: Cannot compare types '" <> MT.show' t1 <> "' to '" <> MT.show' t2 <> "'"
    f' _ _ = return ()

checkup :: Gamma -> Expr -> Type -> Stack (Gamma, [Type], Expr)
checkup g e t = do
  say "checkup"
  (g', t', e') <- check g e t
  return (g', [t'], e')

typesetFromList :: Gamma -> EVar -> [Type] -> Stack TypeSet
typesetFromList g v ts = do 
  say "typesetFromList"
  let gentype = [makeEType t | t <- ts, (isNothing . langOf) t]
      contype = [makeEType t | t <- ts, (isJust . langOf) t]
  case (gentype, contype) of
    ([x], cs) -> return $ TypeSet (Just x) cs
    ([], cs) -> return $ TypeSet Nothing cs
    _ -> throwError $ OtherError "ambiguous general type"
  where
    makeEType :: Type -> EType
    makeEType t = EType
      { etype = t
      , eprop = Set.empty
      , econs = Set.empty
      }

chainInfer :: Maybe Lang -> Gamma -> [Expr] -> Stack (Gamma, [(Type, Expr)])
chainInfer lang g0 es0 = do
  say "chainInfer"
  chainInfer' g0 (reverse es0) []
  where
    chainInfer' ::
         Gamma -> [Expr] -> [(Type,Expr)] -> Stack (Gamma, [(Type, Expr)])
    chainInfer' g [] xs = return (g, xs)
    chainInfer' g (e:es) xs = do
      (g', ts, e') <- infer lang g e
      case filter (\t -> langOf t == lang) ts of
        -- FIXME - performance bug. In cases such as: `[foo x, 42]`, the
        -- application `foo x` will be evaluated identically each time
        -- chainInfer is called with a different language. Each time all
        -- concrete instances will be resolved and only one will be used. 
        [t'] -> chainInfer' g' es ((t', e'):xs)
        ts -> throwError . OtherError . render $
          "Expected unique type from infer, found (see issue #9):" <+> list (map prettyGreenType ts)

chainCheck :: [(Type, Expr)] -> Gamma -> Stack (Gamma, [Type], [Expr])
chainCheck xs g = do
  (g, ts, es) <- foldM f (g, [], []) xs
  return (g, reverse ts, reverse es)
  where
    f :: (Gamma, [Type], [Expr]) -> (Type, Expr) -> Stack (Gamma, [Type], [Expr])
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
