{-|
Module      : Morloc.CodeGenerator.Typecheck
Description : Check the concrete type of an unambiguous AST (SAnno One)
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Typecheck
(
  typecheck
) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Internal
import Morloc.CodeGenerator.Grammars.Common
import Morloc.Typecheck.Internal
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
import qualified Morloc.Frontend.PartialOrder as P

typecheck
  :: SAnno (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (SAnno Int One (Indexed TypeP))
typecheck e = do
  packers <- MM.gets statePackers
  e' <- retrieveTypes e
  let g0 = Gamma {gammaCounter = 0, gammaContext = []}
  case synth g0 e' of
    (Left err) -> MM.throwError . ConcreteTypeError $ err
    (Right (_, _, e'')) -> weaveAndResolve e''


-- | Load the known concrete types into the tree. This is all the information
-- necessary for concrete type checking.
retrieveTypes
  :: SAnno (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (SAnno (Indexed Type) One (Indexed (Lang, [EType])))
retrieveTypes (SAnno (One (x, Idx i lang)) g@(Idx j _)) = do
  ts <- case x of
    (CallS src) -> do
      mayts <- MM.metaTermTypes j
      case fmap termConcrete mayts of
        (Just ts) -> case [es | (_, src', es, _) <- ts, src == src] of
          [es] -> return es
          _ -> MM.throwError . CallTheMonkeys $ "Malformed TermTypes"
        Nothing -> MM.throwError . CallTheMonkeys $ "Missing TermTypes"
    _ -> return []

  x' <- case x of
    UniS -> return UniS
    (VarS v) -> return $ VarS v
    (AccS x k) -> AccS <$> retrieveTypes x <*> pure k
    (LstS xs) -> LstS <$> mapM retrieveTypes xs
    (TupS xs) -> TupS <$> mapM retrieveTypes xs
    (LamS vs f) -> LamS vs <$> retrieveTypes f
    (AppS f xs) -> AppS <$> retrieveTypes f <*> mapM retrieveTypes xs
    (NumS x) -> return $ NumS x
    (LogS x) -> return $ LogS x
    (StrS x) -> return $ StrS x
    (NamS rs) -> do
      xs' <- mapM (retrieveTypes . snd) rs
      return $ NamS (zip (map fst rs) xs')
    (CallS src) -> return $ CallS src

  return $ SAnno (One (x', Idx i (lang, ts))) g


weaveAndResolve
  :: SAnno (Indexed Type) One (Indexed TypeU)
  -> MorlocMonad (SAnno Int One (Indexed TypeP))
weaveAndResolve (SAnno (One (x, Idx i ct)) (Idx j gt)) = do
  pt <- weaveResolvedTypes gt (typeOf ct)
  x' <- case x of
    UniS -> return UniS
    (VarS v) -> return $ VarS v
    (AccS x k) -> AccS <$> weaveAndResolve x <*> pure k
    (LstS xs) -> LstS <$> mapM weaveAndResolve xs
    (TupS xs) -> TupS <$> mapM weaveAndResolve xs
    (LamS vs x) -> LamS vs <$> weaveAndResolve x
    (AppS f xs) -> AppS <$> weaveAndResolve f <*> mapM weaveAndResolve xs
    (NumS x) -> return $ NumS x
    (LogS x) -> return $ LogS x
    (StrS x) -> return $ StrS x
    (NamS rs) -> do
      xs <- mapM (weaveAndResolve . snd) rs
      return $ NamS (zip (map fst rs) xs)
    (CallS src) -> return $ CallS src
  return $ SAnno (One (x', Idx i pt)) j

 

-- | type 1 is more polymorphic than type 2 (Dunfield Figure 9)
subtype :: TypeU -> TypeU -> Gamma -> Either TypeError Gamma

-- VarU vs VarT
subtype t1@(VarU (TV lang1 a1)) t2@(VarU (TV lang2 a2)) g
  -- If everything is the same, do nothing
  --
  -- ----------------------------------------- <:Var
  --  G[a] |- a_l <: a_l -| G[a]
  | lang1 == lang2 && a1 == a2 = return g
  -- If languages are different, do nothing
  --  l1 != l2    b_l2 ~~> a_l1
  -- ----------------------------------------- <:Var
  --  G[a] |- a_l1 <: b_l2 -| G[a]
  | lang1 /= lang2 = return $ g +> SerialConstraint t1 t2

  -- If languages are same, but types are different, raise error
  | lang1 == lang2 && a1 /= a2 = Left $ NotYetImplemented t1 t2 "Within language type conversion not yet implemented"

subtype a@(ExistU (TV l1 _) _ _) b@(ExistU (TV l2 _) _ _) g
  --
  -- ----------------------------------------- <:Exvar
  --  G[E.a] |- E.a <: E.a -| G[E.a]
  | a == b = return g
  --  l1 == l2
  -- ----------------------------------------- <:AlienExvar
  --  G[E.a,E.b] |- E.a <: E.b -| G[E.a,E.b], E.a ~~> E.b
  | l1 /= l2 = return $ g +> SerialConstraint a b
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
subtype (FunU _ _) (FunU _ _) _ = undefined 
-- subtype (FunU a1 a2) (FunU b1 b2) g1
--   -- function subtypes are *contravariant* with respect to the input, that is,
--   -- the subtypes are reversed so we have b1<:a1 instead of a1<:b1.
--  = do
--   g2 <- subtype b1 a1 g1
--   subtype (apply g2 a2) (apply g2 b2) g2

--  g1 |- A1 <: B1
-- ----------------------------------------- <:App
--  g1 |- A1 A2 <: B1 B2 -| g2
--  unparameterized types are the same as VarT, so subtype on that instead
subtype t1@(AppU v1 []) t2@(AppU v2 []) g
  | langOf v1 == langOf v2 = subtype (VarU v1) (VarU v2) g
  | otherwise = Left $ SubtypeError t1 t2 "<:App - Cannot compare between languages" 
subtype t1@(AppU v1@(TV l1 _) vs1) t2@(AppU v2@(TV l2 _) vs2) g
  | length vs1 /= length vs2 = Left $ SubtypeError t1 t2 "<:App - Cannot subtype types with unequal parameter count"
  | l1 /= l2 = return $ g +> SerialConstraint t1 t2
  | v1 == v2 = compareApp vs1 vs2 g
  | otherwise = Left $ SubtypeError t1 t2 "<:App - Unequal names in AppU of the same language" 
  where
    compareApp :: [TypeU] -> [TypeU] -> Gamma -> Either TypeError Gamma
    compareApp [] [] g' = return g'
    compareApp (t1':ts1') (t2':ts2') g' = do
      g'' <- subtype t1' t2' g'
      compareApp ts1' ts2' g''
    compareApp _ _ _ = Left $ SubtypeError t1 t2 "<:App - Type mismatch in AppU"

-- subtype unordered records
subtype t1@(NamU _ v1 _ rs1) t2@(NamU _ v2 _ rs2) g = undefined -- do
  -- g' <- subtype (VarU v1) (VarU v2) g
  -- compareEntry (sort rs1) (sort rs2) g'
  -- where
  --   compareEntry :: [(MT.Text, TypeU)] -> [(MT.Text, TypeU)] -> Gamma -> Either TypeError Gamma
  --   compareEntry [] [] g2 = return g2
  --   compareEntry ((k1, t1):rs1') ((k2, t2):rs2') g2
  --     | l1 == l2 = do
  --         g3 <- subtype (VarU (TV l1 k1)) (VarU (TV l2 k2)) g2
  --         g4 <- subtype t1 t2 g3
  --         compareEntry rs1' rs2' g4
  --     | otherwise = return $ g +> SerialConstraint t1 t2
  --     where
  --       l1 = langOf t1
  --       l2 = langOf t2
  --   compareEntry _ _ _ = Left $ SubtypeError t1 t2 "Type mismatch in NamU"

--  Ea not in FV(a)
--  g1[Ea] |- A <=: Ea -| g2
-- ----------------------------------------- <:InstantiateR
--  g1[Ea] |- A <: Ea -| g2
subtype a b@(ExistU _ [] _) g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = occursCheck a b "InstantiateR" >> instantiate a b g
--  Ea not in FV(a)
--  g1[Ea] |- Ea <=: A -| g2
-- ----------------------------------------- <:InstantiateL
--  g1[Ea] |- Ea <: A -| g2
subtype a@(ExistU _ [] _) b g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = occursCheck b a "InstantiateL" >> instantiate a b g

subtype a@(AppU v1 ps1) b@(ExistU v2 ps2 _) g
  | langOf a /= langOf b = return g -- incomparable
  | otherwise = subtype (AppU v1 ps1) (ExistU v2 ps2 []) g
subtype t1@(ExistU v1 ps1 _) t2@(AppU v2 ps2) g1
  | langOf v1 /= langOf v2 = return g1 -- incomparable
  | length ps1 /= length ps2 = Left $ SubtypeError t1 t2 "InstantiateL - Expected equal number of type parameters"
  | otherwise = do
    g2 <- foldM (\g (p1, p2) -> subtype p1 p2 g) g1 (zip ps1 ps2)
    case access1 v1 (gammaContext g2) of
      Just (rs, _, ls) ->
        return $ g2 { gammaContext = rs ++ [SolvedG v1 t2] ++ ls }
      Nothing -> return g2 -- it is already solved, so do nothing

--  g1,>Ea,Ea |- [Ea/x]A <: B -| g2,>Ea,g3
-- ----------------------------------------- <:ForallL
--  g1 |- Forall x . A <: B -| g2
--
subtype (ForallU v@(TV lang _) a) b g0
  | lang /= langOf b = return g0
  | otherwise = do
      let (g1, a') = newvar lang g0
      g2 <- subtype (substituteTVar v a' a) b (g1 +> MarkG v +> a')
      cut (MarkG v) g2

--  g1,a |- A <: B -| g2,a,g3
-- ----------------------------------------- <:ForallR
--  g1 |- A <: Forall a. B -| g2
subtype a (ForallU v@(TV lang _) b) g
  | lang /= langOf a = return g
  | otherwise = subtype a b (g +> VarG v) >>= cut (VarG v)

-- fall through
subtype a b _ = Left $ SubtypeError a b "Type mismatch"



-- | Dunfield Figure 10 -- type-level structural recursion
instantiate :: TypeU -> TypeU -> Gamma -> Either TypeError Gamma

-- --  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- A1 <=: Ea1 -| g2
-- --  g2 |- Ea2 <=: [g2]A2 -| g3
-- -- ----------------------------------------- InstLApp
-- --  g1[Ea] |- Ea <=: A1 -> A2 -| g3
instantiate (ExistU _ _ _) (FunU _ _) _ = undefined
-- instantiate ta@(ExistU v@(TV lang _) [] _) tb@(FunU t1 t2) g1 = do
--   let (g2, ea1) = newvar lang g1
--       (g3, ea2) = newvar lang g2
--   g4 <-
--     case access1 v (gammaContext g3) of
--       Just (rs, _, ls) ->
--         return $ g3 { gammaContext = rs ++ [SolvedG v (FunU ea1 ea2), index ea1, index ea2] ++ ls }
--       Nothing -> Left $ InstantiationError ta tb "Error in InstLApp"
--   g5 <- instantiate t1 ea1 g4
--   g6 <- instantiate ea2 (apply g5 t2) g5
--   return g6

--  g1[Ea2,Ea1,Ea=Ea1->Ea2] |- Ea1 <=: A1 -| g2
--  g2 |- [g2]A2 <=: Ea2 -| g3
-- ----------------------------------------- InstRApp
--  g1[Ea] |- A1 -> A2 <=: Ea -| g3

instantiate (FunU _ _) (ExistU _ _ _) _ = undefined
-- instantiate ta@(FunU t1 t2) tb@(ExistU v@(TV lang _) [] _) g1 = do
--   let (g2, ea1) = newvar lang g1
--       (g3, ea2) = newvar lang g2
--   g4 <-
--     case access1 v (gammaContext g3) of
--       Just (rs, _, ls) ->
--         return $ g3 { gammaContext = rs ++ [SolvedG v (FunU ea1 ea2), index ea1, index ea2] ++ ls }
--       Nothing -> Left $ InstantiationError ta tb "Error in InstRApp"
--   g5 <- instantiate t1 ea1 g4
--   g6 <- instantiate ea2 (apply g5 t2) g5
--   return g6

--
-- ----------------------------------------- InstLAllR
--
instantiate ta@(ExistU _ _ _) tb@(ForallU v2 t2) g1
  | langOf ta /= langOf tb = return g1
  | otherwise = instantiate ta t2 (g1 +> VarG v2) >>= cut (VarG v2)
-- InstLReach or instRReach -- each rule eliminates an existential
-- Replace the rightmost with leftmost (G[a][b] --> L,a,M,b=a,R)
-- WARNING: be careful here, since the implementation adds to the front and the
-- formal syntax adds to the back. Don't change anything in the function unless
-- you really know what you are doing and have tests to confirm it.
instantiate ta@(ExistU v1 ps1 []) tb@(ExistU v2 ps2 []) g1 = do
  g2 <- foldM (\g (t1, t2) -> subtype t1 t2 g) g1 (zip ps1 ps2)
  g3 <- case access2 v1 v2 (gammaContext g2) of
    -- InstLReach
    (Just (ls, _, ms, x, rs)) -> return $ g2 { gammaContext = ls <> (SolvedG v1 tb : ms) <> (x : rs) }
    Nothing ->
      case access2 v2 v1 (gammaContext g2) of
      -- InstRReach
        (Just (ls, _, ms, x, rs)) ->
          return $ g2 { gammaContext = ls <> (SolvedG v2 ta : ms) <> (x : rs) }
        Nothing -> return g2
  return g3
--  g1[Ea],>Eb,Eb |- [Eb/x]B <=: Ea -| g2,>Eb,g3
-- ----------------------------------------- InstRAllL
--  g1[Ea] |- Forall x. B <=: Ea -| g2
instantiate ta@(ForallU x b) tb@(ExistU _ [] _) g1
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
instantiate ta tb@(ExistU v [] []) g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      case access1 v (gammaContext g1) of
        (Just (ls, _, rs)) -> return $ g1 { gammaContext = ls ++ (SolvedG v ta) : rs }
        Nothing ->
          case lookupU v g1 of
            (Just _) -> return g1
            Nothing ->
              Left $ InstantiationError ta tb "Error in InstRSolve"

--  g1 |- t
-- ----------------------------------------- instLSolve
--  g1,Ea,g2 |- Ea <=: t -| g1,Ea=t,g2
instantiate ta@(ExistU v [] []) tb g1
  | langOf ta /= langOf tb = return g1
  | otherwise =
      case access1 v (gammaContext g1) of
        (Just (ls, _, rs)) -> return $ g1 { gammaContext = ls ++ (SolvedG v tb) : rs }
        Nothing ->
          case lookupU v g1 of
            (Just _) -> return g1
            Nothing -> Left $ InstantiationError ta tb "Error in InstLSolve"

-- if defaults are involved, no solving is done, but the subtypes of parameters
-- and defaults needs to be checked. 
instantiate (ExistU _ ps1 ds1) (ExistU _ ps2 ds2) g1 = do
  g2 <- foldM (\g (t1, t2) -> subtype t1 t2 g) g1 (zip ps1 ps2)
  g3 <- foldM (\g d1 -> foldM (\g' d2 -> subtype d1 d2 g') g ds2) g2 ds1
  return g3

-- bad
instantiate _ _ g = return g



-- Concrete typechecking must deal with primitive defaults, containter
-- defaults, and function overloading.

synth
  :: Gamma
  -> SAnno (Indexed Type) One (Indexed (Lang, [EType]))
  -> Either
       TypeError
       ( Gamma
       , TypeU
       , SAnno (Indexed Type) One (Indexed TypeU)
       )
--
-- ----------------------------------------- <primitive>
--  g |- <primitive expr> => <primitive type> -| g
--
--  Primitive types may have many possible defaults in a given language. For
--  example, in Rust a primitive Num may be a signed or unsigned, be a float or
--  an int, and have a size ranging from 8 to 128 bits. If no concrete types
--  are available, then the first default value will be used when the
--  TypeU is resolved. 

-- AnnoOne=>
synth g (SAnno (One (x, Idx i (l, [EType ct _ _]))) gt)
  = check g (SAnno (One (x, Idx i (l, []))) gt) ct

-- AnnoMany=>
synth g (SAnno (One (x, Idx i (l, cts@(_:_)))) gt) =
  let (g', t) = newvarRich [] [t | (EType t _ _) <- cts] (Just l) g
  in check g' (SAnno (One (x, Idx i (l, []))) gt) t

-- if there are no annotations, the SAnno can be simplified and synth' can be called
synth g (SAnno (One (x, Idx i (l, []))) gt) = do
  (g', t, x') <- synthExpr l g x
  checkAgreement (Idx i t) gt
  return (g', t, SAnno (One (x', Idx i t)) gt)


synthExpr
  :: Lang
  -> Gamma
  -> SExpr (Indexed Type) One (Indexed (Lang, [EType]))
  -> Either
       TypeError
       ( Gamma
       , TypeU
       , SExpr (Indexed Type) One (Indexed TypeU)
       )

-- Uni=>
synthExpr lang g UniS = do
  let ts = MLD.defaultNull (Just lang)
      (g', t) = newvarRich [] (MLD.defaultNull (Just lang)) (Just lang) g
  return (g' +> t, t, UniS)

-- Num=>
synthExpr lang g (NumS x) = do
  let ts = MLD.defaultNumber (Just lang)
      (g', t) = newvarRich [] (MLD.defaultNull (Just lang)) (Just lang) g
  return (g' +> t, t, NumS x)

-- Str=>
synthExpr lang g (StrS x) = do
  let ts = MLD.defaultString (Just lang)
      (g', t) = newvarRich [] (MLD.defaultNull (Just lang)) (Just lang) g
  return (g' +> t, t, StrS x)

-- Log=>
synthExpr lang g (LogS x) = do
  let ts = MLD.defaultBool (Just lang)
      (g', t) = newvarRich [] (MLD.defaultNull (Just lang)) (Just lang) g
  return (g' +> t, t, LogS x)

-- In SAnno, a VarS can only be a bound variable, thus it should only ever be
-- checked against since it's type will be known at a higher level.
synthExpr _ _ (VarS v) = Left $ UnboundVariable v

-- Acc=>
synthExpr lang g0 (AccS x k) = do
  (g1, tx, x1) <- synth g0 x
  tk <- accessRecord k tx
  return (g1, tk, AccS x1 k)
  where
    accessRecord :: MT.Text -> TypeU -> Either TypeError TypeU
    accessRecord k r@(NamU _ _ _ rs) = case lookup k rs of
      (Just t) -> return t
      Nothing -> Left $ KeyError k r
    accessRecord k r = Left $ KeyError k r

-- List=>
--
-- The elements in xs are all of the same general type, however they may be in
-- different languages.
synthExpr lang g0 (LstS (x:xs)) = do
  -- here t1 is the element type
  (g1, t1, x1) <- synth g0 x

  -- create an existential container type with sensible default
  let (g2, containerType) = newvarRich [t1] (MLD.defaultList (Just lang) t1) (Just lang) g1

  -- and t2 is the list type
  (g3, t2, listExpr) <- checkExpr lang g2 (LstS xs) containerType

  case listExpr of
    (LstS xs2) -> return (g3, t2, (LstS (x1:xs2)))
    _ -> impossible -- check never changes the top data constructor

-- Tuple=>
--
synthExpr lang g0 (TupS []) = do
  let (g1, t) = newvarRich [] (MLD.defaultTuple (Just lang) []) (Just lang) g0
  return (g1, t, TupS [])
synthExpr lang g0 (TupS (x:xs)) = do
  -- type the head
  (g1, t, x') <- synth g0 x

  -- type the tail
  (g2, tupleType, tupleExpr) <- synthExpr lang g1 (TupS xs)

  -- merge the head and tail
  (g3, t3) <- case tupleType of
    (ExistU _ ts _) -> return $ newvarRich (t:ts) (MLD.defaultTuple (Just lang) (t:ts)) (Just lang) g2
    _ -> impossible -- the tuple was created by newvarRich which can only return existentials

  xs' <- case tupleExpr of
    (TupS xs') -> return xs'
    _ -> impossible -- synth does not change data constructors

  return (g3, t3, TupS (x':xs'))

-- Rec=>
--
synthExpr lang g0 (NamS []) = do
  let (g1, t) = newvarRich [] (MLD.defaultRecord (Just lang) []) (Just lang) g0
  return (g1, t, NamS [])
synthExpr lang g0 (NamS ((k,x):rs)) = do
  -- type the head
  (g1, headType, headExpr) <- synth g0 x

  -- type the tail
  (g2, tailType, tailExpr) <- synthExpr lang g1 (NamS rs)

  -- merge the head with tail
  (g3, t3) <- case tailType of
    (ExistU _ _ [NamU _ _ _ rs]) -> return $ newvarRich [] (MLD.defaultRecord (Just lang) ((k,headType):rs)) (Just lang) g2
    _ -> impossible -- the record was created by newvarRich which can only return existentials

  tailExprs <- case tailExpr of
    (NamS xs') -> return xs'
    _ -> impossible -- synth does not change data constructors

  return (g3, t3, NamS ((k, headExpr):tailExprs))

-- Lam=>
--
-- foo xs ys = zipWith (\x y -> [1,y,x]) xs ys
synthExpr lang g0 (LamS [] x0) = do
  (g1, bodyType, bodyExpr) <- synth g0 x0
  return (g1, FunU [] bodyType, LamS [] bodyExpr)
synthExpr lang g0 (LamS (v@(EV n):vs) x) = do
  let mark = MarkG (TV (Just lang) n)
      g1 = g0 +> mark
      (g2, headType) = bindTerm lang g1 v

  (g3, tailType, tailExpr) <- synthExpr lang g2 (LamS vs x) 

  fullType <- case tailType of
    (FunU tailInputs tailOutput) -> return (FunU (headType:tailInputs) tailOutput)
    _ -> impossible -- LamS type is always a function (see base case)

  fullExpr <- case tailExpr of
    (LamS vs x) -> return $ LamS (v:vs) x
    _ -> impossible -- synthExpr does not change data constructors

  g4 <- cut mark g3

  return (g4, fullType, fullExpr)

-- App=>
--
synthExpr lang g0 (AppS v xs) = undefined
  -- (g1, t0, f1) <- synth f0
  -- (g2, t1, f2) <- applicationMany g1 xs t0
  -- -- is f2 are "real thing?"
  


  -- :: Gamma
  -- -> [SAnno (Indexed Type) One (Lang, [EType])]
  -- -> TypeU
  -- -> Either
  --      TypeError
  --      ( Gamma
  --      , TypeU
  --      , [SAnno (Indexed Type) One (Indexed TypeU)]
  --      )

-- For now, sources must be annotated by a concrete type signature. Annotations
-- are stored in the (Indexed [EType) term. If this term were not empty, it
-- would have been matched by either the AnnoOne=> or AnnoMany=> rules
-- where the expression would be checked against the annotation.
synthExpr _ _ (CallS src) = Left $ MissingConcreteSignature src
  

bindTerm :: Lang -> Gamma -> EVar -> (Gamma, TypeU)
bindTerm lang g0 v =
  let (g1, t) = newvar (Just lang) g0
      idx = AnnG v t
  in (g1 +> idx, t)


chainCheck
  :: Gamma
  -> [SAnno (Indexed Type) One (Indexed (Lang, [EType]))]
  -> TypeU
  -> Either
        TypeError
        ( Gamma
        , [( TypeU
           , SAnno (Indexed Type) One (Indexed TypeU)
          )]
        )
chainCheck g0 [] _ = Right (g0, [])
chainCheck g0 (x0:rs) t0 = do
  (g1, t1, x1) <- check g0 x0 t0
  (gn, rs') <- chainCheck g1 rs t1
  return (gn, (t1, x1):rs')



check
  :: Gamma
  -> SAnno (Indexed Type) One (Indexed (Lang, [EType]))
  -> TypeU
  -> Either
        TypeError
        ( Gamma
        , TypeU
        , SAnno (Indexed Type) One (Indexed TypeU)
        )
check g (SAnno (One (x, Idx i (l, _))) gt) t = do
  (g', t', x') <- checkExpr l g x t
  checkAgreement (Idx i t') gt
  return $ (g', t', SAnno (One (x', Idx i t')) gt)

checkExpr
  :: Lang
  -> Gamma
  -> SExpr (Indexed Type) One (Indexed (Lang, [EType]))
  -> TypeU
  -> Either
        TypeError
        ( Gamma
        , TypeU
        , SExpr (Indexed Type) One (Indexed TypeU)
        )

--  g1,x:A |- e <= B -| g2,x:A,g3
-- ----------------------------------------- -->I
--  g1 |- \x.e <= A -> B -| g2
checkExpr lang g1 (LamS vs x) t1@(FunU a b) = undefined

--  g1,x |- e <= A -| g2,x,g3
-- ----------------------------------------- Forall.I
--  g1 |- e <= Forall x.A -| g2
checkExpr lang g1 e1 t2@(ForallU x a) = undefined

--  g1 |- e => A -| g2
--  g2 |- [g2]A <: [g2]B -| g3
-- ----------------------------------------- Sub
--  g1 |- e <= B -| g3
checkExpr lang g1 e1 b = undefined



applicationMany
  :: Gamma
  -> [SAnno (Indexed Type) One (Lang, [EType])]
  -> TypeU
  -> Either
       TypeError
       ( Gamma
       , TypeU
       , [SAnno (Indexed Type) One (Indexed TypeU)]
       )
applicationMany g0 [] t0 = return (g0, t0, [])
applicationMany _ _ _ = undefined
-- applicationMany g0 (e0:es0) t0 = do
--   (g1, t1, e1) <- application g0 e0 t0
--   (g2, t2, es1) <- applicationMany g1 es0 t1
--   return (g2, FunU t1 t2, e1:es1)

application
  :: Gamma
  -> SAnno (Indexed Type) One (Lang, [EType])
  -> TypeU
  -> Either
       TypeError
       ( Gamma
       , TypeU
       , SAnno (Indexed Type) One (Indexed TypeU)
       )
--  g1 |- e <= A -| g2
-- ----------------------------------------- -->App
--  g1 |- A->C o e =>> C -| g2

application _ _ (FunU ts t) = undefined
-- application g0 (e0:es) (FunU a b) = do
--   (g1, a1, e1) <- check g e0 a
--   (g2, a2, e2) <- application g1 es b
--   let b' = apply g1 b
--   return (g1, FunU a' b', apply g' e')

--  g1,Ea |- [Ea/a]A o e =>> C -| g2
-- ----------------------------------------- Forall App
--  g1 |- Forall x.A o e =>> C -| g2
application g e (ForallU x s) = application (g +> ExistG x [] []) e (substitute x s)

--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- e <= Ea1 -| g2
-- ----------------------------------------- EaApp
--  g1[Ea] |- Ea o e =>> Ea2 -| g2
application g e (ExistU v@(TV lang _) [] _) = undefined
  -- case access1 v g of
  --   -- replace <t0> with <t0>:<ea1> -> <ea2>
  --   Just (rs, _, ls) -> do
  --     ea1 <- newvar lang
  --     ea2 <- newvar lang
  --     let t' = FunU ea1 ea2
  --         g2 = rs ++ [SolvedG v t', index ea1, index ea2] ++ ls
  --     (g3, a', e2) <- check g2 e ea1
  --     let f' = FunU a' (apply g3 ea2)
  --     return (g3, f', e2)
  --   -- if the variable has already been solved, use solved value
  --   Nothing -> case lookupU v g of
  --     (Just (FunU t1 t2)) -> do
  --       (g2, _, e2) <- check g e t1
  --       return (g2, FunU t1 t2, e2)
  --     _ -> Left ApplicationOfNonFunction
    

application _ _ _ = Left ApplicationOfNonFunction



checkAgreement :: Indexed TypeU -> Indexed Type -> Either TypeError ()
checkAgreement = undefined



lookupSourceTypes :: Int -> Source -> MorlocMonad [EType]
lookupSourceTypes i src = do
  mayts <- MM.metaTermTypes i
  case mayts of
    Nothing -> MM.throwError . CallTheMonkeys $ "Missing TermTypes for source"
    (Just ts) -> case [ es | (_, src', es, _) <- termConcrete ts, src' == src] of
      [es'] -> return es'
      _ -> MM.throwError . CallTheMonkeys $ "Expected exactly one list of types for a source"


chain2 :: Monad m => (s -> a -> m (s, b, c)) -> s -> [a] -> m (s, [(b, c)])
chain2 f s0 [] = return (s0, [])
chain2 f s0 (x:xs) = do
  (s1, x, y) <- f s0 x
  (sn, xs') <- chain2 f s1 xs
  return (sn, (x,y):xs')


-- I don't need explicit convert functions, necessarily. The pack functions can
-- be used to convert between values that are in the same language. Because
-- they hae the same general types and the general types define the packed
-- form. Minimizing convert steps would certainly be a valuable optimization,
-- but I can leave that for later.

-- -- | Ensure that all concrete source signatures match general types
-- checkSources :: SAnno (Indexed Type) One (Indexed Lang) -> MorlocMonad ()
-- checkSources (SAnno (Many xs) i) = do
--   mayts <- MM.metaTermTypes i
--   case mayts |>> toTypePairs >>= mapM (uncurry checkConcrete) of
--     (Just ((e1, e2):_)) -> undefined -- create error message for mismatched general/concrete types
--     _ -> return ()
--   where
--   toTypePairs :: TermTypes -> [(EType, EType)]
--   toTypePairs (TermTypes Nothing _ _) = []
--   toTypePairs (TermTypes (Just gt) cts _) = [(gt, ct) | ct <- concat [ts | (_, _, ts, _) <- cts]]
--
--   -- return Nothing if the types are the same, otherwise return the types
--   checkConcrete :: EType -> EType -> Maybe (EType, EType)
--   checkConcrete e1 e2
--     | checkConcreteType e1 e2 = Nothing
--     | otherwise = Just (e1, e2)
--
-- -- | This is a key function that is exported primarily so it can be tested.
-- checkConcreteType :: EType -> EType -> Bool
-- checkConcreteType = undefined
