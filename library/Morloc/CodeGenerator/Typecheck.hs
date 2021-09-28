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

-- I don't need explicit convert functions, necessarily. The pack functions can
-- be used to convert between values that are in the same language. Because
-- they hae the same general types and the general types define the packed
-- form. Minimizing convert steps would certainly be a valuable optimization,
-- but I can leave that for later.

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
-- 
-- function subtypes are *contravariant* with respect to the input, that is,
-- the subtypes are reversed so we have b1<:a1 instead of a1<:b1.
subtype (FunU [] a2) (FunU [] b2) g = subtype a2 b2 g
subtype (FunU (a1:rs1) a2) (FunU (b1:rs2) b2) g1 = do
  g2 <- subtype b1 a1 g1
  subtype (apply g2 (FunU rs1 a2)) (apply g2 (FunU rs2 b2)) g2

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
subtype (NamU _ v1 _ []) (NamU _ v2 _ []) g = subtype (VarU v1) (VarU v2) g
subtype t1@(NamU _ _ _ []) t2@(NamU _ _ _ _) g =
  Left $ SubtypeError t1 t2 "NamU - Unequal number of fields"
subtype t1@(NamU _ _ _ _ ) t2@(NamU _ _ _ []) g =
  Left $ SubtypeError t1 t2 "NamU - Unequal number of fields"
subtype t1@(NamU o1 v1 p1 ((k1,x1):rs1)) t2@(NamU o2 v2 p2 rs2') g0
  | langOf v1 == langOf t2 =
    case filterApart (\(k2, x) -> k2 == k1) rs2' of
      (Nothing, _) -> Left $ SubtypeError t1 t2 "NamU - Unequal fields"
      (Just (_, x2), rs2) -> do
          g1 <- subtype x1 x2 g0
          g2 <- subtype (NamU o1 v1 p1 rs1) (NamU o2 v2 p2 rs2') g1
          return g2
  | otherwise = return g0 -- incomparable

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
instantiate ta@(ExistU v@(TV lang _) [] _) tb@(FunU as b) g1 = do
  let (g2, eas) = statefulMap (\g _ -> newvar lang g) g1 as 
      (g3, eb) = newvar lang g2
  g4 <- case access1 v (gammaContext g3) of
      Just (rs, _, ls) ->
        return $ g3 { gammaContext = rs ++ [SolvedG v (FunU eas eb)] ++ map index eas ++ ls }
      Nothing -> Left $ InstantiationError ta tb "Error in InstLApp"
  g5 <- foldlM (\g (e, t) -> instantiate e t g) g4 (zip eas as)
  instantiate eb (apply g5 b) g5 



--  g1[Ea2,Ea1,Ea=Ea1->Ea2] |- Ea1 <=: A1 -| g2
--  g2 |- [g2]A2 <=: Ea2 -| g3
-- ----------------------------------------- InstRApp
--  g1[Ea] |- A1 -> A2 <=: Ea -| g3
instantiate ta@(FunU as b) tb@(ExistU v@(TV lang _) [] _) g1 = do
  let (g2, eas) = statefulMap (\g _ -> newvar lang g) g1 as 
      (g3, eb) = newvar lang g2
  g4 <- case access1 v (gammaContext g3) of
    Just (rs, _, ls) ->
        return $ g3 { gammaContext = rs ++ [SolvedG v (FunU eas eb)] ++ map index eas ++ ls }
    Nothing -> Left $ InstantiationError ta tb "Error in InstRApp"
  g5 <- foldlM (\g (e, t) -> instantiate t e g) g4 (zip eas as)
  g6 <- instantiate eb (apply g5 b) g5
  return g6

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
synth g (SAnno (One (x, Idx i (l, [EType ct _ _]))) m)
  = check g (SAnno (One (x, Idx i (l, []))) m) ct

-- AnnoMany=>
synth g (SAnno (One (x, Idx i (l, cts@(_:_)))) m) =
  let (g', t) = newvarRich [] [t | (EType t _ _) <- cts] (Just l) g
  in check g' (SAnno (One (x, Idx i (l, []))) m) t

-- if there are no annotations, the SAnno can be simplified and synth' can be called
synth g0 (SAnno (One (x, Idx i (l, []))) m@(Idx _ tg)) = do
  (g1, t, x') <- synthExpr l g0 x
  g2 <- subtype t (type2typeu tg) g1
  return (g2, t, SAnno (One (x', Idx i t)) m)

-- check g0 (SAnno (One (x, Idx i (l, _))) m@(Idx _ tg)) t = do
--   (g1, t', x') <- checkExpr l g0 x t
--   g2 <- subtype t' (type2typeu tg) g1
--   return $ (g2, t', SAnno (One (x', Idx i t')) m)

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
synthExpr lang g0 (AppS f []) = do
  (g1, t1, f1) <- synth g0 f
  return (g1, t1, AppS f1 [])
synthExpr lang g0 (AppS f xs) = do

  -- get the potentially qualified function type and expression
  (g1, qfunType, qfunExpr) <- synth g0 f

  -- unqualify the expression
  (g2, uFunType, uFunExpr) <- application g1 qfunType qfunExpr

  -- extract output type from the type of f
  (ts, outputType) <- case uFunType of
    (FunU ts t) -> return (ts, t)
    _ -> impossible

  -- create a tuple from the input arguments
  let tupleType = head $ MLD.defaultTuple (Just lang) ts

  -- check the tuple of argument expressions against a tuple of argument types,
  -- collect the types of missing arguments (partial application)
  (leftoverTypes, (g3, argTupleType, argTupleExpr)) <- case compare (length ts) (length xs) of
    -- there are more inputs than arguments: partial application
    GT -> case splitAt (length xs) ts of
      (args, remainder) -> checkExpr lang g2 (TupS xs) tupleType |>> (,) remainder
    -- there are the same number of inputs and arguments: full application
    EQ -> checkExpr lang g2 (TupS xs) tupleType |>> (,) []
    -- there are more arguments than inputs: TYPE ERROR!!!
    LT -> Left TooManyArguments

  -- extract the types of the input arguments
  inputTypes <- case argTupleType of
    (AppU _ ts') -> return ts'
    _ -> impossible

  -- extract the input expressions
  inputExprs <- case argTupleExpr of
    (TupS xs') -> return xs'
    _ -> impossible

  -- synthesize the final type
  finalType <- case leftoverTypes of
    -- full application, just return the output type
    [] -> return outputType
    -- partial application, create a new function with unapplied types
    ts -> return (FunU ts outputType)

  -- put the AppS back together with the synthesized function and input expressions
  return (g3, finalType, AppS uFunExpr inputExprs)
  

-- For now, sources must be annotated by a concrete type signature. Annotations
-- are stored in the (Indexed [EType) term. If this term were not empty, it
-- would have been matched by either the AnnoOne=> or AnnoMany=> rules
-- where the expression would be checked against the annotation.
synthExpr _ _ (CallS src) = Left $ MissingConcreteSignature src


application
  :: Gamma
  -> TypeU
  -> SAnno (Indexed Type) One (Indexed TypeU)
  -> Either
       TypeError
       ( Gamma
       , TypeU
       , SAnno (Indexed Type) One (Indexed TypeU)
       )
application g0 t0 (SAnno (One (e0, (Idx j _))) gt) = do
  (g1, t1, e1) <- applicationExpr g0 t0 e0
  return (g1, t1, SAnno (One (e1, Idx j t1)) gt) 

applicationExpr
  :: Gamma
  -> TypeU
  -> SExpr (Indexed Type) One (Indexed TypeU)
  -> Either
       TypeError
       ( Gamma
       , TypeU
       , SExpr (Indexed Type) One (Indexed TypeU)
       )
applicationExpr g0 (ForallU v t) e = applicationExpr (g0 +> ExistG v [] []) (substitute v t) e
applicationExpr g0 t@(FunU _ _) e = return (g0, t, e)
applicationExpr _ _ _ = Left ApplicationOfNonFunction
  

bindTerm :: Lang -> Gamma -> EVar -> (Gamma, TypeU)
bindTerm lang g0 v =
  let (g1, t) = newvar (Just lang) g0
      idx = AnnG v t
  in (g1 +> idx, t)


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
check g0 (SAnno (One (x, Idx i (l, _))) m@(Idx _ tg)) t = do
  (g1, t', x') <- checkExpr l g0 x t
  g2 <- subtype t' (type2typeu tg) g1
  return $ (g2, t', SAnno (One (x', Idx i t')) m)


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

------ this rule is for the deep style, not the new wide style
--  g1,x:A |- e <= B -| g2,x:A,g3
-- ----------------------------------------- -->I
--  g1 |- \x.e <= A -> B -| g2
--
-- t2 will have form (FunU [] b) if the function is fully applied, but partial application is allowed
checkExpr lang g1 (LamS [] e1) (FunU as1 b1) = do
  (g2, b2, e2) <- check g1 e1 b1
  return (g2, FunU as1 b2, LamS [] e2)
checkExpr lang g1 (LamS (v:vs) e1) (FunU (a1:as1) b1) = do
  -- defined x:A
  let vardef = AnnG v a1
      g2 = g1 +> vardef 
  -- peal off one layer of bound terms and check
  (g3, t3, e2) <- checkExpr lang g2 (LamS vs e1) (FunU as1 b1)

  -- ignore trailing context `x:A,g3`
  g4 <- cut vardef g3

  -- construct the final type
  t4 <- case t3 of
    (FunU as2 b2) -> return $ FunU (a1:as2) b2
    _ -> impossible

  -- construct the final expression
  e3 <- case e2 of
    (LamS vs' body) -> return $ LamS (v:vs') body
    _ -> impossible

  return (g4, t4, e3)
  

--  g1,x |- e <= A -| g2,x,g3
-- ----------------------------------------- Forall.I
--  g1 |- e <= Forall x.A -| g2
checkExpr lang g1 e1 t2@(ForallU x a) = do
  (g2, _, e2) <- checkExpr lang (g1 +> VarG x) e1 a
  g3 <- cut (VarG x) g2
  let t3 = apply g3 t2
  return (g3, t3, e2)

--  g1 |- e => A -| g2
--  g2 |- [g2]A <: [g2]B -| g3
-- ----------------------------------------- Sub
--  g1 |- e <= B -| g3
checkExpr lang g1 e1 b = do
  (g2, a, e2) <- synthExpr lang g1 e1
  let a' = apply g2 a
      b' = apply g2 b
  g3 <- subtype a' b' g2
  return (g3, a', e2)


type2typeu :: Type -> TypeU
type2typeu (VarT v) = VarU v
type2typeu (UnkT v) = ForallU v (VarU v)
type2typeu (FunT ts t) = FunU (map type2typeu ts) (type2typeu t)
type2typeu (AppT v ts) = AppU v (map type2typeu ts)
type2typeu (NamT o n ps rs) = NamU o n ps [(k, type2typeu x) | (k,x) <- rs]
