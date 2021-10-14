{-|
Module      : Morloc.Frontend.Typecheck
Description : Core inference module
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.Typecheck (typecheck, resolveTypes) where

import Morloc.Frontend.Namespace
import Morloc.Typecheck.Internal
import Morloc.Pretty
import Morloc.Typecheck.Pretty
import Morloc.Data.Doc
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as R
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Monad as MM

import qualified Control.Monad.State as CMS
import qualified Data.Map as Map

-- | Each SAnno object in the input list represents one exported function.
-- Modules, scopes, imports and and everything else are abstracted away,
-- wrapped into GMeta or stored in the Stack state.
--
-- Check the general types, do nothing to the concrete types which may only be
-- solved after segregation. Later the concrete types will need to be checked
-- for type consistency, correctness of packers, inferences of packers (both
-- for serialization and for casting).
typecheck
  :: [SAnno Int Many Int]
  -> MorlocMonad [SAnno (Indexed TypeU) Many Int]
typecheck es = mapM run es where
    run :: SAnno Int Many Int -> MorlocMonad (SAnno (Indexed TypeU) Many Int)
    run e0 = do
      initialContext <- createGeneralContext e0

      -- standardize names for lambda bound variables (e.g., x0, x1 ...)
      let ((_, g), e1) = renameSAnno (Map.empty, initialContext) e0
      (_, _, e2) <- synthG' g e1
      return e2

resolveTypes :: SAnno (Indexed TypeU) Many Int -> SAnno (Indexed Type) Many Int
resolveTypes (SAnno (Many es) (Idx i t))
  = SAnno (Many (map (\(e, i') -> (f e, i')) es)) (Idx i (typeOf t)) where
  f :: SExpr (Indexed TypeU) Many Int -> SExpr (Indexed Type) Many Int
  f (AccS x k) = AccS (resolveTypes x) k
  f (AppS x xs) = AppS (resolveTypes x) (map resolveTypes xs) 
  f (LamS vs x) = LamS vs (resolveTypes x)
  f (LstS xs) = LstS (map resolveTypes xs)
  f (TupS xs) = TupS (map resolveTypes xs)
  f (NamS rs) = NamS (zip (map fst rs) (map (resolveTypes . snd) rs))
  f (NumS x) = NumS x
  f (LogS x) = LogS x
  f (StrS x) = StrS x
  f (CallS x) = CallS x
  f UniS = UniS
  f (VarS x) = VarS x

-- lookup a general type associated with an index
-- standardize naming of qualifiers
lookupType :: Int -> Gamma -> MorlocMonad (Maybe (Gamma, TypeU))
lookupType i g = do
  m <- CMS.gets stateSignatures
  return $ case GMap.lookup i m of
    GMapJust (TermTypes (Just (EType t _ _)) _ _) -> Just $ rename g t
    _ -> Nothing

-- prepare a general, indexed typechecking error
gerr :: Int -> TypeError -> MorlocMonad a
gerr i e = MM.throwError $ IndexedError i (GeneralTypeError e)

synthG
  :: Gamma
  -> SAnno Int Many Int
  -> MorlocMonad
       ( Gamma
       , TypeU
       , SAnno (Indexed TypeU) Many Int
       )
-- it is possible to export just a type signature
synthG g (SAnno (Many []) i) = do
  maybeType <- lookupType i g
  case maybeType of
    (Just (g', t)) -> return (g', t, SAnno (Many []) (Idx i t))
    Nothing -> gerr i EmptyExpression

synthG g0 (SAnno (Many ((e0, j):es)) i) = do
  -- if a type annotation exists for this term check against it
  -- otherwise synthesize a type
  maybeType <- lookupType i g0
  (g1, t1, e1) <- case maybeType of
    Nothing  -> synthE' i g0 e0
    (Just (g', t)) -> say "fetching annotation" >> checkE' i g' e0 t

  -- then check all other implementations against the first one
  (g2, t2, SAnno (Many es') _) <- checkG' g1 (SAnno (Many es) i) t1

  -- finally cons the head element back and apply everything we learned
  let finalExpr = applyS g2 $ SAnno (Many ((e1, j):es')) (Idx i t2)

  return (g2, t2, finalExpr)

checkG
  :: Gamma
  -> SAnno Int Many Int
  -> TypeU
  -> MorlocMonad
       ( Gamma
       , TypeU
       , SAnno (Indexed TypeU) Many Int
       )
checkG g (SAnno (Many []) i) t = return (g, t, SAnno (Many []) (Idx i t)) 
checkG g0 (SAnno (Many ((e, j):es)) i) t0 = do 
  (g1, t1, e') <- checkE' i g0 e t0
  (g2, t2, SAnno (Many es') idType) <- checkG' g1 (SAnno (Many es) i) t1
  return (g2, t2, SAnno (Many ((e', j):es')) idType)


synthE
  :: Int
  -> Gamma
  -> SExpr Int Many Int
  -> MorlocMonad
       ( Gamma
       , TypeU
       , SExpr (Indexed TypeU) Many Int
       )

synthE _ g (UniS) = return (g, MLD.defaultGeneralType UniS, UniS)
synthE _ g (NumS x) = return (g, MLD.defaultGeneralType (NumS x), NumS x)
synthE _ g (LogS x) = return (g, MLD.defaultGeneralType (LogS x), LogS x)
synthE _ g (StrS x) = return (g, MLD.defaultGeneralType (StrS x), StrS x)

synthE i g (AccS e k) = do
  (g1, t1, e1) <- synthG' g e
  valType <- case t1 of
    (NamU _ _ _ rs) -> case lookup k rs of
      Nothing -> gerr i (KeyError k t1)
      (Just t) -> return t
    _ -> gerr i (KeyError k t1)
  return (g1, valType, AccS e1 k)
synthE _ g (AppS f []) = do
  (g1, t1, f1) <- synthG' g f
  return (g1, t1, AppS f1 [])
synthE i g0 e@(AppS f xs0) = do
  -- synthesize the type of the function
  (g1, funType0, funExpr0) <- synthG g0 f

  -- extend the function type with the type of the expressions it is applied to
  (g2, funType1, inputExprs) <- application' i g1 xs0 funType0

  -- determine the type after application
  appliedType <- case funType1 of
    (FunU ts t) -> case drop (length inputExprs) ts of 
      [] -> return t -- full application
      rs -> return $ FunU rs t -- partial application
    _ -> error "impossible"

  -- put the AppS back together with the synthesized function and input expressions
  return (g2, apply g2 appliedType, AppS (applyS g2 funExpr0) inputExprs)

synthE i g0 f@(LamS vs x) = do
  -- create existentials for everything and pass it off to check
  let (g1, ts) = statefulMap (\g' _ -> newvar Nothing g') g0 vs
      (g2, ft) = newvar Nothing g1
      finalType = FunU ts ft
  checkE' i g2 f finalType

synthE _ g (LstS []) =
  let (g1, itemType) = newvar Nothing g
      listType = head $ MLD.defaultList Nothing itemType
  in return (g1, listType, LstS [])
synthE i g (LstS (e:es)) = do
  (g1, itemType, itemExpr) <- synthG' g e 
  (g2, listType, listExpr) <- checkE' i g1 (LstS es) (head $ MLD.defaultList Nothing itemType)
  case listExpr of
    (LstS es') -> return (g2, listType, LstS (itemExpr:es'))
    _ -> error "impossible"

synthE _ g (TupS []) =
  let t = head $ MLD.defaultTuple Nothing []
  in return (g, t, TupS [])
synthE i g (TupS (e:es)) = do
  -- synthesize head
  (g1, itemType, itemExpr) <- synthG' g e

  -- synthesize tail
  (g2, tupleType, tupleExpr) <- synthE' i g1 (TupS es)

  -- merge the head and tail
  t3 <- case tupleType of
    (AppU _ ts) -> return . head $ MLD.defaultTuple Nothing (apply g2 itemType : ts)
    _ -> error "impossible" -- the general tuple will always be (AppU _ _)

  xs' <- case tupleExpr of
    (TupS xs') -> return xs'
    _ -> error "impossible" -- synth does not change data constructors

  return (g2, t3, TupS (itemExpr:xs'))

synthE _ g (NamS []) = return (g, head $ MLD.defaultRecord Nothing [], NamS [])
synthE i g0 (NamS ((k,x):rs)) = do
  -- type the head
  (g1, headType, headExpr) <- synthG' g0 x

  -- type the tail
  (g2, tailType, tailExpr) <- synthE' i g1 (NamS rs)

  -- merge the head with tail
  t <- case tailType of
    (NamU o1 n1 ps1 rs1) -> return $ NamU o1 n1 ps1 ((k, apply g2 headType):rs1)
    _ -> error "impossible" -- the synthE on NamS will always return NamU type

  tailExprs <- case tailExpr of
    (NamS xs') -> return xs'
    _ -> error "impossible" -- synth does not change data constructors

  return (g2, t, NamS ((k, headExpr):tailExprs))

-- Sources are axiomatic. They are they type they are said to be.
synthE i g (CallS src) = gerr i (MissingGeneralSignature src)

-- Any morloc variables should have been expanded by treeify. Any bound
-- variables should be checked against. I think (this needs formalization).
synthE i g (VarS v) = do
  -- is this a bound variable that has already been solved
  (g', t') <- return $ case lookupE v g of 
    -- yes, return the solved type
    (Just t) -> (g, t)
    -- no, so is it a variable that has a type annotation?
    Nothing -> newvar Nothing g
  return (g', t', VarS v)


application
  :: Int
  -> Gamma
  -> [SAnno Int Many Int] -- the expressions that are passed to the function
  -> TypeU -- the function type
  -> MorlocMonad
      ( Gamma
      , TypeU -- output function type
      , [SAnno (Indexed TypeU) Many Int] -- @e@, with type annotation
      )

--  g1 |- e <= A -| g2
-- ----------------------------------------- -->App
--  g1 |- A->C o e =>> C -| g2
application i g0 es0 (FunU as0 b0) = do
  (g1, as1, es1, remainder) <- zipCheck i g0 es0 as0
  let es2 = map (applyS g1) es1 
      funType = apply g1 $ FunU (as1 <> remainder) b0
  say $ "remainder:" <+> vsep (map prettyGreenTypeU remainder)
  return (g1, funType, es2)

--  g1,Ea |- [Ea/a]A o e =>> C -| g2
-- ----------------------------------------- Forall App
--  g1 |- Forall x.A o e =>> C -| g2
application i g0 es (ForallU v s) = application' i (g0 +> v) es (substitute v s)

--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- e <= Ea1 -| g2
-- ----------------------------------------- EaApp
--  g1[Ea] |- Ea o e =>> Ea2 -| g2
application i g0 es (ExistU v [] _) =
  case access1 v (gammaContext g0) of
    -- replace <t0> with <t0>:<ea1> -> <ea2>
    Just (rs, _, ls) -> do
      let (g1, veas) = statefulMap (\g _ -> tvarname g "v" Nothing) g0 es
          (g2, vea) = tvarname g1 "v" Nothing
          eas = [ExistU v [] [] | v <- veas]
          ea = ExistU vea [] []
          f = FunU eas ea
          g3 = g2 {gammaContext = rs <> [SolvedG v f] <> map index eas <> [index ea] <> ls}
      (g4, _, es', _) <- zipCheck i g3 es eas
      return (g4, apply g4 f, es')
    -- if the variable has already been solved, use solved value
    Nothing -> case lookupU v g0 of
      (Just (FunU ts t)) -> do
        (g1, ts', es', _) <- zipCheck i g0 es ts
        return (g1, apply g1 (FunU ts' t), es')
      _ -> gerr i ApplicationOfNonFunction

application i _ e t = do
  gerr i ApplicationOfNonFunction



-- Tip together the arguments passed to an application
zipCheck
  :: Int
  -> Gamma
  -> [SAnno Int Many Int]
  -> [TypeU]
  -> MorlocMonad
    ( Gamma
    , [TypeU]
    , [SAnno (Indexed TypeU) Many Int]
    , [TypeU] -- remainder
    )
-- check the first elements, cdr down the remaining values
zipCheck i g0 (x0:xs0) (t0:ts0) = do
  (g1, t1, x1) <- checkG' g0 x0 t0
  (g2, ts1, xs1, remainder) <- zipCheck i g1 xs0 ts0
  return (g2, t1:ts1, x1:xs1, remainder)
-- If there are fewer arguments than types, this may be OK, just partial application
zipCheck _ g0 [] ts = return (g0, [], [], ts)
-- If there are fewer types than arguments, then die
zipCheck i _ es [] = gerr i TooManyArguments


checkE
  :: Int
  -> Gamma
  -> SExpr Int Many Int
  -> TypeU
  -> MorlocMonad
       ( Gamma
       , TypeU
       , SExpr (Indexed TypeU) Many Int
       )
checkE i g1 (LstS (e:es)) (AppU v [t]) = do
  (g2, t2, e') <- checkG' g1 e t 
  -- LstS [] will go to the normal Sub case
  (g3, t3, LstS es') <- checkE i g2 (LstS es) (AppU v [t2])
  return (g3, t3, (LstS (map (applyS g3) (e':es'))))

checkE _ g1 (LamS [] e1) (FunU [] b1) = do
  (g2, b2, e2) <- checkG' g1 e1 b1
  return (g2, FunU [] b2, LamS [] e2)

checkE _ g1 (LamS [] e1) t = do
  (g2, t, e2) <- checkG' g1 e1 t
  return (g2, t, LamS [] e2)

checkE i g1 (LamS (v:vs) e1) (FunU (a1:as1) b1) = do
  -- defined x:A
  let vardef = AnnG v a1
      g2 = g1 +> vardef

  -- peal off one layer of bound terms and check
  (g3, t3, e2) <- checkE' i g2 (LamS vs e1) (FunU as1 b1)

  -- construct the final type
  t4 <- case t3 of
    (FunU as2 b2) -> return $ FunU (a1:as2) b2
    _ -> error "impossible"

  let t5 = apply g3 t4

  -- construct the final expression
  e3 <- case e2 of
    (LamS vs' body) -> return $ LamS (v:vs') body
    _ -> error "impossible"

  -- ignore trailing context `x:A,g3`
  g4 <- cut' i vardef g3

  return (g4, t5, e3)

  return (g3, t5, e3)

checkE i g1 e1 (ForallU v a) = checkE' i (g1 +> v) e1 (substitute v a)

checkE i g1 e1 b = do
  (g2, a, e2) <- synthE' i g1 e1
  let a' = apply g2 a
      b' = apply g2 b
  g3 <- subtype' i a' b' g2
  return (g3, apply g3 b', e2)

subtype' :: Int -> TypeU -> TypeU -> Gamma -> MorlocMonad Gamma
subtype' i a b g = do
  say $ parens (prettyGreenTypeU a) <+> "<:" <+> parens (prettyGreenTypeU b)
  case subtype a b g of
    (Left err') -> gerr i err'
    (Right x) -> return x

cut' :: Int -> GammaIndex -> Gamma -> MorlocMonad Gamma
cut' i idx g = case cut idx g of
  (Left terr) -> gerr i terr
  (Right x) -> return x

unpartial :: TypeU -> TypeU
unpartial (FunU ts1 (FunU ts2 x)) = unpartial $ FunU (ts1 <> ts2) x
unpartial x = x

---- debugging

enter :: Doc R.AnsiStyle -> MorlocMonad ()
enter d = do
  depth <- MM.incDepth
  debugLog $ pretty (take depth (repeat '-')) <> ">" <+> d <> "\n"

say :: Doc R.AnsiStyle -> MorlocMonad ()
say d = do
  depth <- MM.getDepth
  debugLog $ pretty (take depth (repeat ' ')) <> ":" <+> d <> "\n"

seeGamma :: Gamma -> MorlocMonad ()
seeGamma g = say $ nest 4 $ "Gamma:" <> line <> (vsep (map prettyGammaIndex (gammaContext g)))

seeType :: TypeU -> MorlocMonad ()
seeType t = say $ prettyGreenTypeU t

leave :: Doc R.AnsiStyle -> MorlocMonad ()
leave d = do
  depth <- MM.decDepth
  debugLog $ "<" <> pretty (take (depth+1) (repeat '-')) <+> d <> "\n"

debugLog :: Doc R.AnsiStyle -> MorlocMonad ()
debugLog d = do
  verbosity <- MM.gets stateVerbosity
  if verbosity > 0
    then (liftIO . putDoc) d
    else return ()

zipCheck' i g es ts = do
  enter "zipCheck" 
  r@(g, ts, es, rs) <- zipCheck i g es ts
  leave "zipCheck"
  return r

synthG' g x = do
  -- enter "synthG"
  r <- synthG g x
  -- leave "synthG"
  return r

checkG' g x t = do
  -- enter "checkG"
  r <- checkG g x t
  -- leave "checkG"
  return r

synthE' i g x = do
  enter "synthE"
  peak x
  seeGamma g
  r@(g', t, _) <- synthE i g x 
  leave "synthE"
  seeGamma g'
  seeType t
  return r

checkE' i g x t = do
  enter "checkE"
  peak x
  seeType t
  seeGamma g
  r@(g', t', _) <- checkE i g x t 
  leave "checkE"
  seeType t'
  seeGamma g'
  return r

application' i g es t = do
  enter "application"
  seeGamma g
  seeType t
  mapM_ peakGen es
  r@(g',t',es') <- application i g es t
  leave "application"
  seeGamma g'
  seeType t'
  mapM_ peakGen es'
  return r


prettyCon :: SExpr g Many Int -> Doc ann
prettyCon (UniS) = "UniS"
prettyCon (VarS v) = "VarS<" <> pretty v <> ">"
prettyCon (AccS x k ) = "AccS" <+> pretty k <+> parens (prettyGen x)
prettyCon (AppS f xs) = "AppS" <+> parens (prettyGen f) <+> tupled (map prettyGen xs)
prettyCon (LamS vs x) = "LamS" <+> tupled (map pretty vs) <+> braces (prettyGen x)
prettyCon (LstS xs) = "LstS" <+> tupled (map prettyGen xs)
prettyCon (TupS xs) = "TupS" <+> tupled (map prettyGen xs)
prettyCon (NamS rs) = "NamS" <+> tupled (map (\(k,x) -> pretty k <+> "=" <+> prettyGen x) rs)
prettyCon (NumS x) = "NumS<" <> viaShow x <> ">"
prettyCon (LogS x) = "LogS<" <> viaShow x <> ">"
prettyCon (StrS x) = "StrS<" <> viaShow x <> ">"
prettyCon (CallS src) = "NumS<" <> pretty src <> ">"

prettyGen :: SAnno g Many Int -> Doc ann
prettyGen (SAnno (Many [(e, _)]) _) = prettyCon e
prettyGen (SAnno (Many _) _) = "..."

peak :: SExpr g Many Int -> MorlocMonad ()
peak = say . prettyCon
-- peak x = say $ f x where

peakGen :: SAnno g Many Int -> MorlocMonad ()
peakGen = say . prettyGen
-- peak x = say $ f x where
