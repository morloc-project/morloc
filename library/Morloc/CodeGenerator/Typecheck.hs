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
import Morloc.Typecheck.Internal
import Morloc.Pretty
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Frontend.Lang.DefaultTypes as MLD
import Morloc.Frontend.PartialOrder ()
import qualified Control.Monad.State as CMS
import qualified Morloc.Data.GMap as GMap

-- I don't need explicit convert functions, necessarily. The pack functions can
-- be used to convert between values that are in the same language. Because
-- they hae the same general types and the general types define the packed
-- form. Minimizing convert steps would certainly be a valuable optimization,
-- but I can leave that for later.

typecheck
  :: SAnno (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (SAnno Int One (Indexed TypeP))
typecheck e0 = do

  say "------ entering typechecker ------"
  peakGen e0
  say "---------------^------------------"

  let g0 = Gamma {gammaCounter = 0, gammaContext = []}

  -- -- FIXME: should typechecking here consider the packers?
  -- packers <- MM.gets statePackers
  (g1, e1) <- retrieveTypes g0 e0

  say "------ exprssion after type retrieval ------"
  peakGen e1
  say "----------------------^---------------------"

  (g2, t, e2) <- synthG g1 e1 

  -- show the final gamma and type if verbose
  say "------ exiting typechecker ------"
  seeGamma g2
  say $ pretty t
  say "---------------^-----------------"
  weaveAndResolve (applyCon g2 e2)

-- | Load the known concrete types into the tree. This is all the information
-- necessary for concrete type checking.
retrieveTypes
  :: Gamma
  -> SAnno (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (Gamma, SAnno (Indexed Type) One (Indexed (Lang, [EType])))
retrieveTypes g0 e@(SAnno (One (x0, Idx i lang)) g@(Idx j _)) = do
  (g1, ts) <- case x0 of
    (CallS src) -> do
      mayts <- MM.metaTermTypes j
      case fmap termConcrete mayts of
        (Just ts) -> case [es | (_, es, Just (Idx _ src')) <- ts, src' == src] of
          [es] -> do
              let (g1', ts') = statefulMap rename g0 (map etype es)
              return $ (g1', zipWith (\e t -> e {etype = t}) es ts')
          _ -> MM.throwError . CallTheMonkeys $ "Malformed TermTypes"
        Nothing -> MM.throwError . CallTheMonkeys $ "Missing TermTypes"
    _ -> return (g0, [])

  say $ "retrieveTypes" <+> pretty i
  say $ pretty ts
  peakGen e
  say $ "----------------------"

  (g2, x1) <- case x0 of
    UniS -> return (g1, UniS)
    (VarS v) -> return $ (g1, VarS v)
    (AccS x k) -> do
      (g', x') <- retrieveTypes g1 x
      return (g', AccS x' k)
    (LstS xs) -> do
      (g', xs') <- statefulMapM retrieveTypes g1 xs
      return (g', LstS xs')
    (TupS xs) -> do
      (g', xs') <- statefulMapM retrieveTypes g1 xs
      return (g', TupS xs')
    (LamS vs x) -> do
      (g', x') <- retrieveTypes g1 x
      return (g', LamS vs x')
    (AppS x xs) -> do
      (g', x') <- retrieveTypes g1 x
      (g'', xs') <- statefulMapM retrieveTypes g' xs
      return (g'', AppS x' xs')
    (NumS x) -> return $ (g1, NumS x)
    (LogS x) -> return $ (g1, LogS x)
    (StrS x) -> return $ (g1, StrS x)
    (NamS rs) -> do
      (g', xs') <- statefulMapM retrieveTypes g1 (map snd rs)
      return $ (g', NamS (zip (map fst rs) xs'))
    (CallS src) -> return $ (g1, CallS src)

  let e2 = SAnno (One (x1, Idx i (lang, ts))) g
  peakGen e2
  say $ "======================"

  return (g2, e2)


weaveAndResolve
  :: SAnno (Indexed Type) One (Indexed TypeU)
  -> MorlocMonad (SAnno Int One (Indexed TypeP))
weaveAndResolve e@(SAnno (One (x0, Idx i ct)) (Idx j gt)) = do
  pt <- weaveResolvedTypes gt (typeOf ct)
  x1 <- case x0 of
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
  return $ SAnno (One (x1, Idx i pt)) j

-- Concrete typechecking must deal with primitive defaults, containter
-- defaults, and function overloading.

-- lookup a general type associated with an index
-- standardize naming of qualifiers
lookupType :: Int -> Gamma -> MorlocMonad (Maybe (Gamma, TypeU))
lookupType i g = do
  m <- CMS.gets stateSignatures
  return $ case GMap.lookup i m of
    GMapJust (TermTypes (Just (EType t _ _)) _ _) -> Just $ rename g t
    _ -> Nothing

-- prepare a general, indexed typechecking error
cerr :: Int -> TypeError -> MorlocMonad a
cerr i e = MM.throwError $ IndexedError i (ConcreteTypeError e)


-- AnnoOne=>
synthG g (SAnno (One (x, Idx i (l, [EType ct _ _]))) m)
  = checkG g (SAnno (One (x, Idx i (l, []))) m) ct

-- AnnoMany=>
synthG g0 (SAnno (One (x, Idx i (l, cts@(_:_)))) m) =
  let (g1, t) = newvarRich [] [t' | (EType t' _ _) <- cts] "x" (Just l) g0
  in checkG g1 (SAnno (One (x, Idx i (l, []))) m) t

-- if there are no annotations, the SAnno can be simplified and synth' can be called
synthG g0 (SAnno (One (x, Idx i (l, []))) m@(Idx _ tg)) = do
  (g1, t, x') <- synthE' i l g0 x
  -- g2 <- subtype' i t (type2typeu tg) g1
  -- return (g2, t, SAnno (One (x', Idx i t)) m)
  return (g1, t, SAnno (One (x', Idx i t)) m)


checkG
  :: Gamma
  -> SAnno (Indexed Type) One (Indexed (Lang, [EType]))
  -> TypeU
  -> MorlocMonad
       ( Gamma
       , TypeU
       , SAnno (Indexed Type) One (Indexed TypeU)
       )
checkG g0 (SAnno (One (e0, Idx i (l, (et:ets)))) m) t0 = do
  g1 <- subtype' i (etype et) t0 g0
  checkG g1 (SAnno (One (e0, Idx i (l, ets))) m) (apply g1 t0)

checkG g0 (SAnno (One (e0, Idx i (l, []))) m@(Idx _ tg)) t0 = do
  (g1, t1, e1) <- checkE i l g0 e0 t0
  -- FIXME: I do need to somewhere ensure that concrete and general agree
  -- g2 <- subtype' i t1 (type2typeu tg) g1
  -- return $ (g2, t1, SAnno (One (e1, Idx i t1)) m)
  return (g1, t1, SAnno (One (e1, Idx i t1)) m)


synthE
  :: Int
  -> Lang
  -> Gamma
  -> SExpr (Indexed Type) One (Indexed (Lang, [EType]))
  -> MorlocMonad
       ( Gamma
       , TypeU
       , SExpr (Indexed Type) One (Indexed TypeU)
       )
-- Uni=>
synthE _ lang g UniS = do
  let ts = MLD.defaultNull (Just lang)
      (g', t) = newvarRich [] ts "uni_" (Just lang) g
  return (g' +> t, t, UniS)

-- Num=>
synthE _ lang g (NumS x) = do
  let ts = MLD.defaultNumber (Just lang)
      (g', t) = newvarRich [] ts "num_" (Just lang) g
  return (g' +> t, t, NumS x)

-- Str=>
synthE _ lang g (StrS x) = do
  let ts = MLD.defaultString (Just lang)
      (g', t) = newvarRich [] ts "str_" (Just lang) g
  return (g' +> t, t, StrS x)

-- Log=>
synthE _ lang g (LogS x) = do
  let ts = MLD.defaultBool (Just lang)
      (g', t) = newvarRich [] ts "log_" (Just lang) g
  return (g' +> t, t, LogS x)

synthE i lang g (AccS e k) = do
  (g1, t1, e1) <- synthG' g e
  valType <- case t1 of
    (NamU _ _ _ rs) -> case lookup k rs of
      Nothing -> cerr i (KeyError k t1)
      (Just t) -> return t
    _ -> cerr i (KeyError k t1)
  return (g1, valType, AccS e1 k)

--   -->E0
synthE _ lang g (AppS f []) = do
  (g1, t1, f1) <- synthG' g f
  return (g1, t1, AppS f1 [])

--   -->E
synthE i lang g0 (AppS f xs0) = do
  -- synthesize the type of the function
  (g1, funType0, funExpr0) <- synthG g0 f

  -- extend the function type with the type of the expressions it is applied to
  (g2, funType1, inputExprs) <- application' i lang g1 xs0 funType0

  -- determine the type after application
  appliedType <- case funType1 of
    (FunU ts t) -> case drop (length inputExprs) ts of
      [] -> return t -- full application
      rs -> return $ FunU rs t -- partial application
    _ -> error "impossible"

  -- put the AppS back together with the synthesized function and input expressions
  return (g2, apply g2 appliedType, AppS (applyCon g2 funExpr0) inputExprs)

--   -->I==>
synthE i lang g0 f@(LamS vs x) = do
  -- create existentials for everything and pass it off to check
  let (g1, ts) = statefulMap (\g' v -> newvar (unEVar v <> "_x") (Just lang) g') g0 vs
      (g2, ft) = newvar "o_" (Just lang) g1
      finalType = FunU ts ft
  checkE' i lang g2 f finalType

--   List
synthE _ lang g (LstS []) =
  let (g1, itemType) = newvar "itemType_" (Just lang) g
      listType = head $ MLD.defaultList (Just lang) itemType
  in return (g1, listType, LstS [])
synthE i lang g (LstS (e:es)) = do
  (g1, itemType, itemExpr) <- synthG' g e
  (g2, listType, listExpr) <- checkE' i lang g1 (LstS es) (head $ MLD.defaultList (Just lang) itemType)
  case listExpr of
    (LstS es') -> return (g2, listType, LstS (itemExpr:es'))
    _ -> error "impossible"

--   Tuple
synthE _ lang g (TupS []) =
  let t = head $ MLD.defaultTuple (Just lang) []
  in return (g, t, TupS [])
synthE i lang g (TupS (e:es)) = do
  -- synthesize head
  (g1, itemType, itemExpr) <- synthG' g e

  -- synthesize tail
  (g2, tupleType, tupleExpr) <- synthE' i lang g1 (TupS es)

  -- merge the head and tail
  t3 <- case tupleType of
    (AppU _ ts) -> return . head $ MLD.defaultTuple (Just lang) (apply g2 itemType : ts)
    _ -> error "impossible" -- the general tuple will always be (AppU _ _)

  xs' <- case tupleExpr of
    (TupS xs') -> return xs'
    _ -> error "impossible" -- synth does not change data constructors

  return (g2, t3, TupS (itemExpr:xs'))

--   Records
synthE _ lang g (NamS []) = return (g, head $ MLD.defaultRecord (Just lang) [], NamS [])
synthE i lang g0 (NamS ((k,x):rs)) = do
  -- type the head
  (g1, headType, headExpr) <- synthG' g0 x

  -- type the tail
  (g2, tailType, tailExpr) <- synthE' i lang g1 (NamS rs)

  -- merge the head with tail
  t <- case tailType of
    (NamU o1 n1 ps1 rs1) -> return $ NamU o1 n1 ps1 ((k, apply g2 headType):rs1)
    _ -> error "impossible" -- the synthE on NamS will always return NamU type

  tailExprs <- case tailExpr of
    (NamS xs') -> return xs'
    _ -> error "impossible" -- synth does not change data constructors

  return (g2, t, NamS ((k, headExpr):tailExprs))

-- Sources are axiomatic. They are they type they are said to be.
synthE i lang g (CallS src) = do
  let (g', t) = newvar "src_" (Just lang) g
  return (g', t, CallS src)

-- Any morloc variables should have been expanded by treeify. Any bound
-- variables should be checked against. I think (this needs formalization).
synthE i lang g (VarS v) = do
  -- is this a bound variable that has already been solved
  (g', t') <- case lookupE v g of
    -- yes, return the solved type
    (Just t) -> return (g, t)
    Nothing -> return $ newvar (unEVar v <> "_u") (Just lang) g
  return (g', t', VarS v)


application
  :: Int
  -> Lang
  -> Gamma
  -> [SAnno (Indexed Type) One (Indexed (Lang, [EType]))]
  -> TypeU -- the function type
  -> MorlocMonad
      ( Gamma
      , TypeU -- output function type
      , [SAnno (Indexed Type) One (Indexed TypeU)] -- @e@, with type annotation
      )

--  g1 |- e <= A -| g2
-- ----------------------------------------- -->App
--  g1 |- A->C o e =>> C -| g2
application i lang g0 es0 (FunU as0 b0) = do
  (g1, as1, es1, remainder) <- zipCheck i g0 es0 as0
  let es2 = map (applyCon g1) es1
      funType = apply g1 $ FunU (as1 <> remainder) b0
  say $ "remainder:" <+> vsep (map pretty remainder)
  return (g1, funType, es2)

--  g1,Ea |- [Ea/a]A o e =>> C -| g2
-- ----------------------------------------- Forall App
--  g1 |- Forall x.A o e =>> C -| g2
application i lang g0 es (ForallU v s) = application' i lang (g0 +> v) es (substitute v s)

--  g1[Ea2, Ea1, Ea=Ea1->Ea2] |- e <= Ea1 -| g2
-- ----------------------------------------- EaApp
--  g1[Ea] |- Ea o e =>> Ea2 -| g2
application i lang g0 es (ExistU v@(TV _ s) [] _) =
  case access1 v (gammaContext g0) of
    -- replace <t0> with <t0>:<ea1> -> <ea2>
    Just (rs, _, ls) -> do
      let (g1, veas) = statefulMap (\g _ -> tvarname g "a_" (Just lang)) g0 es
          (g2, vea) = tvarname g1 (s <> "o_") (Just lang)
          eas = [ExistU v [] [] | v <- veas]
          ea = ExistU vea [] []
          f = FunU eas ea
          g3 = g2 {gammaContext = rs <> [SolvedG v f] <> map index eas <> [index ea] <> ls}
      (g4, _, es', _) <- zipCheck i g3 es eas
      return (g4, apply g4 f, map (applyCon g4) es')
    -- if the variable has already been solved, use solved value
    Nothing -> case lookupU v g0 of
      (Just (FunU ts t)) -> do
        (g1, ts', es', _) <- zipCheck i g0 es ts
        return (g1, apply g1 (FunU ts' t), es')
      _ -> cerr i ApplicationOfNonFunction

application i lang _ e t = cerr i ApplicationOfNonFunction
 

-- Tip together the arguments passed to an application
zipCheck
  :: Int
  -> Gamma
  -> [SAnno (Indexed Type) One (Indexed (Lang, [EType]))]
  -> [TypeU]
  -> MorlocMonad
    ( Gamma
    , [TypeU]
    , [SAnno (Indexed Type) One (Indexed TypeU)]
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
zipCheck i _ es [] = cerr i TooManyArguments


checkE
  :: Int
  -> Lang
  -> Gamma
  -> SExpr (Indexed Type) One (Indexed (Lang, [EType]))
  -> TypeU
  -> MorlocMonad
        ( Gamma
        , TypeU
        , SExpr (Indexed Type) One (Indexed TypeU)
        )
checkE i lang g1 (LstS (e:es)) (AppU v [t]) = do
  (g2, t2, e') <- checkG' g1 e t
  -- LstS [] will go to the normal Sub case
  (g3, t3, LstS es') <- checkE i lang g2 (LstS es) (AppU v [t2])
  return (g3, t3, (LstS (map (applyCon g3) (e':es'))))

checkE _ lang g1 (LamS [] e1) (FunU [] b1) = do
  (g2, b2, e2) <- checkG' g1 e1 b1
  return (g2, FunU [] b2, LamS [] e2)

checkE _ lang g1 (LamS [] e1) t = do
  (g2, t, e2) <- checkG' g1 e1 t
  return (g2, t, LamS [] e2)

checkE i lang g1 (LamS (v:vs) e1) (FunU (a1:as1) b1) = do
  -- defined x:A
  let vardef = AnnG v a1
      g2 = g1 +> vardef

  -- peal off one layer of bound terms and check
  (g3, t3, e2) <- checkE' i lang g2 (LamS vs e1) (FunU as1 b1)

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

checkE i lang g1 e1 (ForallU v a) = checkE' i lang (g1 +> v) e1 (substitute v a)

-- Called functions are axiomatic
checkE i lang g1 (CallS src) t = return (g1, t, CallS src)

--   Sub
checkE i lang g1 e1 b = do
  (g2, a, e2) <- synthE' i lang g1 e1
  let a' = apply g2 a
      b' = apply g2 b
  g3 <- subtype' i a' b' g2
  return (g3, apply g3 b', e2)

subtype' :: Int -> TypeU -> TypeU -> Gamma -> MorlocMonad Gamma
subtype' i a b g = do
  say $ parens (pretty a) <+> "<:" <+> parens (pretty b)
  case subtype a b g of
    (Left err') -> cerr i err'
    (Right x) -> return x

cut' :: Int -> GammaIndex -> Gamma -> MorlocMonad Gamma
cut' i idx g = case cut idx g of
  (Left terr) -> cerr i terr
  (Right x) -> return x

-- apply context to a SAnno
applyCon :: (Functor cf, Functor f, Applicable c)
         => Gamma -> SAnno g f (cf c) -> SAnno g f (cf c)
applyCon g = mapSAnno id (fmap (apply g))

---- debugging

enter :: Doc ann -> MorlocMonad ()
enter d = do
  depth <- MM.incDepth
  debugLog $ pretty (take depth (repeat '-')) <> ">" <+> d <> "\n"

say :: Doc ann -> MorlocMonad ()
say d = do
  depth <- MM.getDepth
  debugLog $ pretty (take depth (repeat ' ')) <> ":" <+> d <> "\n"

seeGamma :: Gamma -> MorlocMonad ()
seeGamma g = say $ nest 4 $ "Gamma:" <> line <> (vsep (map pretty (gammaContext g)))

seeType :: TypeU -> MorlocMonad ()
seeType t = say $ pretty t

leave :: Doc ann -> MorlocMonad ()
leave d = do
  depth <- MM.decDepth
  debugLog $ "<" <> pretty (take (depth+1) (repeat '-')) <+> d <> "\n"

debugLog :: Doc ann -> MorlocMonad ()
debugLog d = do
  verbosity <- MM.gets stateVerbosity
  if verbosity > 0
    then (liftIO . putDoc) d
    else return ()

peak :: (Pretty c) => SExpr (Indexed Type) One c -> MorlocMonad ()
peak = say . prettySExpr pretty showGen

peakGen :: (Pretty c) => SAnno (Indexed Type) One c -> MorlocMonad ()
peakGen = say . prettySAnno pretty showGen

showGen :: (Indexed Type) -> Doc ann
showGen (Idx _ t) = parens (pretty t)

zipCheck' i g es ts = do
  enter "zipCheck"
  r@(g, ts, es, rs) <- zipCheck i g es ts
  leave "zipCheck"
  return r

synthG' g x = do
  enter "synthG"
  r <- synthG g x
  leave "synthG"
  return r

checkG' g x t = do
  enter "checkG"
  seeType t
  r <- checkG g x t
  leave "checkG"
  return r

synthE' i l g x = do
  enter "synthE"
  peak x
  seeGamma g
  r@(g', t, _) <- synthE i l g x
  leave "synthE"
  seeGamma g'
  seeType t
  return r

checkE' i l g x t = do
  enter "checkE"
  peak x
  seeType t
  seeGamma g
  r@(g', t', _) <- checkE i l g x t
  leave "checkE"
  seeType t'
  seeGamma g'
  return r

application' i l g es t = do
  enter "application"
  seeGamma g
  seeType t
  mapM_ peakGen es
  r@(g',t',es') <- application i l g es t
  leave "application"
  seeGamma g'
  seeType t'
  mapM_ peakGen es'
  return r
