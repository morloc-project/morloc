{-|
Module      : Morloc.CodeGenerator.Generate
Description : Translate AST forests into target language source code
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

The single @generate@ function wraps the entire AST forest to source code
translation process.

The input the @generate@ is of type @[SAnno (Indexed Type) Many [Type]]@. The @SAnno
(Indexed Type) Many [Type]@ elements each represent a single command exported from the
main function. The @(Indexed Type)@ type stores all general information about a given
"manifold" (a node in the function graph and all its wrappings). The term
@Many@ states that there may be one of more AST describing each expression. The
term @[Type]@ states that there may be multiple concrete, language-specific
types associated with any term.

The @generate@ function converts the @SAnno (Indexed Type) Many [Type]@ types into
@SAnno (Indexed Type) One Type@ unambiguous ASTs. This step is an important
optimization step in the morloc build pipeline. Currently the compiler uses a
flat scoring matrix for the cost of interop between languages (e.g., 0 for C++
to C++, 1000 for anything to R, 5 for R to R since there is a function call
cost, etc). Replacing this algorithm with an empirically parameterized
performance model is a major goal.

Additional manipulations of the AST can reduce the number of required foreign
calls, (de)serialization calls, and duplicate computation.

The @SAnno (Indexed Type) One Type@ expression is ultimately translated into a simple
@ExprM@ type that is then passed to a language-specific translator.

-}

module Morloc.CodeGenerator.Generate
(
  generate
) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Internal
import Morloc.CodeGenerator.Typecheck (typecheck)
import Morloc.Data.Doc
import Morloc.Pretty (prettyType)
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as Lang
import qualified Morloc.Monad as MM
import Morloc.CodeGenerator.Grammars.Common
import qualified Morloc.CodeGenerator.Nexus as Nexus
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Morloc.CodeGenerator.Grammars.Translator.Cpp as Cpp
import qualified Morloc.CodeGenerator.Grammars.Translator.Rust as Rust
import qualified Morloc.CodeGenerator.Grammars.Translator.R as R
import qualified Morloc.CodeGenerator.Grammars.Translator.Python3 as Python3

-- | Translate typed, abstract syntax forests into compilable code
generate ::
  [SAnno (Indexed Type) Many Int]
  -- ^ one AST forest for each command exported from main
  -> MorlocMonad (Script, [Script]) 
  -- ^ the nexus code and the source code for each language pool
generate es = do
  -- translate modules into bitrees
  (gASTs, rASTs)
    -- select a single instance at each node in the tree
    <- mapM realize es  -- [Either (SAnno GMeta One CType) (SAnno GMeta One CType)]
    -- separate unrealized (general) ASTs (uASTs) from realized ASTs (rASTs)
    |>> partitionEithers

  -- Collect all call-free data
  gSerial <- mapM generalSerial gASTs

  rASTs' <- mapM typecheck rASTs

  -- build nexus
  -- -----------
  -- Each nexus subcommand calls one function from one one pool.
  -- The call passes the pool an index for the function (manifold) that will be called.
  nexus <- Nexus.generate
    gSerial
    [(t, i) | (SAnno (One (_, Idx _ t)) i) <- rASTs']

  -- for each language, collect all functions into one "pool"
  pools
    -- thread arguments across the tree
    <- mapM parameterize rASTs'
    -- convert from AST to manifold tree
    >>= mapM express

    -- -- rewrite lets to minimize the number of foreign calls
    -- -- FIXME: let optimization is not possible until an effect system is implemented since it only is correct for pure functions
    -- >>= mapM letOptimize

    -- Separate the call trees into mono-lingual segments terminated in
    -- primitives or foreign calls.
    >>= mapM segment |>> concat
    -- Cast each call tree root as a manifold
    >>= mapM rehead
    -- Gather segments into pools, currently this entails gathering all
    -- segments from a given language into one pool. Later it may be more
    -- nuanced.
    >>= pool
    -- Generate the code for each pool
    >>= mapM findSources
    -- find sources dependencies for each concrete AST
    >>= mapM (uncurry encode)

  return (nexus, pools)


-- | Choose a single concrete implementation. This function is algorithmically
-- the most complex component of the morloc compiler. In the future, it will
-- probably need to be implemented using an optimizing SMT solver.
realize
  :: SAnno (Indexed Type) Many Int
  -> MorlocMonad (Either (SAnno (Indexed Type) One ())
                         (SAnno (Indexed Type) One (Indexed Lang)))
realize s0 = do
  e@(SAnno (One (_, li)) _) <- scoreSAnno [] s0 >>= collapseSAnno Nothing
  case li of
    (Idx _ Nothing) -> makeGAST e |>> Left 
    (Idx _ _) -> Right <$> mapCM unmaybeIdx e
  where
  scoreSAnno
    :: [Lang]
    -> SAnno (Indexed Type) Many Int
    -> MorlocMonad (SAnno (Indexed Type) Many (Indexed [(Lang, Int)]))
  scoreSAnno langs (SAnno (Many xs) t) = do
    xs' <- mapM (scoreExpr langs) xs
    return (SAnno (Many xs') t)

  scoreExpr
    :: [Lang]
    -> (SExpr (Indexed Type) Many Int, Int)
    -> MorlocMonad (SExpr (Indexed Type) Many (Indexed [(Lang, Int)]), Indexed [(Lang, Int)])
  scoreExpr langs (AccS x k, i) = do
    x' <- scoreSAnno langs x
    return (AccS x' k, Idx i (scoresOf x'))
  scoreExpr langs (ListS xs, i) = do
    (xs', best) <- scoreMany langs xs
    return (ListS xs', Idx i best)
  scoreExpr langs (TupleS xs, i) = do
    (xs', best) <- scoreMany langs xs
    return (TupleS xs', Idx i best)
  scoreExpr langs (LamS vs x, i) = do
    x' <- scoreSAnno langs x
    return (LamS vs x', Idx i (scoresOf x'))
  scoreExpr _ (AppS f xs, i) = do
    f' <- scoreSAnno [] f
    let scores = scoresOf f'
    xs' <- mapM (scoreSAnno (unique $ map fst scores)) xs
    let pairss = [(maxPairs . concat) [xs''' | (_, Idx _ xs''') <- xs''] | SAnno (Many xs'') _ <- xs']
        best = [ (l1, sum [ maximum [s1 + s2 + Lang.pairwiseCost l1 l2
                          | (l2, s2) <- pairs] | pairs <- pairss])
               | (l1, s1) <- scores]
    return (AppS f' xs', Idx i best)
  scoreExpr langs (RecS rs, i) = do
    (xs, best) <- scoreMany langs (map snd rs)
    return $ (RecS (zip (map fst rs) xs), Idx i best)
  scoreExpr _ (CallS s, i) = return (CallS s, Idx i [(srcLang s, 0)])
  -- non-recursive expressions
  scoreExpr langs (UniS, i) = return (UniS, zipLang i langs)
  scoreExpr langs (VarS v, i) = return (VarS v, zipLang i langs)
  scoreExpr langs (NumS x, i) = return (NumS x, zipLang i langs)
  scoreExpr langs (LogS x, i) = return (LogS x, zipLang i langs)
  scoreExpr langs (StrS x, i) = return (StrS x, zipLang i langs)
  scoreExpr langs (FixS, i) = return (FixS, zipLang i langs)

  zipLang :: Int -> [Lang] -> Indexed [(Lang, Int)]
  zipLang i langs = Idx i (zip langs (repeat 0))

  scoresOf :: SAnno a Many (Indexed [(Lang, Int)]) -> [(Lang, Int)]
  scoresOf (SAnno (Many xs) _) = maxPairs . concat $ [xs' | (_, Idx _ xs') <- xs]

  scoreMany
    :: [Lang]
    -> [SAnno (Indexed Type) Many Int]
    -> MorlocMonad ([SAnno (Indexed Type) Many (Indexed [(Lang, Int)])], [(Lang, Int)])
  scoreMany langs xs0 = do
    xs1 <- mapM (scoreSAnno langs) xs0
    return (xs1, scoreMany xs1)
    where
      scoreMany :: [SAnno (Indexed Type) Many (Indexed [(Lang, Int)])] -> [(Lang, Int)]
      scoreMany xs =
        let pairss = [(maxPairs . concat) [xs'' | (_, Idx _ xs'') <- xs'] | SAnno (Many xs') _ <- xs]
            langs = unique (langs <> (concat . map (map fst)) pairss)
        in [(l1, sum [maximum [score + Lang.pairwiseCost l1 l2 | (l2, score) <- pairs] | pairs <- pairss]) | l1 <- langs]


  collapseSAnno
    :: Maybe Lang
    -> SAnno (Indexed Type) Many (Indexed [(Lang, Int)])
    -> MorlocMonad (SAnno (Indexed Type) One (Indexed (Maybe Lang)))
  collapseSAnno l1 (SAnno (Many es) t) = do
    e <- case maxBy (\(_, Idx _ ss) -> maximumMay [cost l1 l2 s | (l2, s) <- ss]) es of
      Nothing -> MM.throwError . CallTheMonkeys $ "A SAnno must contain an SExpr"
      (Just x@(e, Idx _ ss)) -> collapseExpr (fmap fst (maxBy snd ss)) x
    return (SAnno (One e) t)


  cost
    :: Maybe Lang -- parent language (if given)
    -> Lang -- child lang (should always be given if we are working from scored pairs)
    -> Int -- score
    -> Int
  cost (Just l1) l2 score = score + Lang.pairwiseCost l1 l2
  cost _ _ score = score


  collapseExpr
    :: Maybe Lang -- the language of the parent expression (if Nothing, then this is a GAST)
    -> (SExpr (Indexed Type) Many (Indexed [(Lang, Int)]), (Indexed [(Lang, Int)]))
    -> MorlocMonad (SExpr (Indexed Type) One (Indexed (Maybe Lang)), Indexed (Maybe Lang))
  collapseExpr l1 (AccS x k, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    x' <- collapseSAnno lang x
    return (AccS x' k, Idx i lang)
  collapseExpr _ (CallS src, Idx i _) = do
   return (CallS src, Idx i (Just $ srcLang src))
  collapseExpr l1 (ListS xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseSAnno lang) xs
    return (ListS xs', Idx i lang)
  collapseExpr l1 (TupleS xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseSAnno lang) xs
    return (TupleS xs', Idx i lang)
  collapseExpr l1 (LamS vs x, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    x' <- collapseSAnno lang x
    return (LamS vs x', Idx i lang)
  collapseExpr l1 (AppS f xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    f' <- collapseSAnno lang f
    xs' <- mapM (collapseSAnno lang) xs
    return (AppS f' xs', Idx i lang)
  collapseExpr l1 (RecS rs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseSAnno lang . snd) rs
    return (RecS (zip (map fst rs) xs'), Idx i lang)
  -- collapse leaf expressions
  collapseExpr lang (UniS,   Idx i _) = return (UniS,   Idx i lang)
  collapseExpr lang (VarS v, Idx i _) = return (VarS v, Idx i lang)
  collapseExpr lang (NumS x, Idx i _) = return (NumS x, Idx i lang)
  collapseExpr lang (LogS x, Idx i _) = return (LogS x, Idx i lang)
  collapseExpr lang (StrS x, Idx i _) = return (StrS x, Idx i lang)
  collapseExpr lang (FixS,   Idx i _) = return (FixS,   Idx i lang)

  chooseLanguage :: (Maybe Lang) -> [(Lang, Int)] -> MorlocMonad (Maybe Lang)
  chooseLanguage l1 ss =
    case maxBy snd [(l2, cost l1 l2 s2) | (l2, s2) <- ss] of
      Nothing -> MM.throwError . CallTheMonkeys $ "This shouldn't happen"
      (Just (l3, _)) -> return (Just l3)


  maxBy :: Ord b => (a -> b) -> [a] -> Maybe a
  maxBy _ [] = Nothing
  maxBy _ [x] = Just x
  maxBy f (x1:rs) = case maxBy f rs of
    Nothing -> Just x1
    (Just x2) -> if f x1 >= f x2 then Just x1 else Just x2

  -- find the highest scoring value for each key
  maxPairs :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
  maxPairs xs = map (\(k, vs) -> (k, maximum vs)) $ groupSort xs

  unmaybeIdx :: (Indexed (Maybe a)) -> MorlocMonad (Indexed a)
  unmaybeIdx (Idx _ Nothing) = MM.throwError . CallTheMonkeys $ "Expected no Nothings"
  unmaybeIdx (Idx i (Just x)) = return (Idx i x)


-- | This function is called on trees that contain no language-specific
-- components.  "GAST" refers to General Abstract Syntax Tree. The most common
-- GAST case, and the only one that is currently supported, is a expression
-- that merely rearranges data structures without calling any functions. Here
-- are a few examples:
--
--  Constant values and containters (currently supported):
--  f1 = 5
--  f2 = [1,2,3]
--
--  Variable values and containers (coming soon):
--  f3 x = x
--
--  f4 x = [1,2,x]
--
--  Combinations of transformations on containers (possible, but not coming soon):
--  f5 :: forall a b . (a, b) -> (b, a)
--  f6 (x,y) = (y,x)
--
-- The idea could be elaborated into a full-fledged language.
makeGAST :: SAnno (Indexed Type) One (Indexed (Maybe Lang)) -> MorlocMonad (SAnno (Indexed Type) One ())
makeGAST = mapCM (\(Idx _ _) -> return ())


generalSerial :: SAnno (Indexed Type) One () -> MorlocMonad NexusCommand
generalSerial x0@(SAnno _ (Idx i t)) = do
  mayName <- metaName i
  name <- case mayName of
    Nothing -> MM.throwError . OtherError $ "No general type found for call-free function"
    (Just name) -> return name
  let base = NexusCommand {
      commandName = name -- EVar -- user-exposed subcommand name in the nexus
    , commandType = t -- Type -- the general type of the expression
    , commandJson = (dquotes "_") -- MDoc -- JSON output with null's where values will be replaced
    , commandArgs = [] -- [EVar] -- list of function arguments
    , commandSubs = [] -- [(JsonPath, Text, JsonPath)]
    -- list of tuples with values 1) path in JSON to value needs to be replaced
    -- 2) the function argument from which to pull replacement value and 3) the
    -- path to the replacement value
    }
  generalSerial' base [] x0
  where
    generalSerial' :: NexusCommand -> JsonPath -> SAnno (Indexed Type) One () -> MorlocMonad NexusCommand
    generalSerial' base _ (SAnno (One (UniS,   _)) _)
      = return $ base { commandJson = "null" }
    generalSerial' base _ (SAnno (One (NumS x, _)) _)
      = return $ base { commandJson = viaShow x }
    generalSerial' base _ (SAnno (One (LogS x, _)) _)
      = return $ base { commandJson = if x then "true" else "false" }
    generalSerial' base _ (SAnno (One (StrS x, _)) _)
      = return $ base { commandJson = dquotes (pretty x) }
    -- if a nested accessor is observed, evaluate the nested expression and
    -- append the path 
    generalSerial' base ps (SAnno (One (AccS x@(SAnno (One (AccS _ _, _)) _) k, _)) _) = do
      ncmd <- generalSerial' base ps x
      case commandSubs ncmd of
        [(ps1, arg, ps2)] ->
          return $ ncmd { commandSubs = [(ps1, arg, JsonKey k : ps2)] }
        _ -> error "Bad record access"
    -- record the path to and from a record access, leave the value as null, it
    -- will be set in the nexus
    generalSerial' base ps (SAnno (One (AccS (SAnno (One (VarS v, _)) (Idx _ g)) k, _)) _) =
      case g of
        (NamT {}) ->
          return $ base { commandSubs = [(ps, unEVar v, [JsonKey k])] }
        _ -> error "Attempted to use key access to non-record"
    generalSerial' base ps (SAnno (One (ListS xs, _)) _) = do
      ncmds <- zipWithM (generalSerial' base)
                        [ps ++ [JsonIndex i] | i <- [0..]] xs
      return $ base 
        { commandJson = list (map commandJson ncmds)
        , commandSubs = conmap commandSubs ncmds
        }
    generalSerial' base ps (SAnno (One (TupleS xs, _)) _) = do
      ncmds <- zipWithM (generalSerial' base)
                        [ps ++ [JsonIndex i] | i <- [0..]] xs
      return $ base
        { commandJson = list (map commandJson ncmds)
        , commandSubs = conmap commandSubs ncmds
        }
    generalSerial' base ps (SAnno (One (RecS es, _)) _) = do
      ncmds <- zipWithM (generalSerial' base)
                        [ps ++ [JsonKey k] | k <- map fst es]
                        (map snd es)
      let entries = zip (map fst es) (map commandJson ncmds)
          obj = encloseSep "{" "}" ","
                (map (\(k, v) -> dquotes (pretty k) <> ":" <> v) entries)
      return $ base
        { commandJson = obj
        , commandSubs = conmap commandSubs ncmds
        }
    generalSerial' base ps (SAnno (One (LamS vs x, _)) _) = do
      ncmd <- generalSerial' base ps x
      return $ ncmd { commandArgs = vs }
    generalSerial' base ps (SAnno (One (VarS (EV v), _)) _) =
      return $ base { commandSubs = [(ps, v, [])] }
    generalSerial' _ _ (SAnno (One _) (Idx _ t)) = do
      MM.throwError . OtherError . render $
        "Cannot serialize general type:" <+> prettyType t


-- | Add arguments that are required for each term. Unneeded arguments are
-- removed at each step.
parameterize
  :: SAnno Int One (Indexed TypeP)
  -> MorlocMonad (SAnno Int One (Indexed TypeP, [(EVar, Argument)]))
parameterize (SAnno (One (LamS vs x, c@(Idx _ t))) m) = do
  let args0 = zip vs $ zipWith makeArgument [0..] (decomposeFull t)
  x' <- parameterize' args0 x
  return $ SAnno (One (LamS vs x', (c, args0))) m
parameterize (SAnno (One (CallS src, c@(Idx _ t))) m) = do
  let ts = init . decomposeFull $ t
      vs = map EV (freshVarsAZ [])
      args0 = zipWith makeArgument [0..] ts
  return $ SAnno (One (CallS src, (c, zip vs args0))) m
parameterize x = parameterize' [] x

-- TODO: the arguments coupled to every term should be the arguments USED
-- (not inherited) by the term. I need to ensure the argument threading
-- leads to correct passing of serialized/unserialized arguments. AppS should
-- "know" that it needs to deserialize functions that are passed to a foreign
-- call, for instance.
parameterize'
  :: [(EVar, Argument)] -- arguments in parental scope (child needn't retain them)
  -> SAnno Int One (Indexed TypeP)
  -> MorlocMonad (SAnno Int One (Indexed TypeP, [(EVar, Argument)]))
-- primitives, no arguments are required for a primitive, so empty lists
parameterize' _ (SAnno (One (UniS, c)) m) = return $ SAnno (One (UniS, (c, []))) m
parameterize' _ (SAnno (One (NumS x, c)) m) = return $ SAnno (One (NumS x, (c, []))) m
parameterize' _ (SAnno (One (LogS x, c)) m) = return $ SAnno (One (LogS x, (c, []))) m
parameterize' _ (SAnno (One (StrS x, c)) m) = return $ SAnno (One (StrS x, (c, []))) m
parameterize' _ (SAnno (One (FixS, c)) m) = return $ SAnno (One (FixS, (c, []))) m
-- VarS EVar
parameterize' args (SAnno (One (VarS v, c)) m) = do
  let args' = [(v', r) | (v', r) <- args, v' == v]
  return $ SAnno (One (VarS v, (c, args'))) m
-- CallS Source
parameterize' _ (SAnno (One (CallS src, c)) m) = do
  return $ SAnno (One (CallS src, (c, []))) m
-- record access
parameterize' args (SAnno (One (AccS x k, c)) m) = do
  x' <- parameterize' args x
  return $ SAnno (One (AccS x' k, (c, args))) m
-- containers
parameterize' args (SAnno (One (ListS xs, c)) m) = do
  xs' <- mapM (parameterize' args) xs
  let usedArgs = map fst . unique . concatMap sannoSnd $ xs'
      args' = [(v, r) | (v, r) <- args, elem v usedArgs] 
  return $ SAnno (One (ListS xs', (c, args'))) m
parameterize' args (SAnno (One (TupleS xs, c)) m) = do
  xs' <- mapM (parameterize' args) xs
  let usedArgs = map fst . unique . concatMap sannoSnd $ xs'
      args' = [(v, r) | (v, r) <- args, elem v usedArgs] 
  return $ SAnno (One (TupleS xs', (c, args'))) m
parameterize' args (SAnno (One (RecS entries, c)) m) = do
  vs' <- mapM (parameterize' args . snd) entries
  let usedArgs = map fst . unique . concatMap sannoSnd $ vs'
      args' = [(v, r) | (v, r) <- args, elem v usedArgs] 
  return $ SAnno (One (RecS (zip (map fst entries) vs'), (c, args'))) m
parameterize' args (SAnno (One (LamS vs x, c@(Idx _ t))) m) = do
  let args' = [(v, r) | (v, r) <- args, notElem v vs]
      startId = maximum (map (argId . snd) args) + 1
      args0 = zip vs $ map unpackArgument $ zipWith makeArgument [startId..] (decomposeFull t)
  x' <- parameterize' (args' ++ args0) x
  return $ SAnno (One (LamS vs x', (c, args'))) m
parameterize' args (SAnno (One (AppS x xs, c)) m) = do
  x' <- parameterize' args x
  xs' <- mapM (parameterize' args) xs
  let usedArgs = map fst $ sannoSnd x' ++ (unique . concatMap sannoSnd $ xs')
      args' = [(v, r) | (v, r) <- args, elem v usedArgs] 
  return $ SAnno (One (AppS x' xs', (c, args'))) m


makeArgument :: Int -> TypeP -> Argument
makeArgument i (UnkP _) = PassThroughArgument i
makeArgument i t = SerialArgument i t


express :: SAnno Int One (Indexed TypeP, [(EVar, Argument)]) -> MorlocMonad (ExprM Many)
express s0@(SAnno (One (_, (Idx _ c0, _))) _) = express' True c0 s0 where
  express' :: Bool -> TypeP -> SAnno Int One (Indexed TypeP, [(EVar, Argument)]) -> MorlocMonad (ExprM Many)

  -- primitives
  express' _ _ (SAnno (One (NumS x, (Idx _ c, _))) _) = return $ NumM (Native c) x
  express' _ _ (SAnno (One (LogS x, (Idx _ c, _))) _) = return $ LogM (Native c) x
  express' _ _ (SAnno (One (StrS x, (Idx _ c, _))) _) = return $ StrM (Native c) x
  express' _ _ (SAnno (One (UniS, (Idx _ c, _))) _) = return $ NullM (Native c)

  -- record access
  express' isTop pc (SAnno (One (AccS x k, _)) m) = do
    x' <- express' isTop pc x >>= unpackExprM m
    return (AccM x' k)

  -- containers
  express' isTop _ (SAnno (One (ListS xs, (Idx _ c@(ArrP _ [t]), args))) m) = do
    xs' <- mapM (express' False t) xs >>= mapM (unpackExprM m)
    let x = ListM (Native c) xs'
    if isTop
      then do
        x' <- packExprM m x
        return $ ManifoldM m (map snd args) (ReturnM x')
      else return x
  express' _ _ (SAnno (One (ListS _, _)) _) = MM.throwError . CallTheMonkeys $ "ListS can only be ArrP type"

  express' isTop _ (SAnno (One (TupleS xs, (Idx _ c@(ArrP _ ts), args))) m) = do
    xs' <- zipWithM (express' False) ts xs >>= mapM (unpackExprM m)
    let x = TupleM (Native c) xs'
    if isTop
      then do
        x' <- packExprM m x
        return $ ManifoldM m (map snd args) (ReturnM x')
      else return x

  express' isTop _ (SAnno (One (RecS entries, (Idx _ c@(NamP _ _ _ rs), args))) m) = do
    xs' <- zipWithM (express' False) (map snd rs) (map snd entries) >>= mapM (unpackExprM m)
    let x = RecordM (Native c) (zip (map fst entries) xs')
    if isTop
      then do
        x' <- packExprM m x
        return $ ManifoldM m (map snd args) (ReturnM x')
      else return x

  -- lambda
  express' isTop _ (SAnno (One (LamS _ x@(SAnno (One (_, (Idx _ c,_))) _), _)) _) = express' isTop c x

  -- var
  express' _ _ (SAnno (One (VarS v, (Idx _ c, rs))) _) =
    case [r | (v', r) <- rs, v == v'] of
      [r] -> case r of
        (SerialArgument i _) -> return $ BndVarM (Serial c) i
        (NativeArgument i _) -> return $ BndVarM (Native c) i
        -- NOT passthrough, since it doesn't
        -- After segmentation, this type will be used to resolve passthroughs everywhere
        (PassThroughArgument i) -> return $ BndVarM (Serial c) i
      _ -> MM.throwError . OtherError $ "Expected VarS to match exactly one argument"

  -- Apply arguments to a sourced function
  -- The CallS object may be in a foreign language. These inter-language
  -- connections will be snapped apart in the segment step.
  express' _ pc (SAnno (One (AppS (SAnno (One (CallS src, (Idx _ fc, _))) _) xs, (_, args))) m)
    -- case #1
    | sameLanguage && fullyApplied = do
        xs' <- zipWithM (express' False) inputs xs >>= mapM (unpackExprM m)
        return . ManifoldM m (map snd args) $
          ReturnM (AppM f xs')

    -- case #2
    | sameLanguage && not fullyApplied = do
        xs' <- zipWithM (express' False) inputs xs >>= mapM (unpackExprM m)
        let startId = maximum (map (argId . snd) args) + 1
            lambdaTypes = drop (length xs) (map typeP2typeM inputs)
            lambdaArgs = zipWith NativeArgument [startId ..] inputs
            lambdaVals = zipWith BndVarM          lambdaTypes [startId ..]
        return . ManifoldM m (map snd args) $
          ReturnM (LamM lambdaArgs (AppM f (xs' ++ lambdaVals)))

    -- case #3
    | not sameLanguage && fullyApplied = do
          xs' <- zipWithM (express' False) inputs xs >>= mapM (unpackExprM m)
          return . ForeignInterfaceM (packTypeM (typeP2typeM pc)) . ManifoldM m (map snd args) $
            ReturnM (AppM f xs')

    -- case #4
    | not sameLanguage && not fullyApplied = do
        xs' <- zipWithM (express' False) inputs xs >>= mapM (unpackExprM m)
        let startId = maximum (map (argId . snd) args) + 1
            lambdaTypes = drop (length xs) (map typeP2typeM inputs)
            lambdaArgs = zipWith NativeArgument [startId ..] inputs
            lambdaVals = zipWith BndVarM lambdaTypes [startId ..]
        return . ForeignInterfaceM (packTypeM (typeP2typeM pc))
               . ManifoldM m (map snd args)
               $ ReturnM (LamM lambdaArgs (AppM f (xs' ++ lambdaVals)))
    where
      (inputs, _) = decompose fc
      sameLanguage = langOf pc == langOf fc
      fullyApplied = length inputs == length xs
      f = SrcM (typeP2typeM fc) src

  -- CallS - direct export of a sourced function, e.g.:
  express' True _ (SAnno (One (CallS src, (Idx _ c, _))) m) = do
    let (inputs, _) = decompose c
        lambdaArgs = zipWith SerialArgument [0 ..] inputs
        lambdaTypes = map (packTypeM . typeP2typeM) inputs
        f = SrcM (typeP2typeM c) src
    lambdaVals <- mapM (unpackExprM m) $ zipWith BndVarM lambdaTypes [0 ..]
    return $ ManifoldM m lambdaArgs (ReturnM $ AppM f lambdaVals)

  -- An un-applied source call
  express' False pc (SAnno (One (CallS src, (Idx _ c, _))) m) = do
    let (inputs, _) = decompose c
        lambdaTypes = map typeP2typeM inputs
        lambdaArgs = zipWith NativeArgument [0 ..] inputs
        lambdaVals = zipWith BndVarM lambdaTypes [0 ..]
        f = SrcM (typeP2typeM c) src
        manifold = ManifoldM m lambdaArgs (ReturnM $ AppM f lambdaVals)

    if langOf pc == langOf c
      then return manifold
      else return $ ForeignInterfaceM (typeP2typeM pc) manifold

  express' _ _ (SAnno (One (_, (Idx _ t, _))) m) = do
    name <- metaName m
    MM.throwError . CallTheMonkeys . render $
      "Invalid input to express' in module (" <> viaShow name <> ") - type: " <> prettyTypeP t


segment :: ExprM Many -> MorlocMonad [ExprM Many]
segment e0
  = segment' (gmetaOf e0) (argsOf e0) e0
  |>> (\(ms,e) -> e:ms)
  |>> map reparameterize where

  -- This is where segmentation happens, every other match is just traversal
  segment' _ args (ForeignInterfaceM t e@(ManifoldM m args' _)) = do
    (ms, e') <- segment' m args' e
    config <- MM.ask
    case MC.buildPoolCallBase config (langOf e') m of
      (Just cmds) -> return (e':ms, PoolCallM (packTypeM t) m cmds args)
      Nothing -> MM.throwError . OtherError $ "Unsupported language: " <> MT.show' (langOf e')

  segment' m args (SerializeM _ (AppM e@(ForeignInterfaceM _ _) es)) = do
    (ms, e') <- segment' m args e
    (mss, es') <- mapM (segment' m args) es |>> unzip
    es'' <- mapM (packExprM m) es'
    return (ms ++ concat mss, AppM e' es'')

  segment' _ _ (ManifoldM m args e) = do
    (ms, e') <- segment' m args e
    return (ms, ManifoldM m args e')

  segment' m args (AppM e es) = do
    (ms, e') <- segment' m args e
    (mss, es') <- mapM (segment' m args) es |>> unzip
    return (ms ++ concat mss, AppM e' es')

  segment' m args0 (LamM args1 e) = do
    (ms, e') <- segment' m (args0 ++ args1) e
    return (ms, LamM args1 e')

  segment' m args (LetM i e1 e2) = do
    (ms1, e1') <- segment' m args e1
    (ms2, e2') <- segment' m args e2
    return (ms1 ++ ms2, LetM i e1' e2')

  segment' m args (AccM e k) = do
    (ms, e') <- segment' m args e
    return (ms, AccM e' k)

  segment' m args (ListM t es) = do
    (mss, es') <- mapM (segment' m args) es |>> unzip
    return (concat mss, ListM t es')

  segment' m args (TupleM t es) = do
    (mss, es') <- mapM (segment' m args) es |>> unzip
    return (concat mss, TupleM t es')

  segment' m args (RecordM t entries) = do
    (mss, es') <- mapM (segment' m args . snd) entries |>> unzip
    return (concat mss, RecordM t (zip (map fst entries) es'))

  segment' m args (SerializeM s e) = do
    (ms, e') <- segment' m args e
    return (ms, SerializeM s e')

  segment' m args (DeserializeM s e) = do
    (ms, e') <- segment' m args e
    return (ms, DeserializeM s e')

  segment' m args (ReturnM e) = do
    (ms, e') <- segment' m args e
    return (ms, ReturnM e')

  segment' _ _ e = return ([], e)


-- Now that the AST is segmented by language, we can resolve passed-through
-- arguments where possible.
reparameterize :: (ExprM Many) -> (ExprM Many)
reparameterize e0 = snd (substituteBndArgs e0) where 
  substituteBndArgs :: (ExprM Many) -> ([(Int, TypeM)], ExprM Many) 
  substituteBndArgs (ForeignInterfaceM i e) =
    let (vs, e') = substituteBndArgs e
    in (vs, ForeignInterfaceM i (snd $ substituteBndArgs e'))
  substituteBndArgs (ManifoldM m args e) =
    let (vs, e') = substituteBndArgs e
    in (vs, ManifoldM m (map (sub vs) args) e')
  substituteBndArgs (AppM e es) =
    let (vs, e') = substituteBndArgs e
        (vss, es') = unzip $ map substituteBndArgs es
    in (vs ++ concat vss, AppM e' es')
  substituteBndArgs (LamM args e) =
    let (vs, e') = substituteBndArgs e
    in (vs, LamM (map (sub vs) args) e')
  substituteBndArgs (LetM i e1 e2) =
    let (vs1, e1') = substituteBndArgs e1
        (vs2, e2') = substituteBndArgs e2
    in (vs1 ++ vs2, LetM i e1' e2')
  substituteBndArgs (AccM e k) =
    let (vs, e') = substituteBndArgs e
    in (vs, AccM e' k)
  substituteBndArgs (ListM t es) =
    let (vss, es') = unzip $ map substituteBndArgs es
    in (concat vss, ListM t es')
  substituteBndArgs (TupleM t es) =
    let (vss, es') = unzip $ map substituteBndArgs es
    in (concat vss, TupleM t es')
  substituteBndArgs (RecordM t entries) =
    let (vss, es') = unzip $ map substituteBndArgs (map snd entries)
    in (concat vss, RecordM t (zip (map fst entries) es'))
  substituteBndArgs (SerializeM s e) =
    let (vs, e') = substituteBndArgs e
    in (vs, SerializeM s e')
  substituteBndArgs (DeserializeM s e) =
    let (vs, e') = substituteBndArgs e
    in (vs, DeserializeM s e')
  substituteBndArgs (ReturnM e) =
    let (vs, e') = substituteBndArgs e
    in (vs, ReturnM e')
  substituteBndArgs e@(BndVarM t i) = ([(i, t)], e)
  substituteBndArgs e = ([], e)

  sub :: [(Int, TypeM)] -> Argument -> Argument
  sub bnds r@(PassThroughArgument i) = case [t | (i', t) <- bnds, i == i'] of
    ((Serial t):_) -> SerialArgument i t 
    ((Native t):_) -> NativeArgument i t 
    ((Function _ _):_) -> error "You don't need to pass functions as manifold arguments"
    (Passthrough : _) -> error "What about 'Passthrough' do you not understand?"
    _ -> r 
  sub _ r = r


rehead :: ExprM Many -> MorlocMonad (ExprM Many)
rehead (LamM _ e) = rehead e
rehead (ManifoldM m args (ReturnM e)) = do
  e' <- packExprM m e
  return $ ManifoldM m args (ReturnM e')
rehead _ = MM.throwError $ CallTheMonkeys "Bad Head"


-- Sort manifolds into pools. Within pools, group manifolds into call sets.
pool :: [ExprM Many] -> MorlocMonad [(Lang, [ExprM Many])]
pool = return . groupSort . map (\e -> (fromJust $ langOf e, e))

findSources
  :: (Lang, [ExprM Many])
  -> MorlocMonad (Lang, [([Source], ExprM Many)])
findSources = undefined
-- -- | find all sources required, both in CallS statements and those used for serialization
-- getSrcs :: SExpr Int One c -> Int -> c -> MorlocMonad [Source]
-- getSrcs (CallS src) g _ = (:) src <$> getSrcsFromManifold g
-- getSrcs _ g _ = getSrcsFromManifold g
--
-- getSrcsFromManifold :: Int -> MorlocMonad [Source]
-- getSrcsFromManifold g = do
--   packers <- MM.gets statePackers |>> Map.elems |>> concat
--   constructors <- metaConstructors g
--   return . unique
--     $  concat [unresolvedPackerForward p <> unresolvedPackerReverse p | p <- packers]
--     <> constructors

encode
  :: Lang
  -> [([Source], ExprM Many)]
  -- ^ The input preserves the connection between the AST and the specific
  -- sources it uses, currently this information is not used. However, in the
  -- future it may sometimes be necessary to split the functions in one
  -- language into multiple pools (e.g., to resolve version conflicts).
  -> MorlocMonad Script
encode lang xss = do
  let srcs' = unique [s | s <- concat (map fst xss), srcLang s == lang]
  xs' <- mapM (preprocess lang) (map snd xss) >>= chooseSerializer
  -- translate each node in the AST to code
  translate lang srcs' xs'


translate :: Lang -> [Source] -> [ExprM One] -> MorlocMonad Script
translate lang srcs es = do
  case lang of
    CppLang -> Cpp.translate srcs es
    RustLang -> Rust.translate srcs es
    RLang -> R.translate srcs es
    Python3Lang -> Python3.translate srcs es
    x -> MM.throwError . PoolBuildError . render
      $ "Language '" <> viaShow x <> "' has no translator"


preprocess :: Lang -> ExprM Many -> MorlocMonad (ExprM Many)
preprocess CppLang es = Cpp.preprocess es
preprocess RustLang es = Rust.preprocess es
preprocess RLang es = R.preprocess es
preprocess Python3Lang es = Python3.preprocess es
preprocess l _ = MM.throwError . PoolBuildError . render
               $ "Language '" <> viaShow l <> "' has no translator"


chooseSerializer :: [ExprM Many] -> MorlocMonad [ExprM One]
chooseSerializer xs = mapM chooseSerializer' xs where
  chooseSerializer' :: ExprM Many -> MorlocMonad (ExprM One)
  -- This is where the magic happens, the rest is just plumbing
  chooseSerializer' (SerializeM s e) = SerializeM <$> oneSerial s <*> chooseSerializer' e
  chooseSerializer' (DeserializeM s e) = DeserializeM <$> oneSerial s <*> chooseSerializer' e
  -- plumbing
  chooseSerializer' (ManifoldM g args e) = ManifoldM g args <$> chooseSerializer' e
  chooseSerializer' (ForeignInterfaceM t e) = ForeignInterfaceM t <$> chooseSerializer' e
  chooseSerializer' (LetM i e1 e2) = LetM i <$> chooseSerializer' e1 <*> chooseSerializer' e2
  chooseSerializer' (AppM e es) = AppM <$> chooseSerializer' e <*> mapM chooseSerializer' es
  chooseSerializer' (LamM args e) = LamM args <$> chooseSerializer' e
  chooseSerializer' (AccM e k) = AccM <$> chooseSerializer' e <*> pure k
  chooseSerializer' (ListM t es) = ListM t <$> mapM chooseSerializer' es
  chooseSerializer' (TupleM t es) = TupleM t <$> mapM chooseSerializer' es
  chooseSerializer' (RecordM t rs) = do
    ts <- mapM (chooseSerializer' . snd) rs
    return $ RecordM t (zip (map fst rs) ts)
  chooseSerializer' (ReturnM e ) = ReturnM <$> chooseSerializer' e
  chooseSerializer' (SrcM t s) = return $ SrcM t s
  chooseSerializer' (PoolCallM t i d args) = return $ PoolCallM t i d args
  chooseSerializer' (BndVarM t i ) = return $ BndVarM t i
  chooseSerializer' (LetVarM t i) = return $ LetVarM t i
  chooseSerializer' (LogM t x) = return $ LogM t x
  chooseSerializer' (NumM t x) = return $ NumM t x
  chooseSerializer' (StrM t x) = return $ StrM t x
  chooseSerializer' (NullM t) = return $ NullM t

  oneSerial :: SerialAST Many -> MorlocMonad (SerialAST One)
  oneSerial (SerialPack _ (Many [])) = MM.throwError . SerializationError $ "No valid serializer found"
  oneSerial (SerialPack v (Many ((p,s):_))) = do
    s' <- oneSerial s
    return $ SerialPack v (One (p, s'))
  oneSerial (SerialList s) = SerialList <$> oneSerial s
  oneSerial (SerialTuple ss) = SerialTuple <$> mapM oneSerial ss
  oneSerial (SerialObject r v ps rs) = do
    ts <- mapM (oneSerial . snd) rs
    return $ SerialObject r v ps (zip (map fst rs) ts)
  oneSerial (SerialNum t) = return $ SerialNum t
  oneSerial (SerialBool t) = return $ SerialBool t
  oneSerial (SerialString t) = return $ SerialString t
  oneSerial (SerialNull t) = return $ SerialNull t
  oneSerial (SerialUnknown t) = return $ SerialUnknown t


mapCM :: (c -> MorlocMonad c') -> SAnno g One c -> MorlocMonad (SAnno g One c')
mapCM f (SAnno (One (AccS x k, c)) g) = do
  x' <- mapCM f x
  c' <- f c
  return $ SAnno (One (AccS x' k, c')) g
mapCM f (SAnno (One (ListS xs, c)) g) = do
  xs' <- mapM (mapCM f) xs
  c' <- f c
  return $ SAnno (One (ListS xs', c')) g
mapCM f (SAnno (One (TupleS xs, c)) g) = do
  xs' <- mapM (mapCM f) xs
  c' <- f c
  return $ SAnno (One (TupleS xs', c')) g
mapCM f (SAnno (One (RecS entries, c)) g) = do
  xs' <- mapM (mapCM f) (map snd entries)
  c' <- f c
  return $ SAnno (One (RecS (zip (map fst entries) xs'), c')) g
mapCM f (SAnno (One (LamS vs x, c)) g) = do
  x' <- mapCM f x
  c' <- f c
  return $ SAnno (One (LamS vs x', c')) g
mapCM f (SAnno (One (AppS x xs, c)) g) = do
  x' <- mapCM f x
  xs' <- mapM (mapCM f) xs
  c' <- f c
  return $ SAnno (One (AppS x' xs', c')) g
mapCM f (SAnno (One (VarS x, c)) g) = do
  c' <- f c
  return $ SAnno (One (VarS x, c')) g
mapCM f (SAnno (One (CallS src, c)) g) = do
  c' <- f c
  return $ SAnno (One (CallS src, c')) g
mapCM f (SAnno (One (UniS, c)) g) = do
  c' <- f c
  return $ SAnno (One (UniS, c')) g
mapCM f (SAnno (One (NumS x, c)) g) = do
  c' <- f c
  return $ SAnno (One (NumS x, c')) g
mapCM f (SAnno (One (LogS x, c)) g) = do
  c' <- f c
  return $ SAnno (One (LogS x, c')) g
mapCM f (SAnno (One (StrS x, c)) g) = do
  c' <- f c
  return $ SAnno (One (StrS x, c')) g
mapCM f (SAnno (One (FixS, c)) g) = do
  c' <- f c
  return $ SAnno (One (FixS, c')) g

sannoSnd :: SAnno g One (a, b) -> b
sannoSnd (SAnno (One (_, (_, x))) _) = x

unpackSAnno :: (SExpr g One c -> g -> c -> MorlocMonad a) -> SAnno g One c -> MorlocMonad [a]
unpackSAnno f (SAnno (One (e@(AccS x _),     c)) g) = (:) <$> f e g c <*> unpackSAnno f x
unpackSAnno f (SAnno (One (e@(ListS xs),     c)) g) = (:) <$> f e g c <*> conmapM (unpackSAnno f) xs
unpackSAnno f (SAnno (One (e@(TupleS xs),    c)) g) = (:) <$> f e g c <*> conmapM (unpackSAnno f) xs
unpackSAnno f (SAnno (One (e@(RecS entries), c)) g) = (:) <$> f e g c <*> conmapM (unpackSAnno f) (map snd entries)
unpackSAnno f (SAnno (One (e@(LamS _ x),     c)) g) = (:) <$> f e g c <*> unpackSAnno f x
unpackSAnno f (SAnno (One (e@(AppS x xs),    c)) g) = (:) <$> f e g c <*> conmapM (unpackSAnno f) (x:xs)
unpackSAnno f (SAnno (One (e, c)) g)                = f e g c |>> return


-- generate infinite list of fresh variables of form
-- ['a','b',...,'z','aa','ab',...,'zz',...]
freshVarsAZ
  :: [MT.Text] -- variables to exclude
  -> [MT.Text]
freshVarsAZ exclude =
  filter
    (\x -> not (elem x exclude))
    ([1 ..] >>= flip replicateM ['a' .. 'z'] |>> MT.pack)
