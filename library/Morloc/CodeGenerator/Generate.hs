{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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
    realityCheck
  , generate
) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Typecheck (typecheck, peak)
import Morloc.Data.Doc
import Morloc.Pretty ()
import qualified Data.Map as Map
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as Lang
import qualified Morloc.Monad as MM
import qualified Morloc.CodeGenerator.Nexus as Nexus
import qualified Data.Set as Set
import Morloc.Monad (runIdentity)
import Control.Monad.Except (Except, runExcept, throwError)

import qualified Morloc.CodeGenerator.Grammars.Translator.Cpp as Cpp
import qualified Morloc.CodeGenerator.Grammars.Translator.Rust as Rust
import qualified Morloc.CodeGenerator.Grammars.Translator.R as R
import qualified Morloc.CodeGenerator.Grammars.Translator.Python3 as Python3
import qualified Morloc.CodeGenerator.Serial as Serial


realityCheck
  :: [SAnno (Indexed Type) Many Int]
  -- ^ one AST forest for each command exported from main
  -> MorlocMonad ( [SAnno (Indexed Type) One ()]
                 , [SAnno Int One (Indexed TypeP)]
                 )
realityCheck es = do
  -- translate modules into bitrees
  (gASTs, rASTs)
    -- select a single instance at each node in the tree
    <- mapM realize es
    -- separate unrealized (general) ASTs (uASTs) from realized ASTs (rASTs)
    |>> partitionEithers

  -- concrete typecheck fully realized tree
  rASTs' <- mapM typecheck rASTs

  return (gASTs, rASTs')

-- | Translate typed, abstract syntax forests into compilable code
generate
  :: [SAnno (Indexed Type) One ()]
  -> [SAnno Int One (Indexed TypeP)]
  -> MorlocMonad (Script, [Script])
  -- ^ the nexus code and the source code for each language pool
generate gASTs rASTs = do
  -- Collect all call-free data
  gSerial <- mapM generalSerial gASTs

  -- build nexus
  -- -----------
  -- Each nexus subcommand calls one function from one one pool.
  -- The call passes the pool an index for the function (manifold) that will be called.
  nexus <- Nexus.generate
    gSerial
    [(t, i) | (SAnno (One (_, Idx _ t)) i) <- rASTs]


  -- initialize counter for use in express
  MM.startCounter

  -- for each language, collect all functions into one "pool"
  pools
    <- mapM applyLambdas rASTs   -- SAnno Int One (Indexed TypeP)
    -- thread arguments across the tree
    >>= mapM parameterize
    -- convert from AST to manifold tree
    >>= mapM express

    -- Separate the call trees into mono-lingual segments terminated in
    -- primitives or foreign calls.
    >>= mapM segment |>> concat

    >>= serialize

    -- Gather segments into pools, currently this entails gathering all
    -- segments from a given language into one pool. Later it may be more
    -- nuanced.
    |>> pool -- [SerialManifold]
    -- Generate the code for each pool
    >>= mapM (uncurry encode)    -- Script

  return (nexus, pools)


-- | Choose a single concrete implementation. In the future, this component
-- may be one of te more complex components of the morloc compiler. It will
-- probably need to be implemented using an optimizing SMT solver. It will
-- also need benchmarking data from all the implementations and possibly
-- statistical info describing inputs.
realize
  :: SAnno (Indexed Type) Many Int
  -> MorlocMonad (Either (SAnno (Indexed Type) One ())
                         (SAnno (Indexed Type) One (Indexed Lang)))
realize s0 = do
  e@(SAnno (One (_, li)) (Idx _ _)) <- scoreSAnno [] s0 >>= collapseSAnno Nothing
  case li of
    (Idx _ Nothing) -> makeGAST e |>> Left
    (Idx _ _) -> Right <$> propagateDown e
  where

  -- | Depth first pass calculating scores for each language. Alternates with
  -- scoresSExpr.
  --
  scoreSAnno
    :: [Lang]
    -> SAnno (Indexed Type) Many Int
    -> MorlocMonad (SAnno (Indexed Type) Many (Indexed [(Lang, Int)]))
  scoreSAnno langs (SAnno (Many xs) t) = do
    xs' <- mapM (scoreExpr langs) xs
    return (SAnno (Many xs') t)

  -- | Alternates with scoresSAnno, finds the best score for each language at
  -- application nodes.
  scoreExpr
    :: [Lang]
    -> (SExpr (Indexed Type) Many Int, Int)
    -> MorlocMonad (SExpr (Indexed Type) Many (Indexed [(Lang, Int)]), Indexed [(Lang, Int)])
  scoreExpr langs (AccS x k, i) = do
    x' <- scoreSAnno langs x
    return (AccS x' k, Idx i (scoresOf x'))
  scoreExpr langs (LstS xs, i) = do
    (xs', best) <- scoreMany langs xs
    return (LstS xs', Idx i best)
  scoreExpr langs (TupS xs, i) = do
    (xs', best) <- scoreMany langs xs
    return (TupS xs', Idx i best)
  scoreExpr langs (LamS vs x, i) = do
    x' <- scoreSAnno langs x
    return (LamS vs x', Idx i (scoresOf x'))
  scoreExpr _ (AppS f xs, i) = do
    f' <- scoreSAnno [] f
    let scores = scoresOf f'
    xs' <- mapM (scoreSAnno (unique $ map fst scores)) xs
    -- FIXME: using an arbitrary big number as the default minimum is obviously a bad idea.
    -- I could transform the scores such that this is a maximization problem.
    let pairss = [(minPairs . concat) [xs''' | (_, Idx _ xs''') <- xs''] | SAnno (Many xs'') _ <- xs']
        best = [ (l1, sum [ minimumDef 999999999 [s1 + s2 + Lang.pairwiseCost l1 l2
                          | (l2, s2) <- pairs] | pairs <- pairss])
               | (l1, s1) <- scores]
    return (AppS f' xs', Idx i best)
  scoreExpr langs (NamS rs, i) = do
    (xs, best) <- scoreMany langs (map snd rs)
    return (NamS (zip (map fst rs) xs), Idx i best)
  scoreExpr _ (CallS s, i) = return (CallS s, Idx i [(srcLang s, callCost s)])
  -- non-recursive expressions
  scoreExpr langs (UniS, i) = return (UniS, zipLang i langs)
  scoreExpr langs (VarS v, i) = return (VarS v, zipLang i langs)
  scoreExpr langs (RealS x, i) = return (RealS x, zipLang i langs)
  scoreExpr langs (IntS x, i) = return (IntS x, zipLang i langs)
  scoreExpr langs (LogS x, i) = return (LogS x, zipLang i langs)
  scoreExpr langs (StrS x, i) = return (StrS x, zipLang i langs)

  zipLang :: Int -> [Lang] -> Indexed [(Lang, Int)]
  zipLang i langs = Idx i (zip langs (repeat 0))

  scoresOf :: SAnno a Many (Indexed [(Lang, Int)]) -> [(Lang, Int)]
  scoresOf (SAnno (Many xs) _) = minPairs . concat $ [xs' | (_, Idx _ xs') <- xs]

  -- find the scores of all implementations from all possible language contexts
  scoreMany
    :: [Lang]
    -> [SAnno (Indexed Type) Many Int]
    -> MorlocMonad ([SAnno (Indexed Type) Many (Indexed [(Lang, Int)])], [(Lang, Int)])
  scoreMany langs xs0 = do
    xs1 <- mapM (scoreSAnno langs) xs0
    return (xs1, scoreMany' xs1)
    where
      scoreMany' :: [SAnno (Indexed Type) Many (Indexed [(Lang, Int)])] -> [(Lang, Int)]
      scoreMany' xs =
        let pairss = [ (minPairs . concat) [xs'' | (_, Idx _ xs'') <- xs']
                     | SAnno (Many xs') _ <- xs]
            langs' = unique (langs <> concatMap (map fst) pairss)
        -- Got 10 billion nodes in your AST? I didn't think so, so don't say my sentinal's ugly.
        in [(l1, sum [ minimumDef 999999999 [ score + Lang.pairwiseCost l1 l2
                               | (l2, score) <- pairs]
                     | pairs <- pairss])
           | l1 <- langs']


  collapseSAnno
    :: Maybe Lang
    -> SAnno (Indexed Type) Many (Indexed [(Lang, Int)])
    -> MorlocMonad (SAnno (Indexed Type) One (Indexed (Maybe Lang)))
  collapseSAnno l1 (SAnno (Many es) t@(Idx i _)) = do
    e <- case minBy (\(_, Idx _ ss) -> minimumMay [cost l1 l2 s | (l2, s) <- ss]) es of
      Nothing -> do
        s <- MM.get
        case Map.lookup i (stateName s) of
            (Just generalName) -> MM.throwError . GeneratorError . render $
                "No implementation found for" <+> squotes (pretty generalName)
            Nothing -> undefined
      (Just x@(_, Idx _ ss)) -> collapseExpr (fmap fst (minBy snd ss)) x
    return (SAnno (One e) t)


  cost
    :: Maybe Lang -- parent language (if given)
    -> Lang -- child lang (should always be given if we are working from scored pairs)
    -> Int -- score
    -> Int
  cost (Just l1) l2 score = score + Lang.pairwiseCost l1 l2
  cost _ _ score = score

  -- FIXME: in the future, this function should be replaced by an estimate of
  -- the function runtime, for now I will just base it off languages.
  callCost :: Source -> Int
  callCost src = Lang.languageCost (srcLang src)

  collapseExpr
    :: Maybe Lang -- the language of the parent expression (if Nothing, then this is a GAST)
    -> (SExpr (Indexed Type) Many (Indexed [(Lang, Int)]), Indexed [(Lang, Int)])
    -> MorlocMonad (SExpr (Indexed Type) One (Indexed (Maybe Lang)), Indexed (Maybe Lang))
  collapseExpr l1 (AccS x k, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    x' <- collapseSAnno lang x
    return (AccS x' k, Idx i lang)
  collapseExpr _ (CallS src, Idx i _) = do
   return (CallS src, Idx i (Just $ srcLang src))
  collapseExpr l1 (LstS xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseSAnno lang) xs
    return (LstS xs', Idx i lang)
  collapseExpr l1 (TupS xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseSAnno lang) xs
    return (TupS xs', Idx i lang)
  collapseExpr l1 (LamS vs x, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    x' <- collapseSAnno lang x
    return (LamS vs x', Idx i lang)
  collapseExpr l1 (AppS f xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    f' <- collapseSAnno lang f
    xs' <- mapM (collapseSAnno lang) xs
    return (AppS f' xs', Idx i lang)
  collapseExpr l1 (NamS rs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseSAnno lang . snd) rs
    return (NamS (zip (map fst rs) xs'), Idx i lang)
  -- collapse leaf expressions
  collapseExpr lang (UniS,   Idx i _) = return (UniS,   Idx i lang)
  collapseExpr lang (VarS v, Idx i _) = return (VarS v, Idx i lang)
  collapseExpr lang (RealS x, Idx i _) = return (RealS x, Idx i lang)
  collapseExpr lang (IntS x, Idx i _) = return (IntS x, Idx i lang)
  collapseExpr lang (LogS x, Idx i _) = return (LogS x, Idx i lang)
  collapseExpr lang (StrS x, Idx i _) = return (StrS x, Idx i lang)

  chooseLanguage :: Maybe Lang -> [(Lang, Int)] -> MorlocMonad (Maybe Lang)
  chooseLanguage l1 ss = do
    case minBy snd [(l2, cost l1 l2 s2) | (l2, s2) <- ss] of
      Nothing -> return Nothing
      (Just (l3, _)) -> return (Just l3)

  minBy :: Ord b => (a -> b) -> [a] -> Maybe a
  minBy _ [] = Nothing
  minBy _ [x] = Just x
  minBy f (x1:rs) = case minBy f rs of
    Nothing -> Just x1
    (Just x2) -> if f x1 <= f x2 then Just x1 else Just x2

  -- find the lowest cost function for each key
  -- the groupSort function will never yield an empty value for vs, so `minimum` is safe
  minPairs :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]
  minPairs = map (second minimum) . groupSort

  propagateDown
    ::              SAnno (Indexed Type) One (Indexed (Maybe Lang))
    -> MorlocMonad (SAnno (Indexed Type) One (Indexed        Lang))
  propagateDown (SAnno (One (_, Idx _ Nothing)) _) = MM.throwError . CallTheMonkeys $ "Nothing is not OK"
  propagateDown e@(SAnno (One (_, Idx _ (Just lang0))) _) = f lang0 e where
    f :: Lang ->     SAnno (Indexed Type) One (Indexed (Maybe Lang))
      -> MorlocMonad (SAnno (Indexed Type) One (Indexed        Lang))
    f lang (SAnno (One (e', Idx i Nothing)) g) = f lang (SAnno (One (e', Idx i (Just lang))) g)
    f _ (SAnno (One (e', Idx i (Just lang))) g) = do
      e'' <- case e' of
        (AccS x k) -> AccS <$> f lang x <*> pure k
        (AppS x xs) -> AppS <$> f lang x <*> mapM (f lang) xs
        (LamS vs x) -> LamS vs <$> f lang x
        (LstS xs) -> LstS <$> mapM (f lang) xs
        (TupS xs) -> TupS <$> mapM (f lang) xs
        (NamS rs) -> NamS <$> (zip (map fst rs) <$> mapM (f lang . snd) rs)
        UniS -> return UniS
        (VarS x) -> return (VarS x)
        (RealS x) -> return (RealS x)
        (IntS x) -> return (IntS x)
        (LogS x) -> return (LogS x)
        (StrS x) -> return (StrS x)
        (CallS x) -> return (CallS x)
      return (SAnno (One (e'', Idx i lang)) g)

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
  mayName <- MM.metaName i
  n <- case mayName of
    Nothing -> MM.throwError . OtherError $ "No general type found for call-free function"
    (Just n') -> return n'
  let base = NexusCommand {
      commandName = n -- EVar -- user-exposed subcommand name in the nexus
    , commandType = t -- Type -- the general type of the expression
    , commandJson = dquotes "_" -- MDoc -- JSON output with null's where values will be replaced
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
    generalSerial' base _ (SAnno (One (RealS x, _)) _)
      = return $ base { commandJson = viaShow x }
    generalSerial' base _ (SAnno (One (IntS x, _)) _)
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
    generalSerial' base ps (SAnno (One (AccS (SAnno (One (VarS v, _)) (Idx _ NamT {})) k, _)) _) =
      return $ base { commandSubs = [(ps, unEVar v, [JsonKey k])] }
    -- If the accessed type is not a record, then die
    generalSerial' _ _ (SAnno (One (AccS (SAnno _ (Idx _ t')) _, _)) _) =
        MM.throwError . OtherError . render $ "Non-record access of type:" <+> pretty t'
    generalSerial' base ps (SAnno (One (LstS xs, _)) _) = do
      ncmds <- zipWithM (generalSerial' base) [ps ++ [JsonIndex j] | j <- [0..]] xs
      return $ base
        { commandJson = list (map commandJson ncmds)
        , commandSubs = concatMap commandSubs ncmds
        }
    generalSerial' base ps (SAnno (One (TupS xs, _)) _) = do
      ncmds <- zipWithM (generalSerial' base) [ps ++ [JsonIndex j] | j <- [0..]] xs
      return $ base
        { commandJson = list (map commandJson ncmds)
        , commandSubs = concatMap commandSubs ncmds
        }
    generalSerial' base ps (SAnno (One (NamS es, _)) _) = do
      ncmds <- fromJust <$>
        safeZipWithM
          (generalSerial' base)
          [ps ++ [JsonKey k] | k <- map fst es]
          (map snd es)
      let entries = zip (map fst es) (map commandJson ncmds)
          obj = encloseSep "{" "}" ","
                (map (\(k, v) -> dquotes (pretty k) <> ":" <> v) entries)
      return $ base
        { commandJson = obj
        , commandSubs = concatMap commandSubs ncmds
        }
    generalSerial' base ps (SAnno (One (LamS vs x, _)) _) = do
      ncmd <- generalSerial' base ps x
      return $ ncmd { commandArgs = vs }
    generalSerial' base ps (SAnno (One (VarS (EV v), _)) _) =
      return $ base { commandSubs = [(ps, v, [])] }
    generalSerial' _ _ (SAnno (One _) (Idx _ gt)) = do
      MM.throwError . OtherError . render $
        "Cannot serialize general type:" <+> pretty gt


{- | Remove lambdas introduced through substitution

For example:

 bif x = add x 10
 bar py :: "int" -> "int"
 bar y = add y 30
 f z = bar (bif z)

In Treeify.hs, the morloc declarations will be substituted in as lambdas. But
we want to preserve the link to any annotations (in this case, the annotation
that `bar` should be in terms of python ints). The morloc declarations can be
substituted in as follows:

 f z = (\y -> add y 30) ((\x -> add x 10) z)

The indices for bif and bar that link the annotations to the functions are
relative to the lambda expressions, so this substitution preserves the link.
Typechecking can proceed safely.

The expression can be simplified:

 f z = (\y -> add y 30) ((\x -> add x 10) z)
 f z = (\y -> add y 30) (add z 10)            -- [z / x]
 f z = add (add z 10) 30                      -- [add z 10 / y]

The simplified expression is what should be written in the generated code. It
would also be easier to typecheck and debug. So should these substitutions be
done immediately after parsing? We need to preserve
 1. links to locations in the original source code (for error messages)
 2. type annotations.
 3. declaration names for generated comments and subcommands

Here is the original expression again, but annotated and indexed

 (\x -> add_2 x_3 10_4)_1
 (\y -> add_6 y_7 30_8)_5
 (\z -> bar_10 (bif_11 z_12))_9

 1: name="bif"
 5: name="bar", type="int"@py -> "int"@py
 9: name="f"

Each add is also associated with a type defined in a signature in an
unmentioned imported library, but those will be looked up by the typechecker
and will not be affected by rewriting.

Substitution requires reindexing. A definition can be used multiple times and
we need to distinguish between the use cases.

Replace bif and bar with their definition and create fresh indices:

 (\z -> (\y -> add_18 y_19 30_20)_17 ((\x -> add_14 x_15 10_16)_13 z_12)_9

 13,1: name="bif"
 17,5: name="bar", type="int"@py -> "int"@py
 9: name="f"

Now we can substitute for y

 (\z -> add_18 ((\x -> add_14 x_15 10_16)_13 z_12)_9 30_20)

But this destroyed index 17 and the link to the python annotation. We can
preserve the type by splitting the annotation of bar.

 13,1: name="bif"
 18,17,5: name="bar"
 12: "int"@py
 13: "int"@py
 9: name="f"

Index 18 should be associated with the *name* "bar", but not the type, since it
has been applied. The type of bar is now split between indices 12 and 13.

This case works fine, but it breaks down when types are polymorphic. If the
annotation of bar had been `a -> a`, then how would we type 12 and 13? We can't
say that `12 :: forall a . a` and `13 :: forall a . a`, since this
eliminates the constraint that the `a`s must be the same.

If instead we rewrite lambdas after typechecking, then everything works out.

Thus applyLambdas is done here, rather than in Treeify.hs or Desugar.hs.

It also must be done BEFORE conversion to ExprM in `express`, where manifolds
are resolved.
-}
applyLambdas
  :: SAnno Int One (Indexed TypeP)
  -> MorlocMonad (SAnno Int One (Indexed TypeP))
-- eliminate empty lambdas
applyLambdas (SAnno (One (AppS ( SAnno (One (LamS [] (SAnno e _), _)) _) [], _)) i) = applyLambdas $ SAnno e i

-- eliminate empty applications
applyLambdas (SAnno (One (AppS (SAnno e _) [], _)) i) = applyLambdas $ SAnno e i

-- substitute applied lambdas
applyLambdas (SAnno (One (AppS (SAnno (One (LamS (v:vs) e2, Idx j2 (FunP (_:tas) tb2))) i2) (e1:es), tb1)) i1) = do
  let e2' = substituteSAnno v e1 e2
  applyLambdas (SAnno (One (AppS (SAnno (One (LamS vs e2', Idx j2 (FunP tas tb2))) i2) es, tb1)) i1)

-- propagate the changes
applyLambdas (SAnno (One (AppS f es, c)) g) = do
  f' <- applyLambdas f
  es' <- mapM applyLambdas es
  return (SAnno (One (AppS f' es', c)) g)
applyLambdas (SAnno (One (AccS e k, c)) g) = do
  e' <- applyLambdas e
  return (SAnno (One (AccS e' k, c)) g)
applyLambdas (SAnno (One (LamS vs e, c)) g) = do
  e' <- applyLambdas e
  return (SAnno (One (LamS vs e', c)) g)
applyLambdas (SAnno (One (LstS es, c)) g) = do
  es' <- mapM applyLambdas es
  return (SAnno (One (LstS es', c)) g)
applyLambdas (SAnno (One (TupS es, c)) g) = do
  es' <- mapM applyLambdas es
  return (SAnno (One (TupS es', c)) g)
applyLambdas (SAnno (One (NamS rs, c)) g) = do
  es' <- mapM (applyLambdas . snd) rs
  return (SAnno (One (NamS (zip (map fst rs) es'), c)) g)
applyLambdas x = return x

substituteSAnno
  :: EVar
  -> SAnno Int One (Indexed TypeP)
  -> SAnno Int One (Indexed TypeP)
  -> SAnno Int One (Indexed TypeP)
substituteSAnno v r = f where
  f e@(SAnno (One (VarS v', _)) _)
    | v == v' = r
    | otherwise = e
  -- propagate the changes
  f (SAnno (One (AppS e es, c)) g) =
    let f' = f e
        es' = map f es
    in SAnno (One (AppS f' es', c)) g
  f (SAnno (One (AccS e k, c)) g) =
    let e' = f e
    in SAnno (One (AccS e' k, c)) g
  f (SAnno (One (LamS vs e, c)) g) =
    let e' = f e
    in SAnno (One (LamS vs e', c)) g
  f (SAnno (One (LstS es, c)) g) =
    let es' = map f es
    in SAnno (One (LstS es', c)) g
  f (SAnno (One (TupS es, c)) g) =
    let es' = map f es
    in SAnno (One (TupS es', c)) g
  f (SAnno (One (NamS rs, c)) g) =
    let es' = map (f . snd) rs
    in SAnno (One (NamS (zip (map fst rs) es'), c)) g
  f x = x

-- | Add arguments that are required for each term. Unneeded arguments are
-- removed at each step.
parameterize
  :: SAnno Int One (Indexed TypeP)
  -> MorlocMonad (SAnno Int One (Indexed TypeP, [Arg EVar]))
parameterize (SAnno (One (LamS vs x, c@(Idx _ (FunP inputs _)))) m) = do
  MM.sayVVV "Entering parameterize LamS"
  ids <- MM.takeFromCounter (length inputs)
  let args0 = fromJust $ safeZipWith Arg ids vs
  x' <- parameterize' args0 x
  return $ SAnno (One (LamS vs x', (c, args0))) m
parameterize (SAnno (One (CallS src, c@(Idx _ (FunP inputs _)))) m) = do
  MM.sayVVV $ "Entering parameterize CallS - " <> pretty (srcName src) <> "@" <> pretty (srcLang src)
  ids <- MM.takeFromCounter (length inputs)
  let vs = map EV (freshVarsAZ [])
      args0 = fromJust $ safeZipWith Arg ids vs
  return $ SAnno (One (CallS src, (c, args0))) m
parameterize x = do
  MM.sayVVV "Entering parameterize Other"
  parameterize' [] x

parameterize'
  :: [Arg EVar] -- arguments in parental scope (child needn't retain them)
  -> SAnno Int One (Indexed TypeP)
  -> MorlocMonad (SAnno Int One (Indexed TypeP, [Arg EVar]))
-- primitives, no arguments are required for a primitive, so empty lists
parameterize' _ (SAnno (One (UniS, c)) m) = return $ SAnno (One (UniS, (c, []))) m
parameterize' _ (SAnno (One (RealS x, c)) m) = return $ SAnno (One (RealS x, (c, []))) m
parameterize' _ (SAnno (One (IntS x, c)) m) = return $ SAnno (One (IntS x, (c, []))) m
parameterize' _ (SAnno (One (LogS x, c)) m) = return $ SAnno (One (LogS x, (c, []))) m
parameterize' _ (SAnno (One (StrS x, c)) m) = return $ SAnno (One (StrS x, (c, []))) m
parameterize' args (SAnno (One (VarS v, c)) m) = do
  let args' = [r | r@(Arg _ v') <- args, v' == v]
  return $ SAnno (One (VarS v, (c, args'))) m
parameterize' _ (SAnno (One (CallS src, c)) m) = do
  return $ SAnno (One (CallS src, (c, []))) m
parameterize' args (SAnno (One (AccS x k, c)) m) = do
  x' <- parameterize' args x
  let args' = pruneArgs args [x']
  return $ SAnno (One (AccS x' k, (c, args'))) m
parameterize' args (SAnno (One (LstS xs, c)) m) = do
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args xs'
  return $ SAnno (One (LstS xs', (c, args'))) m
parameterize' args (SAnno (One (TupS xs, c)) m) = do
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args xs'
  return $ SAnno (One (TupS xs', (c, args'))) m
parameterize' args (SAnno (One (NamS entries, c)) m) = do
  xs' <- mapM (parameterize' args . snd) entries
  let args' = pruneArgs args xs'
  return $ SAnno (One (NamS (zip (map fst entries) xs'), (c, args'))) m
parameterize' args (SAnno (One (LamS vs x, c@(Idx _ (FunP inputs _)))) m) = do
  ids <- MM.takeFromCounter (length inputs)
  let contextArgs = [r | r@(Arg _ v) <- args, v `notElem` vs] -- remove shadowed arguments
      boundArgs = fromJust $ safeZipWith Arg ids vs
  x' <- parameterize' (contextArgs <> boundArgs) x
  let contextArgs' = pruneArgs contextArgs [x']
  return $ SAnno (One (LamS vs x', (c, contextArgs' <> boundArgs))) m
-- LamS MUST have a functional type, deviations would have been caught by the typechecker
parameterize' _ (SAnno (One (LamS _ _, _)) _) = error "impossible"
parameterize' args (SAnno (One (AppS x xs, c)) m) = do
  x' <- parameterize' args x
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args (x':xs')
  return $ SAnno (One (AppS x' xs', (c, args'))) m

pruneArgs :: [Arg a] -> [SAnno c One (g, [Arg a])] -> [Arg a]
pruneArgs args xs = 
  let usedArgs = unique $ concatMap (map ann . sannoSnd) xs
  in [r | r@(Arg i _) <- args, i `elem` usedArgs]

express :: SAnno Int One (Indexed TypeP, [Arg EVar]) -> MorlocMonad PolyHead
-- CallS - direct export of a sourced function, e.g.:
express (SAnno (One (CallS src, (Idx _ c@(FunP inputs _), _))) m) = do
  MM.sayVVV $ "express CallS - direct export:" <+> parens (pretty $ srcName src) <+> "::" <+> pretty c
  ids <- MM.takeFromCounter (length inputs)
  let lambdaVals = fromJust $ safeZipWith PolyBndVar (map Right inputs) ids
  return
    . PolyHead m [Arg i None | i <- ids]
    . PolyReturn
    $ PolyApp (PolySrc c src) lambdaVals

-- *****************  EVIL INDEX REWRITE HACK WARNING ************************
-- Move the index from the lambda to the application.
-- Changing indices is a BAD idea, it breaks the link to the source code
-- I do it here so that the nexus indices and pool indices match, but there
-- should be a more elegant solution.
-- ***************************************************************************
-- We pass the index and the arguments from the top-level lamda expression
-- to the application. The arguments for the lambda will include the specific
-- arguments the type signature and declaration specify, the arguments that
-- the user expects to enter. However, the application will prune any
-- arguments that are not used. So here we want the lambda, not the
-- application.
-- ----
-- lambda
express (SAnno (One (LamS _ (SAnno (One (x, (Idx i c, _))) _), (_, lambdaArgs))) lambdaIndex) = do
  MM.sayVVV "express LamS"
  express (SAnno (One (x, (Idx i c, lambdaArgs))) lambdaIndex)

express (SAnno (One (LstS xs, (Idx _ (AppP (VarP v) [t]), args))) m) = do
  xs' <- mapM (expressPolyExpr t) xs
  let x = PolyList v t xs'
  return $ PolyHead m [Arg i None | Arg i _ <- args] (PolyReturn x)
express (SAnno (One (LstS _, _)) _) = error "Invalid list form"

express (SAnno (One (TupS xs, (Idx _ (AppP (VarP v) ts), args))) m) = do
  xs' <- fromJust <$> safeZipWithM expressPolyExpr ts xs
  let x = PolyTuple v (fromJust $ safeZip ts xs')
  return $ PolyHead m [Arg i None | Arg i _ <- args] (PolyReturn x)
express (SAnno (One (TupS _, _)) _) = error "Invalid tuple form"

-- records
express (SAnno (One (NamS entries, (Idx _ (NamP o v ps rs), args))) m) = do
  xs' <- fromJust <$> safeZipWithM expressPolyExpr (map snd rs) (map snd entries)
  let x = PolyRecord o v ps (zip (map fst rs) (zip (map snd rs) xs'))
  return $ PolyHead m [Arg i None | Arg i _ <- args] (PolyReturn x)

-- In other cases, it doesn't matter whether we are at the top of the call
express e@(SAnno (One (_, (Idx _ t, args))) m)
  = PolyHead m [Arg i None | Arg i _ <- args] <$> expressPolyExpr t e

expressPolyExpr :: TypeP -> SAnno Int One (Indexed TypeP, [Arg EVar]) -> MorlocMonad PolyExpr

-- these cases will include partially applied functions and explicit lambdas
-- the former is transformed into the latter in the frontend typechecker
expressPolyExpr pc
  (SAnno (One (LamS vs
    (SAnno (One (AppS
      (SAnno (One (CallS src
                  , (Idx _ callType@(FunP callInputTypes callOutputType), _)
                  )
             ) _)
      xs
                , (Idx _ appType, appArgs)
                )
           ) _)
              , (Idx _ lamType@(FunP lamInputTypes lamOutType), lamArgs))
         ) m)
 
  ----------------------------------------------------------------------------------------
  -- #3 cis full lambda                                        | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      f_L :: A -> B -> C -> D                              | []          | [x,y,z]   |
  --      g_L (\x y z -> f_L y z x) xs                         |             |           |
  --      ----------------------------                         |             |           |
  --      def m1(xs):                                          |             |           |
  --          return g (lambda x, y, z: f(y, z, x), xs)        |             |           |
  --                                                           |             |           |
  ----------------------------------------------------------------------------------------
  | sameLanguage && length appArgs == length vs = do
      MM.sayVVV "case #3"
      MM.sayVVV $ "appArgs:" <+> list (map pretty appArgs)
      MM.sayVVV $ "callInputTypes:" <+> list (map viaShow callInputTypes)

      let args = fromJust $ safeZipWith (\(Arg i _) t -> Arg i (Just (typeFof t))) lamArgs lamInputTypes
      xs' <- fromJust <$> safeZipWithM expressPolyExpr callInputTypes xs
      return
          . PolyManifold m (ManifoldPass args)
          . PolyReturn
          $ PolyApp (PolySrc callType src) xs'

  ----------------------------------------------------------------------------------------
  -- #4 cis partial lambda                                     | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      g_L (\z x -> f_L x y z) xs                           | [y]         | [x,z]     |
  --      --------------------------                           |             |           |
  --      def m1(xs, y):                                       |             |           |
  --          return g(lambda x, z: m2(z, y, x), xs)           |             |           |
  --                                                           |             |           |
  ----------------------------------------------------------------------------------------
  | sameLanguage = do
      MM.sayVVV "case #4"
      let nContextArgs = length appArgs - length vs
          contextArgs = map unvalue (take nContextArgs appArgs)

          typedLambdaArgs = fromJust $ safeZipWith (\(Arg i _) t -> Arg i (Just (typeFof t)))
            (drop nContextArgs lamArgs)
            lamInputTypes

      xs' <- fromJust <$> safeZipWithM expressPolyExpr callInputTypes xs
      return
        . PolyManifold m (ManifoldPart contextArgs typedLambdaArgs)
        . PolyReturn
        $ PolyApp call xs'


  ----------------------------------------------------------------------------------------
  -- #7 trans full lambda                                      | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      f_M :: A -> B -> C -> D                              | []          | [x,y,z]   |
  --      g_L (\z y x -> f_L x y z)                            |             |           |
  --      -------------------------                            |             |           |
  --      def m2(x, y, z):                                     |             |           |
  --          x' = SERIALiZE(x)                                |             |           |
  --          y' = SERIALiZE(y)                                |             |           |
  --          z' = SERIALiZE(z)                                |             |           |
  --          r' = CALL(2, x', y', z')                         |             |           |
  --          return DESERIALIZE(r')                           |             |           |
  --                                                           |             |           |
  --      def m1(xs):                                          |             |           |
  --          return g (lambda z y x: m2(x, y, z)) xs          |             |           |
  --                                                           |             |           |
  ----------------------------------------------------------------------------------------
  | not sameLanguage && length appArgs == length vs = do
      MM.sayVVV "case #7"
      let n = length xs - length vs
      xsLocal <- zipWithM expressPolyExpr callInputTypes (take n xs)

      let xsPassed = bindVar appArgs (drop n callInputTypes)
          xs' = xsLocal <> xsPassed

      let typedBoundArgs = fromJust $ safeZipWith
            (\(Arg i _) t -> Arg i (Just (typeFof t)))
            (drop (length appArgs - length lamInputTypes) lamArgs)
            lamInputTypes

      return
        . PolyManifold m (ManifoldPass typedBoundArgs)
        . PolyReturn
        . PolyApp
          ( PolyForeignInterface appType (map ann appArgs)
          . PolyManifold m (ManifoldFull (map unvalue appArgs))
          . PolyReturn
          $ PolyApp call xs'
          )
        -- These arguments are passed to a foreign function, however, they are
        -- called by a local function which means they must be native and
        -- their types must be known. Leaving these terms as passthrough types
        -- was the cause of the cadf62 bug.
        $ bindVar appArgs lamInputTypes


  ----------------------------------------------------------------------------------------
  -- #8 trans partial lambda                                   | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      g_L (\z x -> f_L x y z)                              | [y]         | [z,x]     |
  --      -----------------------                              |             |           |
  --      def m2(x, y', z):                                    |             |           |
  --          x' = SERIALIZE(x)                                |             |           |
  --          z' = SERIALIZE(z)                                |             |           |
  --          r' = CALL(2, x', y', z')                         |             |           |
  --          return DESERIALIZE(r')                           |             |           |
  --                                                           |             |           |
  --      def m1(y):                                           |             |           |
  --          y' = SERIALIZE(y)                                |             |           |
  --          return g(lambda z, x: m2(x, y, z))               |             |           |
  ----------------------------------------------------------------------------------------
  | not sameLanguage = do
      MM.sayVVV "case #8"
      -- 1. express all xs under the foreign type if they are in the foreign language or
      --    the parent type (pc) if they are in the parent language
      --
      -- 2. let extract each expression that is not in the foreign type,
      --    replace the corresponding xs' value with a BndVar. store these
      --    ids.
      --
      -- 3. Translate the letArgs to the foreign type (based on id and position in
      --    callInputs)
      --
      -- 4. Find foreign arg list: extract args in foreign language from each
      --    xs' element and then append translated let args (step 3)
      --
      -- 5. Using argument ids, map these foreign args to local args in
      --    (appArgs + letArgs)
      --
      -- 6. Fold let statements over local manifold

      -- evaluate arguments and derive any required let bindings
      xsInfo <- fromJust <$> safeZipWithM (partialExpress pc) callInputTypes xs

      let xs' = map (\(_, _, e) -> e) xsInfo
          -- rs: the list of arguments (by index) required by a single expression
          --     passed to the foreign function
          -- callArgs: the ordered unique set of the required arguments
          callArgs = unique (concatMap (\(rs, _, _) -> rs) xsInfo)
          args = [i | Arg i _ <- appArgs]
          allParentArgs = args <> [i | (_, Just (i, _), _) <- xsInfo]
          lets = [PolyLet i e | (_, Just (i, e), _) <- xsInfo]
          -- arguments on the calling side
          passedParentArgs = concat [[r | r <- allParentArgs, r == i] | i <- callArgs]
          -- manifold arguments
          nContextArgs = length appArgs - length vs

          -- the types of the terms passed through the lambda
          lambdaTypeMap = zip vs lamInputTypes
          -- variables passed to the manifold, those bound by the lambda will
          -- have known local types
          boundVars = [ PolyBndVar (maybe (Left (langOf' pc)) Right (lookup v lambdaTypeMap)) i
                      | Arg i v <- appArgs
                      ]
          untypedContextArgs = map unvalue $ take nContextArgs appArgs
          typedPassedArgs = fromJust $ safeZipWith (\(Arg i _) t -> Arg i (Just (typeFof t))) (drop nContextArgs lamArgs) lamInputTypes

      return
        . PolyManifold m (ManifoldPart untypedContextArgs
                                       typedPassedArgs)
        . chain lets
        . PolyReturn
        . PolyApp
            ( PolyForeignInterface lamOutType passedParentArgs
            . PolyManifold m (ManifoldFull (map unvalue appArgs))
            . PolyReturn
            $ PolyApp call xs'
            )
        $ boundVars



  where
    sameLanguage = langOf pc == langOf callType

    call = PolySrc callType src

    chain :: [a -> a] -> a -> a
    chain [] x = x
    chain (f:fs) x = chain fs (f x)

    -- This function is applied to every argument of a foreign call in a lambda (case #8)
    --
    -- Partitions evaluation of expressions applied to a foreign pool between the
    -- local and foreign contexts
    partialExpress
        :: TypeP -- parent type of the manifold (not this expression)
        -> TypeP -- foreign type for this expression
        -> SAnno Int One (Indexed TypeP, [Arg EVar]) -- expression
        -> MorlocMonad
            ( [Int] -- ordered foreign arguments, should include ids bound by let (next arg)
            , Maybe (Int, PolyExpr) -- parent let statement if not in child language and eval is needed
            , PolyExpr -- final foreign expression
            )
    -- If the argument is a variable, link the argument id to the variable id and
    -- assign it the foreign call type
    partialExpress _ foreignType (SAnno (One (VarS _, (_, [Arg idx _]))) _) = do
      let x' = PolyBndVar (Right foreignType) idx
      return ([idx], Nothing, x')
    -- Otherwise
    partialExpress localType foreignType x@(SAnno (One (_, (Idx _ exprType, args))) _)
      -- if this expression is implemented on the foreign side,
      --   translate to ExprM and record
      | langOf exprType == langOf foreignType = do
          x' <- expressPolyExpr foreignType x
          return ([i | Arg i _ <- args], Nothing, x')
      -- if this expression is implemented on the calling side,
      --   let-bind the expression on the calling side and use the let-index as an
      --   argument index
      | otherwise = do
          letVal <- expressPolyExpr localType x
          idx <- MM.getCounter
          let x' = PolyLetVar foreignType idx
          -- Only the let-bound argument is used on the foreign side
          return ([idx], Just (idx, letVal), x')


expressPolyExpr _ (SAnno (One (LamS vs body, (Idx _ lambdaType, manifoldArguments))) m) = do
    body' <- expressPolyExpr lambdaType body

    inputTypes <- case lambdaType of
      (FunP ts _) -> return $ map typeFof ts
      _ -> return []

    let contextArguments = map unvalue $ take (length manifoldArguments - length vs) manifoldArguments
        boundArguments = map unvalue $ drop (length contextArguments) manifoldArguments
        typeBoundArguments = fromJust $ safeZipWith (\t (Arg i _) -> Arg i (Just t)) inputTypes boundArguments

    MM.sayVVV $ "Express lambda:"
              <> "\n  vs:" <+> pretty vs
              <> "\n  lambdaType:" <+> pretty lambdaType
              <> "\n  manifoldArguments:" <+> list (map pretty manifoldArguments)
              <> "\n  contextArguments:" <+> list (map pretty contextArguments)
              <> "\n  boundArguments" <+> list (map pretty typeBoundArguments)

    return
      . PolyManifold m (ManifoldPart contextArguments typeBoundArguments)
      . PolyReturn
      $ body'

-- Apply arguments to a sourced function
-- * The CallS object may be in a foreign language. These inter-language
--   connections will be snapped apart in the segment step.
-- * These applications will be fully applied, the case of partially applied
--   functions will have been handled previously by LamM
expressPolyExpr pc (SAnno (One (AppS (SAnno (One (CallS src, (Idx _ fc@(FunP inputs _), _))) _) xs, (Idx _ appType, args))) m)

  ----------------------------------------------------------------------------------------
  -- #1 cis applied                                            | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      f_L :: A -> B -> C -> D                              | [x,y,z]     | []        |
  --      g_L (f_L x y z)                                      |             |           |
  --      -----------------------                              |             |           |
  --      def m1(x,y,z):                                       |             |           |
  --          g m2(x, y, z)                                    |             |           |
  ----------------------------------------------------------------------------------------
  | sameLanguage = do
      MM.sayVVV $ "case #1 - " <> parens (pretty (srcName src)) <> ":"
      -- There should be an equal number of input types and input arguments
      -- That is, the function should be fully applied. If it were partially
      -- applied, the lambda case would have been entered previously instead.
      xs' <- fromJust <$> safeZipWithM expressPolyExpr inputs xs

      MM.sayVVV "  leaving case #1"
      return
          . PolyManifold m (ManifoldFull (map unvalue args))
          . PolyReturn
          $ PolyApp f xs'

  ----------------------------------------------------------------------------------------
  -- #5 trans applied                                          | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --                                                           | [x,y,z]     | []        |
  --      f_M :: A -> B -> C -> D                              |             |           |
  --      g_L (f_M x y z)                                      |             |           |
  --      -----------------------                              |             |           |
  --      def m1(x,y,z):                                       |             |           |
  --          x' = SERIALIZE(x)   -- maybe, depending on       |             |           |
  --          y' = SERIALIZE(y)   -- context                   |             |           |
  --          z' = SERIALIZE(z)                                |             |           |
  --          r' = CALL(2, x', y', z')   -- where 2 is         |             |           |
  --          r = DESERIALIZE(r')        -- the foreign        |             |           |
  --          return g(r)                -- manifold number    |             |           |
  --                                                           |             |           |
  ----------------------------------------------------------------------------------------
  | not sameLanguage = do
        MM.sayVVV $ "case #5 - " <> parens (pretty (srcName src)) <> ":"
        MM.sayVVV $ "args:" <+> list (map pretty args)

        xs' <- fromJust <$> safeZipWithM expressPolyExpr inputs xs
        return
          . PolyManifold m (ManifoldFull (map unvalue args))
          . PolyReturn
          . PolyApp
              ( PolyForeignInterface pc [] -- no args are passed, so empty
              . PolyManifold m (ManifoldFull (map unvalue args))
              . PolyReturn
              $ PolyApp f xs'
              )
          -- non-native use, leave as passthrough
          -- the contextual language, though, is the same as the parent
          $ [PolyBndVar (Left (langOf' pc)) i | Arg i _ <- args]

  where
    sameLanguage = langOf pc == langOf fc
    f = PolySrc fc src

-- An un-applied source call
expressPolyExpr pc@(FunP pinputs poutput) (SAnno (One (CallS src, (Idx _ c@(FunP callInputs _), _))) m)
  ----------------------------------------------------------------------------------------
  -- #2 cis passed                                             | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      f_L :: A -> B                                        | []          | []        | -- FIXME
  --      g_L f_L xs                                           |             |           |
  --      -------------                                        |             |           |
  --      def m1(xs):                                          |             |           |
  --          g(m2, xs)                                        |             |           |
  ----------------------------------------------------------------------------------------
  | langOf pc == langOf c = do
      MM.sayVVV $ "case #2 - un-applied cis source call:" <+> pretty (srcName src)
      ids <- MM.takeFromCounter (length callInputs)
      let lambdaVals = bindVarIds ids callInputs
          lambdaTypedArgs = fromJust $ safeZipWith annotate ids (map (Just . typeFof) callInputs)
      return
        . PolyManifold m (ManifoldPass lambdaTypedArgs)
        . PolyReturn
        $ PolyApp (PolySrc c src) lambdaVals

  ----------------------------------------------------------------------------------------
  -- #6 trans passed                                           | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      f_L :: A -> B                                        | []          | []        | -- FIXME
  --      g_L f_L xs                                           |             |           |
  --      -------------                                        |             |           |
  --      def m2(x):                                           |             |           |
  --          x' = SERIALiZE(x)                                |             |           |
  --          r' = CALL(2, x')                                 |             |           |
  --          return DESERIALIZE(r')                           |             |           |
  --                                                           |             |           |
  --      def m1(xs):                                          |             |           |
  --          return g(m2, xs)                                 |             |           |
  ----------------------------------------------------------------------------------------
  | otherwise = do
      MM.sayVVV $ "case #6 - " <> pretty m
      MM.sayVVV $ "Un-applied trans source call:" <+> pretty (srcName src)
      ids <- MM.takeFromCounter (length callInputs)
      let lambdaArgs = [Arg i None | i <- ids]
          lambdaTypedArgs = map (`Arg` Nothing) ids
          callVals = bindVarIds ids callInputs

      MM.sayVVV $ "src:" <+> pretty src
      MM.sayVVV $ "lambdaArgs:" <+> list (map pretty lambdaArgs)

      return
       . PolyManifold m (ManifoldPass lambdaTypedArgs)
       . PolyReturn
       . PolyApp
           ( PolyForeignInterface poutput (map ann lambdaArgs)
           . PolyManifold m (ManifoldFull lambdaArgs)
           . PolyReturn
           $ PolyApp (PolySrc c src) callVals
           )
       $ fromJust $ safeZipWith (PolyBndVar . Right) pinputs (map ann lambdaArgs)

-- bound variables
expressPolyExpr _ (SAnno (One (VarS v, (Idx _ c, rs))) _) = do
  MM.sayVVV $ "express' VarS" <+> parens (pretty v) <+> "::" <+> pretty c
  case [i | (Arg i v') <- rs, v == v'] of
    [r] -> return $ PolyBndVar (Right c) r
    rs' -> MM.throwError . OtherError . render
        $ "Expected VarS" <+> dquotes (pretty v) <+>
          "of type" <+> parens (pretty c) <+> "to match exactly one argument, found:" <+> list (map pretty rs')

-- primitives
expressPolyExpr _ (SAnno (One (RealS x, (Idx _ (VarP v), _))) _) = return $ PolyReal v x
expressPolyExpr _ (SAnno (One (IntS x, (Idx _ (VarP v), _))) _)  = return $ PolyInt v x
expressPolyExpr _ (SAnno (One (LogS x, (Idx _ (VarP v), _))) _)  = return $ PolyLog v x
expressPolyExpr _ (SAnno (One (StrS x, (Idx _ (VarP v), _))) _)  = return $ PolyStr v x
expressPolyExpr _ (SAnno (One (UniS, (Idx _ (VarP v), _))) _)    = return $ PolyNull v

-- record access
expressPolyExpr pc (SAnno (One (AccS record@(SAnno (One (_, (Idx _ (NamP o v _ rs), _))) _) key, _)) _) = do
  record' <- expressPolyExpr pc record
  case lookup key [(ckey, ct) | (PV _ _ ckey, ct) <- rs] of
    (Just valType) -> return $ PolyAcc valType o v record' key
    Nothing -> error "invalid key access"

-- lists
expressPolyExpr _ (SAnno (One (LstS xs, (Idx _ (AppP (VarP v) [t]), _))) _)
  = PolyList v t <$> mapM (expressPolyExpr t) xs
expressPolyExpr _ (SAnno (One (LstS _, _)) _) = error "LstS can only be (AppP (VarP _) [_]) type"

-- tuples
expressPolyExpr _ (SAnno (One (TupS xs, (Idx _ (AppP (VarP v) ts), _))) _) = do
  xs' <- fromJust <$> safeZipWithM expressPolyExpr ts xs
  return $ PolyTuple v (fromJust $ safeZip ts xs')
expressPolyExpr _ (SAnno (One (TupS _, _)) _) = error "TupS can only be (TupP (TupP _) ts) type"

-- records
expressPolyExpr _ (SAnno (One (NamS entries, (Idx _ (NamP o v ps rs), _))) _) = do
  xs' <- fromJust <$> safeZipWithM expressPolyExpr (map snd rs) (map snd entries)
  return $ PolyRecord o v ps (zip (map fst rs) (zip (map snd rs) xs'))

-- Unapplied and unexported source
expressPolyExpr _ (SAnno (One (CallS src, _)) _)
  = MM.throwError . OtherError . render
  $ "Cannot export the value" <+> squotes (pretty (srcName src)) <+> "from a pool, you should define this in morloc code instead"

expressPolyExpr _ (SAnno (One (AppS (SAnno (One (VarS f, _)) _) _, _)) _)
  = MM.throwError . ConcreteTypeError $ FunctionSerialization f

-- catch all exception case - not very classy
expressPolyExpr _ (SAnno (One (e, (Idx _ t, _))) m) = do
  MM.sayVVV "Bad case"
  MM.sayVVV $ "  t :: " <> pretty t
  peak e
  name' <- MM.metaName m
  case name' of
      (Just v) -> MM.throwError . ConcreteTypeError $ MissingConcreteSignature v (langOf' t)
      Nothing ->  error "Bug in expressPolyExpr - this should be unreachable"


unvalue :: Arg a -> Arg None
unvalue (Arg i _) = Arg i None

bindVar :: [Arg a] -> [TypeP] -> [PolyExpr]
bindVar args = bindVarIds (map ann args)

bindVarIds :: [Int] -> [TypeP] -> [PolyExpr]
bindVarIds [] [] = []
bindVarIds (i : args) (t : types) = PolyBndVar (Right t) i : bindVarIds args types
-- These error states indicate a bug in the compiler, not the user code, so no mercy
bindVarIds [] ts = error $ "bindVarIds: too few arguments: " <> show ts
bindVarIds _ [] = error "bindVarIds: too few types"



segment :: PolyHead -> MorlocMonad [MonoHead]
segment (PolyHead m0 args0 e0) = do
  (heads, (lang, topExpr)) <- segmentExpr m0 (map ann args0) e0
  return (MonoHead lang m0 args0 topExpr : heads)

segmentExpr
  :: Int -- manifold index
  -> [Int] -- argument indices
  -> PolyExpr
  -> MorlocMonad ([MonoHead], (Lang, MonoExpr))

-- This is where segmentation happens, every other match is just traversal
segmentExpr _ args (PolyForeignInterface callingType _ e@(PolyManifold m (ManifoldFull foreignArgs) _)) = do
  -- MM.sayVVV $ "segmenting foreign interface" <+> pretty m
  (ms, (foreignLang, e')) <- segmentExpr m (map ann foreignArgs) e
  let foreignHead = MonoHead foreignLang m foreignArgs e'
  config <- MM.ask
  callingTypeF <- typeP2typeFSafe callingType
  case MC.buildPoolCallBase config (Just foreignLang) m of
    (Just cmds) -> return (foreignHead:ms, (langOf' callingType, MonoPoolCall callingTypeF m cmds [Arg i None | i <- args]))
    Nothing -> MM.throwError . OtherError $ "Unsupported language: " <> MT.show' foreignLang

segmentExpr m _ (PolyForeignInterface callingType args e) = do
  (ms, (foreignLang, e')) <- segmentExpr m args e
  -- create the foreign manifold, make sure all arugments are packed
  let foreignHead = MonoHead foreignLang m [Arg i None | i <- args] (MonoReturn e')
      -- pack the arguments that will be passed to the foreign manifold
      es' = map (MonoBndVar Nothing) args
  config <- MM.ask
  callingTypeF <- typeP2typeFSafe callingType
  -- create the body of the local helper function
  localFun <- case MC.buildPoolCallBase config (Just foreignLang) m of
    (Just cmds) -> return $ MonoApp (MonoPoolCall callingTypeF m cmds [Arg i None | i <- args]) es'
    Nothing -> MM.throwError . OtherError $ "Unsupported language: " <> MT.show' foreignLang
  return (foreignHead:ms, (langOf' callingType, localFun))

segmentExpr _ _ (PolyManifold m form e) = do
  (ms, (lang, e')) <- segmentExpr m (abilist const const form) e
  return (ms, (lang, MonoManifold m form e'))

segmentExpr m args (PolyApp e es) = do
  (ms, (lang, e')) <- segmentExpr m args e
  (mss, es') <- mapM (segmentExpr m args) es |>> unzip
  return (ms ++ concat mss, (lang, MonoApp e' (map snd es')))

segmentExpr m args (PolyLet i e1 e2) = do
  (ms1, (_, e1')) <- segmentExpr m args e1
  (ms2, (lang, e2')) <- segmentExpr m args e2
  return (ms1 ++ ms2, (lang, MonoLet i e1' e2'))

segmentExpr m args (PolyAcc t o v e k) = do
  t' <- typeP2typeFSafe t
  v' <- pvar2fvarSafe v
  (ms, (_, e')) <- segmentExpr m args e
  return (ms, (langOf' v, MonoAcc t' o v' e' k))

segmentExpr m args (PolyList v t es) = do
  v' <- pvar2fvarSafe v
  t' <- typeP2typeFSafe t
  (mss, es') <- mapM (segmentExpr m args) es |>> unzip
  return (concat mss, (langOf' v, MonoList v' t' (map snd es')))

segmentExpr m args (PolyTuple v es) = do
  v' <- pvar2fvarSafe v
  ts' <- mapM (typeP2typeFSafe . fst) es
  (mss, es') <- mapM (segmentExpr m args . snd) es |>> unzip
  return (concat mss, (langOf' v, MonoTuple v' (zip ts' (map snd es'))))

segmentExpr m args (PolyRecord o v ps entries) = do
  v' <- pvar2fvarSafe v
  ps' <- mapM typeP2typeFSafe ps
  entryTypes <- mapM (typeP2typeFSafe . fst . snd) entries
  (mss, es') <- mapM (segmentExpr m args . snd . snd) entries |>> unzip
  keys <- mapM (pvar2fvarSafe . fst) entries
  return (concat mss, (langOf' v, MonoRecord o v' ps' (zip keys (zip entryTypes (map snd es')))))

segmentExpr m args (PolyReturn e) = do
  (ms, (lang, e')) <- segmentExpr m args e
  return (ms, (lang, MonoReturn e'))

segmentExpr _ _ (PolyLetVar t x) = do
    e' <- MonoLetVar <$> typeP2typeFSafe t <*> pure x
    return ([], (langOf' t, e'))
segmentExpr _ _ (PolyBndVar (Right t) i)   = do
    t' <- typeP2typeFSafe t
    return ([], (langOf' t, MonoBndVar (Just t') i))
segmentExpr _ _ (PolyBndVar (Left lang) i) = return ([], (lang, MonoBndVar Nothing i))
segmentExpr _ _ (PolySrc    t src) = do
    e' <- MonoSrc <$> typeP2typeFSafe t <*> pure src
    return ([], (langOf' t, e'))
segmentExpr _ _ (PolyLog v x)   = do
    e' <- MonoLog <$> pvar2fvarSafe v <*> pure x
    return ([], (langOf' v, e'))
segmentExpr _ _ (PolyReal v x) = do
    v' <- pvar2fvarSafe v
    return ([], (langOf' v, MonoReal v' x))
segmentExpr _ _ (PolyInt v x) = do
    v' <- pvar2fvarSafe v
    return ([], (langOf' v, MonoInt v' x))
segmentExpr _ _ (PolyStr v x) = do
    v' <- pvar2fvarSafe v
    return ([], (langOf' v, MonoStr v' x))
segmentExpr _ _ (PolyNull v) = do
    v' <- pvar2fvarSafe v
    return ([], (langOf' v, MonoNull v'))


pvar2fvarSafe :: PVar -> MorlocMonad FVar
pvar2fvarSafe (PV _ (Just g) c) = return $ FV g c
pvar2fvarSafe (PV _ Nothing _) = MM.throwError . OtherError $ "Missing general type"

typeP2typeFSafe :: TypeP -> MorlocMonad TypeF
typeP2typeFSafe (UnkP v) = UnkF <$> pvar2fvarSafe v
typeP2typeFSafe (VarP v) = VarF <$> pvar2fvarSafe v
typeP2typeFSafe (FunP ts t) = FunF <$> mapM typeP2typeFSafe ts <*> typeP2typeFSafe t
typeP2typeFSafe (AppP t ts) = AppF <$> typeP2typeFSafe t <*> mapM typeP2typeFSafe ts
typeP2typeFSafe (NamP o v ds rs) = do
    keys' <- mapM (pvar2fvarSafe . fst) rs
    vals' <- mapM (typeP2typeFSafe . snd) rs
    ds' <- mapM typeP2typeFSafe ds
    v' <- pvar2fvarSafe v
    return $ NamF o v' ds' (zip keys' vals')


serialize :: [MonoHead] -> MorlocMonad [SerialManifold]
serialize heads = do
    packerSets <- makePackerSets heads
    concat <$> mapM (\(xs, p) -> mapM (serializeOne p) xs) packerSets

makePackerSets :: [MonoHead] -> MorlocMonad [([MonoHead], Map.Map MT.Text [ResolvedPacker])]
makePackerSets mheads = do
  let groupedHeads = groupSort $ [(lang, mhead) | mhead@(MonoHead lang _ _ _) <- mheads]
  packers <- mapM (resolvePackers . snd) groupedHeads
  return (zip (map snd groupedHeads) packers)

-- | This is a very naive solution to choosing a source function for a given
-- (un)packer. It will only work reliable when there is only one source to
-- choose from. In other cases, the selection will arbitrarily be the first in
-- the source list.
resolvePackers :: [MonoHead] -> MorlocMonad (Map.Map MT.Text [ResolvedPacker])
resolvePackers mheads =
  collectUnresolvedPackers mheads >>= mapM (mapM resolvePacker)
  where
    resolvePacker :: UnresolvedPacker -> MorlocMonad ResolvedPacker
    resolvePacker up = do
      forwardPacker <- case unresolvedPackerForward up of
        [] -> MM.throwError . SerializationError $ "Missing forward packer"
        (fsrc:_) -> return fsrc
      reversePacker <- case unresolvedPackerReverse up of
        [] -> MM.throwError . SerializationError $ "Missing reverse packer"
        (fsrc:_) -> return fsrc
      return $ ResolvedPacker
          { resolvedPackerTerm = unresolvedPackerTerm up
          , resolvedPackedType = unresolvedPackedType up
          , resolvedUnpackedType = unresolvedUnpackedType up
          , resolvedPackerForward = forwardPacker
          , resolvedPackerReverse = reversePacker
          , resolvedPackerGeneralTypes = unresolvedPackerGeneralTypes up
          }

collectUnresolvedPackers :: [MonoHead] -> MorlocMonad (Map.Map MT.Text [UnresolvedPacker])
collectUnresolvedPackers mheads = do
  packmap <- mapM unresolvedPackers mheads |>> Map.unionsWith Set.union
  return $ Map.map Set.toList packmap
  where
    unresolvedPackers :: MonoHead -> MorlocMonad (Map.Map MT.Text (Set.Set UnresolvedPacker))
    unresolvedPackers (MonoHead _ _ _ e0) = f e0

    f :: MonoExpr -> MorlocMonad (Map.Map MT.Text (Set.Set UnresolvedPacker))
    f (MonoManifold m _ e) = do
      packmap <- MM.metaPackMap m |>> Map.mapKeys (\(TV _ v) -> v) |>> Map.map Set.fromList
      childPackmap <- f e
      return (Map.unionWith Set.union packmap childPackmap)
    f (MonoLet _ e1 e2) = Map.unionWith Set.union <$> f e1 <*> f e2
    f (MonoReturn e) = f e
    f (MonoApp e es) = Map.unionsWith Set.union <$> mapM f (e:es)
    f (MonoAcc _ _ _ e _) = f e
    f (MonoList _ _ es) = Map.unionsWith Set.union <$> mapM f es
    f (MonoTuple _ rs) = Map.unionsWith Set.union <$> mapM (f . snd) rs
    f (MonoRecord _ _ _ rs) = Map.unionsWith Set.union <$> mapM (f . snd . snd) rs
    f _ = return Map.empty


-- | This step is performed after segmentation, so all terms are in the same
-- language. Here we need to determine where inputs are (de)serialized and the
-- serialization states of arguments and variables.
serializeOne :: Map.Map MT.Text [ResolvedPacker] -> MonoHead -> MorlocMonad SerialManifold
serializeOne packmap (MonoHead lang m0 args0 e0) = do
  let form0 = ManifoldFull [Arg i (L . typeSof $ Map.lookup i typemap) | (Arg i _) <- args0]
  se1 <- serialExpr e0
  let sm = SerialManifold m0 lang form0 se1
  wireSerial lang packmap sm
  where

  -- map of argument indices to native types
  typemap = makeTypemap e0

  contextArg :: Int -> Or TypeS TypeF
  contextArg i = case Map.lookup i typemap of
    (Just t) -> LR (typeSof t) t
    Nothing -> L PassthroughS

  boundArg :: Int -> TypeF
  boundArg i = case Map.lookup i typemap of
    (Just t) -> t
    Nothing -> error "Untyped native arg"


  serialExpr :: MonoExpr -> MorlocMonad SerialExpr
  serialExpr (MonoManifold _ _ e) = serialExpr e
  serialExpr (MonoLet i e1 e2) = case inferState e1 of
    Serialized -> SerialLetS i <$> serialExpr e1 <*> serialExpr e2
    Unserialized -> do
      ne1 <- nativeExpr e1
      NativeLetS i ne1 <$> serialExpr e2
  serialExpr (MonoLetVar t i) = return (LetVarS (Just t) i)
  serialExpr (MonoReturn e) = ReturnS <$> serialExpr e
  serialExpr (MonoApp (MonoPoolCall t m docs contextArgs) es) = do
    let contextArgs' = map (typeArg Serialized . ann) contextArgs
        poolCall' = PoolCall m docs contextArgs'
    es' <- mapM serialArg es
    return $ AppPoolS t poolCall' es'
  serialExpr (MonoBndVar t i) = return (BndVarS t i)
  -- failing cases that should be unreachable
  serialExpr (MonoSrc _ _) = error "Can represent MonoSrc as SerialExpr"
  serialExpr MonoPoolCall{} = error "MonoPoolCall does not map to a SerialExpr"
  serialExpr (MonoApp MonoManifold{} _) = error "Illegal?"
  -- the following are all native types that need to be directly serialized
  serialExpr e = nativeExpr e >>= serializeS

  serialArg :: MonoExpr -> MorlocMonad SerialArg
  serialArg e@MonoManifold{} = do
    se <- serialExpr e
    case se of
      (ManS sm) -> return $ SerialArgManifold sm
      _ -> error "Unreachable?"
  -- Pool and source calls should have previously been wrapped in manifolds
  serialArg MonoPoolCall{} = error "This step should be unreachable"
  serialArg (MonoSrc    _ _) = error "This step should be unreachable"
  serialArg (MonoReturn _) = error "Return should not happen hear (really I should remove this term completely)"
  serialArg e = SerialArgExpr <$> serialExpr e

  nativeArg :: MonoExpr -> MorlocMonad NativeArg
  -- This case may occur, for example, with `(add 1.0 2.0)`. Here `add` has two
  -- native arguments, but the manifold that wraps it will have no
  -- arguments. This is because `1.0` and `2.0` are primitive and will be
  -- generated in place rather than passed as arguments.
  nativeArg e@(MonoManifold m form _) = do
    MM.say $ "nativeArg MonoManifold" <+> pretty m
    MM.say $ "  form:" <+> viaShow form
    ne <- nativeExpr e
    case ne of
      (ManN nm) -> return $ NativeArgManifold nm
      _ -> error "Unreachable?"
  -- Pool and source calls should have previously been wrapped in manifolds
  nativeArg MonoPoolCall{} = error "This step should be unreachable"
  nativeArg (MonoSrc    _ _) = error "This step should be unreachable"
  nativeArg (MonoReturn _) = error "Return should not happen here (really I should remove this term completely)"
  nativeArg e = NativeArgExpr <$> nativeExpr e

  nativeExpr
    :: MonoExpr
    -> MorlocMonad NativeExpr
  nativeExpr (MonoManifold m form e) = do
    ne <- nativeExpr e
    let form' = abimap (\i _ -> contextArg i) (\i _ -> boundArg i) form
    return . ManN $ NativeManifold m lang form' ne

  nativeExpr MonoPoolCall{} = error "MonoPoolCall does not map to NativeExpr"
  nativeExpr (MonoLet i e1 e2) = case inferState e1 of
    Serialized -> do
      ne2 <- nativeExpr e2
      SerialLetN i <$> serialExpr e1 <*> pure ne2
    Unserialized -> do
      ne1 <- nativeExpr e1
      ne2 <- nativeExpr e2
      return $ NativeLetN i ne1 ne2
  nativeExpr (MonoLetVar t i) = return (LetVarN t i)
  nativeExpr (MonoReturn e) = do
    ne <- nativeExpr e
    return (ReturnN ne)
  nativeExpr (MonoApp (MonoSrc (FunF inputTypes outputType) src) es) = do
    MM.say $ "nativeExpr MonoApp" <+> pretty (srcName src)
    args <- mapM nativeArg es
    appType <- case drop (length es) inputTypes of
        [] -> return outputType
        remaining -> return $ FunF remaining outputType
    return $ AppSrcN appType src args
  nativeExpr e@(MonoApp (MonoPoolCall t _ _ _) _) = serialExpr e >>= naturalizeN t
  nativeExpr (MonoApp _ _) = error "Illegal application"
  nativeExpr (MonoSrc t src) = return (SrcN t src)
  nativeExpr (MonoBndVar (Just t) i) = return (BndVarN t i)
  nativeExpr (MonoBndVar Nothing _) = error "MonoBndVar must have a type if used in native context"
  -- simple native types
  nativeExpr (MonoAcc _ o v e k) = AccN o v <$> nativeExpr e <*> pure k
  nativeExpr (MonoList v t es) = ListN v t <$> mapM nativeExpr es
  nativeExpr (MonoTuple v rs) = TupleN v <$> mapM (nativeExpr . snd) rs
  nativeExpr (MonoRecord o v ps rs) = do
    let keys = map fst rs
    vals <- mapM (nativeExpr . snd . snd) rs
    let rs' = zip keys vals
    return $ RecordN o v ps rs'
  -- primitives
  nativeExpr (MonoLog    v x) = return (LogN v x)
  nativeExpr (MonoReal   v x) = return (RealN v x)
  nativeExpr (MonoInt    v x) = return (IntN v x)
  nativeExpr (MonoStr    v x) = return (StrN v x)
  nativeExpr (MonoNull   v) = return (NullN v)

  typeArg :: SerializationState -> Int -> Arg TypeM
  typeArg s i = case (s, Map.lookup i typemap) of
    (Serialized, Just t) -> Arg i (Serial t)
    (Serialized, Nothing) -> Arg i Passthrough
    (Unserialized, Just t) -> Arg i (Native t)
    (Unserialized, Nothing) -> error "Bug: untyped non-passthrough value"

  makeTypemap :: MonoExpr -> Map.Map Int TypeF
  -- map variables used in this segment to their types
  makeTypemap (MonoLetVar t i) = Map.singleton i t
  makeTypemap (MonoBndVar (Just t) i) = Map.singleton i t
  -- recursive calls
  makeTypemap (MonoManifold _ (manifoldBound -> ys) e) =
    -- bound arguments may not be used, but they where passed in from the
    -- source function, so they cannot be removed.
    Map.union (Map.fromList [(i, t) | (Arg i (Just t)) <- ys]) (makeTypemap e)
  makeTypemap (MonoLet _ e1 e2) = Map.union (makeTypemap e1) (makeTypemap e2)
  makeTypemap (MonoReturn e) = makeTypemap e
  makeTypemap (MonoApp e es) = Map.unions (map makeTypemap (e:es))
  makeTypemap (MonoAcc _ _ _ e _) = makeTypemap e
  makeTypemap (MonoList _ _ es) = Map.unions (map makeTypemap es)
  makeTypemap (MonoTuple _ (map snd -> es)) = Map.unions (map makeTypemap es)
  makeTypemap (MonoRecord _ _ _ (map (snd . snd) -> es)) = Map.unions (map makeTypemap es)
  makeTypemap _ = Map.empty

  -- make (SerialAST One) then convert to serializeMany
  serializeS :: NativeExpr -> MorlocMonad SerialExpr
  serializeS se = case runExcept (SerializeS <$> Serial.makeSerialAST packmap lang (typeFof se) <*> pure se) of
    (Left serr) -> MM.throwError . SerializationError . render $ serr
    (Right ne) -> return ne

  naturalizeN :: TypeF -> SerialExpr -> MorlocMonad NativeExpr
  naturalizeN t se = case runExcept (DeserializeN t <$> Serial.makeSerialAST packmap lang t <*> pure se) of
    (Left serr) -> MM.throwError . SerializationError . render $ serr
    (Right ne) -> return ne

  -- infer the preferred serialization state for an expression.
  inferState :: MonoExpr -> SerializationState
  inferState (MonoApp MonoPoolCall{} _) = Serialized
  inferState (MonoApp MonoSrc{} _) = Unserialized
  inferState (MonoApp (MonoManifold _ _ e) _) = inferState e
  inferState (MonoLet _ _ e) = inferState e
  inferState (MonoReturn e) = inferState e
  inferState (MonoManifold _ _ e) = inferState e
  inferState MonoPoolCall{} = Unserialized
  inferState MonoBndVar{} = error "Ambiguous bound term"
  inferState _ = Unserialized

type (D a) = (Map.Map Int Request, a)

wireSerial :: Lang -> Map.Map MT.Text [ResolvedPacker] -> SerialManifold -> MorlocMonad SerialManifold
wireSerial lang packmap sm0 = foldSerialManifoldM fm sm0 |>> snd
  where
  defs = makeMonoidFoldDefault Map.empty (Map.unionWith (<>))

  fm = FoldManifoldM
    { opSerialManifoldM = wireSerialManifold
    , opNativeManifoldM = wireNativeManifold
    , opSerialExprM = wireSerialExpr
    , opNativeExprM = wireNativeExpr
    , opSerialArgM = monoidSerialArg defs
    , opNativeArgM = monoidNativeArg defs
    }

  wireSerialManifold :: SerialManifold_ (D SerialExpr) -> MorlocMonad (D SerialManifold)
  wireSerialManifold (SerialManifold_ m _ form (req, e)) = do
    let e' = letWrapS form req e
        form' = afirst (specialize req) form
        req' = Map.map fst (manifoldToMap form')
    return (req', SerialManifold m lang form' e')

  wireNativeManifold :: NativeManifold_ (D NativeExpr) -> MorlocMonad (D NativeManifold)
  wireNativeManifold (NativeManifold_ m _ form (req, e)) = do
    let e' = letWrapN form req e
        form' = afirst (specialize req) form
        req' = Map.map fst (manifoldToMap form')
    return (req', NativeManifold m lang form' e')

  wireSerialExpr (LetVarS_ t i) = return (Map.singleton i SerialContent, LetVarS t i)
  wireSerialExpr (BndVarS_ t i) = return (Map.singleton i SerialContent, BndVarS t i)
  wireSerialExpr e = monoidSerialExpr defs e

  wireNativeExpr :: NativeExpr_ (D NativeManifold) (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg) -> MorlocMonad (D NativeExpr)
  wireNativeExpr (LetVarN_ t i) = return (Map.singleton i NativeContent, LetVarN t i)
  wireNativeExpr (BndVarN_ t i) = return (Map.singleton i NativeContent, BndVarN t i)
  wireNativeExpr e = monoidNativeExpr defs e

  specialize :: Map.Map Int Request -> Int -> Or TypeS TypeF -> Or TypeS TypeF
  specialize req i r = case (Map.lookup i req, r) of
    (Nothing, _) -> L PassthroughS
    (Just SerialContent, LR t _) -> L t
    (Just NativeContent, LR _ t) -> R t
    _ -> r

  letWrapN :: ManifoldForm (Or TypeS TypeF) TypeF -> Map.Map Int Request -> NativeExpr -> NativeExpr
  letWrapN form0 req0 ne0 = foldl wrapAsNeeded ne0 (Map.toList req0) where

    formMap = manifoldToMap form0

    wrapAsNeeded :: NativeExpr -> (Int, Request) -> NativeExpr
    wrapAsNeeded ne (i, req) = case (req, Map.lookup i formMap) of
      (SerialContent, Just (NativeContent, Just t)) -> SerialLetN i (serializeS t (BndVarN t i)) ne
      (NativeAndSerialContent, Just (NativeContent, Just t)) -> SerialLetN i (serializeS t (BndVarN t i)) ne
      (NativeContent, Just (SerialContent, Just t)) -> NativeLetN i (naturalizeN t (BndVarS (Just t) i)) ne
      (NativeAndSerialContent, Just (SerialContent, Just t)) -> NativeLetN i (naturalizeN t (BndVarS (Just t) i)) ne
      _ -> ne

  letWrapS :: ManifoldForm (Or TypeS TypeF) TypeS -> Map.Map Int Request -> SerialExpr -> SerialExpr
  letWrapS form0 req0 se0 = foldl wrapAsNeeded se0 (Map.toList req0) where

    formMap = manifoldToMap form0

    wrapAsNeeded :: SerialExpr -> (Int, Request) -> SerialExpr
    wrapAsNeeded se (i, req) = case (req, Map.lookup i formMap) of
      (SerialContent, Just (NativeContent, Just t)) -> SerialLetS i (serializeS t (BndVarN t i)) se
      (NativeAndSerialContent, Just (NativeContent, Just t)) -> SerialLetS i (serializeS t (BndVarN t i)) se
      (NativeContent, Just (SerialContent, Just t)) -> NativeLetS i (naturalizeN t (BndVarS (Just t) i)) se
      (NativeAndSerialContent, Just (SerialContent, Just t)) -> NativeLetS i (naturalizeN t (BndVarS (Just t) i)) se
      _ -> se

  manifoldToMap :: (HasRequest t, MayHaveTypeF t) => ManifoldForm (Or TypeS TypeF) t -> Map.Map Int (Request, Maybe TypeF)
  manifoldToMap form = f form where
    mapRequestFromXs xs = Map.fromList [(i, (requestOf t, mayHaveTypeF t)) | (Arg i t) <- typeMofRs xs]
    mapRequestFromYs ys = Map.fromList [(i, (requestOf t, mayHaveTypeF t)) | (Arg i t) <- ys]

    f (ManifoldFull xs) = mapRequestFromXs xs
    f (ManifoldPass ys) = mapRequestFromYs ys
    f (ManifoldPart xs ys) = Map.union (mapRequestFromXs xs) (mapRequestFromYs ys)

  naturalizeN :: TypeF -> SerialExpr -> NativeExpr
  naturalizeN t se = case runExcept (DeserializeN t <$> Serial.makeSerialAST packmap lang t <*> pure se) of
    (Left serr) -> error $ show serr
    (Right x) -> x

  serializeS :: TypeF -> NativeExpr -> SerialExpr
  serializeS t se = case runExcept (SerializeS <$> Serial.makeSerialAST packmap lang t <*> pure se) of
    (Left serr) -> error $ "for language " <> show lang <> " and type " <> show t <> " found error:" <> show serr
    (Right x) -> x


data Request = SerialContent | NativeContent | NativeAndSerialContent
  deriving(Ord, Eq, Show)

class HasRequest a where
  requestOf :: a -> Request

instance HasRequest TypeM where
  requestOf Passthrough = SerialContent
  requestOf (Serial _) = SerialContent
  requestOf (Native _) = NativeContent
  requestOf (Function _ _) = NativeContent

instance HasRequest SerialExpr where
  requestOf _ = SerialContent

instance HasRequest NativeExpr where
  requestOf _ = NativeContent

instance HasRequest SerialArg where
  requestOf _ = SerialContent

instance HasRequest NativeArg where
  requestOf _ = NativeContent

instance HasRequest TypeS where
  requestOf _ = SerialContent

instance HasRequest TypeF where
  requestOf _ = NativeContent


instance Semigroup Request where
 SerialContent <> SerialContent = SerialContent
 NativeContent <> NativeContent = NativeContent
 _ <> _ = NativeAndSerialContent

data SerializationState = Serialized | Unserialized
  deriving(Show, Eq, Ord)

class HasSerializationState a where
  isSerialized :: a -> SerializationState

instance HasSerializationState SerialExpr where
  isSerialized _ = Serialized

instance HasSerializationState SerialManifold where
  isSerialized _ = Serialized

instance HasSerializationState NativeExpr where
  isSerialized _ = Unserialized

instance HasSerializationState NativeManifold where
  isSerialized _ = Unserialized


-- Sort manifolds into pools. Within pools, group manifolds into call sets.
pool :: [SerialManifold] -> [(Lang, [SerialManifold])]
pool es =
    -- [SerialManifold] --> [(Lang, [(Int, SerialManifold)])]
    let (langs, indexedSegments) = unzip . groupSort . map (\x@(SerialManifold i lang _ _) -> (lang, (i, x))) $ es 
        {-
        Each of the `SerialManifold` values is represents a single subtree of the
        program and may thus contain many nested manifolds. Each is thus the root
        of a tree. If two of these trees share a same root and language, then they
        should contain the children. So here we prune the duplicate trees.
        -}
        uniqueSegments = map (Map.elems . Map.fromList) indexedSegments
    in zip langs uniqueSegments

encode
  :: Lang
  -> [SerialManifold]
  -- ^ The input preserves the connection between the AST and the specific
  -- sources it uses, currently this information is not used. However, in the
  -- future it may sometimes be necessary to split the functions in one
  -- language into multiple pools (e.g., to resolve version conflicts).
  -> MorlocMonad Script
encode lang xs = do
  srcs' <- findSources xs
  xs' <- mapM (preprocess lang) xs
  -- translate each node in the AST to code
  translate lang srcs' xs'

findSources :: [SerialManifold] -> MorlocMonad [Source]
findSources ms = unique <$> concatMapM (foldSerialManifoldM fm) ms
  where
  fm = defaultValue
    { opSerialExprM = serialExprSrcs
    , opNativeExprM = nativeExprSrcs
    , opNativeManifoldM = nativeManifoldSrcs
    , opSerialManifoldM = nativeSerialSrcs
    }
  
  nativeExprSrcs (AppSrcN_ _ src xss) = return (src : concat xss)
  nativeExprSrcs (SrcN_ _ src) = return [src]
  nativeExprSrcs (DeserializeN_ _ s xs) = return $ serialASTsources s <> xs
  nativeExprSrcs e = return $ foldlNE (<>) [] e

  serialExprSrcs (SerializeS_ s xs) = return $ serialASTsources s <> xs
  serialExprSrcs e = return $ foldlSE (<>) [] e

  -- Collect sources for all type (un)packers that are used in serialization
  serialASTsources :: SerialAST -> [Source]
  serialASTsources (SerialPack _ (p, s)) = [ typePackerForward p, typePackerReverse p ] <> serialASTsources s
  serialASTsources (SerialList _ s) = serialASTsources s
  serialASTsources (SerialTuple _ ss) = concatMap serialASTsources ss
  serialASTsources (SerialObject _ _ _ (map snd -> ss)) = concatMap serialASTsources ss
  serialASTsources _ = []

  nativeManifoldSrcs (NativeManifold_ m lang _ e) = (<>) e <$> lookupConstructors lang m
  nativeSerialSrcs (SerialManifold_ m lang _ e) = (<>) e <$> lookupConstructors lang m

  -- Find object constructors that are NOT defined (un)pack functions
  -- These are object constructors imported from the concrete sources that are
  -- used as concrete types. For example:
  --   source py from "person.py" ("PersonObj")
  --   table (Person a) = Person {name :: Str, info :: a}
  --   table py (Person a) = "PersonObj" {name :: "str", info :: a}
  lookupConstructors :: Lang -> Int -> MorlocMonad [Source]
  lookupConstructors lang i = MM.metaSources i |>> filter ((==) lang . srcLang)


translate :: Lang -> [Source] -> [SerialManifold] -> MorlocMonad Script
translate lang srcs es = do
  case lang of
    CppLang -> Cpp.translate srcs es
    RustLang -> Rust.translate srcs es
    RLang -> R.translate srcs es
    Python3Lang -> Python3.translate srcs es
    x -> MM.throwError . PoolBuildError . render
      $ "Language '" <> viaShow x <> "' has no translator"

preprocess :: Lang -> SerialManifold -> MorlocMonad SerialManifold
preprocess CppLang es = Cpp.preprocess es
preprocess RustLang es = Rust.preprocess es
preprocess RLang es = R.preprocess es
preprocess Python3Lang es = Python3.preprocess es
preprocess l _ = MM.throwError . PoolBuildError . render
               $ "Language '" <> viaShow l <> "' has no translator"

mapCM :: (c -> MorlocMonad c') -> SAnno g One c -> MorlocMonad (SAnno g One c')
mapCM f (SAnno (One (AccS x k, c)) g) = do
  x' <- mapCM f x
  c' <- f c
  return $ SAnno (One (AccS x' k, c')) g
mapCM f (SAnno (One (LstS xs, c)) g) = do
  xs' <- mapM (mapCM f) xs
  c' <- f c
  return $ SAnno (One (LstS xs', c')) g
mapCM f (SAnno (One (TupS xs, c)) g) = do
  xs' <- mapM (mapCM f) xs
  c' <- f c
  return $ SAnno (One (TupS xs', c')) g
mapCM f (SAnno (One (NamS entries, c)) g) = do
  xs' <- mapM (mapCM f . snd) entries
  c' <- f c
  return $ SAnno (One (NamS (zip (map fst entries) xs'), c')) g
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
mapCM f (SAnno (One (RealS x, c)) g) = do
  c' <- f c
  return $ SAnno (One (RealS x, c')) g
mapCM f (SAnno (One (IntS x, c)) g) = do
  c' <- f c
  return $ SAnno (One (IntS x, c')) g
mapCM f (SAnno (One (LogS x, c)) g) = do
  c' <- f c
  return $ SAnno (One (LogS x, c')) g
mapCM f (SAnno (One (StrS x, c)) g) = do
  c' <- f c
  return $ SAnno (One (StrS x, c')) g

sannoSnd :: SAnno g One (a, b) -> b
sannoSnd (SAnno (One (_, (_, x))) _) = x

-- generate infinite list of fresh variables of form
-- ['a','b',...,'z','aa','ab',...,'zz',...]
freshVarsAZ
  :: [MT.Text] -- variables to exclude
  -> [MT.Text]
freshVarsAZ exclude =
  filter
    (`notElem` exclude)
    ([1 ..] >>= flip replicateM ['a' .. 'z'] |>> MT.pack)
