{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Generate
Description : Translate AST forests into target language source code
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Generate
(
    realityCheck
  , generate
  , generatePools
) where

import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Data.Map as Map
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as Lang
import qualified Morloc.Monad as MM
import qualified Morloc.CodeGenerator.Nexus as Nexus
import Morloc.CodeGenerator.Infer

import qualified Morloc.CodeGenerator.Grammars.Translator.Cpp as Cpp
import qualified Morloc.CodeGenerator.Grammars.Translator.R as R
import qualified Morloc.CodeGenerator.Grammars.Translator.Python3 as Python3
import qualified Morloc.CodeGenerator.Serial as Serial


realityCheck
  :: [AnnoS (Indexed Type) Many Int]
  -- ^ one AST forest for each command exported from main
  -> MorlocMonad ( [AnnoS (Indexed Type) One ()]
                 , [AnnoS (Indexed Type) One (Indexed Lang)]
                 )
realityCheck es = do

  -- translate modules into bitrees
  (gASTs, rASTs)
    -- select a single instance at each node in the tree
    <- mapM realize es
    -- separate unrealized (general) ASTs (uASTs) from realized ASTs (rASTs)
    |>> partitionEithers

  return (gASTs, rASTs)

-- | Translate typed, abstract syntax forests into compilable code
generate
  :: [AnnoS (Indexed Type) One ()]
  -> [AnnoS (Indexed Type) One (Indexed Lang)]
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
    [(t, i, lang) | (AnnoS (Idx i t) (Idx _ lang) _) <- rASTs]


  -- initialize counter for use in express
  MM.startCounter

  -- for each language, collect all functions into one "pool"
  pools <- generatePools rASTs >>= mapM (uncurry encode)

  return (nexus, pools)


-- | Do everything except language specific code generation.
generatePools :: [AnnoS (Indexed Type) One (Indexed Lang)] -> MorlocMonad [(Lang, [SerialManifold])]
generatePools rASTs = do
  -- for each language, collect all functions into one "pool"
  mapM applyLambdas rASTs
    -- thread arguments across the tree
    >>= mapM parameterize
    -- convert from AST to manifold tree
    >>= mapM express

    -- Separate the call trees into mono-lingual segments terminated in
    -- primitives or foreign calls.
    >>= mapM segment |>> concat

    >>= mapM serialize

    -- Gather segments into pools, currently this entails gathering all
    -- segments from a given language into one pool. Later it may be more
    -- nuanced.
    |>> pool


-- | Choose a single concrete implementation. In the future, this component
-- may be one of the more complex components of the morloc compiler. It will
-- probably need to be implemented using an optimizing SMT solver. It will
-- also need benchmarking data from all the implementations and possibly
-- statistical info describing inputs.
realize
  :: AnnoS (Indexed Type) Many Int
  -> MorlocMonad (Either (AnnoS (Indexed Type) One ())
                         (AnnoS (Indexed Type) One (Indexed Lang)))
realize s0 = do
  e@(AnnoS _ li _) <- scoreAnnoS [] s0 >>= collapseAnnoS Nothing |>> removeVarS
  case li of
    (Idx _ Nothing) -> makeGAST e |>> Left
    (Idx _ _) -> Right <$> propagateDown e
  where

  -- | Depth first pass calculating scores for each language. Alternates with
  -- scoresSExpr.
  --
  scoreAnnoS
    :: [Lang]
    -> AnnoS (Indexed Type) Many Int
    -> MorlocMonad (AnnoS (Indexed Type) Many (Indexed [(Lang, Int)]))
  scoreAnnoS langs (AnnoS gi ci e) = do
    (e', ci') <- scoreExpr langs (e, ci)
    return $ AnnoS gi ci' e'

  -- | Alternates with scoresAnnoS, finds the best score for each language at
  -- application nodes.
  scoreExpr
    :: [Lang]
    -> (ExprS (Indexed Type) Many Int, Int)
    -> MorlocMonad (ExprS (Indexed Type) Many (Indexed [(Lang, Int)]), Indexed [(Lang, Int)])
  scoreExpr langs (AccS k x, i) = do
    x' <- scoreAnnoS langs x
    return (AccS k x', Idx i (scoresOf x'))
  scoreExpr langs (LstS xs, i) = do
    (xs', best) <- scoreMany langs xs
    return (LstS xs', Idx i best)
  scoreExpr langs (TupS xs, i) = do
    (xs', best) <- scoreMany langs xs
    return (TupS xs', Idx i best)
  scoreExpr langs (LamS vs x, i) = do
    MM.sayVVV $ "scoreExpr LamS"
              <> "\n  langs:" <+> pretty langs
              <> "\n  vs:" <+> pretty vs
              <> "\n  i:" <+> pretty i
    x' <- scoreAnnoS langs x
    return (LamS vs x', Idx i (scoresOf x'))
  scoreExpr _ (AppS f xs, i) = do
    MM.sayVVV $ "scoreExpr AppS"
              <> "\n  i" <> pretty i
    f' <- scoreAnnoS [] f

    -- best scores for each language for f
    let scores = scoresOf f'

    xs' <- mapM (scoreAnnoS (unique $ map fst scores)) xs

    -- [[(Lang, Int)]] : where Lang is unique within each list and Int is minimized
    let pairss = [minPairs pairs | AnnoS _ (Idx _ pairs) _ <- xs']

        {- find the best score for each language supported by function f

           Below is the cost function where
            l1: the language of the ith calling function implementation
            s1: the score of the ith implementation
            l2: the language of the jth implementation of the kth argument
            s2: the score of the jth implementation of the kth argument
        -}
        best = [ (l1, s1 + sum [ minimumDef 999999999 [s2 + Lang.pairwiseCost l1 l2 | (l2, s2) <- pairs]
                               | pairs <- pairss
                               ]
                 )
               | (l1, s1) <- scores
               ]

    return (AppS f' xs', Idx i best)
  scoreExpr langs (NamS rs, i) = do
    (xs, best) <- scoreMany langs (map snd rs)
    return (NamS (zip (map fst rs) xs), Idx i best)
  -- non-recursive expressions
  scoreExpr langs (UniS, i) = return (UniS, zipLang i langs)

  scoreExpr langs (VarS v (Many xs), i) = do
    (xs', best) <- scoreMany langs xs
    return (VarS v (Many xs'), Idx i best)

  scoreExpr _ (CallS src, i) = return (CallS src, Idx i [(srcLang src, callCost src)])
  scoreExpr langs (BndS v, i) = return (BndS v, zipLang i langs)
  scoreExpr langs (RealS x, i) = return (RealS x, zipLang i langs)
  scoreExpr langs (IntS x, i) = return (IntS x, zipLang i langs)
  scoreExpr langs (LogS x, i) = return (LogS x, zipLang i langs)
  scoreExpr langs (StrS x, i) = return (StrS x, zipLang i langs)

  zipLang :: Int -> [Lang] -> Indexed [(Lang, Int)]
  zipLang i langs = Idx i (zip langs (repeat 0))

  scoresOf :: AnnoS a Many (Indexed [(Lang, Int)]) -> [(Lang, Int)]
  scoresOf (AnnoS _ (Idx _ xs) _) = minPairs xs

  -- find the scores of all implementations from all possible language contexts
  scoreMany
    :: [Lang]
    -> [AnnoS (Indexed Type) Many Int]
    -> MorlocMonad ([AnnoS (Indexed Type) Many (Indexed [(Lang, Int)])], [(Lang, Int)])
  scoreMany langs xs0 = do
    xs1 <- mapM (scoreAnnoS langs) xs0
    return (xs1, scoreMany' xs1)
    where
      scoreMany' :: [AnnoS (Indexed Type) Many (Indexed [(Lang, Int)])] -> [(Lang, Int)]
      scoreMany' xs =
        let pairss = [ (minPairs . concat) [xs' | (AnnoS _ (Idx _ xs') _) <- xs] ]
            langs' = unique (langs <> concatMap (map fst) pairss)
        -- Got 10 billion nodes in your AST? I didn't think so, so don't say my sentinal's ugly.
        in [(l1, sum [ minimumDef 999999999 [ score + Lang.pairwiseCost l1 l2
                               | (l2, score) <- pairs]
                     | pairs <- pairss])
           | l1 <- langs']


  collapseAnnoS
    :: Maybe Lang
    -> AnnoS (Indexed Type) Many (Indexed [(Lang, Int)])
    -> MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)))
  collapseAnnoS l1 (AnnoS gi ci e) = do
    (e', ci') <- collapseExpr l1 (e, ci)
    return (AnnoS gi ci' e')

  -- The biased cost adds a slight penalty to changing language.
  -- This penalty is unrelated to the often large penalty of foreign calls.
  -- Rather, the purpose is just to distinguish VarS terms. It is totally
  -- kludgy, a better recursion scheme is needed here.
  biasedCost :: Maybe Lang -> (Lang, Int) -> Int
  biasedCost l1 (l2, s)
    | l1 == Just l2 = cost l1 l2 s
    | otherwise = 1 + cost l1 l2 s

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
    -> (ExprS (Indexed Type) Many (Indexed [(Lang, Int)]), Indexed [(Lang, Int)])
    -> MorlocMonad (ExprS (Indexed Type) One (Indexed (Maybe Lang)), Indexed (Maybe Lang))

  -- This case should be caught earlier
  collapseExpr _ (VarS v (Many []), _)
    = MM.throwError . GeneratorError . render
    $ "No implementation found for" <+> squotes (pretty v)

  -- Select one implementation for the given term
  collapseExpr l1 (VarS v (Many xs), Idx i _) = do
    let mayX = minBy (\(AnnoS _ (Idx _ ss) _) -> minimumMay [cost l1 l2 s | (l2, s) <- ss]) xs
    (x, lang) <- case mayX of
      Nothing -> MM.throwError . GeneratorError . render $
                 "No implementation found for" <+> squotes (pretty v)
      (Just x@(AnnoS _ (Idx _ ss) _)) -> do
        let newLang = fmap fst (minBy (biasedCost l1) ss)
        x' <- collapseAnnoS newLang x
        return (x', newLang)
    return (VarS v (One x), Idx i lang)

  -- Propagate downwards
  collapseExpr l1 (AccS k x, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    x' <- collapseAnnoS lang x
    return (AccS k x', Idx i lang)
  collapseExpr l1 (LstS xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseAnnoS lang) xs
    return (LstS xs', Idx i lang)
  collapseExpr l1 (TupS xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseAnnoS lang) xs
    return (TupS xs', Idx i lang)
  collapseExpr l1 (LamS vs x, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    x' <- collapseAnnoS lang x
    return (LamS vs x', Idx i lang)
  collapseExpr l1 (AppS f xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    f' <- collapseAnnoS lang f
    xs' <- mapM (collapseAnnoS lang) xs
    return (AppS f' xs', Idx i lang)
  collapseExpr l1 (NamS rs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseAnnoS lang . snd) rs
    return (NamS (zip (map fst rs) xs'), Idx i lang)
  -- collapse leaf expressions
  collapseExpr _ (CallS src, Idx i _) = return (CallS src, Idx i (Just (srcLang src)))
  collapseExpr lang (BndS v,   Idx i _) = return (BndS v,   Idx i lang)
  collapseExpr lang (UniS,   Idx i _) = return (UniS,   Idx i lang)
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
    ::              AnnoS (Indexed Type) One (Indexed (Maybe Lang))
    -> MorlocMonad (AnnoS (Indexed Type) One (Indexed        Lang))
  propagateDown (AnnoS _ (Idx _ Nothing) _) = MM.throwError . CallTheMonkeys $ "Nothing is not OK"
  propagateDown e@(AnnoS _ (Idx _ (Just lang0)) _) = f lang0 e where
    f :: Lang ->     AnnoS (Indexed Type) One (Indexed (Maybe Lang))
      -> MorlocMonad (AnnoS (Indexed Type) One (Indexed        Lang))
    f lang (AnnoS g (Idx i Nothing) e') = f lang (AnnoS g (Idx i (Just lang)) e')
    f _ (AnnoS g (Idx i (Just lang)) e') = do
      e'' <- case e' of
        (AccS k x) -> AccS k <$> f lang x
        (AppS x xs) -> AppS <$> f lang x <*> mapM (f lang) xs
        (LamS vs x) -> LamS vs <$> f lang x
        (LstS xs) -> LstS <$> mapM (f lang) xs
        (TupS xs) -> TupS <$> mapM (f lang) xs
        (NamS rs) -> NamS <$> (zip (map fst rs) <$> mapM (f lang . snd) rs)
        UniS -> return UniS
        (VarS v (One x)) -> VarS v . One <$> f lang x
        (BndS v) -> return (BndS v)
        (RealS x) -> return (RealS x)
        (IntS x) -> return (IntS x)
        (LogS x) -> return (LogS x)
        (StrS x) -> return (StrS x)
        (CallS x) -> return (CallS x)
      return (AnnoS g (Idx i lang) e'')

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
makeGAST :: AnnoS (Indexed Type) One (Indexed (Maybe Lang)) -> MorlocMonad (AnnoS (Indexed Type) One ())
makeGAST = mapAnnoSCM (\(Idx _ _) -> return ())

removeVarS :: AnnoS g One c -> AnnoS g One c
removeVarS (AnnoS g1 _ (VarS _ (One (AnnoS _ c2 x)))) = removeVarS (AnnoS g1 c2 x)
removeVarS (AnnoS g c (AccS k x)) = AnnoS g c (AccS k (removeVarS x))
removeVarS (AnnoS g c (AppS x xs)) = AnnoS g c (AppS (removeVarS x) (map removeVarS xs))
removeVarS (AnnoS g c (LamS vs x )) = AnnoS g c (LamS vs (removeVarS x))
removeVarS (AnnoS g c (LstS xs)) = AnnoS g c (LstS (map removeVarS xs))
removeVarS (AnnoS g c (TupS xs)) = AnnoS g c (TupS (map removeVarS xs))
removeVarS (AnnoS g c (NamS rs)) = AnnoS g c (NamS (map (second removeVarS) rs))
removeVarS x = x


generalSerial :: AnnoS (Indexed Type) One () -> MorlocMonad NexusCommand
generalSerial x0@(AnnoS (Idx i t) _ _) = do
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
    generalSerial' :: NexusCommand -> JsonPath -> AnnoS (Indexed Type) One () -> MorlocMonad NexusCommand
    generalSerial' base _ (AnnoS _ _ UniS)
      = return $ base { commandJson = "null" }
    generalSerial' base _ (AnnoS _ _ (RealS x))
      = return $ base { commandJson = viaShow x }
    generalSerial' base _ (AnnoS _ _ (IntS x))
      = return $ base { commandJson = viaShow x }
    generalSerial' base _ (AnnoS _ _ (LogS x))
      = return $ base { commandJson = if x then "true" else "false" }
    generalSerial' base _ (AnnoS _ _ (StrS x))
      = return $ base { commandJson = dquotes (pretty x) }
    -- if a nested accessor is observed, evaluate the nested expression and
    -- append the path
    generalSerial' base ps (AnnoS _ _ (AccS k x@(AnnoS _ _ (AccS _ _)))) = do
      ncmd <- generalSerial' base ps x
      case commandSubs ncmd of
        [(ps1, arg, ps2)] ->
          return $ ncmd { commandSubs = [(ps1, arg, JsonKey k : ps2)] }
        _ -> error "Bad record access"
    -- record the path to and from a record access, leave the value as null, it
    -- will be set in the nexus
    generalSerial' base ps (AnnoS _ _ (AccS k (AnnoS (Idx _ NamT {}) _ (BndS v)))) =
      return $ base { commandSubs = [(ps, unEVar v, [JsonKey k])] }
    -- If the accessed type is not a record, try to simplify the type
    generalSerial' base ps (AnnoS g1 c1 (AccS key (AnnoS (Idx m oldType) c2 x))) = do
      mayT <- evalGeneralStep i (type2typeu oldType)
      case mayT of
        (Just recordType) ->
          generalSerial' base ps (AnnoS g1 c1 (AccS key (AnnoS (Idx m (typeOf recordType)) c2 x)))
        Nothing -> MM.throwError . OtherError . render $ "Non-record access of type:" <+> pretty oldType
    generalSerial' base ps (AnnoS _ _ (LstS xs)) = do
      ncmds <- zipWithM (generalSerial' base) [ps ++ [JsonIndex j] | j <- [0..]] xs
      return $ base
        { commandJson = list (map commandJson ncmds)
        , commandSubs = concatMap commandSubs ncmds
        }
    generalSerial' base ps (AnnoS _ _ (TupS xs)) = do
      ncmds <- zipWithM (generalSerial' base) [ps ++ [JsonIndex j] | j <- [0..]] xs
      return $ base
        { commandJson = list (map commandJson ncmds)
        , commandSubs = concatMap commandSubs ncmds
        }
    generalSerial' base ps (AnnoS _ _ (NamS es)) = do
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
    generalSerial' base ps (AnnoS _ _ (LamS vs x)) = do
      ncmd <- generalSerial' base ps x
      return $ ncmd { commandArgs = vs }
    generalSerial' base ps (AnnoS _ _(BndS (EV v))) =
      return $ base { commandSubs = [(ps, v, [])] }
    -- bad states
    generalSerial' _ _ (AnnoS _ _ (VarS v _)) = error $ "VarS should have been removed in the prior step, found: " <> show v
    generalSerial' NexusCommand{} _ (AnnoS _ _ (CallS _)) = error "Functions should not occur here, observed AppS"
    generalSerial' NexusCommand{} _ (AnnoS _ _ (AppS _ _)) = error "Functions should not occur here, observed AppS"


-- {- | Remove lambdas introduced through substitution
--
-- For example:
--
--  bif x = add x 10
--  bar py :: "int" -> "int"
--  bar y = add y 30
--  f z = bar (bif z)
--
-- In Treeify.hs, the morloc declarations will be substituted in as lambdas. But
-- we want to preserve the link to any annotations (in this case, the annotation
-- that `bar` should be in terms of python ints). The morloc declarations can be
-- substituted in as follows:
--
--  f z = (\y -> add y 30) ((\x -> add x 10) z)
--
-- The indices for bif and bar that link the annotations to the functions are
-- relative to the lambda expressions, so this substitution preserves the link.
-- Typechecking can proceed safely.
--
-- The expression can be simplified:
--
--  f z = (\y -> add y 30) ((\x -> add x 10) z)
--  f z = (\y -> add y 30) (add z 10)            -- [z / x]
--  f z = add (add z 10) 30                      -- [add z 10 / y]
--
-- The simplified expression is what should be written in the generated code. It
-- would also be easier to typecheck and debug. So should these substitutions be
-- done immediately after parsing? We need to preserve
--  1. links to locations in the original source code (for error messages)
--  2. type annotations.
--  3. declaration names for generated comments and subcommands
--
-- Here is the original expression again, but annotated and indexed
--
--  (\x -> add_2 x_3 10_4)_1
--  (\y -> add_6 y_7 30_8)_5
--  (\z -> bar_10 (bif_11 z_12))_9
--
--  1: name="bif"
--  5: name="bar", type="int"@py -> "int"@py
--  9: name="f"
--
-- Each add is also associated with a type defined in a signature in an
-- unmentioned imported library, but those will be looked up by the typechecker
-- and will not be affected by rewriting.
--
-- Substitution requires reindexing. A definition can be used multiple times and
-- we need to distinguish between the use cases.
--
-- Replace bif and bar with their definition and create fresh indices:
--
--  (\z -> (\y -> add_18 y_19 30_20)_17 ((\x -> add_14 x_15 10_16)_13 z_12)_9
--
--  13,1: name="bif"
--  17,5: name="bar", type="int"@py -> "int"@py
--  9: name="f"
--
-- Now we can substitute for y
--
--  (\z -> add_18 ((\x -> add_14 x_15 10_16)_13 z_12)_9 30_20)
--
-- But this destroyed index 17 and the link to the python annotation. We can
-- preserve the type by splitting the annotation of bar.
--
--  13,1: name="bif"
--  18,17,5: name="bar"
--  12: "int"@py
--  13: "int"@py
--  9: name="f"
--
-- Index 18 should be associated with the *name* "bar", but not the type, since it
-- has been applied. The type of bar is now split between indices 12 and 13.
--
-- This case works fine, but it breaks down when types are polymorphic. If the
-- annotation of bar had been `a -> a`, then how would we type 12 and 13? We can't
-- say that `12 :: forall a . a` and `13 :: forall a . a`, since this
-- eliminates the constraint that the `a`s must be the same.
--
-- If instead we rewrite lambdas after typechecking, then everything works out.
--
-- Thus applyLambdas is done here, rather than in Treeify.hs or Desugar.hs.
--
-- It also must be done BEFORE conversion to ExprM in `express`, where manifolds
-- are resolved.
-- -}
applyLambdas
  :: AnnoS (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang))
-- eliminate empty lambdas
applyLambdas (AnnoS g1 _ (AppS (AnnoS _ _ (LamS [] (AnnoS _ c2 e))) [])) = applyLambdas $ AnnoS g1 c2 e

-- eliminate empty applications
applyLambdas (AnnoS g1 _ (AppS (AnnoS _ c2 e) [])) = applyLambdas $ AnnoS g1 c2 e

-- substitute applied lambdas
applyLambdas
  (AnnoS i1 tb1
    ( AppS
      ( AnnoS
          (Idx i2 (FunT (_:tas) tb2))
          (Idx j2 lang)
          (LamS (v:vs) e2)
      )
      ( e1:es )
    )
  ) = let e2' = substituteAnnoS v e1 e2
      in applyLambdas
          (AnnoS i1 tb1
            ( AppS
              ( AnnoS
                  (Idx i2 (FunT tas tb2))
                  (Idx j2 lang)
                  (LamS vs e2')
              )
              es
            )
          )

-- propagate the changes
applyLambdas (AnnoS g c (AppS f es)) = do
  f' <- applyLambdas f
  es' <- mapM applyLambdas es
  return (AnnoS g c (AppS f' es'))
applyLambdas (AnnoS g c (AccS k e)) = AnnoS g c . AccS k <$> applyLambdas e
applyLambdas (AnnoS g c (LamS vs e)) = AnnoS g c . LamS vs <$> applyLambdas e
applyLambdas (AnnoS g c (LstS es)) = AnnoS g c . LstS <$> mapM applyLambdas es
applyLambdas (AnnoS g c (TupS es)) = AnnoS g c . TupS <$> mapM applyLambdas es
applyLambdas (AnnoS g c (NamS rs)) = AnnoS g c . NamS <$> mapM (secondM applyLambdas) rs
applyLambdas (AnnoS g c (VarS v (One e))) = AnnoS g c . VarS v . One <$> applyLambdas e
applyLambdas x = return x

substituteAnnoS
  :: EVar
  -> AnnoS (Indexed Type) One (Indexed Lang)
  -> AnnoS (Indexed Type) One (Indexed Lang)
  -> AnnoS (Indexed Type) One (Indexed Lang)
substituteAnnoS v r = f where
  f e@(AnnoS _ _ (BndS v'))
    | v == v' = r
    | otherwise = e
  -- propagate the changes
  f (AnnoS g c (AppS e es)) =
    let f' = f e
        es' = map f es
    in AnnoS g c (AppS f' es')
  f (AnnoS g c (AccS k e)) =
    let e' = f e
    in AnnoS g c (AccS k e')
  f (AnnoS g c (LamS vs e)) =
    let e' = f e
    in AnnoS g c (LamS vs e')
  f (AnnoS g c (LstS es)) =
    let es' = map f es
    in AnnoS g c (LstS es')
  f (AnnoS g c (TupS es)) =
    let es' = map f es
    in AnnoS g c (TupS es')
  f (AnnoS g c (NamS rs)) =
    let es' = map (f . snd) rs
    in AnnoS g c (NamS (zip (map fst rs) es'))
  f x = x


-- | Add arguments that are required for each term. Unneeded arguments are
-- removed at each step.
parameterize
  :: AnnoS (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]))
parameterize (AnnoS m@(Idx _ (FunT inputs _)) c (LamS vs x)) = do
  MM.sayVVV "Entering parameterize LamS"
  ids <- MM.takeFromCounter (length inputs)
  let args0 = fromJust $ safeZipWith Arg ids vs
  x' <- parameterize' args0 x
  return $ AnnoS m (c, args0) (LamS vs x')
parameterize (AnnoS m@(Idx _ (FunT inputs _)) c@(Idx _ lang) (BndS v)) = do
  MM.sayVVV $ "Entering parameterize VarS function - " <> pretty v <> "@" <> pretty lang
  ids <- MM.takeFromCounter (length inputs)
  let vs = map EV (freshVarsAZ [])
      args0 = fromJust $ safeZipWith Arg ids vs
  return $ AnnoS m (c, args0) (BndS v)
parameterize x = do
  MM.sayVVV "Entering parameterize Other"
  parameterize' [] x

parameterize'
  :: [Arg EVar] -- arguments in parental scope (child needn't retain them)
  -> AnnoS (Indexed Type) One (Indexed Lang)
  -> MorlocMonad (AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]))
-- primitives, no arguments are required for a primitive, so empty lists
parameterize' _ (AnnoS g c UniS) = return $ AnnoS g (c, []) UniS
parameterize' _ (AnnoS g c (RealS x)) = return (AnnoS g (c, []) (RealS x))
parameterize' _ (AnnoS g c (IntS x))  = return (AnnoS g (c, []) (IntS x))
parameterize' _ (AnnoS g c (LogS x))  = return (AnnoS g (c, []) (LogS x))
parameterize' _ (AnnoS g c (StrS x))  = return (AnnoS g (c, []) (StrS x))
parameterize' args (AnnoS g c (BndS v)) = do
  let args' = [r | r@(Arg _ v') <- args, v' == v]
  return $ AnnoS g (c, args') (BndS v)
parameterize' args (AnnoS g c (AccS k x)) = do
  x' <- parameterize' args x
  let args' = pruneArgs args [x']
  return $ AnnoS g (c, args') (AccS k x')
parameterize' _ (AnnoS m c (CallS src)) = do
  return $ AnnoS m (c, []) (CallS src)
parameterize' args (AnnoS g c (LstS xs)) = do
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args xs'
  return $ AnnoS g (c, args') (LstS xs')
parameterize' args (AnnoS g c (TupS xs)) = do
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args xs'
  return $ AnnoS g (c, args') (TupS xs')
parameterize' args (AnnoS g c (NamS entries)) = do
  xs' <- mapM (parameterize' args . snd) entries
  let args' = pruneArgs args xs'
  return $ AnnoS g (c, args') (NamS (zip (map fst entries) xs'))
parameterize' args (AnnoS g@(Idx _ (FunT inputs _)) c (LamS vs x)) = do
  ids <- MM.takeFromCounter (length inputs)
  let contextArgs = [r | r@(Arg _ v) <- args, v `notElem` vs] -- remove shadowed arguments
      boundArgs = fromJust $ safeZipWith Arg ids vs
  x' <- parameterize' (contextArgs <> boundArgs) x
  let contextArgs' = pruneArgs contextArgs [x']
  return $ AnnoS g (c, contextArgs' <> boundArgs) (LamS vs x')
-- LamS MUST have a functional type, deviations would have been caught by the typechecker
parameterize' _ (AnnoS _ _ (LamS _ _)) = error "impossible"
parameterize' args (AnnoS g c (AppS x xs)) = do
  x' <- parameterize' args x
  xs' <- mapM (parameterize' args) xs
  let args' = pruneArgs args (x':xs')
  return $ AnnoS g (c, args') (AppS x' xs')
parameterize' _ (AnnoS _ _ (VarS _ _)) = undefined

pruneArgs :: [Arg a] -> [AnnoS c One (g, [Arg a])] -> [Arg a]
pruneArgs args xs =
  let usedArgs = unique $ concatMap (map ann . sannoSnd) xs
  in [r | r@(Arg i _) <- args, i `elem` usedArgs]

mkIdx :: AnnoS g One (Indexed c, d) -> Type -> Indexed Type
mkIdx (AnnoS _ (Idx i _, _) _) = Idx i

-- Conventions:
--   midx: The general index, used for identifying manifolds since this index is
--         common across languages.
--   cidx: The concrete index, will be associated with types that in the given
--         language and is required later for evaluating them into concrete
--         types. The language of the type cidx is coupled to in `express` must
--         be the same as the language cidx is coupled to in the SAnno
--         expression.
express :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
-- CallS - direct export of a sourced function, e.g.:
express (AnnoS (Idx midx c@(FunT inputs _)) (Idx cidx lang, _) (CallS src)) = do
  ids <- MM.takeFromCounter (length inputs)
  let lambdaVals = fromJust $ safeZipWith PolyBndVar (map (C . Idx cidx) inputs) ids
  return
    . PolyHead lang midx [Arg i None | i <- ids]
    . PolyReturn
    $ PolyApp (PolySrc (Idx midx c) src) lambdaVals

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
express (AnnoS (Idx midx _) (_, lambdaArgs) (LamS _ (AnnoS (Idx _ applicationType) (c, _) x))) = do
  MM.sayVVV "express LamS:"
  express (AnnoS (Idx midx applicationType) (c, lambdaArgs) x)

express (AnnoS (Idx midx (AppT (VarT v) [t])) (Idx cidx lang, args) (LstS xs)) = do
  xs' <- mapM (\x -> expressPolyExpr lang (mkIdx x t) x) xs
  let x = PolyList (Idx cidx v) (Idx cidx t) xs'
  return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)
express (AnnoS (Idx _ t) _ (LstS _)) = error $ "Invalid list form: " <> show t

express (AnnoS t@(Idx midx (AppT (VarT v) ts)) (Idx cidx lang, args) (TupS xs)) = do
  MM.sayVVV $ "express TupS:" <+> pretty t
  let idxTs = zipWith mkIdx xs ts
  xs' <- fromJust <$> safeZipWithM (expressPolyExpr lang) idxTs xs
  let x = PolyTuple (Idx cidx v) (fromJust $ safeZip idxTs xs')
  return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)

express (AnnoS g _ (TupS _)) = error $ "Invalid tuple form: " <> show g

-- records
express (AnnoS (Idx midx (NamT o v ps rs)) (Idx cidx lang, args) (NamS entries)) = do
  let idxTypes = zipWith mkIdx (map snd entries) (map snd rs)
  xs' <- fromJust <$> safeZipWithM (expressPolyExpr lang) idxTypes (map snd entries)
  let x = PolyRecord o (Idx cidx v) (map (Idx cidx) ps) (zip (map fst rs) (zip idxTypes xs'))
  return $ PolyHead lang midx [Arg i None | Arg i _ <- args] (PolyReturn x)

-- expand the record type if possible, otherwise die
express (AnnoS (Idx midx t) (Idx cidx lang, args) (NamS entries)) = do
  mayT <- evalGeneralStep midx (type2typeu t)
  case mayT of
    (Just t') -> express (AnnoS (Idx midx (typeOf t')) (Idx cidx lang, args) (NamS entries))
    Nothing -> MM.throwError . OtherError . render $ "Missing concrete:" <+> "t=" <> pretty t

-- In other cases, it doesn't matter whether we are at the top of the call
express e = expressDefault e


expressDefault :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyHead
expressDefault e@(AnnoS (Idx midx t) (Idx cidx lang, args) _)
  = PolyHead lang midx [Arg i None | Arg i _ <- args] <$> expressPolyExpr lang (Idx cidx t) e


expressPolyExpr :: Lang -> Indexed Type -> AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -> MorlocMonad PolyExpr
-- these cases will include partially applied functions and explicit lambdas
-- the former is transformed into the latter in the frontend typechecker
expressPolyExpr parentLang _
  (AnnoS (Idx midx lamType@(FunT lamInputTypes lamOutType)) (Idx cidxLam _, lamArgs)
    (LamS vs
      (AnnoS (Idx _ appType) (Idx cidxApp appLang, appArgs)
        (AppS
          (AnnoS callTypeI@(Idx _ (FunT callInputTypes _)) (Idx _ callLang, _)
            (CallS src)) xs))))

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
      MM.sayVVV $ "appArgs:" <+> list (map pretty appArgs)

      let lamIdxTypes = zipWith mkIdx xs lamInputTypes
      let args = fromJust $ safeZipWith (\(Arg i _) t -> Arg i (Just (val t))) lamArgs lamIdxTypes
      xs' <- fromJust <$> safeZipWithM (expressPolyExpr appLang) (zipWith mkIdx xs callInputTypes) xs
      return
          . PolyManifold parentLang midx (ManifoldPass args)
          . PolyReturn
          $ PolyApp (PolySrc callTypeI src) xs'

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

          typedLambdaArgs = fromJust $ safeZipWith (\(Arg i _) t -> Arg i (Just t))
            (drop nContextArgs lamArgs)
            lamInputTypes

      xs' <- fromJust <$> safeZipWithM (expressPolyExpr appLang) (zipWith mkIdx xs callInputTypes) xs
      return
        . PolyManifold parentLang midx (ManifoldPart contextArgs typedLambdaArgs)
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
      xsLocal <- zipWithM (expressPolyExpr appLang) (zipWith mkIdx (take n xs) callInputTypes) (take n xs)

      let xsPassed = bindVar appArgs (map B $ drop n callInputTypes)
          xs' = xsLocal <> xsPassed

      let typedBoundArgs = fromJust $ safeZipWith
            (\(Arg i _) t -> Arg i (Just t))
            (drop (length appArgs - length lamInputTypes) lamArgs)
            lamInputTypes

      MM.sayVVV $ "Making foreign interface 7 of type:" <+> pretty (Idx cidxApp appType)

      return
        . PolyManifold parentLang midx (ManifoldPass typedBoundArgs)
        . PolyReturn
        . PolyApp
          ( PolyForeignInterface callLang (Idx cidxApp appType) (map ann appArgs)
          . PolyManifold callLang midx (ManifoldFull (map unvalue appArgs))
          . PolyReturn
          $ PolyApp call xs'
          )
        -- These arguments are passed to a foreign function, however, they are
        -- called by a local function which means they must be native and
        -- their types must be known. Leaving these terms as passthrough types
        -- was the cause of the cadf62 bug.
        $ bindVar appArgs (map B lamInputTypes)


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
      MM.sayVVV $ "case #8 m" <> pretty midx
                <> "\n  lamType = " <+> pretty lamType
                <> "\n  len(xs) = " <+> pretty (length xs)
                <> "\n  src = " <+> pretty src
                <> "\n  appArgs:" <+> list (map pretty appArgs)
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
      xsInfo <- mapM partialExpress xs

      MM.sayVVV $ "  xsInfo:" <+> pretty xsInfo

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
          lambdaTypeMap = zip vs (map (Idx cidxLam) lamInputTypes)
          -- variables passed to the manifold, those bound by the lambda will
          -- have known local types
          boundVars = [ PolyBndVar (maybe (A parentLang) C (lookup v lambdaTypeMap)) i
                      | Arg i v <- appArgs
                      ]
          untypedContextArgs = map unvalue $ take nContextArgs appArgs
          typedPassedArgs = fromJust $ safeZipWith (\(Arg i _) t -> Arg i (Just t)) (drop nContextArgs lamArgs) lamInputTypes

          localForm = ManifoldPart untypedContextArgs typedPassedArgs

          -- contextArgs
          -- boundArgs = map unvalue (drop () appArgs)
          foreignForm = ManifoldFull [Arg i None | i <- passedParentArgs]

      MM.sayVVV $ "Making foreign interface 8 of type:" <+> pretty (Idx cidxLam lamOutType)
        <> "\n  appArgs" <+> pretty appArgs
        <> "\n  src" <+> pretty src
        <> "\n  vs:" <+> pretty vs
        <> "\n  callArgs:" <+> pretty callArgs
        <> "\n  args:" <+> pretty args
        <> "\n  allParentArgs:" <+> pretty allParentArgs
        <> "\n  passedParentArgs:" <+> pretty passedParentArgs
        <> "\n  nContextArgs:" <+> pretty nContextArgs
        <> "\n  boundVars:" <+> pretty boundVars
        <> "\n  untypedContextArgs:" <+> pretty untypedContextArgs
        <> "\n  typedPassedArgs:" <+> pretty typedPassedArgs
        <> "\n  local manifold form args:" <+> pretty localForm
        <> "\n  foreign manifold form args:" <+> pretty (foreignForm :: ManifoldForm None (Maybe Type))

      return
        . PolyManifold parentLang midx localForm
        . chain lets
        . PolyReturn
        . PolyApp
            ( PolyForeignInterface callLang (Idx cidxLam lamOutType) passedParentArgs
            . PolyManifold callLang midx foreignForm
            . PolyReturn
            $ PolyApp call xs'
            )
        $ boundVars

  where
    sameLanguage = parentLang == callLang

    call = PolySrc callTypeI src

    chain :: [a -> a] -> a -> a
    chain [] x = x
    chain (f:fs) x = chain fs (f x)

    -- This function is applied to every argument of a foreign call in a lambda (case #8)
    --
    -- Partitions evaluation of expressions applied to a foreign pool between the
    -- local and foreign contexts
    partialExpress
        :: AnnoS (Indexed Type) One (Indexed Lang, [Arg EVar]) -- expression
        -> MorlocMonad
            ( [Int] -- ordered foreign arguments, should include ids bound by let (next arg)
            , Maybe (Int, PolyExpr) -- parent let statement if not in child language and eval is needed
            , PolyExpr -- final foreign expression
            )
    -- If the argument is a variable, link the argument id to the variable id and
    -- assign it the foreign call type
    partialExpress (AnnoS (Idx _ t) (Idx cidx argLang, args@[Arg idx _]) (BndS v)) = do
      MM.sayVVV $ "partialExpress case #0:" <+> "x=" <> pretty v <+> "cidx=" <> pretty cidx <+> "t =" <+> pretty t
                <> "\n  parentLang:" <> pretty parentLang
                <> "\n  callLang:" <> pretty callLang
                <> "\n  argLang:" <> pretty argLang
                <> "\n  args:" <> pretty args
      let x' = PolyBndVar (C (Idx cidx t)) idx
      return ([idx], Nothing, x')
    -- Otherwise
    partialExpress x@(AnnoS (Idx _ t) (Idx cidx argLang, args) _)
      -- if this expression is implemented on the foreign side,
      --   translate to ExprM and record
      | argLang == callLang = do
          MM.sayVVV $ "partialExpress case #2:" <+> "cidx=" <> pretty cidx <+> "t =" <+> pretty t
                    <> "\n  parentLang:" <> pretty parentLang
                    <> "\n  callLang:" <> pretty callLang
                    <> "\n  argLang:" <> pretty argLang
                    <> "\n  args:" <> pretty args
          let argParentType = Idx cidx t
          x' <- expressPolyExpr argLang argParentType x
          return ([i | Arg i _ <- args], Nothing, x')
      -- if this expression is implemented on the calling side,
      --   let-bind the expression on the calling side and use the let-index as an
      --   argument index
      | otherwise = do
          MM.sayVVV $ "partialExpress case #1:" <+> "cidx=" <> pretty cidx <+> "t =" <+> pretty t
                    <> "\n  parentLang:" <> pretty parentLang
                    <> "\n  callLang:" <> pretty callLang
                    <> "\n  argLang:" <> pretty argLang
                    <> "\n  args:" <> pretty args
          let argparentType = Idx cidx t
          letVal <- expressPolyExpr argLang argparentType x
          idx <- MM.getCounter
          MM.sayVVV $ "making index in partialExpress #1:" <+> pretty idx

          -- This let variable is used in the foreign side, so the cidx from the
          -- argument is correct
          let x' = PolyLetVar (Idx cidx t) idx
          -- Only the let-bound argument is used on the foreign side
          return ([idx], Just (idx, letVal), x')

expressPolyExpr _ _ (AnnoS lambdaType@(Idx midx _) (Idx _ lang, manifoldArguments) (LamS vs body)) = do
    MM.sayVVV $ "expressPolyExpr LamS:" <+> pretty lambdaType

    body' <- expressPolyExpr lang lambdaType body

    inputTypes <- case val lambdaType of
      (FunT ts _) -> return ts
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
      . PolyManifold lang midx (ManifoldPart contextArguments typeBoundArguments)
      . PolyReturn
      $ body'

-- Apply arguments to a sourced function
-- * The CallS object may be in a foreign language. These inter-language
--   connections will be snapped apart in the segment step.
-- * These applications will be fully applied, the case of partially applied
--   functions will have been handled previously by LamM
expressPolyExpr parentLang pc (AnnoS (Idx midx _) (_, args) (AppS (AnnoS (Idx _ fc@(FunT inputs _)) (Idx cidxCall callLang, _) (CallS src)) xs))

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
      MM.sayVVV $ "case #1 - m" <> pretty midx <> parens (pretty (srcName src)) <> ":" <+> pretty fc
                <> "\n  " <> list (map pretty args)

      -- There should be an equal number of input types and input arguments
      -- That is, the function should be fully applied. If it were partially
      -- applied, the lambda case would have been entered previously instead.
      xs' <- fromJust <$> safeZipWithM (expressPolyExpr callLang) (map (Idx cidxCall) inputs) xs

      MM.sayVVV "  leaving case #1"
      return
          . PolyManifold callLang midx (ManifoldFull (map unvalue args))
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
        MM.sayVVV $ "case #5 - m" <> pretty midx <+> parens (pretty (srcName src)) <> ":"
        MM.sayVVV $ "args:" <+> list (map pretty args)
        MM.sayVVV $ "callLang:" <+> pretty callLang
        MM.sayVVV $ "parentLang:" <+> pretty parentLang

        let idxInputTypes = zipWith mkIdx xs inputs
        xs' <- fromJust <$> safeZipWithM (expressPolyExpr callLang) idxInputTypes xs

        MM.sayVVV $ "Making foreign interface 5 of type:" <+> pretty pc

        return
          . PolyManifold parentLang midx (ManifoldFull (map unvalue args))
          . PolyReturn
          . PolyApp
              ( PolyForeignInterface callLang pc [] -- no args are passed, so empty
              . PolyManifold callLang midx (ManifoldFull (map unvalue args))
              . PolyReturn
              $ PolyApp f xs'
              )
          -- non-native use, leave as passthrough
          -- the contextual language, though, is the same as the parent
          $ [PolyBndVar (A parentLang) i | Arg i _ <- args]

  where
    sameLanguage = parentLang == callLang
    f = PolySrc (Idx cidxCall fc) src

-- An un-applied source call
expressPolyExpr parentLang (val -> FunT pinputs poutput) (AnnoS (Idx midx c@(FunT callInputs _)) (Idx cidx callLang, _) (CallS src))

  ----------------------------------------------------------------------------------------
  -- #2 cis passed                                             | contextArgs | boundArgs |
  ----------------------------------------------------------------------------------------
  --      f_L :: A -> B                                        | []          | []        | -- FIXME
  --      g_L f_L xs                                           |             |           |
  --      -------------                                        |             |           |
  --      def m1(xs):                                          |             |           |
  --          g(m2, xs)                                        |             |           |
  ----------------------------------------------------------------------------------------
  | parentLang == callLang = do
      MM.sayVVV $ "case #2 - un-applied cis source call:" <+> pretty (srcName src)
      ids <- MM.takeFromCounter (length callInputs)
      let lambdaVals = bindVarIds ids (map (C . Idx cidx) callInputs)
          lambdaTypedArgs = fromJust $ safeZipWith annotate ids (map Just callInputs)
      return
        . PolyManifold callLang midx (ManifoldPass lambdaTypedArgs)
        . PolyReturn
        $ PolyApp (PolySrc (Idx cidx c) src) lambdaVals

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
      MM.sayVVV $ "case #6 - " <> pretty midx
      MM.sayVVV $ "Un-applied trans source call:" <+> pretty (srcName src)
      ids <- MM.takeFromCounter (length callInputs)
      let lambdaArgs = [Arg i None | i <- ids]
          lambdaTypedArgs = map (`Arg` Nothing) ids
          callVals = bindVarIds ids (map (C . Idx cidx) callInputs)

      MM.sayVVV $ "src:" <+> pretty src
      MM.sayVVV $ "lambdaArgs:" <+> list (map pretty lambdaArgs)

      MM.sayVVV $ "Making foreign interface 6 of type:" <+> pretty (Idx cidx poutput)

      return
       . PolyManifold parentLang midx (ManifoldPass lambdaTypedArgs)
       . PolyReturn
       . PolyApp
           ( PolyForeignInterface callLang (Idx cidx poutput) (map ann lambdaArgs)
           . PolyManifold callLang midx (ManifoldFull lambdaArgs)
           . PolyReturn
           $ PolyApp (PolySrc (Idx cidx c) src) callVals
           )
       $ fromJust $ safeZipWith (PolyBndVar . C) (map (Idx cidx) pinputs) (map ann lambdaArgs)

-- bound variables
expressPolyExpr _ _ (AnnoS (Idx _ c) (Idx cidx _, rs) (BndS v)) = do
  MM.sayVVV $ "express' VarS" <+> parens (pretty v) <+> "::" <+> pretty c
  case [i | (Arg i v') <- rs, v == v'] of
    [r] -> return $ PolyBndVar (C (Idx cidx c)) r
    rs' -> MM.throwError . OtherError . render
        $ "Expected VarS" <+> dquotes (pretty v) <+>
          "of type" <+> parens (pretty c) <+> "to match exactly one argument, found:" <+> list (map pretty rs')
        <> "\n  v:" <+> pretty v
        <> "\n  cidx:" <+> pretty cidx
        <> "\n  gidx:" <+> pretty cidx
        <> "\n  rs:" <+> list (map pretty rs)

-- primitives
expressPolyExpr _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (RealS x )) = return $ PolyReal (Idx cidx v) x
expressPolyExpr _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (IntS x  )) = return $ PolyInt  (Idx cidx v) x
expressPolyExpr _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (LogS x  )) = return $ PolyLog  (Idx cidx v) x
expressPolyExpr _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _) (StrS x  )) = return $ PolyStr  (Idx cidx v) x
expressPolyExpr _ _ (AnnoS (Idx _ (VarT v)) (Idx cidx _, _)  UniS     ) = return $ PolyNull (Idx cidx v)

-- record access
expressPolyExpr _ pc (AnnoS _ _ (AccS key record@(AnnoS (Idx _ (NamT o v _ rs)) (Idx cidx lang, _) _))) = do
  record' <- expressPolyExpr lang pc record
  case lookup key rs of
    (Just valType) -> return $ PolyAcc (Idx cidx valType) o (Idx cidx v) record' key
    Nothing -> error "invalid key access"

-- lists
expressPolyExpr _ _ (AnnoS (Idx _ (AppT (VarT v) [t])) (Idx cidx lang, _) (LstS xs))
  = PolyList (Idx cidx v) (Idx cidx t) <$> mapM (\x -> expressPolyExpr lang (mkIdx x t) x) xs
expressPolyExpr _ _ (AnnoS _ _ (LstS _)) = error "LstS can only be (AppP (VarP _) [_]) type"

-- tuples
expressPolyExpr _ _ (AnnoS (Idx _ (AppT (VarT v) ts)) (Idx cidx lang, _) (TupS xs)) = do
  let idxTs = zipWith mkIdx xs ts
  xs' <- fromJust <$> safeZipWithM (expressPolyExpr lang) idxTs xs
  return $ PolyTuple (Idx cidx v) (fromJust $ safeZip idxTs xs')
expressPolyExpr _ _ (AnnoS _ _ (TupS _)) = error "TupS can only be (TupP (TupP _) ts) type"

-- records
expressPolyExpr _ _ (AnnoS (Idx _ (NamT o v ps rs)) (Idx cidx lang, _) (NamS entries)) = do
  let tsIdx = zipWith mkIdx (map snd entries) (map snd rs)
  xs' <- fromJust <$> safeZipWithM (expressPolyExpr lang) tsIdx (map snd entries)
  return $ PolyRecord o (Idx cidx v) (map (Idx cidx) ps) (zip (map fst rs) (zip tsIdx xs'))

expressPolyExpr _ _ (AnnoS _ _ (AppS (AnnoS _ _ (BndS v)) _))
  = MM.throwError . ConcreteTypeError $ FunctionSerialization v

-- catch all exception case - not very classy
expressPolyExpr _ _ (AnnoS _ _ (AppS (AnnoS _ _ (LamS vs _)) _))
  = error $ "All applications of lambdas should have been eliminated of length " <> show (length vs)
expressPolyExpr _ parentType (AnnoS (Idx m t) _ _) = do
  MM.sayVVV "Bad case"
  MM.sayVVV $ "  t :: " <> pretty t
  name' <- MM.metaName m
  case name' of
      (Just v) -> MM.throwError . OtherError . render
               $ "Missing concrete:"
               <> "\n  t:" <+> pretty t
               <> "\n  v:" <+> pretty v
               <> "\n parentType:" <+> pretty parentType
      Nothing ->  error "Bug in expressPolyExpr - this should be unreachable"

unvalue :: Arg a -> Arg None
unvalue (Arg i _) = Arg i None

bindVar :: [Arg a] -> [Three Lang Type (Indexed Type)] -> [PolyExpr]
bindVar args = bindVarIds (map ann args)

bindVarIds :: [Int] -> [Three Lang Type (Indexed Type)] -> [PolyExpr]
bindVarIds [] [] = []
bindVarIds (i : args) (t   : types) = PolyBndVar t i : bindVarIds args types
-- These error states indicate a bug in the compiler, not the user code, so no mercy
bindVarIds [] ts = error $ "bindVarIds: too few arguments: " <> show ts
bindVarIds _ [] = error "bindVarIds: too few types"



segment :: PolyHead -> MorlocMonad [MonoHead]
segment (PolyHead lang m0 args0 e0) = do
  (heads, (_, topExpr)) <- segmentExpr m0 (map ann args0) e0

  MM.sayVVV $ "segmentation complete"
            <> "\n  topExpr language:" <+> pretty lang
            <> "\n  topExpr: " <+> pretty topExpr
            <> "\n  heads:" <+> list (map pretty heads)

  return (MonoHead lang m0 args0 topExpr : heads)

segmentExpr
  :: Int -- manifold index
  -> [Int] -- argument indices
  -> PolyExpr
  -> MorlocMonad ([MonoHead], (Maybe Lang, MonoExpr))
-- This is where segmentation happens, every other match is just traversal
segmentExpr _ args (PolyForeignInterface lang callingType cargs e@(PolyManifold _ m (ManifoldFull foreignArgs) _)) = do
  MM.sayVVV $ "segmentExpr PolyForeignInterface PolyManifold m" <> pretty m
            <> "\n  forced ManifoldFull" <+> pretty foreignArgs
            <> "\n  lang" <+> pretty lang
            <> "\n  args" <+> pretty args
            <> "\n  cargs" <+> pretty cargs
            <> "\n  foreignArgs" <+> pretty (map ann foreignArgs)
  (ms, (_, e')) <- segmentExpr m (map ann foreignArgs) e
  let foreignHead = MonoHead lang m foreignArgs e'
  config <- MM.ask
  case MC.buildPoolCallBase config (Just lang) m of
    (Just cmds) -> return (foreignHead:ms, (Nothing, MonoPoolCall callingType m cmds foreignArgs))
    Nothing -> MM.throwError . OtherError $ "Unsupported language: " <> MT.show' lang

segmentExpr m _ (PolyForeignInterface lang callingType args e) = do
  MM.sayVVV $ "segmentExpr PolyForeignInterface m" <> pretty m
            <> "\n  args" <+> pretty args
            <> "\n  lang" <+> pretty lang
  (ms, (_, e')) <- segmentExpr m args e
  -- create the foreign manifold, make sure all arugments are packed
  let foreignHead = MonoHead lang m [Arg i None | i <- args] (MonoReturn e')
      -- pack the arguments that will be passed to the foreign manifold
      es' = map (MonoBndVar (A None)) args
  config <- MM.ask
  -- create the body of the local helper function
  localFun <- case MC.buildPoolCallBase config (Just lang) m of
    (Just cmds) -> return $ MonoApp (MonoPoolCall callingType m cmds [Arg i None | i <- args]) es'
    Nothing -> MM.throwError . OtherError $ "Unsupported language: " <> MT.show' lang
  return (foreignHead:ms, (Nothing, localFun))

segmentExpr _ _ (PolyManifold lang m form e) = do
  (ms, (_, e')) <- segmentExpr m (abilist const const form) e
  return (ms, (Just lang, MonoManifold m form e'))

segmentExpr m args (PolyApp e es) = do
  (ms, (lang, e')) <- segmentExpr m args e
  (mss, es') <- mapM (segmentExpr m args) es |>> unzip
  return (ms ++ concat mss, (lang, MonoApp e' (map snd es')))

segmentExpr m args (PolyLet i e1 e2) = do
  MM.sayVVV "segmentExpr PolyLet"
  (ms1, (_, e1')) <- segmentExpr m args e1
  (ms2, (lang2, e2')) <- segmentExpr m args e2
  return (ms1 ++ ms2, (lang2, MonoLet i e1' e2'))

segmentExpr m args (PolyAcc t o v e k) = do
  (ms, (_, e')) <- segmentExpr m args e
  return (ms, (Nothing, MonoAcc t o v e' k))

segmentExpr m args (PolyList v t es) = do
  (mss, es') <- mapM (segmentExpr m args) es |>> unzip
  return (concat mss, (Nothing, MonoList v t (map snd es')))

segmentExpr m args (PolyTuple v es) = do
  (mss, es') <- mapM (segmentExpr m args . snd) es |>> unzip
  return (concat mss, (Nothing, MonoTuple v (zip (map fst es) (map snd es'))))

segmentExpr m args (PolyRecord o v ps entries) = do
  let entryTypes = map (fst . snd) entries
  (mss, es') <- mapM (segmentExpr m args . snd . snd) entries |>> unzip
  let keys = map fst entries
  return (concat mss, (Nothing, MonoRecord o v ps (zip keys (zip entryTypes (map snd es')))))

segmentExpr m args (PolyReturn e) = do
  (ms, (lang, e')) <- segmentExpr m args e
  return (ms, (lang, MonoReturn e'))

segmentExpr _ _ (PolyLetVar t x) = return ([], (Nothing, MonoLetVar t x))
segmentExpr _ _ (PolyBndVar (A lang) i) = return ([], (Just lang, MonoBndVar (A None) i))
segmentExpr _ _ (PolyBndVar (B t) i) = return ([], (Nothing, MonoBndVar (B t) i))
segmentExpr _ _ (PolyBndVar (C t) i) = return ([], (Nothing, MonoBndVar (C t) i))
segmentExpr _ _ (PolySrc t src) = return ([], (Nothing, MonoSrc t src))
segmentExpr _ _ (PolyLog v x)   = return ([], (Nothing, MonoLog v x))
segmentExpr _ _ (PolyReal v x)  = return ([], (Nothing, MonoReal v x))
segmentExpr _ _ (PolyInt v x)   = return ([], (Nothing, MonoInt v x))
segmentExpr _ _ (PolyStr v x)   = return ([], (Nothing, MonoStr v x))
segmentExpr _ _ (PolyNull v)    = return ([], (Nothing, MonoNull v))


-- | This step is performed after segmentation, so all terms are in the same
-- language. Here we need to determine where inputs are (de)serialized and the
-- serialization states of arguments and variables.
serialize :: MonoHead -> MorlocMonad SerialManifold
serialize (MonoHead lang m0 args0 e0) = do

  form0 <- ManifoldFull <$> mapM prepareArg args0

  MM.sayVVV $ "In serialize for" <+> "m" <> pretty m0 <+> pretty lang <+> "segment"
            <>  "\n  form0:" <+> pretty form0
            <>  "\n  typemap:" <+> viaShow typemap
            <>  "\n  This map we made from the expression:\n  " <> pretty e0

  se1 <- serialExpr m0 e0
  let sm = SerialManifold m0 lang form0 se1
  wireSerial lang sm
  where

  inferType = inferConcreteType lang
  inferTypeUniversal = inferConcreteTypeUniversal lang
  inferVar = inferConcreteVar lang

  -- map of argument indices to native types
  typemap = makeTypemap m0 e0

  prepareArg
    :: Arg None
    -> MorlocMonad (Arg (Or TypeS TypeF))
  prepareArg (Arg i _) = case Map.lookup i typemap of
    Nothing -> return $ Arg i (L PassthroughS)
    (Just (Right t)) -> do
      t' <- inferType t
      return $ Arg i (L (typeSof t'))
    (Just (Left t)) -> do
      MM.sayVVV "Warning: using universal inference at prepareArg"
      t' <- inferTypeUniversal t
      return $ Arg i (L (typeSof t'))

  contextArg
    :: Int
    -> MorlocMonad (Or TypeS TypeF)
  contextArg i = case Map.lookup i typemap of
    (Just (Right t)) -> do
      t' <- inferType t
      return $ LR (typeSof t') t'
    Nothing -> return $ L PassthroughS
    (Just (Left t)) -> do
      MM.sayVVV "Warning: using universal inference at contextArg"
      t' <- inferTypeUniversal t
      return $ LR (typeSof t') t'

  boundArg :: Int -> MorlocMonad TypeF
  boundArg i = case Map.lookup i typemap of
    (Just (Right t)) -> inferType t
    Nothing -> error "Untyped native arg"
    (Just (Left t)) -> do
      MM.sayVVV "Warning: using universal inference at boundArg"
      inferTypeUniversal t

  serialExpr
    :: Int
    -> MonoExpr
    -> MorlocMonad SerialExpr
  serialExpr _ (MonoManifold m form e) = do
    MM.sayVVV $ "serialExpr MonoManifold m" <> pretty m <> parens (pretty form)
    serialExpr m e
  serialExpr m (MonoLet i e1 e2) = case inferState e1 of
    Serialized -> SerialLetS i <$> serialExpr m e1 <*> serialExpr m e2
    Unserialized -> do
      ne1 <- nativeExpr m e1
      NativeLetS i ne1 <$> serialExpr m e2
  serialExpr _ (MonoLetVar t i) = do
    t' <- inferType t
    return $ LetVarS (Just t') i
  serialExpr m (MonoReturn e) = ReturnS <$> serialExpr m e
  serialExpr _ (MonoApp (MonoPoolCall t m docs contextArgs) es) = do
    contextArgs' <- mapM (typeArg Serialized . ann) contextArgs
    let poolCall' = PoolCall m docs contextArgs'
    es' <- mapM (serialArg m) es
    t' <- inferType t
    return $ AppPoolS t' poolCall' es'
  serialExpr _ (MonoBndVar (A _) i) = return $ BndVarS Nothing i
  serialExpr _ (MonoBndVar (B _) i) =
    case Map.lookup i typemap of
      (Just (Right t)) -> BndVarS <$> fmap Just (inferType t) <*> pure i
      _ -> return $ BndVarS Nothing i
  serialExpr _ (MonoBndVar (C t) i) = BndVarS <$> fmap Just (inferType t) <*> pure i
  -- failing cases that should be unreachable
  serialExpr _ (MonoSrc _ _) = error "Can represent MonoSrc as SerialExpr"
  serialExpr _ MonoPoolCall{} = error "MonoPoolCall does not map to a SerialExpr"
  serialExpr _ (MonoApp MonoManifold{} _) = error "Illegal?"
  -- the following are all native types that need to be directly serialized
  serialExpr m e = nativeExpr m e >>= serializeS "serialE e" m

  serialArg
    :: Int
    -> MonoExpr
    -> MorlocMonad SerialArg
  serialArg _ e@(MonoManifold m form _) = do
    MM.sayVVV $ "serialArg MonoManifold m" <> pretty m <> parens (pretty form)
    se <- serialExpr m e
    case se of
      (ManS sm) -> return $ SerialArgManifold sm
      _ -> error "Unreachable?"
  -- Pool and source calls should have previously been wrapped in manifolds
  serialArg _ MonoPoolCall{} = error "This step should be unreachable"
  serialArg _ (MonoSrc    _ _) = error "This step should be unreachable"
  serialArg _ (MonoReturn _) = error "Return should not happen hear (really I should remove this term completely)"
  serialArg m e = SerialArgExpr <$> serialExpr m e

  nativeArg
    :: Int
    -> MonoExpr
    -> MorlocMonad NativeArg
  -- This case may occur, for example, with `(add 1.0 2.0)`. Here `add` has two
  -- native arguments, but the manifold that wraps it will have no
  -- arguments. This is because `1.0` and `2.0` are primitive and will be
  -- generated in place rather than passed as arguments.
  nativeArg _ e@(MonoManifold m form _) = do
    MM.sayVVV $ "nativeArg MonoManifold m" <> pretty m <> parens (pretty form)
    ne <- nativeExpr m e
    case ne of
      (ManN nm) -> return $ NativeArgManifold nm
      _ -> error "Unreachable?"
  -- Pool and source calls should have previously been wrapped in manifolds
  nativeArg _ MonoPoolCall{} = error "This step should be unreachable"
  nativeArg _ (MonoSrc    _ _) = error "This step should be unreachable"
  nativeArg _ (MonoReturn _) = error "Return should not happen here (really I should remove this term completely)"
  nativeArg m e = NativeArgExpr <$> nativeExpr m e

  nativeExpr
    :: Int
    -> MonoExpr
    -> MorlocMonad NativeExpr
  nativeExpr _ (MonoManifold m form e) = do
    MM.sayVVV $ "nativeExpr MonoManifold m" <> pretty m <> parens (pretty form)
    ne <- nativeExpr m e
    form' <- abimapM (\i _ -> contextArg i) (\i _ -> boundArg i) form
    return . ManN $ NativeManifold m lang form' ne

  nativeExpr _ MonoPoolCall{} = error "MonoPoolCall does not map to NativeExpr"
  nativeExpr m (MonoLet i e1 e2) = case inferState e1 of
    Serialized -> do
      ne2 <- nativeExpr m e2
      SerialLetN i <$> serialExpr m e1 <*> pure ne2
    Unserialized -> do
      ne1 <- nativeExpr m e1
      ne2 <- nativeExpr m e2
      return $ NativeLetN i ne1 ne2
  nativeExpr _ (MonoLetVar t i) = LetVarN <$> inferType t <*> pure i
  nativeExpr m (MonoReturn e) = ReturnN <$> nativeExpr m e
  nativeExpr m (MonoApp (MonoSrc (Idx idx (FunT inputTypes outputType)) src) es) = do
    args <- mapM (nativeArg m) es
    appType <- case drop (length es) inputTypes of
        [] -> inferType (Idx idx outputType)
        remaining -> inferType $ Idx idx (FunT remaining outputType)
    return $ AppSrcN appType src args
  nativeExpr m e@(MonoApp (MonoPoolCall t _ _ _) _) = do
    e' <- serialExpr m e
    t' <- inferType t
    MM.sayVVV $ "nativeExpr MonoApp:" <+> pretty t'
    naturalizeN "nativeE MonoApp" m lang t' e'
  nativeExpr _ (MonoApp _ _) = error "Illegal application"
  nativeExpr _ (MonoSrc t src) = SrcN <$> inferType t <*> pure src
  nativeExpr _ (MonoBndVar (A _) _) = error "MonoBndVar must have a type if used in native context"
  nativeExpr _ (MonoBndVar (B _) i) =
    case Map.lookup i typemap of
      (Just (Right t)) -> BndVarN <$> inferType t <*> pure i
      _ -> error "No type found"
  nativeExpr _ (MonoBndVar (C t) i) = BndVarN <$> inferType t <*> pure i
  -- simple native types
  nativeExpr m (MonoAcc _ o v e k) = do
    v' <- inferVar v
    e' <- nativeExpr m e
    return $ AccN o v' e' k
  nativeExpr m (MonoList v t es) =
    ListN
      <$> inferVar v
      <*> inferType t
      <*> mapM (nativeExpr m) es
  nativeExpr m (MonoTuple v rs) =
    TupleN
      <$> inferVar v
      <*> mapM (nativeExpr m . snd) rs
  nativeExpr m (MonoRecord o v ps rs) =
    RecordN o
      <$> inferVar v
      <*> mapM inferType ps
      <*> mapM (secondM (nativeExpr m . snd)) rs
  -- primitives
  nativeExpr _ (MonoLog    v x) = LogN  <$> inferVar v <*> pure x
  nativeExpr _ (MonoReal   v x) = RealN <$> inferVar v <*> pure x
  nativeExpr _ (MonoInt    v x) = IntN  <$> inferVar v <*> pure x
  nativeExpr _ (MonoStr    v x) = StrN  <$> inferVar v <*> pure x
  nativeExpr _ (MonoNull   v  ) = NullN <$> inferVar v

  typeArg
    :: SerializationState
    -> Int
    -> MorlocMonad (Arg TypeM)
  typeArg s i = case (s, Map.lookup i typemap) of
    (Serialized, Just (Right t)) -> do
      t' <- inferType t
      return $ Arg i (Serial t')
    (Serialized, Nothing) -> return $ Arg i Passthrough
    (Serialized, Just (Left t)) -> do
      MM.sayVVV $ "typeArg universal inference of unindexed type " <> pretty t
      t' <- inferTypeUniversal t
      return $ Arg i (Serial t')
    (Unserialized, Just (Right t)) -> do
      t' <- inferType t
      return $ Arg i (Native t')
    (Unserialized, Nothing) -> error "Bug: untyped non-passthrough value"
    (Unserialized, Just (Left t)) -> do
      MM.sayVVV $ "typeArg universal inference of unindexed type " <> pretty t
      t' <- inferTypeUniversal t
      return $ Arg i (Native t')

  makeTypemap :: Int -> MonoExpr -> Map.Map Int (Either Type (Indexed Type))
  -- map variables used in this segment to their types
  makeTypemap _ (MonoLetVar t i) = Map.singleton i (Right t)
  makeTypemap parentIndex (MonoBndVar (B t) i) = Map.singleton i (Right (Idx parentIndex t))
  makeTypemap _ (MonoBndVar (C t) i) = Map.singleton i (Right t)
  -- recursive calls
  makeTypemap _ (MonoManifold midx (manifoldBound -> ys) e) =
    -- bound arguments may not be used, but they where passed in from the
    -- source function, so they cannot be removed.
    Map.union (Map.fromList [(i, Left t) | (Arg i (Just t)) <- ys]) (makeTypemap midx e)
  makeTypemap parentIdx (MonoLet _ e1 e2) = Map.union (makeTypemap parentIdx e1) (makeTypemap parentIdx e2)
  makeTypemap parentIdx (MonoReturn e) = makeTypemap parentIdx e
  makeTypemap _ (MonoApp (MonoSrc (ann -> idx) _) es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
  makeTypemap parentIdx (MonoApp e es) = Map.unionsWith mergeTypes (map (makeTypemap parentIdx) (e:es))
  makeTypemap parentIdx (MonoAcc _ _ _ e _) = makeTypemap parentIdx e
  makeTypemap _ (MonoList (ann -> idx) _ es) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
  makeTypemap _ (MonoTuple (ann -> idx) (map snd -> es)) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
  makeTypemap _ (MonoRecord _ (ann -> idx) _ (map (snd . snd) -> es)) = Map.unionsWith mergeTypes (map (makeTypemap idx) es)
  makeTypemap _ _ = Map.empty

  mergeTypes :: Either Type (Indexed Type) -> Either Type (Indexed Type) -> Either Type (Indexed Type)
  mergeTypes (Right t) _ = Right t
  mergeTypes _ (Right t) = Right t
  mergeTypes x _ = x

  -- make (SerialAST One) then convert to serializeMany
  serializeS :: MDoc -> Int -> NativeExpr -> MorlocMonad SerialExpr
  serializeS msg m se = do
    MM.sayVVV $ "serializeS" <+> pretty m <> ":" <+> msg
    SerializeS <$> Serial.makeSerialAST m lang (typeFof se) <*> pure se

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

class IsSerializable a where
  serialLet :: Int -> SerialExpr -> a -> a
  nativeLet :: Int -> NativeExpr -> a -> a

instance IsSerializable SerialExpr where
  serialLet = SerialLetS
  nativeLet = NativeLetS

instance IsSerializable NativeExpr where
  serialLet = SerialLetN
  nativeLet = NativeLetN

type (D a) = (Map.Map Int Request, a)

-- (de)serialize arguments as needed
-- determines whether arguments are passed as serialized, native, or both
wireSerial :: Lang -> SerialManifold -> MorlocMonad SerialManifold
wireSerial lang sm0@(SerialManifold m0 _ _ _) = foldSerialManifoldM fm sm0 |>> snd
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
    let form' = afirst (specialize req) form
        req' = Map.map fst (manifoldToMap form')
    e' <- letWrap m form' req e
    return (req', SerialManifold m lang form' e')

  wireNativeManifold :: NativeManifold_ (D NativeExpr) -> MorlocMonad (D NativeManifold)
  wireNativeManifold (NativeManifold_ m _ form (req, e)) = do
    let form' = afirst (specialize req) form
        req' = Map.map fst (manifoldToMap form')
    e' <- letWrap m form' req e
    return (req', NativeManifold m lang form' e')

  wireSerialExpr (LetVarS_ t i) = return (Map.singleton i SerialContent, LetVarS t i)
  wireSerialExpr (BndVarS_ t i) = return (Map.singleton i SerialContent, BndVarS t i)
  wireSerialExpr (AppPoolS_ t p@(PoolCall _ _ pargs) args) = do
    let req1 = Map.unionsWith (<>) (map fst args)
        req2 = Map.fromList [(i, requestOf tm) | Arg i tm <- pargs]
        req3 = Map.unionWith (<>) req1 req2
    return (req3, AppPoolS t p (map snd args))

  -- for let expressions, I need to determine how the bound expression is used
  -- downstream and then (un)serialize accordingly
  wireSerialExpr (SerialLetS_ i (req1, se1) (req2, se2)) = do
    let req' = Map.unionWith (<>) req1 req2
    e' <- case Map.lookup i req2 of
      (Just NativeContent) -> case typeSof se1 of
        (SerialS tf) -> NativeLetS i <$> naturalizeN "a" m0 lang tf se1 <*> pure se2
        _ -> error "Unuseable let definition"
      (Just NativeAndSerialContent) -> error "Unsupported dual use let definition"
      _ -> return $ SerialLetS i se1 se2
    return (req', e')
  wireSerialExpr (NativeLetS_ i (req1, ne1) (req2, se2)) = do
    let req' = Map.unionWith (<>) req1 req2
    e' <- case Map.lookup i req2 of
      (Just SerialContent) -> SerialLetS i <$> serializeS "b" m0 (typeFof ne1) ne1 <*> pure se2
      (Just NativeAndSerialContent) -> error "Unsupported dual use let definition"
      _ -> return $ NativeLetS i ne1 se2
    return (req', e')

  wireSerialExpr e = monoidSerialExpr defs e

  wireNativeExpr :: NativeExpr_ (D NativeManifold) (D SerialExpr) (D NativeExpr) (D SerialArg) (D NativeArg) -> MorlocMonad (D NativeExpr)
  wireNativeExpr (LetVarN_ t i) = return (Map.singleton i NativeContent, LetVarN t i)
  wireNativeExpr (BndVarN_ t i) = return (Map.singleton i NativeContent, BndVarN t i)

  wireNativeExpr (SerialLetN_ i (req1, se1) (req2, ne2)) = do
    let req' = Map.unionWith (<>) req1 req2
    e' <- case Map.lookup i req2 of
      (Just NativeContent) -> case typeSof se1 of
        (SerialS tf) -> NativeLetN i <$> naturalizeN "a" m0 lang tf se1 <*> pure ne2
        _ -> error "Unuseable let definition"
      (Just NativeAndSerialContent) -> error "Unsupported dual use let definition"
      _ -> return $ SerialLetN i se1 ne2
    return (req', e')
  wireNativeExpr (NativeLetN_ i (req1, ne1) (req2, ne2)) = do
    let req' = Map.unionWith (<>) req1 req2
    e' <- case Map.lookup i req2 of
      (Just SerialContent) -> SerialLetN i <$> serializeS "b" m0 (typeFof ne1) ne1 <*> pure ne2
      (Just NativeAndSerialContent) -> error "Unsupported dual use let definition"
      _ -> return $ NativeLetN i ne1 ne2
    return (req', e')

  wireNativeExpr e = monoidNativeExpr defs e

  specialize :: Map.Map Int Request -> Int -> Or TypeS TypeF -> Or TypeS TypeF
  specialize req i r = case (Map.lookup i req, r) of
    (Nothing, _) -> L PassthroughS
    (Just SerialContent, LR t _) -> L t
    (Just NativeContent, LR _ t) -> R t
    _ -> r

  letWrap :: (IsSerializable e, HasRequest t, MayHaveTypeF t)
          => Int -> ManifoldForm (Or TypeS TypeF) t -> Map.Map Int Request -> e -> MorlocMonad e
  letWrap m form0 req0 e0 = foldlM wrapAsNeeded e0 (Map.toList req0) where

    formMap = manifoldToMap form0

    wrapAsNeeded :: IsSerializable e => e -> (Int, Request) -> MorlocMonad e
    wrapAsNeeded e (i, req) = case (req, Map.lookup i formMap) of
      (SerialContent,          Just (NativeContent, Just t)) -> serialLet i <$> serializeS "wan 1" m t (BndVarN t i) <*> pure e
      (NativeAndSerialContent, Just (NativeContent, Just t)) -> serialLet i <$> serializeS "wan 2" m t (BndVarN t i) <*> pure e
      (NativeContent,          Just (SerialContent, Just t)) -> nativeLet i <$> naturalizeN "wan 3" m lang t (BndVarS (Just t) i) <*> pure e
      (NativeAndSerialContent, Just (SerialContent, Just t)) -> nativeLet i <$> naturalizeN "wan 4" m lang t (BndVarS (Just t) i) <*> pure e
      _ -> return e

  manifoldToMap :: (HasRequest t, MayHaveTypeF t) => ManifoldForm (Or TypeS TypeF) t -> Map.Map Int (Request, Maybe TypeF)
  manifoldToMap form = f form where
    mapRequestFromXs xs = Map.fromList [(i, (requestOf t, mayHaveTypeF t)) | (Arg i t) <- typeMofRs xs]
    mapRequestFromYs ys = Map.fromList [(i, (requestOf t, mayHaveTypeF t)) | (Arg i t) <- ys]

    f (ManifoldFull xs) = mapRequestFromXs xs
    f (ManifoldPass ys) = mapRequestFromYs ys
    f (ManifoldPart xs ys) = Map.union (mapRequestFromXs xs) (mapRequestFromYs ys)

  serializeS :: MDoc -> Int -> TypeF -> NativeExpr -> MorlocMonad SerialExpr
  serializeS msg m t se = do
    MM.sayVVV $ "serializeS" <+> pretty m <> ":" <+> msg
    SerializeS <$> Serial.makeSerialAST m lang t <*> pure se


naturalizeN :: MDoc -> Int -> Lang -> TypeF -> SerialExpr -> MorlocMonad NativeExpr
naturalizeN msg m lang t se = do
  MM.sayVVV $ "naturalizeN at" <+> msg
  DeserializeN t <$> Serial.makeSerialAST m lang t <*> pure se


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
    RLang -> R.translate srcs es
    Python3Lang -> Python3.translate srcs es
    x -> MM.throwError . PoolBuildError . render
      $ "Language '" <> viaShow x <> "' has no translator"

preprocess :: Lang -> SerialManifold -> MorlocMonad SerialManifold
preprocess CppLang es = Cpp.preprocess es
preprocess RLang es = R.preprocess es
preprocess Python3Lang es = Python3.preprocess es
preprocess l _ = MM.throwError . PoolBuildError . render
               $ "Language '" <> viaShow l <> "' has no translator"


sannoSnd :: AnnoS g One (a, b) -> b
sannoSnd (AnnoS _ (_, x) _) = x

-- generate infinite list of fresh variables of form
-- ['a','b',...,'z','aa','ab',...,'zz',...]
freshVarsAZ
  :: [MT.Text] -- variables to exclude
  -> [MT.Text]
freshVarsAZ exclude =
  filter
    (`notElem` exclude)
    ([1 ..] >>= flip replicateM ['a' .. 'z'] |>> MT.pack)
