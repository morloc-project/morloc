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
generate = undefined
  -- -- translate modules into bitrees
  -- (gASTs, rASTs)
  --   -- select a single instance at each node in the tree
  --   <- mapM realize   -- [Either (SAnno GMeta One CType) (SAnno GMeta One CType)]
  --   -- separate unrealized (general) ASTs (uASTs) from realized ASTs (rASTs)
  --   |>> partitionEithers
  --
  -- -- Collect all call-free data
  -- gSerial <- mapM generalSerial gASTs
  --
  -- -- build nexus
  -- -- -----------
  -- -- Each nexus subcommand calls one function from one one pool.
  -- -- The call passes the pool an index for the function (manifold) that will be called.
  -- nexus <- Nexus.generate
  --   gSerial
  --   -- FIXME
  --   -- [ (t, poolId m x, metaName m)
  --   -- | SAnno (One (x, t)) m <- rASTs
  --   -- ]
  --
  -- -- find all sources files
  -- let srcs = unique . concat . conmap (unpackSAnno getSrcs) $ rASTs
  --
  -- -- for each language, collect all functions into one "pool"
  -- pools
  --   -- -- thread arguments across the tree
  --   -- <- mapM parameterize rASTs
  --   -- -- convert from AST to manifold tree
  --   -- >>= mapM express
  --   -- -- rewrite lets to minimize the number of foreign calls
  --   -- >>= mapM letOptimize
  --   -- -- Separate the call trees into mono-lingual segments terminated in
  --   -- -- primitives or foreign calls.
  --   -- >>= mapM segment |>> concat
  --   -- -- Cast each call tree root as a manifold
  --   -- >>= mapM rehead
  --   -- -- Gather segments into pools, currently this entails gathering all
  --   -- -- segments from a given language into one pool. Later it may be more
  --   -- -- nuanced.
  --   -- >>= pool
  --   -- -- Generate the code for each pool
  --   -- >>= mapM (encode srcs)


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

  maximumMay :: Ord a => [a] -> Maybe a
  maximumMay [] = Nothing
  maximumMay xs = Just (maximum xs)

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
makeGAST = undefined


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
