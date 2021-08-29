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
  e@(SAnno (One (_, li)) _) <- scoreAnno [] s0 >>= collapse
  case li of
    (Idx _ Nothing) -> makeGAST e |>> Left 
    (Idx _ _) -> Right <$> mapCM unmaybeIdx e
  where
  scoreAnno
    :: [Lang]
    -> SAnno (Indexed Type) Many Int
    -> MorlocMonad (SAnno (Indexed Type) Many (Indexed [(Lang, Int)]))
  scoreAnno langs (SAnno (Many xs) t) = do
    xs' <- mapM (scoreExpr langs) xs
    return (SAnno (Many xs') t)

  scoreExpr
    :: [Lang]
    -> (SExpr (Indexed Type) Many Int, Int)
    -> MorlocMonad (SExpr (Indexed Type) Many (Indexed [(Lang, Int)]), Indexed [(Lang, Int)])
  scoreExpr = undefined
  -- scoreExpr langs (AccS x k, i) = do
  --   x' <- realizeSAnno langs x
  --   return (AccS x' k, Idx i (scoresOf x'))
  -- scoreExpr _ (ListS xs, i) = do
  --   (xs', best) <- realizeMany langs xs
  --   return (ListS xs', Idx i best)
  -- scoreExpr langs (TupleS xs, i) = do
  --   (xs', best) <- realizeMany langs xs
  --   return (TupleS xs', Idx i best)
  -- scoreExpr langs (LamS vs x, i) = do
  --   x' <- scoreAnno langs x
  --   return (LamS vs x', Idx i (scoresOf x'))
  -- scoreExpr _ (AppS f xs, i) = do
  --   f' <- scoreAnno [] f
  --   let scores = scoresOf f'
  --   xs' <- mapM (scoreAnno (unique $ map fst scores)) xs
  --   pairss <- [maxPairs [xs''' | (_, Idx _ xs''') <- xs''] | SAnno (Many xs'') _ <- xs']
  --   let best = [ (l1, sum [ maximum [s1 + s2 + Lang.pairwiseCost l1 l2
  --                         | (l2, s2) <- pairs] | pairs <- pairss])
  --              | (l1, s1) <- scores]
  --   return (AppS f' xs', Idx i best)
  -- scoreExpr langs (RecS rs, i) = do
  --   (xs, best) <- realizeMany langs (map snd rs)
  --   return $ (RecS (zip (map fst rs) xs), Idx i best)
  -- scoreExpr _ (CallS s, i) = return (CallS s, Idx i [(srcLang s, 0)])
  -- scoreExpr langs (e, i) = return (e, Idx i (zip langs (repeat 0)))

  scoresOf :: SAnno a Many (Indexed [(Lang, Int)]) -> [(Lang, Int)]
  scoresOf (SAnno (Many xs) _) = maxPairs . concat $ [xs' | (_, Idx _ xs') <- xs]

  realizeMany
    :: [Lang]
    -> [SAnno (Indexed Type) Many Int]
    -> MorlocMonad ([SAnno (Indexed Type) Many (Indexed [(Lang, Int)])], [(Lang, Int)])
  realizeMany = undefined
  -- realizeMany langs xs = do
  --   xs' <- mapM scoreAnno xs
  --   return (xs', scoreMany xs')
  --   where
  --     scoreMany :: [Lang] -> [SAnno (Indexed Type) Many (Indexed [(Lang, Int)])] -> [(Lang, Int)]
  --     scoreMany langs0 xs =
  --       let pairss = [maxPairs [xs'' | (_, Idx _ xs'') <- xs'] | SAnno (Many xs') _ <- xs]
  --           langs = unique (langs0 <> (concat . map (map fst)) pairss)
  --       in [(l1, sum [ maximum [score + Lang.pairwiseCost l1 l2 | (l2, score) <- pairs] | pairs <- pairss]) | l1 <- langs]

  collapse
    :: SAnno (Indexed Type) Many (Indexed [(Lang, Int)])
    -> MorlocMonad (SAnno (Indexed Type) One (Indexed (Maybe Lang)))
  collapse = undefined
  -- collapse (SAnno (Many xs) t) = do
  --   let (x, Idx i lang) = bestSExpr (head xs) (tail xs)
  --   x' <- collapseExpr lang x
  --   return (SAnno (One x') t)
  --   where
  --     bestSExpr :: (a, [(Lang, Int)]) -> [(a, [(Lang, Int)])] -> (a, Maybe Lang)
  --     bestSExpr (a, []) [] = (a, Nothing)
  --     bestSExpr (a, xs) [] = (a, Nothing)
  --     bestSExpr (x1, y1) ((x2, y2):rs)
  --       | y2 > y1 = bestSExpr (x2, y2) rs
  --       | otherwise = bestSExpr (x1, y1) rs

  collapseSAnno
    :: Maybe Lang
    -> SAnno (Indexed Type) Many (Indexed [(Lang, Int)])
    -> MorlocMonad (SAnno (Indexed Type) One (Indexed (Maybe Lang)), Indexed (Maybe Lang))
  collapseSAnno = undefined  

  collapseExpr
    :: Maybe Lang -- the language of the parent expression (if Nothing, then this is a GAST)
    -> SExpr (Indexed Type) Many (Indexed [(Lang, Int)])
    -> MorlocMonad (SExpr (Indexed Type) One (Indexed (Maybe Lang)), Indexed (Maybe Lang))
  collapseExpr = undefined
  -- -- collapse GAST expressions
  -- collapseExpr Nothing (AccS x k, Idx i _) = do
  --   x' <- collapseSAnno Nothing x
  --   return (AccS x' k, Idx i Nothing)
  -- collapseExpr Nothing (ListS xs, Idx i _) = do
  --   xs' <- mapM (collapseSAnno Nothing) xs
  --   return (ListS xs', Idx i Nothing)
  -- collapseExpr Nothing (TupleS xs, Idx i _) = do
  --   xs' <- mapM (collapseSAnno Nothing) xs
  --   return (TupleS xs', Idx i Nothing)
  -- collapseExpr Nothing (LamS vs x, Idx i _) = do
  --   x' <- collapseSAnno Nothing x
  --   return (LamS vs x', Idx i Nothing)
  -- collapseExpr Nothing (AppS f xs, Idx i _) = do
  --   f' <- collapseSAnno Nothing f
  --   xs' <- mapM (collapseSAnno Nothing) xs
  --   return (AppS f' xs', Idx i Nothing)
  -- collapseExpr Nothing (RecS rs, Idx i _) = do
  --   xs' <- mapM (collapseSAnno Nothing . snd) rs
  --   return (RecS (zip (map fst rs) xs'), Idx i Nothing)
  -- -- collapse using parent language
  -- collapseExpr (Just l1) (AccS x k, Idx i ss) = do
  --   lang <- chooseLanguage l1 ss
  --   x' <- collapseSAnno (Just lang) x
  --   return (AccS x' k, Idx i (Just lang))
  -- collapseExpr (Just l1) (ListS xs, Idx i ss) = do
  --   lang <- chooseLanguage l1 ss
  --   xs' <- mapM (collapseSAnno (Just lang)) xs
  --   return (ListS xs', Idx i (Just lang))
  -- collapseExpr (Just l1) (TupleS xs, Idx i ss) = do
  --   lang <- chooseLanguage l1 ss
  --   xs' <- mapM (collapseSAnno (Just lang)) xs
  --   return (TupleS xs', Idx i (Just lang))
  -- collapseExpr (Just l1) (LamS vs x, Idx i ss) = do
  --   lang <- chooseLanguage l1 ss
  --   x' <- collapseSAnno (Just lang) x
  --   return (LamS vs x', Idx i (Just lang))
  -- collapseExpr (Just l1) (AppS f xs, Idx i ss) = do
  --   lang <- chooseLanguage l1 ss
  --   f' <- collapseSAnno (Just lang) f
  --   xs' <- mapM (collapseSAnno (Just lang)) xs
  --   return (AppS f' xs', Idx i (Just lang))
  -- collapseExpr (Just l1) (RecS rs, Idx i ss) = do
  --   lang <- chooseLanguage l1 ss
  --   xs' <- mapM (collapseSAnno (Just lang) . snd) rs
  --   return (RecS (zip (map fst rs) xs'), Idx i (Just lang))
  -- -- collapse leaf expressions
  -- collapseExpr lang (e, Idx i _) = return (e, Idx i lang)

  chooseLanguage :: Lang -> [(Lang, Int)] -> MorlocMonad Lang
  chooseLanguage = undefined
  -- chooseLanguage l1 ss =
  --   case maxScore [(l2, s2 + Lang.pairwiseCost l1 l2) | (l2, s2) <- ss] of
  --     Nothing -> MM.throwError . CallTheMonkeys $ "This shouldn't happen"
  --     (Just (l3, s3)) -> return l3

  maxScore :: [(a, Int)] -> Maybe a
  maxScore [] = Nothing
  maxScore (x0:rs0) = f x0 rs0 where
    f (x, _) [] = Just x
    f (x1, i1) ((x2, i2):rs)
      | i2 > i1 = f (x2, i2) rs
      | otherwise = f (x1, i1) rs

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
