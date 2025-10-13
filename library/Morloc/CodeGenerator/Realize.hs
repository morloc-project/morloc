{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Realize
Description : Resolve all implementation polymorphism
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Realize (
    realityCheck
) where

import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Language as Lang
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import qualified Morloc.CodeGenerator.SystemConfig as MCS

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

  -- check and configure the system
  -- in the future, the results of this step may be used to winnow the build
  MCS.configure rASTs

  return (gASTs, rASTs)

-- State for the realize scoring algorithm
data RState = RState
    { rLangs :: [Lang]
    , rApplied :: [AnnoS (Indexed Type) Many Int]
    , rBndVars :: Map.Map EVar (AnnoS (Indexed Type) Many Int)
    }

emptyRState = RState
    { rLangs = []
    , rApplied = []
    , rBndVars = Map.empty
    }

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
  e@(AnnoS _ li _) <- scoreAnnoS emptyRState s0 >>= collapseAnnoS Nothing |>> removeVarS
  case li of
    (Idx _ Nothing) -> makeGAST e |>> Left
    (Idx _ _) -> Right <$> propagateDown e
  where

  -- | Depth first pass calculating scores for each language. Alternates with
  -- scoresSExpr.
  scoreAnnoS
    :: RState
    -> AnnoS (Indexed Type) Many Int
    -> MorlocMonad (AnnoS (Indexed Type) Many (Indexed [(Lang, Int)]))
  scoreAnnoS rstat (AnnoS gi ci e) = do
    (e', ci') <- scoreExpr rstat (e, ci)
    return $ AnnoS gi ci' e'

  -- | Alternates with scoresAnnoS, finds the best score for each language at
  -- application nodes.
  scoreExpr
    :: RState
    -> (ExprS (Indexed Type) Many Int, Int)
    -> MorlocMonad (ExprS (Indexed Type) Many (Indexed [(Lang, Int)]), Indexed [(Lang, Int)])
  scoreExpr rstat (AccS k x, i) = do
    x' <- scoreAnnoS rstat x
    return (AccS k x', Idx i (scoresOf x'))
  scoreExpr rstat (LstS xs, i) = do
    (xs', best) <- scoreMany rstat xs
    return (LstS xs', Idx i best)
  scoreExpr rstat (TupS xs, i) = do
    (xs', best) <- scoreMany rstat xs
    return (TupS xs', Idx i best)
  scoreExpr rstat (LamS vs x, i) = do
    x' <- scoreAnnoS (updateRState vs rstat) x
    return (LamS vs x', Idx i (scoresOf x'))
  scoreExpr rstat (AppS f xs, i) = do

    -- store all applied arguments
    -- these may be bound to lambdas within f
    -- they are required for resolving the application language
    let rstat' = rstat { rLangs = [], rApplied = xs }

    f' <- scoreAnnoS rstat' f

    -- best scores for each language for f
    let scores = scoresOf f'
        rstat'' = emptyRState { rLangs = unique $ map fst scores }

    xs' <- mapM (scoreAnnoS rstat'') xs

    -- [[(Lang, Int)]] : where Lang is unique within each list and Int is minimized
    let pairss = [minPairs pairs | AnnoS _ (Idx _ pairs) _ <- xs']
    let best = scoreApp scores pairss

    return (AppS f' xs', Idx i best)
  scoreExpr rstat (NamS rs, i) = do
    (xs, best) <- scoreMany rstat (map snd rs)
    return (NamS (zip (map fst rs) xs), Idx i best)
  -- non-recursive expressions
  scoreExpr rstat (UniS, i) = return (UniS, zipLang i rstat)

  scoreExpr rstat (VarS v (Many xs), i) = do
    (xs', best) <- scoreMany rstat xs
    return (VarS v (Many xs'), Idx i best)

  scoreExpr rstat (BndS v, i) = do
    case Map.lookup v (rBndVars rstat) of
        (Just e@(AnnoS (Idx _ (FunT _ _)) _ _)) -> do
            scores <- scoreAnnoS rstat e |>> scoresOf
            return (BndS v, Idx i scores)
        _ -> return (BndS v, zipLang i rstat)

  scoreExpr _     (ExeS x@(SrcCall src), i) = return (ExeS x, Idx i [(srcLang src, callCost src)])
  scoreExpr rstat (ExeS x@(PatCall   _), i) = return (ExeS x, zipLang i rstat)
  scoreExpr rstat (RealS x, i) = return (RealS x, zipLang i rstat)
  scoreExpr rstat (IntS x, i) = return (IntS x, zipLang i rstat)
  scoreExpr rstat (LogS x, i) = return (LogS x, zipLang i rstat)
  scoreExpr rstat (StrS x, i) = return (StrS x, zipLang i rstat)


  -- calculate the score for an application based on the score of the function
  -- and the scores of the arguments
  scoreApp
    :: [ ( Lang -- the language of the ith calling function implementation
         , Int  -- the score of the ith implementation
         )]
    -> [[( Lang -- the language of the jth implementation of the kth argument
         , Int  -- the score of the jth implementation of the kth argument
         )]]
    -> [(Lang, Int)]
  -- if nothing is known, nothing is returned
  scoreApp [] (concat -> []) = []
  -- if none of the arguments are language-specific, the scores are based only
  -- on the functions
  scoreApp scores (concat -> []) = scores
  -- if the function is not language-specific, calculate the cost of calling
  -- all arguments from each possible language context
  scoreApp [] pairss =
    let score = [(lang, 0) | lang <- unique $ map fst (concat pairss)]
    in scoreApp score pairss
  -- if arguments and function have implementations, calculate cost relative to
  -- each function implementation
  scoreApp scores pairss =
    [ (l1, s1 + sum [ minimumDef 999999999 [s2 + Lang.pairwiseCost l1 l2 | (l2, s2) <- pairs]
                    | pairs <- pairss
                    ]
      )
    | (l1, s1) <- scores
    ]


  updateRState :: [EVar] -> RState -> RState
  updateRState [] rstat = rstat
  updateRState _ rstat@(RState _ [] _) = rstat
  updateRState (v:vs) rstat@(RState _ (p:ps) bound) = updateRState vs $
    rstat { rApplied = ps, rBndVars = Map.insert v p bound }

  zipLang :: Int -> RState -> Indexed [(Lang, Int)]
  zipLang i (rLangs -> langs) = Idx i (zip langs (repeat 0))

  scoresOf :: AnnoS a Many (Indexed [(Lang, Int)]) -> [(Lang, Int)]
  scoresOf (AnnoS _ (Idx _ xs) _) = minPairs xs

  -- find the scores of all implementations from all possible language contexts
  scoreMany
    :: RState
    -> [AnnoS (Indexed Type) Many Int]
    -> MorlocMonad ([AnnoS (Indexed Type) Many (Indexed [(Lang, Int)])], [(Lang, Int)])
  scoreMany rstat xs0 = do
    xs1 <- mapM (scoreAnnoS rstat) xs0
    return (xs1, scoreMany' xs1)
    where
      scoreMany' :: [AnnoS (Indexed Type) Many (Indexed [(Lang, Int)])] -> [(Lang, Int)]
      scoreMany' xs =
        let pairss = [ (minPairs . concat) [xs' | (AnnoS _ (Idx _ xs') _) <- xs] ]
            langs' = unique (rLangs rstat <> concatMap (map fst) pairss)
        -- Got 10 billion nodes in your AST? I didn't think so, so don't say my sentinal's ugly.
        in [(l1, sum [ minimumDef 999999999 [ score + Lang.pairwiseCost l1 l2
                               | (l2, score) <- pairs]
                     | pairs <- pairss])
           | l1 <- langs']


  collapseAnnoS
    :: Maybe Lang
    -> AnnoS (Indexed Type) Many (Indexed [(Lang, Int)])
    -> MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)))
  collapseAnnoS l1 (AnnoS gi@(Idx _ gt) ci e) = do
    (e', ci') <- collapseExpr gt l1 (e, ci)
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
    :: Type
    -> Maybe Lang -- the language of the parent expression (if Nothing, then this is a GAST)
    -> (ExprS (Indexed Type) Many (Indexed [(Lang, Int)]), Indexed [(Lang, Int)])
    -> MorlocMonad (ExprS (Indexed Type) One (Indexed (Maybe Lang)), Indexed (Maybe Lang))

  -- This case should be caught earlier
  collapseExpr _ _ (VarS v (Many []), _)
    = error $ "Unreachable case found - no implementations for '" <> show v <> "'"
  -- Select one implementation for the given term
  collapseExpr gt l1 (VarS v (Many xs), Idx i _) = do
    let minXs = minsBy (\(AnnoS _ (Idx _ ss) _) -> minimumMay [cost l1 l2 s | (l2, s) <- ss]) xs
    (x, lang) <- case minXs of
      [] -> MM.throwError . GeneratorError . render $
             "No implementation found for" <+> squotes (pretty v)
      [x] -> handleOne x
      choices -> mapM handleOne choices >>= handleMany gt
    return (VarS v (One x), Idx i lang)
    where
      handleOne
        :: AnnoS (Indexed Type) Many (Indexed [(Lang, Int)])
        -> MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)), Maybe Lang)
      handleOne x@(AnnoS _ (Idx _ ss) _) = do
        let newLang = fmap fst (minBy (biasedCost l1) ss)
        x' <- collapseAnnoS newLang x
        return (x', newLang)

      handleMany
        :: Type
        -> [(AnnoS (Indexed Type) One (Indexed (Maybe Lang)), Maybe Lang)]
        -> MorlocMonad (AnnoS (Indexed Type) One (Indexed (Maybe Lang)), Maybe Lang)
      handleMany gt' xs' =
        -- select instances that exactly match the unevaluated general type
        --
        -- WARNING: this is an oversimplification, a temporary solution, I will
        -- update it when I find a breaking case.
        case [x | x@(AnnoS (Idx _ t) _ _, _) <- xs', gt' == t] of
          [] -> do
            gscope <- MM.getGeneralScope i
            case TE.reduceType gscope (type2typeu gt') of
              (Just gt'') -> handleMany (typeOf gt'') xs'
              Nothing -> MM.throwError . GeneratorError . render $
                "I couldn't find implementation for" <+> squotes (pretty v) <+> "gt' = " <+> pretty gt'
          [x'] -> return x'

          (x':_) -> return x'

          ----- NOTE: Some cases are inseperable, the code above does not
          ----- account for this, which may allow incorrect code to be
          ----- generated.
          -- xs' ->  MM.throwError . InseperableDefinitions . render
          --   $ "no rule to separate the following sourced functions of type" <+> parens (pretty gt)":\n"
          --   <> indent 2 (vsep [ "*" <+> pretty t <+> ":" <+> pretty y | y@(AnnoS (Idx _ t) _ _, _)  <- xs'])

  -- Propagate downwards
  collapseExpr _ l1 (AccS k x, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    x' <- collapseAnnoS lang x
    return (AccS k x', Idx i lang)
  collapseExpr _ l1 (LstS xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseAnnoS lang) xs
    return (LstS xs', Idx i lang)
  collapseExpr _ l1 (TupS xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseAnnoS lang) xs
    return (TupS xs', Idx i lang)
  collapseExpr _ l1 (LamS vs x, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    x' <- collapseAnnoS lang x
    return (LamS vs x', Idx i lang)
  collapseExpr _ l1 (AppS f xs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    f' <- collapseAnnoS lang f
    xs' <- mapM (collapseAnnoS lang) xs
    return (AppS f' xs', Idx i lang)
  collapseExpr _ l1 (NamS rs, Idx i ss) = do
    lang <- chooseLanguage l1 ss
    xs' <- mapM (collapseAnnoS lang . snd) rs
    return (NamS (zip (map fst rs) xs'), Idx i lang)
  -- collapse leaf expressions
  collapseExpr _ _ (ExeS x@(SrcCall src), Idx i _) = return (ExeS x, Idx i (Just (srcLang src)))
  collapseExpr _ lang (ExeS x@(PatCall _), Idx i _) = return (ExeS x, Idx i lang)
  collapseExpr _ lang (BndS v,   Idx i _) = return (BndS v,   Idx i lang)
  collapseExpr _ lang (UniS,   Idx i _) = return (UniS,   Idx i lang)
  collapseExpr _ lang (RealS x, Idx i _) = return (RealS x, Idx i lang)
  collapseExpr _ lang (IntS x, Idx i _) = return (IntS x, Idx i lang)
  collapseExpr _ lang (LogS x, Idx i _) = return (LogS x, Idx i lang)
  collapseExpr _ lang (StrS x, Idx i _) = return (StrS x, Idx i lang)

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

  minsBy :: Ord b => (a -> b) -> [a] -> [a]
  minsBy _ [] = []
  minsBy f (x:xs) = snd $ minsBy' (f x, [x]) xs where
    minsBy' (best, grp) [] = (best, grp)
    minsBy' (best, grp) (y:ys) = minsBy' (newSet (f y)) ys
      where
        newSet newScore
          | newScore == best = (best, y:grp)
          | newScore < best = (newScore, [y])
          | otherwise = (best, grp)


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
        (ExeS x) -> return (ExeS x)
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
