{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.StaticArgs
Description : Specialize recursive helpers on constant function arguments
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A recursive helper that takes a function argument and passes it unchanged to
every recursive call (a static argument) receives that function as a value:
a closure its manifold is handed at entry. A closure value is heavier than the
function it names. It is a trait object in Rust, where it cannot be shared
between threads, so a sourced function that runs it on several threads cannot
take it; and it is a proxy that calls back into the pool when the helper is
entered through its serial interface, one round trip per application.

When a call site passes a closed function (a sourced function, or a lambda
with no free variables) at such a position, this pass gives the call its own
copy of the helper with that function written in place of the parameter and
the parameter removed. The copy refers to the function directly, as a
non-recursive body would.

Declining is always sound, so the pass declines whatever it does not
recognize: a helper whose back-edges do not all pass the parameter through,
a helper whose type does not show its inputs, a helper with a stage entry or
manifold configuration (a label or a cache is keyed to the helper's own
manifold), and any argument that is not a closed value. A partial application
is not specialized even when closed: written into the body it would be
recomputed at every iteration.

Copies are bounded. A copy is never specialized on a helper it is already a
copy of (mutually recursive helpers would otherwise copy each other forever),
calls passing the same sourced function share one copy, and a helper has at
most 'maxCopies' copies.
-}
module Morloc.CodeGenerator.StaticArgs
  ( specializeStaticArgs
  ) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.LambdaEval (applyPoolLambdas, reindexTree)
import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)

type Tree = AnnoS (Indexed Type) One (Indexed Lang)

-- | A self-recursive helper that may be specialized: its parameters, and the
-- positions every back-edge passes through unchanged.
data Helper = Helper
  { hTree :: Tree
  , hParams :: [EVar]
  , hStatic :: Set.Set Int
  }

-- | Copies already made: by helper, the number made, and the copy for each
-- set of fixed sourced functions.
data Copies = Copies
  { cCount :: Map.Map EVar Int
  , cShared :: Map.Map (EVar, [(Int, Source)]) EVar
  }

-- | The most copies one helper is given; further calls keep the original.
maxCopies :: Int
maxCopies = 32

-- | Specialize every call of a recursive helper that passes a closed function
-- at a static position. New copies are processed in turn, so a copy that
-- calls another helper with a closed function is specialized too.
specializeStaticArgs :: [Tree] -> MorlocMonad [Tree]
specializeStaticArgs trees0 = do
  helpers <- helperTable trees0
  if Map.null helpers
    then return trees0
    else do
      copiesRef <- MM.liftIO (newIORef (Copies Map.empty Map.empty))
      work <- mapM (\t -> selfName t >>= \s -> return (t, maybe Set.empty Set.singleton s)) trees0
      trees1 <- go helpers copiesRef [] work
      dropUncalled helpers trees1
  where
    -- each work item carries the helpers it is a copy of
    go _ _ done [] = return (reverse done)
    go helpers ref done ((t, chain) : rest) = do
      self <- selfName t
      (t', copies) <- rewriteSites helpers ref chain self t
      go helpers ref (t' : done) (rest <> copies)

-- | Drop the helpers no other tree calls any more, repeatedly: a helper whose
-- every call was specialized, and then whatever only it called. The dropped
-- helpers' closure parameters are the form the copies exist to avoid.
dropUncalled :: Map.Map EVar Helper -> [Tree] -> MorlocMonad [Tree]
dropUncalled helpers trees = do
  named <- mapM (\t -> (,) <$> selfName t <*> pure t) trees
  let calledFrom = Map.fromListWith Set.union
        [ (w, maybe Set.empty Set.singleton self)
        | (self, t) <- named, w <- callsIn t ]
      externallyCalled v = maybe False (not . Set.null . Set.delete v) (Map.lookup v calledFrom)
      droppable (Just v, _) = (Map.member v helpers || isCopy v) && not (externallyCalled v)
      droppable _ = False
      kept = [t | n@(_, t) <- named, not (droppable n)]
  if length kept == length trees
    then return trees
    else dropUncalled helpers kept
  where
    isCopy v = "@static" `MT.isInfixOf` unEVar v

-- | The names a tree calls.
callsIn :: Tree -> [EVar]
callsIn (AnnoS _ _ e) = case e of
  CallS w -> [w]
  _ -> concat (foldExprS (\x -> [callsIn x]) e)

-- | The helper this tree defines, if it is one: its back-edges are calls to
-- itself and are not specialization sites.
selfName :: Tree -> MorlocMonad (Maybe EVar)
selfName (AnnoS (Idx gi _) _ _) = Map.lookup gi <$> MM.gets stateName

helperTable :: [Tree] -> MorlocMonad (Map.Map EVar Helper)
helperTable trees = do
  names <- MM.gets stateName
  stages <- MM.gets stateRecStages
  config <- MM.gets stateManifoldConfig
  return $ Map.fromList
    [ (v, Helper t vs static)
    | t@(AnnoS (Idx gi ht) _ (LamS vs body)) <- trees
    , Just v <- [Map.lookup gi names]
    , not (Map.member v stages)
    , not (maybe False observed (Map.lookup gi config))
    , maybe False (>= length vs) (inputCount ht)
    , Just static <- [staticPositions v vs body]
    , not (Set.null static)
    ]

-- | A manifold configuration that attaches something to the manifold's own
-- identity (a cache, a benchmark, a log line, a label, a remote placement),
-- which a copy would duplicate or detach.
observed :: ManifoldConfig -> Bool
observed c =
  manifoldConfigCache c == Just True
    || manifoldConfigBenchmark c == Just True
    || manifoldConfigLog c == Just True
    || isJust (manifoldConfigLabel c)
    || isJust (manifoldConfigRemote c)

-- | The number of inputs a function type shows, through any effect around it.
-- Nothing for anything else (an alias is not looked through: a type whose
-- inputs are not visible here cannot have one removed).
inputCount :: Type -> Maybe Int
inputCount (FunT ins _) = Just (length ins)
inputCount (EffectT _ t) = inputCount t
inputCount _ = Nothing

-- | The parameter positions every back-edge to @v@ in @body@ passes through
-- unchanged. Nothing when @v@ is not recursive or a back-edge is anything but
-- a full application (a bare or partially applied back-edge escapes as a
-- function value, and a copy could not drop its parameter).
staticPositions :: EVar -> [EVar] -> Tree -> Maybe (Set.Set Int)
staticPositions v vs body = do
  sites <- backEdges Set.empty body
  if null sites
    then Nothing
    else Just (foldr1 Set.intersection sites)
  where
    n = length vs

    -- the positions a back-edge passes through, given the names rebound
    -- around it (a rebound name is not the parameter)
    passed rebound xs =
      Set.fromList
        [ i
        | (i, p, AnnoS _ _ (BndS x)) <- zip3 [0 ..] vs xs
        , x == p
        , not (Set.member p rebound)
        ]

    backEdges :: Set.Set EVar -> Tree -> Maybe [Set.Set Int]
    backEdges rebound (AnnoS _ _ e) = case e of
      AppS (AnnoS _ _ (CallS w)) xs
        | w == v ->
            if length xs >= n
              then (passed rebound xs :) <$> concatMapM (backEdges rebound) xs
              else Nothing
      CallS w
        | w == v -> Nothing
      LamS ws b -> backEdges (Set.union rebound (Set.fromList ws)) b
      LetS w e1 e2 -> (<>) <$> backEdges rebound e1 <*> backEdges (Set.insert w rebound) e2
      _ -> concatMapM (backEdges rebound) (foldExprS (: []) e)

-- | Rewrite the specializable call sites in a tree, returning the tree and
-- the copies it now calls, each with the helpers it is a copy of.
rewriteSites ::
  Map.Map EVar Helper ->
  IORef Copies ->
  Set.Set EVar ->
  Maybe EVar ->
  Tree ->
  MorlocMonad (Tree, [(Tree, Set.Set EVar)])
rewriteSites helpers ref chain self = walk
  where
    walk (AnnoS g c e) = case e of
      AppS (AnnoS (Idx fi fT) fc (CallS h)) xs
        | Just hp <- Map.lookup h helpers
        , Just h /= self
        , not (Set.member h chain)
        , maybe False (>= length (hParams hp)) (inputCount fT)
        , fixed <- [i | i <- Set.toList (hStatic hp), i < length xs, closedFunction (xs !! i)]
        , not (null fixed)
        , length fixed < length (hParams hp) -> do
            (xs', copiesArgs) <- walkAll xs
            made <- copyFor h hp (Map.fromList [(i, xs' !! i) | i <- fixed])
            case made of
              Nothing -> return (AnnoS g c (AppS (AnnoS (Idx fi fT) fc (CallS h)) xs'), copiesArgs)
              Just (name, newCopy) -> do
                let dropped = Set.fromList fixed
                    call = AnnoS (Idx fi (dropInputs dropped fT)) fc (CallS name)
                    copies = maybe [] (\t -> [(t, Set.insert h chain)]) newCopy
                return (AnnoS g c (AppS call (dropAt dropped xs')), copies <> copiesArgs)
      _ -> do
        (e', copies) <- walkExpr e
        return (AnnoS g c e', copies)

    walkAll xs = do
      rs <- mapM walk xs
      return (map fst rs, concatMap snd rs)

    walkExpr e = do
      found <- MM.liftIO (newIORef [])
      e' <- mapExprSM (step found) e
      copies <- MM.liftIO (readIORef found)
      return (e', copies)

    step found x = do
      (x', cs) <- walk x
      MM.liftIO (modifyIORef found (<> cs))
      return x'

    -- The copy a call uses: one already made for the same sourced functions,
    -- a new one (returned so it is processed in turn), or none past the bound.
    copyFor h hp fixedArgs = do
      Copies counts shared <- MM.liftIO (readIORef ref)
      let key = (,) h <$> mapM (\(i, a) -> (,) i <$> sourceOf a) (Map.toList fixedArgs)
      case key >>= (`Map.lookup` shared) of
        Just name -> return (Just (name, Nothing))
        Nothing
          | Map.findWithDefault 0 h counts >= maxCopies -> return Nothing
          | otherwise -> do
              (name, t) <- specialize h hp fixedArgs
              MM.liftIO $ modifyIORef ref $ \cs ->
                cs { cCount = Map.insertWith (+) h 1 (cCount cs)
                   , cShared = maybe (cShared cs) (\k -> Map.insert k name (cShared cs)) key
                   }
              return (Just (name, Just t))

    sourceOf (AnnoS _ _ (ExeS (SrcCall src))) = Just src
    sourceOf _ = Nothing

-- | A function value that refers to nothing bound around it: a sourced
-- function, or a lambda with no free variables.
closedFunction :: Tree -> Bool
closedFunction (AnnoS (Idx _ t) _ e) = isFunction t && case e of
  ExeS (SrcCall _) -> True
  LamS vs body -> Set.null (freeBound (Set.fromList vs) body)
  _ -> False
  where
    isFunction (FunT _ _) = True
    isFunction _ = False

-- | Lambda- and let-bound names used in a tree and not bound inside it.
freeBound :: Set.Set EVar -> Tree -> Set.Set EVar
freeBound bs (AnnoS _ _ e) = case e of
  BndS v | not (Set.member v bs) -> Set.singleton v
  LetBndS v | not (Set.member v bs) -> Set.singleton v
  LamS vs body -> freeBound (Set.union bs (Set.fromList vs)) body
  LetS v e1 e2 -> Set.union (freeBound bs e1) (freeBound (Set.insert v bs) e2)
  _ -> Set.unions (foldExprS (\x -> [freeBound bs x]) e)

-- | A copy of helper @h@ with the arguments at the given positions written in
-- place of their parameters, which it no longer takes.
specialize :: EVar -> Helper -> Map.Map Int Tree -> MorlocMonad (EVar, Tree)
specialize h hp fixedArgs = do
  AnnoS (Idx gi t) c root <- reindexTree (hTree hp)
  let name = EV (unEVar h <> MT.pack ("@static" <> show gi))
      dropped = Map.keysSet fixedArgs
      params = hParams hp
      replacements = Map.fromList [(params !! i, a) | (i, a) <- Map.toList fixedArgs]
  body <- case root of
    LamS _ b -> substitute replacements b
    _ -> MM.throwCompilerBug "a recursive helper is not a lambda"
  let body' = renameBackEdges h name dropped body
  -- A substituted lambda the body applies is a new redex; reduce the copy as
  -- every tree was reduced before this pass.
  tree <- applyPoolLambdas (AnnoS (Idx gi (dropInputs dropped t)) c (LamS (dropAt dropped params) body'))
  MM.modify $ \s ->
    s { stateName = Map.insert gi name (stateName s)
      , stateRecursionTargets = Map.insert name gi (stateRecursionTargets s)
      }
  return (name, tree)

-- | Write a fresh copy of each replacement in place of each use of its
-- parameter. A binder of the same name hides the parameter below it.
substitute :: Map.Map EVar Tree -> Tree -> MorlocMonad Tree
substitute reps n@(AnnoS g c e)
  | Map.null reps = return n
  | otherwise = case e of
      BndS v | Just a <- Map.lookup v reps -> reindexTree a
      LamS vs body -> AnnoS g c . LamS vs <$> substitute (foldr Map.delete reps vs) body
      LetS v e1 e2 -> do
        e1' <- substitute reps e1
        e2' <- substitute (Map.delete v reps) e2
        return (AnnoS g c (LetS v e1' e2'))
      _ -> AnnoS g c <$> mapExprSM (substitute reps) e

-- | Point the back-edges to @old@ at @new@, without the dropped arguments.
renameBackEdges :: EVar -> EVar -> Set.Set Int -> Tree -> Tree
renameBackEdges old new dropped = go
  where
    go (AnnoS g c e) = case e of
      AppS (AnnoS (Idx fi fT) fc (CallS w)) xs
        | w == old ->
            AnnoS g c (AppS (AnnoS (Idx fi (dropInputs dropped fT)) fc (CallS new)) (dropAt dropped (map go xs)))
      _ -> AnnoS g c (mapExprS go e)

-- | A function type without the inputs at the given positions, through any
-- effect around it.
dropInputs :: Set.Set Int -> Type -> Type
dropInputs ds (FunT ins out) = FunT (dropAt ds ins) out
dropInputs ds (EffectT effs t) = EffectT effs (dropInputs ds t)
dropInputs _ t = t

dropAt :: Set.Set Int -> [a] -> [a]
dropAt ds xs = [x | (i, x) <- zip [0 ..] xs, not (Set.member i ds)]
