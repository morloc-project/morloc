{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Share
Description : Evaluate each named value once, where its uses need it
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A value bound by name -- a parameterless @where@ binding, a top-level constant,
a @let@ -- is evaluated at most once per evaluation of the scope that binds it,
at the nearest point every use passes through:

* a use in one arm of a conditional (or one implementation of a term, or the
  argument of @\@try@) is served in that arm; uses in several exclusive arms
  are each served in their own arm;
* a lambda or a do-block does not delay it: a use inside one counts as a use
  where the lambda or do-block is built, so the value is computed once there
  and captured;
* a @where@ binding or top-level constant nothing uses is never evaluated;
  a @let@ nothing uses is evaluated where it is written.

A constant -- a value a definition computes without reading its parameters or
other local values ('Morloc.Frontend.Restructure.markConstants') -- is
evaluated once per command, like a top-level one: the copies of it in one
command, whichever function or recursion they came from, are one group.

Treeify expands a term at each place it is named, so the uses of one binding
are several copies of it. Here the copies of a data-typed binding (a type with
no function or suspension in it) within one evaluation of its scope are
replaced by one variable, bound at the placement point by a beta-redex
@(\\x -> region) rhs@. Beta-reduction then applies the ordinary rule for an
argument: a value is substituted, anything else is bound once
('Morloc.CodeGenerator.Value').

A @where@ binding's scope is one expansion of the definition that owns it;
a top-level constant's scope is one command. A pure @let@ is moved into the
regions its uses lie in; it is never moved above anything else, and a let
that performs an effect, a do-block's discarded statement, and a pattern
check's guard are never moved.
-}
module Morloc.Frontend.Share
  ( shareBindings
  , configured
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Morloc.BaseTypes as BT
import Morloc.CodeGenerator.Value (etaParts, isSuspension, isValue)
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM

type Node = AnnoS (Indexed Type) Many Int

-- | Share the named values of one export's tree.
shareBindings :: Node -> MorlocMonad Node
shareBindings root = do
  env <- shareEnv
  root' <- bottomUp env root
  -- top-level constants, and constants inside definitions: one evaluation
  -- per command
  mapRegions (placeGroups env (termKey env (cafCandidate env))) root'
    >>= mapRegions (placeGroups env (originKey env))

data ShareEnv = ShareEnv
  { seTermOf :: Int -> Maybe Int
  , seOwner :: Map.Map Int Int
  , seMethod :: Int -> Bool
  , seOwners :: Set.Set Int
  -- ^ the terms that own at least one where-binding
  , seLabeled :: Int -> Bool
  -- ^ an index carrying manifold configuration
  , seOrigin :: Map.Map Int Int
  -- ^ the constant an expression is a copy of, by index
  }

shareEnv :: MorlocMonad ShareEnv
shareEnv = do
  GMap idmap sigmap <- MM.gets stateSignatures
  owners <- MM.gets stateWhereOwner
  config <- MM.gets stateManifoldConfig
  origins <- MM.gets stateConstantOrigin
  let isMethod k = case Map.lookup k sigmap of
        Just (Polymorphic {}) -> True
        _ -> False
  return ShareEnv
    { seTermOf = \i -> Map.lookup i idmap
    , seOwner = owners
    , seMethod = isMethod
    , seOwners = Set.fromList (Map.elems owners)
    , seLabeled = \i -> maybe False configured (Map.lookup i config)
    , seOrigin = origins
    }

-- | A configuration that sets anything: a label, caching, logging, remote
-- resources.
configured :: ManifoldConfig -> Bool
configured cfg =
  isJust (manifoldConfigLabel cfg)
    || isJust (manifoldConfigRemote cfg)
    || any (== Just True) [manifoldConfigCache cfg, manifoldConfigBenchmark cfg, manifoldConfigLog cfg]

-- | Process innermost scopes first: a definition's own where-bindings at each
-- of its expansions, and each pure let.
bottomUp :: ShareEnv -> Node -> MorlocMonad Node
bottomUp env n0 = do
  AnnoS g c e <- descend n0
  case e of
    VarS v (Many alts)
      | Just d <- termOfNode env (AnnoS g c e)
      , Set.member d (seOwners env) -> do
          alts' <- mapM (mapRegionOf (placeGroups env (termKey env (ownedBy env d)))) alts
          return (AnnoS g c (VarS v (Many alts')))
    LetS v e1 e2 -> sinkLet (AnnoS g c (LetS v e1 e2))
    _ -> return (AnnoS g c e)
  where
    descend (AnnoS g c e) = AnnoS g c <$> mapExprSM (bottomUp env) e

-- | The regions of an export root: the body of each implementation, under
-- the command's own parameters.
mapRegions :: (Node -> MorlocMonad Node) -> Node -> MorlocMonad Node
mapRegions f (AnnoS g c (VarS v (Many alts))) = AnnoS g c . VarS v . Many <$> mapM (mapRegionOf f) alts
mapRegions f n = mapRegionOf f n

-- | Apply to the region under a definition's own parameters. A lambda the
-- body returns is not a parameter list: a value it captures is computed once
-- per call of the definition.
mapRegionOf :: (Node -> MorlocMonad Node) -> Node -> MorlocMonad Node
mapRegionOf f (AnnoS g c (LamS vs body)) = AnnoS g c . LamS vs <$> f body
mapRegionOf f n = f n

-- | The term a variable node is a copy of, by its concrete index: an
-- implementation's root keeps the index of the use it was expanded at as its
-- general index.
termOfNode :: ShareEnv -> Node -> Maybe Int
termOfNode env (AnnoS _ ci (VarS _ _)) = seTermOf env ci
termOfNode _ _ = Nothing

-- | A where-bound term owned by term @d@.
ownedBy :: ShareEnv -> Int -> EVar -> Int -> Bool
ownedBy env d _ k = Map.lookup k (seOwner env) == Just d

-- | A top-level term that is not a class method. Local names are renamed
-- with a backtick, which no top-level name contains.
cafCandidate :: ShareEnv -> EVar -> Int -> Bool
cafCandidate env (EV v) k = not (T.any (== '`') v) && not (seMethod env k)

nodeIndex :: Node -> Int
nodeIndex (AnnoS (Idx gi _) _ _) = gi

-- | The group of a copy of an eligible term: the term and its type.
termKey :: ShareEnv -> (EVar -> Int -> Bool) -> Node -> Maybe (Int, Type)
termKey env eligible u@(AnnoS (Idx gi t) ci (VarS v (Many alts)))
  | Just k <- termOfNode env u
  , eligible v k
  , isData t || (isFunctionValued t && any (not . isValue) alts)
  , not (null alts)
  , all (not . isLambda) alts
  , not (seLabeled env gi || seLabeled env ci) =
      Just (k, t)
termKey _ _ _ = Nothing

-- | The group of a copy of a constant a definition computes: the constant
-- and its type (a definition used at two types computes two values).
originKey :: ShareEnv -> Node -> Maybe (Int, Type)
originKey env u@(AnnoS (Idx gi t) ci e)
  | Just o <- Map.lookup ci (seOrigin env) <|> Map.lookup gi (seOrigin env)
  , notTerm e
  , isData t || (isFunctionValued t && not (isValue u))
  , not (isLambda u)
  , not (seLabeled env gi || seLabeled env ci) =
      Just (o, t)
  where
    notTerm (VarS _ _) = False
    notTerm _ = True
originKey _ _ = Nothing

-- | Share every group of a region, users of a binding before the binding
-- they use, so a binding's uses inside another's right-hand side are seen
-- after that right-hand side has been reduced to one copy.
placeGroups :: ShareEnv -> (Node -> Maybe (Int, Type)) -> Node -> MorlocMonad Node
placeGroups _ groupOf region
  | Map.null uses = return region
  | otherwise = foldM placeGroup region order
  where
    -- every copy in the region, by its index
    uses = Map.fromList [(nodeIndex u, k) | u <- allNodes region, Just k <- [groupOf u]]
    isUse k u = lookupUse u == Just k
    -- the groups used inside a group's right-hand side (every copy of a group
    -- is the same expression)
    firstCopy = Map.fromListWith (\_ a -> a) [(k, u) | u <- allNodes region, Just k <- [lookupUse u]]
    lookupUse u = Map.lookup (nodeIndex u) uses
    inner k = case Map.lookup k firstCopy of
      Just (AnnoS _ _ e) -> Set.fromList [k' | c <- childrenOf e, u <- allNodes c, Just k' <- [lookupUse u]]
      Nothing -> Set.empty
    keys = Set.toList (Set.fromList (Map.elems uses))
    innerOf = Map.fromList [(k, inner k) | k <- keys]
    order = topo keys
    -- a group is placed once no unplaced group uses it
    topo [] = []
    topo ks = case [k | k <- ks, not (any (\k' -> k' /= k && Set.member k (Map.findWithDefault Set.empty k' innerOf)) ks)] of
      [] -> ks
      ready -> ready ++ topo (filter (`notElem` ready) ks)
    placeGroup r k = placeAt sequenced (isUse k) (bindCopies k) r
    -- bind the copies under @node@ to one fresh variable
    bindCopies k node@(AnnoS (Idx gi tNode) ci _) =
      case copiesUnder k node of
        [] -> return node
        cs@(rhs@(AnnoS (Idx _ tx) _ _) : _)
          -- a recursive call in a copy must stay under the term it calls
          | any (escapes node) cs -> return node
          | otherwise -> do
              x <- freshName rhs
              node' <- replaceNodesM (isUse k) (bndFor x) node
              gBody <- plainIndex gi
              cBody <- plainIndex ci
              gLam <- plainIndex gi
              cLam <- plainIndex ci
              let body = case node' of AnnoS (Idx _ t') _ e' -> AnnoS (Idx gBody t') cBody e'
                  lam = AnnoS (Idx gLam (FunT [tx] tNode)) cLam (LamS [x] body)
              return (AnnoS (Idx gi tNode) ci (AppS lam [rhs]))
    copiesUnder k n = [u | u <- allNodes n, isUse k u]
    bndFor x (AnnoS (Idx gu tu) cu _) = do
      gu' <- plainIndex gu
      cu' <- plainIndex cu
      return (AnnoS (Idx gu' tu) cu' (BndS x))
    -- the term a copy calls back to lies strictly below the placement point
    escapes node copy =
      let targets = Set.difference (callNames copy) (varNames copy)
       in not (Set.null targets) && not (Set.null (Set.intersection targets (varNames' node)))
    callNames n = Set.fromList [v | AnnoS _ _ (CallS v) <- allNodes n]
    varNames n = Set.fromList [v | AnnoS _ _ (VarS v _) <- allNodes n]
    varNames' (AnnoS _ _ e) = Set.unions (map varNames (childrenOf e))

-- | A pure let: moved into the exclusive regions its uses lie in, but never
-- past an effect: it stays ordered with the statements of its do-block. A let
-- nothing uses stays where it is.
sinkLet :: Node -> MorlocMonad Node
sinkLet n@(AnnoS _ _ (LetS v e1 e2))
  | sequencing v || not (isPure e1) = return n
  -- a let nothing reads still runs where it is written: the function it
  -- calls may act in ways no type records
  | not (any isUse (allNodes e2)) = return n
  -- evaluating a value does nothing, so where it is bound does not matter
  | isValue e1 = return n
  -- a function is built where it is written, like a term whose
  -- implementations are functions
  | isFunctionValued (nodeType e1) = return n
  -- moving it without reaching a region would not change when it runs
  | not (reachesRegion sequenced isUse e2) = return n
  | otherwise = placeAt sequenced isUse bindHere e2
  where
    isUse (AnnoS _ _ (LetBndS v')) = v' == v
    isUse _ = False
    -- each region served gets its own copy of the right-hand side
    bindHere r@(AnnoS (Idx gi t) ci _) = do
      e1' <- reindex e1
      gi' <- plainIndex gi
      ci' <- plainIndex ci
      return (AnnoS (Idx gi' t) ci' (LetS v e1' r))
sinkLet n = return n

-- | A let that orders an effect: nothing is placed past it.
sequenced :: Node -> Bool
sequenced (AnnoS _ _ (LetS w r _)) = sequencing w || not (isPure r)
sequenced _ = False

-- | A do-block's discarded statement and a pattern check's guard bind a name
-- nothing reads; they are evaluated for their effect or their throw.
sequencing :: EVar -> Bool
sequencing (EV v) = T.isPrefixOf BT.doDiscardPrefix v || T.isPrefixOf BT.doGuardPrefix v

-- | No effect is performed evaluating it (forces under a lambda or a
-- do-block are performed when those are run, not here).
isPure :: Node -> Bool
isPure (AnnoS _ _ e) = case e of
  EvalS _ -> False
  LamS _ _ -> True
  DoBlockS _ -> True
  _ -> all isPure (childrenOf e)

-- | Place a binding for the uses (@isUse@) below @node@ at the nearest point
-- they all pass through, per exclusive region, and not below a node @stop@
-- holds for; @bind@ builds the binding at a placement point.
placeAt :: (Node -> Bool) -> (Node -> Bool) -> (Node -> MorlocMonad Node) -> Node -> MorlocMonad Node
placeAt stop isUse bind = place
  where
    has n = any isUse (allNodes n)
    place n@(AnnoS g c e)
      | isUse n || stop n = bind n
      | otherwise = case e of
          LamS _ _ -> bind n
          DoBlockS _ -> bind n
          -- building a suspension runs nothing; its body runs when forced
          AppS _ _ | isSuspension (nodeType n) -> bind n
          -- an applied lambda binds its parameters and runs its body now
          AppS lam@(AnnoS gl cl (LamS vs body)) args
            | not (any has args) -> do
                body' <- place body
                return (AnnoS g c (AppS (AnnoS gl cl (LamS vs body')) args))
            | [_] <- filter has args, not (has body) ->
                AnnoS g c . AppS lam <$> mapM (\x -> if has x then place x else return x) args
            | otherwise -> bind n
          AppS f _ | has f -> bind n
          IfS cond th el
            | has cond -> bind n
            | otherwise -> do
                th' <- if has th then place th else return th
                el' <- if has el then place el else return el
                return (AnnoS g c (IfS cond th' el'))
          -- a term whose implementations are functions is built where it
          -- is named, like a lambda; data implementations are exclusive
          -- regions
          VarS _ (Many alts)
            | any isLambda alts -> bind n
          VarS v (Many alts) -> AnnoS g c . VarS v . Many <$> mapM (\a -> if has a then place a else return a) alts
          IntrinsicS IntrTry [arg] -> do
            arg' <- placeTry arg
            return (AnnoS g c (IntrinsicS IntrTry [arg']))
          _ -> case filter has (childrenOf e) of
            [_] -> AnnoS g c <$> mapExprSM (\x -> if has x then place x else return x) e
            _ -> bind n
    -- @try runs its argument once, now: a do-block directly under it is a
    -- region, not a delay
    placeTry a@(AnnoS g c (DoBlockS body))
      | has body = AnnoS g c . DoBlockS <$> place body
      | otherwise = return a
    placeTry a = place a

-- | Whether 'placeAt' with the same @stop@ and @isUse@ would place anything
-- inside an exclusive region (a conditional arm, one implementation of a
-- data term, the argument of @try).
reachesRegion :: (Node -> Bool) -> (Node -> Bool) -> Node -> Bool
reachesRegion stop isUse = go
  where
    has n = any isUse (allNodes n)
    go n@(AnnoS _ _ e)
      | isUse n || stop n = False
      | otherwise = case e of
          LamS _ _ -> False
          DoBlockS _ -> False
          AppS _ _ | isSuspension (nodeType n) -> False
          AppS (AnnoS _ _ (LamS _ body)) args
            | not (any has args) -> go body
            | [x] <- filter has args, not (has body) -> go x
            | otherwise -> False
          AppS f _ | has f -> False
          IfS cond _ _ -> not (has cond)
          VarS _ (Many alts) -> not (any isLambda alts)
          IntrinsicS IntrTry [_] -> True
          _ -> case filter has (childrenOf e) of
            [x] -> go x
            _ -> False

-------------------------------------------------------------------------------
-- Tree utilities

childrenOf :: ExprS (Indexed Type) Many Int -> [Node]
childrenOf e = foldExprS (: []) e

allNodes :: Node -> [Node]
allNodes n@(AnnoS _ _ e) = n : concatMap allNodes (childrenOf e)

replaceNodesM :: (Node -> Bool) -> (Node -> MorlocMonad Node) -> Node -> MorlocMonad Node
replaceNodesM p f n@(AnnoS g c e)
  | p n = f n
  | otherwise = AnnoS g c <$> mapExprSM (replaceNodesM p f) e

nodeType :: Node -> Type
nodeType (AnnoS (Idx _ t) _ _) = t

-- | A lambda, not counting a partial application the typechecker wrote as
-- one ('etaParts'): that computes its function when evaluated.
isLambda :: Node -> Bool
isLambda n@(AnnoS _ _ (LamS _ _)) = case etaParts n of
  Just _ -> False
  Nothing -> True
isLambda _ = False

-- | A function type; one built by a computation is shared like data.
isFunctionValued :: Type -> Bool
isFunctionValued (FunT _ _) = True
isFunctionValued _ = False

-- | A type with no function or suspension anywhere in it.
isData :: Type -> Bool
isData t = case t of
  FunT _ _ -> False
  EffectT _ _ -> False
  UnkT _ -> False
  AppT h ts -> all isData (h : ts)
  NamT _ _ ps rs -> all isData ps && all (isData . snd) rs
  OptionalT x -> isData x
  _ -> True

freshName :: Node -> MorlocMonad EVar
freshName (AnnoS _ _ (VarS v _)) = do
  k <- MM.getCounter
  return (EV (unEVar v <> "`s" <> T.pack (show k)))
freshName _ = do
  k <- MM.getCounter
  return (EV ("shared`s" <> T.pack (show k)))

-- | A fresh index carrying @parent@'s state except its manifold
-- configuration and export membership: a node introduced here computes
-- nothing the user labeled and is not a command.
plainIndex :: Int -> MorlocMonad Int
plainIndex parent = do
  i <- newIndex parent
  MM.modify (\s -> s { stateManifoldConfig = Map.delete i (stateManifoldConfig s)
                     , stateExports = filter (/= i) (stateExports s) })
  return i

-- | A copy of a subtree with fresh indices, for a right-hand side served in
-- more than one region.
reindex :: Node -> MorlocMonad Node
reindex (AnnoS (Idx gi t) ci e) = do
  gi' <- newIndex gi
  ci' <- newIndex ci
  AnnoS (Idx gi' t) ci' <$> mapExprSM reindex e
