{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.HostProvenance
Description : Where each function value was made, and the pools that keeps code in
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A function made by host code has no identity outside the pool that made it
(HOST-9). This pass follows every function value through the normalized pool
trees, recording at each position of its type whether morloc made it or which
host did, and gives every node that holds or receives a host-made function the
language it must run in, a limit the placement honors (COST-15). A recursive
function given host-made functions is copied per pattern of what it is given.
-}
module Morloc.CodeGenerator.HostProvenance
  ( HostLimit (..)
  , planHostFunctions
  , chooseSourceHint
  , sameTermSources
  ) where

import Control.Monad.State (State, evalStateT, get, gets, lift, modify, put, runState)
import qualified Data.List as List
import qualified Data.Set as Set
import Morloc.CodeGenerator.EffectBoundary (sourceSignature)
import Morloc.CodeGenerator.LambdaEval (reindexTree)
import Morloc.CodeGenerator.Serial (containsFunT)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.GMap as GMap
import Morloc.Data.Doc
import Morloc.Data.Map (Map)
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import Morloc.Frontend.Namespace (newIndex)
import Morloc.Frontend.Rename (displayName)
import qualified Morloc.Monad as MM

type Tree = AnnoS (Indexed Type) One (Indexed Lang)

-- | What made a host-made function: a sourced call, or a sourced function
-- that passes its own functions to a callback.
data Maker = MadeBy Source | PassedBy Source

-- | Where the function at one position was made. Makers are carried for
-- messages only; they take no part in comparisons.
data Origin = Portable | Host Lang Maker | Conflict (Lang, Maker) (Lang, Maker)

rank :: Origin -> (Int, [Lang])
rank Portable = (0, [])
rank (Host l _) = (1, [l])
rank (Conflict (a, _) (b, _)) = (2, [a, b])

instance Eq Origin where
  a == b = rank a == rank b

instance Ord Origin where
  compare a b = compare (rank a) (rank b)

joinO :: Origin -> Origin -> Origin
joinO Portable o = o
joinO o Portable = o
joinO c@Conflict {} _ = c
joinO _ c@Conflict {} = c
joinO h@(Host l1 m1) (Host l2 m2)
  | l1 == l2 = h
  | otherwise = Conflict (l1, m1) (l2, m2)

joinsO :: [Origin] -> Origin
joinsO = foldr joinO Portable

-- | Provenance shaped like a type. A function or suspension: its own origin,
-- how many arguments its first call takes (none for a suspension), and what
-- that call returns. Data: the origin of the functions inside it that no
-- part below describes, and its parts.
data Prov = PFun Origin Int Prov | PData Origin [Prov]
  deriving (Eq, Ord)

-- | The functions a value holds, not counting those its calls return.
held :: Prov -> Origin
held (PFun o _ _) = o
held (PData o ps) = foldr (joinO . held) o ps

-- | Every function a value holds or its calls may return.
deep :: Prov -> Origin
deep (PFun o _ r) = joinO o (deep r)
deep (PData o ps) = foldr (joinO . deep) o ps

dataKids :: Type -> [Type]
dataKids (OptionalT t) = [t]
dataKids (AppT _ ts) = ts
dataKids (NamT _ _ _ fs) = map snd fs
dataKids _ = []

-- | Every function at or under a type made at one origin.
mark :: Origin -> Type -> Prov
mark o (FunT ins out) = PFun o (length ins) (mark o out)
mark o (EffectT _ t) = PFun o 0 (mark o t)
mark o t = PData (if null (dataKids t) && containsFunT t then o else Portable) (map (mark o) (dataKids t))

portable :: Type -> Prov
portable = mark Portable

-- | The type a function's first call returns.
afterCall :: Type -> Int -> Type
afterCall (FunT ins out) m
  | m < length ins = FunT (drop m ins) out
  | otherwise = out
afterCall (EffectT _ t) _ = t
afterCall t _ = t

-- | Provenance made to fit a type; a shape that does not fit becomes the
-- type with everything at the provenance's deepest origin.
fit :: Type -> Prov -> Prov
fit t@(FunT ins _) (PFun o m r) | m >= 1, m <= length ins = PFun o m (fit (afterCall t m) r)
fit (EffectT _ t) (PFun o 0 r) = PFun o 0 (fit t r)
fit t (PData o ps)
  | not (isCallable t)
  , length ps == length (dataKids t) =
      PData o (zipWith fit (dataKids t) ps)
fit t p = mark (deep p) t

joinP :: Type -> Prov -> Prov -> Prov
joinP t p q = case (fit t p, fit t q) of
  (PFun a m x, PFun b n y) | m == n -> PFun (joinO a b) m (joinP (afterCall t m) x y)
  (PData a xs, PData b ys) | length xs == length ys -> PData (joinO a b) (zipWith3 joinP (dataKids t) xs ys)
  (p', q') -> mark (joinO (deep p') (deep q')) t

joinsP :: Type -> [Prov] -> Prov
joinsP t = foldr (joinP t) (portable t)

isCallable :: Type -> Bool
isCallable FunT {} = True
isCallable EffectT {} = True
isCallable _ = False

-- | A call of a function value with @args@: what it returns, and the origin
-- of every function the call runs on the way.
applyP :: Type -> Prov -> [Prov] -> (Prov, Origin)
applyP t p [] = (fit t p, Portable)
applyP t p args = case fit t p of
  PFun o m r
    | m == 0 -> (fit t p, Portable)
    | length args < m ->
        (PFun (joinsO (o : map held args)) (m - length args) r, Portable)
    | otherwise ->
        let (rest, o') = applyP (afterCall t m) r (drop m args)
         in (rest, joinO o o')
  q -> (mark (deep q) (resultOf t (length args)), deep q)
  where
    resultOf ty 0 = ty
    resultOf (FunT ins out) k
      | k < length ins = FunT (drop k ins) out
      | otherwise = resultOf out (k - length ins)
    resultOf ty _ = ty

-- | A sourced call's result, given its arguments (HOST-9): a function in it
-- was made by the host, unless its position is a type variable of the
-- source's signature that a parameter also holds, where it is what the
-- arguments hold there. A suspension the call returns directly is morloc's;
-- the effect ends the call (HOST-3).
sourcedResult :: Origin -> Maybe TypeU -> [Prov] -> Type -> Prov
sourcedResult host sig args = top (resultU <$> sig)
  where
    sig' = flatU . unforall <$> sig
    foralls = maybe Set.empty boundVars sig
    params = case sig' of
      Just (FunU ps _) -> ps
      _ -> []
    resultU _ = case sig' of
      Just (FunU _ r) -> r
      Just r -> r
      Nothing -> VarU (TV "")
    bound = Map.fromListWith (<>) (concat (zipWith bindVars params args))
    captured = joinsO (map held args)

    top (Just (VarU v)) t | Just ps <- relayed v = joinsP t ps
    top gu (EffectT _ t) = PFun captured 0 (top (effInner =<< gu) t)
    top gu t = madeIn gu t

    madeIn (Just (VarU v)) t | Just ps <- relayed v = joinsP t ps
    madeIn _ t@FunT {} = mark host t
    madeIn _ t@EffectT {} = mark host t
    madeIn gu t =
      let ks = dataKids t
          gks = case gu of
            Just g | length (kidsU g) == length ks -> map Just (kidsU g)
            _ -> map (const Nothing) ks
       in PData (if null ks && containsFunT t then host else Portable) (zipWith madeIn gks ks)

    relayed v
      | Set.member v foralls = Map.lookup v bound
      | otherwise = Nothing

    bindVars (VarU v) p = [(v, [p])]
    bindVars (ForallU _ u) p = bindVars u p
    bindVars u@(FunU _ _) (PFun _ _ r) = case flatU u of
      FunU _ out -> bindVars out r
      _ -> []
    bindVars (EffectU _ u) (PFun _ 0 r) = bindVars u r
    bindVars u (PData _ ps)
      | length (kidsU u) == length ps = concat (zipWith bindVars (kidsU u) ps)
    bindVars _ _ = []

    effInner (EffectU _ u) = Just u
    effInner _ = Nothing

    kidsU (OptionalU u) = [u]
    kidsU (AppU _ us) = us
    kidsU (NamU _ _ _ fs) = map snd fs
    kidsU _ = []

    unforall (ForallU _ u) = unforall u
    unforall u = u

    boundVars (ForallU v u) = Set.insert v (boundVars u)
    boundVars _ = Set.empty

    flatU (FunU ps (FunU qs r)) = flatU (FunU (ps <> qs) r)
    flatU u = u

-- | What the analysis records about a tree, keyed by node.
data Found = Found
  { fReq :: Map Int Origin
  -- ^ by c index: the origin of the host-made functions a node holds or receives
  , fCalls :: Map Int (Int, [Prov])
  -- ^ by c index of a call of another tree: that tree and what it is given
  , fDemand :: Map EVar Origin
  -- ^ variables bound to a function a host will call with its own functions
  , fFloat :: Set.Set (Int, Int)
  -- ^ (c index of an application, argument position): an argument computed
  -- from a host-made function whose value is morloc's, to be let-bound
  , fBinders :: Map EVar Origin
  -- ^ binders of host-made functions
  , fSinks :: [(Int, Origin, MDoc)]
  -- ^ by g index: a host-made function that would be serialized, and why
  , fStray :: [(Int, Origin, Source)]
  -- ^ by g index: a function a host calls with its own functions whose uses
  -- of them cannot be followed
  , fUnsettled :: [Int]
  -- ^ by g index: a let whose binding's demand did not settle
  , fBranches :: [(Int, Origin)]
  -- ^ by g index: a conditional whose branches give functions made in two pools
  }

emptyFound :: Found
emptyFound = Found Map.empty Map.empty Map.empty Set.empty Map.empty [] [] [] []

data Ctx = Ctx
  { cxSig :: Source -> Maybe TypeU
  , cxTree :: EVar -> Maybe Int
  , cxArity :: Int -> Int
  , cxSummary :: (Int, [Prov]) -> (Prov, [Origin])
  , cxParamType :: Int -> [Type]
  , cxSerializes :: Int -> Bool
  }

type Env = Map EVar Prov

data Info = Info {iProv :: Prov, iFree :: Map EVar Origin}

type A = State Found

-- | The provenance of a node's value and of the host-made functions free in
-- it, recording what each node must keep. @dm@ is the origin of the
-- functions a host will call the value with: the parameters of each
-- function the value can be are bound to them.
analyze :: Ctx -> Env -> Origin -> AnnoS (Indexed Type) One (Indexed Lang) -> A Info
analyze cx env dm (AnnoS (Idx gi t) (Idx ci _) e) = case e of
  UniS -> leaf
  NullS -> leaf
  RealS _ _ -> leaf
  IntS _ _ -> leaf
  LogS _ -> leaf
  StrS _ -> leaf
  BndS v -> var v
  LetBndS v -> var v
  VarS _ (One x) -> do
    i <- analyze cx env dm x
    done (iProv i) (iFree i) [i]
  ExeS (SrcCall src) -> do
    stray
    -- a source as a value: its first call group runs the source; a call
    -- that stops at a group boundary returns a function the host made
    let p = case srcRsize src of
          (g1 : _) | g1 < arity t -> PFun Portable g1 (mark (hostOf src) (afterCall t g1))
          _ -> PFun Portable (arity t) (sourcedResult (hostOf src) (cxSig cx src) [] (afterCall t (arity t)))
    done p Map.empty []
  ExeS (PatCall _) -> stray >> leaf
  CallS v -> do
    stray
    case cxTree cx v of
      Just j -> do
        let key = (j, map portable (cxParamType cx j))
        modify (\f -> f {fCalls = Map.insert ci key (fCalls f)})
        done (PFun Portable (arity t) (fst (cxSummary cx key))) Map.empty []
      Nothing -> leaf
  AppS f xs -> stray >> app f xs
  LamS vs b -> do
    let ins = case t of
          FunT ts _ -> ts
          _ -> []
        -- a parameter already in scope was bound by the caller (a mapped
        -- element); binder names are unique within a tree
        bound = [(v, if dm == Portable then fit it (Map.findWithDefault (portable it) v env) else mark dm it) | (v, it) <- zip vs ins]
        env' = foldr (uncurry Map.insert) env bound
    binders [(v, held p) | (v, p) <- bound]
    i <- analyze cx env' Portable b
    -- a parameter the body hands to a host as a callback that takes
    -- functions would need its own arguments followed back to whoever
    -- calls this lambda, which this pass does not do
    demands <- gets fDemand
    sequence_
      [ modify (\f -> f {fStray = (gi, o, src) : fStray f})
      | v <- vs
      , Just o@(Host _ (PassedBy src)) <- [Map.lookup v demands]
      ]
    let fv = foldr Map.delete (iFree i) vs
    done (PFun (joinsO (Map.elems fv)) (length vs) (iProv i)) fv [i]
  LstS xs -> do
    is <- mapM (analyze cx env dm) xs
    let elemT = case dataKids t of
          [et] -> et
          _ -> t
    done (fit t (PData Portable [joinsP elemT (map iProv is)])) (frees is) is
  TupS xs -> do
    is <- mapM (analyze cx env dm) xs
    done (fit t (PData Portable (map iProv is))) (frees is) is
  NamS rs -> do
    is <- mapM (analyze cx env dm . snd) rs
    let byKey = Map.fromList (zip (map fst rs) (map iProv is))
        parts = case t of
          NamT _ _ _ fs -> [Map.findWithDefault (portable ft) k byKey | (k, ft) <- fs]
          _ -> map iProv is
    done (fit t (PData Portable parts)) (frees is) is
  ConS _ _ _ xs -> do
    is <- mapM (analyze cx env dm) xs
    done (PData (joinsO (map (deep . iProv) is)) (map portable (dataKids t))) (frees is) is
  LetS v a b -> do
    -- a binding a host will call with its own functions is known to be one
    -- only once the body is read, so the binding is read again with that
    let pass d = do
          ia <- analyze cx env d a
          binders [(v, held (iProv ia))]
          ib <- analyze cx (Map.insert v (iProv ia) env) dm b
          d' <- gets (Map.findWithDefault Portable v . fDemand)
          return (ia, ib, d')
    d0 <- gets (Map.findWithDefault Portable v . fDemand)
    before <- get
    (ia1, ib1, d1) <- pass d0
    (ia, ib) <-
      if d1 == d0
        then return (ia1, ib1)
        else do
          -- the first pass read the binding with the wrong demand
          demands <- gets fDemand
          put before {fDemand = demands}
          (ia2, ib2, d2) <- pass d1
          when (d2 /= d1) $ modify (\f -> f {fUnsettled = gi : fUnsettled f})
          return (ia2, ib2)
    done (iProv ib) (Map.unionWith joinO (iFree ia) (Map.delete v (iFree ib))) [ia, ib]
  IfS c a b -> do
    ic <- analyze cx env Portable c
    ia <- analyze cx env dm a
    ib <- analyze cx env dm b
    let joined = joinP t (iProv ia) (iProv ib)
        conflicted o = case o of
          Conflict {} -> True
          _ -> False
    when (conflicted (deep joined) && not (conflicted (deep (iProv ia))) && not (conflicted (deep (iProv ib)))) $
      modify (\f -> f {fBranches = (gi, deep joined) : fBranches f})
    done joined (frees [ic, ia, ib]) [ic, ia, ib]
  DoBlockS x -> do
    stray
    i <- analyze cx env Portable x
    done (PFun (joinsO (Map.elems (iFree i))) 0 (iProv i)) (iFree i) [i]
  EvalS x -> do
    stray
    i <- analyze cx env Portable x
    let (r, through) = case fit (annoType x) (iProv i) of
          PFun o 0 p -> (p, o)
          q -> (mark (deep q) t, deep q)
    done' through r (iFree i) [i]
  CoerceS _ x -> do
    i <- analyze cx env dm x
    done (fit t (iProv i)) (iFree i) [i]
  IntrinsicS intr xs -> stray >> intrinsic intr xs
  LabelS k x -> do
    i <- analyze cx env dm x
    -- a labeled call (not a labeled function) serializes what it returns
    -- and what it captures when its label caches or logs
    when (cxSerializes cx k && not (isCallable t)) $
      sink (joinsO (held (iProv i) : Map.elems (iFree i))) "the result of a labeled call that caches or logs"
    done (iProv i) (iFree i) [i]
  where
    leaf = done (portable t) Map.empty []

    var v = do
      when (dm /= Portable) $ modify (\f -> f {fDemand = Map.insertWith joinO v dm (fDemand f)})
      let p = fit t (Map.findWithDefault (portable t) v env)
          fv = if held p == Portable then Map.empty else Map.singleton v (held p)
      done p fv []

    -- a value a host calls with its own functions whose uses of them this
    -- pass cannot follow
    stray = case dm of
      Host _ (PassedBy src) | isCallable t -> modify (\f -> f {fStray = (gi, dm, src) : fStray f})
      _ -> return ()

    binders bs = modify (\f -> f {fBinders = foldr (\(v, o) -> if o == Portable then id else Map.insert v o) (fBinders f) bs})

    done p fv kids = done' Portable p fv kids

    -- the node holds @p@, receives @fv@, and runs every function in
    -- @through@ on the way
    done' through p fv kids = do
      let req = joinsO (through : held p : Map.elems fv <> map (held . iProv) kids)
      when (req /= Portable) $ modify (\f -> f {fReq = Map.insertWith joinO ci req (fReq f)})
      return (Info p fv)

    frees = Map.unionsWith joinO . map iFree

    sink o what
      | o == Portable = return ()
      | otherwise = modify (\f -> f {fSinks = (gi, o, what) : fSinks f})

    app f xs = case sourcedHead f of
      Just src -> sourcedApp f src xs
      Nothing -> case calledTree f of
        Just j -> treeApp f j xs
        Nothing -> do
          fi <- analyze cx env Portable f
          is <- mapM (analyze cx env Portable) xs
          floats xs is
          let (r, through) = case f of
                AnnoS _ _ (ExeS (PatCall _)) -> (mark (joinsO (map (held . iProv) is)) t, Portable)
                _ -> applyP (annoType f) (iProv fi) (map iProv is)
          labeled f is r
          done' through (fit t r) (frees (fi : is)) (fi : is)

    sourcedApp f src xs = do
      fi <- analyze cx env Portable f
      let ft = annoType f
          ins = case ft of
            FunT ts _ -> ts
            _ -> []
          -- a callback the host calls with functions
          passes pt = case pt of
            FunT pins _ -> any (\it -> containsFunT it || isCallable it) pins
            _ -> False
          given = Host (srcLang src) (PassedBy src)
      is <- sequence [analyze cx env (if passes pt then given else Portable) x | (x, pt) <- zip xs (ins <> repeat t)]
      floats xs is
      let m = arity ft
          k = length xs
          host = hostOf src
          groups = scanl1 (+) (srcRsize src)
          full = sourcedResult host (cxSig cx src) (map iProv is) (afterCall ft m)
          (r, through)
            | k < m && k `elem` groups = (mark host (afterCall ft k), Portable)
            | k < m = (PFun (joinsO (map (held . iProv) is)) (m - k) full, Portable)
            | otherwise = applyP (afterCall ft m) full (map iProv (drop m is))
      labeled f is r
      done' through (fit t r) (frees (fi : is)) (fi : is)

    treeApp f j xs = do
      before <- get
      is0 <- mapM (analyze cx env Portable) xs
      let m = cxArity cx j
          pts = cxParamType cx j
          keyOf is = (j, normalKey pts [fit pt p | (pt, p) <- zip pts (map iProv is <> map portable (drop (length is) pts))])
          (_, demands) = cxSummary cx (keyOf is0)
      is <-
        if all (== Portable) demands
          then return is0
          else do
            -- the arguments were read without the demand the callee puts on them
            found <- gets fDemand
            put before {fDemand = found}
            sequence [analyze cx env d x | (x, d) <- zip xs (demands <> repeat Portable)]
      floats xs is
      let key = keyOf is
          (res, _) = cxSummary cx key
          k = length xs
          (r, through)
            | k < m = (PFun (joinsO (map (held . iProv) is)) (m - k) res, Portable)
            | otherwise = applyP (afterCall (annoType f) m) res (map iProv (drop m is))
      modify (\fd -> fd {fCalls = Map.insert (cIdx f) key (fCalls fd)})
      done' through (fit t r) (frees is) is

    -- an argument computed from a host-made function, whose value morloc
    -- made, is bound by a let before the call so that only its value crosses
    floats xs is =
      let sites = [(ci, k) | (k, x, i) <- zip3 [0 :: Int ..] xs is, floatable x, held (iProv i) == Portable, not (Map.null (iFree i))]
       in unless (null sites) $ modify (\f -> f {fFloat = foldr Set.insert (fFloat f) sites})

    floatable (AnnoS _ _ x) = case x of
      LamS _ _ -> False
      BndS _ -> False
      LetBndS _ -> False
      _ -> True

    -- a labeled call that caches or logs serializes its arguments and result
    labeled f is r = case labelOf f of
      Just k | cxSerializes cx k -> sink (joinsO (held r : map (held . iProv) is)) "an argument or the result of a labeled call that caches or logs"
      _ -> return ()

    labelOf (AnnoS _ _ (LabelS k _)) = Just k
    labelOf (AnnoS _ _ (VarS _ (One x))) = labelOf x
    labelOf _ = Nothing

    intrinsic intr xs = case (intr, xs) of
      (_, [f, c]) | intr `elem` [IntrMap, IntrMapOptional] -> do
        ic <- analyze cx env Portable c
        let element = case fit (annoType c) (iProv ic) of
              PData _ [p] -> p
              q -> PData (deep q) []
            env' = case f of
              AnnoS _ _ (LamS (v : _) _) -> Map.insert v element env
              _ -> env
        fi <- analyze cx env' Portable f
        let (r, through) = applyP (annoType f) (iProv fi) [element]
        done' through (fit t (PData Portable [r])) (frees [fi, ic]) [fi, ic]
      _ -> do
        is <- mapM (analyze cx env Portable) xs
        let o = joinsO (map (held . iProv) is)
        if intr `elem` [IntrSave, IntrSaveJ, IntrHash, IntrEq, IntrLe, IntrShow]
          then sink o "a value an intrinsic writes out" >> done (portable t) (frees is) is
          else done (mark o t) (frees is) is

    sourcedHead (AnnoS _ _ x) = case x of
      ExeS (SrcCall src) -> Just src
      VarS _ (One y) -> sourcedHead y
      LabelS _ y -> sourcedHead y
      CoerceS _ y -> sourcedHead y
      _ -> Nothing

    calledTree (AnnoS _ _ x) = case x of
      CallS v -> cxTree cx v
      VarS _ (One y) -> calledTree y
      _ -> Nothing

    cIdx (AnnoS _ (Idx j _) _) = j

-- | A call pattern holding no host-made function is the tree's own.
normalKey :: [Type] -> [Prov] -> [Prov]
normalKey pts ps
  | all ((== Portable) . deep) ps = map portable pts
  | otherwise = ps

arity :: Type -> Int
arity (FunT ins _) = length ins
arity _ = 0

annoType :: AnnoS (Indexed Type) f c -> Type
annoType (AnnoS (Idx _ t) _ _) = t

hostOf :: Source -> Origin
hostOf src = Host (srcLang src) (MadeBy src)

-- | What a host-made function is, for a message.
describe :: Origin -> MDoc
describe (Host l (MadeBy src)) = "made by" <+> pretty l <+> "code (" <> squotes (pretty (displayName (srcAlias src))) <> ")"
describe (Host l (PassedBy src)) = "that" <+> squotes (pretty (displayName (srcAlias src))) <+> "passes from" <+> pretty l
describe (Conflict a b) = describe (uncurry Host a) <+> "or" <+> describe (uncurry Host b)
describe Portable = "made by morloc"

hintOf :: Origin -> MorlocMonad MDoc
hintOf (Host _ (MadeBy src)) = chooseSourceHint src
hintOf (Conflict (la, ma) (lb, mb)) = (<>) <$> hintOf (Host la ma) <*> hintOf (Host lb mb)
hintOf _ = return mempty

stays :: MDoc
stays = "a function made by host code cannot leave the pool that made it (HOST-9)"

-- | The language a node must run in, for the placement (COST-15), and the
-- refusal when it cannot run there.
data HostLimit = HostLimit Lang MDoc

-- | Follow every function value through the pool trees (after normalization),
-- copy each recursive function given host-made functions per pattern of what
-- it is given, bind by a let each argument only computed from one, and
-- return the trees with the language each node holding or receiving a
-- host-made function must run in, by the tree's root and the node's c index.
-- A program that would move a host-made function between pools, or
-- serialize one, is refused.
planHostFunctions ::
  (EVar -> Maybe Int) ->
  [Tree] ->
  MorlocMonad ([Tree], Map Int (Map Int HostLimit))
planHostFunctions target trees00
  | not (any hasFunction trees00) = return (trees00, Map.empty)
  | otherwise = do
      trees0 <- mapM uniqueBinders trees00
      let byRoot = Map.fromList [(rootIdx t, t) | t <- trees0]
          paramTypes j = case Map.lookup j byRoot of
            Just (AnnoS (Idx _ (FunT ts _)) _ (LamS vs _)) -> take (length vs) ts
            _ -> []
          rootKeys = [(rootIdx t, map portable (paramTypes (rootIdx t))) | t <- trees0]
      sigs <- signatures trees0
      labels <- MM.gets stateManifoldConfig
      exports <- Set.fromList <$> MM.gets stateExports
      let serializes k = case Map.lookup k labels of
            Just mc -> any (== Just True) [manifoldConfigCache mc, manifoldConfigLog mc] || isJust (manifoldConfigRemote mc)
            Nothing -> False
          ctxWith sums =
            Ctx
              { cxSig = (`Map.lookup` sigs)
              , cxTree = target
              , cxArity = length . paramTypes
              , cxSummary = \key -> Map.findWithDefault (PData Portable [], map (const Portable) (snd key)) key sums
              , cxParamType = paramTypes
              , cxSerializes = serializes
              }
      sums <- settle byRoot rootKeys ctxWith
      let cx = ctxWith sums
          reachable = reach byRoot cx (Set.fromList rootKeys) rootKeys
          copied = [key | key <- Set.toList reachable, key `notElem` rootKeys]
      names <- Map.fromList <$> mapM (\key -> (,) key <$> copyName key) copied
      copies <- mapM (\key -> (,) key <$> makeCopy byRoot names key) copied
      let instances = zip rootKeys trees0 <> copies
          -- a call of a copy reaches the tree it copies
          copyTree = Map.fromList [(name, j) | ((j, _), name) <- Map.toList names]
          cxCopies = cx {cxTree = \v -> maybe (target v) Just (Map.lookup v copyTree)}
      rewritten <- mapM (\(key, t) -> (,) key <$> rewrite cx names key t) instances
      limits <- mapM (\(key, t) -> (,) (rootIdx t) <$> limitsOf cxCopies exports key t) rewritten
      return (map snd rewritten, Map.fromList limits)
  where
    hasFunction = any (\(AnnoS (Idx _ t) _ _) -> containsFunT t || isCallable t) . nodes
    nodes n@(AnnoS _ _ e) = n : foldExprS nodes e
    rootIdx (AnnoS (Idx i _) _ _) = i

    signatures trees0 = do
      let srcs = [(ci, src) | t <- trees0, AnnoS _ (Idx ci _) (ExeS (SrcCall src)) <- nodes t]
      pairs <- mapM (\(ci, src) -> fmap ((,) src . etype) <$> sourceSignature (Just ci) src) srcs
      return (Map.fromList (catMaybes pairs))

    -- the analysis of one tree given what its parameters hold: what it
    -- found, what it returns, and the demand on each parameter
    analyzeTree cx pat t@(AnnoS (Idx _ ty) _ e) =
      let (vs, body) = case e of
            LamS ws b -> (ws, b)
            _ -> ([], t)
          ret = afterCall ty (length vs)
          (info, found) = runState (analyze cx (Map.fromList (zip vs pat)) Portable body) emptyFound
          demands = [Map.findWithDefault Portable v (fDemand found) | v <- vs]
       in (found {fBinders = foldr (\(v, p) -> if held p == Portable then id else Map.insert v (held p)) (fBinders found) (zip vs pat)}, fit ret (iProv info), demands)

    analyzeKey byRoot cx (j, pat) = case Map.lookup j byRoot of
      Just t -> analyzeTree cx pat t
      Nothing -> (emptyFound, PData Portable [], [])

    -- what every instance reached from the roots returns, to a fixed point
    settle byRoot rootKeys ctxWith = go (0 :: Int) Map.empty (Set.fromList rootKeys)
      where
        go k sums keys
          | k > 100 = MM.throwCompilerBug "following host-made functions through recursive functions did not settle"
          | otherwise = do
              let cx = ctxWith sums
                  results = [(key, analyzeKey byRoot cx key) | key <- Set.toList keys]
                  sums' = Map.fromList [(key, (r, ds)) | (key, (_, r, ds)) <- results]
                  keys' = Set.union keys (Set.fromList (concat [Map.elems (fCalls f) | (_, (f, _, _)) <- results]))
              if sums' == sums && keys' == keys then return sums else go (k + 1) sums' keys'

    reach _ _ seen [] = seen
    reach byRoot cx seen (key : rest) =
      let (f, _, _) = analyzeKey byRoot cx key
          new = unique [k | k <- Map.elems (fCalls f), not (Set.member k seen)]
       in reach byRoot cx (foldr Set.insert seen new) (rest <> new)

    copyName (j, _) = do
      k <- MM.getCounter
      names <- MM.gets stateName
      recs <- MM.gets stateRecursionTargets
      let base = case [v | (v, i) <- Map.toList recs, i == j] of
            (v : _) -> unEVar v
            [] -> maybe "tree" unEVar (Map.lookup j names)
      return (EV (base <> "@host" <> MT.show' k))

    -- a copy of a tree for one pattern of what it is given, with fresh
    -- indices, built in the language of the host functions it is given
    makeCopy byRoot names key@(j, pat) = case (Map.lookup j byRoot, Map.lookup key names) of
      (Just t, Just name) -> do
        AnnoS g@(Idx gi _) c e <- reindexTree t >>= mapAnnoSCM (\(Idx ci cl) -> (`Idx` cl) <$> newIndex ci)
        MM.modify $ \st ->
          st { stateName = Map.insert gi name (stateName st)
             , stateNameSource = Map.insert gi j (stateNameSource st)
             , stateRecursionTargets = Map.insert name gi (stateRecursionTargets st)
             }
        return $ case [l | Host l _ <- map held pat] of
          (l : _) -> relangRoot l (AnnoS g c e)
          [] -> AnnoS g c e
      _ -> MM.throwCompilerBug "a copied recursive function has no tree"

    relangRoot l (AnnoS g (Idx ci _) e) = AnnoS g (Idx ci l) $ case e of
      LamS vs (AnnoS bg (Idx bci _) be) -> LamS vs (AnnoS bg (Idx bci l) be)
      _ -> e

    -- calls of copies, arguments let-bound, and host-made binders renamed
    -- apart and recorded, so serialization can tell them from others
    rewrite cx names (_, pat) t = do
      let (found, _, _) = analyzeTree cx pat t
      renames <- Map.fromList <$> mapM (\v -> (,) v <$> hostName v) (Map.keys (fBinders found))
      MM.modify (\st -> st {stateHostFunctionVars = foldr Set.insert (stateHostFunctionVars st) (Map.elems renames)})
      let ren v = Map.findWithDefault v v renames
          go (AnnoS g c@(Idx ci cl) e) = case e of
            CallS _
              | Just key <- Map.lookup ci (fCalls found)
              , Just name <- Map.lookup key names ->
                  return (AnnoS g c (CallS name))
            BndS v -> return (AnnoS g c (BndS (ren v)))
            LetBndS v -> return (AnnoS g c (LetBndS (ren v)))
            LamS vs b -> AnnoS g c . LamS (map ren vs) <$> go b
            LetS v a b -> AnnoS g c <$> (LetS (ren v) <$> go a <*> go b)
            AppS f xs -> do
              f' <- go f
              xs' <- mapM go xs
              let sites = [k | (k', k) <- Set.toList (fFloat found), k' == ci]
              if null sites
                then return (AnnoS g c (AppS f' xs'))
                else floatArgs g cl sites f' xs'
            _ -> AnnoS g c <$> mapExprSM go e
      go t

    hostName v = do
      k <- MM.getCounter
      return (EV (unEVar (displayName v) <> "`h" <> MT.show' k))

    floatArgs g@(Idx gi _) cl sites f xs = do
      bound <-
        mapM
          ( \(k, x@(AnnoS (Idx xi xt) (Idx xci xl) _)) ->
              if k `elem` sites
                then do
                  n <- MM.getCounter
                  let v = EV ("hosted`c" <> MT.show' n)
                  ri <- newIndex xi
                  rci <- newIndex xci
                  return (AnnoS (Idx ri xt) (Idx rci xl) (LetBndS v), [(v, x)])
                else return (x, [])
          )
          (zip [0 :: Int ..] xs)
      ci' <- MM.getCounter
      let app = AnnoS g (Idx ci' cl) (AppS f (map fst bound))
          letIn body@(AnnoS (Idx _ bt) _ _) (v, x@(AnnoS _ (Idx _ xl) _)) = do
            li <- newIndex gi
            lci <- MM.getCounter
            return (AnnoS (Idx li bt) (Idx lci xl) (LetS v x body))
      foldM letIn app (reverse (concatMap snd bound))

    -- the limits of one rewritten tree, refusing what cannot be placed
    limitsOf cx exports (_, pat) t = do
      let (found, ret, _) = analyzeTree cx pat t
      case fSinks found of
        ((i, o, what) : _) -> refuse i o ("a function" <+> describe o <+> "would be serialized here, as" <+> what)
        [] -> return ()
      case fStray found of
        ((i, o, src) : _) ->
          refuse i o $
            squotes (pretty (displayName (srcAlias src))) <+> "calls this function with functions it makes in"
              <+> pretty (srcLang src) <> ", and morloc cannot follow where it uses them; pass a lambda here"
        [] -> return ()
      case fUnsettled found of
        (i : _) -> MM.throwCompilerBugAt i "the functions a host passes to this binding did not settle"
        [] -> return ()
      let returned = exportResult t ret
      when (Set.member (rootIdx t) exports && deep returned /= Portable) $
        refuse (rootIdx t) (deep returned) ("this function returns a function" <+> describe (deep returned) <> ", which its caller would receive")
      case fBranches found of
        ((i, o@(Conflict (la, _) (lb, _))) : _) ->
          refuse i o $
            "this conditional gives a function made by host code in" <+> pretty la <+> "or in" <+> pretty lb
              <> ", depending on which branch runs, so it cannot be kept in one pool"
        _ -> return ()
      case deepestConflict found t of
        Just (i, o@(Conflict a b)) ->
              refuse i o $
                "this expression uses a function" <+> describe (uncurry Host a) <+> "and one"
                  <+> describe (uncurry Host b) <> "; one of them would have to pass through the other's pool"
        _ -> return ()
      hints <- Map.fromList <$> mapM (\o -> (,) o <$> hintOf o) (unique [o | o@Host {} <- Map.elems (fReq found)])
      return $
        Map.fromList
          [ (ci, HostLimit l ("a function" <+> describe o <+> "is used here, by code that cannot run in" <+> pretty l <> ";" <+> stays <> Map.findWithDefault mempty o hints))
          | (ci, o@(Host l _)) <- Map.toList (fReq found)
          ]

    -- what the caller of an export receives: the value after every argument
    -- its type takes, with a returned effect run
    exportResult (AnnoS (Idx _ ty) _ e) ret =
      let given = case e of
            LamS vs _ -> length vs
            _ -> 0
          rest = afterCall ty given
          applied = fst (applyP rest ret (map portable (paramsOf rest)))
       in case fit (resultType rest) applied of
            PFun _ 0 r -> r
            p -> p
      where
        paramsOf (FunT ins _) = ins
        paramsOf _ = []
        resultType (FunT _ out) = out
        resultType u = u

    refuse i o msg = do
      hint <- hintOf o
      MM.throwSourcedError i (msg <> ";" <+> stays <> hint)

    deepestConflict found t =
      let go d (AnnoS (Idx gi _) (Idx ci _) e) =
            [(d, gi, o) | Just o@Conflict {} <- [Map.lookup ci (fReq found)]]
              <> concat (foldExprS (\y -> [go (d + 1 :: Int) y]) e)
       in case reverse (List.sortOn (\(d, _, _) -> d) (go 0 t)) of
            ((_, gi, o) : _) -> Just (gi, o)
            [] -> Nothing

-- | Every binder of a tree given a name no other binder in it has, so what
-- is found about a name holds for one binding.
uniqueBinders :: Tree -> MorlocMonad Tree
uniqueBinders t = evalStateT (go Map.empty t) Set.empty
  where
    go ren (AnnoS g c e) = AnnoS g c <$> case e of
      BndS v -> return (BndS (Map.findWithDefault v v ren))
      LetBndS v -> return (LetBndS (Map.findWithDefault v v ren))
      LamS vs b -> do
        vs' <- mapM fresh vs
        LamS vs' <$> go (foldr (uncurry Map.insert) ren (zip vs vs')) b
      LetS v a b -> do
        a' <- go ren a
        v' <- fresh v
        LetS v' a' <$> go (Map.insert v v' ren) b
      _ -> mapExprSM (go ren) e
    fresh v = do
      seen <- get
      v' <-
        if Set.member v seen
          then (\k -> EV (unEVar (displayName v) <> "`u" <> MT.show' k)) <$> lift MM.getCounter
          else return v
      put (Set.insert v' seen)
      return v'

-- | The sources of each term a signature declares.
implementations :: SignatureSet -> [[Source]]
implementations (Monomorphic t) = [[s | (_, Idx _ s) <- termConcrete t]]
implementations (Polymorphic _ _ _ ts) = [[s | (_, Idx _ s) <- termConcrete t] | t <- ts]

-- | The other sources of the term @src@ implements, in other languages, with
-- the same call groups (HOST-3).
sameTermSources :: Source -> MorlocMonad [Source]
sameTermSources src = do
  sigs <- GMap.elems <$> MM.gets stateSignatures
  return $
    unique
      [ s
      | srcs <- concatMap implementations sigs
      , src `elem` srcs
      , s <- srcs
      , srcLang s /= srcLang src
      , srcRsize s == srcRsize src
      ]

-- | The advice for a function made by @src@: if its term has sources in
-- other languages, how to have one of those make it instead; otherwise
-- nothing.
chooseSourceHint :: Source -> MorlocMonad MDoc
chooseSourceHint src = do
  sigs <- GMap.elems <$> MM.gets stateSignatures
  let terms = concatMap implementations sigs
      others = unique [srcLang s | srcs <- terms, src `elem` srcs, s <- srcs, srcLang s /= srcLang src]
      name = squotes (pretty (displayName (srcAlias src)))
  return $ case others of
    [] -> mempty
    _ ->
      line <> name <+> "has sources in" <+> hsep (punctuate "," (map pretty (srcLang src : others)))
        <+> "and the one in" <+> pretty (srcLang src) <+> "was chosen; to use another, choose it"
        <+> "explicitly: import only the module that sources" <+> name <+> "in the language you need,"
        <+> "or source it in that language only"
