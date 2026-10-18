{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.OwnershipCheck
Description : Check that no value is read after its payload was taken

An independent check of the take decisions a C++ member renders
(model/compiler/codegen/ownership.md, OWN-1 and OWN-2). It recomputes
evaluation order from the IR itself: a let's right-hand side runs before its
body, a conditional runs one arm, the parts of one step run in an
unspecified order, a loop's back edge rebinds its carried slots, and a
nested manifold or @do@ block is a scope of its own. It takes the decisions
as given -- which lets take their payload and which bind a reference -- and
reports every read that could see a taken payload. Rust takes are left to
rustc, which rejects a read of a moved value.
-}
module Morloc.CodeGenerator.OwnershipCheck
  ( checkManifold
  , checkNative
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.CodeGenerator.Grammars.Translator.Imperative (RNode (..), aliasChain, rnodeChildren)
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Ownership (LetMode (..))

-- | The state that flows in evaluation order: roots whose payload was taken,
-- and roots a body that may run later has captured.
data St = St
  { stTaken :: Set.Set Int
  , stCaptured :: Set.Set Int
  , stMoved :: Set.Set Int
  -- ^ locals moved whole, which only a reference into them still sees
  , stHeld :: Set.Set Int
  -- ^ locals a @do@ block bound by a let reads (OWN-3)
  }
  deriving (Eq)

-- | Either of two states, or both.
joinSt :: St -> St -> St
joinSt a b = St (stTaken a <> stTaken b) (stCaptured a <> stCaptured b) (stMoved a <> stMoved b) (stHeld a <> stHeld b)

-- | A state without the given locals, which are bound again.
forget :: Set.Set Int -> St -> St
forget vs st = St (stTaken st `Set.difference` vs) (stCaptured st `Set.difference` vs) (stMoved st `Set.difference` vs) (stHeld st `Set.difference` vs)


-- | What evaluating a node did: the state once it is done; the values it
-- read through any alias, and through references only; the payloads it
-- took and the locals it moved; what it bound; and what went wrong.
data Eval = Eval
  { rSt :: St
  , rReads :: Set.Set Int
  , rRefReads :: Set.Set Int
  , rTakes :: Set.Set Int
  , rMoves :: Set.Set Int
  , rBound :: Set.Set Int
  , rViol :: [Text]
  }

-- | How the member binds each native let: the mode of let @i@ with
-- right-hand side @rhs@; whether a tag test may read a value whose payload
-- was taken ('Morloc.CodeGenerator.Ownership.ocTagAfterTake'); and which
-- locals share storage with which root.
data Env = Env
  { envMode :: Int -> NativeExpr -> LetMode
  , envTagAfterTake :: Bool
  , envAlias :: Map.Map Int Int
  , envMoves :: Set.Set Int
  -- ^ the lets whose last read in a keeping position moves them
  , envBorrow :: Map.Map Int Int
  -- ^ the locals that are references, to what they refer into
  }

-- | The violations in a top-level manifold's tree, where the lets in
-- @moves@ are moved at a last read in a keeping position.
checkManifold :: (Int -> NativeExpr -> LetMode) -> Bool -> Set.Set Int -> SerialManifold -> [Text]
checkManifold mode tagOk moves (SerialManifold m _ _ _ e) =
  map (\v -> "manifold " <> num m <> ": " <> v) (rViol (walkS (Env mode tagOk Map.empty moves Map.empty) start e))

-- | The violations in a native expression evaluated from scratch.
checkNative :: (Int -> NativeExpr -> LetMode) -> Bool -> Set.Set Int -> NativeExpr -> [Text]
checkNative mode tagOk moves e = rViol (walkN (Env mode tagOk Map.empty moves Map.empty) start e)

start :: St
start = St Set.empty Set.empty Set.empty Set.empty

num :: Int -> Text
num = T.pack . show

done :: St -> Eval
done s = Eval s Set.empty Set.empty Set.empty Set.empty Set.empty []

-- | The parts of one step, which run in an unspecified order: each starts
-- from the same state, and a root one part takes must not be read or taken
-- by another.
step :: St -> [St -> Eval] -> Eval
step s parts =
  Eval
    { rSt = foldr (joinSt . rSt) s rs
    , rReads = Set.unions (map rReads rs)
    , rRefReads = Set.unions (map rRefReads rs)
    , rTakes = Set.unions (map rTakes rs)
    , rMoves = Set.unions (map rMoves rs)
    , rBound = Set.unions (map rBound rs)
    , rViol = concatMap rViol rs <> clashes
    }
  where
    rs = map ($ s) parts
    count f = Map.fromListWith (+) [(v, 1 :: Int) | r <- rs, v <- Set.toList (f r)]
    takers = count rTakes
    movers = count rMoves
    others counts f r v = Map.findWithDefault 0 v counts - (if Set.member v (f r) then 1 else 0) > 0
    clashes =
      ["the payload of n" <> num v <> " is taken by two parts of one step" | (v, k) <- Map.toList takers, k > 1]
        <> ["n" <> num v <> " is moved by two parts of one step" | (v, k) <- Map.toList movers, k > 1]
        <> ["n" <> num v <> " is taken and read in one step" | r <- rs, v <- Set.toList (rReads r), others takers rTakes r v]
        <> ["n" <> num v <> " is moved and read in one step" | r <- rs, v <- Set.toList (rRefReads r), others movers rMoves r v]

-- | One part after another.
andThen :: Eval -> (St -> Eval) -> Eval
andThen a k =
  let b = k (rSt a)
   in both (rSt b) a b

-- | Either of two arms.
eitherArm :: Eval -> Eval -> Eval
eitherArm a b = both (joinSt (rSt a) (rSt b)) a b

-- | What two evaluations did together, ending in a given state.
both :: St -> Eval -> Eval -> Eval
both st a b =
  Eval
    st
    (rReads a <> rReads b)
    (rRefReads a <> rRefReads b)
    (rTakes a <> rTakes b)
    (rMoves a <> rMoves b)
    (rBound a <> rBound b)
    (rViol a <> rViol b)

rootOf :: Env -> Int -> Int
rootOf env i = maybe i (rootOf env) (Map.lookup i (envAlias env))

readVar :: Env -> St -> Int -> Eval
readVar env s i = (refRead env s i) {rReads = Set.fromList (i : aliasChain (envAlias env) i), rViol = taken <> rViol (refRead env s i)}
  where
    taken = ["n" <> num i <> " is read after the payload of n" <> num v <> " was taken" | v <- take 1 [v | v <- i : aliasChain (envAlias env) i, Set.member v (stTaken s)]]

-- | A read that sees only what a reference sees: whether the local, or what
-- it refers into, was moved.
refRead :: Env -> St -> Int -> Eval
refRead env s i =
  let chain = i : aliasChain (envBorrow env) i
   in (done s)
        { rRefReads = Set.fromList chain
        , rViol = ["n" <> num i <> " is read after n" <> num v <> " was moved" | v <- take 1 [v | v <- chain, Set.member v (stMoved s)]]
        }

-- | A part of a value being built, or a value a loop passes on: a last read
-- of a let in 'envMoves' there moves it.
keepPart :: Env -> St -> NativeExpr -> Eval
keepPart env s x = case x of
  LetVarN _ i LastRead | Set.member i (envMoves env) ->
    let r = readVar env s i
        later = ["n" <> num i <> " is moved while a do block bound by a let holds it" | Set.member i (stHeld s)]
     in r {rSt = (rSt r) {stMoved = Set.insert i (stMoved (rSt r))}, rMoves = Set.insert i (rMoves r), rViol = later <> rViol r}
  _ -> walkN env s x

-- | The variable at the base of a projection, without following aliases.
projectionBase :: NativeExpr -> Maybe Int
projectionBase e = case e of
  LetVarN _ i _ -> Just i
  BndVarN _ i _ -> Just i
  IntrinsicN _ IntrCtorField _ (x : _) -> projectionBase x
  IntrinsicN _ IntrListElem _ (x : _) -> projectionBase x
  AppExeN _ (PatCallP (PatternStruct _)) [NativeArgExpr x] -> projectionBase x
  _ -> Nothing

-- | The variable a projection reads from, through aliases.
projectionRoot :: Env -> NativeExpr -> Maybe Int
projectionRoot env e = case e of
  LetVarN _ i _ -> Just (rootOf env i)
  BndVarN _ i _ -> Just (rootOf env i)
  IntrinsicN _ IntrCtorField _ (x : _) -> projectionRoot env x
  IntrinsicN _ IntrListElem _ (x : _) -> projectionRoot env x
  AppExeN _ (PatCallP (PatternStruct _)) [NativeArgExpr x] -> projectionRoot env x
  _ -> Nothing

-- | A native let: its right-hand side, then its body with the binding in
-- place. A take records its root as taken. A binding to a projection that
-- is not a take, whether a reference or a copy, may share storage with its
-- root (a copied @data@ value shares its arms), so reads of the local are
-- reads of the root. Binding an index rebinds it, so what happened to an
-- earlier binding of it no longer matters.
bindLet :: Env -> St -> Int -> NativeExpr -> (Env -> St -> (Eval, a)) -> (Eval, a)
bindLet env s i rhs body =
  let rebound = forget (Set.singleton i) t
      unaliased = env {envAlias = Map.delete i (envAlias env), envBorrow = Map.delete i (envBorrow env)}
      (b, x) = case (envMode env i rhs, projectionRoot env rhs) of
        (LetTake, Just root) ->
          let problems =
                ["the payload of n" <> num root <> " is taken twice" | Set.member root (stTaken t)]
                  <> ["the payload of n" <> num root <> " is taken after a body that may run later captured it" | Set.member root (stCaptured t)]
              (b', x') = body unaliased rebound {stTaken = Set.insert root (stTaken rebound)}
           in (b' {rTakes = Set.insert root (rTakes b'), rViol = problems <> rViol b'}, x')
        (LetBorrow, Just root)
          | Just base <- projectionBase rhs ->
              body env {envAlias = Map.insert i root (envAlias env), envBorrow = Map.insert i base (envBorrow env)} rebound
        _ | Just j <- movedIn -> case Map.lookup j (envAlias env) of
          Just root -> body unaliased {envAlias = Map.insert i root (envAlias env)} rebound
          Nothing -> body unaliased rebound
        (_, Just root) -> body env {envAlias = Map.insert i root (envAlias env), envBorrow = Map.delete i (envBorrow env)} rebound
        _ -> body unaliased rebound
   in (rhsR `andThen` const (b {rBound = Set.insert i (rBound b)}), x)
  where
    rhsR = keepValue env s rhs
    t = (rSt rhsR) {stHeld = stHeld (rSt rhsR) <> Set.fromList [v | j <- Set.toList (heldBy rhs), v <- j : aliasChain (envAlias env) j]}
    -- the local the right-hand side ends in, when it was moved there
    movedIn = case valueOf rhs of
      LetVarN _ j _ | Set.member j (rMoves rhsR) -> Just j
      _ -> Nothing
    valueOf (NativeLetN _ _ v) = valueOf v
    valueOf v = v

-- | The locals a @do@ block in a let's value reads: the value itself, a
-- conditional's arm, the value a chain of lets ends in, or a part of a value
-- being built.
heldBy :: NativeExpr -> Set.Set Int
heldBy e = case e of
  DoBlockN _ x -> namesIn x
  IfN _ _ a b -> heldBy a <> heldBy b
  NativeLetN _ _ b -> heldBy b
  TupleN _ xs -> foldMap heldBy xs
  RecordN _ _ _ kvs -> foldMap (heldBy . snd) kvs
  VariantN _ _ _ xs -> foldMap heldBy xs
  ListN _ _ xs -> foldMap heldBy xs
  CoerceN _ _ x -> heldBy x
  _ -> Set.empty

-- | Every native local named under an expression, the context arguments of
-- the manifolds under it included.
namesIn :: NativeExpr -> Set.Set Int
namesIn = go . RN
  where
    go n = here n <> foldMap go (rnodeChildren n)
    here n = case n of
      RN (LetVarN _ j _) -> Set.singleton j
      RN (BndVarN _ j _) -> Set.singleton j
      RNM (NativeManifold _ _ form _) -> Set.fromList [j | Arg j _ <- manifoldContext form]
      RSM (SerialManifold _ _ form _ _) -> Set.fromList [j | Arg j _ <- manifoldContext form]
      _ -> Set.empty

-- | A let's right-hand side: the value it ends in is kept (OWN-2).
keepValue :: Env -> St -> NativeExpr -> Eval
keepValue env s e = case e of
  NativeLetN j rhs body -> bindLet_ env s j rhs (\env' s' -> keepValue env' s' body)
  _ -> keepPart env s e

-- | 'bindLet' for a body with nothing to report but its result.
bindLet_ :: Env -> St -> Int -> NativeExpr -> (Env -> St -> Eval) -> Eval
bindLet_ env s i rhs body = fst (bindLet env s i rhs (\env' s' -> (body env' s', ())))

walkN :: Env -> St -> NativeExpr -> Eval
walkN env s e = case e of
  LetVarN _ i _ -> readVar env s i
  BndVarN _ i _ -> readVar env s i
  NativeLetN i rhs body -> bindLet_ env s i rhs (\env' s' -> walkN env' s' body)
  SerialLetN _ rhs body -> walkS env s rhs `andThen` \s' -> walkN env s' body
  IfN _ c a b ->
    walkN env s c `andThen` \s' -> eitherArm (walkN env s' a) (walkN env s' b)
  IntrinsicN _ IntrTagTest _ (LetVarN _ i _ : rest)
    | envTagAfterTake env -> step s ((\s' -> refRead env s' i) : map (\x s' -> walkN env s' x) rest)
  AppExeN _ _ args -> step s (map (\a s' -> walkArg env s' a) args)
  ReturnN x -> walkN env s x
  DeserializeN _ _ x -> walkS env s x
  ListN _ _ xs -> parts xs
  TupleN _ xs -> keeps xs
  RecordN _ _ _ kvs -> keeps (map snd kvs)
  VariantN _ _ _ xs -> keeps xs
  IntrinsicN _ _ _ xs -> parts xs
  DoBlockN _ x -> region (walkN env s x)
  EvalN _ x -> walkN env s x
  CoerceN _ _ x -> keepPart env s x
  MapOptionalN _ _ _ x -> walkN env s x
  ManN nm -> nativeFrame env s nm
  LoopN _ starts body ->
    parts (map snd starts) `andThen` \s' -> loop env s' (map fst starts) body (\env' s'' b -> walkN env' s'' b)
  ExeN {} -> done s
  LogN {} -> done s
  RealN {} -> done s
  IntN {} -> done s
  StrN {} -> done s
  EnumN {} -> done s
  NullN {} -> done s
  where
    parts xs = step s (map (\x s' -> walkN env s' x) xs)
    keeps xs = step s (map (\x s' -> keepPart env s' x) xs)

walkArg :: Env -> St -> NativeArg -> Eval
walkArg env s (NativeArgExpr x) = walkN env s x
walkArg env s (NativeArgManifold nm) = nativeFrame env s nm

nativeFrame :: Env -> St -> NativeManifold -> Eval
nativeFrame env s (NativeManifold m _ form body) =
  frame env s m form (rViol (walkN env {envAlias = Map.empty, envBorrow = Map.empty} start body))

serialFrame :: Env -> St -> SerialManifold -> Eval
serialFrame env s (SerialManifold m _ form _ body) =
  frame env s m form (rViol (walkS env {envAlias = Map.empty, envBorrow = Map.empty} start body))

-- | A nested manifold: building it reads its context arguments here, and its
-- body is a frame of its own. One that is not called on the spot is a value
-- that may run later, so it captures what it reads.
frame :: Env -> St -> Int -> ManifoldForm a b -> [Text] -> Eval
frame env s m form inner =
  let ctx = step s [\s' -> readVar env s' i | Arg i _ <- manifoldContext form]
      later = case form of
        ManifoldFull _ -> Set.empty
        _ -> rReads ctx
      st = rSt ctx
   in ctx
        { rSt = st {stCaptured = stCaptured st <> later}
        , rViol = rViol ctx <> map (("nested manifold " <> num m <> ": ") <>) inner
        }

-- | A body that may run any number of times (OWN-3): it may not take a
-- payload from outside itself, what it takes does not outlive it, and what
-- it reads from outside is captured.
region :: Eval -> Eval
region r =
  let outside = (rTakes r <> rMoves r) `Set.difference` rBound r
      st = rSt r
   in r
        { rSt = St (stTaken st `Set.difference` rTakes r) (stCaptured st <> (rReads r `Set.difference` rBound r)) (stMoved st `Set.difference` rMoves r) (stHeld st)
        , rTakes = Set.empty
        , rMoves = Set.empty
        , rViol = rViol r <> ["n" <> num v <> " is taken or moved in a body that may run more than once" | v <- Set.toList outside]
        }

walkS :: Env -> St -> SerialExpr -> Eval
walkS env s e = case e of
  NativeLetS i rhs body -> bindLet_ env s i rhs (\env' s' -> walkS env' s' body)
  SerialLetS _ rhs body -> walkS env s rhs `andThen` \s' -> walkS env s' body
  SerializeS _ x -> walkN env s x
  ReturnS x -> walkS env s x
  AppPoolS _ _ args -> step s (map (\a s' -> walkSArg env s' a) args)
  AppRecS _ _ xs -> step s (map (\x s' -> walkS env s' x) xs)
  AppForeignRecS _ _ _ xs -> step s (map (\x s' -> walkS env s' x) xs)
  CacheBodyS _ _ _ _ _ x -> walkS env s x
  DebugWrapS _ _ _ x -> walkS env s x
  ManS sm -> serialFrame env s sm
  LoopS _ ids body -> loop env s ids body (\env' s' b -> walkS env' s' b)
  LetVarS {} -> done s
  BndVarS {} -> done s

walkSArg :: Env -> St -> SerialArg -> Eval
walkSArg env s (SerialArgExpr x) = walkS env s x
walkSArg env s (SerialArgManifold sm) = serialFrame env s sm

-- | A tail loop, iterated until the state at its head is stable; the state
-- only grows, so this ends. A continue is one step, and its back edge
-- rebinds every carried slot.
loop :: Env -> St -> [Int] -> LoopBody NativeExpr SerialExpr b -> (Env -> St -> b -> Eval) -> Eval
loop env entry ids body base = go entry
  where
    go hd =
      let (exitR, back) = walkB env hd body
          hd' = joinSt hd back
       in if hd' == hd then exitR else go hd'
    walkB env' s b = case b of
      LoopIf c th el ->
        let rc = walkN env' s c
            (ra, ba) = walkB env' (rSt rc) th
            (rb, bb) = walkB env' (rSt rc) el
         in (rc `andThen` const (eitherArm ra rb), joinSt ba bb)
      LoopNLet i rhs k -> bindLet env' s i rhs (\env'' s' -> walkB env'' s' k)
      LoopSLet _ rhs k ->
        let rs = walkS env' s rhs
            (rk, bk) = walkB env' (rSt rs) k
         in (rs `andThen` const rk, bk)
      LoopBase x -> (base env' s x, start)
      LoopContinue xs ->
        let r = step s (map (\x s' -> keepPart env' s' x) xs)
         in (r, forget (Set.fromList ids) (rSt r))
