{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Specialize
Description : Typecheck each definition once per type it is used at
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A signed top-level definition of the root module has been checked against its
signature ('Morloc.Frontend.Typecheck.validate'). Its uses therefore need only
its signature: an export is typechecked with each reference to such a
definition left as the signature alone, and the type the reference is solved
at (its key) selects one elaboration of the definition, checked once against
that type and shared by every reference at the same key.

Each reference is then replaced by a copy of its elaboration with fresh
indices and binder names, so the output has the same shape as a tree whose
every term was expanded in place, and every later pass is unchanged.

A reference whose key is not fully solved (an existential remains) cannot be
elaborated on its own: the definition, or export, it occurs in is instead
checked with every term expanded in place. There, a use of a method inside its
own expansion is at first a back-edge; one at another type becomes a reference
again, since that type may select another instance (CLS-2). A generic export is
never realized
(SIG-5), so it is checked with its references left as their signatures and
nothing is chosen for it.

Recursion between definitions is found while splicing: a reference to a
definition already being spliced at the same key becomes a 'CallS' back-edge
to it, and at a different key is polymorphic recursion, which is rejected.
-}
module Morloc.Frontend.Specialize (specialize, isGenericExport) where

import qualified Morloc.BaseTypes as BT
import Morloc.Data.Zip (indexed, zipWithSame)
import Data.IORef
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Morloc.Frontend.Namespace
import Morloc.CodeGenerator.Value (isValue)
import Morloc.Frontend.Rename (displayName, fresh)

import Morloc.Frontend.Treeify (Validation (..), collectRoot, recName)
import Morloc.Frontend.Typecheck (typecheckRoot, typecheckRootAt)
import Morloc.Data.Doc
import qualified Morloc.Monad as MM
import Morloc.Typecheck.Internal (mapTypeUChildren, typeUChildren, unqualify)

type Typed = AnnoS (Indexed TypeU) Many Int

type SpecKey = (Int, TypeU)

data Spec = Spec
  { specDefs :: Map.Map Int (Int, EVar)
  , specMemo :: IORef (Map.Map SpecKey Typed)
  , specShared :: IORef (Map.Map SpecKey EVar)
  -- ^ the keys shared as a root of their own, by the name uses call
  , specInlined :: IORef (Set.Set SpecKey)
  -- ^ the keys found not to be shareable wherever they are used
  }

-- | The term an index refers to. Read at each use: collecting and copying
-- trees add indices.
termOf :: Int -> MorlocMonad (Maybe Int)
termOf i = do
  GMap idmap _ <- MM.gets stateSignatures
  return (Map.lookup i idmap)

-- | Typecheck the tree of each export.
specialize :: Validation -> [(Int, EVar)] -> MorlocMonad [Typed]
specialize checks exports = do
  spec <- liftIO (Spec (validationDefs checks) <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef Set.empty)
  mapM (specializeExport spec) exports

specializeExport :: Spec -> (Int, EVar) -> MorlocMonad Typed
specializeExport spec (gi, v) = do
  (outcome, typed) <- collectElaborable spec True typecheckRoot (gi, v)
  k0m <- termOf gi
  let spliceRoot = do
        backs <- liftIO (newIORef Set.empty)
        splice spec backs [] typed
  case outcome of
    Elaborable -> case (k0m, typed) of
      -- the export is itself a checked definition: a use of it, at its own
      -- type, inside what it reaches is a back-edge to it
      (Just k0, AnnoS (Idx gt t) c (VarS name alts))
        | Map.member k0 (specDefs spec) -> do
            backs <- liftIO (newIORef Set.empty)
            alts' <- mapM (splice spec backs [(k0, canonical (snd (unqualify t)))]) alts
            back <- Set.member (k0, canonical (snd (unqualify t))) <$> liftIO (readIORef backs)
            if back
              then do
                let tok = recName v k0
                MM.modify' (\st -> st {stateRecursionTargets = Map.insert tok gi (stateRecursionTargets st)})
                return (AnnoS (Idx gt t) c (VarS tok alts'))
              else return (AnnoS (Idx gt t) c (VarS name alts'))
      _ -> spliceRoot
    InPlace -> spliceRoot
    Generic -> return typed

-- | How far a checked tree was taken.
data Collected
  = -- | every reference can be elaborated: ready to splice
    Elaborable
  | -- | checked with every term expanded in place
    InPlace
  | -- | a generic export (SIG-5), checked with its references left as their
    -- signatures; it is never realized, so nothing is chosen for it
    Generic

-- | Whether an export's type keeps a type variable, so that it is not
-- realized (SIG-5).
isGenericExport :: TypeU -> Bool
isGenericExport = containsUnk . typeOf

-- | Collect a tree and check it, expanding in place any term whose reference
-- cannot be elaborated on its own, and leaving the rest to be shared.
--
-- Each pass expands at least one more term than the last, so this
-- terminates. A stuck reference this pass cannot expand (a class method)
-- leaves the set unchanged, and the final pass expands everything. With
-- @export@ set, an export whose type is still generic once no definition is
-- left to expand is not taken further.
collectElaborable
  :: Spec
  -> Bool
  -> (AnnoS Int ManyPoly Int -> MorlocMonad Typed)
  -> (Int, EVar)
  -> MorlocMonad (Collected, Typed)
collectElaborable spec export check root = go (Map.keysSet (specDefs spec))
  where
    go only = do
      typed@(AnnoS (Idx _ rootType) _ _) <- collectRoot (Just only) root >>= check
      stuck <- map fst . filter (not . isElaborable . snd) <$> references spec typed
      let only' = Set.difference only (Set.fromList stuck)
          -- decided only once no definition is left to expand, since a
          -- definition's body can fix what its signature leaves open
          generic = export && isGenericExport rootType
      if
        | not (null stuck) && Set.size only' < Set.size only -> go only'
        | generic -> return (Generic, typed)
        | null stuck -> return (Elaborable, typed)
        | otherwise -> (,) InPlace <$> (collectRoot Nothing root >>= check >>= reopenMethodBackEdges)

-- | A method back-edge at another type than the expansion it points to becomes
-- a reference, for 'splice' to elaborate at its own type. An open position does
-- not select an instance, so two types that agree elsewhere keep the back-edge;
-- any other difference needs a type to elaborate at.
reopenMethodBackEdges :: Typed -> MorlocMonad Typed
reopenMethodBackEdges = go Map.empty
  where
    go :: Map.Map EVar TypeU -> Typed -> MorlocMonad Typed
    go env (AnnoS idx@(Idx gi t) ci e) = case e of
      CallS v
        | Just target <- Map.lookup v env -> do
            sigs <- MM.gets stateSignatures
            if
              | agrees t target -> return (AnnoS idx ci e)
              | not (isMethodAt sigs ci) ->
                  if isElaborable t && isElaborable target
                    then
                      MM.throwCompilerBugAt gi $
                        "the call of" <+> squotes (pretty v) <+> "here is bound to its own definition, of type"
                          <+> pretty target <> ", at another type," <+> pretty t
                    else return (AnnoS idx ci e)
              | isElaborable t -> return (AnnoS idx ci (VarS (displayName v) (Many [] [])))
              | otherwise ->
                  MM.throwSourcedError gi $
                    "The type of this use of" <+> squotes (pretty (displayName v))
                      <+> "inside its own instance is not determined, so it cannot be compiled:"
                      <+> pretty t <> ". Add a signature or an annotation that fixes it."
      VarS v _ -> AnnoS idx ci <$> mapExprSM (go (Map.insert v t env)) e
      _ -> AnnoS idx ci <$> mapExprSM (go env) e

    agrees a b = case (a, b) of
      (ExistU {}, _) -> True
      (_, ExistU {}) -> True
      (ForallU _ x, _) -> agrees x b
      (_, ForallU _ y) -> agrees a y
      _ ->
        let heads = mapTypeUChildren (const BT.unitU)
         in heads a == heads b && and (zipWithSame agrees (typeUChildren a) (typeUChildren b))

-- | Whether an index names a class method.
isMethodAt :: GMap Int Int SignatureSet -> Int -> Bool
isMethodAt (GMap idmap sigmap) ci = case Map.lookup ci idmap >>= (`Map.lookup` sigmap) of
  Just Polymorphic {} -> True
  _ -> False

-- | The references to checked definitions in a tree, with the type each is
-- solved at.
references :: Spec -> Typed -> MorlocMonad [(Int, TypeU)]
references spec = go
  where
    go n@(AnnoS (Idx _ t) _ e) = do
      r <- refTerm spec n
      case r of
        Just (k, _) -> return [(k, t)]
        Nothing -> concat <$> mapM go (foldExprS (: []) e)

-- | What a reference names: a checked definition, or a class method.
data Target = Definition | Method
  deriving (Eq)

-- | The term a node references by its type alone: a checked definition or a
-- class method with no implementations spliced in yet.
refTerm :: Spec -> Typed -> MorlocMonad (Maybe (Int, Target))
-- The concrete index names the reference: an implementation's root keeps
-- the index of the use it was expanded at as its general index.
refTerm spec (AnnoS _ ci (VarS _ (Many _ []))) = do
  k <- termOf ci
  GMap _ sigmap <- MM.gets stateSignatures
  return $ case k of
    Just k'
      | Map.member k' (specDefs spec) -> Just (k', Definition)
      | Just (Polymorphic {}) <- Map.lookup k' sigmap -> Just (k', Method)
    _ -> Nothing
refTerm _ _ = return Nothing

-- | A type a definition can be elaborated at on its own: no unsolved
-- existential, and no free effect variable.
--
-- A free kind variable is allowed: no later pass solves it and it erases
-- before code generation, so two uses that leave it open compile
-- identically. An 'ExistU' is still to be solved by the enclosing gamma.
-- An effect variable is excluded because a signature's effect variables are
-- rigid when it is checked, but the row a use supplies is part of what the
-- use computes.
isElaborable :: TypeU -> Bool
isElaborable = go
  where
    go t = case t of
      ExistU {} -> False
      KVarU _ -> True
      ListVarU _ -> True
      SetVarU _ -> True
      VarU _ -> True
      ForallU _ x -> go x
      FunU ts x -> all go (x : ts)
      AppU x ts -> all go (x : ts)
      NamU _ _ ps rs -> all go ps && all (go . snd) rs
      EffectU es x -> closedEffects es && go x
      OptionalU x -> go x
      OpU _ ts -> all go ts
      LitU l -> case l of
        LList ts -> all go ts
        LSet ts -> all go ts
        LRec rs -> all (go . snd) rs
        _ -> True
      LabeledU _ x -> go x
      _ -> True
    closedEffects (EffectSet _) = True
    closedEffects (EffectVar _) = False
    closedEffects (EffectUnion a b) = closedEffects a && closedEffects b

-- | The variables the checker generated in a type, in order of appearance.
generatedVars :: TypeU -> [TVar]
generatedVars t = case t of
  VarU v | invented v -> [v]
  KVarU (v, _) | invented v -> [v]
  ListVarU v | invented v -> [v]
  SetVarU v | invented v -> [v]
  ForallU _ x -> generatedVars x
  FunU ts x -> concatMap generatedVars ts <> generatedVars x
  AppU x ts -> generatedVars x <> concatMap generatedVars ts
  NamU _ _ ps rs -> concatMap generatedVars ps <> concatMap (generatedVars . snd) rs
  EffectU _ x -> generatedVars x
  OptionalU x -> generatedVars x
  OpU _ ts -> concatMap generatedVars ts
  LitU l -> case l of
    LList ts -> concatMap generatedVars ts
    LSet ts -> concatMap generatedVars ts
    LRec rs -> concatMap (generatedVars . snd) rs
    _ -> []
  LabeledU _ x -> generatedVars x
  _ -> []
  where
    invented (TV v) = T.any (== '@') v

-- | The Type-kinded variables the checker generated in a type; 'shareable'
-- does not share a key holding one.
--
-- 'Morloc.Typecheck.Internal.renameWithMap' tags a generated variable by
-- kind: @\@q@ for Type, @\@n@, @\@s@, @\@r@, @\@l@, @\@e@ for the others.
generatedTypeVars :: TypeU -> [TVar]
generatedTypeVars t = [v | v@(TV n) <- generatedVars t, "@q" `T.isInfixOf` n]

-- | A key's type with the variables the checker generated renamed by order
-- of appearance, so equal instantiations compare equal.
canonical :: TypeU -> TypeU
canonical t0 = rename t0
  where
    generated = nubOrd (generatedVars t0)
    table = Map.fromList [(v, TV ("v@" <> T.pack (show k))) | (k, v) <- indexed generated]
    rename t = case t of
      VarU v | Just v' <- Map.lookup v table -> VarU v'
      KVarU (v, k) | Just v' <- Map.lookup v table -> KVarU (v', k)
      ListVarU v | Just v' <- Map.lookup v table -> ListVarU v'
      SetVarU v | Just v' <- Map.lookup v table -> SetVarU v'
      -- The binder is deliberately not renamed. Renaming it would make two
      -- instantiations of one quantified type into a single key, and a second
      -- use of an unresolved class method would then reuse the first's
      -- elaboration instead of being reported as ambiguous.
      ForallU v x -> ForallU v (rename x)
      FunU ts x -> FunU (map rename ts) (rename x)
      AppU x ts -> AppU (rename x) (map rename ts)
      NamU n v ps rs -> NamU n v (map rename ps) [(k, rename x) | (k, x) <- rs]
      EffectU es x -> EffectU es (rename x)
      OptionalU x -> OptionalU (rename x)
      OpU o ts -> OpU o (map rename ts)
      LitU (LList ts) -> LitU (LList (map rename ts))
      LitU (LSet ts) -> LitU (LSet (map rename ts))
      LitU (LRec rs) -> LitU (LRec [(k, rename x) | (k, x) <- rs])
      LabeledU v x -> LabeledU v (rename x)
      _ -> t

-- | The term @k@ checked at type @t@, once per key. A definition is
-- collected at its declaration; a method at @use@, any reference to it.
elaborate :: Spec -> Target -> (Int, EVar) -> SpecKey -> MorlocMonad Typed
elaborate spec target use key@(k, t) = do
  memo <- liftIO (readIORef (specMemo spec))
  case Map.lookup key memo of
    Just e -> return e
    Nothing -> do
      root <- case target of
        Method -> return use
        Definition -> case Map.lookup k (specDefs spec) of
          Just x -> return x
          Nothing -> MM.throwCompilerBug $ "specialize: no definition for term" <+> pretty k
      -- checked at the key; a method's result is the implementations of
      -- the instance the key selects
      let check = typecheckRootAt t
      e <- snd <$> collectElaborable spec False check root
      -- A body may determine a kind-tagged slot that the key it is filed
      -- under leaves open: a signature's kind variables are not rigid when it
      -- is checked, so @q :: Int -> Table m r@ whose body has type
      -- @Int -> Table n {x = Int}@ is accepted. Copied into the use it was
      -- checked for, that is what the use asked for. Lifted into a root every
      -- use of the key calls, the root's boundary would be built from the open
      -- key and its interior from the fixed form, and the two would disagree.
      -- Keep such a key out of the shared roots.
      when (pinsKey key e) $
        liftIO (modifyIORef' (specInlined spec) (Set.insert key))
      liftIO (modifyIORef' (specMemo spec) (Map.insert key e))
      return e

-- | Whether an elaboration means something narrower than the key it was
-- checked at. The check runs over the elaboration's own root type because
-- 'Morloc.Frontend.Typecheck.typecheckRootAt' applies the final substitution
-- to every annotation before returning, so a slot the body fixed is fixed
-- there too. A slot solved to another variable is not narrower: both sides
-- canonicalise to the same name.
--
-- 'Morloc.Typecheck.Internal.recheckDeferred' does not write its solutions
-- back, so a slot fixed only by a deferred constraint reaches here open:
-- either the elaboration shows the value, and the key is not shared, or the
-- program leaves it open.
pinsKey :: SpecKey -> Typed -> Bool
pinsKey (_, t) (AnnoS (Idx _ t') _ _) = canonical t' /= t

-- | Replace each reference by a copy of its elaboration. @stack@ holds the
-- references being spliced around this point; @backs@ collects the keys a
-- back-edge was made to.
splice :: Spec -> IORef (Set.Set SpecKey) -> [SpecKey] -> Typed -> MorlocMonad Typed
splice spec backs stack n@(AnnoS (Idx gi t) ci e) = refTerm spec n >>= \r -> let key = canonical t in case r of
  Just (k, target)
    | (k, key) `elem` stack -> do
        liftIO (modifyIORef' backs (Set.insert (k, key)))
        return (AnnoS (Idx gi t) ci (CallS (token stack (k, key))))
    -- a definition recursing through others at a different type, or a
    -- method whose instances recurse at ever larger types
    | target == Definition && any ((== k) . fst) stack
        || length (filter ((== k) . fst) stack) >= methodDepth ->
        MM.throwSourcedError gi $
          "Polymorphic recursion is not supported: this use of"
            <+> squotes (pretty (refName e))
            <+> "is at a different type than the use it recurses from"
    | otherwise -> do
        elab <- elaborate spec target (ci, refName e) (k, key)
        shared <- Map.lookup (k, key) <$> liftIO (readIORef (specShared spec))
        case shared of
          Just tok -> sharedUse tok elab
          _ -> do
            copy <- instantiate elab
            case copy of
              AnnoS _ _ (VarS name (Many ev alts)) -> do
                inner <- liftIO (newIORef Set.empty)
                alts' <- mapM (traverse (splice spec inner ((k, key) : stack))) alts
                innerBacks <- liftIO (readIORef inner)
                liftIO (modifyIORef' backs (Set.union (Set.delete (k, key) innerBacks)))
                let name' = if Set.member (k, key) innerBacks then token ((k, key) : stack) (k, key) else name
                -- a body with no back-edge is closed: it means the same
                -- wherever it is used, so whether it is shared is decided once
                inlined <- Set.member (k, key) <$> liftIO (readIORef (specInlined spec))
                let closed = target == Definition && Set.null innerBacks
                share <-
                  if closed && not inlined
                    then shareable key t (map snd alts')
                    else return False
                if share
                  then do
                    tok <- newSpecName name
                    let (sourced, defined) = List.partition (isSourced . snd) alts'
                    root <- AnnoS <$> (Idx <$> plainIndex gi <*> pure t) <*> plainIndex ci <*> pure (VarS tok (Many ev defined))
                    liftIO (modifyIORef' (specShared spec) (Map.insert (k, key) tok))
                    MM.modify' (\st -> let Specs xs = stateSpecs st in st {stateSpecs = Specs (xs <> [root]), stateSpecNames = Set.insert tok (stateSpecNames st)})
                    callAt tok name ev sourced
                  else do
                    when closed $
                      liftIO (modifyIORef' (specInlined spec) (Set.insert (k, key)))
                    let alts'' = map (fmap (atUse ci)) alts'
                    return (AnnoS (Idx gi t) ci (VarS name' (Many (useEvidence ev) alts'')))
              _ -> MM.throwCompilerBug "specialize: an elaboration is not a term"
  Nothing -> AnnoS (Idx gi t) ci <$> mapExprSM (splice spec backs stack) e
  where
    -- one name per key on the stack: the instances of a method can recurse
    -- through each other, each its own function
    token keys sk@(k, _) =
      let base = recName (maybe (refName e) snd (Map.lookup k (specDefs spec))) k
       in case length (filter ((== k) . fst) (dropWhile (/= sk) keys)) of
            n | n > 1 -> EV (unEVar base <> "`" <> T.pack (show n))
            _ -> base
    refName (VarS v _) = v
    refName _ = EV "?"
    -- the reference's own evidence: how the use meets its constraints
    useEvidence ev = case e of
      VarS _ (Many evs _) -> List.nub (evs <> ev)
      _ -> ev
    methodDepth = 64
    -- a use of a shared specialization: its sourced implementations, each
    -- chosen per use, and a call of the shared one
    sharedUse tok (AnnoS _ _ (VarS name (Many ev alts))) = do
      sourced <- mapM (traverse instantiate) (filter (isSourced . snd) alts)
      callAt tok name ev sourced
    sharedUse _ _ = MM.throwCompilerBug "specialize: an elaboration is not a term"
    callAt tok name ev sourced = do
      let sourced' = map (fmap (atUse ci)) sourced
      call <- AnnoS <$> (Idx <$> plainIndex gi <*> pure t) <*> plainIndex ci <*> pure (CallS tok)
      return (AnnoS (Idx gi t) ci (VarS name (Many (useEvidence ev) (sourced' <> [(Nothing, call)]))))

isSourced :: Typed -> Bool
isSourced (AnnoS _ _ (ExeS (SrcCall _))) = True
isSourced _ = False

-- | A name for a shared specialization, never a term's or a recursion
-- token's.
newSpecName :: EVar -> MorlocMonad EVar
newSpecName name = do
  k <- MM.getCounter
  return (EV (unEVar (displayName name) <> "`p" <> T.pack (show k)))

-- | Whether a specialization, its references already spliced, is shared as a
-- root of its own rather than copied to each use. Sharing turns a use into a
-- call; it must not change what the program computes or how often:
--
-- * a function of at least one argument whose result is not a suspension;
-- * its key is fully solved, with no Type variable the checker invented;
-- * each implementation defined here is a value taking every argument at
--   once: a function built by a computation (a lambda doing work before the
--   function it returns, a partial application computing an argument or
--   reaching a stage) is a constant, evaluated once per command, not once
--   per call; and it does not recurse (a recursion keeps its back-edges to
--   the copy it is in; one defined inside it goes with it);
-- * it reads no top-level constant (evaluated once per command at the use,
--   it would be evaluated per call);
-- * it is large enough that copies compound.
shareable :: TypeU -> TypeU -> [Typed] -> MorlocMonad Bool
shareable key t alts = do
  names <- MM.gets stateSpecNames
  sigs <- MM.gets stateSignatures
  let (params, result) = arrow (snd (unqualify t))
      defined = filter (not . isSourced) alts
      ns = concatMap annoNodes defined
      inner = Set.fromList [v | AnnoS _ _ (VarS v _) <- ns]
  return $
    length ns + length [() | AnnoS _ _ PatS {} <- ns] > shareSize
      && not (null params)
      && not (isSuspension result)
      && isElaborable key
      && null (generatedTypeVars key)
      && all (takesAll (length params)) defined
      && all isValue defined
      && not (any (readsConstant sigs) ns)
      && not (any (\(AnnoS _ _ x) -> recurses names inner x) ns)
  where
    arrow (FunU ts r) = let (ts', r') = arrow r in (ts <> ts', r')
    arrow x = ([], x)
    isArrow (FunU _ _) = True
    isArrow (ForallU _ x) = isArrow x
    isArrow _ = False
    isSuspension (EffectU _ _) = True
    isSuspension _ = False
    -- directly nested lambdas are one parameter list
    takesAll n (AnnoS _ _ (LamS vs body)) = case body of
      AnnoS _ _ (LamS _ _) -> takesAll (n - length vs) body
      _ -> length vs == n
    takesAll _ _ = False
    -- a back-edge to the definition itself; one to a recursion defined
    -- inside it is closed with it
    recurses names defs (CallS v) = not (Set.member v names || Set.member v defs)
    recurses _ _ _ = False
    -- a top-level name, not a class method, whose value is computed once
    -- per command: data, or a function built by a computation
    readsConstant sigs (AnnoS (Idx _ ty) ci (VarS (EV v) xs)) =
      not (T.any (== '`') v)
        && not (isMethodAt sigs ci)
        && (not (isArrow ty) || not (all isValue xs))
    readsConstant _ _ = False


-- | The node count above which a closed specialization is shared. An applied
-- pattern counts its head as a node of its own.
shareSize :: Int
shareSize = 32

-- | An implementation spliced at a reference takes the reference's own index
-- as its outer index, as a term expanded in place does.
atUse :: Int -> Typed -> Typed
atUse gi (AnnoS (Idx _ t) ci e) = AnnoS (Idx gi t) ci e

-- | A copy of a tree with fresh indices (carrying the state of the indices
-- they copy) and fresh names for the variables it binds. A source call keeps
-- its source's index.
instantiate :: Typed -> MorlocMonad Typed
instantiate e0 = do
  seen <- liftIO (newIORef Map.empty)
  let go :: Map.Map EVar EVar -> Typed -> MorlocMonad Typed
      go names (AnnoS (Idx gi t) ci e) = do
        gi' <- freshIdx gi
        ci' <- case e of
          ExeS (SrcCall _) -> return ci
          _ -> freshIdx ci
        AnnoS (Idx gi' t) ci' <$> expr names e
      expr names e = case e of
        LamS vs body -> do
          pairs <- mapM (\v -> (,) v <$> fresh v) vs
          LamS (map snd pairs) <$> go (Map.union (Map.fromList pairs) names) body
        LetS v e1 e2 -> do
          e1' <- go names e1
          v' <- fresh v
          LetS v' e1' <$> go (Map.insert v v' names) e2
        BndS v -> return (BndS (Map.findWithDefault v v names))
        LetBndS v -> return (LetBndS (Map.findWithDefault v v names))
        IntS i x -> (\i' -> IntS i' x) <$> freshIdx i
        RealS i x -> (\i' -> RealS i' x) <$> freshIdx i
        _ -> mapExprSM (go names) e
      freshIdx i = do
        m <- liftIO (readIORef seen)
        case Map.lookup i m of
          Just j -> return j
          Nothing -> do
            j <- plainIndex i
            liftIO (modifyIORef' seen (Map.insert i j))
            return j
  go Map.empty e0

-- | A fresh index carrying @parent@'s state, never an export.
plainIndex :: Int -> MorlocMonad Int
plainIndex parent = do
  i <- newIndex parent
  MM.modify' (\s -> s {stateExports = filter (/= i) (stateExports s)})
  return i
