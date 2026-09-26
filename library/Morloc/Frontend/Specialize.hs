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
checked with every term expanded in place.

Recursion between definitions is found while splicing: a reference to a
definition already being spliced at the same key becomes a 'CallS' back-edge
to it, and at a different key is polymorphic recursion, which is rejected.
-}
module Morloc.Frontend.Specialize (specialize) where

import Data.IORef
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Morloc.Frontend.Namespace
import Morloc.CodeGenerator.Value (etaParts)
import Morloc.Frontend.Rename (displayName, fresh)
import qualified Morloc.Frontend.Share as Share
import Morloc.Frontend.Treeify (Validation (..), collectRoot, recName)
import Morloc.Frontend.Typecheck (typecheckRoot, typecheckRootAt)
import Morloc.Data.Doc
import qualified Morloc.Monad as MM
import Morloc.Typecheck.Internal (unqualify)

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
  (ground, typed) <- collectElaborable spec typecheckRoot (gi, v)
  k0m <- termOf gi
  if ground
    then case (k0m, typed) of
      -- the export is itself a checked definition: a use of it, at its own
      -- type, inside what it reaches is a back-edge to it
      (Just k0, AnnoS (Idx gt t) c (VarS name alts))
        | Map.member k0 (specDefs spec) -> do
            backs <- liftIO (newIORef Set.empty)
            alts' <- mapM (splice spec backs [(k0, canonical (snd (unqualify t)))]) alts
            back <- Set.member k0 <$> liftIO (readIORef backs)
            if back
              then do
                let tok = recName v k0
                MM.modify (\st -> st {stateRecursionTargets = Map.insert tok gi (stateRecursionTargets st)})
                return (AnnoS (Idx gt t) c (VarS tok alts'))
              else return (AnnoS (Idx gt t) c (VarS name alts'))
      _ -> do
        backs <- liftIO (newIORef Set.empty)
        splice spec backs [] typed
    else return typed

-- | Collect a tree and check it, expanding in place any term whose reference
-- cannot be elaborated on its own, and leaving the rest to be shared. The
-- 'Bool' says whether every remaining reference can be elaborated, so the
-- caller knows whether the tree is ready to splice.
--
-- One such reference used to cost the whole tree its sharing. It need not:
-- a definition whose element type nothing determines is expanded where it is
-- used, and a deep chain beside it is still shared.
--
-- Each pass expands at least one more term than the last, so this ends after
-- at most as many passes as there are terms to share; in practice the first
-- answers. A reference that is not a term this pass can expand -- a class
-- method, whose instances are held back whenever anything is -- leaves the
-- set unchanged, and the last pass expands everything, as before.
collectElaborable
  :: Spec
  -> (AnnoS Int ManyPoly Int -> MorlocMonad Typed)
  -> (Int, EVar)
  -> MorlocMonad (Bool, Typed)
collectElaborable spec check root = go (Map.keysSet (specDefs spec))
  where
    go only = do
      typed <- collectRoot (Just only) root >>= check
      stuck <- map fst . filter (not . isElaborable . snd) <$> references spec typed
      let only' = Set.difference only (Set.fromList stuck)
      if null stuck
        then return (True, typed)
        else
          if Set.size only' < Set.size only
            then go only'
            else (,) False <$> (collectRoot Nothing root >>= check)

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
refTerm spec (AnnoS _ ci (VarS _ (Many []))) = do
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
-- A free kind variable is allowed, and a free 'ExistU' is not, because of
-- what each one means downstream. A kind variable stands for a row count, a
-- column set or a label that the program never determines; every one of them
-- erases to the same thing before code generation ('typeOf'), and no pass
-- after this one can solve it, so two uses that both leave it open compile
-- identically and may share one elaboration. A use that does determine it
-- has a key with the value in it and gets an elaboration of its own. An
-- 'ExistU' is different: it is a variable the checker still intends to
-- solve, and its solution is the enclosing gamma's to give.
--
-- An effect variable is excluded because a signature's effect variables are
-- rigid when it is checked ('Morloc.Frontend.Typecheck.validate'), but the
-- row a use supplies is part of what the use computes.
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

-- | The Type-kinded variables the checker generated in a type.
--
-- A kind-tagged variable is not one of these. The reason is not that it
-- erases small: it is that a Type variable is a wildcard in implementation
-- selection and a kind variable is not. An unsolved Type variable reaches
-- 'Morloc.CodeGenerator.Realize' as @UnkT@, which matches any candidate, so
-- which implementation a use gets can depend on the use. A free Nat reaches
-- it as @NatVoidT@, compatible only with a Nat literal, and a free row as a
-- phantom; neither can steer the choice. So a root shared under a kind
-- variable means the same thing at every use, and one shared under an
-- invented Type variable need not.
--
-- 'Morloc.Typecheck.Internal.renameWithMap' writes the kind into the name it
-- invents -- @\@q@ for a Type variable, @\@n@, @\@s@, @\@r@, @\@l@,
-- @\@e@ for the others -- so the tag answers this exactly.
generatedTypeVars :: TypeU -> [TVar]
generatedTypeVars t = [v | v@(TV n) <- generatedVars t, "@q" `T.isInfixOf` n]

-- | A key's type with the variables the checker generated renamed by order
-- of appearance, so equal instantiations compare equal.
canonical :: TypeU -> TypeU
canonical t0 = rename t0
  where
    generated = nubOrd (generatedVars t0)
    table = Map.fromList (zip generated [TV ("v@" <> T.pack (show k)) | k <- [(0 :: Int) ..]])
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
      -- checked as a use at the key is: a definition against its signature,
      -- a method by resolving its instances
      let check = typecheckRootAt t
      e <- snd <$> collectElaborable spec check root
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
-- This is also what keeps sharing independent of how much the typechecker
-- writes back. 'Morloc.Typecheck.Internal.recheckDeferred' currently drops
-- the substitutions it computes, so a slot determined only by a deferred
-- constraint reaches here open. Either the elaboration shows the value, and
-- the key is not shared, or the program really does leave it open. Teaching
-- the recheck to write its solutions back would move such a key from the
-- first case to a ground key of its own, and neither outcome shares an
-- elaboration across two different values.
pinsKey :: SpecKey -> Typed -> Bool
pinsKey (_, t) (AnnoS (Idx _ t') _ _) = canonical t' /= t

-- | Replace each reference by a copy of its elaboration. @stack@ holds the
-- references being spliced around this point; @backs@ collects the terms a
-- back-edge was made to.
splice :: Spec -> IORef (Set.Set Int) -> [SpecKey] -> Typed -> MorlocMonad Typed
splice spec backs stack n@(AnnoS (Idx gi t) ci e) = refTerm spec n >>= \r -> let key = canonical t in case r of
  Just (k, target)
    | (k, key) `elem` stack -> do
        liftIO (modifyIORef' backs (Set.insert k))
        return (AnnoS (Idx gi t) ci (CallS (token k)))
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
        labeledUse <- configured [gi, ci]
        shared <- Map.lookup (k, key) <$> liftIO (readIORef (specShared spec))
        case shared of
          Just tok | not labeledUse -> sharedUse tok elab
          _ -> do
            copy <- instantiate elab
            case copy of
              AnnoS _ _ (VarS name (Many alts)) -> do
                inner <- liftIO (newIORef Set.empty)
                alts' <- mapM (splice spec inner ((k, key) : stack)) alts
                innerBacks <- liftIO (readIORef inner)
                liftIO (modifyIORef' backs (Set.union (Set.delete k innerBacks)))
                let name' = if Set.member k innerBacks then token k else name
                -- a body with no back-edge is closed: it means the same
                -- wherever it is used, so whether it is shared is decided once
                inlined <- Set.member (k, key) <$> liftIO (readIORef (specInlined spec))
                let closed = target == Definition && Set.null innerBacks && not labeledUse
                share <-
                  if closed && not inlined
                    then shareable key t alts'
                    else return False
                if share
                  then do
                    tok <- newSpecName name
                    let (sourced, defined) = List.partition isSourced alts'
                    root <- AnnoS <$> (Idx <$> plainIndex gi <*> pure t) <*> plainIndex ci <*> pure (VarS tok (Many defined))
                    liftIO (modifyIORef' (specShared spec) (Map.insert (k, key) tok))
                    MM.modify (\st -> let Specs xs = stateSpecs st in st {stateSpecs = Specs (xs <> [root]), stateSpecNames = Set.insert tok (stateSpecNames st)})
                    callAt tok name sourced
                  else do
                    when closed $
                      liftIO (modifyIORef' (specInlined spec) (Set.insert (k, key)))
                    alts'' <- mapM (atUse ci) alts'
                    return (AnnoS (Idx gi t) ci (VarS name' (Many alts'')))
              _ -> MM.throwCompilerBug "specialize: an elaboration is not a term"
  Nothing -> AnnoS (Idx gi t) ci <$> mapExprSM (splice spec backs stack) e
  where
    token k = recName (maybe (refName e) snd (Map.lookup k (specDefs spec))) k
    refName (VarS v _) = v
    refName _ = EV "?"
    methodDepth = 64
    -- a use of a shared specialization: its sourced implementations, each
    -- chosen per use as before, and a call of the shared one
    sharedUse tok (AnnoS _ _ (VarS name (Many alts))) = do
      sourced <- mapM instantiate (filter isSourced alts)
      callAt tok name sourced
    sharedUse _ _ = MM.throwCompilerBug "specialize: an elaboration is not a term"
    callAt tok name sourced = do
      sourced' <- mapM (atUse ci) sourced
      call <- AnnoS <$> (Idx <$> plainIndex gi <*> pure t) <*> plainIndex ci <*> pure (CallS tok)
      return (AnnoS (Idx gi t) ci (VarS name (Many (sourced' <> [call]))))

isSourced :: Typed -> Bool
isSourced (AnnoS _ _ (ExeS (SrcCall _))) = True
isSourced _ = False

-- | Whether any of these indices carries manifold configuration (a label, a
-- cache, a remote setting): such a use stays in place, so its configuration
-- stays with it.
configured :: [Int] -> MorlocMonad Bool
configured is = do
  cfg <- MM.gets stateManifoldConfig
  return (any (maybe False Share.configured . (`Map.lookup` cfg)) is)

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
-- * its key is fully solved, with no variable the checker invented;
-- * each implementation defined here takes every argument at once (a lambda
--   doing work before the function it returns is staged per use), and does
--   not recurse (a recursion keeps its back-edges to the copy it is in; one
--   defined inside it goes with it);
-- * it reads no top-level constant (evaluated once per command at the use,
--   it would be evaluated per call);
-- * it is large enough that copies compound.
shareable :: TypeU -> TypeU -> [Typed] -> MorlocMonad Bool
shareable key t alts = do
  names <- MM.gets stateSpecNames
  GMap idmap sigmap <- MM.gets stateSignatures
  let (params, result) = arrow (snd (unqualify t))
      defined = filter (not . isSourced) alts
      ns = concatMap annoNodes defined
      inner = Set.fromList [v | AnnoS _ _ (VarS v _) <- ns]
  labeled <- configured (concat [[gi, ci] | AnnoS (Idx gi _) ci _ <- defined])
  return $
    not (null (drop shareSize ns))
      && not (null params)
      && not (isSuspension result)
      && isElaborable key
      && null (generatedTypeVars key)
      && all (takesAll (length params)) defined
      && not (any (readsConstant idmap sigmap) ns)
      && not (any (\(AnnoS _ _ x) -> recurses names inner x) ns)
      && not labeled
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
    readsConstant idmap sigmap (AnnoS (Idx _ ty) ci (VarS (EV v) (Many xs))) =
      not (T.any (== '`') v)
        && not (isMethod idmap sigmap ci)
        && (not (isArrow ty) || not (all built xs))
    readsConstant _ _ _ = False
    -- a function that exists without computing anything
    built n@(AnnoS _ _ e) = case e of
      LamS _ _ | Just (f, pre) <- etaParts n -> all built (f : pre)
      LamS _ _ -> True
      ExeS _ -> True
      BndS _ -> True
      IntS _ _ -> True
      RealS _ _ -> True
      StrS _ -> True
      LogS _ -> True
      UniS -> True
      NullS -> True
      CallS _ -> True
      VarS _ (Many xs) -> all built xs
      _ -> False
    isMethod idmap sigmap ci = case Map.lookup ci idmap >>= (`Map.lookup` sigmap) of
      Just (Polymorphic {}) -> True
      _ -> False


-- | The node count above which a closed specialization is shared.
shareSize :: Int
shareSize = 32

-- | An implementation spliced at a reference takes the reference's own index
-- as its outer index, as a term expanded in place does, and a label on the
-- implementation applies at the reference.
atUse :: Int -> Typed -> MorlocMonad Typed
atUse gi (AnnoS (Idx _ t) ci e) = do
  st <- MM.get
  case Map.lookup ci (stateManifoldConfig st) of
    Just cfg
      | Just _ <- manifoldConfigLabel cfg ->
          MM.modify (\s -> s {stateManifoldConfig = Map.insert gi cfg (stateManifoldConfig s)})
    _ -> return ()
  return (AnnoS (Idx gi t) ci e)

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
          vs' <- mapM fresh vs
          LamS vs' <$> go (Map.union (Map.fromList (zip vs vs')) names) body
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
  MM.modify (\s -> s {stateExports = filter (/= i) (stateExports s)})
  return i
