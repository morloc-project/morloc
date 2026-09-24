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
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Morloc.Frontend.Namespace
import Morloc.Frontend.Rename (fresh)
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
  memo <- liftIO (newIORef Map.empty)
  mapM (specializeExport (Spec (validationDefs checks) memo)) exports

specializeExport :: Spec -> (Int, EVar) -> MorlocMonad Typed
specializeExport spec (gi, v) = do
  let only = Map.keysSet (specDefs spec)
  typed <- collectRoot (Just only) (gi, v) >>= typecheckRoot
  ground <- all (isGround . snd) <$> references spec typed
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
    else collectRoot Nothing (gi, v) >>= typecheckRoot

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
-- existential, and no free kind or effect variable. A signature's kind and
-- effect variables are not rigid when it is checked, so a body may fix one;
-- only the use's own context would then see it.
isGround :: TypeU -> Bool
isGround t = case t of
  ExistU {} -> False
  KVarU _ -> False
  ListVarU _ -> False
  SetVarU _ -> False
  VarU _ -> True
  ForallU _ x -> isGround x
  FunU ts x -> all isGround (x : ts)
  AppU x ts -> all isGround (x : ts)
  NamU _ _ ps rs -> all isGround ps && all (isGround . snd) rs
  EffectU es x -> closedEffects es && isGround x
  OptionalU x -> isGround x
  OpU _ ts -> all isGround ts
  LitU l -> case l of
    LList ts -> all isGround ts
    LSet ts -> all isGround ts
    LRec rs -> all (isGround . snd) rs
    _ -> True
  LabeledU _ x -> isGround x
  _ -> True
  where
    closedEffects (EffectSet _) = True
    closedEffects (EffectVar _) = False
    closedEffects (EffectUnion a b) = closedEffects a && closedEffects b

-- | A key's type with the variables the checker generated renamed by order
-- of appearance, so equal instantiations compare equal.
canonical :: TypeU -> TypeU
canonical t0 = rename t0
  where
    generated = nubOrd (collect t0)
    table = Map.fromList (zip generated [TV ("v@" <> T.pack (show k)) | k <- [(0 :: Int) ..]])
    collect t = case t of
      VarU (TV v) | T.any (== '@') v -> [TV v]
      ForallU _ x -> collect x
      FunU ts x -> concatMap collect ts <> collect x
      AppU x ts -> collect x <> concatMap collect ts
      NamU _ _ ps rs -> concatMap collect ps <> concatMap (collect . snd) rs
      EffectU _ x -> collect x
      OptionalU x -> collect x
      OpU _ ts -> concatMap collect ts
      _ -> []
    rename t = case t of
      VarU v | Just v' <- Map.lookup v table -> VarU v'
      ForallU v x -> ForallU v (rename x)
      FunU ts x -> FunU (map rename ts) (rename x)
      AppU x ts -> AppU (rename x) (map rename ts)
      NamU n v ps rs -> NamU n v (map rename ps) [(k, rename x) | (k, x) <- rs]
      EffectU es x -> EffectU es (rename x)
      OptionalU x -> OptionalU (rename x)
      OpU o ts -> OpU o (map rename ts)
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
      let only = Map.keysSet (specDefs spec)
          -- checked as a use at the key is: a definition against its
          -- signature, a method by resolving its instances
          check = typecheckRootAt t
      typed <- collectRoot (Just only) root >>= check
      ground <- all (isGround . snd) <$> references spec typed
      e <-
        if ground
          then return typed
          else collectRoot Nothing root >>= check
      liftIO (modifyIORef' (specMemo spec) (Map.insert key e))
      return e

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
        copy <- instantiate elab
        case copy of
          AnnoS _ _ (VarS name (Many alts)) -> do
            inner <- liftIO (newIORef Set.empty)
            alts' <- mapM (splice spec inner ((k, key) : stack)) alts
            innerBacks <- liftIO (readIORef inner)
            liftIO (modifyIORef' backs (Set.union (Set.delete k innerBacks)))
            let name' = if Set.member k innerBacks then token k else name
            alts'' <- mapM (atUse ci) alts'
            return (AnnoS (Idx gi t) ci (VarS name' (Many alts'')))
          _ -> MM.throwCompilerBug "specialize: an elaboration is not a term"
  Nothing -> AnnoS (Idx gi t) ci <$> mapExprSM (splice spec backs stack) e
  where
    token k = recName (maybe (refName e) snd (Map.lookup k (specDefs spec))) k
    refName (VarS v _) = v
    refName _ = EV "?"
    methodDepth = 64

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
