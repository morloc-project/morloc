{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Specialize
Description : Typecheck each definition once per type it is used at
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A signed top-level definition of any module has been checked against its
signature ('Morloc.Frontend.Typecheck.validate'). Its uses therefore need only
its signature: an export is typechecked with each reference to such a
definition left as the signature alone, and the type the reference is solved
at (its key) selects one elaboration of the definition: its body checked once
against its signature and instantiated at the key, shared by every reference
at the same key. A method use's elaboration is the implementations of the
instance its key selects, each checked once and instantiated likewise.

Each reference is then replaced by a copy of its elaboration with fresh
indices and binder names, so the output has the same shape as a tree whose
every term was expanded in place, and every later pass is unchanged.

A reference whose key is not fully solved (an existential remains) cannot be
elaborated on its own: a definition is then expanded in place, and so is a
method at its uses, its instances' own method uses still references. A generic export is never realized (SIG-5), so it is checked with
its references left as their signatures and nothing is chosen for it.

Recursion between definitions is found while splicing: a reference to a
definition already being spliced at the same key becomes a 'CallS' back-edge
to it. At a different key it is polymorphic recursion, which is rejected,
unless a method lies between the two (an instance calling the definition on a
component): that is admitted at a key not met on the path, up to a bound.
-}
module Morloc.Frontend.Specialize (specialize, isGenericExport) where

import Morloc.Data.Zip (indexed, zipPrefix)
import Data.IORef
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Morloc.Frontend.Namespace
import qualified Morloc.Data.GMap as GMap
import Morloc.CodeGenerator.Value (isValue)
import Morloc.Frontend.Rename (displayName, fresh)

import Morloc.Frontend.Treeify (Validation (..), collectRoot, instanceBodySites, instanceBodyTree, recName)
import Morloc.Frontend.Typecheck (Checked, checkSigned, checkedAt, chosenLengthsApplied, defaultedExport, typecheckRoot, typecheckRootAt, useConstraint)
import qualified Morloc.Frontend.Instances as FI
import Morloc.Typecheck.Internal (emptyGamma, typeUChildren)
import System.Environment (lookupEnv)
import Control.Monad.Except (catchError, throwError)
import System.IO (hPutStrLn, stderr)
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
  , specShadow :: Bool
  -- ^ MORLOC_SHADOW_ELABORATE: also check each instantiated definition at
  -- its key and report how the two differ
  , specMethods :: Set.Set Int
  -- ^ the program's class methods, by term id
  , specBodySites :: Map.Map Int (Int, [Constraint])
  -- ^ each instance method body by its expression's index: its binding's
  -- index and its instance declaration's context
  , specBodies :: IORef (Map.Map (Int, Set.Set Int) Checked)
  -- ^ each instance method implementation checked once against the method's
  -- type at the instance, per collection of it
  , specChecked :: IORef (Map.Map (Int, Int, Set.Set Int, Set.Set Int) Checked)
  -- ^ each definition checked against its signature once per collection of
  -- it: its term, the index it is collected at (an export's or its
  -- declaration's), and the collection's sets
  , specInferred :: Set.Set Int
  -- ^ the definitions whose type was inferred (POLY-8)
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
  shadow <- isJust <$> liftIO (lookupEnv "MORLOC_SHADOW_ELABORATE")
  GMap _ sigmap <- MM.gets stateSignatures
  let methods = Set.fromList [k | (k, Polymorphic {}) <- Map.toList sigmap]
  spec <- liftIO (Spec (validationDefs checks) <$> newIORef Map.empty <*> newIORef Map.empty <*> newIORef Set.empty <*> pure shadow <*> pure methods <*> pure (instanceBodySites (validationModules checks)) <*> newIORef Map.empty <*> newIORef Map.empty <*> pure (validationInferred checks))
  mapM (specializeExport spec) exports

specializeExport :: Spec -> (Int, EVar) -> MorlocMonad Typed
specializeExport spec (gi, v) = do
  k0m <- termOf gi
  sigs <- MM.gets stateSignatures
  classes <- MM.gets (ieClasses . stateInstances)
  -- a signed export at a concrete type is its body checked once against its
  -- signature, as every signed definition is; an unsigned export is a use
  -- of its inferred type, whose numeric variables default there (NUM-10)
  let signedAt = case (k0m, GMap.lookup gi sigs) of
        (Just k0, GMapJust (Monomorphic tt))
          | Map.member k0 (specDefs spec)
          , Just et <- termGeneral tt
          , t <- if Set.member k0 (specInferred spec) then defaultedExport classes et else etype et
          , not (isGenericExport t) -> Just (k0, t)
        _ -> Nothing
      check only inline tree = case signedAt of
        Just (k0, st)
          | Set.null inline -> instantiated spec typecheckRoot (k0, only, inline) st tree
          | Set.member k0 (specInferred spec) -> typecheckRootAt st tree
        _ -> typecheckRoot tree
  (outcome, typed) <- collectElaborable spec True check (gi, v)
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
                -- named as the splice names a back-edge to it: by the name
                -- it is declared with, not an alias it is exported under
                let tok = recName (maybe v snd (Map.lookup k0 (specDefs spec))) k0
                MM.modify' (\st -> st {stateRecursionTargets = Map.insert tok gi (stateRecursionTargets st)})
                return (AnnoS (Idx gt t) c (VarS tok alts'))
              else return (AnnoS (Idx gt t) c (VarS name alts'))
      _ -> spliceRoot
    Generic -> return typed

-- | How far a checked tree was taken.
data Collected
  = -- | every reference can be elaborated: ready to splice
    Elaborable
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
-- terminates: a stuck definition is expanded, and a method with a stuck use is
-- expanded at each of its uses, its instances' own uses of it left as
-- references. A reference still stuck when a pass expands nothing new is
-- refused. With @export@ set, an export whose type is still generic
-- once no definition is left to expand is not taken further.
collectElaborable
  :: Spec
  -> Bool
  -> (Set.Set Int -> Set.Set Int -> AnnoS Int ManyPoly Int -> MorlocMonad Typed)
  -> (Int, EVar)
  -> MorlocMonad (Collected, Typed)
collectElaborable spec export check root = go (Map.keysSet (specDefs spec)) Set.empty
  where
    go only inline = do
      typed@(AnnoS (Idx _ rootType) _ _) <- collectRoot only inline root >>= check only inline
      stuck <- filter (\r -> not (isElaborable (refType r))) <$> references spec typed
      let only' = Set.difference only (Set.fromList (map refKey stuck))
          inline' = Set.union inline (Set.fromList [refKey r | r <- stuck, refTarget r == Method])
          -- decided only once no definition is left to expand, since a
          -- definition's body can fix what its signature leaves open
          generic = export && isGenericExport rootType
      case stuck of
        _ | Set.size only' < Set.size only -> go only' inline'
        _ | generic -> return (Generic, typed)
        [] -> return (Elaborable, typed)
        _ | Set.size inline' > Set.size inline -> go only' inline'
        r : _ ->
          MM.throwSourcedError (refIndex r) $
            "The type of this use of" <+> squotes (pretty (refVar r))
              <+> "is not determined, so it cannot be compiled. Add a signature or an annotation that fixes it."

-- | Whether an index names a class method.
isMethodAt :: GMap Int Int SignatureSet -> Int -> Bool
isMethodAt (GMap idmap sigmap) ci = case Map.lookup ci idmap >>= (`Map.lookup` sigmap) of
  Just Polymorphic {} -> True
  _ -> False

-- | A reference in a tree: the term, what it names, the type it is solved at,
-- and its use.
data Reference = Reference
  { refKey :: Int
  , refTarget :: Target
  , refType :: TypeU
  , refIndex :: Int
  , refVar :: EVar
  }

-- | The references to checked definitions and methods in a tree.
references :: Spec -> Typed -> MorlocMonad [Reference]
references spec = go
  where
    go n@(AnnoS (Idx gi t) _ e) = do
      r <- refTerm spec n
      case (r, e) of
        (Just (k, target), VarS v _) -> return [Reference k target t gi (displayName v)]
        _ -> concat <$> mapM go (foldExprS (: []) e)

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
      -- A definition at a solved key is its body checked once against its
      -- signature, instantiated at the key; a method's is the selected
      -- instance's implementations, each checked once. A collection with a
      -- method expanded in place is checked at the key: a check against a
      -- signature does not check instance bodies.
      let check only inline tree = case target of
            Definition | isElaborable t, Set.null inline -> instantiated spec (typecheckRootAt t) (k, only, inline) t tree
            Method | isElaborable t, Set.null inline -> instantiatedMethod spec only t tree
            _ -> typecheckRootAt t tree
      e <- snd <$> collectElaborable spec False check root
      -- A body may determine a kind-tagged slot that the key it is filed
      -- under leaves open: a length that occurs only in the result is the
      -- function's to choose (DIM-10), so @q :: Int -> Vector m Int@ whose
      -- body has type @Int -> Vector 3 Int@ is accepted. Copied into the use it was
      -- checked for, that is what the use asked for. Lifted into a root every
      -- use of the key calls, the root's boundary would be built from the open
      -- key and its interior from the fixed form, and the two would disagree.
      -- Keep such a key out of the shared roots.
      when (pinsKey key e) $
        liftIO (modifyIORef' (specInlined spec) (Set.insert key))
      liftIO (modifyIORef' (specMemo spec) (Map.insert key e))
      return e

-- | A definition's collected tree at a key, from its body checked once
-- against its signature. A result that keeps an unsolved type cannot be
-- compiled and is checked at the key instead. In shadow mode the tree is
-- also checked at the key, and each difference is reported.
instantiated :: Spec -> (AnnoS Int ManyPoly Int -> MorlocMonad Typed) -> (Int, Set.Set Int, Set.Set Int) -> TypeU -> AnnoS Int ManyPoly Int -> MorlocMonad Typed
instantiated spec atKeyCheck (k, only, inline) t tree = do
  chosenLengthsApplied tree
  let memoKey = (k, case tree of AnnoS gi _ _ -> gi, only, inline)
  memo <- liftIO (readIORef (specChecked spec))
  (checked, reused) <- case Map.lookup memoKey memo of
    Just c -> return (c, True)
    Nothing -> do
      c <- checkSigned tree
      liftIO (modifyIORef' (specChecked spec) (Map.insert memoKey c))
      return (c, False)
  -- each key's elaboration is a tree of its own: one checked tree serves
  -- every key, so a reuse is copied with fresh indices and names
  let fresh e = if reused then instantiate e else return e
  if specShadow spec
    then do
      via <- (Right <$> checkedAt t checked) `catchError` (return . Left)
      atKey <- (Right <$> atKeyCheck tree) `catchError` (return . Left)
      case (via, atKey) of
        (Right e, Right a) -> do
          liftIO (reportDifferences (k, only, inline) t e a)
          when (holdsExistential e) $ say ("unsolved after instantiation at " <> T.unpack (render (pretty t)))
          if holdsExistential e then return a else fresh e
        (Left err, Right a) -> say ("refused, where checking at the key accepts: " <> take 300 (show err)) >> return a
        (Right e, Left err) -> say ("accepted, where checking at the key refuses: " <> take 300 (show err)) >> fresh e
        (Left err, Left _) -> throwError err
    else do
      e <- checkedAt t checked
      if holdsExistential e then atKeyCheck tree else fresh e
  where
    say msg = liftIO (hPutStrLn stderr ("SHADOW term " <> show k <> " " <> msg))
    holdsExistential e = not (null [() | AnnoS (Idx _ ty) _ _ <- annoNodes e, ExistU {} <- universe ty])
    universe ty = ty : concatMap universe (typeUChildren ty)

-- | A method use at a key: the instance the key selects, and its
-- implementations, each body checked once against the method's type at the
-- instance and instantiated at the key, so no other instance's body is
-- checked. A derived comparison, a representation cast, or a body its check
-- refuses is checked at the key instead, with every candidate.
instantiatedMethod :: Spec -> Set.Set Int -> TypeU -> AnnoS Int ManyPoly Int -> MorlocMonad Typed
instantiatedMethod spec only t tree = case tree of
  AnnoS gi ci (VarS v (PolymorphicExpr cls m t0 _)) -> do
    scope <- MM.getGeneralScope
    env <- MM.gets stateInstances
    mk <- termOf ci
    c <- useConstraint scope emptyGamma env cls m (etype t0) t
    let selected = case FI.resolve FI.usePolicy scope env [] c of
          Right ev@(EvInstance {})
            | Right impls <- FI.methodImpls FI.defaultPolicy env ev m
            , not (null impls)
            , not (any isCastImpl impls)
            , Just k <- mk
            , Right (primary, _) <- FI.methodSources FI.defaultPolicy env ev m
            , Just mt <- listToMaybe [FI.mtType x | x <- FI.methodTerms env cls m, FI.mtKey x == primary] ->
                Just (ev, k, mt, [i | i@(ImplSource {}) <- impls] <> [i | i@(ImplBody {}) <- impls])
          _ -> Nothing
    case selected of
      Nothing -> typecheckRootAt t tree
      Just (ev, k, mt, impls) -> do
        alts <- (Just <$> mapM (implAt gi v k mt) impls) `catchError` \err -> case err of
          Rejected _ _ -> return Nothing
          _ -> throwError err
        case alts of
          Nothing -> do
            when (specShadow spec) $ liftIO (hPutStrLn stderr ("SHADOW method " <> T.unpack (unEVar m) <> " body refused at " <> T.unpack (render (pretty t))))
            typecheckRootAt t tree
          Just as -> do
            let out = AnnoS (Idx gi t) ci (VarS v (Many [ev] [(evidenceInstance ev, a) | a <- as]))
            when (specShadow spec) $ do
              atKey <- (Right <$> typecheckRootAt t tree) `catchError` (return . Left)
              case atKey of
                Right a -> liftIO (reportDifferences (-1, Set.empty, Set.empty) t out a)
                Left err -> liftIO (hPutStrLn stderr ("SHADOW method " <> T.unpack (unEVar m) <> " accepted, where checking at the key refuses: " <> take 300 (show err)))
            return out
  _ -> typecheckRootAt t tree
  where
    isCastImpl ImplCast {} = True
    isCastImpl _ = False
    -- a source is checked against the method's type at the instance, as a
    -- body is, so that it is shaped (eta-expanded, coerced) as there
    implAt gi v _ mt (ImplSource _ isrc@(Idx sci _)) =
      checkedImpl gi (sci, only) $ do
        let call = AnnoS sci sci (ExeS (SrcCall isrc))
        return (AnnoS sci sci (VarS v (MonomorphicExpr (Just mt) [call])))
    implAt gi v k _ (ImplBody et body@(ExprI bi _)) =
      checkedImpl gi (bi, only) $ do
        (i, ctx) <- maybe (MM.throwCompilerBug ("specialize: an instance body with no site:" <+> pretty bi)) return (Map.lookup bi (specBodySites spec))
        instanceBodyTree (Just only) k i v et ctx body
    implAt _ _ _ _ (ImplCast _ _) = MM.throwCompilerBug "specialize: a representation cast reached instance instantiation"
    checkedImpl gi memoKey collectIt = do
      memo <- liftIO (readIORef (specBodies spec))
      checked <- case Map.lookup memoKey memo of
        Just c -> return c
        Nothing -> do
          c <- collectIt >>= checkSigned
          liftIO (modifyIORef' (specBodies spec) (Map.insert memoKey c))
          return c
      e <- checkedAt t checked >>= instantiate
      case e of
        AnnoS _ _ (VarS _ (Many _ [(_, alt)])) -> return (atUse gi alt)
        _ -> MM.throwCompilerBug "specialize: a checked instance implementation is not one implementation"

-- | The first node at which two elaborations of one tree differ, in shape or
-- in type, walking both in order (generated nodes take fresh indices in each).
reportDifferences :: (Int, Set.Set Int, Set.Set Int) -> TypeU -> Typed -> Typed -> IO ()
reportDifferences (k, _, _) t a b =
  let walk :: Typed -> [(T.Text, T.Text)]
      walk e = [(tag x, render (pretty (canonical ty))) | AnnoS (Idx _ ty) _ x <- annoNodes e]
      tag :: ExprS (Indexed TypeU) Many Int -> T.Text
      tag x = case x of
        VarS v _ -> "VarS " <> unEVar v
        AppS _ _ -> "AppS"
        LamS _ _ -> "LamS"
        BndS _ -> "BndS"
        CallS v -> "CallS " <> unEVar v
        CoerceS _ _ -> "CoerceS"
        IntS _ _ -> "IntS"
        RealS _ _ -> "RealS"
        ExeS _ -> "ExeS"
        PatS {} -> "PatS"
        IntrinsicS _ _ -> "IntrinsicS"
        LetS v _ _ -> "LetS " <> unEVar v
        _ -> "other"
      wa = walk a
      wb = walk b
      firstDiff = [(i, x, y) | (i, (x, y)) <- indexed (zipPrefix wa wb), x /= y]
   in case (firstDiff, length wa == length wb) of
        ([], True) -> return ()
        (d, _) ->
          hPutStrLn stderr . T.unpack $
            "SHADOW term " <> T.pack (show k) <> " at " <> render (pretty t) <> ": "
              <> T.pack (show (length wa)) <> " vs " <> T.pack (show (length wb)) <> " nodes"
              <> case d of
                (i, (xa, ta), (xb, tb)) : _ ->
                  "; first at " <> T.pack (show i) <> ": " <> xa <> " " <> ta <> " /= " <> xb <> " " <> tb
                [] -> ""

-- | Whether an elaboration means something narrower than the key it was
-- checked at. The check runs over the elaboration's own root type because
-- 'Morloc.Frontend.Typecheck.typecheckRootAt' applies the final substitution
-- to every annotation before returning, so a slot the body fixed is fixed
-- there too. A slot solved to another variable is not narrower: both sides
-- canonicalise to the same name.
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
    -- method whose instances recurse at ever larger types. A definition met
    -- again through a method (an instance calling it on a component) is
    -- admitted at a type not met on this path, as a method is: a recursion
    -- over finitely many types ends at a back-edge, and the bound stops one
    -- that does not.
    | target == Definition && any ((== k) . fst) stack && not (throughMethod k)
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
                    -- a copy renamed for its key takes its own self-calls
                    -- with it, so none binds to a copy at another key
                    let alts'' = map (fmap (atUse ci . renameSelfCalls name name')) alts'
                    return (AnnoS (Idx gi t) ci (VarS name' (Many (useEvidence ev) alts'')))
              _ -> MM.throwCompilerBug "specialize: an elaboration is not a term"
  Nothing -> AnnoS (Idx gi t) ci <$> mapExprSM (splice spec backs stack) e
  where
    -- one name per key on the stack: the instances of a method can recurse
    -- through each other, each its own function
    token keys sk@(k, _) =
      let base = recName (maybe (refName e) snd (Map.lookup k (specDefs spec))) k
       in case length (filter ((== k) . fst) (dropWhile (/= sk) keys)) of
            c | c > 1 -> EV (unEVar base <> "`" <> T.pack (show c))
            _ -> base
    refName (VarS v _) = v
    refName _ = EV "?"
    -- the reference's own evidence: how the use meets its constraints
    useEvidence ev = case e of
      VarS _ (Many evs _) -> List.nub (evs <> ev)
      _ -> ev
    methodDepth = 64
    throughMethod k =
      any ((`Set.member` specMethods spec) . fst) (takeWhile ((/= k) . fst) stack)
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

-- | A tree with each back-edge calling @old@ calling @new@ instead, except
-- inside a nested definition named @old@, whose own back-edges they are.
renameSelfCalls :: EVar -> EVar -> Typed -> Typed
renameSelfCalls old new tree
  | old == new = tree
  | otherwise = go tree
  where
    go (AnnoS g c e) = AnnoS g c $ case e of
      CallS v | v == old -> CallS new
      VarS v _ | v == old -> e
      _ -> MM.runIdentity (mapExprSM (pure . go) e)

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
