{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Infer
Description : Infer concrete (language-specific) types from type aliases
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Maps general types to their concrete counterparts by evaluating type
aliases in the language-specific scope. Used by 'Express' and 'Serialize'
to determine how values are represented in each target language.
-}
module Morloc.CodeGenerator.Infer
  ( getScope
  , inferConcreteType
  , inferConcreteTypeUniversal
  , inferConcreteTypeU
  , inferConcreteVar
  , evalGeneralStep
  ) where

import qualified Control.Monad.State as CMS
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as T
import Numeric (showHex)

-- TODO: do not use global scope here
getScope :: Int -> Lang -> MorlocMonad (Scope, Scope)
getScope _ lang = do
  cscope <- MM.getConcreteUniversalScope lang
  gscope <- MM.getGeneralUniversalScope
  MM.sayVVV $ "cscope:" <+> viaShow cscope
  return (cscope, gscope)

evalGeneralStep :: Int -> TypeU -> MorlocMonad (Maybe TypeU)
evalGeneralStep i t = T.evaluateStep <$> MM.getGeneralScope i <*> pure t

inferConcreteTypeU :: Lang -> Indexed TypeU -> MorlocMonad TypeU
inferConcreteTypeU lang (Idx i t0) = do
  attemptT <- inferConcreteTypeU' t0 <$> getScope i lang
  case attemptT of
    (Right t') -> return t'
    (Left _) -> do
      gscopeUni <- MM.getGeneralUniversalScope
      cscopeUni <- MM.getConcreteUniversalScope lang
      case inferConcreteTypeU' t0 (cscopeUni, gscopeUni) of
        (Right t') -> return t'
        (Left (SystemError e2)) -> MM.throwSourcedError i e2
        (Left e2) -> MM.throwError e2

inferConcreteTypeU' :: TypeU -> (Scope, Scope) -> Either MorlocError TypeU
inferConcreteTypeU' generalType (cscope, gscope) = T.pairEval cscope gscope generalType

inferConcreteType :: Lang -> Indexed Type -> MorlocMonad TypeF
inferConcreteType _ (Idx i (UnkT _)) =
  MM.throwSourcedError i "Cannot infer concrete type for UnkT. This may be an unsolved generic term"
inferConcreteType lang (Idx i (type2typeu -> generalType)) = do
  (cscope0, gscope0) <- getScope i lang
  anc <- CMS.gets stateVariantAncestors
  -- A `data` type already being expanded resolves to its NAME and stops.
  -- The check belongs here, at the single entry point, rather than deeper:
  -- resolving a field reaches this function again from the top, and the
  -- work below would expand the type before any inner guard could fire.
  -- Recursion THROUGH a container (`data Rose = Rose [Rose]`) arrives this
  -- way -- the field's head is the container, not the `data` type.
  case dataHeadOf gscope0 generalType of
    Just key@(v, args) | Set.member key anc -> backEdge lang i cscope0 v args
    _ -> do
      concreteType <- inferConcreteTypeU lang (Idx i generalType)
      (_, gscope) <- getScope i lang
      inferConcreteTypeStructural lang i gscope generalType concreteType

-- | Parallel structural walk over (general, concrete) that handles the
-- AppU/VarU mismatch case at any depth. Parameterised newtypes whose
-- per-language form is a non-templated VarU (e.g. @newtype IFile a =
-- UInt64@ + @type Cpp => IFile a = "uint64_t"@) cannot be woven by the
-- pure 'weave' because its catch-all calls 'evaluateStep' on the
-- general type, which treats newtypes as opaque and returns Nothing.
-- The fix is to retain the general args (recursively inferring their
-- concrete forms) so the resulting AppF preserves both shape and
-- downstream pattern matches (e.g. @open's IFile-head check in
-- Imperative.hs).
--
-- For everything else we delegate to the existing pure 'weave' via
-- 'inferConcreteTypeWeave', which still handles transparent aliases,
-- template-bearing per-language forms, records, optionals, and the
-- usual gscope-step fallbacks.
inferConcreteTypeStructural
  :: Lang -> Int -> Scope -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeStructural lang i gscope g c = case (g, c) of
  -- A suspension is a closure of no arguments in every pool. Pierce it,
  -- and an optional, so the AppU/VarU intercept fires through them:
  -- @<IO> (IFile a)@, @?(IFile a)@, etc.
  (EffectU _ g', EffectU _ c') ->
    FunF [] <$> inferConcreteTypeStructural lang i gscope g' c'
  (OptionalU g', OptionalU c') ->
    OptionalF <$> inferConcreteTypeStructural lang i gscope g' c'
  -- A payload-bearing `data`. Its arms' field types have to be resolved to
  -- the target language here rather than in 'weave', which is pure and so
  -- cannot reach the per-language scope: weaving a field against itself
  -- would leave the morloc name in the concrete slot and emit `Real` where
  -- a Rust pool needs `f64`. Argument-free constructors fall through, having
  -- no fields to resolve.
  -- Already being expanded: resolve to the back-edge and stop. This guard
  -- sits at the top of the intercept because the structural walk re-enters
  -- here directly for an AppU's arguments, without passing through
  -- 'inferConcreteType' -- which is how recursion through a container
  -- (`data Rose = Rose [Rose]`) arrives. The back-edge carries the
  -- instantiation's arguments, as a direct field's does, so the element of
  -- a `[Rose a]` arm can still instantiate a template.
  (VarU vG, VarU (TV vC))
    | Just _ <- scopeDataCtors gscope vG -> do
        anc0 <- CMS.gets stateVariantAncestors
        if Set.member (vG, []) anc0
          then backEdgeHere vG []
          else inferVariantArms lang i gscope vG vC []
  -- An APPLIED `data`, e.g. @Try Str a@. Same treatment as the bare case,
  -- except the declaration's parameters must be instantiated with the
  -- applied arguments before the arms are resolved -- otherwise an arm
  -- mentioning a parameter resolves the bare variable and the type reaches
  -- codegen as an AppF rather than a VariantF.
  (AppU (VarU vG) tsG, _)
    | Just _ <- scopeDataCtors gscope vG
    , Just vC <- concreteHeadName c -> do
        anc0 <- CMS.gets stateVariantAncestors
        if Set.member (vG, tsG) anc0
          then backEdgeHere vG tsG
          else inferVariantArms lang i gscope vG vC tsG
  _ -> inferConcreteTypeStructuralRest lang i gscope g c
  where
    backEdgeHere vG tsG = do
      (cscope, _) <- getScope i lang
      backEdge lang i cscope vG tsG
    -- The concrete side of an applied type is either applied too or has
    -- already collapsed to a bare name.
    concreteHeadName (AppU (VarU (TV n)) _) = Just n
    concreteHeadName (VarU (TV n)) = Just n
    concreteHeadName _ = Nothing

-- | Expand a `data` type's arms to the target language, with the type
-- pushed onto the ancestor set for the duration.
inferVariantArms
  :: Lang -> Int -> Scope -> TVar -> MT.Text -> [TypeU] -> MorlocMonad TypeF
inferVariantArms lang i gscope vG vC targs = do
  arms0 <- case scopeDataCtors gscope vG of
    Just as -> return as
    Nothing -> return []
  -- Instantiate the declaration's parameters with the applied arguments.
  -- Empty for a bare `data`, in which case this is the identity.
  let params = case Map.lookup vG gscope of
        Just ((ps, _, _, _, _) : _) -> [tv | Left (tv, _) <- ps]
        _ -> []
      inst t = foldl (\acc (tv, arg) -> substituteTVar tv arg acc)
                     t (zip params targs)
      arms = [(n, map inst ts) | (n, ts) <- arms0]
  -- The applied arguments, resolved to this language, travel on the type:
  -- a user's per-language form may be a template, and these are what it is
  -- instantiated with. They are resolved before the arms so a self-reference
  -- among them is cut by the ordinary guard rather than by this type's own.
  --
  -- An argument nothing ever pinned stays unknown rather than failing the
  -- resolution: a phantom parameter of an argument-free `data` is never
  -- rendered, and a literal like `A :: Tag a` with no use of `a` is
  -- ordinary code. It is only a template that would try to spell it, and
  -- the native compiler reports that where it happens.
  ps <- mapM (argType lang i) targs
  if all (null . snd) arms
    then return $ EnumF (FV vG (CV vC)) ps (map fst arms)
    else do
        cscope <- fst <$> getScope i lang
        anc <- CMS.gets stateVariantAncestors
        let -- A field naming a `data` type that is already being resolved is
            -- left as a PLACEHOLDER rather than expanded. Expanding it would
            -- re-enter this case and never terminate, and it is unnecessary:
            -- the field only needs that instantiation's name and arguments,
            -- which is what 'backEdge' carries. The cycle is cut once,
            -- later, where the wire form is built.
            --
            -- The set is carried in compiler state rather than as an
            -- argument because a field's type is resolved by re-entering
            -- 'inferConcreteType' at the top. A head-only test would miss
            -- recursion THROUGH a container -- @data Rose = Rose [Rose]@
            -- has head @List@, and expanding it reaches @Rose@ again.
            --
            -- The test is on the INSTANTIATION, not the name: the inner
            -- @Box Int@ of a @Box (Box Int)@ is a different type from the
            -- one being resolved, and cutting it would say the value
            -- contains itself.
            resolveField t = case dataHeadOf gscope t of
              Just key@(v, args) | Set.member key anc || key == (vG, targs) ->
                backEdge lang i cscope v args
              _ -> inferConcreteType lang (Idx i (typeOf t))
        -- Guarding on the instantiation rather than the name is what lets
        -- `Box (Box Int)` expand its inner type, and it is also what lets a
        -- NON-REGULAR recursive type -- one whose every ply is a new
        -- instantiation, `data Nest a = Nest (Nest [a])` -- expand without
        -- end. Such a type has no finite native form, so the walk is bounded
        -- and the bound is reported as the error it is, rather than left to
        -- exhaust memory. The limit sits far above any nesting a real
        -- program reaches.
        when (Set.size anc >= 64) $
          MM.throwSourcedError i $
            "Cannot resolve" <+> squotes (pretty vG) <+> "applied to"
              <+> hsep (map pretty targs) <> ":"
              <+> "its expansion does not terminate."
        CMS.modify (\st -> st { stateVariantAncestors = Set.insert (vG, targs) anc })
        arms' <- mapM (\(n, ts) -> (,) n <$> mapM resolveField ts) arms
        CMS.modify (\st -> st { stateVariantAncestors = anc })
        return $ VariantF (FV vG (CV (instanceName cscope vG targs))) ps arms'

-- | The name a pool knows one instantiation of a `data` type by.
--
-- A statically-typed pool declares the type by name, so `Try Str ()` and
-- `Try Str (IFile a)` need two names or the second definition loses to the
-- first. A type the compiler generates gets a suffix derived from its
-- ARGUMENTS -- not from its arms, which for a recursive type would name the
-- type in terms of itself -- so the name is fixed before the arms are read
-- and a self-reference can carry it. When the user mapped the type
-- (`data Cpp => (Box a) = "MyBox<$1>" a`), the name they chose is the
-- contract: a template, if it takes parameters, which the backend
-- instantiates with the resolved arguments.
instanceName :: Scope -> TVar -> [TypeU] -> MT.Text
instanceName cscope v targs
  | generated && not (null targs) = name <> instanceSuffix targs
  | otherwise = name
  where
    name = concreteNameOf cscope v
    generated = name == unTVar v

-- | The placeholder a `data` type's own occurrence inside itself resolves
-- to: the instantiation's name and resolved arguments, with no arms. The
-- wire-form builder recognises the armless shape as the back-edge and ties
-- the knot there; every renderer spells it as it would the full type.

-- | Resolve one applied argument of a `data` type, keeping an unsolved one
-- unsolved. See the note at its use in 'inferVariantArms'.
argType :: Lang -> Int -> TypeU -> MorlocMonad TypeF
argType lang i t = case typeOf t of
  UnkT v -> return $ UnkF (FV v (CV (unTVar v)))
  t' -> inferConcreteType lang (Idx i t')

backEdge :: Lang -> Int -> Scope -> TVar -> [TypeU] -> MorlocMonad TypeF
backEdge lang i cscope v args = do
  ps <- mapM (argType lang i) args
  return $ VariantF (FV v (CV (instanceName cscope v args))) ps []

-- | A short, deterministic suffix distinguishing one instantiation of a
-- parameterized `data` from another in a language that declares types by
-- name. Derived from the applied arguments, so equal instantiations collide
-- on purpose and unequal ones do not.
instanceSuffix :: [TypeU] -> MT.Text
instanceSuffix targs =
  "_" <> MT.pack (showHex (abs (hashText rendered) `mod` 0xFFFFFF) "")
  where
    rendered = MT.pack (show (map pretty targs))
    -- djb2; any stable string hash would do. Kept local so the suffix does
    -- not drift with a library's hashing implementation.
    hashText = MT.foldl' (\h c -> h * 33 + fromEnum c) (5381 :: Int)

-- | The structural walk's remaining cases: shapes 'weave' cannot handle on
-- its own, and the fallback to it.
inferConcreteTypeStructuralRest
  :: Lang -> Int -> Scope -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeStructuralRest lang i gscope g c = case (g, c) of
  -- AppU general / VarU concrete: the per-language form takes no arguments
  -- while the general type has some.
  --
  -- A type that declares its own form must list every parameter, so this shape
  -- means the declaration is missing one. A type with no declaration inherits
  -- its concrete form through its wire parent -- @newtype PatternChain a b =
  -- Str@ takes Str's mapping -- and an inherited form cannot carry the
  -- newtype's parameters, so those arguments are carried here instead.
  (AppU (VarU vG) ts, VarU (TV vC)) -> do
    (cscope, _) <- getScope i lang
    if Map.member vG cscope
      then
        MM.throwSourcedError i $
          "The" <+> pretty lang <+> "form of" <+> squotes (pretty vG)
            <+> "takes no arguments, but" <+> squotes (pretty vG)
            <+> "has" <+> pretty (length ts) <> "."
            <> "\nA type that declares its own per-language form must list"
            <> "\nevery parameter, so that the form's arity matches the"
            <> "\ntype's. A parameter the native macro does not interpolate"
            <> "\nis still listed."
      else do
        argTfs <- mapM (inferConcreteType lang . Idx i . typeOf) ts
        return $ AppF (VarF (FV vG (CV vC))) argTfs
  -- AppU/AppU shortcut: weave the head pairwise, recurse on each
  -- (g, c) arg pair so a nested AppU/VarU on the arg side still
  -- picks up the intercept. The type-arity guard rejects phantom-Nat
  -- aliases like @FixedPair (n :: Nat) a = (a, a)@ where the general
  -- side has [NatLit, Type] but the concrete resolves through Tuple2
  -- to a 2-type template -- a naive zip would pair NatLit with a
  -- type slot, then 'partitionKindArgsF' would strip it at render
  -- time and crash macro expansion. Mismatches fall through to
  -- 'weave', which steps the alias and re-weaves on the body's head.
  (AppU (VarU vG) ts1, AppU (VarU (TV vC)) ts2)
    | length ts1 == length ts2
    , length (fst (partitionKindArgsU ts1))
        == length (fst (partitionKindArgsU ts2)) -> do
        argTfs <- zipWithM
          (inferConcreteTypeStructural lang i gscope) ts1 ts2
        return $ AppF (VarF (FV vG (CV vC))) argTfs
  -- Everything else (VarU/VarU, FunU, NamU, Nat*, leaves, mismatched
  -- shapes) routes through the existing pure weave + scope-step fall
  -- backs.
  _ -> inferConcreteTypeWeave lang i gscope g c

inferConcreteTypeWeave
  :: Lang -> Int -> Scope -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeWeave lang i gscope generalType concreteType =
  case weave gscope generalType concreteType of
    (Right tf) -> return tf
    (Left _) -> do
      gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
      case weave gscopeUni generalType concreteType of
        (Right tf) -> return tf
        (Left _) -> do
          -- Evaluate the general type one level and try again
          --
          -- Weaving will fail for parameterize type definitions, such as
          --   type (Foo a) = [(a, Str)]
          -- Here the primitive type (e.g., "std::vector<std::tuple<$1,std::string>>" a)
          -- cannot be woven with the `Foo a` type. So `Foo a` needs to be
          -- substituted for [(a, Str)], which can be woven.
          --
          -- 'evalGeneralStep' wraps 'TE.evaluateStep' which returns
          -- @Just t@ even when @t == generalType@ (no progress). For a
          -- guarded recursive record like @record Tree where children
          -- :: [Tree]@ the bnd-protected NamU branch in
          -- 'generalTransformType' returns the same NamU as input, so a
          -- naive recursion here loops forever. Compare structurally
          -- before recursing.
          mayReducedGType <- evalGeneralStep i generalType
          case mayReducedGType of
            (Just reducedGType)
              | reducedGType /= generalType ->
                  inferConcreteType lang (Idx i (typeOf reducedGType))
            _ ->
              MM.throwSourcedError i $
                "Cannot infer concrete type for" <+> pretty generalType <> "\nCould not reduce type"

-- | The index is the source position errors are reported against and the
-- site a `data` type's arms are resolved at; the scopes themselves are the
-- universal ones, as the name says.
inferConcreteTypeUniversal :: Lang -> Int -> Type -> MorlocMonad TypeF
inferConcreteTypeUniversal lang i t@(type2typeu -> generalType) = do
  gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
  concreteType <- inferConcreteTypeUUniversal lang generalType
  inferConcreteTypeUniversalStructural lang i gscopeUni t generalType concreteType

-- | Structural walk over (general, concrete) in universal scope. Mirrors
-- 'inferConcreteTypeStructural' but recurses via 'inferConcreteTypeUniversal'
-- so nested type parameters resolve against the universal typedef scope,
-- not the per-language one. Needed for parameterised newtypes like
-- @OStream (IFile [Int])@ where the general side is @AppU@ and the
-- concrete side collapses to a bare @VarU UInt64@ -- the pure @weave@
-- can't bridge that mismatch and its @evaluateStep@ fallback treats the
-- newtype as opaque.
inferConcreteTypeUniversalStructural
  :: Lang -> Int -> Scope -> Type -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeUniversalStructural lang i gscopeUni t g c = case (g, c) of
  (EffectU _ g', EffectU _ c') ->
    FunF [] <$> inferConcreteTypeUniversalStructural lang i gscopeUni t g' c'
  (OptionalU g', OptionalU c') ->
    OptionalF <$> inferConcreteTypeUniversalStructural lang i gscopeUni t g' c'
  -- A `data` type resolves to its arms here exactly as it does in the
  -- module-scoped walk. Without this, the wire form of a parameterized
  -- `data` is asked for as though it were an ordinary applied type, and
  -- answered with a demand for a `Packable` instance -- which a sum type
  -- no more needs than a tuple does.
  (VarU vG, VarU (TV vC))
    | Just _ <- scopeDataCtors gscopeUni vG ->
        inferVariantArms lang i gscopeUni vG vC []
  (AppU (VarU vG) tsG, _)
    | Just _ <- scopeDataCtors gscopeUni vG
    , Just vC <- universalConcreteHead c ->
        inferVariantArms lang i gscopeUni vG vC tsG
  -- Same rule as in the module-scoped walk above.
  (AppU (VarU vG) ts, VarU (TV vC)) -> do
    cscopeUni <- MM.getConcreteUniversalScope lang
    if Map.member vG cscopeUni
      then
        MM.throwSystemError $
          "The" <+> pretty lang <+> "form of" <+> squotes (pretty vG)
            <+> "takes no arguments, but" <+> squotes (pretty vG)
            <+> "has" <+> pretty (length ts) <> "."
            <> "\nA type that declares its own per-language form must list"
            <> "\nevery parameter, so that the form's arity matches the"
            <> "\ntype's. A parameter the native macro does not interpolate"
            <> "\nis still listed."
      else do
        argTfs <- mapM (inferConcreteTypeUniversal lang i . typeOf) ts
        return $ AppF (VarF (FV vG (CV vC))) argTfs
  (AppU (VarU vG) ts1, AppU (VarU (TV vC)) ts2)
    | length ts1 == length ts2
    , length (fst (partitionKindArgsU ts1))
        == length (fst (partitionKindArgsU ts2)) -> do
        argTfs <- zipWithM
          (inferConcreteTypeUniversalStructural lang i gscopeUni t) ts1 ts2
        return $ AppF (VarF (FV vG (CV vC))) argTfs
  _ ->
    case weave gscopeUni g c of
      (Right tf) -> return tf
      (Left _) -> case T.evaluateStep gscopeUni g of
        (Just reducedGType)
          | reducedGType /= g ->
              inferConcreteTypeUniversal lang i (typeOf reducedGType)
        _ ->
          MM.throwSystemError $
            "Failed to infer concrete type for" <+> pretty t
              <> ": Could not reduce type in broadest scope"

-- | The concrete side of an applied type, which is either applied too or
-- has already collapsed to a bare name. Mirrors @concreteHeadName@ in the
-- module-scoped walk.
universalConcreteHead :: TypeU -> Maybe MT.Text
universalConcreteHead (AppU (VarU (TV n)) _) = Just n
universalConcreteHead (VarU (TV n)) = Just n
universalConcreteHead _ = Nothing

inferConcreteTypeUUniversal :: Lang -> TypeU -> MorlocMonad TypeU
inferConcreteTypeUUniversal lang generalType = do
  gscopeUni <- CMS.gets stateUniversalGeneralTypedefs
  cscopeUni <- MM.getConcreteUniversalScope lang
  let attemptUni = inferConcreteTypeU' generalType (cscopeUni, gscopeUni)
  case attemptUni of
    (Right t) -> return t
    (Left (SystemError e2)) ->
      MM.throwSystemError $
        "Failed to infer concrete universal type for lang"
          <+> pretty lang
          <+> "for type"
          <+> pretty generalType
          <> ":" <+> e2
    (Left e) -> MM.throwError e

-- | 'weave' fuses a general TypeU with its language-specific concrete
-- TypeU into a single TypeF. Purely structural -- no cycle detection
-- at the weave level (that would collapse the intermediate alias-
-- expansion node that 'makeSerialAST'' relies on to emit the
-- recursive-schema @&name@ declaration alongside the @^name@ back-
-- reference). Leak prevention for recursive aliases with no concrete-
-- language mapping happens at the C++ render boundary in
-- 'CppTranslator.hs', where every @VarF@/@AppF@/@RecF@ rule consults
-- cscope to distinguish legitimate user mappings from pairEval bnd-
-- protect leaks.
weave :: Scope -> TypeU -> TypeU -> Either MDoc TypeF
weave gscope = w Set.empty
  where
    -- A `data` type weaves to 'EnumF' rather than a plain 'VarF' so the
    -- constructor names reach codegen. Making this the canonical TypeF for
    -- an enum is what lets every backend recognize one structurally --
    -- otherwise only serialized positions (which come through SerialEnum)
    -- would see it, and a native-position enum would look like an opaque
    -- VarF with an unhelpful name.
    w anc (VarU v1) (VarU (TV v2)) = return $ case scopeDataCtors gscope v1 of
      -- A type already being expanded stays an opaque name. Expansion
      -- reaches a constructor's field types, so a field naming an enclosing
      -- `data` would otherwise re-enter it forever -- and branch, once more
      -- than one field does so. Leaving it opaque is how a record's
      -- self-reference already survives this walk: the back-edge is cut
      -- once, later, where the wire form is built and a name is available
      -- to tie the knot.
      _ | Set.member v1 anc -> VarF (FV v1 (CV v2))
      Just ctors
        -- Every constructor argument-free: the one-byte form.
        | all (null . snd) ctors -> EnumF (FV v1 (CV v2)) [] (map fst ctors)
        -- Otherwise a tagged pointer. The arms are NOT expanded here: a
        -- field's concrete form is a per-language question and this walk
        -- has only the general scope, so weaving a field against itself
        -- would leave the morloc name where a pool needs its own. The
        -- reference stays opaque and the arms are built where the wire
        -- form is, which is the same place the recursive case is tied off.
        | otherwise -> VarF (FV v1 (CV v2))
      Nothing -> VarF (FV v1 (CV v2))
    w anc (FunU ts1 t1) (FunU ts2 t2) = FunF <$> zipWithM (w anc) ts1 ts2 <*> w anc t1 t2
    -- AppU vs AppU: weave heads, then args. If heads weave but arg lists
    -- have mismatched lengths (e.g. general @Pair Int@ has 1 arg while
    -- the concrete-side resolution expanded to @"tuple" [int, ?(...)]@
    -- with 2 args), fall through to the catch-all @evaluateStep@ retry
    -- on @t1@. This is the same generalization the catch-all already
    -- performs for type-level mismatches; we just need to opt the AppU
    -- branch into it on arg-length failure instead of failing outright.
    w anc t1@(AppU h1 ts1) t2@(AppU h2 ts2) =
      case (AppF <$> w anc h1 h2 <*> weaveArgs anc ts1 ts2) of
        r@(Right _) -> r
        Left _ -> wStep anc t1 t2
    w anc t1@(NamU o1 v1 ts1 rs1) t2@(NamU o2 v2 ts2 rs2)
      | o1 == o2 && length ts1 == length ts2 && length rs1 == length rs2 =
          NamF o1 (FV v1 (CV (unTVar v2)))
            <$> zipWithM (w anc) ts1 ts2
            <*> zipWithM (\(_, t1') (k2', t2') -> (,) k2' <$> w anc t1' t2') rs1 rs2
      | otherwise = Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <+> "\n  t2:" <+> pretty t2
    w anc (EffectU _ t1) (EffectU _ t2) = FunF [] <$> w anc t1 t2
    w anc (OptionalU t1) (OptionalU t2) = OptionalF <$> w anc t1 t2
    w _ (NatLitU n) (NatLitU _) = return $ NatLitF n
    w _ (NatLitU n) _ = return $ NatLitF n  -- Nat params may be erased in concrete type
    w _ NatVoidU _ = return NatVoidF  -- Erased phantom Nat slot
    w _ (NatAddU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w _ (NatMulU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w _ (NatSubU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w _ (NatDivU _ _) _ = return NatVoidF  -- Nat arithmetic erased in concrete type
    w _ (NatVarU _) _ = return NatVoidF  -- Nat variable erased in concrete type
    w anc (LabeledU _ t1) t2 = w anc t1 t2
    w _ (ForallU v (VarU v')) _ | v == v' = return NatVoidF  -- Unresolved variable (UnkT pattern)
    w anc t1 t2 = wStep anc t1 t2

    -- Step the general type one level via @evaluateStep@ and retry.
    wStep anc t1 t2 = case T.evaluateStep gscope t1 of
      Nothing -> Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <> "\n  t2:" <> pretty t2
      (Just t1') ->
        if t1 == t1'
          then Left ("failed to weave:" <> pretty t1 <+> "vs" <+> pretty t1')
          else w anc t1' t2

    -- Weave type arguments, handling Nat params that may be erased OR
    -- preserved in the concrete type. When the concrete head is also a Nat
    -- expression, consume it in lockstep; otherwise consume only the general
    -- arg (erased in concrete). Either way we emit a NatLitF placeholder.
    weaveArgs :: Set.Set TVar -> [TypeU] -> [TypeU] -> Either MDoc [TypeF]
    weaveArgs _ [] [] = Right []
    weaveArgs _ [] cs
      | all isKindTypeU cs = Right []  -- trailing kind args in concrete only
      | otherwise = Left "concrete type has more non-Nat args than general type in weave"
    weaveArgs anc (NatLitU n : gs) cs = (NatLitF n :) <$> weaveArgs anc gs (dropNatHead cs)
    weaveArgs anc (NatVoidU : gs) cs = (NatVoidF :) <$> weaveArgs anc gs (dropNatHead cs)
    weaveArgs anc (NatAddU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs anc gs (dropNatHead cs)
    weaveArgs anc (NatMulU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs anc gs (dropNatHead cs)
    weaveArgs anc (NatSubU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs anc gs (dropNatHead cs)
    weaveArgs anc (NatDivU _ _ : gs) cs = (NatVoidF :) <$> weaveArgs anc gs (dropNatHead cs)
    -- Unresolved nat dimension variable (opaque output dims): treat as erased
    weaveArgs anc (NatVarU _ : gs) cs = (NatVoidF :) <$> weaveArgs anc gs (dropNatHead cs)
    weaveArgs anc (ForallU v (VarU v') : gs) cs | v == v' = (NatVoidF :) <$> weaveArgs anc gs (dropNatHead cs)
    weaveArgs anc (g:gs) (c:cs) = (:) <$> w anc g c <*> weaveArgs anc gs cs
    weaveArgs _ _ [] = Left "general type has more non-Nat args than concrete type in weave"

    -- Drop a leading kind-shaped concrete arg, if present.
    dropNatHead :: [TypeU] -> [TypeU]
    dropNatHead (c : cs) | isKindTypeU c = cs
    dropNatHead cs = cs

inferConcreteVar :: Lang -> Indexed TVar -> MorlocMonad FVar
inferConcreteVar lang t0@(Idx i v) = do
  MM.sayVVV $ "inferConcreteVar" <+> pretty lang <+> pretty t0
  localScope <- MM.getConcreteScope i lang
  globalScope <- MM.getConcreteUniversalScope lang
  case Map.lookup v localScope of
    (Just ((_, t, _, True, _) : _)) -> return $ FV v (CV . unTVar $ extractKey t)
    -- Non-terminal concrete alias: e.g. `type Cpp => Array a = List a`.
    -- Follow through the body's head (recursively) until a terminal entry
    -- is reached, then pair the *original* v with the resolved concrete
    -- name. This preserves the morloc-level identity (Array stays Array)
    -- while picking up the runtime concrete (std::vector here).
    (Just ((_, t, _, False, _) : _)) -> do
      FV _ cv <- inferConcreteVar lang (Idx i (extractKey t))
      return $ FV v cv
    _ -> case Map.lookup v globalScope of
      (Just ((_, t, _, True, _) : _)) -> do
        -- TODO fix this, the types should be in scope
        MM.sayVVV $ "WARNING: using global definition for v=" <> pretty v
        return $ FV v (CV . unTVar $ extractKey t)
      -- Same recursive resolution at the global scope level.
      (Just ((_, t, _, False, _) : _)) -> do
        FV _ cv <- inferConcreteVar lang (Idx i (extractKey t))
        return $ FV v cv
      _ -> do
        -- Not in any concrete scope. Try general scope: a general-only
        -- alias like `type MyVec (n :: Nat) a = Vector n a` has no
        -- concrete mapping of its own, but its body's head (Vector)
        -- should be resolvable. Recurse on the body's head, preserving
        -- the original morloc identity (`v`) while picking up the
        -- transitively resolved concrete name.
        --
        -- This is the bare-VarU analog of `expandHeadOnly`'s kind-based
        -- realignment: for general aliases that have params but no args
        -- in this lookup, we still want to chase the alias by its head.
        gscopeUni <- MM.getGeneralUniversalScope
        gscopeLocal <- MM.getGeneralScope i
        let
          -- A `data` type with no per-language mapping. Its general body is
          -- the constructor table, not an alias, so chasing the body's head
          -- resolves to whatever type that table is spelled with rather than
          -- to anything the user wrote. The pool generates a native
          -- definition under the type's own name, so that is the concrete
          -- name here.
          isData = case (scopeDataCtors gscopeLocal v, scopeDataCtors gscopeUni v) of
                     (Nothing, Nothing) -> False
                     _ -> True
        let
          -- Guard against self-recursive lookup: if the body's
          -- extracted key resolves back to v (e.g. a record whose
          -- general definition is `record Container a where ...` --
          -- the body's NamU carries the same TVar `Container`), a
          -- naive recursion loops forever because each iteration
          -- looks the same TVar up in gscopeUni and gets the same
          -- body back. When the body's key is v itself, fall through
          -- to pairEval, which produces a comprehensible
          -- "No concrete <lang> type for <v>" error naming the
          -- missing instance.
          gscopeBody = case Map.lookup v gscopeUni of
            (Just ((_, body, _, _, _) : _)) | extractKey body /= v -> Just (extractKey body)
            _ -> Nothing
        case if isData then Nothing else gscopeBody of
          Just bodyKey -> do
            FV _ cv <- inferConcreteVar lang (Idx i bodyKey)
            return $ FV v cv
          Nothing | isData -> return $ FV v (CV (unTVar v))
          Nothing -> do
            -- Last resort: transitive resolution via pairEval.
            (cscope, gscope) <- getScope i lang
            case T.pairEval cscope gscope (VarU v) of
              Right (VarU v') -> return $ FV v (CV (unTVar v'))
              Right t' -> MM.throwSourcedError i $
                "No concrete" <+> pretty lang <+> "type for"
                <+> squotes (pretty v) <> ":"
                <+> "alias resolves to a composite type"
                <+> parens (pretty t')
                <> ", but a single type variable is required here."
              Left _ -> MM.throwSourcedError i $
                "No concrete" <+> pretty lang <+> "type for"
                <+> squotes (pretty v) <> "."
                <+> "Add a 'type" <+> pretty lang <+> "=>"
                <+> pretty v <+> "= \"...\"' declaration,"
                <+> "or import a module that provides one"
                <+> parens ("e.g. root-" <> pretty lang) <> "."

-- | Outer name of a per-language typedef body, when it names one.
bodyNameOf :: TypeU -> Maybe MT.Text
bodyNameOf (VarU (TV n)) = Just n
bodyNameOf (AppU (VarU (TV n)) _) = Just n
bodyNameOf (NamU _ (TV n) _ _) = Just n
bodyNameOf _ = Nothing

-- | A `data` type's instantiation: its name and the arguments it was
-- applied to. Two instantiations of one parameterized type are two types,
-- so the arguments are part of the identity.
dataHeadOf :: Scope -> TypeU -> Maybe (TVar, [TypeU])
dataHeadOf gscope t = case t of
  VarU v | Just _ <- scopeDataCtors gscope v -> Just (v, [])
  AppU (VarU v) ts | Just _ <- scopeDataCtors gscope v -> Just (v, ts)
  _ -> Nothing

-- | A type's per-language name: its mapping when it declares one, its own
-- name when the pool generates the type.
concreteNameOf :: Scope -> TVar -> MT.Text
concreteNameOf cscope v = case Map.lookup v cscope of
  Just entries | (n : _) <- [n' | (_, body, _, _, _) <- entries, Just n' <- [bodyNameOf body]] -> n
  _ -> unTVar v
