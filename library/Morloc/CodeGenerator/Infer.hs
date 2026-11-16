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
  , undeterminedError
  , rejectUndetermined
  , requireDetermined
  , recordUndeterminedSites
  , canHoldType
  , evalGeneralStep
  , weave
  , argWith
  , retagArg
  , groundK
  , kindArgOf
  , heldDataParams
  , concreteNameOf
  ) where

import Morloc.Data.Zip (indexedFrom, zipSame, zipWith3Same, zipWithSame, zipWithSameM)
import qualified Control.Monad.State as CMS
import Data.Functor.Const (Const (..))
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.ValueArgs (appValueArgsT, argKinds, declaredKinds, valueArgsT)
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Language as ML
import qualified Morloc.TypeEval as T
import Morloc.Typecheck.Internal (typeUChildren, typeUToNatExpr)
import qualified Morloc.Typecheck.NatSolver as NS
import Numeric (showHex)
import Data.Char (isAlphaNum, isAscii, isDigit)

evalGeneralStep :: TypeU -> MorlocMonad (Maybe TypeU)
evalGeneralStep t = T.evaluateStep <$> MM.getGeneralScope <*> pure t

getScope :: Lang -> MorlocMonad (Scope, Scope)
getScope lang = do
  cscope <- MM.getConcreteScope lang
  gscope <- MM.getGeneralScope
  MM.sayVVV $ "cscope:" <+> viaShow cscope
  return (cscope, gscope)

inferConcreteTypeU :: Lang -> Indexed TypeU -> MorlocMonad TypeU
inferConcreteTypeU lang (Idx i t0) = do
  attemptT <- inferConcreteTypeU' t0 <$> getScope lang
  case attemptT of
    (Right t') -> return t'
    (Left (Rejected Unplaced e2)) -> MM.throwSourcedError i e2
    (Left (Fault Nothing e2)) -> MM.throwError (Fault (Just (AtNode i)) e2)
    (Left e2) -> MM.throwError e2

inferConcreteTypeU' :: TypeU -> (Scope, Scope) -> Either MorlocError TypeU
inferConcreteTypeU' generalType (cscope, gscope) = T.pairEval cscope gscope generalType

inferConcreteType :: Lang -> Indexed Type -> MorlocMonad TypeF
inferConcreteType lang (Idx i t0) = do
  let generalType = type2typeu t0
  (cscope0, gscope0) <- getScope lang
  requireDetermined i t0
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
      (_, gscope) <- getScope lang
      inferConcreteTypeStructural lang i gscope generalType concreteType >>= checkTied

-- | At the outermost walk, every reference to a record on the walk's path
-- has been tied ('tieSelf').
checkTied :: TypeF -> MorlocMonad TypeF
checkTied tf = do
  outermost <- Set.null <$> CMS.gets stateRecordAncestors
  when (outermost && holdsSelfMark tf) $
    MM.throwCompilerBug ("a record reference was not tied in" <+> pretty tf)
  return tf

-- | POLY-6: a variable nothing determined has no native form. The arguments
-- of a `data` type are exempt here: one its fields use is checked when the
-- fields are resolved, and one they do not use is never spelled.
requireDetermined :: Int -> Type -> MorlocMonad ()
requireDetermined i t = when (containsUnk t) $ do
  gscope <- MM.getGeneralScope
  case undetermined gscope (type2typeu t) of
    [] -> return ()
    vs -> undeterminedError i vs

-- | The variables of a type that nothing determined ('UnkT', a variable
-- quantified over itself in 'TypeU'), outside the arguments of a `data` type.
undetermined :: Scope -> TypeU -> [TVar]
undetermined gscope t = case t of
  ForallU v (VarU v') | v == v' -> [v]
  _ | Just _ <- dataHeadOf gscope t -> []
  _ -> concatMap (undetermined gscope) (typeChildren gscope t)

-- | The children of a type that classify values. A dimension or other
-- non-`Type` argument is erased (KIND-2) and needs no value (DIM-6).
typeChildren :: Scope -> TypeU -> [TypeU]
typeChildren gscope t = case T.flattenApp t of
  OpU {} -> []
  AppU h@(VarU name) args -> h : case declaredKinds gscope name (length args) of
    Just ks -> [a | (a, k) <- zipSame args ks, k == KindType]
    Nothing -> [a | a <- args, isNothing (kindOfU a)]
  t' -> typeUChildren t'

-- | Record, for each undetermined variable, the outermost term whose type
-- holds it. Code generation finds a variable deep inside a type, where the
-- index at hand may be a declaration's rather than the term's.
recordUndeterminedSites :: [AnnoS (Indexed Type) Many Int] -> MorlocMonad ()
recordUndeterminedSites es = do
  gscope <- MM.getGeneralScope
  -- Names are unique within one tree only, so a name met in two trees
  -- locates nothing.
  let perTree = [sites gscope e | e <- es]
      counts = Map.unionsWith (+) [Map.map (const (1 :: Int)) m | m <- perTree]
      located = Map.filterWithKey (\v _ -> Map.lookup v counts == Just 1) (Map.unions perTree)
  located `seq` CMS.modify (\st -> st { stateUndeterminedSites = located })
  where
    sites gscope e = Map.fromListWith (\_ old -> old)
      [(v, ci) | (t, ci) <- ordered [e], containsUnk t, v <- needed gscope (type2typeu t)]
    -- Terms outside any function come first, so a variable is reported at
    -- the argument a caller wrote rather than inside the callee's body.
    ordered [] = []
    ordered roots =
      let (values, functions, inner) = mconcat (map byNesting roots)
      in values <> functions <> ordered inner
    byNesting (AnnoS (Idx _ t) ci e) = case t of
      FunT {} -> ([], [(t, ci)], foldExprS (: []) e)
      _ -> ([(t, ci)], [], []) <> mconcat (map byNesting (foldExprS (: []) e))

-- | The undetermined variables of a type that a native form needs: those
-- outside any `data` type, and those a `data` type's fields use.
needed :: Scope -> TypeU -> [TVar]
needed gscope = go Set.empty
  where
    go seen t = case t of
      ForallU v (VarU v') | v == v' -> [v]
      _ | Just key@(name, args) <- dataHeadOf gscope t ->
            if Set.member key seen || Set.size seen >= 64
              then []
              else concatMap (go (Set.insert key seen)) (fields name args)
      _ -> concatMap (go seen) (typeChildren gscope t)
    fields name args =
      [ instantiateParams (zipSame params args) f
      | Just (params, cs) <- [dataDeclOfName gscope name]
      , (_, fs) <- cs
      , f <- fs
      ]

-- | Reject a type holding a variable nothing determines that a native form
-- needs and that occurs in a term of the program (POLY-6).
rejectUndetermined :: Int -> Type -> MorlocMonad ()
rejectUndetermined i t = do
  gscope <- MM.getGeneralScope
  sites <- CMS.gets stateUndeterminedSites
  case filter (`Map.member` sites) (needed gscope (type2typeu t)) of
    [] -> return ()
    vs -> undeterminedError i vs

-- | A type variable nothing in the program determines, where a native form
-- needs it (POLY-6).
undeterminedError :: Int -> [TVar] -> MorlocMonad a
undeterminedError i0 vs = do
  sites <- CMS.gets stateUndeterminedSites
  let i = case mapMaybe (`Map.lookup` sites) vs of
        (site : _) -> site
        [] -> i0
  MM.throwSourcedError i $ case filter (not . generated) vs of
    [] -> "The type of this term is not determined. Add a signature or an annotation that fixes it."
    named ->
      "The type of this term is not determined: nothing in the program fixes"
        <+> hsep (punctuate "," (map (squotes . pretty . written) named))
        <> ". Add a signature or an annotation that does."
  where
    -- a variable the compiler made up, as for an accessor, names nothing the
    -- reader wrote
    generated (TV n) = "_" `MT.isPrefixOf` n
    -- the name as written: without the suffix that makes it unique, or
    -- without the prefix and counter 'toExistential' gives a class method's
    -- variable
    written (TV n) =
      let base = MT.takeWhile (/= '@') n
       in fromMaybe base (MT.stripPrefix "cls_" base >>= MT.stripSuffix "_" . MT.dropWhileEnd isDigit)

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
-- Compound shapes recurse here ('structuralCompound'); a general name whose
-- expansion corresponds to a compound concrete shape is expanded here
-- ('stepTowardCompound'). What remains -- leaves, template-bearing
-- per-language forms, and pairs no expansion matches -- goes to the pure
-- 'weave' via 'inferConcreteTypeWeave'.
inferConcreteTypeStructural
  :: Lang -> Int -> Scope -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeStructural lang i gscope g c = do
  knot <- stateKnot
  case structuralCompound gscope knot (inferConcreteTypeStructural lang i gscope) g c of
    Just r -> hostConvention lang r
    Nothing -> inferConcreteTypeStructural' lang i gscope g c

inferConcreteTypeStructural'
  :: Lang -> Int -> Scope -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeStructural' lang i gscope g c = case (g, c) of
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
  -- An APPLIED `data`, e.g. @Try Str a@, has the declaration's parameters
  -- instantiated with the applied arguments before the arms are resolved --
  -- otherwise an arm mentioning a parameter resolves the bare variable and
  -- the type reaches codegen as an AppF rather than a VariantF.
  _ | Just (vG, tsG) <- dataHeadOf gscope g
    , Just vC <- concreteHeadName c -> do
        anc0 <- CMS.gets stateVariantAncestors
        if Set.member (vG, tsG) anc0
          then backEdgeHere vG tsG
          else inferVariantArms lang i gscope vG vC tsG
  _ -> inferConcreteTypeStructuralRest lang i gscope g c
  where
    backEdgeHere vG tsG = do
      (cscope, _) <- getScope lang
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
  arms0 <- case dataCtorsOfName gscope vG of
    Just as -> return as
    Nothing -> MM.throwCompilerBug $ "no `data` declaration for" <+> pretty vG
  -- Instantiate the declaration's parameters with the applied arguments.
  -- Empty for a bare `data`, in which case this is the identity.
  let params = maybe [] fst (dataDeclOfName gscope vG)
      arms = [(n, map (instantiateParams (zipSame params targs)) ts) | (n, ts) <- arms0]
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
  --
  -- The type is an ancestor while its arguments are resolved, so an
  -- argument that contains the type itself (@type X = Rose ?X@, whose
  -- argument is @?X@) ends at a back-edge rather than expanding forever.
  ps <- withVariantAncestor (vG, targs) (resolveArgs lang i (vG, targs))
  if all (null . snd) arms
    then return $ EnumF (FV vG (CV vC)) ps (map fst arms)
    else do
        cscope <- fst <$> getScope lang
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
        arms' <- withVariantAncestor (vG, targs) (mapM (\(n, ts) -> (,) n <$> mapM resolveField ts) arms)
        name <- instanceName cscope vG targs
        return $ VariantF (FV vG (CV name)) ps arms'

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
instanceName :: Scope -> TVar -> [TypeU] -> MorlocMonad MT.Text
instanceName cscope v targs = do
  gscope <- MM.getGeneralScope
  let name = concreteNameOf cscope v
      held = Set.fromList (heldDataParams gscope v)
      slots = ["$" <> MT.pack (show k) | (k, p) <- indexedFrom 1 (dataTypeParams gscope v), Set.member p held]
      generated = name == unTVar v
  return $ case () of
    _ | not generated || null targs -> name
      -- An instantiation whose arguments contain it has no finite spelling
      -- at them; it is a type of its own, named by them as written.
      | selfContaining gscope v targs -> name <> "_m" <> mangled (render (hsep (map pretty targs)))
      | null slots -> name
      | otherwise -> name <> "<" <> MT.intercalate ", " slots <> ">"

-- | Whether the arguments of a `data` instantiation contain the
-- instantiation itself, through aliases (a record or `data` type in them
-- is named, and ties): its spelling at them would be infinite.
selfContaining :: Scope -> TVar -> [TypeU] -> Bool
selfContaining gscope v targs = anyThroughAliases gscope reached targs
  where
    reached t = case t of
      VarU h -> h == v && null targs
      AppU (VarU h) as -> h == v && as == targs
      _ -> False

-- | Whether some part of the types satisfies the test, looking through
-- aliases and the arguments of applied types, not into records or the
-- arms of `data` types.
anyThroughAliases :: Scope -> (TypeU -> Bool) -> [TypeU] -> Bool
anyThroughAliases gscope test = any (go Set.empty)
  where
    go seen t
      | test t = True
      | otherwise = case t of
          NamU {} -> False
          -- A `data` type written by its name: its arguments only.
          _ | Just h <- headOf t, isJust (dataDeclOfName gscope h) -> any (go seen) (argsOf t)
          -- An alias, expanded once per name.
          _ | Just h <- headOf t, Set.notMember h seen, Just t' <- T.evaluateStep gscope t, t' /= t -> go (Set.insert h seen) t'
          _ | Just _ <- headOf t -> any (go seen) (argsOf t)
          _ -> any (go seen) (typeChildren gscope t)
    headOf (VarU h) = Just h
    headOf (AppU (VarU h) _) = Just h
    headOf _ = Nothing
    argsOf (AppU _ as) = as
    argsOf _ = []

-- | A text as an identifier part, distinct for distinct texts and with no
-- two underscores in a row: letters and digits as they are, `_` as `_u`,
-- any other character as `_x<hex>z`.
mangled :: MT.Text -> MT.Text
mangled = MT.concatMap esc
  where
    esc c
      | isAscii c && isAlphaNum c = MT.singleton c
      | c == '_' = "_u"
      | otherwise = "_x" <> MT.pack (showHex (fromEnum c) "") <> "z"

-- | The type parameters of a `data` declaration, in order.
dataTypeParams :: Scope -> TVar -> [TVar]
dataTypeParams gscope v = case [[p | Left (p, KindType) <- ps] | (ps, _, _, _, TypedefEnum) <- Map.findWithDefault [] v gscope] of
  (ps : _) -> ps
  [] -> []

-- | The type parameters of a `data` declaration some constructor holds, in
-- order: the parameters of a generated type's native template.
heldDataParams :: Scope -> TVar -> [TVar]
heldDataParams gscope v = [p | p <- dataTypeParams gscope v, Set.member p held]
  where
    held = Set.fromList [p | Just (_, cs) <- [dataDeclOfName gscope v], (_, ts) <- cs, t <- ts, VarU p <- Set.toList (free t)]

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
  resolving <- CMS.gets stateVariantArgsResolving
  gscope <- MM.getGeneralScope
  -- Reached while resolving this instantiation's own arguments. When they
  -- contain the type itself other than through a record, its spelling at
  -- them would be infinite: a generated type is then named by them
  -- ('instanceName'); a user template would have to spell them, and
  -- cannot. Otherwise the cycle passes through a record, which ties, or a
  -- `data` type already being resolved, and they are resolved as any are.
  if Set.member (v, args) resolving && selfContaining gscope v args
    then do
      name <- instanceName cscope v args
      if MT.any (== '$') name
        then MM.throwSourcedError i $
          "The native form of" <+> squotes (pretty v) <+> "is a template,"
            <+> "but its argument" <+> hsep (map pretty args)
            <+> "contains the type itself, so the template has no finite"
            <+> "instance."
        else return $ VariantF (FV v (CV name)) [] []
    else do
      ps <- resolveArgs lang i (v, args)
      name <- instanceName cscope v args
      return $ VariantF (FV v (CV name)) ps []

-- | Resolve the applied arguments of a `data` instantiation, marking it as
-- resolving its arguments for the duration (see 'backEdge').
resolveArgs :: Lang -> Int -> (TVar, [TypeU]) -> MorlocMonad [ArgF]
resolveArgs lang i key@(_, args) = do
  gscope <- MM.getGeneralScope
  withSetMember stateVariantArgsResolving (\v st -> st {stateVariantArgsResolving = v}) key $
    argsWith gscope (fst key) (argType lang i) args

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
    (cscope, gscopeHere) <- getScope lang
    if T.declaresFormAt cscope gscopeHere g
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
        argTfs <- argsWith gscope vG (inferConcreteType lang . Idx i . typeOf) ts
        return $ AppF (VarF (FV vG (CV vC))) argTfs
  -- AppU/AppU shortcut: weave the head pairwise, recurse on each
  -- (g, c) arg pair so a nested AppU/VarU on the arg side still
  -- picks up the intercept. The type-arity guard rejects phantom-Nat
  -- aliases like @FixedPair (n :: Nat) a = (a, a)@ where the general
  -- side has [NatLit, Type] but the concrete resolves through Tuple2
  -- to a 2-type template -- a naive zip would pair NatLit with a
  -- type slot, then 'partitionKindArgs' would strip it at render
  -- time and crash macro expansion. Mismatches fall through to
  -- 'weave', which steps the alias and re-weaves on the body's head.
  (AppU (VarU vG) ts1, AppU (VarU (TV vC)) ts2)
    | appArgsCorrespond ts1 ts2 -> do
        argTfs <- argsWith2 gscope vG (inferConcreteTypeStructural lang i gscope) ts1 ts2
        return $ AppF (VarF (FV vG (CV vC))) argTfs
  -- Everything else (VarU/VarU, Nat*, leaves, mismatched shapes) routes
  -- through the pure weave + scope-step fallbacks.
  _ | Just g' <- stepTowardCompound gscope g c ->
        inferConcreteTypeStructural lang i gscope g' c
    | otherwise -> inferConcreteTypeWeave lang i gscope g c

inferConcreteTypeWeave
  :: Lang -> Int -> Scope -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeWeave lang i gscope generalType concreteType = do
  anc <- CMS.gets stateRecordAncestors
  case weave gscope anc generalType concreteType of
    (Right tf) -> return tf
    (Left _) ->
      -- Weaving fails for a parameterized definition such as
      --   type (Foo a) = [(a, Str)]
      -- whose native form ("std::vector<std::tuple<$1,std::string>>" a)
      -- cannot be woven with `Foo a`; one step of evaluation exposes the
      -- body, which can. 'T.evaluateStep' may return the type unchanged (a
      -- guarded recursive record), which must not recurse.
      case T.evaluateStep gscope generalType of
        Just reducedGType
          | reducedGType /= generalType ->
              inferConcreteType lang (Idx i (typeOf reducedGType))
        _ -> do
          (cscope, _) <- getScope lang
          case inheritedForm cscope gscope generalType of
            Just parent -> inferConcreteType lang (Idx i (typeOf parent))
            Nothing ->
              MM.throwSourcedError i $
                "Cannot infer concrete type for" <+> pretty generalType <> "\nCould not reduce type"

-- | The index is the source position errors are reported against and the
-- site a `data` type's arms are resolved at; the scopes themselves are the
-- universal ones, as the name says.
inferConcreteTypeUniversal :: Lang -> Int -> Type -> MorlocMonad TypeF
inferConcreteTypeUniversal lang i t@(type2typeu -> generalType) = do
  gscopeUni <- CMS.gets stateGeneralTypedefs
  concreteType <- inferConcreteTypeUUniversal lang generalType
  inferConcreteTypeUniversalStructural lang i gscopeUni t generalType concreteType >>= checkTied

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
inferConcreteTypeUniversalStructural lang i gscopeUni t g c = do
  knot <- stateKnot
  case structuralCompound gscopeUni knot (inferConcreteTypeUniversalStructural lang i gscopeUni t) g c of
    Just r -> hostConvention lang r
    Nothing -> inferConcreteTypeUniversalStructural' lang i gscopeUni t g c

inferConcreteTypeUniversalStructural'
  :: Lang -> Int -> Scope -> Type -> TypeU -> TypeU -> MorlocMonad TypeF
inferConcreteTypeUniversalStructural' lang i gscopeUni t g c = case (g, c) of
  -- A `data` type resolves to its arms here exactly as it does in the
  -- module-scoped walk. Without this, the wire form of a parameterized
  -- `data` is asked for as though it were an ordinary applied type, and
  -- answered with a demand for a `Packable` instance -- which a sum type
  -- no more needs than a tuple does.
  _ | Just (vG, tsG) <- dataHeadOf gscopeUni g
    , Just vC <- universalConcreteHead c ->
        inferVariantArms lang i gscopeUni vG vC tsG
  -- Same rule as in the module-scoped walk above.
  (AppU (VarU vG) ts, VarU (TV vC)) -> do
    cscopeUni <- MM.getConcreteScope lang
    if T.declaresFormAt cscopeUni gscopeUni g
      then
        MM.throwRejected $
          "The" <+> pretty lang <+> "form of" <+> squotes (pretty vG)
            <+> "takes no arguments, but" <+> squotes (pretty vG)
            <+> "has" <+> pretty (length ts) <> "."
            <> "\nA type that declares its own per-language form must list"
            <> "\nevery parameter, so that the form's arity matches the"
            <> "\ntype's. A parameter the native macro does not interpolate"
            <> "\nis still listed."
      else do
        argTfs <- argsWith gscopeUni vG (inferConcreteTypeUniversal lang i . typeOf) ts
        return $ AppF (VarF (FV vG (CV vC))) argTfs
  (AppU (VarU vG) ts1, AppU (VarU (TV vC)) ts2)
    | appArgsCorrespond ts1 ts2 -> do
        argTfs <- argsWith2 gscopeUni vG (inferConcreteTypeUniversalStructural lang i gscopeUni t) ts1 ts2
        return $ AppF (VarF (FV vG (CV vC))) argTfs
  _ | Just g' <- stepTowardCompound gscopeUni g c ->
        inferConcreteTypeUniversalStructural lang i gscopeUni t g' c
  _ -> do
    anc <- CMS.gets stateRecordAncestors
    case weave gscopeUni anc g c of
      (Right tf) -> return tf
      (Left _) -> case T.evaluateStep gscopeUni g of
        (Just reducedGType)
          | reducedGType /= g ->
              inferConcreteTypeUniversal lang i (typeOf reducedGType)
        _ -> do
          cscopeUni <- MM.getConcreteScope lang
          case inheritedForm cscopeUni gscopeUni g of
            Just parent -> inferConcreteTypeUniversal lang i (typeOf parent)
            Nothing ->
              MM.throwRejected $
                "Failed to infer concrete type for" <+> pretty t
                  <> ": Could not reduce type in broadest scope"

-- | The wire parent of a newtype that has no form in this language. Such a
-- newtype takes its parent's native form (manual: "When a newtype needs a
-- Packable instance").
inheritedForm :: Scope -> Scope -> TypeU -> Maybe TypeU
inheritedForm cscope gscope g = case g of
  VarU v -> step v
  AppU (VarU v) _ -> step v
  _ -> Nothing
  where
    step v = case Map.lookup v gscope of
      Just entries
        | any (\(_, _, _, _, k) -> k == TypedefNewtype) entries
        , not (T.declaresFormAt cscope gscope g) -> T.expandWireParent gscope g
      _ -> Nothing

-- | In a compiled language a record holds its fields in the host's
-- convention, whether the program declares its type (@record Rust => Ops =
-- "Ops"@) or the compiler generates it (GEN-8): a function of type
-- @A -> \<E\> C@ there runs its effect when called and returns @C@. The
-- record's concrete type says so, so its native declaration, accessors and
-- marshalling agree with the value it holds;
-- 'Morloc.CodeGenerator.EffectBoundary' adapts the fields where such a
-- record is built and where one is read. Every record of a dynamic language
-- holds morloc values. Nested records are left to their own node.
hostConvention :: Lang -> MorlocMonad TypeF -> MorlocMonad TypeF
hostConvention lang built = do
  t <- built
  reg <- CMS.gets stateLangRegistry
  return $ case t of
    NamF o v ps rs
      | LR.registryIsCompiled reg (ML.langName lang) ->
          NamF o v ps [(k, eager f) | (k, f) <- rs]
    _ -> t
  where
    -- a suspension is a closure of no arguments; a function whose result is
    -- one returns the result instead
    eager (FunF [] c) = FunF [] (eager c)
    eager (FunF as r) = FunF (map eager as) (eager (runs r))
    eager (AppF h ts) = AppF h (map (fmap eager) ts)
    eager (OptionalF x) = OptionalF (eager x)
    eager x = x
    runs (FunF [] c) = c
    runs r = r

-- | The compound shapes every walk descends through, given the walk to
-- recurse with; the one list of them, shared by both structural walks and
-- 'weave'. Every type nested in a suspension, optional, function or record
-- must pass through a structural walk rather than the pure 'weave', which
-- cannot reach the per-language scope: a parameterized `data` woven there
-- keeps its morloc name instead of its instance name.
structuralCompound
  :: Applicative f
  => Scope
  -> Knot f
  -> (TypeU -> TypeU -> f TypeF)
  -> TypeU -> TypeU -> Maybe (f TypeF)
structuralCompound scope knot walk g c = case (g, c) of
  -- A reference to a record whose fields are being walked: marked here,
  -- tied by 'tieSelf' once the record is built.
  _ | Just key <- recordKey g, Set.member key (knotAncestors knot) ->
        Just (pure (VarF (FV (selfMark key) (CV ""))))
  -- A suspension is a closure of no arguments in every pool.
  (EffectU _ g', EffectU _ c') -> Just $ FunF [] <$> walk g' c'
  (OptionalU g', OptionalU c') -> Just $ OptionalF <$> walk g' c'
  (FunU gs gr, FunU cs cr)
    | length gs == length cs ->
        Just $ FunF <$> zipWithSameM walk gs cs <*> walk gr cr
  (NamU o1 v1 ts1 rs1, NamU o2 v2 ts2 rs2)
    | o1 == o2 && length ts1 == length ts2 && length rs1 == length rs2 ->
        Just $ fmap (tieSelf (v1, ts1)) $ NamF o1 (FV v1 (CV (unTVar v2)))
          <$> argsWith2 scope v1 walk ts1 ts2
          <*> knotEnter knot (v1, ts1) walk (\walk' -> zipWithSameM (\(_, g') (k, c') -> (,) k <$> walk' g' c') rs1 rs2)
  _ -> Nothing

-- | The mark of a reference to the record occurrence with this key: its
-- name and its general arguments, so a reference inside another
-- instantiation of the record is tied to the one it names.
selfMark :: (TVar, [TypeU]) -> TVar
selfMark key = TV ("#self:" <> MT.pack (show key))

-- | Whether a type holds a mark 'tieSelf' did not replace.
holdsSelfMark :: TypeF -> Bool
holdsSelfMark (VarF (FV (TV n) _)) = "#self:" `MT.isPrefixOf` n
holdsSelfMark t = any holdsSelfMark (childrenOf t)

-- | The records whose fields a walk is inside, by name and general
-- arguments, and how a walk enters one: given the walk the fields are to
-- be walked with. A reference to one of them at its own arguments is a
-- reference to that occurrence (GEN-1).
data Knot f = Knot
  { knotAncestors :: Set.Set (TVar, [TypeU])
  , knotEnter
      :: (TVar, [TypeU])
      -> (TypeU -> TypeU -> f TypeF)
      -> ((TypeU -> TypeU -> f TypeF) -> f [(Key, TypeF)])
      -> f [(Key, TypeF)]
  }

-- | The name and arguments a type refers to a record by.
recordKey :: TypeU -> Maybe (TVar, [TypeU])
recordKey (VarU v) = Just (v, [])
recordKey (AppU (VarU v) ts) = Just (v, ts)
recordKey (LabeledU _ t) = recordKey t
recordKey _ = Nothing

-- | The knot of a monadic walk: the records in compiler state, so a walk
-- re-entered from the top ('inferConcreteType', as a `data` type's arms
-- are) still sees them.
stateKnot :: MorlocMonad (Knot MorlocM)
stateKnot = do
  anc <- CMS.gets stateRecordAncestors
  return (Knot anc enter)
  where
    enter key walk k = withSetMember stateRecordAncestors (\v st -> st {stateRecordAncestors = v}) key (k walk)

-- | Run an action with a `data` instantiation among the ancestors.
withVariantAncestor :: (TVar, [TypeU]) -> MorlocMonad a -> MorlocMonad a
withVariantAncestor = withSetMember stateVariantAncestors (\v st -> st {stateVariantAncestors = v})

-- | Run an action with a key added to a set in compiler state; the set is
-- as it was afterwards, whether the action returns or throws.
withSetMember
  :: Ord k
  => (MorlocState -> Set.Set k)
  -> (Set.Set k -> MorlocState -> MorlocState)
  -> k
  -> MorlocMonad a
  -> MorlocMonad a
withSetMember get set key act = do
  old <- CMS.gets get
  CMS.modify (set (Set.insert key old))
  r <- act `MM.catchError` (\e -> CMS.modify (set old) >> MM.throwError e)
  CMS.modify (set old)
  return r

-- | Replace a built record's marked references to itself by references to
-- it at its own resolved arguments.
tieSelf :: (TVar, [TypeU]) -> TypeF -> TypeF
tieSelf key (NamF o fv ps rs) = NamF o fv ps [(k, go x) | (k, x) <- rs]
  where
    mark = selfMark key
    go t = case t of
      VarF (FV m _) | m == mark -> RecF fv ps
      _ -> mapChildren go t
tieSelf _ t = t

-- | Expand the general side one step when the expansion's shape
-- corresponds to the concrete side's, as when a record, a tuple or an alias
-- of a function type is named on the general side and spelled out on the
-- concrete one. The walk then continues on the expansion; handing the pair
-- to 'weave' would expand it there and weave everything beneath purely.
-- Nothing when the expansion does not correspond, which leaves the pair to
-- 'weave' as before. An applied type with kind arguments (@Table n r@,
-- @Vector n a@) is not stepped: pairing its Nat and row arguments is
-- 'weave's, and the universal walk cannot do it.
stepTowardCompound :: Scope -> TypeU -> TypeU -> Maybe TypeU
stepTowardCompound scope g c = case T.evaluateStep scope g of
  Just g' | g' /= g && corresponds g' -> Just g'
  _ -> Nothing
  where
    corresponds g' = isJust (dataHeadOf scope g')
      || isJust (structuralCompound scope (Knot Set.empty (\_ w k -> k w)) (\_ _ -> Const ()) g' c)
      || case (g', c) of
           (AppU (VarU _) ts1, AppU (VarU _) ts2) ->
             snd (partitionKindArgs kindOfU ts1) == 0 && appArgsCorrespond ts1 ts2
           _ -> False

-- | Two argument lists the applied-type shortcut pairs one to one. The kind
-- test rejects phantom-Nat aliases whose concrete form has fewer type slots.
appArgsCorrespond :: [TypeU] -> [TypeU] -> Bool
appArgsCorrespond ts1 ts2 =
  length ts1 == length ts2
    && length (fst (partitionKindArgs kindOfU ts1)) == length (fst (partitionKindArgs kindOfU ts2))

-- | The concrete side of an applied type, which is either applied too or
-- has already collapsed to a bare name. Mirrors @concreteHeadName@ in the
-- module-scoped walk.
universalConcreteHead :: TypeU -> Maybe MT.Text
universalConcreteHead (AppU (VarU (TV n)) _) = Just n
universalConcreteHead (VarU (TV n)) = Just n
universalConcreteHead _ = Nothing

inferConcreteTypeUUniversal :: Lang -> TypeU -> MorlocMonad TypeU
inferConcreteTypeUUniversal lang generalType = do
  gscopeUni <- CMS.gets stateGeneralTypedefs
  cscopeUni <- MM.getConcreteScope lang
  let attemptUni = inferConcreteTypeU' generalType (cscopeUni, gscopeUni)
  case attemptUni of
    (Right t) -> return t
    (Left (Rejected Unplaced e2)) ->
      MM.throwRejected $
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
weave :: Scope -> Set.Set (TVar, [TypeU]) -> TypeU -> TypeU -> Either MDoc TypeF
weave gscope = w
  where
    -- @anc@: the records whose fields the walk is inside ('Knot').
    w anc t1 t2 | Just r <- structuralCompound gscope (knotOf anc) (w anc) t1 t2 = r
    -- A `data` type weaves to 'EnumF' rather than a plain 'VarF' so the
    -- constructor names reach codegen. Making this the canonical TypeF for
    -- an enum is what lets every backend recognize one structurally --
    -- otherwise only serialized positions (which come through SerialEnum)
    -- would see it, and a native-position enum would look like an opaque
    -- VarF with an unhelpful name.
    w _ (VarU v1) (VarU (TV v2)) = return $ case T.dataHead gscope (T.whnf gscope (VarU v1)) of
      Just (n, _, ctors)
        -- Every constructor argument-free: the one-byte form.
        | all (null . snd) ctors -> EnumF (FV n (CV v2)) [] (map fst ctors)
        -- Otherwise a tagged pointer. The arms are NOT expanded here: a
        -- field's concrete form is a per-language question and this walk
        -- has only the general scope, so weaving a field against itself
        -- would leave the morloc name where a pool needs its own. The
        -- reference stays opaque and the arms are built where the wire
        -- form is, which is the same place the recursive case is tied off.
        | otherwise -> VarF (FV v1 (CV v2))
      Nothing -> VarF (FV v1 (CV v2))
    -- AppU vs AppU: weave heads, then args. If heads weave but arg lists
    -- have mismatched lengths (e.g. general @Pair Int@ has 1 arg while
    -- the concrete-side resolution expanded to @"tuple" [int, ?(...)]@
    -- with 2 args), fall through to the catch-all @evaluateStep@ retry
    -- on @t1@. This is the same generalization the catch-all already
    -- performs for type-level mismatches; we just need to opt the AppU
    -- branch into it on arg-length failure instead of failing outright.
    w anc t1@(AppU h1 ts1) t2@(AppU h2 ts2) =
      case (AppF <$> w anc h1 h2 <*> weaveArgs anc (slotsOf h1 (length ts1)) ts1 ts2) of
        r@(Right _) -> r
        Left _ -> wStep anc t1 t2
    w anc (LabeledU _ t1) t2 = w anc t1 t2
    w _ (ForallU v (VarU v')) _ | v == v' = return $ UnkF (FV v (CV (unTVar v)))
    w anc t1 t2 = wStep anc t1 t2

    knotOf anc = Knot anc (\key _ k -> k (w (Set.insert key anc)))

    -- Step the general type one level via @evaluateStep@ and retry.
    wStep anc t1 t2 = case T.evaluateStep gscope t1 of
      Nothing -> Left $ "failed to weave:" <+> "\n  t1:" <+> pretty t1 <> "\n  t2:" <> pretty t2
      (Just t1') ->
        if t1 == t1'
          then Left ("failed to weave:" <> pretty t1 <+> "vs" <+> pretty t1')
          else w anc t1' t2

    -- A kind argument is decided by the general side alone and consumes the
    -- concrete argument of its kind when the concrete form kept one; a
    -- newtype's expansion may have dropped it.
    slotsOf (VarU v) n = argKinds gscope v n
    slotsOf _ n = replicate n (Nothing, False)

    weaveArgs :: Set.Set (TVar, [TypeU]) -> [(Maybe Kind, Bool)] -> [TypeU] -> [TypeU] -> Either MDoc [ArgF]
    weaveArgs _ _ [] [] = Right []
    weaveArgs _ _ [] cs
      | all (isJust . kindOfU) cs = Right []  -- trailing kind args in concrete only
      | otherwise = Left "concrete type has more non-Nat args than general type in weave"
    weaveArgs anc sls (g : gs) cs
      | Just k <- kindArgOf g = (KiA k :) <$> weaveArgs anc (drop 1 sls) gs (dropKindHead (kindOfK k) cs)
    -- A ground row at a Rec parameter pairs with the concrete row when the
    -- form kept one, and is erased when it did not: a row's columns hold
    -- no values (KIND-2). A table's form keeps its row.
    weaveArgs anc sls@((_, True) : _) (g : gs) cs
      | Just fields <- rowFields g = case cs of
          (c : cs')
            | Just cfields <- rowFields c
            , map fst fields == map fst cfields -> do
                cols <- mapM (\((k, gt), (_, ct)) -> (,) (Key k) <$> w anc gt ct) (zipSame fields cfields)
                (KiA (RowK cols) :) <$> weaveArgs anc (drop 1 sls) gs cs'
            | kindOfU c == Just KindRec -> (KiA (VoidK KindRec) :) <$> weaveArgs anc (drop 1 sls) gs cs'
          _ | length cs < length (g : gs) -> (KiA (VoidK KindRec) :) <$> weaveArgs anc (drop 1 sls) gs cs
          (c : cs') -> (:) <$> (retagArg (headSlot sls) <$> w anc g c) <*> weaveArgs anc (drop 1 sls) gs cs'
          [] -> Left "general type has more non-Nat args than concrete type in weave"
    weaveArgs anc sls (g:gs) (c:cs) =
      (:) <$> (retagArg (headSlot sls) <$> w anc g c) <*> weaveArgs anc (drop 1 sls) gs cs
    weaveArgs _ _ _ [] = Left "general type has more non-Nat args than concrete type in weave"

    -- Drop a leading concrete argument of kind @k@, if there is one.
    dropKindHead :: Kind -> [TypeU] -> [TypeU]
    dropKindHead k (c : cs) | kindOfU c == Just k || k == KindNat && maybe False fillsNat (kindOfU c) = cs
    dropKindHead _ cs = cs

-- | An applied argument: a kind argument, decided by the general type alone
-- (an alias of a kind was expanded before code generation), or the type the
-- walk gives.
argWith :: Applicative f => (Maybe Kind, Bool) -> (TypeU -> f TypeF) -> TypeU -> f ArgF
argWith (_, True) walk g
  | Just fields <- rowFields g = KiA . RowK <$> traverse (\(k, t) -> (,) (Key k) <$> walk t) fields
argWith sl walk g = maybe (retagArg sl <$> walk g) (pure . KiA) (kindArgOf g)

-- | The fields of a ground row (`{y = Int}` at a Rec parameter), in order.
rowFields :: TypeU -> Maybe [(MT.Text, TypeU)]
rowFields (LabeledU _ t) = rowFields t
rowFields (NamU NamRecord r [] rs) | r == anonRecordVar = Just [(k, t) | (Key k, t) <- rs]
rowFields t = groundRecFields t

-- | The arguments of @v@, each at its declared position.
argsWith :: Applicative f => Scope -> TVar -> (TypeU -> f TypeF) -> [TypeU] -> f [ArgF]
argsWith scope v walk gs = sequenceA (zipWithSame (\sl g -> argWith sl walk g) (argKinds scope v (length gs)) gs)

-- | 'argsWith' for a walk over general arguments and their concrete forms.
argsWith2 :: Applicative f => Scope -> TVar -> (TypeU -> TypeU -> f TypeF) -> [TypeU] -> [TypeU] -> f [ArgF]
argsWith2 scope v walk gs cs = sequenceA (zipWith3Same (argWith2 walk) (argKinds scope v (length gs)) gs cs)

-- | 'argWith' for a walk over a general argument and its concrete form.
argWith2 :: Applicative f => (TypeU -> TypeU -> f TypeF) -> (Maybe Kind, Bool) -> TypeU -> TypeU -> f ArgF
argWith2 walk sl@(_, True) g c
  | Just gs <- rowFields g
  , Just cs <- rowFields c
  , map fst gs == map fst cs =
      KiA . RowK <$> traverse (\((k, gt), (_, ct)) -> (,) (Key k) <$> walk gt ct) (zipSame gs cs)
argWith2 walk sl g c = maybe (retagArg sl <$> walk g c) (pure . KiA) (kindArgOf g)

-- | A woven argument at a declared position: a ground row that fills a Rec
-- parameter is that row, and a type nothing determined at a position of
-- another kind is that kind's erased placeholder.
retagArg :: (Maybe Kind, Bool) -> TypeF -> ArgF
retagArg (_, True) (NamF NamRecord (FV r _) [] cols) | r == anonRecordVar = KiA (RowK cols)
retagArg (Just k, _) (UnkF _) | k /= KindType = KiA (VoidK k)
retagArg _ t = TyA t

headSlot :: [(Maybe Kind, Bool)] -> (Maybe Kind, Bool)
headSlot (sl : _) = sl
headSlot [] = (Nothing, False)

-- | A kind argument as a pool sees it: its value when ground, else the
-- erased placeholder of its kind.
groundK :: KTerm -> KArgF
groundK k = fromMaybe (VoidK (kindOf k)) (kindArgOf (ktermU k))
  where
    -- a variable has no kind of its own here; it stands in a Nat or Str term
    kindOf x = case x of
      VoidT kd -> kd
      StrLitT _ -> KindStr
      StrConcatT _ _ -> KindStr
      _ -> KindNat

-- | A general argument that is not a type, as a pool sees it: a ground Nat
-- or Str value, or the erased placeholder of its kind (KIND-2).
kindArgOf :: TypeU -> Maybe KArgF
kindArgOf g = case g of
  LabeledU _ x -> kindArgOf x
  NatLitU n -> Just (NatLitK n)
  NatVoidU -> Just NatVoidK
  NatAddU _ _ -> Just (groundNatK g)
  NatMulU _ _ -> Just (groundNatK g)
  NatSubU _ _ -> Just (groundNatK g)
  NatDivU _ _ -> Just (groundNatK g)
  NatVarU _ -> Just NatVoidK
  IntVarU _ -> Just NatVoidK
  _ -> (\k -> maybe (VoidK k) StrLitK (groundStr g)) <$> kindOfU g
  where
    groundStr (StrLitU s) = Just s
    groundStr (StrConcatU a b) = (<>) <$> groundStr a <*> groundStr b
    groundStr (LabeledU _ t) = groundStr t
    groundStr _ = Nothing

-- | Whether values of a general type have a representation in a language:
-- the type resolves there, and, when @declaredRecords@, every record in it is
-- declared for the language (a record with no declaration there resolves only
-- to its morloc name, which names nothing in the pool). Building a record
-- always needs its declaration; holding one does not in a language with a
-- generic record form. A @data@ type is generated in each pool that uses it.
canHoldType :: Bool -> Lang -> Int -> Type -> MorlocMonad Bool
canHoldType declaredRecords lang i t = do
  let key = (declaredRecords, langName lang, i, t)
  cached <- CMS.gets (Map.lookup key . stateHoldCache)
  case cached of
    Just answer -> return answer
    Nothing -> do
      answer <- canHoldType' declaredRecords lang i t
      CMS.modify (\st -> st {stateHoldCache = Map.insert key answer (stateHoldCache st)})
      return answer

canHoldType' :: Bool -> Lang -> Int -> Type -> MorlocMonad Bool
canHoldType' declaredRecords lang i t = do
  -- a failed inference is an answer here, not an error: whatever it changed
  -- on the way (the `data` types being expanded) is undone
  st0 <- CMS.get
  ( do
      _ <- inferConcreteType lang (Idx i t)
      gscope <- MM.getGeneralScope
      if declaredRecords
        then and <$> mapM (declared gscope) (recordNames gscope t)
        else return True
    )
    `MM.recoverRejected` (CMS.put st0 >> return False)
  where
    declared gscope v = do
      cscope <- MM.getConcreteScope lang
      return (isJust (dataHeadOf gscope (VarU v)) || Map.member v cscope)
    -- the records a type holds, whether written out or named
    recordNames g ty = case ty of
      NamT _ v ps rs -> v : concatMap (recordNames g) (valueArgsT g v ps <> map snd rs)
      VarT v -> [v | namesRecord g v]
      FunT ins out -> concatMap (recordNames g) (out : ins)
      AppT f xs -> concatMap (recordNames g) (f : appValueArgsT g f xs)
      OptionalT x -> recordNames g x
      EffectT _ x -> recordNames g x
      _ -> []

-- | Whether a general type name is a record.
namesRecord :: Scope -> TVar -> Bool
namesRecord g v = case Map.lookup v g of
  Just ((_, NamU {}, _, _, _) : _) -> True
  _ -> False

inferConcreteVar :: Lang -> Indexed TVar -> MorlocMonad FVar
inferConcreteVar lang t0@(Idx i v) = do
  MM.sayVVV $ "inferConcreteVar" <+> pretty lang <+> pretty t0
  cscope <- MM.getConcreteScope lang
  case Map.lookup v cscope of
    (Just ((_, t, _, True, _) : _)) -> return $ FV v (CV . unTVar $ extractKey t)
    -- Non-terminal concrete alias: e.g. `type Cpp => Array a = List a`.
    -- Follow through the body's head (recursively) until a terminal entry
    -- is reached, then pair the *original* v with the resolved concrete
    -- name. This preserves the morloc-level identity (Array stays Array)
    -- while picking up the runtime concrete (std::vector here).
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
        gscope <- MM.getGeneralScope
        let
          -- A `data` type with no per-language mapping. Its general body is
          -- the constructor table, not an alias, so chasing the body's head
          -- resolves to whatever type that table is spelled with rather than
          -- to anything the user wrote. The pool generates a native
          -- definition under the type's own name, so that is the concrete
          -- name here.
          -- By name: an alias of a `data` type is chased through its
          -- body below like any other alias.
          isData = isJust (dataCtorsOfName gscope v)
        let
          -- Guard against self-recursive lookup: if the body's
          -- extracted key resolves back to v (e.g. a record whose
          -- general definition is `record Container a where ...` --
          -- the body's NamU carries the same TVar `Container`), a
          -- naive recursion loops forever because each iteration
          -- looks the same TVar up in gscope and gets the same
          -- body back. When the body's key is v itself, fall through
          -- to pairEval, which produces a comprehensible
          -- "No concrete <lang> type for <v>" error naming the
          -- missing instance.
          gscopeBody = case Map.lookup v gscope of
            (Just ((_, body, _, _, _) : _)) | extractKey body /= v -> Just (extractKey body)
            _ -> Nothing
        case if isData then Nothing else gscopeBody of
          Just bodyKey -> do
            FV _ cv <- inferConcreteVar lang (Idx i bodyKey)
            return $ FV v cv
          Nothing | isData -> return $ FV v (CV (unTVar v))
          Nothing | namesRecord gscope v -> MM.throwSourcedError i $
            "No concrete" <+> pretty lang <+> "type for record"
            <+> squotes (pretty v) <> "." <+> addDecl "record" <> "."
          Nothing -> do
            -- Last resort: transitive resolution via pairEval.
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
                <+> squotes (pretty v) <> "." <+> addDecl "type" <> ","
                <+> "or import a module that provides one"
                <+> parens ("e.g. root-" <> pretty lang) <> "."
  where
    addDecl kw =
      "Add a '" <> kw <+> pretty lang <+> "=>" <+> pretty v <+> "= \"...\"' declaration"

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
dataHeadOf gscope t = (\(v, ts, _) -> (v, ts)) <$> T.dataHead gscope (T.whnf gscope t)

-- | A type's per-language name: its mapping when it declares one, its own
-- name when the pool generates the type.
concreteNameOf :: Scope -> TVar -> MT.Text
concreteNameOf cscope v = case Map.lookup v cscope of
  Just entries | (n : _) <- [n' | (_, body, _, _, _) <- entries, Just n' <- [bodyNameOf body]] -> n
  _ -> unTVar v

-- | A Nat expression in a concrete type: its value when ground, so that a
-- Vector's computed length is still checked (DIM-8), and the unknown
-- placeholder otherwise.
groundNatK :: TypeU -> KArgF
groundNatK t = maybe NatVoidK NatLitK (typeUToNatExpr t >>= NS.evalNat Map.empty)
