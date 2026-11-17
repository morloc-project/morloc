{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : CppTranslator
Description : Translate 'SerialManifold' trees into C++ pool source code
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Stateful C++ translator using the two-phase IR architecture: lower the
'SerialManifold' tree into 'IStmt'/'IExpr' via 'LowerConfig', then print
via 'CppPrinter'. Handles C++-specific concerns like compilation flags,
include paths, struct generation, and template instantiation.
-}
module Morloc.CodeGenerator.Pools.CAbi.Members.Cpp
  ( translate
  , cppLang
  ) where

import Morloc.Data.Zip (indexed, indexedFrom, mapIndexedM, zipExact, zipSame, zipWithSame)
import Data.Foldable (toList)
import Data.Char (isUpper)
import Control.Monad.Identity (Identity, runIdentity)
import qualified Control.Monad.State as CMS
import qualified Morloc.CodeGenerator.Pools.CAbi.Members.CppPrinter as CP
import qualified Data.Char as DC
import Data.Ord (comparing)
import qualified Data.Graph as Graph
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.CodeGenerator.Grammars.Common
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import Morloc.CodeGenerator.Grammars.Translator.Imperative
  (
  Low (..)
  , LowN
  , LowS
  , loopBodyDocs
  , LoopResult (..)
  , armIndex
  , infixOperator
  , papplySteps
  , containsClosure
  , IType (..)
  , IOwnership (..)
  , LowerConfig (..)
  , buildProgramM
  , defaultFoldRules
  , markLastReadsSeeded
  , lastBuilds
  , nativeManifoldsUnder
  , RNode (..)
  , rnodeChildren
  , expandDeserialize
  , expandSerialize
  , nativeToWire
  , renderIType
  , toIType
  , wireToNative
  )
import Morloc.CodeGenerator.LogTemplate (RenderedTemplate (..), collectRenderedTemplates)
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Build.Params as BP
import qualified Morloc.DataFiles as DF
import qualified Morloc.LangRegistry as LR
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Infer (declaredDataNames)
import Morloc.CodeGenerator.Ownership (LetMode (..), LetPlan (..), OwnCaps (..), PoolPlan (..), byValueIndices, changedSlots, emptyPoolPlan, letMode, letPlan, nativeLets, planPool)
import Morloc.CodeGenerator.OwnershipCheck (checkManifold)
import System.Environment (lookupEnv)
import Morloc.CodeGenerator.GenDefs (DataDef (..), Disagreement (..), GenDef (..), carriedTemplateArgs, nestedSelfUses, nestedSelfUseReason, dataDefs, dataNamesIn, nativeDataParams, RecordUse, disagreements, inPoolSerialASTs, placeTypes, genDefs, nativeParam, nativeParams, recordUses)
import qualified Morloc.CodeGenerator.Platform as P
import Morloc.CodeGenerator.Serial
  ( serialAstToType
  , serialAstToNativeType
  , isSerializable
  , generatedObject
  , serialSubtrees
  , unpackedAsTypeF
  , wireSerialAstToType
  , containsFunT
  , containsEffectT
  , serialAstToMsgpackSchema
  , shallowType
  , isSerializable
  )
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.Version as MV
import Morloc.Quasi
import qualified Morloc.System as MS
import Morloc.ProgramBuilder.Build (stagingPoolsDir)
import qualified Morloc.TypeEval as TE

-- HACK: repeating these here is hacky
-- This same data is repeated in cpp/lang.yaml
cppLang :: ML.Lang
cppLang = ML.Lang "cpp" "cpp"

-- | True when a conditional arm is a bare @throw. Used by 'lcMakeIf' to emit
-- the arm as a statement rather than an assignment (see the comment there).
isNativeThrow :: NativeExpr -> Bool
isNativeThrow (IntrinsicN _ IntrThrow _ _) = True
isNativeThrow (DoBlockN _ e) = isNativeThrow e
isNativeThrow _ = False

-- | One arm of a conditional assigning its value to the result variable. A
-- @throw arm never returns, so it is a bare statement rather than an
-- assignment: assigning the universal-conversion `_mlc_throw` helper into
-- a container-typed result is an ambiguous C++ `operator=`.
cppIfArm :: Bool -> MDoc -> Maybe NativeExpr -> PoolDocs -> [MDoc]
cppIfArm unit v armE d
  | maybe False isNativeThrow armE = poolPriorLines d <> [poolExpr d <> ";"]
  | otherwise = poolPriorLines d <> [v <+> "=" <+> cppMoved unit (maybe False armOwnsResult armE) (poolExpr d) <> ";"]

-- | A conditional's value or an arm's, moved when the arm owns it
-- ('armOwnsResult') and the result is not Unit, which is emitted as a bare
-- statement where a discarded std::move is a warning.
cppMoved :: Bool -> Bool -> MDoc -> MDoc
cppMoved unit owned e = if owned && not unit then "std::move(" <> e <> ")" else e

cppUnitResult :: NativeExpr -> Bool
cppUnitResult e = case typeFof e of
  VarF (FV tv _) -> tv == BT.unit
  _ -> False

-- | Whether a conditional arm's value is a let bound inside the arm.
armOwnsResult :: NativeExpr -> Bool
armOwnsResult = go Set.empty
  where
    go bound (NativeLetN i _ body) = go (Set.insert i bound) body
    go bound (SerialLetN _ _ body) = go bound body
    go bound (LetVarN _ j _) = Set.member j bound
    go _ _ = False

serialType :: MDoc
serialType = "uint8_t*"

data CallSemantics = Copy | Reference | ConstPtr

class HasCppType a where
  cppTypeOf :: a -> CppTranslator MDoc

  cppArgOf :: CallSemantics -> Arg a -> CppTranslator MDoc

-- A scalar is passed by value: a reference to it would make the caller keep
-- the value in memory for each call. It stays const (CPP-1).
setCallSemantics :: CallSemantics -> MDoc -> MDoc
setCallSemantics Copy typestr = typestr
setCallSemantics Reference typestr
  | isScalarCppType typestr = "const" <+> typestr
  | otherwise = "const" <+> typestr <> "&"
setCallSemantics ConstPtr typestr = "const" <+> typestr

-- | Cheap-to-copy scalar C++ types: a copy is a register move.
isScalarCppType :: MDoc -> Bool
isScalarCppType t = render t `elem`
  [ "int8_t", "int16_t", "int32_t", "int64_t"
  , "uint8_t", "uint16_t", "uint32_t", "uint64_t"
  , "int", "unsigned int", "long", "unsigned long", "long long", "unsigned long long"
  , "short", "unsigned short", "char", "signed char", "unsigned char", "size_t"
  , "float", "double", "bool" ]

-- | The capture list of a do-block thunk ('lcMakeDoBlock'). By value: a
-- thunk may outlive the frame that built it.
thunkCapture :: Text
thunkCapture = "[=]"

-- | A thunk written inline as the body argument of '_mlc_try' captures by
-- reference: '_mlc_try' calls it once, before it returns, and nothing else
-- can hold the temporary, so a by-value capture only copies each captured
-- value (a whole batch, for a @write). A thunk bound to a name may be
-- stored or shared, so it keeps its by-value capture. The thunk arrives
-- rendered; its lines are re-joined verbatim, which is sound because
-- generated C++ never breaks a line inside a string literal.
tryThunkByReference :: MDoc -> MDoc
tryThunkByReference thunk = case T.stripPrefix (thunkCapture <> "(") (render thunk) of
  Just rest -> concatWith (\a b -> a <> hardline <> b) (map pretty (T.splitOn "\n" ("[&](" <> rest)))
  Nothing -> thunk

-- Render a concrete type name with template arguments. If the name
-- contains `$N` placeholders (positional macros, e.g. "container_t<$1>"),
-- route through `expandMacro` so the args fill the placeholders. Otherwise,
-- treat the name as a bare identifier and append `<arg0, arg1, ...>` via
-- `printRecordTemplate`. This unifies the two conventions for declaring
-- parameterized concrete records:
--   record Cpp => Container a = "container_t"            -- bare name
--   record Cpp => Container a = "container_t<$1>" a      -- macro template
-- both lower to `container_t<int64_t>` for `Container Int`.
--
-- 'argDocs' must already exclude kind args.
renderTemplatedType :: Text -> [MDoc] -> MDoc
renderTemplatedType name argDocs
  | T.any (== '$') name = pretty $ expandMacro name (map render argDocs)
  | otherwise = pretty name <> CP.printRecordTemplate argDocs

-- | No type is cheap enough to copy rather than reference; a tag test may
-- read a value a field was taken from, since the payload stays in its arm.
cppCaps :: OwnCaps
cppCaps = OwnCaps {ocCheap = const False, ocTagAfterTake = True, ocLends = const True}

chooseCallSemantics :: TypeM -> CallSemantics
chooseCallSemantics Passthrough = ConstPtr -- const uint8_t* packet
chooseCallSemantics (Serial _) = ConstPtr -- const uint8_t* packet
chooseCallSemantics (Native _) = Reference -- for now, primitives should be pass by copy
-- A function value is only called ('std::function::operator()' is const) or
-- copied into something that keeps it, so a reference never costs a copy.
chooseCallSemantics (Function _ _) = Reference

instance HasCppType TypeM where
  cppTypeOf (Serial _) = return serialType
  cppTypeOf (Native c) = cppTypeOf c
  cppTypeOf Passthrough = return serialType
  cppTypeOf (Function ts t) = do
    t' <- cppTypeOf t
    ts' <- mapM cppTypeOf ts
    return $ "std::function<" <> t' <> tupled ts' <> ">"

  cppArgOf s (Arg i t) = do
    typeStr <- cppTypeOf t
    let typeStrQualified = setCallSemantics s typeStr
    return $ case t of
      (Serial _) -> typeStrQualified <+> (svarNamer cppLocals) i
      (Native _) -> typeStrQualified <+> (nvarNamer cppLocals) i
      Passthrough -> typeStrQualified <+> (svarNamer cppLocals) i
      (Function _ _) -> typeStrQualified <+> (nvarNamer cppLocals) i

instance HasCppType NativeManifold where
  cppTypeOf = cppTypeOf . typeMof
  cppArgOf s r = cppArgOf s $ fmap typeMof r

instance {-# OVERLAPPABLE #-} (HasTypeF e) => HasCppType e where
  cppTypeOf = f . typeFof
    where
      -- A `data` type's name is its template instantiated with this
      -- occurrence's arguments when the user gave it one (`MyBox<$1>`), and
      -- the name itself otherwise -- a generated type is monomorphic per
      -- instantiation and spells nothing of its arguments.
      dataTypeName gv x ps
        | T.any (== '$') x = do
            let typeTs = tyArgs ps
            ts' <- mapM f typeTs
            -- A generated template in its module's header goes by its
            -- qualified name, any other by its identifier.
            public <- CMS.gets translatorPublic
            ddefs <- CMS.gets translatorDataDefs
            let (h, rest) = T.breakOn "<" x
                x' = case Map.lookup gv public of
                  Just q -> q <> rest
                  Nothing | Map.member gv ddefs -> nativeTypeName h <> rest
                  Nothing -> x
            return . pretty $ expandMacro x' (map render ts')
        -- A generated template's instance named by its arguments
        -- ('Infer.instanceName') is its own type.
        | otherwise = do
            ddefs <- CMS.gets translatorDataDefs
            if Map.member gv ddefs then return (pretty (nativeTypeName x)) else publicOr gv (pretty (nativeTypeName x))

      complainT :: Text -> CppTranslator ()
      complainT msg = CMS.modify $ \st -> st {translatorErrors = msg : translatorErrors st}

      -- A generated type declared in its module's header goes by its
      -- qualified name (GEN-4); any other by the name given.
      publicOr :: TVar -> MDoc -> CppTranslator MDoc
      publicOr gv d = maybe d pretty . Map.lookup gv <$> CMS.gets translatorPublic

      f (UnkF (FV _ x)) = return $ pretty (nativeTypeName (unCVar x))
      -- An enum lowers to its concrete name; the `enum class X : uint8_t`
      -- behind it is generated for this pool or supplied by the user
      -- through a `data Cpp => X = "..."` mapping.
      f (EnumF (FV gv (CV x)) ps _) = dataTypeName gv x ps
      -- A variant lowers to its concrete name; the type carrying the arms
      -- is generated for this pool or supplied by `data Cpp => X = "..."`.
      f (VariantF (FV gv (CV x)) ps _) = dataTypeName gv x ps
      -- Kindless or polymorphic-row `Table` lowers to a VarF tagged with
      -- the general type variable @BT.table@. The wire schema marker is
      -- @T@ on the encoder side; here on the C++ side it must lower to
      -- @mlc::ArrowTable@ regardless of any concrete-type hint (the
      -- legacy @<arrow>@ hint has been retired -- recognition is now
      -- by general-type identity, not by the concrete-name slot).
      f (VarF (FV gv _)) | BT.isTableVar gv = return "mlc::ArrowTable"
      -- Leak guard. pairEval leaves bnd-protected recursive aliases as
      -- @VarU v@ with the morloc-side TVar untouched; weave then
      -- synthesizes a CVar from that TVar's text, so the resulting
      -- FVar has @gv == cv@. Rendering it would emit a bare morloc
      -- identifier (e.g. `Pair`, `LL`) into pool.cpp. But @gv == cv@
      -- alone is ambiguous: a user-written @type Cpp => Foo = "Foo"@
      -- legitimately produces the same shape. Disambiguate by asking
      -- cscope (the same source pairEval uses): if it holds an entry
      -- whose body's outer name equals the CV, the mapping is genuine
      -- and we render normally; otherwise the CV came from bnd-protect
      -- and we raise 'leakError'.
      f (VarF (FV gv@(TV gvText) (CV cvText))) | gvText == cvText = do
        legit <- cscopeMatches gv cvText
        -- A `data` type this pool DECLARES defaults its concrete name to
        -- its own, which is the same shape a bnd-protect leak produces.
        -- The generated-type set is what separates them.
        declared <- CMS.gets translatorVariantNames
        if legit || Set.member gv declared
          then publicOr gv (pretty (nativeTypeName cvText))
          else leakError gvText
      f (VarF (FV _ x)) = return $ pretty (nativeTypeName (unCVar x))
      f (FunF ts t) = do
        t' <- f t
        ts' <- mapM f ts
        return $ "std::function<" <> t' <> tupled ts' <> ">"
      -- Leak guard on the parameterized form (mirror of the VarF
      -- guard above). An unmapped recursive alias appears as
      -- @AppF (VarF (FV gv (CV gv))) [...]@ where the head's CV is
      -- the bare morloc name (no @$N@ macros) and @expandMacro@ would
      -- drop the args. Disambiguate against cscope the same way.
      f (AppF (VarF (FV gv@(TV gvText) (CV cvText))) ts) | gvText == cvText = do
        legit <- cscopeMatches gv cvText
        if legit
          then do
            let typeTs = tyArgs ts
            ts' <- mapM f typeTs
            return . pretty $ expandMacro cvText (map render ts')
          else do
            declaredApp <- CMS.gets translatorVariantNames
            if Set.member gv declaredApp then return (pretty (nativeTypeName cvText)) else leakError gvText
      f (AppF t ts) = do
        -- $N in the CV template indexes type args only (NATIVE-1).
        t' <- f t
        let typeTs = tyArgs ts
        ts' <- mapM f typeTs
        return . pretty $ expandMacro (render t') (map render ts')
      -- A generated record is its definition at the arguments it carries,
      -- which are native at both levels: the unpacked view keeps a
      -- generated record native (GEN-14).
      f (NamF _ (FV gc (CV "struct")) ps _) = do
        records <- CMS.gets translatorRecords
        names <- CMS.gets translatorRecordNames
        defs <- CMS.gets translatorGenDefs
        let q = pretty (Map.findWithDefault (unTVar gc) gc names)
        case Map.lookup gc records of
          Just gd
            | null (nativeParams gd) -> return q
            | otherwise -> case carriedTemplateArgs gd ps of
                Just args -> (q <>) . CP.printRecordTemplate <$> mapM f args
                Nothing -> do
                  complainT ("a use of the record " <> unTVar gc <> " carries no arguments to spell it at")
                  return q
          Nothing -> do
            -- A record refused in planning has said why.
            fails <- CMS.gets translatorGenFailures
            unless (Map.member gc defs) $
              complainT ("the record " <> unTVar gc <> " has no C++ definition: "
                <> Map.findWithDefault "its declaration could not be resolved in C++" gc fails)
            return q
      -- A table's wire shape ('serialAstToType' of its serial form).
      f (NamF NamTable _ _ _) = return "mlc::ArrowTable"
      f (NamF _ (FV _ (CV s)) ps _) = renderTemplatedType s <$> mapM f (tyArgs ps)
      -- A back-reference to a generated record names its definition at
      -- its arguments.
      f (RecF (FV gv (CV "struct")) ps) = do
        names <- CMS.gets translatorRecordNames
        records <- CMS.gets translatorRecords
        case Map.lookup gv names of
          Just name -> case Map.lookup gv records of
            Just gd | not (null (nativeParams gd)) -> case carriedTemplateArgs gd ps of
              Just args -> (pretty name <>) . CP.printRecordTemplate <$> mapM f args
              Nothing -> do
                CMS.modify $ \s -> s {translatorErrors = ("a reference to the record " <> unTVar gv <> " carries no arguments to spell it at") : translatorErrors s}
                return (pretty name)
            _ -> return (pretty name)
          Nothing -> do
            fails <- CMS.gets translatorGenFailures
            CMS.modify $ \s -> s {translatorErrors = ("the record " <> unTVar gv <> " has no C++ definition: "
              <> Map.findWithDefault "its declaration could not be resolved in C++" gv fails) : translatorErrors s}
            return (pretty (tvarIdentifier gv))
      -- A reference resolved through the concrete scope: a user-mapped
      -- record is its form at the reference's arguments, as an occurrence
      -- of it is spelled. A record or alias with no C++ mapping is
      -- 'leakError'.
      f (RecF (FV gv (CV x)) ps) = do
        cscope <- CMS.gets translatorCScope
        dataNames <- CMS.gets translatorDataNames
        if Set.member gv dataNames then dataTypeName gv x ps else case Map.lookup gv cscope of
          -- A generated record's entry names no type ("struct"); its
          -- back-reference is the refusal above.
          Just ((_, body, _, _, _) : _) | Just "struct" <- bodyCV body -> f (RecF (FV gv (CV "struct")) ps)
          Just ((_, body, _, _, _) : _) | Just cv <- bodyCV body ->
            if null ps then return (pretty cv) else renderTemplatedType cv <$> mapM f (tyArgs ps)
          -- A recursive alias has no generated form and would otherwise
          -- reach the pool as a bare morloc identifier.
          _ -> leakError (unTVar gv)
      -- Recursive optional (?T where T points back to a containing record)
      -- collapses to a single `std::shared_ptr<const T>` with `nullptr ==
      -- absent` (CPP-6).
      -- C++'s incomplete-type rule forbids `optional<T>` inside T's own
      -- body (optional needs `sizeof(T)`, not yet known while T is being
      -- parsed), so the cycle must be broken by a pointer. shared_ptr is
      -- copyable (unlike unique_ptr, which would make the struct move-only
      -- and break the codegen's copy-assuming emission sites), and the
      -- collapsed `shared_ptr<T>` form matches the voidstar wire format's
      -- single-relptr Optional encoding directly.
      -- A non-recursive `?T` keeps `std::optional<T>` -- sizeof(T) is
      -- known and inline storage is leaner.
      f (OptionalF t@(RecF (FV gv _) _)) = do
        t' <- f t
        handles <- CMS.gets translatorHandles
        dataNames <- CMS.gets translatorDataNames
        -- A handle is complete, so its optional is the ordinary one; so is
        -- a `data` type's, which is a tagged pointer.
        return $ if Map.member gv handles || Set.member gv dataNames
          then "std::optional<" <> t' <> ">"
          else "std::shared_ptr<const " <> t' <> ">"
      f (OptionalF t) = do
        t' <- f t
        return $ "std::optional<" <> t' <> ">"

      -- True iff cscope holds an entry for @gv@ whose body's outer
      -- name equals @cvText@. A legitimate @type Cpp => Foo = "Foo"@
      -- has @cscope[Foo] = (..., VarU (TV "Foo"), ...)@ so the body's
      -- outer name matches; a pairEval bnd-protect leak has no such
      -- entry (or one whose body's name differs from cvText).
      cscopeMatches gv cvText = do
        cscope <- CMS.gets translatorCScope
        return $ case Map.lookup gv cscope of
          Just entries -> any (\(_, body, _, _, _) -> bodyCV body == Just cvText) entries
          Nothing -> False

      -- Extract the body's outer-name string from a cscope entry's body.
      -- Returns @Just name@ for the alias shapes that contribute a
      -- concrete C++ identifier (terminal var, parameterised template,
      -- record name); @Nothing@ for shapes that don't.
      bodyCV :: TypeU -> Maybe Text
      bodyCV body = case body of
        VarU (TV n)              -> Just n
        AppU (VarU (TV n)) _     -> Just n
        NamU _ (TV n) _ _        -> Just n
        _                        -> Nothing

      -- Shared diagnostic for every C++ leak path: recursive aliases
      -- whose morloc-side name would otherwise reach the generated pool
      -- as an undeclared identifier. Reached from the inline VarF/AppF
      -- guards (where pairEval bnd-protection leaks @gv == cv@ and
      -- cscope confirms no legitimate mapping) and from the RecF rule
      -- (where the back-reference's TVar has no cscope entry).
      -- Recorded as a pool error, reported before any C++ is written.
      leakError :: Text -> CppTranslator MDoc
      leakError gv = do
        cscope <- CMS.gets translatorCScope
        let entries = Map.findWithDefault [] (TV gv) cscope
            generated = any (\(_, body, _, _, _) -> bodyCV body == Just "struct") entries
            -- A record with a form of its own reaches here only where its
            -- expansion was cut by name.
            userRecord = or [True | (_, NamU {}, _, _, _) <- entries]
            msg
              | userRecord && not generated = "the record " <> gv <> " " <> nestedSelfUseReason
              | generated =
                  "the record " <> gv <> " refers to itself, and its C++ definition cannot be"
                    <> " generated in its module's header; give it a C++ form, `record Cpp => "
                    <> gv <> " = \"<struct-name>\"`, naming a struct you write"
              | otherwise =
                  "Recursive type alias `" <> gv <> "` has no concrete C++ mapping. "
                    <> "Add either `type Cpp => " <> gv <> " ... = \"<cpp-template>\" ...` "
                    <> "(for tuple/list aliases mapped to a user-provided struct template) or "
                    <> "`record Cpp => " <> gv <> " = \"<struct-name>\"` "
                    <> "(for records), where the template/struct names a user-provided C++ type."
        CMS.modify $ \st -> st {translatorErrors = msg : translatorErrors st}
        return (pretty (tvarIdentifier (TV gv)))

  cppArgOf s (Arg i t) = do
    t' <- cppTypeOf (typeFof t)
    return $ setCallSemantics s t' <+> (nvarNamer cppLocals) i

data CppTranslatorState = CppTranslatorState
  { translatorCounter :: Int
  , translatorSignatureSet :: Set.Set Int
  , translatorLocalManifoldSet :: Set.Set Int
  , translatorRemoteManifoldSet :: Set.Set Int
  , translatorEffectLabels :: Map.Map Int (Set.Set Text)
  , translatorLogTemplates :: Map.Map Int RenderedTemplate
  -- ^ Per-labeled-midx pre-rendered log templates (see 'LogTemplate').
  -- The IIFE wrap is injected at the definition site so manifolds
  -- referenced by symbol (e.g. @std::bind(m1043, _1)@) also log.
  , translatorDataNames :: Set.Set TVar
  -- ^ The `data` types the program declares.
  , translatorVariantNames :: Set.Set TVar
  -- ^ The payload-bearing `data` types this pool DECLARES. A `RecF`
  -- back-reference naming one of these is legitimate and renders as that
  -- name; the leak diagnostic is for recursive ALIASES, which have no
  -- generated form and would reach the pool as a bare morloc identifier.
  , translatorSchemas :: Map.Map Text Int
  -- The merged C++ concrete scope (the same scope @pairEval@ consults).
  -- 'cppTypeOf' uses it as ground truth to distinguish two cases that
  -- otherwise produce identical-looking @FV gv (CV gv)@ FVars:
  --   1. legitimate user mapping @type Cpp => Foo = "Foo"@ (cscope has an
  --      entry resolving Foo to the C++ identifier "Foo");
  --   2. pairEval bnd-protection leaking the morloc TVar text in place of
  --      a concrete name (cscope has no Foo entry whose body's outer
  --      name matches "Foo").
  -- Without this disambiguation, the inline @gv == cv@ leak guard would
  -- false-positive on case 1.
  , translatorCScope :: Scope
  , translatorDebugInfo :: Int -> (Text, Text)
  -- ^ Snapshot of per-manifold @(name, srcloc)@ metadata (from
  -- 'makeManifoldDebugInfoLookup'); consumed by 'cppDebugWrap'.
  , translatorTraceInfo :: Int -> Maybe (Text, Text)
  -- ^ Snapshot of 'makeTraceLabelLookup': the trace label of a call of a
  -- named function, 'Nothing' for any other manifold (DIAG-5).
  , translatorDebugMode :: Bool
  -- ^ Snapshot of 'stateDebugTrace'. When True, DebugWrapS drains the
  -- traceback in 'cppDebugWrap'; the manifold-level catch in
  -- 'lcMakeFunction' skips its frame-line append to avoid duplicates.
  , translatorPool :: PoolPlan
  -- ^ The pool's ownership decisions; its takes are keyed by the top-level
  -- manifold and the let index, because a let index names a binding within
  -- a manifold and two manifolds may share one.
  , translatorCurrentRoot :: Int
  -- ^ The top-level manifold being lowered (see 'translatorPool').
  , translatorMoves :: Set.Set Int
  -- ^ The owned lets of the segment being lowered (OWN-2).
  , translatorParamMoves :: Set.Set Int
  -- ^ The parameters the segment's native manifolds take by value (OWN-7).
  , translatorLets :: Set.Set Int
  -- ^ Every let index of the segment being lowered.
  , translatorBuilds :: Set.Set (Int, Int)
  -- ^ The (manifold, local) pairs whose building is the local's last read.
  , translatorMovedBuilds :: Set.Set (Int, Int)
  -- ^ The pairs among them the segment's code moves.
  , translatorLowered :: [(SerialManifold, [(Int, NativeExpr, LetMode)], Set.Set Int, Set.Set (Int, Int))]
  -- ^ Each segment as lowered, last reads marked, with its let modes, the
  -- lets and parameters a last read moves, and the (manifold, local) pairs
  -- its code moves into a manifold as it is built.
  , translatorErrors :: [Text]
  , translatorTypeHomes :: Map.Map TVar TypeHome
  -- ^ The declaring module and name of every declared type (GEN-4).
  , translatorPublic :: Map.Map TVar Text
  -- ^ The generated types this pool declares in their module's header,
  -- each with its qualified name (GEN-6).
  , translatorHandles :: Map.Map TVar [(Key, TypeF)]
  -- ^ The generated records that refer to themselves, each a handle to its
  -- shared fields (GEN-1, CPP-6), with its fields.
  , translatorGenDefs :: Map.Map TVar GenDef
  -- ^ Each generated record's definition from its declaration (GEN-9).
  , translatorDataDefs :: Map.Map TVar DataDef
  -- ^ Each generated parameterized `data` type's definition (GEN-9).
  , translatorDataFailures :: Map.Map TVar Text
  -- ^ Why a generated parameterized `data` type has no definition.
  , translatorIntrinsicAsts :: [SerialAST]
  -- ^ The serial forms of the values the pool's intrinsics marshal.
  , translatorGenFailures :: Map.Map TVar Text
  -- ^ Why a used record has no definition, for each that has none.
  , translatorRecordUses :: [RecordUse]
  -- ^ Every occurrence of a generated record in the pool.
  , translatorRecords :: Map.Map TVar GenDef
  -- ^ The generated records of the pool, each defined from its declaration
  -- (GEN-9).
  , translatorRecordNames :: Map.Map TVar Text
  -- ^ The qualified C++ name of each: in its module's header, or in the
  -- pool (GEN-10).
  }

instance Defaultable CppTranslatorState where
  defaultValue =
    CppTranslatorState
      { translatorCounter = 0
      , translatorSignatureSet = Set.empty
      , translatorLocalManifoldSet = Set.empty
      , translatorRemoteManifoldSet = Set.empty
      , translatorEffectLabels = Map.empty
      , translatorLogTemplates = Map.empty
      , translatorVariantNames = Set.empty
      , translatorDataNames = Set.empty
      , translatorSchemas = Map.empty
      , translatorCScope = Map.empty
      , translatorDebugInfo = \_ -> ("", "")
      , translatorTraceInfo = \_ -> Nothing
      , translatorDebugMode = False
      , translatorPool = emptyPoolPlan
      , translatorCurrentRoot = 0
      , translatorMoves = Set.empty
      , translatorParamMoves = Set.empty
      , translatorLets = Set.empty
      , translatorBuilds = Set.empty
      , translatorMovedBuilds = Set.empty
      , translatorLowered = []
      , translatorErrors = []
      , translatorTypeHomes = Map.empty
      , translatorPublic = Map.empty
      , translatorHandles = Map.empty
      , translatorGenDefs = Map.empty
      , translatorDataDefs = Map.empty
      , translatorDataFailures = Map.empty
      , translatorIntrinsicAsts = []
      , translatorGenFailures = Map.empty
      , translatorRecordUses = []
      , translatorRecords = Map.empty
      , translatorRecordNames = Map.empty
      }

type CppTranslator a = CMS.StateT CppTranslatorState Identity a
type CppTranslatorM = CMS.StateT CppTranslatorState Identity

getCounter :: CppTranslator Int
getCounter = do
  s <- CMS.get
  let i = translatorCounter s
  CMS.put $ s {translatorCounter = translatorCounter s + 1}
  return i

resetCounter :: CppTranslator ()
resetCounter = do
  s <- CMS.get
  CMS.put $ s {translatorCounter = 0}

cppRegisterSchema :: Text -> CppTranslator Int
cppRegisterSchema schema = do
  s <- CMS.get
  case Map.lookup schema (translatorSchemas s) of
    Just sid -> return sid
    Nothing -> do
      let sid = Map.size (translatorSchemas s)
      CMS.put $ s {translatorSchemas = Map.insert schema sid (translatorSchemas s)}
      return sid

getCppSchemaTable :: CppTranslator [Text]
getCppSchemaTable = do
  m <- CMS.gets translatorSchemas
  return $ map fst $ sortBy (comparing snd) $ Map.toList m

translate :: [Source] -> [SerialManifold] -> MorlocMonad Script
translate srcs es = do
  universalScopeMap0 <- MM.gets stateConcreteTypedefs

  -- General-scope aliases (e.g. `type Timestamp = Int64`) must chain into
  -- the concrete cpp scope so that a record field declared with a general
  -- alias resolves through its concrete mapping (`Cpp => Int64 = "int64_t"`).
  -- Without this merge, `evaluateType` against the cpp scope alone leaves
  -- `Timestamp` un-expanded and the printer emits the morloc alias name
  -- literally into pool.cpp. Concrete entries take precedence on collision.
  generalScope <- MM.gets stateGeneralTypedefs
  let cppScope = fromMaybe Map.empty (Map.lookup cppLang universalScopeMap0)
      mergedCppScope = Map.union cppScope generalScope
      universalScopeMap = Map.insert cppLang mergedCppScope universalScopeMap0

  effectMap <- MM.gets stateManifoldEffects
  nativeEntries <- Set.fromList . Map.elems <$> MM.gets stateNativeRecEntries

  -- Canonicalize C++ source paths once up front so that the #include
  -- directives emitted by makeCppCode and the -I flags emitted by
  -- makeTheMaker see exactly the same absolute paths. Before this,
  -- `#include "./src/foo.hpp"` could not be resolved against
  -- `-I/abs/src` because the `src/` prefix was duplicated.
  (srcs', cxxFlags, includeDirs) <- handleFlagsAndPaths srcs

  checkOn <- maybe False (`notElem` ["", "0"]) <$> liftIO (lookupEnv "MORLOC_CHECK_OWNERSHIP")
  labels <- collectLogLabels <$> MM.gets stateManifoldConfig
  templates <- collectRenderedTemplates cppLang

  debugInfo <- makeManifoldDebugInfoLookup
  traceInfo <- makeTraceLabelLookup
  debugMode <- MM.gets stateDebugTrace
  closureTable <- computeClosureSchemas cppLang es
  typeHomes <- MM.gets stateTypeHomes
  stageTable <- stageTableEntries
  let -- Closures' captured and bound arguments are deserialized by their
      -- dispatch wrappers, not by any manifold, so their types are added.
      poolTypes = concatMap (poolTypesWith serialAstToType) es <> map (unpackedAsTypeF . serialAstToType) (closureSerialASTs closureTable)
      uses = recordUses poolTypes
  (gdefs, gfails) <- genDefs cppLang (Set.fromList [g | (g, _, _, _) <- uses])
  (ddefs, dfails) <- dataDefs cppLang (dataNamesIn poolTypes)
  intrAsts <- inPoolSerialASTs cppLang es
  gscope <- MM.getGeneralScope
  let translatorState = defaultValue
        { translatorEffectLabels = effectMap
        , translatorDataNames = declaredDataNames gscope
        , translatorLogTemplates = templates
        , translatorCScope = mergedCppScope
        , translatorDebugInfo = debugInfo
        , translatorTraceInfo = traceInfo
        , translatorDebugMode = debugMode
        , translatorPool = pool
        , translatorTypeHomes = typeHomes
        , translatorGenDefs = gdefs
        , translatorDataDefs = ddefs
        , translatorDataFailures = dfails
        , translatorIntrinsicAsts = intrAsts
        , translatorGenFailures = gfails
        , translatorRecordUses = uses
        }
      pool = planPool cppCaps es
      ((code, headers), stEnd) = CMS.runState (makeCppCode labels srcs' es universalScopeMap closureTable stageTable nativeEntries) translatorState
  mapM_ (MM.throwRejected . ("C++ pool:" <+>) . pretty) (translatorErrors stEnd)
  when checkOn $
    case concatMap (checkOne pool) (translatorLowered stEnd) of
      [] -> return ()
      vs -> MM.throwCompilerBug ("C++ pool: ownership check failed (OWN-1):" <+> vsep (map pretty vs))

  -- The generated headers' directory is searched first, so that a stale
  -- copy of one in a source directory never shadows it.
  maker <- makeTheMaker cxxFlags ["pools" </> "types" </> "cpp" | not (null headers)] includeDirs

  let poolSubdir = ML.poolDirKey cppLang
      privateNote = poolPrivateNote [unTVar gv | gv <- Map.keys (translatorRecords stEnd), Map.notMember gv (translatorPublic stEnd)]

  return $
    Script
      { scriptBase = "pool"
      , scriptLang = cppLang
      , scriptCode = "." :/ Dir "pools" ([Dir poolSubdir
          [ File "pool.cpp" (Code (subVersion (render code)))
          , File "pool_host.cpp" (Code (subVersion (DF.embededFileText (DF.poolHostTemplate "cpp"))))
          ]] <> [Dir "types" [Dir "cpp" (typeFileTree ".hpp" headers <> privateNote)] | not (null headers && null privateNote)]
          )
      , scriptMake = maker
      }

-- Substitute the version placeholder shared by the member and host templates.
subVersion :: Text -> Text
subVersion = T.replace "__MORLOC_VERSION__" (MT.pack MV.versionStr)

makeCppCode ::
  Map.Map Int Text ->
  [Source] ->
  [SerialManifold] ->
  Map.Map Lang Scope ->
  Map.Map Int ([SerialAST], [SerialAST], SerialAST) ->
  Map.Map Int StageEntry ->
  Set.Set Int ->
  CppTranslator (MDoc, [([Text], Text)])
makeCppCode labels srcs es univeralScopeMap closureTable0 stageTable nativeEntries = do
  -- Seeded before any type is rendered: 'cppTypeOf' consults it to tell a
  -- back-reference into a generated `data` type from an unmapped alias.
  CMS.modify $ \st -> st
    { translatorVariantNames = Set.fromList [gv | (FV gv _, _, _) <- collectCppVariants es] }
  headers <- planPublicTypes es
  templates <- CMS.gets translatorLogTemplates
  (srcFwds, srcSerial, srcDeserial) <- generateSourcedSerializers univeralScopeMap es

  -- write include statements for sources
  -- The generated headers come first, so a sourced file may name their
  -- types (GEN-6).
  let includeDocs =
        ["#include" <+> dquotes (pretty (cppHeaderPath p)) | (p, _) <- headers]
          <> map translateSource (unique . mapMaybe srcPath $ srcs)

  signatures0 <- concat <$> mapM (makeSignature nativeEntries) es

  -- Only closures that cross a boundary get reify thunks and dispatch wrappers.
  closureTable <- crossingClosures closureCppSig stageTable es closureTable0
  (closureWrappers, reifyThunks) <- makeClosureDispatch closureTable es
  papplyHeads <- papplyHeadSigs closureCppSig es
  let closureGen = ClosureGen reifyThunks stageTable papplyHeads

  intrAsts <- CMS.gets translatorIntrinsicAsts
  (autoDecl, autoFwds, autoLate) <- generateRecords (cppLowerConfig closureGen) (concatMap allSerialASTs es <> intrAsts <> closureSerialASTs closureTable0)
  (varWrappers, varArms, varFwds, varSerial, varLate) <-
    generateCppVariants (cppLowerConfig closureGen) closureTable0 es
  -- Converting marshallers follow the template's closure machinery.
  let signatures = stageLookupDoc stageTable : varLate <> autoLate <> signatures0
  enumDecl <- generateCppEnums es
  valueChecks <- cppValueChecks es
  -- Declaration order is forced by what holds what by value. An enum is a
  -- byte, so it leads. A variant's wrapper holds its arms through pointers,
  -- so every wrapper is complete before any arm body or record exists; a
  -- generated record may then hold a wrapper by value, and an arm body may
  -- hold a generated record by value, so records come before arm bodies.
  -- Every marshaller SIGNATURE follows before any definition, so a
  -- definition that appears later is visible to an earlier call rather
  -- than leaving it to bind to the header's raw-bytes fallback.
  let serializationCode =
        enumDecl ++ varWrappers ++ autoDecl ++ varArms
          ++ varFwds ++ autoFwds ++ srcFwds
          ++ varSerial ++ srcSerial ++ srcDeserial
          ++ valueChecks

  -- build the program (translates each manifold tree)
  program <- buildProgramM labels templates includeDocs [] es (translateSegment closureGen) getCppSchemaTable (Map.map closureSchemaTexts closureTable)

  -- create and return complete pool script
  return (CP.printProgram serializationCode signatures closureWrappers program, headers)

-- | A closure's C++ callable signature "Ret(Arg...)" -- the same string
-- 'lcClosureSig' produces and 'MorlocClosure<Sig>' is keyed on. Rendered from
-- the bound (remaining) argument types and the result type.
closureCppSig :: [TypeF] -> TypeF -> CppTranslator Text
closureCppSig ins out = do
  rt <- cppTypeOf out
  ats <- mapM cppTypeOf ins
  return (render (rt <> tupled ats))

-- | For each defunctionalized closure manifold, emit a serial dispatch wrapper
-- so the home pool can apply the closure when a foreign pool calls back on its
-- manifold id: deserialize the captured ++ bound argument packets, call the
-- native closure body, and serialize the result. The argument/result schemas
-- are the ones 'computeClosureSchemas' already computed (threaded in via
-- @closureTable@); their C++ types come from the manifold's own form.
-- Produce the serial dispatch wrappers for cross-registrable closures AND, per
-- closure, the reify thunk that serializes its captured native values into the
-- closure wire form. The thunk is keyed by manifold name so 'lcMakeLambda' can
-- splice it into that closure's 'MorlocClosure' constructor.
makeClosureDispatch ::
  Map.Map Int ([SerialAST], [SerialAST], SerialAST) -> [SerialManifold] -> CppTranslator ([MDoc], Map.Map Text (MDoc, MDoc))
makeClosureDispatch closureTable es = do
  results <- mapM one (filter inTable (concatMap collectClosureManifolds es))
  return (map fst results, Map.fromList (map snd results))
  where
    -- Only closures the schema pass kept (all-native captured context) get a
    -- dispatch wrapper; the rest are handled by the pre-existing serial path.
    inTable (NativeManifold i _ _ _) = Map.member i closureTable
    one (NativeManifold i _ form _) = do
      let ctxTs = [t | Arg _ o <- manifoldContext form, Just t <- [orNativeType o]]
          bndTs = [t | Arg _ t <- manifoldBound form]
          ctxNames = map (argNamer cppLocals) (typeMofRs (manifoldContext form))
          -- inTable already guaranteed membership, so this key is present.
          (capAsts, bndAsts, resAst) = closureTable Map.! i
      argTypes <- mapM cppTypeOf (ctxTs <> bndTs)
      getVals <- mapM
        (\(j, (t, ast)) -> cppDecode ("args[" <> pretty j <> "]") t ast)
        (indexed (zipSame argTypes (capAsts <> bndAsts)))
      let call = manNamer i <> tupled getVals
      putRes <- cppEncode call resAst
      reifier <- closureReifyThunk ctxNames capAsts
      bndSids <- mapM (cppRegisterSchema . render . serialAstToMsgpackSchema) bndAsts
      let bndSchemas = "std::vector<Schema*>" <> encloseSep "{" "}" ", " ["mlc_schema_table[" <> pretty j <> "]" | j <- bndSids]
      let dispatchDoc =
            [idoc|uint8_t* mlc_closure_dispatch_#{pretty i}(const uint8_t** args) {
    return #{putRes};
}|]
      return (dispatchDoc, (render (manNamer i), (reifier, bndSchemas)))

-- The reify thunk stored inside a 'MorlocClosure'. When the closure crosses a
-- language boundary it serializes each captured native value (by value, so the
-- thunk owns a copy) into a self-contained wire packet via '_mlc_reify_capture';
-- a captured closure is reified first. Empty capture yields an empty vector.
-- Built once per closure by 'makeClosureDispatch' and looked up by
-- 'lcMakeLambda'.
closureReifyThunk :: [MDoc] -> [SerialAST] -> CppTranslator MDoc
closureReifyThunk names asts
  | null names = return "[]() { return std::vector<std::vector<uint8_t>>{}; }"
  | otherwise = do
      caps <- mapM cap (zipSame names asts)
      return
        [idoc|[#{cat (punctuate ", " names)}]() -> std::vector<std::vector<uint8_t>> {
    return { #{cat (punctuate ", " caps)} };
}|]
  where
    cap (nm, ast@(SerialClosure _ _)) = do
      sid <- cppRegisterSchema (render (serialAstToMsgpackSchema ast))
      return [idoc|_mlc_reify_capture(_mlc_reify(#{nm}), mlc_schema_table[#{pretty sid}])|]
    cap (nm, ast) = do
      sid <- cppRegisterSchema (render (serialAstToMsgpackSchema ast))
      return [idoc|_mlc_reify_capture(#{nm}, mlc_schema_table[#{pretty sid}])|]

-- | Serialize a native value by its wire form: a closure is reified first.
cppEncode :: MDoc -> SerialAST -> CppTranslator MDoc
cppEncode v ast = do
  sid <- cppRegisterSchema (render (serialAstToMsgpackSchema ast))
  return $ case ast of
    SerialClosure _ _ -> "_put_value(_mlc_reify(" <> v <> "), mlc_schema_table[" <> pretty sid <> "])"
    _ -> "_put_value(" <> v <> ", mlc_schema_table[" <> pretty sid <> "])"

-- | Deserialize a packet by its wire form into a value of the given C++
-- type: a closure is reflected into a proxy that calls back to its home.
cppDecode :: MDoc -> MDoc -> SerialAST -> CppTranslator MDoc
cppDecode pkt typeStr ast = do
  sid <- cppRegisterSchema (render (serialAstToMsgpackSchema ast))
  case ast of
    SerialClosure ins out -> do
      let cloInit =
            "_get_value<" <> pretty cppClosureWireTupleName <> ">(" <> pkt
              <> ", mlc_schema_table[" <> pretty sid <> "])"
      cppClosureProxyLambda cloInit ins out
    _ -> return $ "_get_value<" <> typeStr <> ">(" <> pkt <> ", mlc_schema_table[" <> pretty sid <> "])"

-- | Collect TVar names of all named (non-anonymous) record types used
-- in a SerialManifold tree.
collectNamedRecordTVars :: SerialManifold -> Set.Set TVar
collectNamedRecordTVars e0 =
  runIdentity $ foldWithSerialManifoldM fm e0
  where
    fm = defaultValue
      { opFoldWithNativeExprM = nativeExpr
      , opFoldWithSerialExprM = serialExpr
      }

    nativeExpr _ (DeserializeN_ t s xs) =
      return $ Set.unions [xs, seekNamedRecs t, seekNamedRecs (unpackedAsTypeF (serialAstToType s))]
    nativeExpr efull e =
      return $ foldlNE Set.union (seekNamedRecs (typeFof efull)) e

    serialExpr _ (SerializeS_ s xs) =
      return $ Set.union (seekNamedRecs (unpackedAsTypeF (serialAstToType s))) xs
    serialExpr _ e = return $ foldlSE Set.union Set.empty e

    seekNamedRecs :: TypeF -> Set.Set TVar
    seekNamedRecs (NamF _ (FV v (CV c)) _ rs)
      | c /= "struct" = Set.insert v (Set.unions (map (seekNamedRecs . snd) rs))
    seekNamedRecs (NamF _ _ _ rs) = Set.unions (map (seekNamedRecs . snd) rs)
    seekNamedRecs (FunF ts t) = Set.unions (map seekNamedRecs (t : ts))
    seekNamedRecs (AppF t ts) = Set.unions (map seekNamedRecs (t : tyArgs ts))
    seekNamedRecs (OptionalF t) = seekNamedRecs t
    -- A record held in a constructor's payload needs its marshaller as
    -- much as one held in a field.
    seekNamedRecs (VariantF (FV _ _) ps as) = Set.unions (map seekNamedRecs (tyArgs ps ++ concatMap snd as))
    seekNamedRecs _ = Set.empty

makeTheMaker :: [Text] -> [Path] -> [Path] -> MorlocMonad [SysCommand]
makeTheMaker flags generated includes = do
  let poolSubdir = ML.poolDirKey cppLang
  let outfile = pretty $ "pools" </> poolSubdir </> ML.makeExecutablePoolName cppLang
  let src = pretty $ "pools" </> poolSubdir </> ML.makeSourcePoolName cppLang
  -- The member-agnostic host translation unit (owns main()/pool_main); the C++
  -- member (pool.cpp) provides cpp_register. Compiled as a second TU and linked.
  let hostSrc = pretty $ "pools" </> poolSubdir </> "pool_host.cpp"

  bconf <- MM.gets stateBuildConfig
  let sanitizeFlags = case buildConfigSanitize bconf of
        Just True -> ["-fsanitize=alignment", "-fno-sanitize-recover=alignment"]
        _ -> []

  -- A source's directory is searched first for quoted includes and, for
  -- angle-bracket includes, only after the system directories: a header
  -- there may include a sibling either way, but a file named like a system
  -- header (a program called `tuple`) never replaces it.
  -- Paths are quoted: the command runs through a shell, and a project under
  -- a directory with spaces (iCloud Drive, Google Drive) is ordinary.
  let incs = concat [[pretty ("-iquote" <> MS.shellQuote i), pretty ("-idirafter" <> MS.shellQuote i)] | i <- generated <> ("." : includes)]
  let flags' = map pretty (flags ++ sanitizeFlags)

  let cmd =
        SysRun . Code . render $
          [idoc|${CXX:-g++} -O2 -o #{outfile} #{src} #{hostSrc} #{hsep flags'} #{hsep incs}|]

  return [SysRequireToolchain CppPoolToolchain, cmd]

-- | Forward declarations. Every manifold of the pool is declared, plus the
-- native entry of any recursive manifold: a nested manifold is otherwise
-- defined before the one that holds it, which is enough for a call from
-- inside, but a recursion's native entry is called from wherever the
-- recursion is reached.
makeSignature :: Set.Set Int -> SerialManifold -> CppTranslator [MDoc]
makeSignature nativeEntries = foldWithSerialManifoldM fm
  where
    fm =
      defaultValue
        { opFoldWithSerialManifoldM = serialManifold
        , opFoldWithNativeManifoldM = nativeManifold
        }

    serialManifold (SerialManifold m _ form _ _) folded = do
      own <- manifoldSignature m serialType form
      return (foldlSM (<>) own folded)

    -- The declared type is the body's, as the definition's is: a manifold
    -- that takes all of its arguments returns a value, not a callable.
    nativeManifold (NativeManifold m _ form body) folded
      | Set.member m nativeEntries = do
          typestr <- cppTypeOf (typeMof body)
          own <- manifoldSignature m typestr form
          return (foldlNM (<>) own folded)
      | otherwise = return (foldlNM (<>) [] folded)

    manifoldSignature ::
      (HasTypeM t) => Int -> MDoc -> ManifoldForm (Or TypeS TypeF) t -> CppTranslator [MDoc]
    manifoldSignature i typestr form = do
      s <- CMS.get
      if Set.member i (translatorSignatureSet s)
        then return []
        else do
          let formArgs = typeMofForm form

          args <- mapIndexedM (cppParamOf i) formArgs
          CMS.put (s {translatorSignatureSet = Set.insert i (translatorSignatureSet s)})
          return [typestr <+> manNamer i <> tupled args <> ";"]

tupleKey :: Int -> MDoc -> MDoc
tupleKey i v = [idoc|std::get<#{pretty i}>(#{v})|]

recordAccess :: MDoc -> MDoc -> MDoc
recordAccess record field = record <> "." <> field

-- | The ownership check (OWN-1) of one marked top-level manifold, given how
-- the pool binds each of its lets.
checkOne :: PoolPlan -> (SerialManifold, [(Int, NativeExpr, LetMode)], Set.Set Int, Set.Set (Int, Int)) -> [Text]
checkOne pp (sm, modes, moves, moved) = checkManifold mode (ocTagAfterTake cppCaps) moves (\m -> Map.findWithDefault Set.empty m (ppByValue pp)) moved sm
  where
    byIndex = Map.fromListWith (\_ old -> old) [(i, m) | (i, _, m) <- modes]
    mode i _ = Map.findWithDefault LetCopy i byIndex

-- | How let @i@ with right-hand side @rhs@ binds in C++: 'letMode', except
-- that a scalar is always copied, as a reference to one saves nothing.
cppLetMode :: Int -> NativeExpr -> CppTranslatorM LetMode
cppLetMode i rhs = do
  pp <- CMS.gets translatorPool
  root <- CMS.gets translatorCurrentRoot
  case letMode pp root i rhs of
    LetCopy -> return LetCopy
    mode -> do
      typestr <- cppTypeOf (typeFof rhs)
      return (if isScalarCppType typestr then LetCopy else mode)

-- | A kept element (OWN-2).
cppKeepElem :: NativeExpr -> MDoc -> CppTranslatorM MDoc
cppKeepElem e x = fromMaybe x <$> cppKeptLet e x

-- | Whether a value of a type is not worth moving: a scalar or the unit.
cppNoMove :: TypeF -> CppTranslatorM Bool
cppNoMove t = do
  typestr <- cppTypeOf t
  return (isScalarCppType typestr || render typestr == "mlc::Unit")

-- | The value a native loop ends with (OWN-2): one of its carried locals at
-- its last read is moved.
cppKeepSlot :: [Int] -> NativeExpr -> MDoc -> CppTranslatorM MDoc
cppKeepSlot ids e x = case e of
  LetVarN t j LastRead -> slot t j
  BndVarN t j LastRead -> slot t j
  _ -> return x
  where
    slot t j
      | j `elem` ids && render x == render ((nvarNamer cppLocals) j) = do
          cheap <- cppNoMove t
          return (if cheap then x else "std::move(" <> x <> ")")
      | otherwise = return x

-- | The move of a kept let (OWN-2), when its lowered form is the let itself.
cppKeptLet :: NativeExpr -> MDoc -> CppTranslatorM (Maybe MDoc)
cppKeptLet (LetVarN t i LastRead) x
  | render x == render ((nvarNamer cppLocals) i) = do
      moves <- CMS.gets translatorMoves
      if Set.member i moves
        then do
          cheap <- cppNoMove t
          return (if cheap then Nothing else Just ("std::move(" <> x <> ")"))
        else return Nothing
cppKeptLet (BndVarN t i LastRead) x
  | render x == render ((nvarNamer cppLocals) i) = do
      params <- CMS.gets translatorParamMoves
      if Set.member i params
        then do
          cheap <- cppNoMove t
          return (if cheap then Nothing else Just ("std::move(" <> x <> ")"))
        else return Nothing
cppKeptLet _ _ = return Nothing

-- | An argument of a call of native manifold @m@: one it takes by value is
-- kept (OWN-7).
cppKeepCallArg :: Int -> Int -> NativeExpr -> MDoc -> CppTranslatorM MDoc
cppKeepCallArg m j e x = do
  pp <- CMS.gets translatorPool
  if Set.member j (Map.findWithDefault Set.empty m (ppByValue pp)) then cppKeepElem e x else return x

-- | Whether building manifold @m@ moves local @i@ of type @t@ into it: the
-- building is its last read and it is an owned let or by-value parameter
-- worth moving (OWN-3). A move is recorded for the ownership check.
cppMovesInto :: Int -> Int -> TypeF -> CppTranslatorM Bool
cppMovesInto m i t = do
  st <- CMS.get
  let owned
        | Set.member i (translatorLets st) = Set.member i (translatorMoves st)
        | otherwise = Set.member i (translatorParamMoves st)
  if Set.member (m, i) (translatorBuilds st) && owned
    then do
      cheap <- cppNoMove t
      unless cheap $ CMS.modify (\s -> s {translatorMovedBuilds = Set.insert (m, i) (translatorMovedBuilds s)})
      return (not cheap)
    else return False

-- | Context argument @j@, local @i@, of a fully applied call of manifold @m@:
-- moved into a parameter it takes by value (OWN-7).
cppKeepContextArg :: Int -> Int -> Int -> TypeF -> MDoc -> CppTranslatorM MDoc
cppKeepContextArg m j i t x = do
  pp <- CMS.gets translatorPool
  if Set.member j (Map.findWithDefault Set.empty m (ppByValue pp)) && not (isFunctionValue t) && render x == render ((nvarNamer cppLocals) i)
    then do
      moves <- cppMovesInto m i t
      return (if moves then "std::move(" <> x <> ")" else x)
    else return x

-- | The parameter at position @j@ of manifold @m@: one it takes by value is
-- declared as a value (OWN-7).
cppParamOf :: Int -> Int -> Arg TypeM -> CppTranslator MDoc
cppParamOf m j r@(Arg _ t) = do
  pp <- CMS.gets translatorPool
  byValue <- case t of
    Native tf
      | Set.member j (Map.findWithDefault Set.empty m (ppByValue pp))
      , not (isFunctionValue tf) -> not <$> cppNoMove tf
    _ -> return False
  cppArgOf (if byValue then Copy else chooseCallSemantics t) r

-- | A segment with the reads of the parameters manifold @m@ takes by value
-- (@seeds m@) written as reads of a parameter: such a parameter shares its
-- index with what its caller passes, which may be one of the caller's lets.
paramReads :: (Int -> [Int]) -> SerialManifold -> SerialManifold
paramReads seeds = mmap (defaultValue {mapNativeManifold = onNM})
  where
    onNM (NativeManifold m l f e) = case seeds m of
      [] -> NativeManifold m l f e
      ps -> NativeManifold m l f (asParams (Set.fromList ps) e)
    asParams ps = mgatedMap gate (defaultValue {mapNativeExpr = rd})
      where
        rd (LetVarN t i r) | Set.member i ps = BndVarN t i r
        rd x = x
    gate = defaultValue {gateNativeManifold = const False, gateSerialManifold = const False}

-- | The carried locals of every loop under a node.
loopSlots :: RNode -> Set.Set Int
loopSlots n = here n <> Set.unions (map loopSlots (rnodeChildren n))
  where
    here (RN (LoopN _ starts _)) = Set.fromList (map fst starts)
    here (RS (LoopS _ ids _)) = Set.fromList ids
    here _ = Set.empty

isFunctionValue :: TypeF -> Bool
isFunctionValue FunF {} = True
isFunctionValue _ = False

-- | Walk a lowered 'LoopBody' into C++ statement lines for the loop body.
-- Guards -> if/else; native/serial lets -> locals; a base leaf ->
-- @resultVar = <base>; break;@; a continue leaf -> compute each new value of
-- a slot it changes into an @auto@ temp, then move it into the slot. Each
-- let binds as its IR node decides.
cppWalkLoopBody :: (MDoc -> MDoc) -> LoopBody LowN LowS PoolDocs -> CppTranslatorM [MDoc]
cppWalkLoopBody setResult = go
  where
    go (LoopBase seDocs) =
      return $ poolPriorLines seDocs <> [setResult (poolExpr seDocs), "break;"]
    -- A slot whose new value is itself is left alone: copying it into a temp
    -- and back would copy the whole value every iteration.
    -- A let moved into its slot needs no temp, as no slot reassignment
    -- changes it.
    go (LoopContinue xs) = do
      let changed = changedSlots [(i, o, d) | (i, Low o d) <- xs]
      moved <- mapM (\(_, cd, oe) -> cppKeptLet oe (poolExpr cd)) changed
      let viaTemp = [(i, cd) | ((i, cd, _), Nothing) <- zipSame changed moved]
          direct = [(i, x) | ((i, _, _), Just x) <- zipSame changed moved]
      tmpVars <- map (helperNamer cppLocals) <$> mapM (const getCounter) viaTemp
      let priors = concatMap (\(_, cd, _) -> poolPriorLines cd) changed
          tmpAssigns = zipWithSame (\tv (_, cd) -> "auto" <+> tv <+> "=" <+> poolExpr cd <> ";") tmpVars viaTemp
          reassigns = zipWithSame (\(i, _) tv -> (nvarNamer cppLocals) i <+> "= std::move(" <> tv <> ");") viaTemp tmpVars
          moves = [(nvarNamer cppLocals) i <+> "=" <+> x <> ";" | (i, x) <- direct]
      return $ priors <> tmpAssigns <> reassigns <> moves
    go (LoopNLet i (Low r neDocs) b) = do
      mode <- cppLetMode i r
      -- A function value keeps its own type; a std::function would allocate.
      typestr <- if isFunctionValue (typeFof r) then return "auto" else cppTypeOf (typeFof r)
      let decl = if mode == LetBorrow then "const" <+> typestr <> "&" else typestr
          letLine
            | render typestr == "mlc::Unit" && isIdentifierExpr (poolExpr neDocs) = [typestr <+> (nvarNamer cppLocals) i <> "{};"]
            | render typestr == "mlc::Unit" = [poolExpr neDocs <> ";", typestr <+> (nvarNamer cppLocals) i <> "{};"]
            | otherwise = [decl <+> (nvarNamer cppLocals) i <+> "=" <+> poolExpr neDocs <> ";"]
      rest <- go b
      return $ poolPriorLines neDocs <> letLine <> rest
    go (LoopSLet i (Low _ seDocs) b) = do
      rest <- go b
      return $ poolPriorLines seDocs <> ["auto" <+> (svarNamer cppLocals) i <+> "=" <+> poolExpr seDocs <> ";"] <> rest
    go (LoopIf (Low _ guardDocs) t e) = do
      tLines <- go t
      eLines <- go e
      -- Every branch ends the iteration, so the else branch follows the
      -- guarded one rather than nesting in it (EMIT-5).
      let next = ["continue;" | not (loopBodyBreaks t)]
          ifBlock = vsep ["if" <+> parens (poolExpr guardDocs) <+> "{", indent 4 (vsep (tLines <> next)), "}"]
      return $ poolPriorLines guardDocs <> [ifBlock] <> eLines

-- | What lowering a closure needs: per crossing closure manifold name, its
-- reify thunk and its bound arguments' schemas; the stage table; and the
-- signatures of the function values this pool partially applies.
data ClosureGen = ClosureGen (Map.Map Text (MDoc, MDoc)) (Map.Map Int StageEntry) (Set.Set Text)

-- | A constructor field a let takes ('consumableProjectionLets') out of its
-- subject's arm (OWN-5, CPP-6).
cppTakeField :: NativeExpr -> MDoc
cppTakeField (IntrinsicN _ IntrCtorField _ [LetVarN t j _, StrN _ n, IntN _ i])
  | VariantF _ _ arms <- t =
      "mlc::take_field(std::get<" <> pretty (armIndex n (map fst arms)) <> ">("
        <> (nvarNamer cppLocals) j <> ".v), [](auto& a) -> auto& { return a.f" <> pretty i <> "; })"
cppTakeField e = error $ "cppTakeField: not a constructor field of a let: " <> show e

-- | Operands that are all scalars reach only a built-in operator (CPP-1).
cppConstArg :: Maybe [TypeM] -> TypeM -> MDoc -> MDoc
cppConstArg operands _ x
  | Just ts <- operands, all isScalar ts = x
  | otherwise = "std::as_const" <> parens x
  where
    isScalar (Native (VarF (FV _ (CV c)))) = isScalarCppType (pretty c)
    isScalar (Serial (VarF (FV _ (CV c)))) = isScalarCppType (pretty c)
    isScalar _ = False

cppLowerConfig :: ClosureGen -> LowerConfig CppTranslatorM
cppLowerConfig (ClosureGen reifyThunks stageTable papplyHeads) =
  LowerConfig
    { lcLocalPrefix = cppLocals
    , lcSrcName = \src -> pretty (srcName src)
    , lcOperator = infixOperator
    , lcApplySrcGroup = \f as -> f <+> tupled as
    , lcSourcedResult = \_ _ _ x -> x
    , lcSourcedArg = \_ _ _ x -> x
    , lcConstArg = cppConstArg
    , lcOwnership = \_ -> return Owned
    , lcArgManifoldOwnership = \_ -> return Owned
    , lcBindCallArgs = False
    , lcOwnArg = \_ _ x -> x
    , lcReadArg = \_ x -> x
    , lcWithCallerScope = id
    -- `?T` is `std::optional<T>` at every value position where sizeof(T) is
    -- known (cppmorloc.hpp), which is exactly where a value-level
    -- CoerceToOptional lands (record fields use `wrap_field`, not this). The
    -- coerced value must therefore BE a std::optional, not just implicitly
    -- convertible to one: the msgpack-leaf serialize path passes the value
    -- straight to `to_voidstar<T>`, whose T is template-deduced from the
    -- expression, so a bare `T` would serialize against a `?`-schema without the
    -- optional layer (a compound inner then hits "compound schema reached a
    -- scalar-sized type" at runtime). `make_optional` deduces T from the value.
    -- Each arm is its own struct (two arms with the same field types must
    -- stay distinguishable), so a value is that struct braced-initialised
    -- and implicitly converted into the variant.
    -- Each arm lives behind a `std::shared_ptr` inside the variant, made by
    -- `mlc::make_arm` so that releasing a deep chain takes bounded stack
    -- (STATE-8), for generated and user-mapped types alike. That gives a recursive arm
    -- a finite size and lets an arm holding another `data` type need only
    -- a forward declaration, so declaration order between two variants
    -- stops mattering. The variant is built from a fresh shared_ptr, which
    -- converts to exactly one alternative, and is read by position, which
    -- is the wire tag for generated and user-mapped types alike.
    , lcVariantLit = \ty n _ xs ->
        let arm = CP.armName ty n
        in ty <> "{mlc::make_arm<" <> arm <> ">"
             <> parens (arm <> encloseSep "{" "}" ", " xs) <> "}"
    , lcEnumLit = \ty _ n _ -> ty <> "::" <> pretty n
    , lcVariantTagTest = \_ _ tag subj ->
        parens (parens subj <> ".v.index() ==" <+> pretty tag)
    , lcCtorField = \_ _ _ tag i subj ->
        "mlc::arm(std::get<" <> pretty tag <> ">" <> parens (parens subj <> ".v") <> ")->f" <> pretty i
    , lcEnumTagTest = \ty _ n _ subj ->
        parens (subj <+> "==" <+> ty <> "::" <> pretty n)
    , lcListLenTest = \subj n -> parens (parens subj <> ".size() ==" <+> pretty n)
    , lcListElem = \_ subj i -> parens subj <> "[" <> pretty i <> "]"
    , lcCoerceOptional = \x -> "std::make_optional(" <> x <> ")"
    , lcTypeOf = \t -> Just . toIType <$> cppTypeOf t
    , lcSerialAstType = serializeTypeOf
    , lcDeserialAstType = \s -> Just . toIType <$> cppTypeOf (shallowType s)
    , lcRawDeserialAstType = rawTypeOf
    , lcTypeMOf = \_ -> return Nothing
    , lcPackerName = \src -> pretty (srcName src)
    , lcUnpackerName = \src -> pretty (srcName src)
    , lcBorrowPackArg = \_ -> False
    , lcRecordAccessor = \_ fv record field -> do
        handles <- CMS.gets translatorHandles
        return $ case fv of
          FV g _ | Map.member g handles -> record <> "->" <> field
          _ -> recordAccess record field
    , lcDeserialRecordAccessor = \i _ v -> tupleKey i v
    , lcTupleAccessor = tupleKey
    , lcNewIndex = getCounter
    , lcPrintExpr = CP.printExpr
    , lcPrintStmt = CP.printStmt
    , lcTakeGetter = \_ _ _ -> return Nothing
    , lcEvalPattern = \t argTypes app -> do
        state <- CMS.get
        -- A getter's receiver is its last argument, after any bracket bounds.
        let rt = case app of
              PatGet {} | not (null argTypes) -> Just (last argTypes)
              _ -> listToMaybe argTypes
        return $ evaluatePattern state t rt app
    , lcTakeSetter = cppTakeSetter
    , lcListConstructor = \listT _ _ es -> do
        ty <- cppTypeOf listT
        return $ case es of
          [] -> ty <> "{}"
          _ -> "mlc::list_of<" <> ty <> ">" <> encloseSep "{" "}" ", " es <> ".value"
    , lcTupleConstructor = (<>) "std::make_tuple" . tupled
    , lcRecordConstructor = \recType _ _ _ rs -> do
        t <- cppTypeOf recType
        let -- For a field at a recursive-cycle position the field's
            -- C++ type is a pointer-shape (default std::shared_ptr<T>)
            -- while the value in hand is a bare T (or std::nullopt
            -- when the source was Null). Wrap through
            -- mlc::wrap_field<decltype(<rec>::<field>)> so the field's
            -- declared C++ type drives the right pointer construction.
            -- Other fields pass through unchanged: aggregate init from
            -- the value's natural shape works as before, and
            -- braced-init-list values (e.g. `{sub, sub}` for a vector
            -- field) keep working because they're never funneled
            -- through wrap_field, which would otherwise fail template
            -- argument deduction on a braced-init-list.
            (selfName, fieldTypes) = case recType of
              NamF _ vname _ fs -> (Just vname, fs)
              _ -> (Nothing, [])
            -- Inner is a back-reference to the enclosing record (the
            -- one we are presently constructing). Each child NativeExpr
            -- in a RecordN carries the value's type post-coercion: a
            -- `?T` field receives a value of TypeF
            -- `OptionalF <T-expanded-as-NamF-or-VarF-or-RecF>`, and the
            -- self-recursion shows up as the inner matching the
            -- enclosing record's name.
            isSelfRef self (VarF v) = v == self
            isSelfRef self (NamF _ v _ _) = v == self
            isSelfRef self (RecF v _) = v == self
            isSelfRef _ _ = False
            needsWrap (OptionalF inner) = case selfName of
              Just s -> isSelfRef s inner
              Nothing -> False
            needsWrap _ = False
            wrapField (k, v) =
              case lookup k fieldTypes of
                Just ft | needsWrap ft ->
                  "mlc::wrap_field<decltype(" <> t <> "::" <> pretty k <> ")>(" <> v <> ")"
                _ -> v
        handles <- CMS.gets translatorHandles
        return $ defaultValue
          { poolExpr = case selfName of
              Just (FV g _) | Map.member g handles -> cppHandleOf t (map snd rs)
              _ -> t <> encloseSep "{" "}" "," (map wrapField rs)
          }
    , lcKeepCallArg = cppKeepCallArg
    , lcKeepClosureArg = cppKeptLet
    , lcKeepContextArg = cppKeepContextArg
    , lcKeepSlot = cppKeepSlot
    , lcKeepElem = cppKeepElem
    , lcStoreField = \_ v -> return v
    , lcApplyClosure = \callee args -> callee <> tupled args
    , lcPapply = \t argTypes f xs -> case t of
        FunF rins out -> do
          argTs <- mapM (cppTypeOf . typeMToF) argTypes
          restTs <- mapM cppTypeOf rins
          resT <- cppTypeOf out
          -- each application typed explicitly: the head may be a bind
          -- expression, which deduction cannot see through
          let step ts acc x = "mlc_apply1<" <> hsep (punctuate "," (resT : ts ++ restTs)) <> ">" <> tupled [acc, x]
          return (papplySteps step argTs f xs)
        _ -> return f
    , lcForeignCall = \socketFile mid args ->
        let argList = [dquotes socketFile, pretty mid] <> args <> ["NULL"]
         in [idoc|foreign_call#{tupled argList}|]
    , lcCacheBody = cppCacheBody
    , lcDebugWrap = cppDebugWrap
    , lcRemoteCall = \socketFile mid res args -> do
        let (rmem, rtime, rcpu, rgpu) = remoteResourceInts res
            (resMem, resTime, resCPU, resGPU) = (pretty rmem, pretty rtime, pretty rcpu, pretty rgpu)
            cacheDir = ".morloc-cache"
            argList = encloseSep "{" "}" "," args
            setup =
              [idoc|resources_t resources = {#{resMem}, #{resTime}, #{resCPU}, #{resGPU}};
const uint8_t* args[] = #{argList};
char* errmsg = NULL;|]
            call =
              [idoc|remote_call(
    #{pretty mid},
    #{dquotes socketFile},
    #{dquotes cacheDir},
    &resources,
    args,
    #{pretty (length args)},
    &errmsg
);
PROPAGATE_ERROR(errmsg)|]
        return $ defaultValue {poolExpr = call, poolPriorLines = [setup]}
    , lcMakeLet = \namer letIndex mt mrhs e1 e2 -> do
        typestr <- case mt of
          (Just t) -> cppTypeOf t
          Nothing -> return serialType
        binding <- maybe (return LetCopy) (cppLetMode letIndex) mrhs
        let e1' = case (binding, mrhs) of
              (LetTake, Just rhs) -> e1 {poolExpr = cppTakeField rhs}
              _ -> e1
        return $ makeLet namer letIndex typestr (isUnitTypeF mt) binding e1' e2
    , lcReleaseStmt = \v -> "_release_packet(" <> pretty v <> ", true);"
    , lcReleaseBorrowedStmt = \v -> "_release_packet(" <> pretty v <> ", false);"
    , lcReturn = \e -> "return(" <> e <> ");"
    -- EMIT-3: the pop follows the result's construction, and the result is
    -- returned by name, so it is elided, moved or copied as it would be
    -- without the pop: a returned local is returned as it is, and any other
    -- result first initializes a local of the return type.
    , lcFrameReturn = \callIndex manifoldType byName e -> do
        state <- CMS.get
        case frameLineLit state callIndex of
          Nothing -> return Nothing
          Just _ -> do
            rt <- returnType manifoldType
            let pop = "mlc::frame_pop();"
            return . Just $ case () of
              _ | render rt == "void" -> vsep [e <> ";", pop, "return;"]
                | byName -> vsep [pop, "return(" <> e <> ");"]
                | otherwise -> vsep [rt <+> "_mlc_r =" <+> e <> ";", pop, "return(_mlc_r);"]
    , lcDupPacket = \e -> "_dup_packet(" <> e <> ")"
    , lcOwnedArg = \e -> "mlc::Packet(" <> e <> ").get()"
    , lcLoopBorrow = \_ _ -> return Nothing
    -- A consumable projection takes its payload, as 'makeLet' does for a
    -- let outside a loop.
    , lcLoopLetRhs = \i orhs d -> do
        mode <- cppLetMode i orhs
        return $
          if mode == LetTake
            then d {poolExpr = cppTakeField orhs}
            else d
    , lcLetRhs = \_ _ d -> return d
    , lcOwnPacketDecl = \v e -> Just ("mlc::Packet" <+> v <> "(" <> e <> ");", v <> ".get()")
    , lcMakeLoop = \res carried body -> do
        -- Tail-loop. Walk the 'LoopBody' tree into C++ control flow. The
        -- loop-carried vars are native locals ('nvarNamer id'), non-const,
        -- reassigned in place: the manifold's deserialized parameters in a
        -- serial loop, copies of its read-only parameters in a native one.
        -- Each continue value goes into an 'auto' temp before any loop-local
        -- is reassigned (parallel-assignment hazard), then MOVED in to avoid a
        -- per-iteration deep copy. A serial result inits to nullptr so a
        -- control path reaching the trailing return without a break is
        -- defined; a native one is an optional, since the value's type need
        -- not be default-constructible.
        resultIdx <- getCounter
        let resultVar = (helperNamer cppLocals) resultIdx
            ids = map fst carried
            initLines = concat [poolPriorLines d <> ["auto" <+> (nvarNamer cppLocals) i <+> "=" <+> poolExpr d <> ";"] | (i, Just d) <- carried]
        (resultDecl, setResult, resultExpr) <- case res of
          LoopResultSerial ->
            return ("uint8_t*" <+> resultVar <+> "= nullptr;", \e -> resultVar <+> "=" <+> e <> ";", resultVar)
          LoopResultNative t -> do
            tStr <- cppTypeOf t
            return ( "std::optional<" <> tStr <> ">" <+> resultVar <> ";"
                   , \e -> resultVar <> ".emplace(" <> e <> ");"
                   , "std::move(*" <> resultVar <> ")" )
        bodyLines <- cppWalkLoopBody setResult body
        let whileDoc = vsep ["while (true) {", indent 4 (vsep bodyLines), "}"]
            leaves = loopBodyDocs body <> [d | (_, Just d) <- carried]
        return $ PoolDocs
          { poolCompleteManifolds = concatMap poolCompleteManifolds leaves
          , poolExpr = resultExpr
          , poolPriorLines = initLines <> [resultDecl, whileDoc]
          , poolPriorExprs = concatMap poolPriorExprs leaves
          , poolReturnFlag = True
          , poolBorrowedLocal = Nothing
          , poolChain = Nothing
          }
    , lcShortCircuit = \isAnd a b -> a <+> (if isAnd then "&&" else "||") <+> b
    , lcChainLadder = \isAnd flag first rungs ->
        let undecided = if isAnd then flag else "!" <> flag
            rung (stmts, e) =
              vsep ["if" <+> parens undecided <+> "{", indent 4 (vsep (stmts <> [flag <+> "=" <+> e <> ";"])), "}"]
         in vsep (("bool" <+> flag <+> "=" <+> first <> ";") : map rung rungs)
    -- A chain of guards as one flat block (EMIT-5): a block run once, left
    -- by the first arm whose test holds. An arm is an expression; a loop's
    -- own exits are written only by the loop walkers, over a 'LoopBody'.
    , lcMakeIfLadder = \origExpr (Guards (c1, e1, t1) rungs (eE, eD)) -> do
        idx <- getCounter
        let v = (helperNamer cppLocals) idx
        typeStr <- cppTypeOf origExpr
        let unit = cppUnitResult origExpr
            taken c armE d = vsep ["if" <+> parens c <+> "{", indent 4 (vsep (cppIfArm unit v (Just armE) d <> ["break;"])), "}"]
            body =
              [taken c1 e1 t1]
                <> concat [pre <> [taken c armE d] | (pre, c, armE, d) <- toList rungs]
                <> cppIfArm unit v (Just eE) eD
        return (cppMoved unit True v, vsep [typeStr <+> v <> ";", "do {", indent 4 (vsep body), "} while (false);"])
    , lcMakeIf = \origExpr condDocs thenDocs elseDocs -> do
        idx <- getCounter
        let v = (helperNamer cppLocals) idx
        typeStr <- cppTypeOf origExpr
        let condE = poolExpr condDocs
            (thenE, elseE) = case origExpr of
              IfN _ _ tn en -> (Just tn, Just en)
              _ -> (Nothing, Nothing)
            unit = cppUnitResult origExpr
            thenBlock = cppIfArm unit v thenE thenDocs
            elseBlock = cppIfArm unit v elseE elseDocs
            decl = typeStr <+> v <> ";"
            ifStmt = vsep
              [ decl
              , "if" <+> parens condE <+> "{"
              , indent 4 (vsep thenBlock)
              , "} else {"
              , indent 4 (vsep elseBlock)
              , "}"
              ]
        return $ PoolDocs
          { poolCompleteManifolds = poolCompleteManifolds condDocs <> poolCompleteManifolds thenDocs <> poolCompleteManifolds elseDocs
          , poolExpr = cppMoved unit True v
          , poolPriorLines = poolPriorLines condDocs <> [ifStmt]
          , poolPriorExprs = poolPriorExprs condDocs <> poolPriorExprs thenDocs <> poolPriorExprs elseDocs
          , poolReturnFlag = poolReturnFlag condDocs || poolReturnFlag thenDocs || poolReturnFlag elseDocs
          , poolBorrowedLocal = Nothing
          , poolChain = Nothing
          }
    , lcMakeDoBlock = \t stmts expr ->
        let isUnit = case t of
              VarF (FV tv _) -> tv == TV "Unit"
              _ -> False
            cap = pretty thunkCapture
        in return . (,) [] $ case (isUnit, stmts) of
          -- Capture by copy: cross-language RPC receiver-manifolds wrap
          -- their deserialized result in a DoBlockN thunk that outlives
          -- the manifold's stack frame. Reference captures ([&]) would
          -- dangle by the time the outer caller invokes the thunk;
          -- captures by value ([=]) copy the parameters (pointers, ints,
          -- copyable structs) and remain valid.
          (True, []) -> cap <> "(){" <> expr <> "; return mlc::Unit{};}"
          (True, _) -> cap <> "(){" <> nest 4 (line <> vsep (stmts <> [expr <> ";", "return mlc::Unit{};"])) <> line <> "}"
          (False, []) -> cap <> "(){return " <> expr <> ";}"
          (False, _) -> cap <> "(){" <> nest 4 (line <> vsep (stmts <> ["return " <> expr <> ";"])) <> line <> "}"
    -- The Ok arm's parameter is declared as the payload type, so a body
    -- whose native result merely converts to it (a handle intrinsic answers
    -- int64_t where the handle type is uint64_t) converts at the call, where
    -- an implicit conversion is allowed, and not inside the arm's brace
    -- initializer, where narrowing is an error. The Err arm's is a
    -- std::string, and both arms must deduce the same Try type for
    -- _mlc_try's return.
    -- The payload arrives as a prvalue -- '_mlc_try' calls the Ok arm with
    -- the body's result and nothing else can observe it -- so the arm takes
    -- it by rvalue reference and hands it on, rather than binding a const
    -- reference and copy-constructing the arm from it. For a batch of
    -- records that copy is the whole payload. 'std::forward' rather than
    -- 'std::move' so the untyped fallback ('auto&&', a forwarding
    -- reference) stays correct on its own terms.
    , lcMakeTry = \thunk okT okWrap errWrap ->
        "_mlc_try" <> tupled
          [ tryThunkByReference thunk
          , "[](" <> maybe "auto&&" (<> "&&") okT <+> "mlcTryV) { return"
              <+> okWrap "std::forward<decltype(mlcTryV)>(mlcTryV)" <> "; }"
          , "[](std::string&& mlcTryM) { return" <+> errWrap "std::move(mlcTryM)" <> "; }"
          ]
    , lcSerialize = \v s -> serialize v s
    , lcDeserialize = \t v s -> do
        typestr <- cppTypeOf t
        deserialize v typestr s
    , lcReifyClosure = \v _ ->
        -- C++ reification (producing a crossing closure from C++) recovers the
        -- manifold id and captured values from the fat MorlocClosure functor
        -- stored in the std::function (see '_mlc_reify' in pool.cpp). Arity-generic
        -- via templates, so the 'SerialClosure' argument is unused.
        return $ "_mlc_reify" <> parens v
    , lcReflectClosure = \pkt s ->
        case s of
          SerialClosure ins out -> do
            tupSid <- cppRegisterSchema (render (serialAstToMsgpackSchema s))
            let cloInit =
                  "_get_value<" <> pretty cppClosureWireTupleName <> ">(" <> pkt
                    <> ", mlc_schema_table[" <> pretty tupSid <> "])"
            cppClosureProxyLambda cloInit ins out
          _ -> error "lcReflectClosure: expected SerialClosure"
    -- A closure NESTED in an aggregate: the enclosing get_value already produced
    -- the wire tuple, so the proxy captures it directly (no second get_value).
    , lcReflectClosureParsed = \tup s ->
        case s of
          SerialClosure ins out -> cppClosureProxyLambda tup ins out
          _ -> error "lcReflectClosureParsed: expected SerialClosure"
    -- A variant's generated marshaller converts its own closures.
    , lcDivertNestedClosure = cppDivertsClosure
    , lcMakeFunction = \callIndex mname args manifoldType priorLines body headForm isClosure -> do
        state <- CMS.get
        let alreadyDone = case headForm of
              (Just HeadManifoldFormRemoteWorker) -> Set.member callIndex (translatorRemoteManifoldSet state)
              _ -> Set.member callIndex (translatorLocalManifoldSet state)
        if alreadyDone
          then return Nothing
          else do
            case headForm of
              (Just HeadManifoldFormRemoteWorker) ->
                CMS.modify
                  (\s -> s {translatorRemoteManifoldSet = Set.insert callIndex (translatorRemoteManifoldSet s)})
              _ ->
                CMS.modify
                  (\s -> s {translatorLocalManifoldSet = Set.insert callIndex (translatorLocalManifoldSet s)})
            returnTypeStr <- returnType manifoldType
            typedArgs <- mapIndexedM (cppParamOf callIndex) args
            let debugMode = translatorDebugMode state
                -- Outside --debug a frame is a call of a named function,
                -- labeled with its definition (DIAG-5); any other manifold
                -- has no frame of its own.
                traceLabel = translatorTraceInfo state callIndex
                (userName, srclocStr)
                  | debugMode = translatorDebugInfo state callIndex
                  | otherwise = maybe ("", "") id traceLabel
                -- Raw text with a literal newline; escapeCxxStringLit converts
                -- '\n' -> "\\n" so the emitted C++ string literal contains the
                -- escape, not an actual newline byte.
                frameName = cppFrameName callIndex userName srclocStr
                frameLine = "\n  at " <> frameName
                frameLit :: MDoc
                frameLit = dquotes (pretty (escapeCxxStringLit frameLine))
                -- Marks this manifold as the one executing on the thread,
                -- so a fatal signal is reported against it.
                frameScope = "mlc::frame_scope _mlc_frame(" <> dquotes (pretty (escapeCxxStringLit frameName)) <> ");"
                fullName = mname <> mnameExt headForm
                decl = returnTypeStr <+> fullName <> tupled typedArgs
                tryBody = block 4 "try" (vsep $ frameScope : priorLines <> [body])
                -- In --debug the drain owns the traceback; appending a
                -- frame line here would duplicate every frame. The
                -- normalize-to-std::runtime_error step remains so that
                -- non-std::exception throws (const char*, std::string)
                -- surface their message through the outer dispatch's
                -- 'catch (const std::exception&)'.
                appendFrame lit e =
                  if debugMode
                    then "throw std::runtime_error(" <> e <> ");"
                    else "throw std::runtime_error(" <> e <> " + " <> lit <> ");"
                -- A closed pipe keeps its class through every frame, so no
                -- outer @try takes it.
                catchBody = vsep
                  [ block 4 "catch (const MorlocPipeClosed&)" "throw;"
                  , block 4 "catch (const std::exception& e)"
                      (appendFrame frameLit "std::string(e.what())")
                  , block 4 "catch (const char* e)"
                      (appendFrame frameLit "std::string(e)")
                  , block 4 "catch (const std::string& e)"
                      (appendFrame frameLit "e")
                  , block 4 "catch (...)"
                      (appendFrame frameLit "std::string(\"An unknown error occurred\")")
                  ]
                -- STATE-5: a named function records its call; the pop is in
                -- its return statement ('lcFrameReturn'). STATE-6: a closure,
                -- which code outside morloc may call and whose failure it may
                -- catch, restores the depth it found when a failure leaves it.
                framePush = case frameLineLit state callIndex of
                  Just lit -> ["mlc::frame_push(" <> lit <> ");"]
                  Nothing -> []
                innerBlock
                  | debugMode = tryBody <+> catchBody
                  | isClosure =
                      vsep
                        [ "size_t _mlc_saved = mlc::frame_enter();"
                        , block 4 "try" (vsep (framePush <> priorLines <> [body]))
                            <+> block 4 "catch (...)" "_mlc_closure_rethrow(_mlc_saved);"
                        ]
                  | otherwise = vsep (framePush <> priorLines <> [body])
            -- Wrap labeled manifolds with a chrono start/pass/fail shim,
            -- using user-supplied templates (from YAML 'log-template').
            -- Each event has its own pre-rendered template; a 'Nothing'
            -- subfield elides that emission. Wrapping at the definition
            -- site (rather than the dispatch table) is required because
            -- inner manifolds are referenced by symbol (std::bind) and
            -- can't be rebound after definition. The IIFE captures the
            -- body's `return X;` so the wrap can log after the result is
            -- computed. The foreign-callee side is skipped: the caller
            -- pool already wraps the cross-pool call and includes the
            -- socket round-trip.
            let bodyDoc = case Map.lookup callIndex (translatorLogTemplates state) of
                  Just tmpl | not (isForeignCalleeForm headForm) ->
                    let groupLit = dquotes (pretty (escapeCxxStringLit (renderedGroup tmpl)))
                        emit Nothing _ = mempty
                        emit (Just t) runtimeExpr =
                          let q = dquotes (pretty (escapeCxxStringLit t))
                           in [idoc|morloc_log_emit(#{q}, #{groupLit}, #{runtimeExpr}, __mlc_id);|]
                        startLine = emit (renderedStart tmpl) ("0.0" :: MDoc)
                        passLine = emit (renderedPass tmpl) "__mlc_dt.count()"
                        failLine = emit (renderedFail tmpl) "__mlc_dt.count()"
                        -- Only the success path is measured: a call that
                        -- raised did not do the work being timed.
                        benchLine = case renderedBenchKey tmpl of
                          Nothing -> mempty
                          Just k ->
                            let kq = dquotes (pretty (escapeCxxStringLit k))
                             in [idoc|morloc_bench_record(#{kq}, __mlc_dt.count());|]
                        dtDecl = "std::chrono::duration<double> __mlc_dt = std::chrono::steady_clock::now() - __mlc_t0;"
                        innerLambda = "[&]()" <> line <> "{" <> nest 4 (line <> innerBlock) <> line <> "}()"
                        outerTry = block 4 "try" $ vsep
                          [ "auto __mlc_result =" <+> innerLambda <> ";"
                          , dtDecl
                          , passLine
                          , benchLine
                          , "return __mlc_result;"
                          ]
                        outerCatch = block 4 "catch (...)" $ vsep
                          [ dtDecl
                          , failLine
                          , "throw;"
                          ]
                     in vsep
                          [ "auto __mlc_t0 = std::chrono::steady_clock::now();"
                          , "auto __mlc_id = morloc_log_next_id();"
                          , startLine
                          , outerTry <+> outerCatch
                          ]
                  _ -> innerBlock
            return . Just . block 4 decl $ bodyDoc
    , lcClosureSig = \tm -> case tm of
        Function ins out -> do
          argTypes <- mapM cppTypeOf ins
          resType <- cppTypeOf out
          return $ resType <> tupled argTypes
        _ -> return ""
    -- An unapplied manifold is its function, unless it is staged or can
    -- cross, which takes the closure form
    , lcManifoldName = manNamer
    , lcMakePass = \_ sig _ mname args ->
        if isJust (stageOf mname) || Map.member (render mname) reifyThunks
          then makeLambda sig mname [] args
          else return mname
    , lcMakeLambda = \_ sig _ -> makeLambda sig
    , lcRegisterSchema = cppRegisterSchema
    , lcTableImportFn = Nothing
    }
  where
    stageOf mname = manifoldIdOf mname >>= (`Map.lookup` stageTable)

    makeLambda :: MDoc -> MDoc -> [Arg TypeM] -> [Arg TypeM] -> CppTranslatorM MDoc
    makeLambda sig mname contextArgs boundArgs =
      let ctxNames = map (argNamer cppLocals) contextArgs
          vs' = take (length boundArgs) (map (\j -> "std::placeholders::_" <> viaShow j) ([1 ..] :: [Int]))
          bindArgs = cat (punctuate "," (mname : (ctxNames ++ vs')))
          -- manNamer is "m<mid>"; the closure's home-pool dispatch mid is the
          -- name minus its leading "m".
          midDoc = pretty (MT.drop 1 (render mname))
          stage = stageOf mname
       in case (Map.lookup (render mname) reifyThunks, stage) of
            -- A staged closure: applied to the arguments before its stage
            -- point, it runs its stage entry (bound to the same captured
            -- values) once and becomes the closure the stage returns.
            (thunk, Just (StageEntry nctx k smid))
              | nctx == length contextArgs ->
                  let (reifier, schemas) = fromMaybe ("mlc_unreifiable", "std::vector<Schema*>{}") thunk
                      stageArgs = cat (punctuate "," (manNamer smid : (ctxNames ++ take k vs')))
                   in return [idoc|mlc_staged_sig<#{pretty k}, #{sig}>::make(std::bind(#{bindArgs}), #{midDoc}, #{reifier}, "cpp", #{schemas}, std::bind(#{stageArgs}))|]
              | otherwise ->
                  error $ "C++: staged closure " <> show (render midDoc) <> " captures " <> show (length contextArgs)
                    <> " values but its stage entry takes " <> show nctx
            -- A closure that can cross a boundary: the fat MorlocClosure
            -- carries its mid and a reify thunk so the far side can call
            -- back; one this pool partially applies also keeps both when
            -- partially applied. (It is stored inside a std::function, so
            -- this has the same C++ type as the thin form below.)
            (Just (reifyThunk, schemas), Nothing)
              | Set.member (render sig) papplyHeads ->
                  return [idoc|mlc_pap_sig<#{sig}>::make(std::bind(#{bindArgs}), #{midDoc}, #{reifyThunk}, "cpp", #{schemas})|]
              | otherwise ->
                  return [idoc|MorlocClosure<#{sig}>{ std::bind(#{bindArgs}), #{midDoc}, #{reifyThunk} }|]
            -- A closure that never crosses: a plain std::bind (OWN-3). The
            -- other forms name their captures twice.
            (Nothing, Nothing) -> do
              ctxDocs <- mapM capture contextArgs
              return [idoc|std::bind(#{cat (punctuate "," (mname : (ctxDocs ++ vs')))})|]
      where
        capture a@(Arg i (Native tf))
          | Just m <- readMay (T.unpack (MT.drop 1 (render mname))), not (isFunctionValue tf) = do
              moves <- cppMovesInto m i tf
              return (if moves then "std::move(" <> (argNamer cppLocals) a <> ")" else (argNamer cppLocals) a)
        capture a = return ((argNamer cppLocals) a)

    -- The wire-side type of a value being taken apart for sending or rebuilt
    -- after receiving: a record that is taken apart or rebuilt (it holds a
    -- packed or closure field, at any depth) is a tuple of its fields, which
    -- _put_value/to_voidstar and the rebuild read by position; anything else
    -- is its wire type, where a closure travels as its reified wire tuple
    -- ('cppClosureWireLeaf').
    serializeTypeOf :: SerialAST -> CppTranslator (Maybe IType)
    serializeTypeOf = rawTypeOf

    rawTypeOf :: SerialAST -> CppTranslator (Maybe IType)
    rawTypeOf s = Just . toIType <$> rawDoc s

    rawDoc :: SerialAST -> CppTranslator MDoc
    rawDoc s | cppIsLeaf s = cppTypeOf (unpackedAsTypeF (wireSerialAstToType cppClosureWireLeaf s))
    rawDoc (SerialObject _ _ _ _ rs) = do
      ds <- mapM (rawDoc . snd) rs
      return $ "std::tuple" <> encloseSep "<" ">" "," ds
    rawDoc (SerialPack _ (_, s)) = rawDoc s
    rawDoc (SerialList v d e) = do
      inner <- rawDoc e
      cppTypeOf (AppF (VarF v) (maybe [] (pure . KiA) d ++ [TyA (rendered inner)]))
    rawDoc (SerialTuple v es) = do
      inners <- mapM rawDoc es
      cppTypeOf (AppF (VarF v) (map (TyA . rendered) inners))
    rawDoc (SerialOptional _ e) = do
      inner <- rawDoc e
      cppTypeOf (OptionalF (rendered inner))
    rawDoc s = cppTypeOf (unpackedAsTypeF (wireSerialAstToType cppClosureWireLeaf s))

    -- Decoded straight into its type, as wireToNative leaves it.
    cppIsLeaf :: SerialAST -> Bool
    cppIsLeaf s = isSerializable s && not (cppDivertsClosure s)

    cppDivertsClosure :: SerialAST -> Bool
    cppDivertsClosure = not . null . serialClosuresOutsideNodes

    rendered :: MDoc -> TypeF
    rendered d = VarF (FV (TV "raw") (CV (render d)))

    -- | Escape a log-template string for embedding in a C++ double-quoted
    -- literal. Reuses the canonical 'Morloc.Data.Doc.escapeStringLit'
    -- + 'escapeQuotes' (also used for Morloc Str literals elsewhere).
    escapeCxxStringLit :: Text -> Text
    escapeCxxStringLit = escapeQuotes "\"" "\\\"" . escapeStringLit

    -- The 'isUnit' flag switches to a void-safe emission shape. C++ source
    -- functions declared '<E> ()' return @void@ by convention, so a plain
    -- @T ni = expr;@ assignment rejects with "void value not ignored as it
    -- ought to be". Split into a statement plus a default-initialised
    -- 'mlc::Unit{}' whenever the let-var's type is 'Unit' -- the effect
    -- fires, the caller's 'Unit' variable is present, and any subsequent
    -- reference to @ni@ still type-checks. A bare variable has no effect,
    -- so it is not emitted as a statement.
    makeLet :: (Int -> MDoc) -> Int -> MDoc -> Bool -> LetMode -> PoolDocs -> PoolDocs -> PoolDocs
    makeLet namer letIndex typestr isUnit binding p1 p2 =
      let letAssignment
            | isUnit && isIdentifierExpr (poolExpr p1) =
                [idoc|#{typestr} #{namer letIndex}{};|]
            | isUnit =
                [idoc|#{poolExpr p1};|]
                  <+> [idoc|#{typestr} #{namer letIndex}{};|]
            -- A field/index projection rooted at a live variable: bind by
            -- const-reference so the projected sub-value is not copied out of
            -- its container (see 'isBorrowableProjection'). The referent (the
            -- root variable) outlives the alias because lets flatten in scope
            -- order. INVARIANT: anything that escapes the frame must COPY this
            -- alias's referent, not hold a reference to it -- today std::bind
            -- decay-copies bound args, '[=]' captures copy the referent, and
            -- a '[&]' capture is only ever an immediately invoked lambda: the
            -- loop IIFE, and an inline @try thunk ('tryThunkByReference'),
            -- which '_mlc_try' calls before returning. A by-reference
            -- capture that escaped would dangle. A binding that takes its
            -- referent is a separate decision and a far narrower one
            -- ('consumableProjectionLets'): nothing else may use the referent
            -- on the path that runs it, so it cannot strand an alias.
            -- Scalars are excluded ('isScalarCppType'): borrowing an int/double
            -- saves nothing, so keep the cheap by-value copy for them. A take
            -- ('cppTakeField') arrives here as an ordinary right-hand side.
            | binding == LetBorrow && not (isScalarCppType typestr) =
                [idoc|const #{typestr}& #{namer letIndex} = #{poolExpr p1};|]
            | otherwise =
                [idoc|#{typestr} #{namer letIndex} = #{poolExpr p1};|]
          rs = poolPriorLines p1 <> [letAssignment] <> poolPriorLines p2
          letName = render (namer letIndex)
          byReference = binding == LetBorrow && not (isScalarCppType typestr)
          borrowedLocal
            | render (poolExpr p2) == letName = if byReference then Just letName else Nothing
            | otherwise = poolBorrowedLocal p2
       in PoolDocs
            { poolCompleteManifolds = poolCompleteManifolds p1 <> poolCompleteManifolds p2
            , poolExpr = poolExpr p2
            , poolPriorLines = rs
            , poolPriorExprs = poolPriorExprs p1 <> poolPriorExprs p2
            , poolReturnFlag = poolReturnFlag p1 || poolReturnFlag p2
            , poolBorrowedLocal = borrowedLocal
            , poolChain = Nothing
            }

    -- Match only bare 'Unit' -- a suspension of Unit is
    -- 'std::function<mlc::Unit()>', an assignable function value, not a
    -- void-returning call site.
    isUnitTypeF :: Maybe TypeF -> Bool
    isUnitTypeF (Just (VarF (FV tv _))) = tv == BT.unit
    isUnitTypeF _                       = False

    mnameExt :: Maybe HeadManifoldForm -> MDoc
    mnameExt (Just HeadManifoldFormRemoteWorker) = "_remote"
    mnameExt _ = ""

    returnType :: TypeM -> CppTranslator MDoc
    returnType = cppTypeOf

    -- The trace line, as a C++ literal, of a manifold that records its call
    -- on the frame stack: outside --debug, a call of a named function,
    -- labeled with its definition (DIAG-5).
    cppFrameName :: Int -> Text -> Text -> Text
    cppFrameName callIndex userName srclocStr =
      (if T.null userName then "_" else userName)
        <> " [cpp] (mid=" <> T.pack (show callIndex)
        <> (if T.null srclocStr then "" else ", " <> srclocStr) <> ")"

    frameLineLit :: CppTranslatorState -> Int -> Maybe MDoc
    frameLineLit state callIndex
      | translatorDebugMode state = Nothing
      | otherwise =
          (\(userName, srclocStr) -> dquotes (pretty (escapeCxxStringLit ("\n  at " <> cppFrameName callIndex userName srclocStr))))
            <$> translatorTraceInfo state callIndex

-- Use `#include "foo.h"` rather than `#include <foo.h>`
translateSource ::
  -- | Path to a header (e.g., `$MORLOC_HOME/src/foo.h`)
  Path ->
  MDoc
translateSource path = "#include" <+> (dquotes . pretty) path

serialize :: MDoc -> SerialAST -> CppTranslator PoolDocs
serialize v0 s = do
  -- A C++ source function or runtime call declared to yield '()' returns
  -- void, so a call in value position is run for its effect and the unit
  -- value is supplied beside it.
  let v = case s of
        SerialNull _ | isCallExpr v0 -> "([&](){" <+> v0 <> "; return mlc::Unit{}; }())"
        _ -> v0
  (expr, stmts) <- expandSerialize (cppLowerConfig (ClosureGen Map.empty Map.empty Set.empty)) v s
  return $
    PoolDocs
      { poolCompleteManifolds = []
      , poolExpr = CP.printExpr expr
      , poolPriorLines = map CP.printStmt stmts
      , poolPriorExprs = []
      , poolReturnFlag = False
      , poolBorrowedLocal = Nothing
      , poolChain = Nothing
      }

-- | Whether a rendered expression is a call (rather than a variable, a
-- literal, or an already-parenthesized expression).
isCallExpr :: MDoc -> Bool
isCallExpr d =
  let t = render d
   in T.isSuffixOf ")" t && not (T.isPrefixOf "(" t) && not (T.isPrefixOf "mlc::Unit" t)

-- | Whether a rendered expression is a bare C++ identifier.
isIdentifierExpr :: MDoc -> Bool
isIdentifierExpr d = case T.uncons (render d) of
  Just (c, rest) -> (DC.isAlpha c || c == '_') && T.all (\x -> DC.isAlphaNum x || x == '_') rest
  Nothing -> False

-- | C++ cache wrap. Mirrors the Python/R generic version but emits
-- C++ syntax and routes through the runtime's C ABI directly (the
-- pool already links libmorloc.so). Native args are serialized via
-- '_put_value' to packets first; serial args pass through. Schema
-- strings are inlined as C string literals since each lcCacheBody
-- call happens once at codegen time -- no need to thread them
-- through a global table.
cppCacheBody ::
  SerialAST ->
  Text ->
  Int ->
  [(Arg TypeM, SerialAST)] ->
  PoolDocs ->
  CppTranslator PoolDocs
cppCacheBody resSa lbl midx args bodyPool = do
  wrapIdx <- getCounter
  let suffix = pretty wrapIdx
      hp = "__morloc_"
      keyVar = hp <> "ck_" <> suffix
      cachedVar = hp <> "cd_" <> suffix
      resultVar = hp <> "cr_" <> suffix
      errVar = hp <> "ce_" <> suffix
      sizeVar = hp <> "cs_" <> suffix
      storeSizeVar = hp <> "css_" <> suffix
      storeOkVar = hp <> "cok_" <> suffix
      packetsVar = hp <> "cpk_" <> suffix
      schemasVar = hp <> "csc_" <> suffix
      labelLit = dquotes (pretty lbl)
      n = length args
      resSchema = render (serialAstToMsgpackSchema resSa)
  -- Per-arg setup: serialize native bindings to packets, route serial
  -- bindings through by name; both feed the packets array.
  preparedArgs <- mapM (prepareCppCacheArg wrapIdx) (indexed args)
  let argRefs = [r | (r, _, _) <- preparedArgs]
      argSchemas = [s | (_, s, _) <- preparedArgs]
      argSetupLines = concatMap (\(_, _, ss) -> ss) preparedArgs
      packetsDecl = "const uint8_t*" <+> packetsVar
        <> brackets (pretty n) <+> "=" <+>
        encloseSep "{ " " }" ", " argRefs <> ";"
      schemasDecl = "const char*" <+> schemasVar
        <> brackets (pretty n) <+> "=" <+>
        encloseSep "{ " " }" ", "
          [dquotes (pretty (cppEscapeString s)) | s <- argSchemas]
        <> ";"
      errDecl = "char*" <+> errVar <+> "= nullptr;"
      keyStmt = vsep
        [ "uint64_t" <+> keyVar <+> "= morloc_cache_key_compute("
            <> pretty midx <> ", " <> packetsVar <> ", " <> schemasVar
            <> ", " <> pretty n <> ", &" <> errVar <> ");"
        , "if (" <> errVar <> ") { PROPAGATE_ERROR(" <> errVar <> ") }"
        ]
      sizeDecl = "size_t" <+> sizeVar <+> "= 0;"
      lookupStmt = vsep
        [ "uint8_t*" <+> cachedVar <+> "= morloc_cache_lookup("
            <> keyVar <> ", " <> labelLit <> ", &" <> sizeVar
            <> ", &" <> errVar <> ");"
        , "if (" <> errVar <> ") { PROPAGATE_ERROR(" <> errVar <> ") }"
        ]
      resultDecl = "uint8_t*" <+> resultVar <> ";"
      hitLines = vsep
        [ "morloc_cache_record_hit();"
        , resultVar <+> "=" <+> cachedVar <> ";"
        ]
      missBodyLines = poolPriorLines bodyPool
        ++ [ resultVar <+> "=" <+> poolExpr bodyPool <> ";"
           , "size_t" <+> storeSizeVar <+> "= morloc_packet_size("
               <> resultVar <> ", &" <> errVar <> ");"
           , "if (" <> errVar <> ") { PROPAGATE_ERROR(" <> errVar <> ") }"
           , "bool" <+> storeOkVar <+> "= morloc_cache_store("
               <> keyVar <> ", " <> labelLit <> ", " <> resultVar
               <> ", " <> storeSizeVar <> ", "
               <> dquotes (pretty (cppEscapeString resSchema))
               <> ", &" <> errVar <> ");"
           , "if (!" <> storeOkVar <> ") {"
           , indent 4 $ vsep
               [ "if (" <> errVar <> ") { PROPAGATE_ERROR(" <> errVar <> ") }"
               , "throw std::runtime_error(\"cache_store failed\");"
               ]
           , "}"
           , "morloc_cache_record_store();"
           ]
      missLines = vsep ("morloc_cache_record_miss();" : missBodyLines)
      ifStmt = vsep
        [ "if (" <> cachedVar <+> "!= nullptr) {"
        , indent 4 hitLines
        , "} else {"
        , indent 4 missLines
        , "}"
        ]
      prior = argSetupLines
        ++ [ errDecl
           , packetsDecl
           , schemasDecl
           , keyStmt
           , sizeDecl
           , lookupStmt
           , resultDecl
           , ifStmt
           ]
  return $ PoolDocs
    { poolExpr = resultVar
    , poolPriorLines = prior
    , poolCompleteManifolds = poolCompleteManifolds bodyPool
    , poolPriorExprs = poolPriorExprs bodyPool
    , poolReturnFlag = poolReturnFlag bodyPool
    , poolBorrowedLocal = Nothing
    , poolChain = Nothing
    }

-- | C++ debug-trace wrap. Wraps the manifold body in @try {...}
-- catch (...) { ... throw; }@. The catch block serializes each arg
-- via the same @_put_value@ path the cache wrap uses, then calls
-- @morloc_debug_record_frame@ with the (mid, schema, packet bytes)
-- tuples before re-throwing. Zero cost on the happy path.
--
-- Schema strings are inlined as C string literals (one-shot at
-- codegen, no need to pass through the schema table).
cppDebugWrap ::
  Int ->
  [(Arg TypeM, SerialAST)] ->
  PoolDocs ->
  CppTranslator PoolDocs
cppDebugWrap midx args bodyPool = do
  wrapIdx <- getCounter
  debugInfoLookup <- CMS.gets translatorDebugInfo
  let (nameStr, srclocStr) = debugInfoLookup midx
      -- Empty string means "no info attached"; the runtime treats it
      -- as NULL and omits the field from the trace line. We still emit
      -- a string literal (not NULL) so the C++ argument type stays
      -- @const char*@ unconditionally.
      nameLit = dquotes (pretty (cppEscapeString nameStr))
      srclocLit = dquotes (pretty (cppEscapeString srclocStr))
      suffix = pretty wrapIdx
      hp = "__morloc_"
      resultVar = hp <> "dr_" <> suffix
      packetsVar = hp <> "dpk_" <> suffix
      schemasVar = hp <> "dsc_" <> suffix
      n = length args
  preparedArgs <- mapM (prepareCppCacheArg wrapIdx) (indexed args)
  let argRefs = [r | (r, _, _) <- preparedArgs]
      argSchemas = [s | (_, s, _) <- preparedArgs]
      argSetupLines = concatMap (\(_, _, ss) -> ss) preparedArgs
      -- Zero-arg manifolds (e.g. an @idcpp@ that produced a nullary
      -- boundary) can't declare zero-length C arrays with initializers
      -- portably, so pass @nullptr@ directly and skip the temporaries.
      (packetsExpr, schemasExpr, argDeclLines)
        | n == 0 = ("nullptr" :: MDoc, "nullptr" :: MDoc, [])
        | otherwise =
            let packetsDecl = "const uint8_t*" <+> packetsVar
                  <> brackets (pretty n) <+> "=" <+>
                  encloseSep "{ " " }" ", " argRefs <> ";"
                schemasDecl = "const char*" <+> schemasVar
                  <> brackets (pretty n) <+> "=" <+>
                  encloseSep "{ " " }" ", "
                    [dquotes (pretty (cppEscapeString s)) | s <- argSchemas]
                  <> ";"
             in (packetsVar, schemasVar, [packetsDecl, schemasDecl])
      tryBody = poolPriorLines bodyPool
        ++ [ resultVar <+> "=" <+> poolExpr bodyPool <> ";" ]
      -- Inner try/catch in the catch block: a serialization failure
      -- during the dump must NOT replace the original exception.
      catchBody = vsep
        [ "try {"
        , indent 4 $ vsep
            [ vsep argSetupLines
            , vsep argDeclLines
            , "morloc_debug_record_frame(" <> pretty midx
                <> ", " <> nameLit
                <> ", " <> srclocLit
                <> ", " <> dquotes "cpp"
                <> ", " <> packetsExpr
                <> ", " <> schemasExpr
                <> ", " <> pretty n <> ");"
            ]
        , "} catch (...) {}"
        , "throw;"
        ]
      tryStmt = vsep
        [ "uint8_t*" <+> resultVar <+> "= nullptr;"
        , "try {"
        , indent 4 (vsep tryBody)
        , "} catch (...) {"
        , indent 4 catchBody
        , "}"
        ]
  return $ PoolDocs
    { poolExpr = resultVar
    , poolPriorLines = [tryStmt]
    , poolCompleteManifolds = poolCompleteManifolds bodyPool
    , poolPriorExprs = poolPriorExprs bodyPool
    , poolReturnFlag = poolReturnFlag bodyPool
    , poolBorrowedLocal = Nothing
    , poolChain = Nothing
    }

-- | Serialize a single C++ cache arg. Native bindings are routed
-- through '_put_value' into a temporary packet; serial bindings
-- pass through by name. The returned schema text is the same schema
-- that the surrounding wrap will register and inline.
prepareCppCacheArg ::
  Int ->
  (Int, (Arg TypeM, SerialAST)) ->
  CppTranslator (MDoc, Text, [MDoc])
prepareCppCacheArg wrapIdx (j, (a@(Arg _ tm), sa)) = do
  let schemaStr = render (serialAstToMsgpackSchema sa)
  case tm of
    Native _ -> do
      sid <- cppRegisterSchema schemaStr
      -- The packet exists only to key and store the cache entry, so its
      -- owner releases it when the manifold's scope ends.
      let argVar = "__morloc_ca_" <> pretty wrapIdx <> "_" <> pretty j
          decl = "mlc::Packet" <+> argVar <> "_own(_put_value("
            <> (argNamer cppLocals) a <> ", mlc_schema_table[" <> pretty sid <> "]));"
            <+> "const uint8_t*" <+> argVar <+> "=" <+> argVar <> "_own.get();"
      return (argVar, schemaStr, [decl])
    _ -> return ((argNamer cppLocals) a, schemaStr, [])

-- | Escape a string for a C++ string literal. Conservative: only
-- handles characters the morloc msgpack-schema serializer emits
-- (backslash, double-quote). Other ASCII passes through; non-ASCII
-- is not produced by the schema serializer.
cppEscapeString :: Text -> Text
cppEscapeString = T.concatMap esc
  where
    esc '\\' = "\\\\"
    esc '"' = "\\\""
    esc c = T.singleton c

-- reverse of serialize, parameters are the same
deserialize :: MDoc -> MDoc -> SerialAST -> CppTranslator (MDoc, [MDoc])
deserialize varname0 typestr0 s0 = do
  (expr, stmts) <- expandDeserialize (cppLowerConfig (ClosureGen Map.empty Map.empty Set.empty)) varname0 s0
  let rendered = CP.printExpr expr
  if null stmts
    then return (rendered, [])
    else do
      schemaVar <- (helperNamer cppLocals) <$> getCounter
      let final = [idoc|#{typestr0} #{schemaVar} = #{rendered};|]
      return (schemaVar, map CP.printStmt stmts ++ [final])

-- The C++ concrete type of a defunctionalized closure's wire tuple:
-- @(home_language, manifold_id, captured_packets)@, as in 'Serial.closureWireSchema'.
cppClosureWireTupleName :: Text
cppClosureWireTupleName = "std::tuple<std::string, int64_t, std::vector<std::vector<uint8_t>>>"

-- The wire-tuple leaf type for a closure. Its concrete type is carried in the
-- CVar so 'cppTypeOf' renders it verbatim (a closure nested in a vector/tuple/
-- struct then becomes a vector/tuple/struct OF this tuple).
cppClosureWireLeaf :: TypeF
cppClosureWireLeaf = VarF (FV (TV "Closure") (CV cppClosureWireTupleName))

-- Build a C++ reflect proxy lambda for a crossing closure. @cloInit@ initializes
-- the captured @__clo@ wire tuple: the deserialized packet at the top level, or
-- the already-parsed tuple for a closure nested in an aggregate. On each
-- application the proxy serializes its arguments, appends them to the captured
-- packets, and RPCs back to the home pool via foreign_call on the manifold id.
cppClosureProxyLambda :: MDoc -> [SerialAST] -> SerialAST -> CppTranslator MDoc
cppClosureProxyLambda cloInit ins out = do
  argTypes <- mapM (cppTypeOf . unpackedAsTypeF . serialAstToType) ins
  resType <- cppTypeOf (unpackedAsTypeF (serialAstToType out))
  sig <- closureCppSig (map (unpackedAsTypeF . serialAstToType) ins) (unpackedAsTypeF (serialAstToType out))
  -- The argument packets and the result packet belong to this application
  -- alone, so each is held by an mlc::Packet and released once the result
  -- has been read, or when a throw unwinds past it. The captured packets
  -- belong to the closure.
  pushes <- mapM
    (\(i, ast) -> do
        enc <- cppEncode ("__a" <> pretty i) ast
        return ("__owned.emplace_back(" <> enc <> "); __pkts.push_back(__owned.back().get());"))
    (indexed ins)
  resDoc <- cppDecode "__res.get()" resType out
  let paramDocs = [t <+> ("__a" <> pretty i) | (i, t) <- indexed argTypes]
      bodyDocs =
        [ "std::vector<const uint8_t*> __pkts;"
        , "std::vector<mlc::Packet> __owned;"
        , "__owned.reserve(" <> pretty (length ins) <> ");"
        , "for (const auto& __c : std::get<2>(__clo)) { __pkts.push_back(__c.data()); }"
        ]
          <> pushes
          <> [ "std::string __sock = std::string(\"pipe-\") + std::get<0>(__clo);"
             , "mlc::Packet __res(foreign_call_v(__sock.c_str(), (size_t)std::get<1>(__clo), __pkts.data(), __pkts.size()));"
             , "return " <> resDoc <> ";"
             ]
      proxy =
        vsep
          [ "[__clo = __origin]"
              <> parens (hsep (punctuate "," paramDocs)) <+> "->" <+> resType <+> "{"
          , indent 4 (vsep bodyDocs)
          , "}"
          ]
  -- Applied to its first argument, the proxy is the proxy of the rest: the
  -- argument joins the origin's captured values, and when that completes a
  -- staged closure's arguments before its stage point, the stage entry runs
  -- in the home pool and the closure it returns is the origin instead.
  apply1 <- case ins of
    [] -> return []
    (a0 : rest) -> do
      a0Type <- cppTypeOf (unpackedAsTypeF (serialAstToType a0))
      a0Sid <- cppRegisterSchema (render (serialAstToMsgpackSchema a0))
      restSig <- closureCppSig (map (unpackedAsTypeF . serialAstToType) rest) (unpackedAsTypeF (serialAstToType out))
      restSid <- cppRegisterSchema (render (serialAstToMsgpackSchema (SerialClosure rest out)))
      restProxy <- cppClosureProxyLambda "__o" rest out
      return
        [ "__c.apply1 = [__origin](const" <+> a0Type <> "& __x) -> std::function<" <> pretty restSig <> "> {"
        , indent 4 (vsep
            [ "auto __o = __origin;"
            , "std::get<2>(__o).push_back(_mlc_reify_arg(__x, mlc_schema_table[" <> pretty a0Sid <> "]));"
            , "size_t __n, __k; int64_t __s;"
            , "if (mlc_stage_lookup(std::get<1>(__o), &__n, &__k, &__s) && std::get<2>(__o).size() == __n + __k) {"
            , indent 4 (vsep
                [ "std::vector<const uint8_t*> __pkts;"
                , "for (const auto& __p : std::get<2>(__o)) { __pkts.push_back(__p.data()); }"
                , "std::string __sock = std::string(\"pipe-\") + std::get<0>(__o);"
                , "mlc::Packet __res(foreign_call_v(__sock.c_str(), (size_t)__s, __pkts.data(), __pkts.size()));"
                , "__o = _get_value<" <> pretty cppClosureWireTupleName <> ">(__res.get(), mlc_schema_table[" <> pretty restSid <> "]);"
                ])
            , "}"
            , "return " <> restProxy <> ";"
            ])
        , "};"
        ]
  -- The proxy is itself a MorlocClosure carrying the origin it was reflected
  -- from, so passing it on to a third pool (or back home) reifies to that
  -- origin rather than to this pool.
  return $
    vsep
      [ "([&]() {"
      , indent 4 (vsep
          ( [ "auto __origin = " <> cloInit <> ";"
            , "auto __c = MorlocClosure<" <> pretty sig <> ">{"
            , indent 4 (vsep
                [ proxy <> ","
                , "std::get<1>(__origin),"
                , "[__origin]() { return std::get<2>(__origin); },"
                , "std::get<0>(__origin)"
                ])
            , "};"
            ]
            ++ apply1
            ++ ["return __c;"]
          ))
      , "}())"
      ]

-- | The stage table ('StageEntry'), for the reflect proxies of closures from
-- other pools.
stageLookupDoc :: Map.Map Int StageEntry -> MDoc
stageLookupDoc tbl =
  vsep
    [ "[[maybe_unused]] static bool mlc_stage_lookup(int64_t mid, size_t* nctx, size_t* k, int64_t* smid) {"
    , indent 4 (vsep
        ( "switch (mid) {"
            : [ "case" <+> pretty f <> ":" <+> "*nctx =" <+> pretty n <> "; *k =" <+> pretty k <> "; *smid =" <+> pretty st <> "; return true;"
              | (f, StageEntry n k st) <- Map.toAscList tbl ]
            ++ ["default: return false;", "}"]
        ))
    , "}"
    ]

-- | The type a value of a 'TypeM' has in the pool.
typeMToF :: TypeM -> TypeF
typeMToF (Native t) = t
typeMToF (Serial t) = t
typeMToF (Function ins out) = FunF (map typeMToF ins) (typeMToF out)
typeMToF Passthrough = error "unreachable: a passthrough value is never a native argument"

translateSegment :: ClosureGen -> SerialManifold -> CppTranslator MDoc
translateSegment closureGen m0@(SerialManifold root _ _ _ _) = do
  resetCounter
  CMS.modify (\st -> st {translatorCurrentRoot = root})
  modes <- mapM (\(i, rhs) -> (\m -> (i, rhs, m)) <$> cppLetMode i rhs) (nativeLets m0)
  pp <- CMS.gets translatorPool
  let plan = letPlan modes
      held = lpHeld plan
      byValue = Map.fromListWith Set.intersection [(m, byValueIndices pp m form) | NativeManifold m _ form@(ManifoldFull _) _ <- nativeManifoldsUnder (RSM m0)]
      params = Set.unions (Map.elems byValue) `Set.difference` loopSlots (RSM m0)
      seeds m = Set.toList (Map.findWithDefault Set.empty m byValue `Set.intersection` params)
      m0' = paramReads seeds m0
      m1 = markLastReadsSeeded (lpAliases plan) held seeds m0'
      builds = lastBuilds (lpAliases plan) held seeds [m0']
  let moves = lpOwned plan
  CMS.modify (\st -> st {translatorMoves = moves, translatorParamMoves = params, translatorLets = Set.fromList [i | (i, _, _) <- modes], translatorBuilds = builds, translatorMovedBuilds = Set.empty})
  e <- foldWithSerialManifoldM (defaultFoldRules (cppLowerConfig closureGen)) m1
  moved <- CMS.gets translatorMovedBuilds
  CMS.modify (\st -> st {translatorLowered = (m1, modes, moves <> params, moved) : translatorLowered st})
  return $ renderPoolDocs e

-- handle string interpolation
-- | A pattern call, given its result type and its receiver's type.
evaluatePattern :: CppTranslatorState -> TypeF -> Maybe TypeF -> PatApp MDoc -> MDoc
evaluatePattern _ _ _ (PatText s xs) = "interweave_strings" <> tupled [fragments, insertions]
  where
    fragments = encloseSep "{" "}" ", " (map (dquotes . pretty . escapeQuotes "\"" "\\\"" . escapeStringLit) (s : map snd xs))
    insertions = encloseSep "{" "}" ", " (map fst xs)

-- handle getters
evaluatePattern state _ rt (PatGet (ungroup -> [ss]) [] m) =
  readSelector (translatorHandles state) rt m ss
evaluatePattern state t rt (PatGet (ungroup -> sss) [] m) =
  CMS.evalState (cppTypeOf t) state <> encloseSep "{" "}" "," (map (readSelector (translatorHandles state) rt m) sss)
-- PatternStruct with bracket steps inside the Selector. App args are
-- [bracket_bounds..., receiver]. Walk the Selector recursively,
-- emitting C++ source for each step: `.field`, `std::get<N>(...)`,
-- `morloc_at(idx, value)`, `morloc_slice(...)`. Multi-sibling groups
-- emit `std::make_tuple(...)`.
evaluatePattern state _ rt (PatGet s bracketArgs receiver) =
  fst (fst (walkCppSelectorBrackets (translatorHandles state) (receiver, rt) bracketArgs s))
evaluatePattern state0 t0 _ (PatSet s0 m0 xs0) = setterOf False state0 t0 s0 m0 (toList xs0)
-- C++ bracket index: List and Vector both use std::vector<T> on the wire,
-- so the same morloc_at helper works for both. Args: [index, receiver].
evaluatePattern _ _ _ (PatIndex i m) =
  "morloc_at" <> tupled [i, m]
-- C++ bracket slice: List and Vector share std::vector<T> on the wire,
-- so morloc_slice works for both. Args: [start, stop, step, receiver].
evaluatePattern _ _ _ (PatSlice start stop step m) =
  "morloc_slice" <> tupled [start, stop, step, m]

-- | A setter on receiver @m0@ with values @xs0@: the receiver rebuilt with
-- the set fields changed; when nothing reads the receiver again (@dead@),
-- its unchanged fields are moved rather than copied.
setterOf :: Bool -> CppTranslatorState -> TypeF -> Selector -> MDoc -> [MDoc] -> MDoc
setterOf dead state0 t0 s0 m0 xs0 =
  patternSetter makeTuple makeRecord accessTuple accessRecord finalizeSet m0 t0 s0 xs0
  where
    makeTuple (AppF _ ts) xs =
      let tupleTypes = CMS.evalState (mapM cppTypeOf (tyArgs ts)) state0
       in "std::tuple" <> encloseSep "<" ">" "," tupleTypes <> tupled xs
    makeTuple _ _ = error "Unreachable"

    -- A `?T` field may be stored as a pointer (CPP-6): its value is
    -- converted to the field's own form.
    handles = translatorHandles state0
    isHandle t = maybe False (`Map.member` handles) (recordNameOf t)
    makeRecord t fields =
      let recT = CMS.evalState (cppTypeOf t) state0
          store ((Key k, OptionalF _), x) = "mlc::wrap_field<decltype(" <> recT <> "::" <> pretty k <> ")>(" <> x <> ")"
          store (_, x) = x
       in if isHandle t
            then cppHandleOf recT (map snd fields)
            else recT <> encloseSep "{" "}" ", " (map store fields)

    accessTuple _ m i = "std::get<" <> pretty i <> ">(" <> m <> ")"
    accessRecord t d k = cppField handles (Just t) d k
    -- An unchanged field is copied into the aggregate init, or moved out of
    -- a receiver nothing reads again (OWN-2).
    finalizeSet _ v
      -- OWN-5: an unchanged field of the receiver's own node is taken from
      -- it whole; a field below it is moved.
      | dead, Just k <- T.stripPrefix (render m0 <> "->") (render v), isIdentifier k =
          "mlc::take_field(" <> m0 <> ".p, [](auto& f) -> auto& { return f." <> pretty k <> "; })"
      | dead = "std::move(" <> v <> ")"
      | otherwise = v

-- | A setter whose receiver is a kept let or by-value parameter at its last
-- read (OWN-2): its unchanged fields are moved out.
cppTakeSetter :: NativeExpr -> TypeF -> Selector -> [MDoc] -> CppTranslatorM (Maybe MDoc)
cppTakeSetter recv t sel (m : xs) = do
  kept <- isJust <$> cppKeptLet recv m
  state <- CMS.get
  return (if kept then Just (setterOf True state t sel m xs) else Nothing)
cppTakeSetter _ _ _ [] = return Nothing

readField :: MDoc -> MDoc
readField access = "mlc::read_field(" <> access <> ")"

-- | A getter's steps from a receiver of the given type. A `?T` field may be
-- stored as a pointer (CPP-6), so a field read that may be one goes through
-- @mlc::read_field@, which gives the std::optional every other `?T` value
-- is and returns any other field as is.
readSelector :: Map.Map TVar [(Key, TypeF)] -> Maybe TypeF -> MDoc -> [Either Int Text] -> MDoc
readSelector _ _ d [] = d
readSelector handles t d (Right k : rs) =
  let ft = t >>= fieldsOf handles >>= lookup (Key k)
      access = cppField handles t d k
      value = case ft of
        Just (OptionalF _) -> readField access
        Nothing -> readField access
        _ -> access
   in readSelector handles ft value rs
readSelector handles t d (Left i : rs) =
  let et = case t of
        Just (AppF _ (tyArgs -> ts)) | i < length ts -> Just (ts !! i)
        _ -> Nothing
   in readSelector handles et ("std::get<" <> pretty i <> ">" <> parens d) rs


-- | Walk a 'Selector' that may contain bracket steps, emitting C++
-- source code per step. Threads the bracket runtime args through
-- bracket steps in DFS order; returns the produced expression and
-- the remaining (unconsumed) bracket args. Multi-sibling groups
-- emit @std::make_tuple(...)@. See 'walkSelectorBrackets'.
walkCppSelectorBrackets :: Map.Map TVar [(Key, TypeF)] -> (MDoc, Maybe TypeF) -> [MDoc] -> Selector -> ((MDoc, Maybe TypeF), [MDoc])
walkCppSelectorBrackets handles =
  walkSelectorBrackets
    (\(rcv, t) k -> (readField (cppField handles t rcv k), t >>= fieldOf k))
    (\(rcv, t) i -> ("std::get<" <> pretty i <> ">" <> parens rcv, t >>= elemOf i))
    (\idx (rcv, t) -> ("morloc_at" <> tupled [idx, rcv], t >>= listElem))
    (\start stop step (rcv, t) -> ("morloc_slice" <> tupled [start, stop, step, rcv], t))
    (\results -> ("std::make_tuple" <> tupled (map fst results), Nothing))
  where
    fieldOf k t = fieldsOf handles t >>= lookup (Key k)
    elemOf i (AppF _ (tyArgs -> ts)) | i >= 0 && i < length ts = Just (ts !! i)
    elemOf _ _ = Nothing
    listElem (AppF _ (tyArgs -> [a])) = Just a
    listElem _ = Nothing

-- | The fields of a record type, a back-reference to a handle included.
fieldsOf :: Map.Map TVar [(Key, TypeF)] -> TypeF -> Maybe [(Key, TypeF)]
fieldsOf _ (NamF _ _ _ fs) = Just fs
fieldsOf handles t = recordNameOf t >>= (`Map.lookup` handles)

-- | A field of a value of the given type: a handle's fields are behind its
-- pointer.
cppField :: Map.Map TVar [(Key, TypeF)] -> Maybe TypeF -> MDoc -> Text -> MDoc
cppField handles t d k = case t >>= recordNameOf of
  Just g | Map.member g handles -> d <> "->" <> pretty k
  _ -> d <> "." <> pretty k

-- | A handle built in pool code, its node from the pool's allocator
-- (CPP-6, STATE-8).
cppHandleOf :: MDoc -> [MDoc] -> MDoc
cppHandleOf t fields = t <> "(mlc::make_arm<" <> CP.handleFields t <> ">(" <> CP.handleFields t <> encloseSep "{" "}" ", " fields <> "))"


-- | Every native type reachable in this pool that names no type variable,
-- with each part of it.
collectCppTypes :: (TVar -> Text -> Bool) -> (TVar -> Bool) -> [SerialManifold] -> [TypeF]
collectCppTypes mapped isData =
  unique
    . filter concrete
    . concatMap parts
    . concatMap (runIdentity . foldWithSerialManifoldM fm)
  where
    fm = defaultValue {opFoldWithNativeExprM = ne, opFoldWithSerialExprM = se}
    ne _ (DeserializeN_ t _ xs) = return $ t : xs
    ne efull e = return $ foldlNE (<>) [typeFof efull] e
    se _ e = return $ foldlSE (<>) [] e

    -- A record's or a variant's type parameters are not descended into:
    -- one may be phantom, naming a type no value carries.
    parts t = t : case t of
      AppF _ ts -> concatMap parts (tyArgs ts)
      NamF _ _ _ rs -> concatMap (parts . snd) rs
      OptionalF x -> parts x
      VariantF _ _ as -> concatMap (concatMap parts . snd) as
      FunF ts r -> concatMap parts (r : ts)
      _ -> []

    concrete t = case t of
      VarF (FV (TV v) (CV c)) -> typeName v && (v /= c || mapped (TV v) c)
      AppF h ts -> concrete h && all concrete (tyArgs ts)
      NamF _ (FV (TV v) _) ps _ -> typeName v && all concrete (tyArgs ps)
      OptionalF x -> concrete x
      VariantF _ ps _ -> all concrete (tyArgs ps)
      RecF (FV g _) ps | isData g -> all concrete (tyArgs ps)
      EnumF {} -> True
      _ -> False
    typeName v = case T.uncons (T.takeWhileEnd (/= '.') v) of
      Just (c, _) -> isUpper c
      Nothing -> False

-- | The build-time checks that each C++ type the pool uses is a value
-- (CPP-3) and that each mapped record accepts moved fields (CPP-4) and
-- holds its recursive fields as shared const values (CPP-6).
cppValueChecks :: [SerialManifold] -> CppTranslator [MDoc]
cppValueChecks es = do
  cscope <- CMS.gets translatorCScope
  let mapped gv c = case Map.lookup gv cscope of
        Just entries -> any (\(_, body, _, _, _) -> outer body == Just c) entries
        Nothing -> False
      outer body = case body of
        VarU (TV n) -> Just n
        AppU (VarU (TV n)) _ -> Just n
        NamU _ (TV n) _ _ -> Just n
        _ -> Nothing
  dataNames <- CMS.gets translatorDataNames
  docs <- mapM check (collectCppTypes mapped (`Set.member` dataNames) es)
  return (map pretty (unique (map render (concat docs))))
  where
    check t = do
      ty <- cppTypeOf t
      handles <- CMS.gets translatorHandles
      -- The name is quoted in a message, so it must not break across lines.
      let name = T.unwords (T.words (render ty))
          valueOf x what = "static_assert(mlc::is_value_type_v<" <> x <> ">, \"CPP-3: " <> what <> " is a value type\");"
          -- A handle's fields live in its fields struct.
          holder = case t of
            NamF _ (FV g _) _ _ | Map.member g handles -> CP.handleFields ty
            _ -> ty
      fields <- case t of
        NamF NamRecord _ _ rs -> return
          [ valueOf ("decltype(" <> holder <> "::" <> pretty k <> ")") ("field " <> pretty k <> " of " <> pretty name)
          | (Key k, _) <- rs ]
        _ -> return []
      let movable = case t of
            NamF NamRecord _ _ rs@(_ : _) ->
              [ "static_assert(mlc::builds_from<" <> holder <> ", "
                  <> hsep (punctuate "," ["decltype(" <> holder <> "::" <> pretty k <> ")" | (Key k, _) <- rs])
                  <> ">, \"CPP-4: " <> pretty name <> " is built from moved fields\");" ]
            _ -> []
      return $ if isScalarCppType ty || T.isInfixOf "\"" name then [] else valueOf ty (pretty name) : fields <> movable

-- | Every occurrence of an argument-free @data@ type in this pool.
collectCppEnums :: [SerialManifold] -> [(FVar, [Text])]
collectCppEnums =
  concatMap (runIdentity . foldWithSerialManifoldM fm)
  where
    fm = defaultValue {opFoldWithNativeExprM = ne, opFoldWithSerialExprM = se}
    ne _ (DeserializeN_ t s xs) = return $ xs <> seek t <> seek (unpackedAsTypeF (serialAstToType s))
    ne efull e = return $ foldlNE (<>) (seek (typeFof efull)) e
    se _ (SerializeS_ s xs) = return $ seek (unpackedAsTypeF (serialAstToType s)) <> xs
    se _ e = return $ foldlSE (<>) [] e

    seek :: TypeF -> [(FVar, [Text])]
    seek (EnumF v _ ns) = [(v, ns)]
    seek (VariantF _ _ as) = concatMap (concatMap seek . snd) as
    seek (NamF _ _ _ rs) = concatMap (seek . snd) rs
    seek (AppF t ts) = seek t <> concatMap seek (tyArgs ts)
    seek (FunF ts t) = concatMap seek (t : ts)
    seek (OptionalF t) = seek t
    seek _ = []

-- | Declare each unmapped enum. A user-mapped type supplies its own
-- definition, exactly as a mapped record does.
generateCppEnums :: [SerialManifold] -> CppTranslator [MDoc]
generateCppEnums es = do
  -- Keyed by the concrete name, which is what the declaration is called.
  let byName = Map.fromListWith Set.union [(cvText, Set.singleton (gv, ns)) | (FV gv (CV cvText), ns) <- collectCppEnums es]
  concat <$> mapM merged (Map.toList byName)
  where
    merged (cvText, occs) = do
      let gvs = nubOrd [gv | (gv, _) <- Set.toList occs]
      unmapped <- filterM (\gv -> not <$> variantIsUserMapped gv cvText) gvs
      case (unmapped, gvs) of
        ([], _) -> return []
        (_, [gv]) -> case mergeEnumOccurrences cvText (map snd (Set.toList occs)) of
          Right ctors -> do
            public <- CMS.gets translatorPublic
            return [CP.printCppEnumDecl (pretty (nativeTypeName cvText)) ctors | not (Map.member gv public)]
          Left msg -> failWith msg
        _ -> failWith $ "the `data` types " <> MT.intercalate ", " (map unTVar gvs) <> " share the C++ name " <> cvText
    failWith :: Text -> CppTranslator [MDoc]
    failWith msg = CMS.modify (\s -> s {translatorErrors = msg : translatorErrors s}) >> return []

-- | Every occurrence of a payload-bearing @data@ type in this pool, with
-- the arguments it was applied to. Occurrences are not merged here: which
-- ones name the same declaration is a question of the RENDERED name --
-- a template instantiated twice is two types, a generated type is one per
-- instantiation whatever it was applied to -- and rendering needs the
-- translator, so the merge happens in 'generateCppVariants'.
collectCppVariants :: [SerialManifold] -> [(FVar, [ArgF], [(Text, [TypeF])])]
collectCppVariants = concatMap (runIdentity . foldWithSerialManifoldM fm)
  where
    fm = defaultValue {opFoldWithNativeExprM = ne, opFoldWithSerialExprM = se}
    ne _ (DeserializeN_ t s xs) = return $ xs <> seek t <> seek (unpackedAsTypeF (serialAstToType s))
    ne efull e = return $ foldlNE (<>) (seek (typeFof efull)) e
    se _ (SerializeS_ s xs) = return $ seek (unpackedAsTypeF (serialAstToType s)) <> xs
    se _ e = return $ foldlSE (<>) [] e

    seek :: TypeF -> [(FVar, [ArgF], [(Text, [TypeF])])]
    seek (VariantF v@(FV _ _) ps as) = (v, ps, as) : concatMap seek (tyArgs ps) <> concatMap (concatMap seek . snd) as
    seek (NamF _ _ _ rs) = concatMap (seek . snd) rs
    seek (AppF t ts) = seek t <> concatMap seek (tyArgs ts)
    seek (FunF ts t) = concatMap seek (t : ts)
    seek (OptionalF t) = seek t
    seek _ = []

-- | Arm structs, wrapper and marshalling for every payload-bearing `data`
-- type; only marshalling for a user-mapped one.
generateCppVariants ::
  LowerConfig CppTranslatorM ->
  Map.Map Int ([SerialAST], [SerialAST], SerialAST) ->
  [SerialManifold] ->
  CppTranslator ([MDoc], [MDoc], [MDoc], [MDoc], [MDoc])
generateCppVariants cfg closureAsts es = do
  ddefs0 <- CMS.gets translatorDataDefs
  -- A template some instance of which holds a function is marshalled at
  -- each instance, whose closures are wired by name; any other once for
  -- every instance.
  let perInstance = Set.fromList ([gv | (FV gv _, _, arms) <- collectCppVariants es, any holdsFunF (concatMap snd arms)]
                      <> [gv | (gv, dd) <- Map.toList ddefs0, any holdsFunF (concatMap snd (ddArms dd))])
  named <- mapM occurrence (collectCppVariants es)
  wireArms <- variantWireArms cppTypeOf closureAsts es
  -- Merged by rendered name: one declaration per name.
  parts <- mapM (uncurry (merged perInstance wireArms)) (Map.toList (Map.fromListWith (flip (<>)) named))
  (tdecls, tbodies, tfwds, tserials) <- templateDecls perInstance
  let decls = tdecls <> concatMap (\(d, _, _, _, _) -> d) parts
      bodies = tbodies <> concatMap (\(_, b, _, _, _) -> b) parts
      fwds = tfwds <> concatMap (\(_, _, f, _, _) -> f) parts
      serials = tserials <> concatMap (\(_, _, _, x, _) -> x) parts
      late = concatMap (\(_, _, _, _, x) -> x) parts
  -- Returned apart so the caller can interleave them with other
  -- declarations.
  return (decls, bodies, fwds, serials, late)
  where
    -- Each generated template, declared from its definition when no header
    -- holds it, and marshalled once for every instance: two morloc types
    -- that are one C++ type are one instance.
    templateDecls perInstance = do
      ddefs <- CMS.gets translatorDataDefs
      public <- CMS.gets translatorPublic
      parts <- sequence
        [ do
            arms' <- mapM (\(c, ts) -> (,) c <$> mapM cppTypeOf ts) (ddArms dd)
            qname <- cppTypeOf (RecF (FV gv (CV (ddForm dd))) [TyA (VarF (FV (TV ("&" <> unTVar p)) (CV (nativeParam p)))) | p <- ddTypeParams dd])
            let leaf = pretty (tvarIdentifier gv)
                decls = if Map.member gv public then [] else [CP.printCppVariantDecl params leaf arms']
                bodies = if Map.member gv public then [] else [CP.printCppVariantArms params leaf arms']
                payload = filter (not . null . snd) arms'
                fwds = CP.printMarshalDecls params qname : [CP.printMarshalDecls params (CP.armName qname c) | (c, _) <- payload]
                armSerials = concat
                  [ [CP.printSerializer params aname fields, CP.printDeserializer params aname fields]
                  | (c, ts) <- payload
                  , let aname = CP.armName qname c
                        fields = [("f" <> pretty i, t) | (i, t) <- indexed ts] ]
                serials = armSerials <> [CP.printCppVariantSerializers params qname arms']
            return (decls, bodies, if Set.member gv perInstance then [] else fwds, if Set.member gv perInstance then [] else serials)
        | (gv, dd) <- Map.toList ddefs
        , let params = map (pretty . nativeParam) (nativeDataParams dd) ]
      return (concatMap (\(a, _, _, _) -> a) parts, concatMap (\(_, b, _, _) -> b) parts, concatMap (\(_, _, c, _) -> c) parts, concatMap (\(_, _, _, d) -> d) parts)
    -- One occurrence under its rendered name, with each arm's field types
    -- as written and as rendered, so the merge can compare spellings.
    occurrence (v, ps, as) = do
      n <- cppTypeOf (VariantF v ps as)
      as' <- mapM (\(c, ts) -> (\rs -> (c, (ts, map render rs))) <$> mapM cppTypeOf ts) as
      return (render n, [(v, ps, as')])
    merged perInstance wireArms name occs = case mergeVariantOccurrences name [((v, ps), as) | (v, ps, as) <- occs] of
      Right ((v, ps), arms) -> makeOne perInstance (Map.findWithDefault [] name wireArms) (v, ps, arms)
      Left msg -> CMS.modify (\s -> s {translatorErrors = msg : translatorErrors s}) >> return ([], [], [], [], [])

    -- A field holding a closure crosses as its wire form, converted by the
    -- shared engine.
    wireField :: Int -> SerialAST -> CppTranslator (Maybe CP.WireField)
    wireField i ast
      | containsClosure ast = do
          (toStmts, toE) <- nativeToWire cfg ("obj.f" <> pretty i) ast
          rawT <- maybe (cppTypeOf (unpackedAsTypeF (wireSerialAstToType cppClosureWireLeaf ast))) (return . renderIType)
                    =<< lcRawDeserialAstType cfg ast
          (fromE, fromStmts) <- wireToNative cfg "mlc_r" ast
          return . Just $
            CP.WireField
              { CP.wfToWire = map CP.printStmt toStmts
              , CP.wfWire = CP.printExpr toE
              , CP.wfRawType = rawT
              , CP.wfFromWire = map CP.printStmt fromStmts
              , CP.wfNative = CP.printExpr fromE
              }
      | otherwise = return Nothing

    makeOne perInstance asts (FV gv (CV cvText), ps, arms) = do
      userMapped <- variantIsUserMapped gv cvText
      arms' <- mapM (\(n, ts) -> (,) n <$> mapM cppTypeOf ts) arms
      name <- cppTypeOf (VariantF (FV gv (CV cvText)) ps arms)
      let -- Each arm's struct needs its own marshalling: the wrapper hands
          -- the payload to the runtime as that struct, and the arm's schema
          -- describes it as a tuple of fields. Without these the size and
          -- write calls fall through to a generic template that reports the
          -- slot alone, and the payload is written over its own slot.
          armSerial (c, ts) = do
            let aname = CP.armName name c
                fields = [("f" <> pretty i, t) | (i, t) <- indexed ts]
            wfs <- case lookup c asts >>= zipExact ts of
              Just tss -> mapIndexedM (\i (t, s) -> (,) t <$> wireField i s) tss
              Nothing -> return [(t, Nothing) | t <- ts]
            return $ if all (isNothing . snd) wfs
              then ([CP.printSerializer [] aname fields, CP.printDeserializer [] aname fields], [])
              else ([], [CP.printArmMarshallers aname wfs])
          serial = CP.printCppVariantSerializers [] name arms'
          payloadArms = filter (not . null . snd) arms'
          fwds = CP.printMarshalDecls [] name : [CP.printMarshalDecls [] (CP.armName name c) | (c, _) <- payloadArms]
      (armSerials, armLate) <- bimap concat concat . unzip <$> mapM armSerial payloadArms
      public <- CMS.gets translatorPublic
      ddefs <- CMS.gets translatorDataDefs
      -- A generated template is declared once, from its definition
      -- ('templateDecls'); an instance has only its marshalling, unless it
      -- is named by its arguments ('Infer.instanceName'), a type of its own.
      let named = Map.member gv ddefs && not (T.any (== '$') cvText)
      -- A generated template whose declaration has no definition: no
      -- instance of it can be spelled.
      dfails <- CMS.gets translatorDataFailures
      when (not userMapped && T.any (== '$') cvText && Map.notMember gv ddefs) $
        CMS.modify $ \s -> s {translatorErrors = ("the generated C++ type for " <> unTVar gv <> " cannot be defined from its declaration: "
          <> Map.findWithDefault "it does not resolve in C++" gv dfails) : translatorErrors s}
      return $ if Map.member gv ddefs && not named && Set.notMember gv perInstance
                 then ([], [], [], [], [])
                 else if userMapped || (Map.member gv public && not named) || (Map.member gv ddefs && not named)
                 then ([], [], fwds, armSerials <> [serial], armLate)
                 else ( [CP.printCppVariantDecl [] name arms']
                      , [CP.printCppVariantArms [] name arms']
                      , fwds
                      , armSerials <> [serial]
                      , armLate )

-- | Whether a type holds a function or suspension.
holdsFunF :: TypeF -> Bool
holdsFunF t = case t of
  FunF _ _ -> True
  _ -> any holdsFunF (childrenOf t)

-- | True when the concrete scope holds an entry for this type whose body
-- names @cvText@, i.e. the user wrote @data Cpp => X = "..."@ and supplies
-- the type themselves. The @gv == cv@ shape alone cannot decide it: an
-- unmapped type defaults its concrete name to its own.
variantIsUserMapped :: TVar -> Text -> CppTranslator Bool
variantIsUserMapped gv cvText = do
  cscope <- CMS.gets translatorCScope
  return $ case Map.lookup gv cscope of
    Just entries -> any (\(_, body, _, _, _) -> outerName body == Just cvText) entries
    Nothing -> False
  where
    outerName (VarU (TV n)) = Just n
    outerName (AppU (VarU (TV n)) _) = Just n
    outerName (NamU _ (TV n) _ _) = Just n
    outerName _ = Nothing

-- | The generated types this pool declares in a header of their own, one
-- header per declaring module (GEN-6), under their qualified names (GEN-4).
-- A type qualifies when its definition cannot depend on its uses (a
-- record with no parameters whose uses all have the same fields, a `data`
-- type with no parameters) and when each field names only C++ builtins,
-- the standard library and other such types, so that the header compiles
-- ahead of every sourced file. Any other generated type is declared in the
-- pool under its pool-local name (GEN-10). Each header is returned with its
-- module path.
planPublicTypes :: [SerialManifold] -> CppTranslator [([Text], Text)]
planPublicTypes es = do
  homes <- CMS.gets translatorTypeHomes
  cscope <- CMS.gets translatorCScope
  defs0 <- CMS.gets translatorGenDefs
  uses0 <- CMS.gets translatorRecordUses
  -- A recursive parameterized record used at an argument holding itself
  -- has no definition yet; its uses say why.
  let generated0 = Map.filter ((== "struct") . gdForm) defs0
      nested = Set.fromList (map fst (nestedSelfUses generated0 (Map.keysSet (Map.filter (\gd -> recursiveRecord (gdVar gd) (gdFields gd)) generated0)) uses0))
      defs = Map.withoutKeys defs0 nested
  let nestedReason = "it " <> nestedSelfUseReason
      -- A user-mapped template is cut by name as a generated record is.
      templates = Map.filter (\gd -> gdForm gd /= "struct" && T.any (== '$') (gdForm gd)) defs0
      templateSelfRefs = Map.keysSet (Map.filter (\gd -> recursiveRecord (gdVar gd) (gdFields gd)) templates)
      nestedTemplates = unique (map fst (nestedSelfUses templates templateSelfRefs uses0))
  CMS.modify $ \s -> s
    { translatorGenDefs = defs
    , translatorGenFailures = Map.union (translatorGenFailures s) (Map.fromSet (const nestedReason) nested)
    , translatorErrors = ["the record " <> unTVar gv <> " " <> nestedSelfUseReason | gv <- nestedTemplates] <> translatorErrors s
    }
  let unparameterized gv = case Map.lookup gv cscope of
        Just ((ps, _, _, _, _) : _) -> null ps
        _ -> False
      eligible gv = unparameterized gv && Map.member gv homes
      -- Every generated record the pool uses has one definition, from its
      -- declaration (GEN-9); one that refers to itself is a handle.
      selfRef gd = recursiveRecord (gdVar gd) (gdFields gd)
      recDefs = Map.filter ((== "struct") . gdForm) defs
      recursive = Map.keysSet (Map.filter selfRef recDefs)
      -- A record may be declared in its module's header when it has a
      -- module and each of its parameters is spelled by an identifier.
      recCands =
        Map.filterWithKey
          (\gv gd -> Map.member gv homes && all (isIdentifier . nativeParam) (nativeParams gd))
          recDefs
  ddefs <- CMS.gets translatorDataDefs
  -- A generated template may be declared in its module's header when each
  -- of its parameters is spelled by an identifier.
  let dataCands = Map.filterWithKey (\gv dd -> Map.member gv homes && all (isIdentifier . nativeParam) (nativeDataParams dd)) ddefs
  allVarOccs <- filterM (\(FV gv (CV cv), _, _) -> not <$> variantIsUserMapped gv cv) (collectCppVariants es)
  let varOccs = [o | o@(_, ps, _) <- allVarOccs, null ps]
  enumOccs <- filterM (\(FV gv (CV cv), _) -> not <$> variantIsUserMapped gv cv) (collectCppEnums es)
  let varCands = Map.fromList [(gv, arms) | (FV gv _, _, arms) <- varOccs, eligible gv]
      varAllArms = Map.fromListWith (<>) [(gv, [arms]) | (FV gv _, _, arms) <- varOccs]
      enumCands = Map.fromList [(gv, ns) | (FV gv _, ns) <- enumOccs, eligible gv]
      qualified gv = maybe (unTVar gv) (\h -> qualifiedCppName h) (Map.lookup gv homes)
      -- A record not in its module's header is defined in the pool, after
      -- the sourced files, under a pool-private name (GEN-10).
      -- One flat namespace per module, at global scope, so no name a field
      -- spells unqualified (`geom::Point`) resolves inside it.
      privateName gv = "::" <> maybe ("mlc_pool_::" <> tvarIdentifier gv) (\h -> "mlc_pool_" <> encodeModulePath (typeHomePath h) <> "::" <> typeHomeName h) (Map.lookup gv homes)
      install cands = do
        let names = Map.fromList [(gv, qualified gv) | gv <- Set.toList cands]
        CMS.modify $ \s -> s
          { translatorPublic = names
          , translatorRecords = recDefs
          , translatorRecordNames = Map.mapWithKey (\gv _ -> Map.findWithDefault (privateName gv) gv names) recDefs
          , translatorHandles =
              Map.fromList [(gv, gdFields gd) | (gv, gd) <- Map.toList recDefs, Set.member gv recursive]
          }
        return (Set.fromList (Map.elems names))
      fieldTypes gv
        | Just gd <- Map.lookup gv recDefs = map snd (gdFields gd)
        | Just arms <- Map.lookup gv varCands = concatMap snd arms
        | Just dd <- Map.lookup gv ddefs = concatMap snd (ddArms dd)
        | otherwise = []
      -- Every occurrence of a variant must render the same arms for the
      -- header to declare them; one that does not stays in the pool, whose
      -- merge reports the conflict.
      agrees gv = case Map.lookup gv varAllArms of
        Just occs -> (== 1) . length . unique <$> mapM (mapM (\(c, ts) -> (,) c . map render <$> mapM cppTypeOf ts)) occs
        Nothing -> return True
      -- Which types the header holds follows from the declarations alone
      -- (GEN-10).
      generated = Set.unions [Map.keysSet recDefs, Set.fromList [gv | (FV gv _, _, _) <- allVarOccs], Set.fromList [gv | (FV gv _, _) <- enumOccs]]
      fieldsOf drop' =
        Map.fromList $
          [(gv, map snd (gdFields gd)) | (gv, gd) <- Map.toList recCands]
            <> [(gv, concatMap snd arms) | (gv, arms) <- Map.toList varCands, Set.notMember gv drop']
            <> [(gv, concatMap snd (ddArms dd)) | (gv, dd) <- Map.toList dataCands]
            <> [(gv, []) | gv <- Map.keys enumCands]
      place drop' = placeTypes (headerSafe Set.empty) generated (fieldsOf drop')
  _ <- install (place Set.empty)
  -- Every occurrence of a variant must render the same arms for the header
  -- to declare them; one that does not stays in the pool, whose merge
  -- reports the conflict. Rendering here is planning: its errors are the
  -- final render's to report.
  errors0 <- CMS.gets translatorErrors
  disagreeing <- Set.fromList <$> filterM (fmap not . agrees) (Map.keys varCands)
  CMS.modify $ \s -> s {translatorErrors = errors0}
  let public = place disagreeing
  _ <- install public
  mapM_ (checkComponents homes) (Set.toList (Set.union public (Map.keysSet recDefs)))
  mapM_ checkFields (Map.elems recDefs)
  -- Every use of a generated record is an instance of its definition
  -- (GEN-9). A user-mapped record's unpacked use is a tuple in C++, never
  -- its form.
  uses <- CMS.gets translatorRecordUses
  forM_ (disagreements recDefs uses) $ \(gv, d) -> case d of
    FieldDiffers k decl use -> do
      dr <- render <$> cppTypeOf decl
      ur <- render <$> cppTypeOf use
      complain ("the generated C++ type for " <> unTVar gv <> " cannot serve one of its uses: its field `" <> unKey k
                 <> "` is `" <> ur <> "` there but `" <> dr <> "` in the definition; give " <> unTVar gv
                 <> " a C++ form, `record Cpp => " <> unTVar gv <> " = \"...\"`")
    Unmatched e -> complain ("the generated C++ type for " <> unTVar gv <> " cannot serve one of its uses: " <> e)
  checkCollisions
    [ (n, unTVar gv)
    | gv <- Set.toList public
    , n <- qualified gv : [qualified gv <> "_fields" | Set.member gv recursive]
        <> [CP.armNameText (qualified gv) c | Just arms <- [Map.lookup gv varCands], (c, _) <- arms]
        <> [CP.armNameText (qualified gv) c | Just dd <- [Map.lookup gv dataCands], (c, _) <- ddArms dd]
    ]
  let byPath = Map.fromListWith (<>) [(typeHomePath h, [gv]) | gv <- Set.toList public, Just h <- [Map.lookup gv homes]]
      pathOf = Map.fromList [(qualified gv, typeHomePath h) | gv <- Set.toList public, Just h <- [Map.lookup gv homes]]
  handles <- CMS.gets translatorHandles
  templates <- CMS.gets translatorRecords
  mapM (header homes recCands templates varCands enumCands pathOf handles) (Map.toList byPath)
  where
    header homes recCands templates varCands enumCands pathOf handles (path, gvs) = do
      let leaf gv = pretty (maybe (unTVar gv) typeHomeName (Map.lookup gv homes))
          enums = [CP.printCppEnumDecl (leaf gv) ns | gv <- gvs, Just ns <- [Map.lookup gv enumCands]]
      ddefs <- CMS.gets translatorDataDefs
      let variantOf gv = case Map.lookup gv varCands of
            Just arms -> Just ([], arms)
            Nothing -> (\dd -> (map (pretty . nativeParam) (nativeDataParams dd), ddArms dd)) <$> Map.lookup gv ddefs
      variants <- sequence
        [ (\arms' -> (CP.printCppVariantDecl params (leaf gv) arms', CP.printCppVariantArms params (leaf gv) arms', concatMap snd arms'))
            <$> mapM (\(c, ts) -> (,) c <$> mapM cppTypeOf ts) arms
        | gv <- gvs, Just (params, arms) <- [variantOf gv] ]
      -- A record that refers to itself is a handle to its fields (CPP-6).
      let structOf gv = if Map.member gv handles then leaf gv <> "_fields" else leaf gv
      let paramsOf gv = [pretty (nativeParam p) | Just gd <- [Map.lookup gv templates], p <- nativeParams gd]
          fieldsOf gv = gdFields <$> Map.lookup gv templates
      records <- sequence
        [ (\fields -> (gv, CP.printStructTypedef (paramsOf gv) (structOf gv) fields, map snd fields))
            <$> mapM (\(k, t) -> (,) (pretty k) <$> cppTypeOf t) fs
        | gv <- gvs, Just fs <- [fieldsOf gv] ]
      let recNames = Map.fromList [(qualifiedCppName h, gv) | (gv, _, _) <- records, Just h <- [Map.lookup gv homes]]
          -- A record holds the records its fields name by value, so those
          -- are declared first.
          ordered =
            Graph.flattenSCCs . Graph.stronglyConnComp $
              [ (decl, gv, [d | tok <- concatMap (cppTokens . render) ts, Just d <- [Map.lookup tok recNames], d /= gv, Map.notMember d handles])
              | (gv, decl, ts) <- records ]
          used = concatMap (cppTokens . render) (concat [ts | (_, _, ts) <- records] <> concat [ts | (_, _, ts) <- variants])
          deps = Set.toList (Set.fromList [p | tok <- used, Just p <- [Map.lookup tok pathOf], p /= path])
          handleGvs = [gv | (gv, _, _) <- records, Map.member gv handles]
          decls =
            [ vsep enums
            , vsep [w | (w, _, _) <- variants]
            , vsep [hsep (filter (not . T.null . render) [CP.printTemplateHeader (paramsOf gv), "struct" <+> structOf gv <> ";"]) | (gv, _, _) <- records]
            , vsep [CP.printHandleDecl (paramsOf gv) (leaf gv) | gv <- handleGvs]
            , vsep ordered
            , vsep [CP.printHandleDefs [] (leaf gv) | gv <- handleGvs, null (paramsOf gv)]
            , vsep [a | (_, a, _) <- variants]
            ]
          -- Only the standard headers that declare a name the definitions use.
          stdHeaders =
            Set.toList . Set.fromList $
              [ h
              | tok <- cppTokens (render (vsep decls))
              , Just h <- [Map.lookup (fromMaybe tok (T.stripPrefix "::" tok)) cppStdHeaderOf]
              ]
          body =
            vsep $
              [ "// Generated by morloc; do not edit."
              , "#pragma once"
              ]
                <> ["#include <" <> pretty h <> ">" | h <- stdHeaders]
                -- A handle's node comes from the pool's allocator where one is
                -- available (STATE-8).
                <> (if null handleGvs then [] else ["#if __has_include(<mlc_rec.hpp>)", "#include <mlc_rec.hpp>", "#endif"])
                <> ["#include" <+> dquotes (pretty (cppHeaderPath p)) | p <- deps]
                <> ["namespace mlc::types::" <> pretty (T.intercalate "::" path) <+> "{"]
                <> decls
                <> ["}", ""]
      return (path, render body)

    -- A generated type's namespace and fields must be spellable in C++
    -- (GEN-5), and no two generated names may coincide.
    complain msg = CMS.modify $ \s -> s {translatorErrors = msg : translatorErrors s}
    checkComponents homes gv =
      mapM_
        (\c -> complain ("the generated C++ type for " <> unTVar gv <> " cannot be written: its namespace would hold `"
                          <> c <> "`, which is not an identifier, is a C++ keyword, or is a namespace the generated code names"))
        [c | Just h <- [Map.lookup gv homes], c <- typeHomePath h, Set.member c cppReservedComponents || not (isIdentifier c)]
    checkFields gd =
      mapM_
        (\k -> complain ("the generated C++ type for " <> unTVar (gdVar gd) <> " cannot be written: `" <> k <> "` is a C++ keyword"))
        [unKey k | (k, _) <- gdFields gd, Set.member (unKey k) cppKeywords]
    checkCollisions named =
      mapM_
        (\(n, owners) -> complain ("the generated C++ name " <> n <> " would name both " <> T.intercalate " and " owners))
        [(n, os) | (n, os) <- Map.toList (Map.fromListWith (flip (<>)) [(n, [o]) | (n, o) <- named]), length os > 1]

-- | The qualified C++ name of a generated type (GEN-4).
qualifiedCppName :: TypeHome -> Text
qualifiedCppName h = "::mlc::types::" <> T.intercalate "::" (typeHomePath h <> [typeHomeName h])

-- | Where a module's generated header lives below @types/cpp@, which is how
-- a sourced file includes it (GEN-6).
cppHeaderPath :: [Text] -> Text
cppHeaderPath p = "mlc/types/" <> T.intercalate "/" p <> ".hpp"

-- | The names in a rendered C++ type: identifiers with their @::@
-- qualifiers, and numbers.
cppTokens :: Text -> [Text]
cppTokens = filter (not . T.null) . T.split (\c -> not (DC.isAlphaNum c || c == '_' || c == ':'))

-- | Whether a rendered field type can be written in a header that comes
-- before every sourced file: each name in it is a builtin, from the
-- standard library, or one of the given names (generated types and the
-- definition's own parameters).
headerSafe :: Set.Set Text -> Text -> Bool
headerSafe known = all ok . cppTokens
  where
    ok t
      | Set.member t known = True
      | "::mlc::types::" `T.isPrefixOf` t = False
      | "std::" `T.isPrefixOf` t = Set.member t cppStdTypeNames
      | T.all DC.isDigit t = True
      | otherwise = Set.member t cppBuiltinTypeWords

cppBuiltinTypeWords :: Set.Set Text
cppBuiltinTypeWords =
  Set.fromList
    [ "int", "double", "float", "bool", "char", "long", "short", "unsigned", "signed", "void", "const"
    , "size_t", "int8_t", "int16_t", "int32_t", "int64_t", "uint8_t", "uint16_t", "uint32_t", "uint64_t"
    , "char8_t", "char16_t", "char32_t", "wchar_t"
    ]

-- | The standard names a generated header may use: those it knows a header
-- for ('cppStdHeaderOf').
cppStdTypeNames :: Set.Set Text
cppStdTypeNames = Set.filter ("std::" `T.isPrefixOf`) (Map.keysSet cppStdHeaderOf)

-- | The standard header declaring each standard name a generated header may
-- use, and each fixed-width integer name it may write unqualified.
cppStdHeaderOf :: Map.Map Text Text
cppStdHeaderOf =
  Map.fromList $
    [ ("std::" <> n, h)
    | (h, ns) <-
        [ ("string", ["string"])
        , ("vector", ["vector"])
        , ("array", ["array"])
        , ("deque", ["deque"])
        , ("list", ["list"])
        , ("map", ["map", "multimap"])
        , ("set", ["set", "multiset"])
        , ("unordered_map", ["unordered_map"])
        , ("unordered_set", ["unordered_set"])
        , ("tuple", ["tuple"])
        , ("utility", ["pair", "move", "forward"])
        , ("optional", ["optional"])
        , ("variant", ["variant", "monostate"])
        , ("memory", ["shared_ptr", "unique_ptr", "make_shared"])
        , ("type_traits", ["is_same_v"])
        , ("functional", ["function"])
        , ("cstddef", ["size_t", "ptrdiff_t", "nullptr_t", "byte"])
        , ("cstdint", fixedWidth)
        ]
    , n <- ns
    ]
      <> [(n, "cstdint") | n <- fixedWidth]
      <> [("size_t", "cstddef")]
  where
    fixedWidth = ["int8_t", "int16_t", "int32_t", "int64_t", "uint8_t", "uint16_t", "uint32_t", "uint64_t"]

-- | Words a generated namespace component may not be (GEN-5): C++
-- keywords, and the namespaces a generated header names without
-- qualification, which a namespace of that name would capture.
cppReservedComponents :: Set.Set Text
cppReservedComponents = Set.insert "std" (Set.insert "mlc" cppKeywords)

-- | C++ keywords, which no generated field may be (GEN-5).
cppKeywords :: Set.Set Text
cppKeywords =
  Set.fromList
    [ "alignas", "alignof", "and", "and_eq", "asm", "auto", "bitand", "bitor", "bool", "break", "case"
    , "catch", "char", "char8_t", "char16_t", "char32_t", "class", "compl", "concept", "const", "consteval"
    , "constexpr", "constinit", "const_cast", "continue", "co_await", "co_return", "co_yield", "decltype"
    , "default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit", "export", "extern"
    , "false", "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable", "namespace"
    , "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private", "protected"
    , "public", "register", "reinterpret_cast", "requires", "return", "short", "signed", "sizeof"
    , "static", "static_assert", "static_cast", "struct", "switch", "template", "this", "thread_local"
    , "throw", "true", "try", "typedef", "typeid", "typename", "union", "unsigned", "using", "virtual"
    , "void", "volatile", "wchar_t", "while", "xor", "xor_eq"
    ]

-- | The struct, the marshaller signatures, and the marshaller definitions
-- for every generated record, returned apart so the signatures can precede
-- every definition in the pool.
-- | The generated records of the pool (GEN-9): the definition of each one
-- its module's header does not hold, in the pool after the sourced files
-- (GEN-10); the declaration of every record's marshalling node; and, late,
-- the conversions of its fields (GEN-14) and the node's steps.
generateRecords :: LowerConfig CppTranslatorM -> [SerialAST] -> CppTranslator ([MDoc], [MDoc], [MDoc])
generateRecords cfg asts = do
  records <- CMS.gets translatorRecords
  names <- CMS.gets translatorRecordNames
  public <- CMS.gets translatorPublic
  handles <- CMS.gets translatorHandles
  let objects = unique [o | a <- asts, o <- generatedObjectsIn a]
  fieldModes <- concat <$> mapM objectConvs objects
  let convs = [(gv, rt, i, doc) | (gv, rt, i, _, _, Just doc) <- fieldModes]
      -- An instance of a template whose field converts at some instance
      -- says so for this one too, so a missed instance does not compile.
      noConvs =
        unique
          [ render (CP.printFieldNoConv rt i)
          | (gv, rt, i, _, _, Nothing) <- fieldModes
          , Just gd <- [Map.lookup gv records]
          , not (null (nativeParams gd))
          , Set.member (gv, i) convertingFields
          ]
      convertingFields = Set.fromList [(g, j) | (g, _, j, _) <- convs]
  -- Two uses that are one C++ type must convert a field the same way, and
  -- writing a field as it is held is one of those ways: a conversion
  -- specialised for the type applies to every use of it. Two fields convert
  -- the same way when their serial forms are one.
  let byKey = Map.fromListWith (flip (<>)) [((render rt, i), [((gv, k), (fa, doc))]) | (gv, rt, i, k, fa, doc) <- fieldModes]
  sequence_
    [ complainCpp
        ( "the generated C++ type " <> rt <> " is used at two morloc types that convert its field `"
            <> unKey k <> "` differently, so one C++ type cannot hold both; give " <> unTVar gv
            <> " a C++ form, `record Cpp => " <> unTVar gv <> " = \"...\"`"
        )
    | ((rt, _), docs@(((gv, k), _) : _)) <- Map.toList byKey
    , length (unique [if isJust doc then Just fa else Nothing | (_, (fa, doc)) <- docs]) > 1
    ]
  let specs = unique ([render doc | (_, _, _, doc) <- convs] <> noConvs)
      converting = Map.fromListWith Set.union [(gv, Set.singleton i) | (gv, _, i, _) <- convs]
      nameOf gv = pretty (Map.findWithDefault (unTVar gv) gv names)
      paramsOf gd = map (pretty . nativeParam) (nativeParams gd)
      rtypeOf gd = nameOf (gdVar gd) <> CP.printRecordTemplate (paramsOf gd)
      modeOf gd i
        | Set.member i (Map.findWithDefault Set.empty (gdVar gd) converting) =
            if null (nativeParams gd) then CP.AlwaysConv else CP.ConvByInstance
        | otherwise = CP.NoConv
  private <- privateDecls [gd | gd <- Map.elems records, Map.notMember (gdVar gd) public]
  let fwds = [CP.printMarshalDecls (paramsOf gd) (rtypeOf gd) | gd <- Map.elems records]
      steps =
        [ CP.printRecordMarshallers (Map.member (gdVar gd) handles) (paramsOf gd) (rtypeOf gd)
            [(pretty k, modeOf gd i) | (i, (k, _)) <- indexed (gdFields gd)]
        | gd <- Map.elems records ]
  return (private, fwds, map pretty specs <> steps)
  where
    complainCpp msg = CMS.modify $ \st -> st {translatorErrors = msg : translatorErrors st}

    -- Every generated record a value of this type holds, at any depth.
    generatedObjectsIn :: SerialAST -> [SerialAST]
    generatedObjectsIn a = [o | o <- serialSubtrees a, generatedObject o]

    -- The conversion of each field of one instance that is not written as
    -- it is held: a packed value or a closure outside any nested
    -- generated record, which converts itself.
    objectConvs obj@(SerialObject _ (FV gv _) _ _ rs) = do
      rt <- cppTypeOf (serialAstToNativeType obj)
      mapIndexedM (\i (k, fa) -> (\d -> (gv, rt, i, k, fa, d)) <$> fieldConv rt i fa) rs
    objectConvs _ = return []

    fieldConv rt i fa
      | not (fieldConverts fa) = return Nothing
      | otherwise = do
          (toStmts, toE) <- nativeToWire cfg "mlc_x" fa
          (fromE, fromStmts) <- wireToNative cfg "mlc_r" fa
          raw <- maybe (cppTypeOf (unpackedAsTypeF (wireSerialAstToType cppClosureWireLeaf fa))) (return . renderIType)
                   =<< lcRawDeserialAstType cfg fa
          native <- cppTypeOf (serialAstToNativeType fa)
          return . Just $
            CP.printFieldConv rt i native raw
              (map CP.printStmt toStmts, CP.printExpr toE)
              (map CP.printStmt fromStmts, CP.printExpr fromE)

    -- The definitions of records the header does not hold, each in its
    -- pool-private namespace, ordered so that a record follows those it
    -- holds by value.
    privateDecls gds = do
      names <- CMS.gets translatorRecordNames
      handles <- CMS.gets translatorHandles
      let split gd =
            let q = Map.findWithDefault (unTVar (gdVar gd)) (gdVar gd) names
                (ns, leaf) = T.breakOnEnd "::" (fromMaybe q (T.stripPrefix "::" q))
             in (fromMaybe ns (T.stripSuffix "::" ns), leaf)
          inNs gd doc = let (ns, _) = split gd in vsep ["namespace" <+> pretty ns <+> "{", doc, "}"]
          leafOf gd = pretty (snd (split gd))
          handle gd = Map.member (gdVar gd) handles
          structOf gd = if handle gd then leafOf gd <> "_fields" else leafOf gd
          params gd = map (pretty . nativeParam) (nativeParams gd)
      decls <- mapM (\gd -> (\fs -> (gd, CP.printStructTypedef (params gd) (structOf gd) fs, map snd fs)) <$> mapM (\(k, t) -> (,) (pretty k) <$> cppTypeOf t) (gdFields gd)) gds
      let byName = Map.fromList [(Map.findWithDefault "" (gdVar gd) names, gdVar gd) | gd <- gds]
          ordered =
            Graph.flattenSCCs . Graph.stronglyConnComp $
              [ (inNs gd decl, gdVar gd, [d | tok <- concatMap (cppTokens . render) ts, Just d <- [Map.lookup tok byName], d /= gdVar gd, Map.notMember d handles])
              | (gd, decl, ts) <- decls ]
      return $
        [inNs gd (hsep (filter (not . T.null . render) [CP.printTemplateHeader (params gd), "struct" <+> structOf gd <> ";"])) | gd <- gds]
          <> [inNs gd (CP.printHandleDecl (params gd) (leafOf gd)) | gd <- gds, handle gd]
          <> ordered
          <> [inNs gd (CP.printHandleDefs [] (leafOf gd)) | gd <- gds, handle gd, null (params gd)]

generateSourcedSerializers ::
  Map.Map Lang Scope ->
  [SerialManifold] -> -- all segments that can be called in this pool
  CppTranslator
    ( [MDoc]
    , [MDoc]
    , [MDoc]
    )
generateSourcedSerializers univeralScopeMap es0 = do
  scope <- case Map.lookup cppLang univeralScopeMap of
    (Just scope) -> return scope
    Nothing -> return Map.empty

  -- Marshallers are emitted only for the record types this pool uses: the
  -- named records referenced in its manifolds, closed under the records
  -- their fields name. A marshaller for a record no manifold references
  -- would name a struct whose header no source pulls in.
  let keep = closeRecordDeps scope (Set.unions (map (collectNamedRecordTVars) es0))
      typedef = Map.filterWithKey (\k entries -> Set.member k keep && not (null entries)) scope

  foldl groupTriple ([], [], []) . concat . Map.elems <$> Map.mapWithKeyM (makeSerials scope) typedef
  where
    -- Fixed-point close a seed set of TVars over Scope-entry field
    -- references. Overapproximates: walks every TypeU subterm and collects
    -- any TVar reference. Non-record TVars naturally drop out of the emitted
    -- output because makeSerial only produces a serializer for NamU entries
    -- whose head TVar is neither "struct" nor "arrow".
    closeRecordDeps :: Scope -> Set.Set TVar -> Set.Set TVar
    closeRecordDeps s = fixSet
      where
        fixSet acc =
          let extra = Set.unions
                [ tvarsInType body
                | v <- Set.toList acc
                , (_, body, _, _, _) <- fromMaybe [] (Map.lookup v s)
                ]
              acc' = Set.union acc extra
          in if Set.size acc' == Set.size acc then acc else fixSet acc'

        tvarsInType :: TypeU -> Set.Set TVar
        tvarsInType (VarU v) = Set.singleton v
        tvarsInType (NamU _ v ps rs) =
          Set.insert v (Set.unions (map tvarsInType ps ++ map (tvarsInType . snd) rs))
        tvarsInType (AppU t ts) = Set.unions (map tvarsInType (t : ts))
        tvarsInType (FunU ts t) = Set.unions (map tvarsInType (t : ts))
        tvarsInType (ForallU _ t) = tvarsInType t
        tvarsInType (EffectU _ t) = tvarsInType t
        tvarsInType (OptionalU t) = tvarsInType t
        tvarsInType (OpU _ ts) = Set.unions (map tvarsInType ts)
        tvarsInType (LabeledU _ t) = tvarsInType t
        -- A `data` body is its constructor table; the references to close
        -- over are the constructors' field types.
        tvarsInType t
          | Just cs <- dataBodyCtors t = Set.unions [tvarsInType a | (_, as) <- cs, a <- as]
        tvarsInType _ = Set.empty

    groupTriple :: ([a], [a], [a]) -> (a, a, a) -> ([a], [a], [a])
    groupTriple (xs, ys, zs) (x, y, z) = (x : xs, y : ys, z : zs)

    makeSerials ::
      Scope -> TVar -> [([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind)] -> CppTranslator [(MDoc, MDoc, MDoc)]
    makeSerials s v xs = catMaybes <$> mapM (makeSerial s v) xs

    -- For each sourced record: the marshalling node, the write step, and
    -- the read and size steps. The node goes out before any step body, so
    -- a body that reaches this record -- a constructor payload holding it,
    -- say -- finds a complete class to name.
    makeSerial ::
      Scope -> TVar -> ([Either (TVar, Kind) TypeU], TypeU, ArgDoc, Bool, TypedefKind) -> CppTranslator (Maybe (MDoc, MDoc, MDoc))
    makeSerial _ _ (_, NamU _ (TV "struct") _ _, _, _, _) = return Nothing
    makeSerial _ _ (_, NamU _ (TV "arrow") _ _, _, _, _) = return Nothing
    -- A record with a function field is marshalled through the shared engine
    -- (its closures reified/reflected in place, the record itself typed as its
    -- wire tuple); its native struct (de)serializer is unused, and showDefType
    -- cannot render a function field, so skip generating one.
    makeSerial scope _ (_, NamU _ _ _ rs, _, _, _)
      | any (holdsClosure . typeOf . evaluateTypeU scope . snd) rs = return Nothing
    makeSerial scope _ (ps, NamU _ (TV v) _ rs, _, _, _) = do
      let selfName = TV v
          -- The struct's own name is needed by showDefType so a `?T`
          -- field referring back to T can be emitted as
          -- `optional<unique_ptr<T>>` (matching the voidstar wire
          -- format's relptr-based Optional and sidestepping C++'s
          -- incomplete-type rule for `optional<T>` inside T's body).
          --
          -- Each type parameter fills one positional slot of the concrete
          -- name's @$N@ macro; kind parameters (Nat, Str, Rec, ...) fill
          -- none. Left positions become template type variables (T<a>);
          -- Right positions are concrete bindings selected by
          -- @selectSpecialization@ (e.g. a @record Cpp => Foo Int@ entry)
          -- and render through showDefType to their C++ name.
          isTypeParam (Left (_, k)) = k == KindType
          isTypeParam (Right t) = isNothing (kindOfU t)
          typeParamSlots = filter isTypeParam ps
          paramAt (Left (p, _)) = "T" <> pretty p
          paramAt (Right t) = showDefType selfName ps (typeOf (evaluateTypeU scope t))
          allParams = map paramAt typeParamSlots
          -- templateTerms drives @template <class T...>@. Only Left
          -- type-kinded positions declare template parameters; Right
          -- positions are already-bound concrete types and must not
          -- appear in the template header.
          templateTerms = ["T" <> pretty p | Left (p, KindType) <- ps]
          -- When the concrete name carries `$N` macros (e.g.
          -- `"container_t<$1>"`), the macros are filled positionally
          -- by allParams (template names for Left, concrete names for
          -- Right). Otherwise the bare name gets a conventional
          -- `<a0, ...>` appended. Without this branching, a templated
          -- concrete name would lower to `container_t<$1><T0>` and the
          -- serializer signature would fail to compile.
          rtype = renderTemplatedType v allParams
          rs' = map (second (evaluateTypeU scope)) rs
          fields = [(pretty k, showDefType selfName ps (typeOf t)) | (k, t) <- rs']
          fwd = CP.printMarshalDecls templateTerms rtype
          serializer = CP.printSerializer templateTerms rtype fields
          deserializer = CP.printDeserializer templateTerms rtype fields
      return $ Just (fwd, serializer, deserializer)
    makeSerial _ _ _ = return Nothing

    evaluateTypeU :: Scope -> TypeU -> TypeU
    evaluateTypeU scope t = case TE.evaluateType scope t of
      (Left e) -> error $ show e
      (Right t') -> t'

    -- True when @t@ is an immediate reference back to the enclosing
    -- struct's name. Used to decide whether an `?T` field needs pointer
    -- indirection at C++ emission. Walks through aliasing shapes (VarT
    -- and an already-expanded NamT) but not through containers --
    -- a `?[T]` field is fine because list-of-incomplete is allowed
    -- and the recursion is broken by the vector indirection.
    isRecursiveRef :: TVar -> Type -> Bool
    isRecursiveRef self (VarT v) = v == self
    isRecursiveRef self (NamT _ v _ _) = v == self
    isRecursiveRef _ _ = False

    showDefType :: TVar -> [Either (TVar, Kind) TypeU] -> Type -> MDoc
    showDefType _ ps (UnkT v)
      | any (\p -> either (\(tv, _) -> tv == v) (const False) p) ps = "T" <> pretty v
      | otherwise = pretty v
    showDefType _ ps (VarT v)
      | any (\p -> either (\(tv, _) -> tv == v) (const False) p) ps = "T" <> pretty v
      | otherwise = pretty v
    showDefType _ _ (FunT _ _) = error "Cannot serialize functions"
    -- Render a NamT with its template arguments. When the concrete name
    -- contains `$N` placeholders (e.g. `"container_t<$1>"`), route through
    -- the macro expander so the args fill the placeholders. Otherwise,
    -- append `<arg0, ...>` via `printRecordTemplate`. Without this, a
    -- recursive `?(Container a)` field would lower to a bare
    -- `std::shared_ptr<container_t>` (no template args) and the generated
    -- deserializer would fail to compile.
    showDefType self ps (NamT _ (TV v) args _) =
      let typeArgs = tyArgs args
          argDocs = [showDefType self ps a | a <- typeArgs]
      in renderTemplatedType v argDocs
    showDefType self ps (AppT (VarT (TV v)) ts) =
      let typeTs = tyArgs ts
      in pretty $ expandMacro v (map (render . showDefType self ps) typeTs)
    showDefType _ _ (AppT _ _) = error "AppT is only OK with VarT, for now"
    showDefType _ _ (EffectT _ _) = error "Cannot serialize suspensions"
    showDefType self ps (OptionalT t)
      | isRecursiveRef self t =
          "std::shared_ptr<" <> showDefType self ps t <> ">"
      | otherwise =
          "std::optional<" <> showDefType self ps t <> ">"

-- C++ specific source handling (flags, headers, libraries)

handleFlagsAndPaths :: [Source] -> MorlocMonad ([Source], [Text], [Path])
handleFlagsAndPaths srcs = do
  state <- MM.get
  let gccversion = gccVersionFlag . foldl max 0 . map packageCppVersion $ statePackageMeta state
  let explicitLibs = map ("-l" <>) . unique . concatMap packageDependencies $ statePackageMeta state
  let userCxxFlags = unique . concatMap packageCxxFlags $ statePackageMeta state
  -- Raw `-X cpp:flags=...` (and the build-config default) passthrough: ordered,
  -- verbatim, never deduplicated, appended after the package flags. Kept out of
  -- `unique` because flag order and adjacency are load-bearing.
  let cliCxxFlags = BP.lookupFlags "cpp" (stateLangParams state)
  -- Collect sources belonging to this pool: cpp sources and any guest member
  -- whose pool host is cpp (e.g. futhark glue headers, srcLang=futhark). Their
  -- includes/-I flags must reach the cpp pool build.
  (srcs', libflags, paths) <-
    fmap unzip3
      . mapM (flagAndPath (stateLangRegistry state))
      . unique
      $ [s | s <- srcs, LR.poolOf (stateLangRegistry state) (srcLang s) == cppLang]

  -- Include directories for every C++ source any imported module declares,
  -- whether or not this program realizes its code. The pool's own `#include`
  -- lines are absolute, so these directories exist solely to resolve the bare
  -- includes inside a user's header; which of a module's functions this
  -- program happens to call must not decide whether such an include resolves.
  moduleDirs <- cppModuleIncludeDirs (stateLangRegistry state)
    . unique
    . concat
    . GMap.elems
    $ stateSources state

  home <- MM.asks configHome
  stateDir <- MM.asks configState
  let -- Search the runtime include dir (home) and the environment's shared C++
      -- module prefix (state/modules/include), where a user-installed library's
      -- headers live.
      mlcInclude = ["-I" <> MS.shellQuote (home <> "/include"), "-I" <> MS.shellQuote (stateDir <> "/modules/include")]
      mlcPch = ["-include", "morloc_pch.hpp"]
      -- No runtime rpath to home/lib: the pool is relocatable and finds
      -- libmorloc via LD_LIBRARY_PATH exported by the nexus at launch, which
      -- guarantees it loads the same libmorloc the nexus resolved. The
      -- state/modules/lib search dir covers a user library referenced by a
      -- `-l` from dependencies/cxx-flags; its runtime load is likewise handled
      -- by the nexus LD_LIBRARY_PATH export, not a baked rpath.
      -- -rdynamic exports the pool's own symbols, so the crash handler's
      -- backtrace names generated manifolds rather than printing offsets.
      mlcLib = ["-L" <> MS.shellQuote (home <> "/lib"), "-L" <> MS.shellQuote (stateDir <> "/modules/lib"), "-lmorloc", "-lcppmorloc", "-lpthread", "-rdynamic"]

  return
    ( filter (isJust . srcPath) srcs'
    , [gccversion] <> explicitLibs <> userCxxFlags <> cliCxxFlags ++ (map MT.pack . concat) (mlcPch : mlcInclude : mlcLib : libflags)
    , unique (catMaybes paths <> moduleDirs)
    )

-- | Directories holding the C++ sources declared by the program's modules.
--
-- Unlike 'flagAndPath' this only resolves a location and never fails: a module
-- may declare a header this build has no use for, and merely enumerating
-- candidate include directories must not turn a working build into an error.
-- Contributes no linker flags, so a module whose code is never called is
-- searched for headers but not linked against.
cppModuleIncludeDirs :: LR.LangRegistry -> [Source] -> MorlocMonad [Path]
cppModuleIncludeDirs reg = fmap catMaybes . mapM dirOf . filter isCpp
  where
    isCpp s = LR.poolOf reg (srcLang s) == cppLang

    dirOf Source {srcPath = Just p} =
      case (MS.takeDirectory p, MS.dropExtensions (MS.takeFileName p)) of
        -- A bare header name resolves through the same search path the
        -- compiled sources use.
        (".", base) -> do
          home <- MM.asks configHome
          stateDir <- MM.asks configState
          found <-
            liftIO . fmap catMaybes . mapM getFile $
              getHeaderPaths home stateDir base [".h", ".hpp", ".hxx"]
          case found of
            (x : _) -> Just . MS.takeDirectory <$> liftIO (MS.canonicalizePath x)
            [] -> return Nothing
        (dir, _) -> do
          exists <- liftIO $ MS.doesDirectoryExist dir
          if exists
            then Just <$> liftIO (MS.canonicalizePath dir)
            else return Nothing
    dirOf _ = return Nothing

-- | The provisional spelling of a standard newer than C++20 (c++2b for 23,
-- c++2c for 26) is accepted by every compiler that implements it, while the
-- final spelling arrived later: Apple clang 15 rejects -std=c++23.
gccVersionFlag :: Int -> Text
gccVersionFlag i
  | i <= 20 = "-std=c++20"
  | i == 23 = "-std=c++2b"
  | i == 26 = "-std=c++2c"
  | otherwise = "-std=c++" <> MT.show' i

flagAndPath :: LR.LangRegistry -> Source -> MorlocMonad (Source, [String], Maybe Path)
flagAndPath reg src@Source {srcLang = srcL, srcPath = Just p} | LR.poolOf reg srcL == cppLang =
  case (MS.takeDirectory p, MS.dropExtensions (MS.takeFileName p), MS.takeExtensions p) of
    (".", base, "") -> do
      header <- lookupHeader base
      libFlags <- lookupLib base
      return (src {srcPath = Just header}, libFlags, Just (MS.takeDirectory header))
    (dir, base, _) -> do
      libFlags <- lookupLib base
      absDir <- liftIO $ MS.canonicalizePath dir
      absPath <- liftIO $ MS.canonicalizePath p
      exists <- liftIO $ MS.doesFileExist absPath
      unless exists . MM.throwRejected $
        "Source file not found:" <+> pretty absPath
      incPath <- poolRelative absPath
      return (src {srcPath = Just incPath}, libFlags, Just absDir)
  where
    -- A header generated inside the pool's own directory (guest glue under
    -- <pool>/cpp-guests/) is included by its pool-relative path: the pool
    -- source then holds no build-time staging path, so an unchanged program
    -- rebuilds to identical bytes (the pool hash keys the runtime cache) and
    -- the include still resolves after the staging tree is swapped into
    -- place. Every other source keeps its absolute path.
    poolRelative :: Path -> MorlocMonad Path
    poolRelative absPath = do
      mPools <- stagingPoolsDir
      case mPools of
        Nothing -> return absPath
        Just pools -> do
          poolDir <- liftIO $ MS.canonicalizePath (pools </> ML.poolDirKey cppLang)
          return (MS.makeRelative poolDir absPath)

    lookupHeader :: String -> MorlocMonad Path
    lookupHeader base = do
      home <- MM.asks configHome
      state <- MM.asks configState
      let allPaths = getHeaderPaths home state base [".h", ".hpp", ".hxx"]
      existingPaths <- liftIO . fmap catMaybes . mapM getFile $ allPaths
      case existingPaths of
        (x : _) -> liftIO $ MS.canonicalizePath x
        [] -> MM.throwRejected $ "Header file " <> pretty base <> ".* not found"

    lookupLib :: String -> MorlocMonad [String]
    lookupLib base = do
      home <- MM.asks configHome
      state <- MM.asks configState
      let libnamebase = filter DC.isAlphaNum (map DC.toLower base)
      let libname = P.sharedLibName P.hostPlatform libnamebase  -- lib<name>.so | .dylib
      let allPaths = getLibraryPaths home state base libname
      existingPaths <- liftIO . fmap catMaybes . mapM getFile $ allPaths
      moduleLibDir <- liftIO . MS.canonicalizePath $ MS.joinPath [state, "modules", "lib"]
      case existingPaths of
        (libpath : _) -> do
          libdir <- liftIO . MS.canonicalizePath . MS.takeDirectory $ libpath
          -- A lib in the shared module prefix is found at run time via the
          -- nexus-exported LD_LIBRARY_PATH, so it needs no baked rpath -- and an
          -- absolute rpath there would pin the pool to the build-time state
          -- path, breaking relocation. A lib elsewhere (a module's own
          -- state/src tree) is not on that export, so it keeps the rpath.
          -- Comma form (`-Wl,-rpath,DIR`), accepted by both GNU ld and macOS ld64
          -- (which rejects the GNU `-Wl,-rpath=DIR` form).
          let rpathFlags = ["-Wl,-rpath," <> MS.shellQuote libdir | libdir /= moduleLibDir]
          return $ rpathFlags <> ["-L" <> MS.shellQuote libdir, "-l" <> libnamebase]
        [] -> return []
flagAndPath reg src@Source {srcLang = srcL, srcPath = Nothing} | LR.poolOf reg srcL == cppLang = return (src, [], Nothing)
flagAndPath _ _ = MM.throwCompilerBug $ "flagAndPath should only be called for C++ functions"

getFile :: Path -> IO (Maybe Path)
getFile x = do
  exists <- MS.doesFileExist x
  return $
    if exists
      then Just x
      else Nothing

-- `home` is the immutable runtime root (runtime headers/libs); `state` is the
-- mutable state root where installed module resources live
-- (state/modules/include/<mod>) and the plane source (state/src/<mod>).
getHeaderPaths :: Path -> Path -> String -> [String] -> [Path]
getHeaderPaths home state base exts = [path <> ext | path <- paths, ext <- exts]
  where
    paths =
      map
        MS.joinPath
        [ [base]
        , ["include", base]
        , [base, base]
        , [home, "include", base]
        , [state, "modules", "include", base]
        , [state, "src", base, base]
        , ["/usr/include", base]
        , ["/usr/local/include", base]
        ]

getLibraryPaths :: Path -> Path -> String -> String -> [Path]
getLibraryPaths home state base sofile =
  map
    MS.joinPath
    [ [sofile]
    , ["lib", sofile]
    , [base, sofile]
    , [home, "lib", sofile]
    , [state, "modules", "lib", sofile]
    , [state, "src", base, sofile]
    , [state, "src", base, "lib", sofile]
    ]

-- | Whether a type holds a function value anywhere: a closure of any
-- arity, a suspension among them. A record holding one is serialized
-- structurally, each closure reified in place, not by a generated struct
-- (de)serializer.
holdsClosure :: Type -> Bool
holdsClosure t = containsFunT t || containsEffectT t

-- | C++ locals take no prefix: C++ reserves none (model/runtime/issues/binder-names.md).
cppLocals :: Text
cppLocals = ""
