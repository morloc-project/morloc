{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Rust
Description : Translate 'SerialManifold' trees into Rust pool source code
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The Rust CAbi pool member: a full member with its own native marshaller
(@rustmorloc@) and dispatch, mirroring the C++ member but far simpler for v1.
Lowers each 'SerialManifold' through the shared 'LowerConfig'/'Imperative' IR,
then prints via 'RustPrinter'. Concrete Rust type names live in the morloc
stdlib as @type Rust => X = "..."@ declarations and reach the translator
pre-resolved in each 'TypeF' CVar slot, so 'rustTypeOf' mostly renders them
directly (mirroring 'cppTypeOf').

Scope: scalars, Str, Vector, tuples, @?T@, records (including recursive),
cross-pool foreign calls, closures/partial application (in-pool capture,
records of functions, and cross-pool defunctionalization -- reify and reflect).
Pattern evaluation is partial (struct getters and bracket index/slice). Remote
calls, manifold caching, the file/stream IO intrinsics, and records with
custom-packer fields crossing a boundary are not yet implemented.
-}
module Morloc.CodeGenerator.Pools.CAbi.Members.Rust
  ( translate
  , rustLang
  ) where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import qualified Control.Monad.State.Strict as CMS
import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.CodeGenerator.Grammars.Common
import Morloc.CodeGenerator.LogTemplate (RenderedTemplate (..), collectRenderedTemplates)
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import Morloc.CodeGenerator.Grammars.Translator.Imperative
  ( ArgSite (..)
  , IOwnership (..)
  , LowerConfig (..)
  , buildProgramM
  , defaultDeserialize
  , defaultFoldRules
  , defaultSerialize
  , toIType
  )
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.PoolHash as PH
import qualified Morloc.CodeGenerator.Pools.CAbi.Members.RustPrinter as RP
import Morloc.CodeGenerator.Serial (containsFunF, serialAstToMsgpackSchema, serialAstToNativeType, serialAstToType, shallowType, wireSerialAstToType)
import Morloc.Typecheck.Internal (unqualify)
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS
import qualified Morloc.Version as MV
import Morloc.Quasi
import System.Directory (findExecutable)

-- | Duplicated here (as in Cpp.hs) to match data/lang/rust/lang.yaml. The
-- second field is the source extension and must match lang.yaml's @extension@
-- so pool naming (@pool.rs@) and the registry's Lang agree.
rustLang :: ML.Lang
rustLang = ML.Lang "rust" "rs"

data RustState = RustState
  { rsCounter :: Int
  , rsSchemas :: Map.Map Text Int
  , rsLocalSet :: Set.Set Int
  , rsRemoteSet :: Set.Set Int
  , rsDebugInfo :: Int -> (Text, Text)
  -- ^ Per-manifold @(userName, srcloc)@ (from 'makeManifoldDebugInfoLookup'),
  -- baked into each manifold's 'FrameGuard' frame line for error tracebacks.
  , rsLogTemplates :: Map.Map Int RenderedTemplate
  -- ^ Rendered @log:@ / @benchmark:@ templates per labeled manifold midx.
  -- Drives the 'rustmorloc::LogGuard' wrap in 'rustMakeFunction'.
  , rsRecmap :: RecMap
  -- ^ Unified record types used in this pool; drives struct generation, the
  -- concrete struct name in 'rustTypeOf', and per-record marshalling impls.
  , rsCScope :: Scope
  -- ^ The merged Rust concrete typedef scope; resolves a recursive back-ref
  -- (@RecF@) to its concrete struct name (as C++'s translatorCScope does).
  , rsThinSinks :: Set.Set Text
  -- ^ Closure manifolds that need no trait object: see 'thinSinkNames'.
  , rsSrcTypeVarMask :: Map.Map SrcName [(Bool, Bool)]
  -- ^ Per sourced function, per parameter position: @(isBareTypeVar,
  -- isFunctionParam)@ from the declared morloc signature. @isBareTypeVar@: the
  -- parameter is a BARE type variable, so it is passed by reference (@&A@) even
  -- at a Copy instantiation (the sourced Rust fn is generic over @&A@).
  -- @isFunctionParam@: the parameter is itself function-typed (@F: Fn@), so a
  -- function-valued argument is a genuine closure passed BY VALUE; when False,
  -- a function-typed argument is a fully-applied sub-manifold VALUE, passed like
  -- data (borrowed when non-'Copy').
  , rsReifyInfo :: Map.Map Text (Int, [(Int, SerialAST)])
  -- ^ Per crossing-closure body-manifold name: @(mid, capturedSchemaIds)@.
  -- Only closures that reach a serialize boundary appear here (the rest stay
  -- thin @impl Fn@/@Rc@ with no reify cost). Drives 'rustClosureWrapper' to
  -- build the closure's @(home pool, mid, captured)@ origin lazily.
  }

instance Defaultable RustState where
  defaultValue = RustState 0 Map.empty Set.empty Set.empty (\_ -> ("", "")) Map.empty [] Map.empty Set.empty Map.empty Map.empty

-- | The ownership environment: the borrowed (@&T@) parameter indices of the
-- manifold whose body is currently being lowered ('oeCurrent') and of its
-- enclosing caller ('oeParent'). This is a lexically scoped Reader environment,
-- extended by 'local' at each manifold boundary -- NOT mutable state -- so a
-- variable's ownership is a pure function of the lexical context, immune to the
-- fold's evaluation order. A manifold-call argument or captured context
-- argument is named by index-aliasing after the caller's variables, so its
-- ownership is read from 'oeParent' rather than 'oeCurrent'.
data OwnEnv = OwnEnv
  { oeCurrent :: Set.Set Int
  , oeParent :: Set.Set Int
  , oeShared :: Set.Set Int
  -- ^ Indices used at more than one point in the current manifold (the "shared"
  -- set, see 'sharedIndicesSM'). A non-'Copy' shared local must be BORROWED at
  -- reference sinks and CLONED at owned sinks so it survives every use -- never
  -- moved. Kept SEPARATE from 'oeCurrent' (a shared owned local is a value, not
  -- a @&T@ reference: tagging it 'BorrowedRef' would miscompile reference sinks).
  , oeParentShared :: Set.Set Int
  -- ^ The enclosing (caller) manifold's shared set. A captured context argument
  -- names a caller variable, so a SHARED non-'Copy' capture must be cloned into
  -- the @move||@ closure (the move happens at closure formation, so the clone is
  -- hoisted before it) to leave the original live for the caller's other uses.
  , oeLoopCarried :: Set.Set Int
  -- ^ The loop-carried variable indices of a native-loop manifold. Each is an
  -- owned native local deserialized once at entry (the carried slot is forced
  -- 'NativeContent' so 'letWrap' emits a deserialize let) and reassigned by the
  -- loop's continue, so its entry let is emitted @let mut@ ('rustMakeLet') and it
  -- is unioned into 'oeShared' so the body borrows/clones but never moves it.
  }

emptyOwnEnv :: OwnEnv
emptyOwnEnv = OwnEnv Set.empty Set.empty Set.empty Set.empty Set.empty

type RustM = ReaderT OwnEnv (CMS.StateT RustState Identity)

getCounter :: RustM Int
getCounter = do
  s <- CMS.get
  CMS.put s {rsCounter = rsCounter s + 1}
  return (rsCounter s)

resetCounter :: RustM ()
resetCounter = CMS.modify $ \s -> s {rsCounter = 0}

rustRegisterSchema :: Text -> RustM Int
rustRegisterSchema schema = do
  s <- CMS.get
  case Map.lookup schema (rsSchemas s) of
    Just sid -> return sid
    Nothing -> do
      let sid = Map.size (rsSchemas s)
      CMS.put s {rsSchemas = Map.insert schema sid (rsSchemas s)}
      return sid

getRustSchemaTable :: RustM [Text]
getRustSchemaTable = do
  m <- CMS.gets rsSchemas
  return $ map fst $ sortBy (comparing snd) $ Map.toList m

-- | Render a native (non-serialized) 'TypeF' to a Rust type string. The
-- concrete name has already been resolved into the CVar slot by the Realize
-- pass against the Rust concrete scope, so terminal variables render directly;
-- parameterized types expand their @$N@ macro from the stdlib declaration.
rustTypeOf :: TypeF -> RustM MDoc
rustTypeOf = f
  where
    -- A nominal type's name is its template instantiated with this
    -- occurrence's arguments when the user gave it one (`MyBox<$1>`), and
    -- the name itself otherwise -- a generated type is monomorphic per
    -- instantiation and spells nothing of its arguments. The arguments are
    -- stored positions, so a function among them boxes ('rustFieldType').
    nominalTypeName x ps
      | T.any (== '$') x = do
          let (typeTs, kindCount) = partitionKindArgsF ps
          ts' <- mapM rustFieldType typeTs
          return . pretty $ expandMacro x (map render ts') kindCount
      | otherwise = return (pretty x)

    f :: TypeF -> RustM MDoc
    f (UnkF (FV _ x)) = return (pretty x)
    f (VarF (FV _ x)) = return (pretty x)
    -- An enum lowers to its concrete name; the `#[repr(u8)] enum` that
    -- name refers to is either generated for this pool or supplied by the
    -- user through a `data Rust => X = "..."` mapping.
    f (EnumF (FV _ (CV x)) ps _) = nominalTypeName x ps
    -- A variant lowers to its concrete name; the `enum` carrying the arms
    -- is generated for this pool or supplied by a `data Rust => X = "..."`.
    f (VariantF (FV _ (CV x)) ps _) = nominalTypeName x ps
    f (AppF t ts) = do
      t' <- f t
      let (typeTs, kindCount) = partitionKindArgsF ts
      -- Type arguments are STORED positions (a Vec element, a tuple slot): a
      -- function leaf among them must box to `Rc<dyn MorlocFnN>` (a bare
      -- `impl Fn` is illegal in a container), which is exactly 'rustFieldType'.
      ts' <- mapM rustFieldType typeTs
      return . pretty $ expandMacro (render t') (map render ts') kindCount
    -- A recursive optional (?T where T points back to a containing record)
    -- must break the cycle with Box (an Option<T> of an infinite-size T won't
    -- compile). None == absent matches the voidstar single-relptr Optional.
    f (OptionalF t@(RecF _)) = do
      t' <- f t
      return $ "::std::option::Option<::std::boxed::Box<" <> t' <> ">>"
    f (OptionalF t) = do
      -- The payload is a stored position; a function payload boxes ('rustFieldType').
      t' <- rustFieldType t
      return $ "::std::option::Option<" <> t' <> ">"
    f (NatLitF _) = return mempty
    f NatVoidF = return mempty
    f (StrLitF _) = return mempty
    f StrVoidF = return mempty
    -- A function value inside a type (a field, an element, a payload, the
    -- result of a function) is a trait object; only a manifold's own
    -- parameters and return are `impl` ('rustArgType', 'rustReturnType').
    f (FunF ts t) = rustFieldType (FunF ts t)
    -- Autogenerated record: resolve the concrete struct name from the recmap
    -- (keyed by the record FVar + field keys).
    f (NamF _ v@(FV _ (CV "struct")) _ rs) = do
      recmap <- CMS.gets rsRecmap
      case lookup (v, map fst rs) recmap of
        Just rec -> do
          -- A field whose native and wire types diverge (a custom-packer field)
          -- is a generic parameter of the struct; render its concrete type at
          -- THIS occurrence as a type argument: `Name<Ta, Tb>`.
          params <- mapM f [t | ((_, Nothing), (_, t)) <- zip (recFields rec) rs]
          return $ recName rec <> if null params then "" else "<" <> hcat (punctuate ", " params) <> ">"
        Nothing -> error $ "Rust: record missing from recmap: " <> show v
    -- User-mapped record: the concrete struct name is the CVar text, or the
    -- template it holds instantiated with this occurrence's arguments.
    f (NamF _ (FV _ (CV s)) ps _) = nominalTypeName s ps
    -- Back-reference to a recursive record: resolve the concrete struct name
    -- via the concrete scope (the CVar slot is unreliable after weave).
    f (RecF (FV gv@(TV gvText) (CV cv)))
      | cv /= "struct" && cv /= gvText = return (pretty cv)
      | otherwise = do
          cscope <- CMS.gets rsCScope
          case Map.lookup gv cscope of
            Just ((_, body, _, _, _) : _) | Just name <- bodyName body -> return (pretty name)
            _ -> error $ "Rust: recursive record `" <> T.unpack gvText <> "` has no concrete mapping"


-- | The bare struct name for a record-literal constructor: 'rustTypeOf' with any
-- generic `<..>` parameters stripped. A Rust struct literal takes no type
-- arguments -- they are inferred from the field values -- so a generic
-- (custom-packer) record still constructs as `Name { .. }`, not `Name<T> { .. }`.
rustStructCtor :: TypeF -> RustM MDoc
rustStructCtor recType = do
  full <- rustTypeOf recType
  return $ pretty (RP.stripTypeParams (render full))

-- | Whether a native type is Rust @Copy@ (freely duplicable, so a sourced-call
-- argument is passed by value rather than borrowed). @Copy@: scalar numerics,
-- @bool@, unit, and tuples/optionals whose leaves are all @Copy@. Not @Copy@:
-- @Str@ (String), lists (Vec), records. Must be EXACT: borrowing a @Copy@ value
-- (@&i64@ into an @i64@ parameter) is a type error, so @Copy@-ness is decided
-- from the resolved concrete type name, which is robust to type aliases.
rustIsCopy :: TypeF -> Bool
rustIsCopy (OptionalF t) = rustIsCopy t
-- A `data` type with argument-free constructors is one byte and holds
-- nothing, so it is Copy. A pool-owned enum derives Copy; a user-mapped
-- one (`data Rust => X = "..."`) must derive it too, which is the same
-- shape 'printRustEnum' emits.
rustIsCopy (EnumF _ _ _) = True
rustIsCopy (AppF (VarF (FV (TV gv) _)) ts)
  | T.isPrefixOf "Tuple" gv = all rustIsCopy (fst (partitionKindArgsF ts))
rustIsCopy (VarF (FV _ (CV cv))) = cv `elem` copyScalars
rustIsCopy (UnkF (FV _ (CV cv))) = cv `elem` copyScalars
rustIsCopy _ = False

-- Concrete Rust @Copy@ scalar type names (from the root-rust @type Rust => ...@
-- mappings). A custom mapping to a non-standard @Copy@ type would be treated as
-- non-@Copy@ (borrowed) -- acceptable for the standard numeric/bool set.
copyScalars :: [Text]
copyScalars =
  [ "i8", "i16", "i32", "i64"
  , "u8", "u16", "u32", "u64"
  , "f32", "f64", "bool", "()"
  ]

-- | The Rust type of a manifold argument: a serial arg is an incoming packet
-- pointer; a native arg is its rendered native type.
rustArgType :: TypeM -> RustM MDoc
rustArgType (Serial _) = return "*const u8"
rustArgType Passthrough = return "*const u8"
rustArgType (Native t) = rustTypeOf (typeFof t)
-- A function-typed parameter is a function value like any other, so it has
-- the one spelling: a trait object, taken by reference ('rustArgIsRef').
rustArgType (Function ts t) = rustStoredType (Function ts t)

-- | A 'TypeM' as a stored (nested) type: a function value is a trait object.
rustStoredType :: TypeM -> RustM MDoc
rustStoredType (Function as r) = do
  argTs <- mapM rustStoredType as
  retT <- rustStoredType r
  return $ "std::rc::Rc<dyn rustmorloc::MorlocFn" <> pretty (length as) <> "<" <> hcat (punctuate ", " (argTs <> [retT])) <> ">>"
rustStoredType (Native t) = rustTypeOf (typeFof t)
rustStoredType (Serial _) = return "*const u8"
rustStoredType Passthrough = return "*const u8"

-- | The Rust return type of a manifold: a serial result is an owned packet;
-- a function value (a closure over its context, a suspension when it takes
-- no arguments) is the one function-value spelling, a trait object, which
-- the C++ member types @std::function<R(A..)>@. An opaque @impl@ return
-- would be a second spelling that nothing outside the manifold can name.
rustReturnType :: TypeM -> RustM MDoc
rustReturnType (Function ts o) = rustStoredType (Function ts o)
rustReturnType (Serial _) = return "*mut u8"
rustReturnType Passthrough = return "*mut u8"
rustReturnType (Native t) = case typeFof t of
  FunF ts inner -> rustReturnType (Function (map typeMof ts) (typeMof inner))
  tf -> rustTypeOf tf

-- | The Rust type of a record field. A function-typed field is stored as a fat
-- trait object @Rc<dyn MorlocFnN<A1,..,An,R>>@ -- a bare @impl Fn@ is illegal in
-- a field, and the trait object carries the reify capability a crossing closure
-- needs. Everything else renders as its ordinary 'rustTypeOf'.
rustFieldType :: TypeF -> RustM MDoc
rustFieldType (FunF ts t) = rustStoredType (Function (map typeMof ts) (typeMof t))
rustFieldType t = rustTypeOf t

-- | The wire-tuple leaf type for a defunctionalized closure nested in an
-- aggregate being serialized/raw-deserialized: the reified
-- @(home_language, manifold_id, captured_packets)@ tuple. Its concrete type is
-- carried in the CVar so 'rustTypeOf' renders it verbatim (`VarF (FV _ (CV s)) ->
-- pretty s`), so a closure nested in a Vec/tuple/Option becomes a Vec/tuple/
-- Option OF this tuple. Mirrors the C++ 'cppClosureWireLeaf'. `ClosureOrigin` is
-- the rustmorloc alias for @(String, i64, Vec<Vec<u8>>)@.
rustClosureWireLeaf :: TypeF
rustClosureWireLeaf = VarF (FV (TV "Closure") (CV "rustmorloc::ClosureOrigin"))

-- | Whether Rust routes this aggregate node through the shared structural
-- (reify/reflect-in-place) path: True iff it carries a closure leaf reachable
-- WITHOUT crossing a nominal record boundary. Recursion stops at a 'SerialObject'
-- (records are marshalled by their own nominal ToVoidstar impls), so a record
-- node returns False, and so does a list/optional OF records (@[R]@/@?R@) -- those
-- fall through to the generic @Vec@/@Option@ codec plus the record's impl.
rustDivertsClosure :: SerialAST -> Bool
rustDivertsClosure = maybe False pathHasClosure . wirePath

-- | Whether a manifold parameter is passed as a shared reference. Mirrors the
-- by-reference cases in 'rustArgOf'.
rustArgIsRef :: TypeM -> Bool
rustArgIsRef (Native tf) = not (rustIsCopy tf)
rustArgIsRef (Function _ _) = True
rustArgIsRef _ = False

-- | Render a manifold parameter.
rustArgOf :: Arg TypeM -> RustM MDoc
rustArgOf a@(Arg _ t) = do
  ts <- rustArgType t
  -- Idiomatic asymmetric passing: a Copy scalar parameter is by value (`i64`);
  -- a non-Copy parameter (Str/Vec/record) is a shared reference (`&T`). Both
  -- are `Copy` at the manifold-param level (`&T` is Copy), so a value fans out
  -- to several callees with no move. Serial/passthrough args are Copy pointers.
  --
  -- A function value is taken by reference too (`&Rc<dyn MorlocFnN<..>>`), so
  -- the manifold stays re-callable across a HOF's per-element calls; a sink
  -- that wants it by value takes an `Rc` clone.
  let ts' = if rustArgIsRef t then "&" <> ts else ts
  return $ argNamer a <> ":" <+> ts'

-- | Adapt a higher-order-function closure's by-reference argument to a
-- manifold parameter: a Copy scalar parameter is taken by value, so deref the
-- reference; a non-Copy parameter is taken by reference, so forward it
-- unchanged. Used to bridge a uniformly-by-reference HOF closure to a
-- manifold's asymmetric parameter convention.
rustBridgeArg :: TypeM -> MDoc -> MDoc
rustBridgeArg (Native tf) name | rustIsCopy tf = "*" <> name
rustBridgeArg _ name = name

-- | The Rust type of a HOF closure parameter: a shared reference to the
-- element/accumuland value type (a HOF passes every closure argument by ref).
closureParamType :: TypeM -> RustM MDoc
closureParamType (Native tf) = ("&" <>) <$> rustTypeOf tf
closureParamType t@(Function _ _) = ("&" <>) <$> rustStoredType t
closureParamType _ = return "_"

-- | Render a getter/bracket/interpolation pattern. A getter (@.0@/@.field@,
-- possibly chained, multi-sibling, or with bracket steps) becomes native field
-- access + @morloc_at@/@morloc_slice@; string interpolation interweaves
-- fragments and insertions. Record/tuple SETTERS are not yet supported (unused
-- by the stdlib) and fall to the catch-all error.
rustEvalPattern :: TypeF -> Pattern -> [MDoc] -> RustM MDoc
-- String interpolation: interweave compile-time fragments with the rendered
-- insertion expressions (each forced to an owned String for a uniform slice).
rustEvalPattern _ (PatternText s ss) xs =
  return $ "rustmorloc::interweave_strings(&["
    <> hcat (punctuate ", " [dquotes (pretty (RP.rustEscape frag)) | frag <- s : ss])
    <> "], &["
    <> hcat (punctuate ", " [parens x <> ".as_str()" | x <- xs])
    <> "])"
-- Field/index getter with NO bracket steps: plain `.field` / `.i` accessor(s).
-- The `not selectorHasBracket` guard is load-bearing: a bracket chain with one
-- receiver arg would otherwise match here and `ungroup` would silently drop the
-- bracket steps, emitting field-only access.
rustEvalPattern _ (PatternStruct sel) [m]
  | not (selectorHasBracket sel) =
      return $ case ungroup sel of
        [ss] -> writeSelectorRust m ss
        sss -> tupled (map (writeSelectorRust m) sss)
-- Getter chain containing bracket steps (`.f.[i]`, `.[i:j].0`, ...). Args are
-- [bracket_bounds..., receiver]; walk the selector, threading the bracket args.
rustEvalPattern _ (PatternStruct s) args
  | selectorHasBracket s, length args == bracketArity s + 1 =
      let n = bracketArity s
          (bracketArgs, receivers) = splitAt n args
          receiver = case receivers of
            [r] -> r
            _ -> error "rustEvalPattern: bracket-in-selector expected 1 receiver"
       in return $ fst (walkRustSelectorBrackets receiver bracketArgs s)
rustEvalPattern _ PatternBracketIndex [i, m] =
  return $ "morloc_at" <> tupled [i, "&(" <> m <> ")"]
rustEvalPattern _ PatternBracketSlice [start, stop, step, m] =
  return $ "morloc_slice" <> tupled [start, stop, step, "&(" <> m <> ")"]
-- Record/tuple SETTER: rebuild the aggregate with one leaf replaced. The shared
-- 'patternSetter' re-accesses every UNCHANGED field on the receiver; in Rust the
-- receiver must be bound once (`&__r`) and each accessed field cloned -- an owned
-- rebuild cannot move a field out of a borrowed/shared receiver (the setter
-- receiver is usually a `&T` param), and a `Copy` field's `.clone()` is a free
-- copy. This is the same per-field copy C++'s aggregate-init makes. The struct
-- name for a record literal is resolved by running 'rustTypeOf' with the
-- captured state (mirrors the C++ `evalState` name capture).
rustEvalPattern t0 (PatternStruct s0) (m0 : xs0)
  | not (null xs0) = do
      st <- CMS.get
      env <- asks id
      let nameOf recType = CMS.evalState (runReaderT (rustStructCtor recType) env) st
          makeTuple _ xs = tupled xs
          makeRecord recType xs = case recType of
            NamF _ _ _ rs ->
              nameOf recType <+> "{" <+> RP.rustRecordFields (zip (map fst rs) xs) <+> "}"
            _ -> error "rustEvalPattern: record setter on a non-record type"
          accessTuple _ d i = d <> "." <> pretty i
          accessRecord _ d k = d <> "." <> RP.rustFieldIdent (Key k)
          -- An unchanged field is cloned into the owned rebuild; a changed
          -- field's accessor is reused bare as a receiver (no clone), so a
          -- nested set does not clone the whole enclosing field.
          finalizeSet _ v = v <> ".clone()"
          spine = patternSetter makeTuple makeRecord accessTuple accessRecord finalizeSet "__r" t0 s0 xs0
      return $ "{ let __r = &(" <> m0 <> "); " <> spine <> " }"
rustEvalPattern _ p args =
  error $ "Rust v1: unsupported pattern " <> show p <> " with " <> show (length args) <> " args"

-- | Walk a 'Selector' that may contain bracket steps, emitting Rust source per
-- step: @.field@ (record), @.i@ (tuple), @morloc_at(idx, &(rcv))@ (index),
-- @morloc_slice(..)@ (slice). Multi-sibling groups emit a Rust tuple. Threads
-- the bracket runtime args in DFS order; returns the expression and the
-- unconsumed bracket args. Mirrors 'walkCppSelectorBrackets'.
walkRustSelectorBrackets :: MDoc -> [MDoc] -> Selector -> (MDoc, [MDoc])
walkRustSelectorBrackets =
  walkSelectorBrackets
    (\rcv k -> rcv <> "." <> RP.rustFieldIdent (Key k))
    (\rcv i -> rcv <> "." <> pretty i)
    (\idx rcv -> "morloc_at" <> tupled [idx, "&(" <> rcv <> ")"])
    (\start stop step rcv -> "morloc_slice" <> tupled [start, stop, step, "&(" <> rcv <> ")"])
    tupled

-- | Walk an (ungrouped) selector, emitting Rust field access: @.i@ for a tuple
-- index, @.field@ for a record key (keyword-escaped to match the struct).
writeSelectorRust :: MDoc -> [Either Int Text] -> MDoc
writeSelectorRust d [] = d
writeSelectorRust d (Right k : rs) = writeSelectorRust (d <> "." <> RP.rustFieldIdent (Key k)) rs
writeSelectorRust d (Left i : rs) = writeSelectorRust (d <> "." <> pretty i) rs

-- | Wrap a manifold body in an on-disk content-addressed cache lookup (`a@fn`
-- / @cache). Mirrors the C++ @cppCacheBody@: serialize native args to packets,
-- compute the key, look it up; on a hit return the cached packet, on a miss run
-- the body and store its packet. The result is a `*mut u8` packet either way.
rustCacheBody
  :: SerialAST
  -> Text
  -> Int
  -> [(Arg TypeM, SerialAST)]
  -> PoolDocs
  -> RustM PoolDocs
rustCacheBody resSa lbl midx args bodyPool = do
  wrapIdx <- getCounter
  let suffix = pretty wrapIdx
      keyVar = "__mlc_ck_" <> suffix
      cachedVar = "__mlc_cd_" <> suffix
      resultVar = "__mlc_cr_" <> suffix
      bodyVar = "__mlc_cb_" <> suffix
      labelLit = dquotes (pretty lbl)
      resSchema = render (serialAstToMsgpackSchema resSa)
  preparedArgs <- mapM (prepareRustCacheArg wrapIdx) (zip [0 :: Int ..] args)
  let argRefs = [r | (r, _, _) <- preparedArgs]
      argSchemas = [s | (_, s, _) <- preparedArgs]
      argSetupLines = concatMap (\(_, _, ss) -> ss) preparedArgs
      packetsArr = "&[" <> hcat (punctuate ", " argRefs) <> "]"
      schemasArr = "&[" <> hcat (punctuate ", " [dquotes (pretty (RP.rustEscape s)) | s <- argSchemas]) <> "]"
      keyStmt = "let" <+> keyVar <> ": u64 = rustmorloc::cache_key("
                  <> pretty midx <> ", " <> packetsArr <> ", " <> schemasArr <> ");"
      lookupStmt = "let" <+> cachedVar <> ": *mut u8 = rustmorloc::cache_lookup("
                  <> keyVar <> ", " <> labelLit <> ");"
      missLines = poolPriorLines bodyPool
        ++ [ "let" <+> bodyVar <> ": *mut u8 =" <+> poolExpr bodyPool <> ";"
           , "rustmorloc::cache_store(" <> keyVar <> ", " <> labelLit <> ", "
               <> bodyVar <> ", " <> dquotes (pretty (RP.rustEscape resSchema)) <> ");"
           , bodyVar
           ]
      ifStmt = vsep
        [ "let" <+> resultVar <> ": *mut u8 = if !" <> cachedVar <> ".is_null() {"
        , indent 4 cachedVar
        , "} else {"
        , indent 4 (vsep missLines)
        , "};"
        ]
  return $ PoolDocs
    { poolExpr = resultVar
    , poolPriorLines = argSetupLines ++ [keyStmt, lookupStmt, ifStmt]
    , poolCompleteManifolds = poolCompleteManifolds bodyPool
    , poolPriorExprs = poolPriorExprs bodyPool
    , poolReturnFlag = poolReturnFlag bodyPool
    }

-- | Prepare one cache argument: a native arg is serialized to a packet via
-- @put_value@; an already-serial arg passes through by name. Returns the
-- packet-pointer expression, the arg's msgpack schema, and any setup lines.
prepareRustCacheArg :: Int -> (Int, (Arg TypeM, SerialAST)) -> RustM (MDoc, Text, [MDoc])
prepareRustCacheArg wrapIdx (j, (a@(Arg i tm), sa)) = do
  let schemaStr = render (serialAstToMsgpackSchema sa)
  case tm of
    Native tf -> do
      sid <- rustRegisterSchema schemaStr
      -- The cache-key value crosses into a 'ToVoidstar' (&T) 'put_value' sink;
      -- own-adapt so a borrowed non-Copy native arg is cloned rather than
      -- double-borrowed ('&(&Vec)').
      own <- rustOwnership (BndVarN tf i)
      let argVar = "__mlc_ca_" <> pretty wrapIdx <> "_" <> pretty j
          decl = "let" <+> argVar <> ": *const u8 = rustmorloc::put_value(&("
                   <> rustOwn own tf (argNamer a) <> "), " <> sch sid <> ");"
      return (argVar, schemaStr, [decl])
    _ -> return (argNamer a, schemaStr, [])

-- | Per-index variable-use counts over a manifold, for the "shared" (used at
-- more than one point) determination. ALL uses are counted (borrows INCLUDED --
-- excluding them is unsound: an owned use followed by a later borrow would slip
-- through to a use-after-move); the two arms of an 'IfN' are combined by MAX
-- (they are mutually exclusive at runtime, so a value used once per arm is one
-- use per run); a captured value (a nested manifold's context arg) counts as a
-- use. Descends into nested manifolds; any resulting over-count is safe (an
-- extra clone, never a missed one).
varUseCountOps :: FoldManifoldM Identity (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int)
varUseCountOps =
  FoldManifoldM
    { opSerialManifoldM = \full@(SerialManifold_ _ _ form _ _) -> return (ctxOf form `add` foldlSM add mempty full)
    , opNativeManifoldM = \full@(NativeManifold_ _ _ form _) -> return (ctxOf form `add` foldlNM add mempty full)
    , opSerialExprM = \node -> return (foldlSE add mempty node)
    , opNativeExprM = \node -> return $ case node of
        BndVarN_ _ i -> Map.singleton i 1
        LetVarN_ _ i -> Map.singleton i 1
        IfN_ _ c t e -> c `add` Map.unionWith max t e
        _ -> foldlNE add mempty node
    , opSerialArgM = \node -> return (foldlSA add mempty node)
    , opNativeArgM = \node -> return (foldlNA add mempty node)
    }
  where
    add = Map.unionWith (+)
    ctxOf form = Map.fromList [(i, 1) | Arg i _ <- manifoldContext form]

sharedOf :: Map.Map Int Int -> Set.Set Int
sharedOf = Map.keysSet . Map.filter (>= 2)

-- | Whether a manifold body places a function value in a container (a list,
-- tuple, record or optional whose type holds a function anywhere). A stored
-- closure is a trait object behind `Rc`, which must outlive every frame, so a
-- closure built in such a frame must own its captures ('capInit'). A
-- nested manifold's containers count too, which only over-clones.


-- | The closure manifolds whose value reaches a sourced function-typed
-- parameter DIRECTLY, and nothing else.
--
-- That position is the only consumer in generated Rust which accepts a plain
-- function: a record field, a list or tuple element, another closure's
-- capture, and a manifold's return are all rendered as the trait object, so a
-- function value reaching any of them must be able to BE one. A closure whose
-- value reaches only this position therefore needs no trait object, and a
-- stateless one needs no allocation either.
--
-- Occurrence is counted rather than assumed: a manifold reaching this position
-- once and a record field elsewhere must take the general form, so a name is
-- kept only if EVERY occurrence of it is such an argument.
thinSinkNames :: Map.Map SrcName [(Bool, Bool)] -> [SerialManifold] -> Set.Set Text
thinSinkNames mask es = Set.filter occursOnlyHere sites
  where
    sites = Set.unions (map (runIdentity . foldWithSerialManifoldM ops) es)

    -- A name is kept only if the whole pool builds that closure exactly once.
    -- Anything built more than once may reach a second consumer, and the two
    -- occurrences cannot take different forms.
    occursOnlyHere n = Map.findWithDefault (0 :: Int) n allCounts == 1
    allCounts =
      Map.fromListWith (+)
        [ (render (manNamer i), 1)
        | sm <- es
        , NativeManifold i _ form _ <- collectClosureManifolds sm
        , isClosureForm form
        ]

    ops :: FoldWithManifoldM Identity (Set.Set Text) (Set.Set Text) (Set.Set Text) (Set.Set Text) (Set.Set Text) (Set.Set Text)
    ops =
      FoldWithManifoldM
        { opFoldWithSerialManifoldM = \_ full -> return (foldlSM Set.union Set.empty full)
        , opFoldWithNativeManifoldM = \_ full -> return (foldlNM Set.union Set.empty full)
        , opFoldWithSerialExprM = \_ node -> return (foldlSE Set.union Set.empty node)
        , opFoldWithNativeExprM = \orig node -> return (Set.union (atSite orig) (foldlNE Set.union Set.empty node))
        , opFoldWithSerialArgM = \_ node -> return (foldlSA Set.union Set.empty node)
        , opFoldWithNativeArgM = \_ node -> return (foldlNA Set.union Set.empty node)
        }

    atSite (AppExeN _ (SrcCallP src) args) =
      Set.fromList
        [ render (manNamer i)
        | (k, NativeArgManifold (NativeManifold i _ form _)) <- zip [(0 :: Int) ..] args
        , isClosureForm form
        , funParamAt src k
        ]
    atSite _ = Set.empty

    funParamAt src k = case Map.lookup (srcName src) mask of
      Just bs | k < length bs -> snd (bs !! k)
      _ -> False

-- | The shared indices of a serial manifold: variables used at more than one
-- point in its body (see 'varUseCountOps').
sharedIndicesSM :: SerialManifold -> Set.Set Int
sharedIndicesSM = sharedOf . runIdentity . foldSerialManifoldM varUseCountOps

-- | The shared indices of a native manifold (used as an argument). Counting its
-- own context args (captures from the enclosing scope) only over-clones (safe).
sharedIndicesNM :: NativeManifold -> Set.Set Int
sharedIndicesNM = sharedOf . runIdentity . foldNativeManifoldM varUseCountOps

-- | The spec's @owner(e, Gamma)@ as a PURE function: the ownership of a native
-- expression given @Gamma@ (the borrowed @&T@ parameter indices) and @Shared@
-- (indices used at more than one point). Because both are explicit arguments
-- (not fold state), the result is correct regardless of the fold's evaluation
-- order. A bound variable is a @&T@ reference only when it is a borrowed
-- parameter; a shared value is a place (borrowed at reference sinks, cloned at
-- owned sinks -- never moved); a field of a borrowed value is a place;
-- everything else is an owned value.
ownerPure :: Set.Set Int -> Set.Set Int -> NativeExpr -> IOwnership
-- A bound variable and a let variable both render as `n<i>` and share the index
-- namespace, so both classify the same way ('classifyIndex').
ownerPure g s (BndVarN _ i) = classifyIndex g s i
ownerPure g s (LetVarN _ i) = classifyIndex g s i
ownerPure g s (ReturnN e) = ownerPure g s e
ownerPure g s (NativeLetN _ _ e) = ownerPure g s e
ownerPure g s (SerialLetN _ _ e) = ownerPure g s e
ownerPure g s (AppExeN _ (PatCallP (PatternStruct _)) [recv]) =
  case argOwnerPure g s recv of
    Owned -> Owned
    _ -> BorrowedPlace
ownerPure _ _ _ = Owned

-- | Classify a variable index: a borrowed parameter (in @Gamma@) is a @&T@
-- reference; a shared value (in @Shared@, used at more than one point) is a place
-- (borrowed at reference sinks, cloned at owned sinks, never moved); otherwise it
-- is an owned value that may be moved.
classifyIndex :: Set.Set Int -> Set.Set Int -> Int -> IOwnership
classifyIndex g s i
  | Set.member i g = BorrowedRef
  | Set.member i s = BorrowedPlace
  | otherwise = Owned

-- | Ownership of an argument's value. An expression argument is read in the
-- current scope (@Gamma@, @Shared@); a manifold argument renders as a CALL that
-- materializes an owned value (a manifold's return type is always the owned @T@,
-- and the return sink clones a borrowed body -- see 'ReturnN_'), so it is 'Owned'
-- regardless of the sub-manifold body's internal ownership.
argOwnerPure :: Set.Set Int -> Set.Set Int -> NativeArg -> IOwnership
argOwnerPure g s (NativeArgExpr e) = ownerPure g s e
argOwnerPure _ _ (NativeArgManifold _) = Owned

rustOwnership :: NativeExpr -> RustM IOwnership
rustOwnership e = do
  g <- asks oeCurrent
  s <- asks oeShared
  return (ownerPure g s e)

-- | Adapt an expression to an owned value at an owned sink (a container element,
-- a return, a let RHS, a by-value parameter): clone a borrowed non-'Copy' value,
-- dereference a borrowed 'Copy' reference, read a 'Copy' place as-is.
rustOwn :: IOwnership -> TypeF -> MDoc -> MDoc
rustOwn Owned _ x = x
rustOwn BorrowedRef tf x
  | rustIsCopy tf = "*" <> x
  | otherwise = x <> ".clone()"
rustOwn BorrowedPlace tf x
  | rustIsCopy tf = x
  | otherwise = x <> ".clone()"

-- | Adapt an expression to a shared reference at a reference sink (a @&T@
-- parameter or a closure application): an owned value or a borrowed place is
-- borrowed; a value that is already a reference is forwarded unchanged.
rustRef :: IOwnership -> MDoc -> MDoc
rustRef BorrowedRef x = x
rustRef _ x = "&(" <> x <> ")"

-- | Reference the pool's schema for a registered schema id via the crate-root
-- @schema(<id>)@ accessor. Fully qualified (@crate::@) so it resolves even inside
-- a scope that binds a local @schema@ -- e.g. a record's generated
-- @ToVoidstar@/@FromVoidstar@ impl, whose closure fields inline a reflect proxy
-- that looks up the closure's own arg/result schemas.
sch :: Int -> MDoc
sch sid = "crate::schema(" <> pretty sid <> ")"

-- | A Rust tuple: the unit, a ONE-tuple (which needs the trailing comma --
-- @(x)@ is a parenthesized value, not a tuple, and its fields do not exist),
-- or the ordinary form. Used for both the capture tuple's type and its value.
rustTuple :: [MDoc] -> MDoc
rustTuple [] = "()"
rustTuple [x] = "(" <> x <> ",)"
rustTuple xs = tupled xs

-- | Build a function value: the one construction site, and the only place
-- that decides a function value's form.
--
-- The captured context is copied ONCE into a tuple the closure owns (I2: a
-- closure owns what it holds, so nothing it captures can dangle and no
-- generated signature needs a lifetime). Both the call and the origin
-- builder are NON-CAPTURING closures over that tuple, so each is a plain
-- function pointer reading the single copy -- the whole value is one
-- allocation.
--
-- The origin is built lazily by the runtime on first reify, so a closure
-- that never crosses a pool boundary pays nothing for the ability. A
-- closure whose manifold has no dispatch entry cannot be called back into,
-- so it is built with no origin builder at all and reifying it fails by
-- name rather than by fabricating an identity.
rustClosureWrapper :: MDoc -> MDoc -> [Arg TypeM] -> [Arg TypeM] -> RustM MDoc
rustClosureWrapper sig mname ctxArgs boundArgs = do
  -- the owned type of each capture, naming the tuple the fn pointers read
  capTypes <- mapM (\(Arg _ t) -> rustStoredType t) ctxArgs
  boundTyped <- mapM (\a@(Arg _ t) -> do
                        base <- closureParamType t
                        return (argNamer a <> ":" <+> base)) boundArgs
  capInits <- mapM capInit ctxArgs
  let n = length boundArgs
      capsT = rustTuple capTypes
      -- Whether the callee takes this capture by reference is the same
      -- question 'rustArgIsRef' answers for a manifold parameter, because
      -- that IS the parameter it is being passed to.
      capRef i (Arg _ t) =
        let fld = "__c." <> pretty (i :: Int)
         in if rustArgIsRef t then "&" <> fld else fld
      callArgs = zipWith capRef [0 ..] ctxArgs
                   <> [rustBridgeArg t (argNamer a) | a@(Arg _ t) <- boundArgs]
      callFn =
        "|__c: &" <> capsT <> hcat [", " <> b | b <- boundTyped] <> "|"
          <+> "unsafe {" <+> mname <> tupled callArgs <+> "}"
  reifyInfo <- CMS.gets rsReifyInfo
  (ctor, mkArgs) <- case Map.lookup (render mname) reifyInfo of
    -- no dispatch entry: nothing can call back into this closure
    Nothing -> return ("rustmorloc::Closure" <> pretty n <> "::local", [])
    Just (mid, capWire)
      -- The capture list the dispatch wrapper decodes and the one the closure
      -- carries are built from different projections of the manifold form; a
      -- length disagreement would pair a capture's value with another's
      -- schema, so it is a compiler bug rather than something to truncate.
      | length capWire /= length ctxArgs ->
          error $
            "Rust: closure " <> show mid <> " captures " <> show (length ctxArgs)
              <> " values but " <> show (length capWire) <> " wire forms are registered"
      | otherwise -> do
          reifiers <- mapM (reifyInPlace . snd) capWire
          let capExprs =
                [ "unsafe { rustmorloc::reify_capture(&" <> maybe fld ($ fld) reify <> ", " <> sch sid <> ") }"
                | (i, (sid, _), reify) <- zip3 [(0 :: Int) ..] capWire reifiers
                , let fld = "__c." <> pretty i
                ]
              mkFn =
                "|__c: &" <> capsT <> "|"
                  <+> "(\"rust\".to_string(), " <> pretty mid <> "i64, vec!["
                  <> hcat (punctuate ", " capExprs) <> "])"
          return ("rustmorloc::Closure" <> pretty n <> "::new", [mkFn])
  let ctorArgs = ["__caps", callFn] <> mkArgs
      boxedDoc =
        vsep
          [ "{"
          , indent 4 $
              vsep
                [ "let __caps:" <+> capsT <+> "=" <+> rustTuple capInits <> ";"
                , "std::rc::Rc::new(" <> ctor <> tupled ctorArgs <> ") as" <+> sig
                ]
          , "}"
          ]
      -- A closure that captures nothing and has no dispatch entry holds no
      -- state and carries no identity, so the trait object buys nothing it
      -- could use: it is a plain function, and a plain function is a POINTER.
      -- Emitting it as one lets a sourced higher-order parameter monomorphize
      -- over a compile-time constant instead of loading a vtable and
      -- allocating twice, once per element.
      --
      -- This is the only position where a function value takes a second form,
      -- and choosing it wrongly cannot miscompile: every position that needs a
      -- value able to cross renders as the trait object, which a function
      -- pointer does not satisfy, so a bad choice is a type error in the
      -- generated Rust rather than a wrong answer at run time.
      thinDoc =
        "rustmorloc::fn_ptr" <> pretty n
          <> parens ("|" <> hcat (punctuate ", " boundTyped) <> "|"
                       <+> "unsafe {" <+> mname <> tupled callArgs <+> "}")
  thinSinks <- CMS.gets rsThinSinks
  return $
    if null ctxArgs
      && not (Map.member (render mname) reifyInfo)
      && Set.member (render mname) thinSinks
      then thinDoc
      else boxedDoc

-- | Copy one captured value into the tuple the closure owns. A function
-- value is an 'Rc' clone (a refcount bump); a non-'Copy' value is cloned so
-- the caller's copy stays live; a 'Copy' scalar is taken as it is.
capInit :: Arg TypeM -> RustM MDoc
capInit a@(Arg i t) = do
  parentBorrowed <- asks oeParent
  let name = argNamer a
      deref = if Set.member i parentBorrowed then "(*" <> name <> ")" else name
  -- A capture the closure owns: a value that is passed by reference must be
  -- cloned out of the frame ('Rc' clone for a function value, which is what
  -- `.clone()` on an `Rc` already is), and a 'Copy' value -- a scalar, or a
  -- raw packet pointer -- is taken as it is.
  return $ if rustArgIsRef t then deref <> ".clone()" else deref

-- | Reflect an incoming closure wire tuple into a native callable: a bare
-- @move@ closure that, on each application, appends the runtime-argument packets
-- to the deserialized captured packets and RPCs back to the producing pool via
-- 'foreign_call' (resolving its socket from the wire tuple's home-language
-- name). The result is a plain @Fn(&A..)->R@, so it satisfies BOTH a sourced
-- @impl Fn@ higher-order parameter (by value) and a morloc-defined
-- @&impl MorlocFnN@ parameter (via the @Fn@ blanket) -- mirroring the C++
-- member, whose reflected value is likewise a plain lambda. (Origin-preserving
-- re-cross of a reflected closure is deferred; it needs the same reify work the
-- C++ member also still lacks.)
-- | How a proxy hands one of its arguments to the wire: a value is put as it
-- is; a closure is reified first (its wire form is its origin tuple).
closureArgPush :: Int -> SerialAST -> Int -> RustM MDoc
closureArgPush i ast sid = do
  toWire <- reifyInPlace ast
  let a = "__a" <> pretty i
      v = maybe a (\f -> "&" <> parens (f a)) toWire
  return $ "__pkts.push(rustmorloc::put_value(" <> v <> ", " <> sch sid <> "));"

-- | How a proxy reads the result of its call off the wire: a value is got
-- as it is; a closure arrives as its origin tuple and is reflected into a
-- proxy of its own, so a closure may return a closure across pools.
closureResultRead :: SerialAST -> MDoc -> Int -> RustM MDoc
closureResultRead out call sid = do
  fromWire <- reflectInPlace out
  wireT <- rustTypeOf (wireSerialAstToType rustClosureWireLeaf out)
  let got = "rustmorloc::get_value::<" <> wireT <> ">(" <> call <> ", " <> sch sid <> ")"
  return $ maybe got ($ got) fromWire

-- | Where a value differs between the form a pool holds and the form it
-- travels as. A value's wire form is a function of its 'SerialAST' and of
-- nothing else -- not of its 'TypeM', not of its rendered signature, not of
-- its arity -- so both ends of the wire are rendered from this one traversal
-- and cannot desynchronize.
--
-- Two node kinds differ. A CLOSURE travels as the manifold to call back into
-- plus its captured environment. A custom-PACKED type travels as whatever its
-- packer unpacks it to, which is why a packed node carries the path of the
-- form beneath it as well.
data WirePath
  = AtClosure SerialAST
  | AtPack TypePacker (Maybe WirePath)
  | OverList WirePath
  | OverOpt WirePath
  | OverTuple [Maybe WirePath]

-- | The positions of a wire form that differ from the native value, or
-- 'Nothing' where the two coincide. A record is 'Nothing': it marshals
-- through its own generated impl, which handles its own fields.
wirePath :: SerialAST -> Maybe WirePath
wirePath ast@(SerialClosure _ _) = Just (AtClosure ast)
wirePath (SerialPack _ (p, inner)) = Just (AtPack p (wirePath inner))
wirePath (SerialList _ _ inner) = OverList <$> wirePath inner
wirePath (SerialOptional _ inner) = OverOpt <$> wirePath inner
wirePath (SerialTuple _ ss)
  | all isNothing ps = Nothing
  | otherwise = Just (OverTuple ps)
  where
    ps = map wirePath ss
wirePath _ = Nothing

-- | Whether a path holds a closure. Routing an aggregate through the
-- structural reify/reflect is a question about CLOSURES only -- the generic
-- codec handles a packer perfectly well on its own -- so it is answered from
-- the one traversal rather than by a second one that could drift from it.
pathHasClosure :: WirePath -> Bool
pathHasClosure (AtClosure _) = True
pathHasClosure (AtPack _ inner) = maybe False pathHasClosure inner
pathHasClosure (OverList p) = pathHasClosure p
pathHasClosure (OverOpt p) = pathHasClosure p
pathHasClosure (OverTuple ps) = any (maybe False pathHasClosure) ps

-- | Rebuild a native value from its wire form: each origin becomes a proxy.
renderReflect :: WirePath -> RustM (MDoc -> MDoc)
renderReflect (AtClosure ast) = rustReflectClosureAssembler ast
-- The wire carries what the packer unpacks to, so rebuilding the native value
-- means rebuilding that form first and then packing it.
renderReflect (AtPack p inner) = do
  f <- maybe (return id) renderReflect inner
  let packer = pretty (srcName (typePackerForward p))
      ref v = if rustIsCopy (typePackerUnpacked p) then v else "&" <> parens v
  return $ \v -> packer <> parens (ref (f v))
renderReflect (OverList p) = do
  f <- renderReflect p
  return $ \v -> v <> ".into_iter().map(|__e| " <> f "__e" <> ").collect::<Vec<_>>()"
renderReflect (OverOpt p) = do
  f <- renderReflect p
  return $ \v -> v <> ".map(|__e| " <> f "__e" <> ")"
-- The source expression is bound ONCE before the slots project out of it.
-- Substituting it per slot would re-evaluate it, and at a dispatch wrapper
-- that expression deserializes a packet: an n-slot tuple would be stitched
-- from n independent deserializations, each with its own shared-memory block
-- to track and free.
renderReflect (OverTuple ps) = do
  u <- getCounter
  fs <- mapM (traverse renderReflect) ps
  let t = "__tup" <> pretty u
  return $ \v ->
    "{ let" <+> t <+> "=" <+> v <> ";"
      <+> tupled [maybe (slot t j) ($ slot t j) mf | (j, mf) <- zip [(0 :: Int) ..] fs]
      <+> "}"
  where
    slot v j = v <> "." <> pretty j

-- | The dual: reduce a native value to its wire form by reifying each
-- closure to its origin. Clause for clause the inverse of 'renderReflect',
-- because both are driven by the same 'WirePath'.
renderReify :: WirePath -> RustM (MDoc -> MDoc)
renderReify (AtClosure (SerialClosure ins _)) =
  return $ \v -> "rustmorloc::require_origin(" <> parens v <> ".reify" <> pretty (length ins) <> "())"
renderReify (AtClosure _) = error "renderReify: a closure node is a SerialClosure"
-- The inverse: unpack to the form the wire carries, then reduce that.
renderReify (AtPack p inner) = do
  f <- maybe (return id) renderReify inner
  let unpacker = pretty (srcName (typePackerReverse p))
      ref v = if rustIsCopy (typePackerPacked p) then v else "&" <> parens v
  return $ \v -> f (unpacker <> parens (ref v))
renderReify (OverList p) = do
  f <- renderReify p
  return $ \v -> v <> ".iter().map(|__e| " <> f "__e" <> ").collect::<Vec<_>>()"
renderReify (OverOpt p) = do
  f <- renderReify p
  return $ \v -> v <> ".as_ref().map(|__e| " <> f "__e" <> ")"
-- A slot that is not a closure is COPIED into the wire tuple. Projecting it
-- out by value would move it out of the shared reference the origin builder
-- holds, which a non-'Copy' slot does not allow; a closure slot is only
-- borrowed, so it projects directly.
renderReify (OverTuple ps) = do
  fs <- mapM (traverse renderReify) ps
  return $ \v ->
    tupled
      [ maybe (parens (slot v j) <> ".clone()") ($ slot v j) mf
      | (j, mf) <- zip [(0 :: Int) ..] fs
      ]
  where
    slot v j = v <> "." <> pretty j

reflectInPlace :: SerialAST -> RustM (Maybe (MDoc -> MDoc))
reflectInPlace = traverse renderReflect . wirePath

reifyInPlace :: SerialAST -> RustM (Maybe (MDoc -> MDoc))
reifyInPlace = traverse renderReify . wirePath

-- | Reflect an incoming closure wire tuple into a native function value.
-- Shares the origin-preserving assembler with the nested and record-field
-- reflects: one reflect, so a value crossing A -> B -> C calls back to A no
-- matter which position it arrived in.
rustReflectClosure :: MDoc -> SerialAST -> RustM MDoc
rustReflectClosure pkt ast@(SerialClosure _ _) = do
  tupSid <- rustRegisterSchema (render (serialAstToMsgpackSchema ast))
  assemble <- rustReflectClosureAssembler ast
  return $ assemble ("rustmorloc::get_value(" <> pkt <> ", " <> sch tupSid <> ")")
rustReflectClosure _ _ = error "rustReflectClosure: expected SerialClosure"

-- | Compute the schema-dependent pieces of an origin-preserving reflect ONCE
-- (in 'RustM', which the schema table needs), returning a PURE assembler that
-- maps a parsed-'ClosureOrigin' expression to the reflected @Rc<dyn MorlocFnN>@.
-- Reused by the nested-aggregate reflect ('rustReflectClosureParsed') and the
-- record-field reflect ('printRecordImpls' via 'generateRustStructs'), which
-- differ only in how the origin expression is obtained (an already-parsed tuple
-- vs a @FromVoidstar::read@ off the record's wire slot).
rustReflectClosureAssembler :: SerialAST -> RustM (MDoc -> MDoc)
rustReflectClosureAssembler (SerialClosure ins out) = do
  argTs <- mapM (rustTypeOf . serialAstToNativeType) ins
  resT <- rustTypeOf (serialAstToNativeType out)
  argSids <- mapM (rustRegisterSchema . render . serialAstToMsgpackSchema) ins
  resSid <- rustRegisterSchema (render (serialAstToMsgpackSchema out))
  resultDoc <- closureResultRead out "rustmorloc::foreign_call(__sock, __c.1.1 as u32, &__pkts)" resSid
  pushes <- sequence [closureArgPush i ast sid | (i, ast, sid) <- zip3 [(0 :: Int) ..] ins argSids]
  let n = length ins
      params = ["__a" <> pretty i <> ": &" <> t | (i, t) <- zip [(0 :: Int) ..] argTs]
      dynT =
        "std::rc::Rc<dyn rustmorloc::MorlocFn" <> pretty n
          <> "<" <> hcat (punctuate ", " (argTs <> [resT])) <> ">>"
      bodyDoc =
        vsep
          [ "let __sock = &__c.0;"
          , "let mut __pkts: Vec<*const u8> = Vec::with_capacity(__c.1.2.len() + " <> pretty n <> ");"
          , "__pkts.extend(__c.1.2.iter().map(|__p| __p.as_ptr()));"
          , vsep pushes
          , resultDoc
          ]
  return $ \tup ->
    vsep
      [ "{"
      , indent 4 $
          vsep
            [ "let __clo: rustmorloc::ClosureOrigin = " <> tup <> ";"
            , "let __caps = (format!(\"pipe-{}\", __clo.0), __clo.clone());"
            , "std::rc::Rc::new(rustmorloc::Closure" <> pretty n <> "::proxy("
            , indent 4 $
                vsep
                  [ "__caps,"
                  , "|__c: &(String, rustmorloc::ClosureOrigin)"
                      <> hcat [", " <> pr | pr <- params] <> "| -> " <> resT <> " { unsafe {"
                  , indent 4 bodyDoc
                  , "} },"
                  , "__clo,"
                  ]
            , ")) as " <> dynT
            ]
      , "}"
      ]
rustReflectClosureAssembler _ = error "rustReflectClosureAssembler: expected SerialClosure"

-- | Reflect a closure NESTED in an aggregate: the enclosing get_value already
-- produced the wire tuple, so @tup@ is the parsed 'ClosureOrigin' (not a raw
-- packet). Build an origin-preserving @Rc<dyn MorlocFnN>@: the reflected value
-- is a proxy carrying the ORIGINAL origin, so a later reify (@reifyN@)
-- reproduces the producing pool (a re-cross A->B->C calls back to A, not B).
rustReflectClosureParsed :: MDoc -> SerialAST -> RustM MDoc
rustReflectClosureParsed tup s = ($ tup) <$> rustReflectClosureAssembler s

-- | Per sourced Rust function, the per-parameter "is a bare type variable"
-- mask from the DECLARED morloc signature. A concrete parameter (Int, Str,
-- [a], record) is False; a bare @a@ is True. Read from 'stateSignatures'
-- (still live at codegen): a plain sourced function is 'Monomorphic' with its
-- type variables 'ForallU'-quantified; a typeclass method is 'Polymorphic',
-- whose per-instance 'termGeneral' has been monomorphized, so the class-level
-- signature + the instance's 'classVars' carry the real type variables.
buildSrcTypeVarMask :: MorlocMonad (Map.Map SrcName [(Bool, Bool)])
buildSrcTypeVarMask = do
  GMap _ sigmap <- MM.gets stateSignatures
  tcls <- MM.gets stateTypeclasses
  return . Map.fromList . concatMap (fromSig tcls) . Map.elems $ sigmap
  where
    fromSig _ (Monomorphic (TermTypes (Just et) srcs _)) =
      let (qs, t') = unqualify (etype et)
       in entries (Set.fromList qs) t' srcs
    fromSig _ (Monomorphic (TermTypes Nothing _ _)) = []
    fromSig tcls (Polymorphic _ methodEV classET tts) =
      let cvars = maybe [] classVars (Map.lookup methodEV tcls)
          (fs, t') = unqualify (etype classET)
          qs = Set.fromList (fs <> cvars)
       in concat [entries qs t' srcs | TermTypes _ srcs _ <- tts]

    entries qs t' srcs =
      [(srcName s, paramMask qs t') | (_, isrc) <- srcs, let s = val isrc, srcLang s == rustLang]

    paramMask qs (FunU args _) = map (\a -> (isBareVar qs a, isFunParam a)) args
    paramMask _ _ = []

    isBareVar qs (VarU v) = Set.member v qs
    isBareVar _ _ = False

    -- A suspension is a function value of no arguments; the host takes it
    -- through the same trait as any other callable.
    isFunParam (FunU _ _) = True
    isFunParam (EffectU _ _) = True
    isFunParam _ = False

translate :: [Source] -> [SerialManifold] -> MorlocMonad Script
translate srcs es = do
  let rustSrcs = unique $ mapMaybe srcPath [s | s <- srcs, srcLang s == rustLang]
  includeDocs <- mapM rustSourceInclude rustSrcs

  debugInfo <- makeManifoldDebugInfoLookup

  -- Merge the general typedef scope into the Rust concrete scope so a record
  -- field declared via a general alias resolves through its Rust mapping (as
  -- the C++ member does). Concrete entries win on collision.
  universalScopeMap <- MM.gets stateUniversalConcreteTypedefs
  generalScope <- MM.gets stateUniversalGeneralTypedefs
  let rustScope = fromMaybe Map.empty (Map.lookup rustLang universalScopeMap)
      mergedRustScope = Map.union rustScope generalScope
      recmap = unifyRecords . concatMap collectRecords $ es

  srcTypeVarMask <- buildSrcTypeVarMask
  -- Per closure-manifold wire schemas (captured/bound/result), shared with every
  -- pool member; drives the reify path + home-pool dispatch wrappers for a
  -- closure this Rust pool produces and sends to another pool.
  closureAsts <- computeClosureSchemas rustLang es
  let closureTable = Map.map closureSchemaTexts closureAsts
  logTemplates <- collectRenderedTemplates rustLang
  let st0 = defaultValue {rsDebugInfo = debugInfo, rsRecmap = recmap, rsCScope = mergedRustScope, rsSrcTypeVarMask = srcTypeVarMask, rsLogTemplates = logTemplates}
      code = CMS.evalState (runReaderT (makeRustCode includeDocs closureAsts closureTable es) emptyOwnEnv) st0

  home <- MM.asks configHome
  deps <- rustDepsUnion
  localCrates <- rustLocalDeps
  installDir <- MM.gets stateInstallDir
  -- The pool crate/bin name is derived from the build LOCATION (unique per
  -- program/build-dir, stable across edits of one program). With a shared cargo
  -- --target-dir (used to cache rustmorloc/deps across builds), this keeps one
  -- `morloc make`'s pool binary from colliding with another's, while repeated
  -- builds of the same program overwrite in place rather than accumulating a new
  -- crate per edit. Falls back to a source hash if the build dir is unset.
  let poolSrc = subVersion (render code)
      crateName = "pool_" <> PH.hashText (maybe poolSrc T.pack installDir)
      (cargoToml, buildRs) = makeCargoDocs crateName deps localCrates home
  maker <- makeTheMaker crateName
  let poolSubdir = ML.poolDirKey rustLang

  -- The Rust pool is a real Cargo project: `src/main.rs` is the rendered pool
  -- code, `Cargo.toml` pulls in rustmorloc (path dep) + any declared rust-deps,
  -- and `build.rs` supplies the libmorloc.so link. `cargo build`
  -- (makeTheMaker) does dependency resolution and linking.
  return $
    Script
      { scriptBase = "pool"
      , scriptLang = rustLang
      , scriptCode =
          "." :/ Dir "pools"
            [ Dir poolSubdir
                [ File "Cargo.toml" (Code (render cargoToml))
                , File "build.rs" (Code (render buildRs))
                , Dir "src" [File "main.rs" (Code poolSrc)]
                ]
            ]
      , scriptMake = maker
      }

-- | Emit an @include!@ of a sourced Rust file at the pool crate root, so its
-- @pub fn@s become directly callable by name (mirroring C++ @#include@).
rustSourceInclude :: Path -> MorlocMonad MDoc
rustSourceInclude p = do
  absPath <- liftIO $ MS.canonicalizePath p
  return $ "include!(" <> dquotes (pretty absPath) <> ");"

subVersion :: Text -> Text
subVersion = T.replace "__MORLOC_VERSION__" (MT.pack MV.versionStr)

makeRustCode :: [MDoc] -> Map.Map Int ([SerialAST], [SerialAST], SerialAST) -> Map.Map Int ([Text], [Text], Text) -> [SerialManifold] -> RustM MDoc
makeRustCode includeDocs closureAsts closureTable0 es = do
  structDocs <- generateRustStructs closureAsts es
  enumDocs <- generateRustEnums es
  variantDocs <- generateRustVariants es
  -- Keep the closures that can cross, closed over what they capture.
  closureTable <- restrictToCrossingClosures es closureTable0
  -- Per crossing closure: a home-pool serial dispatch wrapper (so a foreign pool
  -- can apply it) and the reify info (mid + captured schema ids) that
  -- 'rustClosureWrapper' uses to build the closure's origin builder.
  (closureWrappers, reifyInfo) <- makeClosureDispatch closureAsts closureTable es
  mask <- CMS.gets rsSrcTypeVarMask
  CMS.modify $ \s -> s {rsReifyInfo = reifyInfo, rsThinSinks = thinSinkNames mask es}
  program <- buildProgramM Map.empty Map.empty includeDocs [] es translateSegment getRustSchemaTable closureTable
  -- structDocs go in the schema-table section; the closure dispatch wrappers are
  -- free functions spliced into the signatures section.
  return $ RP.printProgram (structDocs <> enumDocs <> variantDocs) closureWrappers [] program

-- | A closure value's Rust signature: result type + tupled argument types.
-- Matches the signature seen at a serialize site for the same closure, so a
-- crossing closure and its serialize point compare equal.
closureRustSig :: [TypeF] -> TypeF -> RustM Text
closureRustSig ins out = do
  rt <- rustTypeOf out
  ats <- mapM rustTypeOf ins
  return (render (rt <> tupled ats))

-- | The signature of a closure manifold's value: its bound (remaining) argument
-- types and its result type.
manifoldRustSig :: NativeManifold -> RustM Text
manifoldRustSig nm@(NativeManifold _ _ form _) =
  closureRustSig [t | Arg _ t <- manifoldBound form] resultType
  where
    resultType = case typeFof nm of
      FunF _ o -> o
      o -> o

-- | Signatures of every closure that reaches a 'SerialClosure' serialize site,
-- i.e. that crosses a language boundary anywhere in these manifolds.
crossingClosureSigs :: [SerialManifold] -> RustM (Set.Set Text)
crossingClosureSigs es = Set.fromList <$> mapM astSig (concatMap collectSerializedClosures es)
  where
    astSig (SerialClosure ins out) =
      closureRustSig (map serialAstToNativeType ins) (serialAstToNativeType out)
    astSig _ = return "" -- collectSerializedClosures returns only SerialClosure

-- | Keep in the closure table only closures whose signature can cross a
-- boundary. Everything else is a purely in-pool closure that needs no reify or
-- dispatch machinery, and whose captured types therefore need no wire form.
--
-- Crossing is TRANSITIVE over captures: reifying a closure serializes the
-- values it captured, so a function value captured by a crossing closure is
-- itself reified and needs the same machinery. Closing only over the closures
-- that reach a serialize site leaves such a capture with no dispatch entry,
-- and reifying it fails at run time. The fixed point is taken over
-- signatures, as the serialize sites are matched, so it may keep a closure
-- that never crosses but can never drop one that does.
restrictToCrossingClosures ::
  [SerialManifold] ->
  Map.Map Int ([Text], [Text], Text) ->
  RustM (Map.Map Int ([Text], [Text], Text))
restrictToCrossingClosures es closureTable = do
  seedSigs <- crossingClosureSigs es
  let candidates =
        [ nm | nm@(NativeManifold i _ _ _) <- concatMap collectClosureManifolds es
             , Map.member i closureTable ]
  -- each closure manifold: its own signature, and the signatures of the
  -- function values it captures
  entries <- mapM (\nm@(NativeManifold i _ form _) -> do
                     sig <- manifoldRustSig nm
                     capSigs <- sequence
                                  [ closureRustSig ins out
                                  | Arg _ o <- manifoldContext form
                                  , Just (FunF ins out) <- [orNativeType o] ]
                     return (i, sig, capSigs))
                  candidates
  let close sigs =
        let sigs' = Set.union sigs
              (Set.fromList (concat [cs | (_, sig, cs) <- entries, Set.member sig sigs]))
         in if Set.size sigs' == Set.size sigs then sigs else close sigs'
      crossingSigs = close seedSigs
      crossing = Set.fromList [i | (i, sig, _) <- entries, Set.member sig crossingSigs]
  return $ Map.filterWithKey (\i _ -> Set.member i crossing) closureTable

-- | For each crossing closure, emit a home-pool serial dispatch wrapper so a
-- foreign pool can apply it when its reflected proxy calls back on the closure's
-- manifold id: deserialize the captured ++ bound argument packets, call the
-- native closure-body manifold, and serialize the result. Also return, keyed by
-- manifold name, the @(mid, capturedSchemaIds)@ 'rustClosureWrapper' needs to
-- build the closure's origin.
makeClosureDispatch ::
  Map.Map Int ([SerialAST], [SerialAST], SerialAST) ->
  Map.Map Int ([Text], [Text], Text) ->
  [SerialManifold] ->
  RustM ([MDoc], Map.Map Text (Int, [(Int, SerialAST)]))
makeClosureDispatch closureAsts closureTable es = do
  results <- mapM one (filter inTable (concatMap collectClosureManifolds es))
  return (map fst results, Map.fromList (map snd results))
  where
    inTable (NativeManifold i _ _ _) = Map.member i closureTable
    one (NativeManifold i _ form _) = do
      let ctxTs = [t | Arg _ o <- manifoldContext form, Just t <- [orNativeType o]]
          bndTs = [t | Arg _ t <- manifoldBound form]
          -- inTable guaranteed membership, so this key is present.
          (capScs, bndScs, resSc) = closureTable Map.! i
          (capAsts, bndAsts, resAst) = closureAsts Map.! i
      argSids <- mapM rustRegisterSchema (capScs <> bndScs)
      resSid <- rustRegisterSchema resSc
      -- An argument holding a closure at any depth arrives in its WIRE form
      -- (closure slots are origin tuples) and is reflected in place; a result
      -- that is a closure is reified into its wire tuple.
      argReflects <- mapM reflectInPlace (capAsts <> bndAsts)
      argTs <- sequence
        [ if isJust refl
            then rustTypeOf (wireSerialAstToType rustClosureWireLeaf ast)
            else rustTypeOf t
        | (t, ast, refl) <- zip3 (ctxTs <> bndTs) (capAsts <> bndAsts) argReflects
        ]
      let capSids = take (length capScs) argSids
          copies = map rustIsCopy (ctxTs <> bndTs)
          argExprs =
            [ deser t sid copy j refl
            | (j, (t, sid, copy, refl)) <- zip [(0 :: Int) ..] (zip4 argTs argSids copies argReflects)
            ]
          -- Deserialize each argument packet to the closure body's native param
          -- form: a Copy value by value, a non-Copy value by shared reference.
          deser t sid copy j refl =
            let raw = "rustmorloc::get_value::<" <> t <> ">(a(" <> pretty j <> "), " <> sch sid <> ")"
                g = case refl of
                  Just assemble -> assemble raw
                  Nothing -> raw
             in if copy then g else rustRef Owned g
          call = manNamer i <> tupled argExprs
          result = case resAst of
            SerialClosure ins _ ->
              "rustmorloc::require_origin((" <> call <> ").reify" <> pretty (length ins) <> "())"
            _ -> call
          wrapper =
            vsep
              [ "unsafe fn mlc_closure_dispatch_" <> pretty i <> "(args: *const *const u8, nargs: usize) -> *mut u8 {"
              , indent 4 $
                  vsep
                    [ "let a = |k: usize| -> *const u8 { if k < nargs { unsafe { *args.add(k) } } else { std::ptr::null() } };"
                    , "rustmorloc::put_value(&(" <> result <> "), " <> sch resSid <> ")"
                    ]
              , "}"
              ]
      -- The wire form of each capture travels with its schema id: the origin
      -- builder reduces the value through the same 'wirePath' the dispatch
      -- wrapper rebuilds it through.
      return (wrapper, (render (manNamer i), (i, zip capSids capAsts)))

-- | Collect every record type used in these manifolds, keyed by its FVar, with
-- one representative field list (from a use site). Unlike the shared recmap
-- (which only collects @= "struct"@ records), this also collects user-mapped
-- records (@record Rust => X = "Name"@) so their marshalling impls are emitted.
collectRustRecords :: [SerialManifold] -> [(FVar, [TypeF], [(Key, TypeF)])]
collectRustRecords =
  -- One entry per record; keep the first field list. A generated struct
  -- covers every instantiation with generic parameters, so it is one entry
  -- per general name. A user-mapped struct may be a template the user
  -- wrote, and each instantiation needs its own marshalling, so those are
  -- one entry per (name, arguments).
  nubBy ((==) `on` \(FV gv cv, ps, _) -> (gv, if cv == CV "struct" then [] else ps))
    . concatMap (runIdentity . foldWithSerialManifoldM fm)
  where
    fm = defaultValue {opFoldWithNativeExprM = ne, opFoldWithSerialExprM = se}
    ne _ (DeserializeN_ t s xs) = return $ xs <> seek t <> seek (serialAstToType s)
    ne efull e = return $ foldlNE (<>) (seek (typeFof efull)) e
    se _ (SerializeS_ s xs) = return $ seek (serialAstToType s) <> xs
    se _ e = return $ foldlSE (<>) [] e

    seek :: TypeF -> [(FVar, [TypeF], [(Key, TypeF)])]
    -- A table is an Arrow record batch with its own marshalling in
    -- rustmorloc, not a struct to generate.
    seek (NamF NamTable _ _ _) = []
    seek (NamF _ v ps rs) = (v, ps, rs) : concatMap seek ps <> concatMap (seek . snd) rs
    -- A record reachable only as a `data` arm's field still needs its
    -- struct emitted, so the walk descends through arms as the enum
    -- collector's does.
    seek (VariantF _ ps as) = concatMap seek ps <> concatMap (concatMap seek . snd) as
    seek (AppF t ts) = concatMap seek (t : ts)
    seek (FunF ts t) = concatMap seek (t : ts)
    seek (OptionalF t) = seek t
    seek _ = []

-- | Outer name of a concrete-scope typedef body, when it contributes a name.
bodyName :: TypeU -> Maybe Text
bodyName (VarU (TV n)) = Just n
bodyName (AppU (VarU (TV n)) _) = Just n
bodyName (NamU _ (TV n) _ _) = Just n
bodyName _ = Nothing

-- | Every occurrence of an argument-free @data@ type in these manifolds,
-- with its constructor names. Occurrences are merged in
-- 'generateRustEnums' by rendered name, keeping the LONGEST constructor
-- list rather than the first seen: a constructor LITERAL reports a type
-- whose table holds only its own arm, so a first-wins merge could define
-- the type from one arm and silently renumber every other constructor --
-- an arm's position is its wire tag.
collectRustEnums :: [SerialManifold] -> [(FVar, [TypeF], [Text])]
collectRustEnums = concatMap (runIdentity . foldWithSerialManifoldM fm)
  where
    fm = defaultValue {opFoldWithNativeExprM = ne, opFoldWithSerialExprM = se}
    ne _ (DeserializeN_ t s xs) = return $ xs <> seek t <> seek (serialAstToType s)
    ne efull e = return $ foldlNE (<>) (seek (typeFof efull)) e
    se _ (SerializeS_ s xs) = return $ seek (serialAstToType s) <> xs
    se _ e = return $ foldlSE (<>) [] e

    seek :: TypeF -> [(FVar, [TypeF], [Text])]
    seek (EnumF v ps ns) = (v, ps, ns) : concatMap seek ps
    seek (VariantF _ ps as) = concatMap seek ps <> concatMap (concatMap seek . snd) as
    seek (NamF _ _ _ rs) = concatMap (seek . snd) rs
    seek (AppF t ts) = concatMap seek (t : ts)
    seek (FunF ts t) = concatMap seek (t : ts)
    seek (OptionalF t) = seek t
    seek _ = []

-- | Collect every payload-bearing @data@ type used in these manifolds with
-- its arms.
--
-- Occurrences are merged by keeping the WIDEST arm list rather than the
-- first one seen. A constructor literal's type reports only the arm being
-- built, so taking the first occurrence could declare a one-arm enum and
-- leave every other constructor undeclared.
-- Occurrences are not merged here: which ones name the same declaration is
-- a question of the RENDERED name -- a template instantiated twice is two
-- types, a generated type is one per instantiation whatever it was applied
-- to -- and rendering needs the translator, so the merge happens in
-- 'generateRustVariants'.
collectRustVariants :: [SerialManifold] -> [(FVar, [TypeF], [(Text, [TypeF])])]
collectRustVariants = concatMap (runIdentity . foldWithSerialManifoldM fm)
  where
    fm = defaultValue {opFoldWithNativeExprM = ne, opFoldWithSerialExprM = se}
    ne _ (DeserializeN_ t s xs) = return $ xs <> seek t <> seek (serialAstToType s)
    ne efull e = return $ foldlNE (<>) (seek (typeFof efull)) e
    se _ (SerializeS_ s xs) = return $ seek (serialAstToType s) <> xs
    se _ e = return $ foldlSE (<>) [] e

    seek :: TypeF -> [(FVar, [TypeF], [(Text, [TypeF])])]
    seek (VariantF v ps as) = (v, ps, as) : concatMap seek ps <> concatMap (concatMap seek . snd) as
    seek (NamF _ _ _ rs) = concatMap (seek . snd) rs
    seek (AppF t ts) = concatMap seek (t : ts)
    seek (FunF ts t) = concatMap seek (t : ts)
    seek (OptionalF t) = seek t
    seek _ = []

-- | Emit the enum definition and marshalling impls for every payload-bearing
-- @data@ type in the pool. Ownership follows the same rule as records and
-- enums: a user-mapped @data Rust => X = "..."@ writes its own type in
-- sourced Rust and gets only the impls.
generateRustVariants :: [SerialManifold] -> RustM [MDoc]
generateRustVariants es = do
  named <- mapM (\(v, ps, as) -> (\n -> (render n, (v, ps, as))) <$> rustTypeOf (VariantF v ps as))
                (collectRustVariants es)
  -- Merged by the RENDERED name, which is what the declaration is called: a
  -- template instantiated twice is two declarations, a generated type is
  -- one per instantiation, and keying by the general name would collapse
  -- `Try Str ()` and `Try Str (IFile a)` into one and leave the second use
  -- naming a type that was never emitted.
  concat <$> mapM makeOne (Map.elems (Map.fromListWith wider named))
  where
    -- Merge ARM-WISE rather than by arm count. A constructor literal's type
    -- reports only the arm being built, and reports it with no fields, so
    -- comparing lengths cannot tell a complete one-arm type from a
    -- truncated view of it -- and picking the truncated one would declare
    -- an arm as nullary that the schema says carries a payload, which
    -- writes RELNULL where the reader expects a pointer.
    -- Merge arm-wise, but keep DECLARATION ORDER: an arm's position is its
    -- wire tag, so sorting by name here would silently renumber every
    -- constructor. The longer list is the more complete view of the type and
    -- supplies the order; fields come from whichever occurrence has them,
    -- since a constructor literal's type reports its own arm with none.
    wider (v, ps, as) (_, _, bs) = (v, ps, [(n, pick n) | n <- order])
      where
        am = Map.fromList as
        bm = Map.fromList bs
        order = if length as >= length bs then map fst as else map fst bs
        pick n = case (Map.lookup n am, Map.lookup n bm) of
          (Just xs, Just ys) -> if null xs then ys else xs
          (Just xs, Nothing) -> xs
          (Nothing, Just ys) -> ys
          _ -> []

    makeOne (FV gv (CV cvText), ps, arms) = do
      userMapped <- cscopeDeclaresVariant gv cvText
      arms' <- mapM (\(n, ts) -> (,) n <$> mapM rustFieldType ts) arms
      name <- rustTypeOf (VariantF (FV gv (CV cvText)) ps arms)
      let impls = RP.printVariantImpls name arms'
      return $ if userMapped
                 then [impls]
                 else [RP.printRustVariant name arms', impls]

    cscopeDeclaresVariant :: TVar -> Text -> RustM Bool
    cscopeDeclaresVariant gv cvText = do
      cscope <- CMS.gets rsCScope
      return $ case Map.lookup gv cscope of
        Just entries -> any (\(_, body, _, _, _) -> bodyName body == Just cvText) entries
        Nothing -> False

-- | Emit the @ToVoidstar@/@FromVoidstar@ impls for every @data@ type used in
-- the pool, plus the enum definition itself when the pool owns it.
--
-- Ownership follows the record rule: a user-mapped @data Rust => X = "..."@
-- means the user writes the enum in sourced Rust and only the impls are
-- emitted. Otherwise the pool generates both. The @gv == cv@ shape alone
-- cannot decide this -- a user-written @data Rust => Foo = "Foo"@ produces
-- exactly the same FVar as an unmapped @Foo@ -- so the concrete scope is
-- consulted, as 'Cpp.cscopeMatches' does for the same ambiguity.
generateRustEnums :: [SerialManifold] -> RustM [MDoc]
generateRustEnums es = do
  named <- mapM (\(v, ps, ns) -> (\n -> (render n, (v, ps, ns))) <$> rustTypeOf (EnumF v ps ns))
                (collectRustEnums es)
  -- One entry per RENDERED name, as for variants: a user's template is one
  -- type per instantiation, a generated enum one whatever it was applied
  -- to. The longest constructor list is the complete one.
  concat <$> mapM makeOne (Map.elems (Map.fromListWith longest named))
  where
    longest a@(_, _, as) b@(_, _, bs) = if length as >= length bs then a else b

    makeOne (FV gv (CV cvText), ps, ctors) = do
      userMapped <- cscopeDeclares gv cvText
      name <- rustTypeOf (EnumF (FV gv (CV cvText)) ps ctors)
      let impls = RP.printEnumImpls name ctors
      return $ if userMapped
                 then [impls]
                 else [RP.printRustEnum name ctors, impls]

    -- True iff the concrete scope holds an entry for @gv@ whose body names
    -- @cvText@; that is what distinguishes a real per-language mapping from
    -- a name the compiler defaulted to the type's own.
    cscopeDeclares :: TVar -> Text -> RustM Bool
    cscopeDeclares gv cvText = do
      cscope <- CMS.gets rsCScope
      return $ case Map.lookup gv cscope of
        Just entries -> any (\(_, body, _, _, _) -> bodyName body == Just cvText) entries
        Nothing -> False

-- | Emit a struct definition (only for autogenerated @= "struct"@ records) plus
-- @ToVoidstar/FromVoidstar@ impls for every record used in the pool. User-mapped
-- records provide their own struct (in sourced Rust), so only the impls are
-- emitted for them.
generateRustStructs :: Map.Map Int ([SerialAST], [SerialAST], SerialAST) -> [SerialManifold] -> RustM [MDoc]
generateRustStructs closureAsts es = concat <$> mapM makeOne (collectRustRecords es)
  where
    -- Each record's closure fields (keyed by the record's general TVar + field
    -- Key), harvested from every (de)serialization site; the 'SerialClosure'
    -- carries the arg/result wire schemas the reify/reflect need. Duplicate keys
    -- (the same record with a closure field at different arities) keep the last
    -- -- a rare corner also collapsed by 'collectRustRecords' (nubBy general TVar).
    -- A record is also reached through a CLOSURE's captured or bound
    -- arguments, whose wire forms no manifold serializes directly -- the
    -- closure's own dispatch wrapper is what deserializes them. Harvesting
    -- only the manifolds leaves such a record without marshalling, and the
    -- generated wrapper then fails to compile against it.
    closureObjects =
      [ o
      | (caps, bnds, res) <- Map.elems closureAsts
      , ast <- caps <> bnds <> [res]
      , o <- serialObjectsOfAST ast
      ]
    harvest :: Map.Map (TVar, Key) SerialAST
    harvest =
      Map.fromList
        [ ((g, k), s)
        | (FV g _, flds) <- concatMap collectSerialObjects es <> closureObjects
        , (k, s@(SerialClosure _ _)) <- flds
        ]

    -- A function field (needs `Rc<dyn MorlocFnN>` boxing + reify/reflect); a
    -- suspension field is one of arity zero.
    isFunF = isFunctionTypeF

    -- The marshalling strategy for a field: a function field that has a harvested
    -- crossing 'SerialClosure' gets reify/reflect; anything else is plain. A
    -- function field with NO harvest hit returns Nothing -- it is used natively
    -- but never crosses in this pool, so its record stays struct-only.
    fieldMarshal :: TVar -> Key -> TypeF -> RustM (Maybe RP.ClosureMarshal)
    fieldMarshal g k ty
      | isFunF ty = case Map.lookup (g, k) harvest of
          Just s@(SerialClosure sins _) -> do
            assembler <- rustReflectClosureAssembler s
            return $
              Just
                RP.ClosureMarshal
                  { RP.cmReify = "reify" <> pretty (length sins)
                  , RP.cmReflect = assembler
                  }
          _ -> return Nothing
      | otherwise = return Nothing

    makeOne :: (FVar, [TypeF], [(Key, TypeF)]) -> RustM [MDoc]
    makeOne (v@(FV gv _), ps, rs) = case v of
      -- Autogenerated `= "struct"` record: drive from the unified RecEntry so a
      -- field whose native and wire types diverge (a custom-packer field)
      -- becomes a GENERIC parameter -- one struct `S<T1> { w: T1, .. }` covers
      -- both the native (`S<MyWrap>`) and wire (`S<i64>`) instantiations, the
      -- Rust analogue of C++'s template-field records. A closure field also
      -- diverges (native `Rc<dyn MorlocFnN>` vs wire `ClosureOrigin`), so it is a
      -- generic param here and cannot carry a per-field reify/reflect -- an
      -- autogenerated struct with a function field is therefore struct-only (use
      -- a user-mapped `record Rust => X = "..."` for a crossing closure record).
      FV _ (CV "struct") -> do
        recmap <- CMS.gets rsRecmap
        case lookup (v, map fst rs) recmap of
          Just rec -> do
            let assigned = assignGenerics (1 :: Int) (recFields rec)
                params = [p | (_, Left p) <- assigned]
            fields <- mapM (oneField gv (recName rec)) assigned
            let hasFun = any (containsFunF . snd) rs
                fields4 = [(fld, ty, w, Nothing) | (fld, ty, w) <- fields]
                impls = [RP.printRecordImpls (recName rec) params fields4 | not hasFun]
            return $ RP.printRustStruct (recName rec) params [(fld, ty) | (fld, ty, _) <- fields] : impls
          Nothing -> error $ "Rust: autogenerated record missing from recmap: " <> show v
      -- User-mapped record: the user writes the (monomorphic) struct, so only the
      -- marshalling impls are emitted, with concrete field types. A closure field
      -- reifies/reflects in place (via 'fieldMarshal'); the impl is emitted only
      -- when EVERY function field has a harvested crossing site, else struct-only.
      FV _ (CV _) -> do
        -- The struct's name at this instantiation: a template the user
        -- wrote takes this occurrence's arguments.
        name <- rustTypeOf (NamF NamRecord v ps rs)
        fields <- mapM (oneField gv name . fmap Right) rs
        marshals <- mapM (\(k, ty) -> fieldMarshal gv k ty) rs
        let fields4 = zipWith (\(fld, ty, w) m -> (fld, ty, w, m)) fields marshals
            -- Emit the impl only when EVERY function field has a harvested marshal
            -- (a non-function field imposes no requirement); else struct-only.
            emitImpl = and (zipWith (\(_, ty) m -> not (isFunF ty) || maybe False (const True) m) rs marshals)
        return [RP.printRecordImpls name [] fields4 | emitImpl]

    -- Number the generic (native/=wire) fields `T1, T2, ...`; concrete fields
    -- keep their unified type.
    assignGenerics :: Int -> [(Key, Maybe TypeF)] -> [(Key, Either MDoc TypeF)]
    assignGenerics _ [] = []
    assignGenerics n ((k, Nothing) : fs) = (k, Left ("T" <> pretty n)) : assignGenerics (n + 1) fs
    assignGenerics n ((k, Just t) : fs) = (k, Right t) : assignGenerics n fs

    -- Render one field's (name, type, is-variable-width). A generic field is a
    -- bare parameter (variable width, unknown until instantiated); a concrete
    -- field Box'es a self-optional (?self) so the recursive cycle is Sized.
    oneField :: TVar -> MDoc -> (Key, Either MDoc TypeF) -> RustM (MDoc, MDoc, Bool)
    oneField _ _ (k, Left param) = return (RP.rustFieldIdent k, param, True)
    oneField selfGv selfName (k, Right ty) = do
      ty' <- case ty of
        OptionalF inner | refsRecord selfGv inner -> return $ "::std::option::Option<::std::boxed::Box<" <> selfName <> ">>"
        _ -> rustFieldType ty
      return (RP.rustFieldIdent k, ty', isVarWidthF ty)

    -- True when a type is an immediate reference back to the enclosing record.
    refsRecord :: TVar -> TypeF -> Bool
    refsRecord g (NamF _ (FV g' _) _ _) = g == g'
    refsRecord g (VarF (FV g' _)) = g == g'
    refsRecord g (RecF (FV g' _)) = g == g'
    refsRecord _ _ = False

-- | Conservative: only pure fixed-width scalar (and all-fixed tuple) fields let
-- a record short-circuit shm_size to schema.width. Anything variable-region
-- (Str/Vec/Option/record) or unknown counts as variable (always safe -- a false
-- \"variable\" just uses the correct per-field sum).
isVarWidthF :: TypeF -> Bool
isVarWidthF (VarF (FV (TV gv) _)) = gv == "Str"
isVarWidthF (AppF (VarF (FV (TV gv) _)) ts)
  | T.isPrefixOf "Tuple" gv = any isVarWidthF (fst (partitionKindArgsF ts))
isVarWidthF _ = True

-- | The bound-variable indices that are borrowed (@&T@) parameters of a
-- manifold: its non-'Copy' NATIVE parameters. Serial (dispatch) parameters
-- deserialize to owned locals, so they are excluded.
borrowedIndicesOfForm :: (HasTypeM t) => ManifoldForm (Or TypeS TypeF) t -> Set.Set Int
borrowedIndicesOfForm form =
  Set.fromList [i | Arg i tm <- typeMofForm form, isBorrowed tm]
  where
    -- A function-typed parameter is rendered `&Rc<dyn MorlocFnN>` (see 'rustArgOf'),
    -- so it too is a borrowed reference; tracking it keeps a captured function
    -- from being double-referenced when it is forwarded into a nested closure.
    isBorrowed (Native tf) = not (rustIsCopy tf)
    isBorrowed (Function _ _) = True
    isBorrowed _ = False

-- | Lower a manifold body in its own ownership scope: 'oeCurrent' becomes the
-- manifold's borrowed parameter indices, 'oeParent' the enclosing manifold's
-- ('oeCurrent' from before), and 'oeShared' the manifold's used-more-than-once
-- indices. Because this extends the Reader environment with 'local' rather than
-- mutating state, the scope is purely lexical -- sibling and nested manifolds
-- cannot corrupt each other's view, and the answer does not depend on the fold's
-- evaluation order.
withManifoldScope :: Set.Set Int -> Set.Set Int -> Set.Set Int -> RustM a -> RustM a
withManifoldScope borrowed shared carried =
  local (\e -> OwnEnv {oeCurrent = borrowed, oeParent = oeCurrent e, oeShared = shared, oeParentShared = oeShared e, oeLoopCarried = carried})

-- | Run an action in the caller's ownership scope, by making 'oeCurrent' the
-- caller's set ('oeParent'). Used when rendering a manifold call whose arguments
-- are named by index-aliasing after the caller's variables. The loop-carried set
-- does not cross into a callee's argument rendering, so it resets to empty.
rustWithCallerScope :: RustM a -> RustM a
rustWithCallerScope = local (\e -> e {oeCurrent = oeParent e, oeShared = oeParentShared e, oeLoopCarried = Set.empty})

-- | The loop-carried variable indices of a manifold body, or empty if the body
-- is not a native loop. 'addLoopWraps' makes the loop the whole body (possibly
-- under structural wrappers and the entry deserialize lets), so walk that spine.
-- The wrapper set must stay in sync with 'Serialize.loopCarriedTypes', which
-- walks the identical spine to resolve the carried slots' types.
loopCarriedIdsSM :: SerialManifold -> Set.Set Int
loopCarriedIdsSM (SerialManifold _ _ _ _ se) = go se
  where
    go (LoopS _ ids _) = Set.fromList ids
    go (ReturnS x) = go x
    go (SerialLetS _ _ x) = go x
    go (NativeLetS _ _ x) = go x
    go (CacheBodyS _ _ _ _ _ x) = go x
    go (DebugWrapS _ _ _ x) = go x
    go _ = Set.empty

-- | Track each manifold's borrowed parameters as the body is folded, so
-- 'rustOwnership' can tell a borrowed @&T@ parameter from an owned value. For a
-- native-loop manifold the loop-carried locals are additionally recorded (for
-- the @let mut@ / never-move handling) and unioned into the shared set.
rustSurround :: SurroundManifoldM RustM PoolDocs PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs)
rustSurround =
  defaultValue
    { surroundSerialManifoldM = \recurse sm@(SerialManifold _ _ form _ _) ->
        let carried = loopCarriedIdsSM sm
         in withManifoldScope (borrowedIndicesOfForm form) (sharedIndicesSM sm `Set.union` carried) carried (recurse sm)
    , surroundNativeManifoldM = \recurse nm@(NativeManifold _ _ form _) ->
        withManifoldScope (borrowedIndicesOfForm form) (sharedIndicesNM nm) Set.empty (recurse nm)
    }

translateSegment :: SerialManifold -> RustM MDoc
translateSegment m0 = do
  resetCounter
  mask <- CMS.gets rsSrcTypeVarMask
  e <- surroundFoldSerialManifoldM rustSurround (defaultFoldRules (rustLowerConfig mask)) m0
  return $ renderPoolDocs e

-- | Local (filesystem-path) Rust crates declared in @local-deps: {rust: ...}@,
-- resolved to absolute paths for the generated pool @Cargo.toml@ (@crate = { path
-- = ... }@). Only the root module may declare these ('loadModuleMetadata' rejects
-- them elsewhere), so a DAG-wide union yields exactly the root's set. The path is
-- resolved against the project root (the entry module's directory) -- the native
-- real path; the container anchor rewrite is layered on separately.
rustLocalDeps :: MorlocMonad (Map.Map Text FilePath)
rustLocalDeps = do
  metas <- MM.gets statePackageMeta
  root <- fromMaybe "." <$> MM.gets stateProjectRoot
  let locals =
        [ (crate, T.unpack (ldPath ld))
        | m <- metas
        , (crate, ld) <- Map.toList (Map.findWithDefault Map.empty "rust" (packageLocalDeps m))
        ]
  Map.fromList <$> mapM (\(c, p) -> (,) c <$> liftIO (MS.canonicalizePath (root </> p))) locals

-- | DAG-wide union of every imported module's @rust-deps@ (crate -> semver),
-- written into the generated pool @Cargo.toml@. Two modules declaring the same
-- crate at different versions is a hard error (cargo cannot list a crate twice
-- and silent unification would be surprising).
rustDepsUnion :: MorlocMonad (Map.Map Text Text)
rustDepsUnion = do
  metas <- MM.gets statePackageMeta
  foldM
    ( \acc (crate, ver) -> case Map.lookup crate acc of
        Just v
          | v /= ver ->
              MM.throwSystemError $
                "conflicting rust-deps versions for crate "
                  <> squotes (pretty crate)
                  <> ": "
                  <> squotes (pretty v)
                  <> " vs "
                  <> squotes (pretty ver)
        _ -> return (Map.insert crate ver acc)
    )
    Map.empty
    (concatMap (map (\(c, ds) -> (c, dsVersion ds)) . Map.toList . packageRustDeps) metas)

-- | Render the generated pool crate's @Cargo.toml@ and @build.rs@. rustmorloc
-- is a path dependency on the source persisted at @$MORLOC_HOME/rust@ (by
-- @morloc init@); external crates come from the DAG-wide rust-deps union.
-- @build.rs@ carries the libmorloc.so link (link-search under
-- @$MORLOC_HOME/lib@; libmorloc is found at run time via the nexus-exported
-- LD_LIBRARY_PATH). The release profile must set @panic = "unwind"@ to match
-- rustmorloc/morloc-runtime (the SHM arena relies on Drop-on-unwind cleanup).
-- @crateName@ is unique per program (a source hash) so a shared target-dir
-- never collides one program's pool binary with another's.
makeCargoDocs :: Text -> Map.Map Text Text -> Map.Map Text FilePath -> FilePath -> (MDoc, MDoc)
makeCargoDocs crateName deps localCrates home =
  let rustmorlocPath = home </> "rust" </> "rustmorloc"
      libDir = home </> "lib"
      nameLit = dquotes (pretty crateName)
      -- A local crate (path dep) overrides any registry version line of the same
      -- name; cargo cannot list a crate twice.
      versionDeps = Map.filterWithKey (\c _ -> not (Map.member c localCrates)) deps
      depLines = [pretty crate <> " = " <> dquotes (pretty ver) | (crate, ver) <- Map.toList versionDeps]
      localLines =
        [ pretty crate <> " = { path = " <> dquotes (pretty path) <> " }"
        | (crate, path) <- Map.toList localCrates
        ]
      cargoToml =
        vsep
          [ "[package]"
          , "name = " <> nameLit
          , [idoc|version = "0.0.0"|]
          , [idoc|edition = "2021"|]
          , ""
          , "[[bin]]"
          , "name = " <> nameLit
          , [idoc|path = "src/main.rs"|]
          , ""
          , "[dependencies]"
          , "rustmorloc = { path = " <> dquotes (pretty rustmorlocPath) <> " }"
          , vsep depLines
          , vsep localLines
          , ""
          , "[profile.release]"
          , "opt-level = 2"
          -- lto matches the data/rust workspace profile so the rustmorloc built
          -- by `morloc init`'s warm-up (workspace profile) is reused here rather
          -- than recompiled under a differing (no-lto) fingerprint.
          , [idoc|lto = "thin"|]
          , [idoc|panic = "unwind"|]
          ]
      -- No runtime rpath: the pool is relocatable and finds libmorloc via
      -- LD_LIBRARY_PATH exported by the nexus at launch. link-search is kept
      -- for the build-time link only.
      buildRs =
        vsep
          [ "fn main() {"
          , [idoc|    println!("cargo:rustc-link-search=native=#{pretty libDir}");|]
          , [idoc|    println!("cargo:rustc-link-lib=dylib=morloc");|]
          , "}"
          ]
  in (cargoToml, buildRs)

-- | Build the Rust pool with @cargo build@ (one unified dependency resolution
-- over rustmorloc + any external crates), then copy the produced binary to the
-- @pool-rust.out@ path the manifest/nexus expect. @MORLOC_HOME@ is set on the
-- command so rustmorloc's build.rs locates libmorloc.so. The @--target-dir@ is
-- shared across programs so rustmorloc + deps compile once and cache (cargo's
-- own lock serialises concurrent builds; the per-program @crateName@ keeps each
-- program's output binary distinct within that shared dir).
makeTheMaker :: Text -> MorlocMonad [SysCommand]
makeTheMaker crateName = do
  -- cargo is required at make time (a Rust pool is a Cargo project). Fail fast
  -- with a clear message rather than a raw shell "command not found".
  cargoAvail <- liftIO (findExecutable "cargo")
  case cargoAvail of
    Just _ -> return ()
    Nothing ->
      MM.throwSystemError
        "building a Rust pool requires `cargo` on PATH (install Rust: https://rustup.rs)"
  home <- MM.asks configHome
  state <- MM.asks configState
  let poolSubdir = ML.poolDirKey rustLang
      outRel = pretty $ "pools" </> poolSubdir </> ML.makeExecutablePoolName rustLang
      manifestPath = pretty $ "pools" </> poolSubdir </> "Cargo.toml"
      -- The shared cargo build cache is regenerable STATE, not runtime: it lives
      -- under the state root, never inside the immutable runtime lib/.
      targetDir = state </> "cache" </> "rust-build"
      targetD = pretty targetDir
      binPath = pretty $ targetDir </> "release" </> T.unpack crateName
      homeD = pretty home
      -- Paths are single-quoted: they are interpolated into a shell string and
      -- $MORLOC_HOME (hence targetDir/binPath) may contain spaces.
      buildCmd =
        SysRun . Code . render $
          [idoc|MORLOC_HOME='#{homeD}' cargo build --release --manifest-path '#{manifestPath}' --target-dir '#{targetD}'|]
      copyCmd =
        SysRun . Code . render $
          [idoc|cp '#{binPath}' '#{outRel}'|]
  return [buildCmd, copyCmd]

-- | The lowering configuration. The core fields are real; the fields for
-- closures/partial application, remote calls, caching, and pattern evaluation
-- raise a clear v1-unsupported error and are unreachable for the pool shapes
-- v1 supports.
rustLowerConfig :: Map.Map SrcName [(Bool, Bool)] -> LowerConfig RustM
rustLowerConfig mask =
  LowerConfig
    { lcSrcName = \src -> pretty (srcName src)
    -- A curried host returns a function value, and a function value is
    -- applied through its trait method -- Rust has no call syntax for one.
    , lcApplySrcGroup = \f as ->
        parens f <> ".call" <> pretty (length as) <> tupled as
    , lcSourcedArg = \site own tm x ->
        -- Pass each argument to match how the callee's parameter is written,
        -- adapting by the argument's ownership so no unnecessary copy is made.
        -- A non-Copy parameter (and a sourced type-variable parameter, which is
        -- generic over `&A`) is a reference sink; a Copy parameter is a by-value
        -- (owned) sink; a closure application borrows every argument to match its
        -- `Fn(&T, ...)` signature. A function argument passes BY VALUE (the
        -- callee takes it as `F: Fn`; borrowing `&closure` breaks higher-ranked
        -- closure inference).
        let (isVar, isFunParam) = case site of
              SourcedArg src i ->
                maybe (False, False) (\bs -> if i < length bs then bs !! i else (False, False)) (Map.lookup (srcName src) mask)
              _ -> (False, False)
            -- A value parameter: a Copy type is a by-value (owned) sink, a
            -- non-Copy type is a reference sink.
            byType tf = if rustIsCopy tf then rustOwn own tf x else rustRef own x
            sourcedCallee = case site of SourcedArg _ _ -> True; _ -> False
         in case tm of
              -- A function-TYPED argument at a SOURCED call is one of two things:
              -- a GENUINE closure passed to an `F: Fn` parameter (`isFunParam`),
              -- which goes BY VALUE (borrowing `&closure` breaks higher-ranked
              -- inference); or a fully-applied sub-manifold VALUE, whose Function
              -- type is a misnomer (it renders as a call, the callee wants its
              -- RESULT) -- to a bare type-variable parameter (`isVar`) borrow it,
              -- else pass it like a Native arg of its result. At a MANIFOLD-call
              -- site the argument is always a function VALUE (a variable) going to
              -- a morloc-defined `&(impl Fn)` parameter, so borrow it.
              Function _ _
                | not sourcedCallee -> rustRef own x
                -- Host code may declare a higher-order parameter as
                -- `F: Fn(&A..) -> R`, which a trait object cannot satisfy, so
                -- the value is handed over through the runtime's one adapter.
                -- Its arity comes from the value's own type: the type at THIS
                -- site counts a partially applied manifold's captured context
                -- arguments too, so it cannot be read off here.
                | isFunParam ->
                    "rustmorloc::ThinFn::thin" <> parens (rustRef own x)
                | isVar -> rustRef own x
                | otherwise -> case tm of
                    Function _ (Native tf) -> byType tf
                    _ -> rustRef own x
              -- A closure application and a sourced type-variable parameter are
              -- reference sinks; otherwise pass by the value type. (`isVar` is only
              -- ever True under 'SourcedArg'.)
              Native tf
                | ClosureArg <- site -> rustRef own x
                | isVar -> rustRef own x
                | otherwise -> byType tf
              _ -> x
    , lcOwnership = rustOwnership
    , lcArgManifoldOwnership = \_ -> return Owned
    , lcOwnArg = rustOwn
    , lcWithCallerScope = rustWithCallerScope
    -- `?T` is `Option<T>`, whose wire layout is type-driven; a widened value must
    -- be a real `Some(..)` (never a bare `T`) or put_value serializes the wrong
    -- shape. The coercion only ever wraps a non-optional inner, so this is safe.
    -- A constructor in EXPRESSION position is spelled through the qualified
    -- path `<T>::Ctor`, which is legal for any rendered type -- a generic
    -- instantiation `MyBox<i64>` included, where `MyBox<i64>::Ctor` is not.
    -- In PATTERN position that path is not stable Rust, so a pattern names
    -- the bare head (`MyBox::Ctor`); the subject's type pins the arguments.
    , lcVariantLit = \ty n _ xs ->
        let arm = "<" <> ty <> ">::" <> pretty n
        in if null xs
             then arm
             else arm <> parens ("::std::boxed::Box::new" <> parens (RP.tupled1 xs))
    , lcEnumLit = \ty _ n _ -> "<" <> ty <> ">::" <> pretty n
    , lcVariantTagTest = \ty n _ subj ->
        "matches!" <> tupled [subj, RP.typeHead ty <> "::" <> pretty n <> " { .. }"]
    -- The whole payload sits behind one box, mirroring the wire form, so an
    -- arm has a single field whatever its arity and one spelling serves them
    -- all. The guard has already established the arm, so the other branch is
    -- unreachable rather than a fallback.
    -- Matched through a REFERENCE. A pattern binding several fields emits
    -- one projection per field against the same subject, so matching by
    -- value would move the payload on the first and leave the rest with
    -- nothing. Borrowing makes each projection independent; the clone is
    -- what hands an owned value on from a borrowed place.
    , lcCtorField = \ty n i subj ->
        "match" <+> "&" <> parens subj <+> "{"
          <+> RP.typeHead ty <> "::" <> pretty n <> "(mlc_b)"
          <+> "=> mlc_b." <> pretty i <> ".clone(),"
          <+> "_ => unreachable!()"
          <+> "}"
    , lcEnumTagTest = \ty _ n _ subj ->
        parens (subj <+> "==" <+> "<" <> ty <> ">::" <> pretty n)
    , lcCoerceOptional = \x -> "Some(" <> x <> ")"
    , lcTypeOf = \t -> Just . toIType <$> rustTypeOf t
    -- The serialize / raw-deserialize types use the WIRE form: a closure nested
    -- in an aggregate travels as its reified `ClosureOrigin` tuple, so the closure
    -- slot is typed as that tuple ('rustClosureWireLeaf'), not the native
    -- `impl Fn`/`Rc<dyn MorlocFnN>`. Mirrors the C++ 'cppClosureWireLeaf'.
    , lcSerialAstType = \s -> Just . toIType <$> rustTypeOf (wireSerialAstToType rustClosureWireLeaf s)
    , lcDeserialAstType = \s -> Just . toIType <$> rustTypeOf (shallowType s)
    , lcRawDeserialAstType = \s -> Just . toIType <$> rustTypeOf (wireSerialAstToType rustClosureWireLeaf s)
    , lcTypeMOf = \_ -> return Nothing
    , lcPackerName = \src -> pretty (srcName src)
    , lcUnpackerName = \src -> pretty (srcName src)
    -- A non-Copy arg to a pack/unpack function is borrowed to match the packer's
    -- `&T` parameter (a Copy arg is passed by value).
    , lcBorrowPackArg = \t -> not (rustIsCopy t)
    , lcRecordAccessor = \_ _ record field -> record <> "." <> field
    , lcDeserialRecordAccessor = \_ k v -> v <> "." <> pretty k
    , lcTupleAccessor = \i v -> v <> "." <> pretty i
    , lcNewIndex = getCounter
    , lcPrintExpr = RP.printExpr
    , lcPrintStmt = RP.printStmt
    , lcEvalPattern = rustEvalPattern
    , lcListConstructor = \_ _ es -> "vec![" <> hcat (punctuate ", " es) <> "]"
    , lcTupleConstructor = \_ _ es -> tupled es
    , lcRecordConstructor = \recType _ _ _ rs -> do
        name <- rustStructCtor recType
        return $ defaultValue {poolExpr = name <+> "{" <+> RP.rustRecordFields rs <+> "}"}
    -- A function value already IS its stored representation, so storing one
    -- is the identity. Re-boxing would compile -- an `Rc<dyn MorlocFnN>` is
    -- itself a `MorlocFnN`, so `Rc::new(rc) as Rc<dyn ..>` type-checks -- and
    -- would silently cost an allocation and an indirection on every store.
    , lcStoreField = \_ v -> return v
    -- Apply a function value via the MorlocFnN trait (`f.callN(args)`): a thin
    -- closure monomorphizes and inlines, a boxed one dispatches. Zero-cost calls.
    , lcApplyClosure = \callee args -> callee <> ".call" <> pretty (length args) <> tupled args
    , lcForeignCall = \socketFile mid args ->
        let argList = "&[" <> hcat (punctuate ", " [a <+> "as *const u8" | a <- args]) <> "]"
         in [idoc|rustmorloc::foreign_call(#{dquotes socketFile}, #{pretty mid}, #{argList})|]
    -- A remote call: args are already-serialized packet pointers (like a
    -- foreign call), so no put_value here. Resources fill the C `resources_t`
    -- with the C++ defaults (-1 for memory/time/cpus, 0 for gpus).
    , lcRemoteCall = \socketFile mid res args ->
        let (rmem, rtime, rcpu, rgpu) = remoteResourceInts res
            (resMem, resTime, resCPU, resGPU) = (pretty rmem, pretty rtime, pretty rcpu, pretty rgpu)
            argList = "&[" <> hcat (punctuate ", " [a <+> "as *const u8" | a <- args]) <> "]"
            call = "rustmorloc::remote_call(" <> pretty mid <> ", " <> dquotes socketFile
                     <> ", \".morloc-cache\", " <> resMem <> ", " <> resTime <> ", "
                     <> resCPU <> ", " <> resGPU <> ", " <> argList <> ")"
         in return $ defaultValue {poolExpr = call}
    , lcCacheBody = rustCacheBody
    , lcDebugWrap = \_ _ body -> return body
    , lcMakeLet = rustMakeLet
    , lcReleaseStmt = \_ -> ""
    , lcReturn = \e -> "return" <+> e <> ";"
    , lcMakeIf = rustMakeIf
    , lcMakeLoop = rustMakeLoop
    -- An effect thunk captures by value only when it can outlive the frame that
    -- builds it. The only such position is a manifold return typed
    -- @impl MorlocFn0<T>@ ('rustReturnType'); every other renderer erases the
    -- effect row, so a closure reaching one is already a type error. A thunk in
    -- any other frame is forced on the spot or handed to @mlc_catch@, which
    -- consumes both arms before returning, so it borrows what it reads and
    -- leaves the value usable afterwards. The C++ member captures by copy
    -- unconditionally, which is safe there because a copy leaves the original
    -- intact; @move@ does not.
    -- The thunk BORROWS what it reads, always. Every suspension has already
    -- become a closure manifold with an explicit capture list by the time a
    -- pool is rendered, so the only thunk left here is the inline device a
    -- `try` runs immediately in the same scope: it never escapes, and nothing
    -- it touches may be taken from the frame that is still using it.
    --
    -- Capturing by value instead would steal every non-'Copy' value the body
    -- reads -- not because THIS thunk outlives anything, but because some
    -- unrelated part of the same frame happened to hold a function value.
    , lcMakeDoBlock = \_ stmts expr ->
        return
          ( []
          , case stmts of
              [] -> "||" <+> "{" <+> expr <+> "}"
              _ -> "||" <+> "{" <> nest 4 (line <> vsep (stmts ++ [expr])) <> line <> "}"
          )
    -- The helper hands back a plain Result so the panic-payload downcast
    -- (which decides what is catchable) stays in rustmorloc; the arms are
    -- built here because only the caller knows this Try's representation.
    , lcMakeTry = \thunk _ okWrap errWrap ->
        "rustmorloc::mlc_try" <> tupled
          [ thunk
          , "|mlcTryV|" <+> okWrap "mlcTryV"
          , "|mlcTryM|" <+> errWrap "mlcTryM"
          ]
    , lcSerialize = defaultSerialize (rustLowerConfig mask)
    , lcDeserialize = \_ -> defaultDeserialize (rustLowerConfig mask)
    -- Reify a crossing closure to its wire tuple via the arity-indexed trait
    -- method `reifyN` (`ClosureOrigin` = `(String, i64, Vec<Vec<u8>>)`). This is
    -- uniform across a closure built here and a proxy reflected from another
    -- pool, which delegates through the pointer. A function value with no
    -- origin -- one host code created, or one whose context is not wholly
    -- native -- cannot be sent, and 'require_origin' says so by name rather
    -- than panicking on a `None`. The origin is borrowed, and the wire
    -- aggregate that receives it is owned, so it is cloned here.
    , lcReifyClosure = \v s -> case s of
        SerialClosure ins _ ->
          return $ "rustmorloc::require_origin(" <> parens v <> ".reify" <> pretty (length ins) <> "())"
        _ -> error "Rust lcReifyClosure: expected SerialClosure"
    , lcReflectClosure = \pkt s -> rustReflectClosure pkt s
    , lcReflectClosureParsed = \tup s -> rustReflectClosureParsed tup s
    -- Divert a list/tuple/optional through the shared structural path when it
    -- carries a closure leaf NOT enclosed in a nominal record. A closure inside a
    -- record field is marshalled by that record's own (nominal) ToVoidstar impl,
    -- so a record node -- and a list/optional OF records ([R]/?R) -- is NOT
    -- diverted (it falls through to Vec/Option + the record impl). See
    -- 'rustDivertsClosure'.
    , lcDivertNestedClosure = rustDivertsClosure
    , lcMakeFunction = rustMakeFunction
    -- A bare function passed to a HOF is a closure over all its params with no
    -- captured context; a lambda/section is a closure over its remaining params
    -- with the applied args captured. Both are the same safe wrapper.
    , lcMakePass = \sig mname params -> rustClosureWrapper sig mname [] params
    , lcMakeLambda = \sig mname contextArgs boundArgs -> rustClosureWrapper sig mname contextArgs boundArgs
    -- A closure's own type IS its rendered function-value type: one
    -- trait object, the same spelling it has in every other position.
    , lcClosureSig = rustStoredType
    , lcRegisterSchema = rustRegisterSchema
    , lcTableImportFn = Nothing
    }

-- | Assemble a @let@ binding at the PoolDocs level. A serialize let (mt =
-- Nothing) binds an owned packet pointer; a native let binds its native type.
-- | Does a type render as a function value? A function-valued binding omits
-- its annotation and lets Rust infer it, which keeps the binding readable and
-- costs nothing: the value has one spelling, so there is nothing to disambiguate.
isFunctionTypeF :: TypeF -> Bool
-- A suspension is a function value of no arguments, so its binding omits
-- the annotation like any closure's.
isFunctionTypeF (FunF _ _) = True
isFunctionTypeF _ = False

-- The borrow-safe flag ('isBorrowableProjection') is unused: Rust binds owned
-- locals and adapts a borrowed/place RHS via 'adaptOwnedElem' upstream.
rustMakeLet :: (Int -> MDoc) -> Int -> Maybe TypeF -> Bool -> PoolDocs -> PoolDocs -> RustM PoolDocs
rustMakeLet namer letIndex mt _ p1 p2 = do
  annDoc <- case mt of
    Just t | isFunctionTypeF t -> return ""
    Just t -> do ts <- rustTypeOf t; return (":" <+> ts)
    Nothing -> return (":" <+> "*mut u8")
  -- A loop-carried local's entry deserialize let is reassigned each iteration by
  -- the loop's continue, so it must bind mutably.
  carried <- asks oeLoopCarried
  let letKw = if Set.member letIndex carried then "let mut" else "let"
      letLine = letKw <+> namer letIndex <> annDoc <+> "=" <+> poolExpr p1 <> ";"
      rs = poolPriorLines p1 <> [letLine] <> poolPriorLines p2
  return $
    PoolDocs
      { poolCompleteManifolds = poolCompleteManifolds p1 <> poolCompleteManifolds p2
      , poolExpr = poolExpr p2
      , poolPriorLines = rs
      , poolPriorExprs = poolPriorExprs p1 <> poolPriorExprs p2
      , poolReturnFlag = poolReturnFlag p1 || poolReturnFlag p2
      }

-- | Native tail-loop assembly. Walks the 'LoopBody' decision tree into a Rust
-- @loop { ... }@ expression bound to the serial result: guards render to
-- @if/else@, native/serial lets to @let@ locals, a base leaf to @break <base>;@
-- (the loop expression's value), and a continue leaf to @let@ temps for each new
-- value followed by reassignment of the loop-carried locals. The loop-carried
-- vars are the manifold's own owned native locals (deserialized once at entry and
-- bound @let mut@ by 'rustMakeLet'); every continue value was already owned by
-- 'adaptLoopBodyOwned' (a passthrough carried var cloned, a fresh value moved), so
-- computing all temps before any reassignment is safe against the
-- parallel-assignment hazard (a temp reads a carried var by borrow/clone, never
-- moving it out from under a later temp).
rustMakeLoop :: [Int] -> LoopBody PoolDocs PoolDocs -> RustM PoolDocs
rustMakeLoop ids body = do
  resultIdx <- getCounter
  let resultVar = helperNamer resultIdx
  bodyLines <- walk body
  let loopExpr = vsep ["loop {", indent 4 (vsep bodyLines), "}"]
      resultDecl = "let" <+> resultVar <> ": *mut u8 =" <+> loopExpr <> ";"
      leaves = loopBodyLeaves body
  return $ PoolDocs
    { poolCompleteManifolds = concatMap poolCompleteManifolds leaves
    , poolExpr = resultVar
    , poolPriorLines = [resultDecl]
    , poolPriorExprs = concatMap poolPriorExprs leaves
    , poolReturnFlag = True
    }
  where
    walk (LoopBase seDocs) =
      return $ poolPriorLines seDocs <> ["break" <+> poolExpr seDocs <> ";"]
    walk (LoopContinue contDocs) = do
      tmpVars <- map helperNamer <$> mapM (const getCounter) contDocs
      let priors = concatMap poolPriorLines contDocs
          tmpAssigns = zipWith (\tv cd -> "let" <+> tv <+> "=" <+> poolExpr cd <> ";") tmpVars contDocs
          reassigns = zipWith (\i tv -> nvarNamer i <+> "=" <+> tv <> ";") ids tmpVars
      return $ priors <> tmpAssigns <> reassigns
    walk (LoopNLet i neDocs b) = do
      rest <- walk b
      return $ poolPriorLines neDocs <> ["let" <+> nvarNamer i <+> "=" <+> poolExpr neDocs <> ";"] <> rest
    walk (LoopSLet i seDocs b) = do
      rest <- walk b
      return $ poolPriorLines seDocs <> ["let" <+> svarNamer i <+> "=" <+> poolExpr seDocs <> ";"] <> rest
    walk (LoopIf guardDocs t e) = do
      tLines <- walk t
      eLines <- walk e
      let ifBlock = vsep
            [ "if" <+> poolExpr guardDocs <+> "{"
            , indent 4 (vsep tLines)
            , "} else {"
            , indent 4 (vsep eLines)
            , "}"
            ]
      return $ poolPriorLines guardDocs <> [ifBlock]

-- | Native @if@ expression, bound to a fresh temp so it composes as a value.
-- The arms are already adapted to the owned result type by the shared 'IfN_'
-- lowering (via 'adaptOwnedElem'), so they splice directly into the owned let.
rustMakeIf :: NativeExpr -> PoolDocs -> PoolDocs -> PoolDocs -> RustM PoolDocs
rustMakeIf origExpr condDocs thenDocs elseDocs = do
  idx <- getCounter
  let v = helperNamer idx
  -- Both arms have the same rendered type, function-valued or not, so the
  -- conditional SELECTS one and the binding names that type. A suspension
  -- used to be special-cased into a third closure that re-tested the
  -- condition on every force, because no binding could name the two arms'
  -- distinct closure types; one spelling for a function value removed the
  -- reason. Selecting costs one allocation and one indirection less, and
  -- evaluates the condition once.
  ifStmt <- do
      typeStr <- rustTypeOf (typeFof origExpr)
      return $
        vsep
          [ "let" <+> v <> ":" <+> typeStr <+> "= if" <+> parens (poolExpr condDocs) <+> "{"
          , indent 4 (vsep (poolPriorLines thenDocs ++ [poolExpr thenDocs]))
          , "} else {"
          , indent 4 (vsep (poolPriorLines elseDocs ++ [poolExpr elseDocs]))
          , "};"
          ]
  return $
    PoolDocs
      { poolCompleteManifolds =
          poolCompleteManifolds condDocs <> poolCompleteManifolds thenDocs <> poolCompleteManifolds elseDocs
      , poolExpr = v
      , poolPriorLines = poolPriorLines condDocs <> [ifStmt]
      , poolPriorExprs = poolPriorExprs condDocs <> poolPriorExprs thenDocs <> poolPriorExprs elseDocs
      , poolReturnFlag = poolReturnFlag condDocs || poolReturnFlag thenDocs || poolReturnFlag elseDocs
      }

-- | Emit a manifold function definition (@unsafe fn m{i}(..) -> ..@).
-- Deduplicated per (local/remote) so a manifold visited from multiple sites is
-- defined once, matching the C++ member.
rustMakeFunction ::
  Int -> MDoc -> [Arg TypeM] -> TypeM -> [MDoc] -> MDoc -> Maybe HeadManifoldForm -> RustM (Maybe MDoc)
rustMakeFunction callIndex mname args manifoldType priorLines body headForm = do
  st <- CMS.get
  let isRemote = case headForm of
        Just HeadManifoldFormRemoteWorker -> True
        _ -> False
      done =
        if isRemote
          then Set.member callIndex (rsRemoteSet st)
          else Set.member callIndex (rsLocalSet st)
  if done
    then return Nothing
    else do
      CMS.modify $ \s ->
        if isRemote
          then s {rsRemoteSet = Set.insert callIndex (rsRemoteSet s)}
          else s {rsLocalSet = Set.insert callIndex (rsLocalSet s)}
      -- A function value is a trait object in every position, so a returned
      -- closure names no lifetime: it owns what it captures ('capInit')
      -- and a captured function value is an `Rc` clone, not a borrow.
      retStr <- rustReturnType manifoldType
      typedArgs <- mapM rustArgOf args
      let fullName = mname <> if isRemote then "_remote" else ""
          decl = "unsafe fn" <+> fullName <> tupled typedArgs <+> "->" <+> retStr
          -- Per-manifold traceback frame: on a panic unwind the FrameGuard
          -- appends this line to the thread-local trace, which dispatch_guard
          -- folds onto the throw message (mirrors the C++ member's frame).
          (userName, srclocStr) = rsDebugInfo st callIndex
          nameOut = if T.null userName then "_" else userName
          srclocSuffix = if T.null srclocStr then "" else ", " <> srclocStr
          frameText =
            "\n  at " <> nameOut <> " [rust] (mid=" <> T.pack (show callIndex) <> srclocSuffix <> ")"
          frameStmt =
            "let _mlc_frame = rustmorloc::FrameGuard::new("
              <> dquotes (pretty (RP.rustEscape frameText)) <> ");"
          -- A labeled manifold is wrapped in a LogGuard: start on
          -- construction, pass or fail on drop. Nothing is placed after the
          -- body because a generated body ends in `return <expr>;`, which
          -- makes any following statement unreachable -- Drop runs on every
          -- path out regardless. The foreign-callee side is skipped for the
          -- same reason as in the C++ member -- the caller pool's wrap already
          -- spans the round trip.
          litOrEmpty = maybe (dquotes mempty) (\t -> dquotes (pretty (RP.rustEscape t)))
          logged = case Map.lookup callIndex (rsLogTemplates st) of
            Just tmpl | not (isForeignCalleeForm headForm) -> Just tmpl
            _ -> Nothing
          bodyLines = case logged of
            Nothing -> priorLines ++ [body]
            Just tmpl ->
              let guardStmt =
                    "let _mlc_log = rustmorloc::LogGuard::new("
                      <> hsep (punctuate ","
                           [ dquotes (pretty (RP.rustEscape (renderedGroup tmpl)))
                           , litOrEmpty (renderedStart tmpl)
                           , litOrEmpty (renderedPass tmpl)
                           , litOrEmpty (renderedFail tmpl)
                           , litOrEmpty (renderedBenchKey tmpl)
                           ])
                      <> ");"
               in guardStmt : priorLines ++ [body]
      return . Just $
        vsep [decl <+> "{", indent 4 (vsep (frameStmt : bodyLines)), "}"]

-- | Whether a closure wire form has a closure among its arguments or as its
-- result.
