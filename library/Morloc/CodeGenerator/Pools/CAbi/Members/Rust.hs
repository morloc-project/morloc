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

import Control.Monad (foldM)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import qualified Control.Monad.State.Strict as CMS
import Data.Function (on)
import Data.List (nubBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.CodeGenerator.Grammars.Common
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
import Morloc.CodeGenerator.Serial (serialAstToMsgpackSchema, serialAstToType, shallowType, wireSerialAstToType)
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
  , rsRecmap :: RecMap
  -- ^ Unified record types used in this pool; drives struct generation, the
  -- concrete struct name in 'rustTypeOf', and per-record marshalling impls.
  , rsCScope :: Scope
  -- ^ The merged Rust concrete typedef scope; resolves a recursive back-ref
  -- (@RecF@) to its concrete struct name (as C++'s translatorCScope does).
  , rsSrcTypeVarMask :: Map.Map SrcName [(Bool, Bool)]
  -- ^ Per sourced function, per parameter position: @(isBareTypeVar,
  -- isFunctionParam)@ from the declared morloc signature. @isBareTypeVar@: the
  -- parameter is a BARE type variable, so it is passed by reference (@&A@) even
  -- at a Copy instantiation (the sourced Rust fn is generic over @&A@).
  -- @isFunctionParam@: the parameter is itself function-typed (@F: Fn@), so a
  -- function-valued argument is a genuine closure passed BY VALUE; when False,
  -- a function-typed argument is a fully-applied sub-manifold VALUE, passed like
  -- data (borrowed when non-'Copy').
  , rsReifyInfo :: Map.Map Text (Int, [Int])
  -- ^ Per crossing-closure body-manifold name: @(mid, capturedSchemaIds)@.
  -- Only closures that reach a serialize boundary appear here (the rest stay
  -- thin @impl Fn@/@Rc@ with no reify cost). Drives 'rustClosureWrapper' to
  -- build a @FatClosure@ carrying the closure's @(sockid, mid, captured)@
  -- origin instead of a bare closure.
  }

instance Defaultable RustState where
  defaultValue = RustState 0 Map.empty Set.empty Set.empty (\_ -> ("", "")) [] Map.empty Map.empty Map.empty

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
    f :: TypeF -> RustM MDoc
    f (UnkF (FV _ x)) = return (pretty x)
    f (VarF (FV _ x)) = return (pretty x)
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
      return $ "Option<Box<" <> t' <> ">>"
    f (OptionalF t) = do
      -- The payload is a stored position; a function payload boxes ('rustFieldType').
      t' <- rustFieldType t
      return $ "Option<" <> t' <> ">"
    f (NatLitF _) = return mempty
    f NatVoidF = return mempty
    f (StrLitF _) = return mempty
    f StrVoidF = return mempty
    -- A function type renders as a monomorphic `impl Fn` closure (by-reference
    -- arguments, by-value result), matching 'rustArgType' on a 'Function'.
    f (FunF ts t) = do
      argTs <- mapM (\a -> ("&" <>) <$> f a) ts
      retT <- f t
      return $ "impl Fn" <> tupled argTs <+> "->" <+> retT
    -- Effects erase for type rendering: an effectful value's rendered type is
    -- its result type. Effect sequencing is carried by do-block thunks + eval,
    -- not by the type.
    f (EffectF _ t) = f t
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
    -- User-mapped record: the concrete struct name is the CVar text.
    f (NamF _ (FV _ (CV s)) _ _) = return (pretty s)
    -- Back-reference to a recursive record: resolve the concrete struct name
    -- via the concrete scope (the CVar slot is unreliable after weave).
    f (RecF (FV gv@(TV gvText) (CV cv)))
      | cv /= "struct" && cv /= gvText = return (pretty cv)
      | otherwise = do
          cscope <- CMS.gets rsCScope
          case Map.lookup gv cscope of
            Just ((_, body, _, _, _) : _) | Just name <- bodyName body -> return (pretty name)
            _ -> error $ "Rust: recursive record `" <> T.unpack gvText <> "` has no concrete mapping"

    -- Outer name of a concrete-scope typedef body, when it contributes a name.
    bodyName :: TypeU -> Maybe Text
    bodyName (VarU (TV n)) = Just n
    bodyName (AppU (VarU (TV n)) _) = Just n
    bodyName (NamU _ (TV n) _ _) = Just n
    bodyName _ = Nothing

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
-- A function-typed parameter (a morloc-defined combinator abstracting over a
-- function, e.g. `flip`) becomes an `impl MorlocFnN<..>` parameter -- the same
-- fat/thin trait its arguments implement, so it accepts BOTH a thin closure
-- (monomorphized via the `Fn` blanket, zero cost) AND a boxed function value
-- (a record field, `Rc<dyn MorlocFnN>`, via the `Rc` blanket). It is applied via
-- `.callN(..)`. morloc monomorphizes manifolds, so the argument and result types
-- are concrete.
rustArgType (Function ts t) = do
  argTs <- mapM bareParam ts
  retT <- rustReturnType t
  return $ "impl rustmorloc::MorlocFn" <> pretty (length ts) <> "<" <> hcat (punctuate ", " (argTs <> [retT])) <> ">"
  where
    bareParam (Native tf) = rustTypeOf tf
    bareParam _ = return "_"

-- | The Rust return type of a manifold: a serial result is an owned packet.
rustReturnType :: TypeM -> RustM MDoc
rustReturnType (Function _ o) = rustReturnType o
rustReturnType (Serial _) = return "*mut u8"
rustReturnType Passthrough = return "*mut u8"
rustReturnType (Native t) = case typeFof t of
  -- A deferred effect value is returned as a nullary thunk `impl Fn() -> T`
  -- (the C++ member types it `std::function<T()>`), forced at the call site.
  EffectF _ inner -> do
    innerT <- rustTypeOf inner
    return $ "impl Fn() ->" <+> innerT
  tf -> rustTypeOf tf

-- | The Rust type of a record field. A function-typed field is stored as a fat
-- trait object @Rc<dyn MorlocFnN<A1,..,An,R>>@ -- a bare @impl Fn@ is illegal in
-- a field, and the trait object carries the reify capability a crossing closure
-- needs. Everything else renders as its ordinary 'rustTypeOf'.
rustFieldType :: TypeF -> RustM MDoc
rustFieldType (FunF ts t) = do
  argTs <- mapM rustTypeOf ts
  retT <- rustTypeOf t
  let n = length ts
      params = hcat (punctuate ", " (argTs <> [retT]))
  return $ "std::rc::Rc<dyn rustmorloc::MorlocFn" <> pretty n <> "<" <> params <> ">>"
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
rustDivertsClosure (SerialClosure _ _) = True
rustDivertsClosure (SerialObject _ _ _ _) = False
rustDivertsClosure (SerialList _ _ s) = rustDivertsClosure s
rustDivertsClosure (SerialTuple _ ss) = any rustDivertsClosure ss
rustDivertsClosure (SerialOptional _ s) = rustDivertsClosure s
rustDivertsClosure (SerialPack _ (_, s)) = rustDivertsClosure s
rustDivertsClosure _ = False

rustArgOf :: Arg TypeM -> RustM MDoc
rustArgOf a@(Arg _ t) = do
  ts <- rustArgType t
  -- Idiomatic asymmetric passing: a Copy scalar parameter is by value (`i64`);
  -- a non-Copy parameter (Str/Vec/record) is a shared reference (`&T`). Both
  -- are `Copy` at the manifold-param level (`&T` is Copy), so a value fans out
  -- to several callees with no move. Serial/passthrough args are Copy pointers.
  let ts' = case t of
        Native tf | not (rustIsCopy tf) -> "&" <> ts
        -- A captured function value is taken by reference (`&(impl Fn..)`): a
        -- `&F` still implements `Fn`, so the manifold stays re-callable across a
        -- HOF's per-element calls, whereas a by-value `impl Fn` would move out of
        -- the enclosing `move` closure on the first call.
        Function _ _ -> "&" <> ts
        _ -> ts
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

-- | Adapt a partial application's captured context argument to the closure
-- manifold's parameter. The captured value is a variable in the ENCLOSING
-- (caller) scope, so its ownership is read from 'oeParent': a non-'Copy' context
-- parameter is a reference sink, a 'Copy' one is a by-value (owned) sink --
-- matching how a manifold call passes its arguments.
rustBridgeContext :: Int -> TypeM -> MDoc -> RustM MDoc
-- A captured FUNCTION value (a combinator like `any`/`all` whose inner lambda
-- closes over a function argument) is intercepted by `bridgeCapture` (the sole
-- caller) and forwarded by reference as `&impl MorlocFnN` -- which stays
-- re-callable across the per-element calls -- so a Function never reaches here.
rustBridgeContext _ (Function _ _) _ =
  error "Rust: internal error -- a captured Function should be handled by bridgeCapture"
rustBridgeContext i (Native tf) name = do
  parent <- asks oeParent
  let own = if Set.member i parent then BorrowedRef else Owned
  return $ if rustIsCopy tf then rustOwn own tf name else rustRef own name
-- A captured Serial/Passthrough value is a `*const u8` (a Copy pointer): the
-- closure-body manifold takes it by value, so pass it as-is. (This case is only
-- reached when a closure captures a serialized value -- e.g. a list of closures
-- built in the consumer pool, each wrapping a foreign manifold over a serial
-- capture.) A leading `&` would make it `&*const u8`, which the parameter rejects.
rustBridgeContext _ _ name = return name

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

-- | The safe by-reference closure that adapts a manifold to a higher-order
-- function's @F: Fn@ (a bare @unsafe fn@ does not implement @Fn@). Remaining
-- (bound) parameters become the closure's typed @&T@ parameters -- explicit
-- types are required so rustc infers the higher-ranked @for<'a> Fn(&'a T)@
-- signature -- and are bridged to the manifold's parameter form; captured
-- context args are applied. Shared by 'lcMakePass' (no context) and
-- 'lcMakeLambda' (a partial application).
rustClosureWrapper :: MDoc -> [Arg TypeM] -> [Arg TypeM] -> RustM MDoc
rustClosureWrapper mname ctxArgs boundArgs = do
  boundTyped <- mapM (\a@(Arg _ t) -> do
                        base <- closureParamType t
                        return (argNamer a <> ":" <+> base)) boundArgs
  ctxParts <- mapM bridgeCapture ctxArgs
  let cloneLets = concatMap fst ctxParts
      ctxDocs = map snd ctxParts
      boundDocs = [rustBridgeArg t (argNamer a) | a@(Arg _ t) <- boundArgs]
      call = mname <> tupled (ctxDocs ++ boundDocs)
      closure = "move |" <> hcat (punctuate ", " boundTyped) <> "| unsafe {" <+> call <+> "}"
  -- A crossing closure (its body manifold is in the reify table) is wrapped in a
  -- `FatClosure` carrying its `(sockid, mid, captured)` origin, so a serialize
  -- boundary can reify it. A purely in-pool closure stays thin (no origin, no
  -- capture serialization).
  reifyInfo <- CMS.gets rsReifyInfo
  parentBorrowed <- asks oeParent
  let -- `reify_capture` wants `&T`. A captured value that is already a borrowed
      -- `&T` parameter (a non-Copy manifold parameter, tracked in 'oeParent') is
      -- forwarded as-is; an owned or Copy value is referenced with `&`.
      capRef c@(Arg ci _) =
        rustRef (if Set.member ci parentBorrowed then BorrowedRef else Owned) (argNamer c)
      wrapped = case Map.lookup (render mname) reifyInfo of
        Nothing -> closure
        Just (mid, capSids) ->
          -- The origin's captured packets are built (borrowing each captured
          -- value) BEFORE the `move` closure takes ownership -- struct fields
          -- evaluate left to right, so `origin` is written first.
          let capExprs =
                -- `reify_capture` is an unsafe fn; wrap each call so the origin
                -- compiles whether or not the enclosing context is already unsafe
                -- (a crossing closure may be built inside a safe do-block thunk).
                [ "unsafe { rustmorloc::reify_capture(" <> capRef c <> ", " <> sch sid <> ") }"
                | (c, sid) <- zip ctxArgs capSids
                ]
              origin =
                "(\"rust\".to_string(), " <> pretty mid <> "i64, vec!["
                  <> hcat (punctuate ", " capExprs) <> "])"
           in "rustmorloc::FatClosure { origin: " <> origin <> ", f: " <> closure <> " }"
  -- A shared non-Copy capture is cloned in a block that brackets the closure, so
  -- the `move` captures the clone and the original stays live for the caller.
  return $ case cloneLets of
    [] -> wrapped
    _ -> "{" <+> hsep cloneLets <+> wrapped <+> "}"

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
rustReflectClosure :: MDoc -> SerialAST -> RustM MDoc
rustReflectClosure pkt (SerialClosure ins out) = do
  argTs <- mapM (rustTypeOf . serialAstToType) ins
  resT <- rustTypeOf (serialAstToType out)
  tupSid <- rustRegisterSchema (render (serialAstToMsgpackSchema (SerialClosure ins out)))
  argSids <- mapM (rustRegisterSchema . render . serialAstToMsgpackSchema) ins
  resSid <- rustRegisterSchema (render (serialAstToMsgpackSchema out))
  let params = [ "__a" <> pretty i <> ": &" <> t | (i, t) <- zip [(0 :: Int) ..] argTs ]
      pushes =
        [ "__pkts.push(rustmorloc::put_value(__a" <> pretty i <> ", " <> sch sid <> "));"
        | (i, sid) <- zip [(0 :: Int) ..] argSids
        ]
      -- The captured packets and socket name are call-invariant, so `__pkts` is
      -- sized up front (captures + arguments) and only the per-call argument
      -- packets are appended. `__clo.2` is read INSIDE the closure (so disjoint
      -- capture keeps it alive); `__sock` is an independent owned String hoisted
      -- to the outer scope so it is formatted once, not on every application.
      bodyDoc =
        vsep
          [ "let mut __pkts: Vec<*const u8> = Vec::with_capacity(__clo.2.len() + " <> pretty (length argTs) <> ");"
          , "__pkts.extend(__clo.2.iter().map(|__c| __c.as_ptr()));"
          , vsep pushes
          , "rustmorloc::get_value::<" <> resT <> ">(rustmorloc::foreign_call(&__sock, __clo.1 as u32, &__pkts), " <> sch resSid <> ")"
          ]
  return $
    vsep
      [ "{"
      , indent 4 $
          vsep
            [ "let __clo: rustmorloc::ClosureOrigin = rustmorloc::get_value(" <> pkt <> ", " <> sch tupSid <> ");"
            , "let __sock = format!(\"pipe-{}\", __clo.0);"
            , "move |" <> hcat (punctuate ", " params) <> "| -> " <> resT <> " { unsafe {"
            , indent 4 bodyDoc
            , "} }"
            ]
      , "}"
      ]
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
  argTs <- mapM (rustTypeOf . serialAstToType) ins
  resT <- rustTypeOf (serialAstToType out)
  argSids <- mapM (rustRegisterSchema . render . serialAstToMsgpackSchema) ins
  resSid <- rustRegisterSchema (render (serialAstToMsgpackSchema out))
  let n = length ins
      params = ["__a" <> pretty i <> ": &" <> t | (i, t) <- zip [(0 :: Int) ..] argTs]
      pushes =
        [ "__pkts.push(rustmorloc::put_value(__a" <> pretty i <> ", " <> sch sid <> "));"
        | (i, sid) <- zip [(0 :: Int) ..] argSids
        ]
      dynT =
        "std::rc::Rc<dyn rustmorloc::MorlocFn" <> pretty n
          <> "<" <> hcat (punctuate ", " (argTs <> [resT])) <> ">>"
      bodyDoc =
        vsep
          [ "let mut __pkts: Vec<*const u8> = Vec::with_capacity(__cap.2.len() + " <> pretty n <> ");"
          , "__pkts.extend(__cap.2.iter().map(|__c| __c.as_ptr()));"
          , vsep pushes
          , "rustmorloc::get_value::<" <> resT <> ">(rustmorloc::foreign_call(&__sock, __cap.1 as u32, &__pkts), " <> sch resSid <> ")"
          ]
  return $ \tup ->
    vsep
      [ "{"
      , indent 4 $
          vsep
            [ "let __clo: rustmorloc::ClosureOrigin = " <> tup <> ";"
            , "let __sock = format!(\"pipe-{}\", __clo.0);"
            , "let __cap = __clo.clone();"
            , "std::rc::Rc::new(rustmorloc::FatClosure { origin: __clo, f: move |"
                <> hcat (punctuate ", " params) <> "| -> " <> resT <> " { unsafe {"
            , indent 4 bodyDoc
            , "} } }) as " <> dynT
            ]
      , "}"
      ]
rustReflectClosureAssembler _ = error "rustReflectClosureAssembler: expected SerialClosure"

-- | Reflect a closure NESTED in an aggregate: the enclosing get_value already
-- produced the wire tuple, so @tup@ is the parsed 'ClosureOrigin' (not a raw
-- packet). Build an origin-preserving @Rc<dyn MorlocFnN>@: the reflected value
-- is a 'FatClosure' carrying the ORIGINAL origin, so a later reify (@reifyN@)
-- reproduces the producing pool (a re-cross A->B->C calls back to A, not B).
rustReflectClosureParsed :: MDoc -> SerialAST -> RustM MDoc
rustReflectClosureParsed tup s = ($ tup) <$> rustReflectClosureAssembler s

-- | Bridge one captured context argument: returns any hoisted clone bindings and
-- the value passed into the inner manifold call. A SHARED non-'Copy' owned
-- capture (used elsewhere by the caller) is cloned to a fresh local and the call
-- takes a reference to the clone; everything else defers to 'rustBridgeContext'.
bridgeCapture :: Arg TypeM -> RustM ([MDoc], MDoc)
bridgeCapture a@(Arg i t) = do
  parentBorrowed <- asks oeParent
  parentShared <- asks oeParentShared
  case t of
    -- A captured FUNCTION value is forwarded into the inner manifold's
    -- `&impl MorlocFnN` parameter. If it is ALREADY a borrowed reference (a
    -- function-typed manifold parameter, `&impl MorlocFnN`), the `move` closure
    -- captures that Copy reference directly and passes it as-is -- hoisting a
    -- second `&` would make a `&&impl MorlocFnN`, which the inner parameter
    -- rejects. If it is an OWNED local closure, hoist `&f` so the closure
    -- captures the Copy reference and the function stays live for other uses.
    Function _ _ ->
      let name = argNamer a
       in if Set.member i parentBorrowed
        then return ([], name)
        else
          let refVar = name <> "_ref"
           in return (["let" <+> refVar <+> "=" <+> "&" <> name <> ";"], refVar)
    Native tf
      | Set.member i parentShared
          && not (rustIsCopy tf)
          && not (Set.member i parentBorrowed) ->
          let name = argNamer a
              cloneVar = name <> "_c"
           in return (["let" <+> cloneVar <+> "=" <+> name <> ".clone();"], rustRef Owned cloneVar)
    _ -> do
      d <- rustBridgeContext i t (argNamer a)
      return ([], d)

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

    isFunParam (FunU _ _) = True
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
  closureTable <- computeClosureSchemas rustLang es
  let st0 = defaultValue {rsDebugInfo = debugInfo, rsRecmap = recmap, rsCScope = mergedRustScope, rsSrcTypeVarMask = srcTypeVarMask}
      code = CMS.evalState (runReaderT (makeRustCode includeDocs closureTable es) emptyOwnEnv) st0

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

makeRustCode :: [MDoc] -> Map.Map Int ([Text], [Text], Text) -> [SerialManifold] -> RustM MDoc
makeRustCode includeDocs closureTable0 es = do
  structDocs <- generateRustStructs es
  -- Keep only closures that actually cross a boundary (their signature reaches a
  -- serialize site); the rest stay thin `impl Fn`/`Rc` with no reify cost.
  closureTable <- restrictToCrossingClosures es closureTable0
  -- Per crossing closure: a home-pool serial dispatch wrapper (so a foreign pool
  -- can apply it) and the reify info (mid + captured schema ids) that
  -- 'rustClosureWrapper' uses to build a `FatClosure` carrying the closure's
  -- origin instead of a bare closure.
  (closureWrappers, reifyInfo) <- makeClosureDispatch closureTable es
  CMS.modify $ \s -> s {rsReifyInfo = reifyInfo}
  program <- buildProgramM Map.empty Map.empty includeDocs es translateSegment getRustSchemaTable closureTable
  -- structDocs go in the schema-table section; the closure dispatch wrappers are
  -- free functions spliced into the signatures section.
  return $ RP.printProgram structDocs closureWrappers [] program

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
    astSig (SerialClosure ins out) = closureRustSig (map serialAstToType ins) (serialAstToType out)
    astSig _ = return "" -- collectSerializedClosures returns only SerialClosure

-- | Keep in the closure table only closures whose signature can cross a
-- boundary. Everything else is a purely in-pool closure that needs no reify or
-- dispatch machinery (and must stay a thin @impl Fn@/@Rc@ for zero cost).
restrictToCrossingClosures ::
  [SerialManifold] ->
  Map.Map Int ([Text], [Text], Text) ->
  RustM (Map.Map Int ([Text], [Text], Text))
restrictToCrossingClosures es closureTable = do
  crossingSigs <- crossingClosureSigs es
  let candidates =
        [ nm | nm@(NativeManifold i _ _ _) <- concatMap collectClosureManifolds es
             , Map.member i closureTable ]
  crossing <-
    Set.fromList . map fst . filter (\(_, sig) -> Set.member sig crossingSigs)
      <$> mapM (\nm@(NativeManifold i _ _ _) -> (,) i <$> manifoldRustSig nm) candidates
  return $ Map.filterWithKey (\i _ -> Set.member i crossing) closureTable

-- | For each crossing closure, emit a home-pool serial dispatch wrapper so a
-- foreign pool can apply it when its reflected proxy calls back on the closure's
-- manifold id: deserialize the captured ++ bound argument packets, call the
-- native closure-body manifold, and serialize the result. Also return, keyed by
-- manifold name, the @(mid, capturedSchemaIds)@ 'rustClosureWrapper' needs to
-- build the closure's @FatClosure@ origin.
makeClosureDispatch ::
  Map.Map Int ([Text], [Text], Text) ->
  [SerialManifold] ->
  RustM ([MDoc], Map.Map Text (Int, [Int]))
makeClosureDispatch closureTable es = do
  results <- mapM one (filter inTable (concatMap collectClosureManifolds es))
  return (map fst results, Map.fromList (map snd results))
  where
    inTable (NativeManifold i _ _ _) = Map.member i closureTable
    one (NativeManifold i _ form _) = do
      let ctxTs = [t | Arg _ o <- manifoldContext form, Just t <- [orNativeType o]]
          bndTs = [t | Arg _ t <- manifoldBound form]
          -- inTable guaranteed membership, so this key is present.
          (capScs, bndScs, resSc) = closureTable Map.! i
      argTs <- mapM rustTypeOf (ctxTs <> bndTs)
      argSids <- mapM rustRegisterSchema (capScs <> bndScs)
      resSid <- rustRegisterSchema resSc
      let capSids = take (length capScs) argSids
          copies = map rustIsCopy (ctxTs <> bndTs)
          argExprs =
            [ deser t sid copy j
            | (j, (t, sid, copy)) <- zip [(0 :: Int) ..] (zip3 argTs argSids copies)
            ]
          -- Deserialize each argument packet to the closure body's native param
          -- form: a Copy value by value, a non-Copy value by shared reference.
          deser t sid copy j =
            let g = "rustmorloc::get_value::<" <> t <> ">(a(" <> pretty j <> "), " <> sch sid <> ")"
             in if copy then g else rustRef Owned g
          call = manNamer i <> tupled argExprs
          wrapper =
            vsep
              [ "unsafe fn mlc_closure_dispatch_" <> pretty i <> "(args: *const *const u8, nargs: usize) -> *mut u8 {"
              , indent 4 $
                  vsep
                    [ "let a = |k: usize| -> *const u8 { if k < nargs { unsafe { *args.add(k) } } else { std::ptr::null() } };"
                    , "rustmorloc::put_value(&(" <> call <> "), " <> sch resSid <> ")"
                    ]
              , "}"
              ]
      return (wrapper, (render (manNamer i), (i, capSids)))

-- | Collect every record type used in these manifolds, keyed by its FVar, with
-- one representative field list (from a use site). Unlike the shared recmap
-- (which only collects @= "struct"@ records), this also collects user-mapped
-- records (@record Rust => X = "Name"@) so their marshalling impls are emitted.
collectRustRecords :: [SerialManifold] -> [(FVar, [(Key, TypeF)])]
collectRustRecords =
  -- One entry per record (keyed by general TVar); keep the first field list.
  nubBy ((==) `on` \(FV gv _, _) -> gv)
    . concatMap (runIdentity . foldWithSerialManifoldM fm)
  where
    fm = defaultValue {opFoldWithNativeExprM = ne, opFoldWithSerialExprM = se}
    ne _ (DeserializeN_ t s xs) = return $ xs <> seek t <> seek (serialAstToType s)
    ne efull e = return $ foldlNE (<>) (seek (typeFof efull)) e
    se _ (SerializeS_ s xs) = return $ seek (serialAstToType s) <> xs
    se _ e = return $ foldlSE (<>) [] e

    seek :: TypeF -> [(FVar, [(Key, TypeF)])]
    seek (NamF _ v _ rs) = (v, rs) : concatMap (seek . snd) rs
    seek (AppF t ts) = concatMap seek (t : ts)
    seek (FunF ts t) = concatMap seek (t : ts)
    seek (OptionalF t) = seek t
    seek (EffectF _ t) = seek t
    seek _ = []

-- | Emit a struct definition (only for autogenerated @= "struct"@ records) plus
-- @ToVoidstar/FromVoidstar@ impls for every record used in the pool. User-mapped
-- records provide their own struct (in sourced Rust), so only the impls are
-- emitted for them.
generateRustStructs :: [SerialManifold] -> RustM [MDoc]
generateRustStructs es = concat <$> mapM makeOne (collectRustRecords es)
  where
    -- Each record's closure fields (keyed by the record's general TVar + field
    -- Key), harvested from every (de)serialization site; the 'SerialClosure'
    -- carries the arg/result wire schemas the reify/reflect need. Duplicate keys
    -- (the same record with a closure field at different arities) keep the last
    -- -- a rare corner also collapsed by 'collectRustRecords' (nubBy general TVar).
    harvest :: Map.Map (TVar, Key) SerialAST
    harvest =
      Map.fromList
        [ ((g, k), s)
        | sm <- es
        , (FV g _, flds) <- collectSerialObjects sm
        , (k, s@(SerialClosure _ _)) <- flds
        ]

    -- A bare function field (needs `Rc<dyn MorlocFnN>` boxing + reify/reflect).
    -- Deliberately NARROWER than 'isFunctionTypeF' (which also matches `EffectF`,
    -- for let-binding-annotation elision) -- a deferred-effect field is not a
    -- closure and is not boxed this way.
    isFunF (FunF _ _) = True
    isFunF _ = False

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

    makeOne :: (FVar, [(Key, TypeF)]) -> RustM [MDoc]
    makeOne (v@(FV gv _), rs) = case v of
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
            let hasFun = any (isFunF . snd) rs
                fields4 = [(fld, ty, w, Nothing) | (fld, ty, w) <- fields]
                impls = [RP.printRecordImpls (recName rec) params fields4 | not hasFun]
            return $ RP.printRustStruct (recName rec) params [(fld, ty) | (fld, ty, _) <- fields] : impls
          Nothing -> error $ "Rust: autogenerated record missing from recmap: " <> show v
      -- User-mapped record: the user writes the (monomorphic) struct, so only the
      -- marshalling impls are emitted, with concrete field types. A closure field
      -- reifies/reflects in place (via 'fieldMarshal'); the impl is emitted only
      -- when EVERY function field has a harvested crossing site, else struct-only.
      FV _ (CV s) -> do
        fields <- mapM (oneField gv (pretty s) . fmap Right) rs
        marshals <- mapM (\(k, ty) -> fieldMarshal gv k ty) rs
        let fields4 = zipWith (\(fld, ty, w) m -> (fld, ty, w, m)) fields marshals
            -- Emit the impl only when EVERY function field has a harvested marshal
            -- (a non-function field imposes no requirement); else struct-only.
            emitImpl = and (zipWith (\(_, ty) m -> not (isFunF ty) || maybe False (const True) m) rs marshals)
        return [RP.printRecordImpls (pretty s) [] fields4 | emitImpl]

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
        OptionalF inner | refsRecord selfGv inner -> return $ "Option<Box<" <> selfName <> ">>"
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
    -- A function-typed parameter is rendered `&impl MorlocFnN` (see 'rustArgOf'),
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
              Function _ resultT
                | not sourcedCallee -> rustRef own x
                | isFunParam -> x
                | isVar -> rustRef own x
                | otherwise -> case resultT of
                    Native tf -> byType tf
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
    -- Box a function value into its stored fat-trait-object representation
    -- @Rc::new(v) as Rc<dyn MorlocFnN<..>>@. The explicit @as@ cast is required
    -- for a list/tuple element (a @vec![..]@ literal has no per-element declared
    -- type to drive the @Rc<F> -> Rc<dyn>@ unsizing coercion, and distinct
    -- closures are distinct anonymous types); it is redundant-but-valid for a
    -- record field. Only a genuine @FunF@ boxes -- an @EffectF@ (erased) or any
    -- non-function element passes through.
    , lcStoreField = \ty v -> case ty of
        FunF _ _ -> do
          castT <- rustFieldType ty
          return $ "std::rc::Rc::new(" <> parens v <> ") as " <> castT
        _ -> return v
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
    , lcMakeDoBlock = \_ stmts expr ->
        return
          ( []
          , case stmts of
              [] -> "move || {" <+> expr <+> "}"
              _ -> "move || {" <> nest 4 (line <> vsep (stmts ++ [expr])) <> line <> "}"
          )
    , lcSerialize = defaultSerialize (rustLowerConfig mask)
    , lcDeserialize = \_ -> defaultDeserialize (rustLowerConfig mask)
    -- Reify a crossing closure to its wire tuple via the arity-indexed trait
    -- method `reifyN` (`ClosureOrigin` = `(String, i64, Vec<Vec<u8>>)`). This is
    -- uniform across a top-level thin `FatClosure` (built by 'rustClosureWrapper',
    -- `reifyN` = Some(&origin)) AND a nested boxed `Rc<dyn MorlocFnN>` element
    -- (delegates through the pointer) -- a boxed element has no `.origin` field,
    -- so a plain field access would not compile. `.unwrap()` is safe: every
    -- crossing closure is a registered `FatClosure` (see 'rustClosureWrapper' /
    -- 'rsReifyInfo'). `reifyN` borrows the origin; the reified value is placed into
    -- an owned wire aggregate (a `Vec`/tuple/`Option` element, or a `put_value`
    -- argument), so it is `.clone()`d here to own it.
    , lcReifyClosure = \v s -> case s of
        SerialClosure ins _ ->
          return $ parens v <> ".reify" <> pretty (length ins) <> "().unwrap().clone()"
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
    , lcMakePass = \mname params -> rustClosureWrapper mname [] params
    , lcMakeLambda = \_sig mname contextArgs boundArgs -> rustClosureWrapper mname contextArgs boundArgs
    , lcClosureSig = \_ -> return ""
    , lcRegisterSchema = rustRegisterSchema
    }

-- | Assemble a @let@ binding at the PoolDocs level. A serialize let (mt =
-- Nothing) binds an owned packet pointer; a native let binds its native type.
-- | Does a type render as a closure (@impl Fn@)? Such a type cannot annotate a
-- @let@ binding (@impl Trait@ is only legal in argument/return position), so a
-- function-valued binding must omit its type and let Rust infer the closure.
isFunctionTypeF :: TypeF -> Bool
-- A deferred effect value is a nullary thunk (`impl Fn() -> T`, forced at its
-- use site), so it is function-valued like a closure and its let binding must
-- omit the type annotation.
isFunctionTypeF (EffectF _ _) = True
isFunctionTypeF t = case stripEffectF t of
  FunF _ _ -> True
  _ -> False

-- The borrow-safe flag ('isBorrowableProjection') is unused: Rust binds owned
-- locals and adapts a borrowed/place RHS via 'adaptOwnedElem' upstream.
rustMakeLet :: (Int -> MDoc) -> Int -> Maybe TypeF -> Bool -> PoolDocs -> PoolDocs -> RustM PoolDocs
rustMakeLet namer letIndex mt _ p1 p2 = do
  ann <- case mt of
    Just t | isFunctionTypeF t -> return ""
    Just t -> do ts <- rustTypeOf t; return (":" <+> ts)
    Nothing -> return (":" <+> "*mut u8")
  -- A loop-carried local's entry deserialize let is reassigned each iteration by
  -- the loop's continue, so it must bind mutably.
  carried <- asks oeLoopCarried
  let letKw = if Set.member letIndex carried then "let mut" else "let"
      letLine = letKw <+> namer letIndex <> ann <+> "=" <+> poolExpr p1 <> ";"
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
  typeStr <- rustTypeOf (typeFof origExpr)
  let ifStmt =
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
      return . Just $
        vsep [decl <+> "{", indent 4 (vsep (frameStmt : priorLines ++ [body])), "}"]
