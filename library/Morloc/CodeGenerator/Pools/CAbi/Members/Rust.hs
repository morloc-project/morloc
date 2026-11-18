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
  , RustProfile (..)
  , resolveRustProfile
  , rustStandaloneLib
  ) where

import Morloc.Data.Zip (indexed, indexedFrom, mapIndexed, mapIndexedM, zip3Same, zip4Same, zipExact, zipPrefix, zipSame, zipWithSame)
import Data.Foldable (toList)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import qualified Control.Monad.State.Strict as CMS
import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified System.Info as SI
import Data.Char (isAlphaNum, isDigit)
import Morloc.CodeGenerator.Grammars.Common hiding (manNamer, manifoldIdOf)
import Morloc.CodeGenerator.LogTemplate (RenderedTemplate (..), collectRenderedTemplates)
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import Morloc.CodeGenerator.Grammars.Translator.Imperative
  (
  Low (..)
  , nativeArgTypeF
  , LowArg (..)
  , LowN
  , LowS
  , loopBodyDocs
  , LoopResult (..)
  , RNode (RSM)
  , lastBuilds
  , nativeManifoldsUnder
  , infixOperator
  , ArgSite (..)
  , IOwnership (..)
  , LowerConfig (..)
  , buildProgramM
  , markLastReads
  , BorrowPart (..)
  , BorrowTest (..)
  , FieldRead (..)
  , borrowableRoot
  , isLastRead
  , borrowPartOf
  , varIndex
  , IExpr (IStrLit)
  , papplySteps
  , defaultDeserialize
  , defaultFoldRules
  , defaultSerialize
  , toIType
  , nativeToWire
  , wireToNative
  )
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Infer (declaredDataNames)
import Morloc.CodeGenerator.Ownership
import Morloc.Build.CargoLock (mergeCargoLocks)
import qualified Morloc.Build.Params as BP
import qualified Morloc.Data.PoolHash as PH
import qualified Morloc.CodeGenerator.Pools.CAbi.Members.RustPrinter as RP
import Morloc.CodeGenerator.Serial (containsFunF, generatedObject, isSerializable, serialAstToMsgpackSchema, serialAstToNativeType, serialAstToType, serialSubtrees, shallowType, unpackedAsTypeF, wireSerialAstToType)
import qualified Morloc.Frontend.Instances as FI
import Morloc.Typecheck.Internal (unqualify)
import Morloc.CodeGenerator.EffectBoundary (declaredFun, expandT)
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS
import qualified Morloc.Version as MV
import Morloc.Quasi
import System.Directory (findExecutable)
import System.FilePath (takeDirectory)
import Morloc.CodeGenerator.GenDefs (DataDef (..), Disagreement (..), GenDef (..), carriedTemplateArgs, armDataParams, dataDefs, dataNamesIn, nativeDataParams, RecordUse, disagreements, inPoolSerialASTs, placeTypes, genDefs, nativeParam, nativeParams, paramOf, placeholders, recordUses, depNamespaces, nameRoot)

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
  , rsDebugInfo :: Int -> Maybe (Text, Text)
  -- ^ A manifold's frame label @(name, srcloc)@, from which the trace
  -- line it pushes for error tracebacks is built: every manifold under @--debug@
  -- ('makeManifoldDebugInfoLookup'), otherwise only a call of a named
  -- function, labeled with its definition ('makeTraceLabelLookup', DIAG-5).
  , rsLogTemplates :: Map.Map Int RenderedTemplate
  -- ^ Rendered @log:@ / @benchmark:@ templates per labeled manifold midx.
  -- Drives the '::rustmorloc::LogGuard' wrap in 'rustMakeFunction'.
  , rsCScope :: Scope
  -- ^ The merged Rust concrete typedef scope; resolves a recursive back-ref
  -- (@RecF@) to its concrete struct name (as C++'s translatorCScope does).
  , rsThinClosures :: Set.Set Text
  -- ^ Closure manifolds emitted as a plain closure: see 'thinClosures'.
  , rsSrcMakers :: Set.Set SrcName
  -- ^ The sourced functions whose declared result, after its effect, is a
  -- function rather than a type variable: the host makes it ('impl Fn').
  , rsSrcTypeVarMask :: Map.Map SrcName [(Bool, Bool)]
  -- ^ Per sourced function, per parameter position: @(isBareTypeVar,
  -- isFunctionParam)@ from the declared morloc signature. @isBareTypeVar@: the
  -- parameter is a BARE type variable, so it is passed by reference (@&A@) even
  -- at a Copy instantiation (the sourced Rust fn is generic over @&A@).
  -- @isFunctionParam@: the parameter is itself function-typed (@F: Fn@), so a
  -- function-valued argument is handed over through @IntoCallbackN@ rather
  -- than borrowed.
  , rsReifyInfo :: Map.Map Text (Int, [(Int, SerialAST)], [(Int, SerialAST)])
  -- ^ Per crossing-closure body-manifold name: @(mid, capturedSchemaIds)@.
  -- Only closures that reach a serialize boundary appear here (the rest are
  -- built unregistered, with no reify cost). Drives 'rustClosureWrapper' to
  -- build the closure's @(home pool, mid, captured)@ origin lazily. The second
  -- list holds the bound arguments' schema ids, which a partial application
  -- of the closure appends to its origin.
  , rsStageTable :: Map.Map Int StageEntry
  , rsPapplyHeads :: Set.Set Text
  -- ^ The stored types of the function values this pool partially applies.
  , rsPool :: PoolPlan
  , rsBuilds :: Set.Set (Int, Int)
  -- ^ The (manifold, local) pairs whose building is the local's last read
  -- (OWN-3).
  , rsCurrentRoot :: Int
  -- ^ The top-level manifold being lowered (the takes in 'rsPool' are keyed
  -- by it).
  , rsErrors :: [Text]
  , rsTypeHomes :: Map.Map TVar TypeHome
  -- ^ The declaring module and name of every declared type (GEN-4).
  , rsPublic :: Map.Map TVar Text
  -- ^ The generated types this pool declares in their module's file, each
  -- with its path from the crate root (GEN-6).
  , rsPublicDecls :: [(TVar, MDoc, Maybe [Text])]
  -- ^ The declarations of those types, to be written to their files, each
  -- with whether the crate root still offers its module's names.
  , rsHandles :: Set.Set TVar
  -- ^ The generated records that refer to themselves, each a handle to its
  -- shared fields (GEN-1).
  , rsNeedsNode :: Bool
  -- ^ Whether a generated `data` type of the pool has an arm, so the crate
  -- declares its node type (GEN-1).
  , rsUserArms :: Map.Map Text [(Text, [RP.UserField])]
  -- ^ The arms of each user-written `data` type at each occurrence, by its
  -- rendered type: each constructor with morloc's types of its fields.
  , rsDataFailures :: Map.Map TVar Text
  -- ^ Why a generated parameterized `data` type has no definition.
  , rsDataDefs :: Map.Map TVar DataDef
  -- ^ Each generated parameterized `data` type's definition (GEN-9).
  , rsDataNames :: Set.Set TVar
  -- ^ The `data` types the program declares.
  , rsGenDefs :: Map.Map TVar GenDef
  -- ^ Each generated record's definition from its declaration (GEN-9).
  , rsRecords :: Map.Map TVar GenDef
  -- ^ The generated records of the pool, each defined from its declaration
  -- (GEN-9).
  , rsRecordNames :: Map.Map TVar Text
  -- ^ The path of each from the crate root: in its module's file, or in
  -- the pool's private module (GEN-10).
  , rsGenFailures :: Map.Map TVar Text
  -- ^ Why a used record has no definition, for each that has none.
  , rsRecordUses :: [RecordUse]
  -- ^ Every occurrence of a generated record in the pool.
  , rsRecordObjects :: Map.Map TVar [[(Key, SerialAST)]]
  -- ^ The fields of each generated record at each instance the pool
  -- marshals.
  , rsPoolAsts :: [SerialAST]
  -- ^ every serial form the pool writes or reads, its intrinsics' included
  , rsDepNamespaces :: Map.Map Text (Text, [Text])
  -- ^ Dependency namespace roots, each with its crate (GEN-7).
  , rsConvHelpers :: [MDoc]
  -- ^ The functions converting packed fields of generated records to the
  -- form they are written in and back (GEN-14).
  , rsPrivateDecls :: [(Text, MDoc)]
  -- ^ The generated records the pool keeps to itself, each with its module
  -- path from the crate root (GEN-10).
  }

instance Defaultable RustState where
  defaultValue =
    RustState
      { rsCounter = 0
      , rsSchemas = Map.empty
      , rsLocalSet = Set.empty
      , rsRemoteSet = Set.empty
      , rsDebugInfo = (\_ -> Nothing)
      , rsLogTemplates = Map.empty
      , rsCScope = Map.empty
      , rsThinClosures = Set.empty
      , rsSrcMakers = Set.empty
      , rsSrcTypeVarMask = Map.empty
      , rsReifyInfo = Map.empty
      , rsStageTable = Map.empty
      , rsPapplyHeads = Set.empty
      , rsPool = emptyPoolPlan
      , rsBuilds = Set.empty
      , rsCurrentRoot = 0
      , rsErrors = []
      , rsTypeHomes = Map.empty
      , rsPublic = Map.empty
      , rsPublicDecls = []
      , rsHandles = Set.empty
      , rsNeedsNode = False
      , rsUserArms = Map.empty
      , rsGenDefs = Map.empty
      , rsDataDefs = Map.empty
      , rsDataNames = Set.empty
      , rsDataFailures = Map.empty
      , rsRecords = Map.empty
      , rsRecordNames = Map.empty
      , rsGenFailures = Map.empty
      , rsRecordUses = []
      , rsRecordObjects = Map.empty
      , rsPoolAsts = []
      , rsDepNamespaces = Map.empty
      , rsConvHelpers = []
      , rsPrivateDecls = []
      }

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
  -- ^ Indices used at more than one point in the current manifold
  -- ('mpShared', OWN-2). Kept separate from 'oeCurrent': a shared owned local
  -- is a value, not a @&T@ reference, so tagging it 'BorrowedRef' would
  -- miscompile reference sinks.
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
          let typeTs = tyArgs ps
          ts' <- mapM rustFieldType typeTs
          return . pretty $ expandMacro x (map render ts')
      | otherwise = return (pretty (nativeTypeName x))

    -- A generated type declared in its module's file goes by its path from
    -- the crate root (GEN-4); any other by the name given.
    publicOr :: TVar -> MDoc -> RustM MDoc
    publicOr gv d = maybe d pretty . Map.lookup gv <$> CMS.gets rsPublic

    -- A `data` type's name: a generated template in its module's file
    -- goes by its path, spelled at the given arguments; a generated
    -- template's instance named by its arguments is its own type.
    dataName gv x ps
      | T.any (== '$') x = do
          public <- CMS.gets rsPublic
          ddefs <- CMS.gets rsDataDefs
          let rest = snd (T.breakOn "<" x)
              x' = case Map.lookup gv public of
                Just q -> q <> rest
                Nothing | Map.member gv ddefs -> privateDataName gv <> rest
                Nothing -> x
          nominalTypeName x' ps
      | otherwise = do
          ddefs <- CMS.gets rsDataDefs
          if Map.member gv ddefs then nominalTypeName x ps else nominalTypeName x ps >>= publicOr gv
    f :: TypeF -> RustM MDoc
    f (UnkF (FV _ x)) = return (pretty (nativeTypeName (unCVar x)))
    f (VarF (FV gv x)) = publicOr gv (pretty (nativeTypeName (unCVar x)))
    -- An enum lowers to its concrete name; the `#[repr(u8)] enum` that
    -- name refers to is either generated for this pool or supplied by the
    -- user through a `data Rust => X = "..."` mapping.
    f (EnumF (FV gv (CV x)) ps _) = nominalTypeName x ps >>= publicOr gv
    -- A variant lowers to its concrete name; the `enum` carrying the arms
    -- is generated for this pool or supplied by a `data Rust => X = "..."`.
    f (VariantF (FV gv (CV x)) ps _) = dataName gv x ps
    f (AppF t ts) = do
      t' <- f t
      let typeTs = tyArgs ts
      -- Type arguments are STORED positions (a Vec element, a tuple slot): a
      -- function leaf among them must box to `Arc<dyn Fn>` (a bare
      -- `impl Fn` is illegal in a container), which is exactly 'rustFieldType'.
      ts' <- mapM rustFieldType typeTs
      return . pretty $ expandMacro (render t') (map render ts')
    -- A recursive optional (?T where T points back to a containing record)
    -- must break the cycle with Box (an Option<T> of an infinite-size T won't
    -- compile). None == absent matches the voidstar single-relptr Optional.
    f (OptionalF t@(RecF (FV gv _) _)) = do
      t' <- f t
      handles <- CMS.gets rsHandles
      dataNames <- CMS.gets rsDataNames
      -- A handle has a size of its own, so its optional needs no box; so
      -- does a `data` type, which is boxed at its arms.
      return $ if Set.member gv handles || Set.member gv dataNames
        then "::std::option::Option<" <> t' <> ">"
        else "::std::option::Option<::std::boxed::Box<" <> t' <> ">>"
    f (OptionalF t) = do
      -- The payload is a stored position; a function payload boxes ('rustFieldType').
      t' <- rustFieldType t
      return $ "::std::option::Option<" <> t' <> ">"
    -- A function value inside a type (a field, an element, a payload, the
    -- result of a function) is a trait object; only a manifold's own
    -- parameters and return are `impl` ('rustArgType', 'rustReturnType').
    f (FunF ts t) = rustFieldType (FunF ts t)
    -- Autogenerated record: resolve the concrete struct name from the recmap
    -- (keyed by the record FVar + field keys).
    -- A generated record is its definition at the arguments it carries,
    -- which are native at both levels: the unpacked view keeps a
    -- generated record native (GEN-14).
    f (NamF _ (FV gc (CV "struct")) ps _) = do
      records <- CMS.gets rsRecords
      names <- CMS.gets rsRecordNames
      defs <- CMS.gets rsGenDefs
      let q = pretty (Map.findWithDefault (unTVar gc) gc names)
          complain :: Text -> RustM ()
          complain msg = CMS.modify $ \st -> st {rsErrors = msg : rsErrors st}
      case Map.lookup gc records of
        Just gd
          | null (nativeParams gd) -> return q
          | otherwise -> case carriedTemplateArgs gd ps of
              Just args -> do
                as <- mapM rustFieldType args
                return (q <> "<" <> hcat (punctuate ", " as) <> ">")
              Nothing -> do
                complain ("a use of the record " <> unTVar gc <> " carries no arguments to spell it at")
                return q
        Nothing -> do
          -- A record refused in planning has said why.
          fails <- CMS.gets rsGenFailures
          unless (Map.member gc defs) $
            complain ("the record " <> unTVar gc <> " has no Rust definition: "
              <> Map.findWithDefault "its declaration could not be resolved in Rust" gc fails)
          return q
    -- User-mapped record: the concrete struct name is the CVar text, or the
    -- template it holds instantiated with this occurrence's arguments.
    f (NamF _ (FV _ (CV s)) ps _) = nominalTypeName s ps
    -- A reference to a record: a generated one names its definition at
    -- its arguments.
    f (RecF (FV gv (CV "struct")) ps) = do
      names <- CMS.gets rsRecordNames
      records <- CMS.gets rsRecords
      case Map.lookup gv names of
        Just name -> case Map.lookup gv records of
          Just gd | not (null (nativeParams gd)) -> case carriedTemplateArgs gd ps of
            Just args -> (pretty name <>) . RP.paramList <$> mapM rustFieldType args
            Nothing -> do
              CMS.modify $ \s -> s {rsErrors = ("a reference to the record " <> unTVar gv <> " carries no arguments to spell it at") : rsErrors s}
              return (pretty name)
          _ -> return (pretty name)
        Nothing -> do
          fails <- CMS.gets rsGenFailures
          CMS.modify $ \s -> s
            { rsErrors = ("the record " <> unTVar gv <> " has no Rust definition: "
                <> Map.findWithDefault "its declaration could not be resolved in Rust" gv fails) : rsErrors s }
          return (pretty (tvarIdentifier gv))
    -- A user-mapped record is spelled from its form at the reference's
    -- arguments, as an occurrence of it is.
    f (RecF (FV gv@(TV gvText) (CV cv)) ps) = do
      public <- CMS.gets rsPublic
      dataNames <- CMS.gets rsDataNames
      if Set.member gv dataNames then dataName gv cv ps else case Map.lookup gv public of
        Just name -> return (pretty name)
        Nothing
          | cv /= gvText, not (null ps) -> nominalTypeName cv ps
          | cv /= gvText -> return (pretty (nativeTypeName cv))
          | otherwise -> do
              cscope <- CMS.gets rsCScope
              case Map.lookup gv cscope of
                Just ((_, body, _, _, _) : _) | Just name <- bodyName body, name /= "struct" -> return (pretty name)
                _ -> do
                  CMS.modify $ \s -> s
                    { rsErrors = ("the recursive type " <> gvText <> " has no Rust form; give it one, `record Rust => "
                        <> gvText <> " = \"<struct-name>\"`") : rsErrors s }
                  return (pretty (tvarIdentifier gv))


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
  | isTupleName gv = all rustIsCopy (tyArgs ts)
-- An applied type whose Rust spelling is a Copy scalar: its arguments are
-- phantom (no `$n` in the mapping), so it is that scalar. The stream and
-- cell handles (`IFile a`, `IStream a`, `OStream a`, `Cell a`) are all `u64`
-- this way, and every handle intrinsic takes the handle by value.
rustIsCopy (AppF (VarF (FV _ (CV cv))) _) = cv `elem` copyScalars
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
  return $ rustFnValueType argTs retT
rustStoredType (Native t) = rustTypeOf (typeFof t)
rustStoredType (Serial _) = return "*const u8"
rustStoredType Passthrough = return "*const u8"

-- | The one spelling of a function value (HOST-8):
-- @std::sync::Arc<dyn Fn(&A1, .., &An) -> R + Send + Sync>@.
rustFnValueType :: [MDoc] -> MDoc -> MDoc
rustFnValueType argTs retT =
  "::std::sync::Arc<dyn Fn(" <> hcat (punctuate ", " ["&" <> a | a <- argTs]) <> ") -> " <> retT <> " + Send + Sync>"

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

-- | The Rust type of a record field. A function-typed field is stored as
-- @Arc<dyn Fn(&A1,..,&An) -> R + Send + Sync>@ (a bare @impl Fn@ is illegal in
-- a field). Everything else renders as its ordinary 'rustTypeOf'.
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
rustClosureWireLeaf = VarF (FV (TV "Closure") (CV "::rustmorloc::ClosureOrigin"))

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
  -- A function value is taken by reference too (`&Arc<dyn Fn(..)>`), so
  -- the manifold stays re-callable across a HOF's per-element calls; a sink
  -- that wants it by value takes an `Arc` clone.
  let ts' = if rustArgIsRef t then "&" <> ts else ts
  return $ (argNamer rustLocals) a <> ":" <+> ts'

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
-- fragments and insertions.
rustEvalPattern :: TypeF -> PatApp MDoc -> RustM MDoc
-- String interpolation: interweave compile-time fragments with the rendered
-- insertion expressions (each forced to an owned String for a uniform slice).
rustEvalPattern _ (PatText s xs) =
  return $ "::rustmorloc::interweave_strings(&["
    <> hcat (punctuate ", " [dquotes (pretty (RP.rustEscape frag)) | frag <- s : map snd xs])
    <> "], &["
    <> hcat (punctuate ", " [parens x <> ".as_str()" | (x, _) <- xs])
    <> "])"
-- Field/index getter with NO bracket steps: plain `.field` / `.i` accessor(s).
rustEvalPattern _ (PatGet sel [] m) =
  return $ case ungroup sel of
    [ss] -> writeSelectorRust m ss
    sss -> tupled (map (writeSelectorRust m) sss)
-- Getter chain containing bracket steps (`.f.[i]`, `.[i:j].0`, ...). Args are
-- [bracket_bounds..., receiver]; walk the selector, threading the bracket args.
rustEvalPattern _ (PatGet s bracketArgs receiver) =
  return $ fst (walkRustSelectorBrackets receiver bracketArgs s)
rustEvalPattern _ (PatIndex i m) =
  return $ "morloc_at" <> tupled [i, "&(" <> m <> ")"]
rustEvalPattern _ (PatSlice start stop step m) =
  return $ "morloc_slice" <> tupled [start, stop, step, "&(" <> m <> ")"]
-- Record/tuple SETTER: rebuild the aggregate with one leaf replaced. The shared
-- 'patternSetter' re-accesses every UNCHANGED field on the receiver; in Rust the
-- receiver must be bound once (`&__r`) and each accessed field cloned -- an owned
-- rebuild cannot move a field out of a borrowed/shared receiver (the setter
-- receiver is usually a `&T` param), and a `Copy` field's `.clone()` is a free
-- copy. This is the same per-field copy C++'s aggregate-init makes. The struct
-- name for a record literal is resolved by running 'rustTypeOf' with the
-- captured state (mirrors the C++ `evalState` name capture).
rustEvalPattern t0 (PatSet s0 m0 (toList -> xs0)) = do
      st <- CMS.get
      env <- asks id
      let nameOf recType = CMS.evalState (runReaderT (rustStructCtor recType) env) st
          makeTuple _ xs = tupled xs
          makeRecord recType fields =
            RP.rustRecordValue (isHandleType (rsHandles st) recType) (nameOf recType) [(k, x) | ((k, _), x) <- fields]
          accessTuple _ d i = d <> "." <> pretty i
          accessRecord _ d k = d <> "." <> RP.rustFieldIdent (Key k)
          -- An unchanged field is cloned into the owned rebuild; a changed
          -- field's accessor is reused bare as a receiver (no clone), so a
          -- nested set does not clone the whole enclosing field.
          finalizeSet _ v = v <> ".clone()"
          spine = patternSetter makeTuple makeRecord accessTuple accessRecord finalizeSet "__morloc_rec" t0 s0 xs0
      return $ "{ let __morloc_rec = &(" <> m0 <> "); " <> spine <> " }"

-- | A setter on an owned local at its last read (OWN-2) updates it in place,
-- so its unchanged fields are neither copied nor rebuilt. It applies only
-- when the result has the receiver's type and no path repeats or extends
-- another, where the rebuild keeps one write per node.
rustTakeSetter :: NativeExpr -> TypeF -> Selector -> [MDoc] -> RustM (Maybe MDoc)
rustTakeSetter recv t sel (m : xs)
  | isLastRead recv
  , typeFof recv == t
  , not (null xs)
  , Just pxs <- zipExact paths xs
  , not (any overlaps (indexed paths)) = do
      own <- rustOwnership recv
      handles <- CMS.gets rsHandles
      -- GEN-1: a handle is set through `Arc::make_mut`; a path through
      -- another handle is rebuilt instead.
      let root = if isHandleType handles t then "::std::sync::Arc::make_mut(&mut *__morloc_rec.0)" else "__morloc_rec"
          inPlace = not (any (throughHandle handles t) paths)
      return $
        if own == Owned && inPlace
          then Just ("{ let mut __morloc_rec = " <> m <> "; " <> hsep [writeSelectorRust root p <+> "=" <+> x <> ";" | (p, x) <- pxs] <+> "__morloc_rec }")
          else Nothing
  where
    paths = ungroup sel
    overlaps (i, p) = or [q `isPrefixOf` p | (j, q) <- indexed paths, j /= i]
rustTakeSetter _ _ _ _ = return Nothing

-- | Whether a getter on a value of this type takes its fields from the
-- handle's node: each path is one field of the handle.
takesFromHandle :: Set.Set TVar -> TypeF -> Selector -> Bool
takesFromHandle handles t sel = isHandleType handles t && all oneField (ungroup sel)
  where
    oneField [Right _] = True
    oneField _ = False

-- | Whether a type is a record that is a handle to its fields.
isHandleType :: Set.Set TVar -> TypeF -> Bool
isHandleType handles t = maybe False (`Set.member` handles) (recordNameOf t)

-- | Whether a path, past its first step, reaches into a handle.
throughHandle :: Set.Set TVar -> TypeF -> [Either Int Text] -> Bool
throughHandle handles = go
  where
    go _ [] = False
    go _ [_] = False
    go t (step : rest) = case next t step of
      Just t' -> isHandleType handles t' || go t' rest
      Nothing -> False
    next (NamF _ _ _ fs) (Right k) = lookup (Key k) fs
    next (AppF _ (tyArgs -> ts)) (Left i) | i >= 0 && i < length ts = Just (ts !! i)
    next _ _ = Nothing

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

-- | A getter on an owned receiver (OWN-5): the leaf it names, or a tuple of
-- its leaves in order. A record is opened through a pattern naming every
-- field of the record and no others. A receiver that is not a record or a
-- tuple renders as 'lcEvalPattern' would.
rustTakeGetter :: TypeF -> Selector -> MDoc -> RustM (Maybe MDoc)
rustTakeGetter t0 sel0 subj0 = do
  handles <- CMS.gets rsHandles
  rustTakeGetterWith handles t0 sel0 subj0

-- | 'rustTakeGetter', given the records that are handles, which it never
-- opens (GEN-1).
rustTakeGetterWith :: Set.Set TVar -> TypeF -> Selector -> MDoc -> RustM (Maybe MDoc)
rustTakeGetterWith handles t0 sel0 subj0
  -- OWN-5: fields read at the last use of a handle are taken from its node
  -- when no other handle shares it, and copied otherwise.
  | takesFromHandle handles t0 sel0, Just ks <- mapM oneKey (ungroup sel0) =
      let field v k = v <> "." <> RP.rustFieldIdent (Key k)
          leaves v f = case ks of
            [k] -> f (field v k)
            _ -> tupled (map (f . field v) ks)
       in return . Just $
            "{ match (" <> subj0 <> ").into_parts() { Ok(__morloc_f) =>" <+> leaves "__morloc_f" id
              <> ", Err(__morloc_a) =>" <+> leaves "__morloc_a" (<> ".clone()") <+> "} }"
  | opens t0, not (any (readsHandle handles t0) (ungroup sel0)) = do
      (stmts, leaves) <- node t0 subj0 (indexed (ungroup sel0))
      let result = case map snd (sortOn fst leaves) of
            [x] -> x
            xs -> tupled xs
      return (Just ("{" <+> hsep stmts <+> result <+> "}"))
  | otherwise = return Nothing
  where
    opens (NamF NamRecord (FV g _) _ _) = not (Set.member g handles)
    opens t = isJust (tupleArgs t)
    oneKey [Right k] = Just k
    oneKey _ = Nothing

    tupleArgs (AppF (VarF (FV (TV gv) _)) ts)
      | isTupleName gv = Just (tyArgs ts)
    tupleArgs _ = Nothing

    -- The value @v@ is owned; each path is a result index and the steps
    -- left to it.
    node _ v [(k, [])] = return ([], [(k, v)])
    node t v paths
      | any (null . snd) paths = do
          n <- getCounter
          let whole = "__morloc_lv" <> pretty n
              (lastWhole, others) = case reverse [k | (k, []) <- paths] of
                (k : ks) -> (k, ks)
                [] -> error "rustTakeGetter: no whole leaf"
              copies =
                [ (k, "__morloc_lc" <> pretty n <> "_" <> pretty k, "(&" <> whole <> ")" <> writeSelectorRust "" ss <> ".clone()")
                | (k, ss) <- paths
                , not (null ss) || k `elem` others
                ]
          tys <- rustTypeOf t
          return
            ( ("let" <+> whole <> ":" <+> tys <+> "=" <+> v <> ";") : ["let" <+> c <+> "=" <+> e <> ";" | (_, c, e) <- copies]
            , (lastWhole, whole) : [(k, c) | (k, c, _) <- copies]
            )
    node t@(NamF NamRecord (FV g _) _ rs) v paths
      | not (Set.member g handles)
      , all (isKey . snd) paths
      , Just _ <- mapM (\h -> lookup h rs') (heads paths) = do
          n <- getCounter
          tys <- rustTypeOf t
          ctor <- rustStructCtor t
          let shell = "__morloc_ls" <> pretty n
              fieldLocal j = "mlc_" <> pretty n <> "_" <> pretty j
              fieldLocals = [(key, fieldLocal j) | (j, (key, _)) <- indexed rs]
              locals = map snd fieldLocals
              open =
                [ "let" <+> shell <+> "= ::std::mem::ManuallyDrop::<" <> tys <> ">::new(" <> v <> ");"
                , "let" <+> ctor <+> "{" <+> RP.rustRecordFields fieldLocals <+> "} = &*" <> shell <> ";"
                , "let" <+> tupleOf locals <+> "= unsafe {" <+> tupleOf ["::std::ptr::read(" <> l <> ")" | l <- locals] <+> "};"
                ]
          subs <- mapM
            (\(j, (key, ft)) -> node ft (fieldLocal j) [(k, ss) | (k, Right h : ss) <- paths, h == unKey key])
            [x | x@(_, (key, _)) <- indexed rs, unKey key `elem` heads paths]
          return (open <> concatMap fst subs, concatMap snd subs)
      where
        rs' = [(unKey key, ft) | (key, ft) <- rs]
    node t v paths
      | Just ts <- tupleArgs t
      , all (isIdx . snd) paths
      , all (\j -> j >= 0 && j < length ts) [j | (_, Left j : _) <- paths] = do
          n <- getCounter
          let tup = "__morloc_lt" <> pretty n
          subs <- mapM
            (\j -> node (ts !! j) (tup <> "." <> pretty j) [(k, ss) | (k, Left i : ss) <- paths, i == j])
            (Set.toList (Set.fromList [j | (_, Left j : _) <- paths]))
          return (("let" <+> tup <+> "=" <+> v <> ";") : concatMap fst subs, concatMap snd subs)
    node _ v paths = return ([], [(k, writeSelectorRust v ss) | (k, ss) <- paths])

    heads paths = [h | (_, Right h : _) <- paths]
    isKey (Right _ : _) = True
    isKey _ = False
    isIdx (Left _ : _) = True
    isIdx _ = False
    tupleOf [x] = "(" <> x <> ",)"
    tupleOf xs = tupled xs

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
      keyVar = "__morloc_ck_" <> suffix
      cachedVar = "__morloc_cd_" <> suffix
      resultVar = "__morloc_cr_" <> suffix
      bodyVar = "__morloc_cb_" <> suffix
      labelLit = dquotes (pretty lbl)
      resSchema = render (serialAstToMsgpackSchema resSa)
  preparedArgs <- mapM (prepareRustCacheArg wrapIdx) (indexed args)
  let argRefs = [r | (r, _, _) <- preparedArgs]
      argSchemas = [s | (_, s, _) <- preparedArgs]
      argSetupLines = concatMap (\(_, _, ss) -> ss) preparedArgs
      packetsArr = "&[" <> hcat (punctuate ", " argRefs) <> "]"
      schemasArr = "&[" <> hcat (punctuate ", " [dquotes (pretty (RP.rustEscape s)) | s <- argSchemas]) <> "]"
      keyStmt = "let" <+> keyVar <> ": u64 = ::rustmorloc::cache_key("
                  <> pretty midx <> ", " <> packetsArr <> ", " <> schemasArr <> ");"
      lookupStmt = "let" <+> cachedVar <> ": *mut u8 = ::rustmorloc::cache_lookup("
                  <> keyVar <> ", " <> labelLit <> ");"
      missLines = poolPriorLines bodyPool
        ++ [ "let" <+> bodyVar <> ": *mut u8 =" <+> poolExpr bodyPool <> ";"
           , "::rustmorloc::cache_store(" <> keyVar <> ", " <> labelLit <> ", "
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
    , poolBorrowedLocal = Nothing
    , poolChain = Nothing
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
      -- The cache-key value is only read, through a 'ToVoidstar' (&T)
      -- 'put_value' sink, so a borrowed arg is reborrowed ('rustReadPlace').
      own <- rustOwnership (BndVarN tf i KeptRead)
      -- The packet exists only to key and store the cache entry, so its
      -- owner releases it when the manifold's scope ends.
      let argVar = "__morloc_ca_" <> pretty wrapIdx <> "_" <> pretty j
          decl = "let" <+> argVar <> "_own = ::rustmorloc::Packet::new(" <> RP.putValueFn <> "(&("
                   <> rustReadPlace own ((argNamer rustLocals) a) <> "), " <> RP.schemaRef sid <> "));"
                   <+> "let" <+> argVar <> ": *const u8 =" <+> argVar <> "_own.as_ptr();"
      return (argVar, schemaStr, [decl])
    _ -> return ((argNamer rustLocals) a, schemaStr, [])

-- | Whether a manifold body places a function value in a container (a list,
-- tuple, record or optional whose type holds a function anywhere). A stored
-- closure is a trait object behind `Arc`, which must outlive every frame, so a
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
        [ (render (RP.rustManifoldName i), 1)
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

    -- Only the first call group is passed to the sourced function itself;
    -- a later group is applied to the function value it returned.
    atSite (AppExeN _ (SrcCallP _ src) args) =
      let firstGroup = case provideClosure src (indexed args) of
            (g : _) -> g
            [] -> []
       in Set.fromList
            [ render (RP.rustManifoldName i)
            | (k, NativeArgManifold (NativeManifold i _ form _)) <- firstGroup
            , isClosureForm form
            , funParamAt src k
            ]
    atSite _ = Set.empty

    funParamAt src k = case Map.lookup (srcName src) mask of
      Just bs | k < length bs -> snd (bs !! k)
      _ -> False

-- | The closure manifolds emitted as a plain closure ('thinDoc', 'sinkDoc'):
-- those in @sinks@ ('thinSinkNames') that have no stage entry and are not
-- the function a partial application applies (both of those take the staged
-- form). One predicate decides the form and keeps the
-- closure out of the crossing table, so the two cannot disagree.
thinClosures ::
  Set.Set Text ->
  Map.Map Int StageEntry ->
  Set.Set Text ->
  [SerialManifold] ->
  RustM (Set.Set Text)
thinClosures sinks stageTable heads es =
  Set.fromList . catMaybes <$> mapM one (concatMap collectClosureManifolds es)
  where
    one nm@(NativeManifold i _ form _)
      | Set.member name sinks
      , not (Map.member i stageTable) = do
          let bnds = [t | Arg _ t <- manifoldBound form]
              out = case typeFof nm of
                FunF _ o -> o
                o -> o
          sig <- render <$> rustStoredType (typeMof (FunF bnds out))
          return (if Set.member sig heads then Nothing else Just name)
      | otherwise = return Nothing
      where
        name = render (RP.rustManifoldName i)

-- | The spec's @owner(e, Gamma)@ as a PURE function: the ownership of a native
-- expression given @Gamma@ (the borrowed @&T@ parameter indices) and @Shared@
-- (indices used at more than one point). Because both are explicit arguments
-- (not fold state), the result is correct regardless of the fold's evaluation
-- order. A bound variable is a @&T@ reference only when it is a borrowed
-- parameter; a shared value is a place (borrowed at reference sinks, cloned at
-- owned sinks -- never moved); a getter of a borrowed value is a place; a
-- constructor field that is not taken, and a list element of a variable or
-- of such a field, is a reference (OWN-6); everything else is an owned value.
ownerPure :: Set.Set TVar -> Set.Set Int -> Set.Set Int -> NativeExpr -> IOwnership
-- A bound variable and a let variable both render as `n<i>` and share the index
-- namespace, so both classify the same way ('classifyIndex').
-- A last read of a value the pool owns takes it (OWN-2).
ownerPure _ g s (BndVarN _ i r) = classifyRead g s i r
ownerPure _ g s (LetVarN _ i r) = classifyRead g s i r
ownerPure h g s (ReturnN e) = ownerPure h g s e
ownerPure h g s (NativeLetN _ _ e) = ownerPure h g s e
ownerPure h g s (SerialLetN _ _ e) = ownerPure h g s e
-- GEN-1: a getter that reads into a handle is a place, unless it takes
-- fields at the handle's last use ('rustTakeGetterWith').
ownerPure h g s (PatN _ (PatGet sel [] recv))
  | any (readsHandle h (nativeArgTypeF recv)) (ungroup sel) =
      if takesFromHandle h (nativeArgTypeF recv) sel && argOwnerPure h g s recv == Owned
        then Owned
        else BorrowedPlace
ownerPure h g s (PatN _ (PatGet _ [] recv)) =
  case argOwnerPure h g s recv of
    Owned -> Owned
    _ -> BorrowedPlace
ownerPure h g s (IntrinsicN t IntrCtorField _ (subj : _))
  | not (rustIsCopy t)
  , borrowableRoot subj
  , not (isLastRead subj && ownerPure h g s subj == Owned) =
      BorrowedRef
ownerPure h g s (IntrinsicN t IntrListElem _ (subj : _))
  | not (rustIsCopy t)
  , borrowableRoot subj
  , isJust (varIndex subj) || ownerPure h g s subj == BorrowedRef =
      BorrowedRef
ownerPure _ _ _ _ = Owned

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
argOwnerPure :: Set.Set TVar -> Set.Set Int -> Set.Set Int -> NativeArg -> IOwnership
argOwnerPure h g s (NativeArgExpr e) = ownerPure h g s e
argOwnerPure _ _ _ (NativeArgManifold _) = Owned

classifyRead :: Set.Set Int -> Set.Set Int -> Int -> VarRead -> IOwnership
classifyRead g s i r = case classifyIndex g s i of
  BorrowedPlace | r == LastRead -> Owned
  o -> o

rustOwnership :: NativeExpr -> RustM IOwnership
rustOwnership e = do
  g <- asks oeCurrent
  s <- asks oeShared
  h <- CMS.gets rsHandles
  cscope <- CMS.gets rsCScope
  -- A part with nothing of its own type in memory to lend is read as a
  -- fresh copy (RUST-4).
  return $ case ownerPure h g s e of
    BorrowedRef | not (ocLends (rustCaps cscope) e) -> Owned
    o -> o

-- | Whether a getter path, on a value of the given type, reads a field of a
-- handle.
readsHandle :: Set.Set TVar -> TypeF -> [Either Int Text] -> Bool
readsHandle handles t path = not (Set.null handles) && (isHandleType handles t || throughHandle handles t path)

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

-- | Adapt an expression to a place whose address a read-only sink takes
-- (@&(x)@): a borrowed reference is dereferenced, so the sink reborrows the
-- value instead of referencing the reference or cloning it.
rustReadPlace :: IOwnership -> MDoc -> MDoc
rustReadPlace BorrowedRef x = "(*" <> x <> ")"
rustReadPlace _ x = x


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
-- The captured context is gathered into one tuple, held in an 'Arc' when a
-- staged closure's stage entry reads it too (OWN-3), so no generated
-- signature needs a lifetime. Both the call and the origin builder are
-- NON-CAPTURING closures over that tuple, so each is a plain function
-- pointer.
--
-- A closure that can cross is registered (STATE-4) and builds its origin on
-- first reify. A closure whose manifold has no dispatch entry cannot be
-- called back into, so it is built unregistered and reifying it fails by
-- name rather than by fabricating an identity.
rustClosureWrapper :: Int -> MDoc -> TypeM -> MDoc -> [Arg TypeM] -> [Arg TypeM] -> RustM MDoc
rustClosureWrapper mid sig sigType mname ctxArgs boundArgs = do
  -- the owned type of each capture, naming the tuple the fn pointers read
  capTypes <- mapM (\(Arg _ t) -> captureType t) ctxArgs
  boundTyped <- mapM (\a@(Arg _ t) -> do
                        base <- closureParamType t
                        return ((argNamer rustLocals) a <> ":" <+> base)) boundArgs
  builds <- CMS.gets rsBuilds
  let dead i = Set.member (mid, i) builds
  capInits <- mapM (\a@(Arg i _) -> capInit (dead i) a) ctxArgs
  let n = length boundArgs
      capsT = rustTuple capTypes
      -- Whether the callee takes this capture by reference is the same
      -- question 'rustArgIsRef' answers for a manifold parameter, because
      -- that IS the parameter it is being passed to.
      capRef i (Arg _ t) = captureArg t ("__morloc_c." <> pretty (i :: Int))
      callArgs = mapIndexed capRef ctxArgs
                   <> [rustBridgeArg t ((argNamer rustLocals) a) | a@(Arg _ t) <- boundArgs]
      callFn ct =
        "|__morloc_c: &" <> ct <> hcat [", " <> b | b <- boundTyped] <> "|"
          <+> "unsafe {" <+> mname <> tupled callArgs <+> "}"
  reifyInfo <- CMS.gets rsReifyInfo
  (ctor, mkArgs) <- case Map.lookup (render mname) reifyInfo of
    -- no dispatch entry: nothing can call back into this closure
    Nothing -> return ("::rustmorloc::local" <> pretty n, const [])
    Just (_, capWire, _)
      -- The capture list the dispatch wrapper decodes and the one the closure
      -- carries are built from different projections of the manifold form; a
      -- length disagreement would pair a capture's value with another's
      -- schema, so it is a compiler bug rather than something to truncate.
      | length capWire /= length ctxArgs ->
          error $
            "Rust: closure " <> show mid <> " captures " <> show (length ctxArgs)
              <> " values but " <> show (length capWire) <> " wire forms are registered"
      | otherwise -> do
          let capExpr i (sid, ast) = do
                reify <- reifyInPlace ast
                let fld = "__morloc_c." <> pretty i
                return ("unsafe { ::rustmorloc::reify_capture(&" <> maybe fld ($ fld) reify <> ", " <> RP.schemaRef sid <> ") }")
          capExprs <- mapIndexedM capExpr capWire
          let mkFn ct =
                "|__morloc_c: &" <> ct <> "|"
                  <+> "(\"rust\".to_string(), " <> pretty mid <> "i64, vec!["
                  <> hcat (punctuate ", " capExprs) <> "])"
          return ("::rustmorloc::crossing" <> pretty n, \ct -> [mkFn ct])
  let ctorArgsFor ct = ["__morloc_caps", callFn ct] <> mkArgs ct
      ctorArgs = ctorArgsFor capsT
      boxedDoc =
        vsep
          [ "{"
          , indent 4 $
              vsep
                [ "let __morloc_caps:" <+> capsT <+> "=" <+> rustTuple capInits <> ";"
                , ctor <> tupled ctorArgs
                ]
          , "}"
          ]
      -- A closure that captures nothing and is passed straight to a sourced
      -- function's callback parameter is a plain function, and a plain
      -- function is a POINTER: the callee monomorphizes over a compile-time
      -- constant instead of loading a vtable.
      --
      -- Choosing this form or 'sinkDoc' wrongly cannot miscompile: every
      -- position that needs a stored value renders as `Arc<dyn Fn>`, which
      -- neither satisfies, so a bad choice is a type error in the generated
      -- Rust rather than a wrong answer at run time.
      thinDoc =
        "::rustmorloc::fn_ptr" <> pretty n
          <> parens ("|" <> hcat (punctuate ", " boundTyped) <> "|"
                       <+> "unsafe {" <+> mname <> tupled callArgs <+> "}")
      -- A closure that captures and is passed straight to a sourced
      -- function's callback parameter cannot cross either, so it is a
      -- concrete `move` closure over its captures: no allocation, and the
      -- callee can inline it.
      sinkDoc =
        vsep
          [ "{"
          , indent 4 $
              vsep
                [ "let __morloc_c:" <+> capsT <+> "=" <+> rustTuple capInits <> ";"
                , "move |" <> hcat (punctuate ", " boundTyped) <> "|"
                    <+> "unsafe {" <+> mname <> tupled callArgs <+> "}"
                ]
          , "}"
          ]
  thin <- CMS.gets rsThinClosures
  stageTable <- CMS.gets rsStageTable
  heads <- CMS.gets rsPapplyHeads
  let stage = Map.lookup mid stageTable
      bndWire = case Map.lookup (render mname) reifyInfo of
        Just (_, _, ws) -> Just ws
        Nothing -> Nothing
  case stage of
    Just (StageEntry nctx k smid)
      | nctx /= length ctxArgs ->
          error $ "Rust: staged closure " <> show (render mname) <> " captures " <> show (length ctxArgs)
            <> " values but its stage entry takes " <> show nctx
      | otherwise -> stagedDoc (Just (k, smid)) bndWire capsT capInits ctorArgsFor ctor
    Nothing
      | isJust bndWire && Set.member (render sig) heads ->
          stagedDoc Nothing bndWire capsT capInits ctorArgsFor ctor
      | Set.member (render mname) thin -> return (if null ctxArgs then thinDoc else sinkDoc)
      | otherwise -> return boxedDoc
  where
    -- The function value, wrapped so that each application to its first
    -- argument (see '::rustmorloc::apply1_N') is the one morloc means: a
    -- staged closure runs its stage entry once it has the arguments before
    -- the stage point; a closure that can cross keeps its origin, with each
    -- argument appended to its captured values.
    stagedDoc stage bndWire capsT capInits ctorArgsFor ctor = do
      bts <- mapM (\(Arg _ t) -> rustStoredType t) boundArgs
      resT <- case sigType of
        Function _ o -> rustStoredType o
        o -> rustStoredType o
      wires <- case bndWire of
        Nothing -> return (map (const "None") boundArgs)
        Just ws -> sequence
          [ do
              toWire <- reifyInPlace ast
              let v = maybe "__morloc_v" (\f -> "&" <> parens (f "__morloc_v")) toWire
              return $ "Some((|__morloc_v: &" <> bt <> "| unsafe { ::rustmorloc::reify_capture(" <> v <> ", " <> RP.schemaRef sid <> ") }) as fn(&" <> bt <> ") -> Vec<u8>)"
          | ((sid, ast), bt) <- zipSame ws bts ]
      let n = length boundArgs
          shared = isJust stage && not (null capInits)
          heldT = if shared then "::std::sync::Arc<" <> capsT <> ">" else capsT
          held x = if shared then "::std::sync::Arc::new(" <> x <> ")" else x
          stagedArgs = ctorArgsFor heldT
          dynT ts = rustFnValueType ts resT
          capRefS i (Arg _ t) = captureArg t ("__morloc_sc." <> pretty (i :: Int))
          stageCall smid kk =
            let args = mapIndexed capRefS ctxArgs
                         <> [rustBridgeArg t ("&__morloc_x" <> pretty j) | (j, Arg _ t) <- indexed (take kk boundArgs)]
             in "unsafe {" <+> RP.rustManifoldName smid <> tupled args <+> "}"
          level i fv
            | i >= n = fv
            | otherwise =
                let bt = bts !! i
                    clones =
                      ["let __morloc_sc = __morloc_sc.clone();" | isJust stage]
                        <> ["let __morloc_x" <> pretty j <> " = __morloc_x" <> pretty j <> ".clone();" | j <- [0 .. i - 1]]
                    own = "let __morloc_x" <> pretty i <> ":" <+> bt <+> "= __morloc_xr.clone();"
                    rest = case stage of
                      Just (kk, smid) | i + 1 == kk -> stageCall smid kk
                      _ ->
                        vsep
                          [ "let __morloc_g = ::rustmorloc::pap" <> pretty (n - i) <> "(&__morloc_fc, &__morloc_x" <> pretty i <> ", " <> (wires !! i) <> ");"
                          , level (i + 1) "__morloc_g"
                          ]
                 in vsep
                      [ "{"
                      , indent 4 $ vsep
                          [ "let __morloc_fc = " <> fv <> ".clone();"
                          , "::rustmorloc::staged" <> pretty (n - i) <> "(__morloc_fc.clone(), ::std::sync::Arc::new(move |__morloc_xr: &" <> bt <> "| -> " <> dynT (drop (i + 1) bts) <> " {"
                          , indent 4 (vsep (clones <> [own, rest]))
                          , "}))"
                          ]
                      , "}"
                      ]
      return $
        vsep
          [ "{"
          , indent 4 $
              vsep
                ( [ "let __morloc_caps:" <+> heldT <+> "=" <+> held (rustTuple capInits) <> ";" ]
                    <> ["let __morloc_sc = __morloc_caps.clone();" | isJust stage]
                    <> [ "let __f0 =" <+> ctor <> tupled stagedArgs <> ";"
                       , level 0 "__f0"
                       ]
                )
          , "}"
          ]

-- | The type of a capture in the tuple a closure owns: a packet is held as
-- a '::rustmorloc::PacketRef' (STATE-3), anything else as its stored type.
captureType :: TypeM -> RustM MDoc
captureType t
  | isPacketCapture t = return "::rustmorloc::PacketRef"
  | otherwise = rustStoredType t

-- | A capture read out of the tuple as the argument of the manifold call.
captureArg :: TypeM -> MDoc -> MDoc
captureArg t fld
  | isPacketCapture t = fld <> ".get()"
  | rustArgIsRef t = "&" <> fld
  | otherwise = fld

isPacketCapture :: TypeM -> Bool
isPacketCapture (Serial _) = True
isPacketCapture Passthrough = True
isPacketCapture _ = False

-- | Copy one captured value into the tuple the closure owns: an 'Arc' clone
-- for a function value, a clone for a non-'Copy' value unless building the
-- closure is its last read in a frame that owns it (OWN-3), and a 'Copy'
-- scalar as it is.
capInit :: Bool -> Arg TypeM -> RustM MDoc
capInit dead a@(Arg i t) = do
  parentBorrowed <- asks oeParent
  let name = (argNamer rustLocals) a
      deref = if Set.member i parentBorrowed then "(*" <> name <> ")" else name
  -- A capture the closure owns: a value that is passed by reference must be
  -- cloned out of the frame ('Arc' clone for a function value, which is what
  -- `.clone()` on an `Arc` already is), a 'Copy' scalar is taken as it is, and
  -- a packet pointer is wrapped ('captureType').
  return $
    if isPacketCapture t
      then "::rustmorloc::PacketRef::new(" <> name <> ")"
      else if rustArgIsRef t && not (dead && not (Set.member i parentBorrowed)) then deref <> ".clone()" else deref

-- | The parameters each fully applied native manifold takes by value (OWN-7).
rustSeeds :: PoolPlan -> [SerialManifold] -> Int -> [Int]
rustSeeds pp sms = \m -> Set.toList (Map.findWithDefault Set.empty m byValue)
  where
    byValue = Map.fromListWith Set.intersection [(m, byValueIndices pp m form) | sm <- sms, NativeManifold m _ form@(ManifoldFull _) _ <- nativeManifoldsUnder (RSM sm)]

-- | How a proxy hands one of its arguments to the wire: a value is put as it
-- is; a closure is reified first (its wire form is its origin tuple).
closureArgPush :: Int -> SerialAST -> Int -> RustM MDoc
closureArgPush i ast sid = do
  toWire <- reifyInPlace ast
  let a = "__a" <> pretty i
      v = maybe a (\f -> "&" <> parens (f a)) toWire
  -- The packet belongs to this application alone: its owner releases it
  -- when the proxy body ends, after the result has been read.
  return $ "let __morloc_own" <> pretty i <> " = ::rustmorloc::Packet::new(" <> RP.putValueFn <> "(" <> v <> ", " <> RP.schemaRef sid <> "));"
    <+> "__morloc_pkts.push(__morloc_own" <> pretty i <> ".as_ptr());"

-- | Decoded straight into its native type, as the shared engine leaves it.
rustIsLeaf :: SerialAST -> Bool
rustIsLeaf s = isSerializable s && not (rustDivertsClosure s)

-- | The Rust type of a value as it crosses. What is decoded straight into
-- its type is that type; a record rebuilt field by field is a tuple of its
-- fields' crossing types, since the record's own form names its native
-- fields (as in C++). The runtime's tuple codec takes at most 8 slots.
rustRawType :: SerialAST -> RustM MDoc
rustRawType s
  | rustIsLeaf s = rustTypeOf (unpackedAsTypeF (wireSerialAstToType rustClosureWireLeaf s))
rustRawType (SerialObject _ (FV gv _) _ rs) = do
  when (length rs > 8) $
    CMS.modify $ \st -> st {rsErrors = ("the record " <> unTVar gv <> " is rebuilt field by field where it crosses in Rust, which takes at most 8 fields; it has "
      <> T.pack (show (length rs))) : rsErrors st}
  ds <- mapM (rustRawType . snd) rs
  return $ case ds of
    [d] -> "(" <> d <> ",)"
    _ -> tupled ds
rustRawType (SerialPack _ (_, s)) = rustRawType s
rustRawType (SerialList v d e) = do
  inner <- rustRawType e
  rustTypeOf (AppF (VarF v) (maybe [] (pure . KiA) d ++ [TyA (rawTypeF inner)]))
rustRawType (SerialTuple v es) = do
  inners <- mapM rustRawType es
  rustTypeOf (AppF (VarF v) (map (TyA . rawTypeF) inners))
rustRawType (SerialOptional _ e) = do
  inner <- rustRawType e
  return $ "::std::option::Option<" <> inner <> ">"
rustRawType s = rustTypeOf (unpackedAsTypeF (wireSerialAstToType rustClosureWireLeaf s))

-- | An already rendered Rust type, as a type to render inside another.
rawTypeF :: MDoc -> TypeF
rawTypeF d = VarF (FV (TV "raw") (CV (render d)))

-- | How a proxy reads the result of its call off the wire: a value is got
-- as it is; a closure arrives as its origin tuple and is reflected into a
-- proxy of its own, so a closure may return a closure across pools.
closureResultRead :: SerialAST -> MDoc -> Int -> RustM MDoc
closureResultRead out call sid = do
  fromWire <- reflectInPlace out
  wireT <- rustRawType out
  let got = "::rustmorloc::get_value::<" <> wireT <> ">(" <> call <> ", " <> RP.schemaRef sid <> ")"
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
  let packer = pretty (srcName (val (typePackerForward p)))
      ref v = if rustIsCopy (typePackerUnpacked p) then v else "&" <> parens v
  return $ \v -> packer <> parens (ref (f v))
renderReflect (OverList p) = do
  f <- renderReflect p
  return $ \v -> v <> ".into_iter().map(|__morloc_e| " <> f "__morloc_e" <> ").collect::<Vec<_>>()"
renderReflect (OverOpt p) = do
  f <- renderReflect p
  return $ \v -> v <> ".map(|__morloc_e| " <> f "__morloc_e" <> ")"
-- The source expression is bound ONCE before the slots project out of it.
-- Substituting it per slot would re-evaluate it, and at a dispatch wrapper
-- that expression deserializes a packet: an n-slot tuple would be stitched
-- from n independent deserializations, each with its own shared-memory block
-- to track and free.
renderReflect (OverTuple ps) = do
  u <- getCounter
  fs <- mapM (traverse renderReflect) ps
  let t = "__morloc_tup" <> pretty u
  return $ \v ->
    "{ let" <+> t <+> "=" <+> v <> ";"
      <+> tupled [maybe (slot t j) ($ slot t j) mf | (j, mf) <- indexed fs]
      <+> "}"
  where
    slot v j = v <> "." <> pretty j

-- | The dual: reduce a native value to its wire form by reifying each
-- closure to its origin. Clause for clause the inverse of 'renderReflect',
-- because both are driven by the same 'WirePath'.
renderReify :: WirePath -> RustM (MDoc -> MDoc)
renderReify (AtClosure (SerialClosure ins _)) =
  return $ \v -> "::rustmorloc::require_origin(::rustmorloc::reify(&" <> parens v <> "))"
renderReify (AtClosure _) = error "renderReify: a closure node is a SerialClosure"
-- The inverse: unpack to the form the wire carries, then reduce that.
renderReify (AtPack p inner) = do
  f <- maybe (return id) renderReify inner
  let unpacker = pretty (srcName (val (typePackerReverse p)))
      ref v = if rustIsCopy (typePackerPacked p) then v else "&" <> parens v
  return $ \v -> f (unpacker <> parens (ref v))
renderReify (OverList p) = do
  f <- renderReify p
  return $ \v -> v <> ".iter().map(|__morloc_e| " <> f "__morloc_e" <> ").collect::<Vec<_>>()"
renderReify (OverOpt p) = do
  f <- renderReify p
  return $ \v -> v <> ".as_ref().map(|__morloc_e| " <> f "__morloc_e" <> ")"
-- A slot that is not a closure is COPIED into the wire tuple. Projecting it
-- out by value would move it out of the shared reference the origin builder
-- holds, which a non-'Copy' slot does not allow; a closure slot is only
-- borrowed, so it projects directly.
renderReify (OverTuple ps) = do
  fs <- mapM (traverse renderReify) ps
  return $ \v ->
    tupled
      [ maybe (parens (slot v j) <> ".clone()") ($ slot v j) mf
      | (j, mf) <- indexed fs
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
  return $ assemble ("::rustmorloc::get_value(" <> pkt <> ", " <> RP.schemaRef tupSid <> ")")
rustReflectClosure _ _ = error "rustReflectClosure: expected SerialClosure"

-- | Compute the schema-dependent pieces of an origin-preserving reflect ONCE
-- (in 'RustM', which the schema table needs), returning a PURE assembler that
-- maps a parsed-'ClosureOrigin' expression to the reflected @Arc<dyn Fn>@.
-- Reused by the nested-aggregate reflect ('rustReflectClosureParsed') and the
-- record-field reflect ('printRecordImpls' via 'generateRustStructs'), which
-- differ only in how the origin expression is obtained (an already-parsed tuple
-- vs a @FromVoidstar::read@ off the record's wire slot).
rustReflectClosureAssembler :: SerialAST -> RustM (MDoc -> MDoc)
rustReflectClosureAssembler (SerialClosure ins out) = do
  -- Applied to its first argument, the proxy is the proxy of the rest (see
  -- the C++ member's 'cppClosureProxyLambda').
  step <- case ins of
    [] -> return Nothing
    (a0 : rest) -> do
      a0T <- rustTypeOf (serialAstToNativeType a0)
      a0Sid <- rustRegisterSchema (render (serialAstToMsgpackSchema a0))
      toWire <- reifyInPlace a0
      restSid <- rustRegisterSchema (render (serialAstToMsgpackSchema (SerialClosure rest out)))
      restArgTs <- mapM (rustTypeOf . serialAstToNativeType) rest
      restResT <- rustTypeOf (serialAstToNativeType out)
      restAssemble <- rustReflectClosureAssembler (SerialClosure rest out)
      let v = maybe "__morloc_x" (\f -> "&" <> parens (f "__morloc_x")) toWire
          restDyn = rustFnValueType restArgTs restResT
      return . Just $
        vsep
          [ "::std::sync::Arc::new(move |__morloc_x: &" <> a0T <> "| -> " <> restDyn <> " {"
          , indent 4 $ vsep
              [ "let mut __morloc_o: ::rustmorloc::ClosureOrigin = __morloc_clo_s.clone();"
              , "__morloc_o.2.push(unsafe { ::rustmorloc::reify_capture(" <> v <> ", " <> RP.schemaRef a0Sid <> ") });"
              , "if let Some((__morloc_n, __morloc_k, __morloc_s)) = crate::__morloc_stage_lookup(__morloc_o.1) {"
              , indent 4 $ vsep
                  [ "if __morloc_o.2.len() == __morloc_n + __morloc_k {"
                  , indent 4 $ vsep
                      [ "let __morloc_pkts: Vec<*const u8> = __morloc_o.2.iter().map(|__morloc_p| __morloc_p.as_ptr()).collect();"
                      , "__morloc_o = unsafe { ::rustmorloc::get_value(::rustmorloc::Packet::new(::rustmorloc::foreign_call(&format!(\"pipe-{}\", __morloc_o.0), __morloc_s as u32, &__morloc_pkts)).as_ptr(), " <> RP.schemaRef restSid <> ") };"
                      ]
                  , "}"
                  ]
              , "}"
              , restAssemble "__morloc_o"
              ]
          , "})"
          ]
  argTs <- mapM (rustTypeOf . serialAstToNativeType) ins
  resT <- rustTypeOf (serialAstToNativeType out)
  insSids <- mapM (\ast -> (,) ast <$> rustRegisterSchema (render (serialAstToMsgpackSchema ast))) ins
  resSid <- rustRegisterSchema (render (serialAstToMsgpackSchema out))
  resultDoc <- closureResultRead out "::rustmorloc::Packet::new(::rustmorloc::foreign_call(__morloc_sock, __morloc_c.1.1 as u32, &__morloc_pkts)).as_ptr()" resSid
  pushes <- sequence [closureArgPush i ast sid | (i, (ast, sid)) <- indexed insSids]
  let n = length ins
      params = ["__a" <> pretty i <> ": &" <> t | (i, t) <- indexed argTs]
      bodyDoc =
        vsep
          [ "let __morloc_sock = &__morloc_c.0;"
          , "let mut __morloc_pkts: Vec<*const u8> = Vec::with_capacity(__morloc_c.1.2.len() + " <> pretty n <> ");"
          , "__morloc_pkts.extend(__morloc_c.1.2.iter().map(|__morloc_p| __morloc_p.as_ptr()));"
          , vsep pushes
          , resultDoc
          ]
  return $ \tup ->
    vsep
      [ "{"
      , indent 4 $
          vsep
            ( [ "let __morloc_clo: ::rustmorloc::ClosureOrigin = " <> tup <> ";" ]
                <> ["let __morloc_clo_s = __morloc_clo.clone();" | isJust step]
                <> [ "let __morloc_caps = (format!(\"pipe-{}\", __morloc_clo.0), __morloc_clo.clone());"
                   , "let __morloc_p = ::rustmorloc::proxy" <> pretty n <> "("
                   , indent 4 $
                       vsep
                         [ "__morloc_caps,"
                         , "|__morloc_c: &(String, ::rustmorloc::ClosureOrigin)"
                             <> hcat [", " <> pr | pr <- params] <> "| -> " <> resT <> " { unsafe {"
                         , indent 4 bodyDoc
                         , "} },"
                         , "__morloc_clo,"
                         ]
                   , ");"
                   ]
                <> [ case step of
                       Just st -> "::rustmorloc::staged" <> pretty n <> "(__morloc_p, " <> st <> ")"
                       Nothing -> "__morloc_p"
                   ]
            )
      , "}"
      ]
rustReflectClosureAssembler _ = error "rustReflectClosureAssembler: expected SerialClosure"

-- | Reflect a closure NESTED in an aggregate: the enclosing get_value already
-- produced the wire tuple, so @tup@ is the parsed 'ClosureOrigin' (not a raw
-- packet). Build an origin-preserving @Arc<dyn Fn>@: the reflected value
-- is a proxy carrying the ORIGINAL origin, so a later reify (@reifyN@)
-- reproduces the producing pool (a re-cross A->B->C calls back to A, not B).
rustReflectClosureParsed :: MDoc -> SerialAST -> RustM MDoc
rustReflectClosureParsed tup s = ($ tup) <$> rustReflectClosureAssembler s

-- | Per sourced Rust function, the per-parameter "is generic over its
-- Copy-ness" mask from the DECLARED morloc signature. A bare @a@ is True, and
-- so is an optional or tuple holding one (@?a@, @(a, Int)@): whether it is
-- Copy depends on the instantiation, so host code can only take it by
-- reference. A concrete parameter (Int, Str, [a], record) is False. Read from 'stateSignatures'
-- (still live at codegen): a plain sourced function is 'Monomorphic' with its
-- type variables 'ForallU'-quantified; a typeclass method is 'Polymorphic',
-- whose per-instance 'termGeneral' has been monomorphized, so the class-level
-- signature + the instance's 'classVars' carry the real type variables.
buildSrcTypeVarMask :: MorlocMonad (Map.Map SrcName [(Bool, Bool)])
buildSrcTypeVarMask = Map.fromList <$> fromSourceSignatures paramMask
  where
    paramMask scope qs t = case declaredFun scope t of
      FunT ps _ -> map (\a -> (isGenericSink scope qs a, isFunParam scope a)) ps
      _ -> []

    isGenericSink scope qs a = case expandT scope a of
      VarT v -> Set.member v qs
      OptionalT t -> isGenericSink scope qs t
      AppT (VarT (TV n)) ts | isTupleName n -> any (isGenericSink scope qs) (tyArgs ts)
      _ -> False

    -- A suspension is a function value of no arguments; the host takes it
    -- through the same trait as any other callable.
    isFunParam scope a = case expandT scope a of
      FunT _ _ -> True
      EffectT _ _ -> True
      _ -> False

-- | 'rsSrcMakers'.
buildSrcMakers :: MorlocMonad (Set.Set SrcName)
buildSrcMakers = Set.fromList . map fst . filter snd <$> fromSourceSignatures (\scope _ t -> makes scope (result (declaredFun scope t)))
  where
    result (FunT _ r) = r
    result r = r
    makes scope r = case declaredFun scope r of
      EffectT _ u -> makes scope u
      FunT (_ : _) _ -> True
      _ -> False

-- | A value per sourced Rust function, from its declared signature, read
-- through the general scope, and the type variables it quantifies.
fromSourceSignatures :: (Scope -> Set.Set TVar -> Type -> a) -> MorlocMonad [(SrcName, a)]
fromSourceSignatures f0 = do
  GMap _ sigmap <- MM.gets stateSignatures
  env <- MM.gets stateInstances
  scope <- MM.getGeneralScope
  let f qs t = f0 scope qs (typeOf t)
  return . concatMap (fromSig f env) . Map.elems $ sigmap
  where
    fromSig f _ (Monomorphic (TermTypes (Just et) srcs _)) =
      let (qs, t') = unqualify (etype et)
       in entries f (Set.fromList qs) t' srcs
    fromSig _ _ (Monomorphic (TermTypes Nothing _ _)) = []
    fromSig f env (Polymorphic cls methodEV classET) =
      let cvars = maybe [] snd (Map.lookup methodEV (ieMethods env))
          (fs, t') = unqualify (etype classET)
          qs = Set.fromList (fs <> cvars)
       in concat [entries f qs t' (FI.mtSources m) | m <- FI.methodTerms env cls methodEV]

    entries f qs t' srcs =
      [(srcName s, f qs t') | (_, isrc) <- srcs, let s = val isrc, srcLang s == rustLang]

translate :: [Source] -> [SerialManifold] -> MorlocMonad Script
translate srcs es0 = do
  let es = map markLastReads es0
  let rustSrcs = mapMaybe srcPath [s | s <- srcs, srcLang s == rustLang]
  absSrcs <- unique <$> liftIO (mapM MS.canonicalizePath rustSrcs)
  localCrateDirs <- Map.elems <$> rustLocalDeps
  let userDirs = unique (map takeDirectory absSrcs ++ localCrateDirs)
      includeDocs = map rustSourceInclude absSrcs ++ ["mod __morloc_user_ops;", rustUserFiles absSrcs userDirs]

  debugMode <- MM.gets stateDebugTrace
  debugInfo <- if debugMode
    then (\f i -> Just (f i)) <$> makeManifoldDebugInfoLookup
    else makeTraceLabelLookup

  -- Merge the general typedef scope into the Rust concrete scope so a record
  -- field declared via a general alias resolves through its Rust mapping (as
  -- the C++ member does). Concrete entries win on collision.
  universalScopeMap <- MM.gets stateConcreteTypedefs
  generalScope <- MM.gets stateGeneralTypedefs
  let rustScope = fromMaybe Map.empty (Map.lookup rustLang universalScopeMap)
      mergedRustScope = Map.union rustScope generalScope

  srcTypeVarMask <- buildSrcTypeVarMask
  srcMakers <- buildSrcMakers
  -- Per closure-manifold wire schemas (captured/bound/result), shared with every
  -- pool member; drives the reify path + home-pool dispatch wrappers for a
  -- closure this Rust pool produces and sends to another pool.
  closureAsts <- computeClosureSchemas rustLang es
  let closureTable = Map.map closureSchemaTexts closureAsts
  logTemplates <- collectRenderedTemplates rustLang
  stageTable <- stageTableEntries
  typeHomes <- MM.gets stateTypeHomes
  -- Closures' captured and bound arguments are deserialized by their
  -- dispatch wrappers, not by any manifold, so their types are added.
  let poolTypes = concatMap (poolTypesWith serialAstToType) es <> map (unpackedAsTypeF . serialAstToType) (closureSerialASTs closureAsts)
      uses = recordUses poolTypes
      named = dataNamesIn poolTypes
  (gdefs, gfails) <- genDefs rustLang (Set.fromList [g | (g, _, _, _) <- uses] <> named)
  (ddefs, dfails) <- dataDefs rustLang named
  intrAsts <- inPoolSerialASTs rustLang es
  depRoots <- depNamespaces "rust"
  let poolAsts = concatMap allSerialASTs es <> intrAsts <> closureSerialASTs closureAsts
      recordObjects = Map.map unique (Map.fromListWith (<>) [(g, [rs]) | a <- poolAsts, o@(SerialObject _ (FV g _) _ rs) <- serialSubtrees a, generatedObject o])
  let st0 = defaultValue {rsRecordObjects = recordObjects, rsPoolAsts = poolAsts, rsDepNamespaces = depRoots, rsGenFailures = gfails, rsGenDefs = gdefs, rsDataDefs = ddefs, rsDataNames = declaredDataNames generalScope, rsDataFailures = dfails, rsRecordUses = uses, rsTypeHomes = typeHomes, rsDebugInfo = debugInfo, rsCScope = mergedRustScope, rsSrcTypeVarMask = srcTypeVarMask, rsSrcMakers = srcMakers, rsLogTemplates = logTemplates, rsStageTable = stageTable, rsPool = pool, rsBuilds = lastBuilds Map.empty Set.empty (rustSeeds pool es0) es0}
      pool = planPool (rustCaps mergedRustScope) es
      ((code, typeFiles), stEnd) = CMS.runState (runReaderT (makeRustCode includeDocs closureAsts closureTable es) emptyOwnEnv) st0
  mapM_ (MM.throwRejected . ("Rust pool:" <+>) . pretty) (rsErrors stEnd)
  let privateNote = poolPrivateNote [unTVar gv | gv <- Map.keys (rsRecords stEnd), Map.notMember gv (rsPublic stEnd)]

  home <- MM.asks configHome
  deps <- rustDepsUnion
  localCrates <- rustLocalDeps
  -- The crates the generated type files name, for a crate built from them
  -- outside the pool (GEN-7, GEN-13).
  let typeCrates =
        unique
          [ crate
          | (_, txt) <- typeFiles
          , line <- T.lines txt
          , not ("//" `T.isPrefixOf` T.stripStart line)
          , p <- rustPaths line
          , T.isInfixOf "::" p
          , Just (crate, _) <- [Map.lookup (nameRoot p) depRoots]
          ]
      -- A local crate is a path dependency, as in the pool's own manifest.
      depLine crate = case (Map.lookup crate localCrates, Map.lookup crate deps) of
        (Just path, _) -> [render (pretty crate <> " = { path = " <> dquotes (pretty path) <> " }")]
        (_, Just v) -> [render (pretty crate <> " = " <> dquotes (pretty v))]
        _ -> []
      depsNote =
        [ File typesDependenciesName (Code (T.unlines (concatMap depLine typeCrates)))
        | not (null typeCrates)
        ]
  installDir <- MM.gets stateInstallDir
  profile <- MM.gets stateLangParams >>= either (MM.throwRejected . pretty) return . resolveRustProfile
  -- The pool crate/bin name is derived from the build LOCATION (unique per
  -- program/build-dir, stable across edits of one program). With a shared cargo
  -- --target-dir (used to cache rustmorloc/deps across builds), this keeps one
  -- `morloc make`'s pool binary from colliding with another's, while repeated
  -- builds of the same program overwrite in place rather than accumulating a new
  -- crate per edit. Falls back to a source hash if the build dir is unset.
  let renderedSrc = subVersion (render code)
      (unmarkedSrc, userCalls) = takeUserCallMarks renderedSrc
      poolSrc = unmarkedSrc <> "\n" <> rustUserCalls userCalls <> "\n"
      crateName = "pool_" <> PH.hashText (maybe poolSrc T.pack installDir)
      (cargoToml, buildRs) = makeCargoDocs crateName deps localCrates home profile
  -- The pool starts from the environment's lock: the lock persisted with the
  -- Rust workspace (so every crate shared with rustmorloc resolves to the
  -- version rustmorloc was built against) extended by the crates earlier
  -- pools resolved. Cargo prunes the entries the pool does not use and never
  -- re-resolves what the lock pins, so the shared target-dir holds one build
  -- of each dependency rather than one per registry-index snapshot, and the
  -- registry is consulted only for a crate the lock does not pin; the pins
  -- cargo adds are merged back after the build where they agree with the
  -- base (Build/CargoLock.hs).
  locks <- rustLockPaths
  lockText <- mergeCargoLocks <$> readRustLock (rlBase locks) <*> readLockIfPresent (rlEnv locks)
  maker <- makeTheMaker crateName locks
  let poolSubdir = ML.poolDirKey rustLang

  -- The Rust pool is a real Cargo project: `src/main.rs` is the rendered pool
  -- code, `Cargo.toml` pulls in rustmorloc (path dep) + any declared rust-deps,
  -- `Cargo.lock` pins their versions, and `build.rs` supplies the libmorloc.so
  -- link. `cargo build` (makeTheMaker) does dependency resolution and linking.
  return $
    Script
      { scriptBase = "pool"
      , scriptLang = rustLang
      , scriptCode =
          "." :/ Dir "pools"
            ([ Dir poolSubdir
                [ File "Cargo.toml" (Code (render cargoToml))
                , File "Cargo.lock" (Code lockText)
                , File "build.rs" (Code (render buildRs))
                , Dir "src" [File "main.rs" (Code poolSrc), File "__morloc_user_ops.rs" (Code rustUserOps)]
                ]
            ] <> [Dir "types" [Dir "rust" (typeFileTree ".rs" typeFiles <> privateNote <> depsNote)] | not (null typeFiles && null privateNote)]
            )
      , scriptMake = maker
      }

-- | The two locks a pool build reads and the one it writes back to.
data RustLocks = RustLocks
  { rlBase :: FilePath
  -- ^ the workspace lock persisted by @morloc init@ beside the rustmorloc
  -- source the pool path-depends on; immutable runtime, the authority for
  -- every crate rustmorloc pulls in
  , rlEnv :: FilePath
  -- ^ the environment lock under the mutable state root: the base extended
  -- by every crate a pool built here has resolved; absent until first needed
  }

rustLockPaths :: MorlocMonad RustLocks
rustLockPaths = do
  home <- MM.asks configHome
  state <- MM.asks configState
  return
    RustLocks
      { rlBase = home </> "rust" </> "Cargo.lock"
      , rlEnv = state </> "cache" </> "rust-env.v2.lock"
      }

readRustLock :: FilePath -> MorlocMonad Text
readRustLock lockPath = do
  exists <- liftIO (MS.doesFileExist lockPath)
  if exists
    then liftIO (MT.readFile lockPath)
    else
      MM.throwFailed $
        "missing Rust workspace lock at " <> squotes (pretty lockPath)
          <> "; run `morloc init -f` to persist the Rust runtime source"

readLockIfPresent :: FilePath -> MorlocMonad Text
readLockIfPresent lockPath = do
  exists <- liftIO (MS.doesFileExist lockPath)
  if exists then liftIO (MT.readFile lockPath) else return ""

-- | Emit an @include!@ of a sourced Rust file at the pool crate root, so its
-- @pub fn@s become directly callable by name (mirroring C++ @#include@).
rustSourceInclude :: Path -> MDoc
rustSourceInclude absPath = "include!(" <> rustPathLiteral absPath <> ");"

-- | A path as a Rust string literal; the include! and the user-file table
-- must spell it alike.
rustPathLiteral :: Path -> MDoc
rustPathLiteral = dquotes . pretty . RP.rustEscape . MT.pack

-- | The user's code as panic locations name it, so the pool can tell a user
-- panic from a runtime one (model/runtime/panic.md PANIC-9, PANIC-11): the included
-- sources, the generated operator shims, and, as entries ending in @/@, the
-- directories of the sources and of the local crates.
rustUserFiles :: [Path] -> [Path] -> MDoc
rustUserFiles absPaths dirs =
  "const __MORLOC_USER_FILES: &[&str] = &["
    <> hsep (punctuate "," (map rustPathLiteral absPaths ++ opsFiles ++ map (rustPathLiteral . addSlash) dirs))
    <> "];"
  where
    opsFiles = ["\"src/__morloc_user_ops.rs\"", "concat!(env!(\"CARGO_MANIFEST_DIR\"), \"/src/__morloc_user_ops.rs\")"]
    addSlash d = if "/" `isSuffixOf` d then d else d <> "/"

-- | Sourced Rust binary operators applied through shims in a generated user
-- file, so a panic an operator raises (an integer divided by zero) is located
-- in the user's code (PANIC-11). Each entry is the operator, its shim, the
-- trait the shim requires, and whether it compares (by reference) rather
-- than consumes its operands.
rustOperatorShims :: [(Text, (Text, Text, Bool))]
rustOperatorShims =
  [ ("+", ("add", "::std::ops::Add", False))
  , ("-", ("sub", "::std::ops::Sub", False))
  , ("*", ("mul", "::std::ops::Mul", False))
  , ("/", ("div", "::std::ops::Div", False))
  , ("%", ("rem", "::std::ops::Rem", False))
  , ("&", ("bitand", "::std::ops::BitAnd", False))
  , ("|", ("bitor", "::std::ops::BitOr", False))
  , ("^", ("bitxor", "::std::ops::BitXor", False))
  , ("<<", ("shl", "::std::ops::Shl", False))
  , (">>", ("shr", "::std::ops::Shr", False))
  , ("==", ("eq", "PartialEq", True))
  , ("!=", ("ne", "PartialEq", True))
  , ("<", ("lt", "PartialOrd", True))
  , (">", ("gt", "PartialOrd", True))
  , ("<=", ("le", "PartialOrd", True))
  , (">=", ("ge", "PartialOrd", True))
  ]

-- | The prefix of every local the generated code declares: one the
-- language reserves, so no sourced name is shadowed.
rustLocals :: Text
rustLocals = "__mm_"

-- | Marks the callee of a call of a sourced function in the rendered pool
-- source; 'takeUserCallMarks' removes every mark and records where it was.
userCallMark :: Char
userCallMark = '\x01'

-- | The source without its call marks, and the line and column of the text
-- each mark stood before, as backtraces give them: both from 1, the column
-- in bytes of UTF-8.
takeUserCallMarks :: Text -> (Text, [(Int, Int)])
takeUserCallMarks src =
  let ls = T.splitOn "\n" src
      perLine = map (uncurry unmark) (indexedFrom 1 ls)
   in (T.intercalate "\n" (map fst perLine), concatMap snd perLine)
  where
    unmark n l =
      let pieces = T.splitOn (T.singleton userCallMark) l
          cols = scanl1 (+) [BS.length (TE.encodeUtf8 p) | p <- init pieces]
       in (T.concat pieces, [(n, c + 1) | c <- cols])

-- | Where the generated source calls a sourced function (PANIC-15): a frame
-- at one of these positions is the user's.
rustUserCalls :: [(Int, Int)] -> Text
rustUserCalls calls =
  "const __MORLOC_USER_CALLS: &[(u32, u32)] = &["
    <> T.intercalate ", " [T.pack ("(" <> show l <> ", " <> show c <> ")") | (l, c) <- calls]
    <> "];"

-- | A manifold returns its serialized result as the dispatch's reply
-- (PANIC-10).
rustReply :: MDoc -> MDoc
rustReply e = case T.stripPrefix (render RP.putValueFn <> "(") (render e) of
  Just rest -> "::rustmorloc::put_reply(" <> pretty rest
  Nothing -> e

-- | A comparison shim takes references; an arithmetic shim and a bare
-- operator take values.
rustOperator :: Source -> (TypeM, IOwnership, MDoc) -> (TypeM, IOwnership, MDoc) -> MDoc
rustOperator src (lt, lo, l) (rt, ro, r) = case lookup (unSrcName (srcName src)) rustOperatorShims of
  Just (shim, _, True) -> "__morloc_user_ops::" <> pretty shim <> tupled [rustRef lo l, rustRef ro r]
  Just (shim, _, False) -> "__morloc_user_ops::" <> pretty shim <> tupled [owned lt lo l, owned rt ro r]
  Nothing -> infixOperator src (lt, lo, owned lt lo l) (rt, ro, owned rt ro r)
  where
    owned (Native tf) o x = rustOwn o tf x
    owned _ _ x = x

rustUserOps :: Text
rustUserOps = render . vsep $ "#![allow(dead_code)]" : map shimDoc rustOperatorShims
  where
    shimDoc (op, (name, trait, True)) =
      "#[inline(always)] pub fn " <> pretty name <> "<A: " <> pretty trait <> "<B> + ?Sized, B: ?Sized>(a: &A, b: &B) -> bool { a "
        <> pretty op <> " b }"
    shimDoc (op, (name, trait, False)) =
      "#[inline(always)] pub fn " <> pretty name <> "<A: " <> pretty trait <> "<B>, B>(a: A, b: B) -> A::Output { a "
        <> pretty op <> " b }"

subVersion :: Text -> Text
subVersion = T.replace "__MORLOC_VERSION__" (MT.pack MV.versionStr)

makeRustCode :: [MDoc] -> Map.Map Int ([SerialAST], [SerialAST], SerialAST) -> Map.Map Int ([Text], [Text], Text) -> [SerialManifold] -> RustM (MDoc, [([Text], Text)])
makeRustCode includeDocs0 closureAsts closureTable0 es = do
  planRustPublic es
  structDocs <- generateRustStructs closureAsts es
  enumDocs <- generateRustEnums es
  variantDocs <- generateRustVariants closureAsts es
  -- Records' and `data` arms' conversion helpers, once both are planned.
  convHelpers <- CMS.gets rsConvHelpers
  privateDocs <- (convHelpers <>) <$> privateModuleTree
  stageTable <- CMS.gets rsStageTable
  heads <- papplyHeadSigs (\ins out -> render <$> rustStoredType (typeMof (FunF ins out))) es
  mask <- CMS.gets rsSrcTypeVarMask
  thin <- thinClosures (thinSinkNames mask es) stageTable heads es
  -- Keep the closures that can cross, closed over what they capture. The
  -- crossing set matches by signature, so it can name a closure that never
  -- crosses; a thin closure cannot cross, so it is kept out rather than given
  -- a dispatch entry (and with it the boxed form).
  closureTable <-
    Map.filterWithKey (\i _ -> not (Set.member (render (RP.rustManifoldName i)) thin))
      <$> crossingClosures closureRustSig stageTable es closureTable0
  -- Per crossing closure: a home-pool serial dispatch wrapper (so a foreign pool
  -- can apply it) and the reify info (mid + captured schema ids) that
  -- 'rustClosureWrapper' uses to build the closure's origin builder.
  (closureWrappers0, reifyInfo) <- makeClosureDispatch closureAsts closureTable es
  let closureWrappers = stageLookupDoc stageTable : closureWrappers0
  CMS.modify $ \s -> s {rsReifyInfo = reifyInfo, rsThinClosures = thin, rsPapplyHeads = heads}
  homes <- CMS.gets rsTypeHomes
  needsNode <- CMS.gets rsNeedsNode
  needsHook <- (\h -> needsNode || not (Set.null h)) <$> CMS.gets rsHandles
  (typeFiles, typeTree) <- rustTypeFiles homes needsHook needsNode <$> CMS.gets rsPublicDecls
  let includeDocs = includeDocs0 <> typeTree
  program <- buildProgramM Map.empty Map.empty includeDocs [] es translateSegment getRustSchemaTable closureTable
  -- structDocs go in the schema-table section; the closure dispatch wrappers are
  -- free functions spliced into the signatures section.
  return (RP.printProgram (structDocs <> privateDocs <> enumDocs <> variantDocs) closureWrappers [] program, typeFiles)

-- | A closure value's Rust signature: result type + tupled argument types.
-- Matches the signature seen at a serialize site for the same closure, so a
-- crossing closure and its serialize point compare equal.
closureRustSig :: [TypeF] -> TypeF -> RustM Text
closureRustSig ins out = do
  rt <- rustTypeOf out
  ats <- mapM rustTypeOf ins
  return (render (rt <> tupled ats))

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
  RustM ([MDoc], Map.Map Text (Int, [(Int, SerialAST)], [(Int, SerialAST)]))
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
            then rustRawType ast
            else rustTypeOf t
        | (t, ast, refl) <- zip3Same (ctxTs <> bndTs) (capAsts <> bndAsts) argReflects
        ]
      let (capSids, bndSids) = splitAt (length capScs) argSids
          copies = map rustIsCopy (ctxTs <> bndTs)
          argExprs =
            [ deser t sid copy j refl
            | (j, (t, sid, copy, refl)) <- indexed (zip4Same argTs argSids copies argReflects)
            ]
          -- Deserialize each argument packet to the closure body's native param
          -- form: a Copy value by value, a non-Copy value by shared reference.
          deser t sid copy j refl =
            let raw = "::rustmorloc::get_value::<" <> t <> ">(a(" <> pretty j <> "), " <> RP.schemaRef sid <> ")"
                g = case refl of
                  Just assemble -> assemble raw
                  Nothing -> raw
             in if copy then g else rustRef Owned g
          call = RP.rustManifoldName i <> tupled argExprs
          result = case resAst of
            SerialClosure ins _ ->
              "::rustmorloc::require_origin(::rustmorloc::reify(&(" <> call <> ")))"
            _ -> call
          wrapper =
            vsep
              [ "unsafe fn __morloc_closure_dispatch_" <> pretty i <> "(args: *const *const u8, nargs: usize) -> *mut u8 {"
              , indent 4 $
                  vsep
                    [ "let a = |k: usize| -> *const u8 { if k < nargs { unsafe { *args.add(k) } } else { ::std::ptr::null() } };"
                    , "::rustmorloc::put_reply(&(" <> result <> "), " <> RP.schemaRef resSid <> ")"
                    ]
              , "}"
              ]
      -- The wire form of each capture travels with its schema id: the origin
      -- builder reduces the value through the same 'wirePath' the dispatch
      -- wrapper rebuilds it through.
      return (wrapper, (render (RP.rustManifoldName i), (i, zipSame capSids capAsts, zipSame bndSids bndAsts)))

-- | The stage table ('StageEntry'), for the reflect proxies of closures from
-- other pools.
stageLookupDoc :: Map.Map Int StageEntry -> MDoc
stageLookupDoc tbl =
  vsep
    [ "#[allow(dead_code)]"
    , "fn __morloc_stage_lookup(mid: i64) -> Option<(usize, usize, i64)> {"
    , indent 4 $ vsep
        ( "match mid {"
            : [ indent 4 (pretty f <+> "=> Some((" <> pretty n <> ", " <> pretty k <> ", " <> pretty st <> ")),")
              | (f, StageEntry n k st) <- Map.toAscList tbl ]
            ++ [indent 4 "_ => None,", "}"]
        )
    , "}"
    ]

-- | Collect every record type used in these manifolds, keyed by its FVar, with
-- one representative field list (from a use site). Unlike the shared recmap
-- (which only collects @= "struct"@ records), this also collects user-mapped
-- records (@record Rust => X = "Name"@) so their marshalling impls are emitted.
collectRustRecords :: [SerialAST] -> [SerialManifold] -> [(FVar, [ArgF], [(Key, TypeF)])]
collectRustRecords poolAsts es =
  -- One entry per record; keep the first field list. A generated struct
  -- covers every instantiation with generic parameters, so it is one entry
  -- per general name. A user-mapped struct may be a template the user
  -- wrote, whose instantiations each need their own marshalling, so those are
  -- one entry per (name, form, arguments, fields): a newtype's form over a
  -- record keeps the record's name. 'generateRustStructs' keeps one
  -- impl for instantiations that render the same struct.
  nubBy ((==) `on` \(FV gv cv, ps, rs) -> (gv, cv, if cv == CV "struct" then ([], []) else (ps, rs)))
    $ concatMap (runIdentity . foldWithSerialManifoldM fm) es <> concatMap crossing poolAsts
  where
    fm = defaultValue {opFoldWithNativeExprM = ne, opFoldWithSerialExprM = se}
    ne _ (DeserializeN_ t _ xs) = return $ xs <> generated (seek t)
    ne efull e = return $ foldlNE (<>) (generated (seek (typeFof efull))) e
    se _ e = return $ foldlSE (<>) [] e

    -- A generated record converts its own fields, so any value of it a
    -- pool holds may be marshalled as itself (GEN-14).
    generated = filter (\(FV _ cv, _, _) -> cv == CV "struct")

    -- The records a crossing value is marshalled through: those in the
    -- parts decoded straight into their native types, and in closure
    -- signatures. A user-mapped record rebuilt field by field crosses as a
    -- tuple, so it has no marshaller of its own there.
    crossing s = concat [seek (serialAstToNativeType x) | x <- serialSubtrees s, rustIsLeaf x || isClosure x]
    isClosure (SerialClosure _ _) = True
    isClosure _ = False

    seek :: TypeF -> [(FVar, [ArgF], [(Key, TypeF)])]
    -- A table is an Arrow record batch with its own marshalling in
    -- rustmorloc, not a struct to generate.
    seek (NamF NamTable _ _ _) = []
    seek (NamF _ v@(FV _ _) ps rs) = (v, ps, rs) : concatMap seek (tyArgs ps) <> concatMap (seek . snd) rs
    -- A record reachable only as a `data` arm's field still needs its
    -- struct emitted, so the walk descends through arms as the enum
    -- collector's does.
    seek (VariantF (FV _ _) ps as) = concatMap seek (tyArgs ps) <> concatMap (concatMap seek . snd) as
    seek (AppF t ts) = seek t <> concatMap seek (tyArgs ts)
    seek (FunF ts t) = concatMap seek (t : ts)
    seek (OptionalF t) = seek t
    seek _ = []

-- | Outer name of a concrete-scope typedef body, when it contributes a name.
bodyName :: TypeU -> Maybe Text
bodyName (VarU (TV n)) = Just n
bodyName (AppU (VarU (TV n)) _) = Just n
bodyName (NamU _ (TV n) _ _) = Just n
bodyName _ = Nothing

-- | Every argument-free @data@ type used in these manifolds, unmerged.
collectRustEnums :: [SerialManifold] -> [(FVar, [ArgF], [Text])]
collectRustEnums = concatMap (runIdentity . foldWithSerialManifoldM fm)
  where
    fm = defaultValue {opFoldWithNativeExprM = ne, opFoldWithSerialExprM = se}
    ne _ (DeserializeN_ t s xs) = return $ xs <> seek t <> seek (unpackedAsTypeF (serialAstToType s))
    ne efull e = return $ foldlNE (<>) (seek (typeFof efull)) e
    se _ (SerializeS_ s xs) = return $ seek (unpackedAsTypeF (serialAstToType s)) <> xs
    se _ e = return $ foldlSE (<>) [] e

    seek :: TypeF -> [(FVar, [ArgF], [Text])]
    seek (EnumF v@(FV gv _) ps ns) = (v, ps, ns) : concatMap seek (tyArgs ps)
    seek (VariantF (FV _ _) ps as) = concatMap seek (tyArgs ps) <> concatMap (concatMap seek . snd) as
    seek (NamF _ _ _ rs) = concatMap (seek . snd) rs
    seek (AppF t ts) = seek t <> concatMap seek (tyArgs ts)
    seek (FunF ts t) = concatMap seek (t : ts)
    seek (OptionalF t) = seek t
    seek _ = []

-- | Every payload-bearing @data@ type used in these manifolds, unmerged.
collectRustVariants :: [SerialManifold] -> [(FVar, [ArgF], [(Text, [TypeF])])]
collectRustVariants = concatMap (runIdentity . foldWithSerialManifoldM fm)
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

-- | Emit the enum definition and marshalling impls for every payload-bearing
-- @data@ type in the pool. Ownership follows the same rule as records and
-- enums: a user-mapped @data Rust => X = "..."@ writes its own type in
-- sourced Rust and gets only the impls.
generateRustVariants :: Map.Map Int ([SerialAST], [SerialAST], SerialAST) -> [SerialManifold] -> RustM [MDoc]
generateRustVariants closureAsts es = do
  ddefs0 <- CMS.gets rsDataDefs
  -- A template some instance of which holds a function, or converts an
  -- arm's field, is marshalled at each instance, whose conversions are
  -- wired by name; any other once for every instance.
  let perInstance = Set.fromList ([gv | (FV gv _, _, arms) <- collectRustVariants es, any holdsFunR (concatMap snd arms)]
                      <> [gv | (gv, dd) <- Map.toList ddefs0, any holdsFunR (concatMap snd (ddArms dd))])
                      <> convertingVariants closureAsts es
  named <- mapM occurrence (collectRustVariants es)
  wireArms <- variantWireArms rustTypeOf closureAsts es
  -- Merged by rendered name: one declaration per name.
  instances <- concat <$> mapM (uncurry (merged perInstance wireArms)) (Map.toList (Map.fromListWith (flip (<>)) named))
  templates <- templateDecls perInstance
  return (templates <> instances)
  where
    -- Each generated template, declared once from its definition; an
    -- arm's type takes the parameters its fields hold.
    templateDecls perInstance = do
      ddefs <- CMS.gets rsDataDefs
      concat <$> sequence
        [ do
            arms' <- mapM (\(c, ts) -> (,) c <$> mapM rustFieldType ts) (ddArms dd)
            CMS.modify $ \s -> s {rsNeedsNode = rsNeedsNode s || any (not . null . snd) arms'}
            let params = map (pretty . nativeParam) (nativeDataParams dd)
                generics ps = RP.paramList (map (pretty . nativeParam) ps)
                armDecl n c = pretty (render n <> "_" <> c) <> generics (armDataParams dd c)
                decl n = RP.printRustVariant (n <> generics (nativeDataParams dd)) (armDecl n) (armDecl n) arms'
            decls <- declareOrKeep (Just [c | (c, _ : _) <- arms']) gv decl (pretty (privateDataName gv))
            -- Marshalled once for every instance: two morloc types that are
            -- one Rust type are one instance.
            qname <- rustTypeOf (RecF (FV gv (CV (ddForm dd))) [TyA (VarF (FV (TV ("&" <> unTVar p)) (CV (nativeParam p)))) | p <- ddTypeParams dd])
            let qhead = T.takeWhile (/= '<') (render qname)
                impls = [RP.printVariantImpls params qname (\c -> pretty (qhead <> "_" <> c) <> generics (armDataParams dd c)) [(c, ts, Nothing) | (c, ts) <- arms'] | Set.notMember gv perInstance]
            return (decls <> impls)
        | (gv, dd) <- Map.toList ddefs ]

    -- One occurrence under its rendered name, with each arm's field types
    -- as written and as rendered, so the merge can compare spellings.
    occurrence (v, ps, as) = do
      n <- rustTypeOf (VariantF v ps as)
      as' <- mapM (\(c, ts) -> (\rs -> (c, (ts, map render rs))) <$> mapM rustFieldType ts) as
      return (render n, [(v, ps, as')])
    merged perInstance wireArms name occs = case mergeVariantOccurrences name [((v, ps), as) | (v, ps, as) <- occs] of
      Right ((v, ps), arms) -> makeOne perInstance (Map.lookup name wireArms) (v, ps, arms)
      Left msg -> CMS.modify (\s -> s {rsErrors = msg : rsErrors s}) >> return []

    -- A type holding a function gets impls only where it crosses: then each
    -- arm field that does not write as it is held (a closure, a packed
    -- value) crosses as its wire form, converted in the arm's marshalling
    -- (GEN-14).
    makeOne perInstance wireArms (FV gv (CV cvText), ps, arms) = do
      userMapped <- rustUserMapsVariant gv cvText
      arms' <- mapM (\(n, ts) -> (,) n <$> mapM rustFieldType ts) arms
      name <- rustTypeOf (VariantF (FV gv (CV cvText)) ps arms)
      let hasFun = any (any containsFunF . snd) arms
          converting = maybe False (any (any fieldConverts . snd)) wireArms
          leaf = T.filter (\c -> isAlphaNum c || c == '_') (render name)
      wired <- case wireArms of
        Just was | hasFun || converting -> mapM (\(c, ts) -> (,,) c ts <$> armWire ("mlc_arm_" <> pretty leaf <> "_" <> pretty c) ts (lookup c was)) arms'
        _ -> return [(c, ts, Nothing) | (c, ts) <- arms']
      ddefs <- CMS.gets rsDataDefs
      -- A generated template whose declaration has no definition: no
      -- instance of it can be spelled.
      dfails <- CMS.gets rsDataFailures
      when (not userMapped && T.any (== '$') cvText && Map.notMember gv ddefs) $
        CMS.modify $ \s -> s {rsErrors = ("the generated Rust type for " <> unTVar gv <> " cannot be defined from its declaration: "
          <> Map.findWithDefault "it does not resolve in Rust" gv dfails) : rsErrors s}
      -- An instance of a template names its arms at the arguments each
      -- arm's type takes.
      armRef <- case Map.lookup gv ddefs of
        Just dd | not userMapped, T.any (== '$') cvText -> do
          args <- mapM (\(p, t) -> (,) p <$> rustFieldType t) (zipPrefix (ddTypeParams dd) (tyArgs ps))
          let headName = T.takeWhile (/= '<') (render name)
          return $ \c -> pretty (headName <> "_" <> c) <> RP.paramList [d | p <- armDataParams dd c, Just d <- [lookup p args]]
        _ -> return (RP.armTypeName name)
      let impls = [RP.printVariantImpls [] name armRef wired | not hasFun || isJust wireArms]
      if userMapped
        then do
          -- A user-written arm is a tuple variant, one field per morloc
          -- field, walked at morloc's type of the field.
          fields <- mapM (\(c, ts) -> (,) c <$> mapM userField ts) arms
          CMS.modify $ \s -> s {rsUserArms = Map.insert (render name) fields (rsUserArms s)}
          let userWired = [(c, fs, mw) | ((c, fs), (_, _, mw)) <- zipSame fields wired]
          return [RP.printUserVariantImpls name userWired | not hasFun || isJust wireArms]
        else if Map.member gv ddefs && T.any (== '$') cvText
          -- A template is marshalled once for every instance
          -- ('templateDecls') unless some instance holds a function.
          then return (if Set.member gv perInstance then impls else [])
          else do
            CMS.modify $ \s -> s {rsNeedsNode = rsNeedsNode s || any (not . null . snd) arms'}
            let decl n = RP.printRustVariant n (RP.armTypeName n) (RP.armTypeName n) arms'
            -- An instance named by its arguments is the pool's own.
            if Map.member gv ddefs
              then return (decl name : impls)
              else (<> impls) <$> declareOrKeep (Just [c | (c, _ : _) <- arms']) gv decl name

    userField (OptionalF x) = (\t -> (t, True)) <$> rustFieldType x
    userField t = (\t' -> (t', False)) <$> rustFieldType t

    -- An arm whose fields include one that converts crosses as the tuple of
    -- its fields' wire forms. A field reached through closures, packers,
    -- lists, optionals and tuples alone converts in place; one that is not
    -- written as it is held otherwise converts through a helper pair built
    -- by the shared engine; a record holding closures converts them in its
    -- own marshalling.
    armWire :: MDoc -> [MDoc] -> Maybe [SerialAST] -> RustM (Maybe RP.ArmWire)
    armWire base ts (Just ss)
      | Just fields <- zipExact ts ss = do
          cfg <- loweringConfig
          convs <- mapIndexedM (conv cfg) fields
          if all isNothing convs
            then return Nothing
            else do
              u <- getCounter
              let slot v j = v <> "." <> pretty j
                  t = "__morloc_arm" <> pretty u
                  plain ty = (ty, \v -> parens v <> ".clone()", id)
                  convs' = [maybe (plain ty) id c | ((ty, _), c) <- zipSame fields convs]
              return . Just $
                RP.ArmWire
                  { RP.awType = RP.tupled1 [w | (w, _, _) <- convs']
                  , RP.awReify = \v -> RP.tupled1 [r (slot v j) | (j, (_, r, _)) <- indexed convs']
                  , RP.awReflect = \v -> "{ let" <+> t <+> "=" <+> v <> ";" <+> RP.tupled1
                      [f (slot t j) | (j, (_, _, f)) <- indexed convs'] <+> "}"
                  }
      where
        conv cfg j (_, s)
          | Just p <- wirePath s = do
              r <- renderReify p
              f <- renderReflect p
              w <- rustRawType s
              return (Just (w, r, f))
          | isSerializable s = return Nothing
          | otherwise = do
              (w, to, from) <- conversionHelpers cfg (base <> "_" <> pretty (j :: Int)) s
              return (Just (w, \v -> to <> "(&" <> parens v <> ")", \v -> from <> parens v))
    armWire _ _ _ = return Nothing

    loweringConfig = do
      mask <- CMS.gets rsSrcTypeVarMask
      makers <- CMS.gets rsSrcMakers
      consumed <- CMS.gets (ppByValue . rsPool)
      userArms <- CMS.gets rsUserArms
      return (rustLowerConfig mask makers consumed userArms)

-- | Whether a type holds a function or suspension.
holdsFunR :: TypeF -> Bool
holdsFunR t = case t of
  FunF _ _ -> True
  _ -> any holdsFunR (childrenOf t)

-- | The name a generated template no module file holds goes by in the
-- pool: its identifier, prefixed where it would hide a prelude type.
privateDataName :: TVar -> Text
privateDataName gv = let n = tvarIdentifier gv in if Set.member n rustPreludeTypes then "Mlc" <> n else n

-- | Whether the user maps this @data@ type to a Rust type of their own, whose
-- arms are tuple variants (RUST-4).
rustUserMapsVariant :: TVar -> Text -> RustM Bool
rustUserMapsVariant gv cvText = (\cscope -> userMapsVariantIn cscope gv cvText) <$> CMS.gets rsCScope

-- | Impls for every nullary @data@ type in the pool, plus the enum itself
-- unless the user maps it.
generateRustEnums :: [SerialManifold] -> RustM [MDoc]
generateRustEnums es = do
  named <- mapM (\(v, ps, ns) -> (\n -> (render n, (v, ps, ns))) <$> rustTypeOf (EnumF v ps ns))
                (collectRustEnums es)
  concat <$> mapM merged (Map.toList (Map.fromListWith (<>) [(n, [o]) | (n, o) <- named]))
  where
    -- Each Rust type gets its own impls, so one name serves one `data` type.
    merged (name, occs@((v, ps, _) : _)) =
      case nubOrd [gv | (FV gv _, _, _) <- occs] of
        [_] -> case mergeEnumOccurrences name (nubOrd [ns | (_, _, ns) <- occs]) of
          Right ctors -> makeOne (v, ps, ctors)
          Left msg -> failWith msg
        gvs -> failWith $ "the `data` types " <> MT.intercalate ", " (map unTVar gvs) <> " share the Rust name " <> name
    merged (_, []) = return []
    failWith :: Text -> RustM [MDoc]
    failWith msg = CMS.modify (\s -> s {rsErrors = msg : rsErrors s}) >> return []

    makeOne (FV gv (CV cvText), ps, ctors) = do
      userMapped <- cscopeDeclares gv cvText
      name <- rustTypeOf (EnumF (FV gv (CV cvText)) ps ctors)
      let impls = RP.printEnumImpls name ctors
      if userMapped
        then return [impls]
        else (<> [impls]) <$> declareOrKeep (Just []) gv (\n -> RP.printRustEnum n ctors) name

    -- True iff the concrete scope holds an entry for @gv@ whose body names
    -- @cvText@; that is what distinguishes a real per-language mapping from
    -- a name the compiler defaulted to the type's own.
    cscopeDeclares :: TVar -> Text -> RustM Bool
    cscopeDeclares gv cvText = do
      cscope <- CMS.gets rsCScope
      return $ case Map.lookup gv cscope of
        Just entries -> any (\(_, body, _, _, _) -> bodyName body == Just cvText) entries
        Nothing -> False

-- | The declaration of a generated type: set aside for its module's file
-- under the type's own name when the type is declared there (GEN-6), else
-- returned for the pool under the given name. A `data` type is also
-- offered at the crate root under its own name and its arm types' (the
-- given constructors), which sourced files named it by before it had a
-- file; a record (no list) never had a name a source could write, so it is
-- not.
declareOrKeep :: Maybe [Text] -> TVar -> (MDoc -> MDoc) -> MDoc -> RustM [MDoc]
declareOrKeep keepRootName gv decl poolName = do
  public <- CMS.gets rsPublic
  homes <- CMS.gets rsTypeHomes
  case (Map.member gv public, Map.lookup gv homes) of
    (True, Just h) -> do
      CMS.modify $ \s -> s {rsPublicDecls = rsPublicDecls s <> [(gv, decl (pretty (typeHomeName h)), keepRootName)]}
      return []
    _ -> return [decl poolName]

-- | Declare a generated record: in its module's file when it is public,
-- else in the pool's private module (GEN-10), which 'privateModuleTree'
-- writes once all are known.
declareRecord :: TVar -> (MDoc -> MDoc) -> RustM [MDoc]
declareRecord gv decl = do
  public <- CMS.gets rsPublic
  names <- CMS.gets rsRecordNames
  let q = Map.findWithDefault (unTVar gv) gv names
      (path, leaf) = T.breakOnEnd "::" q
  if Map.member gv public
    then declareOrKeep Nothing gv decl (pretty leaf)
    else do
      CMS.modify $ \st -> st {rsPrivateDecls = rsPrivateDecls st <> [(fromMaybe path (T.stripSuffix "::" path), decl (pretty leaf))]}
      return []

-- | The pool's private module tree, holding the generated records no
-- module's file holds (GEN-10).
privateModuleTree :: RustM [MDoc]
privateModuleTree = do
  decls <- CMS.gets rsPrivateDecls
  let byPath = Map.fromListWith (flip (<>)) [(maybe [] (T.splitOn "::") (T.stripPrefix "crate::" p), [d]) | (p, d) <- decls]
      tree pre = Map.fromListWith (<>) [(c, [(rest, ds)]) | (c : rest, ds) <- pre]
      render' entries =
        vsep $
          [d | ([], ds) <- entries, d <- ds]
            <> [ vsep ["pub mod" <+> pretty (rustModIdent c) <+> "{", indent 4 ("#[allow(unused_imports)] use crate::*;" <> line <> render' sub), "}"]
               | (c, sub) <- Map.toList (tree [(p, ds) | (p@(_ : _), ds) <- entries]) ]
  return [render' (Map.toList byPath) | not (null decls)]

-- | The marshalling of a generated record the pool marshals (GEN-14): one
-- generic impl, each field written as it is held, converted the same way
-- at every instance (its declared type names no parameter), or, when its
-- conversion depends on the arguments, through a trait for that field
-- implemented by the field's type at each instance. Nothing for a record
-- the pool never marshals.
recordImpls :: Bool -> GenDef -> MDoc -> [MDoc] -> RustM [MDoc]
recordImpls handle gd name params = do
  objectsOf <- CMS.gets rsRecordObjects
  mask <- CMS.gets rsSrcTypeVarMask
  makers <- CMS.gets rsSrcMakers
  consumed <- CMS.gets (ppByValue . rsPool)
  userArms <- CMS.gets rsUserArms
  homesOf <- CMS.gets rsTypeHomes
  let gv = gdVar gd
      cfg = rustLowerConfig mask makers consumed userArms
      objects = Map.findWithDefault [] gv objectsOf
      converts fa = case fa of
        SerialClosure _ _ -> True
        _ -> not (isSerializable fa) || not (null (serialClosuresOutsideNodes fa))
      -- Names derived from the record are global to the crate, so they
      -- carry its module path.
      leaf = Map.findWithDefault (tvarIdentifier gv) gv homeKeys
      homeKeys = Map.map (\h -> encodeModulePath (typeHomePath h <> [typeHomeName h])) homesOf
      complain :: Text -> RustM ()
      complain msg = CMS.modify $ \st -> st {rsErrors = msg : rsErrors st}
      -- The parameters whose position a reference of the record to itself
      -- holds another type at.
      own = [p | (p, KindType) <- gdParams gd]
      swappedParams =
        unique [ p | (_, f) <- gdFields gd, (g, ps) <- selfRefs f, g == gv
                   , (p, a) <- zipPrefix own (tyArgs ps), paramOf a /= Just p ]
      selfRefs t = case t of
        RecF (FV g _) ps -> [(g, ps)]
        NamF _ (FV g _) ps _ -> (g, ps) : concatMap selfRefs (childrenOf t)
        _ -> concatMap selfRefs (childrenOf t)
  if null objects
    then return []
    else do
      planned <- mapIndexedM (field cfg converts leaf complain objects swappedParams) (gdFields gd)
      let fields4 = [f | (f, _, _) <- planned]
          wheres = ([w | (_, Just (w, _), _) <- planned], [w | (_, Just (_, w), _) <- planned])
          extras = concat [d | (_, _, d) <- planned]
          impls = RP.printRecordImplsWhere handle RP.RenderedFieldTypes name params (if null params then Nothing else Just wheres) fields4
      return (extras <> [impls])
  where
    -- One field: its marshalling, its where clauses (writing, reading),
    -- and the helpers and trait items it needs.
    field cfg converts leaf complain objects swappedParams i (k, declT) = do
      let fas = unique [fa | rs <- objects, Just fa <- [lookup k rs]]
          held = not (null (placeholders declT))
          fld = RP.rustFieldIdent k
      ty <- rustFieldType declT
      -- A field that refers to the record itself is bounded through the
      -- record's parameters: bounding its own type would require the
      -- record's impl at the field's arguments, which for a record nested
      -- at other arguments (`T b a` in `T a b`) never closes.
      let bounded tr
            | recursiveRecord (gdVar gd) [(k, declT)] =
                hcat (punctuate ", " [pretty (nativeParam p) <> ": " <> tr | p <- nativeParams gd])
            | otherwise = ty <> ": " <> tr
          plainWheres = if held then Just (bounded "::rustmorloc::ToVoidstar", bounded "::rustmorloc::FromVoidstar") else Nothing
          width = isVarWidthF declT || held
      case filter converts fas of
        [] -> return ((fld, ty, width, Nothing), plainWheres, [])
        cs@(c : _)
          | not held -> do
              when (length (unique cs) > 1) $
                complain ("the field `" <> unKey k <> "` of the generated Rust type for " <> unTVar (gdVar gd) <> " converts two ways")
              nestedClosure c
              m <- marshal cfg (pretty leaf) i 0 c
              return ((fld, ty, True, Just m), Nothing, [])
          | otherwise -> do
              when (any (`elem` swappedParams) (placeholders declT)) $
                complain ("the generated Rust type for " <> unTVar (gdVar gd) <> " cannot convert its field `" <> unKey k
                            <> "`: the record refers to itself at other arguments, where the field holds another type; give "
                            <> unTVar (gdVar gd) <> " a Rust form, `record Rust => " <> unTVar (gdVar gd) <> " = \"...\"`")
              let tr = "MlcConv_" <> pretty leaf <> "_" <> pretty i
              byType <- mapM (\fa -> (\t -> (render t, fa)) <$> rustFieldType (serialAstToNativeType fa)) fas
              let groups = Map.fromListWith (flip (<>)) [(t, [fa]) | (t, fa) <- byType]
              sequence_
                [ complain ( "the generated Rust type " <> render name <> " is used at two morloc types that convert its field `"
                               <> unKey k <> "` differently, so one Rust type cannot hold both; give " <> unTVar (gdVar gd)
                               <> " a Rust form, `record Rust => " <> unTVar (gdVar gd) <> " = \"...\"`" )
                | (_, g) <- Map.toList groups, length (unique [if converts fa then Just fa else Nothing | fa <- g]) > 1 ]
              impls <- mapIndexedM
                (\j (t, g) -> case g of
                    fa : _ | converts fa -> do
                      nestedClosure fa
                      m <- marshal cfg (pretty leaf) i (j + 1) fa
                      let how = case m of
                            RP.PackMarshal raw to from -> RP.TraitPack raw to from
                            RP.ClosureMarshal reflect -> RP.TraitClosure reflect
                            RP.ViaTrait _ -> RP.TraitPlain
                      return [RP.printFieldTraitImpl tr (pretty t) i how]
                    _ -> return [RP.printFieldTraitImpl tr (pretty t) i RP.TraitPlain])
                (Map.toList groups)
              return ((fld, ty, True, Just (RP.ViaTrait tr)), Just (ty <> ": " <> tr, ty <> ": " <> tr), RP.printFieldTrait tr : concat impls)
      where
        nestedClosure fa = case fa of
          SerialClosure _ _ -> return ()
          _ | not (null (serialClosuresOutsideNodes fa)) ->
                complain ("the field `" <> unKey k <> "` of the generated Rust type for " <> unTVar (gdVar gd)
                            <> " holds a function inside a container, which cannot cross into or out of a Rust pool yet; give "
                            <> unTVar (gdVar gd) <> " a Rust form, `record Rust => " <> unTVar (gdVar gd) <> " = \"...\"`")
            | otherwise -> return ()

    -- How a converting field is marshalled; its conversion functions go to
    -- the pool's helpers.
    marshal cfg leaf i j fa = case fa of
      SerialClosure _ _ -> do
        assembler <- rustReflectClosureAssembler fa
        return (RP.ClosureMarshal {RP.cmReflect = assembler})
      _ -> do
        (raw, to, from) <- conversionHelpers cfg ("mlc_conv_" <> leaf <> "_" <> pretty (i :: Int) <> "_" <> pretty (j :: Int)) fa
        return (RP.PackMarshal raw to from)

-- | A pool helper pair converting a value between its native type and the
-- form it crosses as, through the shared engine: the crossing type and the
-- names of the two functions, which take the native value by reference
-- and the crossing value by value.
conversionHelpers :: LowerConfig RustM -> MDoc -> SerialAST -> RustM (MDoc, MDoc, MDoc)
conversionHelpers cfg base fa = do
  ty <- rustFieldType (serialAstToNativeType fa)
  (toStmts, toE) <- nativeToWire cfg "(*mlc_x)" fa
  (fromE, fromStmts) <- wireToNative cfg "mlc_r" fa
  raw <- rustRawType fa
  let to = base <> "_to"
      from = base <> "_from"
  CMS.modify $ \st -> st
    { rsConvHelpers =
        rsConvHelpers st
          <> [ vsep
                 [ "#[allow(non_snake_case, unused_variables)]"
                 , "fn" <+> to <> "(mlc_x: &" <> ty <> ") ->" <+> raw <+> "{"
                 , indent 4 (vsep (map RP.printStmt toStmts <> [RP.printExpr toE]))
                 , "}"
                 , "#[allow(non_snake_case, unused_variables)]"
                 , "fn" <+> from <> "(mlc_r:" <+> raw <> ") ->" <+> ty <+> "{"
                 , indent 4 (vsep (map RP.printStmt fromStmts <> [RP.printExpr fromE]))
                 , "}"
                 ]
             ]
    }
  return (raw, to, from)

-- | Choose the generated types this pool declares in a file of their own,
-- one per declaring module (GEN-6), and give them their paths from the crate
-- root (GEN-4). A type qualifies when its definition cannot depend on its
-- uses: a record with no parameters whose uses all have the same fields, a
-- `data` type with no parameters. Any other generated type is declared at
-- the crate root under its pool-local name (GEN-10).
planRustPublic :: [SerialManifold] -> RustM ()
planRustPublic es = do
  depRoots <- CMS.gets rsDepNamespaces
  homes <- CMS.gets rsTypeHomes
  ddefs <- CMS.gets rsDataDefs
  cscope <- CMS.gets rsCScope
  defs <- CMS.gets rsGenDefs
  let unparameterized gv = case Map.lookup gv cscope of
        Just ((ps, _, _, _, _) : _) -> null ps
        _ -> False
      -- A type named like a prelude type would hide it in its own module,
      -- where native forms name the prelude type unqualified (`Vec<$1>`),
      -- so it stays in the pool under a pool-private name.
      nameOk gv = maybe False (not . (`Set.member` rustPreludeTypes) . typeHomeName) (Map.lookup gv homes)
      eligible gv = unparameterized gv && nameOk gv
      -- Every generated record the pool uses has one definition, from its
      -- declaration (GEN-9); one that refers to itself is a handle.
      selfRef gd = recursiveRecord (gdVar gd) (gdFields gd)
      recDefs = Map.filter ((== "struct") . gdForm) defs
      -- Rust refuses a generic parameter no field's type names (E0392); a
      -- parameter held only inside a record the pool keeps to itself is
      -- not named.
      usesParams gd = do
        tokens <- Set.fromList . concatMap (T.split (\c -> not (isAlphaNum c || c == '_')) . render) <$> mapM (rustTypeOf . snd) (gdFields gd)
        return (all ((`Set.member` tokens) . nativeParam) (nativeParams gd))
      -- A private record named like a prelude type would hide it in its
      -- module, so its name there is prefixed.
      privateLeaf n = if Set.member n rustPreludeTypes then "Mlc" <> n else n
      privateName gv = "crate::mlc_pool::" <> maybe ("_nohome::" <> privateLeaf (tvarIdentifier gv)) (\h -> render (rustModPath (typeHomePath h)) <> "::" <> privateLeaf (typeHomeName h)) (Map.lookup gv homes)
  vars <- filterM (\(FV gv (CV cv), _, _) -> (&&) (eligible gv) . not <$> rustUserMapsVariant gv cv)
    [o | o@(_, ps, _) <- collectRustVariants es, null ps]
  enums <- filterM (\(FV gv (CV cv), _, _) -> (&&) (eligible gv) . not <$> rustUserMapsVariant gv cv)
    [o | o@(_, ps, _) <- collectRustEnums es, null ps]
  allVars <- filterM (\(FV gv (CV cv), _, _) -> not <$> rustUserMapsVariant gv cv) (collectRustVariants es)
  allEnums <- filterM (\(FV gv (CV cv), _, _) -> not <$> rustUserMapsVariant gv cv) (collectRustEnums es)
  -- A type's place follows from its declaration alone (GEN-10): a file
  -- can name only what any crate can import -- the standard library, the
  -- language's own types, its parameters, and the generated types other
  -- files hold.
  let candidates = Map.filterWithKey (\gv gd -> nameOk gv && all (isIdentifier . nativeParam) (nativeParams gd)) recDefs
      nameAll pub = Map.mapWithKey (\gv _ -> if Set.member gv pub then maybe (privateName gv) qualifiedRustName (Map.lookup gv homes) else privateName gv) recDefs
      generated = Set.unions [Map.keysSet recDefs, Set.fromList [gv | (FV gv _, _, _) <- allVars], Set.fromList [gv | (FV gv _, _, _) <- allEnums]]
      placed =
        placeTypes (rustImportable (Map.keysSet depRoots) Set.empty Set.empty) generated . Map.fromList $
          [(gv, map snd (gdFields gd)) | (gv, gd) <- Map.toList candidates]
            <> [(gv, concatMap snd arms) | (FV gv _, _, arms) <- vars]
            <> [(gv, []) | (FV gv _, _, _) <- enums]
            <> [(gv, concatMap snd (ddArms dd)) | (gv, dd) <- Map.toList ddefs, nameOk gv, all (isIdentifier . nativeParam) (nativeDataParams dd)]
      recPublic = Set.intersection placed (Map.keysSet candidates)
  CMS.modify $ \s -> s {rsRecords = recDefs, rsRecordNames = nameAll recPublic}
  unnamed <- filterM (fmap not . usesParams) (Map.elems recDefs)
  let public = placed
      names = Map.fromList [(gv, qualifiedRustName h) | gv <- Set.toList public, Just h <- [Map.lookup gv homes]]
      handles = Map.keysSet (Map.filter selfRef recDefs)
      badWords =
        [ (gv, c)
        | gv <- Set.toList (Set.union public (Map.keysSet recDefs)), Just h <- [Map.lookup gv homes]
        , c <- typeHomePath h, Set.member c rustUnspellable || not (isIdentifier c)
        ]
  -- Every use of a generated record is an instance of its definition
  -- (GEN-9). A user-mapped record's unpacked use is a tuple, never its form.
  uses <- CMS.gets rsRecordUses
  forM_ (disagreements recDefs uses) $ \(gv, d) -> do
    msg <- case d of
      FieldDiffers k decl use -> do
        dr <- render <$> rustTypeOf decl
        ur <- render <$> rustTypeOf use
        return ("the generated Rust type for " <> unTVar gv <> " cannot serve one of its uses: its field `" <> unKey k
                 <> "` is `" <> ur <> "` there but `" <> dr <> "` in the definition; give " <> unTVar gv
                 <> " a Rust form, `record Rust => " <> unTVar gv <> " = \"...\"`")
      Unmatched e -> return ("the generated Rust type for " <> unTVar gv <> " cannot serve one of its uses: " <> e)
    CMS.modify $ \s -> s {rsErrors = msg : rsErrors s}

  CMS.modify $ \s -> s
    { rsPublic = names
    , rsHandles = handles
    , rsErrors =
        [ "the generated Rust type for " <> unTVar (gdVar gd) <> " cannot be written: a parameter its declaration"
            <> " holds is spelled by none of its fields' Rust forms; give it a Rust form, `record Rust => "
            <> unTVar (gdVar gd) <> " = \"...\"`"
        | gd <- unnamed ]
          <> [ "the generated Rust type for " <> unTVar gv <> " cannot be written: its module path would hold `" <> c
            <> "`, which Rust cannot spell as a module name"
             | (gv, c) <- badWords ]
          <> [ "the generated Rust name " <> n <> " would name both " <> T.intercalate " and " (map unTVar gvs)
             | (n, gvs) <- Map.toList (Map.fromListWith (flip (<>)) ([(n, [gv]) | (gv, n) <- Map.toList names] <> [(n <> "Fields", [gv]) | (gv, n) <- Map.toList names, Set.member gv handles]
                   <> [ (n <> "_" <> c, [gv])
                      | (gv, arms) <- Map.toList (Map.fromList ([(gv, arms) | (FV gv _, _, arms) <- vars] <> [(gv, ddArms dd) | (gv, dd) <- Map.toList ddefs]))
                      , Just n <- [Map.lookup gv names]
                      , (c, _ : _) <- arms
                      ]))
             , length gvs > 1 ]
          <> rsErrors s
    }

-- | Whether a rendered Rust type names only what a crate of generated
-- types can import (GEN-10): the standard library, the language's own and
-- prelude types, paths under the given dependency namespaces (GEN-7), the
-- given generated types, and the given parameters.
rustImportable :: Set.Set Text -> Set.Set Text -> Set.Set Text -> Text -> Bool
rustImportable roots known params = all ok . rustPaths
  where
    ok p
      | Set.member p known = True
      | T.isInfixOf "::" p && Set.member (nameRoot p) roots = True
      | Set.member p params = True
      | Set.member p rustOwnTypes = True
      | any (`T.isPrefixOf` p) ["::std::", "std::", "::core::", "core::"] = True
      | T.all isDigit p = True
      | otherwise = False
    rustOwnTypes =
      Set.fromList
        [ "i8", "i16", "i32", "i64", "i128", "isize", "u8", "u16", "u32", "u64", "u128", "usize"
        , "f32", "f64", "bool", "char", "str", "String", "Vec", "Option", "Box", "dyn", "Fn", "Send", "Sync", "static"
        ]

-- | The paths a Rust text names, each with its separators: `a::b::C`.
rustPaths :: Text -> [Text]
rustPaths t = case T.uncons t of
  Nothing -> []
  Just (c, _)
    | isAlphaNum c || c == '_' || c == ':' ->
        let (p, rest) = T.span (\x -> isAlphaNum x || x == '_' || x == ':') t
         in [T.dropWhileEnd (== ':') p | T.any (\x -> isAlphaNum x || x == '_') p] <> rustPaths rest
    | otherwise -> rustPaths (T.drop 1 t)

-- | A generated type's path from the crate root (GEN-4).
qualifiedRustName :: TypeHome -> Text
qualifiedRustName h = "crate::mlc::types::" <> render (rustModPath (typeHomePath h)) <> "::" <> typeHomeName h

-- | A module path below @crate::mlc::types@, each keyword written as a raw
-- identifier.
rustModPath :: [Text] -> MDoc
rustModPath = pretty . T.intercalate "::" . map rustModIdent

rustModIdent :: Text -> Text
rustModIdent c
  | Set.member c rustReservedWords = "r#" <> c
  | otherwise = c

-- | Types of the Rust prelude, which no generated type may be named.
rustPreludeTypes :: Set.Set Text
rustPreludeTypes =
  Set.fromList
    [ "Vec", "String", "Option", "Result", "Box", "Some", "None", "Ok", "Err", "ToString", "ToOwned"
    , "Fn", "FnMut", "FnOnce", "Send", "Sync", "Sized", "Unpin", "Clone", "Copy", "Drop", "Default"
    , "Eq", "PartialEq", "Ord", "PartialOrd", "Iterator", "IntoIterator", "DoubleEndedIterator"
    , "ExactSizeIterator", "Extend", "FromIterator", "From", "Into", "TryFrom", "TryInto", "AsRef", "AsMut"
    ]

-- | Module path components Rust cannot spell even as raw identifiers.
rustUnspellable :: Set.Set Text
rustUnspellable = Set.fromList ["crate", "self", "super", "Self"]

-- | The files of the generated types set aside in 'rsPublicDecls', one per
-- module path, and the module tree that includes them at the crate root,
-- given the path from the pool's main file to the types directory.
rustTypeFiles :: Map.Map TVar TypeHome -> Bool -> Bool -> [(TVar, MDoc, Maybe [Text])] -> ([([Text], Text)], [MDoc])
rustTypeFiles homes needsHook needsNode decls = (files, [tree | needsHook || needsNode || not (null files)] <> aliases)
  where
    byPath = Map.fromListWith (flip (<>)) [(typeHomePath h, [d]) | (gv, d, _) <- decls, Just h <- [Map.lookup gv homes]]
    -- The `data` types of each file, which the crate root offered by their
    -- own names before they had a file.
    rootNames =
      Map.fromListWith (flip (<>))
        [ (typeHomePath h, typeHomeName h : [typeHomeName h <> "_" <> c | c <- arms])
        | (gv, _, Just arms) <- decls
        , Just h <- [Map.lookup gv homes]
        ]
    -- A glob over a module re-exporting only those types, so that the crate
    -- root offers them as before while a sourced file's own item or `use`
    -- of the same name takes precedence. Records are left out: a glob at
    -- the root would shadow a prelude name (`Vec`, `Result`).
    aliases =
      [ "pub use crate::mlc::types::" <> rustModPath p <> "::__morloc_data::*;"
      | p <- Map.keys rootNames ]
    files =
      [ ( p
        , render . vsep $
            [ "// Generated by morloc; do not edit."
            , "#[allow(unused_imports)]"
            , "use crate::*;"
            ] <> ds
              <> [ "#[doc(hidden)] pub mod __morloc_data { pub use super::{" <> hsep (punctuate "," (map pretty ns)) <> "}; }"
                 | Just ns <- [Map.lookup p rootNames] ]
              <> [""]
        )
      | (p, ds) <- Map.toList byPath
      ]
    -- The release hook every generated arm and handle drops through
    -- (STATE-8), beside the module tree of the generated files.
    tree =
      vsep
        [ "pub mod mlc {"
        , if needsHook then "pub fn rec_release<T>(a: ::std::sync::Arc<T>) { ::rustmorloc::rec_release(a) }" else ""
        , if needsNode then vsep [RP.printRustNode "::rustmorloc::rec_release", RP.printRustNodeImpls] else ""
        , if null files then "" else rustTypesModule "../../types/rust/mlc/types/" (map fst files)
        , "}"
        ]

-- | The @types@ module of a crate's @mlc@ module: one module per generated
-- file path, each including its file from below the given directory.
rustTypesModule :: Text -> [[Text]] -> MDoc
rustTypesModule dir paths = "pub mod types {" <+> hsep (map node (grouped paths [])) <+> "}"
  where
    grouped ps prefix =
      [ (c, prefix <> [c], [rest | (c' : rest) <- ps, c' == c])
      | c <- unique [c | (c : _) <- ps]
      ]
    node (c, full, rests) =
      "pub mod" <+> pretty (rustModIdent c) <+> "{"
        <+> (if [] `elem` rests then "include!(" <> dquotes (pretty (dir <> T.intercalate "/" full <> ".rs")) <> ");" else "")
        <+> hsep (map node (grouped (filter (not . null) rests) full))
        <+> "}"

-- | The root of a standalone crate holding a program's generated Rust types
-- (GEN-13): the @mlc@ module a pool supplies, with a release that drops
-- plainly, since outside a pool there is no runtime drain.
rustStandaloneLib :: [[Text]] -> Text
rustStandaloneLib paths =
  render . vsep $
    [ "// Generated by morloc; do not edit."
    , "#![allow(dead_code, non_camel_case_types)]"
    , "pub mod mlc {"
    , "pub fn rec_release<T>(a: ::std::sync::Arc<T>) { drop(a) }"
    , RP.printRustNode "crate::mlc::rec_release"
    , rustTypesModule "../mlc/types/" paths
    , "}"
    , ""
    ]

-- | Rust keywords, written as raw identifiers in a generated module path.
rustReservedWords :: Set.Set Text
rustReservedWords =
  Set.fromList
    [ "as", "async", "await", "break", "const", "continue", "crate", "dyn", "else", "enum", "extern"
    , "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub"
    , "ref", "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe"
    , "use", "where", "while", "abstract", "become", "box", "do", "final", "macro", "override", "priv"
    , "typeof", "unsized", "virtual", "yield", "try", "gen"
    ]

-- | Emit a struct definition (only for autogenerated @= "struct"@ records) plus
-- @ToVoidstar/FromVoidstar@ impls for every record used in the pool. User-mapped
-- records provide their own struct (in sourced Rust), so only the impls are
-- emitted for them.
generateRustStructs :: Map.Map Int ([SerialAST], [SerialAST], SerialAST) -> [SerialManifold] -> RustM [MDoc]
generateRustStructs closureAsts es = do
  poolAsts <- CMS.gets rsPoolAsts
  keyed <- concat <$> mapM makeOne (collectRustRecords poolAsts es)
  -- A Rust struct gets one impl. Instantiations of a user-mapped record that
  -- render the same struct must agree on its fields (they differ only in
  -- parameters no field uses); otherwise one struct cannot hold both.
  let users = Map.fromListWith (flip (<>))
        [(render n, [(gv, render (fieldSig fs))]) | (Just (n, gv, fs), _) <- keyed]
  forM_ (Map.toList users) $ \(n, xs) -> case (nubOrd (map fst xs), nubOrd (map snd xs)) of
    ([_], [_]) -> return ()
    (gvs@(_ : _ : _), _) -> rustError $
      "the records " <> T.intercalate ", " (map unTVar gvs) <> " are all mapped to the Rust struct " <> n
        <> ", whose marshalling only one of them can have"
    _ -> rustError $
      "the Rust struct " <> n <> " is the form of a record used at instantiations whose field types differ;"
        <> " map the record to a generic struct that takes the varying parameters"
  return (firstPerStruct Set.empty keyed)
  where
    rustError :: Text -> RustM ()
    rustError msg = CMS.modify (\s -> s {rsErrors = msg : rsErrors s})
    fieldSig fs = hsep [fld <> ":" <+> ty | (fld, ty, _) <- fs]
    firstPerStruct _ [] = []
    firstPerStruct seen ((Nothing, d) : rest) = d : firstPerStruct seen rest
    firstPerStruct seen ((Just (n, _, _), d) : rest)
      | Set.member (render n) seen = firstPerStruct seen rest
      | otherwise = d : firstPerStruct (Set.insert (render n) seen) rest
    -- Each record's closure fields (keyed by the record's general TVar + field
    -- Key), harvested from every (de)serialization site; the 'SerialClosure'
    -- carries the arg/result wire schemas the reify/reflect need. Duplicate keys
    -- (the same record with a closure field at different arities) keep the last.
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

    -- A function field (needs `Arc<dyn Fn>` boxing + reify/reflect); a
    -- suspension field is one of arity zero.
    isFunF = isFunctionTypeF

    -- The marshalling strategy for a field: a function field that has a harvested
    -- crossing 'SerialClosure' gets reify/reflect; anything else is plain. A
    -- function field with NO harvest hit returns Nothing -- it is used natively
    -- but never crosses in this pool, so its record stays struct-only.
    fieldMarshal :: TVar -> Key -> TypeF -> RustM (Maybe RP.ClosureMarshal)
    fieldMarshal g k ty
      | isFunF ty = case Map.lookup (g, k) harvest of
          Just s@(SerialClosure _ _) -> do
            assembler <- rustReflectClosureAssembler s
            return $ Just RP.ClosureMarshal {RP.cmReflect = assembler}
          _ -> return Nothing
      | otherwise = return Nothing

    makeOne :: (FVar, [ArgF], [(Key, TypeF)]) -> RustM [(Maybe (MDoc, TVar, [(MDoc, MDoc, Bool)]), MDoc)]
    makeOne (v@(FV gv _), ps, rs) = case v of
      -- A generated record is defined from its declaration (GEN-9) and
      -- marshalled by one impl ('recordImpls', GEN-14).
      FV _ (CV "struct") -> do
        records <- CMS.gets rsRecords
        case Map.lookup gv records of
          Nothing -> return []
          Just gd -> do
            names <- CMS.gets rsRecordNames
            handle <- Set.member gv <$> CMS.gets rsHandles
            let name = pretty (Map.findWithDefault (unTVar gv) gv names)
                params = map (pretty . nativeParam) (nativeParams gd)
            -- A field holding a parameter has the width of its argument,
            -- which may vary.
            fields <- mapM (\(k, t) -> (\ty -> (RP.rustFieldIdent k, ty, isVarWidthF t || not (null (placeholders t)))) <$> rustFieldType t) (gdFields gd)
            let printDecl n
                  | handle = RP.printRustHandle n params [(fld, ty) | (fld, ty, _) <- fields]
                  | otherwise = RP.printRustStruct n params [(fld, ty) | (fld, ty, _) <- fields]
            decl <- declareRecord gv printDecl
            impls <- recordImpls handle gd name params
            return $ map ((,) Nothing) (decl <> impls)
      -- User-mapped record: the user writes the (monomorphic) struct, so only the
      -- marshalling impls are emitted, with concrete field types. A closure field
      -- reifies/reflects in place (via 'fieldMarshal'); the impl is emitted only
      -- when EVERY function field has a harvested crossing site, else struct-only.
      FV _ (CV _) -> do
        -- The struct's name at this instantiation: a template the user
        -- wrote takes this occurrence's arguments.
        name <- rustTypeOf (NamF NamRecord v ps rs)
        typed <- mapM (\r -> (,) r <$> oneField gv name r) rs
        marshalled <- mapM (\((k, ty), (fld, ty', w)) -> (,) ty . (,,,) fld ty' w <$> fieldMarshal gv k ty) typed
        let fields4 = map snd marshalled
            -- Emit the impl only when EVERY function field has a harvested marshal
            -- (a non-function field imposes no requirement); else struct-only.
            emitImpl = and [not (isFunF ty) || isJust m | (ty, (_, _, _, m)) <- marshalled]
        return [(Just (name, gv, map snd typed), RP.printRecordImpls RP.StructFieldTypes name [] fields4) | emitImpl]

    -- Render one field's (name, type, is-variable-width). A self-optional
    -- (?self) is boxed so the recursive cycle is Sized.
    oneField :: TVar -> MDoc -> (Key, TypeF) -> RustM (MDoc, MDoc, Bool)
    oneField selfGv selfName (k, ty) = do
      handles <- CMS.gets rsHandles
      ty' <- case ty of
        OptionalF inner | refersTo selfGv inner, not (Set.member selfGv handles) -> return $ "::std::option::Option<::std::boxed::Box<" <> selfName <> ">>"
        _ -> rustFieldType ty
      return (RP.rustFieldIdent k, ty', isVarWidthF ty)


-- | Conservative: only pure fixed-width scalar (and all-fixed tuple) fields let
-- a record short-circuit shm_size to schema.width. Anything variable-region
-- (Str/Vec/Option/record) or unknown counts as variable (always safe -- a false
-- \"variable\" just uses the correct per-field sum).
isVarWidthF :: TypeF -> Bool
isVarWidthF (VarF (FV (TV gv) _)) = gv == "Str"
isVarWidthF (AppF (VarF (FV (TV gv) _)) ts)
  | isTupleName gv = any isVarWidthF (tyArgs ts)
isVarWidthF _ = True

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

-- | Track each manifold's borrowed parameters as the body is folded, so
-- 'rustOwnership' can tell a borrowed @&T@ parameter from an owned value. For a
-- native-loop manifold the loop-carried locals are additionally recorded (for
-- the @let mut@ / never-move handling) and unioned into the shared set.
rustSurround :: SurroundManifoldM RustM PoolDocs PoolDocs LowS LowN (TypeS, PoolDocs) LowArg
rustSurround =
  defaultValue
    { surroundSerialManifoldM = \recurse sm -> do
        cscope <- CMS.gets rsCScope
        withPlan (planSerialManifold (rustCaps cscope) sm) (recurse sm)
    , surroundNativeManifoldM = \recurse nm -> do
        pp <- CMS.gets rsPool
        cscope <- CMS.gets rsCScope
        withPlan (planNativeManifold (rustCaps cscope) pp nm) (recurse nm)
    }
  where
    withPlan p = withManifoldScope (mpBorrowed p) (mpShared p) (mpCarried p)

-- | No tag test may follow a take (see the projection-consume golden's
-- secondMatchRust).
rustCaps :: Scope -> OwnCaps
rustCaps cscope = OwnCaps {ocCheap = rustIsCopy, ocTagAfterTake = False, ocLends = lends}
  where
    -- RUST-4: an optional field of a user-written enum may sit behind a
    -- pointer, leaving no optional value in memory to lend.
    lends (IntrinsicN (OptionalF _) IntrCtorField _ (s : _)) = case typeFof s of
      VariantF (FV gv (CV cv)) _ _ -> not (userMapsVariantIn cscope gv cv)
      _ -> True
    lends _ = True

-- | Whether the user maps a `data` type to a Rust type of their own.
userMapsVariantIn :: Scope -> TVar -> Text -> Bool
userMapsVariantIn cscope gv cvText = case Map.lookup gv cscope of
  Just entries -> any (\(_, body, _, _, _) -> bodyName body == Just cvText) entries
  Nothing -> False

translateSegment :: SerialManifold -> RustM MDoc
translateSegment m0@(SerialManifold root _ _ _ _) = do
  resetCounter
  CMS.modify (\st -> st {rsCurrentRoot = root})
  mask <- CMS.gets rsSrcTypeVarMask
  makers <- CMS.gets rsSrcMakers
  consumed <- CMS.gets (ppByValue . rsPool)
  userArms <- CMS.gets rsUserArms
  e <- surroundFoldSerialManifoldM rustSurround (defaultFoldRules (rustLowerConfig mask makers consumed userArms)) m0
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
              MM.throwRejected $
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

-- | The pool crate's @[profile.release]@ settings, as TOML literals (string
-- forms carry their quotes). Both come from the @rust:@ build parameters.
data RustProfile = RustProfile
  { rpLto :: Text
  , rpOptLevel :: Text
  }
  deriving (Eq, Show)

-- | Resolve the release profile from build parameters, validating against
-- Cargo's own vocabulary. @rust:lto@ takes the values Cargo's @lto@ field takes
-- (@off@, @false@, @thin@, @fat@, @true@) and defaults to @thin@: cross-crate
-- LTO over rustmorloc and the Arrow crates, which costs seconds per pool link
-- and is what a test run turns off. @rust:opt-level@ takes @0@-@3@, @s@ or @z@
-- and defaults to @2@. Returns 'Left' with a user-facing message on an unknown
-- value.
resolveRustProfile :: BP.LangParams -> Either Text RustProfile
resolveRustProfile lp = do
  lto <- pick "lto" "thin" ltoValues
  opt <- pick "opt-level" "2" optValues
  return (RustProfile lto opt)
  where
    -- (accepted spelling, TOML literal)
    ltoValues = [("off", "\"off\""), ("false", "false"), ("thin", "\"thin\""), ("fat", "\"fat\""), ("true", "true")]
    optValues = [(v, v) | v <- ["0", "1", "2", "3"]] ++ [("s", "\"s\""), ("z", "\"z\"")]
    pick key def table =
      let v = fromMaybe def (BP.lookupParam "rust" key lp)
       in case lookup v table of
            Just lit -> Right lit
            Nothing ->
              Left $
                "unknown rust:" <> key <> " value '" <> v <> "'; expected one of "
                  <> T.intercalate ", " (map fst table)

-- | Render the generated pool crate's @Cargo.toml@ and @build.rs@. rustmorloc
-- is a path dependency on the source persisted at @$MORLOC_HOME/rust@ (by
-- @morloc init@); external crates come from the DAG-wide rust-deps union.
-- @build.rs@ carries the libmorloc.so link (link-search under
-- @$MORLOC_HOME/lib@; libmorloc is found at run time via the nexus-exported
-- LD_LIBRARY_PATH). The release profile must set @panic = "unwind"@ to match
-- rustmorloc/morloc-runtime (the SHM arena relies on Drop-on-unwind cleanup).
-- @crateName@ is unique per program (a source hash) so a shared target-dir
-- never collides one program's pool binary with another's.
makeCargoDocs :: Text -> Map.Map Text Text -> Map.Map Text FilePath -> FilePath -> RustProfile -> (MDoc, MDoc)
makeCargoDocs crateName deps localCrates home profile =
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
        vsep $
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
          -- The defaults match the data/rust workspace profile so the
          -- rustmorloc built by `morloc init`'s warm-up is reused here rather
          -- than recompiled under a differing fingerprint; a non-default lto
          -- or opt-level builds (and caches) its own set of dependencies.
          , "opt-level = " <> pretty (rpOptLevel profile)
          , "lto = " <> pretty (rpLto profile)
          , [idoc|panic = "unwind"|]
          -- Line tables let the panic hook walk inlined frames (PANIC-9).
          , [idoc|debug = "line-tables-only"|]
          ]
          -- PANIC-12: macOS otherwise leaves them in the build cache's object
          -- files; packed, they go to a bundle copied beside the pool.
          ++ [[idoc|split-debuginfo = "packed"|] | SI.os == "darwin"]
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
-- over rustmorloc + any external crates, pinned by the shipped Cargo.lock),
-- merge the pins cargo
-- added into the environment lock, then copy the binary to the
-- @pool-rust.out@ path the manifest/nexus expect. @MORLOC_HOME@ is set on the
-- command so rustmorloc's build.rs locates libmorloc.so. The @--target-dir@ is
-- shared across programs so rustmorloc + deps compile once and cache (cargo's
-- own lock serialises concurrent builds; the per-program @crateName@ keeps each
-- program's output binary distinct within that shared dir).
makeTheMaker :: Text -> RustLocks -> MorlocMonad [SysCommand]
makeTheMaker crateName locks = do
  -- cargo is required at make time (a Rust pool is a Cargo project). Fail fast
  -- with a clear message rather than a raw shell "command not found".
  cargoAvail <- liftIO (findExecutable "cargo")
  case cargoAvail of
    Just _ -> return ()
    Nothing ->
      MM.throwFailed
        "building a Rust pool requires `cargo` on PATH (install Rust: https://rustup.rs)"
  home <- MM.asks configHome
  state <- MM.asks configState
  let poolSubdir = ML.poolDirKey rustLang
      outRel = pretty $ "pools" </> poolSubdir </> ML.makeExecutablePoolName rustLang
      manifestPath = pretty $ "pools" </> poolSubdir </> "Cargo.toml"
      poolLock = "pools" </> poolSubdir </> "Cargo.lock"
      -- The shared cargo build cache is regenerable STATE, not runtime: it lives
      -- under the state root, never inside the immutable runtime lib/.
      targetDir = state </> "cache" </> "rust-build"
      targetD = pretty targetDir
      binPath = pretty $ targetDir </> "release" </> T.unpack crateName
      homeD = pretty home
      -- Paths are single-quoted: they are interpolated into a shell string and
      -- $MORLOC_HOME (hence targetDir/binPath) may contain spaces.
      -- Cargo keeps every pin of a consistent lock and fetches only what the
      -- lock lacks or the cache does not hold, so a pool whose crates are all
      -- pinned and cached needs no network. The build is never forced
      -- offline: a crate's features can need a dependency the lock has no
      -- table for, which only an online resolution can add. A user without
      -- a network sets CARGO_NET_OFFLINE.
      buildCmd =
        SysRun . Code . render $
          [idoc|MORLOC_HOME='#{homeD}' cargo build --release --manifest-path '#{manifestPath}' --target-dir '#{targetD}'|]
      copyCmd =
        SysRun . Code . render $
          if SI.os == "darwin"
            then [idoc|cp '#{binPath}' '#{outRel}' && rm -rf '#{outRel}.dSYM' && cp -RL '#{binPath}.dSYM' '#{outRel}.dSYM'|]
            else [idoc|cp '#{binPath}' '#{outRel}'|]
      -- Runs only after a successful build, so a failed resolution never
      -- writes anything into the environment lock.
      mergeCmd = SysMergeCargoLock (rlBase locks) (rlEnv locks) poolLock
  return [SysRequireToolchain RustPoolToolchain, buildCmd, mergeCmd, copyCmd]

-- | The lowering configuration. The core fields are real; the fields for
-- closures/partial application, remote calls, caching, and pattern evaluation
-- raise a clear v1-unsupported error and are unreachable for the pool shapes
-- v1 supports.
rustLowerConfig :: Map.Map SrcName [(Bool, Bool)] -> Set.Set SrcName -> Map.Map Int (Set.Set Int) -> Map.Map Text [(Text, [RP.UserField])] -> LowerConfig RustM
rustLowerConfig mask makers consumed userArms =
  LowerConfig
    { lcLocalPrefix = rustLocals
    , lcSrcName = \src -> pretty (T.cons userCallMark (unSrcName (srcName src)))
    , lcOperator = rustOperator
    -- A curried host returns a function value, applied with call syntax; the
    -- mark records where the call is (PANIC-16).
    , lcApplySrcGroup = \f as ->
        pretty (T.singleton userCallMark) <> parens f <> tupled as
    -- A function the host returns is any 'impl Fn'; morloc holds it as a
    -- function value. One a call group returns is the host's too; a
    -- function at a type variable of the signature is one the host was
    -- given, and is returned as it was (HOST-8).
    , lcSourcedResult = \src n t x -> case t of
        FunF ins@(_ : _) _
          | n < maybe 0 length (Map.lookup (srcName src) mask) || Set.member (srcName src) makers ->
              "::rustmorloc::IntoFnValue" <> pretty (length ins) <> "::into_fn_value" <> parens x
        _ -> x
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
         in case tm of
              -- A function-typed argument is a function value (a saturated
              -- sub-manifold is typed by its result, so it never lands here).
              -- Host code may declare a higher-order parameter as
              -- `F: Fn(&A..) -> R`, which an `Arc` does not satisfy, so the
              -- value is handed over through `IntoCallbackN`; every
              -- other function sink (a morloc-defined `&Arc<dyn Fn>`
              -- parameter, a type-variable parameter, a closure application)
              -- borrows it.
              Function ts _
                | isFunParam ->
                    "::rustmorloc::IntoCallback" <> pretty (length ts) <> "::into_callback"
                      <> parens (case own of Owned -> x; _ -> parens x <> ".clone()")
                | otherwise -> rustRef own x
              -- A closure application and a sourced type-variable parameter are
              -- reference sinks; otherwise pass by the value type. (`isVar` is only
              -- ever True under 'SourcedArg'.)
              Native tf
                | ClosureArg <- site -> rustRef own x
                | isVar -> rustRef own x
                | ManifoldArg m j <- site
                , Set.member j (Map.findWithDefault Set.empty m consumed) ->
                    rustOwn own tf x
                | otherwise -> byType tf
              _ -> x
    , lcConstArg = \_ _ x -> x
    , lcOwnership = rustOwnership
    , lcArgManifoldOwnership = \_ -> return Owned
    , lcBindCallArgs = False
    , lcOwnArg = rustOwn
    , lcReadArg = rustReadPlace
    , lcWithCallerScope = rustWithCallerScope
    -- `?T` is `Option<T>`, whose wire layout is type-driven; a widened value must
    -- be a real `Some(..)` (never a bare `T`) or put_value serializes the wrong
    -- shape. The coercion only ever wraps a non-optional inner, so this is safe.
    -- A constructor in EXPRESSION position is spelled through the qualified
    -- path `<T>::Ctor`, which is legal for any rendered type -- a generic
    -- instantiation `MyBox<i64>` included, where `MyBox<i64>::Ctor` is not.
    -- In PATTERN position that path is not stable Rust, so a pattern names
    -- the bare head (`MyBox::Ctor`); the subject's type pins the arguments.
    -- A generated arm takes its fields as a tuple converted into its arm type;
    -- a user-written arm takes each field converted into the form its
    -- variant declares (RUST-4).
    , lcVariantLit = \ty n _ xs ->
        let arm = "<" <> ty <> ">::" <> pretty n
        in case userArmFields userArms ty n of
             _ | null xs -> arm
             Just fs -> arm <> tupled [RP.userFieldFrom f x | (f, x) <- zipSame fs xs]
             Nothing -> arm <> parens (parens (RP.tupled1 xs) <> ".into()")
    , lcEnumLit = \ty _ n _ -> "<" <> ty <> ">::" <> pretty n
    , lcVariantTagTest = \ty n _ subj ->
        "matches!" <> tupled [subj, RP.typeHead ty <> "::" <> pretty n <> " { .. }"]
    -- A generated arm's whole payload sits in one node, mirroring the wire
    -- form, so it has a single field whatever its arity; a user-written arm
    -- is a tuple variant read field by field ('userCtorField', RUST-4). The guard has already established the arm, so the other branch is
    -- unreachable rather than a fallback.
    -- Matched through a REFERENCE. A pattern binding several fields emits
    -- one projection per field against the same subject, so matching by
    -- value would move the payload on the first and leave the rest with
    -- nothing. Borrowing makes each projection independent; the clone is
    -- what hands an owned value on from a borrowed place. A field that is
    -- itself a `data` value clones as a shared box, so a loop that walks a
    -- spine by projection stays linear.
    -- At the subject's last read the field is taken out of the payload
    -- instead ('::rustmorloc::MlcTake').
    , lcCtorField = \how ty n _ i subj -> case userArmFields userArms ty n of
      Just fs -> userCtorField how ty n fs i subj
      Nothing -> case how of
        FieldTake -> rustTakeField ty n i subj
        FieldBorrow -> parens (rustArmField ty n ("&" <> parens subj) ("&__morloc_lb." <> pretty i))
        FieldCopy -> rustArmField ty n ("&" <> parens subj) ("__morloc_lb." <> pretty i <> ".clone()")
    , lcEnumTagTest = \ty _ n _ subj ->
        parens (subj <+> "==" <+> "<" <> ty <> ">::" <> pretty n)
    , lcListLenTest = \subj n -> parens (parens subj <> ".len() ==" <+> pretty n)
    -- Indexing yields a place, not a value: an element read in place is a
    -- reference into it, and any other is cloned out, as a constructor field
    -- is.
    , lcListElem = \inPlace subj i ->
        if inPlace then rustElemRef subj (fromIntegral i) else parens subj <> "[" <> pretty i <> "].clone()"
    , lcCoerceOptional = \x -> "Some(" <> x <> ")"
    , lcTypeOf = \t -> Just . toIType <$> rustTypeOf t
    -- The serialize / raw-deserialize types use the WIRE form: a closure nested
    -- in an aggregate travels as its reified `ClosureOrigin` tuple, so the closure
    -- slot is typed as that tuple ('rustClosureWireLeaf'), not the native
    -- `impl Fn`/`Arc<dyn Fn>`. Mirrors the C++ 'cppClosureWireLeaf'.
    , lcSerialAstType = \s -> Just . toIType <$> rustRawType s
    , lcDeserialAstType = \s -> Just . toIType <$> rustTypeOf (shallowType s)
    , lcRawDeserialAstType = \s -> Just . toIType <$> rustRawType s
    , lcTypeMOf = \_ -> return Nothing
    , lcPackerName = \src -> pretty (srcName src)
    , lcUnpackerName = \src -> pretty (srcName src)
    -- A non-Copy arg to a pack/unpack function is borrowed to match the packer's
    -- `&T` parameter (a Copy arg is passed by value).
    , lcBorrowPackArg = \t -> not (rustIsCopy t)
    , lcRecordAccessor = \_ _ record field -> return (record <> "." <> field)
    , lcDeserialRecordAccessor = \i _ v -> v <> "." <> pretty i
    , lcTupleAccessor = \i v -> v <> "." <> pretty i
    , lcNewIndex = getCounter
    , lcPrintExpr = RP.printExpr
    , lcPrintStmt = RP.printStmt
    , lcEvalPattern = \t _ -> rustEvalPattern t
    , lcTakeGetter = rustTakeGetter
    , lcListConstructor = \_ _ _ es -> return ("vec![" <> hcat (punctuate ", " es) <> "]")
    , lcTupleConstructor = tupled
    , lcRecordConstructor = \recType _ _ _ rs -> do
        name <- rustStructCtor recType
        handles <- CMS.gets rsHandles
        return $ defaultValue {poolExpr = RP.rustRecordValue (isHandleType handles recType) name rs}
    -- A function value already IS its stored representation, so storing one
    -- is the identity; re-boxing it would cost an allocation and an
    -- indirection on every store, and lose its registry identity (STATE-4).
    , lcTakeSetter = rustTakeSetter
    , lcKeepContextArg = \_ _ _ _ v -> return v
    , lcKeepCallArg = \_ _ _ v -> return v
    , lcKeepClosureArg = \_ _ -> return Nothing
    , lcKeepSlot = \_ _ v -> return v
    , lcKeepElem = \_ v -> return v
    , lcStoreField = \_ v -> return v
    -- Apply a function value with call syntax: a concrete closure inlines, an
    -- `Arc<dyn Fn>` dispatches through its vtable.
    , lcApplyClosure = \callee args -> parens callee <> tupled args
    -- Partially apply a function value one argument at a time, each step
    -- typed explicitly so a borrowed head coerces to the trait object.
    , lcPapply = \t argTypes f xs -> case t of
        FunF rins out -> do
          argTs <- mapM rustStoredType argTypes
          restTs <- mapM (rustStoredType . typeMof) rins
          resT <- rustStoredType (typeMof out)
          let step ts acc x =
                "::rustmorloc::apply1_" <> pretty (length ts + length restTs)
                  <> "::<" <> hcat (punctuate ", " (ts ++ restTs ++ [resT])) <> ">"
                  <> tupled ["&" <> acc, x]
          return (papplySteps step argTs f xs)
        _ -> return f
    , lcForeignCall = \socketFile mid args ->
        let argList = "&[" <> hcat (punctuate ", " [a <+> "as *const u8" | a <- args]) <> "]"
         in [idoc|::rustmorloc::foreign_call(#{dquotes socketFile}, #{pretty mid}, #{argList})|]
    -- A remote call: args are already-serialized packet pointers (like a
    -- foreign call), so no put_value here. Resources fill the C `resources_t`
    -- with the C++ defaults (-1 for memory/time/cpus, 0 for gpus).
    , lcRemoteCall = \socketFile mid res args ->
        let (rmem, rtime, rcpu, rgpu) = remoteResourceInts res
            (resMem, resTime, resCPU, resGPU) = (pretty rmem, pretty rtime, pretty rcpu, pretty rgpu)
            argList = "&[" <> hcat (punctuate ", " [a <+> "as *const u8" | a <- args]) <> "]"
            call = "::rustmorloc::remote_call(" <> pretty mid <> ", " <> dquotes socketFile
                     <> ", \".morloc-cache\", " <> resMem <> ", " <> resTime <> ", "
                     <> resCPU <> ", " <> resGPU <> ", " <> argList <> ")"
         in return $ defaultValue {poolExpr = call}
    , lcCacheBody = rustCacheBody
    , lcDebugWrap = \_ _ body -> return body
    , lcMakeLet = rustMakeLet
    , lcReleaseStmt = \v -> "unsafe { ::rustmorloc::release_packet(" <> pretty v <> ", true) };"
    , lcReleaseBorrowedStmt = \v -> "unsafe { ::rustmorloc::release_packet(" <> pretty v <> ", false) };"
    , lcReturn = \e -> "return" <+> rustReply e <> ";"
    -- EMIT-3: the pop follows the result's construction (a move).
    , lcFrameReturn = \i _ _ e -> do
        st <- CMS.get
        return $ case rustFrameLine st i of
          Nothing -> Nothing
          Just _ -> Just (vsep ["let __morloc_r =" <+> rustReply e <> ";", "::rustmorloc::frame_pop();", "return __morloc_r;"])
    , lcDupPacket = \e -> "::rustmorloc::dup_packet(" <> e <> ")"
    , lcOwnedArg = \e -> "::rustmorloc::Packet::new(" <> e <> ").as_ptr()"
    , lcLoopBorrow = rustLoopBorrow
    , lcLoopLetRhs = rustConsumeLetRhs
    , lcLetRhs = rustConsumeLetRhs
    , lcOwnPacketDecl = \v e -> Just ("let" <+> v <+> "= ::rustmorloc::Packet::new(" <> e <> ");", v <> ".as_ptr()")
    , lcMakeIf = rustMakeIf
    , lcMakeIfLadder = rustMakeIfLadder
    , lcShortCircuit = \isAnd a b -> a <+> (if isAnd then "&&" else "||") <+> b
    , lcChainLadder = \isAnd flag first rungs ->
        let undecided = if isAnd then flag else "!" <> flag
            rung (stmts, e) =
              vsep ["if" <+> undecided <+> "{", indent 4 (vsep (stmts <> [flag <+> "=" <+> e <> ";"])), "}"]
         in vsep (("let mut" <+> flag <> ": bool =" <+> first <> ";") : map rung rungs)
    , lcMakeLoop = rustMakeLoop
    -- An effect thunk captures by value only when it can outlive the frame that
    -- builds it. The only such position is a manifold return typed
    -- a function value of no arguments ('rustReturnType'); every other renderer erases the
    -- effect row, so a closure reaching one is already a type error. A thunk in
    -- any other frame is forced on the spot or handed to @mlc_try@, which
    -- runs it before returning, so it borrows what it reads and
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
        "::rustmorloc::mlc_try" <> tupled
          [ thunk
          , "|mlcTryV|" <+> okWrap "mlcTryV"
          , "|mlcTryM|" <+> errWrap "mlcTryM"
          ]
    , lcSerialize = defaultSerialize (rustLowerConfig mask makers consumed userArms)
    , lcDeserialize = \_ -> defaultDeserialize (rustLowerConfig mask makers consumed userArms)
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
          return $ "::rustmorloc::require_origin(::rustmorloc::reify(&" <> parens v <> "))"
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
    , lcManifoldName = RP.rustManifoldName
    , lcMakePass = \m sig sigType mname params -> rustClosureWrapper m sig sigType mname [] params
    , lcMakeLambda = rustClosureWrapper
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

-- | A projection in the pool plan's takes takes its field like a last
-- read does ('rustTakeField'). Every other projection borrows the subject and
-- clones the field.
rustConsumeLetRhs :: Int -> NativeExpr -> PoolDocs -> RustM PoolDocs
rustConsumeLetRhs i rhs@(IntrinsicN _ IntrCtorField _ [LetVarN t@(VariantF {}) subj _, StrN _ n, IntN _ k]) d = do
  pp <- CMS.gets rsPool
  root <- CMS.gets rsCurrentRoot
  borrowed <- asks oeCurrent
  if letMode pp root i rhs == LetTake && not (Set.member subj borrowed)
    then do
      ty <- rustTypeOf t
      userArms <- CMS.gets rsUserArms
      let subj' = (nvarNamer rustLocals) subj
      return d
        { poolExpr = case userArmFields userArms ty n of
            Just fs -> userCtorField FieldTake ty n fs (fromIntegral k) subj'
            Nothing -> rustTakeField ty n (fromIntegral k) subj'
        }
    else return d
rustConsumeLetRhs _ _ d = return d

-- | A loop local 'loopBorrowLocals' found is a reference into what the loop
-- borrowed (OWN-6), so it joins the manifold's borrowed set; its expression
-- is rendered as that reference.
rustLoopBorrow :: Int -> NativeExpr -> RustM (Maybe MDoc)
rustLoopBorrow i e = do
  borrowed <- asks oeCurrent
  if not (Set.member i borrowed)
    then return Nothing
    else case borrowPartOf (const True) (const True) e of
      Just part -> Just <$> renderPart part
      Nothing -> error ("rustLoopBorrow: a borrowed loop local bound to " <> show e)
  where
    renderPart part = case part of
      BorrowVar j -> return ((nvarNamer rustLocals) j)
      BorrowCtorField t p n k -> do
        ty <- rustTypeOf t
        p' <- renderPart p
        userArms <- CMS.gets rsUserArms
        return $ case userArmFields userArms ty n of
          Just fs -> userCtorField FieldBorrow ty n fs (fromIntegral k) ("*" <> p')
          Nothing -> rustArmField ty n p' ("&__morloc_lb." <> pretty k)
      BorrowField j path -> return ("&" <> writeSelectorRust ((nvarNamer rustLocals) j) path)
      BorrowElem p k -> (`rustElemRef` k) <$> renderPart p
      BorrowIf c a b -> do
        c' <- renderTest c
        a' <- renderPart a
        b' <- renderPart b
        return ("if" <+> c' <+> "{" <+> a' <+> "} else {" <+> b' <+> "}")
      BorrowLet j rhs body -> do
        rhs' <- renderPart rhs
        body' <- renderPart body
        return ("{ let" <+> (nvarNamer rustLocals) j <+> "=" <+> rhs' <> ";" <+> body' <+> "}")
      BorrowFail t msg -> do
        ty <- rustTypeOf t
        return ("::rustmorloc::morloc_throw_as::<&" <> ty <> ">(" <> RP.printExpr (IStrLit Nothing msg) <> ")")
    renderTest c = case c of
      TestTag t p n -> do
        ty <- rustTypeOf t
        p' <- renderPart p
        return ("matches!(" <> p' <> "," <+> RP.typeHead ty <> "::" <> pretty n <+> "{ .. })")
      TestAnd x y -> do
        x' <- renderTest x
        y' <- renderTest y
        return (parens (x' <+> "&&" <+> y'))

-- | A reference to element @k@ of a list.
rustElemRef :: MDoc -> Integer -> MDoc
rustElemRef subj k = parens ("&" <> parens subj <> "[" <> pretty k <> "]")

-- | The fields of arm @n@ of a user-written `data` type at the given
-- rendered type, if it is one.
userArmFields :: Map.Map Text [(Text, [RP.UserField])] -> MDoc -> Text -> Maybe [RP.UserField]
userArmFields userArms ty n = Map.lookup (render ty) userArms >>= lookup n

-- | Read field @i@ of arm @n@ of a user-written `data` value (a tuple
-- variant): borrowed, copied, or taken (OWN-5), each at morloc's type of
-- the field. An optional field held through a pointer has no value of
-- morloc's type to borrow, so its borrow is a copy.
userCtorField :: FieldRead -> MDoc -> Text -> [RP.UserField] -> Int -> MDoc -> MDoc
userCtorField how ty n fs i subj = case how of
  FieldBorrow -> case field of
    (_, False) -> parens (arm ("&" <> parens subj) (RP.userFieldRef field "__morloc_lf"))
    (_, True) -> "&" <> parens (arm ("&" <> parens subj) (RP.userFieldCopy field "__morloc_lf"))
  FieldCopy -> arm ("&" <> parens subj) (RP.userFieldCopy field "__morloc_lf")
  FieldTake ->
    "{ let __morloc_ls = ::std::mem::ManuallyDrop::new(" <> subj <> "); match &*__morloc_ls {"
      <+> RP.userArmPattern (RP.typeHead ty) n all_
      <+> "=> {" <+> hsep ["let" <+> v <+> "= unsafe { ::std::ptr::read(" <> v <> ") };" | v <- all_]
      <+> RP.userFieldTake field ("__morloc_lf" <> pretty i) <+> "},"
      <+> "#[allow(unreachable_patterns)] _ => unreachable!()"
      <+> "} }"
  where
    field = case drop i fs of
      (f : _) -> f
      [] -> error ("userCtorField: arm " <> T.unpack n <> " has no field " <> show i)
    all_ = ["__morloc_lf" <> pretty j | (j, _) <- indexed fs]
    one = [if j == i then "__morloc_lf" else "_" | (j, _) <- indexed fs]
    arm s' yield =
      "match" <+> s' <+> "{"
        <+> RP.userArmPattern (RP.typeHead ty) n one <+> "=>" <+> yield <> ","
        <+> "#[allow(unreachable_patterns)] _ => unreachable!()"
        <+> "}"

-- | Match a subject already known to be in arm @n@ and yield an expression
-- over its payload, @mlc_b@.
rustArmField :: MDoc -> Text -> MDoc -> MDoc -> MDoc
rustArmField ty n subj yield =
  "match" <+> subj <+> "{"
    <+> RP.typeHead ty <> "::" <> pretty n <> "(__morloc_lb)"
    <+> "=>" <+> yield <> ","
    <+> "#[allow(unreachable_patterns)] _ => unreachable!()"
    <+> "}"

-- | Take field @i@ out of arm @n@'s payload (OWN-5): the box gives the
-- field up when it holds the only handle, and the field is cloned otherwise
-- ('::rustmorloc::MlcTake'). An arm's one field is its box, so reading the box
-- out and forgetting the shell moves the whole value.
rustTakeField :: MDoc -> Text -> Int -> MDoc -> MDoc
rustTakeField ty n i subj =
  "{ let __morloc_ls = ::std::mem::ManuallyDrop::new(" <> subj <> "); match &*__morloc_ls {"
    <+> RP.typeHead ty <> "::" <> pretty n <> "(__morloc_lb)"
    <+> "=> ::rustmorloc::MlcTake::take_with(unsafe { ::std::ptr::read(__morloc_lb) }, |__morloc_lv| __morloc_lv." <> pretty i
    <> ", |__morloc_lv| __morloc_lv." <> pretty i <> ".clone()),"
    <+> "#[allow(unreachable_patterns)] _ => unreachable!()"
    <+> "} }"

-- | A native let of the IR that holds a borrow (OWN-6) is typed as a
-- reference; every other let binds the value its right-hand side was adapted
-- to upstream.
rustMakeLet :: (Int -> MDoc) -> Int -> Maybe TypeF -> Maybe NativeExpr -> PoolDocs -> PoolDocs -> RustM PoolDocs
rustMakeLet namer letIndex mt mrhs p1 p2 = do
  annDoc <- rustLetAnnotation letIndex mt (isJust mrhs)
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
      , poolBorrowedLocal = Nothing
      , poolChain = Nothing
      }

-- | The type annotation of let @letIndex@ of type @mt@ (Nothing: a serial
-- packet), given whether it binds a native right-hand side.
rustLetAnnotation :: Int -> Maybe TypeF -> Bool -> RustM MDoc
rustLetAnnotation letIndex mt native = do
  borrowed <- asks oeCurrent
  case mt of
    Just t | isFunctionTypeF t -> return ""
    Just t
      | native
      , Set.member letIndex borrowed -> do ts <- rustTypeOf t; return (":" <+> "&" <> ts)
    Just t -> do ts <- rustTypeOf t; return (":" <+> ts)
    Nothing -> return (":" <+> "*mut u8")

-- | Native tail-loop assembly. Walks the 'LoopBody' decision tree into a Rust
-- @loop { ... }@ expression bound to the serial result: guards render to
-- @if/else@, native/serial lets to @let@ locals, a base leaf to @break <base>;@
-- (the loop expression's value), and a continue leaf to @let@ temps for the
-- new value of each slot it changes, followed by reassignment of those slots
-- ('changedSlots'). Every temp is computed before any reassignment; which
-- reads move is OWN-2's, and which locals are borrows is OWN-6's.
rustMakeLoop :: LoopResult -> [(Int, Maybe PoolDocs)] -> LoopBody LowN LowS PoolDocs -> RustM PoolDocs
rustMakeLoop res carried body = do
  resultIdx <- getCounter
  resT <- case res of
    LoopResultSerial -> return "*mut u8"
    LoopResultNative t -> rustTypeOf t
  let resultVar = (helperNamer rustLocals) resultIdx
  bodyLines <- walk body
  -- A native loop's locals are its own, started from their initializers.
  let initLines = concat [poolPriorLines d <> ["let mut" <+> (nvarNamer rustLocals) i <+> "=" <+> poolExpr d <> ";"] | (i, Just d) <- carried]
      loopExpr = vsep ["loop {", indent 4 (vsep bodyLines), "}"]
      resultDecl = "let" <+> resultVar <> ":" <+> resT <+> "=" <+> loopExpr <> ";"
      leaves = loopBodyDocs body <> [d | (_, Just d) <- carried]
  return $ PoolDocs
    { poolCompleteManifolds = concatMap poolCompleteManifolds leaves
    , poolExpr = resultVar
    , poolPriorLines = initLines <> [resultDecl]
    , poolPriorExprs = concatMap poolPriorExprs leaves
    , poolReturnFlag = True
    , poolBorrowedLocal = Nothing
    , poolChain = Nothing
    }
  where
    walk (LoopBase seDocs) =
      return $ poolPriorLines seDocs <> ["break" <+> poolExpr seDocs <> ";"]
    -- A slot the continue leaves as it is gets no temporary.
    walk (LoopContinue xs) = do
      let changed = [(i, v) | (i, v, _) <- changedSlots [(i, o, d) | (i, Low o d) <- xs]]
      tmpVars <- map (helperNamer rustLocals) <$> mapM (const getCounter) changed
      let priors = concatMap (poolPriorLines . snd) changed
          tmpAssigns = zipWithSame (\tv (_, cd) -> "let" <+> tv <+> "=" <+> poolExpr cd <> ";") tmpVars changed
          reassigns = zipWithSame (\(i, _) tv -> (nvarNamer rustLocals) i <+> "=" <+> tv <> ";") changed tmpVars
      return $ priors <> tmpAssigns <> reassigns
    walk (LoopNLet i (Low r neDocs) b) = do
      ann <- rustLetAnnotation i (Just (typeFof r)) True
      rest <- walk b
      return $ poolPriorLines neDocs <> ["let" <+> (nvarNamer rustLocals) i <> ann <+> "=" <+> poolExpr neDocs <> ";"] <> rest
    walk (LoopSLet i (Low _ seDocs) b) = do
      rest <- walk b
      return $ poolPriorLines seDocs <> ["let" <+> (svarNamer rustLocals) i <+> "=" <+> poolExpr seDocs <> ";"] <> rest
    walk (LoopIf (Low _ guardDocs) t e) = do
      tLines <- walk t
      eLines <- walk e
      -- Every branch ends the iteration, so the else branch follows the
      -- guarded one rather than nesting in it (EMIT-5).
      let next = ["continue;" | not (loopBodyBreaks t)]
          ifBlock = vsep ["if" <+> poolExpr guardDocs <+> "{", indent 4 (vsep (tLines <> next)), "}"]
      return $ poolPriorLines guardDocs <> [ifBlock] <> eLines

-- | A chain of guards as one flat block (EMIT-5): a labeled block that
-- breaks out with the first arm whose test holds.
rustMakeIfLadder :: NativeExpr -> Guards -> RustM (MDoc, MDoc)
rustMakeIfLadder origExpr (Guards (c1, _, t1) rungs (_, eD)) = do
  idx <- getCounter
  let v = (helperNamer rustLocals) idx
      label = "'mlc_g" <> pretty idx
  typeStr <- rustTypeOf (typeFof origExpr)
  let taken c d = vsep ["if" <+> parens c <+> "{", indent 4 (vsep (poolPriorLines d ++ ["break" <+> label <+> poolExpr d <> ";"])), "}"]
      body =
        [taken c1 t1]
          <> concat [pre <> [taken c d] | (pre, c, _, d) <- toList rungs]
          <> poolPriorLines eD
          <> [poolExpr eD]
  return (v, vsep ["let" <+> v <> ":" <+> typeStr <+> "=" <+> label <> ": {", indent 4 (vsep body), "};"])

-- | Native @if@ expression, bound to a fresh temp so it composes as a value.
-- The arms are already adapted to the owned result type by the shared 'IfN_'
-- lowering (via 'adaptOwnedElem'), so they splice directly into the owned let.
rustMakeIf :: NativeExpr -> PoolDocs -> PoolDocs -> PoolDocs -> RustM PoolDocs
rustMakeIf origExpr condDocs thenDocs elseDocs = do
  idx <- getCounter
  let v = (helperNamer rustLocals) idx
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
      , poolBorrowedLocal = Nothing
      , poolChain = Nothing
      }

-- | Emit a manifold function definition (@unsafe fn m{i}(..) -> ..@).
-- Deduplicated per (local/remote) so a manifold visited from multiple sites is
-- defined once, matching the C++ member.
-- | The trace line, as a Rust literal, of a manifold that records its call on
-- the frame stack (STATE-5): a call of a named function, labeled with its
-- definition (DIAG-5).
rustFrameLine :: RustState -> Int -> Maybe MDoc
rustFrameLine st callIndex = case rsDebugInfo st callIndex of
  Nothing -> Nothing
  Just (userName, srclocStr) ->
    let nameOut = if T.null userName then "_" else userName
        srclocSuffix = if T.null srclocStr then "" else ", " <> srclocStr
        frameText =
          "\n  at " <> nameOut <> " [rust] (mid=" <> T.pack (show callIndex) <> srclocSuffix <> ")"
     in Just (dquotes (pretty (RP.rustEscape frameText)))

rustMakeFunction ::
  Int -> MDoc -> [Arg TypeM] -> TypeM -> [MDoc] -> MDoc -> Maybe HeadManifoldForm -> Bool -> RustM (Maybe MDoc)
rustMakeFunction callIndex mname args manifoldType priorLines body headForm isClosure = do
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
      -- and a captured function value is an `Arc` clone, not a borrow.
      retStr <- rustReturnType manifoldType
      kept <- CMS.gets (Map.findWithDefault Set.empty callIndex . ppByValue . rsPool)
      typedArgs <-
        mapIndexedM
          (\j a@(Arg _ t) -> if Set.member j kept then (\ts -> (argNamer rustLocals) a <> ":" <+> ts) <$> rustArgType t else rustArgOf a)
          args
      let fullName = mname <> if isRemote then "_remote" else ""
          decl = "unsafe fn" <+> fullName <> tupled typedArgs <+> "->" <+> retStr
          -- STATE-5: a named function records its call; the pop is in its
          -- return statement ('lcFrameReturn'), which follows any sourced
          -- call. A function that records nothing and calls sourced code
          -- keeps a frame at the call (PANIC-15).
          frameStmts = case rustFrameLine st callIndex of
            Nothing -> []
            Just lit -> ["::rustmorloc::frame_push(" <> lit <> ");"]
          callFrame
            | null frameStmts && T.any (== userCallMark) (render (vsep (priorLines ++ [body]))) =
                ["let __morloc_call = ::rustmorloc::CallFrame;"]
            | otherwise = []
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
                    "let __morloc_log = ::rustmorloc::LogGuard::new("
                      <> hsep (punctuate ","
                           [ dquotes (pretty (RP.rustEscape (renderedGroup tmpl)))
                           , litOrEmpty (renderedStart tmpl)
                           , litOrEmpty (renderedPass tmpl)
                           , litOrEmpty (renderedFail tmpl)
                           , litOrEmpty (renderedBenchKey tmpl)
                           ])
                      <> ");"
               in guardStmt : priorLines ++ [body]
      -- STATE-6: a closure, which code outside morloc may call and whose
      -- panic it may catch, restores the depth it found when a panic
      -- leaves it.
      let inner = frameStmts ++ callFrame ++ bodyLines
          fnBody
            | isClosure =
                [ "let __morloc_saved = ::rustmorloc::frame_enter();"
                , "match ::std::panic::catch_unwind(::std::panic::AssertUnwindSafe(||" <+> "{"
                , indent 4 (vsep inner)
                , "})) {"
                , indent 4 (vsep ["Ok(v) => v,", "Err(p) => ::rustmorloc::closure_rethrow(__morloc_saved, p),"])
                , "}"
                ]
            | otherwise = inner
      return . Just $
        vsep [decl <+> "{", indent 4 (vsep fnBody), "}"]

-- | Whether a closure wire form has a closure among its arguments or as its
-- result.
