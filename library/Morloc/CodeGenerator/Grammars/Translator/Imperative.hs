{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.Imperative
Description : Imperative IR for two-phase translation
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Provides an imperative IR for code generation. The fold from the compiler's
SerialManifold/NativeExpr AST to IR is written once; per-language printers
convert IR to source code.
-}
module Morloc.CodeGenerator.Grammars.Translator.Imperative
  ( -- * IR types
    IStmt (..)
  , IExpr (..)
  , IParam (..)
  , IType (..)
  , IAccessor (..)
  , IFunMeta (..)
  , IProgram (..)

    -- * IType rendering and conversion
  , renderIType
  , renderITypeText
  , toIType

    -- * Program construction
  , buildProgram
  , buildProgramM
  , papplySteps

    -- * Lowering: serialize/deserialize expansion
  , expandSerialize
  , expandDeserialize
  , containsClosure
  , nativeToWire
  , wireToNative

    -- * Expression lowering
  , lowerSerialExpr
  , lowerNativeExpr

    -- * Manifold lowering
  , lowerSerialManifold
  , lowerNativeManifold
  , defaultFoldRules

    -- * Full lowering config
  , LowerConfig (..)
  , infixOperator
  , LoopResult (..)
  , ArgSite (..)
  , IOwnership (..)
  , FieldRead (..)

    -- * Ownership of a projected value
  , consumableProjectionLets
  , markLastReads
  , markLastReadsWith
  , markLastReadsSeeded
  , lastBuilds
  , markLastReadsInBody
  , markLastReadsInNative
  , namedIn
  , loopBorrowLocals
  , BorrowPart (..)
  , BorrowTest (..)
  , borrowPartOf
  , consumedParams
  , nativeManifoldsUnder
  , RNode (..)
  , rnodeChildren
  , rnodeLet
  , aliasChain
  , isLastRead
  , isBorrowableProjection
  , borrowableRoot

    -- * Default serialize/deserialize (for Python/R)
  , defaultSerialize
  , defaultDeserialize

    -- * Re-exported type alias
  , IndexM
  ) where

import Control.Monad.Identity (Identity)
import qualified Control.Monad.State as CMS
import Data.Binary (Binary)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Morloc.BaseTypes as BT
import Morloc.CodeGenerator.Grammars.Common
  ( DispatchEntry (..)
  , PoolDocs (..)
  , argNamer
  , collectManifoldIds
  , extractLocalDispatch
  , extractRemoteDispatch
  , helperNamer
  , isForeignCalleeForm
  , manNamer
  , mergePoolDocs
  , nvarNamer
  , provideClosure
  , serialClosuresOf
  , StageEntry (..)
  , svarNamer
  )
import Morloc.CodeGenerator.LogTemplate (RenderedTemplate (..))
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Serial (containsFunF, isSerializable, serialAstHasString, serialAstToMsgpackSchema, serialAstToType)
import Morloc.Data.Doc
import Morloc.Monad (IndexState)

-- Statements
data IStmt
  = IFunDef IFunMeta [IParam] [IStmt] IExpr
  | IAssign Text (Maybe IType) IExpr
  | -- | resultVar, resultType, iterVar, collection, bodyStmts, yieldExpr
    -- Python/C++: resultVar = []; for iterVar in collection: bodyStmts; resultVar.append(yieldExpr)
    -- R: resultVar <- lapply(collection, function(iterVar) { bodyStmts; yieldExpr })
    -- resultType is used by C++ for typed declarations; Python/R pass Nothing
    IMapList Text (Maybe IType) Text IExpr [IStmt] IExpr
  | -- | resultVar, resultType, condition, thenStmts, thenExpr, elseStmts, elseExpr
    -- Semantics: declare resultVar; if cond { thenStmts; resultVar = thenExpr } else { elseStmts; resultVar = elseExpr }
    -- For elif chains, elseStmts contains another IIf and elseExpr is unused (IVar resultVar)
    IIf Text (Maybe IType) IExpr [IStmt] IExpr [IStmt] IExpr
  | -- | resultVar, resultType (optional<final>), source (optional<raw>),
    -- unwrappedVar, unwrappedType (raw), bodyStmts, bodyExpr.
    -- Lifts an inner transformation through an Optional. If source is
    -- null, resultVar is null; otherwise unwrappedVar is bound to the
    -- source's contents, bodyStmts run, and resultVar is set to a
    -- wrapped bodyExpr. Used by SerialOptional in expand{Ser,Deser}ialize
    -- when the inner SerialAST itself requires statement-level construction.
    IIfNotNull Text (Maybe IType) IExpr Text (Maybe IType) [IStmt] IExpr
  | IReturn IExpr
  | IExprStmt IExpr

-- Expressions
data IExpr
  = ICall Text (Maybe [IType]) [[IExpr]]
  | IVar Text
  | IBoolLit Bool
  | IIntLit (Maybe Text) Integer  -- concrete type name (e.g. "int64_t"), Nothing for default
  | IRealLit (Maybe Text) RealLit  -- concrete type name (e.g. "float"), Nothing for default
  | IStrLit (Maybe Text) Text  -- concrete type name (e.g. "bytes"), Nothing for default
  | INullLit (Maybe IType)
  | -- | The one value of @Unit@. Distinct from 'INullLit', which is
    -- the absent value of an optional: a language whose unit and
    -- none are different expressions (Rust's @()@ and
    -- @Option::None@) cannot spell one with the other.
    IUnitLit
  | IListLit [IExpr]
  | ITupleLit [IExpr]
  | IRecordLit NamType FVar [(Key, IExpr)]
  | IAccess IExpr IAccessor
  | ISerCall Int IExpr -- put_value(schemaId, expr)
  -- get_value[<T>](schemaId, expr); type used by C++ template. The Bool says
  -- whether the deserialized type carries a native string anywhere inside it,
  -- which is what a receiving language that cannot hold an interior NUL needs
  -- to know. It is carried here rather than decided in the printer because
  -- only this layer still has the SerialAST.
  | IDesCall Int (Maybe IType) Bool IExpr
  | IForeignCall Text Int [IExpr]
  | IRemoteCall Text Int RemoteResources [IExpr]
  | ILambda [Text] IExpr
  | IPack Text IExpr -- packer(expr)
  | IRawExpr Text
  | IEval IExpr -- eval: call effect with no args
  | IIntrinsicSave Text Int IExpr IExpr IExpr -- format, schemaId, level, data, path
  | IIntrinsicLoad Int (Maybe IType) IExpr -- schemaId, returnType, path -> result (nullable)
  | IIntrinsicHash Int IExpr -- schemaId, data -> hex string
  | IIntrinsicShow Int IExpr -- schemaId, data -> JSON string
  | IIntrinsicRead Int (Maybe IType) IExpr -- schemaId, returnType, json_string -> typed data (nullable)
  | IIntrinsicUnpack Int (Maybe IType) IExpr -- schemaId, returnType, packet bytes -> typed data
  | IIntrinsicOpen Word8 IExpr -- kind byte (IFile=0, IStream=1, OStream=2), path -> handle
  | IIntrinsicClose IExpr -- handle -> ()
  | IIntrinsicUnlinkTemp IExpr
      -- ^ @close given a Str path: unlink a registered temp file (created
      --   by @tmpfile) and drop it from the registry. Errors if the path
      --   was not registered -- @close is not a general file-removal tool.
  | IIntrinsicFSchema IExpr -- path -> schema string
  | IIntrinsicFLength IExpr
      -- ^ handle -> Int element count (read from cached StreamDiag)
  | IIntrinsicStreamLayout Int (Maybe IType) IExpr
      -- ^ @streamLayout on an IFile: schemaId of the [(U64,U64,U64)] result,
      --   return type (for C++ template), handle expression. Per-language
      --   wrappers call mlc_stream_layout and deserialise via
      --   from_voidstar<List<Tuple3<...>>>.
  | IIntrinsicIFileWalk Int (Maybe IType) IExpr IExpr [IExpr]
      -- ^ Unified IFile pattern walker.
      --   schemaId of the result type, return type (for C++ template),
      --   path string expression, handle expression, list of runtime
      --   args (bracket bounds, in DFS order). The walker resolves
      --   the path against the file's value schema; each bracket step
      --   consumes args (1 for `.[]`, 3 for `.[:]`). Per-language
      --   wrappers marshal the runtime args (each an
      --   @Optional<Int64>@-shaped pair) into the C ABI.
  | IIntrinsicNext Int (Maybe IType) IExpr
      -- ^ @next on an IStream: schemaId of the [a] result, return type
      --   (for C++ template), handle expression. Per-language wrappers
      --   call mlc_next and deserialise via from_voidstar<List<T>>.
  | IIntrinsicStream IExpr
      -- ^ @stream :: IFile [a] -> <IO> IStream a: derive an IStream
      --   handle from an open IFile handle (independent fd/mmap/cursor).
  | IIntrinsicOpenOStream Int IExpr
      -- ^ @open :: <IO> (OStream a): schemaId of the list-of-element
      --   `[a]` (streams are list-shaped) and the path expression. The
      --   runtime creates the file and writes the stream header with
      --   the schema metadata block.
  | IIntrinsicOpenIStream Int IExpr
      -- ^ @open :: <IO> (IStream a): schemaId of the list-of-element
      --   `[a]` and the path expression. The declared schema lets the
      --   runtime open the stdin sentinel (routing to the nexus stdin
      --   channel and guarding the incoming stream schema); for a real
      --   file the schema is still read from disk.
  | IIntrinsicWrite Int IExpr IExpr IExpr
      -- ^ @write: schemaId of the [a] value type, level, value, handle.
  | IIntrinsicAppend Int IExpr
      -- ^ @append :: Str -> <IO> (OStream a): schemaId of `[a]`, path.
  | IIntrinsicConcat IExpr IExpr
      -- ^ @concat :: [Str] -> Str -> <IO> (): paths expr, dest expr.
  | IIntrinsicChannel Int
      -- ^ internal @channel: schemaId of the stream's `[a]`.
  | IIntrinsicSpawn Int IExpr IExpr
      -- ^ internal @spawn: schemaId of the producer's OStream argument,
      --   the channel handle, the producer.
  | IIntrinsicSettle IExpr
      -- ^ internal @settle: the channel handle.
  | IIntrinsicFlush IExpr
      -- ^ @flush :: OStream a -> <IO> (): force any buffered elements
      --   to be emitted as a sub-packet. Single-argument: handle expr.
  | IIntrinsicTell
      -- ^ @tell :: <IO> U64: nullary. Number of elements written to the
      --   process's @stdout OStream so far (its element_count).
  | IIntrinsicCellNew Int IExpr
      -- ^ @cellnew: schemaId of the accumulator type, seed expression.
  | IIntrinsicCellGet Int (Maybe IType) IExpr
      -- ^ @cellget: schemaId, accumulator type, handle. The type appears
      --   nowhere but the return, so a statically typed member needs it
      --   given rather than deduced (as for @load).
  | IIntrinsicCellPut Int IExpr IExpr
      -- ^ @cellput: schemaId, handle, value.
  | IIntrinsicCellReduce Int (Maybe IType) IExpr IExpr
  | IIntrinsicReplay Int (Maybe IType) IExpr IExpr
      -- ^ @replay: schemaId of one frame (the list `[a]`), element type
      --   `a`, stream handle, function. The per-language helper reads the
      --   stream to its end and calls the function on each frame.
      -- ^ @cellreduce: schemaId, accumulator type, combine function,
      --   handle. The combine is an ordinary native callable in the pool
      --   the reduce was realized in; the per-language helper applies it
      --   over the accumulators and releases the cell.
  | IIntrinsicTmpfile
      -- ^ @tmpfile :: <IO> Str: nullary. Create a fresh empty file in the
      --   morloc tmpdir, register it for removal at end-of-call, and return
      --   its path. Used by the whole-form with:/render: gather.
  | IIntrinsicStdin Int
      -- ^ @stdin :: <IO> IStream a: nullary. schemaId of `[a]`.
  | IIntrinsicStdout Int
      -- ^ @stdout :: <IO> OStream a: nullary. schemaId of `[a]`.
  | IIntrinsicStderr Int
      -- ^ @stderr :: <IO> OStream a: nullary. schemaId of `[a]`.
  | IIntrinsicThrow (Maybe IType) IExpr
      -- ^ @throw :: e -> a: abandon the computation with this message. The
      -- result type @a@ appears nowhere but the return, so a statically
      -- typed member that spells the raise as a value needs it given.

data IParam = IParam Text (Maybe IType)

{- | Structured type representation for the IR.
Carries enough information for any language's printer to render typed declarations.
-}
data IType
  = -- | Primitive type: "int", "double", "std::string", "bool", etc.
    ITyPrim Text
  | -- | List/vector type
    ITyList IType
  | -- | Tuple type
    ITyTuple [IType]
  | -- | Record: name, type params, fields
    ITyRecord Text [IType] [(Key, IType)]
  | -- | Function type
    ITyFunction [IType] IType
  | -- | Unit/void type
    ITyUnit
  | -- | Named type with parameters (e.g., Map k v)
    ITyNamed Text [IType]
  | -- | Serialized data (e.g., const uint8_t* in C++)
    ITySerial
  | -- | Optional type (e.g., std::optional<T> in C++)
    ITyOptional IType
  | -- | Type not known or not needed (Python, R)
    ITyUnknown
  deriving (Show, Eq, Ord, Generic)

instance Binary IType

{- | Render an IType to an MDoc for use in code generation output.
This is used by printers that need the type as rendered text.
-}
renderIType :: IType -> MDoc
renderIType (ITyPrim t) = pretty t
renderIType (ITyList t) = "std::vector<" <> renderIType t <> ">"
renderIType (ITyTuple ts) = "std::tuple<" <> hcat (punctuate ", " (map renderIType ts)) <> ">"
renderIType (ITyRecord name [] _) = pretty name
renderIType (ITyRecord name params _) = pretty name <> encloseSep "<" ">" "," (map renderIType params)
renderIType (ITyFunction args ret) = "std::function<" <> renderIType ret <> tupled (map renderIType args) <> ">"
renderIType ITyUnit = "void"
renderIType (ITyNamed name []) = pretty name
renderIType (ITyNamed name params) = pretty name <> encloseSep "<" ">" "," (map renderIType params)
renderIType (ITyOptional t) = "std::optional<" <> renderIType t <> ">"
renderIType ITySerial = "const uint8_t*"
renderIType ITyUnknown = "auto"

-- | Render an IType to Text (for macro expansion, etc.)
renderITypeText :: IType -> Text
renderITypeText = render . renderIType

{- | Convert a rendered MDoc type to an opaque IType.
This is a transitional bridge: preserves the rendered form as ITyNamed.
C++ currently produces rendered MDoc types; this wraps them for the new IR.
-}
toIType :: MDoc -> IType
toIType d = ITyNamed (render d) []

data IAccessor
  = IIdx Int
  | IKey Key
  | IField Text

data IFunMeta = IFunMeta
  { ifName :: Text
  , ifReturnType :: Maybe IType
  , ifHeadForm :: Maybe HeadManifoldForm
  }

data IProgram = IProgram
  { ipSources :: [Text]
    -- ^ Module-top / parent-load source lines: the language preamble (runtime
    -- bootstrap) for the interpreted pools, or the includes for C++/Rust.
  , ipIncludes :: [Text]
    -- ^ User `source` imports, kept separate so interpreted pools can defer them
    -- past the worker fork (importing thread-spinning libraries in the
    -- coordinator parent and then forking is unsafe on macOS). Empty for the
    -- C++/Rust members, whose includes live in 'ipSources'.
  , ipManifolds :: [Text]
  , ipLocalDispatch :: [DispatchEntry]
  , ipRemoteDispatch :: [DispatchEntry]
  , ipSchemaTable :: [Text]  -- ordered list of schema strings; index = schema ID
  , ipLogTemplates :: Map.Map Int RenderedTemplate
    -- ^ Per-labeled-midx pre-rendered log templates. See 'LogTemplate'.
  , ipClosureTable :: Map.Map Int ([Text], [Text], Text)
    -- ^ Per-closure-mid wire schemas: (captured-arg schemas, bound-arg schemas,
    -- result schema). Drives the home-pool serial dispatch wrapper and the reify
    -- path for defunctionalized closures that cross a language boundary. Empty
    -- for pools that produce no crossing closures.
  , ipStageTable :: Map.Map Int StageEntry
    -- ^ The program's stage table, the same in each pool, read by its
    -- @mlc_papply@.
  }
  deriving (Generic)

instance Binary IProgram

-- | Build an IProgram from pre-rendered sources and manifolds (pure, for Python/R).
--
-- Foreign-callee midxes are stripped from the templates map: a cross-pool
-- labeled call exists once in each pool, and the caller's wrap already
-- measures the full call (socket round-trip included). Wrapping the callee
-- would double-count.
buildProgram ::
  Map.Map Int Text ->
  Map.Map Int RenderedTemplate ->
  [MDoc] ->  -- module-top sources (preamble for py/r, includes for cpp/rust)
  [MDoc] ->  -- deferred user includes (py/r); empty for cpp/rust
  [MDoc] ->
  [SerialManifold] ->
  [Text] ->
  Map.Map Int ([Text], [Text], Text) ->
  IProgram
buildProgram labels templates sources includes manifolds es schemas closureTable =
  let definedIds = collectManifoldIds es
      foreignCalleeIds =
        Set.fromList [i | SerialManifold i _ _ f _ <- es, isForeignCalleeForm (Just f)]
      labels' = Map.restrictKeys labels definedIds
                  `Map.withoutKeys` foreignCalleeIds
      templates' = Map.restrictKeys templates definedIds
                     `Map.withoutKeys` foreignCalleeIds
   in IProgram
        { ipSources = map render sources
        , ipIncludes = map render includes
        , ipManifolds = map render manifolds
        , ipLocalDispatch = extractLocalDispatch labels' es
        , ipRemoteDispatch = extractRemoteDispatch labels' es
        , ipSchemaTable = schemas
        , ipLogTemplates = templates'
        , ipClosureTable = closureTable
        , ipStageTable = Map.empty
        }

-- | Build an IProgram monadically (for C++ where translateSegment runs in a monad).
-- | A partial application written one argument at a time: @step ts acc x@
-- applies @acc@, a function of the argument types @ts@ (then the rest), to
-- @x@, its first argument.
papplySteps :: ([MDoc] -> MDoc -> MDoc -> MDoc) -> [MDoc] -> MDoc -> [MDoc] -> MDoc
papplySteps step = go
  where
    go ts@(_ : rest) acc (x : xs) = go rest (step ts acc x) xs
    go _ acc _ = acc

buildProgramM ::
  (Monad m) =>
  Map.Map Int Text ->
  Map.Map Int RenderedTemplate ->
  [MDoc] ->  -- module-top sources
  [MDoc] ->  -- deferred user includes (empty for cpp/rust)
  [SerialManifold] ->
  (SerialManifold -> m MDoc) ->
  m [Text] ->
  Map.Map Int ([Text], [Text], Text) ->
  m IProgram
buildProgramM labels templates sources includes es translateSeg getSchemas closureTable = do
  manifolds <- mapM translateSeg es
  schemas <- getSchemas
  return $ buildProgram labels templates sources includes manifolds es schemas closureTable

-- | Which kind of call an argument is being passed into. Members that vary
-- argument passing by call kind (currently only Rust) branch on this; the
-- others ignore it. Every argument-emission site names its kind, so a new
-- call-emitting path is forced to choose one rather than defaulting silently.
data ArgSite
  = SourcedArg Source Int
  -- ^ i-th argument of a sourced foreign call (carries the declared signature
  -- so a member can, e.g., pass a type-variable parameter by reference)
  | ManifoldArg Int Int
  -- ^ argument of a generated manifold call: the callee and the position of
  -- its parameter
  | ClosureArg
  -- ^ argument of a native closure application (a function-valued variable)

-- | The ownership form a lowered native expression evaluates to. Members that
-- do not distinguish ownership (C++/Python/R) treat everything as 'Owned'; the
-- Rust member uses it to decide whether to clone, dereference, or borrow an
-- expression at a use site (its borrow-by-default parameter convention makes a
-- non-'Copy' parameter a reference, so feeding it to an owned position requires
-- a clone).
data IOwnership
  = Owned
  -- ^ an owned value (@T@): a literal, a call result, a constructor
  | BorrowedRef
  -- ^ a shared reference (@&T@): a non-'Copy' bound variable, or a
  -- constructor field read in place (OWN-6)
  | BorrowedPlace
  -- ^ a place of type @T@ reached through a borrow (a field of a borrowed
  -- value): moving it out needs a clone, but it is not itself a reference
  deriving (Eq, Show)

-- | How a constructor field is read: taken out of its subject (OWN-5),
-- borrowed where it lies (OWN-6), or copied out.
data FieldRead = FieldTake | FieldBorrow | FieldCopy
  deriving (Eq, Show)

-- | A sourced binary operator written between its operands.
infixOperator :: Source -> MDoc -> MDoc -> MDoc
infixOperator src l r = parens (l <+> pretty (unSrcName (srcName src)) <+> r)

-- | Per-language configuration for lowering
data LowerConfig m = LowerConfig
  { lcSrcName :: Source -> MDoc
  , lcOperator :: Source -> MDoc -> MDoc -> MDoc
  -- ^ Apply a sourced binary operator to its two operands.
  , lcApplySrcGroup :: MDoc -> [MDoc] -> MDoc
  -- ^ Apply one CONTINUATION group of a curried source call (@f(a)(b)@, the
  -- groups a source statement's @\@rsize@ declares). The first group is the
  -- call itself; each later group applies the function value the previous
  -- group returned. Default is juxtaposition, which is how a host language
  -- with first-class callables spells it.
  , lcSourcedArg :: ArgSite -> IOwnership -> TypeM -> MDoc -> MDoc
  -- ^ How to pass one argument at a call, given the call kind ('ArgSite'), the
  -- argument's ownership ('IOwnership'), its 'TypeM', and its rendered
  -- expression. Default is identity (C++/Generic pass args unchanged); Rust
  -- borrows non-'Copy' native args so a value fanned out to several consumers is
  -- shared, borrows every argument of a closure application to match the
  -- by-reference @Fn(&T, ...)@ calling convention, and clones/derefs a borrowed
  -- argument passed to a by-value parameter.
  , lcOwnership :: NativeExpr -> m IOwnership
  -- ^ The ownership form a native expression evaluates to (default: always
  -- 'Owned'). Computed from the original IR so a use site can adapt it; monadic
  -- because a bound variable's ownership depends on the enclosing manifold's
  -- parameter conventions (tracked in member state).
  , lcArgManifoldOwnership :: NativeManifold -> m IOwnership
  -- ^ The ownership of the value produced by a manifold used as an argument (a
  -- 'NativeArgManifold'). Always 'Owned' -- a manifold call materializes an owned
  -- value (its return type is the owned @T@, and the return sink clones a borrowed
  -- body) -- so no member distinguishes on the manifold.
  , lcBindCallArgs :: Bool
  -- ^ Bind every computed call argument to a local before the call. Set for a
  -- language that passes arguments unevaluated (R), where an argument nested
  -- in a call would otherwise run inside the callee's frame, or not at all if
  -- the callee never reads it.
  , lcOwnArg :: IOwnership -> TypeF -> MDoc -> MDoc
  -- ^ Adapt an expression to an owned value at an owned sink (a container
  -- element, a return, a let binding, a by-value parameter). Default is identity;
  -- Rust clones a borrowed non-'Copy' value and dereferences a borrowed 'Copy'
  -- reference.
  , lcReadArg :: IOwnership -> MDoc -> MDoc
  -- ^ Adapt an expression to a place a read-only sink takes the address of (a
  -- value serialized, shown, hashed or saved). Default is identity; Rust
  -- dereferences a borrowed reference, so the sink reborrows the value rather
  -- than referencing the reference.
  , lcWithCallerScope :: forall a. m a -> m a
  -- ^ Run an action as if in the enclosing (caller) manifold's scope, so
  -- 'lcOwnership' of a manifold-call argument (named by index-aliasing after the
  -- caller's variables) reflects the caller rather than the callee being
  -- rendered. Default is identity (only Rust distinguishes ownership).
  , lcVariantLit :: MDoc -> Text -> Int -> [MDoc] -> MDoc
  -- ^ Build a payload-bearing `data` value: the type's native spelling at
  -- this occurrence, the arm's name, its 0-based tag, and the rendered
  -- field expressions.
  --
  -- The spelling is the RENDERED type, not the concrete name the type was
  -- declared with: a user's per-language form may be a template, and what
  -- a constructor of @Box Int@ has to name is the instantiation.
  --
  -- Each backend constructs one its own way -- a Rust or C++ enum arm takes
  -- its fields positionally, a Python dataclass is called by name, an R S3
  -- value is a classed list -- so there is no shared spelling to default to.
  , lcEnumLit :: MDoc -> [Text] -> Text -> Int -> MDoc
  -- ^ Render a `data` constructor as a value in this language: the enum's
  -- rendered type, every constructor in declaration order, the
  -- constructor's own name, and its 0-based tag.
  --
  -- All four are needed because the backends disagree about what an enum
  -- value IS. C++ and Rust name it (@DNA::A@). Python receives the tag as a
  -- plain int from the runtime, so the tag is the value. R holds an ordered
  -- factor, which is its whole level set plus a 1-based code into it, so a
  -- literal cannot be written without the other constructors.
  , lcVariantTagTest :: MDoc -> Text -> Int -> MDoc -> MDoc
  -- ^ Test whether a payload-bearing value carries a given arm: the type's
  -- rendered spelling, the arm's name, its tag, and the subject.
  --
  -- Separate from 'lcEnumTagTest' because an arm carries arguments:
  -- equality would compare payloads where a pattern must test the
  -- discriminant alone. Every backend asks a different way (@matches!@,
  -- @holds_alternative@, @isinstance@, @inherits@).
  , lcCtorField :: FieldRead -> MDoc -> Text -> Int -> Int -> MDoc -> MDoc
  -- ^ Read one field out of a value whose arm is already established: how
  -- the field is read, the type's rendered spelling, the arm's name, its
  -- tag, the field's index, the subject.
  --
  -- Only ever emitted under a passing tag test, and unreachable from
  -- surface syntax -- a getter aimed at a `data` type is rejected, because
  -- which fields exist depends on the constructor.
  , lcEnumTagTest :: MDoc -> [Text] -> Text -> Int -> MDoc -> MDoc
  -- ^ Test whether an argument-free constructor's value carries a given
  -- tag ('IntrTagTest'): the enum's rendered type, every constructor
  -- in declaration order, the constructor under test, its 0-based tag, and
  -- the subject.
  --
  -- Sibling of 'lcVariantTagTest', and takes the same arguments for the
  -- same reason: a test cannot be spelled from two rendered expressions
  -- alone. An enum holds nothing but its tag, so every backend still
  -- writes a native @==@ -- but against different right-hand sides, and R
  -- deliberately compares against the bare constructor name rather than
  -- against the factor 'lcEnumLit' builds, which would allocate a whole
  -- level set per test.
  , lcListLenTest :: MDoc -> Int -> MDoc
  -- ^ Test whether a list has exactly this many elements ('IntrListLenTest'):
  -- the subject and the length.
  , lcListElem :: MDoc -> Int -> MDoc
  -- ^ Read the element at a 0-based index of a list ('IntrListElem'): the
  -- subject and the index. Only ever emitted under a passing
  -- 'lcListLenTest', so the index is in range.
  , lcCoerceOptional :: MDoc -> MDoc
  -- ^ Widen a value to an optional (@CoerceToOptional@). Default is identity: in
  -- C++/Python/R a @T@ is a valid @?T@ (their serializers are schema-driven). In
  -- Rust @?T@ is a distinct @Option<T>@ whose wire layout is type-driven, so the
  -- value must be wrapped in @Some(..)@. The coercion is only inserted to widen a
  -- non-optional inner, so this never double-wraps.
  , lcTypeOf :: TypeF -> m (Maybe IType)
  , lcSerialAstType :: SerialAST -> m (Maybe IType)
  -- ^ type of a SerialAST for serialization (used for C++ typed declarations)
  , lcDeserialAstType :: SerialAST -> m (Maybe IType)
  -- ^ type of a SerialAST for deserialization (for C++, uses shallowType)
  , lcRawDeserialAstType :: SerialAST -> m (Maybe IType)
  -- ^ raw deserialized type for the _get_value template parameter (C++ specific)
  -- For records, C++ converts to std::tuple; for others, uses serialAstToType
  , lcTypeMOf :: TypeM -> m (Maybe IType)
  , lcPackerName :: Source -> MDoc
  , lcUnpackerName :: Source -> MDoc
  -- | Whether the argument passed to a pack/unpack function during serialize
  -- weaving must be borrowed. Default is 'False' (C++/Py/R bind a value to a
  -- const-ref/by-value param implicitly); Rust borrows a non-'Copy' arg so it
  -- matches the packer's `&T` parameter. The argument is the arg's type.
  , lcBorrowPackArg :: TypeF -> Bool
  , lcRecordAccessor :: NamType -> CVar -> MDoc -> MDoc -> MDoc
  , lcDeserialRecordAccessor :: Int -> Key -> MDoc -> MDoc
  -- ^ How to access record fields during deserialization.
  -- For Python/R: same as lcRecordAccessor (by key name)
  -- For C++: uses tuple indexing since records are deserialized as tuples
  , lcTupleAccessor :: Int -> MDoc -> MDoc
  , lcNewIndex :: m Int
  , -- expression/arg lowering fields
    lcPrintExpr :: IExpr -> MDoc
  , lcPrintStmt :: IStmt -> MDoc
  , lcEvalPattern :: TypeF -> Pattern -> [MDoc] -> m MDoc
  -- ^ Pattern evaluation (language-specific because patterns use
  -- language-specific constructors for tuples/records)
  , lcTakeSetter :: NativeExpr -> TypeF -> Selector -> [MDoc] -> m (Maybe MDoc)
  -- ^ A setter whose receiver is kept (OWN-2): the receiver's original, the
  -- result type, the selector, and the receiver and values. 'Nothing'
  -- renders it with 'lcEvalPattern'.
  , lcTakeGetter :: TypeF -> Selector -> MDoc -> m (Maybe MDoc)
  -- ^ A getter on a receiver it owns (OWN-5): the receiver's type, the
  -- selector and the receiver. 'Nothing' renders it with 'lcEvalPattern'.
  , lcListConstructor :: FVar -> [TypeF] -> [MDoc] -> MDoc
  -- ^ Build a list literal from rendered elements. R needs FVar to choose c() vs list().
  -- The [TypeF] is the FULL applied-type arg list (kind positions included).
  -- Use 'partitionKindArgsF' to extract type-args only when needed.
  , lcTupleConstructor :: FVar -> [TypeF] -> [MDoc] -> MDoc
  -- ^ Build a tuple literal. The [TypeF] is the per-slot type list
  -- (one entry per element). C++ codegen will use these to dispatch
  -- on field shape when a user-mapped tuple-alias is supported; today
  -- both Python/R and C++ ignore them (Python/R because their tuple
  -- constructors don't need type info, C++ because alias preservation
  -- is not yet plumbed through Express -> Mono -> TupleN's FVar).
  , lcRecordConstructor :: TypeF -> NamType -> FVar -> [TypeF] -> [(Key, MDoc)] -> m PoolDocs
  -- ^ Build a record literal. C++ needs type lookup + counter for temp var.
  , lcKeepContextArg :: Int -> Int -> Int -> TypeF -> MDoc -> m MDoc
  -- ^ Adapt context argument @j@, local @i@ of the given type, of a fully
  -- applied call of native manifold @m@ (OWN-3, OWN-7).
  , lcKeepCallArg :: Int -> Int -> NativeExpr -> MDoc -> m MDoc
  -- ^ Adapt argument @j@ of a call of native manifold @m@, given its
  -- original (OWN-7).
  , lcKeepSlot :: [Int] -> NativeExpr -> MDoc -> m MDoc
  -- ^ Adapt the value a native loop ends with, given the loop's carried
  -- locals and the value's original (OWN-2).
  , lcKeepElem :: NativeExpr -> MDoc -> m MDoc
  -- ^ Adapt a tuple, record or constructor element, the value an optional
  -- coercion wraps, or the value a let's right-hand side ends in, given its
  -- original (OWN-2).
  , lcStoreField :: TypeF -> MDoc -> m MDoc
  -- ^ Adapt a value to its stored representation inside an aggregate (given the
  -- element/field type). Applied to record fields AND list/tuple elements.
  -- Default is identity; the Rust member wraps a function value in
  -- @Arc<dyn Fn>@ so it fits the boxed trait-object
  -- element type (a bare @impl Fn@ is illegal in a Vec/tuple/struct field, and
  -- the explicit cast is needed because a @vec![..]@ has no per-element declared
  -- type to drive the unsizing coercion). Monadic so the cast type can render.
  , lcPapply :: TypeF -> [TypeM] -> MDoc -> [MDoc] -> m MDoc
  -- ^ Partially apply a local function value (a 'PapplyP'): the result type
  -- (the function of the remaining arguments), the applied arguments' types,
  -- the function, and the arguments. The pool's runtime runs the function's
  -- stage entry when it has one ('mlc_papply').
  , lcApplyClosure :: MDoc -> [MDoc] -> MDoc
  -- ^ Apply a local function value (a 'LocalCallP') to its arguments. Default is
  -- a direct call @f(args)@; the Rust member parenthesizes the callee,
  -- @(f)(args)@, since a field or call expression cannot be called bare.
  , lcForeignCall :: MDoc -> Int -> [MDoc] -> MDoc
  , lcRemoteCall :: MDoc -> Int -> RemoteResources -> [MDoc] -> m PoolDocs
  , lcCacheBody :: SerialAST -> Text -> Int -> [(Arg TypeM, SerialAST)] -> PoolDocs -> m PoolDocs
  -- ^ Wrap a labeled manifold's body in a cache lookup. The returned
  -- 'PoolDocs' must set 'poolExpr' to the temp holding the cached or
  -- freshly-computed result. Args are paired with their 'SerialAST'
  -- so the hook can serialize native bindings and pass each arg's
  -- schema to the content-aware hash; the leading 'SerialAST' is the
  -- result schema (used by cache_store to materialize SHM-backed
  -- payloads before writing).
  , lcDebugWrap :: Int -> [(Arg TypeM, SerialAST)] -> PoolDocs -> m PoolDocs
  -- ^ Wrap a manifold body in a per-language try/catch that, on any
  -- thrown exception, serializes each arg via its 'SerialAST', calls
  -- @morloc_debug_record_frame@ with the (mid, schema, packet bytes)
  -- tuples, and re-raises. Zero cost on the happy path: only the
  -- try-frame overhead, no serialization or hashing. Each translator's
  -- lcDebugWrap closes over its own manifold @(name, srcloc)@ lookup
  -- (from 'makeManifoldDebugInfoLookup') and bakes the strings into the
  -- codegen'd call so the runtime can render them next to @mid=@.
  , lcMakeLet :: (Int -> MDoc) -> Int -> Maybe TypeF -> Maybe NativeExpr -> PoolDocs -> PoolDocs -> m PoolDocs
  -- ^ Let binding assembly at the PoolDocs level. A native let of the IR
  -- passes its right-hand side, from which a member decides how the local
  -- binds it ('Morloc.CodeGenerator.Ownership.letMode'); a serial or helper
  -- let passes nothing.
  , lcReleaseStmt :: Text -> MDoc
  -- ^ Produce a statement finishing with a let-bound packet variable:
  -- releasing the shared memory it owns, and, for a member that manages
  -- its own packet buffers, freeing the buffer too. Called at the end of
  -- the let's body, so what the packet holds is given back as soon as the
  -- body is done with it rather than accumulating until the next dispatch
  -- flush. For inline packets (no SHM), the runtime function this targets
  -- is a no-op on that half, so emitting the call unconditionally is safe.
  , lcReleaseBorrowedStmt :: Text -> MDoc
  -- ^ As 'lcReleaseStmt', for a packet whose bytes the value read out of
  -- it still refers to. A function value from another pool is the case
  -- that matters: it carries the packets of whatever it captured, so that
  -- it can be applied later or passed on. Only the shared memory is given
  -- back; the buffer stays.
  , lcReturn :: MDoc -> MDoc
  , lcFrameReturn :: Int -> TypeM -> Bool -> MDoc -> m (Maybe MDoc)
  -- ^ For a manifold that records its call on the frame stack (STATE-5):
  -- its return statement, which pops the call once the result is computed
  -- (EMIT-3), given its mid, return type, whether the returned expression
  -- names a local the function owns, and the expression. Nothing for a
  -- manifold that records nothing, which returns with 'lcReturn'.
  , -- | A copy of an argument packet the manifold returns as its result.
    -- The argument lives in the incoming call, which is released before the
    -- result is sent, so a language that frees packets by hand must return
    -- a copy holding its own reference; a collected language returns it.
    lcDupPacket :: MDoc -> MDoc
  , lcLoopBorrow :: Int -> NativeExpr -> m (Maybe MDoc)
  -- ^ A native loop's local that holds a borrow ('loopBorrowLocals'): the
  -- local, and the expression it is bound to, started from or replaced by,
  -- rendered as a borrow. 'Nothing' when the local holds a value.
  , lcLoopLetRhs :: Int -> NativeExpr -> PoolDocs -> m PoolDocs
  -- ^ The right-hand side of a native let in a loop body, given the let's
  -- index and original expression. A loop let is emitted by the member's
  -- loop walker rather than 'lcMakeLet', so a member that binds some lets by
  -- taking their value (see 'consumableProjectionLets') applies it here.
  , lcLetRhs :: Int -> NativeExpr -> PoolDocs -> m PoolDocs
  -- ^ As 'lcLoopLetRhs', for a native let outside a loop: a member may
  -- rewrite the lowered right-hand side here, before 'lcMakeLet' binds it.
  , lcOwnPacketDecl :: MDoc -> MDoc -> Maybe (MDoc, MDoc)
  -- ^ Given an owner variable and a fresh packet expression, a statement
  -- declaring the owner (which releases the packet when its scope ends) and
  -- the expression reading the packet out of it. Nothing for a language
  -- that cannot scope a packet; the dispatch flush then releases it.
  , lcOwnedArg :: MDoc -> MDoc
  -- ^ A packet serialized for one call's argument list and used by nothing
  -- else, wrapped so it is released once the call has returned (and when an
  -- exception unwinds past the call). Identity for a language that cannot
  -- scope it; the dispatch flush then releases it.
  , lcMakeIf :: NativeExpr -> PoolDocs -> PoolDocs -> PoolDocs -> m PoolDocs
  -- ^ origExpr, condDocs, thenDocs, elseDocs -> result PoolDocs
  -- Produces language-specific if/else structure using a temp result variable
  , lcShortCircuit :: Bool -> MDoc -> MDoc -> MDoc
  -- ^ The language's own short-circuit operator applied to two expressions:
  -- @True@ for and, @False@ for or. A conditional with a Bool literal arm
  -- and an arm that needs no statements is written with it (BASE-3).
  , lcMakeLoop :: LoopResult -> [(Int, Maybe PoolDocs)] -> Maybe (LoopBody NativeExpr SerialExpr ()) -> LoopBody PoolDocs PoolDocs PoolDocs -> m PoolDocs
  -- ^ Tail-loop assembly. Args: what a base leaf yields; each loop-carried
  -- native local id, with the value it starts from when the loop declares
  -- it; the loop body as the IR gives it, base leaves dropped, when it is
  -- known, so a member can decide how each let binds; and the body with its
  -- leaves already lowered to 'PoolDocs'. Emits @while(1){ <walk the tree: guards -> if/else,
  -- lets -> assignments, LoopBase -> result = base; break, LoopContinue ->
  -- reassign the loop-locals via temps> }@. The result 'PoolDocs' carries the
  -- loop as prior lines with the return flag set (a base value is the
  -- manifold's return). In a serial loop the locals are the manifold's
  -- native param vars (@nvarNamer id@), deserialized once by the manifold
  -- prologue; in a native loop they are declared from their initializers,
  -- since the parameters are read-only.
  , lcMakeDoBlock :: TypeF -> [MDoc] -> MDoc -> m ([MDoc], MDoc)
  -- ^ type -> prior statements -> return expression -> (hoisted lines,
  -- suspended-thunk expression). Monadic so a language whose thunk form
  -- cannot hold statements (e.g. a Python lambda) can mint a fresh name
  -- for a hoisted def-thunk.
  -- The second argument is the Ok arm's payload type, when the language can
  -- name it; a typed language declares the Ok callback's parameter with it,
  -- so the body's result converts at the call rather than inside the arm's
  -- initializer.
  , lcMakeTry :: MDoc -> Maybe MDoc -> (MDoc -> MDoc) -> (MDoc -> MDoc) -> MDoc
  -- ^ @try body@: the body as a no-arg thunk, a builder wrapping a value
  -- expression in the result's @Ok@ arm, and one wrapping a message
  -- expression in its @Err@ arm.
  --
  -- The arms are passed as builders rather than rendered because only the
  -- backend knows how to spell a unary lambda, and only the caller knows
  -- how to spell a variant literal for this particular @Try@ -- which
  -- differs between a mapped and a generated representation. Each backend
  -- calls its own @mlc_try@ helper, which owns the language's notion of
  -- which failures are catchable.
  , lcSerialize :: MDoc -> SerialAST -> m PoolDocs
  , lcDeserialize :: TypeF -> MDoc -> SerialAST -> m (MDoc, [MDoc])
  , lcReifyClosure :: MDoc -> SerialAST -> m MDoc
  -- ^ Reify a defunctionalized closure value into its wire form: given the
  -- native closure expression and its 'SerialClosure', produce an expression
  -- yielding the tuple @(home_language, manifold_id, captured_packets)@ that the
  -- generic tuple codec then serializes. The 'SerialClosure' carries the arity,
  -- needed by backends whose reify is arity-indexed (e.g. Rust @reifyN@). Called
  -- at the serialize boundary for a 'SerialClosure'. See 'computeClosureSchemas'.
  , lcReflectClosure :: MDoc -> SerialAST -> m MDoc
  -- ^ Reflect an incoming closure wire form into a native callable: given the
  -- serialized packet expression and the 'SerialClosure' type, produce an
  -- expression yielding a callable that, on application, serializes its
  -- arguments, calls back to the home pool via @foreign_call@ on the closure's
  -- manifold id, and deserializes the result. Called at the deserialize
  -- boundary for a top-level 'SerialClosure' (the whole packet is the closure).
  , lcReflectClosureParsed :: MDoc -> SerialAST -> m MDoc
  -- ^ Like 'lcReflectClosure', but for a closure NESTED in an aggregate: the
  -- enclosing @get_value@ has already deserialized the wire tuple, so the given
  -- expression is the parsed @(home_language, manifold_id, captured_packets)@
  -- tuple, not a raw packet. Build the callable directly from it (no second
  -- @get_value@). Called at the deserialize boundary for a nested 'SerialClosure'.
  , lcDivertNestedClosure :: SerialAST -> Bool
  -- ^ Whether this backend routes THIS aggregate node through 'construct' so a
  -- closure inside it is reified/reflected in place, rather than the whole-value
  -- msgpack short-circuit. C++/Python/R return True for any
  -- list/tuple/record/optional holding a closure; Rust returns False everywhere
  -- (nested-closure support is a documented follow-on -- a bare crossing closure
  -- still works). Lets the shared engine opt in per backend AND per aggregate
  -- shape.
  , -- manifold lowering fields
    lcMakeFunction ::
      Int ->
      MDoc ->
      [Arg TypeM] ->
      TypeM ->
      [MDoc] ->
      MDoc ->
      Maybe HeadManifoldForm ->
      Bool ->
      m (Maybe MDoc)
  -- ^ mid, name, all args, return (body) type, priorLines, body, headForm,
  -- and whether the function is a closure (code outside morloc may call it)
  -- Returns Nothing if dedup'd (C++), Just funcDef otherwise. The mid
  -- is threaded so the per-manifold error-wrap can look up user name
  -- and srcloc for the trace line.
  , lcMakePass :: MDoc -> TypeM -> MDoc -> [Arg TypeM] -> m MDoc
  -- ^ Render a whole function/operator passed to a higher-order function
  -- (@ManifoldPass@), given the closure's own callable signature (from
  -- 'lcClosureSig'), the manifold name and its parameters. Most
  -- languages pass the manifold by name; the Rust member wraps it in a safe
  -- closure that adapts each argument to the manifold's parameter convention
  -- (deref a Copy scalar, forward a reference) since a bare @unsafe fn@ does
  -- not implement @Fn@.
  , lcMakeLambda :: MDoc -> TypeM -> MDoc -> [Arg TypeM] -> [Arg TypeM] -> m MDoc
  -- ^ closureSig, name, contextArgs, boundArgs - partial application
  -- expression. @closureSig@ is the language's rendering of the closure's own
  -- callable signature (from 'lcClosureSig'); languages that do not need it
  -- (Python, R) ignore it.
  , lcClosureSig :: TypeM -> m MDoc
  -- ^ Render a closure manifold's callable signature (e.g. C++ @int(int)@ for a
  -- @ManifoldPart@ that takes an int and returns an int). Empty for languages
  -- whose closures need no static signature.
  , lcRegisterSchema :: Text -> m Int
  -- ^ Register a schema string and return its unique ID (index into schema table)
  , lcTableImportFn :: Maybe Text
  -- ^ A runtime helper that converts a received Arrow record batch into the
  -- language type a module maps @Table@ to, given that type's name. Nothing
  -- for languages with a single table type.
  }

-- | True if a closure appears anywhere inside the AST. An aggregate is
-- @isSerializable@ even when it holds a closure field (a closure reports
-- serializable so a bare crossing closure hits the top-level reify/reflect
-- clause), so the recursion helpers must additionally refuse to treat such an
-- aggregate as a msgpack leaf -- otherwise the nested closure field is never
-- routed through 'lcReifyClosure'/'lcReflectClosure'.
containsClosure :: SerialAST -> Bool
containsClosure = not . null . serialClosuresOf

-- | A node that serializes as an opaque msgpack leaf: serializable AND not
-- diverted for in-place nested-closure handling by this backend.
isMsgpackLeaf :: LowerConfig m -> SerialAST -> Bool
isMsgpackLeaf cfg s = isSerializable s && not (lcDivertNestedClosure cfg s)

{- | Expand serialization into IR statements.
Returns (final expression representing the serialized value, prior statements).
-}
expandSerialize :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m (IExpr, [IStmt])
-- A closure has no msgpack leaf form: reify it into its wire tuple first, then
-- let the generic tuple codec serialize that tuple.
expandSerialize cfg v0 s0@(SerialClosure _ _) = do
  reified <- lcReifyClosure cfg v0 s0
  schemaId <- lcRegisterSchema cfg (render $ serialAstToMsgpackSchema s0)
  return (ISerCall schemaId (IRawExpr (render reified)), [])
expandSerialize cfg v0 s0 = do
  (stmts, vExpr) <- nativeToWire cfg v0 s0
  schemaId <- lcRegisterSchema cfg (render $ serialAstToMsgpackSchema s0)
  return (ISerCall schemaId vExpr, stmts)

-- | A native value as its wire form: closures reified, packers unpacked.
nativeToWire :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m ([IStmt], IExpr)
nativeToWire cfg = go
  where
    go v s
      | isMsgpackLeaf cfg s = return ([], IRawExpr (render v))
      | otherwise = construct v s

    -- A closure field inside an aggregate: reify it in place into its wire
    -- tuple, which the enclosing aggregate's generic codec then serializes
    -- against the closure field's tuple schema.
    construct v s@(SerialClosure _ _) = do
      reified <- lcReifyClosure cfg v s
      return ([], IRawExpr (render reified))
    construct v (SerialPack _ (p, s)) =
      let unpacker = lcUnpackerName cfg (typePackerReverse p)
          arg = if lcBorrowPackArg cfg (typePackerPacked p) then "&(" <> v <> ")" else v
       in go (unpacker <> parens arg) s
    construct v lst@(SerialList _ _ s) = do
      idx <- lcNewIndex cfg
      resultType <- lcSerialAstType cfg lst
      let v' = render $ helperNamer idx
          iterVar = render $ "i" <> pretty idx
      (before, x) <- go ("i" <> pretty idx) s
      return ([IMapList v' resultType iterVar (IRawExpr (render v)) before x], IVar v')
    construct v tup@(SerialTuple _ ss) = do
      results <- zipWithM (\i s -> go (lcTupleAccessor cfg i v) s) [0 ..] ss
      let (befores, exprs) = unzip results
      idx <- lcNewIndex cfg
      typeM <- lcSerialAstType cfg tup
      let v' = render $ helperNamer idx
      return (concat befores ++ [IAssign v' typeM (ITupleLit exprs)], IVar v')
    construct v obj@(SerialObject namType fv@(FV _ constructor) _ rs) = do
      let accessor = lcRecordAccessor cfg namType constructor
      results <- mapM (\(key, s) -> go (accessor v (pretty key)) s) rs
      let (befores, exprs) = unzip results
      idx <- lcNewIndex cfg
      typeM <- lcSerialAstType cfg obj
      let v' = render $ helperNamer idx
      return
        ( concat befores ++ [IAssign v' typeM (IRecordLit namType fv (zip (map fst rs) exprs))]
        , IVar v'
        )
    construct v opt@(SerialOptional _ innerS) = do
      idx <- lcNewIndex cfg
      resultType <- lcSerialAstType cfg opt
      -- The unwrapped value is the inner *user-facing* (shallow) type, not
      -- the wire form: the inner construct (e.g. SerialPack's unpacker)
      -- consumes the user-facing value and produces the wire form. Using
      -- lcSerialAstType here would declare u0 as the wire form and then
      -- pass it to an unpacker expecting the user-facing form, breaking
      -- compilation. lcDeserialAstType returns the shallow type per the
      -- C++ translator's `cppTypeOf . shallowType`.
      unwrappedType <- lcDeserialAstType cfg innerS
      let v' = render $ helperNamer idx
          uVar = "u" <> T.pack (show idx)
      (before, x) <- go (pretty uVar) innerS
      return
        ( [IIfNotNull v' resultType (IRawExpr (render v)) uVar unwrappedType before x]
        , IVar v'
        )
    construct v var@(SerialVariant (FV _ cv) _ arms@(_ : _)) = do
      ty <- nominalTypeDoc cfg (serialAstToType var) cv
      resultType <- lcSerialAstType cfg var
      byVariantArm cfg resultType ty v arms $ \n i ss -> do
        results <- zipWithM (\j s -> go (lcCtorField cfg FieldCopy ty n i j v) s) [0 ..] ss
        let (befores, exprs) = unzip results
        return (concat befores, IRawExpr (render (lcVariantLit cfg ty n i (map (lcPrintExpr cfg) exprs))))
    construct _ _ = error "Unreachable in nativeToWire"

-- | A table always arrives as an Arrow record batch; when the module maps
-- @Table@ to some other library type and the language has a converter,
-- wrap the deserialized value in it, naming the mapped type.
tableImport :: LowerConfig m -> SerialAST -> IExpr -> IExpr
tableImport cfg (SerialObject NamTable (FV _ (CV cv)) _ _) e
  | Just fn <- lcTableImportFn cfg
  , not (T.null cv) =
      ICall fn Nothing [[e, IRawExpr (render (dquotes (pretty cv)))]]
tableImport _ _ e = e

{- | Expand deserialization into IR statements.
Returns (final expression representing the deserialized value, prior statements).
-}
expandDeserialize :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m (IExpr, [IStmt])
-- A closure arrives as its wire tuple: reflect it into a native callable that
-- calls back to the producing pool on apply.
expandDeserialize cfg v0 s0@(SerialClosure _ _) = do
  reflected <- lcReflectClosure cfg v0 s0
  return (IRawExpr (render reflected), [])
expandDeserialize cfg v0 s0
  | isMsgpackLeaf cfg s0 = do
      schemaId <- lcRegisterSchema cfg (render $ serialAstToMsgpackSchema s0)
      desType <- lcDeserialAstType cfg s0
      let call = IDesCall schemaId desType (serialAstHasString s0) (IRawExpr (render v0))
      return (tableImport cfg s0 call, [])
  | otherwise = do
      idx <- lcNewIndex cfg
      rawType <- lcRawDeserialAstType cfg s0
      let rawvar = render $ helperNamer idx
      schemaId <- lcRegisterSchema cfg (render $ serialAstToMsgpackSchema s0)
      (x, befores) <- wireToNative cfg (helperNamer idx) s0
      return (x, IAssign rawvar rawType (IDesCall schemaId rawType (serialAstHasString s0) (IRawExpr (render v0))) : befores)

-- | A parsed wire value as its native form: closures reflected, packers
-- applied.
wireToNative :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m (IExpr, [IStmt])
wireToNative cfg = check
  where
    check v s
      | isMsgpackLeaf cfg s = return (IRawExpr (render v), [])
      | otherwise = construct v s

    -- A closure field: the enclosing get_value has already parsed its wire
    -- tuple, so reflect the parsed tuple in place into a native callable that
    -- RPCs back to the producing pool on application.
    construct v s@(SerialClosure _ _) = do
      reflected <- lcReflectClosureParsed cfg v s
      return (IRawExpr (render reflected), [])
    construct v (SerialPack _ (p, s')) = do
      (x, before) <- check v s'
      let packer = render $ lcPackerName cfg (typePackerForward p)
          -- Borrow the packer's arg (the wire value) for a non-Copy wire type,
          -- matching a `&T` packer parameter. The common (no-borrow) case keeps
          -- `x` structured; only a borrow needs a rendered `&(..)` wrapper.
          x' = if lcBorrowPackArg cfg (typePackerUnpacked p)
                 then IRawExpr (render ("&(" <> lcPrintExpr cfg x <> ")"))
                 else x
      return (IPack packer x', before)
    construct v lst@(SerialList _ _ s) = do
      idx <- lcNewIndex cfg
      resultType <- lcDeserialAstType cfg lst
      let v' = render $ helperNamer idx
          iterVar = render $ "i" <> pretty idx
      (x, before) <- check ("i" <> pretty idx) s
      return (IVar v', [IMapList v' resultType iterVar (IRawExpr (render v)) before x])
    construct v tup@(SerialTuple _ ss) = do
      results <- zipWithM (\i s -> check (lcTupleAccessor cfg i v) s) [0 ..] ss
      let (exprs, befores) = unzip results
      typeM <- lcDeserialAstType cfg tup
      v' <- (render . helperNamer) <$> lcNewIndex cfg
      return (IVar v', concat befores ++ [IAssign v' typeM (ITupleLit exprs)])
    construct v (SerialObject namType fv@(FV _ _) _ rs) = do
      let accessor = lcDeserialRecordAccessor cfg
      results <- zipWithM (\i (k, s) -> check (accessor i k v) s) [0 ..] rs
      let (exprs, befores) = unzip results
      typeM <- lcDeserialAstType cfg (SerialObject namType fv [] rs)
      idx <- lcNewIndex cfg
      let v' = render $ helperNamer idx
      return
        (IVar v', concat befores ++ [IAssign v' typeM (IRecordLit namType fv (zip (map fst rs) exprs))])
    construct v opt@(SerialOptional _ innerS) = do
      idx <- lcNewIndex cfg
      resultType <- lcDeserialAstType cfg opt
      unwrappedType <- lcRawDeserialAstType cfg innerS
      let v' = render $ helperNamer idx
          uVar = "u" <> T.pack (show idx)
      (x, before) <- check (pretty uVar) innerS
      return
        ( IVar v'
        , [IIfNotNull v' resultType (IRawExpr (render v)) uVar unwrappedType before x]
        )
    construct v var@(SerialVariant (FV _ cv) _ arms@(_ : _)) = do
      ty <- nominalTypeDoc cfg (serialAstToType var) cv
      resultType <- lcDeserialAstType cfg var
      swap <$> byVariantArm cfg resultType ty v arms (\n i ss -> do
        results <- zipWithM (\j s -> check (lcCtorField cfg FieldCopy ty n i j v) s) [0 ..] ss
        let (exprs, befores) = unzip results
        return (concat befores, IRawExpr (render (lcVariantLit cfg ty n i (map (lcPrintExpr cfg) exprs)))))
    construct _ _ = error "Unreachable in wireToNative"

-- | Branch on a variant's arm and rebuild the arms whose fields need
-- conversion; the others pass through as they are.
byVariantArm ::
  (Monad m) =>
  LowerConfig m ->
  Maybe IType ->
  MDoc ->
  MDoc ->
  [(Text, [SerialAST])] ->
  (Text -> Int -> [SerialAST] -> m ([IStmt], IExpr)) ->
  m ([IStmt], IExpr)
byVariantArm cfg resultType ty v arms rebuild = chain (zip [0 ..] arms)
  where
    pass = return ([], IRawExpr (render v))
    arm (i, (n, ss))
      | all (isMsgpackLeaf cfg) ss = pass
      | otherwise = rebuild n i ss
    chain [] = pass
    chain [a] = arm a
    chain (a@(i, (n, _)) : rest) = do
      (thenStmts, thenExpr) <- arm a
      (elseStmts, elseExpr) <- chain rest
      v' <- render . helperNamer <$> lcNewIndex cfg
      let cond = IRawExpr (render (lcVariantTagTest cfg ty n i v))
      return ([IIf v' resultType cond thenStmts thenExpr elseStmts elseExpr], IVar v')

-- | What a tail loop's base leaves yield: a packet (a loop that is a pool's
-- serial entry) or a native value of the given type (a loop's native entry).
data LoopResult = LoopResultSerial | LoopResultNative TypeF

-- | Lower a serial expression to PoolDocs via the IR.
lowerSerialExpr ::
  (Monad m) =>
  LowerConfig m ->
  SerialExpr ->
  SerialExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) ->
  m PoolDocs
lowerSerialExpr _ _ (ManS_ f) = return f
lowerSerialExpr cfg _ (AppPoolS_ _ (PoolCall mid (Socket _ socketFile) ForeignCall args) _) =
  return $ defaultValue {poolExpr = lcForeignCall cfg socketFile mid (map argNamer args)}
lowerSerialExpr cfg _ (AppPoolS_ _ (PoolCall mid (Socket _ socketFile) (RemoteCall res) args) _) =
  lcRemoteCall cfg socketFile mid res (map argNamer args)
lowerSerialExpr cfg (AppRecS _ _ origEs) (AppRecS_ _ mid es) = do
  return $ mergePoolDocs ((<>) (manNamer mid) . tupled) (ownFreshArgs cfg origEs es)
lowerSerialExpr cfg (AppForeignRecS _ _ _ origEs) (AppForeignRecS_ _ mid (Socket _ socketFile) es) = do
  return $ mergePoolDocs (\args -> lcForeignCall cfg socketFile mid args) (ownFreshArgs cfg origEs es)
lowerSerialExpr _ _ (AppRecS_ _ mid es) = do
  return $ mergePoolDocs ((<>) (manNamer mid) . tupled) es
lowerSerialExpr cfg _ (AppForeignRecS_ _ mid (Socket _ socketFile) es) = do
  return $ mergePoolDocs (\args -> lcForeignCall cfg socketFile mid args) es
lowerSerialExpr cfg _ (CacheBodyS_ _ resSa lbl mid args body) =
  lcCacheBody cfg resSa lbl mid args body
lowerSerialExpr cfg _ (DebugWrapS_ _ mid args body) =
  lcDebugWrap cfg mid args body
lowerSerialExpr cfg (ReturnS (BndVarS _ _)) (ReturnS_ x) =
  return $ x {poolExpr = lcDupPacket cfg (poolExpr x), poolReturnFlag = True}
lowerSerialExpr _ _ (ReturnS_ x) = return $ x {poolReturnFlag = True}
lowerSerialExpr cfg (LoopS _ _ origBody) (LoopS_ _ ids body) = do
  body' <- adaptLoopBodyOwned cfg (\_ d -> return d) ids origBody body
  lcMakeLoop cfg LoopResultSerial [(i, Nothing) | i <- ids] (Just (trimap id id (const ()) origBody)) body'
lowerSerialExpr cfg _ (LoopS_ _ ids body) =
  lcMakeLoop cfg LoopResultSerial [(i, Nothing) | i <- ids] Nothing body
lowerSerialExpr cfg (SerialLetS _ (SerializeS _ _) _) (SerialLetS_ i e1 e2) = do
  -- The let RHS is a SerializeS, so the bound variable owns a put_value
  -- tracker entry. Wrap the body to bind its result to a temp helper var,
  -- emit a release call against the let-bound variable, and expose the
  -- temp as the new pool expression. This makes the SHM ref's lifetime
  -- end exactly at the body's last use of the bound variable rather than
  -- carrying it to the outer dispatch boundary.
  letResult <- lcMakeLet cfg svarNamer i Nothing Nothing e1 e2
  if bodyIsBoundVar i e2
    then return letResult
    else do
      tmpIdx <- lcNewIndex cfg
      let releaseLine = lcReleaseStmt cfg (render (svarNamer i))
          releaseBody =
            defaultValue
              { poolExpr = helperNamer tmpIdx
              , poolPriorLines = [releaseLine]
              }
      lcMakeLet cfg helperNamer tmpIdx Nothing Nothing letResult releaseBody
lowerSerialExpr cfg _ (SerialLetS_ i e1 e2) =
  lcMakeLet cfg svarNamer i Nothing Nothing e1 e2
-- As for a native let ('NativeLetN_'): the bound local is owned, so its RHS
-- is an owned sink.
lowerSerialExpr cfg (NativeLetS _ rhsE _) (NativeLetS_ i e1 e2) = do
  e1' <- adaptOwnedElem cfg rhsE e1 >>= keepRhs cfg rhsE >>= lcLetRhs cfg i rhsE
  lcMakeLet cfg nvarNamer i (Just (typeFof rhsE)) (Just rhsE) e1' e2
lowerSerialExpr _ _ (NativeLetS_ i _ _) =
  error ("morloc bug: native let " <> show i <> " lowered without its original")
lowerSerialExpr _ _ (LetVarS_ _ i) = return $ defaultValue {poolExpr = svarNamer i}
lowerSerialExpr _ _ (BndVarS_ _ i) = return $ defaultValue {poolExpr = svarNamer i}
lowerSerialExpr cfg (SerializeS _ origE) (SerializeS_ s e) = do
  -- A leaf is serialized by reading it through 'put_value''s '&T', and a
  -- packed value by an unpacker that borrows it, so a borrowed Rust value
  -- (e.g. a '&Vec' parameter) is reborrowed rather than referenced ('&&Vec'
  -- is not 'ToVoidstar') or cloned. Any other aggregate may hand its parts to
  -- code that takes them by value, so it is owned first. Both are no-ops
  -- where the adapters are identity (C++/py/r).
  e' <- if readsInPlace s
    then adaptReadElem cfg origE e
    else adaptOwnedElem cfg origE e
  se <- lcSerialize cfg (poolExpr e') s
  return $ e' {poolExpr = poolExpr se, poolPriorLines = poolPriorLines e' <> poolPriorLines se}
  where
    readsInPlace s'@(SerialPack _ (p, _)) =
      isMsgpackLeaf cfg s' || lcBorrowPackArg cfg (typePackerPacked p)
    readsInPlace s' = isMsgpackLeaf cfg s'
lowerSerialExpr cfg _ (SerializeS_ s e) = do
  se <- lcSerialize cfg (poolExpr e) s
  return $ e {poolExpr = poolExpr se, poolPriorLines = poolPriorLines e <> poolPriorLines se}

-- | True when a native expression lowers to a reference into a live variable:
-- a bracket-free, single-path field/index projection (@.field@ / @.N@ chains)
-- ultimately rooted at a bound variable. Binding such a RHS by const-reference
-- (in a statically typed member that does no last-use moves) avoids copying the
-- projected sub-value out of its container; the reference stays valid because
-- the root variable is single-assignment and never moved.
--
-- Bracket steps (@.[i]@ / slices) build new values (they call @morloc_at@ /
-- @morloc_slice@, which return by value), so a selector containing one is
-- excluded. Multi-sibling selectors materialise a fresh tuple, so only a
-- single-leaf selector qualifies. A receiver that is a call result (a
-- temporary) is excluded -- a reference into it would dangle.
--
-- Reading one field out of a matched @data@ value (@IntrCtorField@) is the
-- same shape and qualifies on the same terms: the field lives in a block the
-- subject owns, the read materialises nothing, and the subject outlives the
-- alias. It is by far the most common projection in a program that handles
-- failure, since every fallible operation yields a @Try@ whose payload is
-- reached this way.
--
-- NOTE for zero-copy Vector work: when the root variable is a zero-copy view
-- into incoming-packet SHM, the const& alias holds a reference into that SHM.
-- Safe today because the packet's SHM is released only at the dispatch
-- boundary, after the manifold body (put_value-tracked serialize-lets pass
-- @borrowSafe = False@). If a future path frees a value's backing SHM mid-body,
-- revisit whether that value's fields may be borrowed.
isBorrowableProjection :: NativeExpr -> Bool
isBorrowableProjection (AppExeN _ (PatCallP (PatternStruct sel)) [NativeArgExpr arg]) =
  not (selectorHasBracket sel)
    && length (ungroup sel) == 1
    && borrowableRoot arg
isBorrowableProjection (IntrinsicN _ IntrCtorField _ (arg : _)) = borrowableRoot arg
isBorrowableProjection _ = False

-- | A projection root that a reference may point into: a variable, or another
-- projection of one.
--
-- INVARIANT, load-bearing rather than incidental: a member may bind an alias
-- to such a root but must never take the root's storage. Every native
-- manifold parameter is passed by const reference and a loop-carried local is
-- a manifold parameter too, so a frame that moved out of a @BndVarN@ would
-- empty its caller's value, or the next iteration's. The frame-local
-- 'LetVarN' is the only root a member may consider consuming, and even then
-- only under 'consumableLetRoot'.
borrowableRoot :: NativeExpr -> Bool
borrowableRoot (LetVarN _ _ _) = True
borrowableRoot (BndVarN _ _ _) = True
borrowableRoot e = isBorrowableProjection e

-- | The let indices whose constructor-payload projection may TAKE the payload
-- out of its subject rather than copy it or alias it.
--
-- Taking a payload is not, in general, an operation on a frame-local. A
-- generated @data@ value holds its constructor's fields behind a shared
-- pointer, so copying the value shares the fields; emptying one through a
-- name that is dead can empty a value this frame never owned. Counting a
-- name's uses therefore proves nothing. This pass fires only where uniqueness
-- is a fact about how the subject was BUILT:
--
--   * the subject is a frame-local let, never a parameter -- see the
--     invariant on 'borrowableRoot';
--   * the subject's own right-hand side constructs a fresh value: a @\@try@
--     result or a constructor literal, each of which allocates its payload on
--     the spot and hands back the only reference to it;
--   * the projection is the subject's only use that could read or share that
--     payload on the path that runs it. A tag test reads the discriminant
--     and never the payload, and an arm that ends in a throw returns
--     nothing, so neither is counted, and of a conditional's two arms only
--     the one that uses the subject more is. Every other mention -- a
--     second projection, a capture, a pass to a call -- disqualifies it;
--   * walking the manifold in evaluation order, nothing names the subject
--     after the projection on the path that runs it (a throw's argument may
--     read it), and no suspension or closure names the subject or holds the
--     projection of a subject bound outside it, since it may run later or
--     more than once. @tagAfterMove@ allows a tag test after the
--     projection; a member sets it when its projection leaves the
--     discriminant readable.
--
-- A member consuming this set must render a bare-throw arm as non-returning,
-- which is what makes the throw exemption sound.
-- A let index names one binding within a manifold, not within a pool: two
-- manifolds routinely bind the same index to different values. So each
-- top-level manifold -- with every manifold nested in it -- is scanned as one
-- body, and a decision is keyed by that manifold's id as well as the let's
-- index; the translator asks with the id of the manifold it is lowering. A
-- scan merges the uses of any index two of its nested manifolds share, which
-- can only make a candidate look more used than it is -- costing a copy,
-- never permitting a take.
--
-- A let index that two projections in one scan share is never a candidate:
-- the decision is keyed by the index, so it would also reach the projection
-- the conditions were not checked for.
consumableProjectionLets :: Bool -> [SerialManifold] -> Set.Set (Int, Int)
consumableProjectionLets tagAfterMove sms =
  Set.fromList
    [ (root, letIdx)
    | sm@(SerialManifold root _ _ _ body) <- sms
    , let scan = scanSM sm
          projected = Map.fromListWith (+) [(l, 1 :: Int) | (l, _) <- csProjections scan]
    , (letIdx, subjIdx) <- csProjections scan
    , Map.lookup letIdx projected == Just 1
    , maybe False buildsFreshValue (Map.lookup subjIdx (csDefs scan))
    , Map.findWithDefault 0 subjIdx (csUses scan) == 1
    , isJust (moveWalk tagAfterMove subjIdx letIdx False (RS body))
    ]

-- | A node of the IR, for walks that cross between its kinds.
data RNode
  = RS SerialExpr
  | RN NativeExpr
  | RSM SerialManifold
  | RNM NativeManifold
  | RLS (LoopBody NativeExpr SerialExpr SerialExpr)
  | RLN (LoopBody NativeExpr SerialExpr NativeExpr)

-- | A node's children, in the order the generated code evaluates them.
rnodeChildren :: RNode -> [RNode]
rnodeChildren (RSM (SerialManifold _ _ _ _ e)) = [RS e]
rnodeChildren (RNM (NativeManifold _ _ _ e)) = [RN e]
rnodeChildren (RS e) = case e of
  ManS sm -> [RSM sm]
  AppPoolS _ _ args -> map serialArg args
  AppRecS _ _ es -> map RS es
  AppForeignRecS _ _ _ es -> map RS es
  CacheBodyS _ _ _ _ _ x -> [RS x]
  DebugWrapS _ _ _ x -> [RS x]
  ReturnS x -> [RS x]
  SerialLetS _ a b -> [RS a, RS b]
  NativeLetS _ a b -> [RN a, RS b]
  LetVarS _ _ -> []
  BndVarS _ _ -> []
  SerializeS _ x -> [RN x]
  LoopS _ _ body -> [RLS body]
  where
    serialArg (SerialArgManifold sm) = RSM sm
    serialArg (SerialArgExpr x) = RS x
rnodeChildren (RN e) = case e of
  ManN nm -> [RNM nm]
  AppExeN _ _ args -> map nativeArg args
  ReturnN x -> [RN x]
  SerialLetN _ a b -> [RS a, RN b]
  NativeLetN _ a b -> [RN a, RN b]
  LetVarN _ _ _ -> []
  BndVarN _ _ _ -> []
  DeserializeN _ _ x -> [RS x]
  ExeN _ _ -> []
  ListN _ _ xs -> map RN xs
  TupleN _ xs -> map RN xs
  RecordN _ _ _ kvs -> map (RN . snd) kvs
  LogN _ _ -> []
  RealN _ _ -> []
  IntN _ _ -> []
  StrN _ _ -> []
  EnumN _ _ _ -> []
  VariantN _ _ _ xs -> map RN xs
  NullN _ -> []
  DoBlockN _ x -> [RN x]
  EvalN _ x -> [RN x]
  CoerceN _ _ x -> [RN x]
  IfN _ c t f -> [RN c, RN t, RN f]
  IntrinsicN _ _ _ xs -> map RN xs
  MapOptionalN _ _ _ x -> [RN x]
  LoopN _ starts body -> map (RN . snd) starts <> [RLN body]
  where
    nativeArg (NativeArgManifold nm) = RNM nm
    nativeArg (NativeArgExpr x) = RN x
rnodeChildren (RLS b) = loopChildren RS RLS b
rnodeChildren (RLN b) = loopChildren RN RLN b

loopChildren :: (b -> RNode) -> (LoopBody NativeExpr SerialExpr b -> RNode) -> LoopBody NativeExpr SerialExpr b -> [RNode]
loopChildren _ wrap (LoopIf c t f) = [RN c, wrap t, wrap f]
loopChildren _ wrap (LoopNLet _ a b) = [RN a, wrap b]
loopChildren _ wrap (LoopSLet _ a b) = [RS a, wrap b]
loopChildren base _ (LoopBase x) = [base x]
loopChildren _ _ (LoopContinue xs) = map RN xs

-- | The native variable a let binds, its right-hand side and its body.
rnodeLet :: RNode -> Maybe (Int, RNode, RNode)
rnodeLet (RN (NativeLetN i a b)) = Just (i, RN a, RN b)
rnodeLet (RS (NativeLetS i a b)) = Just (i, RN a, RS b)
rnodeLet (RLS (LoopNLet i a b)) = Just (i, RN a, RLS b)
rnodeLet (RLN (LoopNLet i a b)) = Just (i, RN a, RLN b)
rnodeLet _ = Nothing

-- | Whether a node names native variable @i@: a reference, or a manifold that
-- captures it.
rnodeNames :: Int -> RNode -> Bool
rnodeNames i n = here n || any (rnodeNames i) (rnodeChildren n)
  where
    here (RN (LetVarN _ j _)) = i == j
    here (RN (BndVarN _ j _)) = i == j
    here (RSM (SerialManifold _ _ form _ _)) = any (\(Arg j _) -> j == i) (manifoldContext form)
    here (RNM (NativeManifold _ _ form _)) = any (\(Arg j _) -> j == i) (manifoldContext form)
    here _ = False

-- | Whether a let binding native variable @i@ lies under a node.
rnodeBinds :: Int -> RNode -> Bool
rnodeBinds i n = maybe False (\(j, _, _) -> i == j) (rnodeLet n) || any (rnodeBinds i) (rnodeChildren n)

-- | Walk a node in evaluation order, knowing whether subject @subj@ has been
-- moved by projection let @k@: the state after it, or Nothing when the
-- subject is named after the move, or moved from inside a closure that did
-- not bind it. With @tagAfterMove@, a tag test may read the subject after
-- the move.
moveWalk :: Bool -> Int -> Int -> Bool -> RNode -> Maybe Bool
moveWalk tagAfterMove subj k moved n = case n of
  RN (IntrinsicN _ IntrTagTest _ (LetVarN _ j _ : rest))
    | tagAfterMove && j == subj -> foldM (moveWalk tagAfterMove subj k) moved (map RN rest)
  RN (LetVarN _ j _) | j == subj -> if moved then Nothing else Just moved
  RN (BndVarN _ j _) | j == subj -> if moved then Nothing else Just moved
  RN (IfN _ c t f) -> branch (RN c) (RN t) (RN f)
  RLS (LoopIf c t f) -> branch (RN c) (RLS t) (RLS f)
  RLN (LoopIf c t f) -> branch (RN c) (RLN t) (RLN f)
  RN (DoBlockN _ _) -> closure
  RNM _ -> closure
  RSM _ -> closure
  _ -> case rnodeLet n of
    Just (i, rhs, body)
      | i == k -> moveWalk tagAfterMove subj k moved rhs >> moveWalk tagAfterMove subj k True body
      | i == subj -> moveWalk tagAfterMove subj k moved rhs >> moveWalk tagAfterMove subj k False body
      | otherwise -> moveWalk tagAfterMove subj k moved rhs >>= \m -> moveWalk tagAfterMove subj k m body
    Nothing -> foldM (moveWalk tagAfterMove subj k) moved (rnodeChildren n)
  where
    branch c t f = do
      m <- moveWalk tagAfterMove subj k moved c
      mt <- moveWalk tagAfterMove subj k m t
      mf <- moveWalk tagAfterMove subj k m f
      return (mt || mf)
    closure
      | rnodeBinds k n = if rnodeBinds subj n then foldM (moveWalk tagAfterMove subj k) moved (rnodeChildren n) else Nothing
      | rnodeNames subj n = Nothing
      | otherwise = Just moved

-- | Mark the last reads of the values a native loop owns, carried or bound
-- in the iteration (OWN-2, OWN-3, OWN-4).
markLastReads :: (MFunctor a) => a -> a
markLastReads = markLastReadsWith Map.empty Set.empty

-- | 'markLastReads' where a local may refer into another value: @aliases@
-- maps such a local to the value it refers into (OWN-2), and no read of a
-- value in @held@ is marked.
markLastReadsWith :: (MFunctor a) => Map.Map Int Int -> Set.Set Int -> a -> a
markLastReadsWith aliases held = markLastReadsSeeded aliases held (const [])

-- | 'markLastReadsWith' where native manifold @m@ also owns the parameters
-- @params m@ (OWN-7).
markLastReadsSeeded :: (MFunctor a) => Map.Map Int Int -> Set.Set Int -> (Int -> [Int]) -> a -> a
markLastReadsSeeded aliases held params =
  mmap (defaultValue {mapSerialExpr = onS, mapNativeExpr = onN, mapSerialManifold = onSM, mapNativeManifold = onNM})
  where
    mk = markers aliases held params
    onS e@(LoopS t ids b) = maybe e (LoopS t ids . fst) (mkLoopS mk ids b)
    onS e = e
    onN e@(LoopN t starts b) = maybe e (LoopN t starts . fst) (mkLoopN mk (map fst starts) b)
    onN e = e
    onSM (SerialManifold m l f h e) = SerialManifold m l f h (fst (mkSerial mk e))
    onNM (NativeManifold m l f e) = NativeManifold m l f (fst (mkNative mk m e))

-- | How one body is marked, for 'markLastReadsSeeded' and 'lastBuilds' alike.
data Markers = Markers
  { mkNative :: Int -> NativeExpr -> (NativeExpr, [(Int, Int)])
  , mkSerial :: SerialExpr -> (SerialExpr, [(Int, Int)])
  , mkLoopN :: [Int] -> LoopBody NativeExpr SerialExpr NativeExpr -> Maybe (LoopBody NativeExpr SerialExpr NativeExpr, [(Int, Int)])
  , mkLoopS :: [Int] -> LoopBody NativeExpr SerialExpr SerialExpr -> Maybe (LoopBody NativeExpr SerialExpr SerialExpr, [(Int, Int)])
  }

markers :: Map.Map Int Int -> Set.Set Int -> (Int -> [Int]) -> Markers
markers aliases held params =
  Markers
    { mkNative = \m e ->
        let en = env (RN e)
            (e', (_, caps)) = markSpineN en (filter (`Set.notMember` meHeld en) (params m)) [] e
         in (e', caps)
    , mkSerial = \e -> let (e', (_, caps)) = markSpineS (env (RS e)) [] e in (e', caps)
    , mkLoopN = \ids b -> Just (markLoopBody (env (RLN b)) RN ids b)
    , mkLoopS = \ids b -> Just (markLoopBody (env (RLS b)) RS ids b)
    }
  where
    env n = let e = markEnv aliases n in e {meHeld = meHeld e <> held}

-- | The (manifold, value) pairs under some segments whose building is the
-- value's last read at every place the manifold is built (OWN-3), marked as
-- 'markLastReadsSeeded' marks them.
lastBuilds :: Map.Map Int Int -> Set.Set Int -> (Int -> [Int]) -> [SerialManifold] -> Set.Set (Int, Int)
lastBuilds aliases held params sms =
  Set.fromList [mv | (mv, k) <- Map.toList found, Just k == Map.lookup (fst mv) sites]
  where
    mk = markers aliases held params
    found = Map.fromListWith (+) [(mv, 1 :: Int) | sm <- sms, mv <- concatMap bodyBuilds (nodes (RSM sm))]
    sites = Map.fromListWith (+) [(m, 1 :: Int) | sm <- sms, RNM (NativeManifold m _ _ _) <- nodes (RSM sm)]
    nodes n = n : concatMap nodes (rnodeChildren n)
    bodyBuilds n = case n of
      RNM (NativeManifold m _ _ e) -> snd (mkNative mk m e)
      RSM (SerialManifold _ _ _ _ e) -> snd (mkSerial mk e)
      RN (LoopN _ starts b) -> maybe [] snd (mkLoopN mk (map fst starts) b)
      RS (LoopS _ ids b) -> maybe [] snd (mkLoopS mk ids b)
      _ -> []

-- | What marking a body needs: the locals whose reads are reads of a value,
-- and the values a @do@ block bound by a let reads, which none of their
-- reads may move (OWN-3).
data MarkEnv = MarkEnv
  { meNames :: Int -> [Int]
  , meHeld :: Set.Set Int
  }

markEnv :: Map.Map Int Int -> RNode -> MarkEnv
markEnv aliases n = MarkEnv (namesOf aliases) (Set.fromList [t | j <- Set.toList (heldReads n), t <- j : aliasChain aliases j])

-- | The variables read in the @do@ blocks a let binds under a node, a block
-- the let's value holds through a conditional's arm, a nested let or a
-- container included.
heldReads :: RNode -> Set.Set Int
heldReads n = here n <> Set.unions (map heldReads (rnodeChildren n))
  where
    here m = case m of
      RN (NativeLetN _ rhs _) -> blocks rhs
      RS (NativeLetS _ rhs _) -> blocks rhs
      RLN (LoopNLet _ rhs _) -> blocks rhs
      RLS (LoopNLet _ rhs _) -> blocks rhs
      _ -> Set.empty
    blocks e = case e of
      DoBlockN _ x -> namedIn (RN x)
      IfN _ _ a b -> blocks a <> blocks b
      NativeLetN _ _ b -> blocks b
      TupleN _ xs -> foldMap blocks xs
      RecordN _ _ _ kvs -> foldMap (blocks . snd) kvs
      VariantN _ _ _ xs -> foldMap blocks xs
      ListN _ _ xs -> foldMap blocks xs
      CoerceN _ _ x -> blocks x
      _ -> Set.empty

-- | Every native variable named under a node, the context arguments of the
-- manifolds under it included.
namedIn :: RNode -> Set.Set Int
namedIn n = here n <> Set.unions (map namedIn (rnodeChildren n))
  where
    here m = case m of
      RN (LetVarN _ j _) -> Set.singleton j
      RN (BndVarN _ j _) -> Set.singleton j
      RNM (NativeManifold _ _ f _) -> Set.fromList [i | Arg i _ <- manifoldContext f]
      RSM (SerialManifold _ _ f _ _) -> Set.fromList [i | Arg i _ <- manifoldContext f]
      _ -> Set.empty

-- | 'markLastReads' over one serial body, with no aliases.
markLastReadsInBody :: SerialExpr -> SerialExpr
markLastReadsInBody e = fst (markSpineS (markEnv Map.empty (RS e)) [] e)

-- | 'markLastReads' over one native body, with no aliases.
markLastReadsInNative :: NativeExpr -> NativeExpr
markLastReadsInNative e = fst (markSpineN (markEnv Map.empty (RN e)) [] [] e)

-- | What marking a body found besides its marks: the reads of owned values
-- it makes, and the (manifold, value) pairs whose building is the value's
-- last read (OWN-3).
type Found = (Map.Map Int Int, [(Int, Int)])

-- | A let binding @i@ to a right-hand side, over a body the continuation
-- marks: @i@ is owned in the body unless a @do@ block holds it. The
-- right-hand side is marked last, given the reads of the body.
markLet :: MarkEnv -> [Int] -> Int -> (Map.Map Int Int -> (x, Found)) -> ([Int] -> (b, Found)) -> ((x, b), Found)
markLet env owned i rhs body =
  let owned' = if Set.member i (meHeld env) then owned else i : owned
      (body', (after, cb)) = body owned'
      rest = Map.delete i after
      (rhs', (here, cr)) = rhs rest
   in ((rhs', body'), (Map.unionWith (+) here rest, cr <> cb))

-- | Mark the last reads of the native lets of a body along its spine of
-- lets and conditionals, every right-hand side a spine of its own: a step is
-- a conditional's test or the value a spine ends in (OWN-2). @owned@ holds
-- the lets in scope and @later@ the reads that follow the body; the result
-- pairs the marked body with what it found.
markSpineN :: MarkEnv -> [Int] -> [Map.Map Int Int] -> NativeExpr -> (NativeExpr, Found)
markSpineN env owned later e = case e of
  NativeLetN i rhs body ->
    let ((rhs', body'), r) = markLet env owned i (\rest -> markSpineN env owned (rest : later) rhs) (\o -> markSpineN env o later body)
     in (NativeLetN i rhs' body', r)
  IfN t c a b ->
    let (a', (ma, ca)) = markSpineN env owned later a
        (b', (mb, cb)) = markSpineN env owned later b
        arms = Map.unionWith max ma mb
        here = stepReads env owned (RN c)
        (c', cc) = markStep here (arms : later) c
     in (IfN t c' a' b', (Map.unionWith (+) here arms, cc <> ca <> cb))
  _ ->
    let here = stepReads env owned (RN e)
        (e', ce) = markStep here later e
     in (e', (here, ce))

markSpineS :: MarkEnv -> [Int] -> SerialExpr -> (SerialExpr, Found)
markSpineS env owned e = case e of
  NativeLetS i rhs body ->
    let ((rhs', body'), r) = markLet env owned i (\rest -> markSpineN env owned [rest] rhs) (\o -> markSpineS env o body)
     in (NativeLetS i rhs' body', r)
  SerialLetS i rhs body ->
    let (body', (after, cb)) = markSpineS env owned body
        here = stepReads env owned (RS rhs)
        (rhs', cr) = markStep here [after] rhs
     in (SerialLetS i rhs' body', (Map.unionWith (+) here after, cr <> cb))
  ReturnS x -> let (x', r) = markSpineS env owned x in (ReturnS x', r)
  SerializeS ast x -> let (x', r) = markSpineN env owned [] x in (SerializeS ast x', r)
  DebugWrapS t m args x -> let (x', r) = markSpineS env owned x in (DebugWrapS t m args x', r)
  CacheBodyS t ast lbl m args x -> let (x', r) = markSpineS env owned x in (CacheBodyS t ast lbl m args x', r)
  _ ->
    let here = stepReads env owned (RS e)
        (e', ce) = markStep here [] e
     in (e', (here, ce))

-- | Reads of owned values in one step, a read through an alias counting for
-- every value its chain passes through.
stepReads :: MarkEnv -> [Int] -> RNode -> Map.Map Int Int
stepReads env owned n = Map.fromList [(v, k) | v <- owned, let k = sum [Map.findWithDefault 0 j counts | j <- meNames env v], k > 0]
  where
    counts = readCounts n

-- | Mark, in a step, the reads of the values it reads once and nothing reads
-- after it, and find the manifolds the step builds whose building is such a
-- read.
markStep :: (LastReadIn a) => Map.Map Int Int -> [Map.Map Int Int] -> a -> (a, [(Int, Int)])
markStep here later x =
  let vs = lastReadsOf here later
   in (foldr lastReadIn x vs, [(m, v) | (m, ctx) <- builtIn (asNode x), v <- ctx, v `elem` vs])

-- | The values a step reads once and nothing reads after it.
lastReadsOf :: Map.Map Int Int -> [Map.Map Int Int] -> [Int]
lastReadsOf here later = [v | (v, 1) <- Map.toList here, all (Map.notMember v) later]

-- | The native manifolds a step builds, with the locals each reads to be
-- built: none inside a body that may run more than once.
builtIn :: RNode -> [(Int, [Int])]
builtIn n = case n of
  RNM (NativeManifold m _ f _) -> [(m, [i | Arg i _ <- manifoldContext f])]
  RSM _ -> []
  RN (DoBlockN _ _) -> []
  RN (LoopN _ starts _) -> concatMap (builtIn . RN . snd) starts
  RS (LoopS {}) -> []
  _ -> concatMap builtIn (rnodeChildren n)

-- | The locals whose reads are reads of a value: the value and every local
-- whose chain of aliases passes through it. Built once per alias map.
namesOf :: Map.Map Int Int -> Int -> [Int]
namesOf aliases = \v -> v : Map.findWithDefault [] v referrers
  where
    referrers = Map.fromListWith (<>) [(t, [a]) | a <- Map.keys aliases, t <- aliasChain aliases a]

-- | The values a local refers into, nearest first, stopping at a cycle.
aliasChain :: Map.Map Int Int -> Int -> [Int]
aliasChain aliases a = follow (Set.singleton a) (Map.lookup a aliases)
  where
    follow seen (Just t) | not (Set.member t seen) = t : follow (Set.insert t seen) (Map.lookup t aliases)
    follow _ _ = []

markLoopBody ::
  (LastReadIn b) =>
  MarkEnv ->
  (b -> RNode) ->
  [Int] ->
  LoopBody NativeExpr SerialExpr b ->
  (LoopBody NativeExpr SerialExpr b, [(Int, Int)])
markLoopBody env baseNode carried b0 = let (b', (_, caps)) = go (filter (`Set.notMember` meHeld env) carried) b0 in (b', caps)
  where
    go owned (LoopBase x) =
      let here = stepReads env owned (baseNode x)
          (x', cx) = markStep here [] x
       in (LoopBase x', (here, cx))
    go owned (LoopContinue xs) =
      let total = Map.unionsWith (+) (map (stepReads env owned . RN) xs)
          marked = map (markStep total []) xs
       in (LoopContinue (map fst marked), (total, concatMap snd marked))
    go owned (LoopNLet i x b) =
      let ((x', b'), r) = markLet env owned i (\rest -> markSpineN env owned [rest] x) (\o -> go o b)
       in (LoopNLet i x' b', r)
    go owned (LoopSLet i x b) =
      let (b', (after, cb)) = go owned b
          here = stepReads env owned (RS x)
          (x', cx) = markStep here [after] x
       in (LoopSLet i x' b', (Map.unionWith (+) here after, cx <> cb))
    go owned (LoopIf g t e) =
      let (t', (mt, ct)) = go owned t
          (e', (me, ce)) = go owned e
       in (LoopIf g t' e', (Map.unionWith (+) (stepReads env owned (RN g)) (Map.unionWith max mt me), ct <> ce))

-- | How many times native variable @v@ is read under a node, where a read
-- that may run more than once counts as two.
readCount :: Int -> RNode -> Int
readCount v n = Map.findWithDefault 0 v (readCounts n)

-- | 'readCount' for every native variable read under a node.
readCounts :: RNode -> Map.Map Int Int
readCounts n = case n of
  RN (LetVarN _ j _) -> Map.singleton j 1
  RN (BndVarN _ j _) -> Map.singleton j 1
  RN (IfN _ c a b) -> Map.unionWith (+) (readCounts (RN c)) (Map.unionWith max (readCounts (RN a)) (readCounts (RN b)))
  RN (DoBlockN _ _) -> repeated n
  RN (LoopN _ starts body) -> Map.unionsWith (+) (repeated (RLN body) : [readCounts (RN x) | (_, x) <- starts])
  RS (LoopS {}) -> repeated n
  RNM (NativeManifold _ _ f body) -> Map.unionWith (+) (Map.fromListWith (+) [(i, 1) | Arg i _ <- manifoldContext f]) (Map.fromSet (const 2) (outside f body))
  RSM _ -> repeated n
  _ -> Map.unionsWith (+) (map readCounts (rnodeChildren n))
  where
    repeated m = Map.fromSet (const 2) (namedIn m)
    -- a native manifold reads each context argument once, when it is built
    -- (OWN-3); a local its body names that is neither one of those nor its
    -- own counts as read any number of times
    outside f body = namedIn (RN body) `Set.difference` (Set.fromList [i | Arg i _ <- typeMofForm f] <> boundIn (RN body))
    boundIn m = Set.fromList (maybe [] (\(i, _, _) -> [i]) (rnodeLet m) <> slots m) <> Set.unions (map boundIn (rnodeChildren m))
    slots (RN (LoopN _ starts _)) = map fst starts
    slots (RS (LoopS _ ids _)) = ids
    slots (RNM (NativeManifold _ _ g _)) = [i | Arg i _ <- typeMofForm g]
    slots (RSM (SerialManifold _ _ g _ _)) = [i | Arg i _ <- typeMofForm g]
    slots _ = []

-- | Mark the reads of native variable @v@ under a node as its last, leaving
-- closures, manifolds and nested loops alone.
class LastReadIn a where
  lastReadIn :: Int -> a -> a
  asNode :: a -> RNode

instance LastReadIn NativeExpr where
  lastReadIn v = mgatedMap lastReadGate (lastReadMap v)
  asNode = RN

instance LastReadIn SerialExpr where
  lastReadIn v = mgatedMap lastReadGate (lastReadMap v)
  asNode = RS

lastReadGate :: GateMap
lastReadGate =
  defaultValue
    { gateSerialManifold = const False
    , gateNativeManifold = const False
    , gateSerialExpr = \e -> case e of
        LoopS {} -> False
        _ -> True
    , gateNativeExpr = \e -> case e of
        DoBlockN {} -> False
        _ -> True
    }

lastReadMap :: Int -> ManifoldMap
lastReadMap v = defaultValue {mapNativeExpr = mark}
  where
    mark (LetVarN t j _) | j == v = LetVarN t j LastRead
    mark (BndVarN t j _) | j == v = BndVarN t j LastRead
    mark e = e

-- | Whether an expression is a read of a native variable marked as its last.
isLastRead :: NativeExpr -> Bool
isLastRead (LetVarN _ _ LastRead) = True
isLastRead (BndVarN _ _ LastRead) = True
isLastRead _ = False

-- | The locals under a manifold that hold a borrow rather than a value
-- (OWN-6), given the manifold's borrowed parameters and which types may be
-- borrowed. A loop's carried local qualifies when it starts from a borrow
-- and every continue replaces it with part of one; a let, in a loop or not,
-- qualifies when it names part of one. A part is the value itself, a
-- constructor field or a record field. Nested manifolds are left to their
-- own scope, and an index bound more than once under the manifold never
-- qualifies.
loopBorrowLocals :: (TypeF -> Bool) -> Set.Set Int -> RNode -> Set.Set Int
loopBorrowLocals mayBorrow g root = go g root
  where
    go env n = case n of
      RN (NativeLetN i rhs body)
        | not (Set.member i reused)
        , isJust (borrowPartOf mayBorrow (`Set.member` env) rhs) ->
            Set.insert i (go env (RN rhs) <> go (Set.insert i env) (RN body))
      RN (LoopN _ starts body) ->
        let b = loopBorrows mayBorrow env reused starts body
         in b <> mconcat (map (go (env <> b)) (filter (not . isManifold) (rnodeChildren n)))
      _ -> mconcat (map (go env) (filter (not . isManifold) (rnodeChildren n)))
    reused = Map.keysSet (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(i, 1) | i <- bindings root]))
    bindings n = loopIds n <> maybe [] (\(i, _, _) -> [i]) (rnodeLet n) <> concatMap bindings (rnodeChildren n)
    loopIds (RN (LoopN _ starts _)) = map fst starts
    loopIds (RS (LoopS _ ids _)) = ids
    loopIds _ = []
    isManifold (RNM _) = True
    isManifold (RSM _) = True
    isManifold _ = False

-- | The candidate parameters OWN-7 passes by value. The sinks are found
-- only on the part of the body every evaluation runs.
consumedParams :: Bool -> (Int -> Set.Set Int) -> (TypeF -> Bool) -> Set.Set Int -> NativeManifold -> Set.Set Int
consumedParams listKeeps byValue mayBorrow cands nm@(NativeManifold _ _ _ body) =
  Set.filter (\p -> Set.member p kept && Map.lookup p counts == Just 1) cands
  where
    counts = readCounts (RN body)
    kept = sinks (RN body)
    borrows = loopBorrowLocals mayBorrow cands (RNM nm)
    sinks n = case n of
      RN (IfN _ c _ _) -> sinks (RN c)
      RN (LoopN _ starts _) -> here n <> mconcat [sinks (RN x) | (_, x) <- starts]
      RN (DoBlockN _ _) -> Set.empty
      RNM (NativeManifold m _ form@(ManifoldFull _) _) -> Set.fromList [p | (j, Arg p _) <- zip [0 ..] (typeMofForm form), Set.member j (byValue m)]
      _ -> here n <> mconcat (map sinks (filter (not . isValueManifold) (rnodeChildren n)))
    here (RN e) = case e of
      LoopN _ starts _ -> Set.fromList [p | (s, BndVarN _ p _) <- starts, not (Set.member s borrows)]
      VariantN _ _ _ xs -> vars xs
      ListN _ _ xs | listKeeps -> vars xs
      TupleN _ xs -> vars xs
      RecordN _ _ _ kvs -> vars (map snd kvs)
      AppExeN _ (RecCallP mid _) args -> Set.fromList [p | (j, NativeArgExpr a) <- zip [0 ..] args, Just p <- [readOf a], Set.member j (byValue mid)]
      _ -> Set.empty
    here _ = Set.empty
    vars xs = Set.fromList [p | BndVarN _ p _ <- xs]
    -- a nested manifold reads a parameter under its caller's index
    readOf (BndVarN _ p _) = Just p
    readOf (LetVarN _ p _) = Just p
    readOf _ = Nothing
    -- a fully applied manifold is a call, whose arguments may keep (OWN-7)
    isValueManifold (RNM (NativeManifold _ _ ManifoldFull {} _)) = False
    isValueManifold (RNM _) = True
    isValueManifold (RSM _) = True
    isValueManifold _ = False

-- | Every native manifold under a node, nested ones included.
nativeManifoldsUnder :: RNode -> [NativeManifold]
nativeManifoldsUnder n = here <> concatMap nativeManifoldsUnder (rnodeChildren n)
  where
    here = case n of
      RNM nm -> [nm]
      _ -> []

loopBorrows :: (TypeF -> Bool) -> Set.Set Int -> Set.Set Int -> [(Int, NativeExpr)] -> LoopBody NativeExpr SerialExpr NativeExpr -> Set.Set Int
loopBorrows mayBorrow g reused starts body = settle (Set.fromList [i | (i, s) <- starts, borrowVar g s, not (Set.member i reused)])
  where
    ids = map fst starts
    settle b =
      let (lets, bad) = scan (Set.union g b) body
          b' = Set.difference b bad
       in if b' == b then Set.union b lets else settle b'
    scan env (LoopNLet i rhs k)
      | borrowPart env rhs && not (Set.member i reused) = let (l, bad) = scan (Set.insert i env) k in (Set.insert i l, bad)
      | otherwise = scan (Set.delete i env) k
    scan env (LoopSLet _ _ k) = scan env k
    scan env (LoopIf _ t e) = scan env t <> scan env e
    scan _ (LoopBase _) = (Set.empty, Set.empty)
    scan env (LoopContinue xs) = (Set.empty, Set.fromList [i | (i, x) <- zip ids xs, not (borrowPart env x)])

    borrowPart env e = isJust (borrowPartOf mayBorrow (`Set.member` env) e)

    borrowVar env x = maybe False (`Set.member` env) (varIndex x)

-- | The index a variable read reads.
varIndex :: NativeExpr -> Maybe Int
varIndex (LetVarN _ j _) = Just j
varIndex (BndVarN _ j _) = Just j
varIndex _ = Nothing

-- | An expression whose value is a part of a borrow (OWN-6), in the shape a
-- member renders it as a reference.
data BorrowPart
  = BorrowVar Int
  | BorrowCtorField TypeF BorrowPart Text Integer
  -- ^ the subject's type, the subject, the arm and the field
  | BorrowField Int [Either Int Text]
  -- ^ a getter of one path
  | BorrowIf BorrowTest BorrowPart BorrowPart
  -- ^ a test, then the two arms
  | BorrowLet Int BorrowPart BorrowPart
  | BorrowFail TypeF Text
  -- ^ the type that fails to be made, and the message

-- | A test a match makes on parts, without copying them.
data BorrowTest
  = TestTag TypeF BorrowPart Text
  -- ^ a variant subject's type, the subject and the arm tested for
  | TestAnd BorrowTest BorrowTest

-- | The part of a borrow an expression names, given which types may be
-- borrowed and which locals hold a borrow.
borrowPartOf :: (TypeF -> Bool) -> (Int -> Bool) -> NativeExpr -> Maybe BorrowPart
borrowPartOf mayBorrow = go
  where
    go env e
      | not (mayBorrow (typeFof e)) = Nothing
      | otherwise = case e of
          IntrinsicN _ IntrCtorField _ [s, StrN _ n, IntN _ k] -> (\p -> BorrowCtorField (typeFof s) p n k) <$> go env s
          AppExeN _ (PatCallP (PatternStruct sel)) [NativeArgExpr s]
            | not (selectorHasBracket sel), [path] <- ungroup sel -> (`BorrowField` path) <$> var env s
          IfN _ c a b
            | Just t <- test env c -> BorrowIf t <$> go env a <*> go env b
          NativeLetN i rhs body -> BorrowLet i <$> go env rhs <*> go (\j -> j == i || env j) body
          IntrinsicN t IntrThrow _ [StrN _ msg] -> Just (BorrowFail t msg)
          _ -> BorrowVar <$> var env e
    var env x = varIndex x >>= \j -> if env j then Just j else Nothing
    test env c = case c of
      IntrinsicN _ IntrTagTest _ [s, StrN _ n]
        | not (isEnum (typeFof s)) ->
            (\p -> TestTag (typeFof s) p n) <$> maybe (go env s) (Just . BorrowVar) (varIndex s)
      IfN _ c1 c2 (LogN _ False) -> TestAnd <$> test env c1 <*> test env c2
      NativeLetN i rhs body | varIndex body == Just i -> test env rhs
      _ -> Nothing
    isEnum EnumF {} = True
    isEnum _ = False

-- | What 'consumableProjectionLets' gathers in one pass: every native let's
-- right-hand side, every payload projection paired with the index it reads
-- from, and the uses of each index that could observe a payload.
data ConsumeScan = ConsumeScan
  { csDefs :: Map.Map Int NativeExpr
  , csProjections :: [(Int, Int)]
  , csUses :: Map.Map Int Int
  }

instance Semigroup ConsumeScan where
  a <> b =
    ConsumeScan
      -- Two bindings on one index cannot both be described, and whichever was
      -- kept would describe the other. Drop both: an index with no definition
      -- is never a candidate.
      (dropColliding (csDefs a) (csDefs b))
      (csProjections a <> csProjections b)
      (Map.unionWith (+) (csUses a) (csUses b))

instance Monoid ConsumeScan where
  mempty = ConsumeScan mempty mempty mempty

-- | The union of two definition maps with every key they share removed.
dropColliding :: Map.Map Int NativeExpr -> Map.Map Int NativeExpr -> Map.Map Int NativeExpr
dropColliding a b =
  Map.union (Map.difference a b) (Map.difference b a)

-- | A right-hand side that allocates its own payload and yields the only
-- reference to it.
buildsFreshValue :: NativeExpr -> Bool
buildsFreshValue (IntrinsicN _ IntrTry _ _) = True
buildsFreshValue (VariantN _ _ _ _) = True
-- A loop body keeps a right-hand side's own lets inside it (see the loop
-- seal in 'invertSerialManifold'); the value is the let's body.
buildsFreshValue (NativeLetN _ _ body) = buildsFreshValue body
buildsFreshValue (SerialLetN _ _ body) = buildsFreshValue body
buildsFreshValue _ = False

-- | A payload projection whose subject is named directly, as @(this let, the
-- index it reads)@. A projection through another projection is not a
-- candidate: its subject is an alias, and an alias says nothing about who
-- else holds the payload.
projectionOf :: Int -> NativeExpr -> [(Int, Int)]
projectionOf letIdx (IntrinsicN _ IntrCtorField _ (LetVarN _ subjIdx _ : _)) =
  [(letIdx, subjIdx)]
projectionOf _ _ = []

scanUse :: Int -> ConsumeScan
scanUse i = mempty {csUses = Map.singleton i 1}

scanDef :: Int -> NativeExpr -> ConsumeScan
scanDef i rhs = mempty {csDefs = Map.singleton i rhs}

scanLet :: Int -> NativeExpr -> ConsumeScan
scanLet i rhs = scanDef i rhs <> mempty {csProjections = projectionOf i rhs}

scanSM :: SerialManifold -> ConsumeScan
scanSM (SerialManifold _ _ form _ e) = scanContext form <> scanSE e

scanNM :: NativeManifold -> ConsumeScan
scanNM (NativeManifold _ _ form e) = scanContext form <> scanNE e

-- A nested manifold names its captures by the enclosing frame's indices, so
-- each one is a use there.
scanContext :: ManifoldForm (Or TypeS TypeF) a -> ConsumeScan
scanContext form = mconcat [scanUse i | Arg i _ <- manifoldContext form]

scanSE :: SerialExpr -> ConsumeScan
scanSE (ManS sm) = scanSM sm
scanSE (AppPoolS _ _ args) = mconcat (map scanSA args)
scanSE (AppRecS _ _ es) = mconcat (map scanSE es)
scanSE (AppForeignRecS _ _ _ es) = mconcat (map scanSE es)
scanSE (CacheBodyS _ _ _ _ _ e) = scanSE e
scanSE (DebugWrapS _ _ _ e) = scanSE e
scanSE (ReturnS e) = scanSE e
scanSE (SerialLetS _ e1 e2) = scanSE e1 <> scanSE e2
scanSE (NativeLetS i rhs body) = scanLet i rhs <> scanNE rhs <> scanSE body
scanSE (LetVarS _ _) = mempty
scanSE (BndVarS _ _) = mempty
scanSE (SerializeS _ e) = scanNE e
-- The carried ids are reassigned on every back edge, which is a use of each
-- on every iteration. They are manifold parameters and so never candidates,
-- but counting them keeps that a consequence of the scan rather than of where
-- the parameters happen to come from.
scanSE (LoopS _ carried body) = mconcat (map scanUse carried) <> scanLoop scanSE body

scanSA :: SerialArg -> ConsumeScan
scanSA (SerialArgManifold sm) = scanSM sm
scanSA (SerialArgExpr e) = scanSE e

scanNA :: NativeArg -> ConsumeScan
scanNA (NativeArgManifold nm) = scanNM nm
scanNA (NativeArgExpr e) = scanNE e

-- A loop-carried local is a manifold parameter, and a let inside a loop body
-- is emitted without consulting the binding decision, so a projection there
-- is recorded as a definition and a use but never as a candidate.
-- A loop let's projection is a candidate only when its subject is bound in
-- the same loop body: a subject bound before the loop is projected once per
-- iteration from one syntactic use, and taking its payload would leave the
-- next iteration an empty value.
scanLoop :: (b -> ConsumeScan) -> LoopBody NativeExpr SerialExpr b -> ConsumeScan
scanLoop scanB lbody =
  let inner = go lbody
   in inner {csProjections = [p | p@(_, subj) <- csProjections inner, Map.member subj (csDefs inner)]}
  where
    go (LoopIf e t f) = scanNE e <> eitherArm (go t) (go f)
    go (LoopNLet i rhs body) = scanLet i rhs <> scanNE rhs <> go body
    go (LoopSLet _ e body) = scanSE e <> go body
    go (LoopBase e) = scanB e
    go (LoopContinue es) = mconcat (map scanNE es)

scanNE :: NativeExpr -> ConsumeScan
scanNE (ManN nm) = scanNM nm
scanNE (AppExeN _ _ args) = mconcat (map scanNA args)
scanNE (ReturnN e) = scanNE e
scanNE (SerialLetN _ e1 e2) = scanSE e1 <> scanNE e2
scanNE (NativeLetN i rhs body) = scanLet i rhs <> scanNE rhs <> scanNE body
scanNE (LetVarN _ i _) = scanUse i
scanNE (BndVarN _ i _) = scanUse i
scanNE (DeserializeN _ _ e) = scanSE e
scanNE (LoopN _ starts lbody) = mconcat (map (scanUse . fst) starts) <> mconcat (map (scanNE . snd) starts) <> scanLoop scanNE lbody
scanNE (ExeN _ _) = mempty
scanNE (ListN _ _ es) = mconcat (map scanNE es)
scanNE (TupleN _ es) = mconcat (map scanNE es)
scanNE (RecordN _ _ _ kvs) = mconcat (map (scanNE . snd) kvs)
scanNE (LogN _ _) = mempty
scanNE (RealN _ _) = mempty
scanNE (IntN _ _) = mempty
scanNE (StrN _ _) = mempty
scanNE (EnumN _ _ _) = mempty
scanNE (VariantN _ _ _ es) = mconcat (map scanNE es)
scanNE (NullN _) = mempty
scanNE (DoBlockN _ e) = scanNE e
scanNE (EvalN _ e) = scanNE e
scanNE (CoerceN _ _ e) = scanNE e
-- An arm that ends in a throw returns nothing, so it is not counted; whether
-- it runs after the projection is 'moveWalk''s question. Only one arm runs,
-- so an index is used as often as the arm that uses it most.
scanNE (IfN _ c t f) = scanNE c <> eitherArm (scanArm t) (scanArm f)
-- A tag test reads the discriminant, never the payload, so naming a variable
-- here does not make its payload observable. The discriminant is the
-- variant's own index and lives outside the pointer the fields hang off, so
-- it still reads correctly after a field has been taken -- a tag test that
-- runs after the projection answers the same as one that runs before it.
scanNE (IntrinsicN _ IntrTagTest _ (LetVarN _ _ _ : rest)) = mconcat (map scanNE rest)
scanNE (IntrinsicN _ IntrTagTest _ (BndVarN _ _ _ : rest)) = mconcat (map scanNE rest)
scanNE (IntrinsicN _ _ _ es) = mconcat (map scanNE es)
scanNE (MapOptionalN _ _ _ e) = scanNE e

eitherArm :: ConsumeScan -> ConsumeScan -> ConsumeScan
eitherArm a b =
  ConsumeScan
    (dropColliding (csDefs a) (csDefs b))
    (csProjections a <> csProjections b)
    (Map.unionWith max (csUses a) (csUses b))

scanArm :: NativeExpr -> ConsumeScan
scanArm e
  | endsInThrow e = mempty
  | otherwise = scanNE e
  where
    endsInThrow (IntrinsicN _ IntrThrow _ _) = True
    endsInThrow (DoBlockN _ x) = endsInThrow x
    endsInThrow (EvalN _ x) = endsInThrow x
    endsInThrow (ReturnN x) = endsInThrow x
    endsInThrow _ = False

-- | The ownership of each argument of an @AppExeN@, taken from the original IR
-- (a manifold-valued argument is always an owned call result).
argOwnerships :: (Monad m) => LowerConfig m -> NativeExpr -> m [IOwnership]
argOwnerships cfg (AppExeN _ _ args) = mapM (nativeArgOwnership cfg) args
argOwnerships _ _ = return (repeat Owned)

nativeArgTypeF :: NativeArg -> TypeF
nativeArgTypeF (NativeArgExpr e) = typeFof e
nativeArgTypeF (NativeArgManifold nm) = typeFof nm

nativeArgOwnership :: LowerConfig m -> NativeArg -> m IOwnership
nativeArgOwnership cfg (NativeArgExpr e) = lcOwnership cfg e
nativeArgOwnership cfg (NativeArgManifold nm) = lcArgManifoldOwnership cfg nm

-- | Assemble a thunk from its already-adapted body.
lowerDoBlock :: (Monad m) => LowerConfig m -> TypeF -> PoolDocs -> m PoolDocs
lowerDoBlock cfg t x = do
  (hoisted, effectExpr) <- lcMakeDoBlock cfg t (poolPriorLines x) (poolExpr x)
  return
    defaultValue
      { poolExpr = effectExpr
      , poolCompleteManifolds = poolCompleteManifolds x
      , poolPriorLines = hoisted
      , poolPriorExprs = poolPriorExprs x
      }

-- | Adapt a container element (an owned sink) to an owned value, using its
-- ownership and type from the original IR.
adaptOwnedElem :: (Monad m) => LowerConfig m -> NativeExpr -> PoolDocs -> m PoolDocs
adaptOwnedElem cfg origE pd = do
  own <- lcOwnership cfg origE
  return pd {poolExpr = lcOwnArg cfg own (typeFof origE) (poolExpr pd)}

-- | Adapt a value a read-only sink takes the address of (see 'lcReadArg').
adaptReadElem :: (Monad m) => LowerConfig m -> NativeExpr -> PoolDocs -> m PoolDocs
adaptReadElem cfg origE pd = do
  own <- lcOwnership cfg origE
  return pd {poolExpr = lcReadArg cfg own (poolExpr pd)}

-- | Adapt each element of a container (list/tuple/record) to an owned value,
-- pairing the lowered elements with their originals; the elements pass through
-- unadapted if the counts disagree.
adaptOwnedElems :: (Monad m) => LowerConfig m -> [NativeExpr] -> [PoolDocs] -> m [PoolDocs]
adaptOwnedElems cfg origEs xs
  | length origEs == length xs = zipWithM (adaptOwnedElem cfg) origEs xs
  | otherwise = return xs

-- | Render a loop local's expression as the borrow the local holds, or
-- otherwise as the value the fallback makes.
loopBorrowOr :: (Monad m) => LowerConfig m -> Int -> NativeExpr -> PoolDocs -> m PoolDocs -> m PoolDocs
loopBorrowOr cfg i oe d fallback = do
  borrowed <- lcLoopBorrow cfg i oe
  maybe fallback (\x -> return d {poolExpr = x, poolPriorLines = []}) borrowed

-- | Own-adapt a native loop body's owned sinks, pairing the lowered body with
-- the original IR. A 'LoopContinue' argument feeds a loop-carried reassignment
-- and an intermediate 'LoopNLet' RHS feeds a fresh local -- both owned sinks, so
-- a borrowed value must be cloned to survive (Rust); a no-op where ownership is
-- trivial (C++/py/r 'lcOwnArg' is identity). Guards, serial lets, and the
-- already-serialized base pass through. Shape mismatch falls back to the lowered
-- body unchanged, mirroring 'adaptOwnedElems'.
adaptLoopBodyOwned ::
  (Monad m) =>
  LowerConfig m ->
  (b -> PoolDocs -> m PoolDocs) ->
  [Int] ->
  LoopBody NativeExpr SerialExpr b ->
  LoopBody PoolDocs PoolDocs PoolDocs ->
  m (LoopBody PoolDocs PoolDocs PoolDocs)
adaptLoopBodyOwned cfg adaptBase ids origBody body = go origBody body
  where
    go (LoopIf _ ot oe) (LoopIf c t e) = LoopIf c <$> go ot t <*> go oe e
    -- A let kept only when its original is the same let.
    go (LoopNLet j orhs ob) (LoopNLet i rhs b) = do
      let keep = if j == i then keepRhs cfg orhs else return
      rhs' <- loopBorrowOr cfg i orhs rhs (adaptOwnedElem cfg orhs rhs >>= keep >>= lcLoopLetRhs cfg i orhs)
      LoopNLet i rhs' <$> go ob b
    go (LoopSLet _ orhs ob) (LoopSLet i rhs b) = do
      rhs' <- ownLoopPacket i orhs rhs
      LoopSLet i rhs' <$> go ob b
    go (LoopContinue ones) (LoopContinue pds)
      | length ones == length pds && length ids == length pds =
          LoopContinue <$> sequence (zipWith3 (\i oe d -> loopBorrowOr cfg i oe d (adaptOwnedElem cfg oe d)) ids ones pds)
      | otherwise = error "morloc bug: a loop continue whose values do not match its carried locals"
    go (LoopBase ob) (LoopBase d) = LoopBase <$> adaptBase ob d
    go _ lowered = return lowered

    -- A packet a serial let makes each iteration is released when the
    -- iteration ends, rather than when the dispatch does. Only a fresh packet
    -- (made here, or sent back by another pool) is owned, and never one a
    -- base leaf names: that one may be the loop's result.
    ownLoopPacket i orhs rhs
      | isFreshPacket orhs && not (any (mentionsVar (render (svarNamer i))) (loopBases body)) = do
          ownerIdx <- lcNewIndex cfg
          case lcOwnPacketDecl cfg (helperNamer ownerIdx) (poolExpr rhs) of
            Just (decl, ptr) ->
              return rhs {poolPriorLines = poolPriorLines rhs <> [decl], poolExpr = ptr}
            Nothing -> return rhs
      | otherwise = return rhs

    isFreshPacket SerializeS {} = True
    isFreshPacket e = isCallResult e

    loopBases (LoopIf _ t e) = loopBases t <> loopBases e
    loopBases (LoopNLet _ _ b) = loopBases b
    loopBases (LoopSLet _ _ b) = loopBases b
    loopBases (LoopBase d) = [d]
    loopBases (LoopContinue _) = []

    -- Whether the rendered leaf uses the variable as a whole word.
    mentionsVar v d = any (== v) (T.split (not . isIdentChar) (render (vsep (poolPriorLines d <> [poolExpr d]))))
    isIdentChar c = isAlphaNum c || c == '_'

-- | Adapt a let's right-hand side through 'lcKeepElem', given its original:
-- the value a chain of lets ends in is kept.
keepRhs :: (Monad m) => LowerConfig m -> NativeExpr -> PoolDocs -> m PoolDocs
keepRhs cfg rhsE = keepOne cfg (valueOf rhsE)
  where
    valueOf (NativeLetN _ _ b) = valueOf b
    valueOf e = e

-- | Adapt one kept value through 'lcKeepElem', given its original.
keepOne :: (Monad m) => LowerConfig m -> NativeExpr -> PoolDocs -> m PoolDocs
keepOne cfg e pd = (\v -> pd {poolExpr = v}) <$> lcKeepElem cfg e (poolExpr pd)

-- | Adapt each element of a tuple, record or constructor through
-- 'lcKeepElem', given its original. Passes through unchanged when the
-- original expression list is unavailable (length mismatch), mirroring
-- 'adaptOwnedElems'.
keepElems :: (Monad m) => LowerConfig m -> [NativeExpr] -> [PoolDocs] -> m [PoolDocs]
keepElems cfg origEs xs
  | length origEs == length xs = zipWithM (keepOne cfg) origEs xs
  | otherwise = return xs

-- | Adapt each aggregate element/field to its stored representation via
-- 'lcStoreField' (e.g. Rust boxes a function value into its @Arc<dyn Fn>@
-- element type). Passes through unchanged when the original expression list is
-- unavailable (length mismatch), mirroring 'adaptOwnedElems'.
storeElems :: (Monad m) => LowerConfig m -> [NativeExpr] -> [PoolDocs] -> m [PoolDocs]
storeElems cfg origEs xs
  | length origEs == length xs =
      zipWithM
        (\e pd -> do
            v <- lcStoreField cfg (typeFof e) (poolExpr pd)
            return pd {poolExpr = v})
        origEs
        xs
  | otherwise = return xs

-- | Lower a native expression to PoolDocs via the IR.
--
lowerNativeExpr ::
  (Monad m) =>
  LowerConfig m ->
  NativeExpr ->
  NativeExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) ->
  m PoolDocs
lowerNativeExpr cfg origExpr ne = lowerNativeExprRaw cfg origExpr ne

-- | Does a let's body evaluate to exactly the variable the let bound?
--
-- When it does, the packet the binding owns IS the result being handed
-- back, so releasing it here would free what the caller is about to read.
-- Ownership passes outward instead, and the dispatch boundary releases it
-- with everything else it holds.
-- | How to finish with a packet, given the type of the value read out of
-- it. A function value keeps the packets of whatever it captured, so its
-- packet's bytes are still in use; anything else has been read into
-- storage of its own.
releaseStmtFor :: LowerConfig m -> TypeF -> Text -> MDoc
releaseStmtFor cfg t
  | containsFunF t = lcReleaseBorrowedStmt cfg
  | otherwise = lcReleaseStmt cfg

-- | A serial expression whose value is a packet another pool sent back: a
-- fresh packet carrying a reference of its own. A same-pool serial call
-- ('AppRecS') is not one: in a language that returns an argument packet
-- as it is (see 'lcDupPacket') its result may be a packet something else
-- still holds.
isCallResult :: SerialExpr -> Bool
isCallResult AppPoolS {} = True
isCallResult AppForeignRecS {} = True
isCallResult _ = False

-- | A call's argument serialized in place is made for that call alone, so it
-- is handed to the call as an owned argument; a packet bound to a variable
-- is released by its binding.
ownFreshArgs :: LowerConfig m -> [SerialExpr] -> [PoolDocs] -> [PoolDocs]
ownFreshArgs cfg = zipWith own
  where
    own (SerializeS _ _) d = d {poolExpr = lcOwnedArg cfg (poolExpr d)}
    own _ d = d

bodyIsBoundVar :: Int -> PoolDocs -> Bool
bodyIsBoundVar i body = render (poolExpr body) == render (svarNamer i)

-- | A constructor's 0-based tag within an arm list.
armTagOf :: T.Text -> [(T.Text, [TypeF])] -> Maybe Int
armTagOf nm as = lookup nm (zip (map fst as) [0 ..])


-- | The per-shape lowering rules. Reached through 'lowerNativeExpr', which
-- adds the fallible-intrinsic wrap.
-- | The unit type.
--
-- @()@ and @Null@ arrive at the same 'NullN' node because their wire form is
-- the same absent-value byte, but their native spellings are not: Rust writes
-- @()@ for the one and @Option::None@ for the other. An optional is never a
-- unit, whatever it wraps.
-- | The native spelling of a `data` type at one occurrence: its per-language
-- form with this instantiation's arguments filled in, which is what a
-- constructor, a tag test or a field projection has to name. A language
-- that renders no types has no spelling to give, and its forms for these do
-- not name the type anyway, so the declared name stands in.
nominalTypeDoc :: (Monad m) => LowerConfig m -> TypeF -> CVar -> m MDoc
nominalTypeDoc cfg t cv = do
  mayT <- lcTypeOf cfg t
  return $ case mayT of
    Just (ITyNamed n []) -> pretty n
    Just it -> renderIType it
    Nothing -> pretty (unCVar cv)

isUnitTypeF :: TypeF -> Bool
isUnitTypeF (VarF (FV gv _)) = gv == BT.unit
isUnitTypeF _ = False

-- | Bind each computed argument of a call to a local ahead of the call when
-- the language would otherwise pass it unevaluated ('lcBindCallArgs').
-- Infix operators are left alone: R's arithmetic forces its operands, and
-- its @&&@ and @||@ short-circuit as they do in every other language.
bindCallArgs ::
  (Monad m) => LowerConfig m -> NativeExpr -> [(TypeM, PoolDocs)] -> m [(TypeM, PoolDocs)]
bindCallArgs cfg (AppExeN _ _ args) xs
  | lcBindCallArgs cfg && length args == length xs = zipWithM bind args xs
  where
    bind a (t, x)
      | isValueArg a = return (t, x)
      | otherwise = do
          v <- render . helperNamer <$> lcNewIndex cfg
          return
            ( t
            , x
                { poolExpr = pretty v
                , poolPriorLines = poolPriorLines x <> [lcPrintStmt cfg (IAssign v Nothing (IRawExpr (render (poolExpr x))))]
                }
            )
bindCallArgs _ _ xs = return xs

-- | An argument that is already a value, so passing it evaluates nothing.
isValueArg :: NativeArg -> Bool
isValueArg (NativeArgExpr e) = case e of
  BndVarN {} -> True
  LetVarN {} -> True
  ExeN {} -> True
  LogN {} -> True
  RealN {} -> True
  IntN {} -> True
  StrN {} -> True
  NullN {} -> True
  EnumN {} -> True
  _ -> False
isValueArg (NativeArgManifold _) = False

-- | @if c then b else False@ is @c && b@ and @if c then True else b@ is
-- @c || b@, written with the language's operator when @b@ needs no
-- statements, so its evaluation stays where the conditional put it.
-- An operand that is itself a chain of the same operator is written without
-- its parentheses, so a chain is flat however long it is: both operators are
-- associative and evaluate their operands left to right in every language.
shortCircuit :: LowerConfig m -> NativeExpr -> NativeExpr -> NativeExpr -> PoolDocs -> PoolDocs -> PoolDocs -> Maybe PoolDocs
shortCircuit cfg condE thenE elseE condDocs thenDocs elseDocs = case (thenE, elseE) of
  (_, LogN _ False) | null (poolPriorLines thenDocs) -> Just (combine True thenE thenDocs)
  (LogN _ True, _) | null (poolPriorLines elseDocs) -> Just (combine False elseE elseDocs)
  _ -> Nothing
  where
    combine isAnd otherE other = PoolDocs
      { poolCompleteManifolds = poolCompleteManifolds condDocs <> poolCompleteManifolds other
      , poolExpr = parens (lcShortCircuit cfg isAnd (operand isAnd condE condDocs) (operand isAnd otherE other))
      , poolPriorLines = poolPriorLines condDocs
      , poolPriorExprs = poolPriorExprs condDocs <> poolPriorExprs other
      , poolReturnFlag = poolReturnFlag condDocs || poolReturnFlag other
      , poolBorrowedLocal = Nothing
      }
    -- A chain written by 'combine' is exactly its operator expression in
    -- parentheses; one that needed statements is a variable instead.
    operand isAnd e docs
      | chainOperator e == Just isAnd
      , Just inner <- T.stripPrefix "(" (render (poolExpr docs)) >>= T.stripSuffix ")" =
          pretty inner
      | otherwise = poolExpr docs
    chainOperator (IfN _ _ _ (LogN _ False)) = Just True
    chainOperator (IfN _ _ (LogN _ True) _) = Just False
    chainOperator _ = Nothing

lowerNativeExprRaw ::
  (Monad m) =>
  LowerConfig m ->
  NativeExpr ->
  NativeExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) ->
  m PoolDocs
-- Binary operator: emit (lhs op rhs) instead of function call
lowerNativeExprRaw cfg _ (AppExeN_ _ (SrcCallP _ src) (map snd -> [lhs, rhs]))
  | srcOperator src =
      return $ mergePoolDocs (\xs -> case xs of [l, r] -> lcOperator cfg src l r; _ -> error "binary operator requires exactly 2 args") [lhs, rhs]
lowerNativeExprRaw cfg origExpr (AppExeN_ _ (SrcCallP _ src) es0) = do
  es <- bindCallArgs cfg origExpr es0
  owns <- argOwnerships cfg origExpr
  let argTypes = map fst es
      -- Only the FIRST group is passed to the source function; every later
      -- group is applied to the function value the previous group returned,
      -- so those arguments take the closure-application convention rather
      -- than the host's parameter convention.
      firstGroup = case provideClosure src [0 .. length es - 1] of
        (g : _) -> g
        [] -> []
      site i = if i `elem` firstGroup then SourcedArg src i else ClosureArg
      handleFunctionArgs exprs =
        case provideClosure src (zipWith (\(i, t, own) e -> lcSourcedArg cfg (site i) own t e) (zip3 [0 ..] argTypes owns) exprs) of
          [] -> lcSrcName cfg src
          (g0 : gs) -> foldl (lcApplySrcGroup cfg) (lcSrcName cfg src <> (if null g0 then tupled g0 else tupledNoFold g0)) gs
  return $ mergePoolDocs handleFunctionArgs (map snd es)
lowerNativeExprRaw cfg origExpr (AppExeN_ t (PatCallP p) xs) = do
  let es = map snd xs
  taken <- case (p, origExpr, es) of
    (PatternStruct sel, AppExeN _ _ [recv], [e])
      | not (selectorHasBracket sel) -> do
          own <- nativeArgOwnership cfg recv
          if own == Owned
            then lcTakeGetter cfg (nativeArgTypeF recv) sel (poolExpr e)
            else return Nothing
    (PatternStruct sel, AppExeN _ _ (NativeArgExpr recv : _ : _), _)
      | not (selectorHasBracket sel) -> lcTakeSetter cfg recv t sel (map poolExpr es)
    _ -> return Nothing
  patResult <- maybe (lcEvalPattern cfg t p (map poolExpr es)) return taken
  return $
    PoolDocs
      { poolCompleteManifolds = concatMap poolCompleteManifolds es
      , poolExpr = patResult
      , poolPriorLines = concatMap poolPriorLines es
      , poolPriorExprs = concatMap poolPriorExprs es
      , poolReturnFlag = any poolReturnFlag es
      , poolBorrowedLocal = Nothing
      }
-- Manifold-call arguments go through lcSourcedArg too (identity for most
-- languages; the Rust member borrows every native arg so a value can fan out to
-- several manifold calls as shared borrows instead of a move-after-move).
lowerNativeExprRaw cfg origExpr (AppExeN_ _ (LocalCallP idx) xs0) = do
  xs <- bindCallArgs cfg origExpr xs0
  owns <- argOwnerships cfg origExpr
  let argTypes = map fst xs
  return $ mergePoolDocs (\es -> lcApplyClosure cfg (nvarNamer idx) (zipWith3 (\own t e -> lcSourcedArg cfg ClosureArg own t e) owns argTypes es)) (map snd xs)
-- A partial application of a closure: the pool's runtime runs its stage
-- entry, if it has one, and returns the closure of the remaining arguments.
lowerNativeExprRaw cfg origExpr (AppExeN_ t (PapplyP idx) xs0) = do
  xs <- bindCallArgs cfg origExpr xs0
  owns <- argOwnerships cfg origExpr
  let argTypes = map fst xs
      es = zipWith3 (\own at e -> lcSourcedArg cfg ClosureArg own at e) owns argTypes (map (poolExpr . snd) xs)
  call <- lcPapply cfg t argTypes (nvarNamer idx) es
  return $ mergePoolDocs (const call) (map snd xs)
lowerNativeExprRaw cfg origExpr (AppExeN_ _ (RecCallP mid _) xs0) = do
  xs1 <- bindCallArgs cfg origExpr xs0
  xs <- case origExpr of
    AppExeN _ _ args
      | length args == length xs1 ->
          sequence [(,) t <$> keepArg j a d | (j, a, (t, d)) <- zip3 [0 ..] args xs1]
    _ -> return xs1
  owns <- argOwnerships cfg origExpr
  let argTypes = map fst xs
  return $ mergePoolDocs (\es -> manNamer mid <> tupled (zipWith4 (\j own t e -> lcSourcedArg cfg (ManifoldArg mid j) own t e) [0 ..] owns argTypes es)) (map snd xs)
  where
    keepArg j (NativeArgExpr e) d = (\v -> d {poolExpr = v}) <$> lcKeepCallArg cfg mid j e (poolExpr d)
    keepArg _ _ d = return d
lowerNativeExprRaw _ _ (ManN_ call) = return call
-- The manifold return is an owned sink: adapt the returned value to an owned
-- value (Rust clones a borrowed/place value; other members are unchanged) so a
-- `&T`/place value does not reach the owned return slot.
lowerNativeExprRaw cfg (ReturnN innerE) (ReturnN_ x) = do
  x' <- adaptOwnedElem cfg innerE x
  return $ x' {poolReturnFlag = True}
lowerNativeExprRaw _ _ (ReturnN_ x) =
  return $ x {poolReturnFlag = True}
lowerNativeExprRaw cfg (SerialLetN _ (SerializeS _ _) body) (SerialLetN_ i x1 x2) = do
  -- Same shape as the SerialLetS-with-SerializeS case in lowerSerialExpr:
  -- the let-bound variable owns a put_value tracker entry, so wrap the
  -- body's result with a temp and emit a release call once the body
  -- finishes using the bound variable. This branch covers inner (native-
  -- returning) manifolds whose put_value let-bindings live in NativeExpr
  -- via SerialLetN -- e.g., the m1417-style wrapper around a foreign call.
  -- The body is a NativeExpr, so the temp's declared type must match it
  -- (rather than falling back to the serial type used for SerialLetS).
  letResult <- lcMakeLet cfg svarNamer i Nothing Nothing x1 x2
  if bodyIsBoundVar i x2
    then return letResult
    else do
      tmpIdx <- lcNewIndex cfg
      let bodyT = typeFof body
          releaseLine = releaseStmtFor cfg bodyT (render (svarNamer i))
          releaseBody =
            defaultValue
              { poolExpr = helperNamer tmpIdx
              , poolPriorLines = [releaseLine]
              }
      lcMakeLet cfg helperNamer tmpIdx (Just bodyT) Nothing letResult releaseBody
lowerNativeExprRaw cfg (SerialLetN _ rhs body) (SerialLetN_ i x1 x2)
  | isCallResult rhs = do
  -- The let RHS is a call into another pool, so the bound variable owns
  -- the shared-memory reference the callee donated before sending. The
  -- body is native: it has read what it needs out of the packet and holds
  -- it in its own right -- a table's view takes a reference of its own,
  -- and every other value is copied out -- so the donated reference ends
  -- at the body's last use rather than at the dispatch boundary. Without
  -- this a loop that calls into another pool holds every result it has
  -- ever received until the whole loop is done.
  letResult <- lcMakeLet cfg svarNamer i Nothing Nothing x1 x2
  tmpIdx <- lcNewIndex cfg
  let bodyT = typeFof body
      releaseLine = releaseStmtFor cfg bodyT (render (svarNamer i))
      releaseBody =
        defaultValue
          { poolExpr = helperNamer tmpIdx
          , poolPriorLines = [releaseLine]
          }
  lcMakeLet cfg helperNamer tmpIdx (Just bodyT) Nothing letResult releaseBody
lowerNativeExprRaw cfg _ (SerialLetN_ i x1 x2) = lcMakeLet cfg svarNamer i Nothing Nothing x1 x2
-- A native let binds an owned local, so its RHS is an owned sink: adapt the
-- bound value (Rust clones a borrowed/place RHS -- e.g. a getter through a
-- borrowed receiver -- to match the owned binding type; other members unchanged).
lowerNativeExprRaw cfg (NativeLetN _ rhsE _) (NativeLetN_ i x1 x2) = do
  x1' <- loopBorrowOr cfg i rhsE x1 (adaptOwnedElem cfg rhsE x1 >>= keepRhs cfg rhsE >>= lcLetRhs cfg i rhsE)
  lcMakeLet cfg nvarNamer i (Just (typeFof rhsE)) (Just rhsE) x1' x2
lowerNativeExprRaw _ _ (NativeLetN_ i _ _) =
  error ("morloc bug: native let " <> show i <> " lowered without its original")
lowerNativeExprRaw _ _ (LetVarN_ _ i _) = return $ defaultValue {poolExpr = nvarNamer i}
lowerNativeExprRaw _ _ (BndVarN_ _ i _) = return $ defaultValue {poolExpr = nvarNamer i}
lowerNativeExprRaw cfg _ (DeserializeN_ t s x) = do
  (deserialized, assignments) <- lcDeserialize cfg t (poolExpr x) s
  return $
    x
      { poolExpr = deserialized
      , poolPriorLines = poolPriorLines x <> assignments
      }
lowerNativeExprRaw cfg _ (ExeN_ _ (SrcCallP _ src)) = return $ defaultValue {poolExpr = lcSrcName cfg src}
lowerNativeExprRaw _ _ (ExeN_ _ (PatCallP _)) = error "Unreachable: patterns are always used in applications"
lowerNativeExprRaw _ _ (ExeN_ _ (LocalCallP idx)) = return $ defaultValue {poolExpr = nvarNamer idx}
lowerNativeExprRaw _ _ (ExeN_ _ (PapplyP idx)) = return $ defaultValue {poolExpr = nvarNamer idx}
lowerNativeExprRaw _ _ (ExeN_ _ (RecCallP mid _)) = return $ defaultValue {poolExpr = manNamer mid}
lowerNativeExprRaw cfg origExpr (ListN_ v t xs) = do
  let elemEs = case origExpr of ListN _ _ es -> es; _ -> []
  xs' <- adaptOwnedElems cfg elemEs xs >>= storeElems cfg elemEs
  return $ mergePoolDocs (lcListConstructor cfg v t) xs'
lowerNativeExprRaw cfg origExpr (TupleN_ v xs) = do
  let slotTypes = case typeFof origExpr of
        AppF _ ts -> ts
        _ -> []
      elemEs = case origExpr of TupleN _ es -> es; _ -> []
  xs' <- adaptOwnedElems cfg elemEs xs >>= keepElems cfg elemEs >>= storeElems cfg elemEs
  return $ mergePoolDocs (lcTupleConstructor cfg v slotTypes) xs'
lowerNativeExprRaw cfg origExpr (RecordN_ o v ps rs) = do
  let fieldEs = case origExpr of RecordN _ _ _ kvs -> map snd kvs; _ -> []
  es <- adaptOwnedElems cfg fieldEs (map snd rs) >>= keepElems cfg fieldEs >>= storeElems cfg fieldEs
  let recType = typeFof origExpr
  rec' <- lcRecordConstructor cfg recType o v ps (zip (map fst rs) (map poolExpr es))
  return $
    rec'
      { poolCompleteManifolds = concatMap poolCompleteManifolds es <> poolCompleteManifolds rec'
      , poolPriorLines = concatMap poolPriorLines es <> poolPriorLines rec'
      , poolPriorExprs = concatMap poolPriorExprs es <> poolPriorExprs rec'
      }
lowerNativeExprRaw cfg _ (LogN_ _ v) = return $ defaultValue {poolExpr = lcPrintExpr cfg (IBoolLit v)}
lowerNativeExprRaw cfg _ (RealN_ (FV _ cv) v) = return $ defaultValue {poolExpr = lcPrintExpr cfg (IRealLit (Just (unCVar cv)) v)}
-- An integer literal whose slot resolved to a real type must be emitted in
-- float form. The typechecker deliberately allows Int-literal-to-real
-- promotion (it is what lets @4 + 2.3@ check), and the nexus side already
-- converts through 'Morloc.CodeGenerator.NumericLiteral.resolveNumericLiteral'.
-- The pool side emitted a bare integer, so a promoted literal reaching an F32
-- slot was written as a Python @int@ against an @f4@ schema and rejected at
-- run time by @pybinding__put_value@. Dispatch on the GENERAL type, which is
-- the only place the promotion is visible; the concrete name is still passed
-- through for the target language's own literal suffix.
lowerNativeExprRaw cfg _ (IntN_ (FV gv cv) v)
  | BT.isRealBaseType (VarU gv) =
      return $ defaultValue
        {poolExpr = lcPrintExpr cfg (IRealLit (Just (unCVar cv)) (RealFinite (fromInteger v)))}
lowerNativeExprRaw cfg _ (IntN_ (FV _ cv) v) = return $ defaultValue {poolExpr = lcPrintExpr cfg (IIntLit (Just (unCVar cv)) v)}
lowerNativeExprRaw cfg _ (StrN_ (FV _ cv) v) =
  let hint = if cv == CV "" then Nothing else Just (unCVar cv)
  in return $ defaultValue {poolExpr = lcPrintExpr cfg (IStrLit hint v)}
lowerNativeExprRaw cfg _ (EnumN_ t n i)
  | EnumF (FV _ cv) _ names <- t = do
      ty <- nominalTypeDoc cfg t cv
      return $ defaultValue {poolExpr = lcEnumLit cfg ty names n i}
  | otherwise = error $ "constructor literal carries a non-enum type: " <> show (pretty t)
-- A constructor's fields are stored positions exactly as a tuple's are: an
-- argument a borrowed variable supplies is cloned into its slot, so one
-- variable can fill two fields.
lowerNativeExprRaw cfg origExpr (VariantN_ t n i xs)
  | VariantF (FV _ cv) _ _ <- t = do
      ty <- nominalTypeDoc cfg t cv
      let fieldEs = case origExpr of VariantN _ _ _ es -> es; _ -> []
      xs' <- adaptOwnedElems cfg fieldEs xs >>= keepElems cfg fieldEs >>= storeElems cfg fieldEs
      return $ mergePoolDocs (lcVariantLit cfg ty n i) xs'
  | otherwise = error $ "constructor literal carries a non-variant type: " <> show (pretty t)
-- The unit value. @UniS@ and @NullS@ share this node -- the wire form is
-- the same absent-value byte -- but the native literal is not: a language
-- with distinct unit and none spellings needs the one the type names. The
-- type is resolved through the alias chain first, so @type Done = ()@ is
-- still a unit.
lowerNativeExprRaw cfg _ (NullN_ t)
  | isUnitTypeF t = return $ defaultValue {poolExpr = lcPrintExpr cfg IUnitLit}
lowerNativeExprRaw cfg _ (NullN_ t) = do
  -- NullN_ now carries the full @TypeF@ of the Null's type slot
  -- (e.g. @?(BTree Int)@), not just the underlying constructor's
  -- @FVar@. Pass it through @lcTypeOf@ to get the IType the printer
  -- uses for the null literal -- though most language printers
  -- ignore this IType (Python emits "None", C++ emits "std::nullopt"
  -- unconditionally), threading it preserves the option for languages
  -- that do type-tagged null forms.
  mayT <- lcTypeOf cfg t
  return $ defaultValue {poolExpr = lcPrintExpr cfg (INullLit mayT)}
-- A thunk's result leaves the thunk by value, so it is an owned sink like a
-- container element: a value that is merely borrowed here, or that is used
-- somewhere else as well, has to be copied out rather than moved out. Without
-- this, a thunk that yields a value another thunk is still reading -- the two
-- arms of a `@catch` over one variable -- takes it away from them.
lowerNativeExprRaw cfg (DoBlockN _ innerE) (DoBlockN_ t x) =
  adaptOwnedElem cfg innerE x >>= lowerDoBlock cfg t
lowerNativeExprRaw cfg _ (DoBlockN_ t x) = lowerDoBlock cfg t x
-- Running a suspension is applying a closure to no arguments.
lowerNativeExprRaw cfg _ (EvalN_ _ x) = return $ x {poolExpr = lcApplyClosure cfg (poolExpr x) []}
-- CoerceToOptional widens a value to an optional. Most languages treat a T as a
-- valid ?T (identity); Rust must wrap the value in Some(..) (see lcCoerceOptional).
-- The wrapped value must be OWNED first: `Some(&T)` is an `Option<&T>`, not the
-- `Option<T>` the sink expects, so a borrowed/place inner is cloned before wrapping.
lowerNativeExprRaw cfg (CoerceN _ _ innerE) (CoerceN_ CoerceToOptional _ x) = do
  x' <- adaptOwnedElem cfg innerE x >>= keepOne cfg innerE
  -- Adapt the inner to its stored representation before wrapping (Rust boxes a
  -- closure into `Arc<dyn Fn>` so the payload matches the boxed
  -- `Option<Arc<dyn Fn>>` element type), mirroring list/tuple/record.
  boxed <- lcStoreField cfg (typeFof innerE) (poolExpr x')
  return $ x' {poolExpr = lcCoerceOptional cfg boxed}
lowerNativeExprRaw cfg _ (CoerceN_ CoerceToOptional _ x) =
  return $ x {poolExpr = lcCoerceOptional cfg (poolExpr x)}
-- The two arms feed the conditional's owned result sink, so adapt each to an
-- owned value (like every other owned sink); the condition is not a sink.
lowerNativeExprRaw cfg origExpr@(IfN _ condE thenE elseE) (IfN_ _ condDocs thenDocs elseDocs) = do
  thenDocs' <- adaptOwnedElem cfg thenE thenDocs
  elseDocs' <- adaptOwnedElem cfg elseE elseDocs
  case shortCircuit cfg condE thenE elseE condDocs thenDocs' elseDocs' of
    Just docs -> return docs
    Nothing -> lcMakeIf cfg origExpr condDocs thenDocs' elseDocs'
lowerNativeExprRaw cfg origExpr (IfN_ _ condDocs thenDocs elseDocs) =
  lcMakeIf cfg origExpr condDocs thenDocs elseDocs
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [dataE]) (IntrinsicN_ _ IntrHash (Just schema) [dataDocs]) = do
  sid <- lcRegisterSchema cfg schema
  -- The hashed value is only read, through a 'ToVoidstar' (&T) sink; see
  -- 'lcReadArg'.
  dataDocs' <- adaptReadElem cfg dataE dataDocs
  return $ dataDocs' {poolExpr = lcPrintExpr cfg (IIntrinsicHash sid (IRawExpr (render (poolExpr dataDocs'))))}
-- @save takes source args in (level, path, value) order; path-first
-- (after the level) mirrors @savem/@savej. The runtime ABI is unchanged:
-- IIntrinsicSave keeps (level, data, path).
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [_, _, dataE]) (IntrinsicN_ _ IntrSave (Just schema) [levelDocs, pathDocs, dataDocs]) = do
  sid <- lcRegisterSchema cfg schema
  -- The saved value is only read, through a 'ToVoidstar' (&T) sink (see
  -- @hash above). level and path are scalar/Str, not value sinks.
  dataDocs' <- adaptReadElem cfg dataE dataDocs
  let fmt = "voidstar"
      saveExpr = IIntrinsicSave fmt sid
                   (IRawExpr (render (poolExpr levelDocs)))
                   (IRawExpr (render (poolExpr dataDocs')))
                   (IRawExpr (render (poolExpr pathDocs)))
   in return $ mergePoolDocs (const $ lcPrintExpr cfg saveExpr) [levelDocs, pathDocs, dataDocs']
-- @savem/@savej take source args in (path, value) order for
-- partial-application ergonomics (`@savem path` is a reusable sink).
-- The runtime ABI is unchanged: IIntrinsicSave keeps (level, data, path).
-- They are not packet formats; codegen always passes a zero level
-- expression so the printed call shape is uniform with @save.
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [_, dataE]) (IntrinsicN_ _ IntrSaveM (Just schema) [pathDocs, dataDocs]) = do
  sid <- lcRegisterSchema cfg schema
  dataDocs' <- adaptReadElem cfg dataE dataDocs
  let fmt = "msgpack"
      saveExpr = IIntrinsicSave fmt sid
                   (IRawExpr "0")
                   (IRawExpr (render (poolExpr dataDocs')))
                   (IRawExpr (render (poolExpr pathDocs)))
   in return $ mergePoolDocs (const $ lcPrintExpr cfg saveExpr) [pathDocs, dataDocs']
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [_, dataE]) (IntrinsicN_ _ IntrSaveJ (Just schema) [pathDocs, dataDocs]) = do
  sid <- lcRegisterSchema cfg schema
  dataDocs' <- adaptReadElem cfg dataE dataDocs
  let fmt = "json"
      saveExpr = IIntrinsicSave fmt sid
                   (IRawExpr "0")
                   (IRawExpr (render (poolExpr dataDocs')))
                   (IRawExpr (render (poolExpr pathDocs)))
   in return $ mergePoolDocs (const $ lcPrintExpr cfg saveExpr) [pathDocs, dataDocs']
lowerNativeExprRaw cfg origExpr (IntrinsicN_ _ IntrLoad (Just schema) [pathDocs]) = do
  sid <- lcRegisterSchema cfg schema
  -- Post effect-migration, @load returns bare `T` (no OptionalF wrap).
  -- The pool-side shim `_mlc_load<T>` requires an explicit template
  -- argument since `T` only appears in the return position; template
  -- deduction from the surrounding context is unreliable, so pass the
  -- resolved type unconditionally.
  innerType <- lcTypeOf cfg (typeFof origExpr)
  return $ pathDocs {poolExpr = lcPrintExpr cfg (IIntrinsicLoad sid innerType (IRawExpr (render (poolExpr pathDocs))))}
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [dataE]) (IntrinsicN_ _ IntrShow (Just schema) [dataDocs]) = do
  sid <- lcRegisterSchema cfg schema
  -- The shown value is only read, through a 'ToVoidstar' (&T) sink (see @hash
  -- above).
  dataDocs' <- adaptReadElem cfg dataE dataDocs
  return $ dataDocs' {poolExpr = lcPrintExpr cfg (IIntrinsicShow sid (IRawExpr (render (poolExpr dataDocs'))))}
lowerNativeExprRaw cfg origExpr (IntrinsicN_ _ IntrRead (Just schema) [strDocs]) = do
  sid <- lcRegisterSchema cfg schema
  -- Same rationale as IntrLoad above: @read returns bare `T` now,
  -- pass the resolved type so `_mlc_read<T>` gets its template arg.
  innerType <- lcTypeOf cfg (typeFof origExpr)
  return $ strDocs {poolExpr = lcPrintExpr cfg (IIntrinsicRead sid innerType (IRawExpr (render (poolExpr strDocs))))}
lowerNativeExprRaw cfg origExpr (IntrinsicN_ _ IntrUnpack (Just schema) [bytesDocs]) = do
  sid <- lcRegisterSchema cfg schema
  innerType <- lcTypeOf cfg (typeFof origExpr)
  return $ bytesDocs {poolExpr = lcPrintExpr cfg (IIntrinsicUnpack sid innerType (IRawExpr (render (poolExpr bytesDocs))))}
-- @schema and @typeof erase their argument: the result is a compile-time
-- constant string (the schema or user-facing type name), already resolved
-- into the Intrinsic node's schema slot by Serialize.hs. Emit it as a
-- string literal and discard the data expression; the type of the
-- argument is all that matters.
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrSchema (Just s) [dataDocs]) =
  return $ dataDocs {poolExpr = lcPrintExpr cfg (IStrLit Nothing s)}
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrTypeof (Just s) [dataDocs]) =
  return $ dataDocs {poolExpr = lcPrintExpr cfg (IStrLit Nothing s)}
-- @open: dispatch on the resolved handle type. IFile/IStream go through
-- the generic kind-byte runtime entry; OStream needs the element schema
-- threaded so the runtime can write the stream header at open time.
lowerNativeExprRaw cfg origExpr (IntrinsicN_ _ IntrOpen maySchema [pathDocs]) = do
  let unwrapHead (AppF (VarF (FV v _)) _) = Just v
      unwrapHead (VarF (FV v _)) = Just v
      unwrapHead _ = Nothing
  let headVar = unwrapHead (typeFof origExpr)
      rawPath = IRawExpr (render (poolExpr pathDocs))
      -- Typed open (OStream/IStream): thread the ascribed `[a]` schema through
      -- lcRegisterSchema into the given IExpr constructor. OStream needs it to
      -- write the stream header; IStream to open the stdin sentinel and guard
      -- the incoming stream.
      openTyped mkExpr what = do
        schemaText <- case maySchema of
          Just s  -> return s
          Nothing -> error ("@open :: " ++ what ++ " a -- schema not synthesized (Serialize.hs bug)")
        sid <- lcRegisterSchema cfg schemaText
        return pathDocs { poolExpr = lcPrintExpr cfg (mkExpr sid rawPath) }
  case headVar of
    Just v | v == BT.ostreamVar -> openTyped IIntrinsicOpenOStream "OStream"
    Just v | v == BT.istreamVar -> openTyped IIntrinsicOpenIStream "IStream"
    Just v | v == BT.ifileVar ->
      -- IFile reads its schema off disk (no ascription schema needed); an
      -- IFile open of the stdin sentinel is a catchable runtime error.
      return pathDocs { poolExpr = lcPrintExpr cfg (IIntrinsicOpen BT.mlcKindIFile rawPath) }
    Just (TV t) ->
      error $ "@open: result type must be IFile/IStream/OStream, got " <> T.unpack t
    Nothing ->
      error "@open: unsupported handle type"
-- @close: a Str-path arg (marked by Serialize.hs) unlinks a registered temp
-- file; a handle arg closes the stream/file handle.
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrClose maySchema [handleDocs]) =
  let raw = IRawExpr (render (poolExpr handleDocs))
      node | maySchema == Just BT.closeTmpUnlinkMarker = IIntrinsicUnlinkTemp raw
           | otherwise                                 = IIntrinsicClose raw
  in return $ handleDocs { poolExpr = lcPrintExpr cfg node }
-- A constructor-pattern tag test. No schema and no runtime call: the whole
-- operation is a comparison in the target language. The arm arrives as a
-- literal name, so the subject's own type decides how to ask -- an enum
-- compares values, because for an argument-free constructor the value IS
-- the tag, while a variant must test the discriminant without touching the
-- payload. See the note on 'IntrTagTest' for why this is a tag test rather
-- than an equality.
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [subjectE, StrN _ n]) (IntrinsicN_ _ IntrTagTest _ [subjectDocs, _]) =
  case typeFof subjectE of
    t@(VariantF (FV _ cv) _ arms) -> do
      ty <- nominalTypeDoc cfg t cv
      return $ mergePoolDocs
        (const (lcVariantTagTest cfg ty n (armIndex n (map fst arms)) (poolExpr subjectDocs)))
        [subjectDocs]
    t@(EnumF (FV _ cv) _ names) -> do
      ty <- nominalTypeDoc cfg t cv
      return $ mergePoolDocs
        (const (lcEnumTagTest cfg ty names n (armIndex n names)
                  (poolExpr subjectDocs)))
        [subjectDocs]
    t -> error $ "tag test on a type that is not a `data`: " <> show (pretty t)
-- Reading one field out of a value whose arm a guarding tag test has already
-- established. Emitted only under that guard, and with no surface spelling.
lowerNativeExprRaw cfg origE@(IntrinsicN _ _ _ [subjectE, StrN _ n, IntN _ i]) (IntrinsicN_ _ IntrCtorField _ [subjectDocs, _, _]) =
  case typeFof subjectE of
    t@(VariantF (FV _ cv) _ arms) -> do
      ty <- nominalTypeDoc cfg t cv
      own <- lcOwnership cfg subjectE
      self <- lcOwnership cfg origE
      let how
            | isLastRead subjectE && own == Owned = FieldTake
            | self == BorrowedRef = FieldBorrow
            | otherwise = FieldCopy
      return $ mergePoolDocs
        (const (lcCtorField cfg how ty n (armIndex n (map fst arms)) (fromIntegral i) (poolExpr subjectDocs)))
        [subjectDocs]
    t -> error $ "constructor field projection on a non-variant: " <> show (pretty t)
-- A list pattern's length test and element read. Like the tag test, each is
-- written in the target language with no runtime call.
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [_, IntN _ n]) (IntrinsicN_ _ IntrListLenTest _ [subjectDocs, _]) =
  return $ mergePoolDocs
    (const (lcListLenTest cfg (poolExpr subjectDocs) (fromIntegral n)))
    [subjectDocs]
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [_, IntN _ i]) (IntrinsicN_ _ IntrListElem _ [subjectDocs, _]) =
  return $ mergePoolDocs
    (const (lcListElem cfg (poolExpr subjectDocs) (fromIntegral i)))
    [subjectDocs]
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrFSchema _ [pathDocs]) =
  return $ pathDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicFSchema (IRawExpr (render (poolExpr pathDocs))))
    }
-- @flen: handle -> Int element count. No schema needed; the runtime
-- returns a raw integer.
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrFLength _ [handleDocs]) =
  return $ handleDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicFLength (IRawExpr (render (poolExpr handleDocs))))
    }
-- @next: IStream handle -> <IO> [a]. The wrapper deserialises the
-- materialised voidstar payload into a typed list of the result.
lowerNativeExprRaw cfg origExpr (IntrinsicN_ _ IntrNext (Just schema) [handleDocs]) = do
  sid <- lcRegisterSchema cfg schema
  resultType <- lcTypeOf cfg (typeFof origExpr)
  return $ handleDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicNext sid resultType
          (IRawExpr (render (poolExpr handleDocs))))
    }
-- @streamLayout: IFile handle -> <IO> [(U64,U64,U64)]. The wrapper
-- deserialises the materialised voidstar layout into a typed list of
-- triples.
lowerNativeExprRaw cfg origExpr (IntrinsicN_ _ IntrStreamLayout (Just schema) [handleDocs]) = do
  sid <- lcRegisterSchema cfg schema
  resultType <- lcTypeOf cfg (typeFof origExpr)
  return $ handleDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicStreamLayout sid resultType
          (IRawExpr (render (poolExpr handleDocs))))
    }
-- @stream: derive an IStream handle from an IFile. The runtime opens
-- a fresh slot bound to the same path.
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrStream _ [handleDocs]) =
  return $ handleDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicStream (IRawExpr (render (poolExpr handleDocs))))
    }
-- @write: emit one sub-packet of the [a] value to the OStream handle.
-- Source order is (level, handle, value); IIntrinsicWrite keeps
-- (level, value, handle) to match the C ABI `mlc_write(level, handle,
-- voidstar)` and the pool's `_mlc_write(schema, level, value, handle)`
-- helper.
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [_, _, valueE]) (IntrinsicN_ _ IntrWrite (Just schema)
                                  [levelDocs, handleDocs, valueDocs]) = do
  sid <- lcRegisterSchema cfg schema
  -- The written value is only read, through a 'ToVoidstar' (&T) sink (see
  -- @hash above). level and handle are not value sinks.
  valueDocs' <- adaptReadElem cfg valueE valueDocs
  let allDocs = [levelDocs, handleDocs, valueDocs']
      raw d = IRawExpr (render (poolExpr d))
  return $ handleDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicWrite sid (raw levelDocs) (raw valueDocs') (raw handleDocs))
    , poolPriorExprs = concatMap poolPriorExprs allDocs
    , poolPriorLines = concatMap poolPriorLines allDocs
    , poolCompleteManifolds = concatMap poolCompleteManifolds allDocs
    }
-- @append: open an existing stream file for append, returning a fresh
-- OStream handle.
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrAppend (Just schema) [pathDocs]) = do
  sid <- lcRegisterSchema cfg schema
  return $ pathDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicAppend sid (IRawExpr (render (poolExpr pathDocs))))
    }
-- @concat: byte-level concat of N stream files into one.
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrConcat _ [pathsDocs, destDocs]) =
  let allDocs = [pathsDocs, destDocs]
      raw d = IRawExpr (render (poolExpr d))
   in return $ pathsDocs
        { poolExpr = lcPrintExpr cfg
            (IIntrinsicConcat (raw pathsDocs) (raw destDocs))
        , poolPriorExprs = concatMap poolPriorExprs allDocs
        , poolPriorLines = concatMap poolPriorLines allDocs
        , poolCompleteManifolds = concatMap poolCompleteManifolds allDocs
        }
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrChannel (Just schema) []) = do
  sid <- lcRegisterSchema cfg schema
  return $ defaultValue {poolExpr = lcPrintExpr cfg (IIntrinsicChannel sid)}
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrSpawn (Just schema) [handleDocs, fnDocs]) = do
  sid <- lcRegisterSchema cfg schema
  let raw d = IRawExpr (render (poolExpr d))
  return $ mergePoolDocs (const $ lcPrintExpr cfg (IIntrinsicSpawn sid (raw handleDocs) (raw fnDocs))) [handleDocs, fnDocs]
-- @mapoptional: the function applied to the optional's value, if it has one,
-- in the same per-language null test that lifts a packer through an optional.
lowerNativeExprRaw cfg origExpr@(IntrinsicN _ _ _ [_, optE]) (IntrinsicN_ _ IntrMapOptional _ [fnDocs, optDocs]) = do
  idx <- lcNewIndex cfg
  resultType <- lcTypeOf cfg (typeFof origExpr)
  let innerTf = case typeFof optE of
        OptionalF a -> a
        t -> t
  unwrapType <- lcTypeOf cfg innerTf
  let v' = render $ helperNamer idx
      uVar = "u" <> T.pack (show idx)
      arg = lcSourcedArg cfg ClosureArg Owned (typeMof innerTf) (pretty uVar)
      call = IRawExpr (render (lcApplyClosure cfg (poolExpr fnDocs) [arg]))
      ifStmt = IIfNotNull v' resultType (IRawExpr (render (poolExpr optDocs))) uVar unwrapType [] call
      merged = mergePoolDocs (const (pretty v')) [fnDocs, optDocs]
  return $ merged {poolPriorLines = poolPriorLines merged ++ [lcPrintStmt cfg ifStmt]}
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrSettle _ [handleDocs]) =
  return $ handleDocs {poolExpr = lcPrintExpr cfg (IIntrinsicSettle (IRawExpr (render (poolExpr handleDocs))))}
-- @flush: force any buffered elements out as a sub-packet.
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrFlush _ [handleDocs]) =
  return $ handleDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicFlush (IRawExpr (render (poolExpr handleDocs))))
    }
-- @tell: nullary; read the @stdout stream's element_count.
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrTell _ []) =
  return $ defaultValue { poolExpr = lcPrintExpr cfg IIntrinsicTell }
-- @tmpfile: nullary; create + register a temp file, return its path.
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrTmpfile _ []) =
  return $ defaultValue { poolExpr = lcPrintExpr cfg IIntrinsicTmpfile }
-- The fold accumulator. @cellnew and @cellput hand a value into the
-- runtime, which reads it through a 'ToVoidstar' (&T) sink, like @write's.
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [initE]) (IntrinsicN_ _ IntrCellNew (Just schema) [initDocs]) = do
  sid <- lcRegisterSchema cfg schema
  initDocs' <- adaptReadElem cfg initE initDocs
  return $ initDocs'
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicCellNew sid (IRawExpr (render (poolExpr initDocs')))) }
lowerNativeExprRaw cfg origExpr (IntrinsicN_ _ IntrCellGet (Just schema) [handleDocs]) = do
  sid <- lcRegisterSchema cfg schema
  innerType <- lcTypeOf cfg (typeFof origExpr)
  return $ handleDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicCellGet sid innerType (IRawExpr (render (poolExpr handleDocs)))) }
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [_, valueE]) (IntrinsicN_ _ IntrCellPut (Just schema)
                                  [handleDocs, valueDocs]) = do
  sid <- lcRegisterSchema cfg schema
  valueDocs' <- adaptReadElem cfg valueE valueDocs
  let raw d = IRawExpr (render (poolExpr d))
      putExpr = IIntrinsicCellPut sid (raw handleDocs) (raw valueDocs')
  return $ mergePoolDocs (const $ lcPrintExpr cfg putExpr) [handleDocs, valueDocs']
lowerNativeExprRaw cfg origExpr (IntrinsicN_ _ IntrCellReduce (Just schema)
                                  [combineDocs, handleDocs]) = do
  sid <- lcRegisterSchema cfg schema
  innerType <- lcTypeOf cfg (typeFof origExpr)
  let raw d = IRawExpr (render (poolExpr d))
      reduceExpr = IIntrinsicCellReduce sid innerType (raw combineDocs) (raw handleDocs)
  return $ mergePoolDocs (const $ lcPrintExpr cfg reduceExpr) [combineDocs, handleDocs]
lowerNativeExprRaw cfg (IntrinsicN _ _ _ [handleE, _]) (IntrinsicN_ _ IntrReplay (Just schema)
                                  [handleDocs, fnDocs]) = do
  sid <- lcRegisterSchema cfg schema
  elemType <- case typeFof handleE of
    AppF _ (e : _) -> lcTypeOf cfg e
    _ -> return Nothing
  let raw d = IRawExpr (render (poolExpr d))
      replayExpr = IIntrinsicReplay sid elemType (raw handleDocs) (raw fnDocs)
  return $ mergePoolDocs (const $ lcPrintExpr cfg replayExpr) [handleDocs, fnDocs]
lowerNativeExprRaw cfg origExpr (IntrinsicN_ _ IntrThrow _ [msgDocs]) = do
  resultType <- lcTypeOf cfg (typeFof origExpr)
  return $ msgDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicThrow resultType (IRawExpr (render (poolExpr msgDocs))))
    }
-- @try: Serialize.hs::thunkifyForTry has already forced the body into a
-- DoBlockN thunk, so poolExpr is a no-arg callable (often a bound helper
-- name). The Ok and Err arms are built here rather than in the runtime
-- helper because a `Try` may be mapped to a native type or left for the
-- compiler to generate, and only 'lcVariantLit' knows which.
lowerNativeExprRaw cfg _ (IntrinsicN_ t IntrTry _ [bodyDocs])
  | tryT@(VariantF (FV _ cv) _ arms) <- t
  , Just okTag <- armTagOf BT.tryOkCtor arms
  , Just errTag <- armTagOf BT.tryErrCtor arms = do
      ty <- nominalTypeDoc cfg tryT cv
      okT <- case lookup BT.tryOkCtor arms of
        Just [payloadT] -> fmap renderIType <$> lcTypeOf cfg payloadT
        _ -> return Nothing
      return $ bodyDocs
        { poolExpr = lcMakeTry cfg (poolExpr bodyDocs) okT
            (\v -> lcVariantLit cfg ty BT.tryOkCtor okTag [v])
            (\m -> lcVariantLit cfg ty BT.tryErrCtor errTag [m])
        }
  | otherwise =
      error $ "@try result is not a Try variant: " <> show (pretty t)
-- @stdin / @stdout / @stderr: nullary intrinsics. The runtime enforces
-- singleton opens per stdio kind; codegen just emits the call with the
-- element schema.
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrStdin (Just schema) []) = do
  sid <- lcRegisterSchema cfg schema
  return $ defaultValue
    { poolExpr = lcPrintExpr cfg (IIntrinsicStdin sid) }
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrStdout (Just schema) []) = do
  sid <- lcRegisterSchema cfg schema
  return $ defaultValue
    { poolExpr = lcPrintExpr cfg (IIntrinsicStdout sid) }
lowerNativeExprRaw cfg _ (IntrinsicN_ _ IntrStderr (Just schema) []) = do
  sid <- lcRegisterSchema cfg schema
  return $ defaultValue
    { poolExpr = lcPrintExpr cfg (IIntrinsicStderr sid) }
-- @ifile_walk: unified IFile pattern walker.
-- Args: [pathDocs, handleDocs, runtimeArg0, runtimeArg1, ...].
-- Express.hs assembles the path string and runtime args according to
-- the walk-step chain; we forward them to the per-language wrapper.
-- The schema slot from Serialize.hs carries the result type's msgpack
-- schema so the wrapper can convert the returned voidstar via
-- from_voidstar<T>.
lowerNativeExprRaw cfg origExpr (IntrinsicN_ _ IntrIFileWalk (Just schema) (pathDocs : handleDocs : runtimeDocs)) = do
  sid <- lcRegisterSchema cfg schema
  resultType <- lcTypeOf cfg (typeFof origExpr)
  let allDocs = pathDocs : handleDocs : runtimeDocs
      raw d = IRawExpr (render (poolExpr d))
  return $ handleDocs
    { poolExpr = lcPrintExpr cfg
        (IIntrinsicIFileWalk sid resultType
          (raw pathDocs)
          (raw handleDocs)
          (map raw runtimeDocs))
    , poolPriorExprs = concatMap poolPriorExprs allDocs
    , poolPriorLines = concatMap poolPriorLines allDocs
    , poolCompleteManifolds = concatMap poolCompleteManifolds allDocs
    }
lowerNativeExprRaw _ _ (IntrinsicN_ _ intr _ _) =
  error $ "Runtime intrinsic @" <> show intr <> " reached code generation without schema"
-- Lift a source function through an Optional. Mirrors how
-- expandDeserialize's SerialPack arm wraps an inner deserialization
-- with a packer call, but at the NativeExpr level: the inner
-- expression evaluates to optional<wireT> (e.g. the wire form returned
-- by @load), and we produce optional<userT> by applying `src` to the
-- inner value when present.
-- A native tail loop: its locals start from their initializers (owned
-- copies of the manifold's read-only parameters) and a base leaf is the
-- loop's native value, an owned sink like any returned value.
lowerNativeExprRaw cfg (LoopN t origInits origBody) (LoopN_ _ starts body) = do
  starts' <- zipWithM (\(_, oe) (i, d) -> (,) i . Just <$> loopBorrowOr cfg i oe d (adaptOwnedElem cfg oe d >>= keepOne cfg oe)) origInits starts
  let ids = map fst origInits
      keepBase ob d = adaptOwnedElem cfg ob d >>= \d' -> (\v -> d' {poolExpr = v}) <$> lcKeepSlot cfg ids ob (poolExpr d')
  body' <- adaptLoopBodyOwned cfg keepBase ids origBody body
  lcMakeLoop cfg (LoopResultNative t) starts' (Just (trimap id id (const ()) origBody)) body'
lowerNativeExprRaw _ _ (LoopN_ {}) =
  error "morloc bug: a native loop lowered without its original expression"
lowerNativeExprRaw cfg origExpr (MapOptionalN_ _ wireTf src innerDocs) = do
  idx <- lcNewIndex cfg
  -- Result type: optional<userT>. origExpr's TypeF is the outer
  -- optional in user-facing form.
  resultType <- lcTypeOf cfg (typeFof origExpr)
  -- Unwrap type: the wire-form inner (what the inner expression's
  -- optional dereferences to).
  unwrapType <- lcTypeOf cfg wireTf
  let v' = render $ helperNamer idx
      uVar = "u" <> T.pack (show idx)
      packerCall = ICall (render (lcSrcName cfg src)) Nothing [[IVar uVar]]
      ifStmt = IIfNotNull v' resultType (IRawExpr (render (poolExpr innerDocs))) uVar unwrapType [] packerCall
  return $ innerDocs
    { poolPriorLines = poolPriorLines innerDocs ++ [lcPrintStmt cfg ifStmt]
    , poolExpr = pretty v'
    }

{- | Lower a serial manifold to PoolDocs.
Replaces translateManifold from Common.hs for serial manifolds.
-}
lowerSerialManifold ::
  (Monad m) =>
  LowerConfig m ->
  SerialManifold ->
  SerialManifold_ PoolDocs ->
  m PoolDocs
lowerSerialManifold cfg (SerialManifold _ _ _ _ body) (SerialManifold_ m _ form headForm e) =
  lowerManifold cfg m form (Just headForm) (typeMof body) e

{- | Lower a native manifold to PoolDocs.
Replaces translateManifold from Common.hs for native manifolds.
-}
lowerNativeManifold ::
  (Monad m) =>
  LowerConfig m ->
  NativeManifold ->
  NativeManifold_ PoolDocs ->
  m PoolDocs
lowerNativeManifold cfg (NativeManifold _ _ _ body) (NativeManifold_ m _ form e) =
  lowerManifold cfg m form Nothing (typeMof body) e

-- | The manifold's def takes every argument of its form and returns its
-- body's value, whose type is the given 'TypeM' (a closure type when the
-- body is a function value). A closure form is then applied to its context
-- arguments to produce the closure value the surrounding expression uses.
lowerManifold ::
  (Monad m, HasTypeM t) =>
  LowerConfig m ->
  Int ->
  ManifoldForm (Or TypeS TypeF) t ->
  Maybe HeadManifoldForm ->
  TypeM ->
  PoolDocs ->
  m PoolDocs
lowerManifold cfg m form headForm bodyType bodyPool = do
  let PoolDocs completeManifolds bodyExpr priorLines priorExprs retFlag borrowed = bodyPool
      returned = render bodyExpr
      -- A parameter or a local bound by reference is copied when returned;
      -- any other name is a local the function owns.
      ownedByName =
        isIdentifierText returned
          && returned `notElem` map (render . argNamer) (typeMofForm form)
          && borrowed /= Just returned
  framed <- lcFrameReturn cfg m bodyType ownedByName bodyExpr
  -- Apply the return at the manifold boundary if a nested ReturnS_/ReturnN_
  -- set the return flag. Deferring it to here lets intermediate handlers
  -- (notably the serialize-let-wrap that rebinds the body expression to a
  -- temp before emitting a release call) keep poolExpr as a value rather
  -- than a return statement.
  let body = case (retFlag, framed) of
        (True, Just ret) -> ret
        (True, Nothing) -> lcReturn cfg bodyExpr
        (False, Nothing) -> bodyExpr
        (False, Just _) -> error $ "EMIT-3: manifold " <> show m <> " records its call but its body has no return"
      args = typeMofForm form
      mname = manNamer m
  maybeNewManifold <- lcMakeFunction cfg m mname args bodyType priorLines body headForm (isClosureForm form)
  call <- case form of
        (ManifoldPass _) -> do
          -- An unapplied manifold's callable signature is its whole type:
          -- every parameter is still to come.
          let sigType = Function [t | Arg _ t <- args] bodyType
          sig <- lcClosureSig cfg sigType
          lcMakePass cfg sig sigType mname args
        -- Wrap each manifold-call argument through lcSourcedArg (identity for
        -- most languages; the Rust member adapts each native arg to the callee's
        -- convention by ownership, so an already-borrowed arg passes as `&T` and
        -- an owned one is borrowed to `&T` -- never a double reference).
        (ManifoldFull rs) -> do
          -- The call arguments are named by index-aliasing after the CALLER's
          -- variables, so their ownership is read in the caller's scope. A
          -- non-Native (serialized) argument is an owned deserialized value.
          callArgs <- lcWithCallerScope cfg $
            zipWithM
              ( \j a@(Arg i t) -> do
                  own <- case t of
                    Native tf -> lcOwnership cfg (BndVarN tf i KeptRead)
                    _ -> return Owned
                  x <- case t of
                    Native tf -> lcKeepContextArg cfg m j i tf (argNamer a)
                    _ -> return (argNamer a)
                  return $ lcSourcedArg cfg (ManifoldArg m j) own t x
              )
              [0 ..]
              (typeMofRs rs)
          return $ mname <> tupled callArgs
        (ManifoldPart rs vs) -> do
          -- The closure's callable signature is result(bound...) -- only the
          -- REMAINING parameters, never the captured context args. Build it
          -- from the bound args and the body's type.
          let sigType = Function [typeMof t | Arg _ t <- vs] bodyType
          sig <- lcClosureSig cfg sigType
          lcMakeLambda cfg sig sigType mname (typeMofRs rs) [Arg i (typeMof t) | Arg i t <- vs]
  return $
    PoolDocs
      { poolCompleteManifolds = completeManifolds <> maybeToList maybeNewManifold
      , poolExpr = call
      , poolPriorLines = []
      , poolPriorExprs = priorExprs
      , poolReturnFlag = False
      , poolBorrowedLocal = Nothing
      }

-- | Bundle all six fold callbacks into a single FoldWithManifoldM record.
defaultFoldRules ::
  (Monad m) =>
  LowerConfig m ->
  FoldWithManifoldM m PoolDocs PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs)
defaultFoldRules cfg =
  FoldWithManifoldM
    { opFoldWithSerialManifoldM = lowerSerialManifold cfg
    , opFoldWithNativeManifoldM = lowerNativeManifold cfg
    , opFoldWithSerialExprM = lowerSerialExpr cfg
    , opFoldWithNativeExprM = lowerNativeExpr cfg
    , opFoldWithSerialArgM = \sr sa -> return $ case sa of
        SerialArgManifold_ x -> (typeSof sr, x)
        SerialArgExpr_ x -> (typeSof sr, x)
    -- An argument is typed by the value it passes: a saturated manifold is
    -- called in place, so it passes its result; a closure passes a function
    -- of its bound arguments only ('typeFof' of a 'NativeManifold').
    , opFoldWithNativeArgM = \nr na -> return $ case (nr, na) of
        (NativeArgManifold nm, NativeArgManifold_ x) -> (typeMof (typeFof nm), x)
        (_, NativeArgManifold_ x) -> (typeMof nr, x)
        (_, NativeArgExpr_ x) -> (typeMof nr, x)
    }

-- | Default serialization for languages without custom PoolDocs logic (Python, R).
defaultSerialize :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m PoolDocs
defaultSerialize cfg v s = do
  (expr, stmts) <- expandSerialize cfg v s
  return $
    defaultValue
      { poolExpr = lcPrintExpr cfg expr
      , poolPriorLines = map (lcPrintStmt cfg) stmts
      }

-- | Default deserialization for languages without custom logic (Python, R).
defaultDeserialize :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m (MDoc, [MDoc])
defaultDeserialize cfg v s = do
  (expr, stmts) <- expandDeserialize cfg v s
  return (lcPrintExpr cfg expr, map (lcPrintStmt cfg) stmts)

type IndexM = CMS.StateT IndexState Identity

-- | A constructor's position in its type's declaration order, which is its
-- wire tag.
--
-- The typechecker resolved this name against the type's own table before
-- codegen ran, so a miss is a compiler bug. Saying so beats returning a
-- past-the-end index, which is a tag no arm answers to and would surface as
-- a wrong branch rather than an error.
armIndex :: Text -> [Text] -> Int
armIndex n names = case [i | (i, m) <- zip [0 ..] names, m == n] of
  (i : _) -> i
  [] -> error ("compiler bug: no tag for constructor " <> show n)

isIdentifierText :: Text -> Bool
isIdentifierText t = case T.uncons t of
  Just (c, rest) -> (isAlpha c || c == '_') && T.all (\x -> isAlphaNum x || x == '_') rest
  Nothing -> False
