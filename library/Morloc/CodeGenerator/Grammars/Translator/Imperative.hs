{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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

    -- * Lowering: serialize/deserialize expansion
  , expandSerialize
  , expandDeserialize

    -- * Expression lowering
  , lowerSerialExpr
  , lowerNativeExpr

    -- * Manifold lowering
  , lowerSerialManifold
  , lowerNativeManifold
  , defaultFoldRules

    -- * Full lowering config
  , LowerConfig (..)

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
import qualified Data.Text as T
import GHC.Generics (Generic)
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
  , svarNamer
  )
import Morloc.CodeGenerator.LogTemplate (RenderedTemplate (..))
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Serial (isSerializable, serialAstToMsgpackSchema)
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
  | IListLit [IExpr]
  | ITupleLit [IExpr]
  | IRecordLit NamType FVar [(Key, IExpr)]
  | IAccess IExpr IAccessor
  | ISerCall Int IExpr -- put_value(schemaId, expr)
  | IDesCall Int (Maybe IType) IExpr -- get_value[<T>](schemaId, expr); type used by C++ template
  | IForeignCall Text Int [IExpr]
  | IRemoteCall Text Int RemoteResources [IExpr]
  | ILambda [Text] IExpr
  | IPack Text IExpr -- packer(expr)
  | IRawExpr Text
  | IDoBlock IExpr -- effect: lambda wrapping expression
  | IEval IExpr -- eval: call effect with no args
  | IIntrinsicSave Text Int IExpr IExpr -- format, schemaId, data, path
  | IIntrinsicLoad Int (Maybe IType) IExpr -- schemaId, returnType, path -> result (nullable)
  | IIntrinsicHash Int IExpr -- schemaId, data -> hex string
  | IIntrinsicShow Int IExpr -- schemaId, data -> JSON string
  | IIntrinsicRead Int (Maybe IType) IExpr -- schemaId, returnType, json_string -> typed data (nullable)

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
  , ipManifolds :: [Text]
  , ipLocalDispatch :: [DispatchEntry]
  , ipRemoteDispatch :: [DispatchEntry]
  , ipSchemaTable :: [Text]  -- ordered list of schema strings; index = schema ID
  , ipLogTemplates :: Map.Map Int RenderedTemplate
    -- ^ Per-labeled-midx pre-rendered log templates. See 'LogTemplate'.
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
  [MDoc] ->
  [MDoc] ->
  [SerialManifold] ->
  [Text] ->
  IProgram
buildProgram labels templates sources manifolds es schemas =
  let definedIds = collectManifoldIds es
      foreignCalleeIds =
        Set.fromList [i | SerialManifold i _ _ f _ <- es, isForeignCalleeForm (Just f)]
      labels' = Map.restrictKeys labels definedIds
                  `Map.withoutKeys` foreignCalleeIds
      templates' = Map.restrictKeys templates definedIds
                     `Map.withoutKeys` foreignCalleeIds
   in IProgram
        { ipSources = map render sources
        , ipManifolds = map render manifolds
        , ipLocalDispatch = extractLocalDispatch labels' es
        , ipRemoteDispatch = extractRemoteDispatch labels' es
        , ipSchemaTable = schemas
        , ipLogTemplates = templates'
        }

-- | Build an IProgram monadically (for C++ where translateSegment runs in a monad).
buildProgramM ::
  (Monad m) =>
  Map.Map Int Text ->
  Map.Map Int RenderedTemplate ->
  [MDoc] ->
  [SerialManifold] ->
  (SerialManifold -> m MDoc) ->
  m [Text] ->
  m IProgram
buildProgramM labels templates sources es translateSeg getSchemas = do
  manifolds <- mapM translateSeg es
  schemas <- getSchemas
  return $ buildProgram labels templates sources manifolds es schemas

-- | Per-language configuration for lowering
data LowerConfig m = LowerConfig
  { lcSrcName :: Source -> MDoc
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
  , lcForeignCall :: MDoc -> Int -> [MDoc] -> MDoc
  , lcRemoteCall :: MDoc -> Int -> RemoteResources -> [MDoc] -> m PoolDocs
  , lcMakeLet :: (Int -> MDoc) -> Int -> Maybe TypeF -> PoolDocs -> PoolDocs -> m PoolDocs
  -- ^ Let binding assembly at the PoolDocs level
  , lcReleaseStmt :: Text -> MDoc
  -- ^ Produce a statement releasing the SHM owned by a serialize-let-bound
  -- packet variable. Called at the end of a serialize let's body so the
  -- per-call SHM tracker entry can be dropped as soon as the body finishes
  -- using the packet, rather than accumulating until the next dispatch flush.
  -- For inline packets (no SHM), the runtime function this targets is a
  -- no-op, so emitting the call unconditionally is safe.
  , lcReturn :: MDoc -> MDoc
  , lcMakeIf :: NativeExpr -> PoolDocs -> PoolDocs -> PoolDocs -> m PoolDocs
  -- ^ origExpr, condDocs, thenDocs, elseDocs -> result PoolDocs
  -- Produces language-specific if/else structure using a temp result variable
  , lcMakeDoBlock :: TypeF -> [MDoc] -> MDoc -> m ([MDoc], MDoc)
  -- ^ type -> prior statements -> return expression -> (hoisted lines,
  -- suspended-thunk expression). Monadic so a language whose thunk form
  -- cannot hold statements (e.g. a Python lambda) can mint a fresh name
  -- for a hoisted def-thunk.
  , lcSerialize :: MDoc -> SerialAST -> m PoolDocs
  , lcDeserialize :: TypeF -> MDoc -> SerialAST -> m (MDoc, [MDoc])
  , -- manifold lowering fields
    lcMakeFunction ::
      MDoc ->
      [Arg TypeM] ->
      TypeM ->
      [MDoc] ->
      MDoc ->
      Maybe HeadManifoldForm ->
      m (Maybe MDoc)
  -- ^ name, all args, manifold type, priorLines, body, headForm
  -- Returns Nothing if dedup'd (C++), Just funcDef otherwise
  , lcMakeLambda :: MDoc -> [MDoc] -> [MDoc] -> MDoc
  -- ^ name, contextArgs, boundArgs - partial application expression
  , lcRegisterSchema :: Text -> m Int
  -- ^ Register a schema string and return its unique ID (index into schema table)
  }

{- | Expand serialization into IR statements.
Returns (final expression representing the serialized value, prior statements).
-}
expandSerialize :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m (IExpr, [IStmt])
expandSerialize cfg v0 s0 = do
  (stmts, vExpr) <- go v0 s0
  schemaId <- lcRegisterSchema cfg (render $ serialAstToMsgpackSchema s0)
  return (ISerCall schemaId vExpr, stmts)
  where
    go v s
      | isSerializable s = return ([], IRawExpr (render v))
      | otherwise = construct v s

    construct v (SerialPack _ (p, s)) =
      let unpacker = lcUnpackerName cfg (typePackerReverse p)
       in go (unpacker <> parens v) s
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
    construct _ _ = error "Unreachable in expandSerialize"

{- | Expand deserialization into IR statements.
Returns (final expression representing the deserialized value, prior statements).
-}
expandDeserialize :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m (IExpr, [IStmt])
expandDeserialize cfg v0 s0
  | isSerializable s0 = do
      schemaId <- lcRegisterSchema cfg (render $ serialAstToMsgpackSchema s0)
      desType <- lcDeserialAstType cfg s0
      return (IDesCall schemaId desType (IRawExpr (render v0)), [])
  | otherwise = do
      idx <- lcNewIndex cfg
      rawType <- lcRawDeserialAstType cfg s0
      let rawvar = render $ helperNamer idx
      schemaId <- lcRegisterSchema cfg (render $ serialAstToMsgpackSchema s0)
      (x, befores) <- check (helperNamer idx) s0
      return (x, IAssign rawvar rawType (IDesCall schemaId rawType (IRawExpr (render v0))) : befores)
  where
    check v s
      | isSerializable s = return (IRawExpr (render v), [])
      | otherwise = construct v s

    construct v (SerialPack _ (p, s')) = do
      (x, before) <- check v s'
      let packer = render $ lcPackerName cfg (typePackerForward p)
      return (IPack packer x, before)
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
    construct _ _ = error "Unreachable in expandDeserialize"

-- | Lower a serial expression to PoolDocs via the IR.
lowerSerialExpr ::
  (Monad m) =>
  LowerConfig m ->
  SerialExpr ->
  SerialExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) ->
  m PoolDocs
lowerSerialExpr _ _ (ManS_ f) = return f
lowerSerialExpr cfg _ (AppPoolS_ _ (PoolCall mid (Socket _ _ socketFile) ForeignCall args) _) =
  return $ defaultValue {poolExpr = lcForeignCall cfg socketFile mid (map argNamer args)}
lowerSerialExpr cfg _ (AppPoolS_ _ (PoolCall mid (Socket _ _ socketFile) (RemoteCall res) args) _) =
  lcRemoteCall cfg socketFile mid res (map argNamer args)
lowerSerialExpr _ _ (AppRecS_ _ mid es) = do
  return $ mergePoolDocs ((<>) (manNamer mid) . tupled) es
lowerSerialExpr cfg _ (AppForeignRecS_ _ mid (Socket _ _ socketFile) es) = do
  return $ mergePoolDocs (\args -> lcForeignCall cfg socketFile mid args) es
lowerSerialExpr _ _ (ReturnS_ x) = return $ x {poolReturnFlag = True}
lowerSerialExpr cfg (SerialLetS _ (SerializeS _ _) _) (SerialLetS_ i e1 e2) = do
  -- The let RHS is a SerializeS, so the bound variable owns a put_value
  -- tracker entry. Wrap the body to bind its result to a temp helper var,
  -- emit a release call against the let-bound variable, and expose the
  -- temp as the new pool expression. This makes the SHM ref's lifetime
  -- end exactly at the body's last use of the bound variable rather than
  -- carrying it to the outer dispatch boundary.
  letResult <- lcMakeLet cfg svarNamer i Nothing e1 e2
  tmpIdx <- lcNewIndex cfg
  let releaseLine = lcReleaseStmt cfg (render (svarNamer i))
      releaseBody =
        defaultValue
          { poolExpr = helperNamer tmpIdx
          , poolPriorLines = [releaseLine]
          }
  lcMakeLet cfg helperNamer tmpIdx Nothing letResult releaseBody
lowerSerialExpr cfg _ (SerialLetS_ i e1 e2) =
  lcMakeLet cfg svarNamer i Nothing e1 e2
lowerSerialExpr cfg (NativeLetS _ (typeFof -> t) _) (NativeLetS_ i e1 e2) =
  lcMakeLet cfg nvarNamer i (Just t) e1 e2
lowerSerialExpr cfg _ (NativeLetS_ i e1 e2) =
  lcMakeLet cfg nvarNamer i Nothing e1 e2
lowerSerialExpr _ _ (LetVarS_ _ i) = return $ defaultValue {poolExpr = svarNamer i}
lowerSerialExpr _ _ (BndVarS_ _ i) = return $ defaultValue {poolExpr = svarNamer i}
lowerSerialExpr cfg _ (SerializeS_ s e) = do
  se <- lcSerialize cfg (poolExpr e) s
  return $ e {poolExpr = poolExpr se, poolPriorLines = poolPriorLines e <> poolPriorLines se}

-- | Lower a native expression to PoolDocs via the IR.
lowerNativeExpr ::
  (Monad m) =>
  LowerConfig m ->
  NativeExpr ->
  NativeExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) ->
  m PoolDocs
-- Binary operator: emit (lhs op rhs) instead of function call
lowerNativeExpr _ _ (AppExeN_ _ (SrcCallP src) (map snd -> [lhs, rhs]))
  | srcOperator src =
      return $ mergePoolDocs (\xs -> case xs of [l, r] -> parens (l <+> pretty (unSrcName (srcName src)) <+> r); _ -> error "binary operator requires exactly 2 args") [lhs, rhs]
lowerNativeExpr cfg _ (AppExeN_ _ (SrcCallP src) (map snd -> es)) = do
  let handleFunctionArgs =
        (<>) (lcSrcName cfg src)
          . hsep
          . map tupled
          . provideClosure src
  return $ mergePoolDocs handleFunctionArgs es
lowerNativeExpr cfg _ (AppExeN_ t (PatCallP p) xs) = do
  let es = map snd xs
  patResult <- lcEvalPattern cfg t p (map poolExpr es)
  return $
    PoolDocs
      { poolCompleteManifolds = concatMap poolCompleteManifolds es
      , poolExpr = patResult
      , poolPriorLines = concatMap poolPriorLines es
      , poolPriorExprs = concatMap poolPriorExprs es
      , poolReturnFlag = any poolReturnFlag es
      }
lowerNativeExpr _ _ (AppExeN_ _ (LocalCallP idx) (map snd -> es)) = do
  return $ mergePoolDocs ((<>) (nvarNamer idx) . tupled) es
lowerNativeExpr _ _ (AppExeN_ _ (RecCallP mid _) (map snd -> es)) = do
  return $ mergePoolDocs ((<>) (manNamer mid) . tupled) es
lowerNativeExpr _ _ (ManN_ call) = return call
lowerNativeExpr _ _ (ReturnN_ x) =
  return $ x {poolReturnFlag = True}
lowerNativeExpr cfg (SerialLetN _ (SerializeS _ _) body) (SerialLetN_ i x1 x2) = do
  -- Same shape as the SerialLetS-with-SerializeS case in lowerSerialExpr:
  -- the let-bound variable owns a put_value tracker entry, so wrap the
  -- body's result with a temp and emit a release call once the body
  -- finishes using the bound variable. This branch covers inner (native-
  -- returning) manifolds whose put_value let-bindings live in NativeExpr
  -- via SerialLetN -- e.g., the m1417-style wrapper around a foreign call.
  -- The body is a NativeExpr, so the temp's declared type must match it
  -- (rather than falling back to the serial type used for SerialLetS).
  letResult <- lcMakeLet cfg svarNamer i Nothing x1 x2
  tmpIdx <- lcNewIndex cfg
  let bodyT = typeFof body
      releaseLine = lcReleaseStmt cfg (render (svarNamer i))
      releaseBody =
        defaultValue
          { poolExpr = helperNamer tmpIdx
          , poolPriorLines = [releaseLine]
          }
  lcMakeLet cfg helperNamer tmpIdx (Just bodyT) letResult releaseBody
lowerNativeExpr cfg _ (SerialLetN_ i x1 x2) = lcMakeLet cfg svarNamer i Nothing x1 x2
lowerNativeExpr cfg (NativeLetN _ (typeFof -> t) _) (NativeLetN_ i x1 x2) = lcMakeLet cfg nvarNamer i (Just t) x1 x2
lowerNativeExpr cfg _ (NativeLetN_ i x1 x2) = lcMakeLet cfg nvarNamer i Nothing x1 x2
lowerNativeExpr _ _ (LetVarN_ _ i) = return $ defaultValue {poolExpr = nvarNamer i}
lowerNativeExpr _ _ (BndVarN_ _ i) = return $ defaultValue {poolExpr = nvarNamer i}
lowerNativeExpr cfg _ (DeserializeN_ t s x) = do
  (deserialized, assignments) <- lcDeserialize cfg t (poolExpr x) s
  return $
    x
      { poolExpr = deserialized
      , poolPriorLines = poolPriorLines x <> assignments
      }
lowerNativeExpr cfg _ (ExeN_ _ (SrcCallP src)) = return $ defaultValue {poolExpr = lcSrcName cfg src}
lowerNativeExpr _ _ (ExeN_ _ (PatCallP _)) = error "Unreachable: patterns are always used in applications"
lowerNativeExpr _ _ (ExeN_ _ (LocalCallP idx)) = return $ defaultValue {poolExpr = nvarNamer idx}
lowerNativeExpr _ _ (ExeN_ _ (RecCallP mid _)) = return $ defaultValue {poolExpr = manNamer mid}
lowerNativeExpr cfg _ (ListN_ v t xs) = return $ mergePoolDocs (lcListConstructor cfg v t) xs
lowerNativeExpr cfg origExpr (TupleN_ v xs) =
  let slotTypes = case typeFof origExpr of
        AppF _ ts -> ts
        _ -> []
  in return $ mergePoolDocs (lcTupleConstructor cfg v slotTypes) xs
lowerNativeExpr cfg origExpr (RecordN_ o v ps rs) = do
  let es = map snd rs
      recType = typeFof origExpr
  rec' <- lcRecordConstructor cfg recType o v ps (zip (map fst rs) (map poolExpr es))
  return $
    rec'
      { poolCompleteManifolds = concatMap poolCompleteManifolds es <> poolCompleteManifolds rec'
      , poolPriorLines = concatMap poolPriorLines es <> poolPriorLines rec'
      , poolPriorExprs = concatMap poolPriorExprs es <> poolPriorExprs rec'
      }
lowerNativeExpr cfg _ (LogN_ _ v) = return $ defaultValue {poolExpr = lcPrintExpr cfg (IBoolLit v)}
lowerNativeExpr cfg _ (RealN_ (FV _ cv) v) = return $ defaultValue {poolExpr = lcPrintExpr cfg (IRealLit (Just (unCVar cv)) v)}
lowerNativeExpr cfg _ (IntN_ (FV _ cv) v) = return $ defaultValue {poolExpr = lcPrintExpr cfg (IIntLit (Just (unCVar cv)) v)}
lowerNativeExpr cfg _ (StrN_ (FV _ cv) v) =
  let hint = if cv == CV "" then Nothing else Just (unCVar cv)
  in return $ defaultValue {poolExpr = lcPrintExpr cfg (IStrLit hint v)}
lowerNativeExpr cfg _ (NullN_ t) = do
  -- NullN_ now carries the full @TypeF@ of the Null's type slot
  -- (e.g. @?(BTree Int)@), not just the underlying constructor's
  -- @FVar@. Pass it through @lcTypeOf@ to get the IType the printer
  -- uses for the null literal -- though most language printers
  -- ignore this IType (Python emits "None", C++ emits "std::nullopt"
  -- unconditionally), threading it preserves the option for languages
  -- that do type-tagged null forms.
  mayT <- lcTypeOf cfg t
  return $ defaultValue {poolExpr = lcPrintExpr cfg (INullLit mayT)}
lowerNativeExpr cfg _ (DoBlockN_ t x) = do
  (hoisted, effectExpr) <- lcMakeDoBlock cfg t (poolPriorLines x) (poolExpr x)
  return
    defaultValue
      { poolExpr = effectExpr
      , poolCompleteManifolds = poolCompleteManifolds x
      , poolPriorLines = hoisted
      , poolPriorExprs = poolPriorExprs x
      }
lowerNativeExpr cfg _ (EvalN_ _ x) = return $ x {poolExpr = lcPrintExpr cfg (IEval (IRawExpr (render (poolExpr x))))}
-- CoerceToOptional is a noop in all target languages: T is a valid ?T
lowerNativeExpr _ _ (CoerceN_ CoerceToOptional _ x) = return x
lowerNativeExpr cfg origExpr (IfN_ _ condDocs thenDocs elseDocs) =
  lcMakeIf cfg origExpr condDocs thenDocs elseDocs
lowerNativeExpr cfg _ (IntrinsicN_ _ IntrHash (Just schema) [dataDocs]) = do
  sid <- lcRegisterSchema cfg schema
  return $ dataDocs {poolExpr = lcPrintExpr cfg (IIntrinsicHash sid (IRawExpr (render (poolExpr dataDocs))))}
lowerNativeExpr cfg _ (IntrinsicN_ _ IntrSave (Just schema) [dataDocs, pathDocs]) = do
  sid <- lcRegisterSchema cfg schema
  let fmt = "voidstar"
   in return $ mergePoolDocs (const $ lcPrintExpr cfg (IIntrinsicSave fmt sid (IRawExpr (render (poolExpr dataDocs))) (IRawExpr (render (poolExpr pathDocs))))) [dataDocs, pathDocs]
lowerNativeExpr cfg _ (IntrinsicN_ _ IntrSaveM (Just schema) [dataDocs, pathDocs]) = do
  sid <- lcRegisterSchema cfg schema
  let fmt = "msgpack"
   in return $ mergePoolDocs (const $ lcPrintExpr cfg (IIntrinsicSave fmt sid (IRawExpr (render (poolExpr dataDocs))) (IRawExpr (render (poolExpr pathDocs))))) [dataDocs, pathDocs]
lowerNativeExpr cfg _ (IntrinsicN_ _ IntrSaveJ (Just schema) [dataDocs, pathDocs]) = do
  sid <- lcRegisterSchema cfg schema
  let fmt = "json"
   in return $ mergePoolDocs (const $ lcPrintExpr cfg (IIntrinsicSave fmt sid (IRawExpr (render (poolExpr dataDocs))) (IRawExpr (render (poolExpr pathDocs))))) [dataDocs, pathDocs]
lowerNativeExpr cfg origExpr (IntrinsicN_ _ IntrLoad (Just schema) [pathDocs]) = do
  sid <- lcRegisterSchema cfg schema
  innerType <- case typeFof origExpr of
    OptionalF t -> lcTypeOf cfg t
    _ -> return Nothing
  return $ pathDocs {poolExpr = lcPrintExpr cfg (IIntrinsicLoad sid innerType (IRawExpr (render (poolExpr pathDocs))))}
lowerNativeExpr cfg _ (IntrinsicN_ _ IntrShow (Just schema) [dataDocs]) = do
  sid <- lcRegisterSchema cfg schema
  return $ dataDocs {poolExpr = lcPrintExpr cfg (IIntrinsicShow sid (IRawExpr (render (poolExpr dataDocs))))}
lowerNativeExpr cfg origExpr (IntrinsicN_ _ IntrRead (Just schema) [strDocs]) = do
  sid <- lcRegisterSchema cfg schema
  innerType <- case typeFof origExpr of
    OptionalF t -> lcTypeOf cfg t
    _ -> return Nothing
  return $ strDocs {poolExpr = lcPrintExpr cfg (IIntrinsicRead sid innerType (IRawExpr (render (poolExpr strDocs))))}
-- @schema and @typeof erase their argument: the result is a compile-time
-- constant string (the schema or user-facing type name), already resolved
-- into the Intrinsic node's schema slot by Serialize.hs. Emit it as a
-- string literal and discard the data expression; the type of the
-- argument is all that matters.
lowerNativeExpr cfg _ (IntrinsicN_ _ IntrSchema (Just s) [dataDocs]) =
  return $ dataDocs {poolExpr = lcPrintExpr cfg (IStrLit Nothing s)}
lowerNativeExpr cfg _ (IntrinsicN_ _ IntrTypeof (Just s) [dataDocs]) =
  return $ dataDocs {poolExpr = lcPrintExpr cfg (IStrLit Nothing s)}
lowerNativeExpr _ _ (IntrinsicN_ _ intr _ _) =
  error $ "Runtime intrinsic @" <> show intr <> " reached code generation without schema"
-- Lift a source function through an Optional. Mirrors how
-- expandDeserialize's SerialPack arm wraps an inner deserialization
-- with a packer call, but at the NativeExpr level: the inner
-- expression evaluates to optional<wireT> (e.g. the wire form returned
-- by @load), and we produce optional<userT> by applying `src` to the
-- inner value when present.
lowerNativeExpr cfg origExpr (MapOptionalN_ _ wireTf src innerDocs) = do
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
lowerSerialManifold cfg sm (SerialManifold_ m _ form headForm e) =
  lowerManifold cfg m form (Just headForm) (typeMof sm) e

{- | Lower a native manifold to PoolDocs.
Replaces translateManifold from Common.hs for native manifolds.
-}
lowerNativeManifold ::
  (Monad m) =>
  LowerConfig m ->
  NativeManifold ->
  NativeManifold_ PoolDocs ->
  m PoolDocs
lowerNativeManifold cfg nm (NativeManifold_ m _ form e) =
  lowerManifold cfg m form Nothing (typeMof nm) e

lowerManifold ::
  (Monad m, HasTypeM t) =>
  LowerConfig m ->
  Int ->
  ManifoldForm (Or TypeS TypeF) t ->
  Maybe HeadManifoldForm ->
  TypeM ->
  PoolDocs ->
  m PoolDocs
lowerManifold cfg m form headForm manifoldType bodyPool = do
  let PoolDocs completeManifolds bodyExpr priorLines priorExprs retFlag = bodyPool
      -- Apply lcReturn at the manifold boundary if a nested ReturnS_/ReturnN_
      -- set the return flag. Deferring lcReturn to here lets intermediate
      -- handlers (notably the serialize-let-wrap that rebinds the body
      -- expression to a temp before emitting a release call) keep
      -- poolExpr as a value rather than a return statement.
      body = if retFlag then lcReturn cfg bodyExpr else bodyExpr
      args = typeMofForm form
      mname = manNamer m
  maybeNewManifold <- lcMakeFunction cfg mname args manifoldType priorLines body headForm
  let call = case form of
        (ManifoldPass _) -> mname
        (ManifoldFull rs) -> mname <> tupled (map argNamer (typeMofRs rs))
        (ManifoldPart rs vs) ->
          lcMakeLambda
            cfg
            mname
            (map argNamer (typeMofRs rs))
            [argNamer (Arg i (typeMof t)) | Arg i t <- vs]
  return $
    PoolDocs
      { poolCompleteManifolds = completeManifolds <> maybeToList maybeNewManifold
      , poolExpr = call
      , poolPriorLines = []
      , poolPriorExprs = priorExprs
      , poolReturnFlag = False
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
    , opFoldWithNativeArgM = \nr na -> return $ case na of
        NativeArgManifold_ x -> (typeMof nr, x)
        NativeArgExpr_ x -> (typeMof nr, x)
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

