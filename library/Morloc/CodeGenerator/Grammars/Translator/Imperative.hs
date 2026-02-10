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
  , IStructDef (..)

    -- * Lowering: serialize/deserialize expansion
  , expandSerialize
  , expandDeserialize

    -- * Expression lowering (Stage 2)
  , lowerSerialExpr
  , lowerNativeExpr

    -- * Manifold lowering (Stage 3)
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
import Data.Scientific (Scientific)
import Data.Text (Text)
import Morloc.CodeGenerator.Grammars.Common (PoolDocs(..), mergePoolDocs, helperNamer, svarNamer, nvarNamer, argNamer, manNamer, provideClosure)
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Serial (isSerializable, serialAstToMsgpackSchema)
import Morloc.Data.Doc
import Morloc.Monad (IndexState)

-- Statements
data IStmt
  = IFunDef IFunMeta [IParam] [IStmt] IExpr
  | IAssign MDoc (Maybe IType) IExpr
  | IMapList MDoc (Maybe IType) MDoc MDoc [IStmt] IExpr
    -- ^ resultVar, resultType, iterVar, collection, bodyStmts, yieldExpr
    -- Python/C++: resultVar = []; for iterVar in collection: bodyStmts; resultVar.append(yieldExpr)
    -- R: resultVar <- lapply(collection, function(iterVar) { bodyStmts; yieldExpr })
    -- resultType is used by C++ for typed declarations; Python/R pass Nothing
  | IReturn IExpr
  | IExprStmt IExpr

-- Expressions
data IExpr
  = ICall MDoc (Maybe [IType]) [[IExpr]]
  | IVar MDoc
  | IBoolLit Bool
  | IIntLit Integer
  | IRealLit Scientific
  | IStrLit Text
  | INullLit
  | IListLit [IExpr]
  | ITupleLit [IExpr]
  | IRecordLit NamType FVar [(Key, IExpr)]
  | IAccess IExpr IAccessor
  | ISerCall MDoc IExpr             -- put_value(schema, expr)
  | IDesCall MDoc (Maybe IType) IExpr -- get_value[<T>](schema, expr); type used by C++ template
  | IForeignCall MDoc Int [IExpr]
  | IRemoteCall MDoc Int RemoteResources [IExpr]
  | ILambda [MDoc] IExpr
  | IPack MDoc IExpr          -- packer(expr)
  | IRawExpr MDoc

data IParam = IParam MDoc (Maybe IType)
newtype IType = IType MDoc

data IAccessor
  = IIdx Int
  | IKey Key
  | IField MDoc

data IFunMeta = IFunMeta
  { ifName :: MDoc
  , ifReturnType :: Maybe IType
  , ifHeadForm :: Maybe HeadManifoldForm
  }

data IProgram = IProgram
  { ipStructDefs :: [IStructDef]
  , ipSignatures :: [(MDoc, [IParam], Maybe IType)]
  , ipManifolds  :: [IStmt]
  , ipDispatch   :: [(Int, MDoc, Maybe HeadManifoldForm)]
  }

data IStructDef = IStructDef
  { isdName :: MDoc
  , isdTemplateParams :: [MDoc]
  , isdFields :: [(Key, IType)]
  , isdSerializer :: Maybe IStmt
  , isdDeserializer :: Maybe IStmt
  }

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
  , lcTemplateArgs :: [(Text, TypeF)] -> m (Maybe [IType])
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
    -- Stage 2: expression/arg lowering fields
  , lcPrintExpr :: IExpr -> MDoc
  , lcPrintStmt :: IStmt -> MDoc
  , lcEvalPattern :: TypeF -> Pattern -> [MDoc] -> m MDoc
    -- ^ Pattern evaluation (language-specific because patterns use
    -- language-specific constructors for tuples/records)
  , lcListConstructor :: FVar -> TypeF -> [MDoc] -> MDoc
    -- ^ Build a list literal from rendered elements. R needs FVar to choose c() vs list().
  , lcTupleConstructor :: FVar -> [MDoc] -> MDoc
  , lcRecordConstructor :: TypeF -> NamType -> FVar -> [TypeF] -> [(Key, MDoc)] -> m PoolDocs
    -- ^ Build a record literal. C++ needs type lookup + counter for temp var.
  , lcForeignCall :: MDoc -> Int -> [MDoc] -> MDoc
  , lcRemoteCall :: MDoc -> Int -> RemoteResources -> [MDoc] -> m PoolDocs
  , lcMakeLet :: (Int -> MDoc) -> Int -> Maybe TypeF -> PoolDocs -> PoolDocs -> m PoolDocs
    -- ^ Let binding assembly at the PoolDocs level
  , lcReturn :: MDoc -> MDoc
  , lcSerialize :: MDoc -> SerialAST -> m PoolDocs
  , lcDeserialize :: TypeF -> MDoc -> SerialAST -> m (MDoc, [MDoc])
    -- Stage 3: manifold lowering fields
  , lcMakeFunction :: MDoc -> [Arg TypeM] -> TypeM -> [MDoc] -> MDoc
                   -> Maybe HeadManifoldForm -> m (Maybe MDoc)
    -- ^ name, all args, manifold type, priorLines, body, headForm
    -- Returns Nothing if dedup'd (C++), Just funcDef otherwise
  , lcMakeLambda :: MDoc -> [MDoc] -> [MDoc] -> MDoc
    -- ^ name, contextArgs, boundArgs → partial application expression
  }

-- | Expand serialization into IR statements.
-- Returns (final expression representing the serialized value, prior statements).
expandSerialize :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m (IExpr, [IStmt])
expandSerialize cfg v0 s0 = do
  (stmts, vExpr) <- go v0 s0
  let schema = serialAstToMsgpackSchema s0
  return (ISerCall schema vExpr, stmts)
  where
    go v s
      | isSerializable s = return ([], IVar v)
      | otherwise = construct v s

    construct v (SerialPack _ (p, s)) =
      let unpacker = lcUnpackerName cfg (typePackerReverse p)
       in go (unpacker <> parens v) s

    construct v lst@(SerialList _ s) = do
      idx <- lcNewIndex cfg
      resultType <- lcSerialAstType cfg lst
      let v' = helperNamer idx
          iterVar = "i" <> pretty idx
      (before, x) <- go iterVar s
      return ([IMapList v' resultType iterVar v before x], IVar v')

    construct v tup@(SerialTuple _ ss) = do
      results <- zipWithM (\i s -> go (lcTupleAccessor cfg i v) s) [0..] ss
      let (befores, exprs) = unzip results
      idx <- lcNewIndex cfg
      typeM <- lcSerialAstType cfg tup
      let v' = helperNamer idx
      return (concat befores ++ [IAssign v' typeM (ITupleLit exprs)], IVar v')

    construct v obj@(SerialObject namType fv@(FV _ constructor) _ rs) = do
      let accessor = lcRecordAccessor cfg namType constructor
      results <- mapM (\(key, s) -> go (accessor v (pretty key)) s) rs
      let (befores, exprs) = unzip results
      idx <- lcNewIndex cfg
      typeM <- lcSerialAstType cfg obj
      let v' = helperNamer idx
      return
        ( concat befores ++ [IAssign v' typeM (IRecordLit namType fv (zip (map fst rs) exprs))]
        , IVar v'
        )

    construct _ _ = error "Unreachable in expandSerialize"

-- | Expand deserialization into IR statements.
-- Returns (final expression representing the deserialized value, prior statements).
expandDeserialize :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m (IExpr, [IStmt])
expandDeserialize cfg v0 s0
  | isSerializable s0 = do
      let schema = serialAstToMsgpackSchema s0
      desType <- lcDeserialAstType cfg s0
      return (IDesCall schema desType (IVar v0), [])
  | otherwise = do
      idx <- lcNewIndex cfg
      rawType <- lcRawDeserialAstType cfg s0
      let rawvar = helperNamer idx
          schema = serialAstToMsgpackSchema s0
      (x, befores) <- check rawvar s0
      return (x, IAssign rawvar rawType (IDesCall schema rawType (IVar v0)) : befores)
  where
    check v s
      | isSerializable s = return (IVar v, [])
      | otherwise = construct v s

    construct v (SerialPack _ (p, s')) = do
      (x, before) <- check v s'
      let packer = lcPackerName cfg (typePackerForward p)
      return (IPack packer x, before)

    construct v lst@(SerialList _ s) = do
      idx <- lcNewIndex cfg
      resultType <- lcDeserialAstType cfg lst
      let v' = helperNamer idx
          iterVar = "i" <> pretty idx
      (x, before) <- check iterVar s
      return (IVar v', [IMapList v' resultType iterVar v before x])

    construct v tup@(SerialTuple _ ss) = do
      results <- zipWithM (\i s -> check (lcTupleAccessor cfg i v) s) [0..] ss
      let (exprs, befores) = unzip results
      typeM <- lcDeserialAstType cfg tup
      v' <- helperNamer <$> lcNewIndex cfg
      return (IVar v', concat befores ++ [IAssign v' typeM (ITupleLit exprs)])

    construct v (SerialObject namType fv@(FV _ constructor) _ rs) = do
      let accessor = lcDeserialRecordAccessor cfg
      results <- zipWithM (\i (k, s) -> check (accessor i k v) s) [0..] rs
      let (exprs, befores) = unzip results
      typeM <- lcDeserialAstType cfg (SerialObject namType fv [] rs)
      idx <- lcNewIndex cfg
      let v' = helperNamer idx
      return (IVar v', concat befores ++ [IAssign v' typeM (IRecordLit namType fv (zip (map fst rs) exprs))])

    construct _ _ = error "Unreachable in expandDeserialize"

-- | Lower a serial expression to PoolDocs via the IR.
-- Replaces genericMakeSerialExpr from Syntax.hs.
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
lowerSerialExpr cfg _ (ReturnS_ x) = return $ x {poolExpr = lcReturn cfg (poolExpr x)}
lowerSerialExpr cfg _ (SerialLetS_ i e1 e2) = lcMakeLet cfg svarNamer i Nothing e1 e2
lowerSerialExpr cfg (NativeLetS _ (typeFof -> t) _) (NativeLetS_ i e1 e2) = lcMakeLet cfg nvarNamer i (Just t) e1 e2
lowerSerialExpr cfg _ (NativeLetS_ i e1 e2) = lcMakeLet cfg nvarNamer i Nothing e1 e2
lowerSerialExpr _ _ (LetVarS_ _ i) = return $ defaultValue {poolExpr = svarNamer i}
lowerSerialExpr _ _ (BndVarS_ _ i) = return $ defaultValue {poolExpr = svarNamer i}
lowerSerialExpr cfg _ (SerializeS_ s e) = do
  se <- lcSerialize cfg (poolExpr e) s
  return $ e {poolExpr = poolExpr se, poolPriorLines = poolPriorLines e <> poolPriorLines se}

-- | Lower a native expression to PoolDocs via the IR.
-- Replaces genericMakeNativeExpr from Syntax.hs.
lowerNativeExpr ::
  (Monad m) =>
  LowerConfig m ->
  NativeExpr ->
  NativeExpr_ PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs) ->
  m PoolDocs
lowerNativeExpr cfg _ (AppExeN_ _ (SrcCallP src) qs (map snd -> es)) = do
  templateArgs <- lcTemplateArgs cfg qs
  let handleFunctionArgs ts =
        (<>) (lcSrcName cfg src <> printTemplateArgs ts)
          . hsep
          . map tupled
          . provideClosure src
      printTemplateArgs Nothing = ""
      printTemplateArgs (Just ts) = encloseSep "<" ">" "," [t' | IType t' <- ts]
  return $ mergePoolDocs (handleFunctionArgs templateArgs) es
lowerNativeExpr cfg _ (AppExeN_ t (PatCallP p) _ xs) = do
  let es = map snd xs
  patResult <- lcEvalPattern cfg t p (map poolExpr es)
  return $ PoolDocs
    { poolCompleteManifolds = concatMap poolCompleteManifolds es
    , poolExpr = patResult
    , poolPriorLines = concatMap poolPriorLines es
    , poolPriorExprs = concatMap poolPriorExprs es
    }
lowerNativeExpr cfg _ (AppExeN_ _ (LocalCallP idx) qs (map snd -> es)) = do
  templateArgs <- lcTemplateArgs cfg qs
  let printTemplateArgs Nothing = ""
      printTemplateArgs (Just ts) = encloseSep "<" ">" "," [t' | IType t' <- ts]
  return $ mergePoolDocs ((<>) (nvarNamer idx <> printTemplateArgs templateArgs) . tupled) es
lowerNativeExpr _ _ (ManN_ call) = return call
lowerNativeExpr cfg _ (ReturnN_ x) =
  return $ x {poolExpr = lcReturn cfg (poolExpr x)}
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
lowerNativeExpr cfg _ (ListN_ v t xs) = return $ mergePoolDocs (lcListConstructor cfg v t) xs
lowerNativeExpr cfg _ (TupleN_ v xs) = return $ mergePoolDocs (lcTupleConstructor cfg v) xs
lowerNativeExpr cfg origExpr (RecordN_ o v ps rs) = do
  let es = map snd rs
      recType = typeFof origExpr
  rec' <- lcRecordConstructor cfg recType o v ps (zip (map fst rs) (map poolExpr es))
  return $ rec'
    { poolCompleteManifolds = concatMap poolCompleteManifolds es <> poolCompleteManifolds rec'
    , poolPriorLines = concatMap poolPriorLines es <> poolPriorLines rec'
    , poolPriorExprs = concatMap poolPriorExprs es <> poolPriorExprs rec'
    }
lowerNativeExpr cfg _ (LogN_ _ v) = return $ defaultValue {poolExpr = lcPrintExpr cfg (IBoolLit v)}
lowerNativeExpr cfg _ (RealN_ _ v) = return $ defaultValue {poolExpr = lcPrintExpr cfg (IRealLit v)}
lowerNativeExpr cfg _ (IntN_ _ v) = return $ defaultValue {poolExpr = lcPrintExpr cfg (IIntLit v)}
lowerNativeExpr cfg _ (StrN_ _ v) = return $ defaultValue {poolExpr = lcPrintExpr cfg (IStrLit v)}
lowerNativeExpr cfg _ (NullN_ _) = return $ defaultValue {poolExpr = lcPrintExpr cfg INullLit}

-- | Lower a serial manifold to PoolDocs.
-- Replaces translateManifold from Common.hs for serial manifolds.
lowerSerialManifold ::
  (Monad m) =>
  LowerConfig m ->
  SerialManifold ->
  SerialManifold_ PoolDocs ->
  m PoolDocs
lowerSerialManifold cfg sm (SerialManifold_ m _ form headForm e) =
  lowerManifold cfg m form (Just headForm) (typeMof sm) e

-- | Lower a native manifold to PoolDocs.
-- Replaces translateManifold from Common.hs for native manifolds.
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
lowerManifold cfg m form headForm manifoldType (PoolDocs completeManifolds body priorLines priorExprs) = do
  let args = typeMofForm form
      mname = manNamer m
  maybeNewManifold <- lcMakeFunction cfg mname args manifoldType priorLines body headForm
  let call = case form of
        (ManifoldPass _) -> mname
        (ManifoldFull rs) -> mname <> tupled (map argNamer (typeMofRs rs))
        (ManifoldPart rs vs) ->
          lcMakeLambda cfg
            mname
            (map argNamer (typeMofRs rs))
            [argNamer (Arg i (typeMof t)) | Arg i t <- vs]
  return $ PoolDocs
    { poolCompleteManifolds = completeManifolds <> maybeToList maybeNewManifold
    , poolExpr = call
    , poolPriorLines = []
    , poolPriorExprs = priorExprs
    }

-- | Bundle all six fold callbacks into a single FoldWithManifoldM record.
defaultFoldRules ::
  (Monad m) =>
  LowerConfig m ->
  FoldWithManifoldM m PoolDocs PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs)
defaultFoldRules cfg = FoldWithManifoldM
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
  return $ defaultValue
    { poolExpr = lcPrintExpr cfg expr
    , poolPriorLines = map (lcPrintStmt cfg) stmts
    }

-- | Default deserialization for languages without custom logic (Python, R).
defaultDeserialize :: (Monad m) => LowerConfig m -> MDoc -> SerialAST -> m (MDoc, [MDoc])
defaultDeserialize cfg v s = do
  (expr, stmts) <- expandDeserialize cfg v s
  return (lcPrintExpr cfg expr, map (lcPrintStmt cfg) stmts)

type IndexM = CMS.StateT IndexState Identity
