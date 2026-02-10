{-# LANGUAGE OverloadedStrings #-}

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

    -- * Full lowering config
  , LowerConfig (..)
  ) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Morloc.CodeGenerator.Grammars.Common (helperNamer)
import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Serial (isSerializable, serialAstToMsgpackSchema)
import Morloc.Data.Doc

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
