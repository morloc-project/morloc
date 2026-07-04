{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Nexus
Description : Generate the @.manifest@ JSON file consumed by the pre-compiled nexus
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Produces the JSON manifest that the static nexus binary reads at startup.
The manifest describes all exported subcommands, their argument types,
help text, and which pool executables to dispatch to.
-}
module Morloc.CodeGenerator.Nexus
  ( generate
  ) where

import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BSL
import Data.Int (Int64)
import Data.Word (Word8)
import qualified Data.Map as Map
import qualified Data.Scientific as DS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as MT
import qualified Data.Text.Encoding as TE
import qualified Data.Time.Clock
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Format
import qualified Data.Vector as V
import qualified Morloc.BaseTypes as MBT
import qualified Morloc.CodeGenerator.Docstrings as Docstrings
import qualified Morloc.CodeGenerator.Infer as Infer
import Morloc.CodeGenerator.LogTemplate (RenderedRunLog (..), renderRunLogTemplate)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.CodeGenerator.IFile as IFile
import qualified Morloc.CodeGenerator.Serial as Serial
import qualified Morloc.Config as MC
import Morloc.Data.Doc (concatWith, hardline, indent, line, pretty, render, squotes, vcat, (<+>))
import Morloc.Data.Json
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.Version
import qualified System.Directory as Dir

-- ======================================================================
-- Data types
-- ======================================================================

cLang :: ML.Lang
cLang = Lang "c" "c"

data FData = FData
  { fdataSocket :: Socket
  , fdataSubcommand :: Text
  , fdataMid :: Int
  , fdataType :: Type
  , fdataSubSockets :: [Socket]
  , fdataArgSchemas :: [Text]
  , fdataReturnSchema :: Text
  , fdataCmdDocSet :: CmdDocSet
  }

data GastData = GastData
  { commandName :: Text
  , commandMid :: Int
  , commandType :: Type
  , commandDocs :: CmdDocSet
  , commandExpr :: NexusExpr
  , commandReturnSchema :: Text
  , commandArgSchemas :: [Text]
  }

data NexusExpr
  = AppX Text NexusExpr [NexusExpr]
  | LamX [Text] NexusExpr
  | BndX Text Text
  | PatX Text Pattern
  | LstX Text [NexusExpr]
  | TupX Text [NexusExpr]
  | NamX Text [(Text, NexusExpr)]
  | StrX Text Text
  | LitX LitType Text
  | ShowX Text NexusExpr  -- schema (return type = Str), child expression
  | ReadX Text NexusExpr  -- schema (return type = ?a), child expression
  | HashX Text NexusExpr  -- schema + child -> Str (xxhash hex)
  | SaveX Text Text NexusExpr NexusExpr NexusExpr
  -- ^ format + schema + level + value + path -> (). For "voidstar" the
  -- level is a 0-9 zstd-preset expression evaluated at runtime; for
  -- "msgpack"/"json" the level is currently always a zero literal
  -- because those formats are not packets and don't carry compression.
  | LoadX Text NexusExpr  -- schema + path -> ?a
  | OptX Text NexusExpr   -- ?T schema wrapping a child of inner type T
                          -- (Just-coerce: at runtime sets tag=1 and writes
                          -- the child into the optional's inner slot).
  | OptNullX Text         -- absent ?T: at runtime sets tag=0 and leaves the
                          -- inner slot zero. Schema is the outer ?T schema so
                          -- the slot has the right width inside arrays/records.
  | MapX Text NexusExpr NexusExpr  -- schema (return type = List b), lambda, list.
                                   -- Emits the runtime Map intrinsic: per-element
                                   -- loop applying the lambda body to each input
                                   -- element. Only the desugar's bracket-accessor
                                   -- lowering generates this; there is no
                                   -- user-facing @map syntax.
  -- ── IFile/IStream/OStream handle intrinsics ─────────────────────────
  -- The schema in each variant is the result type's msgpack schema. For
  -- handle-returning intrinsics the schema is the IFile a type (which
  -- newtypes to UInt64 = "u8" at the wire level). For bracket/struct
  -- access the schema is the materialised result type.
  | OpenX     Text Word8 NexusExpr  -- schema, kind byte, path expr -> handle
  | CloseX    NexusExpr             -- handle expr -> () (no schema needed)
  | FSchemaX  Text NexusExpr        -- schema (Str), path expr -> schema string
  | FLengthX  Text NexusExpr        -- schema (Int), handle expr -> total element count
  | IFileWalkX Text NexusExpr Text [NexusExpr]
      -- ^ Unified IFile pattern walker: result schema, handle, path
      -- string (e.g. ".1.[]"), runtime args in DFS order. Each bracket
      -- step in the path consumes args (1 for `.[]`, 3 for `.[:]`).
  | NextX     Text NexusExpr        -- result schema ([a]), IStream handle expr
  | StreamX   Text NexusExpr        -- result schema (IStream a), IFile handle expr
  | OpenOStreamX Text NexusExpr     -- element schema, path expr (typed OStream open)
  | WriteX    Text NexusExpr NexusExpr NexusExpr -- value [a] schema, level, value, handle
  | AppendX   Text NexusExpr        -- element schema, path expr
  | ConcatX   NexusExpr NexusExpr   -- paths [Str], dest Str
  | FlushX    NexusExpr             -- handle expr -> () (force buffer flush)
  | StdinX    Text                  -- element schema (a) -- @stdin :: <IO> IStream a
  | StdoutX   Text                  -- element schema (a) -- @stdout :: <IO> OStream a
  | StderrX   Text                  -- element schema (a) -- @stderr :: <IO> OStream a

data LitType = F32X | F64X | I8X | I16X | I32X | I64X | U8X | U16X | U32X | U64X | BoolX | NullX | IntX

-- ======================================================================
-- Data extraction
-- ======================================================================

makeFData ::
  (AnnoS (Indexed Type) One (Indexed Lang), CmdDocSet) ->
  MorlocMonad (Type, Int, Lang, CmdDocSet, [Socket])
makeFData (e@(AnnoS (Idx i t) (Idx _ lang) _), d) = do
  sockets <- findSockets e
  return (t, i, lang, d, sockets)

findSockets :: AnnoS e One (Indexed Lang) -> MorlocMonad [Socket]
findSockets rAST = do
  config <- MM.ask
  registry <- MM.gets stateLangRegistry
  return . map (MC.setupServerAndSocket config registry) . unique $ findAllLangsSAnno rAST

findAllLangsSAnno :: AnnoS e One (Indexed Lang) -> [Lang]
findAllLangsSAnno (AnnoS _ (Idx _ lang) e) = lang : findAllLangsExpr e
  where
    findAllLangsExpr (VarS _ (One x)) = findAllLangsSAnno x
    findAllLangsExpr (AppS x xs) = concatMap findAllLangsSAnno (x : xs)
    findAllLangsExpr (LamS _ x) = findAllLangsSAnno x
    findAllLangsExpr (LstS xs) = concatMap findAllLangsSAnno xs
    findAllLangsExpr (TupS xs) = concatMap findAllLangsSAnno xs
    findAllLangsExpr (NamS rs) = concatMap (findAllLangsSAnno . snd) rs
    findAllLangsExpr (LetS _ e1 e2) = findAllLangsSAnno e1 ++ findAllLangsSAnno e2
    findAllLangsExpr (IfS c t e') = concatMap findAllLangsSAnno [c, t, e']
    findAllLangsExpr (DoBlockS x) = findAllLangsSAnno x
    findAllLangsExpr (EvalS x) = findAllLangsSAnno x
    findAllLangsExpr (CoerceS _ x) = findAllLangsSAnno x
    findAllLangsExpr _ = []

getFData :: (Type, Int, Lang, CmdDocSet, [Socket]) -> MorlocMonad FData
getFData (t, i, lang, doc, sockets) = do
  mayName <- MM.metaName i
  name' <- case mayName of
    Just n -> return n
    Nothing -> MM.throwSourcedError i "No name in FData"
  -- Same guard as annotateGasts, applied to sourced exports. Without
  -- it, higher-order arguments slip through and fall out of
  -- Serial.makeSerialAST as an unlocalized raw-TypeF dump.
  checkExportedHigherOrder i name' t
  (argAsts, returnAst) <- makeSerialASTs i lang t
  let argSchemas    = map (render . Serial.serialAstToMsgpackSchema) argAsts
      returnSchema  = render (Serial.serialAstToMsgpackSchema returnAst)
  -- Validate wire-shape constraints and default values now that the
  -- SerialASTs are in hand. The rendered schema texts are reused
  -- here so the validator never re-renders.
  validateArgSpecs i (cmdDocArgs doc) argAsts argSchemas
  config <- MM.ask
  registry <- MM.gets stateLangRegistry
  let socket = MC.setupServerAndSocket config registry lang
  return $
    FData
      { fdataSocket = socket
      , fdataSubcommand = maybe (unEVar name') id (cmdDocName doc)
      , fdataMid = i
      , fdataType = t
      , fdataSubSockets = sockets
      , fdataArgSchemas = argSchemas
      , fdataReturnSchema = returnSchema
      , fdataCmdDocSet = doc
      }

-- ======================================================================
-- Schema building
-- ======================================================================

-- | Build SerialASTs for a function's argument list (or a single
-- non-function type). The rendered msgpack schema text is derived from
-- the same AST at the call sites, so 'validateArgSpecs' (operating on
-- the AST) and the manifest emitter (operating on the rendered text)
-- never go out of sync.
makeSerialASTs :: Int -> Lang -> Type -> MorlocMonad ([SerialAST], SerialAST)
makeSerialASTs mid lang (FunT ts t) = do
  ss <- mapM (makeSerialAST mid lang) ts
  s <- makeSerialAST mid lang t
  return (ss, s)
makeSerialASTs mid lang t = do
  s <- makeSerialAST mid lang t
  return ([], s)

makeSerialAST :: Int -> Lang -> Type -> MorlocMonad SerialAST
makeSerialAST mid lang t = do
  ft <- Infer.inferConcreteTypeUniversal lang t
  ast <- Serial.makeSerialAST mid lang ft
  -- Apply nat dimension constraints from the original type to the SerialAST.
  -- The TypeF may have lost nat params during alias expansion, but the
  -- original Type still has them.
  return $ applyNatDimsFromType t ast

-- | Extract nat dimension constraints from a Type and apply them to a SerialAST.
-- For example, Matrix 2 3 Int has NatLitT args [2, 3]. After alias expansion,
-- the SerialAST is SerialList(SerialList(SerialInt)). This function annotates
-- each nesting level with the corresponding dimension constraint.
applyNatDimsFromType :: Type -> SerialAST -> SerialAST
applyNatDimsFromType (AppT _ args) ast =
  let dims = [Just (NatLitF n) | NatLitT n <- args, n > 0]
  in applyDims dims ast
  where
    applyDims (d:ds) (SerialList v _ inner) = SerialList v d (applyDims ds inner)
    applyDims _ s = s
applyNatDimsFromType _ ast = ast

-- | SerialASTs for a pure (gast) function's return + argument types.
-- The 'FunT' input puts the return type first, then arguments. The
-- source index `i` is the export's manifold id; threaded into
-- 'generalTypeToSerialAST' so any "cannot serialize" error carets at
-- the export rather than surfacing as an unlocalized SystemError.
makeGastSerialASTs :: Int -> Type -> MorlocMonad (SerialAST, [SerialAST])
makeGastSerialASTs i (FunT ts t) = do
  serialAsts <- zipWith applyNatDimsFromType (t : ts) <$> mapM (generalTypeToSerialAST i) (t : ts)
  case serialAsts of
    (s : ss) -> return (s, ss)
    [] -> error "makeGastSerialASTs: FunT produced empty serial AST list"
makeGastSerialASTs i t = do
  s <- applyNatDimsFromType t <$> generalTypeToSerialAST i t
  return (s, [])

-- | Build a SerialAST for a general (nexus-side) type. The Set
-- parameter tracks alias names currently being expanded so a
-- recursive alias (e.g. @type Pair a = (a, ?(Pair a))@) emits a
-- @SerialRec@ back-reference instead of recursing into itself
-- forever. Mirrors @stateSerialAncestors@ in @Serial.makeSerialAST'@
-- on the pool side -- the nexus needs the same protection because
-- top-level constant bindings whose RHS is a recursive-type literal
-- route through @annotateGasts@ here rather than through a pool.
generalTypeToSerialAST :: Int -> Type -> MorlocMonad SerialAST
generalTypeToSerialAST i = generalTypeToSerialAST' i Set.empty

generalTypeToSerialAST' :: Int -> Set TVar -> Type -> MorlocMonad SerialAST
generalTypeToSerialAST' i anc (VarT v)
  | v == MBT.real = return $ SerialReal (FV v (CV ""))
  -- Dispatch f32/f64 to the precision-specific SerialAST constructors,
  -- not SerialReal. checkRealBounds (and any future bound check) keys off
  -- this distinction to bound a literal against its target type's range
  -- (Float32 max ~3.4e38 vs Float64 max ~1.8e308). Collapsing both to
  -- SerialReal makes Float32 overflow checks silently fall through to
  -- the Float64 path.
  | v == MBT.f32 = return $ SerialFloat32 (FV v (CV ""))
  | v == MBT.f64 = return $ SerialFloat64 (FV v (CV ""))
  | v == MBT.int = return $ SerialInt (FV v (CV ""))
  | v == MBT.i8 = return $ SerialInt8 (FV v (CV ""))
  | v == MBT.i16 = return $ SerialInt16 (FV v (CV ""))
  | v == MBT.i32 = return $ SerialInt32 (FV v (CV ""))
  | v == MBT.i64 = return $ SerialInt64 (FV v (CV ""))
  | v == MBT.u8 = return $ SerialUInt8 (FV v (CV ""))
  | v == MBT.u16 = return $ SerialUInt16 (FV v (CV ""))
  | v == MBT.u32 = return $ SerialUInt32 (FV v (CV ""))
  | v == MBT.u64 = return $ SerialUInt64 (FV v (CV ""))
  | v == MBT.bool = return $ SerialBool (FV v (CV ""))
  | v == MBT.str = return $ SerialString (FV v (CV ""))
  | v == MBT.unit = return $ SerialNull (FV v (CV ""))
  | Set.member v anc = return $ SerialRec (FV v (CV ""))
  | otherwise = do
      scope <- MM.gets stateUniversalGeneralTypedefs
      case Map.lookup v scope of
        (Just [(_, _, _, True, _)]) -> error "Cannot handle terminal types"
        (Just [([], t', _, False, _)]) -> do
          -- Same retag-outer rule as in @resolveAliasApp@: the alias's
          -- body has a different outer constructor (e.g. @List@ for
          -- @type Pat = [Pat]@), but the @&Pat@/@^Pat@ pair must agree
          -- on the alias's own name. Retag the outer SerialAST node
          -- with @v@ after recursing.
          inner <- generalTypeToSerialAST' i (Set.insert v anc) (typeOf t')
          return $ retagOuterName v inner
        (Just [_]) -> MM.throwSourcedError i $
          "cannot serialize parameterised pure morloc type:" <+> pretty v
        Nothing -> MM.throwSourcedError i $
          "cannot serialize unknown type variable:" <+> pretty v
        x -> MM.throwSourcedError i $
          "cannot serialize type" <+> pretty v
            <+> "-- unexpected scope shape" <+> pretty (show x)
generalTypeToSerialAST' i anc (AppT (VarT v) [t])
  | v == MBT.list = SerialList (FV v (CV "")) Nothing <$> generalTypeToSerialAST' i anc t
  -- Stream-handle types share the 16-byte tagged-union wire form. The
  -- schema code (F/O/I) picks the receiver's open kind; the per-instance
  -- tag byte picks the encoding (path or intra-nexus handle).
  | v == MBT.ifileVar   = return $ SerialIFile   (FV v (CV ""))
  | v == MBT.ostreamVar = return $ SerialOStream (FV v (CV ""))
  | v == MBT.istreamVar = return $ SerialIStream (FV v (CV ""))
  | otherwise = resolveAliasApp i anc v [t]
generalTypeToSerialAST' i anc (AppT (VarT v) ts)
  | v == (MBT.tuple (length ts)) =
      SerialTuple (FV v (CV "")) <$> mapM (generalTypeToSerialAST' i anc) ts
  -- A Table lowers to a SerialObject NamTable. The encoder emits the
  -- @T@ wire token (or @T:K<entries>@ when the row is concrete);
  -- 'Serial.hasArrowHint' identifies these structurally for the
  -- Arrow-SHM dispatch. No concrete-type hint is attached -- the @T@
  -- marker itself is the dispatch signal.
  | v == MBT.table =
      let cols = case ts of
            [_, NamT _ _ _ rs] -> rs
            _                  -> []
      in SerialObject NamTable (FV MBT.table (CV "")) []
           <$> mapM (secondM (generalTypeToSerialAST' i anc)) cols
  | otherwise = resolveAliasApp i anc v ts
generalTypeToSerialAST' i anc (EffectT _ t) = generalTypeToSerialAST' i anc t
generalTypeToSerialAST' i anc (OptionalT t) = do
  inner <- generalTypeToSerialAST' i anc t
  return $ SerialOptional (FV (TV "Optional") (CV "")) inner
generalTypeToSerialAST' i anc (NamT o v _ rs) =
  -- Add @v@ to the ancestor set before recursing into the record's
  -- fields: a recursive record (e.g. @record Tree where children :: [Tree]@)
  -- has a field whose type mentions @Tree@ again, which would otherwise
  -- expand back into the same NamT and loop. Parameter types are
  -- already substituted into the field types @rs@ by this point, so
  -- they do not need to appear in the resulting SerialAST.
  let anc' = Set.insert v anc
  in SerialObject o (FV v (CV "")) []
       <$> mapM (secondM (generalTypeToSerialAST' i anc')) rs
generalTypeToSerialAST' i _ t = MM.throwSourcedError i $
  "cannot serialize type:" <+> pretty t

-- | Check whether a type contains a function type anywhere in its structure.
-- Used to detect higher-order functions appearing as arguments or in
-- compound positions (lists, tuples, records, optionals), which the CLI
-- nexus cannot serialize.
containsFunT :: Type -> Bool
containsFunT (FunT _ _) = True
containsFunT (AppT t ts) = containsFunT t || any containsFunT ts
containsFunT (NamT _ _ ts rs) = any containsFunT ts || any (containsFunT . snd) rs
containsFunT (EffectT _ t) = containsFunT t
containsFunT (OptionalT t) = containsFunT t
containsFunT _ = False

-- | Reject main-module exports whose type carries a function in argument or
-- return position. The nexus turns each such export into a CLI subcommand
-- and passes arguments as serialized data on the command line; closures
-- have no wire form. Library modules can still export higher-order
-- functions -- the restriction applies only to exports of the main module
-- being compiled. Names the offending position (argument N or return
-- type) and echoes the full signature so the user can locate the fix.
checkExportedHigherOrder :: Int -> EVar -> Type -> MorlocMonad ()
checkExportedHigherOrder i name t = case findOffender t of
  Nothing -> return ()
  Just (locDesc, offender) ->
    MM.throwSourcedError i $
      "Cannot generate a CLI subcommand for" <+> squotes (pretty name)
        <+> "--" <+> locDesc <> ":"
        <> line <> indent 2 (pretty offender)
        <> line
        <> line <> "Full signature:"
        <> line <> indent 2 (pretty name <+> "::" <+> pretty t)
        <> line
        <> line <> vcat
            [ "Exports from the top-level module become CLI subcommands in"
            , "the compiled nexus. Subcommand arguments and results are"
            , "passed as serialized data on the command line, so they"
            , "cannot contain functions -- there is no wire form for a"
            , "closure."
            ]
  where
    findOffender :: Type -> Maybe (MDoc, Type)
    findOffender (FunT ts ret) =
      case [(n, a) | (n, a) <- zip [1 :: Int ..] ts, containsFunT a] of
        ((n, a) : _) -> Just ("argument" <+> pretty n <+> "is a function", a)
        [] | containsFunT ret ->
             Just ("return type contains a function", ret)
           | otherwise -> Nothing
    findOffender ty
      | containsFunT ty = Just ("exported value is or contains a function", ty)
      | otherwise = Nothing

resolveAliasApp :: Int -> Set TVar -> TVar -> [Type] -> MorlocMonad SerialAST
resolveAliasApp i anc v ts
  -- The same self-recursion cutoff as in the @VarT@ clause above:
  -- if we encounter an alias we are already expanding, emit a
  -- @SerialRec@ back-reference instead of recursing into its body.
  -- The runtime parser resolves @^name@ against the enclosing
  -- @&name@ that the encoder emits at the outer occurrence.
  | Set.member v anc = return $ SerialRec (FV v (CV ""))
  | otherwise = do
      scope <- MM.gets stateUniversalGeneralTypedefs
      case Map.lookup v scope of
        (Just [(params, body, _, False, _)]) -> do
          let tvars = [tv | Left (tv, _) <- params]
              resolved = foldl (\acc (tv, arg) -> substituteTVar tv arg acc) (typeOf body) (zip tvars ts)
              anc' = Set.insert v anc
          inner <- generalTypeToSerialAST' i anc' resolved
          -- The expanded body's outer constructor is the alias's
          -- underlying shape (e.g. @Tuple2@ for @type Pair a = (a, ?(Pair a))@).
          -- We must retag the outer SerialAST node with the alias's own
          -- name so the @&name@ declaration emitted by the schema encoder
          -- matches the @^name@ back-references inside (which carry the
          -- alias's name from the @SerialRec@ created by the cutoff
          -- above). Without this retag, an outer @SerialTuple Tuple2@
          -- produces no @&Pair@ on the wire and the @^Pair@ inside
          -- becomes a dangling back-reference.
          return $ retagOuterName v inner
        _ -> MM.throwSourcedError i $
          "cannot serialize type" <+> pretty (AppT (VarT v) ts)
            <+> "-- no per-language alias resolution for" <+> pretty v
            <> ". If" <+> pretty v <+> "is a newtype handle, add"
            <+> "`newtype" <+> pretty v <+> "<params> = <wire-type>` in stdlib/internal."

-- | Replace the outermost FVar's general name with the alias's name.
-- Used after expanding an alias body to keep the alias's identity on
-- the SerialAST's outer node so the @&name@ wire declaration matches
-- the @^name@ back-references produced by @SerialRec@. Preserves the
-- concrete-name slot (empty for nexus-side SerialASTs) and the node's
-- structural shape; primitives and @SerialRec@ pass through unchanged.
retagOuterName :: TVar -> SerialAST -> SerialAST
retagOuterName v' s = case s of
  SerialList     (FV _ cv) d inner -> SerialList     (FV v' cv) d inner
  SerialTuple    (FV _ cv) xs      -> SerialTuple    (FV v' cv) xs
  SerialObject o (FV _ cv) ps rs   -> SerialObject o (FV v' cv) ps rs
  SerialOptional (FV _ cv) inner   -> SerialOptional (FV v' cv) inner
  _ -> s

-- ======================================================================
-- Pure expression extraction
-- ======================================================================

annotateGasts :: (AnnoS (Indexed Type) One (), CmdDocSet) -> MorlocMonad GastData
annotateGasts (x0@(AnnoS (Idx i gtype) _ _), docs) = do
  mayName <- MM.metaName i
  gname <- case mayName of
    Nothing -> MM.throwSourcedError i $ "No name found for call-free function"
    (Just n') -> return n'

  checkExportedHigherOrder i gname gtype

  (retAst, argAsts) <- makeGastSerialASTs i gtype
  let returnSchema = render (Serial.serialAstToMsgpackSchema retAst)
      argSchemas   = map (render . Serial.serialAstToMsgpackSchema) argAsts
  validateArgSpecs i (cmdDocArgs docs) argAsts argSchemas
  expr <- toNexusExpr x0

  return $
    GastData
      { commandName = maybe (unEVar gname) id (cmdDocName docs)
      , commandMid = i
      , commandType = gtype
      , commandDocs = docs
      , commandExpr = expr
      , commandReturnSchema = returnSchema
      , commandArgSchemas = argSchemas
      }
  where
    type2schema :: Type -> MorlocMonad Text
    type2schema t = (render . Serial.serialAstToMsgpackSchema) <$> generalTypeToSerialAST i t

    -- Construct an IFileWalkX node for a unified IFile pattern walk.
    -- Used by every IFile-routing case in toNexusExpr (BracketSlice,
    -- BracketIndex, PatternStruct) so the path-encoding logic is in one
    -- place.
    emitIFileWalkX
      :: Type
      -> AnnoS (Indexed Type) One ()
      -> [IFile.WalkStep]
      -> [AnnoS (Indexed Type) One ()]
      -> MorlocMonad NexusExpr
    emitIFileWalkX resultT handleE steps runtimeArgs =
      IFileWalkX
        <$> type2schema resultT
        <*> toNexusExpr handleE
        <*> pure (IFile.walkStepsToPath steps)
        <*> mapM toNexusExpr runtimeArgs

    toNexusExpr :: AnnoS (Indexed Type) One () -> MorlocMonad NexusExpr
    -- Pure-path bracket validation: each non-Null bound must resolve
    -- to a wire integer type. There is no per-language IndexLike
    -- instance to fall back on in the nexus runtime, so anything
    -- non-integral is rejected at codegen with a clear message.
    -- @(Null :: ?Int64)@ (from the desugar's empty-bound annotation),
    -- bare integer literals, and explicit @(_ :: Int*)@ annotations
    -- all pass.
    -- IFile pattern detection at the nexus path: any PatCall on an
    -- IFile-typed receiver routes to the unified IFile walker rather
    -- than the generic Array/Tuple dispatch. The receiver is a u64
    -- handle, not an in-memory structure; the standard apply_*
    -- functions would cast garbage. Mirrors Express.hs's pool-path
    -- IFile detection.
    toNexusExpr (AnnoS (Idx _ t) _ (AppS funcE@(AnnoS _ _ (ExeS (PatCall PatternBracketSlice))) [sE, eE, pE, rE])) = do
      validateBracketBound "start" sE
      validateBracketBound "stop"  eE
      validateBracketBound "step"  pE
      if recvIsIFile rE
        then emitIFileWalkX t rE IFile.bracketSliceSteps [sE, eE, pE]
        else AppX <$> type2schema t <*> toNexusExpr funcE <*> mapM toNexusExpr [sE, eE, pE, rE]
    toNexusExpr (AnnoS (Idx _ t) _ (AppS funcE@(AnnoS _ _ (ExeS (PatCall PatternBracketIndex))) [iE, rE])) = do
      validateBracketBound "index" iE
      if recvIsIFile rE
        then emitIFileWalkX t rE IFile.bracketIndexSteps [iE]
        else AppX <$> type2schema t <*> toNexusExpr funcE <*> mapM toNexusExpr [iE, rE]
    -- PatternStruct app with brackets in the Selector. The first
    -- bracketArity(sel) args are the bracket bounds (DFS order) and
    -- the LAST arg is the receiver. IFile receivers route to the
    -- walker; non-IFile receivers with bracket-in-Selector are
    -- currently unsupported by the runtime (codegen for that case
    -- isn't wired yet).
    toNexusExpr (AnnoS (Idx midx t) _ (AppS funcE@(AnnoS _ _ (ExeS (PatCall (PatternStruct sel)))) appArgs))
      | bracketArity sel > 0 =
          let n        = bracketArity sel
              rE       = last appArgs
              brackets = take n appArgs
          in if length appArgs /= n + 1
               then MM.throwSourcedError midx
                      ("PatternStruct app with brackets: expected "
                       <> pretty (n + 1)
                       <> " args (bracket bounds + receiver), got "
                       <> pretty (length appArgs))
               else do
                 mapM_ (validateBracketBound "bracket") brackets
                 if recvIsIFile rE
                   then do
                     steps <- IFile.selectorToWalkSteps (annoIdx rE) sel
                     emitIFileWalkX t rE steps brackets
                   else
                     AppX <$> type2schema t
                          <*> toNexusExpr funcE
                          <*> mapM toNexusExpr appArgs
    -- Pure-field PatternStruct getter on an IFile receiver (no
    -- brackets, single receiver arg). Routes to the walker.
    toNexusExpr (AnnoS (Idx _ t) _ (AppS funcE@(AnnoS _ _ (ExeS (PatCall (PatternStruct sel)))) [rE]))
      | recvIsIFile rE = do
          steps <- IFile.selectorToWalkSteps (annoIdx rE) sel
          emitIFileWalkX t rE steps []
      | otherwise =
          AppX <$> type2schema t <*> toNexusExpr funcE <*> mapM toNexusExpr [rE]
    toNexusExpr (AnnoS (Idx _ t) _ (AppS e es)) = AppX <$> type2schema t <*> toNexusExpr e <*> mapM toNexusExpr es
    toNexusExpr (AnnoS _ _ (LamS vs e)) = LamX (map (render . pretty) vs) <$> toNexusExpr e
    toNexusExpr (AnnoS (Idx _ (FunT _ t)) _ (ExeS (PatCall p))) = PatX <$> type2schema t <*> pure p
    toNexusExpr (AnnoS (Idx _ t) _ (BndS v)) = BndX <$> type2schema t <*> pure (render (pretty v))
    toNexusExpr (AnnoS (Idx _ t) _ (LstS es)) = LstX <$> type2schema t <*> mapM toNexusExpr es
    -- TupS construction. Multi-field patterns are no longer fragmented
    -- at desugar time (they emit a unified PatCall (PatternStruct ...)
    -- instead), so any TupS reaching here is a user-written tuple
    -- literal or the legacy fall-back form for mixed-kind groups that
    -- can't be unified. Standard per-element rendering applies.
    toNexusExpr (AnnoS (Idx _ t) _ (TupS es)) =
      TupX <$> type2schema t <*> mapM toNexusExpr es
    toNexusExpr (AnnoS (Idx _ t) _ (NamS rs)) =
      NamX <$> type2schema t <*> mapM (\(k, e) -> (,) (unKey k) <$> toNexusExpr e) rs
    toNexusExpr (AnnoS (Idx _ t) _ (StrS v)) = StrX <$> type2schema t <*> pure v
    -- Use the literal's own source-map index (litIx) for overflow reporting,
    -- mirroring the IntS path. RealS carries litIx in its first parameter.
    -- Non-finite variants (Inf/-Inf/NaN) bypass bounds-checking and emit
    -- the canonical wire string ("inf" / "-inf" / "nan"); the runtime JSON
    -- decoder accepts these on parse.
    toNexusExpr (AnnoS (Idx _ t) _ (RealS litIx v)) = do
      s <- generalTypeToSerialAST litIx t
      checkRealBounds litIx v s
      let litCode = case s of
            (SerialFloat32 _) -> F32X
            _ -> F64X
      return $ LitX litCode (MT.pack (showRealLit v))
    -- Use the literal's own source-map index (litIx) for overflow reporting.
    -- The wrapping AnnoS index points at the term that owns the literal,
    -- which after term inlining is often the export reference, not the
    -- literal itself. See Treeify.collectExprS for where litIx is set.
    toNexusExpr (AnnoS (Idx _ t) _ (IntS litIx v)) = do
      s <- generalTypeToSerialAST litIx t
      checkIntBounds litIx v s
      return $ case s of
        (SerialInt8 _) -> LitX I8X (MT.pack (show v))
        (SerialInt16 _) -> LitX I16X (MT.pack (show v))
        (SerialInt _) -> LitX IntX (MT.pack (show v))
        (SerialInt32 _) -> LitX I32X (MT.pack (show v))
        (SerialInt64 _) -> LitX I64X (MT.pack (show v))
        (SerialUInt8 _) -> LitX U8X (MT.pack (show v))
        (SerialUInt16 _) -> LitX U16X (MT.pack (show v))
        (SerialUInt _) -> LitX U64X (MT.pack (show v))
        (SerialUInt32 _) -> LitX U32X (MT.pack (show v))
        (SerialUInt64 _) -> LitX U64X (MT.pack (show v))
        _ -> LitX I64X (MT.pack (show v))
    toNexusExpr (AnnoS _ _ (LogS True)) = return $ LitX BoolX "1"
    toNexusExpr (AnnoS _ _ (LogS False)) = return $ LitX BoolX "0"
    toNexusExpr (AnnoS _ _ UniS) = return $ LitX NullX "0"
    -- A bare NullS lowers to a width-1 'z' literal. When the surrounding type
    -- is a (possibly Effect-wrapped) Optional, that 1-byte schema does not fit
    -- the optional's wider tag+inner slot, which corrupts list/record/tuple
    -- evaluation in the runtime. Emit OptNullX with the outer ?T schema so
    -- the slot has the correct width.
    toNexusExpr (AnnoS (Idx _ t) _ NullS) = case stripEffect t of
      OptionalT _ -> OptNullX <$> type2schema t
      _           -> return $ LitX NullX "0"
    toNexusExpr (AnnoS (Idx _ t) _ (LetBndS v)) = BndX <$> type2schema t <*> pure (render (pretty v))
    -- Desugar let to lambda application: let x = e1 in e2 -> (\x -> e2) e1
    toNexusExpr (AnnoS (Idx _ t) _ (LetS v e1 body)) = do
      schema <- type2schema t
      bodyX <- toNexusExpr body
      e1X <- toNexusExpr e1
      return $ AppX schema (LamX [render (pretty v)] bodyX) [e1X]
    toNexusExpr (AnnoS _ _ (IfS _ t _)) = toNexusExpr t
    toNexusExpr (AnnoS _ _ (DoBlockS e)) = toNexusExpr e
    toNexusExpr (AnnoS _ _ (EvalS e)) = toNexusExpr e
    -- CoerceToOptional changes the value's runtime layout: the voidstar
    -- slot becomes a relptr (RELNULL = absent, otherwise points to T's
    -- body). The eval path allocates T at the cursor and stores the
    -- relptr; here at the pure-nexus level we just materialize the
    -- wrapping explicitly so the downstream evaluator sees a real
    -- Optional node to populate.
    toNexusExpr (AnnoS (Idx _ t) _ (CoerceS CoerceToOptional e)) = do
      outerSchema <- type2schema t
      childX <- toNexusExpr e
      return $ OptX outerSchema childX
    -- IntrMap: the desugar-emitted implicit map for bracket-accessor
    -- chains. The runtime evaluator's Map intrinsic handles this as a
    -- per-element loop over the input array, applying the lambda body
    -- without any pool dispatch.
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrMap [funcE, listE])) =
      MapX <$> type2schema t <*> toNexusExpr funcE <*> toNexusExpr listE
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrShow [arg])) =
      ShowX <$> type2schema t <*> toNexusExpr arg
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrRead [arg])) =
      ReadX <$> type2schema t <*> toNexusExpr arg
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrHash [arg])) =
      HashX <$> type2schema t <*> toNexusExpr arg
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrSave [levelExpr, valExpr, path])) =
      SaveX "voidstar"
        <$> type2schema t
        <*> toNexusExpr levelExpr
        <*> toNexusExpr valExpr
        <*> toNexusExpr path
    -- @savem/@savej carry no compression level; emit a zero literal so the
    -- nexus dispatch (which always reads a level field from save_expr) has
    -- a uniform shape. The runtime ignores the field for non-voidstar formats.
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrSaveM [valExpr, path])) =
      SaveX "msgpack"
        <$> type2schema t
        <*> pure (LitX IntX "0")
        <*> toNexusExpr valExpr
        <*> toNexusExpr path
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrSaveJ [valExpr, path])) =
      SaveX "json"
        <$> type2schema t
        <*> pure (LitX IntX "0")
        <*> toNexusExpr valExpr
        <*> toNexusExpr path
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrLoad [path])) =
      LoadX <$> type2schema t <*> toNexusExpr path
    -- @open: dispatch by result-type head. IFile/IStream go to OpenX
    -- (generic mlc_open(path, kind) entry); OStream goes to OpenOStreamX
    -- (typed mlc_open_ostream(schema_str, path) entry) since the writer
    -- needs the element schema at open time.
    toNexusExpr (AnnoS (Idx iOpen t) _ (IntrinsicS IntrOpen [path])) = do
      let peelEffect (EffectT _ inner) = peelEffect inner
          peelEffect ot = ot
          unwrapped = peelEffect t
          peelHead (AppT h _) = peelHead h
          peelHead ot = ot
          head_ = peelHead unwrapped
          elemT = case unwrapped of
            AppT _ (a : _) -> a
            _ -> unwrapped
      case head_ of
        VarT v
          | v == MBT.ifileVar   ->
              OpenX <$> type2schema t <*> pure MBT.mlcKindIFile <*> toNexusExpr path
          | v == MBT.istreamVar ->
              OpenX <$> type2schema t <*> pure MBT.mlcKindIStream <*> toNexusExpr path
          | v == MBT.ostreamVar ->
              OpenOStreamX <$> type2schema elemT <*> toNexusExpr path
          | otherwise ->
              MM.throwSourcedError iOpen $
                "@open: result type must be IFile/IStream/OStream, got " <> pretty v
        _ ->
          MM.throwSourcedError iOpen $
            "@open: unsupported handle type" <+> pretty (show t)
    toNexusExpr (AnnoS _ _ (IntrinsicS IntrClose [handle])) =
      CloseX <$> toNexusExpr handle
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrFSchema [path])) =
      FSchemaX <$> type2schema t <*> toNexusExpr path
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrFLength [handle])) =
      FLengthX <$> type2schema t <*> toNexusExpr handle
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrNext [handle])) =
      NextX <$> type2schema t <*> toNexusExpr handle
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrStream [handle])) =
      StreamX <$> type2schema t <*> toNexusExpr handle
    toNexusExpr (AnnoS (Idx _ _) _ (IntrinsicS IntrWrite [levelE, handleE, valE@(AnnoS (Idx _ valT) _ _)])) =
      WriteX
        <$> type2schema valT
        <*> toNexusExpr levelE
        <*> toNexusExpr valE
        <*> toNexusExpr handleE
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrAppend [pathE])) =
      AppendX <$> type2schema (peelHandleElemT t) <*> toNexusExpr pathE
    toNexusExpr (AnnoS _ _ (IntrinsicS IntrConcat [pathsE, destE])) =
      ConcatX <$> toNexusExpr pathsE <*> toNexusExpr destE
    toNexusExpr (AnnoS _ _ (IntrinsicS IntrFlush [handle])) =
      FlushX <$> toNexusExpr handle
    -- @stdin / @stdout / @stderr: nullary. Result type is
    -- <IO> IStream a / <IO> OStream a; peel the effect and the head
    -- to get the element type, then serialise its schema.
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrStdin _)) =
      StdinX <$> type2schema (peelHandleElemT t)
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrStdout _)) =
      StdoutX <$> type2schema (peelHandleElemT t)
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrStderr _)) =
      StderrX <$> type2schema (peelHandleElemT t)
    -- @ifile_walk: synthesised by Express.hs on pool-bound manifolds.
    -- Args: [pathExpr (Str lit), handle, runtime args...]. The nexus
    -- path normally constructs IFileWalkX directly from PatCall +
    -- IFile-receiver detection (see toNexusExpr cases above); this
    -- handler exists to support any future site that synthesises the
    -- IntrinsicS form directly.
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrIFileWalk (pathExpr : handle : runtimeArgs))) = do
      pathText <- case pathExpr of
        AnnoS _ _ (StrS s) -> return s
        _ -> error "IntrIFileWalk's first arg must be a string literal (compiler bug)"
      IFileWalkX
        <$> type2schema t
        <*> toNexusExpr handle
        <*> pure pathText
        <*> mapM toNexusExpr runtimeArgs
    -- @lang in a language-independent context is always "morloc"; the
    -- nexus has no pool dispatch here.
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrLang _)) =
      StrX <$> type2schema t <*> pure "morloc"
    -- @schema arg: emit the hint-free msgpack schema of the argument's
    -- general type. type2schema already produces the same string the
    -- @save* / @load paths use, so the value is well-defined here.
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrSchema [AnnoS (Idx _ argT) _ _])) = do
      s <- type2schema argT
      StrX <$> type2schema t <*> pure s
    -- @typeof arg: emit the user-facing general type name from the
    -- argument's type annotation, matching what the user wrote.
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrTypeof [AnnoS (Idx _ argT) _ _])) =
      StrX <$> type2schema t <*> pure (render (pretty argT))
    -- @datafile path: resolve a literal path string against the install
    -- directory. Mirrors Reduce.hs:68-78 for the pool side; a non-literal
    -- path arg yields the same fallback sentinel as the pool path.
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrDatafile [pathExpr])) = do
      resolved <- resolveDatafilePath pathExpr
      StrX <$> type2schema t <*> pure resolved
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS intr _)) = do
      v <- resolveCompileTimeIntrinsic intr
      StrX <$> type2schema t <*> pure v
    toNexusExpr (AnnoS (Idx _ t) _ (CallS v)) = BndX <$> type2schema t <*> pure (render (pretty v))
    toNexusExpr _ = error $ "Unreachable value of type reached"

checkIntBounds :: Int -> Integer -> SerialAST -> MorlocMonad ()
checkIntBounds i v s =
  let (name, lo, hi) = case s of
        SerialInt8 _  -> ("Int8", -128, 127)
        SerialInt16 _ -> ("Int16", -32768, 32767)
        SerialInt32 _ -> ("Int32", -2147483648, 2147483647)
        SerialInt _   -> ("", v, v) -- variable-width, no overflow possible
        SerialInt64 _ -> ("Int64", -9223372036854775808, 9223372036854775807)
        SerialUInt8 _  -> ("UInt8", 0, 255)
        SerialUInt16 _ -> ("UInt16", 0, 65535)
        SerialUInt32 _ -> ("UInt32", 0, 4294967295)
        SerialUInt _   -> ("UInt", 0, 18446744073709551615)
        SerialUInt64 _ -> ("UInt64", 0, 18446744073709551615)
        _ -> ("", v, v) -- no check for non-integer types
  in CM.when (v < lo || v > hi) $
       MM.throwSourcedError i $
         "Integer literal " <> pretty v
         <> " overflows " <> (name :: MDoc)
         <> " (range " <> pretty lo <> " to " <> pretty hi <> ")"

-- Check that a finite Real literal fits its target IEEE-754 representation.
-- Data.Scientific.toRealFloat collapses out-of-range values to infinity, so
-- isInfinite on the result detects overflow at compile time. The non-finite
-- RealLit variants (Inf/-Inf/NaN) are explicitly written by the user and
-- bypass the bounds check entirely.
checkRealBounds :: Int -> RealLit -> SerialAST -> MorlocMonad ()
checkRealBounds i (RealFinite v) s = case s of
  SerialFloat32 _ ->
    let f = DS.toRealFloat v :: Float
     in CM.when (isInfinite f) $
          MM.throwSourcedError i $
            "Float literal " <> pretty (show v)
            <> " overflows Float32 (|x| > 3.4e38)"
  -- Real and Float64 share the same IEEE-754 binary64 representation; bound
  -- both against Float64 max.
  SerialFloat64 _ -> overflowsF64
  SerialReal _    -> overflowsF64
  _ -> return ()
  where
    overflowsF64 =
      let d = DS.toRealFloat v :: Double
       in CM.when (isInfinite d) $
            MM.throwSourcedError i $
              "Float literal " <> pretty (show v)
              <> " overflows Float64 (|x| > 1.8e308)"
checkRealBounds _ _ _ = return ()

-- | Strip a leading `<IO>` and one `AppT` head to reach the element
-- type carried by an `IStream a` / `OStream a` / `IFile a` result.
peelHandleElemT :: Type -> Type
peelHandleElemT (EffectT _ inner) = peelHandleElemT inner
peelHandleElemT (AppT _ (a : _)) = a
peelHandleElemT other = other

resolveCompileTimeIntrinsic :: Intrinsic -> MorlocMonad Text
resolveCompileTimeIntrinsic IntrVersion = return $ MT.pack Morloc.Version.versionStr
resolveCompileTimeIntrinsic IntrCompiled = do
  now <- liftIO Data.Time.Clock.getCurrentTime
  return . MT.pack $ Data.Time.Format.formatTime Data.Time.Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
-- Any intrinsic reaching this point lacks a dedicated nexus case above.
-- That is a compiler bug, not a user error: every Intrinsic constructor
-- should be resolved either inline in toNexusExpr or here.
resolveCompileTimeIntrinsic intr =
  MM.throwSystemError $ "Compiler bug: @" <> pretty (intrinsicName intr) <> " has no nexus-side handler"

-- Resolve a @datafile path expression in the nexus. Mirrors
-- Reduce.hs::reduceNativeExpr IntrDatafile: a literal Str argument is
-- joined with stateInstallDir; anything else gets the same sentinel
-- string the pool side emits.
resolveDatafilePath :: AnnoS (Indexed Type) One () -> MorlocMonad Text
resolveDatafilePath (AnnoS _ _ (StrS rel)) = do
  mInstallDir <- MM.gets stateInstallDir
  return $ case mInstallDir of
    Just dir -> MT.pack (dir </> MT.unpack rel)
    Nothing -> rel
resolveDatafilePath _ = return "<datafile: could not resolve path>"

-- ======================================================================
-- CLI argument serialization
-- ======================================================================

-- | Encode a single source atom as a JSON string.
sourceAtomJson :: SourceAtom -> Text
sourceAtomJson SourceInline = jsonStr "inline"
sourceAtomJson SourceFile   = jsonStr "file"

-- | Encode a single form atom as a JSON string.
formAtomJson :: FormAtom -> Text
formAtomJson FormPacket    = jsonStr "packet"
formAtomJson FormBytes     = jsonStr "bytes"
formAtomJson FormBytesOnly = jsonStr "bytes-only"
formAtomJson FormList      = jsonStr "list"

-- | Encode a single check as `{"kind": ..., "value": ...}`. Each future
-- check kind adds a constructor and a matching JSON shape; the runtime
-- decoder dispatches on the `kind` field.
checkJson :: Check -> Text
checkJson (CheckPath (PathPerm p)) =
  jsonObj [("kind", jsonStr "path"), ("value", jsonStr p)]

-- | Resolve the outer source field to emit. `auto` is the internal
-- sentinel meaning "use the runtime classifier"; it is never written
-- by users (the parser rejects `source: auto`) but the manifest uses
-- it as the default for list / compound types where the classifier
-- handles inline-or-file dispatch. `literal: true` is the deprecated
-- alias for `source: inline`.
resolvedSourceJson :: Maybe Bool -> Maybe SourceAtom -> Maybe Text -> Text
resolvedSourceJson _ (Just a) _ = sourceAtomJson a
resolvedSourceJson mLit Nothing mschema
  | mLit == Just True = sourceAtomJson SourceInline
  | otherwise = case fmap classifySchema mschema of
      Just CatScalarPrim    -> sourceAtomJson SourceInline
      Just CatStr           -> sourceAtomJson SourceInline
      _                     -> jsonStr "auto"

-- | Emit the outer form atom, or `"auto"` when none is set. `auto` is
-- the runtime sentinel for "use the classifier"; it is never written
-- by users.
resolvedFormJson :: Maybe FormAtom -> Text
resolvedFormJson Nothing  = jsonStr "auto"
resolvedFormJson (Just a) = formAtomJson a

-- | Emit the per-element source atom or `"inline"` (the default).
resolvedListSourceJson :: Maybe SourceAtom -> Text
resolvedListSourceJson Nothing  = sourceAtomJson SourceInline
resolvedListSourceJson (Just a) = sourceAtomJson a

-- | Emit the per-element form atom or `"auto"` (the classifier).
resolvedListFormJson :: Maybe FormAtom -> Text
resolvedListFormJson Nothing  = jsonStr "auto"
resolvedListFormJson (Just a) = formAtomJson a

-- | Emit a list of checks as a JSON array.
checksJson :: [Check] -> Text
checksJson cs = jsonArr (map checkJson cs)

-- | The CLI-shape fields shared by positional and option args. The
-- wire schema (when known) drives the default source: scalar
-- primitives and Str default to `"inline"`, everything else defaults
-- to the classifier sentinel `"auto"`.
ioShapeFields :: Maybe Text
              -> Bool
              -> Maybe Bool
              -> Maybe SourceAtom
              -> Maybe FormAtom
              -> [Check]
              -> Maybe SourceAtom
              -> Maybe FormAtom
              -> [Check]
              -> [(Text, Text)]
ioShapeFields mschema many mLit mSrc mForm cks mLSrc mLForm lcks =
  [ ("source",      resolvedSourceJson mLit mSrc mschema)
  , ("form",        resolvedFormJson mForm)
  , ("checks",      checksJson cks)
  , ("list_source", resolvedListSourceJson mLSrc)
  , ("list_form",   resolvedListFormJson mLForm)
  , ("list_checks", checksJson lcks)
  , ("format",      jsonMaybeStr
                      (renderFormatHint mschema many mSrc mForm cks
                         mLSrc mLForm lcks))
  ]

-- | One-line user-facing "Format:" hint describing how to type input
-- for an argument that uses a non-default `source:` / `form:` /
-- `check.*:` / `list.*:` configuration. Returns 'Nothing' when the
-- default contract applies (the type line is then enough).
renderFormatHint :: Maybe Text -> Bool
                 -> Maybe SourceAtom -> Maybe FormAtom -> [Check]
                 -> Maybe SourceAtom -> Maybe FormAtom -> [Check]
                 -> Maybe Text
renderFormatHint mschema many mSrc mForm cks mLSrc mLForm lcks
  | many = Just "one or more argv tokens, each parsed as the element type"
  | otherwise = case mschema of
      Nothing -> Nothing
      Just s -> case classifySchema s of
        CatScalarPrim    -> Nothing
        CatStr           -> strFormatHint mSrc cks
        CatList elemS    -> listFormatHint elemS mSrc mForm cks mLSrc mLForm lcks
        CatOtherCompound -> Nothing

-- | Format hint for `Str` arguments.
strFormatHint :: Maybe SourceAtom -> [Check] -> Maybe Text
strFormatHint mSrc cks
  | mSrc == Just SourceFile =
      Just "read the value from the file at this path"
  | otherwise = case cks of
      (CheckPath (PathPerm p) : _) -> Just (pathCheckHint p)
      _                            -> Nothing

-- | Map a path-permission mode to a user-facing phrase. The four
-- accepted modes are r / w / x / rw; see 'parseCheck' for the
-- pre-open predicates each mode names.
pathCheckHint :: Text -> Text
pathCheckHint p = case MT.unpack p of
  "r"  -> "path to a readable file"
  "w"  -> "path to a writable file (created if it does not exist)"
  "x"  -> "path to a file that does not yet exist (parent must be writable)"
  "rw" -> "path to a readable and writable existing file"
  _    -> "must satisfy path mode `" <> p <> "`"

-- | Format hint for a list argument. Considers the outer
-- `form:` / `source:` / `check.*:` first, then per-element overrides.
listFormatHint :: Text -> Maybe SourceAtom -> Maybe FormAtom -> [Check]
               -> Maybe SourceAtom -> Maybe FormAtom -> [Check]
               -> Maybe Text
listFormatHint elemSchema mSrc mForm _cks mLSrc mLForm lcks =
  case mForm of
    Just FormList ->
      Just (formListHint elemSchema mLSrc mLForm lcks)
    Just FormBytes ->
      Just (bytesHint elemSchema mSrc Hybrid)
    Just FormBytesOnly ->
      Just (bytesHint elemSchema mSrc Raw)
    Just FormPacket ->
      Just (bytesHint elemSchema mSrc Packet)
    _ -> Nothing

-- | Internal enum for the three bytes-family display variants.
data BytesVariant = Hybrid | Raw | Packet

-- | Format hint for `form: bytes` / `bytes-only` / `packet`.
-- The inline-bytes shortcut (`source: inline + form: bytes/bytes-only`
-- on `[UInt8]`) is its own case.
bytesHint :: Text -> Maybe SourceAtom -> BytesVariant -> Text
bytesHint elemSchema mSrc variant
  | mSrc == Just SourceInline && elemSchema == "u1" =
      "literal byte sequence with escapes allowed"
  | otherwise = case variant of
      Hybrid -> "path to readable file (morloc packet or " <> packedDescr elemSchema <> ")"
      Raw    -> "path to readable " <> packedDescr elemSchema
      Packet -> "path to morloc packet"


-- | A short phrase describing the binary layout for a fixed-width
-- scalar element schema.
packedDescr :: Text -> Text
packedDescr "u1" = "raw binary"
packedDescr "i1" = "packed int8"
packedDescr "u2" = "packed uint16"
packedDescr "i2" = "packed int16"
packedDescr "u4" = "packed uint32"
packedDescr "i4" = "packed int32"
packedDescr "u8" = "packed uint64"
packedDescr "i8" = "packed int64"
packedDescr "f4" = "packed f32"
packedDescr "f8" = "packed f64"
packedDescr "b"  = "packet booleans (0/1)"
packedDescr _    = "packed bytes"

-- | Format hint for `form: list`: file with newline-delimited
-- elements, optionally with per-element overrides.
formListHint :: Text -> Maybe SourceAtom -> Maybe FormAtom -> [Check] -> Text
formListHint elemSchema mLSrc mLForm lcks =
  let base = baseLine elemSchema mLSrc mLForm
      checkSuffix = case lcks of
        (CheckPath (PathPerm p) : _) -> "; " <> pathCheckHint p
        _                            -> ""
  in base <> checkSuffix
  where
    baseLine es ms mf = case mf of
      Just FormPacket ->
        "path to text file; each line a morloc packet filename"
      Just FormBytes ->
        "path to text file; each line a " <> "morloc packet or " <> packedDescr (innerOrSelf es)
      Just FormBytesOnly ->
        "path to a text file; each line a path to " <> packedDescr (innerOrSelf es)
      _ -> case ms of
        Just SourceFile ->
          "path to a text file; each line is one value"
        _ -> defaultPerLine es

    defaultPerLine es = case classifySchema es of
      CatStr           -> "path to text file with one string per line"
      CatScalarPrim    -> "path to text file with one value per line"
      CatList _        -> "path to text file with one JSON array per line"
      CatOtherCompound -> "path to file with one row entry line (JSON or table)"

    -- For `[[UInt8]]` etc., the bytes-family description uses the
    -- innermost element width.
    innerOrSelf es = case classifySchema es of
      CatList inner -> inner
      _             -> es

-- | Serialize a 'CmdArg' to its JSON manifest form. The optional
-- 'Maybe Text' is the pre-rendered serialization schema for typed
-- args (pos/opt/grp); flags pass 'Nothing' since they have no schema.
-- Group entries also pass 'Nothing' because the group's top-level
-- schema covers the whole record; entries never dispatch individually.
argToJson :: Maybe Text -> CmdArg -> Text
argToJson mschema (CmdArgPos r) =
  jsonObj $
    [ ("kind", jsonStr "pos") ]
    ++ schemaField mschema
    ++ [ ("type", jsonStr (typeDescStr (argPosDocType r) (argPosDocLiteral r) (argPosDocSource r) (argPosDocChecks r)))
       , ("metavar", jsonMaybeStr (argPosDocMetavar r))
       , ("quoted", jsonBool (isQuotedArg (argPosDocLiteral r) (argPosDocSource r) (argPosDocMany r) mschema))
       , ("many", jsonBool (argPosDocMany r))
       , ("desc", jsonStrArr (argPosDocDesc r))
       , ("constraints", constraintsJsonFor (argPosDocType r))
       , ("metadata", metadataEmpty)
       ]
    ++ ioShapeFields mschema (argPosDocMany r)
         (argPosDocLiteral r)
         (argPosDocSource r) (argPosDocForm r) (argPosDocChecks r)
         (argPosDocListSource r) (argPosDocListForm r) (argPosDocListChecks r)
argToJson mschema (CmdArgOpt r) =
  jsonObj $
    [ ("kind", jsonStr "opt") ]
    ++ schemaField mschema
    ++ [ ("type", jsonStr (typeDescStr (argOptDocType r) (argOptDocLiteral r) (argOptDocSource r) (argOptDocChecks r)))
       , ("metavar", jsonStr (argOptDocMetavar r))
       , ("quoted", jsonBool (isQuotedArg (argOptDocLiteral r) (argOptDocSource r) (argOptDocMany r) mschema))
       , ("many", jsonBool (argOptDocMany r))
       , ("short", cliOptShortJson (argOptDocArg r))
       , ("long", cliOptLongJson (argOptDocArg r))
       , ("default", jsonStr (argOptDocDefault r))
       , ("desc", jsonStrArr (argOptDocDesc r))
       , ("constraints", constraintsJsonFor (argOptDocType r))
       , ("metadata", metadataEmpty)
       ]
    ++ ioShapeFields mschema (argOptDocMany r)
         (argOptDocLiteral r)
         (argOptDocSource r) (argOptDocForm r) (argOptDocChecks r)
         (argOptDocListSource r) (argOptDocListForm r) (argOptDocListChecks r)
argToJson _ (CmdArgFlag r) =
  jsonObj
    [ ("kind", jsonStr "flag")
    , ("short", cliOptShortJson (argFlagDocOpt r))
    , ("long", cliOptLongJson (argFlagDocOpt r))
    , ("long_rev", flagRevJson (argFlagDocOptRev r))
    , ("default", jsonStr (argFlagDocDefault r))
    , ("desc", jsonStrArr (argFlagDocDesc r))
    , ("metadata", metadataEmpty)
    ]
argToJson mschema (CmdArgGrp r) =
  jsonObj $
    [ ("kind", jsonStr "grp") ]
    ++ schemaField mschema
    ++ [ ("type", jsonStr (render (pretty (recDocType r))))
       , ("metavar", jsonStr (recDocMetavar r))
       , ("desc", jsonStrArr (recDocDesc r))
       , ("group_opt", grpOptJson (recDocOpt r))
       , ("entries", jsonArr [grpEntryJson k v | (k, v) <- recDocEntries r])
       , ("constraints", constraintsJsonFor (recDocType r))
       , ("metadata", metadataEmpty)
       ]
  where
    grpOptJson Nothing = jsonNull
    grpOptJson (Just opt) =
      jsonObj
        [ ("short", cliOptShortJson opt)
        , ("long", cliOptLongJson opt)
        ]

    -- Group entries never carry their own schema; the group's top-level
    -- schema is used for dispatch. Pass 'Nothing' to the recursive call.
    grpEntryJson key entry =
      jsonObj
        [ ("key", jsonStr (unKey key))
        , ("arg", argToJson Nothing (either CmdArgFlag CmdArgOpt entry))
        ]

-- | Prefixed @schema@ field when a schema is present, otherwise empty.
-- Used by 'argToJson' to splice the field into the per-variant field
-- list in a consistent position.
schemaField :: Maybe Text -> [(Text, Text)]
schemaField Nothing  = []
schemaField (Just s) = [("schema", jsonStr s)]

-- | Compute the `quoted` flag emitted for a typed CLI arg. The flag
-- tells the nexus to JSON-wrap the argv string before handing it to
-- the runtime, so a bare Str like `Joe Dirt` becomes `"Joe Dirt"` for
-- the JSON parser. The flag fires only when the wire schema reduces
-- to the Str primitive `s` AND the argv is going to be interpreted as
-- an inline literal: with `source: file` the argv is a path and the
-- runtime's File branch must see it unquoted.
isQuotedArg :: Maybe Bool -> Maybe SourceAtom -> Bool -> Maybe Text -> Bool
isQuotedArg _literal mSource many mschema =
  case mschema of
    Nothing -> False
    Just s
      | mSource == Just SourceFile -> False
      | many -> isListOfStrWireSchema s
      | otherwise -> isStrOrOptStrWireSchema s

-- Check if a type is Str or ?Str (for literal string handling).
-- NOTE: this is the surface-type check used for help-text rendering;
-- the manifest's `quoted` field uses the schema-text check below
-- ('isStrWireSchema') so that newtypes-over-Str (whose surface type
-- is not 'Str' but whose wire format is the Str primitive 's') also
-- get the JSON-quoting treatment.
isStrType :: Type -> Bool
isStrType (VarT v) = v == MBT.str
isStrType (OptionalT t) = isStrType t
isStrType _ = False

-- | Strip a leading addHint decoration `<...>` if present. Hints
-- carry the concrete-type tag for newtypes / aliases over a built-in
-- primitive. The implementation drops the literal characters between
-- the angle brackets; brackets without a closing `>` are left in place
-- (defensive against malformed schemas).
peelHint :: Text -> Text
peelHint t = case MT.uncons t of
  Just ('<', tt) -> case MT.span (/= '>') tt of
    (_, post) -> case MT.uncons post of
      Just ('>', after) -> peelHint after
      _ -> t
  _ -> t

-- | Strip a leading `&<klen><name>` rec-decl prefix if present. The
-- klen is a base64-encoded character count; for names of length < 64
-- (the practical case), klen is a single character we can decode by
-- table. Multi-character klens (encoding lengths >= 64, marked with
-- a leading `=`) are left in place -- recursive records with
-- 64+-character names never appear at the position the wire-shape
-- helpers below probe (primitive scalar / top-level array), so the
-- fallback is harmless.
peelRecDecl :: Text -> Text
peelRecDecl t = case MT.uncons t of
  Just ('&', rest) -> case MT.uncons rest of
    Just (c, body) | Just n <- decode1 c -> MT.drop n body
    _ -> t
  _ -> t
  where
    decode1 c
      | c >= '0' && c <= '9' = Just (fromEnum c - fromEnum '0')
      | c >= 'a' && c <= 'z' = Just (fromEnum c - fromEnum 'a' + 10)
      | c >= 'A' && c <= 'Z' = Just (fromEnum c - fromEnum 'A' + 36)
      | c == '+'             = Just 62
      | c == '/'             = Just 63
      | otherwise            = Nothing

-- | True iff the rendered wire schema reduces to the plain Str
-- primitive 's'. Peels rec-decl and addHint decorations but NOT the
-- optional `?` wrapper; 'isOptStrWireSchema' handles `?s` separately
-- so the `many + literal` validator can reject optional-element
-- lists (no token can express null in literal mode).
isStrWireSchema :: Text -> Bool
isStrWireSchema s0 = peelHint (peelRecDecl s0) == "s"

-- | True iff the schema reduces to `?s` (Optional Str primitive),
-- peeling the same outer decorations as 'isStrWireSchema' and a single
-- `?` wrapper before the primitive token.
isOptStrWireSchema :: Text -> Bool
isOptStrWireSchema s0 = case MT.uncons (peelHint (peelRecDecl s0)) of
  Just ('?', rest) -> peelHint rest == "s"
  _ -> False

-- | True iff the schema reduces to either Str or Optional Str. Used by
-- 'isQuotedArg' (the scalar literal:true gate) and by validateArgSpecs
-- when checking scalar literal:true.
isStrOrOptStrWireSchema :: Text -> Bool
isStrOrOptStrWireSchema s = isStrWireSchema s || isOptStrWireSchema s

-- | True iff the schema is array-shaped at the top level. Peels
-- rec-decl + addHint, expects `a`. Used by the `many: true` validator
-- in lieu of the surface-type 'isListType' check, so newtypes / aliases
-- whose wire form is `a<elem>` (but whose surface type isn't the
-- built-in `List` constructor) are also accepted.
isArrayWireSchema :: Text -> Bool
isArrayWireSchema s0 = case MT.uncons (peelHint (peelRecDecl s0)) of
  Just ('a', _) -> True
  _ -> False

-- | For a list-shape schema ('a' + optional dim + element schema),
-- return the element schema text. Returns Nothing if the schema is
-- not list-shaped at the top level.
listElementSchema :: Text -> Maybe Text
listElementSchema s = case MT.uncons (peelHint (peelRecDecl s)) of
  Just ('a', rest) -> Just (skipDim rest)
  _ -> Nothing
  where
    skipDim t = case MT.uncons t of
      Just (':', tt) -> MT.dropWhile (\c -> c >= '0' && c <= '9') tt
      _ -> t

-- | True iff the schema is a list whose element wire schema is the
-- plain Str primitive 's' (NOT `?s`). Optional-element lists are
-- rejected so the `many + literal` validator can produce a focused
-- diagnostic.
isListOfStrWireSchema :: Text -> Bool
isListOfStrWireSchema s = maybe False isStrWireSchema (listElementSchema s)

-- ======================================================================
-- Argument spec validation (compile-time)
-- ======================================================================
--
-- 'validateArgSpecs' is invoked once per command (remote or pure)
-- after the SerialASTs and rendered schema text are computed. It
-- enforces the wire-shape constraints that depend on the schema
-- (and therefore cannot be checked at the docstring-resolution
-- stage), and type-checks each option's 'default:' value against
-- its SerialAST.

-- | Top-level validator. Walks each CmdArg in lockstep with its
-- SerialAST and the already-rendered schema text. The caller renders
-- the schemas once for the manifest; the validator reuses them rather
-- than re-rendering.
validateArgSpecs :: Int -> [CmdArg] -> [SerialAST] -> [Text] -> MorlocMonad ()
validateArgSpecs i cmdargs asts schemas = do
  loc <- Docstrings.argLocPrefix i
  CM.zipWithM_ (validateOne loc) (zip3 [(1::Int) ..] cmdargs schemas) asts
  where
    validateOne :: MDoc -> (Int, CmdArg, Text) -> SerialAST -> MorlocMonad ()
    validateOne loc (n, arg, schemaText) ast = do
      let argLoc = loc <> "argument #" <> pretty n
      case arg of
        CmdArgPos r -> do
          checkManyWire argLoc (argPosDocMany r) schemaText
          checkArgShape argLoc schemaText arg
        CmdArgOpt r -> do
          checkManyWire argLoc (argOptDocMany r) schemaText
          checkDefault  argLoc ast (argOptDocDefault r)
          checkArgShape argLoc schemaText arg
        CmdArgFlag _ -> return ()
        CmdArgGrp r  -> validateGroup argLoc r ast

    checkArgShape :: MDoc -> Text -> CmdArg -> MorlocMonad ()
    checkArgShape argLoc schemaText arg =
      case Docstrings.validateCmdArg schemaText arg of
        Right _ -> return ()
        Left e  -> MM.throwSystemError $ argLoc <> ": " <> pretty e

-- | Wire-schema category. Drives the per-shape rules: each category
-- has its own narrow vocabulary for `source:` / `form:` / `list.*:`.
-- Lists carry their element schema so per-element validation can
-- recurse into the element's category.
data SchemaCat
  = CatScalarPrim       -- Bool, Int, Real, UInt*, Float*, Null, etc.
  | CatStr              -- Str (`s`)
  | CatList Text        -- `a<elem>` for any elem schema
  | CatOtherCompound    -- tuples, records, maps, tables
  deriving (Eq, Show)

-- | Classify the wire schema. The hint/recursive-decl prefix and outer
-- `?` are peeled before classification so optional and aliased forms
-- inherit the underlying category.
classifySchema :: Text -> SchemaCat
classifySchema s0 =
  let peeled = peelHint (peelRecDecl s0)
      core = case MT.uncons peeled of
        Just ('?', rest) -> peelHint rest
        _ -> peeled
  in if core == "s"
       then CatStr
       else if isScalarPrimCore core
              then CatScalarPrim
              else case MT.uncons core of
                Just ('a', rest) -> CatList rest
                _                -> CatOtherCompound

isScalarPrimCore :: Text -> Bool
isScalarPrimCore c = c `elem`
  [ "b", "j", "z"
  , "i1", "i2", "i4", "i8"
  , "u1", "u2", "u4", "u8"
  , "f4", "f8"
  ]

-- | `many: true` requires an array wire type.
checkManyWire :: MDoc -> Bool -> Text -> MorlocMonad ()
checkManyWire _   False _      = return ()
checkManyWire loc True  schema
  | isArrayWireSchema schema = return ()
  | otherwise = MM.throwSystemError $
      loc <> ": `many: true` requires a list-typed argument (wire schema"
        <> " starting with `a`); got `" <> pretty schema <> "`. Remove"
        <> " `many: true` or change the type to a list."

-- | Parse the default text as JSON and structurally validate it
-- against the SerialAST. Parser failures are surfaced verbatim --
-- being explicit about the message's origin is preferable to
-- rewriting the parser's framing, which varies between aeson
-- versions.
checkDefault :: MDoc -> SerialAST -> Text -> MorlocMonad ()
checkDefault loc ast defaultText =
  case Aeson.eitherDecodeStrict (TE.encodeUtf8 defaultText) of
    Left e -> MM.throwSystemError $ formatBlock
      [ loc <> ": default value is not valid JSON, parser message:"
      , pretty (MT.pack e)
      , "Default text:"
      , pretty (truncateForMsg defaultText)
      ]
    Right value -> validateValueAgainstAST loc Map.empty rootPath ast value

-- Recurse into the entries of an unrolled record group, matching them
-- against the SerialObject's per-field parameters.
validateGroup :: MDoc -> RecDocSet -> SerialAST -> MorlocMonad ()
validateGroup loc r ast = case peelGroupAst ast of
  Just (SerialObject _ _ _ pairs) -> do
    let asts = [(k, sub) | (k, sub) <- pairs]
        entries = recDocEntries r
    CM.forM_ entries $ \(k, entry) -> do
      let entryLoc = loc <> "/field " <> pretty (unKey k)
      case lookup k asts of
        Nothing -> MM.throwSystemError $
          entryLoc <> ": no schema field for unrolled record entry"
        Just subAst -> case entry of
          Right opt -> do
            let subSchema = render (Serial.serialAstToMsgpackSchema subAst)
            checkManyWire entryLoc (argOptDocMany opt) subSchema
            checkDefault entryLoc subAst (argOptDocDefault opt)
            case Docstrings.validateCmdArg subSchema (CmdArgOpt opt) of
              Right _ -> return ()
              Left e  -> MM.throwSystemError $ entryLoc <> ": " <> pretty e
          Left _flag -> return ()
  _ -> return ()
  where
    -- A group's AST may be wrapped in a SerialPack (newtype-over-record).
    peelGroupAst (SerialPack _ (_, inner)) = peelGroupAst inner
    peelGroupAst a = Just a

-- ----------------------------------------------------------------------
-- Default-value validation
-- ----------------------------------------------------------------------

-- | Recursive-type env: each entry binds a TVar (the record's name) to
-- the SerialAST that declared it, so SerialRec back-references can
-- resolve. Bound at SerialObject; consulted at SerialRec.
type RecEnv = Map.Map TVar SerialAST

-- | The "JSON path" within the default value at which validation is
-- currently happening, used to anchor mismatch diagnostics. Local to
-- this module; do NOT confuse with 'Morloc.Namespace.Prim.Path' (the
-- filesystem-path alias used elsewhere).
type JsonPath = MDoc

-- | Walk a JSON value against a SerialAST. Errors point at the path,
-- the expected wire shape, and a (truncated) snippet of the offending
-- value.
validateValueAgainstAST :: MDoc -> RecEnv -> JsonPath -> SerialAST -> Aeson.Value -> MorlocMonad ()
validateValueAgainstAST loc env path ast value = case (ast, value) of
  -- SerialPack: newtype/alias is transparent for default validation;
  -- the user writes the JSON shape of the underlying packed type.
  (SerialPack _ (_, inner), _) ->
    validateValueAgainstAST loc env path inner value

  -- Primitives.
  (SerialNull _,    Aeson.Null)     -> return ()
  (SerialBool _,    Aeson.Bool _)   -> return ()
  (SerialString _,  Aeson.String _) -> return ()
  -- Stream-handle types at the JSON CLI boundary appear as the path
  -- string the receiving pool will re-open. Same wire shape as
  -- SerialString.
  (SerialIFile _,   Aeson.String _) -> return ()
  (SerialOStream _, Aeson.String _) -> return ()
  (SerialIStream _, Aeson.String _) -> return ()
  (SerialReal _,    v)              -> checkRealLike   loc path ast v
  (SerialFloat32 _, v)              -> checkRealLike   loc path ast v
  (SerialFloat64 _, v)              -> checkRealLike   loc path ast v
  (SerialInt _,     v)              -> checkInteger    loc path ast (Nothing, Nothing) v
  (SerialInt8 _,    v)              -> checkBoundedInt loc path ast (-128, 127) v
  (SerialInt16 _,   v)              -> checkBoundedInt loc path ast (-32768, 32767) v
  (SerialInt32 _,   v)              -> checkBoundedInt loc path ast (-2147483648, 2147483647) v
  (SerialInt64 _,   v)              -> checkBoundedInt loc path ast (-9223372036854775808, 9223372036854775807) v
  -- UInt is bigint-capable, so the only constraint is "non-negative";
  -- use checkInteger with min-only (Just 0, Nothing) instead of a
  -- fictitious u64 upper bound.
  (SerialUInt _,    v)              -> checkInteger    loc path ast (Just 0, Nothing) v
  (SerialUInt8 _,   v)              -> checkBoundedInt loc path ast (0, 255) v
  (SerialUInt16 _,  v)              -> checkBoundedInt loc path ast (0, 65535) v
  (SerialUInt32 _,  v)              -> checkBoundedInt loc path ast (0, 4294967295) v
  (SerialUInt64 _,  v)              -> checkBoundedInt loc path ast (0, 18446744073709551615) v

  -- Optional: null OR the inner type's value.
  (SerialOptional _ _,     Aeson.Null) -> return ()
  (SerialOptional _ inner, v)          -> validateValueAgainstAST loc env path inner v

  -- Lists: array, with an optional fixed-length constraint.
  (SerialList _ dim elemAst, Aeson.Array arr) -> do
    case dim of
      Just (NatLitF n) | fromIntegral (V.length arr) /= n ->
        MM.throwSystemError $ formatBlock
          [ loc <> ": default" <> pathAt path <> " is an array of length "
              <> pretty (V.length arr) <> ", expected length "
              <> pretty n <> "."
          , "Expected: " <> pretty (expectedJsonShape ast)
          , "Got: " <> pretty (truncateForMsg (jsonGotDisplay value))
          ]
      _ -> return ()
    CM.zipWithM_
      (\j v -> validateValueAgainstAST loc env (path <> "[" <> pretty j <> "]") elemAst v)
      [(0::Int) ..]
      (V.toList arr)

  -- Tuples: array of exactly the right length.
  (SerialTuple _ subs, Aeson.Array arr)
    | length subs /= V.length arr ->
        MM.throwSystemError $ formatBlock
          [ loc <> ": default" <> pathAt path <> " is an array of length "
              <> pretty (V.length arr) <> ", expected tuple of length "
              <> pretty (length subs) <> "."
          , "Expected: " <> pretty (expectedJsonShape ast)
          , "Got: " <> pretty (truncateForMsg (jsonGotDisplay value))
          ]
    | otherwise ->
        CM.zipWithM_
          (\j (subAst, v) -> validateValueAgainstAST loc env (path <> "[" <> pretty j <> "]") subAst v)
          [(0::Int) ..]
          (zip subs (V.toList arr))

  -- Records: object form. Collect every missing required field up
  -- front so the diagnostic lists all of them at once (for a large
  -- record, a one-by-one report would force the user through many
  -- recompiles to discover the full set).
  (SerialObject _ (FV v _) _ fields, Aeson.Object obj) -> do
    let env' = Map.insert v ast env
        missing = [k | (k, _) <- fields
                     , not (KM.member (AesonKey.fromText (unKey k)) obj)]
    case missing of
      []  -> CM.forM_ fields $ \(k, fieldAst) ->
        case KM.lookup (AesonKey.fromText (unKey k)) obj of
          Just fv -> validateValueAgainstAST loc env'
                       (path <> "." <> pretty (unKey k)) fieldAst fv
          Nothing -> return ()
      ks  -> MM.throwSystemError $ formatBlock
        [ loc <> ": default" <> pathAt path
            <> (if length ks == 1
                then " is missing required key "
                else " is missing required keys ")
            <> pretty (renderKeyList ks) <> "."
        , "Expected: " <> pretty (expectedJsonShape ast)
        , "Got " <> pretty (jsonKind value) <> ": "
            <> pretty (truncateForMsg (jsonGotDisplay value))
        ]

  -- Recursive back-reference: look up the binding, recurse.
  (SerialRec (FV v _), _) -> case Map.lookup v env of
    Just bound -> validateValueAgainstAST loc env path bound value
    Nothing -> MM.throwSystemError $
      loc <> ": compiler bug: unbound SerialRec `" <> pretty (unTVar v)
        <> "` reached default-value validator" <> pathAt path

  -- SerialUnknown: opaque type, cannot validate; allow anything.
  (SerialUnknown _, _) -> return ()

  -- Anything else is a type mismatch.
  _ -> typeMismatch loc path ast value

-- | Mismatch reporter. Describes the offending JSON value's kind
-- (Object / Array / String / Number / Bool / Null) AND the full
-- expected JSON shape derived from the SerialAST so the diagnostic
-- carries the same information whether the user wrote a primitive,
-- a string, a list, or a nested record. Laid out in three blocks
-- separated by blank lines so the terminal-wrap doesn't smear them.
typeMismatch :: MDoc -> JsonPath -> SerialAST -> Aeson.Value -> MorlocMonad ()
typeMismatch loc path expectedAst value =
  MM.throwSystemError $ formatBlock
    [ loc <> ": default" <> pathAt path <> " has the wrong shape."
    , "Expected: " <> pretty (expectedJsonShape expectedAst)
    , "Got " <> pretty (jsonKind value) <> ": "
        <> pretty (truncateForMsg (jsonGotDisplay value))
    ]

-- | Render the "at <path>" suffix for diagnostics. The root path is
-- the empty MDoc ('rootPath'); descents prepend their own delimiter
-- (`.` for record fields, `[i]` for list / tuple elements), so the
-- suffix only appears once we've actually walked into a field or
-- element.
pathAt :: JsonPath -> MDoc
pathAt p
  | render p == "" = ""
  | otherwise      = " at " <> p

-- | The empty root JSON path. Centralised so the "no descent yet"
-- sentinel is constructed in one place rather than as a stringly
-- literal scattered across call sites.
rootPath :: JsonPath
rootPath = mempty

-- | Lay out a validator diagnostic as one summary line followed by
-- blank-separated detail blocks. Keeps Expected / Got on their own
-- lines so terminal wrap doesn't muddle them.
formatBlock :: [MDoc] -> MDoc
formatBlock = concatWith (\a b -> a <> hardline <> hardline <> b)

-- | Render a list of record keys in JSON-array shape, e.g.
-- @["alpha", "beta"]@. Used in the missing-keys diagnostic to mirror
-- the JSON formality.
renderKeyList :: [Key] -> Text
renderKeyList ks = "[" <> MT.intercalate ", " ["\"" <> unKey k <> "\"" | k <- ks] <> "]"

-- | "Got" display for an Aeson value. For 'Aeson.String' we want the
-- raw string content -- showing the JSON-encoded form would add an
-- extra layer of quotes and backslash escapes that the user did NOT
-- write and that obscures the actual value. For non-strings the
-- canonical JSON encoding is the right thing.
jsonGotDisplay :: Aeson.Value -> Text
jsonGotDisplay (Aeson.String s) = s
jsonGotDisplay v                = jsonSnippet v

-- | Compact human-readable name for a JSON value's kind. Goes into
-- the mismatch diagnostic so the user can immediately tell what they
-- wrote at the top level (e.g. a JSON String containing JSON object
-- text vs. a real JSON object).
jsonKind :: Aeson.Value -> Text
jsonKind (Aeson.Null)     = "null"
jsonKind (Aeson.Bool _)   = "JSON Bool"
jsonKind (Aeson.Number _) = "JSON Number"
jsonKind (Aeson.String _) = "JSON String"
jsonKind (Aeson.Array _)  = "JSON Array"
jsonKind (Aeson.Object _) = "JSON Object"

-- | Render a SerialAST as the expected JSON shape, in a JSON-Schema-ish
-- syntax that names every field and element type. Primitives use their
-- bare morloc type names (no range annotations) -- the range info is
-- only relevant when the JSON value IS a number and its value falls
-- out of bounds, which is reported separately by 'rangeMismatch' using
-- 'expectedRange'.
expectedJsonShape :: SerialAST -> Text
expectedJsonShape = go
  where
    go (SerialPack _ (_, s))         = go s
    go (SerialNull _)                = "null"
    go (SerialBool _)                = "Bool"
    go (SerialString _)              = "Str"
    go (SerialIFile _)               = "IFile path (Str)"
    go (SerialOStream _)             = "OStream path (Str)"
    go (SerialIStream _)             = "IStream path (Str)"
    go (SerialReal _)                = "Real"
    go (SerialFloat32 _)             = "Float32"
    go (SerialFloat64 _)             = "Float64"
    go (SerialInt _)                 = "Int"
    go (SerialInt8 _)                = "Int8"
    go (SerialInt16 _)               = "Int16"
    go (SerialInt32 _)               = "Int32"
    go (SerialInt64 _)               = "Int64"
    go (SerialUInt _)                = "UInt"
    go (SerialUInt8 _)               = "UInt8"
    go (SerialUInt16 _)              = "UInt16"
    go (SerialUInt32 _)              = "UInt32"
    go (SerialUInt64 _)              = "UInt64"
    go (SerialOptional _ s)          = go s <> " | null"
    go (SerialList _ Nothing s)      = "[" <> go s <> "]"
    go (SerialList _ (Just (NatLitF n)) s) =
      "[" <> go s <> "] of length " <> MT.pack (show n)
    go (SerialList _ _ s)            = "[" <> go s <> "]"
    go (SerialTuple _ ss)            =
      "(" <> MT.intercalate ", " (map go ss) <> ")"
    go (SerialObject _ _ _ fields)   =
      "{ " <> MT.intercalate ", " [unKey k <> ": " <> go v | (k, v) <- fields] <> " }"
    go (SerialRec (FV (TV name) _))  = name <> " (recursive)"
    go (SerialUnknown _)             = "Unknown"

-- | Numeric range description, used ONLY by 'rangeMismatch' to spell
-- the legal bounds when the user's number falls outside the type's
-- range. Only invoked from the integer arms of 'checkInteger', so
-- non-numeric nodes here are a compiler bug, not a runtime case.
expectedRange :: SerialAST -> Text
expectedRange (SerialPack _ (_, s)) = expectedRange s
expectedRange (SerialInt _)         = "any integer (BigInt)"
expectedRange (SerialInt8 _)        = "Int8 in -128 .. 127"
expectedRange (SerialInt16 _)       = "Int16 in -32768 .. 32767"
expectedRange (SerialInt32 _)       = "Int32 in -2147483648 .. 2147483647"
expectedRange (SerialInt64 _)       = "Int64 in -9223372036854775808 .. 9223372036854775807"
expectedRange (SerialUInt _)        = "UInt >= 0 (BigInt)"
expectedRange (SerialUInt8 _)       = "UInt8 in 0 .. 255"
expectedRange (SerialUInt16 _)      = "UInt16 in 0 .. 65535"
expectedRange (SerialUInt32 _)      = "UInt32 in 0 .. 4294967295"
expectedRange (SerialUInt64 _)      = "UInt64 in 0 .. 18446744073709551615"
expectedRange ast = error
  $ "expectedRange: non-numeric SerialAST node reached the range reporter; "
  <> "this indicates 'rangeMismatch' was called outside an integer arm. "
  <> "Node shape: " <> MT.unpack (expectedJsonShape ast)

-- | Accept a JSON Number OR one of the three special-string forms
-- morloc uses on the wire for non-finite IEEE-754 values
-- (`"inf"` / `"-inf"` / `"nan"`).
checkRealLike :: MDoc -> JsonPath -> SerialAST -> Aeson.Value -> MorlocMonad ()
checkRealLike _   _    _   (Aeson.Number _) = return ()
checkRealLike _   _    _   (Aeson.String s)
  | s `elem` ["inf", "-inf", "nan"]         = return ()
checkRealLike loc path ast v                = typeMismatch loc path ast v

-- | Integer check with optional bounds. Bounds are 'Maybe' on each
-- side independently so unbounded-low (e.g. SerialInt, no lower bound
-- because bigint) or unbounded-high (SerialUInt, no upper bound: only
-- non-negative) can be expressed without inventing a sentinel max.
-- Out-of-range values report against the SerialAST's
-- 'expectedJsonShape', which already spells the legal range.
checkInteger
  :: MDoc -> JsonPath -> SerialAST
  -> (Maybe Integer, Maybe Integer)  -- (lo, hi)
  -> Aeson.Value
  -> MorlocMonad ()
checkInteger loc path ast (mlo, mhi) (Aeson.Number n)
  | DS.isInteger n = case toIntegerSafe n of
      Just k
        | maybe True (k >=) mlo && maybe True (k <=) mhi -> return ()
        | otherwise -> rangeMismatch loc path ast (Aeson.Number n)
      Nothing -> rangeMismatch loc path ast (Aeson.Number n)
  | otherwise = typeMismatch loc path ast (Aeson.Number n)
checkInteger loc path ast _ v = typeMismatch loc path ast v

-- | Bounded-integer convenience for sized types. Both bounds required.
checkBoundedInt
  :: MDoc -> JsonPath -> SerialAST -> (Integer, Integer) -> Aeson.Value -> MorlocMonad ()
checkBoundedInt loc path ast (lo, hi) v =
  checkInteger loc path ast (Just lo, Just hi) v

-- | Range mismatch: the JSON IS a number, but its value falls outside
-- the type's allowed range. Distinguished from 'typeMismatch' so the
-- diagnostic says "out of range" rather than "wrong shape" and uses
-- 'expectedRange' (which spells the legal numeric bounds) instead of
-- 'expectedJsonShape'.
rangeMismatch :: MDoc -> JsonPath -> SerialAST -> Aeson.Value -> MorlocMonad ()
rangeMismatch loc path ast value =
  MM.throwSystemError $ formatBlock
    [ loc <> ": default" <> pathAt path <> " is out of range."
    , "Expected: " <> pretty (expectedRange ast)
    , "Got: " <> pretty (truncateForMsg (jsonGotDisplay value))
    ]

toIntegerSafe :: DS.Scientific -> Maybe Integer
toIntegerSafe n
  | DS.isInteger n = Just (DS.coefficient n * 10 ^ DS.base10Exponent n)
  | otherwise = Nothing

-- | Truncating display of a Text value (default text or JSON snippet)
-- for inclusion in error messages. Keeps diagnostics readable when the
-- user pastes a multi-KB JSON literal.
truncateForMsg :: Text -> Text
truncateForMsg t
  | MT.length t <= 80 = t
  | otherwise = MT.take 77 t <> "..."

-- | Render a JSON Value as a single-line snippet for error messages.
jsonSnippet :: Aeson.Value -> Text
jsonSnippet = TE.decodeUtf8 . BSL.toStrict . Aeson.encode

-- | The user-facing "type:" line for a CLI arg. For a Str argument
-- that isn't declared literal, the parenthetical clarifies whether
-- argv is treated as a filesystem path only (when `source: file` or a
-- `check.path:*` forces path semantics) or as the default
-- filename-or-quoted-JSON.
typeDescStr :: Type -> Maybe Bool -> Maybe SourceAtom -> [Check] -> Text
typeDescStr t isLiteral mSrc checks
  | isStrType t && isLiteral /= Just True =
      if pathOnly then "Str    (a filename)"
                  else "Str    (a filename or quoted JSON string)"
  | otherwise = render (pretty t)
  where
    pathOnly = mSrc == Just SourceFile || any isPathCheck checks
    isPathCheck (CheckPath _) = True

-- | Strip outer wrappers that don't change a type's "name kind" identity
-- (Optional and Effect wrappers are transparent for record/object/table
-- classification). Used by 'surfaceNamKind'.
stripSurface :: Type -> Type
stripSurface (OptionalT t) = stripSurface t
stripSurface (EffectT _ t) = stripSurface t
stripSurface t             = t

-- | Peel only the 'EffectT' wrappers, leaving any 'OptionalT' visible.
-- Used by the bare-Null lowering to detect when a 'NullS' literal sits in
-- an optional position (and therefore needs the wider OptNullX layout).
stripEffect :: Type -> Type
stripEffect (EffectT _ t) = stripEffect t
stripEffect t             = t

-- | Validate a bracket bound's type for the pure-runtime path. NullS
-- bounds and integer-typed bounds pass; anything else raises a sourced
-- codegen error citing the inferred type and the bound's role
-- (start / stop / step / index).
validateBracketBound :: Text -> AnnoS (Indexed Type) One () -> MorlocMonad ()
validateBracketBound role (AnnoS (Idx i t) _ _) =
  if isIntegralPureBound t
    then return ()
    else MM.throwSourcedError i $
      "Bracket" <+> pretty role <+> "bound must be an integer wire type"
      <+> "in pure morloc (one of Int, Int8/16/32/64, UInt8/16/32/64);"
      <+> "got" <+> pretty t

-- | True if the receiver expression has an IFile-headed type. Used to
-- route bracket and struct patterns through the IFile runtime walker
-- rather than the generic Array/Tuple dispatch.
recvIsIFile :: AnnoS (Indexed Type) One () -> Bool
recvIsIFile (AnnoS (Idx _ t) _ _) = MBT.isIFileHead t

-- | Manifold index of an AnnoS subtree (used to anchor sourced errors
-- on the closest source span).
annoIdx :: AnnoS (Indexed Type) One () -> Int
annoIdx (AnnoS (Idx i _) _ _) = i

-- | True if 't' is an integer wire type, possibly wrapped in a single
-- 'OptionalT' (the @(Null :: ?Int64)@ shape from the desugar's empty
-- bound annotation) and/or 'EffectT' layers.
isIntegralPureBound :: Type -> Bool
isIntegralPureBound t = case peel t of
  VarT (TV n) -> n `elem`
    [ "Int"
    , "Int8", "Int16", "Int32", "Int64"
    , "UInt", "UInt8", "UInt16", "UInt32", "UInt64"
    ]
  _ -> False
  where
    peel (OptionalT inner) = peel inner
    peel (EffectT _ inner) = peel inner
    peel inner             = inner

-- | If a type's surface form is a named type, return its 'NamType' tag.
-- Otherwise Nothing. Single source of the @kind@ constraint.
surfaceNamKind :: Type -> Maybe NamType
surfaceNamKind t = case stripSurface t of
  NamT o _ _ _ -> Just o
  _            -> Nothing

-- | Lowercase label for a 'NamType' constructor, used as the value of
-- the @kind@ constraint in the manifest.
namTagLabel :: NamType -> Text
namTagLabel NamRecord = "record"
namTagLabel NamObject = "object"
namTagLabel NamTable  = "table"

-- | Build the JSON @constraints@ array for a surface type. Only the
-- @kind@ constraint is populated today; future constraints (min, max,
-- regex, length, ...) will append to this list.
constraintsJsonFor :: Type -> Text
constraintsJsonFor t = jsonArr $ catMaybes
  [ (\nt -> jsonObj
      [ ("type", jsonStr "kind")
      , ("value", jsonStr (namTagLabel nt))
      ]) <$> surfaceNamKind t
  ]

-- | An empty @metadata@ slot. Always emitted so consumers never have to
-- check presence.
metadataEmpty :: Text
metadataEmpty = jsonObj []

cliOptShortJson :: CliOpt -> Text
cliOptShortJson (CliOptShort c) = jsonStr (MT.singleton c)
cliOptShortJson (CliOptBoth c _) = jsonStr (MT.singleton c)
cliOptShortJson _ = jsonNull

cliOptLongJson :: CliOpt -> Text
cliOptLongJson (CliOptLong l) = jsonStr l
cliOptLongJson (CliOptBoth _ l) = jsonStr l
cliOptLongJson _ = jsonNull

flagRevJson :: Maybe CliOpt -> Text
flagRevJson Nothing = jsonNull
flagRevJson (Just (CliOptLong l)) = jsonStr l
flagRevJson (Just (CliOptBoth _ l)) = jsonStr l
flagRevJson _ = jsonNull

-- ======================================================================
-- Expression tree serialization
-- ======================================================================

exprToJson :: NexusExpr -> Text
exprToJson (LitX lt v) =
  jsonObj
    [ ("tag", jsonStr "lit")
    , ("schema", jsonStr (litSchemaStr lt))
    , ("lit_type", jsonStr (litSchemaStr lt))
    , ("value", jsonStr v)
    ]
exprToJson (StrX schema v) =
  jsonObj
    [ ("tag", jsonStr "str")
    , ("schema", jsonStr schema)
    , ("value", jsonStr v)
    ]
exprToJson (LstX schema es) =
  jsonObj
    [ ("tag", jsonStr "container")
    , ("schema", jsonStr schema)
    , ("elements", jsonArr (map exprToJson es))
    ]
exprToJson (TupX schema es) =
  jsonObj
    [ ("tag", jsonStr "container")
    , ("schema", jsonStr schema)
    , ("elements", jsonArr (map exprToJson es))
    ]
exprToJson (NamX schema entries) =
  jsonObj
    [ ("tag", jsonStr "container")
    , ("schema", jsonStr schema)
    , ("elements", jsonArr (map (exprToJson . snd) entries))
    ]
exprToJson (AppX schema func args) =
  jsonObj
    [ ("tag", jsonStr "app")
    , ("schema", jsonStr schema)
    , ("func", exprToJson func)
    , ("args", jsonArr (map exprToJson args))
    ]
exprToJson (LamX vars body) =
  jsonObj
    [ ("tag", jsonStr "lambda")
    , ("vars", jsonStrArr vars)
    , ("body", exprToJson body)
    ]
exprToJson (BndX schema var) =
  jsonObj
    [ ("tag", jsonStr "bound")
    , ("schema", jsonStr schema)
    , ("var", jsonStr var)
    ]
exprToJson (MapX schema funcExpr listExpr) =
  jsonObj
    [ ("tag", jsonStr "map")
    , ("schema", jsonStr schema)
    , ("func", exprToJson funcExpr)
    , ("list", exprToJson listExpr)
    ]
exprToJson (ShowX schema child) =
  jsonObj
    [ ("tag", jsonStr "show")
    , ("schema", jsonStr schema)
    , ("child", exprToJson child)
    ]
exprToJson (ReadX schema child) =
  jsonObj
    [ ("tag", jsonStr "read")
    , ("schema", jsonStr schema)
    , ("child", exprToJson child)
    ]
exprToJson (HashX schema child) =
  jsonObj
    [ ("tag", jsonStr "hash")
    , ("schema", jsonStr schema)
    , ("child", exprToJson child)
    ]
exprToJson (SaveX fmt schema level value path) =
  jsonObj
    [ ("tag", jsonStr "save")
    , ("format", jsonStr fmt)
    , ("schema", jsonStr schema)
    , ("level", exprToJson level)
    , ("value", exprToJson value)
    , ("path", exprToJson path)
    ]
exprToJson (LoadX schema child) =
  jsonObj
    [ ("tag", jsonStr "load")
    , ("schema", jsonStr schema)
    , ("child", exprToJson child)
    ]
exprToJson (OpenX schema kind path) =
  jsonObj
    [ ("tag", jsonStr "open")
    , ("schema", jsonStr schema)
    , ("kind", jsonInt (fromIntegral kind))
    , ("path", exprToJson path)
    ]
exprToJson (CloseX handle) =
  jsonObj
    [ ("tag", jsonStr "close")
    , ("handle", exprToJson handle)
    ]
exprToJson (FSchemaX schema path) =
  jsonObj
    [ ("tag", jsonStr "fschema")
    , ("schema", jsonStr schema)
    , ("path", exprToJson path)
    ]
exprToJson (FLengthX schema handle) =
  jsonObj
    [ ("tag", jsonStr "flen")
    , ("schema", jsonStr schema)
    , ("handle", exprToJson handle)
    ]
exprToJson (IFileWalkX schema handle path runtimeArgs) =
  jsonObj
    [ ("tag", jsonStr "ifile_walk")
    , ("schema", jsonStr schema)
    , ("handle", exprToJson handle)
    , ("path", jsonStr path)
    , ("args", jsonArr (map exprToJson runtimeArgs))
    ]
exprToJson (NextX schema handle) =
  jsonObj
    [ ("tag", jsonStr "next")
    , ("schema", jsonStr schema)
    , ("handle", exprToJson handle)
    ]
exprToJson (StreamX schema handle) =
  jsonObj
    [ ("tag", jsonStr "stream")
    , ("schema", jsonStr schema)
    , ("handle", exprToJson handle)
    ]
exprToJson (OpenOStreamX schema path) =
  jsonObj
    [ ("tag", jsonStr "open_ostream")
    , ("schema", jsonStr schema)
    , ("path", exprToJson path)
    ]
exprToJson (WriteX schema level value handle) =
  jsonObj
    [ ("tag", jsonStr "write")
    , ("schema", jsonStr schema)
    , ("level", exprToJson level)
    , ("value", exprToJson value)
    , ("handle", exprToJson handle)
    ]
exprToJson (AppendX schema path) =
  jsonObj
    [ ("tag", jsonStr "append")
    , ("schema", jsonStr schema)
    , ("path", exprToJson path)
    ]
exprToJson (ConcatX paths dest) =
  jsonObj
    [ ("tag", jsonStr "concat")
    , ("paths", exprToJson paths)
    , ("dest", exprToJson dest)
    ]
exprToJson (FlushX handle) =
  jsonObj
    [ ("tag", jsonStr "flush")
    , ("handle", exprToJson handle)
    ]
exprToJson (StdinX schema) =
  jsonObj
    [ ("tag", jsonStr "stdin")
    , ("schema", jsonStr schema)
    ]
exprToJson (StdoutX schema) =
  jsonObj
    [ ("tag", jsonStr "stdout")
    , ("schema", jsonStr schema)
    ]
exprToJson (StderrX schema) =
  jsonObj
    [ ("tag", jsonStr "stderr")
    , ("schema", jsonStr schema)
    ]
exprToJson (OptX schema child) =
  jsonObj
    [ ("tag", jsonStr "container")
    , ("schema", jsonStr schema)
    , ("elements", jsonArr [exprToJson child])
    ]
exprToJson (OptNullX schema) =
  jsonObj
    [ ("tag", jsonStr "container")
    , ("schema", jsonStr schema)
    , ("elements", jsonArr [])
    ]
exprToJson (PatX schema (PatternText p ps)) =
  jsonObj
    [ ("tag", jsonStr "interpolation")
    , ("schema", jsonStr schema)
    , ("strings", jsonStrArr (p : ps))
    ]
exprToJson (PatX schema (PatternStruct sel)) =
  jsonObj
    [ ("tag", jsonStr "pattern")
    , ("schema", jsonStr schema)
    , ("pattern", selectorToJson sel)
    ]
-- Bracket patterns (index, slice). When these appear at the nexus
-- boundary (a partially-applied accessor exposed as a top-level value),
-- emit a pattern record the nexus interprets opaquely. The nexus does
-- not execute the bracket operation itself; the call always dispatches
-- to a pool manifold that contains the resolved bracket call.
exprToJson (PatX schema PatternBracketIndex) =
  jsonObj
    [ ("tag", jsonStr "pattern")
    , ("schema", jsonStr schema)
    , ("pattern", jsonObj [("type", jsonStr "bracket_index")])
    ]
exprToJson (PatX schema PatternBracketSlice) =
  jsonObj
    [ ("tag", jsonStr "pattern")
    , ("schema", jsonStr schema)
    , ("pattern", jsonObj [("type", jsonStr "bracket_slice")])
    ]

selectorToJson :: Selector -> Text
selectorToJson SelectorEnd = jsonObj [("type", jsonStr "end")]
selectorToJson (SelectorIdx t ts) =
  jsonObj
    [ ("type", jsonStr "idx")
    , ("selectors", jsonArr [idxSel i s | (i, s) <- t : ts])
    ]
  where
    idxSel i sub =
      jsonObj
        [ ("index", jsonInt i)
        , ("sub", selectorToJson sub)
        ]
selectorToJson (SelectorKey t ts) =
  jsonObj
    [ ("type", jsonStr "key")
    , ("selectors", jsonArr [keySel k s | (k, s) <- t : ts])
    ]
  where
    keySel k sub =
      jsonObj
        [ ("key", jsonStr k)
        , ("sub", selectorToJson sub)
        ]
-- Bracket steps inside a Selector chain: emitted as their own JSON
-- nodes. The runtime's apply_getter walks into them, consumes one
-- bracket runtime arg (index for bracket_index; three for
-- bracket_slice), and continues with the bracket result.
--
-- 'bracket_index' carries a "sub" selector that runs on the indexed
-- element. 'bracket_slice' is terminal (slice returns a list; any
-- per-element walk is morloc's IntrMap territory, not a single
-- selector walk).
selectorToJson (SelectorBracketIndex sub) =
  jsonObj
    [ ("type", jsonStr "bracket_index")
    , ("sub", selectorToJson sub)
    ]
selectorToJson SelectorBracketSlice =
  jsonObj
    [ ("type", jsonStr "bracket_slice")
    ]

litSchemaStr :: LitType -> Text
litSchemaStr F32X = "f4"
litSchemaStr F64X = "f8"
litSchemaStr I8X = "i1"
litSchemaStr I16X = "i2"
litSchemaStr I32X = "i4"
litSchemaStr I64X = "i8"
litSchemaStr U8X = "u1"
litSchemaStr U16X = "u2"
litSchemaStr U32X = "u4"
litSchemaStr U64X = "u8"
litSchemaStr BoolX = "b"
litSchemaStr NullX = "z"
litSchemaStr IntX = "j"

-- ======================================================================
-- Manifest builder
-- ======================================================================

-- | Bag of inputs to 'buildManifest'. New top-level manifest fields
-- get a record field here and a corresponding emission in the JSON
-- object below; the call site changes once and the function arity
-- stays at one.
data ManifestInputs = ManifestInputs
  { miConfig              :: !Config
  , miRegistry            :: !LangRegistry
  , miProgramName         :: !String
  , miBuildDir            :: !String
  , miBuildTime           :: !Int
  , miDaemonSets          :: ![(Lang, Socket)]
  , miFData               :: ![FData]
  , miGasts               :: ![GastData]
  , miLangToPool          :: !(Lang -> Int)
  , miIndexToGroup        :: !(Map.Map Int Text)
  , miGroupDescs          :: !(Map.Map Text [Text])
  , miModuleDoc           :: ![Text]
  , miModuleEpilogues     :: ![[Text]]
  , miUnsafeSkipNullCheck :: !Bool
  , miInlineSize          :: !(Maybe Int64)
  , miNoShm               :: !Bool
  , miTmpdir              :: !(Maybe Path)
  , miRunLog              :: !(Maybe RenderedRunLog)
  , miCapabilities        :: ![Text]
  }

buildManifest :: ManifestInputs -> Text
buildManifest ManifestInputs{..} =
  jsonObj
    [ ("name", jsonStr (MT.pack miProgramName))
    , ("build", buildJson)
    , ("pools", jsonArr (map poolJson miDaemonSets))
    , ("commands", jsonArr (map remoteCmdJson miFData ++ map pureCmdJson miGasts))
    , ("groups", jsonArr (map groupJson (Map.toList miGroupDescs)))
    , ("desc", jsonStrArr miModuleDoc)
    , ("epilogues", jsonArr (map jsonStrArr miModuleEpilogues))
    , ("unsafe_skip_null_check", jsonBool miUnsafeSkipNullCheck)
    , ("inline_size", maybe jsonNull jsonInt64 miInlineSize)
    , ("no_shm", jsonBool miNoShm)
    , ("tmpdir", maybe jsonNull (jsonStr . MT.pack) miTmpdir)
    , ("run_log", runLogJson miRunLog)
    , ("capabilities", jsonStrArr miCapabilities)
    , ("metadata", metadataEmpty)
    ]
  where
    -- Compiler-sourced build metadata. Distinct from the top-level
    -- user-sourced @metadata@ slot. Future additions (hash, host, user,
    -- system, source_hash, ...) go directly in this object.
    buildJson :: Text
    buildJson =
      jsonObj
        [ ("path", jsonStr (MT.pack miBuildDir))
        , ("time", jsonInt miBuildTime)
        , ("morloc_version", jsonStr (MT.pack Morloc.Version.versionStr))
        ]

    poolJson :: (Lang, Socket) -> Text
    poolJson (lang, _) =
      jsonObj
        [ ("lang", jsonStr (ML.showLangName lang))
        , ("exec", jsonStrArr (map MT.pack (makeExecArgs lang)))
        , ("socket", jsonStr ("pipe-" <> ML.showLangName lang))
        -- Per-pool source fingerprint. The compiler emits a placeholder
        -- token here and overwrites it after the pool sources are
        -- rendered ('Morloc.patchManifestPoolHashes'). The runtime nexus
        -- exports the resolved hex via @MORLOC_POOL_HASH@ to each pool
        -- on spawn; the pool's cache wrap mixes it into every cache key
        -- so a source edit invalidates stale entries.
        , ("pool_hash", jsonStr ("<MORLOC_POOL_HASH:" <> ML.showLangName lang <> ">"))
        , ("allow_string_null",
            jsonBool (LR.registryAllowStringNull miRegistry (ML.langName lang)))
        , ("metadata", metadataEmpty)
        ]

    makeExecArgs :: Lang -> [String]
    makeExecArgs lang =
      let name = ML.langName lang
          isCompiled = LR.registryIsCompiled miRegistry name
          runCmd = case Map.lookup name (MC.configLangOverrides miConfig) of
            Just cmd -> map MT.unpack cmd
            Nothing -> map MT.unpack (LR.registryRunCommand miRegistry name)
          poolExe = miBuildDir </> "pools" </> miProgramName </> ML.makeExecutablePoolName lang
       in if isCompiled
            then [poolExe]
            else
              if null runCmd
                then [MT.unpack name, poolExe]
                else runCmd ++ [poolExe]

    groupJson :: (Text, [Text]) -> Text
    groupJson (gname, desc) =
      jsonObj
        [ ("name", jsonStr gname)
        , ("desc", jsonStrArr desc)
        , ("metadata", metadataEmpty)
        ]

    -- Pre-rendered run-scope templates. The nexus does the final
    -- runtime placeholder substitution at start/end of the run; the
    -- string here already has {module}/{version}/{morloc_version}/color
    -- placeholders resolved.
    runLogJson :: Maybe RenderedRunLog -> Text
    runLogJson Nothing = jsonNull
    runLogJson (Just rl) =
      jsonObj
        [ ("prologue",     maybe jsonNull jsonStr (renderedPrologue rl))
        , ("epilogue_ok",   maybe jsonNull jsonStr (renderedEpilogueOk rl))
        , ("epilogue_fail", maybe jsonNull jsonStr (renderedEpilogueFail rl))
        ]

    -- Emit a real JSON null when the command has no group, not the
    -- literal string "null". Consumers (notably Rust serde) treat the
    -- two differently: a real null deserializes to None, while a
    -- string "null" used to require a custom deserializer that has
    -- since been dropped.
    -- Look up by manifold ID rather than subcommand name, since the
    -- subcommand may be renamed via --' name: docstrings.
    cmdGroupField :: Int -> (Text, Text)
    cmdGroupField mid = case Map.lookup mid miIndexToGroup of
      Just gname -> ("group", jsonStr gname)
      Nothing -> ("group", jsonNull)

    remoteCmdJson :: FData -> Text
    remoteCmdJson fd =
      jsonObj
        [ ("name", jsonStr (fdataSubcommand fd))
        , ("type", jsonStr "remote")
        , ("mid", jsonInt (fdataMid fd))
        , ("pool", jsonInt (miLangToPool (socketLang (fdataSocket fd))))
        , ("needed_pools", jsonArr (map (jsonInt . miLangToPool . socketLang) (fdataSubSockets fd)))
        , ("desc", jsonStrArr (cmdDocDesc (fdataCmdDocSet fd)))
        , ("args", argsJson (cmdDocArgs (fdataCmdDocSet fd)) (fdataArgSchemas fd))
        , ("return", returnJson (fdataReturnSchema fd) (fdataType fd) (snd (cmdDocRet (fdataCmdDocSet fd))))
        , ("constraints", jsonArr [])
        , ("metadata", metadataEmpty)
        , cmdGroupField (fdataMid fd)
        ]

    pureCmdJson :: GastData -> Text
    pureCmdJson g =
      jsonObj
        [ ("name", jsonStr (commandName g))
        , ("type", jsonStr "pure")
        , ("desc", jsonStrArr (cmdDocDesc (commandDocs g)))
        , ("args", argsJson (cmdDocArgs (commandDocs g)) (commandArgSchemas g))
        , ("return", returnJson (commandReturnSchema g) (commandType g) (snd (cmdDocRet (commandDocs g))))
        , ("expr", exprToJson (commandExpr g))
        , ("constraints", jsonArr [])
        , ("metadata", metadataEmpty)
        , cmdGroupField (commandMid g)
        ]

    -- Render the @args@ JSON array. 'makeSerialASTs' produces one
    -- SerialAST per arg position in the original function signature,
    -- INCLUDING flags; the rendered schemas in 'fdataArgSchemas' are
    -- index-aligned 1:1 with 'docArgs'.
    -- For each arg we attach the corresponding schema; flags drop
    -- their schema in the JSON output (it's never used at dispatch
    -- time for boolean flags) but we still consume the schema slot to
    -- keep the index alignment intact for subsequent args.
    argsJson :: [CmdArg] -> [Text] -> Text
    argsJson docArgs schemas =
      jsonArr (pairArgsWithSchemas docArgs schemas)
      where
        pairArgsWithSchemas :: [CmdArg] -> [Text] -> [Text]
        pairArgsWithSchemas [] _ = []
        -- Flags consume a schema slot but emit no `schema` field.
        pairArgsWithSchemas (a@(CmdArgFlag _) : rest) (_ : ss) =
          argToJson Nothing a : pairArgsWithSchemas rest ss
        pairArgsWithSchemas (a : rest) (s : ss) =
          argToJson (Just s) a : pairArgsWithSchemas rest ss
        pairArgsWithSchemas (a : rest) [] =
          -- Defensive: more args than schemas. Emit with no schema
          -- so we fail cleanly downstream rather than silently
          -- misaligning.
          argToJson Nothing a : pairArgsWithSchemas rest []

    -- Nested @return@ object replacing v1's flat @return_schema@ /
    -- @return_type@ / @return_desc@. Also carries @constraints@ and
    -- @metadata@ for symmetry with args.
    returnJson :: Text -> Type -> [Text] -> Text
    returnJson schema t desc =
      let retT = stripThunks (returnTypeOnly t)
      in jsonObj
        [ ("schema", jsonStr schema)
        , ("type", jsonStr (render (pretty retT)))
        , ("desc", jsonStrArr desc)
        , ("constraints", constraintsJsonFor retT)
        , ("metadata", metadataEmpty)
        ]

    -- Extract the return type from a function type; pass other types
    -- through unchanged.
    returnTypeOnly :: Type -> Type
    returnTypeOnly (FunT _ t) = t
    returnTypeOnly t          = t

    stripThunks :: Type -> Type
    stripThunks (EffectT _ t') = stripThunks t'
    stripThunks t' = t'

-- ======================================================================
-- Main entry point
-- ======================================================================

generate ::
  [(AnnoS (Indexed Type) One (), CmdDocSet)] ->
  [(AnnoS (Indexed Type) One (Indexed Lang), CmdDocSet)] ->
  MorlocMonad Script
generate cs rASTs = do
  config <- MM.ask
  st <- CMS.get

  -- Extract data for remote commands
  xs <- mapM makeFData rASTs
  fdata <- CM.mapM getFData xs

  -- Extract data for pure commands
  gasts <- mapM annotateGasts cs

  -- Get build time and compute build directory
  buildTime <- liftIO $ floor <$> Time.getPOSIXTime
  programName <- MM.getModuleName
  -- In eval mode the source module is always synthesized as `main`, so a
  -- shared `exe/main` directory would collide across distinct `--save NAME`
  -- invocations. Use the outfile name (which carries --save NAME) to give
  -- each saved eval program its own install directory.
  installName <-
    if stateEvalMode st
      then MM.getOutfileName
      else return programName
  buildDir <-
    if stateInstall st
      then do
        let installDir = configHome config </> "exe" </> installName
        CMS.modify (\s -> s {stateInstallDir = Just installDir})
        return installDir
      else liftIO Dir.getCurrentDirectory

  -- Build pool list (deduplicated by language)
  let allSockets = concatMap (\x -> fdataSocket x : fdataSubSockets x) fdata
      daemonSets = uniqueFst [(socketLang s, s) | s <- allSockets]

      langToPoolIndex :: Lang -> Int
      langToPoolIndex lang =
        case findIndex ((== lang) . fst) daemonSets of
          Just idx -> idx
          Nothing -> error $ "Pool not found for language: " <> show lang

  -- Build manifest JSON with relative pool paths
  outfileName <- MM.getOutfileName
  registry <- MM.gets stateLangRegistry

  -- Build group info for manifest
  exportGroups <- MM.gets stateExportGroups
  let indexToGroup =
        Map.fromList
          [ (idx, gname)
          | (gname, (_, indices)) <- Map.toList exportGroups
          , idx <- indices
          ]
      groupDescs =
        Map.fromList
          [ (gname, desc)
          | (gname, (desc, _)) <- Map.toList exportGroups
          ]

  moduleDoc <- MM.gets stateModuleDoc
  moduleEpilogues <- MM.gets stateModuleEpilogues
  unsafeSkipNullCheck <- MM.gets stateUnsafeSkipNullCheck
  inlineSize <- MM.gets stateInlineSize
  noShm <- MM.gets stateNoShm
  tmpdir <- MM.gets stateTmpdir
  runLog <- renderRunLogTemplate
  debugTrace <- MM.gets stateDebugTrace
  -- Capability strings advertise optional codegen features baked into
  -- the binary. The nexus's mode-specific clap parser reveals flag
  -- groups keyed off these strings; a binary without a capability
  -- literally cannot accept (or display) the corresponding flags.
  -- @log@ is always present; @debug_trace@ is set when @morloc make
  -- --debug@ was used. Future strings (@slurm@, @sanitizer@,
  -- @profile@) accumulate the same way.
  let capabilities = "log" : ["debug_trace" | debugTrace]

  let manifestJson =
        buildManifest
          ManifestInputs
            { miConfig              = config
            , miRegistry            = registry
            , miProgramName         = programName
            , miBuildDir            = buildDir
            , miBuildTime           = buildTime
            , miDaemonSets          = daemonSets
            , miFData               = fdata
            , miGasts               = gasts
            , miLangToPool          = langToPoolIndex
            , miIndexToGroup        = indexToGroup
            , miGroupDescs          = groupDescs
            , miModuleDoc           = moduleDoc
            , miModuleEpilogues     = moduleEpilogues
            , miUnsafeSkipNullCheck = unsafeSkipNullCheck
            , miInlineSize          = inlineSize
            , miNoShm               = noShm
            , miTmpdir              = tmpdir
            , miRunLog              = runLog
            , miCapabilities        = capabilities
            }
      wrapperScript = makeWrapperScript manifestJson

  return $
    Script
      { scriptBase = outfileName
      , scriptLang = cLang
      , scriptCode = "." :/ File outfileName (Code wrapperScript)
      , scriptMake = [SysExe outfileName]
      }

-- Build a self-contained wrapper script with embedded manifest.
--
-- The `# morloc-program v<version>` sentinel comment is the marker
-- `morloc-nexus daemon` uses to confirm a target path is a morloc
-- wrapper before resolving the sibling `<target>.manifest`. The
-- exec line hardcodes the `run` subcommand so the wrapper artifact
-- can never be (mis)used to start a daemon directly -- daemons are
-- launched through an explicit `morloc-nexus daemon <target>`.
makeWrapperScript :: Text -> Text
makeWrapperScript manifestJson =
  "#!/bin/sh\n# morloc-program v"
    <> MT.pack Morloc.Version.versionStr
    <> "\nexec morloc-nexus run \"$0\" \"$@\"\n### MANIFEST ###\n"
    <> manifestJson

-- ======================================================================
-- Utilities
-- ======================================================================

uniqueFst :: (Eq a) => [(a, b)] -> [(a, b)]
uniqueFst = f []
  where
    f _ [] = []
    f seen (x@(a, _) : xs)
      | a `elem` seen = f seen xs
      | otherwise = x : f (a : seen) xs
