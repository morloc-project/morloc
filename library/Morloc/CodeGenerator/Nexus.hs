{-# LANGUAGE OverloadedStrings #-}
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
import Data.Int (Int64)
import qualified Data.Map as Map
import qualified Data.Scientific as DS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as MT
import qualified Data.Time.Clock
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Format
import qualified Morloc.BaseTypes as MBT
import qualified Morloc.CodeGenerator.Infer as Infer
import Morloc.CodeGenerator.LogTemplate (RenderedRunLog (..), renderRunLogTemplate)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.CodeGenerator.Serial as Serial
import qualified Morloc.Config as MC
import Morloc.Data.Doc (pretty, render, (<+>))
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
  | SaveX Text Text NexusExpr NexusExpr  -- format + schema + value + path -> ()
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
  (argSchemas, returnSchema) <- makeSchemas i lang t

  case mayName of
    (Just name') -> do
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
          , fdataArgSchemas = map render argSchemas
          , fdataReturnSchema = render returnSchema
          , fdataCmdDocSet = doc
          }
    Nothing -> MM.throwSourcedError i $ "No name in FData"

-- ======================================================================
-- Schema building
-- ======================================================================

makeSchemas :: Int -> Lang -> Type -> MorlocMonad ([MDoc], MDoc)
makeSchemas mid lang (FunT ts t) = do
  ss <- mapM (makeSchema mid lang) ts
  s <- makeSchema mid lang t
  return (ss, s)
makeSchemas mid lang t = do
  s <- makeSchema mid lang t
  return ([], s)

makeSchema :: Int -> Lang -> Type -> MorlocMonad MDoc
makeSchema mid lang t = do
  ft <- Infer.inferConcreteTypeUniversal lang t
  ast <- Serial.makeSerialAST mid lang ft
  -- Apply nat dimension constraints from the original type to the SerialAST.
  -- The TypeF may have lost nat params during alias expansion, but the
  -- original Type still has them.
  let ast' = applyNatDimsFromType t ast
  return $ Serial.serialAstToMsgpackSchema ast'

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

makeGastSchemas :: Type -> MorlocMonad (MDoc, [MDoc])
makeGastSchemas (FunT ts t) = do
  serialAsts <- zipWith applyNatDimsFromType (t : ts) <$> mapM generalTypeToSerialAST (t : ts)
  case map Serial.serialAstToMsgpackSchema serialAsts of
    (s : ss) -> return (s, ss)
    [] -> error "makeGastSchemas: FunT produced empty serial AST list"
makeGastSchemas t = do
  s <- Serial.serialAstToMsgpackSchema . applyNatDimsFromType t <$> generalTypeToSerialAST t
  return (s, [])

-- | Build a SerialAST for a general (nexus-side) type. The Set
-- parameter tracks alias names currently being expanded so a
-- recursive alias (e.g. @type Pair a = (a, ?(Pair a))@) emits a
-- @SerialRec@ back-reference instead of recursing into itself
-- forever. Mirrors @stateSerialAncestors@ in @Serial.makeSerialAST'@
-- on the pool side -- the nexus needs the same protection because
-- top-level constant bindings whose RHS is a recursive-type literal
-- route through @annotateGasts@ here rather than through a pool.
generalTypeToSerialAST :: Type -> MorlocMonad SerialAST
generalTypeToSerialAST = generalTypeToSerialAST' Set.empty

generalTypeToSerialAST' :: Set TVar -> Type -> MorlocMonad SerialAST
generalTypeToSerialAST' anc (VarT v)
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
          inner <- generalTypeToSerialAST' (Set.insert v anc) (typeOf t')
          return $ retagOuterName v inner
        (Just [_]) -> error $ "Cannot currently handle parameterized pure morloc types"
        Nothing -> error $ "Failed to interpret type variable: " <> show (unTVar v)
        x -> error $ "Unexpected scope: " <> show x
generalTypeToSerialAST' anc (AppT (VarT v) [t])
  | v == MBT.list = SerialList (FV v (CV "")) Nothing <$> generalTypeToSerialAST' anc t
  | otherwise = resolveAliasApp anc v [t]
generalTypeToSerialAST' anc (AppT (VarT v) ts)
  | v == (MBT.tuple (length ts)) =
      SerialTuple (FV v (CV "")) <$> mapM (generalTypeToSerialAST' anc) ts
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
           <$> mapM (secondM (generalTypeToSerialAST' anc)) cols
  | otherwise = resolveAliasApp anc v ts
generalTypeToSerialAST' anc (EffectT _ t) = generalTypeToSerialAST' anc t
generalTypeToSerialAST' anc (OptionalT t) = do
  inner <- generalTypeToSerialAST' anc t
  return $ SerialOptional (FV (TV "Optional") (CV "")) inner
generalTypeToSerialAST' anc (NamT o v _ rs) =
  -- Add @v@ to the ancestor set before recursing into the record's
  -- fields: a recursive record (e.g. @record Tree where children :: [Tree]@)
  -- has a field whose type mentions @Tree@ again, which would otherwise
  -- expand back into the same NamT and loop. Parameter types are
  -- already substituted into the field types @rs@ by this point, so
  -- they do not need to appear in the resulting SerialAST.
  let anc' = Set.insert v anc
  in SerialObject o (FV v (CV "")) []
       <$> mapM (secondM (generalTypeToSerialAST' anc')) rs
generalTypeToSerialAST' _ t = MM.throwSystemError $ "cannot serialize this type: " <> pretty (show t)

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

-- | True if the export type carries a function type in argument or return
-- position. The top-level FunT of an exported function itself is fine; what
-- isn't fine is any nested FunT (HOF) embedded inside arguments or returns.
exportHasHigherOrder :: Type -> Bool
exportHasHigherOrder (FunT ts ret) = any containsFunT ts || containsFunT ret
exportHasHigherOrder t = containsFunT t

resolveAliasApp :: Set TVar -> TVar -> [Type] -> MorlocMonad SerialAST
resolveAliasApp anc v ts
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
          inner <- generalTypeToSerialAST' anc' resolved
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
        _ -> MM.throwSystemError $ "Cannot serialize type: " <> pretty (show (AppT (VarT v) ts))

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

  CM.when (exportHasHigherOrder gtype) $
    MM.throwSourcedError i "cannot export higher-order functions through the CLI"

  (retSchemaDoc, argSchemaDocs) <- makeGastSchemas gtype
  expr <- toNexusExpr x0

  return $
    GastData
      { commandName = maybe (unEVar gname) id (cmdDocName docs)
      , commandMid = i
      , commandType = gtype
      , commandDocs = docs
      , commandExpr = expr
      , commandReturnSchema = render retSchemaDoc
      , commandArgSchemas = map render argSchemaDocs
      }
  where
    type2schema :: Type -> MorlocMonad Text
    type2schema t = (render . Serial.serialAstToMsgpackSchema) <$> generalTypeToSerialAST t

    toNexusExpr :: AnnoS (Indexed Type) One () -> MorlocMonad NexusExpr
    -- Pure-path bracket validation: each non-Null bound must resolve
    -- to a wire integer type. There is no per-language IndexLike
    -- instance to fall back on in the nexus runtime, so anything
    -- non-integral is rejected at codegen with a clear message.
    -- @(Null :: ?Int64)@ (from the desugar's empty-bound annotation),
    -- bare integer literals, and explicit @(_ :: Int*)@ annotations
    -- all pass.
    toNexusExpr (AnnoS (Idx _ t) _ (AppS funcE@(AnnoS _ _ (ExeS (PatCall PatternBracketSlice))) [sE, eE, pE, rE])) = do
      validateBracketBound "start" sE
      validateBracketBound "stop"  eE
      validateBracketBound "step"  pE
      AppX <$> type2schema t <*> toNexusExpr funcE <*> mapM toNexusExpr [sE, eE, pE, rE]
    toNexusExpr (AnnoS (Idx _ t) _ (AppS funcE@(AnnoS _ _ (ExeS (PatCall PatternBracketIndex))) [iE, rE])) = do
      validateBracketBound "index" iE
      AppX <$> type2schema t <*> toNexusExpr funcE <*> mapM toNexusExpr [iE, rE]
    toNexusExpr (AnnoS (Idx _ t) _ (AppS e es)) = AppX <$> type2schema t <*> toNexusExpr e <*> mapM toNexusExpr es
    toNexusExpr (AnnoS _ _ (LamS vs e)) = LamX (map (render . pretty) vs) <$> toNexusExpr e
    toNexusExpr (AnnoS (Idx _ (FunT _ t)) _ (ExeS (PatCall p))) = PatX <$> type2schema t <*> pure p
    toNexusExpr (AnnoS (Idx _ t) _ (BndS v)) = BndX <$> type2schema t <*> pure (render (pretty v))
    toNexusExpr (AnnoS (Idx _ t) _ (LstS es)) = LstX <$> type2schema t <*> mapM toNexusExpr es
    toNexusExpr (AnnoS (Idx _ t) _ (TupS es)) = TupX <$> type2schema t <*> mapM toNexusExpr es
    toNexusExpr (AnnoS (Idx _ t) _ (NamS rs)) =
      NamX <$> type2schema t <*> mapM (\(k, e) -> (,) (unKey k) <$> toNexusExpr e) rs
    toNexusExpr (AnnoS (Idx _ t) _ (StrS v)) = StrX <$> type2schema t <*> pure v
    -- Use the literal's own source-map index (litIx) for overflow reporting,
    -- mirroring the IntS path. RealS carries litIx in its first parameter.
    -- Non-finite variants (Inf/-Inf/NaN) bypass bounds-checking and emit
    -- the canonical wire string ("inf" / "-inf" / "nan"); the runtime JSON
    -- decoder accepts these on parse.
    toNexusExpr (AnnoS (Idx _ t) _ (RealS litIx v)) = do
      s <- generalTypeToSerialAST t
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
      s <- generalTypeToSerialAST t
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
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrSave [valExpr, path])) =
      SaveX "voidstar" <$> type2schema t <*> toNexusExpr valExpr <*> toNexusExpr path
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrSaveM [valExpr, path])) =
      SaveX "msgpack" <$> type2schema t <*> toNexusExpr valExpr <*> toNexusExpr path
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrSaveJ [valExpr, path])) =
      SaveX "json" <$> type2schema t <*> toNexusExpr valExpr <*> toNexusExpr path
    toNexusExpr (AnnoS (Idx _ t) _ (IntrinsicS IntrLoad [path])) =
      LoadX <$> type2schema t <*> toNexusExpr path
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
    ++ [ ("type", jsonStr (typeDescStr (argPosDocType r) (argPosDocLiteral r)))
       , ("metavar", jsonMaybeStr (argPosDocMetavar r))
       , ("quoted", jsonBool (argPosDocLiteral r == Just True && isStrType (argPosDocType r)))
       , ("desc", jsonStrArr (argPosDocDesc r))
       , ("constraints", constraintsJsonFor (argPosDocType r))
       , ("metadata", metadataEmpty)
       ]
argToJson mschema (CmdArgOpt r) =
  jsonObj $
    [ ("kind", jsonStr "opt") ]
    ++ schemaField mschema
    ++ [ ("type", jsonStr (typeDescStr (argOptDocType r) (argOptDocLiteral r)))
       , ("metavar", jsonStr (argOptDocMetavar r))
       , ("quoted", jsonBool (argOptDocLiteral r == Just True && isStrType (argOptDocType r)))
       , ("short", cliOptShortJson (argOptDocArg r))
       , ("long", cliOptLongJson (argOptDocArg r))
       , ("default", jsonStr (argOptDocDefault r))
       , ("desc", jsonStrArr (argOptDocDesc r))
       , ("constraints", constraintsJsonFor (argOptDocType r))
       , ("metadata", metadataEmpty)
       ]
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

-- Check if a type is Str or ?Str (for literal string handling)
isStrType :: Type -> Bool
isStrType (VarT v) = v == MBT.str
isStrType (OptionalT t) = isStrType t
isStrType _ = False

typeDescStr :: Type -> Maybe Bool -> Text
typeDescStr t isLiteral
  | isStrType t && isLiteral /= Just True = "Str    (a filename or quoted JSON string)"
  | otherwise = render (pretty t)

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
exprToJson (SaveX fmt schema value path) =
  jsonObj
    [ ("tag", jsonStr "save")
    , ("format", jsonStr fmt)
    , ("schema", jsonStr schema)
    , ("value", exprToJson value)
    , ("path", exprToJson path)
    ]
exprToJson (LoadX schema child) =
  jsonObj
    [ ("tag", jsonStr "load")
    , ("schema", jsonStr schema)
    , ("child", exprToJson child)
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

buildManifest ::
  Config ->
  LangRegistry ->
  String ->
  String ->
  Int ->
  [(Lang, Socket)] ->
  [FData] ->
  [GastData] ->
  (Lang -> Int) ->
  Map.Map Int Text ->
  Map.Map Text [Text] ->
  [Text] ->
  [[Text]] ->
  Bool ->
  Maybe Int64 ->
  Bool ->
  Maybe Path ->
  Maybe RenderedRunLog ->
  Text
buildManifest config registry programName buildDir buildTime daemonSets fdata gasts langToPool indexToGroup groupDescs moduleDoc moduleEpilogues unsafeSkipNullCheck inlineSize noShm tmpdir runLog =
  jsonObj
    [ ("name", jsonStr (MT.pack programName))
    , ("build", buildJson)
    , ("pools", jsonArr (map poolJson daemonSets))
    , ("commands", jsonArr (map remoteCmdJson fdata ++ map pureCmdJson gasts))
    , ("groups", jsonArr (map groupJson (Map.toList groupDescs)))
    , ("desc", jsonStrArr moduleDoc)
    , ("epilogues", jsonArr (map jsonStrArr moduleEpilogues))
    , ("unsafe_skip_null_check", jsonBool unsafeSkipNullCheck)
    , ("inline_size", maybe jsonNull jsonInt64 inlineSize)
    , ("no_shm", jsonBool noShm)
    , ("tmpdir", maybe jsonNull (jsonStr . MT.pack) tmpdir)
    , ("run_log", runLogJson runLog)
    , ("metadata", metadataEmpty)
    ]
  where
    -- Compiler-sourced build metadata. Distinct from the top-level
    -- user-sourced @metadata@ slot. Future additions (hash, host, user,
    -- system, source_hash, ...) go directly in this object.
    buildJson :: Text
    buildJson =
      jsonObj
        [ ("path", jsonStr (MT.pack buildDir))
        , ("time", jsonInt buildTime)
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
            jsonBool (LR.registryAllowStringNull registry (ML.langName lang)))
        , ("metadata", metadataEmpty)
        ]

    makeExecArgs :: Lang -> [String]
    makeExecArgs lang =
      let name = ML.langName lang
          isCompiled = LR.registryIsCompiled registry name
          runCmd = case Map.lookup name (MC.configLangOverrides config) of
            Just cmd -> map MT.unpack cmd
            Nothing -> map MT.unpack (LR.registryRunCommand registry name)
          poolExe = buildDir </> "pools" </> programName </> ML.makeExecutablePoolName lang
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
    cmdGroupField mid = case Map.lookup mid indexToGroup of
      Just gname -> ("group", jsonStr gname)
      Nothing -> ("group", jsonNull)

    remoteCmdJson :: FData -> Text
    remoteCmdJson fd =
      jsonObj
        [ ("name", jsonStr (fdataSubcommand fd))
        , ("type", jsonStr "remote")
        , ("mid", jsonInt (fdataMid fd))
        , ("pool", jsonInt (langToPool (socketLang (fdataSocket fd))))
        , ("needed_pools", jsonArr (map (jsonInt . langToPool . socketLang) (fdataSubSockets fd)))
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

    -- Render the @args@ JSON array. 'makeSchemas' produces one schema
    -- per arg position in the original function signature, INCLUDING
    -- flags. So 'fdataArgSchemas' is index-aligned 1:1 with 'docArgs'.
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

  let manifestJson =
        buildManifest
          config
          registry
          programName
          buildDir
          buildTime
          daemonSets
          fdata
          gasts
          langToPoolIndex
          indexToGroup
          groupDescs
          moduleDoc
          moduleEpilogues
          unsafeSkipNullCheck
          inlineSize
          noShm
          tmpdir
          runLog
      wrapperScript = makeWrapperScript manifestJson

  return $
    Script
      { scriptBase = outfileName
      , scriptLang = cLang
      , scriptCode = "." :/ File outfileName (Code wrapperScript)
      , scriptMake = [SysExe outfileName]
      }

-- Build a self-contained wrapper script with embedded manifest
makeWrapperScript :: Text -> Text
makeWrapperScript manifestJson =
  "#!/bin/sh\nexec morloc-nexus \"$0\" \"$@\"\n### MANIFEST ###\n" <> manifestJson

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
