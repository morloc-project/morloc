{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Guest.Futhark
Description : Futhark guest: build, signature check, and C++ glue generation
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The Futhark guest. Futhark source is compiled to a C library and called from
generated C++ glue in the cpp pool. This module provides the 'Guest' value:

  * 'guestBuild' runs the Futhark compiler to produce a C object, header, and
    manifest.
  * 'guestCheck' parses the manifest and validates each sourced morloc signature
    against the corresponding entry point (element type, array rank, and tuple
    structure).
  * 'guestGlue' renders the C++ glue that bridges native host tensors and tuples
    to the Futhark C library. It is pure: the host translator renders the argument
    and return types and passes them in through 'GlueEntry'.

The build recipe, element-type map, and ABI symbol scheme live here (not in
lang.yaml) because they carry Futhark-specific semantics.

Tuples cross the boundary as Futhark "opaque" values. Futhark does not flatten a
tuple argument or return; it stays a single input/output whose manifest type is an
opaque record. The manifest 'types' block names the constructor, per-field
projectors, and free function, all read verbatim (never reconstructed). The glue
builds an opaque from its marshalled fields on the way in and projects each field
back out on the way out.
-}
module Morloc.CodeGenerator.Guest.Futhark
  ( futharkGuest
  , hostEntryTypes
  , hostTypeOfFT
  , FutharkEntry (..)
  , FutharkType (..)
  , FNum (..)
  , TupleOps (..)
  , decomposeSig
  , resolveFutharkBuild
  , backendLinkFlags
  , futharkBackends
  ) where

import Morloc.Data.Zip (indexed, zipWithSameM_)
import qualified Morloc.BaseTypes as BT
import Data.Aeson (FromJSON (..), eitherDecodeFileStrict', withObject, (.!=), (.:), (.:?))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeBaseName, takeFileName, (<.>))

import Morloc.Build.Params (LangParams)
import qualified Morloc.Build.Params as BP
import Morloc.CodeGenerator.Guest
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc (pretty, render)
import qualified Morloc.Monad as MM
import qualified Morloc.CodeGenerator.Platform as P
import Morloc.ProgramBuilder.Build (requireStrictToolchain)
import qualified Morloc.System as MS

-- ---------------------------------------------------------------------------
-- Parsed manifest entry
-- ---------------------------------------------------------------------------

-- | A fixed-width numeric element at an array rank (0 = scalar, 1 = vector,
-- 2 = matrix, ...).
data FNum = FNum Text Int
  deriving (Show, Eq)

-- | A Futhark ABI type: a number, or a non-nested tuple of numbers. A tuple
-- read from the manifest carries its C ABI functions ('TupleOps') and, with
-- each field, the field's projector ('AbiFT'); one implied by a morloc
-- signature carries nothing ('ShapeFT').
data FutharkType t p
  = FTNum FNum
  | FTTuple t [(p, FNum)]
  deriving (Show)

type ShapeFT = FutharkType () ()

type AbiFT = FutharkType TupleOps Text

-- | The opaque-record C ABI for one tuple type, read from the manifest.
data TupleOps = TupleOps
  { tiCtype :: Text -- ^ e.g. "struct futhark_opaque_tup2_f32_f32 *" (includes '*')
  , tiNew :: Text -- ^ constructor, e.g. "futhark_new_opaque_tup2_f32_f32"
  , tiFree :: Text -- ^ "futhark_free_opaque_tup2_f32_f32"
  }
  deriving (Show)

-- | Equality of shape, ignoring what each side knows of the C ABI.
sameShape :: FutharkType a b -> FutharkType c d -> Bool
sameShape (FTNum x) (FTNum y) = x == y
sameShape (FTTuple _ as) (FTTuple _ bs) = map snd as == map snd bs
sameShape _ _ = False

-- | True when this shape lowers to an @mlc::Tensor@ (rank>=2), i.e. it needs
-- the tensor.hpp include.
usesTensor :: FutharkType t p -> Bool
usesTensor (FTNum (FNum _ r)) = r >= 2
usesTensor (FTTuple _ fs) = any (\(_, FNum _ r) -> r >= 2) fs

-- | One entry point parsed from the Futhark manifest.
data FutharkEntry = FutharkEntry
  { feName :: Text
  , feCFun :: Text
  , feInputs :: [AbiFT]
  , feOutput :: AbiFT
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Manifest JSON parsing (only the fields we need)
-- ---------------------------------------------------------------------------

-- Manifest shape (the parts we read):
--   { "entry_points": { "<name>": { "cfun":..., "inputs":[{"type":...}], "output":{"type":...} } }
--   , "types":        { "<type>": { "kind":..., "ctype":..., "ops":{"free":...}
--                                 , "record":{"new":..., "fields":[{"name","project","type"}]} } } }
-- The entry "output" is singular even for a tuple return: futhark 0.26.4 keeps it
-- a single opaque value. If a future version emits multiple/renamed outputs the
-- ".: output" decode fails cleanly rather than misparsing.
data Manifest = Manifest (Map Text RawEntry) (Map Text RawType)

data RawEntry = RawEntry Text [Text] Text -- cfun, input type strings, output type string

data RawType = RawType
  { rtKind :: Text
  , rtCtype :: Maybe Text
  , rtFree :: Maybe Text
  , rtRecord :: Maybe RawRecord
  , rtRecordArray :: Bool
  }

data RawRecord = RawRecord Text [RawField] -- new fn, fields

data RawField = RawField Text Text Text -- name, project fn, type string

newtype TypeField = TypeField {unTypeField :: Text}

-- reads just the "free" out of the "ops" object
newtype OpsFree = OpsFree (Maybe Text)

-- marks the mere presence of a JSON key (its shape is irrelevant here)
newtype Present = Present ()

instance FromJSON TypeField where
  parseJSON = withObject "typed" $ \v -> TypeField <$> v .: "type"

instance FromJSON OpsFree where
  parseJSON = withObject "ops" $ \v -> OpsFree <$> v .:? "free"

instance FromJSON Present where
  parseJSON _ = pure (Present ())

instance FromJSON RawEntry where
  parseJSON = withObject "entry_point" $ \v -> do
    cfun <- v .: "cfun"
    ins <- v .: "inputs"
    out <- v .: "output"
    pure (RawEntry cfun (map unTypeField ins) (unTypeField out))

instance FromJSON RawField where
  parseJSON = withObject "record field" $ \v ->
    RawField <$> v .: "name" <*> v .: "project" <*> v .: "type"

instance FromJSON RawRecord where
  parseJSON = withObject "record" $ \v ->
    RawRecord <$> v .: "new" <*> v .: "fields"

instance FromJSON RawType where
  parseJSON = withObject "type" $ \v -> do
    kind <- v .: "kind"
    ct <- v .:? "ctype"
    mops <- v .:? "ops"
    rec' <- v .:? "record"
    mra <- v .:? "record_array"
    let freeName = case mops of
          Just (OpsFree f) -> f
          Nothing -> Nothing
        hasRA = maybe False (\(Present _) -> True) mra
    pure (RawType kind ct freeName rec' hasRA)

instance FromJSON Manifest where
  parseJSON = withObject "manifest" $ \v ->
    Manifest <$> v .: "entry_points" <*> (v .:? "types" .!= Map.empty)

-- ---------------------------------------------------------------------------
-- Type maps (Futhark-specific ABI semantics)
-- ---------------------------------------------------------------------------

-- The single source of truth: morloc concrete numeric constructor ->
-- (futhark element token, host C++ scalar type). Everything else is derived.
numericTypes :: Map Text (Text, Text)
numericTypes =
  Map.fromList
    [ ("F32", ("f32", "float")), ("F64", ("f64", "double"))
    , ("I8", ("i8", "int8_t")), ("I16", ("i16", "int16_t"))
    , ("I32", ("i32", "int32_t")), ("I64", ("i64", "int64_t"))
    , ("U8", ("u8", "uint8_t")), ("U16", ("u16", "uint16_t"))
    , ("U32", ("u32", "uint32_t")), ("U64", ("u64", "uint64_t"))
    ]

-- morloc constructor -> futhark element token
elemToken :: Map Text Text
elemToken = Map.map fst numericTypes

-- futhark element token -> host C++ scalar type (keyed by token)
cppScalar :: Map Text Text
cppScalar = Map.fromList (Map.elems numericTypes)

-- known futhark element tokens, for manifest validation
elemTokenSet :: Set Text
elemTokenSet = Set.fromList (Map.elems elemToken)

-- ---------------------------------------------------------------------------
-- The guest value
-- ---------------------------------------------------------------------------

futharkLang :: Lang
futharkLang = Lang "futhark" "fut"

futharkGuest :: Guest FutharkEntry
futharkGuest =
  Guest
    { guestLang = futharkLang
    , guestBuild = futharkBuild
    , guestCheck = futharkCheck
    , guestGlue = futharkGlue
    }

-- ---------------------------------------------------------------------------
-- Build: run the Futhark compiler + compile the emitted C
-- ---------------------------------------------------------------------------

futharkBuild :: [GuestSource] -> BuildOpts -> MorlocMonad BuildProducts
futharkBuild srcs opts = do
  let backend = fromMaybe "c" (boBackend opts)
      outDir = boOutDir opts
  requireStrictToolchain CGuestToolchain
  triples <- mapM (buildOne backend outDir) srcs
  let (os, hs, jsons) = unzip3 triples
  pure
    BuildProducts
      { bpObjects = os
      , bpHeaders = hs
      , bpIncludeDirs = [outDir]
      , bpLinkFlags = backendLinkFlags P.hostPlatform backend
      , bpManifests = jsons
      , bpDevice = boDevice opts
      }

-- returns (object, header, manifest) paths
buildOne :: Text -> Path -> GuestSource -> MorlocMonad (Path, Path, Path)
buildOne backend outDir gs = do
  envPrefix <- MM.gets stateEnvPrefix
  let stem = takeBaseName (gsPath gs)
      outStem = outDir </> stem
      cFile = outStem <.> "c"
      hFile = outStem <.> "h"
      jsonFile = outStem <.> "json"
      oFile = outStem <.> "o"
      q = T.pack . MS.shellQuote
      futCmd =
        T.unwords ["futhark", backend, "--library", "-o", q outStem, q (gsPath gs)]
      -- $CC when set: a conda/pixi toolchain names its own compiler, and on
      -- macOS without the command-line tools /usr/bin/gcc is only a stub.
      -- PKG-13: the environment's headers.
      envInclude = maybe [] (\p -> ["-isystem", q (p </> "include")]) envPrefix
      gccCmd = T.unwords (["${CC:-cc}", "-O2"] <> envInclude <> ["-c", q cFile, "-o", q oFile])
  MM.runCommand "Futhark.build" futCmd
  MM.runCommand "Futhark.build" gccCmd
  pure (oFile, hFile, jsonFile)

-- Link flags for each backend's runtime. The GPU cases are best-effort: the
-- exact libraries and their locations vary by CUDA/ROCm install layout and
-- Futhark version, so a user targeting an unusual layout supplements these via
-- @-X cpp:flags=-L/path@ etc.
backendLinkFlags :: P.Platform -> Text -> [Text]
backendLinkFlags _ "multicore" = ["-lpthread", "-lm"]
backendLinkFlags platform "opencl" = case platform of
  -- macOS ships OpenCL only as a framework.
  P.Darwin -> ["-framework", "OpenCL", "-lm"]
  P.Linux -> ["-lOpenCL", "-lm"]
backendLinkFlags _ "cuda" = ["-lcuda", "-lnvrtc", "-lm"]
backendLinkFlags _ "hip" = ["-lamdhip64", "-lhiprtc", "-lm"]
backendLinkFlags _ _ = ["-lm"]

-- | The Futhark compiler backends morloc knows how to build and link.
futharkBackends :: [Text]
futharkBackends = ["c", "multicore", "opencl", "cuda", "hip", "ispc"]

-- | Backends that target a GPU and accept a device selector.
isGpuBackend :: Text -> Bool
isGpuBackend b = b `elem` ["opencl", "cuda", "hip"]

-- | Resolve the futhark backend and device selector from build parameters,
-- validating the values. This is where Futhark's own vocabulary is checked --
-- the morloc UI passes @-X futhark:...@ through untouched. Returns 'Left' with a
-- user-facing message on invalid input.
resolveFutharkBuild :: LangParams -> Either Text (Maybe Text, Maybe Text)
resolveFutharkBuild lp =
  let backend = BP.lookupParam "futhark" "backend" lp
      device = BP.lookupParam "futhark" "device" lp
      resolved = fromMaybe "c" backend
   in if resolved `notElem` futharkBackends
        then
          Left $
            "unknown futhark backend '"
              <> resolved
              <> "'; expected one of "
              <> T.intercalate ", " futharkBackends
        else case device of
          Just _
            | not (isGpuBackend resolved) ->
                Left $
                  "futhark:device requires a GPU backend (opencl, cuda, or hip), but backend is '"
                    <> resolved
                    <> "'"
          _ -> Right (backend, device)

-- ---------------------------------------------------------------------------
-- Check: parse the manifest and validate each morloc signature
-- ---------------------------------------------------------------------------

futharkCheck :: [SourcedSig] -> BuildProducts -> MorlocMonad [(SourcedSig, FutharkEntry)]
futharkCheck sigs products = do
  entries <- loadEntries (bpManifests products)
  mapM (checkOne entries) sigs

loadEntries :: [Path] -> MorlocMonad (Map Text FutharkEntry)
loadEntries paths = Map.unions <$> mapM loadManifest paths

loadManifest :: Path -> MorlocMonad (Map Text FutharkEntry)
loadManifest path = do
  eres <- liftIO (eitherDecodeFileStrict' path)
  case eres of
    Left err ->
      MM.throwFailed ("Futhark: could not read manifest " <> pretty path <> ": " <> pretty err)
    Right (Manifest raws types) -> Map.traverseWithKey (convert types) raws

convert :: Map Text RawType -> Text -> RawEntry -> MorlocMonad FutharkEntry
convert reg name (RawEntry cfun ins out) = do
  fins <- mapM (resolveType name reg) ins
  fout <- resolveType name reg out
  pure (FutharkEntry name cfun fins fout)

badFut :: Text -> MDoc
badFut name = "Futhark entry " <> pretty name <> ": "

-- strip leading "[]" tokens, returning (rank, remaining base string)
stripArray :: Text -> (Int, Text)
stripArray = go 0
  where
    go n s = case T.stripPrefix "[]" s of
      Just rest -> go (n + 1) rest
      Nothing -> (n, s)

-- resolve a manifest type string into a FutharkType. Scalars and arrays are
-- parsed directly; a bare opaque name is looked up in the types registry (a
-- tuple). Arrays of tuples and nested tuples are rejected.
resolveType :: Text -> Map Text RawType -> Text -> MorlocMonad AbiFT
resolveType name reg s =
  let (rank, base) = stripArray s
   in if base `Set.member` elemTokenSet
        then pure (FTNum (FNum base rank))
        else
          if rank == 0
            then resolveOpaque name reg s
            else MM.throwRejected (badFut name <> "arrays of tuples are not supported")

resolveOpaque :: Text -> Map Text RawType -> Text -> MorlocMonad AbiFT
resolveOpaque name reg key = case Map.lookup key reg of
  Nothing -> MM.throwRejected (badFut name <> "unknown opaque type " <> pretty key)
  Just rt
    | rtRecordArray rt -> MM.throwRejected (badFut name <> "arrays of tuples are not supported")
    | rtKind rt /= "opaque" ->
        MM.throwRejected (badFut name <> "unsupported opaque kind " <> pretty (rtKind rt))
    | otherwise -> case (rtRecord rt, rtCtype rt, rtFree rt) of
        (Just (RawRecord newFn fields), Just ct, Just freeFn) -> do
          mapM_ (uncurry (checkFieldLabel name)) (indexed fields)
          pfs <- mapM (\f@(RawField _ p _) -> (,) p <$> resolveField name reg f) fields
          pure (FTTuple (TupleOps ct newFn freeFn) pfs)
        _ -> MM.throwRejected (badFut name <> "opaque type " <> pretty key <> " is not a supported tuple")

-- Futhark labels tuple fields "0","1",...; codegen binds morloc field k to the
-- field labelled (show k). Verify the labels are positional so a future
-- multi-digit tuple (where lexical ordering diverges) fails loud here.
checkFieldLabel :: Text -> Int -> RawField -> MorlocMonad ()
checkFieldLabel name i (RawField lbl _ _) =
  unless (lbl == tshow i) $
    MM.throwRejected
      (badFut name <> "unexpected Futhark tuple field order (field " <> pretty i <> " labelled " <> pretty lbl <> ")")

resolveField :: Text -> Map Text RawType -> RawField -> MorlocMonad FNum
resolveField name reg (RawField _ _ fty) = do
  ft <- resolveType name reg fty
  case ft of
    FTTuple{} -> MM.throwRejected (badFut name <> "nested tuples are not supported")
    FTNum n -> pure n

checkOne :: Map Text FutharkEntry -> SourcedSig -> MorlocMonad (SourcedSig, FutharkEntry)
checkOne entries sig = do
  let entryName = unSrcName (srcName (ssSource sig))
  entry <- case Map.lookup entryName entries of
    Just e -> pure e
    Nothing ->
      MM.throwRejected ("Futhark: no entry point named " <> pretty entryName <> " in the manifest")
  (argTs, retT) <- decomposeSig (ssType sig)
  unless (length argTs == length (feInputs entry)) $
    MM.throwRejected
      ( "Futhark entry " <> pretty entryName <> ": morloc signature has "
          <> pretty (length argTs) <> " arguments but the entry takes "
          <> pretty (length (feInputs entry))
      )
  zipWithSameM_ (compareFT entryName) argTs (feInputs entry)
  compareFT entryName retT (feOutput entry)
  pure (sig, entry)

compareFT :: Text -> ShapeFT -> AbiFT -> MorlocMonad ()
compareFT name expected actual =
  unless (sameShape expected actual) $
    MM.throwRejected
      ( "Futhark entry " <> pretty name <> ": morloc signature implies "
          <> showFT expected <> " but the entry has " <> showFT actual
      )

showFT :: FutharkType t p -> MDoc
showFT (FTNum n) = showNum n
showFT (FTTuple _ fs) = "(" <> joinCommaD (map (showNum . snd) fs) <> ")"

showNum :: FNum -> MDoc
showNum (FNum e r) = pretty e <> " rank-" <> pretty r

-- decompose a morloc signature into the futhark types it implies
decomposeSig :: TypeU -> MorlocMonad ([ShapeFT], ShapeFT)
decomposeSig t0 = do
  let (args, ret) = unfoldFun (stripForall t0)
  fargs <- mapM morlocToFT args
  fret <- morlocToFT ret
  pure (fargs, fret)

stripForall :: TypeU -> TypeU
stripForall (ForallU _ t) = stripForall t
stripForall t = t

unfoldFun :: TypeU -> ([TypeU], TypeU)
unfoldFun (FunU args ret) =
  let (args', ret') = unfoldFun ret in (args ++ args', ret')
unfoldFun t = ([], t)

-- the surface shape of a morloc type, as this guest cares about it
data MShape = MArr Text Int | MTup [TypeU] | MOther

-- Classify a morloc type. Tuples (checked before the array pattern, since
-- both are AppU-headed) become MTup with their field types; a numeric array
-- (`Vector`, `Matrix`, `Tensor3`..`Tensor5`, with as many Nat dimension
-- arguments as its rank) becomes MArr; a bare type name is a rank-0 scalar.
classifyMorloc :: TypeU -> MShape
classifyMorloc (EffectU _ t) = classifyMorloc t
classifyMorloc (AppU (VarU (TV nm)) args)
  | isTupleName nm = MTup (fst (partitionKindArgs kindOfU args))
  | otherwise = case fst (partitionKindArgs kindOfU args) of
      [VarU (TV e)]
        | Just r <- arrayRank nm
        , all (maybe True fillsNat . kindOfU) args
        , length [a | a <- args, maybe False fillsNat (kindOfU a)] == r ->
            MArr e r
      _ -> MOther
  where
    -- The heads the C++ host maps: std::vector, then mlc::Tensor2..5.
    arrayRank n
      | TV n == BT.vector = Just 1
      | TV n == BT.matrix = Just 2
      | otherwise = lookup n [(unTVar (BT.tensor k), k) | k <- [3 .. 5]]
classifyMorloc (VarU (TV e)) = MArr e 0
classifyMorloc _ = MOther

-- map a single morloc argument/return type to its futhark ABI type
morlocToFT :: TypeU -> MorlocMonad ShapeFT
morlocToFT t = case classifyMorloc t of
  MTup fields
    | length fields > 8 -> unsupported t "tuples with more than 8 fields are not supported"
    | otherwise -> FTTuple () <$> mapM (fmap ((,) ()) . morlocToFTField) fields
  _ -> FTNum <$> morlocToFTField t

morlocToFTField :: TypeU -> MorlocMonad FNum
morlocToFTField t = case classifyMorloc t of
  MArr e r
    | Just tok <- Map.lookup e elemToken -> pure (FNum tok r)
  MTup _ -> unsupported t "nested tuples are not supported"
  _ -> unsupportedType t

unsupportedType :: TypeU -> MorlocMonad a
unsupportedType t =
  MM.throwRejected
    ( "Futhark: unsupported type "
        <> pretty t
        <> " (only fixed-width numeric scalars, Vector/Matrix of them, and non-nested tuples of those are allowed)"
    )

unsupported :: TypeU -> Text -> MorlocMonad a
unsupported t msg =
  MM.throwRejected ("Futhark: " <> pretty msg <> " (in type " <> pretty t <> ")")

-- ---------------------------------------------------------------------------
-- Host (C++) type rendering -- the pass calls this to fill GlueEntry.
-- One rule, 'hostTypeOfFT', maps a FutharkType to its C++ host type; the checker
-- and the renderer both flow through it. Path A: the output matches cppTypeOf for
-- the supported set (numeric scalars, Vector/Matrix, and tuples of those).
-- ---------------------------------------------------------------------------

-- futhark element token -> C++ scalar
cppScalar' :: Text -> Text
cppScalar' e = Map.findWithDefault "void" e cppScalar

-- the single rank/shape -> C++ type rule
hostTypeOfFT :: FutharkType t p -> MDoc
hostTypeOfFT (FTNum n) = hostTypeOfNum n
hostTypeOfFT (FTTuple _ fs) = "std::tuple<" <> joinCommaD (map (hostTypeOfNum . snd) fs) <> ">"

hostTypeOfNum :: FNum -> MDoc
hostTypeOfNum (FNum e 0) = pretty (cppScalar' e)
hostTypeOfNum (FNum e 1) = "std::vector<" <> pretty (cppScalar' e) <> ">"
hostTypeOfNum (FNum e r) = "mlc::Tensor" <> pretty r <> "<" <> pretty (cppScalar' e) <> ">"

-- | The host C++ types of an entry's arguments and return, read from the
-- manifest after 'checkOne' has matched them to the signature.
hostEntryTypes :: FutharkEntry -> ([MDoc], MDoc)
hostEntryTypes e = (map hostTypeOfFT (feInputs e), hostTypeOfFT (feOutput e))

-- ---------------------------------------------------------------------------
-- Glue: render the C++ bridge (pure; types are pre-rendered by the pass)
-- ---------------------------------------------------------------------------

futharkGlue :: BuildProducts -> [GlueEntry FutharkEntry] -> HostGlue
futharkGlue products entries =
  HostGlue
    { hgCode = pretty glueText
    , hgHeader = glueHeaderPath
    , hgBindings =
        [ (srcAlias (ssSource (gleSig ge)), SrcName (glueName (gleEntry ge)))
        | ge <- entries
        ]
    }
  where
    glueHeaderPath = case bpIncludeDirs products of
      (d : _) -> d </> "mlc_futhark_glue.hpp"
      [] -> "mlc_futhark_glue.hpp"
    -- One function per distinct entry: an entry called from several sites (or
    -- imported under several aliases) appears once in 'entries' per site, but
    -- must be emitted only once. Bindings above still cover every alias.
    glueText = T.intercalate "\n" (preludeText needsTensor products : map renderEntryText (uniqueByName entries))
    -- Only pull in tensor.hpp (which supplies mlc::Tensor) when some entry
    -- actually produces or consumes a rank>=2 array. A scalar/vector-only
    -- kernel must not force the tensor-cpp include path into the host build.
    needsTensor = any entryUsesTensor entries
    entryUsesTensor ge = let e = gleEntry ge in any usesTensor (feOutput e : feInputs e)

-- keep the first glue entry for each generated function name
uniqueByName :: [GlueEntry FutharkEntry] -> [GlueEntry FutharkEntry]
uniqueByName = go Set.empty
  where
    go _ [] = []
    go seen (e : es)
      | n `Set.member` seen = go seen es
      | otherwise = e : go (Set.insert n seen) es
      where
        n = glueName (gleEntry e)

glueName :: FutharkEntry -> Text
glueName entry = "_mlc_fut_" <> feName entry

preludeText :: Bool -> BuildProducts -> Text
preludeText needsTensor products =
  T.unlines $
    [ "// GENERATED by Morloc.CodeGenerator.Guest.Futhark -- do not edit"
    , "#include <mutex>"
    , "#include <atomic>"
    , "#include <vector>"
    , "#include <array>"
    , "#include <tuple>"
    , "#include <string>"
    , "#include <stdexcept>"
    , "#include <cstdlib>"
    , "#include <cstdint>"
    ]
      ++ ["#include \"tensor.hpp\"" | needsTensor]
      ++ ["#include \"" <> T.pack (takeFileName h) <> "\"" | h <- bpHeaders products]
      ++ [ ""
         , "extern \"C\" uint64_t morloc_fork_generation(void);"
         , "// FORK-8: one state per fork generation; a forked child makes its own"
         , "// and never touches its parent's lock or context."
         , "struct _mlc_fut_state { uint64_t gen; std::mutex m; futhark_context* c = nullptr; };"
         , "inline _mlc_fut_state* _mlc_fut_now() {"
         , "    static std::atomic<_mlc_fut_state*> cur;"
         , "    uint64_t g = morloc_fork_generation();"
         , "    _mlc_fut_state* s = cur.load(std::memory_order_acquire);"
         , "    while (s == nullptr || s->gen != g) {"
         , "        _mlc_fut_state* fresh = new _mlc_fut_state{g};"
         , "        if (cur.compare_exchange_strong(s, fresh, std::memory_order_acq_rel)) return fresh;"
         , "        delete fresh;"
         , "    }"
         , "    return s;"
         , "}"
         , "// INIT-1: built under the state's own lock."
         , "inline futhark_context* _mlc_fut_ctx(_mlc_fut_state* st) {"
         , "    if (st->c == nullptr) {"
         , "        futhark_context_config* cfg = futhark_context_config_new();"
         ]
      -- Select a specific device (GPU backends only); validated upstream.
      ++ ["        futhark_context_config_set_device(cfg, \"" <> dev <> "\");" | Just dev <- [bpDevice products]]
      ++ [ "        st->c = futhark_context_new(cfg);"
         , "        if (st->c == nullptr) throw std::runtime_error(\"futhark: failed to create context\");"
         , "    }"
         , "    return st->c;"
         , "}"
         ]

renderEntryText :: GlueEntry FutharkEntry -> Text
renderEntryText ge =
  T.unlines $
    [header]
      ++ ["    _mlc_fut_state* _st = _mlc_fut_now();"]
      ++ ["    std::lock_guard<std::mutex> _lk(_st->m);"]
      ++ ["    futhark_context* ctx = _mlc_fut_ctx(_st);"]
      ++ map (indent 4) inputSetup
      ++ [indent 4 retDecl]
      ++ [ indent 4 ("if (" <> callExpr <> " != 0 || futhark_context_sync(ctx) != 0) {")
         , indent 8 "char* _e = futhark_context_get_error(ctx); std::string _m = _e ? _e : \"futhark error\"; free(_e);"
         ]
      ++ map (indent 8) inputFrees
      ++ [ indent 8 "throw std::runtime_error(_m);"
         , indent 4 "}"
         ]
      -- inputs are no longer needed once the entry has run; free them before
      -- extracting the result so a later projection failure cannot leak them
      ++ map (indent 4) inputFrees
      ++ map (indent 4) resultLines
      ++ map (indent 4) freeReturnLines
      ++ [indent 4 returnLine, "}"]
  where
    entry = gleEntry ge
    out = feOutput entry
    argTys = map render (gleArgTypes ge)
    retTy = render (gleRetType ge)

    (inputSetup, argToks, inputFrees) = buildInputs (feInputs entry)

    callExpr = feCFun entry <> "(ctx, " <> T.intercalate ", " ("&r" : argToks) <> ")"

    (retDecl, resultLines, freeReturnLines, returnLine) = renderReturn out

    header =
      retTy <> " " <> glueName entry <> "("
        <> T.intercalate ", " ["const " <> aty <> "& x" <> tshow i | (i, aty) <- indexed argTys]
        <> ") {"

-- Build every entry input in order, threading the running list of frees (all
-- non-null, most-recent first) so each construction's null-guard can clean up
-- everything created before it. Returns (setup+guard lines, arg tokens for the
-- call, success-path free lines).
buildInputs :: [AbiFT] -> ([Text], [Text], [Text])
buildInputs = go (0 :: Int) []
  where
    go _ frees [] = ([], [], frees)
    go i frees (ft : rest) =
      let (lns, tok, frees') = marshalIn ("a" <> tshow i) ("x" <> tshow i) frees ft
          (lnsR, toksR, freesR) = go (i + 1) frees' rest
       in (lns ++ lnsR, tok : toksR, freesR)

-- Marshal one host value into the futhark ABI. Scalars pass by value; arrays are
-- created with futhark_new_* and null-checked; tuples recurse over their fields
-- and then build the opaque. Every guard frees the intermediates built so far.
marshalIn :: Text -> Text -> [Text] -> AbiFT -> ([Text], Text, [Text])
marshalIn nm hostExpr frees (FTNum n) = marshalNum nm hostExpr frees n
marshalIn nm hostExpr frees (FTTuple ops fields) =
  let (fieldLines, fieldToks, freesAfter) = goFields 0 frees (map snd fields)
      decl = tiCtype ops <> " " <> nm <> " = nullptr;"
      call = tiNew ops <> "(ctx, &" <> nm <> ", " <> T.intercalate ", " fieldToks <> ")"
      guardLines = opaqueGuard nm call freesAfter
      freeThis = tiFree ops <> "(ctx, " <> nm <> ");"
   in (fieldLines ++ [decl] ++ guardLines, nm, freeThis : freesAfter)
  where
    goFields _ fr [] = ([], [], fr)
    goFields k fr (f : fs) =
      let (lns, tok, fr') = marshalNum (nm <> "_" <> tshow k) (getField hostExpr k) fr f
          (lnsR, toksR, frR) = goFields (k + 1) fr' fs
       in (lns ++ lnsR, tok : toksR, frR)

marshalNum :: Text -> Text -> [Text] -> FNum -> ([Text], Text, [Text])
marshalNum _ hostExpr frees (FNum _ 0) = ([], hostExpr, frees)
marshalNum nm hostExpr frees n@(FNum e r) =
  let decl =
        arrayCType n <> "* " <> nm <> " = futhark_new_" <> e <> "_" <> tshow r
          <> "d(ctx, " <> hostExpr <> ".data(), " <> dimsOf hostExpr r <> ");"
      guardLines = nullGuard nm frees
      freeThis = "futhark_free_" <> e <> "_" <> tshow r <> "d(ctx, " <> nm <> ");"
   in (decl : guardLines, nm, freeThis : frees)

-- guard for a freshly-created array pointer
nullGuard :: Text -> [Text] -> [Text]
nullGuard nm frees =
  ["if (" <> nm <> " == nullptr) {"]
    ++ map ("    " <>) (frees ++ [marshalThrow])
    ++ ["}"]

-- guard for a freshly-constructed opaque (constructor returns int and sets *nm)
opaqueGuard :: Text -> Text -> [Text] -> [Text]
opaqueGuard nm call frees =
  ["if (" <> call <> " != 0 || " <> nm <> " == nullptr) {"]
    ++ map ("    " <>) (frees ++ [marshalThrow])
    ++ ["}"]

marshalThrow :: Text
marshalThrow = "throw std::runtime_error(\"futhark: input marshalling failed\");"

-- The result out-parameter is always "&r", inlined at the call site. Returns
-- (declaration, result-extraction lines, free lines for the result, return stmt).
renderReturn :: AbiFT -> (Text, [Text], [Text], Text)
renderReturn (FTNum n@(FNum _ 0)) =
  (cppScalarOf n <> " r = 0;", [], [], "return r;")
renderReturn (FTNum n@(FNum e r)) =
  let (readLines, value, freeLine) = readArray "r" e r
   in ( arrayCType n <> "* r = nullptr;"
      , readLines ++ ["futhark_context_sync(ctx);"]
      , [freeLine]
      , "return " <> value <> ";"
      )
renderReturn (FTTuple ti fields) =
  let pf = [projectField k proj f | (k, (proj, f)) <- indexed fields]
      decls = concatMap pfDecls pf
      projs = map pfProj pf
      errFrees = concatMap pfErrFree pf
      reads_ = concatMap pfRead pf
      succFrees = concatMap pfSuccFree pf
      values = map pfValue pf
      guardLines =
        ["if (_pc != 0 || futhark_context_sync(ctx) != 0) {"]
          ++ map ("    " <>) (errFrees ++ [tiFree ti <> "(ctx, r);", readThrow])
          ++ ["}"]
      resultLines =
        ["int _pc = 0;"]
          ++ decls
          ++ projs
          ++ guardLines
          ++ reads_
          ++ ["futhark_context_sync(ctx);"]
          ++ succFrees
   in ( tiCtype ti <> " r = nullptr;"
      , resultLines
      , [tiFree ti <> "(ctx, r);"]
      , "return std::make_tuple(" <> T.intercalate ", " values <> ");"
      )

readThrow :: Text
readThrow = "throw std::runtime_error(\"futhark: failed to read tuple result\");"

-- extraction plan for one field of a tuple return
data ProjField = ProjField
  { pfDecls :: [Text]
  , pfProj :: Text
  , pfErrFree :: [Text]
  , pfRead :: [Text]
  , pfSuccFree :: [Text]
  , pfValue :: Text
  }

projectField :: Int -> Text -> FNum -> ProjField
projectField k proj (FNum e 0) =
  let p = "p" <> tshow k
   in ProjField
        { pfDecls = [cppScalar' e <> " " <> p <> " = 0;"]
        , pfProj = "_pc |= " <> proj <> "(ctx, &" <> p <> ", r);"
        , pfErrFree = []
        , pfRead = []
        , pfSuccFree = []
        , pfValue = p
        }
projectField k proj (FNum e r) =
  let p = "p" <> tshow k
      (readLines, value, freeLine) = readArray p e r
   in ProjField
        { pfDecls = ["futhark_" <> e <> "_" <> tshow r <> "d* " <> p <> " = nullptr;"]
        , pfProj = "_pc |= " <> proj <> "(ctx, &" <> p <> ", r);"
        , pfErrFree = ["if (" <> p <> ") futhark_free_" <> e <> "_" <> tshow r <> "d(ctx, " <> p <> ");"]
        , pfRead = readLines
        , pfSuccFree = [freeLine]
        , pfValue = value
        }

-- Read a futhark array pointer 'ptr' (element token 'e', rank 'r') into a host
-- value. Shared by top-level array returns and tuple array fields. Returns
-- (read lines, host value expression, free line for 'ptr').
readArray :: Text -> Text -> Int -> ([Text], Text, Text)
readArray ptr e r =
  let sVar = ptr <> "_s"
      vVar = ptr <> "_v"
      dVar = ptr <> "_d"
      cpp = cppScalar' e
      -- The result value is materialised AFTER the array is freed, so for rank
      -- >= 2 the shape must be copied into stack locals ('dVar') before the free;
      -- reading the futhark-owned shape pointer ('sVar') past the free is a
      -- use-after-free. Rank 1 never reads the shape in its return, so it keeps
      -- using 'sVar' directly (only in the pre-free vector sizing).
      dimSrc
        | r == 1 = sVar
        | otherwise = dVar
      total = T.intercalate " * " ["(size_t)(" <> dimSrc <> "[" <> tshow d <> "])" | d <- [0 .. r - 1]]
      shapeDims = [dimSrc <> "[" <> tshow d <> "]" | d <- [0 .. r - 1]]
      value
        | r == 1 = "std::move(" <> vVar <> ")"
        | otherwise =
            "mlc::Tensor" <> tshow r <> "<" <> cpp <> ">(std::move(" <> vVar
              <> "), std::array<int64_t, " <> tshow r <> ">{" <> T.intercalate ", " shapeDims <> "})"
      captureDims
        | r == 1 = []
        | otherwise =
            [ "int64_t " <> dVar <> "[" <> tshow r <> "] = {"
                <> T.intercalate ", " [sVar <> "[" <> tshow d <> "]" | d <- [0 .. r - 1]]
                <> "};"
            ]
      readLines =
        [ "const int64_t* " <> sVar <> " = futhark_shape_" <> e <> "_" <> tshow r <> "d(ctx, " <> ptr <> ");" ]
          ++ captureDims
          ++ [ "std::vector<" <> cpp <> "> " <> vVar <> "(" <> total <> ");"
             , "futhark_values_" <> e <> "_" <> tshow r <> "d(ctx, " <> ptr <> ", " <> vVar <> ".data());"
             ]
      freeLine = "futhark_free_" <> e <> "_" <> tshow r <> "d(ctx, " <> ptr <> ");"
   in (readLines, value, freeLine)

getField :: Text -> Int -> Text
getField e i = "std::get<" <> tshow i <> ">(" <> e <> ")"

dimsOf :: Text -> Int -> Text
dimsOf e 1 = "(int64_t)" <> e <> ".size()"
dimsOf e r = T.intercalate ", " [e <> ".shape(" <> tshow d <> ")" | d <- [0 .. r - 1]]

arrayCType :: FNum -> Text
arrayCType (FNum e r) = "futhark_" <> e <> "_" <> tshow r <> "d"

cppScalarOf :: FNum -> Text
cppScalarOf (FNum e _) = cppScalar' e

-- ---------------------------------------------------------------------------
-- small helpers
-- ---------------------------------------------------------------------------

tshow :: (Show a) => a -> Text
tshow = T.pack . show

indent :: Int -> Text -> Text
indent n s = T.replicate n " " <> s

joinCommaD :: [MDoc] -> MDoc
joinCommaD [] = ""
joinCommaD [x] = x
joinCommaD (x : xs) = x <> ", " <> joinCommaD xs
