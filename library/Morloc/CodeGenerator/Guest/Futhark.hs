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
    against the corresponding entry point (element type and array rank).
  * 'guestGlue' renders the C++ glue that bridges native host tensors to the
    Futhark C library. It is pure: the host translator renders the argument and
    return types and passes them in through 'GlueEntry'.

The build recipe, element-type map, and ABI symbol scheme live here (not in
lang.yaml) because they carry Futhark-specific semantics.
-}
module Morloc.CodeGenerator.Guest.Futhark
  ( futharkGuest
  , FutharkEntry
  , hostSigTypes
  ) where

import Data.Aeson (FromJSON (..), eitherDecodeFileStrict', withObject, (.:))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeBaseName, takeFileName, (<.>))

import Morloc.CodeGenerator.Guest
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc (pretty, render)
import qualified Morloc.Monad as MM

-- ---------------------------------------------------------------------------
-- Parsed manifest entry
-- ---------------------------------------------------------------------------

-- | A Futhark ABI type: a fixed-width numeric element at some array rank
-- (0 = scalar, 1 = vector, 2 = matrix, ...).
data FutharkType = FutharkType
  { ftElem :: Text
  , ftRank :: Int
  }
  deriving (Eq, Show)

-- | One entry point parsed from the Futhark manifest.
data FutharkEntry = FutharkEntry
  { feName :: Text
  , feCFun :: Text
  , feInputs :: [FutharkType]
  , feOutput :: FutharkType
  }
  deriving (Show)

-- ---------------------------------------------------------------------------
-- Manifest JSON parsing (only the fields we need)
-- ---------------------------------------------------------------------------

-- Manifest shape:
--   {"entry_points": {"<name>": {"cfun":..., "inputs":[{"type":...}], "output":{"type":...}}}}
newtype Manifest = Manifest (Map Text RawEntry)

data RawEntry = RawEntry Text [Text] Text -- cfun, input type strings, output type string

newtype TypeField = TypeField {unTypeField :: Text}

instance FromJSON TypeField where
  parseJSON = withObject "typed" $ \v -> TypeField <$> v .: "type"

instance FromJSON RawEntry where
  parseJSON = withObject "entry_point" $ \v -> do
    cfun <- v .: "cfun"
    ins <- v .: "inputs"
    out <- v .: "output"
    pure (RawEntry cfun (map unTypeField ins) (unTypeField out))

instance FromJSON Manifest where
  parseJSON = withObject "manifest" $ \v -> Manifest <$> v .: "entry_points"

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
  triples <- mapM (buildOne backend outDir) srcs
  let (os, hs, jsons) = unzip3 triples
  pure
    BuildProducts
      { bpObjects = os
      , bpHeaders = hs
      , bpIncludeDirs = [outDir]
      , bpLinkFlags = backendLinkFlags backend
      , bpManifests = jsons
      }

-- returns (object, header, manifest) paths
buildOne :: Text -> Path -> GuestSource -> MorlocMonad (Path, Path, Path)
buildOne backend outDir gs = do
  let stem = takeBaseName (gsPath gs)
      outStem = outDir </> stem
      cFile = outStem <.> "c"
      hFile = outStem <.> "h"
      jsonFile = outStem <.> "json"
      oFile = outStem <.> "o"
      futCmd =
        T.unwords ["futhark", backend, "--library", "-o", T.pack outStem, T.pack (gsPath gs)]
      gccCmd = T.unwords ["gcc", "-O2", "-c", T.pack cFile, "-o", T.pack oFile]
  MM.runCommand "Futhark.build" futCmd
  MM.runCommand "Futhark.build" gccCmd
  pure (oFile, hFile, jsonFile)

backendLinkFlags :: Text -> [Text]
backendLinkFlags "multicore" = ["-lpthread", "-lm"]
backendLinkFlags _ = ["-lm"]

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
      MM.throwSystemError ("Futhark: could not read manifest " <> pretty path <> ": " <> pretty err)
    Right (Manifest raws) -> Map.traverseWithKey convert raws

convert :: Text -> RawEntry -> MorlocMonad FutharkEntry
convert name (RawEntry cfun ins out) = do
  fins <- mapM (parseFutharkType name) ins
  fout <- parseFutharkType name out
  pure (FutharkEntry name cfun fins fout)

-- parse a manifest type string like "[][]f32" into (element, rank)
parseFutharkType :: Text -> Text -> MorlocMonad FutharkType
parseFutharkType name = go 0
  where
    go :: Int -> Text -> MorlocMonad FutharkType
    go n s = case T.stripPrefix "[]" s of
      Just rest -> go (n + 1) rest
      Nothing ->
        if s `Set.member` elemTokenSet
          then pure (FutharkType s n)
          else
            MM.throwSystemError
              ("Futhark entry " <> pretty name <> ": unsupported element type " <> pretty s)

checkOne :: Map Text FutharkEntry -> SourcedSig -> MorlocMonad (SourcedSig, FutharkEntry)
checkOne entries sig = do
  let entryName = unSrcName (srcName (ssSource sig))
  entry <- case Map.lookup entryName entries of
    Just e -> pure e
    Nothing ->
      MM.throwSystemError ("Futhark: no entry point named " <> pretty entryName <> " in the manifest")
  (argTs, retT) <- decomposeSig (ssType sig)
  unless (length argTs == length (feInputs entry)) $
    MM.throwSystemError
      ( "Futhark entry " <> pretty entryName <> ": morloc signature has "
          <> pretty (length argTs) <> " arguments but the entry takes "
          <> pretty (length (feInputs entry))
      )
  zipWithM_ (compareFT entryName) argTs (feInputs entry)
  compareFT entryName retT (feOutput entry)
  pure (sig, entry)

compareFT :: Text -> FutharkType -> FutharkType -> MorlocMonad ()
compareFT name expected actual =
  unless (expected == actual) $
    MM.throwSystemError
      ( "Futhark entry " <> pretty name <> ": morloc signature implies "
          <> showFT expected <> " but the entry has " <> showFT actual
      )
  where
    showFT ft = pretty (ftElem ft) <> " rank-" <> pretty (ftRank ft)

-- decompose a morloc signature into the futhark types it implies
decomposeSig :: TypeU -> MorlocMonad ([FutharkType], FutharkType)
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

-- map a single morloc argument/return type to its futhark ABI type
morlocToFT :: TypeU -> MorlocMonad FutharkType
morlocToFT t = case decomposeMorloc t of
  Just (elemCon, rank)
    | Just tok <- Map.lookup elemCon elemToken -> pure (FutharkType tok rank)
  _ -> unsupportedType t

-- decompose a morloc type to its (element constructor, array rank), or Nothing.
-- Shared by the signature check and the host-type renderer.
decomposeMorloc :: TypeU -> Maybe (Text, Int)
decomposeMorloc (EffectU _ t) = decomposeMorloc t
decomposeMorloc (AppU (VarU _) args) =
  case partitionKindArgsU args of
    ([VarU (TV elemCon)], rank) -> Just (elemCon, rank)
    _ -> Nothing
decomposeMorloc (VarU (TV elemCon)) = Just (elemCon, 0)
decomposeMorloc _ = Nothing

unsupportedType :: TypeU -> MorlocMonad a
unsupportedType t =
  MM.throwSystemError
    ( "Futhark: unsupported type "
        <> pretty t
        <> " (only fixed-width numeric scalars and Vector/Matrix of them are allowed)"
    )

-- ---------------------------------------------------------------------------
-- Host (C++) type rendering -- the pass calls this to fill GlueEntry.
-- Path A: a restricted-set renderer whose output matches cppTypeOf for numeric
-- scalars and Vector/Matrix. A future path B would use the real cppTypeOf.
-- ---------------------------------------------------------------------------

-- render the host C++ types for a sourced signature's arguments and return
hostSigTypes :: TypeU -> ([MDoc], MDoc)
hostSigTypes t0 =
  let (args, ret) = unfoldFun (stripForall t0)
   in (map renderHostType args, renderHostType ret)

renderHostType :: TypeU -> MDoc
renderHostType t = case decomposeMorloc t of
  Just (e, 0) -> pretty (cppScalarForMorloc e)
  Just (e, 1) -> "std::vector<" <> pretty (cppScalarForMorloc e) <> ">"
  Just (e, r) -> "mlc::Tensor" <> pretty r <> "<" <> pretty (cppScalarForMorloc e) <> ">"
  Nothing -> "void"

cppScalarForMorloc :: Text -> Text
cppScalarForMorloc e = maybe "void" snd (Map.lookup e numericTypes)

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
    glueText = T.intercalate "\n" (preludeText products : map renderEntryText entries)

glueName :: FutharkEntry -> Text
glueName entry = "_mlc_fut_" <> feName entry

preludeText :: BuildProducts -> Text
preludeText products =
  T.unlines $
    [ "// GENERATED by Morloc.CodeGenerator.Guest.Futhark -- do not edit"
    , "#include <mutex>"
    , "#include <vector>"
    , "#include <array>"
    , "#include <string>"
    , "#include <stdexcept>"
    , "#include <cstdlib>"
    , "#include <cstdint>"
    , "#include \"tensor.hpp\""
    ]
      ++ ["#include \"" <> T.pack (takeFileName h) <> "\"" | h <- bpHeaders products]
      ++ [ ""
         , "inline std::mutex& _mlc_fut_mtx() { static std::mutex m; return m; }"
         , "inline futhark_context* _mlc_fut_ctx() {"
         , "    static futhark_context* c = nullptr;"
         , "    if (c == nullptr) {"
         , "        futhark_context_config* cfg = futhark_context_config_new();"
         , "        c = futhark_context_new(cfg);"
         , "        if (c == nullptr) throw std::runtime_error(\"futhark: failed to create context\");"
         , "    }"
         , "    return c;"
         , "}"
         ]

renderEntryText :: GlueEntry FutharkEntry -> Text
renderEntryText ge =
  T.unlines $
    [header]
      ++ ["    futhark_context* ctx = _mlc_fut_ctx();"]
      ++ ["    std::lock_guard<std::mutex> _lk(_mlc_fut_mtx());"]
      ++ map (indent 4) createLines
      ++ [indent 4 retDecl]
      ++ [ indent 4 ("if (" <> callExpr <> " != 0 || futhark_context_sync(ctx) != 0) {")
         , indent 8 "char* _e = futhark_context_get_error(ctx); std::string _m = _e ? _e : \"futhark error\"; free(_e);"
         ]
      ++ map (indent 8) freeInputLines
      ++ [ indent 8 "throw std::runtime_error(_m);"
         , indent 4 "}"
         ]
      ++ map (indent 4) resultLines
      ++ map (indent 4) freeAllLines
      ++ [indent 4 returnLine, "}"]
  where
    entry = gleEntry ge
    inputs = feInputs entry
    out = feOutput entry
    argTys = map render (gleArgTypes ge)
    retTy = render (gleRetType ge)
    ixInputs = zip [0 :: Int ..] inputs

    header =
      retTy <> " " <> glueName entry <> "("
        <> T.intercalate ", " ["const " <> aty <> "& x" <> tshow i | (i, aty) <- zip [0 :: Int ..] argTys]
        <> ") {"

    createLines =
      [ arrayCType ft <> "* a" <> tshow i <> " = futhark_new_" <> ftElem ft <> "_" <> tshow (ftRank ft)
          <> "d(ctx, x" <> tshow i <> ".data(), " <> dimsArgs i ft <> ");"
      | (i, ft) <- ixInputs, ftRank ft >= 1
      ]

    callArgs = [if ftRank ft >= 1 then "a" <> tshow i else "x" <> tshow i | (i, ft) <- ixInputs]
    callExpr = feCFun entry <> "(ctx, " <> T.intercalate ", " ("&r" : callArgs) <> ")"

    freeInputLines =
      [ "futhark_free_" <> ftElem ft <> "_" <> tshow (ftRank ft) <> "d(ctx, a" <> tshow i <> ");"
      | (i, ft) <- ixInputs, ftRank ft >= 1
      ]
    freeReturnLines
      | ftRank out >= 1 = ["futhark_free_" <> ftElem out <> "_" <> tshow (ftRank out) <> "d(ctx, r);"]
      | otherwise = []
    freeAllLines = freeInputLines ++ freeReturnLines

    (retDecl, resultLines, returnLine) = renderReturn out retTy

-- (declaration, result-extraction lines, return statement); the out-arg is
-- always "&r", inlined at the call site.
renderReturn :: FutharkType -> Text -> (Text, [Text], Text)
renderReturn ft retTy
  | ftRank ft == 0 = (cppScalarOf ft <> " r = 0;", [], "return r;")
  | otherwise =
      ( arrayCType ft <> "* r = nullptr;"
      , [ "const int64_t* s = futhark_shape_" <> ftElem ft <> "_" <> tshow rank <> "d(ctx, r);"
        , "std::vector<" <> cppScalarOf ft <> "> out(" <> total <> ");"
        , "futhark_values_" <> ftElem ft <> "_" <> tshow rank <> "d(ctx, r, out.data()); futhark_context_sync(ctx);"
        ]
      , returnStmt
      )
  where
    rank = ftRank ft
    dims = ["s[" <> tshow d <> "]" | d <- [0 .. rank - 1]]
    total = T.intercalate " * " ["(size_t)(" <> d <> ")" | d <- dims]
    returnStmt
      | rank == 1 = "return out;"
      | otherwise =
          "return " <> retTy <> "(std::move(out), std::array<int64_t, " <> tshow rank <> ">{"
            <> T.intercalate ", " dims <> "});"

dimsArgs :: Int -> FutharkType -> Text
dimsArgs i ft
  | ftRank ft == 1 = "(int64_t)x" <> tshow i <> ".size()"
  | otherwise = T.intercalate ", " ["x" <> tshow i <> ".shape(" <> tshow d <> ")" | d <- [0 .. ftRank ft - 1]]

arrayCType :: FutharkType -> Text
arrayCType ft = "futhark_" <> ftElem ft <> "_" <> tshow (ftRank ft) <> "d"

cppScalarOf :: FutharkType -> Text
cppScalarOf ft = Map.findWithDefault "void" (ftElem ft) cppScalar

-- ---------------------------------------------------------------------------
-- small helpers
-- ---------------------------------------------------------------------------

tshow :: (Show a) => a -> Text
tshow = T.pack . show

indent :: Int -> Text -> Text
indent n s = T.replicate n " " <> s
