{- |
Module      : MorlocManager.Freeze
Description : Export installed morloc state as a portable frozen artifact
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Captures the complete installed morloc state (modules, programs, pools) as
a tarball with a JSON manifest, suitable for building an immutable serve
image.

@
  Builder Container           Frozen Artifact           Serve Image
  +------------------+       +------------------+     +------------------+
  | lib/  (modules)  |       | state.tar.gz     |     | read-only        |
  | fdb/  (manifests)| tar   |   lib/           |COPY | fdb + lib        |
  | bin/  (programs) |------>|   fdb/           |---->| bin + pools      |
  | pool files       |       |   bin/           |     | nexus router     |
  | morloc binary    |       |   morloc (bin)   |     | morloc (eval)    |
  +------------------+       | manifest.json    |     +------------------+
                              +------------------+
  FROZEN (excluded):
    GHC, stack, Cabal, build tools, package caches
@

The freeze manifest (@freeze-manifest.json@) provides an auditable record:
morloc version, installed modules with SHA-256 checksums, compiled programs
with their exported commands, base image reference, and environment layer.
This manifest can be reviewed before deploying to production.
-}

module MorlocManager.Freeze
  ( -- * Freeze operation
    freeze
    -- * Manifest
  , writeFreezeManifest
  , readFreezeManifest
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Word (Word8)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, hFlush, stderr)
import System.Process (readProcessWithExitCode)

import MorlocManager.Types
import MorlocManager.Config (readVersionConfig, readActiveConfig, versionDataDir)

-- | Freeze the installed morloc state into a tarball and manifest.
--
-- The output directory receives:
--
--   * @state.tar.gz@ -- compressed archive of lib\/, fdb\/, bin\/, and the
--     morloc compiler binary
--   * @freeze-manifest.json@ -- auditable record of frozen contents
--
-- This runs inside (or against the data volume of) the builder container.
-- It does NOT freeze GHC, stack, or other build tooling.
freeze :: Scope -> Version -> FilePath -> IO (Either ManagerError ())
freeze scope ver outputDir = do
  createDirectoryIfMissing True outputDir
  vDataDir <- versionDataDir scope ver
  exists <- doesDirectoryExist vDataDir
  if not exists
    then pure (Left (VersionNotInstalled ver))
    else do
      -- Build the tarball
      hPutStrLn stderr $ "Freezing installed state from " <> vDataDir <> "..."
      hFlush stderr
      let tarPath = outputDir </> "state.tar.gz"
      (code, _stdout, tarErr) <- readProcessWithExitCode "tar"
        [ "-czf", tarPath
        , "-C", vDataDir
        , "lib", "fdb", "bin"
        ] ""
      case code of
        ExitFailure _ -> pure (Left (FreezeError ("tar failed: " <> tarErr)))
        ExitSuccess -> do
          hPutStrLn stderr $ "Created " <> tarPath
          -- Scan installed modules and programs for manifest
          modules' <- scanModules (vDataDir </> "lib")
          programs' <- scanPrograms (vDataDir </> "fdb")
          now <- getCurrentTime
          -- Read version config for image reference and active environment
          vcResult <- readVersionConfig scope ver
          mCfg <- readActiveConfig
          let baseImg = case vcResult of
                Right vc -> vcImage vc
                Left _   -> "unknown"
              envLyr = case mCfg of
                Just cfg | configActiveEnv cfg /= "base" -> Just (configActiveEnv cfg)
                _ -> Nothing
          let manifest = FreezeManifest
                { fmMorlocVersion = ver
                , fmFrozenAt      = now
                , fmModules       = modules'
                , fmPrograms      = programs'
                , fmBaseImage     = baseImg
                , fmEnvLayer      = envLyr
                }
          let manifestPath = outputDir </> "freeze-manifest.json"
          writeFreezeManifest manifestPath manifest
          hPutStrLn stderr $ "Wrote " <> manifestPath
          hPutStrLn stderr $ "Frozen state written to " <> outputDir
          hFlush stderr
          pure (Right ())

-- | Write a freeze manifest as pretty-printed JSON.
writeFreezeManifest :: FilePath -> FreezeManifest -> IO ()
writeFreezeManifest path manifest =
  BL.writeFile path (Aeson.encode manifest)

-- | Read a freeze manifest from a JSON file.
readFreezeManifest :: FilePath -> IO (Either ManagerError FreezeManifest)
readFreezeManifest path = do
  bytes <- BL.readFile path
  case Aeson.eitherDecode bytes of
    Left msg  -> pure (Left (FreezeError ("Invalid manifest: " <> msg)))
    Right val -> pure (Right val)

-- ======================================================================
-- Internal: scanning installed state
-- ======================================================================

-- | Scan the lib/ directory for installed modules.
--
-- For each subdirectory in lib/, reads the module name from the directory
-- name and computes a SHA-256 hash of all .loc files concatenated.
scanModules :: FilePath -> IO [ModuleEntry]
scanModules libDir = do
  exists <- doesDirectoryExist libDir
  if not exists
    then pure []
    else do
      entries <- listDirectory libDir
      dirs <- filterM (doesDirectoryExist . (libDir </>)) entries
      mapM (scanOneModule libDir) dirs

-- | Scan a single module directory for its name, version, and content hash.
scanOneModule :: FilePath -> String -> IO ModuleEntry
scanOneModule libDir name = do
  let modDir = libDir </> name
  -- Hash all .loc files in the module directory for a content fingerprint
  files <- listDirectory modDir
  let locFiles = [f | f <- files, hasSuffix ".loc" f]
  contentHash <- hashDirectoryFiles modDir locFiles
  pure ModuleEntry
    { meName    = Text.pack name
    , meVersion = Nothing  -- modules don't yet have standardized version metadata
    , meSha256  = contentHash
    }

-- | Scan the fdb/ directory for compiled program manifests.
--
-- For each @.manifest@ file, extracts the program name and parses the
-- JSON to find exported command names.
scanPrograms :: FilePath -> IO [ProgramEntry]
scanPrograms fdbDir = do
  exists <- doesDirectoryExist fdbDir
  if not exists
    then pure []
    else do
      entries <- listDirectory fdbDir
      let manifests = [e | e <- entries, hasSuffix ".manifest" e]
      mapM (scanOneProgram fdbDir) manifests

-- | Parse a single .manifest JSON file for program name and commands.
scanOneProgram :: FilePath -> String -> IO ProgramEntry
scanOneProgram fdbDir filename = do
  let path = fdbDir </> filename
      progName = stripSuffix ".manifest" filename
  commands <- parseManifestCommands path
  pure ProgramEntry
    { peName     = Text.pack progName
    , peCommands = commands
    }

-- | Extract command names from a manifest JSON file.
--
-- Parses the manifest as a simple structure with a @"commands"@ array,
-- each element having a @"name"@ field. Returns an empty list if the
-- file cannot be parsed.
parseManifestCommands :: FilePath -> IO [Text]
parseManifestCommands path = do
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      bytes <- BL.readFile path
      case Aeson.decode bytes :: Maybe ManifestStub of
        Nothing   -> pure []
        Just stub -> pure (map mscName (msCommands stub))

-- | Minimal manifest structure for extracting command names during freeze.
-- We only need the command names, not the full manifest schema.
data ManifestStub = ManifestStub
  { msCommands :: [ManifestStubCmd]
  } deriving (Show)

data ManifestStubCmd = ManifestStubCmd
  { mscName :: Text
  } deriving (Show)

instance Aeson.FromJSON ManifestStub where
  parseJSON = Aeson.withObject "ManifestStub" $ \o ->
    ManifestStub <$> o Aeson..:? "commands" Aeson..!= []

instance Aeson.FromJSON ManifestStubCmd where
  parseJSON = Aeson.withObject "ManifestStubCmd" $ \o ->
    ManifestStubCmd <$> o Aeson..: "name"

-- ======================================================================
-- Internal utilities
-- ======================================================================

-- | Compute SHA-256 hash of concatenated file contents from a directory.
hashDirectoryFiles :: FilePath -> [String] -> IO Text
hashDirectoryFiles dir files = do
  contents <- mapM (\f -> BS.readFile (dir </> f)) (sort files)
  let digest = SHA256.hash (BS.concat contents)
  pure (hexEncode digest)

-- | Encode a ByteString as a hex Text.
hexEncode :: BS.ByteString -> Text
hexEncode bs = Text.pack (concatMap byteToHex (BS.unpack bs))
  where
    byteToHex :: Word8 -> String
    byteToHex b =
      let (hi, lo) = b `divMod` 16
      in [hexChar hi, hexChar lo]
    hexChar :: Word8 -> Char
    hexChar n
      | n < 10    = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)

hasSuffix :: String -> String -> Bool
hasSuffix suffix str = drop (length str - length suffix) str == suffix

stripSuffix :: String -> String -> String
stripSuffix suffix str
  | hasSuffix suffix str = take (length str - length suffix) str
  | otherwise = str

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM p (x:xs) = do
  ok <- p x
  rest <- filterM p xs
  pure (if ok then x : rest else rest)
