{- |
Module      : SystemConfigTests
Description : Unit tests for the conda toolchain-coherence guard

Guards the two pure decisions behind the native-backend coherence check.

'Morloc.CodeGenerator.SystemConfig.pathIsWithin' decides whether a build tool is
coherent: only when its directory is the conda prefix itself or nested under it.
The subtle case is a sibling whose name merely shares a textual prefix
(@/opt/env@ vs @/opt/env2@), which must NOT count as "within" -- a naive
@isPrefixOf@ without the trailing separator would wrongly accept it and let a
foreign toolchain pass the guard.

'Morloc.CodeGenerator.SystemConfig.compilerIncoherence' decides WHY a C/C++
compiler resolved outside the prefix, which is what the reported error tells the
user to go fix: a tool the env owns but PATH order lost, or an environment that
activated no compiler at all.

'Morloc.CodeGenerator.SystemConfig.toolsOutsidePrefix' lists the programs a
pool build would run from outside a strict conda prefix. It is run against
fixture prefix and host directories of fake executables, with the environment
passed explicitly, so no test touches the process environment.
-}
module SystemConfigTests (systemConfigTests) where

import Morloc.CodeGenerator.SystemConfig
  ( BuildTool (..)
  , Incoherence (..)
  , PoolToolchain (..)
  , ToolchainHost (..)
  , compilerIncoherence
  , BindingState (..)
  , bindingState
  , bindingsReportJson
  , effectiveInitFlags
  , findRustDir
  , installedRuntimeMismatches
  , Mismatch (..)
  , interpreterIdentity
  , langMarkerText
  , nexusAgrees
  , pathIsWithin
  , runtimeInstallScript
  , runtimeInstalled
  , toolsOutsidePrefix
  )
import Control.Exception (bracket)
import Morloc.Abi (abiVersion)
import qualified Morloc.DataFiles as DF
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , createFileLink
  , getPermissions
  , getTemporaryDirectory
  , removePathForcibly
  , setOwnerExecutable
  , setPermissions
  )
import Data.List (isInfixOf)
import System.FilePath (takeDirectory, (</>))
import qualified System.Info as SI
import System.Posix.Process (getProcessID)
import System.Process (callProcess, readProcessWithExitCode)
import System.Exit (ExitCode (..))
import qualified Data.Text.IO as TIO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

systemConfigTests :: TestTree
systemConfigTests =
  testGroup
    "SystemConfig (conda coherence guard)"
    [ pathIsWithinTests
    , compilerIncoherenceTests
    , toolsOutsidePrefixTests
    , runtimeInstallTests
    , bindingTests
    ]

bindingTests :: TestTree
bindingTests =
  testGroup
    "language bindings"
    [ testCase "spec-shim-4-1" $
        effectiveInitFlags True (False, False) (Just True, Just True) @?= (True, True)
    , testCase "a full init takes slurm and sanitize from its flags" $
        effectiveInitFlags False (False, False) (Just True, Just True) @?= (False, False)
    , testCase "an incremental init can still turn a setting on" $
        effectiveInitFlags True (True, False) (Nothing, Nothing) @?= (True, False)
    , testCase "python's identity is its extension suffix and numpy's major version" $
        withScript "py" "echo '.cpython-313-x86_64-linux-gnu.so numpy 2'" $ \exe ->
          interpreterIdentity pySetup exe >>= (@?= Right (T.pack ".cpython-313-x86_64-linux-gnu.so numpy 2"))
    , testCase "R's identity is its major and minor version, past any R_HOME warning" $
        withScript "r" "echo 'WARNING: ignoring environment value of R_HOME'; echo 'R version 4.4.1 (2024-06-14) -- \"x\"'" $ \exe ->
          interpreterIdentity rSetup' exe >>= (@?= Right (T.pack "R 4.4"))
    , testCase "a binding with no marker is absent even before any runtime is recorded" $
        withHome "bindfresh" [] $ \home ->
          bindingState home False Nothing pySetup (Just (Right ident)) >>= (@?= BindingAbsent)
    , testCase "an interpreter that fails is reported" $
        withScript "bad" "exit 3" $ \exe -> do
          r <- interpreterIdentity pySetup exe
          assertBool (show r) (either (const True) (const False) r)
    , testCase "spec-shim-1-1" $
        withHome "bindabsent" [] $ \home ->
          bindingState home False (Just bid) pySetup (Just (Right ident)) >>= (@?= BindingAbsent)
    , testCase "spec-shim-1-2" $
        withHome "bindok" ["opt/pymorloc" <> T.unpack ident] $ \home -> do
          writeOneMarker home pySetup (langMarkerText bid ident False pySetup)
          bindingState home False (Just bid) pySetup (Just (Right ident)) >>= (@?= BindingOk)
    , testCase "spec-shim-1-3" $
        withHome "bindinterp" ["opt/pymorloc" <> T.unpack ident] $ \home -> do
          writeOneMarker home pySetup (langMarkerText bid (T.pack ".cpython-312-x86_64-linux-gnu.so") False pySetup)
          r <- bindingState home False (Just bid) pySetup (Just (Right ident))
          assertBool (show r) (isStale r)
    , testCase "make judges a binding by its record alone, without the interpreter" $
        withHome "bindmake" ["opt/pymorloc.cpython-312-x86_64-linux-gnu.so"] $ \home -> do
          writeOneMarker home pySetup (langMarkerText bid (T.pack ".cpython-312-x86_64-linux-gnu.so numpy 2") False pySetup)
          bindingState home False (Just bid) pySetup Nothing >>= (@?= BindingOk)
    , testCase "a binding built against another runtime build is stale, naming both" $
        withHome "bindbuild" ["opt/pymorloc" <> T.unpack ident] $ \home -> do
          writeOneMarker home pySetup (langMarkerText (T.pack "aaaa") ident False pySetup)
          r <- bindingState home False (Just bid) pySetup Nothing
          assertBool (show r) (case r of BindingStale w -> "aaaa" `isInfixOf` w && T.unpack bid `isInfixOf` w; _ -> False)
    , testCase "spec-shim-5-1" $
        loadCheck "data/lang/r/lang.yaml" "morloc_built_r_version" $ \line -> do
          let expr built = T.unpack (T.replace (T.pack ".Call(\"morloc_built_r_version\")") (T.pack built) line)
          (bad, _, err) <- readProcessWithExitCode "Rscript" ["-e", expr "\"1.0.0\""] ""
          assertBool err (bad /= ExitSuccess && "built for R 1.0" `isInfixOf` err)
          (good, _, err') <- readProcessWithExitCode "Rscript" ["-e", expr "paste(R.version$major, R.version$minor, sep = \".\")"] ""
          assertEqual err' ExitSuccess good
    , testCase "spec-shim-5-2" $ do
        text <- TIO.readFile "data/lang/py/lang.yaml"
        let ls' = [unquote l | l <- T.lines text, any (`T.isInfixOf` l) [T.pack "\"try:\"", T.pack "import pymorloc as morloc", T.pack "except ImportError as _mlc_e", T.pack "_mlc_e)"]]
            script = T.unlines (T.pack "import sys" : map (T.replace (T.pack "import pymorloc as morloc") (T.pack "import pymorloc_absent_for_this_test as morloc")) ls')
        (code, _, err) <- readProcessWithExitCode "python3" ["-I", "-c", T.unpack script] ""
        assertBool err (code /= ExitSuccess && "run `morloc init -f`" `isInfixOf` err)
    , testCase "spec-shim-1-4" $
        withHome "bindartifact" [] $ \home -> do
          writeOneMarker home pySetup (langMarkerText bid ident False pySetup)
          r <- bindingState home False (Just bid) pySetup (Just (Right ident))
          assertBool (show r) (isStale r)
    , testCase "an interpreter outside the environment is reported as such, not as stale" $
        withHome "bindoutside" [] $ \home -> writeOneMarker home pySetup (langMarkerText bid ident False pySetup) >>
          bindingState home False (Just bid) pySetup (Just (Left (BindingOutsidePrefix "py"))) >>= (@?= BindingOutsidePrefix "py")
    , testCase "spec-shim-2-1" $ do
        let j = T.unpack (bindingsReportJson (Left "torn") [(T.pack "py", BindingOk), (T.pack "r", BindingStale "old")])
        mapM_ (\needle -> assertBool (needle <> " in " <> j) (needle `isInfixOf` j))
          ["\"bindings_check\":1", "\"ok\":false", "torn", "\"lang\":\"py\"", "\"state\":\"ok\"", "\"state\":\"stale\"", "old"]
    ]
  where
    bid = T.pack "0123456789abcdef"
    ident = T.pack ".cpython-313-x86_64-linux-gnu.so"
    pySetup = DF.pythonSetup
    rSetup' = DF.rSetup
    unquote l = let t = T.strip l in T.dropEnd 1 (T.drop 3 t)
    loadCheck file needle act = do
      text <- TIO.readFile file
      case [unquote l | l <- T.lines text, T.pack needle `T.isInfixOf` l] of
        [line] -> act line
        found -> assertFailure ("expected one " <> needle <> " line in " <> file <> ", found " <> show (length found))
    isStale (BindingStale _) = True
    isStale _ = False
    writeOneMarker home ls text = do
      let dir = home </> "opt" </> "lang-configured"
      createDirectoryIfMissing True dir
      writeFile (dir </> DF.lsName ls) text
    withHome tag files act = do
      tmp <- getTemporaryDirectory
      pid <- getProcessID
      let root = tmp </> ("morloc-binding-" <> show pid <> "-" <> tag)
      bracket
        (do removePathForcibly root
            mapM_ (\f -> createDirectoryIfMissing True (takeDirectory (root </> f)) >> writeFile (root </> f) "") files
            createDirectoryIfMissing True root
            return root)
        removePathForcibly
        act
    withScript tag body act = do
      tmp <- getTemporaryDirectory
      pid <- getProcessID
      let dir = tmp </> ("morloc-interp-" <> show pid <> "-" <> tag)
          exe = dir </> "interp"
      bracket
        (do removePathForcibly dir
            createDirectoryIfMissing True dir
            callProcess "sh" ["-c", "printf '%s' \"$2\" > \"$1\" && chmod +x \"$1\"", "sh", exe, "#!/bin/sh\n" <> body <> "\n"]
            return exe)
        (const (removePathForcibly dir))
        act

runtimeInstallTests :: TestTree
runtimeInstallTests =
  testGroup
    "runtime install"
    [ testCase "a home with libmorloc, the nexus, its build id and this compiler's morloc.h holds a runtime" $
        withHome "all" ["lib/" <> libName, "bin/morloc-nexus", "lib/morloc.build-id"] $ \home -> do
          writeEmbeddedHeader home
          runtimeInstalled home >>= assertBool "installed"
    , testCase "a home without the runtime's recorded build id does not hold a usable runtime" $
        withHome "nobuildid" ["lib/" <> libName, "bin/morloc-nexus"] $ \home -> do
          writeEmbeddedHeader home
          runtimeInstalled home >>= assertBool "not installed" . not
    , testCase "a home whose morloc.h differs from this compiler's does not hold a usable runtime" $
        withHome "editedheader" ["lib/" <> libName, "bin/morloc-nexus", "lib/morloc.build-id"] $ \home -> do
          writeHeader home abiVersion
          runtimeInstalled home >>= assertBool "not installed" . not
    , testCase "a home without morloc.h does not hold a runtime" $
        withHome "noheader" ["lib/" <> libName, "bin/morloc-nexus"] $ \home ->
          runtimeInstalled home >>= assertBool "not installed" . not
    , testCase "a home without the nexus does not hold a runtime" $
        withHome "nonexus" ["lib/" <> libName, "include/morloc.h"] $ \home ->
          runtimeInstalled home >>= assertBool "not installed" . not
    , testCase "a runtime source with install.sh and this compiler's header is used through it" $
        withHome "script" ["Cargo.toml", "install.sh"] $ \src -> do
          writeEmbeddedHeader src
          runtimeInstallScript src >>= (@?= Right (src </> "install.sh"))
    , testCase "a runtime source whose header differs at the same ABI is refused" $
        withHome "samabi" ["Cargo.toml", "install.sh"] $ \src -> do
          writeHeader src abiVersion
          r <- runtimeInstallScript src
          case r of
            Left msg -> assertBool msg ("differs" `isInfixOf` msg)
            Right p -> assertFailure ("accepted " <> p)
    , testCase "a directory without Cargo.toml is not a runtime source" $
        withHome "notsource" ["install.sh"] $ \src -> do
          r <- runtimeInstallScript src
          case r of
            Left msg -> assertBool msg ("Cargo.toml" `isInfixOf` msg)
            Right p -> assertFailure ("accepted " <> p)
    , testCase "a runtime source whose header declares another ABI is refused, naming both" $
        withHome "otherabi" ["Cargo.toml", "install.sh"] $ \src -> do
          writeHeader src (abiVersion + 1)
          r <- runtimeInstallScript src
          case r of
            Left msg -> assertBool msg (show abiVersion `isInfixOf` msg && show (abiVersion + 1) `isInfixOf` msg)
            Right p -> assertFailure ("accepted " <> p)
    , testCase "a runtime source without morloc.h is refused" $
        withHome "noheaderdir" ["Cargo.toml", "install.sh"] $ \src -> do
          r <- runtimeInstallScript src
          case r of
            Left msg -> assertBool msg ("morloc.h" `isInfixOf` msg)
            Right p -> assertFailure ("accepted " <> p)
    , testCase "the source search skips a candidate without install.sh" $
        withHome "search" ["old/Cargo.toml", "new/Cargo.toml", "new/install.sh"] $ \root -> do
          writeEmbeddedHeader (root </> "new")
          findRustDir [root </> "old", root </> "new"] >>= (@?= (root </> "new"))
    , testCase "the source search skips a candidate whose header differs" $
        withHome "searchheader" ["old/Cargo.toml", "old/install.sh", "new/Cargo.toml", "new/install.sh"] $ \root -> do
          writeHeader (root </> "old") abiVersion
          writeEmbeddedHeader (root </> "new")
          findRustDir [root </> "old", root </> "new"] >>= (@?= (root </> "new"))
    , testCase "an installed nexus that accepts its libmorloc at this ABI agrees" $
        withHome "nexusok" [] $ \home -> do
          fakeNexus home 0 (show abiVersion)
          writeBuildId home fakeBuildId
          nexusAgrees home >>= (@?= Right ())
    , testCase "an installed nexus whose build id is not the recorded one does not agree" $
        withHome "nexusunfinished" [] $ \home -> do
          fakeNexus home 0 (show abiVersion)
          writeBuildId home "fedcba9876543210"
          r <- nexusAgrees home
          case r of
            Left msg -> assertBool msg ("did not finish" `isInfixOf` msg)
            Right () -> assertFailure "an unfinished install agreed"
    , testCase "an installed nexus that refuses its libmorloc does not agree" $
        withHome "nexustorn" [] $ \home -> do
          fakeNexus home 1 (show abiVersion)
          r <- nexusAgrees home
          case r of
            Left msg -> assertBool msg ("build" `isInfixOf` msg)
            Right () -> assertFailure "a torn install agreed"
    , testCase "an installed nexus of another ABI does not agree, naming both" $
        withHome "nexusabi" [] $ \home -> do
          fakeNexus home 0 "999"
          writeBuildId home fakeBuildId
          r <- nexusAgrees home
          case r of
            Left msg -> assertBool msg ("999" `isInfixOf` msg && show abiVersion `isInfixOf` msg)
            Right () -> assertFailure "another ABI agreed"
    , testCase "a home init configured with this compiler has no mismatches" $
        withHome "stampok" [] $ \home -> do
          writeEmbeddedHeader home
          writeBuildId home "aaaa"
          writeMarkers home "aaaa" False id
          mismatches False allLangs home >>= (@?= [])
    , testCase "bindings built against another runtime build are reported" $
        withHome "stampbuild" [] $ \home -> do
          writeEmbeddedHeader home
          writeBuildId home "bbbb"
          writeMarkers home "aaaa" False id
          r <- mismatches False (map T.pack ["py"]) home
          assertBool (show r) (any ("python" `isInfixOf`) r)
    , testCase "bindings built with other sanitize settings are reported" $
        withHome "stampsan" [] $ \home -> do
          writeEmbeddedHeader home
          writeBuildId home "aaaa"
          writeMarkers home "aaaa" False id
          r <- mismatches True (map T.pack ["py"]) home
          assertBool (show r) (any ("python" `isInfixOf`) r)
    , testCase "a language the program uses with no configured binding is reported" $
        withHome "stampabsent" [] $ \home -> do
          writeEmbeddedHeader home
          writeBuildId home "aaaa"
          r <- mismatches False (map T.pack ["r"]) home
          assertBool (show r) (any ("R" `isInfixOf`) r)
    , testCase "a home with no recorded runtime build id is reported" $
        withHome "stampnoid" [] $ \home -> do
          writeEmbeddedHeader home
          r <- mismatches False [] home
          assertBool (show r) (not (null r))
    , testCase "a home whose header differs from this compiler's is reported" $
        withHome "stampheader" [] $ \home -> do
          writeHeader home abiVersion
          writeBuildId home "aaaa"
          writeMarkers home "aaaa" False id
          r <- mismatches False allLangs home
          assertBool (show r) (any ("morloc.h" `isInfixOf`) r)
    , testCase "spec-shim-3-1" $
        withHome "stamplang" [] $ \home -> do
          writeEmbeddedHeader home
          writeBuildId home "aaaa"
          writeMarkers home "aaaa" False (\(name, text) -> (name, if name == "python" then "other" else text))
          r <- mismatches False (map T.pack ["py"]) home
          assertBool (show r) (any ("python" `isInfixOf`) r)
    , testCase "a language the program does not use is not checked" $
        withHome "stampunused" [] $ \home -> do
          writeEmbeddedHeader home
          writeBuildId home "aaaa"
          writeMarkers home "aaaa" False (\(name, text) -> (name, if name == "R" then "other" else text))
          mismatches False (map T.pack ["py", "cpp"]) home >>= (@?= [])
    , testCase "the source search falls back to a candidate without install.sh" $
        withHome "searchold" ["old/Cargo.toml"] $ \root ->
          findRustDir [root </> "missing", root </> "old"] >>= (@?= (root </> "old"))
    , testCase "a runtime source without install.sh is refused, naming the source" $
        withHome "noscript" ["Cargo.toml"] $ \src -> do
          r <- runtimeInstallScript src
          case r of
            Left msg -> assertBool msg (src `isInfixOf` msg && "predates" `isInfixOf` msg)
            Right p -> assertFailure ("accepted " <> p)
    ]
  where
    libName = if SI.os == "darwin" then "libmorloc.dylib" else "libmorloc.so"
    allLangs = map DF.lsLang DF.langSetups
    fakeBuildId = "0123456789abcdef"
    writeBuildId home bid = do
      createDirectoryIfMissing True (home </> "lib")
      writeFile (home </> "lib" </> "morloc.build-id") (bid <> "\n")
    mismatches sanitize langs home = map mismatchText <$> installedRuntimeMismatches sanitize langs home
    writeMarkers home bid sanitize edit = do
      let dir = home </> "opt" </> "lang-configured"
      createDirectoryIfMissing True dir
      createDirectoryIfMissing True (home </> "lib")
      mapM_ (\f -> writeFile (home </> f) "") ["opt/pymorloc", "lib/librmorloc.so", "lib/libcppmorloc.a"]
      mapM_ (\ls -> let (name, text) = edit (DF.lsName ls, langMarkerText (T.pack bid) T.empty sanitize ls) in writeFile (dir </> name) text) DF.langSetups
    fakeNexus home code abi = do
      createDirectoryIfMissing True (home </> "bin")
      let f = home </> "bin" </> "morloc-nexus"
          body = unlines
            [ "#!/bin/sh"
            , "case \"$1\" in"
            , "  --build-id) echo 0123456789abcdef; exit " <> show (code :: Int) <> " ;;"
            , "  --abi-version) echo " <> abi <> " ;;"
            , "esac"
            ]
      callProcess "sh" ["-c", "printf '%s' \"$2\" > \"$1\" && chmod +x \"$1\"", "sh", f, body]
    writeEmbeddedHeader src = do
      createDirectoryIfMissing True (src </> "include")
      BS.writeFile (src </> "include" </> "morloc.h") (TE.encodeUtf8 DF.libmorlocHeader)
    writeHeader src n = do
      createDirectoryIfMissing True (src </> "include")
      writeFile (src </> "include" </> "morloc.h") ("#ifndef X\n#define MORLOC_ABI_VERSION " <> show n <> "\n#endif\n")
    withHome tag files act = do
      tmp <- getTemporaryDirectory
      pid <- getProcessID
      let root = tmp </> ("morloc-runtime-install-" <> show pid <> "-" <> tag)
      bracket
        (do removePathForcibly root
            mapM_ (\f -> createDirectoryIfMissing True (takeDirectory (root </> f)) >> writeFile (root </> f) "") files
            return root)
        removePathForcibly
        act

pathIsWithinTests :: TestTree
pathIsWithinTests =
  testGroup
    "pathIsWithin"
    [ testCase "the prefix itself is within" $
        assertBool "prefix == child" (pathIsWithin "/opt/env" "/opt/env")
    , testCase "a nested directory is within" $
        assertBool "child under prefix" (pathIsWithin "/opt/env" "/opt/env/bin")
    , testCase "a deeply nested directory is within" $
        assertBool "deep child" (pathIsWithin "/opt/env" "/opt/env/x86_64-conda/bin")
    , testCase "a host directory is NOT within" $
        assertBool "system path" (not (pathIsWithin "/opt/env" "/usr/bin"))
    , testCase "a name-sharing sibling is NOT within" $
        -- The load-bearing case: /opt/env2 must not be accepted just because it
        -- starts with the string "/opt/env".
        assertBool "sibling prefix" (not (pathIsWithin "/opt/env" "/opt/env2/bin"))
    , testCase "a parent directory is NOT within" $
        assertBool "parent" (not (pathIsWithin "/opt/env" "/opt"))
    ]

compilerIncoherenceTests :: TestTree
compilerIncoherenceTests =
  testGroup
    "compilerIncoherence"
    [ testCase "an env copy losing the PATH race is shadowed" $
        Shadowed @=? compilerIncoherence True False
    , testCase "a $CC naming a compiler outside the env is shadowed" $
        Shadowed @=? compilerIncoherence False True
    , testCase "both together are still shadowed" $
        Shadowed @=? compilerIncoherence True True
    , testCase "no env compiler and no $CC means the activation never ran" $
        -- A macOS conda env has no bin/gcc at all (its compiler is clang, reached
        -- only through $CC), so an unset $CC here is a failed activation, NOT a
        -- host copy shadowing anything -- and telling the user to reorder PATH
        -- would send them after a cause that does not exist.
        Unactivated @=? compilerIncoherence False False
    ]

-- | A conda prefix, a host directory and a second host directory standing in
-- for a package manager's bin (Homebrew), each with a bin/ of fake executables.
data Fixture = Fixture {fxPrefix :: FilePath, fxHost :: FilePath, fxBrew :: FilePath}

prefixBin, hostBin, brewBin :: Fixture -> FilePath
prefixBin fx = fxPrefix fx </> "bin"
hostBin fx = fxHost fx </> "bin"
brewBin fx = fxBrew fx </> "bin"

withFixture :: String -> (Fixture -> IO a) -> IO a
withFixture tag act = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  let root = tmp </> ("morloc-toolchain-" <> show pid <> "-" <> tag)
  bracket (setup root) (const (removePathForcibly root)) act
  where
    setup root = do
      removePathForcibly root
      createDirectoryIfMissing True root
      root' <- canonicalizePath root
      let fx = Fixture (root' </> "env prefix") (root' </> "host") (root' </> "brew")
      mapM_ (\(dir, names) -> mapM_ (executable (dir fx)) names)
        [ (prefixBin, ["cargo", "rustc", "cc", "c++", "x86_64-conda-linux-gnu-cc", "x86_64-conda-linux-gnu-c++", "x86_64-conda-linux-gnu-ar", "g++"])
        , (hostBin, ["cargo", "rustc", "cc", "c++", "ar", "ccache", "distcc", "g++"])
        , (brewBin, ["cc"])
        ]
      return fx
    executable dir name = do
      createDirectoryIfMissing True dir
      let f = dir </> name
      writeFile f "#!/bin/sh\n"
      perms <- getPermissions f
      setPermissions f (setOwnerExecutable True perms)

-- | A recorded Linux activation: absolute tools in the prefix, prefix bin first.
linuxEnv :: Fixture -> [(String, String)]
linuxEnv fx =
  [ ("PATH", prefixBin fx <> ":" <> hostBin fx)
  , ("CC", prefixBin fx </> "x86_64-conda-linux-gnu-cc")
  , ("CXX", prefixBin fx </> "x86_64-conda-linux-gnu-c++")
  , ("AR", prefixBin fx </> "x86_64-conda-linux-gnu-ar")
  , ("CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER", prefixBin fx </> "x86_64-conda-linux-gnu-cc")
  ]

set :: String -> String -> [(String, String)] -> [(String, String)]
set k v env = (k, v) : filter ((/= k) . fst) env

unset :: String -> [(String, String)] -> [(String, String)]
unset k = filter ((/= k) . fst)

linuxHost :: ToolchainHost
linuxHost = ToolchainHost Nothing (const (return (Just "x86_64-unknown-linux-gnu")))

-- | macOS, where the fixture's host cc stands in for /usr/bin/cc.
macHost :: Fixture -> ToolchainHost
macHost fx = ToolchainHost (Just (hostBin fx </> "cc")) (const (return (Just "x86_64-unknown-linux-gnu")))

outside :: ToolchainHost -> PoolToolchain -> Fixture -> [(String, String)] -> IO [(Maybe String, String, Maybe FilePath)]
outside host kind fx env =
  map (\t -> (btVar t, btValue t, btFound t)) <$> toolsOutsidePrefix host (fxPrefix fx) env kind

hostTool :: Fixture -> Maybe String -> String -> String -> (Maybe String, String, Maybe FilePath)
hostTool fx var value name = (var, value, Just (hostBin fx </> name))

toolsOutsidePrefixTests :: TestTree
toolsOutsidePrefixTests =
  testGroup
    "toolsOutsidePrefix"
    [ testCase "a recorded activation passes" $
        withFixture "clean" $ \fx ->
          outside linuxHost RustPoolToolchain fx (linuxEnv fx) >>= (@?= [])
    , testCase "a $CC naming no program is reported as not found" $
        withFixture "bare-missing" $ \fx ->
          outside linuxHost RustPoolToolchain fx (set "CC" "arm64-apple-darwin20.0.0-clang" (linuxEnv fx))
            >>= (@?= [(Just "CC", "arm64-apple-darwin20.0.0-clang", Nothing)])
    , testCase "a bare $CC found through the prefix's bin passes" $
        withFixture "bare-found" $ \fx ->
          outside linuxHost RustPoolToolchain fx (set "CC" "x86_64-conda-linux-gnu-cc" (linuxEnv fx)) >>= (@?= [])
    , testCase "a host cargo and rustc ahead on PATH are reported" $
        withFixture "host-cargo" $ \fx ->
          outside linuxHost RustPoolToolchain fx (set "PATH" (hostBin fx <> ":" <> prefixBin fx) (linuxEnv fx))
            >>= (@?= [hostTool fx Nothing "cargo" "cargo", hostTool fx Nothing "rustc" "rustc"])
    , testCase "$RUSTC, then $CARGO_BUILD_RUSTC, replace rustc on PATH" $
        withFixture "rustc-var" $ \fx -> do
          let hostRustc = hostBin fx </> "rustc"
          outside linuxHost RustPoolToolchain fx (set "RUSTC" hostRustc (linuxEnv fx))
            >>= (@?= [hostTool fx (Just "RUSTC") hostRustc "rustc"])
          outside linuxHost RustPoolToolchain fx (set "CARGO_BUILD_RUSTC" hostRustc (linuxEnv fx))
            >>= (@?= [hostTool fx (Just "CARGO_BUILD_RUSTC") hostRustc "rustc"])
          outside linuxHost RustPoolToolchain fx (set "RUSTC" (prefixBin fx </> "rustc") (set "CARGO_BUILD_RUSTC" hostRustc (linuxEnv fx)))
            >>= (@?= [])
    , testCase "cargo runs $RUSTC as one program, even an empty one" $
        withFixture "rustc-words" $ \fx -> do
          outside linuxHost RustPoolToolchain fx (set "RUSTC" "sccache rustc" (linuxEnv fx))
            >>= (@?= [(Just "RUSTC", "sccache rustc", Nothing)])
          outside linuxHost RustPoolToolchain fx (set "RUSTC" "" (linuxEnv fx))
            >>= (@?= [(Just "RUSTC", "", Nothing)])
    , testCase "cc-rs's per-target compiler is the one checked" $
        withFixture "target-cc" $ \fx -> do
          let hostCc = hostBin fx </> "cc"
          outside linuxHost RustPoolToolchain fx (set "CC_x86_64-unknown-linux-gnu" hostCc (linuxEnv fx))
            >>= (@?= [hostTool fx (Just "CC_x86_64-unknown-linux-gnu") hostCc "cc"])
          outside linuxHost RustPoolToolchain fx (set "HOST_CC" hostCc (linuxEnv fx))
            >>= (@?= [hostTool fx (Just "HOST_CC") hostCc "cc"])
          -- The per-target form wins, so a host $CC under it is never run.
          let cc = prefixBin fx </> "x86_64-conda-linux-gnu-cc"
          outside linuxHost RustPoolToolchain fx (set "CC_x86_64_unknown_linux_gnu" cc (set "CC" hostCc (linuxEnv fx)))
            >>= (@?= [])
    , testCase "variables this build never reads are ignored" $
        withFixture "unread" $ \fx -> do
          let hostCc = hostBin fx </> "cc"
              env =
                set "CC_FOR_BUILD" hostCc
                  . set "TARGET_CC" hostCc
                  . set "CC_aarch64-unknown-linux-gnu" hostCc
                  . set "CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER" hostCc
                  $ linuxEnv fx
          outside linuxHost RustPoolToolchain fx env >>= (@?= [])
    , testCase "an unset $AR falls back to ar on PATH" $
        withFixture "no-ar" $ \fx ->
          outside linuxHost RustPoolToolchain fx (unset "AR" (linuxEnv fx))
            >>= (@?= [hostTool fx Nothing "ar" "ar"])
    , testCase "a compiler with flags or behind ccache is judged by the compiler" $
        withFixture "wrapped" $ \fx -> do
          let cc = prefixBin fx </> "x86_64-conda-linux-gnu-cc"
          outside linuxHost RustPoolToolchain fx (set "CC" "x86_64-conda-linux-gnu-cc -m64" (linuxEnv fx)) >>= (@?= [])
          -- cc-rs splits a value at whitespace unless the whole value is a file,
          -- so flags after a path with a space break the build.
          outside linuxHost RustPoolToolchain fx (set "CC" (cc <> " -m64") (linuxEnv fx))
            >>= (@?= [(Just "CC", cc <> " -m64", Nothing)])
          -- ... but trims it first.
          outside linuxHost RustPoolToolchain fx (set "CC" (cc <> " ") (linuxEnv fx)) >>= (@?= [])
          outside linuxHost RustPoolToolchain fx (set "CC" "ccache x86_64-conda-linux-gnu-cc" (linuxEnv fx)) >>= (@?= [])
          outside linuxHost RustPoolToolchain fx (set "CC" "ccache cc" (set "PATH" (hostBin fx) (linuxEnv fx)))
            >>= (@?= [ hostTool fx Nothing "cargo" "cargo"
                     , hostTool fx Nothing "rustc" "rustc"
                     , hostTool fx (Just "CC") "ccache cc" "cc" ])
    , testCase "a cargo linker outside the prefix is reported" $
        withFixture "linker" $ \fx -> do
          let hostCc = hostBin fx </> "cc"
          outside linuxHost RustPoolToolchain fx (set "CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER" hostCc (linuxEnv fx))
            >>= (@?= [hostTool fx (Just "CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER") hostCc "cc"])
    , testCase "with no linker variable, cc links: the prefix's on Linux, also the system's on macOS" $
        withFixture "default-linker" $ \fx -> do
          let env = unset "CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER" (linuxEnv fx)
          outside linuxHost RustPoolToolchain fx env >>= (@?= [])
          -- Only a foreign cc ahead of the prefix: cargo and rustc stay the
          -- environment's, so the triple, and with it the linker, is known.
          let brewFirst = set "PATH" (brewBin fx <> ":" <> prefixBin fx) env
              brewCc = [(Nothing, "cc", Just (brewBin fx </> "cc"))]
          outside linuxHost RustPoolToolchain fx brewFirst >>= (@?= brewCc)
          outside (macHost fx) RustPoolToolchain fx brewFirst >>= (@?= brewCc)
          let brewIsSystem = (macHost fx) {thSystemLinker = Just (brewBin fx </> "cc")}
          outside brewIsSystem RustPoolToolchain fx brewFirst >>= (@?= [])
    , testCase "a Rust pool checks cc-rs's default C++ compiler" $
        withFixture "no-cxx" $ \fx -> do
          let env = unset "CXX" (linuxEnv fx)
          outside linuxHost RustPoolToolchain fx env >>= (@?= [])
          outside linuxHost RustPoolToolchain fx (set "PATH" (hostBin fx <> ":" <> prefixBin fx) env)
            >>= (@?= [hostTool fx Nothing "cargo" "cargo", hostTool fx Nothing "rustc" "rustc", hostTool fx Nothing "c++" "c++"])
    , testCase "a blank value is read as each build reads it" $
        withFixture "blank" $ \fx -> do
          -- cc-rs: the first variable set wins, and a blank one means cc.
          let cc = prefixBin fx </> "x86_64-conda-linux-gnu-cc"
              brewFirst = set "PATH" (brewBin fx <> ":" <> prefixBin fx)
          outside linuxHost RustPoolToolchain fx (set "CC" "  " (linuxEnv fx)) >>= (@?= [])
          outside linuxHost RustPoolToolchain fx (brewFirst (set "HOST_CC" " " (set "CC" cc (linuxEnv fx))))
            >>= (@?= [(Nothing, "cc", Just (brewBin fx </> "cc"))])
          -- The shell: ${CXX:-g++} falls back on empty, but blanks run nothing.
          outside linuxHost CppPoolToolchain fx (set "CXX" "" (linuxEnv fx)) >>= (@?= [])
          outside linuxHost CppPoolToolchain fx (set "CXX" " " (linuxEnv fx)) >>= (@?= [(Just "CXX", " ", Nothing)])
    , testCase "a local compiler cache is skipped, a remote compiler is judged as itself" $
        withFixture "remote" $ \fx -> do
          outside linuxHost CppPoolToolchain fx (set "CXX" "ccache g++" (linuxEnv fx)) >>= (@?= [])
          outside linuxHost CppPoolToolchain fx (set "CXX" "distcc g++" (linuxEnv fx))
            >>= (@?= [(Just "CXX", "distcc g++", Just (hostBin fx </> "distcc"))])
    , testCase "a prefix rustc that names no host triple is reported" $
        withFixture "no-host" $ \fx -> do
          let noTriple = ToolchainHost Nothing (const (return Nothing))
          outside noTriple RustPoolToolchain fx (linuxEnv fx)
            >>= (@?= [(Nothing, "rustc", Just (prefixBin fx </> "rustc"))])
    , testCase "the target triple is asked only of the prefix's rustc" $
        withFixture "triple-ask" $ \fx -> do
          let asked = ToolchainHost Nothing (\r -> if r == prefixBin fx </> "rustc" then return (Just "x86_64-unknown-linux-gnu") else assertFailure ("asked " <> r) >> return Nothing)
          outside asked RustPoolToolchain fx (linuxEnv fx) >>= (@?= [])
          outside asked RustPoolToolchain fx (set "RUSTC" (hostBin fx </> "rustc") (linuxEnv fx))
            >>= (@?= [hostTool fx (Just "RUSTC") (hostBin fx </> "rustc") "rustc"])
    , testCase "macOS's system linker passes when named, by any spelling" $
        withFixture "mac-linker" $ \fx -> do
          let hostCc = hostBin fx </> "cc"
          outside (macHost fx) RustPoolToolchain fx (set "CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER" hostCc (linuxEnv fx))
            >>= (@?= [])
          outside (macHost fx) RustPoolToolchain fx (set "CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER" (hostBin fx </> "." </> "cc") (linuxEnv fx))
            >>= (@?= [])
          outside (macHost fx) RustPoolToolchain fx (set "CC" hostCc (linuxEnv fx))
            >>= (@?= [hostTool fx (Just "CC") hostCc "cc"])
    , testCase "a C guest compile uses $CC, else cc on PATH" $
        withFixture "c-guest" $ \fx -> do
          outside linuxHost CGuestToolchain fx (set "CC" "x86_64-conda-linux-gnu-cc" (linuxEnv fx)) >>= (@?= [])
          outside linuxHost CGuestToolchain fx (set "PATH" (hostBin fx) (unset "CC" (linuxEnv fx)))
            >>= (@?= [hostTool fx Nothing "cc" "cc"])
    , testCase "a C++ pool with no $CXX uses g++ on PATH" $
        withFixture "cpp-default" $ \fx -> do
          let env = unset "CXX" (linuxEnv fx)
          outside linuxHost CppPoolToolchain fx env >>= (@?= [])
          outside linuxHost CppPoolToolchain fx (set "PATH" (hostBin fx) env)
            >>= (@?= [hostTool fx Nothing "g++" "g++"])
    , testCase "a C++ pool's $CXX is split as the shell splits it" $
        withFixture "cpp-split" $ \fx -> do
          outside linuxHost CppPoolToolchain fx (set "CXX" "x86_64-conda-linux-gnu-c++ -m64" (linuxEnv fx)) >>= (@?= [])
          let cxx = prefixBin fx </> "x86_64-conda-linux-gnu-c++"
          outside linuxHost CppPoolToolchain fx (linuxEnv fx) >>= (@?= [(Just "CXX", cxx, Nothing)])
          -- Only space, tab and newline split.
          outside linuxHost CppPoolToolchain fx (set "CXX" "g++\r" (linuxEnv fx)) >>= (@?= [(Just "CXX", "g++\r", Nothing)])
    , testCase "a prefix tool linked into a shared package store passes" $
        withFixture "store-link" $ \fx -> do
          let linked = prefixBin fx </> "linked-cc"
          createFileLink (hostBin fx </> "cc") linked
          outside linuxHost RustPoolToolchain fx (set "CC" linked (linuxEnv fx)) >>= (@?= [])
    ]
