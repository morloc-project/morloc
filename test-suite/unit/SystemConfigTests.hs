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
  , pathIsWithin
  , toolsOutsidePrefix
  )
import Control.Exception (bracket)
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
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

systemConfigTests :: TestTree
systemConfigTests =
  testGroup
    "SystemConfig (conda coherence guard)"
    [ pathIsWithinTests
    , compilerIncoherenceTests
    , toolsOutsidePrefixTests
    ]

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
