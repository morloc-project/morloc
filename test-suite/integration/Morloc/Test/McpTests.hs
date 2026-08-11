module Morloc.Test.McpTests (mcpTests) where

import Control.Monad (forM_, when)
import Data.List (isInfixOf, isSuffixOf)
import System.Directory (copyFile, doesFileExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

import Morloc.Test.Common

-- ======================================================================
-- The MCP server (`morloc-nexus mcp <target>`) exposes a compiled
-- program's exported functions as MCP tools over stdio JSON-RPC. These
-- tests drive it through the suite's stdio JSON-RPC client
-- (test-suite/mcp-tests/mcp_client.py) -- the same client the standalone
-- ./run-tests.sh uses -- so the tricky session/handshake logic has one
-- implementation. The focus is on how docstring / type features map to
-- the tool surface (tools/list) and how MCP named arguments invert to the
-- positional call (tools/call). The comprehensive matrix lives in
-- test-suite/mcp-tests/README.md; this is a representative subset that
-- runs under `stack test`.
-- ======================================================================

-- | Compile an mcp-tests .loc program into a fresh temp dir (copying every
-- .py, including the client). Returns the work dir; the compiled wrapper is
-- at <workDir>/nexus and the client at <workDir>/mcp_client.py.
compileMcpProgram :: TestEnv -> String -> IO FilePath
compileMcpProgram env locFile = do
  let srcDir = teSuiteDir env </> "mcp-tests"
  tmpBase <- getCanonicalTemporaryDirectory
  tmpDir <- createTempDirectory tmpBase "morloc-mcp"
  copyFile (srcDir </> locFile) (tmpDir </> locFile)
  entries <- listDirectory srcDir
  forM_ entries $ \f -> do
    let src = srcDir </> f
    isFile <- doesFileExist src
    when (isFile && ".py" `isSuffixOf` f) $ copyFile src (tmpDir </> f)
  (ec, _, err) <- morlocMake tmpDir "nexus" locFile
  case ec of
    ExitSuccess -> return tmpDir
    ExitFailure c ->
      assertFailure $ locFile ++ ": compile failed (exit " ++ show c ++ "):\n" ++ err

-- | Run the stdio client. A non-zero exit is a transport/protocol failure
-- (the client exits 3 on any non-JSON byte on the protocol stream).
runClient :: FilePath -> [String] -> String -> IO String
runClient workDir args stdinData = do
  (ec, out, err) <-
    readProcessWithExitCode "python3" ((workDir </> "mcp_client.py") : args) stdinData
  case ec of
    ExitSuccess -> return (strip out)
    ExitFailure c ->
      assertFailure $
        "mcp_client " ++ unwords args ++ " failed (exit " ++ show c ++ "):\n" ++ err

target :: FilePath -> FilePath
target workDir = workDir </> "nexus"

-- | tools/list -> the tools array (JSON).
mcpList :: FilePath -> IO String
mcpList wd = runClient wd ["list", target wd] ""

-- | tools/call -> the result object (JSON), or {"error":...} for a
-- protocol error.
mcpCall :: FilePath -> String -> String -> IO String
mcpCall wd tool args = runClient wd ["call", target wd, tool, args] ""

-- | Send raw JSON-RPC messages (no auto handshake); one message per input
-- line. Returns {"responses":[...]} for the id-bearing messages.
mcpRaw :: FilePath -> String -> IO String
mcpRaw wd msgs = runClient wd ["raw", target wd] msgs

-- | Extract a value from JSON via the client's `jget` helper. `expr` is a
-- Python expression over `d` (the parsed value) plus `tools`/`t(name)`/
-- `names` when it is a tools array.
jget :: FilePath -> String -> String -> IO String
jget wd json expr = runClient wd ["jget", expr] json

assertJget :: String -> FilePath -> String -> String -> String -> Assertion
assertJget label wd json expr expected = do
  actual <- jget wd json expr
  assertEqual label expected actual

-- ======================================================================
-- Group: shape -- docstring/type -> tools/list
-- ======================================================================

shapeTests :: TestEnv -> TestTree
shapeTests env = testCase "tools/list shape (docstring -> MCP)" $ do
  wd <- compileMcpProgram env "shapes.loc"
  tools <- mcpList wd

  assertJget "all exports servable" wd tools "len(tools)" "11"
  assertContains "name: override -> hello" "\"hello\"" tools
  assertBool "original name greet absent" (not ("\"greet\"" `isInfixOf` tools))
  assertJget "command doc -> description" wd tools "t('addOne')['description']" "Add one to an integer"

  -- positionals -> reserved indexed keys
  assertJget "positional -> _1" wd tools
    "list(t('addOne')['inputSchema']['properties'].keys())[0]" "_1"
  assertJget "variadic positional -> _1" wd tools
    "list(t('sumMany')['inputSchema']['properties'].keys())[0]" "_1"

  -- scalar types
  assertJget "Int -> integer" wd tools
    "t('addOne')['inputSchema']['properties']['_1']['type']" "integer"
  assertJget "Str -> string" wd tools
    "t('hello')['inputSchema']['properties']['_1']['type']" "string"

  -- optional / required
  assertJget "?Int not required" wd tools "t('maybeDouble')['inputSchema']['required']" "[]"
  assertJget "?Int nullable" wd tools
    "t('maybeDouble')['inputSchema']['properties']['_1']['type']" "[\"integer\",\"null\"]"

  -- variadic
  assertJget "many -> array" wd tools
    "t('sumMany')['inputSchema']['properties']['_1']['type']" "array"
  assertJget "many items -> integer" wd tools
    "t('sumMany')['inputSchema']['properties']['_1']['items']['type']" "integer"

  -- top-level option (arg: + default:) -> non-required
  assertJget "positional+option required=[_1]" wd tools
    "t('scale')['inputSchema']['required']" "[\"_1\"]"
  assertJget "option default in description" wd tools
    "t('scale')['inputSchema']['properties']['factor'].get('description','')" "(default: 2)"

  -- unrolled record -> flat per-field; defaulted fields NOT required
  assertJget "unrolled field count" wd tools
    "len(t('configFlat')['inputSchema']['properties'])" "3"
  assertJget "unrolled defaulted fields not required" wd tools
    "t('configFlat')['inputSchema']['required']" "[]"
  assertJget "unrolled Bool field -> boolean" wd tools
    "t('configFlat')['inputSchema']['properties']['removeCaches']['type']" "boolean"

  -- record arg: -> whole-object property named for the type
  assertJget "group_opt single property" wd tools
    "len(t('configWhole')['inputSchema']['properties'])" "1"
  assertJget "group_opt property type object" wd tools
    "t('configWhole')['inputSchema']['properties']['configwhole']['type']" "object"

  -- returns
  assertJget "scalar return -> no outputSchema" wd tools
    "'PRESENT' if 'outputSchema' in t('addOne') else 'MISSING'" "MISSING"
  assertJget "record return -> object outputSchema" wd tools
    "t('mkResult')['outputSchema']['type']" "object"

-- ======================================================================
-- Group: call -- named args -> positional dispatch -> result
-- ======================================================================

callTests :: TestEnv -> TestTree
callTests env = testCase "tools/call inversion + result shaping" $ do
  wd <- compileMcpProgram env "shapes.loc"

  r1 <- mcpCall wd "addOne" "{\"_1\":41}"
  assertJget "addOne{41} -> 42" wd r1 "d['content'][0]['text']" "42"
  assertJget "addOne not isError" wd r1 "d.get('isError',False)" "false"

  r2 <- mcpCall wd "sumMany" "{\"_1\":[1,2,3,4]}"
  assertJget "sumMany variadic -> 10" wd r2 "d['content'][0]['text']" "10"

  r3 <- mcpCall wd "scale" "{\"_1\":10}"
  assertJget "scale default factor -> 20" wd r3 "d['content'][0]['text']" "20"
  r3b <- mcpCall wd "scale" "{\"_1\":10,\"factor\":5}"
  assertJget "scale factor 5 -> 50" wd r3b "d['content'][0]['text']" "50"

  r4 <- mcpCall wd "maybeDouble" "{}"
  assertJget "maybeDouble{} omitted -> null" wd r4 "d['content'][0]['text']" "null"

  -- unrolled record: omitted fields fall back to typed defaults
  r5 <- mcpCall wd "configFlat" "{}"
  assertJget "configFlat{} -> all defaults" wd r5 "d['content'][0]['text']" "\"/tmp:1:False\""
  r6 <- mcpCall wd "configFlat" "{\"tmpDir\":\"/x\",\"numThreads\":9,\"removeCaches\":true}"
  assertJget "configFlat overrides" wd r6 "d['content'][0]['text']" "\"/x:9:True\""

  -- whole-object record passed under the type-named property
  r7 <- mcpCall wd "configWhole" "{\"configwhole\":{\"tmpDir\":\"/y\",\"numThreads\":3,\"removeCaches\":false}}"
  assertJget "configWhole whole-object" wd r7 "d['content'][0]['text']" "\"/y:3:False\""

  -- record return -> structuredContent mirrors the object
  r8 <- mcpCall wd "mkResult" "{\"_1\":7}"
  assertJget "mkResult structuredContent.value" wd r8 "d['structuredContent']['value']" "7"

  -- pure command (no pool)
  r9 <- mcpCall wd "pureAnswer" "{}"
  assertJget "pure command -> 42" wd r9 "d['content'][0]['text']" "42"

-- ======================================================================
-- Group: error -- protocol errors vs isError results
-- ======================================================================

errorTests :: TestEnv -> TestTree
errorTests env = testCase "error handling" $ do
  wd <- compileMcpProgram env "shapes.loc"

  -- @throw -> a result with isError:true (not a protocol error)
  r1 <- mcpCall wd "mustFail" "{\"_1\":1}"
  assertJget "@throw -> isError result" wd r1 "d.get('isError',False)" "true"
  assertContains "@throw message surfaced" "boom" r1

  -- unknown tool / unknown arg / missing required -> -32602
  r2 <- mcpCall wd "noSuchTool" "{}"
  assertJget "unknown tool -> -32602" wd r2 "d['error']['code']" "-32602"
  r3 <- mcpCall wd "addOne" "{\"bogus\":1}"
  assertJget "unknown argument -> -32602" wd r3 "d['error']['code']" "-32602"
  r4 <- mcpCall wd "addOne" "{}"
  assertJget "missing required -> -32602" wd r4 "d['error']['code']" "-32602"

-- ======================================================================
-- Group: protocol -- handshake gating, ping, unknown method
-- ======================================================================

protocolTests :: TestEnv -> TestTree
protocolTests env = testCase "JSON-RPC lifecycle" $ do
  wd <- compileMcpProgram env "shapes.loc"

  let handshakeSeq =
        unlines
          [ "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"
          , "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{}}}"
          , "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
          , "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/list\"}"
          ]
  s1 <- mcpRaw wd handshakeSeq
  assertJget "tools/list before init -> error" wd s1
    "'PRESENT' if 'error' in d['responses'][0] else 'MISSING'" "PRESENT"
  assertJget "initialize echoes version" wd s1
    "d['responses'][1]['result']['protocolVersion']" "2025-06-18"
  assertJget "notification silent (3 id'd replies)" wd s1 "len(d['responses'])" "3"
  assertJget "tools/list after init -> ok" wd s1
    "'PRESENT' if 'result' in d['responses'][2] else 'MISSING'" "PRESENT"

  -- ping before init; id echoed verbatim (int + string)
  let pingSeq =
        unlines
          [ "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"ping\"}"
          , "{\"jsonrpc\":\"2.0\",\"id\":\"abc\",\"method\":\"ping\"}"
          ]
  s2 <- mcpRaw wd pingSeq
  assertJget "ping before init answered" wd s2 "d['responses'][0]['id']" "7"
  assertJget "string id echoed" wd s2 "d['responses'][1]['id']" "abc"

  -- unknown method -> method not found
  s3 <- mcpRaw wd "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"does/notExist\"}\n"
  assertJget "unknown method -> -32601" wd s3 "d['responses'][0]['error']['code']" "-32601"

-- ======================================================================
-- Group: fd -- stray pool stdout must not corrupt the protocol stream
-- ======================================================================

fdTests :: TestEnv -> TestTree
fdTests env = testCase "fd-discipline (stray pool stdout)" $ do
  wd <- compileMcpProgram env "shapes.loc"
  -- noisyAdd's pool prints to stdout. A clean, correct result proves the
  -- stray bytes went to stderr (the client raises on any non-JSON byte on
  -- the protocol stream, so a corrupted stream would fail this call).
  r <- mcpCall wd "noisyAdd" "{\"_1\":41}"
  assertJget "noisy pool -> clean 42" wd r "d['content'][0]['text']" "42"
  assertBool "no stray bytes in result" (not ("STRAY-POOL-STDOUT" `isInfixOf` r))

-- ======================================================================
-- Group: exclude -- unservable commands dropped from tools/list
-- ======================================================================

excludeTests :: TestEnv -> TestTree
excludeTests env = testCase "tool-surface exclusion (@stdin)" $ do
  wd <- compileMcpProgram env "excluded.loc"
  tools <- mcpList wd
  assertContains "control plainAdd present" "plainAdd" tools
  assertBool "@stdin readStdin excluded" (not ("readStdin" `isInfixOf` tools))

-- ======================================================================
-- Top-level tree
-- ======================================================================

mcpTests :: TestEnv -> TestTree
mcpTests env =
  testGroup
    "MCP"
    [ shapeTests env
    , callTests env
    , errorTests env
    , protocolTests env
    , fdTests env
    , excludeTests env
    ]
