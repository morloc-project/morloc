module Morloc.Test.DaemonTests (daemonTests) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import System.Directory (copyFile, doesFileExist, listDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import Morloc.Test.Common

-- ======================================================================
-- Test data compilation
-- ======================================================================

compileDaemonProgram :: TestEnv -> String -> IO FilePath
compileDaemonProgram env locFile = do
  let srcDir = teSuiteDir env </> "daemon-tests"
  tmpDir <- do
    tmpBase <- getCanonicalTemporaryDirectory
    createTempDirectory tmpBase "morloc-daemon"
  copyFile (srcDir </> locFile) (tmpDir </> locFile)
  entries <- listDirectory srcDir
  mapM_
    ( \f -> do
        let src = srcDir </> f
        isFile <- doesFileExist src
        if isFile && (hasSuffix ".py" f || hasSuffix ".R" f || hasSuffix ".hpp" f)
          then copyFile src (tmpDir </> f)
          else return ()
    )
    entries
  (ec, _, err) <- morlocMake tmpDir "nexus" locFile
  case ec of
    ExitSuccess -> return tmpDir
    ExitFailure c -> do
      assertFailure $ locFile ++ ": compile failed (exit " ++ show c ++ "):\n" ++ err
      return tmpDir

hasSuffix :: String -> String -> Bool
hasSuffix suf s = reverse suf == take (length suf) (reverse s)

-- | Wait for the daemon to be fully ready (health + discover responding)
waitForDaemonReady :: Int -> Int -> IO Bool
waitForDaemonReady port maxWaitMs = go 0
  where
    stepMs = 300
    go elapsed
      | elapsed >= maxWaitMs = return False
      | otherwise = do
          result <-
            try $ httpGet ("http://127.0.0.1:" ++ show port ++ "/discover") ::
              IO (Either SomeException (ExitCode, String, String))
          case result of
            Right (ExitSuccess, body, _)
              | length body > 10 -> return True
            _ -> do
              threadDelay (stepMs * 1000)
              go (elapsed + stepMs)

-- | Wait for a socket/TCP endpoint to respond
waitForSocket :: String -> Int -> IO Bool
waitForSocket target maxWaitMs = go 0
  where
    stepMs = 500
    go elapsed
      | elapsed >= maxWaitMs = return False
      | otherwise = do
          result <-
            try $ lpRequest target "{\"method\":\"health\"}" ::
              IO (Either SomeException String)
          case result of
            Right r | length r > 2 -> return True
            _ -> do
              threadDelay (stepMs * 1000)
              go (elapsed + stepMs)

-- ======================================================================
-- HTTP API tests
-- ======================================================================

httpTests :: TestEnv -> TestTree
httpTests env = testCase "HTTP API (arithmetic)" $ do
  arithDir <- compileDaemonProgram env "arithmetic.loc"
  port <- pickFreePort
  withDaemon arithDir ["--http-port", show port] $ \_ -> do
    ok <- waitForDaemonReady port 15000
    assertBool ("daemon did not become ready (port " ++ show port ++ ", dir " ++ arithDir ++ ")") ok

    -- Health
    (_, body, _) <- httpGet ("http://127.0.0.1:" ++ show port ++ "/health")
    assertJsonEq "health" body "status" "ok"

    -- Discovery
    (_, disco, _) <- httpGet ("http://127.0.0.1:" ++ show port ++ "/discover")
    assertContains "discover lists add" "add" disco
    assertContains "discover lists mul" "mul" disco
    assertContains "discover lists neg" "neg" disco
    assertContains "discover lists square" "square" disco

    -- add(3,4) -> 7
    (_, r1, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/add") "[3, 4]"
    assertJsonEq "add" r1 "status" "ok"
    assertJsonEq "add" r1 "result" "7"

    -- mul(5,6) -> 30
    (_, r2, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/mul") "[5, 6]"
    assertJsonEq "mul" r2 "result" "30"

    -- neg(42) -> -42
    (_, r3, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/neg") "[42]"
    assertJsonEq "neg" r3 "result" "-42"

    -- square(7) -> 49
    (_, r4, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/square") "[7]"
    assertJsonEq "square" r4 "result" "49"

    -- Args as object form
    (_, r5, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/add") "{\"args\": [10, 20]}"
    assertJsonEq "add object form" r5 "result" "30"

    -- Float args
    (_, r6, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/add") "[1.5, 2.5]"
    assertJsonEq "add float" r6 "result" "4"

    -- Unknown command
    (_, r7, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/nonexistent") "[1]"
    assertJsonEq "unknown command" r7 "status" "error"

httpPyTests :: TestEnv -> TestTree
httpPyTests env = testCase "HTTP API (Python strings)" $ do
  strDir <- compileDaemonProgram env "strings.loc"
  port <- pickFreePort
  withDaemon strDir ["--http-port", show port] $ \_ -> do
    ok <- waitForDaemonReady port 15000
    assertBool ("daemon did not start (port " ++ show port ++ ", dir " ++ strDir ++ ")") ok

    (_, r1, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/greet") "[\"world\"]"
    assertJsonEq "greet" r1 "status" "ok"
    assertJsonEq "greet" r1 "result" "Hello, world!"

    (_, r2, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/strlen") "[\"morloc\"]"
    assertJsonEq "strlen" r2 "result" "6"

    (_, r3, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/strlen") "[\"\"]"
    assertJsonEq "strlen empty" r3 "result" "0"

httpPureTests :: TestEnv -> TestTree
httpPureTests env = testCase "HTTP API (pure commands)" $ do
  pureDir <- compileDaemonProgram env "pure.loc"
  port <- pickFreePort
  withDaemon pureDir ["--http-port", show port] $ \_ -> do
    ok <- waitForDaemonReady port 15000
    assertBool ("daemon did not start (port " ++ show port ++ ", dir " ++ pureDir ++ ")") ok

    (_, r1, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/checkInt") "[]"
    assertJsonEq "checkInt" r1 "status" "ok"
    assertJsonEq "checkInt" r1 "result" "42"

    (_, r2, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/checkReal") "[]"
    assertJsonEq "checkReal" r2 "result" "3.14"

    (_, r3, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/checkBool") "[]"
    assertJsonEq "checkBool" r3 "result" "true"

    (_, r4, _) <- httpPost ("http://127.0.0.1:" ++ show port ++ "/call/checkStr") "[]"
    assertJsonEq "checkStr" r4 "result" "hello"

-- ======================================================================
-- Unix socket tests
-- ======================================================================

socketTests :: TestEnv -> TestTree
socketTests env = testCase "Unix socket API" $ do
  arithDir <- compileDaemonProgram env "arithmetic.loc"
  let sockPath = "/tmp/morloc-test-haskell-socket.sock"
  removeIfExists sockPath
  withDaemon arithDir ["--socket", sockPath] $ \_ -> do
    ok <- waitForSocket sockPath 15000
    assertBool ("socket daemon did not start (socket " ++ sockPath ++ ", dir " ++ arithDir ++ ")") ok

    r1 <- lpRequest sockPath "{\"method\":\"health\"}"
    assertJsonEq "socket health" r1 "status" "ok"

    r2 <- lpRequest sockPath "{\"method\":\"discover\"}"
    assertContains "socket discover" "add" r2

    r3 <- lpRequest sockPath "{\"method\":\"call\",\"command\":\"add\",\"args\":[10,20]}"
    assertJsonEq "socket add" r3 "status" "ok"
    assertJsonEq "socket add" r3 "result" "30"

    r4 <-
      lpRequest sockPath "{\"id\":\"req-42\",\"method\":\"call\",\"command\":\"mul\",\"args\":[3,7]}"
    assertJsonEq "socket mul" r4 "id" "req-42"
    assertJsonEq "socket mul" r4 "result" "21"

    r5 <- lpRequest sockPath "{\"method\":\"call\",\"command\":\"bogus\",\"args\":[1]}"
    assertJsonEq "socket unknown cmd" r5 "status" "error"

  removeIfExists sockPath

-- ======================================================================
-- TCP tests
-- ======================================================================

tcpTests :: TestEnv -> TestTree
tcpTests env = testCase "TCP API" $ do
  arithDir <- compileDaemonProgram env "arithmetic.loc"
  port <- pickFreePort
  withDaemon arithDir ["--port", show port] $ \_ -> do
    ok <- waitForSocket ("127.0.0.1:" ++ show port) 15000
    assertBool ("tcp daemon did not start (port " ++ show port ++ ", dir " ++ arithDir ++ ")") ok

    r1 <- lpRequest ("127.0.0.1:" ++ show port) "{\"method\":\"health\"}"
    assertJsonEq "tcp health" r1 "status" "ok"

    r2 <-
      lpRequest
        ("127.0.0.1:" ++ show port)
        "{\"method\":\"call\",\"command\":\"add\",\"args\":[100,200]}"
    assertJsonEq "tcp add" r2 "status" "ok"
    assertJsonEq "tcp add" r2 "result" "300"

    r3 <-
      lpRequest
        ("127.0.0.1:" ++ show port)
        "{\"method\":\"call\",\"command\":\"square\",\"args\":[9]}"
    assertJsonEq "tcp square" r3 "result" "81"

-- ======================================================================
-- Multi-listener tests
-- ======================================================================

multiListenerTests :: TestEnv -> TestTree
multiListenerTests env = testCase "Multi-listener (HTTP+TCP+socket)" $ do
  arithDir <- compileDaemonProgram env "arithmetic.loc"
  let sockPath = "/tmp/morloc-test-haskell-multi.sock"
  removeIfExists sockPath
  httpPort <- pickFreePort
  tcpPort <- pickFreePort
  withDaemon arithDir ["--socket", sockPath, "--port", show tcpPort, "--http-port", show httpPort] $ \_ -> do
    ok <- waitForDaemonReady httpPort 15000
    assertBool
      ( "multi daemon did not start (http="
          ++ show httpPort
          ++ ", tcp="
          ++ show tcpPort
          ++ ", dir "
          ++ arithDir
          ++ ")"
      )
      ok

    (_, r1, _) <- httpPost ("http://127.0.0.1:" ++ show httpPort ++ "/call/add") "[1, 2]"
    assertJsonEq "multi HTTP" r1 "result" "3"

    r2 <-
      lpRequest ("127.0.0.1:" ++ show tcpPort) "{\"method\":\"call\",\"command\":\"add\",\"args\":[1,2]}"
    assertJsonEq "multi TCP" r2 "result" "3"

    r3 <- lpRequest sockPath "{\"method\":\"call\",\"command\":\"add\",\"args\":[1,2]}"
    assertJsonEq "multi socket" r3 "result" "3"

  removeIfExists sockPath

-- ======================================================================
-- Sequential requests tests
-- ======================================================================

sequentialTests :: TestEnv -> TestTree
sequentialTests env = testCase "Sequential requests" $ do
  arithDir <- compileDaemonProgram env "arithmetic.loc"
  port <- pickFreePort
  withDaemon arithDir ["--http-port", show port] $ \_ -> do
    ok <- waitForDaemonReady port 15000
    assertBool ("daemon did not start (port " ++ show port ++ ", dir " ++ arithDir ++ ")") ok

    mapM_
      ( \i -> do
          (_, r, _) <-
            httpPost
              ("http://127.0.0.1:" ++ show port ++ "/call/add")
              ("[" ++ show i ++ ", " ++ show i ++ "]")
          assertJsonEq ("sequential add " ++ show i) r "result" (show (i + i))
      )
      [1 .. 10 :: Int]

-- ======================================================================
-- Concurrent requests tests
-- ======================================================================

concurrentHttpTests :: TestEnv -> TestTree
concurrentHttpTests env = testCase "Concurrent HTTP requests" $ do
  arithDir <- compileDaemonProgram env "arithmetic.loc"
  port <- pickFreePort
  withDaemon arithDir ["--http-port", show port] $ \_ -> do
    ok <- waitForDaemonReady port 15000
    assertBool ("daemon did not start (port " ++ show port ++ ", dir " ++ arithDir ++ ")") ok

    mapM_
      ( \i -> do
          (_, r, _) <-
            httpPost
              ("http://127.0.0.1:" ++ show port ++ "/call/square")
              ("[" ++ show i ++ "]")
          assertJsonEq ("concurrent square " ++ show i) r "result" (show (i * i))
      )
      [1 .. 5 :: Int]

-- ======================================================================
-- Graceful shutdown tests
-- ======================================================================

shutdownTests :: TestEnv -> TestTree
shutdownTests env = testCase "Graceful shutdown" $ do
  arithDir <- compileDaemonProgram env "arithmetic.loc"
  port <- pickFreePort
  let sockPath = "/tmp/morloc-test-haskell-shutdown.sock"
  removeIfExists sockPath

  withDaemon arithDir ["--http-port", show port, "--socket", sockPath] $ \_ -> do
    ok <- waitForDaemonReady port 15000
    assertBool ("daemon did not start (port " ++ show port ++ ")") ok

    (_, body, _) <- httpGet ("http://127.0.0.1:" ++ show port ++ "/health")
    assertJsonEq "alive before shutdown" body "status" "ok"

  threadDelay 500000
  sockExists <- doesFileExist sockPath
  assertBool "socket file should be removed after shutdown" (not sockExists)
  removeIfExists sockPath

-- ======================================================================
-- Pool health tests
-- ======================================================================

poolHealthTests :: TestEnv -> TestTree
poolHealthTests env = testCase "Health reports pool status" $ do
  arithDir <- compileDaemonProgram env "arithmetic.loc"
  port <- pickFreePort
  withDaemon arithDir ["--http-port", show port] $ \_ -> do
    ok <- waitForDaemonReady port 15000
    assertBool ("daemon did not start (port " ++ show port ++ ")") ok

    (_, body, _) <- httpGet ("http://127.0.0.1:" ++ show port ++ "/health")
    assertContains "health includes ok" "ok" body

-- ======================================================================
-- Helpers
-- ======================================================================

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  if exists then removeFile path else return ()

-- ======================================================================
-- Top-level test tree
-- ======================================================================

daemonTests :: TestEnv -> TestTree
daemonTests env =
  testGroup
    "Daemon"
    [ httpTests env
    , httpPyTests env
    , httpPureTests env
    , socketTests env
    , tcpTests env
    , multiListenerTests env
    , sequentialTests env
    , concurrentHttpTests env
    , shutdownTests env
    , poolHealthTests env
    ]
