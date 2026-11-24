{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Monad
Description : Compiler monad runner and accessors
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

'MorlocMonad' is the main effect stack used throughout the compiler:

@Reader Config (Except MorlocError (Writer [Text] (State MorlocState IO)))@

This module provides the runner ('runMorlocMonad'), error formatting
('makeMorlocError' with source snippets), state accessors (counter,
depth, metadata lookups), logging ('say'\/'sayV'\/'sayVV'), system call
wrappers, and an independent 'Index' monad for local re-indexing passes.
-}
module Morloc.Monad
  ( MorlocReturn
  , runMorlocMonad
  , runMorlocStack
  , writeMorlocReturn
  , printWarnings
  , makeMorlocError
  , warnAt
  , sourceAt
  , runCommand
  , runCommandWith
  , logFile
  , logFileWith
  , readLang

    -- * re-exports
  , module Control.Monad.Trans
  , module Control.Monad.Except
  , module Control.Monad.Reader
  , module Control.Monad.State
  , module Control.Monad.Writer
  , module Control.Monad.Identity

    -- * reusable counter
  , startCounter
  , freshManifoldIndex
  , getCounter
  , getCounterWithPos
  , setCounter
  , takeFromCounter

    -- * metadata accessors
  , metaSources
  , metaName
  , getDocStrings
  , getConcreteScope
  , getGeneralScope

    -- * handling tree depth
  , incDepth
  , getDepth
  , decDepth
  , setDepth

    -- * messages
  , say
  , sayV
  , sayVV
  , sayVVV

    -- * throwing errors
  , throwSourcedError
  , throwRejected
  , throwRejectedSource
  , recoverRejected
  , isDefect
  , throwFailed
  , throwUnificationError
  , throwCompilerBug
  , throwCompilerBugAt

    -- * naming helpers
  , getModuleName
  , getOutfileName
  , getProgramKey

    -- * Indexing monad
  , Index
  , IndexState (..)
  , runIndex
  , runIndexFrom
  , newIndex
  , getIndex
  , setIndex
  , registerSchemaIndex
  , getSchemaTable
  ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State.Strict as SS
import Control.Monad.Trans
import Control.Monad.Writer
import qualified Control.Monad.Trans.Writer.CPS as SW
import Data.Ord (comparing)
import Data.Text (Text)
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.DataFiles as DF
import qualified Morloc.LangRegistry as LR
import qualified Morloc.Language as ML
import Morloc.Namespace.Expr
import Morloc.Namespace.Prim
import Morloc.Namespace.State
import Morloc.Namespace.Type
import qualified Morloc.System as MS
import GHC.Stack (CallStack, HasCallStack, callStack, getCallStack, srcLocFile, srcLocStartLine)
import qualified System.Exit as SE
import System.IO (stderr)
import qualified System.Process as SP

runMorlocMonad ::
  Maybe Path -> Int -> Config -> BuildConfig -> MorlocMonad a -> IO (MorlocReturn a)
runMorlocMonad outfile v config buildConfig ev = do
  let langFiles = [(n, DF.embededFileText f) | (n, f) <- DF.langRegistryFiles]
      languagesText = DF.embededFileText DF.languagesYaml
      registry = case LR.buildDefaultRegistry langFiles languagesText of
        Right r -> r
        Left err -> error $ "Failed to build language registry: " ++ err
      state0 = emptyState outfile v
      state1 =
        state0
          { stateBuildConfig = buildConfig
          , stateLangRegistry = registry
          }
  runMorlocStack config state1 ev

-- | Run a 'MorlocMonad' action from a given config and state
runMorlocStack :: Config -> MorlocState -> MorlocMonad a -> IO (MorlocReturn a)
runMorlocStack config st ev =
  SS.runStateT (SW.runWriterT (runExceptT (runReaderT ev config))) st

emptyState :: Maybe Path -> Int -> MorlocState
emptyState path v =
  defaultValue
    { stateVerbosity = v
    , stateOutfile = path
    }

-- | Start code generation: the manifold allocator is seeded past every
-- expression index handed out so far. Variable indices continue the same
-- sequence, so an expression, a manifold and a variable never share an index.
startCounter :: MorlocMonad ()
startCounter = seedManifoldCounter

-- | Move the manifold allocator past every expression index handed out
-- so far, so a manifold created by code generation never shares an index
-- with one the frontend named.
seedManifoldCounter :: MorlocMonad ()
seedManifoldCounter = do
  s <- get
  put $! s {stateManifoldCounter = max (stateManifoldCounter s) (stateCounter s + 1)}

-- | A fresh manifold index, carrying the source position of a parent
-- index so diagnostics can locate the manifold.
freshManifoldIndex :: Int -> MorlocMonad Int
freshManifoldIndex parentIdx = do
  seedManifoldCounter
  s <- get
  let i = stateManifoldCounter s
      srcs = case Map.lookup parentIdx (stateSourceMap s) of
        Just loc -> Map.insert i loc (stateSourceMap s)
        Nothing -> stateSourceMap s
  put $! s {stateManifoldCounter = i + 1, stateSourceMap = srcs}
  return $! i

-- | A fresh index, above every expression, manifold and variable index
-- handed out so far.
getCounter :: MorlocMonad Int
getCounter = do
  s <- get
  let i = max (stateCounter s) (stateManifoldCounter s)
  put $! s {stateCounter = i + 1}
  return $! i

-- | Create a new index that inherits the source position of a parent index
getCounterWithPos :: Int -> MorlocMonad Int
getCounterWithPos parentIdx = do
  i <- getCounter
  s <- get
  case Map.lookup parentIdx (stateSourceMap s) of
    Just loc -> put $! s {stateSourceMap = Map.insert i loc (stateSourceMap s)}
    Nothing -> return ()
  return i

takeFromCounter :: Int -> MorlocMonad [Int]
takeFromCounter 0 = return []
takeFromCounter i = do
  x <- getCounter
  xs <- takeFromCounter (i - 1)
  return (x : xs)

setCounter :: Int -> MorlocMonad ()
setCounter i = do
  s <- get
  put $! s {stateCounter = i}
  return ()

incDepth :: MorlocMonad Int
incDepth = do
  s <- get
  let i = stateDepth s + 1
  put $! s {stateDepth = i}
  return i

getDepth :: MorlocMonad Int
getDepth = gets stateDepth

decDepth :: MorlocMonad Int
decDepth = do
  s <- get
  let i = stateDepth s - 1
  put $! s {stateDepth = i}
  return i

setDepth :: Int -> MorlocMonad ()
setDepth i = do
  s <- get
  put $! s {stateDepth = i}
  return ()

-- | Print a run's warnings to stderr.
printWarnings :: [MT.Text] -> IO ()
printWarnings msgs = unless (null msgs) $ MT.hPutStrLn stderr (MT.unlines msgs)

-- | Print a compilation's accumulated messages and, on failure, its error.
--
-- Messages are written on both branches. They are warnings -- an
-- unrecognized docstring directive, a deprecated spelling -- and a warning
-- that only appears when the build also fails is a warning no one ever
-- reads: the build it describes is the one that succeeded and shipped a
-- silently different interface.
writeMorlocReturn :: MorlocReturn a -> IO Bool
writeMorlocReturn ((result, msgs), st) = do
  printWarnings msgs
  case result of
    Left err' -> do
      MT.hPutStrLn stderr (render $ makeMorlocError st err')
      return False
    Right _ -> return True

-- | A warning, rendered at the source position of an expression index.
warnAt :: Int -> MDoc -> MorlocMonad ()
warnAt i msg = do
  st <- get
  tell
    [ render $ case Map.lookup i (stateSourceMap st) of
        Just loc -> pretty loc <> ": warning:" <> line <> msg <> snippet st loc
        Nothing -> "warning:" <+> msg
    ]

makeMorlocError :: MorlocState -> MorlocError -> MDoc
makeMorlocError st e = case e of
  Rejected loc msg -> located loc msg
  EnvFailed msg -> msg
  Fault Nothing msg -> msg
  Fault (Just loc) msg -> located loc msg
  where
    srcMap = stateSourceMap st
    noteAt i msg = maybe msg (\note -> note <> line <> msg) (Map.lookup i (stateErrorNotes st))
    located (AtNode i) msg = case Map.lookup i srcMap of
      Just loc -> pretty loc <> ": error:" <> line <> noteAt i (sourceNames msg) <> snippet st loc
      Nothing -> "Compiler bug, broken index" <+> pretty i <+> "with attached error:" <+> msg
    located (AtUnification lhs rhs context) msg =
      case (Map.lookup lhs srcMap, Map.lookup rhs srcMap, Map.lookup context srcMap) of
        (Just lhsLoc, rhsLoc, contextLoc) ->
          maybe mempty (<> line) note
            <> "Unification error:" <+> sourceNames msg
            <> line
            <> "Found while unifying" <+> maybe mempty pretty contextLoc
            <> line
            <> "With values"
            <> line
            <> snippet st lhsLoc
            <> maybe mempty (\l -> "and" <> line <> snippet st l) rhsLoc
        _ -> "Compiler bug, broken indices" <+> pretty (lhs, rhs, context) <+> "with attached error:" <+> msg
      where
        note = listToMaybe (mapMaybe (`Map.lookup` stateErrorNotes st) [lhs, rhs, context])
    -- a parse error is rendered with its snippet where it is raised
    located (AtSource _) msg = msg
    located Unplaced msg = msg

-- | A message with each variable the typechecker renamed (@a\@q0@, @*a\@q3@,
-- @n\@n5@) written as the name the program gave it, where nothing else in
-- the message is written that way.
sourceNames :: MDoc -> MDoc
sourceNames msg =
  let text = render msg
      pieces = splitNames (MT.unpack text)
      renamed = [(t, b) | Renamed t b <- pieces]
      plain = [w | Plain w <- pieces]
      alone b = length (nubOrd [t | (t, b') <- renamed, b' == b]) == 1 && b `notElem` plain
      out p = case p of
        Renamed t b -> if alone b then b else t
        Plain w -> w
        Other c -> [c]
   in if null renamed then msg else vsep (map pretty (MT.lines (MT.pack (concatMap out pieces))))

data NamePiece = Renamed String String | Plain String | Other Char

-- | Split text into identifiers, noting those with a renaming suffix, and
-- the characters between them.
splitNames :: String -> [NamePiece]
splitNames [] = []
splitNames s@(c : cs)
  | c == '*' || isStart c =
      let (star, rest0) = if c == '*' then ("*", cs) else ("", s)
          (ident, rest1) = span isIdent rest0
          (suffix, rest2) = suffixes rest1
       in case (ident, suffix) of
            (x : _, _ : _) | isStart x -> Renamed (star <> ident <> suffix) ident : splitNames rest2
            (x : _, []) | isStart x, null star -> Plain ident : splitNames rest1
            _ -> Other c : splitNames cs
  | isIdent c = let (w, rest) = span isIdent s in Plain w : splitNames rest
  | otherwise = Other c : splitNames cs
  where
    isStart x = isAsciiLetter x || x == '_'
    isIdent x = isAsciiLetter x || isDigitChar x || x == '_' || x == '\''
    isAsciiLetter x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')
    isDigitChar x = x >= '0' && x <= '9'
    -- one or more @tag<digits>, the forms the typechecker's renaming adds
    -- ('Morloc.Typecheck.Internal.renameWithMap')
    suffixes str = case str of
      ('@' : tag : more)
        | tag `elem` ("qnsrle" :: String)
        , (ds@(_ : _), more'') <- span isDigitChar more ->
            let (next, rest) = suffixes more'' in ('@' : tag : ds <> next, rest)
      _ -> ([], str)

-- | The source location of an index followed by its snippet, for naming a
-- second site in an error.
sourceAt :: Int -> MorlocMonad MDoc
sourceAt i = do
  st <- get
  return $ case Map.lookup i (stateSourceMap st) of
    Just loc -> pretty loc <> ":" <> snippet st loc
    Nothing -> "<unknown location>"

{- | Render a source code snippet with error location markers.
For single-line spans: ^~~~^ underline from start to end column.
For multi-line spans: Elm-style vertical bar in the gutter with ^ at start and end.
Spans > 10 lines are truncated to first 5 and last 5 lines.
-}
snippet :: MorlocState -> SrcLoc -> MDoc
snippet st (SrcLoc path ln col endLn endCol) =
  case path >>= \p -> Map.lookup p (stateSourceText st) of
    Nothing -> mempty
    Just src ->
      let srcLines = MT.lines src
          n = length srcLines
       in if n == 0 || ln < 1
            then mempty
            else
              let startLine = min ln n
                  finishLine = min endLn n
                  gw = length (show finishLine)
                  gutter = pretty (MT.replicate gw " ")
                  fmtLineNum num =
                    let s = MT.show' num
                     in pretty (MT.replicate (gw - MT.length s) " ") <> pretty s
               in if startLine == finishLine
                    then snippetSingleLine srcLines startLine col endCol gutter fmtLineNum
                    else snippetMultiLine srcLines startLine col finishLine endCol gutter fmtLineNum
  where
    snippetSingleLine srcLines lineNum startCol eCol gutter fmtNum =
      let errLine = srcLines !! (lineNum - 1)
          sc = max 1 (min startCol (MT.length errLine + 1))
          ec = max sc (min eCol (MT.length errLine + 1))
          pointer
            | sc == ec = pretty (MT.replicate (sc - 1) " ") <> "^"
            | ec > sc + 1 =
                pretty (MT.replicate (sc - 1) " ")
                  <> "^"
                  <> pretty (MT.replicate (ec - sc - 2) "~")
                  <> "^"
            | otherwise = pretty (MT.replicate (sc - 1) " ") <> "^^"
       in line
            <> gutter <+> "|"
            <> line
            <> fmtNum lineNum <+> "|" <+> pretty errLine
            <> line
            <> gutter <+> "|" <+> pointer
            <> line

    snippetMultiLine srcLines startLine startCol finishLine eCol gutter fmtNum =
      let totalLines = finishLine - startLine + 1
          lineNums
            | totalLines <= 10 = [startLine .. finishLine]
            | otherwise = [startLine .. startLine + 4] ++ [finishLine - 4 .. finishLine]
          needsElision = totalLines > 10
          elisionPoint = startLine + 5

          renderLine num =
            let srcLine = srcLines !! (num - 1)
             in fmtNum num <+> "| |" <+> pretty srcLine

          renderStartPointer =
            let sc = max 1 startCol
             in gutter <+> "| |" <+> pretty (MT.replicate (sc - 1) " ") <> "^"

          renderEndPointer =
            let endLine = srcLines !! (finishLine - 1)
                ec = max 1 (min eCol (MT.length endLine + 1))
             in gutter <+> "| |" <+> pretty (MT.replicate (ec - 1) " ") <> "^"

          elisionLine = gutter <+> "  ..."

          renderLines [] = mempty
          renderLines (num : rest)
            | needsElision && num == elisionPoint =
                elisionLine <> line <> renderLines (dropWhile (< finishLine - 4) rest)
            | num == startLine =
                renderLine num
                  <> line
                  <> renderStartPointer
                  <> line
                  <> renderLines rest
            | otherwise =
                renderLine num
                  <> line
                  <> renderLines rest
       in line
            <> gutter <+> "|"
            <> line
            <> renderLines lineNums
            <> renderEndPointer
            <> line

throwSourcedError :: (MonadError MorlocError m) => Int -> MDoc -> m a
throwSourcedError i = throwError . Rejected (AtNode i)

throwUnificationError :: (MonadError MorlocError m) => Int -> Int -> Int -> MDoc -> m a
throwUnificationError lhs rhs context = throwError . Rejected (AtUnification lhs rhs context)

-- | A user error with no position in the program.
throwRejected :: (MonadError MorlocError m) => MDoc -> m a
throwRejected = throwError . Rejected Unplaced

-- | A user error at a source position, its message already showing it.
throwRejectedSource :: (MonadError MorlocError m) => SrcLoc -> MDoc -> m a
throwRejectedSource loc = throwError . Rejected (AtSource loc)

-- | Run an action, and recover from a rejection of the program only: an
-- environment failure or a compiler fault still propagates.
recoverRejected :: MorlocMonad a -> MorlocMonad a -> MorlocMonad a
recoverRejected act recover =
  act `catchError` \e -> case e of
    Rejected _ _ -> recover
    _ -> throwError e

-- | True for an error that is the compiler's: a fault, or a rejection whose
-- position was never recorded.
isDefect :: MorlocState -> MorlocError -> Bool
isDefect st e = case e of
  Fault _ _ -> True
  Rejected (AtNode i) _ -> Map.notMember i (stateSourceMap st)
  Rejected (AtUnification lhs _ _) _ -> Map.notMember lhs (stateSourceMap st)
  _ -> False

-- | The environment failed: a file, a process, an installation.
throwFailed :: (MonadError MorlocError m) => MDoc -> m a
throwFailed = throwError . EnvFailed

-- | Raise an internal-compiler-error ("this should never happen") with the real
-- source location of the throw site and a pointer to the issue
-- tracker. 'HasCallStack' supplies the accurate, non-rotting location
-- automatically.
throwCompilerBug :: (HasCallStack, MonadError MorlocError m) => MDoc -> m a
throwCompilerBug = throwError . Fault Nothing . compilerBugMsg callStack

-- | As 'throwCompilerBug', but tied to an AST node index so the user
-- still sees the offending source snippet.
throwCompilerBugAt :: (HasCallStack, MonadError MorlocError m) => Int -> MDoc -> m a
throwCompilerBugAt i = throwError . Fault (Just (AtNode i)) . compilerBugMsg callStack

compilerBugMsg :: CallStack -> MDoc -> MDoc
compilerBugMsg cs msg =
  "Compiler bug at" <+> pretty bugLoc <> ":" <+> msg
    <> line
    <> "This is an internal compiler error; please report it at"
    <+> "https://github.com/morloc-project/morloc/issues"
  where
    bugLoc :: String
    bugLoc = case getCallStack cs of
      ((_, sl) : _) -> srcLocFile sl <> ":" <> show (srcLocStartLine sl)
      [] -> "<unknown location>"

systemCallError :: Text -> Text -> String -> MorlocMonad a
systemCallError cmd loc msg =
  throwFailed $
    maybe mempty (<> "\n\n") (buildFailureNote cmd msg)
      <> "System call failed at ("
      <> pretty loc
      <> "):\n"
      <> " cmd> "
      <> pretty cmd
      <> "\n"
      <> " msg>\n"
      <> pretty msg

-- | What a failed build means in morloc's terms, when its output shows a
-- cause morloc introduced. rustc's own report follows it unchanged.
buildFailureNote :: Text -> String -> Maybe MDoc
buildFailureNote cmd msg
  | "cargo build" `MT.isInfixOf` cmd
  , any (`MT.isInfixOf` MT.pack msg)
      [ "cannot be shared between threads safely"
      , "cannot be sent between threads safely"
      ] =
      Just . vsep $
        [ "A Rust function value (Arc<dyn Fn(..) + Send + Sync>) may be called from any"
        , "thread, so every value a closure captures, and every value stored in a function"
        , "value or in a recursive type, must be Send + Sync. If rustc's report below traces"
        , "the type it names to a function value, a closure or a recursive type, make that"
        , "type thread-safe (Arc and Mutex in place of Rc and RefCell)."
        ]
  | otherwise = Nothing

-- | Execute a system call
runCommand ::
  Text -> -- function making the call (used only in debugging messages on error)
  Text -> -- system command
  MorlocMonad ()
runCommand loc cmd = do
  liftIO $ MT.hPutStrLn stderr ("$ " <> cmd)
  (exitCode, _, err') <-
    liftIO $ SP.readCreateProcessWithExitCode (SP.shell . MT.unpack $ cmd) []
  case exitCode of
    SE.ExitSuccess -> tell [MT.pack err']
    _ -> systemCallError cmd loc err'

sayIf :: Int -> MDoc -> MorlocMonad ()
sayIf i d = do
  verbosity <- gets stateVerbosity
  when (verbosity >= i) $ liftIO $ MT.hPutStrLn stderr (render d)

-- print anytime
say :: MDoc -> MorlocMonad ()
say = sayIf 0

-- print for verbose level 1
-- messages that may be of interest to the user
sayV :: MDoc -> MorlocMonad ()
sayV = sayIf 1

-- print for verbose level 2
-- messages for the programmer
sayVV :: MDoc -> MorlocMonad ()
sayVV = sayIf 2

-- print for verbose level 3
-- really boring shit that probably no one wants to ever hear, but we spent a
-- lot of time working on it and don't want to delete it.
sayVVV :: MDoc -> MorlocMonad ()
sayVVV = sayIf 3

-- | Execute a system call and return a function of the STDOUT
runCommandWith ::
  Text -> -- function making the call (used only in debugging messages on error)

  -- | A function of the output (run on success)
  (Text -> a) ->
  -- | System command
  Text ->
  MorlocMonad a
runCommandWith loc f cmd = do
  liftIO $ MT.hPutStrLn stderr ("$ " <> cmd)
  (exitCode, out, err') <-
    liftIO $ SP.readCreateProcessWithExitCode (SP.shell . MT.unpack $ cmd) []
  case exitCode of
    SE.ExitSuccess -> return $ f (MT.pack out)
    _ -> systemCallError cmd loc err'

-- | Write a object to a file in the Morloc temporary directory
logFile ::
  (Show a) =>
  -- | A filename
  String ->
  a ->
  MorlocMonad a
logFile s m = do
  tmpdir <- asks configTmpDir
  liftIO $ MS.createDirectoryIfMissing True tmpdir
  let path = MS.combine tmpdir s
  liftIO $ MT.writeFile path (MT.pretty m)
  return m

-- | Write a object to a file in the Morloc temporary directory
logFileWith ::
  (Show b) =>
  -- | A filename
  String ->
  -- | A function to convert a to something presentable
  (a -> b) ->
  a ->
  MorlocMonad a
logFileWith s f m = do
  tmpdir <- asks configTmpDir
  liftIO $ MS.createDirectoryIfMissing True tmpdir
  let path = MS.combine tmpdir s
  liftIO $ MT.writeFile path (MT.pretty (f m))
  return m

-- | Look up a language by name or alias using the registry.
readLang :: Text -> MorlocMonad Lang
readLang langStr = do
  reg <- gets stateLangRegistry
  case LR.lookupByAlias langStr reg of
    Just (name, entry) -> return (ML.makeLang name (LR.lreExtension entry))
    Nothing -> throwRejected $ "Unknown language" <> squotes (pretty langStr)

{- | Return sources for constructing an object. These are used by `NamE NamObject`
expressions. Sources here includes some that are not linked to signatures, such
as language-specific imports of object constructors. So this supersets the
stateSignatures field's sources.
-}
metaSources :: Int -> MorlocMonad [Source]
metaSources i = do
  s <- gets stateSources
  case GMap.lookup i s of
    GMapNoFst -> return []
    GMapNoSnd -> error "Compiler bug: Internal GMap key missing"
    (GMapJust srcs) -> return srcs

----- TODO: metaName should no longer be required - remove

{- | The name of a morloc composition. These names are stored in the monad
after they are resolved away. For example in:
  import math
  export foo
  bar x y = add x (inc y)
  foo x = add (bar x 5) 1
`foo` and `bar` are morloc composition. `foo` will be resolved to
  add (add x (inc 5) 1
The terms "foo" and "bar" have disappeared. They aren't technically needed
anymore. However, the nexus needs a subcommand name to give the user for
calling "foo". In the generated code and in error messages, it is also nice
to keep the label "bar" attached to the second `add` function. `metaName`
can retrieve these names based on the index of the CallS expressions that
wrap the two `add` functions.

The name is linked to the SAnno general data structure.
-}
metaName :: Int -> MorlocMonad (Maybe EVar)
metaName i = gets (Map.lookup i . stateName)

-- Get the docstrings associated with an item
getDocStrings ::
  Int -> -- expression index
  MorlocMonad ArgDoc
getDocStrings i = do
  sgmap <- gets stateSignatures
  case GMap.lookup i sgmap of
    (GMapJust (Monomorphic (TermTypes (Just e) _ _))) -> return $ edocs e
    (GMapJust (Polymorphic _ _ e)) -> return $ edocs e
    GMapNoSnd -> error "Compiler bug: Internal GMap key missing"
    _ -> error "Compiler bug: No entry found for index in stateSignatures"

-- | The program's per-language type forms. Type names are resolved to the
-- declaration they denote before these are built, so one table serves every
-- module.
getConcreteScope :: Lang -> MorlocMonad Scope
getConcreteScope lang0 = do
  -- A guest member (e.g. futhark) has no concrete scope of its own; resolve
  -- against its pool host's scope (cpp). Identity for non-members.
  lang <- LR.poolOf <$> gets stateLangRegistry <*> pure lang0
  Map.findWithDefault Map.empty lang <$> gets stateConcreteTypedefs

-- | The program's general type declarations.
getGeneralScope :: MorlocMonad Scope
getGeneralScope = gets stateGeneralTypedefs

{- | Get the module name from state, falling back to "nexus" if unset.
This is the canonical name for pool subdirectories and manifest references.
-}
getModuleName :: MorlocMonad String
getModuleName = do
  st <- get
  return $ case stateModuleName st of
    Just (MV n) -> MT.unpack n
    Nothing -> "nexus"

{- | Get the output file name: the -o value if given, else the module name.
This controls only the wrapper script filename.
-}
getOutfileName :: MorlocMonad String
getOutfileName = do
  st <- get
  case stateOutfile st of
    Just name -> return name
    Nothing -> getModuleName

{- | Get the program identity used to name the build directory
(@<key>-build@ / @exe/<key>@). Uses 'stateProgramKey' (--name or the
source basename) when set; otherwise falls back to the outfile/module
name, which is how eval reuses its @--save@ name.
-}
getProgramKey :: MorlocMonad String
getProgramKey = do
  st <- get
  case stateProgramKey st of
    Just k -> return k
    Nothing -> getOutfileName

data IndexState = IndexState
  { index :: !Int
  , indexSchemas :: !(Map.Map Text Int)  -- schema string -> ID
  }
type Index a = StateT IndexState Identity a

runIndex :: Int -> Index a -> a
runIndex i x = evalState x (IndexState i Map.empty)

-- | Run from index @i@, returning the next unused index.
runIndexFrom :: Int -> Index a -> (a, Int)
runIndexFrom i x = let (a, st) = runState x (IndexState i Map.empty) in (a, index st)

newIndex :: Index Int
newIndex = do
  s <- get
  let i = index s
  put $! s {index = index s + 1}
  return i

getIndex :: Index Int
getIndex = gets index

setIndex :: Int -> Index ()
setIndex i = do
  s <- get
  put $! s {index = i}
  return ()

registerSchemaIndex :: Text -> Index Int
registerSchemaIndex schema = do
  s <- get
  case Map.lookup schema (indexSchemas s) of
    Just sid -> return sid
    Nothing -> do
      let sid = Map.size (indexSchemas s)
      put $! s {indexSchemas = Map.insert schema sid (indexSchemas s)}
      return sid

getSchemaTable :: Index [Text]
getSchemaTable = do
  m <- gets indexSchemas
  return $ map fst $ sortBy (comparing snd) $ Map.toList m
