{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Monad
Description : A great big stack of monads
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

MorlocMonad is a monad stack that is passed throughout the morloc codebase.
Most functions that raise errors, perform IO, or access global configuration
will return `MorlocMonad a` types. The stack consists of a State, Writer,
Except, and Reader monad.
-}
module Morloc.Monad
  ( MorlocReturn
  , runMorlocMonad
  , writeMorlocReturn
  , makeMorlocError
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
  , getConcreteUniversalScope
  , getGeneralUniversalScope

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
  , throwSystemError
  , throwSourcedError
  , throwUnificationError

    -- * Indexing monad
  , Index
  , IndexState (..)
  , runIndex
  , newIndex
  , getIndex
  , setIndex
  ) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Text (Text)
import Morloc.Data.Doc
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import Morloc.Namespace.Expr
import Morloc.Namespace.State
import qualified Morloc.System as MS
import qualified System.Exit as SE
import System.IO (stderr)
import qualified System.Process as SP

runMorlocMonad ::
  Maybe Path -> Int -> Config -> BuildConfig -> MorlocMonad a -> IO (MorlocReturn a)
runMorlocMonad outfile v config buildConfig ev = do
  let state0 = emptyState outfile v
      state1 = state0 {stateBuildConfig = buildConfig}
  runStateT (runWriterT (runExceptT (runReaderT ev config))) (state1)

emptyState :: Maybe Path -> Int -> MorlocState
emptyState path v =
  defaultValue
    { stateVerbosity = v
    , stateOutfile = path
    }

startCounter :: MorlocMonad ()
startCounter = do
  s <- get
  put $ s {stateCounter = 0}

getCounter :: MorlocMonad Int
getCounter = do
  s <- get
  let i = stateCounter s
  put $ s {stateCounter = stateCounter s + 1}
  return i

-- | Create a new index that inherits the source position of a parent index
getCounterWithPos :: Int -> MorlocMonad Int
getCounterWithPos parentIdx = do
  i <- getCounter
  s <- get
  case Map.lookup parentIdx (stateSourceMap s) of
    Just loc -> put $ s {stateSourceMap = Map.insert i loc (stateSourceMap s)}
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
  put $ s {stateCounter = i}
  return ()

incDepth :: MorlocMonad Int
incDepth = do
  s <- get
  let i = stateDepth s + 1
  put $ s {stateDepth = i}
  return i

getDepth :: MorlocMonad Int
getDepth = gets stateDepth

decDepth :: MorlocMonad Int
decDepth = do
  s <- get
  let i = stateDepth s - 1
  put $ s {stateDepth = i}
  return i

setDepth :: Int -> MorlocMonad ()
setDepth i = do
  s <- get
  put $ s {stateDepth = i}
  return ()

writeMorlocReturn :: MorlocReturn a -> IO Bool
writeMorlocReturn ((Left err', msgs), st) = do
  writeMessages
  MT.hPutStrLn stderr (render $ makeMorlocError st err')
  return False
  where
    writeMessages
      | length msgs > 0 = MT.hPutStrLn stderr (MT.unlines msgs)
      | otherwise = return ()
writeMorlocReturn ((Right _, _), _) = return True

makeMorlocError :: MorlocState -> MorlocError -> MDoc
makeMorlocError st (SourcedError i msg) =
  case Map.lookup i (stateSourceMap st) of
    Just loc -> pretty loc <> ": error:" <> line <> msg <> snippet st loc
    Nothing -> "Compiler bug, broken index" <+> pretty i <+> "with attached error:" <+> msg
makeMorlocError _ (SystemError msg) = msg
makeMorlocError st (UnificationError lhs rhs context msg) =
  case (Map.lookup lhs srcMap, Map.lookup rhs srcMap, Map.lookup context srcMap) of
    (Just lhsLoc, rhsLoc, contextLoc) ->
      "Unification error:" <+> msg <> line
      <> "Found while unifying" <+> maybe mempty pretty contextLoc <> line
      <> "With values" <> line <> snippet st lhsLoc
      <> maybe mempty (\l -> "and" <> line <> snippet st l) rhsLoc
    _ -> "Compiler bug, broken indices" <+> pretty (lhs, rhs, context) <+> "with attached error:" <+> msg
  where
    srcMap = stateSourceMap st

-- | Render a source code snippet with error location markers.
-- For single-line spans: ^~~~^ underline from start to end column.
-- For multi-line spans: Elm-style vertical bar in the gutter with ^ at start and end.
-- Spans > 10 lines are truncated to first 5 and last 5 lines.
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
               fmtLineNum num = let s = MT.show' num
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
            | sc == ec  = pretty (MT.replicate (sc - 1) " ") <> "^"
            | ec > sc + 1 = pretty (MT.replicate (sc - 1) " ") <> "^"
                         <> pretty (MT.replicate (ec - sc - 2) "~") <> "^"
            | otherwise = pretty (MT.replicate (sc - 1) " ") <> "^^"
      in line
         <> gutter <+> "|" <> line
         <> fmtNum lineNum <+> "|" <+> pretty errLine <> line
         <> gutter <+> "|" <+> pointer <> line

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
          renderLines (num:rest)
            | needsElision && num == elisionPoint =
                elisionLine <> line <> renderLines (dropWhile (< finishLine - 4) rest)
            | num == startLine =
                renderLine num <> line
                <> renderStartPointer <> line
                <> renderLines rest
            | otherwise =
                renderLine num <> line
                <> renderLines rest
      in line
         <> gutter <+> "|" <> line
         <> renderLines lineNums
         <> renderEndPointer <> line


throwSystemError :: MonadError MorlocError m => MDoc -> m a
throwSystemError = throwError . SystemError

throwSourcedError :: MonadError MorlocError m => Int -> MDoc -> m a
throwSourcedError i = throwError . SourcedError i

throwUnificationError :: MonadError MorlocError m => Int -> Int -> Int -> MDoc -> m a
throwUnificationError lhs rhs context msg = throwError $ UnificationError lhs rhs context msg

systemCallError ::  Text -> Text -> String -> MorlocMonad a
systemCallError cmd loc msg = throwSystemError $
  "System call failed at ("
    <> pretty loc
    <> "):\n"
    <> " cmd> "
    <> pretty cmd
    <> "\n"
    <> " msg>\n"
    <> pretty msg

-- | Execute a system call
runCommand ::
  Text -> -- function making the call (used only in debugging messages on error)
  Text -> -- system command
  MorlocMonad ()
runCommand loc cmd = do
  liftIO . MT.putStrLn $ "$ " <> cmd
  (exitCode, _, err') <-
    liftIO $ SP.readCreateProcessWithExitCode (SP.shell . MT.unpack $ cmd) []
  case exitCode of
    SE.ExitSuccess -> tell [MT.pack err']
    _ -> systemCallError cmd loc err'

sayIf :: Int -> MDoc -> MorlocMonad ()
sayIf i d = do
  verbosity <- gets stateVerbosity
  when (verbosity >= i) $ (liftIO . putDoc) (d <> "\n")

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
  liftIO . MT.putStrLn $ "$ " <> cmd
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

{- | Attempt to read a language name. This is a wrapper around the
@Morloc.Language::readLangName@ that appropriately handles error.
-}
readLang :: Text -> MorlocMonad Lang
readLang langStr =
  case ML.readLangName langStr of
    (Just x) -> return x
    Nothing -> throwSystemError $ "Unknown language" <> squotes (pretty langStr)

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
    (GMapJust (Polymorphic _ _ e _)) -> return $ edocs e
    GMapNoSnd -> error "Compiler bug: Internal GMap key missing"
    _ -> error "Compiler bug: No entry found for index in stateSignatures"

getConcreteScope :: Int -> Lang -> MorlocMonad Scope
getConcreteScope i lang = do
  p <- gets stateConcreteTypedefs
  return $ case GMap.lookup i p of
    (GMapJust langmap) -> case Map.lookup lang langmap of
      (Just scope) -> scope
      Nothing -> Map.empty
    _ -> Map.empty

getGeneralScope :: Int -> MorlocMonad Scope
getGeneralScope i = do
  p <- gets stateGeneralTypedefs
  return $ case GMap.lookup i p of
    (GMapJust scope) -> scope
    _ -> Map.empty

getConcreteUniversalScope :: Lang -> MorlocMonad Scope
getConcreteUniversalScope lang = do
  scopeMap <- gets stateUniversalConcreteTypedefs
  case Map.lookup lang scopeMap of
    (Just scope) -> return scope
    Nothing -> return Map.empty

getGeneralUniversalScope :: MorlocMonad Scope
getGeneralUniversalScope = gets stateUniversalGeneralTypedefs

newtype IndexState = IndexState {index :: Int}
type Index a = StateT IndexState Identity a

runIndex :: Int -> Index a -> a
runIndex i x = evalState x (IndexState i)

newIndex :: Index Int
newIndex = do
  s <- get
  let i = index s
  put $ s {index = index s + 1}
  return i

getIndex :: Index Int
getIndex = gets index

setIndex :: Int -> Index ()
setIndex i = do
  s <- get
  put $ s {index = i}
  return ()
