{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Monad
Description : A great big stack of monads
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

MorlocMonad is a monad stack that is passed throughout the morloc codebase.
Most functions that raise errors, perform IO, or access global configuration
will return `MorlocMonad a` types. The stack consists of a State, Writer,
Except, and Reader monad.
-}
module Morloc.Monad
  ( MorlocReturn
  , runMorlocMonad
  , writeMorlocReturn
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
  -- * Indexing monad
  , Index
  , runIndex
  , newIndex
  , getIndex
  , setIndex
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Monad.Identity
import Morloc.Namespace
import Morloc.Data.Doc
import System.IO (stderr)
import qualified Morloc.Data.Text as MT
import Data.Text (Text)
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Language as ML
import qualified System.Exit as SE
import qualified System.Process as SP
import qualified Morloc.System as MS

runMorlocMonad ::
     Maybe Path -> Int -> Config -> BuildConfig -> MorlocMonad a -> IO (MorlocReturn a)
runMorlocMonad outfile v config buildConfig ev = do
  let state0 = emptyState outfile v
      state1 = state0 { stateBuildConfig = buildConfig }
  runStateT (runWriterT (runExceptT (runReaderT ev config))) (state1)

emptyState :: Maybe Path -> Int -> MorlocState
emptyState path v = defaultValue
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

takeFromCounter :: Int -> MorlocMonad [Int]
takeFromCounter 0 = return []
takeFromCounter i = do
    x <- getCounter
    xs <- takeFromCounter (i-1)
    return (x:xs)

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
writeMorlocReturn ((Left err', msgs), _) = do
  MT.hPutStrLn stderr (MT.unlines msgs) -- write messages
  MT.hPutStrLn stderr (MT.show' err') -- write terminal failing message
  return False
writeMorlocReturn ((Right _, _), _) = return True


-- | Execute a system call
runCommand ::
     Text -- function making the call (used only in debugging messages on error)
  -> Text -- system command
  -> MorlocMonad ()
runCommand loc cmd = do
  liftIO . MT.putStrLn $ "$ " <> cmd
  (exitCode, _, err') <-
    liftIO $ SP.readCreateProcessWithExitCode (SP.shell . MT.unpack $ cmd) []
  case exitCode of
    SE.ExitSuccess -> tell [MT.pack err']
    _ -> throwError (SystemCallError cmd loc (MT.pack err')) |>> const ()

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
     Text -- function making the call (used only in debugging messages on error)
  -> (Text -> a) -- ^ A function of the output (run on success)
  -> Text -- ^ System command
  -> MorlocMonad a
runCommandWith loc f cmd = do
  liftIO . MT.putStrLn $ "$ " <> cmd
  (exitCode, out, err') <-
    liftIO $ SP.readCreateProcessWithExitCode (SP.shell . MT.unpack $ cmd) []
  case exitCode of
    SE.ExitSuccess -> return $ f (MT.pack out)
    _ -> throwError (SystemCallError cmd loc (MT.pack err'))

-- | Write a object to a file in the Morloc temporary directory
logFile ::
     Show a
  => String -- ^ A filename
  -> a
  -> MorlocMonad a
logFile s m = do
  tmpdir <- asks configTmpDir
  liftIO $ MS.createDirectoryIfMissing True tmpdir
  let path = MS.combine tmpdir s
  liftIO $ MT.writeFile path (MT.pretty m)
  return m

-- | Write a object to a file in the Morloc temporary directory
logFileWith ::
     (Show b)
  => String -- ^ A filename
  -> (a -> b) -- ^ A function to convert a to something presentable
  -> a
  -> MorlocMonad a
logFileWith s f m = do
  tmpdir <- asks configTmpDir
  liftIO $ MS.createDirectoryIfMissing True tmpdir
  let path = MS.combine tmpdir s
  liftIO $ MT.writeFile path (MT.pretty (f m))
  return m

-- | Attempt to read a language name. This is a wrapper around the
-- @Morloc.Language::readLangName@ that appropriately handles error.
readLang :: Text -> MorlocMonad Lang
readLang langStr =
  case ML.readLangName langStr of
    (Just x) -> return x
    Nothing -> throwError $ UnknownLanguage langStr


-- | Return sources for constructing an object. These are used by `NamE NamObject`
-- expressions. Sources here includes some that are not linked to signatures, such
-- as language-specific imports of object constructors. So this supersets the
-- stateSignatures field's sources.
metaSources :: Int -> MorlocMonad [Source]
metaSources i = do
  s <- gets stateSources
  case GMap.lookup i s of
    GMapNoFst -> return []
    GMapNoSnd -> throwError . CallTheMonkeys $ "Internal GMap key missing"
    (GMapJust srcs) -> return srcs


----- TODO: metaName should no longer be required - remove
-- | The name of a morloc composition. These names are stored in the monad
-- after they are resolved away. For example in:
--   import math
--   export foo
--   bar x y = add x (inc y)
--   foo x = add (bar x 5) 1
-- `foo` and `bar` are morloc composition. `foo` will be resolved to
--   add (add x (inc 5) 1
-- The terms "foo" and "bar" have disappeared. They aren't technically needed
-- anymore. However, the nexus needs a subcommand name to give the user for
-- calling "foo". In the generated code and in error messages, it is also nice
-- to keep the label "bar" attached to the second `add` function. `metaName`
-- can retrieve these names based on the index of the CallS expressions that
-- wrap the two `add` functions.
--
-- The name is linked to the SAnno general data structure.
metaName :: Int -> MorlocMonad (Maybe EVar)
metaName i = gets (Map.lookup i . stateName)

-- Get the docstrings associated with an item
getDocStrings
    :: Int -- expression index
    -> MorlocMonad ArgDoc
getDocStrings i = do
  sgmap <- gets stateSignatures
  case GMap.lookup i sgmap of
    (GMapJust (Monomorphic (TermTypes (Just e) _ _))) -> return $ edocs e
    (GMapJust (Polymorphic _ _ e _)) -> return $ edocs e
    GMapNoSnd -> throwError . CallTheMonkeys $ "Internal GMap key missing"
    _ -> throwError . CallTheMonkeys $ "No entry found for index in stateSignatures"


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

newtype IndexState = IndexState { index :: Int }
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
