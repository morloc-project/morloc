{-|
Module      : Morloc.Monad
Description : A great big stack of monads
Copyright   : (c) Zebulun Arendsee, 2021
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
  , say
  -- * re-exports
  , module Control.Monad.Trans
  , module Control.Monad.Except
  , module Control.Monad.Reader
  , module Control.Monad.State
  , module Control.Monad.Writer
  -- * reusable counter
  , startCounter
  , getCounter
  , setCounter
  -- * metadata accessors
  , metaTermTypes
  , metaConstraints
  , metaSources
  , metaName
  , metaProperties
  , metaType
  , metaTypedefs
  , metaPackMap
  -- * handling tree depth
  , incDepth
  , getDepth
  , decDepth
  , setDepth
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Morloc.Error () -- for MorlocError Show instance
import Morloc.Namespace
import Morloc.Data.Doc
import System.IO (stderr)
import qualified Morloc.Data.Text as MT
import qualified Morloc.Data.GMap as GMap
import qualified Morloc.Language as ML
import qualified System.Exit as SE
import qualified System.Process as SP
import qualified Morloc.System as MS
import qualified Data.Map as Map
import qualified Data.Set as Set

runMorlocMonad ::
     Maybe Path -> Int -> Config -> MorlocMonad a -> IO (MorlocReturn a)
runMorlocMonad outfile v config ev =
  runStateT (runWriterT (runExceptT (runReaderT ev config))) (emptyState outfile v)

emptyState :: Maybe Path -> Int -> MorlocState
emptyState path v = MorlocState {
    statePackageMeta = []
  , stateVerbosity = v
  , stateCounter = -1
  , stateDepth = 0
  , stateSignatures = GMap.empty
  , stateTypedefs = GMap.empty
  , stateSources = GMap.empty
  , stateOutfile = path
  , statePackers = GMap.empty
  , stateName = Map.empty
}

startCounter :: MorlocMonad ()
startCounter = do
  s <- get
  put $ s {stateCounter = 0}

getCounter :: MorlocMonad Int
getCounter = do
  s <- get
  let i = stateCounter s
  put $ s {stateCounter = (stateCounter s) + 1}
  return i

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

writeMorlocReturn :: MorlocReturn a -> IO ()
writeMorlocReturn ((Left err', msgs), _)
  =  MT.hPutStrLn stderr (MT.unlines msgs) -- write messages
  >> MT.hPutStrLn stderr (MT.show' err') -- write terminal failing message
writeMorlocReturn ((_, msgs), _) = MT.hPutStrLn stderr (MT.unlines msgs)

-- | Execute a system call
runCommand ::
     MT.Text -- function making the call (used only in debugging messages on error)
  -> MT.Text -- system command
  -> MorlocMonad ()
runCommand loc cmd = do
  liftIO . MT.putStrLn $ "$ " <> cmd
  (exitCode, _, err') <-
    liftIO $ SP.readCreateProcessWithExitCode (SP.shell . MT.unpack $ cmd) []
  case exitCode of
    SE.ExitSuccess -> tell [MT.pack err']
    _ -> throwError (SystemCallError cmd loc (MT.pack err')) |>> (\_ -> ())

say :: MDoc -> MorlocMonad ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

-- | Execute a system call and return a function of the STDOUT
runCommandWith ::
     MT.Text -- function making the call (used only in debugging messages on error)
  -> (MT.Text -> a) -- ^ A function of the output (run on success)
  -> MT.Text -- ^ System command
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
readLang :: MT.Text -> MorlocMonad Lang
readLang langStr =
  case ML.readLangName langStr of
    (Just x) -> return x
    Nothing -> throwError $ UnknownLanguage langStr


metaTermTypes :: Int -> MorlocMonad (Maybe TermTypes)
metaTermTypes i = do
  s <- get
  case GMap.lookup i (stateSignatures s) of
    GMapNoFst -> return Nothing
    GMapNoSnd -> throwError . CallTheMonkeys $ "Internal GMap key missing"
    (GMapJust t) -> return (Just t)

-- | Return sources for constructing an object. These are used by `NamE NamObject` expressions.
metaSources :: Int -> MorlocMonad [Source]
metaSources i = do
  s <- gets stateSources
  case GMap.lookup i s of
    GMapNoFst -> return []
    GMapNoSnd -> throwError . CallTheMonkeys $ "Internal GMap key missing"
    (GMapJust srcs) -> return srcs

-- TODO: rename the meta* functions

-- | The general constraints as defined in the general type signature. These
-- are not used anywhere yet.
metaConstraints :: Int -> MorlocMonad [Constraint]
metaConstraints i = do
  s <- gets stateSignatures 
  return $ case GMap.lookup i s of
    (GMapJust (TermTypes (Just e) _ _)) -> Set.toList (econs e)
    _ -> []

-- | Properties are cunrrently not used after Sanno types are created. The only
-- properties that are considered are the pack/unpack properties. Eventually
-- properties will be part of the typeclass system.
metaProperties :: Int -> MorlocMonad [Property]
metaProperties i = do
  s <- gets stateSignatures 
  return $ case GMap.lookup i s of
    (GMapJust (TermTypes (Just e) _ _)) -> Set.toList (eprop e)
    _ -> []

-- | Store type annotations for an expression. These are the original user
-- provided types NOT the types checked or inferred by the typechecker.
metaType :: Int -> MorlocMonad (Maybe TypeU)
metaType i = do
  s <- gets stateSignatures 
  return $ case GMap.lookup i s of
    (GMapJust (TermTypes (Just e) _ _)) ->
      if langOf e == Nothing
        then Just (etype e) 
        else Nothing
    _ -> Nothing

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
metaName i = Map.lookup i <$> gets stateName

metaPackMap :: Int -> MorlocMonad PackMap
metaPackMap i = do
    p <- gets statePackers
    case GMap.lookup i p of
      (GMapJust p') -> return p'
      _ -> return Map.empty


-- | This is currently only used in the C++ translator.
metaTypedefs :: Int -> MorlocMonad (Map.Map TVar (Type, [TVar]))
metaTypedefs i = do
    p <- gets stateTypedefs
    case GMap.lookup i p of
      (GMapJust p') -> return p'
      _ -> return Map.empty
