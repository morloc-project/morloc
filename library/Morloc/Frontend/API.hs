{-|
Module      : Morloc.Frontend.API
Description : Morloc frontend API
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Frontend.API
  ( parse
  , Parser.readType
  , Typecheck.typecheck
  , Typecheck.resolveTypes
  ) where

import Morloc.Frontend.Namespace
import qualified Control.Monad.Except as ME
import qualified Control.Monad.Reader as MR
import qualified Control.Monad.State as MS
import qualified Control.Monad.Writer as MW
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Morloc.Data.DAG as MDD
import qualified Morloc.Data.Text as MT
import qualified Morloc.Module as Mod
import qualified Morloc.Monad as MM
import qualified Morloc.Frontend.Parser as Parser
import qualified Morloc.Frontend.Typecheck as Typecheck
import qualified Morloc.Frontend.Pretty as Pretty

parse ::
     Maybe Path
  -> Code -- ^ code of the current module
  -> MorlocMonad (DAG MVar Import ExprI)
parse f (Code code) = case Parser.readProgram f code mempty of
  (Left err) -> MM.throwError $ SyntaxError err
  (Right x) -> parseImports x
  where
    parseImports
      :: DAG MVar Import ExprI
      -> MorlocMonad (DAG MVar Import ExprI)
    parseImports d = case unimported of
      [] -> return d
      (child:_) -> do
          importPath <- Mod.findModule child
          Mod.loadModuleMetadata importPath
          (path', code') <- openLocalModule importPath
          case Parser.readProgram path' code' d of
            (Left err) -> MM.throwError $ SyntaxError err
            (Right x) -> parseImports x
      where
        g = MDD.edgelist d
        parents = Set.fromList (map fst g)
        children = Set.fromList (map snd g)
        unimported = Set.toList $ Set.difference children parents

-- | assume @t@ is a filename and open it, return file name and contents
openLocalModule :: Path -> MorlocMonad (Maybe Path, MT.Text)
openLocalModule filename = do
  code <- liftIO $ MT.readFile filename
  return (Just filename, code)
