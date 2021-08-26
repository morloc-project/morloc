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
  , Infer.typecheck
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
import qualified Morloc.Frontend.Infer as Infer
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
    parseImports d
      | length unimported == 0 = return d
      | otherwise = do
          importPath <- Mod.findModule (head unimported)
          Mod.loadModuleMetadata importPath
          (path', code') <- openLocalModule importPath
          case Parser.readProgram path' code' d of
            (Left err) -> MM.throwError $ SyntaxError err
            (Right x) -> parseImports x
      where
        g = MDD.edgelist d
        parents = Map.keysSet d
        children = Set.fromList (map snd g)
        unimported = Set.toList $ Set.difference children parents

-- | assume @t@ is a filename and open it, return file name and contents
openLocalModule :: Path -> MorlocMonad (Maybe Path, MT.Text)
openLocalModule filename = do
  code <- liftIO $ MT.readFile filename
  return (Just filename, code)
