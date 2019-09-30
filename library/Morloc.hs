module Morloc (
    writeProgram
  , P.cute
  , P.ugly
  , P.ignoreSource
  , P.localModules
) where

import Morloc.Global
import qualified Morloc.TypeChecker.API as T
import qualified Morloc.Parser.API as P
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Generator as MG
import qualified Morloc.Build as MB

typecheck :: Maybe Path -> MT.Text -> MorlocMonad [T.Module]
typecheck path code = do
  mods' <- MM.liftIO .  fmap T.typecheck
        $  P.parse P.ignoreSource (P.localModules (fmap MT.unpack path)) path code
  case mods' of
    (Right mods) -> return mods
    (Left err) -> MM.throwError . TypeError . MT.pack $ show err

-- | Build a program as a local executable
writeProgram
  :: Maybe Path -- ^ source code filename (for debugging messages)
  -> MT.Text    -- ^ source code text
  -> MorlocMonad ()
writeProgram path code = typecheck path code >>= MG.generate >>= buildScripts
  where
    buildScripts :: (Script, [Script]) -> MorlocMonad ()
    buildScripts (nexus, pools) = mapM_ MB.build (nexus:pools)
