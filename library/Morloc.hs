module Morloc (
    writeProgram
) where

import Morloc.Global
import Morloc.Operators
import Morloc.Types.API
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Generator as MG
import qualified Morloc.Build as MB
import qualified System.FilePath.Posix as SFP

xi :: Maybe Path -> MT.Text -> MorlocMonad [Module]
xi path code = do
  mods' <- MM.liftIO .  fmap typecheck
        $  parse ignoreSource (localModules (fmap MT.unpack path)) path code
  case mods' of
    (Right mods) -> return mods
    (Left err) -> MM.throwError . TypeError . MT.pack $ show err

-- | Build a program as a local executable
writeProgram
  :: Maybe Path -- ^ source code filename (for debugging messages)
  -> MT.Text    -- ^ source code text
  -> MorlocMonad ()
writeProgram path code = xi path code >>= MG.generate >>= buildScripts
  where
    buildScripts :: (Script, [Script]) -> MorlocMonad ()
    buildScripts (nexus, pools) = mapM_ MB.build (nexus:pools)
