module Morloc (
    writeProgram
) where

import Xi
import Morloc.Global
import Morloc.Operators
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Generator as MG
import qualified Morloc.Build as MB
import qualified System.FilePath.Posix as SFP

-- do not check existence of source files
-- TODO import this from Xi instead
ignoreSource :: MT.Text -> IO () 
ignoreSource _ = return ()

localModules :: Maybe Path -> MVar -> IO MT.Text
localModules (Just filename) (MV f)
  = MT.readFile
  . SFP.replaceFileName (MT.unpack filename)
  $ (MT.unpack f <> ".loc")
localModules Nothing (MV f) = MT.readFile (MT.unpack f <> ".loc")

xi :: Maybe Path -> MT.Text -> MorlocMonad [Module]
xi path code = do
  mods' <- MM.liftIO .  fmap typecheck
        $  parse ignoreSource (localModules path) code
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
