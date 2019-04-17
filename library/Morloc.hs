module Morloc (
    writeProgram
  , writeTurtleTo
  , writeTripleTo
) where

import qualified Data.RDF as DR
import qualified Data.Map.Strict as DMS
import qualified System.IO as SIO
import qualified System.Directory as SD

import qualified Morloc.Monad as MM
import qualified Morloc.Data.Text as MT
import Morloc.Operators
import Morloc.Types
import Morloc.Config (Config)
import qualified Morloc.Parser as MP
import qualified Morloc.Generator as MG
import Morloc.Typecheck (typecheck)

-- | Build a program as a local executable
writeProgram :: SparqlDatabaseLike db
  => Maybe Path -- ^ source code filename (for debugging messages)
  -> MT.Text    -- ^ source code text
  -> db         -- ^ a SPARQL endpoint
  -> MorlocMonad ()
writeProgram path code ep = do
      MP.parse path code   -- Text -> MorlocMonad RDF
  >>= sparqlUpload ep -- SparqlDatabaseLike db => db -> MorlocMonad db
  >>= MG.generate     -- MorlocMonad (Nexus, [Pool])
  >>= writeScripts    -- MorlocMoand ()
  where
    writeScripts :: (Script, [Script]) -> MorlocMonad ()
    writeScripts (n, ps) = do
      MM.liftIO $ writeScript True n
      MM.liftIO $ mapM_ (writeScript False) ps

    writeScript :: Bool -> Script -> IO ()
    writeScript isExe (Script base lang code') = do
      let f = base <> "." <> lang
      MT.writeFile f code'
      p <- SD.getPermissions f
      SD.setPermissions f (p {SD.executable = isExe})

-- | Triple serialization wrapper around `writeRdfTo`
writeTripleTo
  :: Maybe Path -- source code filename
  -> MT.Text    -- source code text
  -> FilePath   -- output filename
  -> MorlocMonad ()
writeTripleTo = writeRdfTo DR.NTriplesSerializer

-- | Turtle serialization wrapper around `writeRdfTo`
writeTurtleTo
  :: Maybe Path -- source code filename
  -> MT.Text    -- source code text
  -> FilePath   -- output filename
  -> MorlocMonad ()
writeTurtleTo = writeRdfTo (DR.TurtleSerializer Nothing (DR.PrefixMappings DMS.empty))

-- | Compile a program to RDF. Does not depend on a local SPARQL database. It
-- also does not consider the local configuration or postprocessing and
-- typechecking.
writeRdfTo :: DR.RdfSerializer s
  => s           -- ^ the RDF serializer
  -> Maybe Path  -- ^ source code file name (Nothing if it is a raw string)
  -> MT.Text     -- ^ source code
  -> FilePath    -- ^ output filename
  -> MorlocMonad ()
writeRdfTo s path code ttl = do
  rdf <- MP.parse path code
  handle <- MM.liftIO $ SIO.openFile ttl SIO.WriteMode
  MM.liftIO $ DR.hWriteRdf s handle rdf <* SIO.hClose handle
