module Morloc (
    buildProgram
  , writeRdfTo
  , writeTurtleTo
  , writeTripleTo
  , writeProgram
) where

import qualified Data.RDF as DR
import qualified Data.Map.Strict as DMS
import qualified System.IO as SIO
import qualified System.Directory as SD

import qualified Morloc.Data.Text as MT
import Morloc.Operators
import Morloc.Types
import Morloc.Config (Config)
import qualified Morloc.Parser as MP
import qualified Morloc.Generator as MG
import Morloc.Typecheck (typecheck)

-- | Build a Morloc program as a graph stored in a SPARQL database
buildProgram :: SparqlDatabaseLike db => Config -> db -> MT.Text -> IO db
buildProgram config ep code = do
  tbl <- MP.parse config Nothing code >>= doOrDie >>= sparqlUpload ep
  typecheck tbl
  -- TODO: construct tbl
  return tbl

-- | Build a program as a local executable
writeProgram :: SparqlDatabaseLike db => Config -> db -> MT.Text -> IO ()
writeProgram config ep code = do
  buildProgram config ep code >>= MG.generate config >>= writeProgram'
  where
    writeProgram' :: (Script, [Script]) -> IO ()
    writeProgram' (n, ps) = do
      writeScript' True n
      mapM_ (writeScript' False) ps
      
    writeScript' :: Bool -> Script -> IO ()
    writeScript' isExe (Script base lang code') = do
      let f = base <> "." <> lang
      MT.writeFile f code'
      p <- SD.getPermissions f
      SD.setPermissions f (p {SD.executable = isExe})

-- | Compile a program to RDF. Does not depend on a local SPARQL database. It
-- also does not consider the local configuration or postprocessing and
-- typechecking.
writeRdfTo :: DR.RdfSerializer s => Config -> s -> MT.Text -> FilePath -> IO ()
writeRdfTo config s loc ttl = do
  rdf <- MP.parse config Nothing loc >>= doOrDie
  handle <- SIO.openFile ttl SIO.WriteMode
  DR.hWriteRdf s handle rdf
    <* SIO.hClose handle

-- | Triple serialization wrapper around `writeRdfTo`
writeTripleTo :: Config -> MT.Text -> FilePath -> IO ()
writeTripleTo config = writeRdfTo config DR.NTriplesSerializer

-- | Turtle serialization wrapper around `writeRdfTo`
writeTurtleTo :: Config -> MT.Text -> FilePath -> IO ()
writeTurtleTo config = writeRdfTo config (DR.TurtleSerializer Nothing (DR.PrefixMappings DMS.empty))

doOrDie :: ThrowsError a -> IO a
doOrDie (Right x) = return x
doOrDie (Left err) = fail $ show err ++ "\n"
