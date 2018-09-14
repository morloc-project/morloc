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
import qualified Morloc.Parser as MP
import qualified Morloc.Database.HSparql.Upload as Up
import qualified Morloc.Generator as MG
import Morloc.Database.Configure
import Morloc.Database.Typecheck
import Morloc.Database.Construct

-- | Build a Morloc program as a graph stored in a SPARQL database
buildProgram :: SparqlDatabaseLike db => db -> MT.Text -> IO db
buildProgram ep code = do
  configure
  tbl <- MP.parse Nothing code >>= doOrDie >>= sparqlUpload ep
  construct tbl
  typecheck tbl
  return tbl

-- | Build a program as a local executable
writeProgram :: SparqlDatabaseLike db => db -> MT.Text -> IO ()
writeProgram ep code = do
  buildProgram ep code >>= MG.generate >>= writeProgram'
  where
    writeProgram' :: (Script, [Script]) -> IO ()
    writeProgram' (n, ps) = do
      writeScript' True n
      mapM_ (writeScript' False) ps
      
    writeScript' :: Bool -> Script -> IO ()
    writeScript' isExe (Script base lang code) = do
      let f = base <> "." <> lang
      MT.writeFile f code
      p <- SD.getPermissions f
      SD.setPermissions f (p {SD.executable = isExe})

-- | Compile a program to RDF. Does not depend on a local SPARQL database. It
-- also does not consider the local configuration or postprocessing and
-- typechecking.
writeRdfTo :: DR.RdfSerializer s => s -> MT.Text -> FilePath -> IO ()
writeRdfTo s loc ttl = do
  rdf <- MP.parse Nothing loc >>= doOrDie
  handle <- SIO.openFile ttl SIO.WriteMode
  DR.hWriteRdf s handle rdf
    <* SIO.hClose handle

-- | Triple serialization wrapper around `writeRdfTo`
writeTripleTo :: MT.Text -> FilePath -> IO ()
writeTripleTo = writeRdfTo DR.NTriplesSerializer

-- | Turtle serialization wrapper around `writeRdfTo`
writeTurtleTo :: MT.Text -> FilePath -> IO ()
writeTurtleTo = writeRdfTo (DR.TurtleSerializer Nothing (DR.PrefixMappings DMS.empty))

doOrDie :: ThrowsError a -> IO a
doOrDie (Right x) = return x
doOrDie (Left err) = fail $ show err ++ "\n"
