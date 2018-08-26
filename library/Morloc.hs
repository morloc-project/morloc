module Morloc (
    writeTurtle
  , writeTriple
  , writeProgram
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Map.Strict as DMS
import qualified Data.Text.IO as DTIO

import Morloc.Operators
import Morloc.Types
import qualified Morloc.Error as ME
import qualified Morloc.Parser as MP
import qualified Morloc.Generator as MG
import qualified Morloc.Database.HSparql.Upload as Up

writeProgram :: SparqlEndPoint -> DT.Text -> IO ()
writeProgram ep code = do
  MP.parse Nothing code >>= doOrDie >>= Up.uploadRDF ep >>= stateResult
  MG.generate ep >>= writeProgram'
  where
    stateResult :: Bool -> IO ()
    stateResult False = fail ("Failed to upload RDF to" ++ ep)
    stateResult True = return ()

    writeProgram' :: (Script, [Script]) -> IO ()
    writeProgram' (n, ps) = do
      writeScript' n
      mapM_ writeScript' ps
      
    writeScript' :: Script -> IO ()
    writeScript' (Script base lang code) =
      DTIO.writeFile (base <> "." <> lang) code
  

writeRDF' :: DR.RdfSerializer s => s -> DT.Text -> IO ()
writeRDF' serializer code
  =   MP.parse Nothing code
  >>= doOrDie
  >>= DR.writeRdf serializer

doOrDie :: ME.ThrowsError a -> IO a
doOrDie (Right x) = return x
doOrDir (Left err) = fail $ show err ++ "\n"

writeTurtle :: DT.Text -> IO ()
writeTurtle = writeRDF' (DR.TurtleSerializer Nothing (DR.PrefixMappings DMS.empty))

writeTriple :: DT.Text -> IO ()
writeTriple = writeRDF' DR.NTriplesSerializer
