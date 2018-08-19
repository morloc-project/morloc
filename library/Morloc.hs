module Morloc (
    writeTurtle
  , writeTriple
  , writeProgram
  , doit -- FIXME this is only a test
) where

import qualified Data.RDF as DR
import qualified Data.Text as DT
import qualified Data.Map.Strict as DMS
import qualified Data.Text.IO as DTIO
import qualified Database.HSparql.Connection as DHC

import Morloc.Operators
import qualified Morloc.Error as ME
import qualified Morloc.Parser as MP
import qualified Morloc.Generator as MG
import qualified Morloc.Query as MQ

doit :: DHC.EndPoint -> IO ()
doit ep = do
  (Just result) <- DHC.selectQuery ep MQ.findallTypes
  putStrLn . show $ result

writeProgram :: DT.Text -> IO ()
writeProgram s = fmap ((=<<) MG.generate) (MP.parse Nothing s) >>= writeProgram'
  where
    writeProgram' :: ME.ThrowsError (MG.Script, [MG.Script]) -> IO ()
    writeProgram' (Right (n, ps)) = do
      writeScript' n
      mapM_ writeScript' ps
    writeProgram' (Left err) = putStr (show err)
      
    writeScript' :: MG.Script -> IO ()
    writeScript' (MG.Script base lang code) =
      DTIO.writeFile (base <> "." <> lang) code

writeRDF' :: DR.RdfSerializer s => s -> DT.Text -> IO ()
writeRDF' serializer code
  =   MP.parse Nothing code
  >>= doOrDie (DR.writeRdf serializer)

-- writeRDF' serializer code = case MP.parse code of
--   Left err -> putStr $ show err ++ "\n"
--   Right rdfOutput -> DR.writeRdf serializer rdfOutput

doOrDie :: (a -> IO ()) -> ME.ThrowsError a -> IO ()
doOrDie f (Right x) = f x
doOrDie _ (Left err) = putStr $ show err ++ "\n"

writeTurtle :: DT.Text -> IO ()
writeTurtle = writeRDF' (DR.TurtleSerializer Nothing (DR.PrefixMappings DMS.empty))

writeTriple :: DT.Text -> IO ()
writeTriple = writeRDF' DR.NTriplesSerializer
