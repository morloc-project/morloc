module Morloc (turtle) where

import qualified Data.RDF as DR
import qualified Data.Map.Strict as DMS
import qualified Morloc.Error as ME
import qualified Morloc.Triple as M3
import qualified Morloc.Parser as MP
import qualified Morloc.Data as MD

turtle :: String -> IO ()
turtle s = case MP.morlocScript s of
  Left err -> putStr $ show err ++ "\n"
  Right (M3.TopRDF _ rdfOutput) ->
    DR.writeRdf (DR.TurtleSerializer Nothing (DR.PrefixMappings DMS.empty)) rdfOutput
