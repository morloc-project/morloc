import qualified Test.Tasty as TT
import Test.Tasty.Hspec as TTH

import ParserTests

main :: IO ()
main = do
  t1 <- TTH.testSpec "Parser: Morloc script to RDF" testParser
  TT.defaultMain $ TT.testGroup "Unit Tests" [t1]
