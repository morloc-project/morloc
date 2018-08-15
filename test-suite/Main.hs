import qualified Test.Tasty as TT
import Test.Tasty.Hspec as TTH

import ParserTests
import WalkerTests

main :: IO ()
main = do
  t1 <- TTH.testSpec "Parser: Morloc script to RDF" testParser
  -- t2 <- TTH.testSpec "Walker: RDF graph operations" testWalker
  TT.defaultMain $ TT.testGroup "Unit Tests" [t1, t2]
