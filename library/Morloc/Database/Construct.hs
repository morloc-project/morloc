{-|
Module      : Morloc.Database.Construct
Description : Check the logical consistency of a program
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Database.Construct (construct) where

import Morloc.Types

construct :: SparqlEndPoint -> IO ()
construct ep = do
  putStrLn "  post-processing RDF graph ... "
