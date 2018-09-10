{-|
Module      : Morloc.Database.Typecheck
Description : Check the logical consistency of a program
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Database.Typecheck (typecheck) where

import Morloc.Types

typecheck :: SparqlEndPoint -> IO ()
typecheck ep = do
  putStrLn "  typechecking RDF graph ... "
