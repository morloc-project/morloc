{-|
Module      : Morloc.Database.Configure
Description : Manage user configuration
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Database.Configure (configure) where

import Morloc.Types

configure :: SparqlDatabaseLike db => db -> IO ()
configure ep = do
  putStrLn "  loading config ... "
