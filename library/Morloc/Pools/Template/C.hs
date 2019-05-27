{-|
Module      : C
Description : Build a C program given a file
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : totally experimental

The build process for C differs from that used in R and python since a
compilation step is needed. This code currently is wildly experimental.

-}

module Morloc.Pools.Template.C
( 
  generate
) where

import Morloc.Global

generate :: SparqlDatabaseLike db => db -> MorlocMonad Script
generate _ = undefined 
