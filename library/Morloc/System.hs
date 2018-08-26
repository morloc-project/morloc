{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.System
Description : Handle dependencies and environment setup
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.System
  ( 
      makeManifoldName
    , makePoolName
    , findExecutor
  ) where

import Morloc.Operators

import qualified Data.Text as DT

findExecutor :: DT.Text -> DT.Text
findExecutor "R" = "Rscript"
findExecutor _ = error "Only R is currently supported"

makePoolName :: DT.Text -> DT.Text
makePoolName lang = "pool." <> lang

makeManifoldName :: DT.Text -> DT.Text
makeManifoldName x = case reverse (DT.splitOn "/" x) of
  (y:ys) -> "m" <> y
  _ -> error "Manifold uri does not match the pattern `.*/\\d+$`"
