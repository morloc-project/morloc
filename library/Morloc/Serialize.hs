{-|
Module      : Morloc.Serialize
Description : Select serialization functions for manifolds
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Serialize
  ( serialize
  ) where

import Morloc.Namespace

serialize :: [Manifold] -> MorlocMonad [Manifold]
serialize ms = return ms
