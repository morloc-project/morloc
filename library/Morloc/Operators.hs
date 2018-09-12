{-|
Module      : Morloc.Operators
Description : Custom binary operators
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Operators
(
  -- * General purpose operators
    (|>>)
  , (<>)
  -- * Filesystem utility operators from System.FilePath
  , (</>) -- ^ join paths 
) where

import Data.Monoid
import System.FilePath

-- | pipe the lhs functor into the rhs function
infixl 1 |>>
(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) = flip fmap
