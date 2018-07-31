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
    (|>>)
  , (<>)
) where

import Data.Monoid

infixl 1 |>>
(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) = flip fmap
