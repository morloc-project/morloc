{-|
Module      : Morloc.Internal
Description : Internal utility functions
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

This module serves as a proto-prelude. Eventually I will probably want to
abandon the default prelude and create my own. But not just yet.
-}
module Morloc.Internal
  ( ifelse
  , module Data.Maybe
  , module Data.Either
  , module Data.List.Extra
  , module Control.Monad
  , module Data.Monoid
  -- ** operators
  , (|>>) -- piped fmap
  , (</>) -- Filesystem utility operators from System.FilePath
  ) where

-- Don't import anything from Morloc here. This module should be VERY lowest
-- in the hierarchy, to avoid circular dependencies, since the lexer needs to
-- access it.
import Data.List.Extra hiding (list) -- 'list' conflicts with Doc
import Data.Maybe
import Data.Either
import System.FilePath
import Control.Monad
import Data.Monoid

ifelse :: Bool -> a -> a -> a
ifelse True x _ = x
ifelse False _ y = y

-- | pipe the lhs functor into the rhs function
infixl 1 |>>

(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) = flip fmap
