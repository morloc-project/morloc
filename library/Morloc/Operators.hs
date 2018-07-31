module Morloc.Operators
(
    (|>>)
  , (<>)
) where

import Data.Monoid

infixl 1 |>>
(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) = flip fmap
