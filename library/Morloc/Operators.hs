module Morloc.Operators
(
    (|>>)
  , (<>)
) where

infixl 1 |>>
(|>>) :: Functor f => f a -> (a -> b) -> f b
(|>>) = flip fmap

infixr 6 <>
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
