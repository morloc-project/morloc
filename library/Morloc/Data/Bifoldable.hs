{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Morloc.Data.Bifoldable
Description : The Bifoldable typeclass with monadic instances
Copyright   : (c) Zebulun Arendsee, 2016-2024
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Data.Bifoldable (Bifoldable(..)) where

import Data.Foldable (foldlM, foldrM)
import Control.Monad.Identity (runIdentity)
import Morloc.Data.Bifunctor
import Data.Maybe (catMaybes)

return2 :: Monad m => (a -> b -> c) -> (a -> b -> m c)
return2 f x y = return $ f x y

class (Bifunctor f) => Bifoldable f where
  bilistM :: Monad m => (a -> m c) -> (b -> m c) -> f a b -> m [c]

  bilistsndM :: Monad m => (b -> m c) -> f a b -> m [c]
  bilistsndM f = fmap catMaybes . bilistM (return . const Nothing) (fmap Just . f)

  bilistfstM :: Monad m => (a -> m c) -> f a b -> m [c]
  bilistfstM f = fmap catMaybes . bilistM (fmap Just . f) (return . const Nothing)

  biappendM :: (Monad m, Monoid c) => (a -> m c) -> (b -> m c) -> f a b -> m c
  biappendM f g = fmap mconcat . bilistM f g

  bicatM :: (Monad m, Foldable t) => (a -> m c) -> (b -> m c) -> t (f a b) -> m [c]
  bicatM f g xs = foldrM (\e' b -> (<>) b <$> bilistM f g e') [] xs

  bifoldMapM :: (Foldable t, Monoid c, Monad m) => (a -> m c) -> (b -> m c) -> t (f a b) -> m c
  bifoldMapM f g = fmap mconcat . bicatM f g

  bifoldrM :: (Monoid c, Foldable t, Monad m) => (a -> c -> m c) -> (b -> c -> m c) -> c -> t (f a b) -> m c
  bifoldrM f g c xs = foldrM (\x c' -> biappendM (`f` c') (`g` c') x) c xs

  bifoldlM :: (Monoid c, Foldable t, Monad m) => (c -> a -> m c) -> (c -> b -> m c) -> c -> t (f a b) -> m c
  bifoldlM f g c xs = foldlM (\c' x -> biappendM (f c') (g c') x) c xs

  bifoldr1M :: (Monoid c, Foldable t, Monad m) => (a -> c -> m c) -> (b -> c -> m c) -> t (f a b) -> m c
  bifoldr1M f g xs = foldrM (\x c' -> biappendM (`f` c') (`g` c') x) mempty xs

  bifoldl1M :: (Monoid c, Foldable t, Monad m) => (c -> a -> m c) -> (c -> b -> m c) -> t (f a b) -> m c
  bifoldl1M f g xs = foldlM (\c' x -> biappendM (f c') (g c') x) mempty xs

  ubimapM :: (a ~ b, Monad m) => (a -> m c) -> f a b -> m (f c c)
  ubimapM f = bimapM f f

  ubiappendM :: (Monoid c, a ~ b, Monad m) => (a -> m c) -> f a b -> m c
  ubiappendM f = biappendM f f

  ubicatM :: (Foldable t, a ~ b, Monad m) => (a -> m c) -> t (f a b) -> m [c]
  ubicatM f = bicatM f f

  ubifoldMapM :: (Foldable t, Monoid c, a ~ b, Monad m) => (a -> m c) -> t (f a b) -> m c
  ubifoldMapM f = bifoldMapM f f

  ubilistM :: (a ~ b, Monad m) => (a -> m c) -> f a b -> m [c]
  ubilistM f = bilistM f f

  ubifoldrM :: (Monoid c, Foldable t, a ~ b, Monad m) => (a -> c -> m c) -> c -> t (f a b) -> m c
  ubifoldrM f = bifoldrM f f

  ubifoldlM :: (Monoid c, Foldable t, a ~ b, Monad m) => (c -> a -> m c) -> c -> t (f a b) -> m c
  ubifoldlM f = bifoldlM f f

  ubifoldr1M :: (Monoid c, Foldable t, a ~ b, Monad m) => (a -> c -> m c) -> t (f a b) -> m c
  ubifoldr1M f = bifoldr1M f f

  ubifoldl1M :: (Monoid c, Foldable t, a ~ b, Monad m) => (c -> a -> m c) -> t (f a b) -> m c
  ubifoldl1M f = bifoldl1M f f

  bilist :: (a -> c) -> (b -> c) -> f a b -> [c]
  bilist f g = runIdentity . bilistM (return . f) (return . g)

  bilistsnd :: (b -> c) -> f a b -> [c]
  bilistsnd f = runIdentity . bilistsndM (return . f)

  bilistfst :: (a -> c) -> f a b -> [c]
  bilistfst f = runIdentity . bilistfstM (return . f)

  biappend :: (Monoid c) => (a -> c) -> (b -> c) -> f a b -> c
  biappend f g = runIdentity . biappendM (return . f) (return . g)

  bicat :: (Foldable t) => (a -> c) -> (b -> c) -> t (f a b) -> [c]
  bicat f g = runIdentity . bicatM (return . f) (return . g)

  bifoldMap :: (Foldable t, Monoid c) => (a -> c) -> (b -> c) -> t (f a b) -> c
  bifoldMap f g = runIdentity . bifoldMapM (return . f) (return . g)

  bifoldr :: (Monoid c, Foldable t) => (a -> c -> c) -> (b -> c -> c) -> c -> t (f a b) -> c
  bifoldr f g c = runIdentity . bifoldrM (\b x -> return $ f b x) (\b x -> return $ g b x) c

  bifoldl :: (Monoid c, Foldable t) => (c -> a -> c) -> (c -> b -> c) -> c -> t (f a b) -> c
  bifoldl f g c = runIdentity . bifoldlM (\x b -> return $ f x b) (\x b -> return $ g x b) c

  bifoldr1 :: (Monoid c, Foldable t) => (a -> c -> c) -> (b -> c -> c) -> t (f a b) -> c
  bifoldr1 f g = runIdentity . bifoldrM (\b x -> return $ f b x) (\b x -> return $ g b x) mempty

  bifoldl1 :: (Monoid c, Foldable t) => (c -> a -> c) -> (c -> b -> c) -> t (f a b) -> c
  bifoldl1 f g = runIdentity . bifoldlM (\x b -> return $ f x b) (\x b -> return $ g x b) mempty

  ubimap :: (a ~ b) => (a -> c) -> f a b -> f c c
  ubimap f = runIdentity . ubimapM (return . f)

  ubiappend :: (Monoid c, a ~ b) => (a -> c) -> f a b -> c
  ubiappend f = runIdentity . ubiappendM (return . f)

  ubicat :: (Foldable t, a ~ b) => (a -> c) -> t (f a b) -> [c]
  ubicat f = runIdentity . ubicatM (return . f)

  ubifoldMap :: (Foldable t, Monoid c, a ~ b) => (a -> c) -> t (f a b) -> c
  ubifoldMap f = runIdentity . ubifoldMap (return . f)

  ubilist :: (a ~ b) => (a -> c) -> f a b -> [c]
  ubilist f = runIdentity . ubilistM (return . f)

  ubifoldr :: (Monoid c, Foldable t, a ~ b) => (a -> c -> c) -> c -> t (f a b) -> c
  ubifoldr f c = runIdentity . ubifoldrM (return2 f) c

  ubifoldl :: (Monoid c, Foldable t, a ~ b) => (c -> a -> c) -> c -> t (f a b) -> c
  ubifoldl f c = runIdentity . ubifoldlM (return2 f) c

  ubifoldr1 :: (Monoid c, Foldable t, a ~ b) => (a -> c -> c) -> t (f a b) -> c
  ubifoldr1 f = runIdentity . ubifoldr1M (return2 f)

  ubifoldl1 :: (Monoid c, Foldable t, a ~ b) => (c -> a -> c) -> t (f a b) -> c
  ubifoldl1 f = runIdentity . ubifoldl1M (return2 f)


instance Bifoldable Either where
  bilistM f _ (Left a) = do
    a' <- f a
    return [a']
  bilistM _ g (Right b) = do
    b' <- g b
    return [b']

instance Bifoldable (,) where
  bilistM f g (a, b) = do
    a' <- f a
    b' <- g b
    return [a', b']
