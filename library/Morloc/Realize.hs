{-|
Module      : Morloc.Realize
Description : Functions for dealing with Manifolds
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Realize
  ( realize
  ) where

import Morloc.Data.Doc
import Morloc.Namespace
import qualified Morloc.Monad as MM

-- | @realize@ determines which instances to use for each manifold.
realize ::
     [Manifold]
  -- ^ Abstract manifolds with possibly multiple realizations or none.
  -> MorlocMonad [Manifold]-- ^ Uniquely realized manifolds (e.g., mRealizations has exactly one element)
realize ms = do
  let ms' = map (chooseRealization ms) ms
  return $ map (compInit ms') ms'
    -- initialize composition realizations
  where
    compInit :: [Manifold] -> Manifold -> Manifold
    compInit ms'' m
      | mDefined m = makeRealization m (mRealizations (findChild ms'' m))
      | otherwise = m
    findChild :: [Manifold] -> Manifold -> Manifold
    findChild ms'' m =
      case filter (\n -> mComposition n == (Just (mMorlocName m))) ms'' of
        (child:_) -> child
        xs ->
          error
            ("error in findChild: m=" <> show m <> " --- " <> "xs=" <> show xs)
    makeRealization :: Manifold -> [Realization] -> Manifold
    makeRealization p rs =
      p {mRealizations = map (\r -> r {rSourced = False}) rs}

-- | Chooses which manifold instance to use when there are multiple
-- alternatives. It should choose the instance that that is supported on the
-- current system and that maximizes performance (e.g., by avoiding foreign
-- calls). Currently, however, it just chooses whichever realization is first
-- in the list.
chooseRealization :: [Manifold] -> Manifold -> Manifold
chooseRealization _ m
  -- -------------^ ignored, since I am not using context yet
  = case mRealizations m of
    [] -> m
    [_] -> m
    (x:_) -> m {mRealizations = [x]}
