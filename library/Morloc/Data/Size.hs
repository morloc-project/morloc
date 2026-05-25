{- |
Module      : Morloc.Data.Size
Description : Parse human-readable byte counts (k/m/g suffixes)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Data.Size
  ( parseHumanSize
  ) where

import Data.Char (toLower)
import Data.Int (Int64)

-- | Parse a non-negative byte count with an optional k/m/g suffix
-- (binary, 1024-based). Returns @Left@ on empty input, negatives,
-- non-digit prefixes, unknown suffixes, or overflow of 'Int64'.
--
-- >>> parseHumanSize "64"
-- Right 64
-- >>> parseHumanSize "64k"
-- Right 65536
-- >>> parseHumanSize "1G"
-- Right 1073741824
-- >>> parseHumanSize "-1"
-- Left "expected a non-negative integer, optionally with a k/m/g suffix"
parseHumanSize :: String -> Either String Int64
parseHumanSize s = case break (not . (`elem` ['0' .. '9'])) s of
  ("", _)     -> Left "expected a non-negative integer, optionally with a k/m/g suffix"
  (digs, []) -> finish digs 1
  (digs, [c]) -> case toLower c of
    'k' -> finish digs 1024
    'm' -> finish digs (1024 * 1024)
    'g' -> finish digs (1024 * 1024 * 1024)
    _   -> Left ("unknown size suffix " ++ show c ++ "; use k, m, or g")
  _ -> Left ("could not parse size " ++ show s ++ "; expected digits with optional k/m/g suffix")
  where
    finish digs mult =
      let n = read digs :: Integer
          v = n * mult
      in if v > toInteger (maxBound :: Int64)
           then Left ("size " ++ show s ++ " exceeds 64-bit range")
           else Right (fromInteger v)
