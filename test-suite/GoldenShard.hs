{- |
Module      : GoldenShard
Description : Split the golden tests across independent test runs

CI runs the suite as several jobs per platform. Each job sets
@MORLOC_TEST_SHARD=i/n@ and runs the @i@th of @n@ shards: every @n@th golden
test directory in sorted order, starting from the @i@th. Unit tests run only in
shard 1. Unset, the whole suite runs.
-}
module GoldenShard
  ( Shard (..)
  , shardEnvVar
  , parseShard
  , lookupShard
  , selectShard
  ) where

import Data.Char (isDigit, isSpace)
import System.Environment (lookupEnv)

-- | The 1-based index of this run and the number of runs.
data Shard = Shard Int Int
  deriving (Eq, Show)

shardEnvVar :: String
shardEnvVar = "MORLOC_TEST_SHARD"

-- | Parse @i/n@ with @1 <= i <= n@.
parseShard :: String -> Either String Shard
parseShard raw =
  case break (== '/') s of
    (is, '/' : ns)
      | isNat is && isNat ns ->
          let i = read is
              n = read ns
           in if n >= 1 && i >= 1 && i <= n
                then Right (Shard i n)
                else Left bad
    _ -> Left bad
  where
    s = reverse . dropWhile isSpace . reverse . dropWhile isSpace $ raw
    isNat x = not (null x) && all isDigit x
    bad = shardEnvVar ++ "='" ++ raw ++ "' is not of the form i/n with 1 <= i <= n"

-- | The shard named by the environment, if any. A malformed value is an
-- error rather than a silent full run, so a CI typo cannot quietly run every
-- test in every job.
lookupShard :: IO (Maybe Shard)
lookupShard = do
  v <- lookupEnv shardEnvVar
  case v of
    Nothing -> return Nothing
    Just raw -> either (ioError . userError) (return . Just) (parseShard raw)

-- | Every @n@th item, starting from the @i@th.
selectShard :: Shard -> [a] -> [a]
selectShard (Shard i n) xs = [x | (k, x) <- zip [0 ..] xs, k `mod` n == i - 1]
