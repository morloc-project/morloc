module Morloc.State
(
    ParserState
  , Parser
  , T
  , withCount
  , pushTriple
) where

import Text.Parsec hiding (Parser, State)

import Morloc.Triple

-- For now, the passed parser state is just an counter
type ParserState = (Int, [Triple])

-- A numbered token
type T a = (Int, a)

-- data ParsecT s u m a
-- where
--   s := stream type
--   u := user state type
--   m := underlying monad
--   a := return type
--
-- type Parsec s u = ParsecT s u Identity
-- type Parser = Parsec String ()
-- 
-- Here, MorlocParser deviates from Parser (defined in Text.Parsec.String) by
-- passing Integer state.
type Parser = Parsec String ParserState

withCount :: Parser a -> Parser (T a)
withCount p = do 
  x <- p
  modifyState (\(i, ts) -> (i+1,ts))
  stat <- getState
  return (fst stat, x)

pushTriple :: Subject -> RelObj -> Parser ()
pushTriple s t = do
  modifyState (\(i, ts) -> (i, (s, t):ts))

pushTripleMaybe :: Subject -> Maybe RelObj -> Parser ()
pushTripleMaybe s (Just t) = do
  modifyState (\(i, ts) -> (i, (s, t):ts))
pushTripleMaybe _ Nothing = do
  modifyState (\x -> x)
