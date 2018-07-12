module Morloc.State
(
    ParserState(..)
  , parserStateEmpty
  , Parser
  , getId
) where

import Text.Parsec hiding (Parser, State)

-- For now, the passed parser state is just an counter
data ParserState = ParserState {
    -- A program unique ID
    stateCount :: Int 
    -- The parent ID
  , stateScope :: Int 
}

parserStateEmpty :: ParserState
parserStateEmpty = ParserState {
    stateCount  = 0
  , stateScope  = 0
}

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

-- setScope = modifyState (\s -> s {stateScope = stateCount s})

getId :: Parser Int
getId = do
  modifyState (\s -> s {stateCount = (stateCount s) + 1})
  s <- getState
  return $ stateCount s
