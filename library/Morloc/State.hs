module Morloc.State
(
    Parser
  , ParserState(..)
  , parserStateEmpty
  , getId
) where

import Text.Parsec hiding (Parser, State)

-- data ParsecT s u m a
-- where
--   s := stream type
--   u := user state type
--   m := underlying monad
--   a := return type
--
-- type Parsec s u = ParsecT s u Identity
type Parser = Parsec String ParserState

data ParserState = ParserState {
    -- an ID that is unique to the program
    stateCount :: Int 
}

parserStateEmpty :: ParserState
parserStateEmpty = ParserState {
    stateCount  = 0
}

getId :: Parser Int
getId = do
  modifyState (\s -> s {stateCount = (stateCount s) + 1})
  s <- getState
  return $ stateCount s
