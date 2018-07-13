module Morloc.State
(
    Parser
  , ParserState(..)
  , parserStateEmpty
  , getId
  , setScope
  , setScope'
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

setScope :: Int -> Parser ()
setScope i = do
  modifyState (\s -> s {stateScope = i})

setScope' :: Parser ()
setScope' = do
  modifyState (\s -> s {stateScope = (stateCount s) - 1})

getId :: Parser Int
getId = do
  modifyState (\s -> s {stateCount = (stateCount s) + 1})
  s <- getState
  return $ stateCount s
