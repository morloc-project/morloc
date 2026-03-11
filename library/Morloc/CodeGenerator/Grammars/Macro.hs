{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Macro
Description : Expand parameters in concrete types
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Grammars.Macro
  ( expandMacro
  ) where

import Data.Text (Text)
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.Text as MT
import Text.Parsec (Parsec, runParser, eof, many1, getState)
import Text.Parsec.Char (noneOf, string, digit)
import Text.Parsec.Text ()

type Parser = Parsec Text ParserState

newtype ParserState = ParserState {stateParameters :: [Text]}

expandMacro :: Text -> [Text] -> Text
expandMacro t [] = t
expandMacro t ps =
  case runParser (pBase <* eof) (ParserState ps) "typemacro" t of
    Left err' -> error (show err')
    Right es -> es

pBase :: Parser Text
pBase = MT.concat <$> many1 (pChar <|> pMacro)

pChar :: Parser Text
pChar = MT.pack <$> many1 (noneOf ['$'])

pMacro :: Parser Text
pMacro = do
  xs <- stateParameters <$> getState
  _ <- string "$"
  n <- read <$> many1 digit
  -- index is 1-based
  let i = n - 1
  return (xs !! i)
