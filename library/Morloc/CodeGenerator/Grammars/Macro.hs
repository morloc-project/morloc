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

data ParserState = ParserState
  { stateTemplate :: Text
  , stateParameters :: [Text]
  }

expandMacro :: Text -> [Text] -> Text
expandMacro t [] = t
expandMacro t ps =
  case runParser (pBase <* eof) (ParserState t ps) "typemacro" t of
    Left err' -> error (show err')
    Right es -> es

-- The pMacro arity check below stays. It produces a clear error if a macro
-- template ever references an index past the parameter list.

pBase :: Parser Text
pBase = MT.concat <$> many1 (pChar <|> pMacro)

pChar :: Parser Text
pChar = MT.pack <$> many1 (noneOf ['$'])

pMacro :: Parser Text
pMacro = do
  st <- getState
  let xs = stateParameters st
      tmpl = stateTemplate st
  _ <- string "$"
  n <- read <$> many1 digit
  -- index is 1-based
  let i = n - 1
  if i >= length xs
    then error $ "expandMacro: macro index $" <> show n
              <> " refers to position " <> show n
              <> " but only " <> show (length xs)
              <> " parameter(s) were given.\n"
              <> "  Template:   " <> show tmpl <> "\n"
              <> "  Parameters: " <> show xs <> "\n"
              <> "  This may mean a type-alias expansion stripped a "
              <> "parameter that the language-specific macro template still "
              <> "references, or that the morloc-side and language-specific "
              <> "declarations of the type disagree on arity."
    else return (xs !! i)
