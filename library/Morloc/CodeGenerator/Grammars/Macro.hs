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
  , stateKindArgsSkipped :: Int
  }

-- | Substitute positional macros @$N@ in the template with the given
-- type-arg renderings. The 'kindArgsSkipped' parameter counts how many
-- kind-kinded (e.g. Nat) parameters the caller filtered out before
-- calling; it is used only to enrich the error message when the
-- template's macro indices overrun the supplied args.
--
-- The convention: @$N@ is 1-based over the TYPE args only. Kind args
-- (phantom dims, type-level strings) are structural metadata, never
-- referenced by macros. Callers must filter kind args before calling.
expandMacro :: Text -> [Text] -> Int -> Text
expandMacro t [] _ = t
expandMacro t ps kindArgsSkipped =
  case runParser (pBase <* eof) (ParserState t ps kindArgsSkipped) "typemacro" t of
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
      kindSkipped = stateKindArgsSkipped st
  _ <- string "$"
  n <- read <$> many1 digit
  -- index is 1-based
  let i = n - 1
      kindHint = if kindSkipped > 0
        then "\n  Note: $N indexes TYPE parameters only; the "
              <> show kindSkipped
              <> " kind-kinded parameter(s) (e.g. Nat phantom dims) "
              <> "of this type are not counted. If you intended to "
              <> "reference a kind parameter, see the per-language form "
              <> "convention -- only type parameters are valid macro targets."
        else ""
  if i >= length xs
    then error $ "expandMacro: macro index $" <> show n
              <> " refers to type-parameter position " <> show n
              <> " but only " <> show (length xs)
              <> " type-parameter(s) were given.\n"
              <> "  Template:   " <> show tmpl <> "\n"
              <> "  Type args:  " <> show xs
              <> kindHint <> "\n"
              <> "  This may mean a type-alias expansion stripped a "
              <> "parameter that the language-specific macro template still "
              <> "references, or that the morloc-side and language-specific "
              <> "declarations of the type disagree on type-arity."
    else return (xs !! i)
