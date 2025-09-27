{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.CodeGenerator.Grammars.Macro
Description : Expand parameters in concrete types
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Grammars.Macro
(   expandMacro
) where

import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.Text as MT
import qualified Control.Monad.State as CMS
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = CMS.StateT ParserState (Parsec Void MT.Text) a

newtype ParserState = ParserState { stateParameters :: [MT.Text] }

expandMacro :: MT.Text -> [MT.Text] -> MT.Text
expandMacro t [] = t
expandMacro t ps =
  case runParser
         (CMS.runStateT (pBase <* eof) (ParserState ps))
         "typemacro"
         t of
    Left err' -> error (show err')
    Right (es, _) -> es


many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

pBase :: Parser MT.Text
pBase = MT.concat <$> many1 (pChar <|> pMacro)

pChar :: Parser MT.Text
pChar = MT.pack <$> many1 (noneOf ['$'])

pMacro :: Parser MT.Text
pMacro = do
  xs <- CMS.gets stateParameters
  _ <- string "$"
  n <- L.decimal
  -- index is 1-based
  let i = n - 1
  return (xs !! i)
