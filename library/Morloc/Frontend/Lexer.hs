{-|
Module      : Morloc.Frontend.Lexer
Description : Lexing functions used in the parser Morloc
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Lexer
  ( Parser
  , ParserState(..)
  , align
  , alignInset
  , angles
  , appendGenerics
  , braces
  , brackets
  , comments
  , emptyState
  , isInset
  , lexeme
  , lexemeBase
  , many1
  , freename
  , name
  , tvar
  , newvar
  , number
  , op
  , operatorChars
  , parens
  , reserved
  , reservedWords
  , resetGenerics
  , sc
  , setLang
  , setMinPos
  , stringLiteral
  , surround
  , symbol
  , pLang
  , tag
  ) where

import Data.Void (Void)
import Morloc.Frontend.Namespace
import Text.Megaparsec
import Text.Megaparsec.Char hiding (eol)
import qualified Control.Monad.State as CMS
import qualified Data.Scientific as DS
import qualified Data.Set as Set
import qualified Data.Char as DC
import qualified Morloc.Data.Text as MT
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Morloc.Language as ML

type Parser a = CMS.StateT ParserState (Parsec Void MT.Text) a

data ParserState = ParserState {
    stateLang :: Maybe Lang
  , stateModulePath :: Maybe Path
  , stateIndex :: Int
  , stateGenerics :: [TVar] -- store the observed generic variables in the current type
                            -- you should reset the field before parsing a new type 
  , stateMinPos :: Pos
  , stateAccepting :: Bool
} deriving(Show)

emptyState :: ParserState
emptyState = ParserState {
    stateLang = Nothing
  , stateModulePath = Nothing
  , stateIndex = 1
  , stateGenerics = []
  , stateMinPos = mkPos 1
  , stateAccepting = False
}

tvar :: MT.Text -> Parser TVar
tvar v = do
  lang <- CMS.gets stateLang
  return $ TV lang v

newvar :: Maybe Lang -> Parser TVar
newvar lang = do
  s <- CMS.get
  let i = stateIndex s 
  CMS.put (s {stateIndex = i + 1}) 
  return (TV lang ("p" <> MT.show' i))

setLang :: Maybe Lang -> Parser ()
setLang lang = do
  s <- CMS.get
  CMS.put (s { stateLang = lang })


setMinPos :: Parser ()
setMinPos = do
  s <- CMS.get
  level <- L.indentLevel
  CMS.put (s { stateMinPos = level })

-- Require elements all start on the same line as the first element
align :: Parser a -> Parser [a] 
align p = do
  s <- CMS.get
  let minPos = stateMinPos s 
      accept = stateAccepting s
  curPos <- L.indentLevel
  xs <- many (CMS.put (s {stateMinPos = curPos, stateAccepting = True}) >> p)
  -- put everything back the way it was
  CMS.put (s {stateMinPos = minPos, stateAccepting = accept})
  return xs

alignInset :: Parser a -> Parser [a]
alignInset p = isInset >> align p

isInset :: Parser ()
isInset = do
  minPos <- CMS.gets stateMinPos
  curPos <- L.indentLevel
  if curPos <= minPos
    then
      L.incorrectIndent GT minPos curPos
    else
      return ()

sc = L.space space1 comments empty

symbol = lexeme . L.symbol sc

lexemeBase :: Parser a -> Parser a
lexemeBase = L.lexeme sc

lexeme :: Parser a -> Parser a
lexeme p = do
  minPos <- CMS.gets stateMinPos
  accepting <- CMS.gets stateAccepting
  s <- CMS.get
  curPos <- L.indentLevel
  if accepting
    then
      if curPos == minPos
        then
          CMS.put (s { stateAccepting = False }) >> lexemeBase p
        else
          L.incorrectIndent GT minPos curPos
    else
      if minPos < curPos
        then
          lexemeBase p
        else 
          L.incorrectIndent GT minPos curPos


resetGenerics :: Parser ()
resetGenerics = do
  s <- CMS.get
  CMS.put (s { stateGenerics = [] })

appendGenerics :: TVar -> Parser ()
appendGenerics v@(TV _ vstr) = do
  s <- CMS.get
  let isGeneric = maybe False (DC.isLower . fst) (MT.uncons vstr)
      gs = stateGenerics s
      gs' = if isGeneric then v : gs else gs
  CMS.put (s {stateGenerics = gs'})

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

comments :: Parser ()
comments =  L.skipLineComment "--"
        <|> L.skipBlockCommentNested "{-" "-}"
        <?> "comment"

number :: Parser DS.Scientific
number = lexeme $ L.signed sc L.scientific

surround :: Parser l -> Parser r -> Parser a -> Parser a
surround l r v = do
  l
  v' <- v
  r
  return v'

brackets :: Parser a -> Parser a
brackets = surround (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = surround (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = surround (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = surround (symbol "<") (symbol ">")

reservedWords :: [MT.Text]
reservedWords =
  [ "module"
  , "source"
  , "from"
  , "where"
  , "import"
  , "export"
  , "as"
  , "True"
  , "False"
  , "type"
  ]

operatorChars :: String
operatorChars = ":!$%&*+./<=>?@\\^|-~#"

op :: MT.Text -> Parser MT.Text
op o = (lexeme . try) (symbol o <* notFollowedBy (oneOf operatorChars))

reserved :: MT.Text -> Parser MT.Text
reserved w = try (symbol w)

stringLiteral :: Parser MT.Text
stringLiteral = lexeme $ do
  _ <- char '\"'
  s <- many (noneOf ['"'])
  _ <- char '\"'
  return $ MT.pack s

freename :: Parser MT.Text
freename = (lexeme . try) (p >>= check)
  where
    p = fmap MT.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x =
      if elem x reservedWords
        then failure Nothing Set.empty -- TODO: error message
        else return x

name :: Parser MT.Text
name = (lexeme . try) (p >>= check)
  where
    p = fmap MT.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x =
      if elem x reservedWords
        then failure Nothing Set.empty -- TODO: error message
        else return x

-- | match the name of a supported language
pLang :: Parser Lang
pLang = do
  langStr <- freename
  case ML.readLangName langStr of
    (Just lang) -> return lang
    Nothing -> fancyFailure . Set.singleton . ErrorFail
      $ "Langage '" <> MT.unpack langStr <> "' is not supported"

-- | match an optional tag that precedes some construction
tag :: Parser a -> Parser (Maybe MT.Text)
tag p = optional (try tag')
  where
    tag' = do
      l <- freename
      _ <- op ":"
      _ <- lookAhead p
      return l
