{-# LANGUAGE OverloadedStrings #-}

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
  , lexeme
  , many1
  , sepBy2
  , freename
  , tvar
  , newvar
  , number
  , op
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
  , exprId
  , exprI
  ) where

import Data.Void (Void)
import Morloc.Frontend.Namespace
import Text.Megaparsec
import Text.Megaparsec.Char hiding (eol)
import qualified Control.Monad.State as CMS
import qualified Data.Scientific as DS
import qualified Data.Set as Set
import qualified Morloc.Data.Text as MT
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Morloc.Language as ML

type Parser a = CMS.StateT ParserState (Parsec Void MT.Text) a

data ParserState = ParserState {
    stateLang :: Maybe Lang
  , stateModulePath :: Maybe Path
  , stateVarIndex :: Int
  , stateExpIndex :: Int
  , stateGenerics :: [TVar] -- store the observed generic variables in the current type
                            -- you should reset the field before parsing a new type 
  , stateMinPos :: Pos
  , stateAccepting :: Bool
} deriving(Show)

emptyState :: ParserState
emptyState = ParserState {
    stateLang = Nothing
  , stateModulePath = Nothing
  , stateVarIndex = 1
  , stateExpIndex = 1
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
  let i = stateVarIndex s 
  CMS.put (s {stateVarIndex = i + 1}) 
  return (TV lang ("p" <> MT.show' i))

exprId :: Parser Int
exprId = do
  s <- CMS.get
  let i = stateExpIndex s
  CMS.put $ s { stateExpIndex = i + 1 }
  return i

exprI :: Expr -> Parser ExprI
exprI e = ExprI <$> exprId <*> pure e

setLang :: Maybe Lang -> Parser ()
setLang lang = do
  s <- CMS.get
  CMS.put (s { stateLang = lang })


setMinPos :: Parser ()
setMinPos = do
  s <- CMS.get
  level <- L.indentLevel
  CMS.put (s { stateMinPos = level })

-- | Require elements all start on the same line as the first element. At least
-- one expression must match.
align :: Parser a -> Parser [a] 
align p = do
  s <- CMS.get
  let minPos0 = stateMinPos s 
      accept0 = stateAccepting s
  curPos <- L.indentLevel
  xs <- many1 (resetPos curPos True >> p)
  -- put everything back the way it was
  resetPos minPos0 accept0
  return xs
  where
    resetPos :: Pos -> Bool -> Parser ()
    resetPos i r = do
      s' <- CMS.get
      CMS.put (s' {stateMinPos = i, stateAccepting = r})

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
          L.incorrectIndent EQ minPos curPos
    else
      if minPos < curPos
        then
          lexemeBase p
        else 
          L.incorrectIndent LT minPos curPos


resetGenerics :: Parser ()
resetGenerics = do
  s <- CMS.get
  CMS.put (s { stateGenerics = [] })

appendGenerics :: TVar -> Parser ()
appendGenerics v = do
  s <- CMS.get
  let gs = stateGenerics s
      gs' = if isGeneric v then v : gs else gs
  CMS.put (s {stateGenerics = gs'})

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

sepBy2 :: Parser a -> Parser s -> Parser [a]
sepBy2 p s = do
  x <- p
  _ <- s
  xs <- sepBy1 p s
  return (x:xs)
  

comments :: Parser ()
comments =  L.skipLineComment "--"
        <|> L.skipBlockCommentNested "{-" "-}"
        <?> "comment"

data Sign = Pos | Neg

number :: Parser (Either Integer DS.Scientific)
number = lexeme number_

number_ :: Parser (Either Integer DS.Scientific)
number_ = do
  x  <- try (fmap (Right . DS.fromFloatDigits) (L.signed sc L.float)) <|> fmap Left (L.signed sc L.decimal)
  e <- optional _exp
  return $ case (x, e) of
    (Left i,  Nothing) -> Left i
    (Right f, Nothing) -> Right f
    -- anything in scientific notation is cast as a real
    (Left i,  Just (Neg, expval)) -> Right  $ DS.scientific i ((-1) * expval)
    (Left i,  Just (Pos, expval)) -> Right $ DS.scientific i expval
    (Right f, Just (Pos, expval)) -> Right $ f * (10 ^^ expval)
    (Right f, Just (Neg, expval)) -> Right $ f * (10 ^^ (-1 * expval))
  where

  _exp :: Parser (Sign, Int)
  _exp = do
    _ <- char 'e'
    expsign <- _sign
    expval <- L.decimal
    return (expsign, expval)

  _sign :: Parser Sign
  _sign = do
    sign <- optional (char '-' <|> char '+')
    case sign of
      (Just '-') -> return Neg
      _ -> return Pos

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
    p = fmap MT.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '\'')
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
