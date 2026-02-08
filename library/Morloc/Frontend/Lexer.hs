{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.Frontend.Lexer
Description : Lexing functions used in the parser Morloc
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Frontend.Lexer
  ( Parser
  , ParserState (..)
  , align
  , foldMany
  , indentFreeTerm
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
  , hole
  , freename
  , freenameU
  , freenameL
  , moduleComponent
  , number
  , op
  , operatorName
  , parenOperator
  , parens
  , reserved
  , reservedWords
  , resetGenerics
  , sc
  , setMinPos
  , stringLiteral
  , stringPatterned
  , parsePatternSetter
  , parsePatternGetter
  , surround
  , symbol
  , pLang
  , exprId
  , exprI

    -- * docstring parsers
  , parseArgDocStr
  , parseFlagDocStr
  , parseLineDocStr
  , parseTextDocStr
  , parseWordDocStr
  , parseIntsDocStr
  ) where

import qualified Control.Monad.State as CMS
import qualified Data.Char as DC
import qualified Data.Map.Strict as Map
import qualified Data.Scientific as DS
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void (Void)
import qualified Morloc.Data.Text as MT
import Morloc.Frontend.Namespace
import qualified Morloc.Language as ML
import Text.Megaparsec
import Text.Megaparsec.Char hiding (eol)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = CMS.StateT ParserState (Parsec Void Text) a

data ParserState = ParserState
  { stateModulePath :: Maybe Path
  , stateVarIndex :: Int
  , stateExpIndex :: Int
  , stateGenerics :: [TVar] -- store the observed generic variables in the current type
  -- you should reset the field before parsing a new type
  , stateMinPos :: Pos
  , stateAccepting :: Bool
  , stateIgnoreAlignment :: Bool
  , stateModuleConfig :: ModuleConfig
  , stateSourcePositions :: Map.Map Int SrcLoc
  }
  deriving (Show)

emptyState :: ParserState
emptyState =
  ParserState
    { stateModulePath = Nothing
    , stateVarIndex = 1
    , stateExpIndex = 1
    , stateGenerics = []
    , stateMinPos = mkPos 1
    , stateAccepting = False
    , stateIgnoreAlignment = False
    , stateModuleConfig = defaultValue
    , stateSourcePositions = Map.empty
    }

exprId :: Parser Int
exprId = do
  s <- CMS.get
  let i = stateExpIndex s
  CMS.put $ s {stateExpIndex = i + 1}
  return i

exprI :: Expr -> Parser ExprI
exprI e = do
  i <- exprId
  pos <- getSourcePos
  s <- CMS.get
  CMS.put $ s { stateSourcePositions = Map.insert i (toSrcLoc pos) (stateSourcePositions s) }
  return (ExprI i e)

toSrcLoc :: SourcePos -> SrcLoc
toSrcLoc pos = SrcLoc
  { srcLocPath = Just (sourceName pos)
  , srcLocLine = unPos (sourceLine pos)
  , srcLocCol = unPos (sourceColumn pos)
  }

setMinPos :: Parser ()
setMinPos = do
  s <- CMS.get
  level <- L.indentLevel
  CMS.put (s {stateMinPos = level})

-- | A general function for parsing aligned things
alignGen :: Bool -> (Parser () -> Parser a) -> Parser a
alignGen flexibleStart p = do
  s <- CMS.get
  let minPos0 = stateMinPos s
      accept0 = stateAccepting s

  minPos <- case flexibleStart of
    True -> L.indentLevel
    False -> return minPos0

  x <- p (resetPos minPos True)

  -- put everything back the way it was
  resetPos minPos0 accept0
  return x
  where
    resetPos :: Pos -> Bool -> Parser ()
    resetPos i r = do
      s' <- CMS.get
      CMS.put (s' {stateMinPos = i, stateAccepting = r})

-- | Map over one or more aligned elements
align :: Parser a -> Parser [a]
align p = alignGen True (\reset -> many1 (reset >> try p))

-- | Fold over 0 or more elements
foldMany :: a -> (a -> Parser a) -> Parser a
foldMany x p = do
  mayX <- optional (p x)
  case mayX of
    (Just x') -> foldMany x' p
    Nothing -> return x

alignInset :: Parser a -> Parser [a]
alignInset p = isInset >> align p

isInset :: Parser ()
isInset = do
  minPos <- CMS.gets stateMinPos
  curPos <- L.indentLevel
  when (curPos <= minPos) (L.incorrectIndent GT minPos curPos)

sc :: Parser ()
sc = L.space space1 comments empty

symbol :: Text -> Parser Text
symbol = lexeme . L.symbol sc

lexemeBase :: Parser a -> Parser a
lexemeBase = L.lexeme sc

lexeme :: Parser a -> Parser a
lexeme p = do
  minPos <- CMS.gets stateMinPos
  s <- CMS.get
  curPos <- L.indentLevel

  -- if indent doesn't matter at for this parse
  if stateIgnoreAlignment s
    -- then just handle terminal whitespace
    then lexemeBase p
    else
      -- if we are awaiting exactly aligned input
      if stateAccepting s
        then
          -- if we are exactly aligned
          if curPos == minPos
            then
              CMS.put (s {stateAccepting = False}) >> lexemeBase p
            -- otherwise die
            else
              L.incorrectIndent EQ minPos curPos
        -- if we are not waiting for aligned input
        else
          -- we are indented further than
          if minPos < curPos
            then
              lexemeBase p
            -- otherwise die
            else
              L.incorrectIndent LT minPos curPos

indentFreeTerm :: Parser a -> Parser a
indentFreeTerm p = do
  s <- CMS.get
  CMS.put (s {stateIgnoreAlignment = True})
  xEither <- observing p
  CMS.put s
  case xEither of
    (Left _) -> failure Nothing Set.empty
    (Right x) -> return x

resetGenerics :: Parser ()
resetGenerics = do
  s <- CMS.get
  CMS.put (s {stateGenerics = []})

appendGenerics :: TVar -> Parser ()
appendGenerics v = do
  s <- CMS.get
  let gs = stateGenerics s
      gs' = if isGeneric (unTVar v) then v : gs else gs
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
  return (x : xs)

comments :: Parser ()
comments =
  try lineComment
    <|> L.skipBlockCommentNested "{-" "-}"
    <?> "comment"
  where
    lineComment :: Parser ()
    lineComment = do
      _ <- string "--"
      -- ignore special comments
      notFollowedBy (oneOf ['\'', '^'])
      _ <- takeWhileP Nothing (/= '\n')
      return ()

docstr :: Parser ()
docstr = do
  _ <- string "--'"
  _ <- hspace
  return ()

parseFlagDocStr :: Text -> Parser Bool
parseFlagDocStr flag = lexeme $ do
  _ <- docstr
  _ <- string (flag <> ":")
  _ <- hspace
  value <- parseTrue <|> parseFalse
  return value
  where
    parseTrue :: Parser Bool
    parseTrue = do
      string "true"
      return True

    parseFalse :: Parser Bool
    parseFalse = do
      string "false"
      return False

parseTextDocStr :: Text -> Parser Text
parseTextDocStr flag = lexeme $ do
  _ <- docstr
  string (flag <> ":")
  _ <- hspace
  value <- takeWhileP Nothing (/= '\n')
  return value

parseIntsDocStr :: Text -> Parser [Int]
parseIntsDocStr flag = lexeme $ do
  _ <- docstr
  string (flag <> ":")
  _ <- hspace
  value <- many1 (lexeme L.decimal)
  return value

parseWordDocStr :: Text -> Parser Text
parseWordDocStr flag = lexeme $ do
  _ <- docstr
  string (flag <> ":")
  _ <- hspace
  value <- many1 (alphaNumChar <|> char '-' <|> char '_')
  return $ MT.pack value

parseLineDocStr :: Parser Text
parseLineDocStr = lexeme $ do
  _ <- docstr
  text <- takeWhileP Nothing (/= '\n')
  return text

parseArgDocStr :: Text -> Parser CliOpt
parseArgDocStr flag = lexeme $ do
  _ <- docstr
  string (flag <> ":")
  _ <- hspace
  mayShort <- optional (try parseShortDocStr)
  case mayShort of
    (Just short) -> do
      mayLong <- optional (char '/' >> parseLongDocStr)
      case mayLong of
        (Just long) -> return $ CliOptBoth short long
        Nothing -> return $ CliOptShort short
    Nothing -> parseLongDocStr |>> CliOptLong
  where
    parseShortDocStr :: Parser Char
    parseShortDocStr = do
      char '-'
      short <- alphaNumChar
      return $ short

    parseLongDocStr :: Parser Text
    parseLongDocStr = do
      char '-'
      char '-'
      longArgStart <- alphaNumChar
      longArgRest <- many (alphaNumChar <|> char '-' <|> char '_')
      let longArg = MT.pack $ longArgStart : longArgRest
      return longArg

data Sign = Pos | Neg

number :: Parser (Either Integer DS.Scientific)
number = lexeme $ do
  x <-
    try (fmap (Right . DS.fromFloatDigits) signedFloat)
      <|> try unsignedHex
      <|> try unsignedOctal
      <|> try unsignedBinary
      <|> fmap Left signedDecimal
  e <- optional _exp
  return $ case (x, e) of
    (Left i, Nothing) -> Left i
    (Right f, Nothing) -> Right f
    -- anything in scientific notation is cast as a real
    (Left i, Just (Neg, expval)) -> Right $ DS.scientific i ((-1) * expval)
    (Left i, Just (Pos, expval)) -> Right $ DS.scientific i expval
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

    -- Hexadecimal: 0x or 0X prefix (unsigned only)
    unsignedHex :: Parser (Either Integer DS.Scientific)
    unsignedHex = do
      _ <- string "0x" <|> string "0X"
      fmap Left L.hexadecimal

    -- Octal: 0o or 0O prefix (unsigned only)
    unsignedOctal :: Parser (Either Integer DS.Scientific)
    unsignedOctal = do
      _ <- string "0o" <|> string "0O"
      fmap Left L.octal

    -- Binary: 0b or 0B prefix (unsigned only)
    unsignedBinary :: Parser (Either Integer DS.Scientific)
    unsignedBinary = do
      _ <- string "0b" <|> string "0B"
      fmap Left L.binary

    -- No space consumer - signs must be directly attached to numbers
    noSpace :: Parser ()
    noSpace = return ()

    -- Signed floating point (no space between sign and number)
    signedFloat :: Parser Double
    signedFloat = L.signed noSpace L.float

    -- Signed decimal integer (no space between sign and number)
    signedDecimal :: Parser Integer
    signedDecimal = L.signed noSpace L.decimal

surround :: Parser l -> Parser r -> Parser a -> Parser a
surround l r v = do
  _ <- l
  v' <- v
  _ <- r
  return v'

brackets :: Parser a -> Parser a
brackets = surround (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = surround (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = surround (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = surround (symbol "<") (symbol ">")

reservedWords :: [Text]
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
  , "instance"
  , "class"
  , "infixl"
  , "infixr"
  , "infix"
  ]

reservedOperators :: [Text]
reservedOperators =
  [ "="
  , ":"
  ]

operatorChars :: String
operatorChars = ":!$%&*+./<=>?@\\^|-~#"

op :: Text -> Parser Text
op o = (lexeme . try) (string o <* notFollowedBy (oneOf operatorChars))

-- | Parse an operator name (sequence of operator characters)
operatorName :: Parser Text
operatorName = (lexeme . try) $ do
  firstChar <- oneOf operatorChars
  restChars <- many (oneOf operatorChars)
  let opName = MT.pack (firstChar : restChars)
  if opName `elem` reservedOperators
    then failure Nothing Set.empty -- TODO: error message
    else return opName

-- | Parse an operator in parentheses (for use as a variable)
-- Example: (+), (*), (<$>)
parenOperator :: Parser EVar
parenOperator = lexeme $ do
  _ <- char '('
  opName <- operatorName
  _ <- char ')'
  return (EV opName)

reserved :: Text -> Parser Text
reserved w = try (symbol w)

stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  _ <- char '\"'
  s <- many (noneOf ['"'])
  _ <- char '\"'
  return $ MT.pack s

parsePatternSetter :: Parser a -> Parser (Selector, [a])
parsePatternSetter parseValue = parsePattern (sc >> symbol "=" >> parseValue)

parsePatternGetter :: Parser Selector
parsePatternGetter = fst <$> parsePattern (return True)

parsePattern :: Parser a -> Parser (Selector, [a])
parsePattern parseValue = lexeme $ try parseIdx <|> try parseKey <|> try parseGrpIdx <|> try parseGrpKey
  where
    parseIdx = parseSegment L.decimal SelectorIdx

    parseKey = parseSegment key SelectorKey
      where
        key = do
          firstLetter <- lowerChar
          remaining <- many (alphaNumChar <|> char '\'' <|> char '_')
          return $ MT.pack (firstLetter : remaining)

    -- parseSegment :: Parser p -> ((p, Selector) -> [(p, Selector)] -> Selector) -> Parser (Selector, [a])
    parseSegment fieldParser constructor = do
      _ <- char '.'
      k <- fieldParser
      mayS <- optional (parsePattern parseValue)
      case mayS of
        (Just (s, es)) -> return (constructor (k, s) [], es)
        Nothing -> do
          e <- parseValue
          return (constructor (k, SelectorEnd) [], [e])

    parseGrpKey = parseGrp parseKey

    parseGrpIdx = parseGrp parseIdx

    parseGrp grpparser = lexeme $ do
      _ <- char '.'
      xs <- parens (sepBy1 grpparser (symbol ","))
      let es = concat $ map snd xs
      case flattenSelector (map fst xs) of
        (s, SelectorEnd) -> return (s, es)
        (SelectorEnd, s) -> return (s, es)
        _ -> error "Unreachable - grpparser can only return on selector type"

    flattenSelector :: [Selector] -> (Selector, Selector)
    flattenSelector sss = (flatIdx, flatKey)
      where
        flatIdx = case concat [s : ss | (SelectorIdx s ss) <- sss] of
          [] -> SelectorEnd
          ss -> SelectorIdx (head ss) (tail ss)

        flatKey = case concat [s : ss | (SelectorKey s ss) <- sss] of
          [] -> SelectorEnd
          ss -> SelectorKey (head ss) (tail ss)

stringPatterned :: Parser a -> Parser (Either Text (Text, [(a, Text)]))
stringPatterned exprParser = do
  x <- stringLiteralMultiline exprParser <|> stringLiteralDoubleQuote exprParser
  case x of
    (s, []) -> return $ Left s
    (s, es) -> return $ Right (s, es)

stringLiteralDoubleQuote :: Parser a -> Parser (Text, [(a, Text)])
stringLiteralDoubleQuote exprParser = lexeme $ do
  _ <- char '\"'
  s <- interpolatedStringParser "\"" exprParser
  _ <- char '\"'
  return $ s

stringLiteralMultiline :: Parser a -> Parser (Text, [(a, Text)])
stringLiteralMultiline exprParser = lexeme $ do
  sep <- string "'''" <|> string "\"\"\""
  (s, exprs) <- interpolatedStringParser sep exprParser
  _ <- string sep
  return . reindent . removeTrailingSpace $ (removeLeadingSpace s, exprs)
  where
    --  1. Remove zero or one leading newlines
    removeLeadingSpace :: Text -> Text
    removeLeadingSpace (MT.lines -> []) = ""
    removeLeadingSpace (MT.lines -> (s : rs))
      | MT.null (MT.strip s) = MT.unlines rs
      | otherwise = MT.unlines (s : rs)

    --  2. Remove the final newline and preceding non-newline space
    removeTrailingSpace :: (Text, [(a, Text)]) -> (Text, [(a, Text)])
    removeTrailingSpace (MT.lines -> [], []) = ("", [])
    removeTrailingSpace x@(MT.lines -> ss, [])
      | MT.null . MT.strip . last $ ss = (MT.unlines (init ss), [])
      | otherwise = x
    removeTrailingSpace (s, ss) = case (init ss, last ss) of
      (initLines, (e, MT.lines -> slines)) ->
        if MT.null . MT.strip . last $ slines
          then (s, initLines <> [(e, MT.unlines (init slines))])
          else (s, ss)

    --  3. Trim an initial number of space from each line equal to the minimum
    --     number of starting spaces
    reindent :: (Text, [(a, Text)]) -> (Text, [(a, Text)])
    reindent (MT.lines -> ss, []) = (MT.unlines $ map (MT.drop initSpaces) ss, [])
      where
        initSpaces = minimum $ map (MT.length . MT.takeWhile DC.isSpace) ss
    reindent (s, xs) = (replaceFun s, map (second replaceFun) xs)
      where
        replaceFun = MT.replace replacePattern "\n"
        replacePattern = MT.pack $ '\n' : take initSpace (repeat ' ')
        initSpace = minimum $ map (MT.length . MT.takeWhile DC.isSpace) (MT.lines $ MT.concat (s : map snd xs))

-- | Parse an interpolated string with embedded expressions
interpolatedStringParser :: Text -> Parser a -> Parser (Text, [(a, Text)])
interpolatedStringParser sep exprParser = do
  initial <- literalText sep
  parts <- many $ do
    expr <- interpolatedExpr exprParser
    text <- literalText sep
    pure (expr, text)
  pure (initial, parts)

-- | Parse literal text until we hit an interpolation or end
literalText :: Text -> Parser Text
literalText sep = MT.pack <$> many literalChar
  where
    literalChar = notFollowedBy (string "#{" <|> string sep) >> anySingle

-- | Parse an interpolated expression: #{...}
interpolatedExpr :: Parser a -> Parser a
interpolatedExpr exprParser = string "#{" *> exprParser <* char '}'

hole :: Parser ()
hole = lexeme $ do
  _ <- char '_'
  return ()

mkFreename :: Parser Char -> Parser Text
mkFreename firstLetter = (lexeme . try) (p >>= check)
  where
    p = fmap MT.pack $ (:) <$> firstLetter <*> many (alphaNumChar <|> char '\'' <|> char '_')
    check x =
      if x `elem` reservedWords
        then failure Nothing Set.empty -- TODO: error message
        else return x

freename :: Parser Text
freename = mkFreename letterChar

-- part of a module path, must start with a lower-case letter
-- may have uppercase or digits or dashes after the first letter
moduleComponent :: Parser Text
moduleComponent = lexeme $ do
  firstLetter <- lowerChar
  nextLetters <- many (alphaNumChar <|> char '-')
  return $ MT.pack (firstLetter : nextLetters)

freenameL :: Parser Text
freenameL = mkFreename lowerChar

freenameU :: Parser Text
freenameU = mkFreename upperChar

-- | match the name of a supported language
pLang :: Parser Lang
pLang = do
  langStr <- freename
  case ML.readLangName langStr of
    (Just lang) -> return lang
    Nothing ->
      fancyFailure . Set.singleton . ErrorFail $
        "Langage '" <> MT.unpack langStr <> "' is not supported"
