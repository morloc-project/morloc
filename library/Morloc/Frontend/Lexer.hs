{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{- |
Module      : Morloc.Frontend.Lexer
Description : Hand-written lexer for Morloc with layout token insertion
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Tokenizes morloc source code and inserts virtual layout tokens
({, }, ;) for indentation-sensitive blocks (module bodies, where clauses,
class/instance bodies, do-blocks).
-}
module Morloc.Frontend.Lexer
  ( lexMorloc
  , LexError (..)
  , showLexError
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, isOctDigit, isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Morloc.Frontend.Token

data LexError = LexError !Pos !String
  deriving (Show, Eq)

showLexError :: LexError -> String
showLexError (LexError pos msg) =
  posFile pos ++ ":" ++ show (posLine pos) ++ ":" ++ show (posCol pos) ++ ": " ++ msg

-- | Lex morloc source code into a token stream with layout tokens inserted,
-- plus a map from positions to associated docstring lines, and a list of
-- group annotation tokens (for command group support in export lists).
lexMorloc :: String -> Text -> Either LexError ([Located], Map.Map Pos [Text], [Located])
lexMorloc filename input = do
  rawTokens <- lexRaw filename (T.unpack input) (startPos filename)
  let (docMap, groupToks, filtered) = extractDocstrings rawTokens
  return (insertLayout filtered, docMap, groupToks)

-- | Extract docstring and group annotation tokens. Docstrings are associated
-- with the position of the following non-doc token. Group annotation tokens
-- are returned in order for post-processing.
extractDocstrings :: [Located] -> (Map.Map Pos [Text], [Located], [Located])
extractDocstrings = go [] Map.empty [] []
  where
    go _acc docMap groupToks accToks [] = (docMap, reverse groupToks, reverse accToks)
    go acc docMap groupToks accToks (Located _ (TokDocLine txt) _ : rest) =
      go (acc ++ [txt]) docMap groupToks accToks rest
    go acc docMap groupToks accToks (tok@(Located _ (TokGroupLine _) _) : rest) =
      -- Flush pending docstrings to the group annotation position, then record the group token
      let docMap' = if null acc then docMap else Map.insert (locPos tok) acc docMap
      in go [] docMap' (tok : groupToks) accToks rest
    go acc docMap groupToks accToks (tok@(Located pos _ _) : rest) =
      let docMap' = if null acc then docMap else Map.insert pos acc docMap
      in go [] docMap' groupToks (tok : accToks) rest

-- Raw lexer state
data LexState = LexState
  { lsInput  :: !String    -- remaining input
  , lsPos    :: !Pos       -- current position
  , lsTokens :: ![Located] -- accumulated tokens (reversed)
  }

-- | Lex into raw tokens (no layout processing)
lexRaw :: String -> String -> Pos -> Either LexError [Located]
lexRaw _filename input pos0 = go (LexState input pos0 [])
  where
    go :: LexState -> Either LexError [Located]
    go st = case lsInput st of
      [] -> Right (reverse (Located (lsPos st) TokEOF "" : lsTokens st))
      _  -> do
        st' <- lexOne st
        go st'

-- | Lex a single token, advancing the state
lexOne :: LexState -> Either LexError LexState
lexOne st@(LexState input pos toks) = case input of
  -- Whitespace
  '\n' : rest -> Right st { lsInput = rest, lsPos = nextLine pos }
  c : rest | c == ' ' || c == '\t' || c == '\r' ->
    Right st { lsInput = rest, lsPos = advanceCol pos 1 }

  -- Block comments {- ... -}
  '{' : '-' : rest -> skipBlockComment (advanceCol pos 2) rest (1 :: Int)
    where
      skipBlockComment p s 0 = Right st { lsInput = s, lsPos = p }
      skipBlockComment p ('{' : '-' : s) n = skipBlockComment (advanceCol p 2) s (n + 1)
      skipBlockComment p ('-' : '}' : s) n = skipBlockComment (advanceCol p 2) s (n - 1)
      skipBlockComment p ('\n' : s) n = skipBlockComment (nextLine p) s n
      skipBlockComment p (_ : s) n = skipBlockComment (advanceCol p 1) s n
      skipBlockComment p [] _ = Left (LexError p "unterminated block comment")

  -- Group annotation comments: --* ...
  '-' : '-' : '*' : rest ->
    let (line, rest') = span (/= '\n') rest
        txt = T.pack line
        len = 3 + length line
    in Right st { lsInput = rest', lsPos = advanceCol pos len
                , lsTokens = Located pos (TokGroupLine txt) (T.pack ("--*" ++ line)) : toks }

  -- Docstring comments: --' ...
  '-' : '-' : '\'' : rest ->
    let (line, rest') = span (/= '\n') rest
        txt = T.pack line
        len = 3 + length line
    in Right st { lsInput = rest', lsPos = advanceCol pos len
                , lsTokens = Located pos (TokDocLine txt) (T.pack ("--'" ++ line)) : toks }

  -- Line comments: -- (but not --' or --^)
  '-' : '-' : rest
    | not (null rest) && head rest `elem` ['\'', '^'] ->
        Left (LexError pos "unexpected docstring marker")
    | otherwise ->
        let (_, rest') = span (/= '\n') rest
        in Right st { lsInput = rest', lsPos = advanceCol pos (2 + length (takeWhile (/= '\n') rest)) }

  -- Triple-quoted strings
  '\'' : '\'' : '\'' : rest -> lexMultilineString pos "'''" rest st
  '"' : '"' : '"' : rest -> lexMultilineString pos "\"\"\"" rest st

  -- Double-quoted strings (with interpolation support)
  '"' : rest -> lexDoubleString pos rest st

  -- Numbers: try hex, octal, binary first, then decimal/float
  '0' : 'x' : rest -> lexHexNumber pos rest st
  '0' : 'X' : rest -> lexHexNumber pos rest st
  '0' : 'o' : rest -> lexOctalNumber pos rest st
  '0' : 'O' : rest -> lexOctalNumber pos rest st
  '0' : 'b' : rest -> lexBinaryNumber pos rest st
  '0' : 'B' : rest -> lexBinaryNumber pos rest st
  c : _ | isDigit c -> lexDecNumber pos input st

  -- Delimiters and punctuation
  '(' : rest -> emit1 TokLParen "(" rest
  ')' : rest -> emit1 TokRParen ")" rest
  '[' : rest -> emit1 TokLBracket "[" rest
  ']' : rest -> emit1 TokRBracket "]" rest
  '{' : rest -> emit1 TokLBrace "{" rest
  '}' : rest -> emit1 TokRBrace "}" rest
  ',' : rest -> emit1 TokComma "," rest
  ';' : rest -> emit1 TokSemicolon ";" rest

  -- Underscore: standalone '_' is a hole, '_var' is an identifier
  '_' : c : rest | isAlphaNum c || c == '\'' || c == '_' ->
    -- identifier starting with underscore (e.g., _do_5)
    lexIdent pos ('_' : c : rest) st
  '_' : rest ->
    emit1 TokUnderscore "_" rest

  -- Backslash (lambda)
  '\\' : rest -> emit1 TokBackslash "\\" rest

  -- Dot: TokGetterDot when immediately followed by lowercase letter, digit, or '(' (getter/setter),
  -- TokDot otherwise (composition operator, module separator).
  -- When followed by an operator char (e.g., '..' or '.='), falls through to the operator lexer.
  -- When followed by a digit, emit both TokGetterDot and TokInteger to prevent float parsing
  -- (e.g., .1.2 should be getter .1 then getter .2, not getter then float 1.2)
  '.' : c : rest | isDigit c ->
    let (digits, rest') = span isDigit (c : rest)
        val = read digits :: Integer
        dotPos = pos
        numPos = advanceCol pos 1
    in Right st { lsInput = rest', lsPos = advanceCol numPos (length digits)
                , lsTokens = Located numPos (TokInteger val) (T.pack digits)
                           : Located dotPos TokGetterDot "."
                           : toks }
  '.' : c : rest | isLower c || c == '(' ->
    emit1 TokGetterDot "." (c : rest)
  '.' : c : rest | not (isOperatorChar c) ->
    emit1 TokDot "." (c : rest)
  '.' : [] ->
    emit1 TokDot "." []

  -- Bang -- special: ! is force operator, !! is chained force
  -- But !!! or != etc. are user-defined operators (fall through to operator lexer)
  '!' : '!' : c : rest | not (isOperatorChar c) ->
    Right st { lsInput = c : rest, lsPos = advanceCol pos 2
             , lsTokens = Located (advanceCol pos 1) TokBang "!"
                        : Located pos TokBang "!" : toks }
  '!' : '!' : [] ->
    Right st { lsInput = [], lsPos = advanceCol pos 2
             , lsTokens = Located (advanceCol pos 1) TokBang "!"
                        : Located pos TokBang "!" : toks }
  '!' : c : rest | not (isOperatorChar c) ->
    emit1 TokBang "!" (c : rest)
  '!' : [] ->
    emit1 TokBang "!" []

  -- Operators and reserved operator sequences
  c : rest | isOperatorChar c -> lexOperator pos (c : rest) st

  -- Identifiers and keywords
  c : rest | isAlpha c -> lexIdent pos (c : rest) st

  -- Negative numbers: sign directly attached
  -- This is handled in the parser by parsing - as a unary operator

  -- Unknown character
  c : _ -> Left (LexError pos ("unexpected character: " ++ show c))
  [] -> Right st -- handled by go

  where
    emit1 tok txt rest =
      Right st { lsInput = rest, lsPos = advanceCol pos (T.length txt)
               , lsTokens = Located pos tok txt : toks }

-- | Lex an identifier or keyword. The first character may be a letter or underscore.
lexIdent :: Pos -> String -> LexState -> Either LexError LexState
lexIdent pos input st =
  let (word, rest) = spanIdent input
      txt = T.pack word
      tok = classifyWord txt
      len = length word
  in Right st { lsInput = rest, lsPos = advanceCol pos len
              , lsTokens = Located pos tok txt : lsTokens st }

spanIdent :: String -> (String, String)
spanIdent [] = ([], [])
spanIdent (c : cs)
  | isAlpha c || c == '_' =
      let (rest, remaining) = span (\x -> isAlphaNum x || x == '\'' || x == '_') cs
      in (c : rest, remaining)
  | otherwise = ([], c : cs)

-- | Module component: lowercase start, may contain dashes
-- We handle dashes in module names in the parser by combining tokens.
-- The lexer just produces normal identifiers.

classifyWord :: Text -> Token
classifyWord "module"   = TokModule
classifyWord "import"   = TokImport
classifyWord "export"   = TokExport
classifyWord "source"   = TokSource
classifyWord "from"     = TokFrom
classifyWord "where"    = TokWhere
classifyWord "as"       = TokAs
classifyWord "True"     = TokTrue
classifyWord "False"    = TokFalse
classifyWord "type"     = TokType
classifyWord "record"   = TokRecord
classifyWord "object"   = TokObject
classifyWord "table"    = TokTable
classifyWord "class"    = TokClass
classifyWord "instance" = TokInstance
classifyWord "infixl"   = TokInfixl
classifyWord "infixr"   = TokInfixr
classifyWord "infix"    = TokInfix
classifyWord "let"      = TokLet
classifyWord "in"       = TokIn
classifyWord "do"       = TokDo
classifyWord t
  | isUpper (T.head t) = TokUpperName t
  | otherwise           = TokLowerName t

-- | Lex an operator
lexOperator :: Pos -> String -> LexState -> Either LexError LexState
lexOperator pos input st =
  let (opStr, rest) = span isOperatorChar input
      txt = T.pack opStr
      tok = classifyOp txt
      len = length opStr
  in Right st { lsInput = rest, lsPos = advanceCol pos len
              , lsTokens = Located pos tok txt : lsTokens st }

classifyOp :: Text -> Token
classifyOp "::" = TokDColon
classifyOp "->" = TokArrow
classifyOp "=>" = TokFatArrow
classifyOp "<-" = TokBind
classifyOp "="  = TokEquals
classifyOp ":"  = TokColon
classifyOp "*"  = TokStar
classifyOp "-"  = TokMinus
-- < and > are also operators but we need them as angle brackets in some contexts
-- The parser handles disambiguation
classifyOp "<"  = TokLAngle
classifyOp ">"  = TokRAngle
classifyOp t    = TokOperator t

isOperatorChar :: Char -> Bool
isOperatorChar c = c `elem` (":!$%&*+./<=>?@\\^|-~#" :: String)

-- | Lex a double-quoted string, handling interpolation
lexDoubleString :: Pos -> String -> LexState -> Either LexError LexState
lexDoubleString start input st = go (advanceCol start 1) input []
  where
    go pos ('"' : rest) acc =
      let txt = T.pack (reverse acc)
          fullTxt = "\"" <> txt <> "\""
      in Right st { lsInput = rest, lsPos = advanceCol pos 1
                  , lsTokens = Located start (TokString txt) fullTxt : lsTokens st }
    go pos ('#' : '{' : rest) acc =
      -- start of interpolation
      let prefix = T.pack (reverse acc)
          tok = if null (lsTokens st) || not (isStringContinuation (lsTokens st))
                then TokStringStart prefix
                else TokStringMid prefix
          prefixTxt = "\"" <> prefix <> "#{"
      in lexInterpBody (advanceCol pos 2) rest 1
           st { lsTokens = Located (advanceCol pos 0) TokInterpOpen "#{" : Located start tok prefixTxt : lsTokens st }
           start
    go pos ('\\' : c : rest) acc =
      let escaped = case c of
            'n'  -> '\n'
            't'  -> '\t'
            '\\' -> '\\'
            '"'  -> '"'
            _    -> c
      in go (advanceCol pos 2) rest (escaped : acc)
    go pos ('\n' : _) _ = Left (LexError pos "unterminated string literal (use triple quotes for multi-line strings)")
    go pos (c : rest) acc = go (advanceCol pos 1) rest (c : acc)
    go pos [] _ = Left (LexError pos "unterminated string literal")

    isStringContinuation (Located _ (TokStringStart _) _ : _) = True
    isStringContinuation (Located _ (TokStringMid _) _ : _) = True
    isStringContinuation _ = False

-- | Lex the body of an interpolation #{...}, tracking brace depth
lexInterpBody :: Pos -> String -> Int -> LexState -> Pos -> Either LexError LexState
lexInterpBody pos ('}' : rest) 1 st stringStartPos =
  -- end of interpolation, resume string lexing
  let st' = st { lsTokens = Located pos TokInterpClose "}" : lsTokens st }
  in lexStringAfterInterp (advanceCol pos 1) rest st' stringStartPos
lexInterpBody pos ('}' : rest) depth st strPos =
  lexInterpBody (advanceCol pos 1) rest (depth - 1) st strPos
lexInterpBody pos ('{' : rest) depth st strPos =
  lexInterpBody (advanceCol pos 1) rest (depth + 1) st strPos
lexInterpBody pos input _ st _ = do
  -- lex one token from the interpolated expression
  st' <- lexOne st { lsInput = input, lsPos = pos }
  -- continue lexing the interpolation body
  case lsInput st' of
    [] -> Left (LexError pos "unterminated string interpolation")
    _ -> case lsTokens st' of
      (Located _ TokEOF _ : _) -> Left (LexError pos "unterminated string interpolation")
      _ -> do
        -- figure out remaining brace depth from what was consumed
        let consumed = length input - length (lsInput st')
            braceChange = countBraces (take consumed input)
        lexInterpBody (lsPos st') (lsInput st') (1 + braceChange) st' (Pos 0 0 "")
  where
    countBraces = foldl (\n c -> case c of '{' -> n + 1; '}' -> n - 1; _ -> n) 0

-- | After interpolation closes, resume lexing the string
lexStringAfterInterp :: Pos -> String -> LexState -> Pos -> Either LexError LexState
lexStringAfterInterp pos ('"' : rest) st _ =
  let txt = T.empty
  in Right st { lsInput = rest, lsPos = advanceCol pos 1
              , lsTokens = Located pos (TokStringEnd txt) "\"" : lsTokens st }
lexStringAfterInterp pos ('#' : '{' : rest) st strStartPos =
  -- another interpolation immediately
  let tok = TokStringMid T.empty
  in lexInterpBody (advanceCol pos 2) rest 1
       st { lsTokens = Located pos TokInterpOpen "#{" : Located pos tok "" : lsTokens st }
       strStartPos
lexStringAfterInterp pos input st _ = go pos input []
  where
    go p ('"' : rest) acc =
      let txt = T.pack (reverse acc)
      in Right st { lsInput = rest, lsPos = advanceCol p 1
                  , lsTokens = Located pos (TokStringEnd txt) ("" <> txt <> "\"") : lsTokens st }
    go p ('#' : '{' : rest) acc =
      let txt = T.pack (reverse acc)
      in lexInterpBody (advanceCol p 2) rest 1
           st { lsTokens = Located p TokInterpOpen "#{" : Located pos (TokStringMid txt) "" : lsTokens st }
           pos
    go p ('\\' : c : rest) acc =
      let escaped = case c of
            'n'  -> '\n'
            't'  -> '\t'
            '\\' -> '\\'
            '"'  -> '"'
            _    -> c
      in go (advanceCol p 2) rest (escaped : acc)
    go p ('\n' : _) _ = Left (LexError p "unterminated string literal")
    go p (c : rest) acc = go (advanceCol p 1) rest (c : acc)
    go p [] _ = Left (LexError p "unterminated string literal")

-- | Lex a multiline (triple-quoted) string with interpolation
lexMultilineString :: Pos -> String -> String -> LexState -> Either LexError LexState
lexMultilineString start delim input st = go (advanceCol start 3) input []
  where
    delimLen = length delim

    go pos s acc
      | take delimLen s == delim =
          let rawTxt = T.pack (reverse acc)
              txt = processMultilineString rawTxt
              fullTxt = T.pack delim <> rawTxt <> T.pack delim
          in Right st { lsInput = drop delimLen s, lsPos = advanceCol pos delimLen
                      , lsTokens = Located start (TokString txt) fullTxt : lsTokens st }
    go pos ('#' : '{' : rest) acc =
      -- interpolation inside multiline string
      let prefix = T.pack (reverse acc)
          tok = TokStringStart prefix
      in lexMultilineInterpBody (advanceCol pos 2) rest 1
           st { lsTokens = Located pos TokInterpOpen "#{" : Located start tok "" : lsTokens st }
           start delim
    go pos ('\\' : c : rest) acc =
      let escaped = case c of
            'n'  -> '\n'
            't'  -> '\t'
            '\\' -> '\\'
            '\'' -> '\''
            '"'  -> '"'
            _    -> c
      in go (advanceCol pos 2) rest (escaped : acc)
    go pos ('\n' : rest) acc = go (nextLine pos) rest ('\n' : acc)
    go pos (c : rest) acc = go (advanceCol pos 1) rest (c : acc)
    go pos [] _ = Left (LexError pos "unterminated multiline string literal")

-- | Lex interpolation body inside a multiline string
lexMultilineInterpBody :: Pos -> String -> Int -> LexState -> Pos -> String -> Either LexError LexState
lexMultilineInterpBody pos ('}' : rest) 1 st strStartPos delim =
  -- end of interpolation, resume multiline string
  let st' = st { lsTokens = Located pos TokInterpClose "}" : lsTokens st }
  in lexMultilineAfterInterp (advanceCol pos 1) rest st' strStartPos delim
lexMultilineInterpBody pos input _ st _ _ = do
  st' <- lexOne st { lsInput = input, lsPos = pos }
  case lsInput st' of
    [] -> Left (LexError pos "unterminated string interpolation")
    ('}' : rest) ->
      let st'' = st' { lsTokens = Located (lsPos st') TokInterpClose "}" : lsTokens st' }
      in lexMultilineAfterInterp (advanceCol (lsPos st') 1) rest st'' pos ""
    _ -> lexMultilineInterpBody (lsPos st') (lsInput st') 1 st' pos ""

-- | Resume multiline string after interpolation
lexMultilineAfterInterp :: Pos -> String -> LexState -> Pos -> String -> Either LexError LexState
lexMultilineAfterInterp pos input st _ delim = go pos input []
  where
    delimLen = length delim

    go p s acc
      | delimLen > 0 && take delimLen s == delim =
          let txt = T.pack (reverse acc)
          in Right st { lsInput = drop delimLen s, lsPos = advanceCol p delimLen
                      , lsTokens = Located pos (TokStringEnd txt) "" : lsTokens st }
    go p ('#' : '{' : rest) acc =
      let txt = T.pack (reverse acc)
      in lexMultilineInterpBody (advanceCol p 2) rest 1
           st { lsTokens = Located p TokInterpOpen "#{" : Located pos (TokStringMid txt) "" : lsTokens st }
           pos delim
    go p ('\n' : rest) acc = go (nextLine p) rest ('\n' : acc)
    go p (c : rest) acc = go (advanceCol p 1) rest (c : acc)
    go p [] _ = Left (LexError p "unterminated multiline string literal")

-- | Lex a hexadecimal number after 0x prefix
lexHexNumber :: Pos -> String -> LexState -> Either LexError LexState
lexHexNumber pos input st =
  let (digits, rest) = span isHexDigit input
  in if null digits
     then Left (LexError pos "expected hexadecimal digits after 0x")
     else let val = foldl (\n d -> n * 16 + fromIntegral (hexVal d)) 0 digits
              len = 2 + length digits
              txt = T.pack ("0x" ++ digits)
          in Right st { lsInput = rest, lsPos = advanceCol pos len
                      , lsTokens = Located pos (TokInteger val) txt : lsTokens st }
  where
    hexVal c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = 0

-- | Lex an octal number after 0o prefix
lexOctalNumber :: Pos -> String -> LexState -> Either LexError LexState
lexOctalNumber pos input st =
  let (digits, rest) = span isOctDigit input
  in if null digits
     then Left (LexError pos "expected octal digits after 0o")
     else let val = foldl (\n d -> n * 8 + fromIntegral (fromEnum d - fromEnum '0')) 0 digits
              len = 2 + length digits
              txt = T.pack ("0o" ++ digits)
          in Right st { lsInput = rest, lsPos = advanceCol pos len
                      , lsTokens = Located pos (TokInteger val) txt : lsTokens st }

-- | Lex a binary number after 0b prefix
lexBinaryNumber :: Pos -> String -> LexState -> Either LexError LexState
lexBinaryNumber pos input st =
  let (digits, rest) = span (\c -> c == '0' || c == '1') input
  in if null digits
     then Left (LexError pos "expected binary digits after 0b")
     else let val = foldl (\n d -> n * 2 + fromIntegral (fromEnum d - fromEnum '0')) 0 digits
              len = 2 + length digits
              txt = T.pack ("0b" ++ digits)
          in Right st { lsInput = rest, lsPos = advanceCol pos len
                      , lsTokens = Located pos (TokInteger val) txt : lsTokens st }

-- | Lex a decimal integer or float, with optional scientific notation
lexDecNumber :: Pos -> String -> LexState -> Either LexError LexState
lexDecNumber pos input st =
  let (intPart, rest1) = span isDigit input
  in case rest1 of
    -- Float: digits.digits
    '.' : c : rest2 | isDigit c ->
      let (fracPart, rest3) = span isDigit (c : rest2)
          (expPart, rest4) = lexExponent rest3
          numStr = intPart ++ "." ++ fracPart ++ expPart
          val = read numStr :: Double
          len = length numStr
      in Right st { lsInput = rest4, lsPos = advanceCol pos len
                  , lsTokens = Located pos (TokFloat val) (T.pack numStr) : lsTokens st }
    -- Integer with exponent (e.g., 5e10) -- treated as float
    'e' : _ ->
      let (expPart, rest3) = lexExponent rest1
      in if null expPart
         then mkInt intPart rest1
         else let numStr = intPart ++ expPart
                  val = read numStr :: Double
                  len = length numStr
              in Right st { lsInput = rest3, lsPos = advanceCol pos len
                          , lsTokens = Located pos (TokFloat val) (T.pack numStr) : lsTokens st }
    'E' : _ ->
      let (expPart, rest3) = lexExponent rest1
      in if null expPart
         then mkInt intPart rest1
         else let numStr = intPart ++ expPart
                  val = read numStr :: Double
                  len = length numStr
              in Right st { lsInput = rest3, lsPos = advanceCol pos len
                          , lsTokens = Located pos (TokFloat val) (T.pack numStr) : lsTokens st }
    -- Plain integer
    _ -> mkInt intPart rest1
  where
    mkInt digits rest =
      let val = read digits :: Integer
          len = length digits
      in Right st { lsInput = rest, lsPos = advanceCol pos len
                  , lsTokens = Located pos (TokInteger val) (T.pack digits) : lsTokens st }

-- | Try to lex a scientific notation exponent (e.g., e10, e-3, E+5)
-- Returns the exponent string and remaining input. Empty string if no exponent.
lexExponent :: String -> (String, String)
lexExponent ('e' : '+' : rest) =
  let (digits, rest') = span isDigit rest
  in if null digits then ("", 'e' : '+' : rest)
     else ("e+" ++ digits, rest')
lexExponent ('e' : '-' : rest) =
  let (digits, rest') = span isDigit rest
  in if null digits then ("", 'e' : '-' : rest)
     else ("e-" ++ digits, rest')
lexExponent ('e' : rest) =
  let (digits, rest') = span isDigit rest
  in if null digits then ("", 'e' : rest)
     else ("e" ++ digits, rest')
lexExponent ('E' : '+' : rest) =
  let (digits, rest') = span isDigit rest
  in if null digits then ("", 'E' : '+' : rest)
     else ("E+" ++ digits, rest')
lexExponent ('E' : '-' : rest) =
  let (digits, rest') = span isDigit rest
  in if null digits then ("", 'E' : '-' : rest)
     else ("E-" ++ digits, rest')
lexExponent ('E' : rest) =
  let (digits, rest') = span isDigit rest
  in if null digits then ("", 'E' : rest)
     else ("E" ++ digits, rest')
lexExponent rest = ("", rest)

-- | Process a multiline (triple-quoted) string: strip leading/trailing blank
-- lines and remove common indentation.
processMultilineString :: Text -> Text
processMultilineString txt =
  let stripped = removeTrailingSpace (removeLeadingSpace txt)
  in reindent stripped
  where
    removeLeadingSpace :: Text -> Text
    removeLeadingSpace s = case T.lines s of
      [] -> ""
      (first : rest)
        | T.null (T.strip first) -> T.unlines rest
        | otherwise -> T.unlines (first : rest)

    removeTrailingSpace :: Text -> Text
    removeTrailingSpace s = case T.lines s of
      [] -> ""
      ls | T.null (T.strip (last ls)) -> T.unlines (init ls)
         | otherwise -> T.unlines ls

    reindent :: Text -> Text
    reindent s = case T.lines s of
      [] -> ""
      ls ->
        let nonEmpty = filter (not . T.null . T.strip) ls
            spaces = map (T.length . T.takeWhile (== ' ')) nonEmpty
            minSpaces = if null spaces then 0 else minimum spaces
        in T.unlines (map (T.drop minSpaces) ls)

-- Position helpers
advanceCol :: Pos -> Int -> Pos
advanceCol (Pos l c f) n = Pos l (c + n) f

nextLine :: Pos -> Pos
nextLine (Pos l _ f) = Pos (l + 1) 1 f

--------------------------------------------------------------------
-- Layout token insertion
--------------------------------------------------------------------

-- | Insert virtual braces and semicolons based on indentation.
--
-- Layout contexts:
--   1. Top-level: the body of a module (or implicit main)
--   2. After 'where' keyword (function where, class/instance bodies)
--   3. After 'do' keyword
--
-- Algorithm (GHC-inspired):
--   - When we see a layout keyword (where, do), the next token's column
--     defines a new layout context. Emit virtual {.
--   - For the top-level, the first declaration's column starts the context.
--   - When the next token aligns with the context column, emit ;.
--   - When the next token is left of the context column, emit } and pop.
--   - Explicit { after a layout keyword enters a non-indentation context.

data LayoutContext
  = IndentCtx !Int   -- ^ virtual { at this column
  | ExplicitCtx      -- ^ real {, no indentation tracking
  deriving (Show, Eq)

-- | Is the token a layout keyword (introduces an indented block)?
isLayoutKeyword :: Token -> Bool
isLayoutKeyword TokWhere = True
isLayoutKeyword TokDo    = True
isLayoutKeyword _        = False

insertLayout :: [Located] -> [Located]
insertLayout [] = []
insertLayout toks = beginTopLevel toks
  where
    -- Handle the top-level layout. The top-level body (after module header
    -- or at the start for implicit main) gets a layout context.
    beginTopLevel :: [Located] -> [Located]
    beginTopLevel ts = case ts of
      -- File starts with 'module': skip the header, then start layout
      (Located p TokModule _ : rest) ->
        Located p TokModule "" : skipModuleHeader rest
      -- File starts with something else: implicit main, start layout immediately
      (_ : _) -> startLayoutCtx [] ts
      [] -> []

    -- Skip past 'module Name (exports)' to find where the body starts.
    -- We need to find the opening '(' of the export list, then track
    -- paren depth to find the matching ')'.
    skipModuleHeader :: [Located] -> [Located]
    skipModuleHeader [] = []
    skipModuleHeader (t@(Located _ TokLParen _) : rest) =
      -- Found the opening ( of exports, now track depth starting at 1
      t : skipExportList 1 rest
    skipModuleHeader (t : rest) = t : skipModuleHeader rest

    -- Track paren depth inside the export list
    skipExportList :: Int -> [Located] -> [Located]
    skipExportList _ [] = []
    skipExportList depth (t@(Located _ TokLParen _) : rest) =
      t : skipExportList (depth + 1) rest
    skipExportList depth (t@(Located _ TokRParen _) : rest)
      | depth <= 1 = t : startLayoutCtx [] rest  -- closing ) of export list
      | otherwise  = t : skipExportList (depth - 1) rest
    skipExportList depth (t : rest) = t : skipExportList depth rest

    -- Skip module header during processing (for multi-module files).
    -- Same as skipModuleHeader/skipExportList but preserves existing contexts.
    skipModuleHeaderInBody :: [LayoutContext] -> [Located] -> [Located]
    skipModuleHeaderInBody ctxs [] = closingBraces ctxs []
    skipModuleHeaderInBody ctxs (t@(Located _ TokLParen _) : rest) =
      t : skipExportListInBody ctxs 1 rest
    skipModuleHeaderInBody ctxs (t : rest) = t : skipModuleHeaderInBody ctxs rest

    skipExportListInBody :: [LayoutContext] -> Int -> [Located] -> [Located]
    skipExportListInBody ctxs _ [] = closingBraces ctxs []
    skipExportListInBody ctxs depth (t@(Located _ TokLParen _) : rest) =
      t : skipExportListInBody ctxs (depth + 1) rest
    skipExportListInBody ctxs depth (t@(Located _ TokRParen _) : rest)
      | depth <= 1 = t : startLayoutCtx ctxs rest
      | otherwise  = t : skipExportListInBody ctxs (depth - 1) rest
    skipExportListInBody ctxs depth (t : rest) = t : skipExportListInBody ctxs depth rest

    -- Start a new layout context at the column of the next token
    startLayoutCtx :: [LayoutContext] -> [Located] -> [Located]
    startLayoutCtx ctxs [] = closingBraces ctxs []
    startLayoutCtx ctxs [eof@(Located _ TokEOF _)] =
      -- empty body
      Located (locPos eof) TokVLBrace "" :
      Located (locPos eof) TokVRBrace "" :
      closingBraces ctxs [eof]
    startLayoutCtx ctxs (t : rest)
      | otherwise =
          let col = posCol (locPos t)
              newCtxs = IndentCtx col : ctxs
              vopen = Located (locPos t) TokVLBrace ""
          -- The first token of a layout block must not get a VSEMI before it
          -- (indentCheck would emit one since col == n). So we handle it
          -- specially, bypassing indentation checking.
          in vopen : emitFirstToken newCtxs t rest

    -- Emit the first token of a layout block. Like processToken but
    -- without indentation checking (the first token defines the column,
    -- it should not receive a VSEMI).
    emitFirstToken :: [LayoutContext] -> Located -> [Located] -> [Located]
    emitFirstToken ctxs tok rest
      | locToken tok == TokEOF = closingBraces ctxs [tok]
      | otherwise = emitToken ctxs tok rest

    -- Main processing loop
    process :: [LayoutContext] -> [Located] -> [Located]
    process ctxs [] = closingBraces ctxs []
    process ctxs (t : rest) = processToken ctxs t rest

    -- Process a single token with the current context stack
    processToken :: [LayoutContext] -> Located -> [Located] -> [Located]
    processToken ctxs tok rest
      -- EOF: close all contexts
      | locToken tok == TokEOF = closingBraces ctxs [tok]
      -- Regular token: check indentation first (may emit VSEMI/VRBRACE)
      | otherwise = indentCheck ctxs tok rest

    -- Check indentation of a regular token against the context stack
    indentCheck :: [LayoutContext] -> Located -> [Located] -> [Located]
    indentCheck [] tok rest = emitToken [] tok rest
    indentCheck (ExplicitCtx : ctxs) tok rest = emitToken (ExplicitCtx : ctxs) tok rest
    indentCheck ctxs@(IndentCtx n : cs) tok rest
      | col == n && isBlockCloser (locToken tok) =
          -- 'module' at layout column closes the current block
          Located (locPos tok) TokVRBrace "" : indentCheck cs tok rest
      | col == n =
          -- aligned: emit semicolon before this token
          Located (locPos tok) TokVSemi "" : emitToken ctxs tok rest
      | col > n =
          -- indented further: continuation of previous item
          emitToken ctxs tok rest
      | otherwise =
          -- dedented: close this context, then re-check
          Located (locPos tok) TokVRBrace "" : indentCheck cs tok rest
      where
        col = posCol (locPos tok)

    -- Tokens that close a layout block even when at the same indentation level
    isBlockCloser :: Token -> Bool
    isBlockCloser TokModule = True
    isBlockCloser _ = False

    -- Emit a token with special handling for keywords
    emitToken :: [LayoutContext] -> Located -> [Located] -> [Located]
    emitToken ctxs tok rest
      -- Module keyword: emit it and skip the header, then start layout
      | locToken tok == TokModule =
          tok : skipModuleHeaderInBody ctxs rest
      -- Layout keywords: emit the keyword, then start a new layout context
      | isLayoutKeyword (locToken tok) =
          tok : startLayoutCtx ctxs rest
      -- Explicit open brace
      | locToken tok == TokLBrace =
          tok : process (ExplicitCtx : ctxs) rest
      -- Explicit close brace
      | locToken tok == TokRBrace =
          closeToExplicit ctxs tok rest
      -- Regular token
      | otherwise = tok : process ctxs rest

    -- Close layout contexts until we find an explicit brace context
    closeToExplicit :: [LayoutContext] -> Located -> [Located] -> [Located]
    closeToExplicit (ExplicitCtx : ctxs) tok rest =
      tok : process ctxs rest
    closeToExplicit (IndentCtx _ : ctxs) tok rest =
      Located (locPos tok) TokVRBrace "" : closeToExplicit ctxs tok rest
    closeToExplicit [] tok rest =
      -- unbalanced }, let the parser report the error
      tok : process [] rest

    -- Emit closing braces for all remaining contexts
    closingBraces :: [LayoutContext] -> [Located] -> [Located]
    closingBraces [] rest = rest
    closingBraces (_ : cs) rest =
      let p = case rest of
            (Located pp _ _ : _) -> pp
            [] -> Pos 1 1 ""
      in Located p TokVRBrace "" : closingBraces cs rest
