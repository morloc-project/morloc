{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.Token
Description : Token types shared between the Alex lexer and Happy parser
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.Frontend.Token
  ( Token (..)
  , Located (..)
  , Pos (..)
  , startPos
  , alexPos
  , showToken
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Source position: line, column (1-based)
data Pos = Pos
  { posLine :: {-# UNPACK #-} !Int
  , posCol :: {-# UNPACK #-} !Int
  , posFile :: !String
  }
  deriving (Show, Eq, Ord)

-- | Initial position (line 1, column 1) for a given filename.
startPos :: String -> Pos
startPos f = Pos 1 1 f

-- | Build a 'Pos' from the line and column reported by the lexer.
alexPos :: String -> Int -> Int -> Pos
alexPos f l c = Pos l c f

-- | A token annotated with its source position and the matched text
data Located = Located
  { locPos :: !Pos
  , locToken :: !Token
  , locText :: !Text
  }
  deriving (Show, Eq)

data Token
  = -- Layout tokens (inserted by layout processor)

    -- | virtual {
    TokVLBrace
  | -- | virtual }
    TokVRBrace
  | -- | virtual ;
    TokVSemi
  | -- Delimiters

    -- | (
    TokLParen
  | -- | )
    TokRParen
  | -- | [
    TokLBracket
  | -- | ]
    TokRBracket
  | -- | {
    TokLBrace
  | -- | }
    TokRBrace
  | -- | <
    TokLAngle
  | -- | >
    TokRAngle
  | -- Punctuation

    -- | ,
    TokComma
  | -- | ; (explicit)
    TokSemicolon
  | -- | backslash (lambda)
    TokBackslash
  | -- | _
    TokUnderscore
  | -- | !
    TokBang
  | -- | . (operator position, e.g., f . g)
    TokDot
  | -- | . (getter prefix, e.g., .name)
    TokGetterDot
  | -- Reserved operators

    -- | =
    TokEquals
  | -- | :
    TokColon
  | -- | ::
    TokDColon
  | -- | ->
    TokArrow
  | -- | =>
    TokFatArrow
  | -- | <-
    TokBind
  | -- | * (only in export context)
    TokStar
  | -- Keywords
    TokModule
  | TokImport
  | TokExport
  | TokSource
  | TokFrom
  | TokWhere
  | TokAs
  | TokTrue
  | TokFalse
  | TokType
  | TokRecord
  | TokObject
  | TokTable
  | TokClass
  | TokInstance
  | TokInfixl
  | TokInfixr
  | TokInfix
  | TokLet
  | TokIn
  | TokDo
  | -- Identifiers and literals

    -- | lowercase identifier
    TokLowerName !Text
  | -- | uppercase identifier
    TokUpperName !Text
  | -- | operator symbol (e.g., +, *, .)
    TokOperator !Text
  | -- | - (needed separately for module names and unary negation)
    TokMinus
  | TokInteger !Integer
  | TokFloat !Double
  | -- | plain string (no interpolation)
    TokString !Text
  | -- | start of interpolated string: text before first #{}
    TokStringStart !Text
  | -- | text between #{} in interpolated string
    TokStringMid !Text
  | -- | text after last #{} to closing quote
    TokStringEnd !Text
  | -- | #{ opening interpolation
    TokInterpOpen
  | -- | } closing interpolation
    TokInterpClose
  | -- Docstrings

    -- | --' followed by text
    TokDocLine !Text
  | -- Group annotations

    -- | --* followed by text
    TokGroupLine !Text
  | -- Special
    TokEOF
  deriving (Show, Eq, Ord)

-- | Human-readable token description for error messages
showToken :: Token -> String
showToken TokVLBrace = "start of indented block"
showToken TokVRBrace = "end of indented block"
showToken TokVSemi = "new declaration"
showToken TokLParen = "'('"
showToken TokRParen = "')'"
showToken TokLBracket = "'['"
showToken TokRBracket = "']'"
showToken TokLBrace = "'{'"
showToken TokRBrace = "'}'"
showToken TokLAngle = "'<'"
showToken TokRAngle = "'>'"
showToken TokComma = "','"
showToken TokSemicolon = "';'"
showToken TokBackslash = "'\\'"
showToken TokUnderscore = "'_'"
showToken TokBang = "'!'"
showToken TokDot = "'.'"
showToken TokGetterDot = "'.'"
showToken TokEquals = "'='"
showToken TokColon = "':'"
showToken TokDColon = "'::'"
showToken TokArrow = "'->'"
showToken TokFatArrow = "'=>'"
showToken TokBind = "'<-'"
showToken TokStar = "'*'"
showToken TokModule = "'module'"
showToken TokImport = "'import'"
showToken TokExport = "'export'"
showToken TokSource = "'source'"
showToken TokFrom = "'from'"
showToken TokWhere = "'where'"
showToken TokAs = "'as'"
showToken TokTrue = "'True'"
showToken TokFalse = "'False'"
showToken TokType = "'type'"
showToken TokRecord = "'record'"
showToken TokObject = "'object'"
showToken TokTable = "'table'"
showToken TokClass = "'class'"
showToken TokInstance = "'instance'"
showToken TokInfixl = "'infixl'"
showToken TokInfixr = "'infixr'"
showToken TokInfix = "'infix'"
showToken TokLet = "'let'"
showToken TokIn = "'in'"
showToken TokDo = "'do'"
showToken (TokLowerName n) = "identifier '" ++ T.unpack n ++ "'"
showToken (TokUpperName n) = "type name '" ++ T.unpack n ++ "'"
showToken (TokOperator n) = "operator '" ++ T.unpack n ++ "'"
showToken TokMinus = "'-'"
showToken (TokInteger _) = "integer literal"
showToken (TokFloat _) = "float literal"
showToken (TokString _) = "string literal"
showToken (TokStringStart _) = "string literal"
showToken (TokStringMid _) = "string continuation"
showToken (TokStringEnd _) = "string end"
showToken TokInterpOpen = "'#{'"
showToken TokInterpClose = "'}' (interpolation)"
showToken (TokDocLine _) = "docstring"
showToken (TokGroupLine _) = "group annotation"
showToken TokEOF = "end of input"
