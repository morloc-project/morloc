{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Cost.Formula
Description : The formulas of cost directives (COST-7)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A formula is arithmetic over numbers with units and names:

@
    expr    ::= term (("+" | "-") term)*
    term    ::= power (("*" | "/") power)*
    power   ::= unary ("^" power)?
    unary   ::= "-" unary | atom
    atom    ::= number unit? | "log" atom | "log2" atom | name | "(" expr ")"
    unit    ::= B KB MB GB TB KiB MiB GiB TiB ns us ms s
@

Every formula has a dimension, a power of time and a power of bytes; a
name is a plain number. A value is in seconds and bytes.
-}
module Morloc.Cost.Formula
  ( Formula (..)
  , Dim (..)
  , plain
  , time
  , bytes
  , parseFormula
  , dimOf
  , evalFormula
  , formulaNames
  , renderDim
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (Parsec, (<|>), char, eof, many, many1, satisfy, spaces, try)
import qualified Text.Parsec as P
import Text.Parsec.Text ()

type P = Parsec Text ()

data Formula
  = Lit Double Dim
  | Name Text
  | Neg Formula
  | Add Formula Formula
  | Sub Formula Formula
  | Mul Formula Formula
  | Div Formula Formula
  | Pow Formula Formula
  | Log Formula
  | Log2 Formula
  deriving (Show, Eq, Ord)

-- | Powers of time and of bytes.
data Dim = Dim {dimTime :: Int, dimBytes :: Int}
  deriving (Show, Eq, Ord)

plain, time, bytes :: Dim
plain = Dim 0 0
time = Dim 1 0
bytes = Dim 0 1

renderDim :: Dim -> Text
renderDim (Dim 0 0) = "a plain number"
renderDim (Dim 1 0) = "a time"
renderDim (Dim 0 1) = "bytes"
renderDim (Dim t b) = "time^" <> T.pack (show t) <> " * bytes^" <> T.pack (show b)

parseFormula :: Text -> Either Text Formula
parseFormula src = case P.parse (spaces *> expr <* eof) "" src of
  Left err -> Left ("cannot read the formula '" <> src <> "': " <> T.pack (show err))
  Right f -> Right f
  where
    expr :: P Formula
    expr = chain term [("+", Add), ("-", Sub)]

    term :: P Formula
    term = chain power [("*", Mul), ("/", Div)]

    power :: P Formula
    power = do
      base <- unary
      (lexeme (char '^') *> (Pow base <$> power)) <|> return base

    unary :: P Formula
    unary = (lexeme (char '-') *> (Neg <$> unary)) <|> atom

    atom :: P Formula
    atom =
      lexeme (char '(') *> expr <* lexeme (char ')')
        <|> number
        <|> word

    word :: P Formula
    word = do
      w <- lexeme ((:) <$> satisfy (\c -> isAlpha c || c == '_') <*> many (satisfy (\c -> isAlphaNum c || c == '_')))
      case w of
        "log" -> Log <$> atom
        "log2" -> Log2 <$> atom
        _ -> return (Name (T.pack w))

    number :: P Formula
    number = do
      digits <- lexeme $ do
        whole <- many1 (satisfy isDigit)
        frac <- P.option "" ((:) <$> char '.' <*> many1 (satisfy isDigit))
        ex <- P.option "" (try exponent)
        return (whole <> frac <> ex)
      let n = read (leadingZero digits) :: Double
      (unit >>= \(scale, d) -> return (Lit (n * scale) d)) <|> return (Lit n plain)

    exponent :: P String
    exponent = do
      e <- char 'e' <|> char 'E'
      sign <- P.option "" ((: []) <$> (char '-' <|> char '+'))
      ds <- many1 (satisfy isDigit)
      return (e : filter (/= '+') sign <> ds)

    leadingZero s = case s of
      ('.' : _) -> '0' : s
      _ -> s

    unit :: P (Double, Dim)
    unit = try $ lexeme $ do
      u <- many1 (satisfy isAlpha)
      case lookup u units of
        Just found -> return found
        Nothing -> P.unexpected ("unit " <> u)

    units :: [(String, (Double, Dim))]
    units =
      [ ("B", (1, bytes))
      , ("KB", (1e3, bytes))
      , ("MB", (1e6, bytes))
      , ("GB", (1e9, bytes))
      , ("TB", (1e12, bytes))
      , ("KiB", (1024, bytes))
      , ("MiB", (1024 ^ (2 :: Int), bytes))
      , ("GiB", (1024 ^ (3 :: Int), bytes))
      , ("TiB", (1024 ^ (4 :: Int), bytes))
      , ("ns", (1e-9, time))
      , ("us", (1e-6, time))
      , ("ms", (1e-3, time))
      , ("s", (1, time))
      ]

    chain :: P Formula -> [(String, Formula -> Formula -> Formula)] -> P Formula
    chain p ops = p >>= rest
      where
        rest acc =
          ( do
              f <- P.choice [lexeme (P.string o) *> return k | (o, k) <- ops]
              b <- p
              rest (f acc b)
          )
            <|> return acc

    lexeme :: P a -> P a
    lexeme p = p <* spaces

-- | The dimension of a formula, or why it has none: the operands of @+@ and
-- @-@ share a dimension, the argument of a logarithm and an exponent are
-- plain, and a base with a dimension takes only an integer literal exponent.
dimOf :: Formula -> Either Text Dim
dimOf f = case f of
  Lit _ d -> Right d
  Name _ -> Right plain
  Neg x -> dimOf x
  Add a b -> same "+" a b
  Sub a b -> same "-" a b
  Mul a b -> combine (+) <$> dimOf a <*> dimOf b
  Div a b -> combine (-) <$> dimOf a <*> dimOf b
  Pow a b -> do
    da <- dimOf a
    db <- dimOf b
    if db /= plain
      then Left "an exponent must be a plain number"
      else
        if da == plain
          then Right plain
          else case b of
            Lit n _ | n == fromIntegral (round n :: Int) -> Right (scaleDim (round n) da)
            _ -> Left ("only an integer literal can raise " <> renderDim da)
  Log x -> logArg "log" x
  Log2 x -> logArg "log2" x
  where
    combine op (Dim t1 b1) (Dim t2 b2) = Dim (op t1 t2) (op b1 b2)
    scaleDim k (Dim t b) = Dim (k * t) (k * b)
    same op a b = do
      da <- dimOf a
      db <- dimOf b
      if da == db
        then Right da
        else Left ("cannot " <> verb op <> " " <> renderDim da <> " and " <> renderDim db)
    verb "+" = "add"
    verb _ = "subtract"
    logArg name x = do
      dx <- dimOf x
      if dx == plain then Right plain else Left ("the argument of " <> name <> " must be a plain number")

-- | The value of a formula with each name taking the value given, or
-- Nothing when it is undefined, infinite or negative (COST-7).
evalFormula :: (Text -> Double) -> Formula -> Maybe Double
evalFormula env f
  | isNaN v || isInfinite v || v < 0 = Nothing
  | otherwise = Just v
  where
    v = go f
    go e = case e of
      Lit n _ -> n
      Name x -> env x
      Neg x -> negate (go x)
      Add a b -> go a + go b
      Sub a b -> go a - go b
      Mul a b -> go a * go b
      Div a b -> go a / go b
      Pow a b -> go a ** go b
      Log x -> log (go x)
      Log2 x -> logBase 2 (go x)

formulaNames :: Formula -> [Text]
formulaNames = nub . go
  where
    go e = case e of
      Lit _ _ -> []
      Name x -> [x]
      Neg x -> go x
      Add a b -> go a <> go b
      Sub a b -> go a <> go b
      Mul a b -> go a <> go b
      Div a b -> go a <> go b
      Pow a b -> go a <> go b
      Log x -> go x
      Log2 x -> go x
