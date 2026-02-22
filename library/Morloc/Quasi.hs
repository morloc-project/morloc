{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Morloc.Quasi
Description : String-interpolating quasiquoter for Doc values
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Provides the @[idoc|...|]@ quasiquoter for building 'Doc' values with
embedded Haskell expressions via @#\{expr\}@ syntax. Used in translators
to generate code with interpolated variable names and types.
-}
module Morloc.Quasi
  ( idoc
  ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Morloc.Data.Doc as G

import qualified Language.Haskell.Meta.Parse as MP

import Text.Parsec

type Parser = Parsec String ()

data I
  = S String
  | V String

pIs :: Parser [I]
pIs = many1 (try pV <|> try pS <|> try pE) <* eof

pV :: Parser I
pV = V <$> between (string "#{") (char '}') (many1 (noneOf "}"))

pS :: Parser I
pS = S <$> many1 (noneOf "#")

-- | match a literal '#' sign
pE :: Parser I
pE = fmap (S . return) $ char '#' <* notFollowedBy (char '}')

-- | __i__nterpolated __doc__ument
idoc :: QuasiQuoter
idoc =
  QuasiQuoter
    { quoteExp = compile
    , quotePat = error "Can't handle patterns"
    , quoteType = error "Can't handle types"
    , quoteDec = error "Can't handle declarations"
    }
  where
    compile :: String -> Q Exp
    compile txt =
      case parse pIs "" txt of
        Left err -> error $ show err
        Right xs -> return $ AppE (VarE 'G.hcat) (ListE (map qI xs))
          where
            qI :: I -> Exp
            qI (S x) = LitE (StringL x)
            qI (V x) =
              case MP.parseExp x of
                (Right hask) -> hask -- a Haskell expression
                (Left err) -> error err
