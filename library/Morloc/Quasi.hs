{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Morloc.Quasi (
    idoc
  , sparql
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Morloc.Data.Doc as Gen
import qualified Morloc.Data.RDF as MR
import qualified Morloc.Data.Text as MT

import qualified Language.Haskell.Meta.Parse as MP

import Text.Parsec

import Morloc.Sparql as Conn
import Morloc.Types (SparqlEndPoint)

type Parser = Parsec String ()

data I = S String | V String

pIs :: Parser [I]
pIs = many1 (try pV <|> pS <|> pE) <* eof

pV :: Parser I
pV = fmap V $ between (string "${") (char '}') (many1 (noneOf "}")) 

pS :: Parser I
pS = fmap S $ many1 (noneOf "$")

-- | match a literal '$' sign
pE :: Parser I
pE = fmap (S . return) $ char '$' <* notFollowedBy (char '}')


-- | __i__nterpolated __doc__ument
idoc :: QuasiQuoter
idoc = QuasiQuoter {
    quoteExp  = compile
  , quotePat  = error "Can't handle patterns"
  , quoteType = error "Can't handle types"
  , quoteDec  = error "Can't handle declarations"
  }
  where
    compile :: String -> Q Exp
    compile txt = case parse pIs "" txt of
      Left err -> error $ show err
      Right xs -> return $ AppE (VarE 'Gen.hcat) (ListE (map qI xs)) where
        qI :: I -> Exp
        qI (S x) = AppE (VarE 'Gen.string) (LitE (StringL x))
        qI (V x) = case MP.parseExp x of
          (Right hask) -> hask -- a Haskell expression
          (Left err) -> error err


maybeValue :: Conn.BindingValue -> Maybe MT.Text
maybeValue (Conn.Bound (MR.LNode (MR.PlainL  x  ))) = Just x
maybeValue (Conn.Bound (MR.LNode (MR.PlainLL x _))) = Just x
maybeValue (Conn.Bound (MR.LNode (MR.TypedL  x _))) = Just x
maybeValue (Conn.Bound (MR.UNode x))             = Just x
maybeValue _ = Nothing

values :: Maybe [[Conn.BindingValue]] -> [[Maybe MT.Text]]
values Nothing = error "SPARQL command failed"
values (Just xss) = (fmap . fmap) maybeValue xss

simpleSelect :: Gen.Doc -> SparqlEndPoint -> IO ([[Maybe MT.Text]])
-- selectQuery' :: SparqlEndPoint -> String -> IO (Maybe [[BindingValue]])
simpleSelect d e = fmap values $ (flip Conn.selectQuery') (Gen.render' d) e

sparql :: QuasiQuoter
sparql = QuasiQuoter {
    quoteExp  = compile
  , quotePat  = error "Can't handle patterns"
  , quoteType = error "Can't handle types"
  , quoteDec  = error "Can't handle declarations"
  }
  where
    compile :: String -> Q Exp
    compile txt = case parse pIs "" txt of
      Left err -> error $ show err
      Right xs -> return $
        LamE [VarP (mkName "e")] (
          AppE (
            AppE (VarE 'simpleSelect) (
                AppE
                  (VarE 'Gen.hcat)
                  (ListE (map qI xs))
              )
            ) (VarE (mkName "e"))
          )
        where
          qI :: I -> Exp
          qI (S x) = AppE (VarE 'Gen.string) (LitE (StringL x))
          qI (V x) = VarE (mkName x)
