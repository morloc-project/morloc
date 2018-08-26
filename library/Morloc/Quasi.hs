{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Morloc.Quasi (
    s
  , sparql
  , render
  , render'
) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Text.PrettyPrint.Leijen.Text as Gen
import qualified Data.RDF as DR
import qualified Morloc.Database.HSparql.Connection as Conn
import qualified Data.Text as DT
import qualified Language.Haskell.Meta.Parse as MP

import Text.Parsec

import Morloc.Types (SparqlEndPoint)

render :: Gen.Doc -> DT.Text
render = Gen.displayTStrict . Gen.renderPretty 0.5 70

render' :: Gen.Doc -> String
render' = DT.unpack . render

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


s :: QuasiQuoter
s = QuasiQuoter {
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


maybeValue :: Conn.BindingValue -> Maybe DT.Text
maybeValue (Conn.Bound (DR.LNode (DR.PlainL  x  ))) = Just x
maybeValue (Conn.Bound (DR.LNode (DR.PlainLL x _))) = Just x
maybeValue (Conn.Bound (DR.LNode (DR.TypedL  x _))) = Just x
maybeValue (Conn.Bound (DR.UNode x))             = Just x
maybeValue _ = Nothing

values :: Maybe [[Conn.BindingValue]] -> [[Maybe DT.Text]]
values Nothing = []
values (Just xss) = (fmap . fmap) maybeValue xss

simpleSelect :: Gen.Doc -> SparqlEndPoint -> IO ([[Maybe DT.Text]])
-- selectQuery' :: SparqlEndPoint -> String -> IO (Maybe [[BindingValue]])
simpleSelect d e = fmap values $ (flip Conn.selectQuery') (render' d) e

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
