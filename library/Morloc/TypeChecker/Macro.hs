{-|
Module      : Morloc.TypeChecker.Macro
Description : Expand parameters in concrete types
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

-}

module Morloc.TypeChecker.Macro
(
    expandMacro
  , buildCType
) where

import Morloc.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import qualified Control.Monad.State as CMS
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser a = CMS.StateT ParserState (Parsec Void MT.Text) a

data ParserState = ParserState {
    stateParameters :: [MT.Text]
}

buildCType
  :: (MDoc -> [MDoc] -> MDoc) -- ^ make function type
  -> ([(MDoc, MDoc)] -> MDoc) -- ^ make record type
  -> CType
  -> MDoc
buildCType mkfun mkrec (CType t) = f t where
  f :: Type -> MDoc
  f (VarT (TV _ x)) = pretty x
  f t@(FunT t1 t2) = mkfun (f t1) (map f (typeArgs t))
  f (ArrT (TV _ v) ts) = pretty $ expandMacro v (map (render . f) ts)
  f (NamT (TV _ v) entries) = mkrec [(pretty k, f t) | (k, t) <- entries]
  f (Forall _ _) = error "Concrete polymorphism is not supported"
  f (ExistT _ _ _) = error "Concrete existentials are not supported"

  typeArgs :: Type -> [Type]
  typeArgs (FunT t1 t2) = t1 : typeArgs t2
  typeArgs t = [t]

expandMacro :: MT.Text -> [MT.Text] -> MT.Text
expandMacro t [] = t
expandMacro t ps =
  case runParser
         (CMS.runStateT (pBase <* eof) (ParserState ps))
         "typemacro"
         t of
    Left err -> error (show err)
    Right (es, _) -> es

-- expandNamedMacro :: MT.Text -> [

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
