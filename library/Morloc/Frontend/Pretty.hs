{-|
Module      : Morloc.Frontend.Pretty
Description : Pretty is as pretty does
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Pretty
  ( module Morloc.Pretty
  , prettyExpr
  , prettyExprI
  , prettyParserError
  ) where

import Morloc.Frontend.Namespace
import qualified Data.Set as Set
import Morloc.Data.Doc hiding (putDoc)
import Morloc.Pretty
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Text.Megaparsec as Mega
import qualified Morloc.Data.Text as MT 
import Data.Void (Void)

prettyParserError :: Mega.ParseErrorBundle MT.Text Void -> Doc AnsiStyle
prettyParserError = undefined

prettyExprI :: ExprI -> Doc AnsiStyle
prettyExprI (ExprI _ e) = prettyExpr e

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr UniE = "()"
prettyExpr (ModE v es) = align . vsep $ ("module" <+> pretty v) : map prettyExprI es
prettyExpr (TypE v vs t) = "type" <+> (pretty v) <+> sep [pretty v' | TV _ v' <- vs] <+> "=" <+> prettyGreenTypeU t
prettyExpr (ImpE (Import m Nothing _ _)) = "import" <+> pretty m 
prettyExpr (ImpE (Import m (Just xs) _ _))
  = "import" <+> pretty m
  <+> tupled [dquotes (pretty n) <+> "as" <+> pretty alias | (n,alias) <- xs] 
prettyExpr (ExpE v) = "export" <+> pretty v
prettyExpr (VarE s) = pretty s
prettyExpr (AccE e k) = parens (prettyExprI e) <> "@" <> pretty k 
prettyExpr (LamE v e) = "\\" <+> pretty v <+> "->" <+> prettyExprI e
prettyExpr (AnnE e ts) = parens
  $   prettyExprI e
  <+> "::"
  <+> encloseSep "(" ")" "; " (map prettyGreenTypeU ts)
prettyExpr (LstE es) = encloseSep "[" "]" "," (map prettyExprI es)
prettyExpr (TupE es) = encloseSep "[" "]" "," (map prettyExprI es)
prettyExpr (AppE f es) = vsep (map prettyExprI (f:es))
prettyExpr (NamE rs) = block 4 "<RECORD>" (vsep [pretty k <+> "::" <+> prettyExprI x | (k, x) <- rs])
prettyExpr (NumE x) = pretty (show x)
prettyExpr (StrE x) = dquotes (pretty x)
prettyExpr (LogE x) = pretty x
prettyExpr (AssE v e es) = pretty v <+> "=" <+> prettyExprI e <+> "where" <+> (align . vsep . map prettyExprI) es
prettyExpr (SrcE (Source name lang file alias label))
  = "source"
  <+> viaShow lang
  <> maybe "" (\f -> "from" <+> pretty f) file 
  <+> "("
  <> dquotes (pretty name) <+> "as" <+>  pretty alias <> maybe "" (\s -> ":" <> pretty s) label
  <> ")"
prettyExpr (SigE v _ e) =
  pretty v <+> elang' <> "::" <+> eprop' <> etype' <> econs'
  where
    elang' :: Doc AnsiStyle
    elang' = maybe "" (\lang -> viaShow lang <> " ") (langOf . etype $ e)
    eprop' :: Doc AnsiStyle
    eprop' =
      case Set.toList (eprop e) of
        [] -> ""
        xs -> tupled (map prettyProperty xs) <+> "=> "
    etype' :: Doc AnsiStyle
    etype' = prettyGreenTypeU (etype e)
    econs' :: Doc AnsiStyle
    econs' =
      case Set.toList (econs e) of
        [] -> ""
        xs -> " where" <+> tupled (map (\(Con x) -> pretty x) xs)

prettyProperty :: Property -> Doc ann
prettyProperty Pack = "pack"
prettyProperty Unpack = "unpack"
prettyProperty Cast = "cast"
prettyProperty (GeneralProperty ts) = hsep (map pretty ts)
