{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Frontend.Pretty
Description : Pretty is as pretty does
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Frontend.Pretty (module Morloc.Pretty) where

import Morloc.Frontend.Namespace
import qualified Data.Set as Set
import Morloc.Data.Doc hiding (putDoc)
import Morloc.Pretty

instance Pretty ExprI where
  pretty (ExprI i e) = parens (pretty e) <> ":" <> pretty i

instance Pretty Expr where
  pretty UniE = "()"
  pretty (ModE v es) = align . vsep $ ("module" <+> pretty v) : map pretty es
  pretty (TypE lang v vs t)
    = "type" <+> pretty lang <> "@" <> pretty v
    <+> sep (map pretty vs) <+> "=" <+> pretty t
  pretty (ImpE (Import m Nothing _ _)) = "import" <+> pretty m 
  pretty (ImpE (Import m (Just xs) _ _)) = "import" <+> pretty m <+> tupled (map pretty xs)
  pretty (ExpE v) = "export" <+> pretty v
  pretty (VarE s) = pretty s
  pretty (AccE e k) = parens (pretty e) <> "@" <> pretty k 
  pretty (LamE v e) = "\\" <+> pretty v <+> "->" <+> pretty e
  pretty (AnnE e ts) = parens
    $   pretty e
    <+> "::"
    <+> encloseSep "(" ")" "; " (map pretty ts)
  pretty (LstE es) = encloseSep "[" "]" "," (map pretty es)
  pretty (TupE es) = encloseSep "[" "]" "," (map pretty es)
  pretty (AppE f es) = vsep (map pretty (f:es))
  pretty (NamE rs) = block 4 "<RECORD>" (vsep [pretty k <+> "::" <+> pretty x | (k, x) <- rs])
  pretty (RealE x) = pretty (show x)
  pretty (IntE x) = pretty (show x)
  pretty (StrE x) = dquotes (pretty x)
  pretty (LogE x) = pretty x
  pretty (AssE v e es) = pretty v <+> "=" <+> pretty e <+> "where" <+> (align . vsep . map pretty) es
  pretty (SrcE (Source name lang file' alias label))
    = "source"
    <+> viaShow lang
    <> maybe "" (\f -> "from" <+> pretty f) file' 
    <+> "("
    <> dquotes (pretty name) <+> "as" <+>  pretty alias <> maybe "" (\s -> ":" <> pretty s) label
    <> ")"
  pretty (SigE v _ e) =
    pretty v <+> "::" <+> eprop' <> etype' <> econs'
    where
      eprop' :: Doc ann
      eprop' =
        case Set.toList (eprop e) of
          [] -> ""
          xs -> tupled (map pretty xs) <+> "=> "
      etype' :: Doc ann 
      etype' = pretty (etype e)
      econs' :: Doc ann
      econs' =
        case Set.toList (econs e) of
          [] -> ""
          xs -> " where" <+> tupled (map (\(Con x) -> pretty x) xs)
