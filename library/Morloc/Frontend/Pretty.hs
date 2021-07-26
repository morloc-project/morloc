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
  , prettyGammaIndex
  , prettyParserError
  , prettyTypeSet
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

prettyTypeSet :: TypeSet -> Doc AnsiStyle
prettyTypeSet (TypeSet Nothing ts)
  = encloseSep "(" ")" ";" (map (prettyGreenUnresolvedType . etype) ts)
prettyTypeSet (TypeSet (Just t) ts)
  = encloseSep "(" ")" ";" (map (prettyGreenUnresolvedType . etype) (t:ts))


prettyGammaIndex :: GammaIndex -> Doc AnsiStyle
prettyGammaIndex (VarG tv) = "VarG:" <+> pretty tv
prettyGammaIndex (AnnG e ts) = "AnnG:" <+> prettyExpr e <+> prettyTypeSet ts
prettyGammaIndex (ExistG tv ts ds)
  = "ExistG:"
  <+> pretty tv
  <+> list (map (parens . prettyGreenUnresolvedType) ts)
  <+> list (map (parens . prettyGreenUnresolvedType) ds)
prettyGammaIndex (SolvedG tv t) = "SolvedG:" <+> pretty tv <+> "=" <+> prettyGreenUnresolvedType t
prettyGammaIndex (MarkG tv) = "MarkG:" <+> pretty tv
prettyGammaIndex (SrcG (Source ev1 lang _ _)) = "SrcG:" <+> pretty ev1 <+> viaShow lang
prettyGammaIndex (UnsolvedConstraint t1 t2) = "UnsolvedConstraint:" <+> prettyGreenUnresolvedType t1 <+> prettyGreenUnresolvedType t2

prettyExpr :: ExprI -> Doc AnsiStyle
prettyExpr (ExprI _ UniE) = "()"
prettyExpr (ExprI _ (ModE v es)) = align . vsep $ ("module" <+> pretty v) : map prettyExpr es
prettyExpr (ExprI _ (TypE v vs t)) = "type" <+> (pretty v) <+> sep [pretty v' | TV _ v' <- vs] <+> "=" <+> prettyGreenUnresolvedType t
prettyExpr (ExprI _ (ImpE (Import m Nothing _ _))) = "import" <+> pretty m 
prettyExpr (ExprI _ (ImpE (Import m (Just xs) _ _)))
  = "import" <+> pretty m
  <+> tupled [dquotes (pretty n) <+> "as" <+> pretty alias | (n,alias) <- xs] 
prettyExpr (ExprI _ (ExpE v)) = "export" <+> pretty v
prettyExpr (ExprI _ (VarE s)) = pretty s
prettyExpr (ExprI _ (AccE e k)) = parens (prettyExpr e) <> "@" <> pretty k 
prettyExpr (ExprI _ (LamE n e)) = "\\" <> pretty n <+> "->" <+> prettyExpr e
prettyExpr (ExprI _ (AnnE e ts)) = parens
  $   prettyExpr e
  <+> "::"
  <+> encloseSep "(" ")" "; " (map prettyGreenUnresolvedType ts)
prettyExpr (ExprI _ (AppE e1@(ExprI _ (LamE _ _)) e2)) = parens (prettyExpr e1) <+> prettyExpr e2
prettyExpr (ExprI _ (AppE e1 e2)) = parens (prettyExpr e1) <+> parens (prettyExpr e2)
prettyExpr (ExprI _ (NumE x)) = pretty (show x)
prettyExpr (ExprI _ (StrE x)) = dquotes (pretty x)
prettyExpr (ExprI _ (LogE x)) = pretty x
prettyExpr (ExprI _ (Declaration v e es)) = pretty v <+> "=" <+> prettyExpr e <+> "where" <+> (align . vsep . map prettyExpr) es
prettyExpr (ExprI _ (ListE xs)) = list (map prettyExpr xs)
prettyExpr (ExprI _ (TupleE xs)) = tupled (map prettyExpr xs)
prettyExpr (ExprI _ (SrcE [])) = ""
prettyExpr (ExprI _ (SrcE srcs@(Source _ lang (Just f) _ : _))) =
  "source" <+>
  viaShow lang <+>
  "from" <+>
  pretty f <+>
  tupled
    (map
       (\(n, EV a) ->
          pretty n <>
          if unName n == a
            then ""
            else (" as" <> pretty a))
       rs)
  where
    rs = [(n, a) | (Source n _ _ a) <- srcs]
prettyExpr (ExprI _ (SrcE srcs@(Source _ lang Nothing _ : _))) =
  "source" <+>
  viaShow lang <+>
  tupled
    (map
       (\(n, EV a) ->
          pretty n <>
          if unName n == a
            then ""
            else (" as" <> pretty a))
       rs)
  where
    rs = [(n,a) | (Source n _ _ a) <- srcs]
prettyExpr (ExprI _ (RecE entries)) =
  encloseSep
    "{"
    "}"
    ", "
    (map (\(v, e) -> pretty v <+> "=" <+> prettyExpr e) entries)
prettyExpr (ExprI _ (Signature v e)) =
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
    etype' = prettyGreenUnresolvedType (etype e)
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
