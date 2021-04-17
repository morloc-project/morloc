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

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr UniE = "()"
prettyExpr (VarE s) = pretty s
prettyExpr (AccE e k) = parens (prettyExpr e) <> "@" <> pretty k 
prettyExpr (LamE n e) = "\\" <> pretty n <+> "->" <+> prettyExpr e
prettyExpr (AnnE e ts) = parens
  $   prettyExpr e
  <+> "::"
  <+> encloseSep "(" ")" "; " (map prettyGreenUnresolvedType ts)
prettyExpr (AppE e1@(LamE _ _) e2) = parens (prettyExpr e1) <+> prettyExpr e2
prettyExpr (AppE e1 e2) = parens (prettyExpr e1) <+> parens (prettyExpr e2)
prettyExpr (NumE x) = pretty (show x)
prettyExpr (StrE x) = dquotes (pretty x)
prettyExpr (LogE x) = pretty x
prettyExpr (Declaration v e) = pretty v <+> "=" <+> prettyExpr e
prettyExpr (ListE xs) = list (map prettyExpr xs)
prettyExpr (TupleE xs) = tupled (map prettyExpr xs)
prettyExpr (SrcE []) = ""
prettyExpr (SrcE srcs@(Source _ lang (Just f) _ : _)) =
  "source" <+>
  viaShow lang <+>
  "from" <+>
  pretty f <+>
  tupled
    (map
       (\(n, EV _ a) ->
          pretty n <>
          if unName n == a
            then ""
            else (" as" <> pretty a))
       rs)
  where
    rs = [(n, a) | (Source n _ _ a) <- srcs]
prettyExpr (SrcE srcs@(Source _ lang Nothing _ : _)) =
  "source" <+>
  viaShow lang <+>
  tupled
    (map
       (\(n, EV _ a) ->
          pretty n <>
          if unName n == a
            then ""
            else (" as" <> pretty a))
       rs)
  where
    rs = [(n,a) | (Source n _ _ a) <- srcs]
prettyExpr (RecE entries) =
  encloseSep
    "{"
    "}"
    ", "
    (map (\(v, e) -> pretty v <+> "=" <+> prettyExpr e) entries)
prettyExpr (Signature v e) =
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
