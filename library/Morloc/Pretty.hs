{-|
Module      : Morloc.Pretty
Description : Pretty print instances
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Pretty
  ( prettyExpr
  , prettyModule
  , prettyType
  , prettyGreenType
  , prettyGammaIndex
  , prettyScream
  ) where

import Data.Text.Prettyprint.Doc.Render.Terminal
import Morloc.Data.Doc
import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import qualified Data.Set as Set
import qualified Data.Text.Prettyprint.Doc.Render.Terminal.Internal as Style

instance Pretty MVar where
  pretty = pretty . unMVar

instance Pretty EVar where
  pretty = pretty . unEVar

instance Pretty Path where
  pretty = pretty . unPath

instance Pretty Code where
  pretty = pretty . unCode

instance Pretty Name where
  pretty = pretty . unName

instance Pretty TVar where
  pretty (TV Nothing t) = pretty t
  pretty (TV (Just lang) t) = pretty t <> "@" <> pretty (show lang)

typeStyle =
  Style.SetAnsiStyle
    { Style.ansiForeground = Just (Vivid, Green)
    , Style.ansiBackground = Nothing
    , Style.ansiBold = Nothing
    , Style.ansiItalics = Nothing
    , Style.ansiUnderlining = Just Underlined
    }

screamStyle =
  Style.SetAnsiStyle
    { Style.ansiForeground = Just (Vivid, Red)
    , Style.ansiBackground = Nothing
    , Style.ansiBold = Nothing
    , Style.ansiItalics = Nothing
    , Style.ansiUnderlining = Just Underlined
    }

prettyModule :: Module -> Doc AnsiStyle
prettyModule m = block 2 (pretty (moduleName m)) (prettyBlock m)

prettyBlock :: Module -> Doc AnsiStyle
prettyBlock m = vsep
  [ vsep $ map prettyImport (moduleImports m)
  , vsep $ map (\v -> "export" <+> pretty v) (Set.toList (moduleExports m))
  , vsep $ map prettyExpr (moduleBody m)
  ]

prettyImport :: Import -> Doc AnsiStyle
prettyImport imp =
  "import" <+>
  pretty (importModuleName imp) <+>
  maybe
    "*"
    (\xs -> encloseSep "(" ")" ", " (map prettyImportOne xs))
    (importInclude imp)
  where
    prettyImportOne (e, alias)
      | e /= alias = pretty e
      | otherwise = pretty e <+> "as" <+> pretty alias

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr UniE = "()"
prettyExpr (VarE s) = pretty s
prettyExpr (LamE n e) = "\\" <> pretty n <+> "->" <+> prettyExpr e
prettyExpr (AnnE e ts) = parens
  $   prettyExpr e
  <+> "::"
  <+> encloseSep "(" ")" "; " (map prettyGreenType ts)
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
       (\(n, a) ->
          pretty n <>
          if unName n == unEVar a
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
       (\(n, a) ->
          pretty n <>
          if unName n == unEVar a
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
    etype' = prettyGreenType (etype e)
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

forallVars :: Type -> [Doc a]
forallVars (Forall v t) = pretty v : forallVars t
forallVars _ = []

forallBlock :: Type -> Doc a
forallBlock (Forall _ t) = forallBlock t
forallBlock t = prettyType t

prettyGreenType :: Type -> Doc AnsiStyle
prettyGreenType t = annotate typeStyle (prettyType t)

prettyScream :: MT.Text -> Doc AnsiStyle
prettyScream x = annotate screamStyle (pretty x)

class PrettyType a where
  prettyType :: a -> Doc ann

instance PrettyType Type where
  prettyType (VarT (TV _ "Unit")) = "()"
  prettyType (VarT v) = pretty v
  prettyType (FunT t1@(FunT _ _) t2) =
    parens (prettyType t1) <+> "->" <+> prettyType t2
  prettyType (FunT t1 t2) = prettyType t1 <+> "->" <+> prettyType t2
  prettyType t@(Forall _ _) =
    "forall" <+> hsep (forallVars t) <+> "." <+> forallBlock t
  prettyType (ExistT v ts) = angles (pretty v) <+> (hsep . map prettyType) ts
  prettyType (ArrT v ts) = pretty v <+> hsep (map prettyType ts)
  prettyType (RecT entries) =
    encloseSep "{" "}" ", "
      (map (\(v, e) -> pretty v <+> "=" <+> prettyType e) entries)

instance PrettyType GeneralType where
  prettyType = prettyType . unGeneralType

instance PrettyType ConcreteType where
  prettyType = prettyType . unConcreteType

prettyTypeSet :: TypeSet -> Doc AnsiStyle
prettyTypeSet (TypeSet Nothing ts)
  = encloseSep "(" ")" ";" (map (prettyGreenType . etype) ts)
prettyTypeSet (TypeSet (Just t) ts)
  = encloseSep "(" ")" ";" (map (prettyGreenType . etype) (t:ts))

prettyGammaIndex :: GammaIndex -> Doc AnsiStyle
prettyGammaIndex (VarG tv) = "VarG:" <+> pretty tv
prettyGammaIndex (AnnG e ts) = "AnnG:" <+> prettyExpr e <+> prettyTypeSet ts
prettyGammaIndex (ExistG tv []) = "ExistG:" <+> pretty tv
prettyGammaIndex (ExistG tv ts) = "ExistG:" <+> pretty tv <+> hsep (map (parens . prettyGreenType) ts)
prettyGammaIndex (SolvedG tv t) = "SolvedG:" <+> pretty tv <+> "=" <+> prettyGreenType t
prettyGammaIndex (MarkG tv) = "MarkG:" <+> pretty tv
prettyGammaIndex (MarkEG ev) = "MarkG:" <+> pretty ev
prettyGammaIndex (SrcG (Source ev1 lang _ _)) = "SrcG:" <+> pretty ev1 <+> viaShow lang
prettyGammaIndex (ConcreteG ev lang t) = "ConcreteG:" <+> pretty ev <+> viaShow lang <+> prettyGreenType t
prettyGammaIndex (UnsolvedConstraint t1 t2) = "UnsolvedConstraint:" <+> prettyGreenType t1 <+> prettyGreenType t2
