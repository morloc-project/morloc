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
  , desc
  ) where

import Morloc.Data.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Text.Prettyprint.Doc.Render.Terminal.Internal as Style
import Morloc.Namespace
import Morloc.TypeChecker.Namespace
import qualified Morloc.Data.Text as MT
import qualified Data.Set as Set

instance Pretty MType where
  pretty (MConcType _ n []) = pretty n
  pretty (MConcType _ n ts) = parens $ hsep (pretty n : (map pretty ts))
  pretty (MAbstType _ n []) = pretty n
  pretty (MAbstType _ n ts) = parens $ hsep (pretty n : (map pretty ts))
  pretty (MFuncType _ ts o) =
    parens $ (hcat . punctuate ", ") (map pretty ts) <> " -> " <> pretty o

instance Pretty MVar where
  pretty (MV t) = pretty t

instance Pretty EVar where
  pretty (EV t) = pretty t

instance Pretty TVar where
  pretty (TV t) = pretty t

typeStyle =
  Style.SetAnsiStyle
    { Style.ansiForeground = Just (Vivid, Green)
    , Style.ansiBackground = Nothing
    , Style.ansiBold = Nothing
    , Style.ansiItalics = Nothing
    , Style.ansiUnderlining = Just Underlined
    }

prettyMVar :: MVar -> Doc AnsiStyle
prettyMVar (MV x) = pretty x

prettyModule :: Module -> Doc AnsiStyle
prettyModule m =
  prettyMVar (moduleName m) <+>
  braces (line <> (indent 4 (prettyBlock m)) <> line)

prettyBlock :: Module -> Doc AnsiStyle
prettyBlock m =
  vsep (map prettyImport (moduleImports m)) <>
  vsep ["export" <+> pretty e <> line | (EV e) <- moduleExports m] <>
  vsep (map prettyExpr (moduleBody m))

prettyImport :: Import -> Doc AnsiStyle
prettyImport imp =
  "import" <+>
  pretty (importModuleName imp) <+>
  maybe
    "*"
    (\xs -> encloseSep "(" ")" ", " (map prettyImportOne xs))
    (importInclude imp)
  where
    prettyImportOne (EV e, EV alias)
      | e /= alias = pretty e
      | otherwise = pretty e <+> "as" <+> pretty alias

prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr UniE = "()"
prettyExpr (VarE (EV s)) = pretty s
prettyExpr (LamE (EV n) e) = "\\" <> pretty n <+> "->" <+> prettyExpr e
prettyExpr (AnnE e t) = parens (prettyExpr e <+> "::" <+> prettyGreenType t)
prettyExpr (AppE e1@(LamE _ _) e2) = parens (prettyExpr e1) <+> prettyExpr e2
prettyExpr (AppE e1 e2) = prettyExpr e1 <+> prettyExpr e2
prettyExpr (NumE x) = pretty (show x)
prettyExpr (StrE x) = dquotes (pretty x)
prettyExpr (LogE x) = pretty x
prettyExpr (Declaration (EV v) e) = pretty v <+> "=" <+> prettyExpr e
prettyExpr (ListE xs) = list (map prettyExpr xs)
prettyExpr (TupleE xs) = tupled (map prettyExpr xs)
prettyExpr (SrcE lang (Just f) rs) =
  "source" <+>
  viaShow lang <+>
  "from" <+>
  pretty f <+>
  tupled
    (map
       (\(EV n, EV a) ->
          pretty n <>
          if n == a
            then ""
            else (" as" <> pretty a))
       rs)
prettyExpr (SrcE lang Nothing rs) =
  "source" <+>
  viaShow lang <+>
  tupled
    (map
       (\(EV n, EV a) ->
          pretty n <>
          if n == a
            then ""
            else (" as" <> pretty a))
       rs)
prettyExpr (RecE entries) =
  encloseSep
    "{"
    "}"
    ", "
    (map (\(EV v, e) -> pretty v <+> "=" <+> prettyExpr e) entries)
prettyExpr (Signature (EV v) e) =
  pretty v <+> elang' <> "::" <+> eprop' <> etype' <> econs'
  where
    elang' :: Doc AnsiStyle
    elang' = maybe "" (\lang -> viaShow lang <> " ") (elang e)
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
forallVars (Forall (TV s) t) = pretty s : forallVars t
forallVars _ = []

forallBlock :: Type -> Doc a
forallBlock (Forall _ t) = forallBlock t
forallBlock t = prettyType t

prettyGreenType :: Type -> Doc AnsiStyle
prettyGreenType t = annotate typeStyle (prettyType t)

prettyType :: Type -> Doc ann
prettyType UniT = "1"
prettyType (VarT (TV s)) = pretty s
prettyType (FunT t1@(FunT _ _) t2) =
  parens (prettyType t1) <+> "->" <+> prettyType t2
prettyType (FunT t1 t2) = prettyType t1 <+> "->" <+> prettyType t2
prettyType t@(Forall _ _) =
  "forall" <+> hsep (forallVars t) <+> "." <+> forallBlock t
prettyType (ExistT (TV e)) = "<" <> pretty e <> ">"
prettyType (ArrT (TV v) ts) = pretty v <+> hsep (map prettyType ts)
prettyType (RecT entries) =
  encloseSep "{" "}" ", "
    (map (\(TV v, e) -> pretty v <+> "=" <+> prettyType e) entries)

class Describable a where
  desc :: a -> String

instance Describable Expr where
  desc (UniE) = "UniE"
  desc (VarE (EV v)) = "VarE:" ++ MT.unpack v
  desc (ListE _) = "ListE"
  desc (TupleE _) = "Tuple"
  desc (SrcE _ _ _) = "SrcE:"
  desc (LamE (EV v) _) = "LamE:" ++ MT.unpack v
  desc (AppE e1 e2) = "AppE (" ++ desc e1 ++ ") (" ++ desc e2 ++ ")"
  desc (AnnE e _) = "AnnE (" ++ desc e ++ ")"
  desc (NumE x) = "NumE:" ++ show x
  desc (LogE x) = "LogE:" ++ show x
  desc (StrE x) = "StrE:" ++ show x
  desc (RecE _) = "RecE:"
  desc (Declaration (EV e) _) = "Declaration:" ++ MT.unpack e
  desc (Signature (EV e) _) = "Signature:" ++ MT.unpack e

instance Describable Type where
  desc (UniT) = "UniT"
  desc (VarT (TV v)) = "VarT:" ++ MT.unpack v
  desc (ExistT (TV v)) = "ExistT:" ++ MT.unpack v
  desc (Forall (TV v) _) = "Forall:" ++ MT.unpack v
  desc (FunT t1 t2) = "FunT (" ++ desc t1 ++ ") (" ++ desc t2 ++ ")"
  desc (ArrT (TV v) xs) = "ArrT:" ++ MT.unpack v ++ " " ++ (concat . map desc) xs
  desc (RecT _) = "RecT:"
