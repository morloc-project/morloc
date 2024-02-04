{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

{-|
Module      : Morloc.Pretty
Description : Pretty print instances
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Pretty () where

import Morloc.Data.Doc
import Morloc.Namespace
import qualified Data.Set as Set
import qualified Data.Map as Map

instance Pretty Symbol where
  pretty (TypeSymbol x) = viaShow x
  pretty (TermSymbol x) = viaShow x

instance Pretty AliasedSymbol where
  pretty (AliasedType x alias)
    | x == alias = pretty x
    | otherwise = pretty x <+> "as" <+> pretty alias
  pretty (AliasedTerm x alias)
    | x == alias = pretty x
    | otherwise = pretty x <+> "as" <+> pretty alias

instance Pretty MVar where
  pretty = pretty . unMVar

instance Pretty EVar where
  pretty (EV v) = pretty v

instance Pretty TVar where
  pretty (TV v) = pretty v

instance Pretty Typeclass where
  pretty = pretty . unTypeclass

instance (Pretty k1, Pretty k2, Pretty v) => Pretty (GMap k1 k2 v) where
  pretty (GMap m1 m2) = "GMap" <+> (align . vsep $ [pretty (Map.toList m1), pretty (Map.toList m2)])

instance Pretty SignatureSet where
  pretty (Monomorphic t) = pretty t
  pretty (Polymorphic cls v t ts)
    = "class" <+> pretty cls
    <+> (align . vsep $ (pretty v <+> "::" <+> parens (pretty t)) : map pretty ts)

instance Pretty TermTypes where
  pretty (TermTypes (Just t) cs es) = "TermTypes" <+> (align . vsep $ (parens (pretty t) : map pretty cs <> map pretty es))
  pretty (TermTypes Nothing cs es) = "TermTypes" <+> "?" <> (align . vsep $ (map pretty cs <> map pretty es))

instance Pretty Key where
  pretty (Key v) = pretty v

instance Pretty Label where
  pretty (Label v) = pretty v

instance Pretty Code where
  pretty = pretty . unCode

instance Pretty SrcName where
  pretty = pretty . unSrcName

instance Pretty Lang where
  pretty = viaShow

instance Pretty NamType where
  pretty = viaShow

instance (Pretty a, Pretty b) => Pretty (Or a b) where
  pretty (L x) = parens ("L" <+> pretty x)
  pretty (R x) = parens ("R" <+> pretty x)
  pretty (LR x y) = parens ("LR" <+> pretty x <> "," <+> pretty y)

instance Pretty Source where
  pretty s
    = "source" <+> pretty (srcLang s)
    <> maybe "" (\ path -> " from" <+> dquotes (pretty path)) (srcPath s)
    <+> dquotes (pretty (srcName s))
    <+> "as" <+> pretty (srcAlias s) <> maybe "" (\t -> ":" <> pretty t) (srcLabel s)

instance Pretty Type where
  pretty (UnkT v) = pretty v
  pretty (VarT v) = pretty v
  pretty (FunT [] t) = "() -> " <> pretty t
  pretty (FunT ts t) = encloseSep "(" ")" " -> " (map pretty (ts <> [t]))
  pretty (AppT t ts) = hsep (map pretty (t:ts))
  pretty (NamT o n ps rs)
    = block 4 (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
              (vsep [pretty k <+> "::" <+> pretty x | (k, x) <- rs])

instance Pretty EType where
  pretty (EType t (Set.toList -> ps) (Set.toList -> cs)) = case (ps, cs) of
    ([], []) -> pretty t
    _ -> parens (psStr ps <> pretty t <> csStr cs)
    where
      psStr [] = ""
      psStr [x] = pretty x <+> "=> "
      psStr xs = tupled (map pretty xs) <+> "=> "

      csStr [] = ""
      csStr xs = " |" <+> hsep (punctuate semi (map pretty xs))

instance Pretty Property where
  pretty Pack = "pack"
  pretty Unpack = "unpack"
  pretty Cast = "cast"
  pretty (GeneralProperty ts) = hsep (map pretty ts)

instance Pretty Constraint where
  pretty (Con x) = pretty x

instance Pretty TypeU where
  pretty (FunU [] t) = "() -> " <> prettyTypeU t
  pretty (FunU ts t) = hsep $ punctuate " ->" (map prettyTypeU (ts <> [t]))
  pretty (ForallU _ t) = pretty t
  pretty t = prettyTypeU t

instance Pretty None where
  pretty None = "()"

instance Pretty a => Pretty (One a) where
  pretty (One x) = pretty x

instance Pretty a => Pretty (Many a) where
  pretty (Many xs) = list $ map pretty xs

prettyTypeU (ExistU v [] []) = angles $ pretty v
prettyTypeU (ExistU v ts rs)
  = angles $ pretty v
  <+> list (map prettyTypeU ts)
  <+> list (map ((\(x,y) -> tupled [x, y]) . bimap pretty prettyTypeU) rs)
prettyTypeU (ForallU _ t) = prettyTypeU t
prettyTypeU (VarU v) = pretty v
prettyTypeU (FunU [] t) = parens $ "() -> " <> prettyTypeU t
prettyTypeU (FunU ts t) = encloseSep "(" ")" " -> " (map prettyTypeU (ts <> [t]))
prettyTypeU (AppU t ts) = hsep $ map parenTypeU (t:ts) where
    parenTypeU t'@(AppU _ _) = parens $ prettyTypeU t'
    parenTypeU t' = prettyTypeU t'
prettyTypeU (NamU o n ps rs)
    = parens
    $ block 4 (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
              (vsep [pretty k <+> "::" <+> prettyTypeU x | (k, x) <- rs])

instance Pretty (AnnoS g f c) where
  pretty (AnnoS e g c) = "AnnoS"

instance Pretty (ExprS g f c) where
  pretty _ = "ExprS"

instance (Pretty k, Pretty a) => Pretty (IndexedGeneral k a) where
  pretty (Idx i x) = parens (pretty i <> ":" <+> pretty x)

instance Pretty GammaIndex where
  pretty (VarG tv) = "VarG:" <+> pretty tv
  pretty (ExistG tv [] []) = angles (pretty tv)
  pretty (ExistG tv ts rs)
    = "ExistG:"
    <+> pretty tv
    <+> list (map (parens . pretty) ts)
    <+> list (map ((\(x,y) -> tupled [x, y]) . bimap pretty prettyTypeU) rs)
  pretty (SolvedG tv t) = "SolvedG:" <+> pretty tv <+> "=" <+> pretty t
  pretty (MarkG tv) = "MarkG:" <+> pretty tv
  pretty (SrcG (Source ev1 lang _ _ _)) = "SrcG:" <+> pretty ev1 <+> viaShow lang
  pretty (AnnG v t) = pretty v <+> "::" <+> pretty t

instance Pretty ExprI where
  pretty (ExprI i e) = parens (pretty e) <> ":" <> pretty i

instance Pretty Expr where
  pretty UniE = "()"
  pretty (ModE v es) = align . vsep $ ("module" <+> pretty v) : map pretty es
  pretty (ClsE cls vs sigs) = "class" <+> pretty cls <+> hsep (map pretty vs) <> (align . vsep . map pretty) sigs
  pretty (IstE cls ts es) = "instance" <+> pretty cls <+> hsep (map (parens . pretty) ts) <> (align . vsep . map pretty) es
  pretty (TypE lang v vs t)
    = "type" <+> pretty lang <> "@" <> pretty v
    <+> sep (map pretty vs) <+> "=" <+> pretty t
  pretty (ImpE (Import m Nothing _ _)) = "import" <+> pretty m
  pretty (ImpE (Import m (Just xs) _ _)) = "import" <+> pretty m <+> tupled (map pretty xs)
  pretty (ExpE v) = "export" <+> pretty v
  pretty (VarE s) = pretty s
  pretty (AccE k e) = parens (pretty e) <> "@" <> pretty k
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
  pretty (SigE (Signature v _ e)) =
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

instance Pretty Signature where
  pretty (Signature v _ e) = pretty v <+> "::" <+> pretty (etype e)
