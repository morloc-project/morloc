{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

{-|
Module      : Morloc.Pretty
Description : Pretty print instances
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Pretty
  ( prettyPackMap
  , prettySAnno
  , prettySExpr
  ) where

import Morloc.Data.Doc
import Morloc.Namespace
import qualified Data.Map as Map
import qualified Data.Set as Set

instance Pretty Symbol where
  pretty (TypeSymbol x) = viaShow x
  pretty (TermSymbol x) = viaShow x

instance Pretty AliasedSymbol where
  pretty (AliasedType x alias) = pretty (AliasedTerm x alias)
  pretty (AliasedTerm x alias)
    | x == alias = pretty x
    | otherwise = pretty x <+> "as" <+> pretty alias

instance Pretty MVar where
  pretty = pretty . unMVar

instance Pretty EVar where
  pretty (EV v) = pretty v

instance Pretty Code where
  pretty = pretty . unCode

instance Pretty Name where
  pretty = pretty . unName

instance Pretty TVar where
  pretty (TV Nothing t) = pretty t
  pretty (TV (Just lang) t) = pretty t <> "@" <> pretty (show lang)

instance Pretty Lang where
  pretty = viaShow

instance Pretty NamType where
  pretty = viaShow

instance Pretty Source where
  pretty s
    = "source" <+> pretty (srcLang s)
    <> maybe "" (\ path -> " from" <+> dquotes (pretty path)) (srcPath s)
    <+> dquotes (pretty (srcName s))
    <+> "as" <+> pretty (srcAlias s) <> maybe "" (\t -> ":" <> pretty t) (srcLabel s)

forallVars :: TypeU -> [Doc ann]
forallVars (ForallU (TV _ v) t) = pretty v : forallVars t
forallVars _ = []

forallBlock :: TypeU -> Doc ann
forallBlock (ForallU _ t) = forallBlock t
forallBlock t = pretty t


instance Pretty Type where
  pretty (UnkT (TV _ v)) = "*" <> pretty v
  pretty (VarT (TV _ "Unit")) = "()"
  pretty (VarT v) = pretty v
  pretty (FunT [] t) = "<MISSING> -> " <> pretty t
  pretty (FunT ts t) = encloseSep "(" ")" " -> " (map pretty (ts <> [t]))
  pretty (AppT t ts) = hsep (map pretty (t:ts))
  pretty (NamT o n ps rs)
    = block 4 (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
              (vsep [pretty k <+> "::" <+> pretty x | (k, x) <- rs])

instance Pretty GType where
  pretty = pretty . unGType

instance Pretty CType where
  pretty = pretty . unCType

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
  pretty (ExistU v ts ds)
    = angles $ pretty v
    <> list (map pretty ts)
    <> list (map pretty ds)
  pretty t@(ForallU _ _) =
    parens $ "forall" <+> hsep (forallVars t) <+> "." <+> forallBlock t
  pretty (VarU (TV _ "Unit")) = "()"
  pretty (VarU v) = pretty v
  pretty (FunU [] t) = parens $ "<MISSING> -> " <> pretty t
  pretty (FunU ts t) = encloseSep "(" ")" " -> " (map pretty (ts <> [t]))
  pretty (AppU t ts) = parens . hsep $ map pretty (t:ts)
  pretty (NamU o n ps rs)
      = block 4 (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
                (vsep [pretty k <+> "::" <+> pretty x | (k, x) <- rs])

instance Pretty UnresolvedPacker where
  pretty (UnresolvedPacker v t fs rs) = vsep
    [ pretty v
    , pretty t 
    , "forward:" <+> tupled (map pretty fs)
    , "reverse:" <+> tupled (map pretty rs)
    ]



prettyPackMap :: PackMap -> Doc ann
prettyPackMap m =  "----- pacmaps ----\n"
                <> vsep (map f (Map.toList m))
                <> "\n------------------" where
  f :: ((TVar, Int), [UnresolvedPacker]) -> Doc ann
  f ((v, i), ps) =
    block 4
      ("packmap" <+> pretty v <> parens (pretty i))
      (vsep $ map pretty ps)


-- For example @prettySAnnoMany id Nothing@ for the most simple printer
prettySAnno
  :: Foldable f
  => (c -> Doc ann)
  -> (g -> Doc ann)
  -> SAnno g f c
  -> Doc ann
prettySAnno writeCon writeGen (SAnno e g)
  = foldr (prettyCon writeCon writeGen) (writeGen g) e
  where
  prettyCon
    :: Foldable f
    => (c -> Doc ann)
    -> (g -> Doc ann)
    -> (SExpr g f c, c)
    -> Doc ann
    -> Doc ann
  prettyCon fc fg (s, c) p = vsep [p, fc c, prettySExpr fc fg s]

prettySExpr
  :: Foldable f
  => (c -> Doc ann)
  -> (g -> Doc ann)
  -> SExpr g f c
  -> Doc ann
prettySExpr fc fg x0 = case x0 of
  UniS -> "UniS"
  (VarS v) -> "VarS<" <> pretty v <> ">"
  (AccS x k ) -> "AccS" <+> pretty k <+> parens (prettySAnno fc fg x)
  (AppS x xs) -> "AppS" <+> parens (prettySAnno fc fg x) <+> tupled (map (prettySAnno fc fg) xs)
  (LamS vs x) -> "LamS" <+> tupled (map pretty vs) <+> braces (prettySAnno fc fg x)
  (LstS xs) -> "LstS" <+> tupled (map (prettySAnno fc fg) xs)
  (TupS xs) -> "TupS" <+> tupled (map (prettySAnno fc fg) xs)
  (NamS rs) -> "NamS" <+> tupled (map (\(k,x) -> pretty k <+> "=" <+> prettySAnno fc fg x) rs)
  (RealS x) -> "RealS<" <> viaShow x <> ">"
  (IntS x) -> "IntS<" <> viaShow x <> ">"
  (LogS x) -> "LogS<" <> viaShow x <> ">"
  (StrS x) -> "StrS<" <> viaShow x <> ">"
  (CallS src) -> "CallS<" <> pretty (srcName src) <> "@" <> pretty (srcLang src) <> ">"

instance Pretty a => Pretty (Indexed a) where
  pretty (Idx i x) = parens (viaShow i <> ":" <+> pretty x) 

instance Pretty GammaIndex where
  pretty (VarG tv) = "VarG:" <+> pretty tv
  pretty (ExistG tv ts ds)
    = "ExistG:"
    <+> pretty tv
    <+> list (map (parens . pretty) ts)
    <+> list (map (parens . pretty) ds)
  pretty (SolvedG tv t) = "SolvedG:" <+> pretty tv <+> "=" <+> pretty t
  pretty (MarkG tv) = "MarkG:" <+> pretty tv
  pretty (SrcG (Source ev1 lang _ _ _)) = "SrcG:" <+> pretty ev1 <+> viaShow lang
  pretty (SerialConstraint t1 t2) = "UnsolvedConstraint:" <+> pretty t1 <+> pretty t2
  pretty (AnnG v t) = pretty v <+> "::" <+> pretty t
