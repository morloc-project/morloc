{-|
Module      : Morloc.Pretty
Description : Pretty print instances
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Pretty
  ( prettyType
  , prettyGreenType
  , prettyGreenTypeU
  , prettyScream
  , prettyLinePrefixes
  , prettyUnresolvedPacker
  , prettyPackMap
  , prettySAnno
  , prettySExpr
  ) where

import Data.Text.Prettyprint.Doc.Render.Terminal
import Morloc.Data.Doc
import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import qualified Data.Map as Map
import qualified Data.Text.Prettyprint.Doc.Render.Terminal.Internal as Style

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

instance Pretty Source where
  pretty s
    = "source" <+> pretty (srcLang s)
    <> maybe "" (\ path -> " from" <+> dquotes (pretty path)) (srcPath s)
    <+> dquotes (pretty (srcName s))
    <+> "as" <+> pretty (srcAlias s) <> maybe "" (\t -> ":" <> pretty t) (srcLabel s)

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


prettyGreenType :: Type -> Doc AnsiStyle
prettyGreenType t = annotate typeStyle (prettyType t)

forallVars :: TypeU -> [Doc AnsiStyle]
forallVars (ForallU (TV _ v) t) = pretty v : forallVars t
forallVars _ = []

forallBlock :: TypeU -> Doc AnsiStyle
forallBlock (ForallU _ t) = forallBlock t
forallBlock t = prettyGreenTypeU t


prettyScream :: MT.Text -> Doc AnsiStyle
prettyScream x = annotate screamStyle (pretty x)

prettyLinePrefixes :: MT.Text -> Doc ann -> Doc ann 
prettyLinePrefixes prefix d = pretty . MT.unlines . map (\l -> prefix <> l) $ MT.lines (render d)


class PrettyType a where
  prettyType :: a -> Doc ann

instance PrettyType Type where
  prettyType (UnkT (TV _ v)) = "*" <> pretty v
  prettyType (VarT (TV _ "Unit")) = "()"
  prettyType (VarT v) = pretty v
  prettyType (FunT [] t) = "<MISSING> -> " <> prettyType t
  prettyType (FunT ts t) = encloseSep "(" ")" " -> " (map prettyType (ts <> [t]))
  prettyType (AppT t ts) = hsep (map prettyType (t:ts))
  prettyType (NamT o n ps rs)
    = block 4 (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
              (vsep [pretty k <+> "::" <+> prettyType x | (k, x) <- rs])

instance PrettyType GType where
  prettyType = prettyType . unGType

instance PrettyType CType where
  prettyType = prettyType . unCType


prettyGreenTypeU :: TypeU -> Doc AnsiStyle
prettyGreenTypeU t = annotate typeStyle (prettyTypeU t)

prettyTypeU :: TypeU -> Doc AnsiStyle
prettyTypeU (ExistU v ts ds)
  = angles $ (pretty v)
  <> list (map prettyTypeU ts)
  <> list (map prettyTypeU ds)
prettyTypeU t@(ForallU _ _) =
  parens $ "forall" <+> hsep (forallVars t) <+> "." <+> forallBlock t
prettyTypeU (VarU (TV _ "Unit")) = "()"
prettyTypeU (VarU v) = pretty v
prettyTypeU (FunU [] t) = parens $ "<MISSING> -> " <> prettyTypeU t
prettyTypeU (FunU ts t) = encloseSep "(" ")" " -> " (map prettyTypeU (ts <> [t]))
prettyTypeU (AppU t ts) = parens . hsep $ map prettyTypeU (t:ts)
prettyTypeU (NamU o n ps rs)
    = block 4 (viaShow o <+> pretty n <> encloseSep "<" ">" "," (map pretty ps))
              (vsep [pretty k <+> "::" <+> prettyTypeU x | (k, x) <- rs])

prettyUnresolvedPacker :: UnresolvedPacker -> Doc AnsiStyle
prettyUnresolvedPacker (UnresolvedPacker v t fs rs) = vsep
  [ pretty v
  , prettyGreenTypeU t 
  , "forward:" <+> hsep (map (\s -> pretty (srcAlias s) <> "@" <> pretty (srcLang s)) fs)
  , "reverse:" <+> hsep (map (\s -> pretty (srcAlias s) <> "@" <> pretty (srcLang s)) rs)
  ]

prettyPackMap :: PackMap -> Doc AnsiStyle
prettyPackMap m =  "----- pacmaps ----\n"
                <> vsep (map f (Map.toList m))
                <> "\n------------------" where
  f :: ((TVar, Int), [UnresolvedPacker]) -> Doc AnsiStyle
  f ((v, i), ps) =
    block 4
      ("packmap" <+> pretty v <> parens (pretty i))
      (vsep $ map prettyUnresolvedPacker ps)

-- For example @prettySAnnoMany id Nothing@ for the most simple printer
prettySAnno
  :: Foldable f
  => (c -> Doc ann)
  -> (g -> Doc ann)
  -> SAnno g f c
  -> Doc ann
prettySAnno writeCon writeGen (SAnno e g)
  = foldr (prettySExpr writeCon writeGen) ("G:" <+> writeGen g) e

prettySExpr
  :: Foldable f
  => (c -> Doc ann)
  -> (g -> Doc ann)
  -> (SExpr g f c, c)
  -> Doc ann
  -> Doc ann
prettySExpr fc fg (s, c) p = hang 2 . vsep $ [p, "C:" <+> fc c, f s] where
  f (UniS) = "UniS"
  f (VarS v) = "VarS:" <> pretty v
  f (AccS x k ) = hang 2 . vsep $ ["AccS[" <> pretty k <> "]", prettySAnno fc fg x]
  f (AppS x xs) = hang 2 . vsep $ ("AppS" <+> prettySAnno fc fg x) : map (prettySAnno fc fg) xs
  f (LamS vs x) = hang 2 . vsep $ ["LamS" <+> hsep (map pretty vs), prettySAnno fc fg x]
  f (LstS xs) = hang 2 . vsep $ ("LstS" : map (prettySAnno fc fg) xs)
  f (TupS xs) = hang 2 . vsep $ ("TupS" : map (prettySAnno fc fg) xs)
  f (NamS es)
    = block 4 "NamS" (vsep [pretty k <+> "=" <+> prettySAnno fc fg x | (k, x) <- es])
  f (NumS x) = "NumS(" <> viaShow x <> ")"
  f (LogS x) = "LogS(" <> viaShow x <> ")"
  f (StrS x) = "StrS(" <> viaShow x <> ")"
  f (CallS src) = "Calls(" <> pretty src <> ")"
