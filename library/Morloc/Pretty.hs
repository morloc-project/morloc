{-|
Module      : Morloc.Pretty
Description : Pretty print instances
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Pretty
  ( prettyType
  , prettyGreenType
  , prettyGreenUnresolvedType
  , prettyScream
  , prettyLinePrefixes
  , prettyUnresolvedPacker
  , prettyPackMap
  ) where

import Data.Text.Prettyprint.Doc.Render.Terminal
import Morloc.Data.Doc
import Morloc.Namespace
import qualified Morloc.Data.Text as MT
import qualified Data.Set as Set
import qualified Data.Map as Map
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

instance Pretty Lang where
  pretty = viaShow

instance Pretty Source where
  pretty (Source name lang pathmay alias)
    = "source" <+> pretty lang
    <> maybe "" (\path->" from" <+> dquotes (pretty path)) pathmay
    <+> dquotes (pretty name) <+> "as" <+> pretty alias

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

forallVars :: UnresolvedType -> [Doc AnsiStyle]
forallVars (ForallU (TV _ v) t) = pretty v : forallVars t
forallVars _ = []

forallBlock :: UnresolvedType -> Doc AnsiStyle
forallBlock (ForallU _ t) = forallBlock t
forallBlock t = prettyGreenUnresolvedType t


prettyScream :: MT.Text -> Doc AnsiStyle
prettyScream x = annotate screamStyle (pretty x)

prettyLinePrefixes :: MT.Text -> Doc ann -> Doc ann 
prettyLinePrefixes prefix d =
  pretty . MT.unlines . map (\l -> prefix <> l) $ MT.lines (render d)


class PrettyType a where
  prettyType :: a -> Doc ann

instance PrettyType Type where
  prettyType (UnkT (TV _ v)) = "*" <> pretty v
  prettyType (VarT (TV lang "Unit")) = "()"
  prettyType (VarT v) = pretty v
  prettyType (FunT t1@(FunT _ _) t2) =
    parens (prettyType t1) <+> "->" <+> prettyType t2
  prettyType (FunT t1 t2) = prettyType t1 <+> "->" <+> prettyType t2
  prettyType (ArrT v ts) = pretty v <+> hsep (map prettyType ts)
  prettyType (NamT (TV Nothing _) entries) =
    encloseSep "{" "}" ", "
      (map (\(v, e) -> pretty v <+> "=" <+> prettyType e) entries)
  prettyType (NamT (TV (Just lang) t) entries) =
    pretty t <> "@" <> viaShow lang <+>
    encloseSep "{" "}" ", "
      (map (\(v, e) -> pretty v <+> "=" <+> prettyType e) entries)

instance PrettyType GType where
  prettyType = prettyType . unGType

instance PrettyType CType where
  prettyType = prettyType . unCType


prettyGreenUnresolvedType :: UnresolvedType -> Doc AnsiStyle
prettyGreenUnresolvedType t = annotate typeStyle (prettyUnresolvedType t)

prettyUnresolvedType :: UnresolvedType -> Doc AnsiStyle
prettyUnresolvedType (ExistU v ts ds)
  = angles $ (pretty v)
  <> list (map prettyUnresolvedType ts)
  <> list (map prettyUnresolvedType ds)
prettyUnresolvedType t@(ForallU _ _) =
  "forall" <+> hsep (forallVars t) <+> "." <+> forallBlock t
prettyUnresolvedType (VarU (TV lang "Unit")) = "()"
prettyUnresolvedType (VarU v) = pretty v
prettyUnresolvedType (FunU t1@(FunU _ _) t2) =
  parens (prettyUnresolvedType t1) <+> "->" <+> prettyUnresolvedType t2
prettyUnresolvedType (FunU t1 t2) = prettyUnresolvedType t1 <+> "->" <+> prettyUnresolvedType t2
prettyUnresolvedType (ArrU v ts) = pretty v <+> hsep (map prettyUnresolvedType ts)
prettyUnresolvedType (NamU (TV Nothing _) entries) =
  encloseSep "{" "}" ", "
    (map (\(v, e) -> pretty v <+> "=" <+> prettyUnresolvedType e) entries)
prettyUnresolvedType (NamU (TV (Just lang) t) entries) =
  pretty t <> "@" <> viaShow lang <+>
  encloseSep "{" "}" ", "
    (map (\(v, e) -> pretty v <+> "=" <+> prettyUnresolvedType e) entries)

prettyUnresolvedPacker :: UnresolvedPacker -> Doc AnsiStyle
prettyUnresolvedPacker (UnresolvedPacker v t fs rs) = vsep
  [ pretty v
  , prettyGreenUnresolvedType t 
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
