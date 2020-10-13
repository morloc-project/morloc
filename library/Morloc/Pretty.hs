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
  , prettyScream
  , prettyLinePrefixes
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

prettyLinePrefixes :: MT.Text -> Doc ann -> Doc ann 
prettyLinePrefixes prefix d =
  pretty . MT.unlines . map (\l -> prefix <> l) $ MT.lines (render d)

class PrettyType a where
  prettyType :: a -> Doc ann

instance PrettyType Type where
  prettyType (VarT (TV lang "Unit")) = "()"
  prettyType (VarT v) = pretty v
  prettyType (FunT t1@(FunT _ _) t2) =
    parens (prettyType t1) <+> "->" <+> prettyType t2
  prettyType (FunT t1 t2) = prettyType t1 <+> "->" <+> prettyType t2
  prettyType t@(Forall _ _) =
    "forall" <+> hsep (forallVars t) <+> "." <+> forallBlock t
  prettyType (ExistT v ts ds)
    = angles $ (pretty v)
    <> list (map prettyType ts)
    <> list (map (prettyType . unDefaultType) ds)
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
