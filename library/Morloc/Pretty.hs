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
  -- , prettySAnnoMany
  -- , prettySAnnoOne
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
prettyLinePrefixes prefix d =
  pretty . MT.unlines . map (\l -> prefix <> l) $ MT.lines (render d)


class PrettyType a where
  prettyType :: a -> Doc ann

instance PrettyType Type where
  prettyType (UnkT (TV _ v)) = "*" <> pretty v
  prettyType (VarT (TV _ "Unit")) = "()"
  prettyType (VarT v) = pretty v
  prettyType (FunT t1@(FunT _ _) t2) =
    parens (prettyType t1) <+> "->" <+> prettyType t2
  prettyType (FunT t1 t2) = prettyType t1 <+> "->" <+> prettyType t2
  prettyType (ArrT (TV Nothing "List") [t]) = brackets (prettyType t)
  prettyType (ArrT v ts) = pretty v <+> hsep (map prettyType ts)
  prettyType (NamT _ (TV Nothing _) _ entries) =
    encloseSep "{" "}" ","
      (map (\(v, e) -> pretty v <> ":" <> prettyType e) entries)
  prettyType (NamT _ (TV (Just lang) t) _ entries) =
    pretty t <> "@" <> viaShow lang <+>
    encloseSep "{" "}" ","
      (map (\(v, e) -> pretty v <> ":" <> prettyType e) entries)


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
  "forall" <+> hsep (forallVars t) <+> "." <+> forallBlock t
prettyTypeU (VarU (TV _ "Unit")) = "()"
prettyTypeU (VarU v) = pretty v
prettyTypeU (FunU t1@(FunU _ _) t2) =
  parens (prettyTypeU t1) <+> "->" <+> prettyTypeU t2
prettyTypeU (FunU t1 t2) = prettyTypeU t1 <+> "->" <+> prettyTypeU t2
prettyTypeU (ArrU v ts) = pretty v <+> hsep (map prettyTypeU ts)
prettyTypeU (NamU r (TV Nothing _) _ entries) =
  viaShow r <> encloseSep "{" "}" ", "
    (map (\(v, e) -> pretty v <+> "=" <+> prettyTypeU e) entries)
prettyTypeU (NamU r (TV (Just lang) t) _ entries) =
  pretty t <> "@" <> viaShow lang <+>
  viaShow r <> encloseSep "{" "}" ", "
    (map (\(v, e) -> pretty v <+> "=" <+> prettyTypeU e) entries)

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


-- prettySAnnoMany :: SAnno GMeta Many [CType] -> Doc AnsiStyle
-- prettySAnnoMany (SAnno (Many xs0) g) =
--      pretty (metaId g)
--   <> maybe "" (\n -> " " <> pretty n) (metaName g)
--   <+> "::" <+> maybe "_" prettyGreenTypeU (metaGType g)
--   <> line <> indent 5 (vsep (map writeSome xs0))
--   where
--     writeSome (s, ts)
--       =  "_ ::"
--       <+> encloseSep "{" "}" ";" (map prettyType ts)
--       <> line <> writeExpr s
--
--     writeExpr (AccS x k) = pretty k <+> "from " <> nest 2 (prettySAnnoMany x)
--     writeExpr (ListS xs) = list (map prettySAnnoMany xs)
--     writeExpr (TupleS xs) = list (map prettySAnnoMany xs)
--     writeExpr (RecS entries) = encloseSep "{" "}" "," $
--       map (\(k,v) -> pretty k <+> "=" <+> prettySAnnoMany v) entries
--     writeExpr (LamS vs x)
--       = "LamS"
--       <+> list (map pretty vs)
--       <> line <> indent 2 (prettySAnnoMany x)
--     writeExpr (AppS f xs) = "AppS" <+> indent 2 (vsep (prettySAnnoMany f : map prettySAnnoMany xs))
--     writeExpr x = descSExpr x
--
-- -- For example @prettySAnnoOne id Nothing@ for the most simple printer
-- prettySAnnoOne
--   :: (a -> CType) -> Maybe (a -> Doc ann) -> SAnno GMeta One a -> Doc ann
-- prettySAnnoOne getType extra s = hang 2 . vsep $ ["AST:", describe s]
--   where
--     addExtra x = case extra of
--       (Just f) -> " " <> f x
--       Nothing -> ""
--
--     describe (SAnno (One (x@(AccS _ _), _)) _) = descSExpr x
--     describe (SAnno (One (x@(ListS _), _)) _) = descSExpr x
--     describe (SAnno (One (x@(TupleS _), _)) _) = descSExpr x
--     describe (SAnno (One (x@(RecS _), _)) _) = descSExpr x
--     describe (SAnno (One (x@(AppS f xs), c)) g) =
--       hang 2 . vsep $
--         [ pretty (metaId g) <+> descSExpr x <+> parens (prettyType (getType c)) <> addExtra c
--         , describe f
--         ] ++ map describe xs
--     describe (SAnno (One (f@(LamS _ x), c)) g) = do
--       hang 2 . vsep $
--         [ pretty (metaId g)
--             <+> name (getType c) g
--             <+> descSExpr f
--             <+> parens (prettyType (getType c))
--             <> addExtra c
--         , describe x
--         ]
--     describe (SAnno (One (x, c)) g) =
--           pretty (metaId g)
--       <+> descSExpr x
--       <+> parens (prettyType (getType c))
--       <>  addExtra c
--
--     name :: CType -> GMeta -> Doc ann
--     name t g =
--       let lang = fromJust (langOf t)
--       in maybe
--           ("_" <+> viaShow lang <+> "::")
--           (\x -> pretty x <+> viaShow lang <+> "::")
--           (metaName g)
--
-- descSExpr :: SExpr g f c -> Doc ann
-- descSExpr (UniS) = "UniS"
-- descSExpr (VarS v) = "VarS" <+> pretty v
-- descSExpr (CallS src)
--   =   "CallS"
--   <+> pretty (srcAlias src) <+> "<" <> viaShow (srcLang src) <> ">"
-- descSExpr (AccS _ k) = "@" <> pretty k
-- descSExpr (ListS _) = "ListS"
-- descSExpr (TupleS _) = "TupleS"
-- descSExpr (LamS vs _) = "LamS" <+> hsep (map pretty vs)
-- descSExpr (AppS _ _) = "AppS"
-- descSExpr (NumS _) = "NumS"
-- descSExpr (LogS _) = "LogS"
-- descSExpr (StrS _) = "StrS"
-- descSExpr (RecS _) = "RecS"
