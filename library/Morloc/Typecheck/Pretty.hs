{-|
Module      : Morloc.Typecheck.Pretty
Description : Pretty printing for type objects
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Typecheck.Pretty
  ( prettyGammaIndex
  ) where

import Morloc.Namespace
import Morloc.Data.Doc hiding (putDoc)
import Morloc.Pretty
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Text.Megaparsec as Mega
import qualified Morloc.Data.Text as MT 


prettyGammaIndex :: GammaIndex -> Doc AnsiStyle
prettyGammaIndex (VarG tv) = "VarG:" <+> pretty tv
prettyGammaIndex (ExistG tv ts ds)
  = "ExistG:"
  <+> pretty tv
  <+> list (map (parens . prettyGreenTypeU) ts)
  <+> list (map (parens . prettyGreenTypeU) ds)
prettyGammaIndex (SolvedG tv t) = "SolvedG:" <+> pretty tv <+> "=" <+> prettyGreenTypeU t
prettyGammaIndex (MarkG tv) = "MarkG:" <+> pretty tv
prettyGammaIndex (SrcG (Source ev1 lang _ _ _)) = "SrcG:" <+> pretty ev1 <+> viaShow lang
prettyGammaIndex (SerialConstraint t1 t2) = "UnsolvedConstraint:" <+> prettyGreenTypeU t1 <+> prettyGreenTypeU t2
prettyGammaIndex (AnnG v t) = pretty v <+> "::" <+> prettyGreenTypeU t
