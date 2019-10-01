{-|
Module      : Morloc.Pretty
Description : Pretty print instances
Copyright   : (c) Zebulun Arendsee, 2019
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pretty ( ) where

import Morloc.Namespace
import Morloc.Data.Doc

instance Pretty MType where
  pretty (MConcType _ n []) = pretty n
  pretty (MConcType _ n ts) = parens $ hsep (pretty n:(map pretty ts))
  pretty (MAbstType _ n []) = pretty n
  pretty (MAbstType _ n ts) = parens $ hsep (pretty n:(map pretty ts))
  pretty (MFuncType _ ts o) = parens $
    (hcat . punctuate ", ") (map pretty ts) <> " -> " <> pretty o
