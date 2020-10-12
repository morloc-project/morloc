{-|
Module      : Morloc.TypeChecker.Pretty
Description : Pretty is as pretty does
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeChecker.Pretty (cute, ugly) where

import Morloc.Namespace
import qualified Data.Map as Map
import Morloc.Data.Doc hiding (putDoc)
import Morloc.Pretty (prettyExpr)
import Data.Text.Prettyprint.Doc.Render.Terminal (putDoc, AnsiStyle)

cute :: DAG MVar [(EVar, EVar)] TypedNode -> IO ()
cute d = mapM_ (putDoc . cute') (Map.toList d) where
  cute' :: (MVar, (TypedNode, [(MVar, [(EVar, EVar)])])) -> Doc AnsiStyle
  cute' (v, (n, xs)) = block 4 (pretty v) (cuteBody n xs)

cuteBody :: TypedNode -> [(MVar, [(EVar, EVar)])] -> Doc AnsiStyle
cuteBody t xs = vsep (map (uncurry cuteImport) xs) <> line <> cuteTypedNode t

cuteImport :: MVar -> [(EVar, EVar)] -> Doc AnsiStyle
cuteImport m xs
  = "from" <+> pretty m <+> "import"
  <+> tupled (map (\(v1,v2) -> pretty v1 <+> "as" <+> pretty v2) xs)

cuteTypedNode :: TypedNode -> Doc AnsiStyle
cuteTypedNode t = vsep (map prettyExpr (typedNodeBody t))

-- FIXME: why exactly do I even have this ugly function???
ugly :: DAG MVar [(EVar, EVar)] TypedNode -> IO ()
ugly = cute 
