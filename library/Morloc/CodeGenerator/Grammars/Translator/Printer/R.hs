{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.Printer.R
Description : R IR printer
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Grammars.Translator.Printer.R
  ( printExpr
  , printStmt
  , printStmts
    -- * Pool-level rendering
  , printProgram
  ) where

import Morloc.CodeGenerator.Grammars.Translator.Imperative
import Morloc.CodeGenerator.Namespace (MDoc, Lang(..))
import Morloc.Data.Doc
import Morloc.DataFiles as DF
import Morloc.Quasi

printExpr :: IExpr -> MDoc
printExpr (IVar v) = v
printExpr (IBoolLit True) = "TRUE"
printExpr (IBoolLit False) = "FALSE"
printExpr INullLit = "NULL"
printExpr (IIntLit i) = viaShow i
printExpr (IRealLit r) = viaShow r
printExpr (IStrLit s) = dquotes (pretty s)
printExpr (IListLit es) = "list" <> tupled (map printExpr es)
printExpr (ITupleLit es) = "list" <> tupled (map printExpr es)
printExpr (IRecordLit _ _ entries) =
  "list" <> tupled [pretty k <> "=" <> printExpr e | (k, e) <- entries]
printExpr (IAccess e (IIdx i)) = printExpr e <> "[[" <> pretty (i + 1) <> "]]"
printExpr (IAccess e (IKey k)) = printExpr e <> "[[" <> dquotes (pretty k) <> "]]"
printExpr (IAccess e (IField f)) = printExpr e <> "$" <> f
printExpr (ISerCall schema e) = [idoc|morloc_put_value(#{printExpr e}, "#{schema}")|]
printExpr (IDesCall schema _ e) = [idoc|morloc_get_value(#{printExpr e}, "#{schema}")|]
printExpr (IPack packer e) = packer <> parens (printExpr e)
printExpr (ICall f Nothing argGroups) =
  f <> hsep (map (tupled . map printExpr) argGroups)
printExpr (ICall f (Just _) argGroups) =
  f <> hsep (map (tupled . map printExpr) argGroups)
printExpr (IForeignCall _ _ _) = error "use IRawExpr for R foreign calls"
printExpr (IRemoteCall _ _ _ _) = error "use IRawExpr for R remote calls"
printExpr (ILambda args body) =
  "function" <+> tupled args <> "{" <> printExpr body <> "}"
printExpr (IRawExpr d) = d

printStmt :: IStmt -> MDoc
printStmt (IAssign v Nothing e) = v <+> "<-" <+> printExpr e
printStmt (IAssign v (Just _) e) = v <+> "<-" <+> printExpr e
printStmt (IMapList resultVar _ iterVar collection bodyStmts yieldExpr) =
  block 4
    [idoc|#{resultVar} <- lapply(#{collection}, function(#{iterVar})|]
    (vsep (map printStmt bodyStmts ++ [printExpr yieldExpr]))
    <> ")"
printStmt (IReturn e) = "return(" <> printExpr e <> ")"
printStmt (IExprStmt e) = printExpr e
printStmt (IFunDef _ _ _ _) = error "IFunDef not yet implemented for R printer"

printStmts :: [IStmt] -> [MDoc]
printStmts = map printStmt

-- | Assemble a complete R pool file from an IProgram and dynlib docs.
printProgram :: [MDoc] -> IProgram -> MDoc
printProgram dynlibs prog =
  format
    (DF.embededFileText (DF.poolTemplate RLang))
    "# <<<BREAK>>>"
    [vsep (ipSources prog), vsep dynlibs, vsep (ipManifolds prog)]
