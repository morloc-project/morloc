{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.Printer.Python3
Description : Python3 IR printer
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Grammars.Translator.Printer.Python3
  ( printExpr
  , printStmt
  , printStmts
    -- * Pool-level rendering
  , printProgram
  ) where

import Morloc.CodeGenerator.Grammars.Common (DispatchEntry(..), manNamer)
import Morloc.CodeGenerator.Grammars.Translator.Imperative
import Morloc.CodeGenerator.Namespace (MDoc, Lang(..))
import Morloc.Data.Doc
import Morloc.DataFiles as DF
import Morloc.Quasi

printExpr :: IExpr -> MDoc
printExpr (IVar v) = v
printExpr (IBoolLit True) = "True"
printExpr (IBoolLit False) = "False"
printExpr INullLit = "None"
printExpr (IIntLit i) = viaShow i
printExpr (IRealLit r) = viaShow r
printExpr (IStrLit s) = dquotes (pretty s)
printExpr (IListLit es) = list (map printExpr es)
printExpr (ITupleLit es) = tupled (map printExpr es)
printExpr (IRecordLit _ _ entries) =
  "dict" <> tupled [pretty k <> "=" <> printExpr e | (k, e) <- entries]
printExpr (IAccess e (IIdx i)) = printExpr e <> "[" <> pretty i <> "]"
printExpr (IAccess e (IKey k)) = printExpr e <> "[" <> dquotes (pretty k) <> "]"
printExpr (IAccess e (IField f)) = printExpr e <> "." <> f
printExpr (ISerCall schema e) = [idoc|morloc.put_value(#{printExpr e}, "#{schema}")|]
printExpr (IDesCall schema _ e) = [idoc|morloc.get_value(#{printExpr e}, "#{schema}")|]
printExpr (IPack packer e) = packer <> parens (printExpr e)
printExpr (ICall f Nothing argGroups) =
  f <> hsep (map (tupled . map printExpr) argGroups)
printExpr (ICall f (Just _) argGroups) =
  f <> hsep (map (tupled . map printExpr) argGroups)
printExpr (IForeignCall _ _ _) = error "use IRawExpr for Python foreign calls"
printExpr (IRemoteCall _ _ _ _) = error "use IRawExpr for Python remote calls"
printExpr (ILambda args body) = "lambda" <+> hsep (punctuate "," args) <> ":" <+> printExpr body
printExpr (IRawExpr d) = d

printStmt :: IStmt -> MDoc
printStmt (IAssign v Nothing e) = v <+> "=" <+> printExpr e
printStmt (IAssign v (Just _) e) = v <+> "=" <+> printExpr e
printStmt (IMapList resultVar _ iterVar collection bodyStmts yieldExpr) =
  vsep
    [ [idoc|#{resultVar} = []|]
    , nest 4 (vsep
        ( [idoc|for #{iterVar} in #{collection}:|]
        : map printStmt bodyStmts
        ++ [[idoc|#{resultVar}.append(#{printExpr yieldExpr})|]]
        ))
    ]
printStmt (IReturn e) = "return(" <> printExpr e <> ")"
printStmt (IExprStmt e) = printExpr e
printStmt (IFunDef _ _ _ _) = error "IFunDef not yet implemented for Python printer"

printStmts :: [IStmt] -> [MDoc]
printStmts = map printStmt

-- | Assemble a complete Python pool file from an IProgram and lib paths.
printProgram :: [MDoc] -> IProgram -> MDoc
printProgram libs prog =
  format
    (DF.embededFileText (DF.poolTemplate Python3Lang))
    "# <<<BREAK>>>"
    [path, includeStatements (ipSources prog), vsep (ipManifolds prog), dispatch]
  where
    path = [idoc|sys.path = #{list (map makePath libs)} + sys.path|]
    makePath filename = [idoc|os.path.expanduser(#{dquotes(filename)})|]

    includeStatements [] = ""
    includeStatements _ = vsep ("import importlib" : ipSources prog)

    dispatch = vsep [localDispatch, remoteDispatch]

    localDispatch = align . vsep $
      [ "dispatch = {"
      , indent 4 (vsep [pretty i <> ":" <+> manNamer i <> "," | DispatchEntry i _ <- ipLocalDispatch prog])
      , "}"
      ]

    remoteDispatch = align . vsep $
      [ "remote_dispatch = {"
      , indent 4 (vsep [pretty i <> ":" <+> manNamer i <> "_remote" <> "," | DispatchEntry i _ <- ipRemoteDispatch prog])
      , "}"
      ]
