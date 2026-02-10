{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : Morloc.CodeGenerator.Grammars.Translator.Printer.Cpp
Description : C++ IR printer
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io
-}
module Morloc.CodeGenerator.Grammars.Translator.Printer.Cpp
  ( printExpr
  , printStmt
  , printStmts
  ) where

import Morloc.CodeGenerator.Grammars.Translator.Imperative
import Morloc.CodeGenerator.Namespace (NamType(..), Key(..), MDoc)
import Morloc.Data.Doc
import Morloc.Quasi

printExpr :: IExpr -> MDoc
printExpr (IVar v) = v
printExpr (IBoolLit True) = "true"
printExpr (IBoolLit False) = "false"
printExpr INullLit = "nullptr"
printExpr (IIntLit i) = viaShow i
printExpr (IRealLit r) = viaShow r
printExpr (IStrLit s) = [idoc|std::string("#{pretty s}")|]
printExpr (IListLit es) = encloseSep "{" "}" "," (map printExpr es)
printExpr (ITupleLit es) = "std::make_tuple" <> tupled (map printExpr es)
printExpr (IRecordLit _ _ entries) =
  encloseSep "{" "}" "," (map (printExpr . snd) entries)
printExpr (IAccess e (IIdx i)) = "std::get<" <> pretty i <> ">(" <> printExpr e <> ")"
printExpr (IAccess e (IKey _)) = printExpr e -- should not be reached for C++
printExpr (IAccess e (IField f)) = printExpr e <> "." <> f
printExpr (ISerCall schema e) = [idoc|_put_value(#{printExpr e}, "#{schema}")|]
printExpr (IDesCall schema (Just (IType rawtype)) e) = [idoc|_get_value<#{rawtype}>(#{printExpr e}, "#{schema}")|]
printExpr (IDesCall schema Nothing e) = [idoc|_get_value(#{printExpr e}, "#{schema}")|]
printExpr (IPack packer e) = packer <> parens (printExpr e)
printExpr (ICall f Nothing argGroups) =
  f <> hsep (map (tupled . map printExpr) argGroups)
printExpr (ICall f (Just ts) argGroups) =
  f <> encloseSep "<" ">" "," [t' | IType t' <- ts] <> hsep (map (tupled . map printExpr) argGroups)
printExpr (IForeignCall _ _ _) = error "use IRawExpr for C++ foreign calls"
printExpr (IRemoteCall _ _ _ _) = error "use IRawExpr for C++ remote calls"
printExpr (ILambda args body) =
  "[&](" <> hsep (punctuate "," ["auto" <+> a | a <- args]) <> "){return " <> printExpr body <> ";}"
printExpr (IRawExpr d) = d

printStmt :: IStmt -> MDoc
printStmt (IAssign v Nothing e) = "auto" <+> v <+> "=" <+> printExpr e <> ";"
printStmt (IAssign v (Just (IType t)) e) = t <+> v <+> "=" <+> printExpr e <> ";"
-- C++ uses an indexed for loop with push_back
printStmt (IMapList resultVar resultType iterVar collection bodyStmts yieldExpr) =
  vsep
    [ resultDecl
    , block 4
        [idoc|for(size_t #{iterVar}_idx = 0; #{iterVar}_idx < #{collection}.size(); #{iterVar}_idx++)|]
        (vsep
          ( [idoc|auto #{iterVar} = #{collection}[#{iterVar}_idx];|]
          : map printStmt bodyStmts
          ++ [[idoc|#{resultVar}.push_back(#{printExpr yieldExpr});|]]
          ))
    ]
  where
    resultDecl = case resultType of
      Just (IType t) -> [idoc|#{t} #{resultVar};|]
      Nothing -> printStmt (IAssign resultVar Nothing (IListLit []))
printStmt (IReturn e) = "return(" <> printExpr e <> ");"
printStmt (IExprStmt e) = printExpr e <> ";"
printStmt (IFunDef _ _ _ _) = error "IFunDef not yet implemented for C++ printer"

printStmts :: [IStmt] -> [MDoc]
printStmts = map printStmt
