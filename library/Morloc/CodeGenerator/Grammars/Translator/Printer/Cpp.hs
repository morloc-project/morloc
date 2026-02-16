{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
    -- * Pool-level rendering
  , printDispatch
  , printProgram
    -- * Struct/serializer rendering
  , printStructTypedef
  , printSerializer
  , printDeserializer
  , printTemplateHeader
  , printRecordTemplate
  ) where

import Morloc.CodeGenerator.Grammars.Common (DispatchEntry(..), manNamer)
import Morloc.CodeGenerator.Grammars.Translator.Imperative
import Morloc.CodeGenerator.Namespace (MDoc, Lang(..))
import Morloc.Data.Doc
import Morloc.DataFiles as DF
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
printExpr (ISuspend e) = "[&](){return " <> printExpr e <> ";}"
printExpr (IForce e) = printExpr e <> "()"

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

-- | Render C++ dispatch functions from structured dispatch entries.
printDispatch :: [DispatchEntry] -> [DispatchEntry] -> MDoc
printDispatch locals remotes =
  [idoc|uint8_t* local_dispatch(uint32_t mid, const uint8_t** args){
    switch(mid){
        #{align (vsep localCases)}
        default:
            std::ostringstream oss;
            oss << "Invalid local manifold id: " << mid;
            throw std::runtime_error(oss.str());
    }
}

uint8_t* remote_dispatch(uint32_t mid, const uint8_t** args){
    switch(mid){
        #{align (vsep remoteCases)}
        default:
            std::ostringstream oss;
            oss << "Invalid remote manifold id: " << mid;
            throw std::runtime_error(oss.str());
    }
}|]
  where
    localCases = map (makeCase "") locals
    remoteCases = map (makeCase "_remote") remotes

    makeCase :: MDoc -> DispatchEntry -> MDoc
    makeCase suffix (DispatchEntry i n) =
      "case" <+> pretty i
        <> ":"
          <+> "return"
          <+> manNamer i
        <> suffix
        <> tupled ["args[" <> pretty j <> "]" | j <- take n ([0 ..] :: [Int])]
        <> ";"

-- | Assemble a complete C++ pool file from an IProgram and C++-specific extras.
printProgram :: [MDoc] -> [MDoc] -> IProgram -> MDoc
printProgram serialization signatures prog =
  format
    (DF.embededFileText (DF.poolTemplate CppLang))
    "// <<<BREAK>>>"
    [ vsep (ipSources prog)
    , vsep serialization
    , vsep signatures
    , vsep (ipManifolds prog)
    , printDispatch (ipLocalDispatch prog) (ipRemoteDispatch prog)
    ]

printTemplateHeader :: [MDoc] -> MDoc
printTemplateHeader [] = ""
printTemplateHeader ts = "template" <+> encloseSep "<" ">" "," ["class" <+> t | t <- ts]

printRecordTemplate :: [MDoc] -> MDoc
printRecordTemplate [] = ""
printRecordTemplate ts = encloseSep "<" ">" "," ts

-- | Render a C++ struct definition.
printStructTypedef ::
  [MDoc] -> -- template parameters (e.g., ["T"])
  MDoc -> -- the name of the structure (e.g., "Person")
  [(MDoc, MDoc)] -> -- key and type for all fields
  MDoc
printStructTypedef params rname fields = vsep [template, struct]
  where
    template = printTemplateHeader params
    struct =
      block
        4
        ("struct" <+> rname)
        (vsep [t <+> k <> ";" | (k, t) <- fields])
        <> ";"

-- | Render a C++ serializer (toAnything) for a struct.
printSerializer ::
  [MDoc] -> -- template parameters
  MDoc -> -- type of thing being serialized
  [(MDoc, MDoc)] -> -- key and type for all fields
  MDoc
printSerializer params rtype fields =
  [idoc|
#{printTemplateHeader params}
void* toAnything(void* dest, void** cursor, const Schema* schema, const #{rtype}& obj)
{
    return toAnything(dest, cursor, schema, std::make_tuple#{arguments});
}
|]
  where
    arguments = tupled ["obj." <> key | (key, _) <- fields]

-- | Render a C++ deserializer (fromAnything + get_shm_size) for a struct.
printDeserializer ::
  Bool -> -- build object with constructor
  [MDoc] -> -- template parameters
  MDoc -> -- type of thing being deserialized
  [(MDoc, MDoc)] -> -- key and type for all fields
  MDoc
printDeserializer _ params rtype fields =
  [idoc|
#{printTemplateHeader params}
#{block 4 header body}

#{printTemplateHeader params}
#{block 4 headerGetSize bodyGetSize}
|]
  where
    header =
      [idoc|#{rtype} fromAnything(const Schema* schema, const void * anything, #{rtype}* dummy = nullptr)|]
    body =
      vsep $
        [[idoc|#{rtype} obj;|]]
          <> zipWith assignFields [0 ..] fields
          <> ["return obj;"]

    assignFields :: Int -> (MDoc, MDoc) -> MDoc
    assignFields idx (keyName, keyType) =
      vsep
        [ [idoc|#{keyType}* elemental_dumby_#{keyName} = nullptr;|]
        , [idoc|obj.#{keyName} = fromAnything(schema->parameters[#{pretty idx}], (char*)anything + schema->offsets[#{pretty idx}], elemental_dumby_#{keyName});|]
        ]

    headerGetSize = [idoc|size_t get_shm_size(const Schema* schema, const #{rtype}& data)|]
    bodyGetSize =
      vsep $
        ["size_t size = 0;"]
          <> [getSize idx key | (idx, (key, _)) <- zip [0 ..] fields]
          <> ["return size;"]

    getSize :: Int -> MDoc -> MDoc
    getSize idx key = [idoc|size += get_shm_size(schema->parameters[#{pretty idx}], data.#{key});|]
