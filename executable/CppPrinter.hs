{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : CppPrinter
Description : Pretty-print the imperative IR as C++ source code
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Converts 'IStmt' and 'IExpr' IR nodes into C++ source text. Handles
type rendering, struct definitions, forward declarations, and C++
idioms (templates, shared_ptr, std::variant).
-}
module CppPrinter
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

import Morloc.CodeGenerator.Grammars.Common (DispatchEntry (..), manNamer)
import Morloc.CodeGenerator.Grammars.Translator.Imperative
import Morloc.CodeGenerator.Namespace (MDoc, RealLit (..))
import Morloc.Data.Doc
import Morloc.DataFiles as DF
import Morloc.Quasi

printExpr :: IExpr -> MDoc
printExpr (IVar v) = pretty v
printExpr (IBoolLit True) = "true"
printExpr (IBoolLit False) = "false"
-- When the storage type is known, emit a value-initialised expression
-- of that type (`std::optional<int>{}`, `std::shared_ptr<X>{}`). The
-- type hint produced by `cppTypeOf` already accounts for the recursive
-- shared_ptr indirection, so `IType{}` produces a null value for both
-- the non-recursive optional and the recursive shared_ptr storage.
-- Crucially, the typed form survives template-argument deduction at
-- call sites (e.g. `morloc_fromMaybe<A>(std::optional<A>)`), where bare
-- @std::nullopt@ has type `std::nullopt_t` and cannot pin `A`.
-- Fall back to @std::nullopt@ when no hint is available; in that path
-- the value is only ever assigned to a slot whose type is locally
-- declared, where the implicit conversion from nullopt_t suffices.
printExpr (INullLit (Just t)) = renderIType t <> "{}"
printExpr (INullLit Nothing) = "std::nullopt"
printExpr (IIntLit Nothing i) = viaShow i
printExpr (IIntLit (Just t) i)
  | t == "int" = viaShow i
  | otherwise = "static_cast<" <> pretty t <> ">(" <> viaShow i <> ")"
printExpr (IRealLit Nothing r) = renderRealLit r
printExpr (IRealLit (Just t) r)
  | t == "double" = renderRealLit r
  | otherwise = "static_cast<" <> pretty t <> ">(" <> renderRealLit r <> ")"
-- Use the (ptr, size) std::string constructor so embedded NUL bytes in the
-- literal survive. The one-argument form interprets the buffer as a C-string
-- and would truncate at the first NUL.
printExpr (IStrLit _ s) = [idoc|std::string(#{textEsc' s}, #{pretty (utf8Length s)})|]
printExpr (IListLit es) = encloseSep "{" "}" "," (map printExpr es)
printExpr (ITupleLit es) = "std::make_tuple" <> tupled (map printExpr es)
printExpr (IRecordLit _ _ entries) =
  encloseSep "{" "}" "," (map (printExpr . snd) entries)
printExpr (IAccess e (IIdx i)) = "std::get<" <> pretty i <> ">(" <> printExpr e <> ")"
printExpr (IAccess e (IKey _)) = printExpr e -- should not be reached for C++
printExpr (IAccess e (IField f)) = printExpr e <> "." <> pretty f
printExpr (ISerCall sid e) = [idoc|_put_value(#{printExpr e}, mlc_schema_table[#{pretty sid}])|]
printExpr (IDesCall sid (Just rawtype) e) = [idoc|_get_value<#{renderIType rawtype}>(#{printExpr e}, mlc_schema_table[#{pretty sid}])|]
printExpr (IDesCall sid Nothing e) = [idoc|_get_value(#{printExpr e}, mlc_schema_table[#{pretty sid}])|]
printExpr (IPack packer e) = pretty packer <> parens (printExpr e)
printExpr (ICall f Nothing argGroups) =
  pretty f <> hsep (map (tupled . map printExpr) argGroups)
printExpr (ICall f (Just ts) argGroups) =
  pretty f
    <> encloseSep "<" ">" "," (map renderIType ts)
    <> hsep (map (tupled . map printExpr) argGroups)
printExpr (IForeignCall _ _ _) = error "use IRawExpr for C++ foreign calls"
printExpr (IRemoteCall _ _ _ _) = error "use IRawExpr for C++ remote calls"
printExpr (ILambda args body) =
  "[&]("
    <> hsep (punctuate "," ["auto" <+> pretty a | a <- args])
    <> "){return "
    <> printExpr body
    <> ";}"
printExpr (IRawExpr d) = pretty d
printExpr (IDoBlock e) = "[&](){return " <> printExpr e <> ";}"
printExpr (IEval e) = printExpr e <> "()"
printExpr (IIntrinsicHash sid e) =
  [idoc|_mlc_hash(#{printExpr e}, mlc_schema_table[#{pretty sid}])|]
printExpr (IIntrinsicSave fmt sid level e path)
  | fmt == "json" = [idoc|_mlc_save_json(#{printExpr e}, mlc_schema_table[#{pretty sid}], #{printExpr level}, #{printExpr path})|]
  | fmt == "voidstar" = [idoc|_mlc_save_voidstar(#{printExpr e}, mlc_schema_table[#{pretty sid}], #{printExpr level}, #{printExpr path})|]
  | otherwise = [idoc|_mlc_save(#{printExpr e}, mlc_schema_table[#{pretty sid}], #{printExpr level}, #{printExpr path})|]
printExpr (IIntrinsicLoad sid (Just t) path) =
  [idoc|_mlc_load<#{renderIType t}>(mlc_schema_table[#{pretty sid}], #{printExpr path})|]
printExpr (IIntrinsicLoad sid Nothing path) =
  [idoc|_mlc_load(mlc_schema_table[#{pretty sid}], #{printExpr path})|]
printExpr (IIntrinsicShow sid e) =
  [idoc|_mlc_show(#{printExpr e}, mlc_schema_table[#{pretty sid}])|]
printExpr (IIntrinsicRead sid (Just t) e) =
  [idoc|_mlc_read<#{renderIType t}>(mlc_schema_table[#{pretty sid}], #{printExpr e})|]
printExpr (IIntrinsicRead sid Nothing e) =
  [idoc|_mlc_read(mlc_schema_table[#{pretty sid}], #{printExpr e})|]

-- C++ non-finite literals: rely on the C99 macros INFINITY and NAN. They are
-- float-typed per C99 but convert losslessly to double; non-default Real
-- types are wrapped in static_cast<T>(...) by the IRealLit caller above.
renderRealLit :: RealLit -> MDoc
renderRealLit (RealFinite r) = viaShow r
renderRealLit RealPosInf = "INFINITY"
renderRealLit RealNegInf = "-INFINITY"
renderRealLit RealNaN    = "NAN"

printStmt :: IStmt -> MDoc
printStmt (IAssign v Nothing e) = "auto" <+> pretty v <+> "=" <+> printExpr e <> ";"
printStmt (IAssign v (Just t) e) = renderIType t <+> pretty v <+> "=" <+> printExpr e <> ";"
-- C++ uses an indexed for loop with push_back
printStmt (IMapList resultVar resultType iterVar collection bodyStmts yieldExpr) =
  vsep
    [ resultDecl
    , block
        4
        [idoc|for(size_t #{pretty iterVar}_idx = 0; #{pretty iterVar}_idx < #{printExpr collection}.size(); #{pretty iterVar}_idx++)|]
        ( vsep
            ( [idoc|auto #{pretty iterVar} = #{printExpr collection}[#{pretty iterVar}_idx];|]
                : map printStmt bodyStmts
                ++ [[idoc|#{pretty resultVar}.push_back(#{printExpr yieldExpr});|]]
            )
        )
    ]
  where
    resultDecl = case resultType of
      Just t -> [idoc|#{renderIType t} #{pretty resultVar};|]
      Nothing -> printStmt (IAssign resultVar Nothing (IListLit []))
printStmt (IIf resultVar resultType condExpr thenStmts thenExpr elseStmts elseExpr) =
  vsep
    [ resultDecl
    , block 4 [idoc|if(#{printExpr condExpr})|]
        (vsep (map printStmt thenStmts ++ [[idoc|#{pretty resultVar} = #{printExpr thenExpr};|]]))
    , block 4 "else"
        (vsep (map printStmt elseStmts ++ [[idoc|#{pretty resultVar} = #{printExpr elseExpr};|]]))
    ]
  where
    resultDecl = case resultType of
      Just t -> [idoc|#{renderIType t} #{pretty resultVar};|]
      Nothing -> [idoc|auto #{pretty resultVar};|]
printStmt (IIfNotNull resultVar resultType source unwrapVar unwrapType bodyStmts bodyExpr) =
  vsep
    [ srcDecl
    , resultDecl
    , block 4 [idoc|if(#{pretty srcVar}.has_value())|]
        ( vsep
            ( unwrapDecl
                : map printStmt bodyStmts
                ++ [[idoc|#{pretty resultVar} = #{printExpr bodyExpr};|]]
            )
        )
    ]
  where
    srcVar = unwrapVar <> "_src"
    srcDecl = [idoc|auto #{pretty srcVar} = #{printExpr source};|]
    resultDecl = case resultType of
      Just t -> [idoc|#{renderIType t} #{pretty resultVar} = std::nullopt;|]
      Nothing -> [idoc|auto #{pretty resultVar} = std::nullopt;|]
    unwrapDecl = case unwrapType of
      Just t -> [idoc|#{renderIType t} #{pretty unwrapVar} = #{pretty srcVar}.value();|]
      Nothing -> [idoc|auto #{pretty unwrapVar} = #{pretty srcVar}.value();|]
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

    -- The dispatch case is just a direct return; per-label logging is
    -- injected at the manifold definition (see lcMakeFunction in
    -- executable/CppTranslator.hs) so callers via std::bind / direct symbol
    -- reference all see the wrapped behavior. The label field on the
    -- DispatchEntry is unused here.
    makeCase :: MDoc -> DispatchEntry -> MDoc
    makeCase suffix (DispatchEntry i n _) =
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
    (DF.embededFileText (DF.poolTemplate "cpp"))
    "// <<<BREAK>>>"
    [ vsep (map pretty (ipSources prog))
    , vsep (schemaTableDecl : serialization)
    , vsep signatures
    , vsep (map pretty (ipManifolds prog))
    , printDispatch (ipLocalDispatch prog) (ipRemoteDispatch prog)
    ]
  where
    schemas = ipSchemaTable prog
    n = length schemas
    schemaTableDecl
      | n == 0 = "void _init_schemas() {}"
      | otherwise = vsep
          [ "static Schema* mlc_schema_table[" <> pretty n <> "];"
          , "void _init_schemas() {"
          , indent 4 $ vsep
              [ "static const char* _schema_strs[] = {"
              , indent 4 $ vsep [dquotes (pretty s) <> "," | s <- schemas]
              , "};"
              , "for (int i = 0; i < " <> pretty n <> "; i++)"
              , indent 4 "mlc_schema_table[i] = parse_schema_cpp(_schema_strs[i]);"
              ]
          , "}"
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
--
-- Writes each field directly via per-field toAnything calls instead of
-- wrapping the whole record in a `std::make_tuple` and re-dispatching to
-- the tuple overload. The tuple form needed to copy/move every field
-- into the temporary tuple, which fails to compile when a field is
-- move-only (e.g. `std::optional<std::unique_ptr<T>>` -- the C++
-- encoding of a recursive `?T`). The per-field form mirrors the
-- deserializer and avoids that constraint.
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
#{block 4 "" (vsep (zipWith assignField [0 ..] (map fst fields)))}
    return dest;
}
|]
  where
    assignField :: Int -> MDoc -> MDoc
    assignField idx key =
      [idoc|toAnything((char*)dest + schema->offsets[#{pretty idx}], cursor, schema->parameters[#{pretty idx}], obj.#{key});|]

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
      [idoc|#{rtype} fromAnything(const Schema* schema, const void * anything, #{rtype}* dummy = nullptr, const void* base_ptr = nullptr)|]
    body =
      vsep $
        [[idoc|#{rtype} obj;|]]
          <> zipWith assignFields [0 ..] fields
          <> ["return obj;"]

    assignFields :: Int -> (MDoc, MDoc) -> MDoc
    assignFields idx (keyName, keyType) =
      vsep
        [ [idoc|#{keyType}* elemental_dumby_#{keyName} = nullptr;|]
        , [idoc|obj.#{keyName} = fromAnything(schema->parameters[#{pretty idx}], (char*)anything + schema->offsets[#{pretty idx}], elemental_dumby_#{keyName}, base_ptr);|]
        ]

    headerGetSize = [idoc|size_t get_shm_size(const Schema* schema, const #{rtype}& data)|]
    bodyGetSize =
      vsep $
        ["size_t size = 0;"]
          <> [getSize idx key | (idx, (key, _)) <- zip [0 ..] fields]
          <> ["return size;"]

    getSize :: Int -> MDoc -> MDoc
    getSize idx key = [idoc|size += get_shm_size(schema->parameters[#{pretty idx}], data.#{key});|]
