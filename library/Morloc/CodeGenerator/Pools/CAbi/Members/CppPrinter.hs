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
module Morloc.CodeGenerator.Pools.CAbi.Members.CppPrinter
  ( printExpr
  , printStmt
  , printStmts

    -- * Pool-level rendering
  , printDispatch
  , printProgram

    -- * Struct/serializer rendering
  , printStructTypedef
  , printMarshalDecls
  , armName
  , printCppEnumDecl
  , printCppVariantDecl
  , printCppVariantArms
  , printCppVariantSerializers
  , printSerializer
  , printDeserializer
  , printTemplateHeader
  , printRecordTemplate
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
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
printExpr IUnitLit = "mlc::Unit{}"
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
printExpr (IDesCall sid (Just rawtype) _ e) = [idoc|_get_value<#{renderIType rawtype}>(#{printExpr e}, mlc_schema_table[#{pretty sid}])|]
printExpr (IDesCall sid Nothing _ e) = [idoc|_get_value(#{printExpr e}, mlc_schema_table[#{pretty sid}])|]
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
printExpr (IIntrinsicOpen kind path) =
  [idoc|_mlc_open(#{printExpr path}, #{pretty kind})|]
printExpr (IIntrinsicClose h) =
  [idoc|_mlc_close(#{printExpr h})|]
printExpr (IIntrinsicUnlinkTemp path) =
  [idoc|_mlc_unlink_tmp(#{printExpr path})|]
printExpr (IIntrinsicFSchema path) =
  [idoc|_mlc_fschema(#{printExpr path})|]
printExpr (IIntrinsicFLength h) =
  [idoc|_mlc_ifile_length(#{printExpr h})|]
printExpr (IIntrinsicIFileWalk sid (Just t) pathExpr h runtimeArgs) =
  let argList = "{" <> hcat (punctuate "," (map printExpr runtimeArgs)) <> "}"
   in [idoc|_mlc_ifile_walk<#{renderIType t}>(mlc_schema_table[#{pretty sid}], #{printExpr h}, #{printExpr pathExpr}, #{argList})|]
printExpr (IIntrinsicIFileWalk sid Nothing pathExpr h runtimeArgs) =
  let argList = "{" <> hcat (punctuate "," (map printExpr runtimeArgs)) <> "}"
   in [idoc|_mlc_ifile_walk(mlc_schema_table[#{pretty sid}], #{printExpr h}, #{printExpr pathExpr}, #{argList})|]
printExpr (IIntrinsicNext sid (Just t) h) =
  [idoc|_mlc_next<#{renderIType t}>(mlc_schema_table[#{pretty sid}], #{printExpr h})|]
printExpr (IIntrinsicNext sid Nothing h) =
  [idoc|_mlc_next(mlc_schema_table[#{pretty sid}], #{printExpr h})|]
printExpr (IIntrinsicStreamLayout sid (Just t) h) =
  [idoc|_mlc_stream_layout<#{renderIType t}>(mlc_schema_table[#{pretty sid}], #{printExpr h})|]
printExpr (IIntrinsicStreamLayout sid Nothing h) =
  [idoc|_mlc_stream_layout(mlc_schema_table[#{pretty sid}], #{printExpr h})|]
printExpr (IIntrinsicStream h) =
  [idoc|_mlc_stream(#{printExpr h})|]
printExpr (IIntrinsicOpenOStream sid path) =
  [idoc|_mlc_open_ostream(mlc_schema_table[#{pretty sid}], #{printExpr path})|]
printExpr (IIntrinsicOpenIStream sid path) =
  [idoc|_mlc_open_istream(mlc_schema_table[#{pretty sid}], #{printExpr path})|]
printExpr (IIntrinsicWrite sid level value handle) =
  [idoc|_mlc_write(mlc_schema_table[#{pretty sid}], #{printExpr level}, #{printExpr value}, #{printExpr handle})|]
printExpr (IIntrinsicAppend sid path) =
  [idoc|_mlc_append(mlc_schema_table[#{pretty sid}], #{printExpr path})|]
printExpr (IIntrinsicConcat paths dest) =
  [idoc|_mlc_concat(#{printExpr paths}, #{printExpr dest})|]
printExpr (IIntrinsicFlush h) =
  [idoc|_mlc_flush(#{printExpr h})|]
printExpr IIntrinsicTell =
  [idoc|_mlc_tell()|]
printExpr IIntrinsicTmpfile =
  [idoc|_mlc_tmpfile()|]
printExpr (IIntrinsicStdin sid) =
  [idoc|_mlc_open_stdin(mlc_schema_table[#{pretty sid}])|]
printExpr (IIntrinsicStdout sid) =
  [idoc|_mlc_open_stdout(mlc_schema_table[#{pretty sid}])|]
printExpr (IIntrinsicStderr sid) =
  [idoc|_mlc_open_stderr(mlc_schema_table[#{pretty sid}])|]
printExpr (IIntrinsicThrow _ msg) =
  [idoc|_mlc_throw(#{printExpr msg})|]

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
printDispatch :: [DispatchEntry] -> [DispatchEntry] -> [Int] -> MDoc
printDispatch locals remotes closureMids =
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
    -- Closure force-registration: each closure mid dispatches to its serial
    -- wrapper (deserialize -> call native closure body -> serialize) so a
    -- foreign pool can apply a boundary-crossing closure via foreign_call.
    localCases = map (makeCase "") locals ++ map makeClosureCase closureMids
    remoteCases = map (makeCase "_remote") remotes

    makeClosureCase :: Int -> MDoc
    makeClosureCase i =
      "case" <+> pretty i <> ":" <+> "return mlc_closure_dispatch_" <> pretty i <> "(args);"

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
printProgram :: [MDoc] -> [MDoc] -> [MDoc] -> IProgram -> MDoc
printProgram serialization signatures closureWrappers prog =
  format
    (DF.embededFileText (DF.poolTemplate "cpp"))
    "// <<<BREAK>>>"
    [ vsep (map pretty (ipSources prog))
    , vsep (schemaTableDecl : serialization)
    , vsep signatures
    , vsep (map pretty (ipManifolds prog) ++ closureWrappers)
    , printDispatch (ipLocalDispatch prog) (ipRemoteDispatch prog) (Map.keys (ipClosureTable prog))
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
-- | Emit a payload-bearing `data` type: one struct per arm, wrapped in a
-- struct holding a @std::variant@.
--
-- Each arm gets its OWN struct rather than sharing a tuple, because two arms
-- may carry identical field types and must still be distinguishable -- a
-- @std::variant<double, double>@ could not tell @Circle@ from @Radius@.
--
-- The wrapper is a struct rather than a bare alias so the type can be
-- forward-declared: a recursive arm holds the wrapper by value behind the
-- variant's @mlc::rec_ptr@, and an alias to @std::variant<...>@ cannot be
-- named before its alternatives are complete. The pointer is a
-- @rec_ptr@ rather than a bare @std::shared_ptr@ so that releasing a deep
-- chain of arms runs iteratively instead of one destructor frame per level.
-- | The marshalling node of a generated type: the @MlcNode@ specialization
-- with its three walk steps declared.
--
-- The whole class goes out before any step body, so a body that reaches
-- this type -- an arm holding it, a record field of it -- finds a complete
-- class to name. Bodies follow as out-of-line member definitions, which is
-- how two generated types can marshal each other in either order.
printMarshalDecls :: [MDoc] -> MDoc -> MDoc
printMarshalDecls params name =
  vsep
    [ specializationHeader params
    , "struct MlcNode<" <> name <> "> {"
    , indent 4 $ vsep
        [ "static void size_step(MlcSizeWalk& w, const Schema* schema, const"
            <+> name <> "& obj, size_t idx);"
        , "static void write_step(MlcWriteWalk& w, const Schema* schema, void* dest, const"
            <+> name <> "& obj, size_t idx);"
        , "static void read_step(MlcReadWalk& w, const Schema* schema, const void* data,"
            <+> name <> "* out, size_t idx);"
        ]
    , "};"
    ]

-- | @template<>@ for an explicit specialization, the parameter list for a
-- partial one.
specializationHeader :: [MDoc] -> MDoc
specializationHeader [] = "template<>"
specializationHeader params = printTemplateHeader params

-- | The header of an out-of-line member of a specialization: an explicit
-- specialization's members take none, a partial one's repeat its parameters.
memberHeader :: [MDoc] -> MDoc
memberHeader [] = ""
memberHeader params = printTemplateHeader params <> line

-- | The forward declarations and wrapper for a variant.
--
-- Emitted for EVERY variant before ANY arm body, because an arm may hold
-- another `data` type by value and would otherwise need that type's wrapper
-- to already exist. Splitting the two phases removes the ordering question
-- entirely, rather than answering it with a topological sort.
-- | Emit a C++ @enum class@ for an argument-free @data@ type.
--
-- A one-byte underlying type with explicit discriminants makes the native
-- value and the wire tag the same byte, so a constructor's declaration
-- ordinal needs no translation. Marshalling comes from the generic
-- one-byte path; only the declaration is emitted here.
printCppEnumDecl :: MDoc -> [Text] -> MDoc
printCppEnumDecl name ctors =
  vsep
    [ "enum class" <+> name <+> ": uint8_t {"
    , indent 4 (vsep [pretty c <+> "=" <+> pretty i <> "," | (i, c) <- zip [0 :: Int ..] ctors])
    , "};"
    ]

printCppVariantDecl :: MDoc -> [(Text, [MDoc])] -> MDoc
printCppVariantDecl name arms =
  vsep
    [ vsep ["struct" <+> armName name c <> ";" | (c, _) <- arms]
    , "struct" <+> name <+> "{"
    , indent 4 ("std::variant<"
                  <> hsep (punctuate "," ["mlc::rec_ptr<" <> armName name c <> ">" | (c, _) <- arms])
                  <> "> v;")
    , "};"
    ]

-- | The arm bodies, emitted after every wrapper exists.
printCppVariantArms :: MDoc -> [(Text, [MDoc])] -> MDoc
printCppVariantArms name arms = vsep [armStruct c ts | (c, ts) <- arms]
  where
    armStruct c ts =
      vsep
        [ "struct" <+> armName name c <+> "{"
        , indent 4 (vsep [t <+> "f" <> pretty i <> ";" | (i, t) <- zip [(0 :: Int) ..] ts])
        , "};"
        ]

-- | The struct that holds one arm's fields: the wrapper's name with
-- @_<Constructor>@ appended to its HEAD. For a plain name that is
-- @Shape_Circle@; for a template instantiation the suffix goes before the
-- argument list, so @MyBox<int>@ has the arm @MyBox_Full<int>@ -- a
-- template of its own, taking the wrapper's arguments. A user who maps a
-- parameterized @data@ writes the arms to that convention.
armName :: MDoc -> Text -> MDoc
armName n c =
  let (headT, args) = T.breakOn "<" (render n)
  in pretty headT <> "_" <> pretty c <> pretty args

-- | Emit the three walk steps of a variant wrapper.
--
-- The slot layout lives in the runtime; these only pick an arm. An arm with
-- fields hands its struct to the walk, which the arm's own node marshals.
printCppVariantSerializers :: MDoc -> [(Text, [MDoc])] -> MDoc
printCppVariantSerializers name arms =
  vsep [sizeFn, "", toFn, "", fromFn]
  where
    idxArms = zip [(0 :: Int) ..] arms

    armPayload i = "*std::get<" <> pretty i <> ">(obj.v)"

    sizeFn =
      vsep
        [ "void MlcNode<" <> name <> ">::size_step(MlcSizeWalk& w, const Schema* schema, const"
            <+> name <> "& obj, size_t) {"
        , indent 4 $ vsep
            [ "switch (obj.v.index()) {"
            , indent 4 $ vsep
                [ "case" <+> pretty i <> ":" <+>
                    (if null ts
                       then "w.total += schema->width; break;"
                       else "w.variant_payload(schema, schema->parameters["
                              <> pretty i <> "], " <> armPayload i <> "); break;")
                | (i, (_, ts)) <- idxArms ]
            , "}"
            ]
        , "}"
        ]

    toFn =
      vsep
        [ "void MlcNode<" <> name <> ">::write_step(MlcWriteWalk& w, const Schema* schema, void* dest, const"
            <+> name <> "& obj, size_t) {"
        , indent 4 $ vsep
            [ "switch (obj.v.index()) {"
            , indent 4 $ vsep
                [ "case" <+> pretty i <> ":" <+>
                    (if null ts
                       then "write_variant_nullary(dest," <+> pretty i <> "); break;"
                       else "w.variant_payload(dest, schema->parameters["
                              <> pretty i <> "]," <+> pretty i <> ", " <> armPayload i <> "); break;")
                | (i, (_, ts)) <- idxArms ]
            , "}"
            ]
        , "}"
        ]

    fromFn =
      vsep
        [ "void MlcNode<" <> name <> ">::read_step(MlcReadWalk& w, const Schema* schema, const void* data,"
            <+> name <> "* out, size_t) {"
        , indent 4 $ vsep
            [ "switch (read_variant_tag(data)) {"
            , indent 4 $ vsep
                [ "case" <+> pretty i <> ":" <+>
                    (if null ts
                       then "out->v = std::make_shared<" <> armName name c <> ">(); break;"
                       else "{ auto arm = std::make_shared<" <> armName name c
                              <> ">(); out->v = arm; w.variant_payload(schema->parameters["
                              <> pretty i <> "], data, arm.get()); break; }")
                | (i, (c, ts)) <- idxArms ]
            , indent 0 ("default: throw std::runtime_error(\"" <> name
                          <> ": no constructor for this tag\");")
            , "}"
            ]
        , "}"
        ]

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

-- | Render the write step of a struct: each field into its slot.
printSerializer ::
  [MDoc] -> -- template parameters
  MDoc -> -- type of thing being serialized
  [(MDoc, MDoc)] -> -- key and type for all fields
  MDoc
printSerializer params rtype fields =
  [idoc|
#{memberHeader params}void MlcNode<#{rtype}>::write_step(MlcWriteWalk& w, const Schema* schema, void* dest, const #{rtype}& obj, size_t)
{
#{block 4 "" (vsep (zipWith writeField [0 ..] (map fst fields)))}
}
|]
  where
    writeField :: Int -> MDoc -> MDoc
    writeField idx key =
      [idoc|w.child(schema->parameters[#{pretty idx}], (char*)dest + schema->offsets[#{pretty idx}], obj.#{key});|]

-- | Render the read and size steps of a struct.
printDeserializer ::
  [MDoc] -> -- template parameters
  MDoc -> -- type of thing being deserialized
  [(MDoc, MDoc)] -> -- key and type for all fields
  MDoc
printDeserializer params rtype fields =
  [idoc|
#{memberHeader params}void MlcNode<#{rtype}>::read_step(MlcReadWalk& w, const Schema* schema, const void* data, #{rtype}* out, size_t)
{
#{block 4 "" (vsep (zipWith readField [0 ..] (map fst fields)))}
}

#{memberHeader params}void MlcNode<#{rtype}>::size_step(MlcSizeWalk& w, const Schema* schema, const #{rtype}& obj, size_t)
{
    w.total += schema->width;
#{block 4 "" (vsep (zipWith sizeField [0 ..] (map fst fields)))}
}
|]
  where
    readField :: Int -> MDoc -> MDoc
    readField idx key =
      [idoc|w.child(schema->parameters[#{pretty idx}], (const char*)data + schema->offsets[#{pretty idx}], &out->#{key});|]

    -- A field's fixed width lies inside the record's, so only its tail counts.
    sizeField :: Int -> MDoc -> MDoc
    sizeField idx key =
      [idoc|w.child(schema->parameters[#{pretty idx}], obj.#{key}, true);|]
