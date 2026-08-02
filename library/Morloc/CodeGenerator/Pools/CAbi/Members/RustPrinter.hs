{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : RustPrinter
Description : Pretty-print the imperative IR as Rust source code
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Converts 'IStmt' and 'IExpr' IR nodes into Rust source text for a CAbi Rust
pool member. Marshalling routes through the prebuilt @rustmorloc@ rlib
(@put_value@/@get_value@); dispatch wraps each manifold in the panic-safe
@dispatch_guard@. Type strings are pre-rendered by the translator ('rustTypeOf'
via 'toIType'), so this printer only unwraps the 'ITyNamed' carrier.
-}
module Morloc.CodeGenerator.Pools.CAbi.Members.RustPrinter
  ( printExpr
  , printStmt
  , printDispatch
  , printProgram
  , rustEscape
  , rustFieldIdent
  , rustRecordFields
  , stripTypeParams
  , printRustStruct
  , printRecordImpls
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Morloc.CodeGenerator.Grammars.Common (DispatchEntry (..), manNamer)
import Morloc.CodeGenerator.Grammars.Translator.Imperative
import Morloc.CodeGenerator.Namespace (MDoc, RealLit (..), Key (..))
import Morloc.Data.Doc
import Morloc.DataFiles as DF

-- | Render a pre-rendered IType to a Rust type string. The translator wraps
-- every rendered type in @ITyNamed s []@ (via 'toIType'); the structural
-- constructors are handled defensively but are not normally produced.
rustType :: IType -> MDoc
rustType (ITyNamed name []) = pretty name
rustType (ITyNamed name ps) = pretty name <> "<" <> hcat (punctuate ", " (map rustType ps)) <> ">"
rustType (ITyPrim t) = pretty t
rustType (ITyList t) = "Vec<" <> rustType t <> ">"
rustType (ITyTuple ts) = tupled (map rustType ts)
rustType (ITyOptional t) = "Option<" <> rustType t <> ">"
rustType (ITyRecord name _ _) = pretty name
rustType ITyUnit = "()"
rustType ITySerial = "*const u8"
rustType ITyUnknown = "_"
rustType t = error $ "RustPrinter: cannot render type " <> show t

printExpr :: IExpr -> MDoc
printExpr (IVar v) = pretty v
printExpr (IBoolLit True) = "true"
printExpr (IBoolLit False) = "false"
-- A bare `None` is ambiguous when passed to a generic function; the qualified
-- path `<Option<T>>::None` pins the type without needing to extract the inner.
printExpr (INullLit (Just t)) = "<" <> rustType t <> ">::None"
printExpr (INullLit Nothing) = "None"
printExpr (IIntLit Nothing i) = viaShow i
printExpr (IIntLit (Just t) i) = parens (viaShow i <+> "as" <+> pretty t)
printExpr (IRealLit Nothing r) = renderRealLit r
printExpr (IRealLit (Just t) r) = parens (renderRealLit r <+> "as" <+> pretty t)
printExpr (IStrLit _ s) = "String::from(" <> dquotes (pretty (rustEscape s)) <> ")"
printExpr (IListLit es) = "vec![" <> hcat (punctuate ", " (map printExpr es)) <> "]"
printExpr (ITupleLit es) = tupled (map printExpr es)
-- A record literal is always the RHS of an `IAssign` (its struct name lives on
-- the assignment's type); `printStmt` handles it there. Reaching this clause
-- means an IRecordLit escaped that context, which the lowering never produces.
printExpr (IRecordLit _ _ _) = error "RustPrinter: record literal outside an assignment (lowering invariant broken)"
printExpr (IAccess e (IIdx i)) = printExpr e <> "." <> pretty i
printExpr (IAccess e (IField f)) = printExpr e <> "." <> pretty f
printExpr (IAccess e (IKey _)) = printExpr e
printExpr (ISerCall sid e) = "rustmorloc::put_value(&(" <> printExpr e <> "), schema(" <> pretty sid <> "))"
printExpr (IDesCall sid (Just t) e) =
  "rustmorloc::get_value::<" <> rustType t <> ">(" <> printExpr e <> ", schema(" <> pretty sid <> "))"
printExpr (IDesCall sid Nothing e) =
  "rustmorloc::get_value(" <> printExpr e <> ", schema(" <> pretty sid <> "))"
printExpr (IPack packer e) = pretty packer <> parens (printExpr e)
printExpr (ICall f Nothing argGroups) =
  pretty f <> hcat (map (tupled . map printExpr) argGroups)
printExpr (ICall f (Just ts) argGroups) =
  pretty f <> "::<" <> hcat (punctuate ", " (map rustType ts)) <> ">"
    <> hcat (map (tupled . map printExpr) argGroups)
printExpr (ILambda args body) =
  "move |" <> hcat (punctuate ", " (map pretty args)) <> "| " <> printExpr body
printExpr (IRawExpr d) = pretty d
printExpr (IDoBlock e) = "move || { " <> printExpr e <> " }"
printExpr (IEval e) = parens (printExpr e) <> "()"
printExpr (IIntrinsicShow sid e) =
  "rustmorloc::show(&(" <> printExpr e <> "), schema(" <> pretty sid <> "))"
printExpr (IIntrinsicRead sid (Just t) e) =
  "rustmorloc::read::<" <> rustType t <> ">(&(" <> printExpr e <> "), schema(" <> pretty sid <> "))"
printExpr (IIntrinsicRead sid Nothing e) =
  "rustmorloc::read(&(" <> printExpr e <> "), schema(" <> pretty sid <> "))"
printExpr (IIntrinsicThrow msg) = "rustmorloc::morloc_throw(" <> printExpr msg <> ")"
printExpr (IIntrinsicCatch fallible fallback) =
  "rustmorloc::mlc_catch(" <> printExpr fallible <> ", " <> printExpr fallback <> ")"
-- File / stream / IO intrinsics. Each mirrors the C++ `_mlc_*` helper
-- (CppPrinter.hs) but calls the corresponding thin `rustmorloc` shim. A value
-- argument is borrowed (`&(..)`, the ToVoidstar shape); a handle is a bare
-- `u64` value; a typed-result shim gets the `::<T>` turbofish so its return
-- type resolves.
printExpr (IIntrinsicHash sid e) =
  "rustmorloc::hash(" <> refExpr e <> ", " <> schemaRef sid <> ")"
printExpr (IIntrinsicSave fmt sid level e path)
  | fmt == "json" = saveCall "save_json"
  | fmt == "voidstar" = saveCall "save_voidstar"
  | otherwise = saveCall "save"
  where
    saveCall f =
      "rustmorloc::" <> f <> "(" <> refExpr e <> ", " <> schemaRef sid
        <> ", " <> printExpr level <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicLoad sid mt path) =
  "rustmorloc::load" <> turbofish mt <> "(" <> schemaRef sid <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicOpen kind path) =
  "rustmorloc::open(" <> refExpr path <> ", " <> pretty kind <> ")"
printExpr (IIntrinsicClose h) = "rustmorloc::close(" <> printExpr h <> ")"
printExpr (IIntrinsicUnlinkTemp path) = "rustmorloc::unlink_tmp(" <> refExpr path <> ")"
printExpr (IIntrinsicFSchema path) = "rustmorloc::fschema(" <> refExpr path <> ")"
printExpr (IIntrinsicFLength h) = "rustmorloc::ifile_length(" <> printExpr h <> ")"
printExpr (IIntrinsicNext sid mt h) =
  "rustmorloc::next" <> turbofish mt <> "(" <> schemaRef sid <> ", " <> printExpr h <> ")"
printExpr (IIntrinsicStreamLayout sid mt h) =
  "rustmorloc::stream_layout" <> turbofish mt <> "(" <> schemaRef sid <> ", " <> printExpr h <> ")"
printExpr (IIntrinsicStream h) = "rustmorloc::stream(" <> printExpr h <> ")"
printExpr (IIntrinsicOpenOStream sid path) =
  "rustmorloc::open_ostream(" <> schemaRef sid <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicOpenIStream sid path) =
  "rustmorloc::open_istream(" <> schemaRef sid <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicWrite sid level value handle) =
  "rustmorloc::write(" <> schemaRef sid <> ", " <> printExpr level
    <> ", " <> refExpr value <> ", " <> printExpr handle <> ")"
printExpr (IIntrinsicAppend sid path) =
  "rustmorloc::append(" <> schemaRef sid <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicConcat paths dest) =
  "rustmorloc::concat(" <> refExpr paths <> ", " <> refExpr dest <> ")"
printExpr (IIntrinsicFlush h) = "rustmorloc::flush(" <> printExpr h <> ")"
printExpr IIntrinsicTell = "rustmorloc::tell()"
printExpr IIntrinsicTmpfile = "rustmorloc::tmpfile()"
printExpr (IIntrinsicStdin sid) = "rustmorloc::open_stdin(" <> schemaRef sid <> ")"
printExpr (IIntrinsicStdout sid) = "rustmorloc::open_stdout(" <> schemaRef sid <> ")"
printExpr (IIntrinsicStderr sid) = "rustmorloc::open_stderr(" <> schemaRef sid <> ")"
printExpr (IIntrinsicIFileWalk sid mt pathExpr h runtimeArgs) =
  "rustmorloc::ifile_walk" <> turbofish mt <> "(" <> schemaRef sid <> ", " <> printExpr h
    <> ", " <> refExpr pathExpr <> ", &[" <> argList <> "])"
  where
    argList = hcat (punctuate ", " (map printExpr runtimeArgs))
printExpr _ = error "RustPrinter: this intrinsic/expression is unsupported in Rust v1"

-- | Reference into the pool's schema table: `schema(<id>)` returns a
-- `&'static Schema`, which coerces to the `&Schema` every shim expects.
schemaRef :: Int -> MDoc
schemaRef sid = "schema(" <> pretty sid <> ")"

-- | Borrow an expression (`&(e)`), the ToVoidstar/`&str` shape the value-in and
-- path shims expect.
refExpr :: IExpr -> MDoc
refExpr e = "&" <> parens (printExpr e)

-- | Optional turbofish for a typed-result intrinsic: `::<T>` when the result
-- type is known (always, for Rust), empty otherwise. Lets a generic shim
-- (`load`/`next`/`stream_layout`/`ifile_walk`) resolve its return type.
turbofish :: Maybe IType -> MDoc
turbofish = maybe "" (\t -> "::<" <> rustType t <> ">")

-- Rust non-finite float literals.
renderRealLit :: RealLit -> MDoc
renderRealLit (RealFinite r) = viaShow r
renderRealLit RealPosInf = "f64::INFINITY"
renderRealLit RealNegInf = "f64::NEG_INFINITY"
renderRealLit RealNaN    = "f64::NAN"

-- | Escape a string for a Rust double-quoted literal. Handles the characters
-- morloc Str literals can carry (backslash, quote, control bytes, interior NUL).
-- Deliberately bespoke rather than the shared 'Morloc.Data.Doc.escapeStringLit'
-- (which CppPrinter reuses): that helper emits C-style octal escapes (@\0@ ->
-- @\000@, ESC -> @\033@), which Rust string literals do not accept.
rustEscape :: T.Text -> T.Text
rustEscape = T.concatMap esc
  where
    esc '\\' = "\\\\"
    esc '"'  = "\\\""
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc '\0' = "\\0"
    esc c    = T.singleton c

-- | Render a Rust named-field list `f0: v0, f1: v1` (no braces) from
-- (field-name, rendered-value) pairs, escaping each name to a valid identifier.
-- The single source for the struct-literal field syntax used by the printer,
-- 'lcRecordConstructor', and the pattern setter.
rustRecordFields :: [(Key, MDoc)] -> MDoc
rustRecordFields kvs =
  hcat (punctuate ", " [rustFieldIdent k <> ":" <+> v | (k, v) <- kvs])

-- | A Rust struct-literal field block `{ f0: v0, f1: v1 }` from record entries.
recordLit :: [(Key, IExpr)] -> MDoc
recordLit entries = "{" <+> rustRecordFields [(k, printExpr e) | (k, e) <- entries] <+> "}"

-- | Drop a rendered type's `<..>` generic-parameter suffix, leaving the bare
-- name. A Rust struct literal takes no type arguments (they are inferred from
-- the field values), so a generic (custom-packer) record still constructs by
-- its bare name.
stripTypeParams :: T.Text -> T.Text
stripTypeParams = T.takeWhile (/= '<')

-- | The bare struct name for a Rust struct-literal constructor.
ctorName :: IType -> MDoc
ctorName (ITyNamed name _) = pretty (stripTypeParams name)
ctorName t = rustType t

-- | Escape a morloc record field name into a valid Rust struct field
-- identifier. A temporary bridge until morloc gains language-specific field
-- aliases (functions/types already alias the foreign name; fields do not yet).
rustFieldIdent :: Key -> MDoc
rustFieldIdent k
  | t `elem` unrawable =
      error $
        "Rust: record field `" <> T.unpack t <> "` is a reserved word that cannot be a raw \
        \identifier. Rename the field (a future field-alias feature will lift this)."
  | t `elem` rustKeywords = "r#" <> pretty t
  | otherwise = pretty t
  where
    t = render (pretty k)
    unrawable = ["self", "super", "crate", "Self"]
    rustKeywords =
      [ "as", "break", "const", "continue", "else", "enum", "extern", "false", "fn", "for"
      , "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref"
      , "return", "static", "struct", "trait", "true", "type", "unsafe", "use", "where"
      , "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final"
      , "macro", "override", "priv", "typeof", "unsized", "virtual", "yield", "try", "gen"
      ]

printStmt :: IStmt -> MDoc
-- A record reconstructed field-by-field (a custom-packer field crossing a
-- boundary) arrives as `IAssign v (Just t) (IRecordLit _ _ entries)`. Rust
-- needs a NAMED struct literal, unlike C++'s positional aggregate init: the
-- struct name is the assignment's already-resolved type `t` (the recmap lookup
-- happened at lowering) and the field names are the entry keys. This mirrors
-- the in-pool `lcRecordConstructor` output. Nested non-serializable records are
-- each hoisted to their own IAssign, so an IRecordLit's field values are never
-- themselves bare record literals.
printStmt (IAssign v (Just t) (IRecordLit _ _ entries)) =
  "let" <+> pretty v <> ":" <+> rustType t <+> "=" <+> ctorName t <+> recordLit entries <> ";"
printStmt (IAssign v Nothing e) = "let" <+> pretty v <+> "=" <+> printExpr e <> ";"
printStmt (IAssign v (Just t) e) = "let" <+> pretty v <> ":" <+> rustType t <+> "=" <+> printExpr e <> ";"
printStmt (IMapList resultVar resultType iterVar collection bodyStmts yieldExpr) =
  vsep
    [ resultDecl
    , "for" <+> pretty iterVar <+> "in" <+> printExpr collection <+> "{"
    , indent 4 $ vsep
        ( map printStmt bodyStmts
            ++ [pretty resultVar <> ".push(" <> printExpr yieldExpr <> ");"]
        )
    , "}"
    ]
  where
    resultDecl = case resultType of
      Just t -> "let mut" <+> pretty resultVar <> ":" <+> rustType t <+> "= Vec::new();"
      Nothing -> "let mut" <+> pretty resultVar <+> "= Vec::new();"
printStmt (IIf resultVar resultType condExpr thenStmts thenExpr elseStmts elseExpr) =
  vsep
    [ letHead <+> "if" <+> printExpr condExpr <+> "{"
    , indent 4 $ vsep (map printStmt thenStmts ++ [printExpr thenExpr])
    , "} else {"
    , indent 4 $ vsep (map printStmt elseStmts ++ [printExpr elseExpr])
    , "};"
    ]
  where
    letHead = case resultType of
      Just t -> "let" <+> pretty resultVar <> ":" <+> rustType t <+> "="
      Nothing -> "let" <+> pretty resultVar <+> "="
printStmt (IIfNotNull resultVar resultType source unwrapVar _unwrapType bodyStmts bodyExpr) =
  vsep
    [ letHead <+> "match" <+> printExpr source <+> "{"
    , indent 4 $ vsep
        [ "Some(" <> pretty unwrapVar <> ") => {"
        , indent 4 $ vsep (map printStmt bodyStmts ++ ["Some(" <> printExpr bodyExpr <> ")"])
        , "},"
        , "None => None,"
        ]
    , "};"
    ]
  where
    letHead = case resultType of
      Just t -> "let" <+> pretty resultVar <> ":" <+> rustType t <+> "="
      Nothing -> "let" <+> pretty resultVar <+> "="
printStmt (IReturn e) = "return" <+> printExpr e <> ";"
printStmt (IExprStmt e) = printExpr e <> ";"
printStmt (IFunDef _ _ _ _) = error "RustPrinter: IFunDef unsupported"

-- | Render the two Rust dispatch functions. Each flushes the deferred SHM
-- tracker at entry, then runs the manifold match under the panic->fail-packet
-- guard. Manifold calls and @fail_packet@ are @unsafe@, so the match body sits
-- in one @unsafe@ block.
printDispatch :: [DispatchEntry] -> [DispatchEntry] -> [Int] -> MDoc
printDispatch locals remotes closureMids =
  vsep [dispatchFn "local_dispatch" "local" (localCases ++ closureCases), "", dispatchFn "remote_dispatch" "remote" remoteCases]
  where
    localCases = map (makeCase "") locals
    remoteCases = map (makeCase "_remote") remotes
    -- A crossing closure is applied in its home pool: a foreign pool's reflected
    -- proxy calls back on the closure's manifold id, routed to its serial
    -- dispatch wrapper (which deserializes captured ++ bound and calls the body).
    closureCases =
      [ pretty i <+> "=>" <+> "mlc_closure_dispatch_" <> pretty i <> "(args, nargs)," | i <- closureMids ]

    makeCase :: MDoc -> DispatchEntry -> MDoc
    makeCase suffix (DispatchEntry i n _) =
      pretty i <+> "=>" <+> manNamer i <> suffix
        <> tupled ["a(" <> pretty j <> ")" | j <- take n ([0 ..] :: [Int])] <> ","

    dispatchFn :: MDoc -> MDoc -> [MDoc] -> MDoc
    dispatchFn name kind cases =
      vsep
        [ "unsafe extern \"C\" fn" <+> name
            <> "(mid: u32, args: *const *const u8, nargs: usize, _ctx: *mut c_void) -> *mut u8 {"
        , indent 4 $ vsep
            [ "rustmorloc::dispatch_flush();"
            , "let args = std::panic::AssertUnwindSafe(args);"
            , "rustmorloc::dispatch_guard(move || {"
            , indent 4 $ vsep
                [ "let args = *args;"
                , "let a = |i: usize| -> *const u8 { if i < nargs { unsafe { *args.add(i) } } else { std::ptr::null() } };"
                , "unsafe { match mid {"
                , indent 4 $ vsep
                    ( cases
                        ++ ["_ => rustmorloc::fail_packet(&format!(\"Invalid " <> kind <> " manifold id: {mid}\")),"]
                    )
                , "} }"
                ]
            , "})"
            ]
        , "}"
        ]

-- | Assemble the complete Rust pool file from an IProgram and the marshalling
-- section (per-record impls; empty in v1). Splices five sections into the
-- embedded @data/lang/rust/pool.rs@ template at its @// <<<BREAK>>>@ markers.
printProgram :: [MDoc] -> [MDoc] -> [MDoc] -> IProgram -> MDoc
printProgram serialization signatures _extra prog =
  format
    (DF.embededFileText (DF.poolTemplate "rust"))
    "// <<<BREAK>>>"
    [ vsep (map pretty (ipSources prog))
    , vsep (schemaSection : serialization)
    , vsep signatures
    , vsep (map pretty (ipManifolds prog))
    , printDispatch (ipLocalDispatch prog) (ipRemoteDispatch prog) (Map.keys (ipClosureTable prog))
    ]
  where
    schemas = ipSchemaTable prog
    schemaSection =
      vsep
        [ "static SCHEMA_STRS: &[&str] = &["
        , indent 4 $ vsep [dquotes (pretty (rustEscape s)) <> "," | s <- schemas]
        , "];"
        , "static SCHEMA_TABLE: OnceLock<Vec<Schema>> = OnceLock::new();"
        , "fn init_schemas() {"
        , indent 4 $ vsep
            [ "let table: Vec<Schema> = SCHEMA_STRS.iter()"
            , indent 4 ".map(|s| parse_schema(s).expect(\"morloc: invalid embedded schema\")).collect();"
            , "let _ = SCHEMA_TABLE.set(table);"
            ]
        , "}"
        , "#[inline]"
        , "fn schema(id: usize) -> &'static Schema {"
        , indent 4 "&SCHEMA_TABLE.get().expect(\"schemas not initialized\")[id]"
        , "}"
        ]

-- | A Rust generic parameter list `<T1, T2>` (empty when there are none).
paramList :: [MDoc] -> MDoc
paramList [] = ""
paramList ps = "<" <> hcat (punctuate ", " ps) <> ">"

-- | Emit a Rust struct definition (for autogenerated @= "struct"@ records).
-- Fields are (escaped-name, rendered-type). @params@ are the generic type
-- parameters (non-empty when a field's type differs between the native and wire
-- forms of a custom-packer record, e.g. `struct S<T1> { w: T1, .. }`).
printRustStruct :: MDoc -> [MDoc] -> [(MDoc, MDoc)] -> MDoc
printRustStruct name params fields =
  vsep
    -- Clone is required by the ownership model: a struct value used at more than
    -- one point is cloned at its by-value uses. All field types are Clone-able
    -- morloc types (scalars, Vec, String, Option, tuples, nested structs, Box).
    [ "#[derive(Clone)]"
    , "struct" <+> name <> paramList params <+> "{"
    , indent 4 (vsep [f <> ":" <+> t <> "," | (f, t) <- fields])
    , "}"
    ]

-- | Emit @impl ToVoidstar/FromVoidstar@ for a record, marshalling each field in
-- place at its schema offset (mirrors the hand-written LL template). Fields are
-- (escaped-name, rendered-type, is-variable-width). @params@ are the generic
-- type parameters; each is bounded by the marshalling trait the impl needs
-- (`impl<T1: ToVoidstar> ToVoidstar for S<T1>`). Recur scope is entered so a
-- back-reference resolves at any depth; fully fixed-width records short-circuit
-- @shm_size@ to @schema.width@.
printRecordImpls :: MDoc -> [MDoc] -> [(MDoc, MDoc, Bool)] -> MDoc
printRecordImpls name params fields = vsep [toImpl, "", fromImpl]
  where
    idx = zip [0 :: Int ..] fields
    allFixed = all (\(_, _, v) -> not v) fields
    ty = name <> paramList params
    -- Bound every generic param by `bound` for the impl's `impl<..>` header.
    boundedParams bound = paramList [p <> ":" <+> bound | p <- params]

    toImpl =
      vsep
        [ "impl" <> boundedParams "ToVoidstar" <+> "ToVoidstar for" <+> ty <+> "{"
        , indent 4 $ vsep
            [ "fn shm_size(&self, schema: &Schema) -> usize {"
            , indent 4 shmBody
            , "}"
            , "unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &Schema) {"
            , indent 4 writeBody
            , "}"
            ]
        , "}"
        ]
    shmBody
      | allFixed = vsep ["let schema = resolve_recur(schema);", "schema.width"]
      | otherwise =
          vsep $
            [ "let schema = resolve_recur(schema);"
            , "let _g = RecurScope::enter(schema);"
            , "let mut total = schema.width;"
            ]
              ++ concat
                [ [ "let fs" <> pretty i <+> "= resolve_recur(&schema.parameters[" <> pretty i <> "]);"
                  , "let e" <> pretty i <+> "= self." <> f <> ".shm_size(fs" <> pretty i <> ");"
                  , "if e" <> pretty i <+> "> fs" <> pretty i <> ".width { total += e" <> pretty i <+> "- fs" <> pretty i <> ".width; }"
                  ]
                | (i, (f, _, _)) <- idx
                ]
              ++ ["total"]
    writeBody =
      vsep $
        [ "let schema = resolve_recur(schema);"
        , "let _g = RecurScope::enter(schema);"
        ]
          ++ [ "self." <> f <> ".write(dest.add(schema.offsets[" <> pretty i <> "]), cursor, resolve_recur(&schema.parameters[" <> pretty i <> "]));"
             | (i, (f, _, _)) <- idx
             ]

    fromImpl =
      vsep
        [ "impl" <> boundedParams "FromVoidstar" <+> "FromVoidstar for" <+> ty <+> "{"
        , indent 4 $ vsep
            [ "unsafe fn read(schema: &Schema, data: *const u8, base: *const u8) -> Self {"
            , indent 4 $ vsep
                [ "let schema = resolve_recur(schema);"
                , "let _g = RecurScope::enter(schema);"
                , name <+> "{"
                , indent 4 $ vsep
                    [ f <> ": <" <> t <> " as FromVoidstar>::read(resolve_recur(&schema.parameters[" <> pretty i <> "]), data.add(schema.offsets[" <> pretty i <> "]), base),"
                    | (i, (f, t, _)) <- idx
                    ]
                , "}"
                ]
            , "}"
            ]
        , "}"
        ]
