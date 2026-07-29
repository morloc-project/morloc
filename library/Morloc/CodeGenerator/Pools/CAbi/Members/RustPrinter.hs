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
  , printRustStruct
  , printRecordImpls
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Morloc.CodeGenerator.Grammars.Common (DispatchEntry (..), manNamer)
import Morloc.CodeGenerator.Grammars.Translator.Imperative
import Morloc.CodeGenerator.Namespace (MDoc, RealLit (..))
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
printExpr (INullLit _) = "None"
printExpr (IIntLit Nothing i) = viaShow i
printExpr (IIntLit (Just t) i) = parens (viaShow i <+> "as" <+> pretty t)
printExpr (IRealLit Nothing r) = renderRealLit r
printExpr (IRealLit (Just t) r) = parens (renderRealLit r <+> "as" <+> pretty t)
printExpr (IStrLit _ s) = "String::from(" <> dquotes (pretty (rustEscape s)) <> ")"
printExpr (IListLit es) = "vec![" <> hcat (punctuate ", " (map printExpr es)) <> "]"
printExpr (ITupleLit es) = tupled (map printExpr es)
printExpr (IRecordLit _ _ _) = error "RustPrinter: record literals unsupported in Rust v1"
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
printExpr (IIntrinsicThrow msg) = "rustmorloc::morloc_throw(" <> printExpr msg <> ")"
printExpr (IIntrinsicCatch fallible fallback) =
  "rustmorloc::mlc_catch(" <> printExpr fallible <> ", " <> printExpr fallback <> ")"
printExpr _ = error "RustPrinter: this intrinsic/expression is unsupported in Rust v1"

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

printStmt :: IStmt -> MDoc
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
printDispatch locals remotes _closureMids =
  vsep [dispatchFn "local_dispatch" "local" localCases, "", dispatchFn "remote_dispatch" "remote" remoteCases]
  where
    localCases = map (makeCase "") locals
    remoteCases = map (makeCase "_remote") remotes

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

-- | Emit a Rust struct definition (for autogenerated @= "struct"@ records).
-- Fields are (escaped-name, rendered-type).
printRustStruct :: MDoc -> [(MDoc, MDoc)] -> MDoc
printRustStruct name fields =
  vsep
    [ "struct" <+> name <+> "{"
    , indent 4 (vsep [f <> ":" <+> t <> "," | (f, t) <- fields])
    , "}"
    ]

-- | Emit @impl ToVoidstar/FromVoidstar@ for a record, marshalling each field in
-- place at its schema offset (mirrors the hand-written LL template). Fields are
-- (escaped-name, rendered-type, is-variable-width). Recur scope is entered so a
-- back-reference resolves at any depth; fully fixed-width records short-circuit
-- @shm_size@ to @schema.width@.
printRecordImpls :: MDoc -> [(MDoc, MDoc, Bool)] -> MDoc
printRecordImpls name fields = vsep [toImpl, "", fromImpl]
  where
    idx = zip [0 :: Int ..] fields
    allFixed = all (\(_, _, v) -> not v) fields

    toImpl =
      vsep
        [ "impl ToVoidstar for" <+> name <+> "{"
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
        [ "impl FromVoidstar for" <+> name <+> "{"
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
