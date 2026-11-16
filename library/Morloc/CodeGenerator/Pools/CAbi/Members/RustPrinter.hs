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
  , rustManifoldName
  , putValueFn
  , schemaRef
  , typeHead
  , printStmt
  , printDispatch
  , printProgram
  , rustEscape
  , rustFieldIdent
  , rustRecordFields
  , stripTypeParams
  , printRustStruct
  , printRustHandle
  , rustRecordValue
  , printRecordImpls
  , printRecordImplsWith
  , printRecordImplsWhere
  , printFieldTrait
  , printFieldTraitImpl
  , FieldTraitImpl (..)
  , RecordFieldTypes(..)
  , printRustEnum
  , printRustVariant
  , printVariantImpls
  , ArmWire (..)
  , armTypeName
  , paramList
  , printRustNode
  , printRustNodeImpls
  , printUserVariantImpls
  , UserField
  , userFieldRef
  , userFieldFrom
  , userFieldCopy
  , userFieldTake
  , userArmPattern
  , printEnumImpls
  , tupled1
  , ClosureMarshal (..)
  ) where

import Morloc.Data.Zip (indexed, zipSame)
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Printf (printf)
import Morloc.CodeGenerator.Grammars.Common (DispatchEntry (..))
import Morloc.CodeGenerator.Grammars.Translator.Imperative
import Morloc.CodeGenerator.Namespace (Intrinsic (..), MDoc, RealLit (..), Key (..), SaveFormat (..))
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
rustType (ITyOptional t) = "::std::option::Option<" <> rustType t <> ">"
rustType (ITyRecord name _ _) = pretty name
rustType ITyUnit = "()"
rustType ITySerial = "*const u8"
rustType ITyUnknown = "_"
rustType t = error $ "RustPrinter: cannot render type " <> show t

-- | A rendered type without its argument list: @MyBox<i64>@ gives @MyBox@,
-- a plain name gives itself. A pattern names an enum by its head alone,
-- because the qualified path a generic instantiation would need is not
-- stable Rust in pattern position, and the subject's type pins the
-- arguments anyway.
typeHead :: MDoc -> MDoc
typeHead t = pretty (fst (T.breakOn "<" (render t)))

printExpr :: IExpr -> MDoc
printExpr (IVar v) = pretty v
printExpr (IBoolLit True) = "true"
printExpr (IBoolLit False) = "false"
-- A bare `None` is ambiguous when passed to a generic function; the qualified
-- path `<Option<T>>::None` pins the type without needing to extract the inner.
printExpr (INullLit (Just t)) = "<" <> rustType t <> ">::None"
printExpr (INullLit Nothing) = "None"
printExpr IUnitLit = "()"
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
printExpr (ISerCall sid e) = putValueFn <> "(&(" <> printExpr e <> "), " <> schemaRef sid <> ")"
printExpr (IDesCall sid (Just t) _ e) =
  "::rustmorloc::get_value::<" <> rustType t <> ">(" <> printExpr e <> ", " <> schemaRef sid <> ")"
printExpr (IDesCall sid Nothing _ e) =
  "::rustmorloc::get_value(" <> printExpr e <> ", " <> schemaRef sid <> ")"
printExpr (IPack packer e) = pretty packer <> parens (printExpr e)
printExpr (ICall f Nothing argGroups) =
  pretty f <> hcat (map (tupled . map printExpr) argGroups)
printExpr (ICall f (Just ts) argGroups) =
  pretty f <> "::<" <> hcat (punctuate ", " (map rustType ts)) <> ">"
    <> hcat (map (tupled . map printExpr) argGroups)
printExpr (ILambda args body) =
  "move |" <> hcat (punctuate ", " (map pretty args)) <> "| " <> printExpr body
printExpr (IRawExpr d) = pretty d
printExpr (IEval e) = parens (printExpr e) <> "()"
printExpr (IIntrinsicShow sid e) =
  "::rustmorloc::show(&(" <> printExpr e <> "), " <> schemaRef sid <> ")"
printExpr (IIntrinsicRead sid (Just t) e) =
  "::rustmorloc::read::<" <> rustType t <> ">(&(" <> printExpr e <> "), " <> schemaRef sid <> ")"
printExpr (IIntrinsicRead sid Nothing e) =
  "::rustmorloc::read(&(" <> printExpr e <> "), " <> schemaRef sid <> ")"
printExpr (IIntrinsicUnpack sid (Just t) e) =
  "::rustmorloc::unpack::<" <> rustType t <> ">(&(" <> printExpr e <> "), " <> schemaRef sid <> ")"
printExpr (IIntrinsicUnpack sid Nothing e) =
  "::rustmorloc::unpack(&(" <> printExpr e <> "), " <> schemaRef sid <> ")"
-- A raise in a value position is typed as that value so the surrounding
-- expression keeps its type: `!` would leave the statements after a
-- throwing arm unreachable and give a serialized throw no `ToVoidstar`.
printExpr (IIntrinsicThrow (Just t) msg) =
  "::rustmorloc::morloc_throw_as::<" <> rustType t <> ">(" <> printExpr msg <> ")"
printExpr (IIntrinsicThrow Nothing msg) = "::rustmorloc::morloc_throw(" <> printExpr msg <> ")"
-- File / stream / IO intrinsics. Each mirrors the C++ `_mlc_*` helper
-- (CppPrinter.hs) but calls the corresponding thin `rustmorloc` shim. A value
-- argument is borrowed (`&(..)`, the ToVoidstar shape); a handle is a bare
-- `u64` value; a typed-result shim gets the `::<T>` turbofish so its return
-- type resolves.
printExpr (IIntrinsicHash sid e) =
  "::rustmorloc::hash(" <> refExpr e <> ", " <> schemaRef sid <> ")"
printExpr (IIntrinsicCompare intr sid a b) =
  "(::rustmorloc::compare(" <> refExpr a <> ", " <> refExpr b <> ", " <> schemaRef sid <> ")"
    <> (if intr == IntrEq then " == 0)" else " <= 0)")
printExpr (IIntrinsicSave fmt sid level e path) = case fmt of
  SaveJson -> saveCall "save_json"
  SaveVoidstar -> saveCall "save"
  where
    saveCall f =
      "::rustmorloc::" <> f <> "(" <> refExpr e <> ", " <> schemaRef sid
        <> ", " <> printExpr level <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicLoad sid mt path) =
  "::rustmorloc::load" <> turbofish mt <> "(" <> schemaRef sid <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicOpenIFile sid path) =
  "::rustmorloc::open_ifile(" <> schemaRef sid <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicClose h) = "::rustmorloc::close(" <> printExpr h <> ")"
printExpr (IIntrinsicUnlinkTemp path) = "::rustmorloc::unlink_tmp(" <> refExpr path <> ")"
printExpr (IIntrinsicFSchema path) = "::rustmorloc::fschema(" <> refExpr path <> ")"
printExpr (IIntrinsicFLength h) = "::rustmorloc::ifile_length(" <> printExpr h <> ")"
printExpr (IIntrinsicNext sid mt h) =
  "::rustmorloc::next" <> turbofish mt <> "(" <> schemaRef sid <> ", " <> printExpr h <> ")"
printExpr (IIntrinsicStreamLayout sid mt h) =
  "::rustmorloc::stream_layout" <> turbofish mt <> "(" <> schemaRef sid <> ", " <> printExpr h <> ")"
printExpr (IIntrinsicStream h) = "::rustmorloc::stream(" <> printExpr h <> ")"
printExpr (IIntrinsicOpenOStream sid path) =
  "::rustmorloc::open_ostream(" <> schemaRef sid <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicOpenIStream sid path) =
  "::rustmorloc::open_istream(" <> schemaRef sid <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicWrite sid level value handle) =
  "::rustmorloc::write(" <> schemaRef sid <> ", " <> printExpr level
    <> ", " <> refExpr value <> ", " <> printExpr handle <> ")"
printExpr (IIntrinsicAppend sid path) =
  "::rustmorloc::append(" <> schemaRef sid <> ", " <> refExpr path <> ")"
printExpr (IIntrinsicConcat paths dest) =
  "::rustmorloc::concat(" <> refExpr paths <> ", " <> refExpr dest <> ")"
printExpr (IIntrinsicFlush h) = "::rustmorloc::flush(" <> printExpr h <> ")"
printExpr IIntrinsicTell = "::rustmorloc::tell()"
printExpr IIntrinsicTmpfile = "::rustmorloc::tmpfile()"
printExpr (IIntrinsicChannel sid) =
  "::rustmorloc::open_channel(" <> schemaRef sid <> ")"
printExpr (IIntrinsicSpawn sid h f) =
  "::rustmorloc::spawn(&" <> printExpr f <> ", " <> printExpr h <> ", " <> schemaRef sid <> ")"
printExpr (IIntrinsicSettle h) =
  "::rustmorloc::settle(" <> printExpr h <> ")"
printExpr (IIntrinsicGather native sid mt f) =
  "::rustmorloc::" <> rustGatherFn native <> reduceTurbofish mt <> "(" <> schemaRef sid <> ", " <> printExpr f <> ")"
printExpr (IIntrinsicGatherPut sid h v) =
  "::rustmorloc::gather_put(" <> schemaRef sid <> ", " <> printExpr h <> ", " <> printExpr v <> ")"
printExpr (IIntrinsicFold True _ _ _ _ _ _ _) =
  error "morloc bug: a Rust fold was given the file route, which only Python and R folds take (FOLD-9)"
printExpr (IIntrinsicFold False (Just sid) mb ml ini step comb f) =
  "::rustmorloc::fold" <> foldTurbofish 3 mb ml <> "(" <> schemaRef sid <> ", "
    <> refExpr ini <> ", " <> printExpr step <> ", " <> printExpr comb <> ", " <> printExpr f <> ")"
printExpr (IIntrinsicFold False Nothing mb ml ini step comb f) =
  "::rustmorloc::fold_schemaless" <> foldTurbofish 3 mb ml <> "("
    <> refExpr ini <> ", " <> printExpr step <> ", " <> printExpr comb <> ", " <> printExpr f <> ")"
printExpr (IIntrinsicFoldPut (Just sid) mb ml step h v) =
  "::rustmorloc::fold_put" <> foldTurbofish 1 mb ml <> "(" <> schemaRef sid <> ", " <> printExpr step <> ", "
    <> printExpr h <> ", " <> refExpr v <> ")"
printExpr (IIntrinsicFoldPut Nothing mb ml step h v) =
  "::rustmorloc::fold_put_schemaless" <> foldTurbofish 1 mb ml <> "(" <> printExpr step <> ", "
    <> printExpr h <> ", " <> refExpr v <> ")"
printExpr (IIntrinsicReplay sid mt h f) =
  -- The element type is given, the function's closure type is left to
  -- inference.
  "::rustmorloc::replay" <> reduceTurbofish mt <> "(" <> schemaRef sid <> ", "
    <> printExpr h <> ", " <> printExpr f <> ")"
printExpr (IIntrinsicStdin sid) = "::rustmorloc::open_stdin(" <> schemaRef sid <> ")"
printExpr (IIntrinsicStdout sid) = "::rustmorloc::open_stdout(" <> schemaRef sid <> ")"
printExpr (IIntrinsicStderr sid) = "::rustmorloc::open_stderr(" <> schemaRef sid <> ")"
printExpr (IIntrinsicIFileWalk sid mt pathExpr h runtimeArgs) =
  "::rustmorloc::ifile_walk" <> turbofish mt <> "(" <> schemaRef sid <> ", " <> printExpr h
    <> ", " <> refExpr pathExpr <> ", &[" <> argList <> "])"
  where
    argList = hcat (punctuate ", " (map printExpr runtimeArgs))
-- A Rust value is decoded into its declared type, so a literal already has
-- the decoder's form.
printExpr (IIntrinsicConform _ e) = printExpr e
printExpr _ = error "RustPrinter: this intrinsic/expression is unsupported in Rust v1"

-- | The serializer a manifold's result goes through; 'rustReply' recognizes it.
putValueFn :: MDoc
putValueFn = "::rustmorloc::put_value"

-- | The name of a manifold's function in a Rust pool.
rustManifoldName :: Int -> MDoc
rustManifoldName i = "__mm_" <> viaShow i

-- | Reference into the pool's schema table: `__morloc_schema(<id>)` returns a
-- `&'static Schema`, which coerces to the `&Schema` every shim expects. By
-- path, so it resolves inside a scope that binds a local of that name.
schemaRef :: Int -> MDoc
schemaRef sid = "crate::__morloc_schema(" <> pretty sid <> ")"

-- | Borrow an expression (`&(e)`), the ToVoidstar/`&str` shape the value-in and
-- path shims expect.
refExpr :: IExpr -> MDoc
refExpr e = "&" <> parens (printExpr e)

rustGatherFn :: Bool -> MDoc
rustGatherFn True = "gather_native"
rustGatherFn False = "gather_file"

-- | The accumulator and batch types of a fold; its @n@ callables' are
-- inferred.
foldTurbofish :: Int -> Maybe IType -> Maybe IType -> MDoc
foldTurbofish n (Just b) (Just l) = "::<" <> rustType b <> ", " <> rustType l <> mconcat (replicate n ", _") <> ">"
foldTurbofish _ _ _ = ""

-- | Optional turbofish for a typed-result intrinsic: `::<T>` when the result
-- type is known (always, for Rust), empty otherwise. Lets a generic shim
-- (`load`/`next`/`stream_layout`/`ifile_walk`) resolve its return type.
turbofish :: Maybe IType -> MDoc
turbofish = maybe "" (\t -> "::<" <> rustType t <> ">")

-- | Turbofish for a shim whose second parameter is a closure type, which can
-- only be inferred.
reduceTurbofish :: Maybe IType -> MDoc
reduceTurbofish = maybe "" (\t -> "::<" <> rustType t <> ", _>")

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
    esc c
      | c < ' ' || c == '\DEL' = T.pack (printf "\\x%02x" (fromEnum c))
      | otherwise = T.singleton c

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
  vsep [dispatchFn "__morloc_local_dispatch" "local" (localCases ++ closureCases), "", dispatchFn "__morloc_remote_dispatch" "remote" remoteCases]
  where
    localCases = map (makeCase "") locals
    remoteCases = map (makeCase "_remote") remotes
    -- A crossing closure is applied in its home pool: a foreign pool's reflected
    -- proxy calls back on the closure's manifold id, routed to its serial
    -- dispatch wrapper (which deserializes captured ++ bound and calls the body).
    closureCases =
      [ pretty i <+> "=>" <+> "__morloc_closure_dispatch_" <> pretty i <> "(args, nargs)," | i <- closureMids ]

    makeCase :: MDoc -> DispatchEntry -> MDoc
    makeCase suffix (DispatchEntry i n _) =
      pretty i <+> "=>" <+> rustManifoldName i <> suffix
        <> tupled ["a(" <> pretty j <> ")" | j <- take n ([0 ..] :: [Int])] <> ","

    dispatchFn :: MDoc -> MDoc -> [MDoc] -> MDoc
    dispatchFn name kind cases =
      vsep
        [ "unsafe extern \"C\" fn" <+> name
            <> "(mid: u32, args: *const *const u8, nargs: usize, _ctx: *mut ::std::os::raw::c_void) -> *mut u8 {"
        , indent 4 $ vsep
            [ "::rustmorloc::dispatch_flush();"
            , "let args = ::std::panic::AssertUnwindSafe(args);"
            , "::rustmorloc::dispatch_guard(move || {"
            , indent 4 $ vsep
                [ "let args = *args;"
                , "let a = |i: usize| -> *const u8 { if i < nargs { unsafe { *args.add(i) } } else { ::std::ptr::null() } };"
                , "unsafe { match mid {"
                , indent 4 $ vsep
                    ( cases
                        ++ ["_ => ::rustmorloc::fail_packet(&format!(\"Invalid " <> kind <> " manifold id: {mid}\")),"]
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
        [ "static __MORLOC_SCHEMA_STRS: &[&str] = &["
        , indent 4 $ vsep [dquotes (pretty (rustEscape s)) <> "," | s <- schemas]
        , "];"
        , "static __MORLOC_SCHEMA_TABLE: ::std::sync::OnceLock<Vec<::rustmorloc::Schema>> = ::std::sync::OnceLock::new();"
        , "fn __morloc_init_schemas() {"
        , indent 4 $ vsep
            [ "let table: Vec<::rustmorloc::Schema> = __MORLOC_SCHEMA_STRS.iter()"
            , indent 4 ".map(|s| ::rustmorloc::parse_schema(s).expect(\"morloc: invalid embedded schema\")).collect();"
            , "let _ = __MORLOC_SCHEMA_TABLE.set(table);"
            ]
        , "}"
        , "#[inline]"
        , "fn __morloc_schema(id: usize) -> &'static ::rustmorloc::Schema {"
        , indent 4 "__MORLOC_SCHEMA_TABLE.get().and_then(|t| t.get(id)).unwrap_or_else(|| ::rustmorloc::morloc_infra_abort(\"a manifold asked for a schema the pool does not have\"))"
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
    , "pub struct" <+> name <> paramList params <+> "{"
    , indent 4 (vsep ["pub" <+> f <> ":" <+> t <> "," | (f, t) <- fields])
    , "}"
    ]

-- | A record that refers to itself, declared as a handle to its fields
-- (GEN-1), read through Deref and released through the crate's release
-- function (STATE-8); with parameters, generic over them. A copy is a count
-- of the shared node, so cloning bounds no parameter.
printRustHandle :: MDoc -> [MDoc] -> [(MDoc, MDoc)] -> MDoc
printRustHandle name params fields =
  vsep
    [ "pub struct" <+> self <> "(pub ::std::mem::ManuallyDrop<::std::sync::Arc<" <> fieldsSelf <> ">>);"
    , printRustStruct fieldsName params fields
    , "impl" <> gens <+> "Clone for" <+> self <+> "{"
    , "    fn clone(&self) -> Self { Self(::std::mem::ManuallyDrop::new(::std::sync::Arc::clone(&self.0))) }"
    , "}"
    , "impl" <> gens <+> "::std::ops::Deref for" <+> self <+> "{"
    , "    type Target =" <+> fieldsSelf <> ";"
    , "    fn deref(&self) -> &" <> fieldsSelf <+> "{ &self.0 }"
    , "}"
    , "impl" <> gens <+> self <+> "{"
    , "    pub fn new(f: " <> fieldsSelf <> ") -> Self { Self(::std::mem::ManuallyDrop::new(::std::sync::Arc::new(f))) }"
    , "    pub fn into_parts(self) -> ::std::result::Result<" <> fieldsSelf <> ", ::std::sync::Arc<" <> fieldsSelf <> ">> {"
    , "        let mut me = ::std::mem::ManuallyDrop::new(self);"
    , "        // SAFETY: `me` is never dropped, so its node is taken exactly once."
    , "        ::std::sync::Arc::try_unwrap(unsafe { ::std::mem::ManuallyDrop::take(&mut me.0) })"
    , "    }"
    , "}"
    , "impl" <> gens <+> "Drop for" <+> self <+> "{"
    , "    fn drop(&mut self) {"
    , "        // SAFETY: the field is taken exactly once, here."
    , "        crate::mlc::rec_release(unsafe { ::std::mem::ManuallyDrop::take(&mut self.0) });"
    , "    }"
    , "}"
    ]
  where
    gens = paramList params
    self = name <> gens
    fieldsName = name <> "Fields"
    fieldsSelf = fieldsName <> gens

-- | A record value from its fields: a struct literal, or a handle around one.
rustRecordValue :: Bool -> MDoc -> [(Key, MDoc)] -> MDoc
rustRecordValue handle name fields
  | handle = name <> "::new(" <> name <> "Fields" <+> "{" <+> rustRecordFields fields <+> "})"
  | otherwise = name <+> "{" <+> rustRecordFields fields <+> "}"

-- | Emit the definition of a pool-owned @data@ type.
--
-- @repr(u8)@ with explicit discriminants is what makes the wire tag and the
-- native value the same byte: the constructor's declaration ordinal IS its
-- tag, so no translation table is needed on either side.
--
-- @PartialOrd@/@Ord@ derive over the variant order, which is the same
-- declaration order every other backend compares in. Without them a
-- comparison that compiles when the realizer puts it in a Python pool
-- fails to compile when it puts it in a Rust one.
printRustEnum :: MDoc -> [T.Text] -> MDoc
printRustEnum name ctors =
  vsep
    [ "#[repr(u8)]"
    , "#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]"
    , "pub enum" <+> name <+> "{"
    , indent 4 (vsep [pretty c <+> "=" <+> pretty i <> "," | (i, c) <- indexed ctors])
    , "}"
    ]

-- | Emit a Rust @enum@ for a payload-bearing @data@ type, with one
-- generated arm type per payload-bearing constructor.
--
-- Every arm's fields sit behind one shared node, holding a tuple, which
-- mirrors the wire form: a tag plus a relative pointer to the arm's fields.
-- Boxing every arm rather than only at self-referential positions is what
-- lets a recursive type have a finite size without any recursion analysis
-- here, mutual recursion included. An arm type is standard Rust (CPP-6's
-- arm structs, in Rust): a reference-counted node read through Deref, so a
-- copy is a count, and dropped through the crate's release hook (STATE-8).
--
-- Deliberately NOT @Copy@ and NOT @#[repr(u8)]@, both of which the
-- argument-free form carries.
--
-- A generic enum's arm types take the parameters their fields hold, so the
-- arm's declared name and its spelling in the enum are given.
printRustVariant :: MDoc -> (T.Text -> MDoc) -> (T.Text -> MDoc) -> [(T.Text, [MDoc])] -> MDoc
printRustVariant name armDeclName armRef arms =
  vsep $
    [printRustArm (armDeclName c) ts | (c, ts@(_ : _)) <- arms]
      <> ["#[derive(Clone)]" | not generic]
      <> [ "pub enum" <+> name <+> "{"
         , indent 4 (vsep [armDecl c ts | (c, ts) <- arms])
         , "}"
         ]
      -- A derived Clone would bound every parameter by Clone; an arm is a
      -- shared node, a copy of which is a count for any parameter.
      <> [cloneImpl | generic]
  where
    generic = T.any (== '<') (render name)
    cloneImpl =
      vsep
        [ "impl" <> pretty (T.dropWhile (/= '<') (render name)) <+> "Clone for" <+> name <+> "{"
        , indent 4 $ vsep
            [ "fn clone(&self) -> Self {"
            , indent 4 $ vsep
                [ "match self {"
                , indent 4 (vsep [if null ts then "Self::" <> pretty c <+> "=> Self::" <> pretty c <> "," else "Self::" <> pretty c <> "(x) => Self::" <> pretty c <> "(x.clone())," | (c, ts) <- arms])
                , "}"
                ]
            , "}"
            ]
        , "}"
        ]
    armDecl c [] = pretty c <> ","
    armDecl c _ = pretty c <> parens (armRef c) <> ","

-- | The generated type of one constructor's arm, given its enum's type:
-- @Shape@ and @Circle@ give @Shape_Circle@.
armTypeName :: MDoc -> T.Text -> MDoc
armTypeName name c = name <> "_" <> pretty c

-- | One arm's type: the crate's shared node holding the constructor's
-- fields ('printRustNode').
printRustArm :: MDoc -> [MDoc] -> MDoc
printRustArm arm ts =
  vsep
    [ "#[allow(non_camel_case_types)]"
    , "pub type" <+> arm <+> "= crate::mlc::Node<" <> tupled1 ts <> ">;"
    ]

-- | The node every generated arm is (GEN-1): standard Rust, a
-- reference-counted value read through Deref, dropped through the release
-- function it is given (STATE-8). One per crate, in its @mlc@ module.
printRustNode :: MDoc -> MDoc
printRustNode release =
  vsep
    [ "pub struct Node<T> { pub(crate) node: ::std::mem::ManuallyDrop<::std::sync::Arc<T>> }"
    , "impl<T> ::std::clone::Clone for Node<T> {"
    , "    fn clone(&self) -> Self { Self { node: ::std::mem::ManuallyDrop::new(::std::sync::Arc::clone(&self.node)) } }"
    , "}"
    , "impl<T> ::std::ops::Deref for Node<T> {"
    , "    type Target = T;"
    , "    fn deref(&self) -> &T { &self.node }"
    , "}"
    , "impl<T> ::std::convert::From<T> for Node<T> {"
    , "    fn from(v: T) -> Self { Self::new(v) }"
    , "}"
    , "impl<T> Node<T> {"
    , "    pub fn new(v: T) -> Self { Self { node: ::std::mem::ManuallyDrop::new(::std::sync::Arc::new(v)) } }"
    , "    pub fn try_into_inner(self) -> ::std::result::Result<T, Self> {"
    , "        let mut me = ::std::mem::ManuallyDrop::new(self);"
    , "        // SAFETY: `me` is never dropped, so its node is taken exactly once."
    , "        let rc = unsafe { ::std::mem::ManuallyDrop::take(&mut me.node) };"
    , "        ::std::sync::Arc::try_unwrap(rc).map_err(|rc| Self { node: ::std::mem::ManuallyDrop::new(rc) })"
    , "    }"
    , "    pub fn into_inner(self) -> T where T: Clone {"
    , "        match self.try_into_inner() { Ok(v) => v, Err(b) => (*b).clone() }"
    , "    }"
    , "}"
    , "impl<T> ::std::ops::Drop for Node<T> {"
    , "    fn drop(&mut self) {"
    , "        // SAFETY: the node is taken exactly once, here."
    , "        " <> release <> "(unsafe { ::std::mem::ManuallyDrop::take(&mut self.node) });"
    , "    }"
    , "}"
    ]

-- | The pool-side impls of the crate's node: marshalling as its payload, and
-- taking the payload at a last use (OWN-5).
printRustNodeImpls :: MDoc
printRustNodeImpls =
  vsep
    [ "impl<T> ::rustmorloc::MlcTake for crate::mlc::Node<T> {"
    , "    type Payload = T;"
    , "    fn take_with<R>(self, owned: impl FnOnce(T) -> R, shared: impl FnOnce(&T) -> R) -> R {"
    , "        match self.try_into_inner() { Ok(v) => owned(v), Err(b) => shared(&b) }"
    , "    }"
    , "}"
    , "impl<T: ::rustmorloc::ToVoidstar> ::rustmorloc::ToVoidstar for crate::mlc::Node<T> {"
    , "    const IS_LEAF: bool = T::IS_LEAF;"
    , "    fn shm_size(&self, schema: &::rustmorloc::Schema) -> usize { (**self).shm_size(schema) }"
    , "    unsafe fn write(&self, dest: *mut u8, cursor: &mut *mut u8, schema: &::rustmorloc::Schema) { (**self).write(dest, cursor, schema) }"
    , "    unsafe fn write_leaf(&self, w: &mut ::rustmorloc::WriteWalk, dest: *mut u8, schema: &::rustmorloc::Schema) { (**self).write_leaf(w, dest, schema) }"
    , "    fn size_step(&self, w: &mut ::rustmorloc::SizeWalk, schema: &::rustmorloc::Schema, idx: usize) { (**self).size_step(w, schema, idx) }"
    , "    unsafe fn write_step(&self, w: &mut ::rustmorloc::WriteWalk, dest: *mut u8, schema: &::rustmorloc::Schema, idx: usize) { (**self).write_step(w, dest, schema, idx) }"
    , "}"
    , "impl<T: ::rustmorloc::FromVoidstar> ::rustmorloc::FromVoidstar for crate::mlc::Node<T> {"
    , "    const IS_LEAF: bool = T::IS_LEAF;"
    , "    unsafe fn read(schema: &::rustmorloc::Schema, data: *const u8, space: ::rustmorloc::MorlocSpace) -> Self { Self::new(T::read(schema, data, space)) }"
    , "    unsafe fn read_step(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8, _idx: usize) {"
    , "        w.push_finish::<Self>(schema, data);"
    , "        w.child_step::<T>(schema, data);"
    , "    }"
    , "    unsafe fn read_finish(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) -> Self { Self::new(w.child_read::<T>(schema, data)) }"
    , "}"
    ]

-- | A one-element tuple needs its trailing comma or it is just parentheses.
tupled1 :: [MDoc] -> MDoc
tupled1 [t] = parens (t <> ",")
tupled1 ts = tupled ts

-- | An arm that crosses as its wire tuple: that tuple's type, the
-- conversion from the payload tuple, and the conversion back.
data ArmWire = ArmWire
  { awType :: MDoc
  , awReify :: MDoc -> MDoc
  , awReflect :: MDoc -> MDoc
  }

-- | A field of a user-written enum arm, by morloc's type of it: the type,
-- rendered, and whether it is an optional of that type.
type UserField = (MDoc, Bool)

-- | The expression giving a user-written arm's field to a walk: the value
-- of morloc's type behind the field, or, for an optional, the field itself
-- once its type is checked (its pointer is transparent on the wire).
userFieldRef :: UserField -> MDoc -> MDoc
userFieldRef (t, False) f = "::rustmorloc::MlcField::<" <> t <> ">::field(" <> f <> ")"
userFieldRef (t, True) f = "::rustmorloc::opt_field::<" <> t <> ", _>(" <> f <> ")"

-- | The type a user-written arm's field is read at.
userFieldReadTy :: UserField -> MDoc
userFieldReadTy (t, False) = t
userFieldReadTy (t, True) = "::std::option::Option<" <> t <> ">"

-- | A user-written arm's field built from a value of morloc's type.
userFieldFrom :: UserField -> MDoc -> MDoc
userFieldFrom (t, False) v = "::rustmorloc::MlcField::<" <> t <> ">::from_field(" <> v <> ")"
userFieldFrom (t, True) v = "::rustmorloc::MlcOptField::<" <> t <> ">::from_opt(" <> v <> ")"

-- | A copy, of morloc's type, of a user-written arm's field.
userFieldCopy :: UserField -> MDoc -> MDoc
userFieldCopy (t, False) f = "::rustmorloc::MlcField::<" <> t <> ">::field(" <> f <> ").clone()"
userFieldCopy (t, True) f = "::rustmorloc::MlcOptField::<" <> t <> ">::opt(" <> f <> ").cloned()"

-- | A user-written arm's field taken as a value of morloc's type.
userFieldTake :: UserField -> MDoc -> MDoc
userFieldTake (t, False) f = "::rustmorloc::MlcField::<" <> t <> ">::unfield(" <> f <> ")"
userFieldTake (t, True) f = "::rustmorloc::MlcOptField::<" <> t <> ">::unfield_opt(" <> f <> ")"

-- | A tuple-variant pattern binding every field of an arm.
userArmPattern :: MDoc -> T.Text -> [MDoc] -> MDoc
userArmPattern hd c [] = hd <> "::" <> pretty c
userArmPattern hd c vs = hd <> "::" <> pretty c <> tupled vs

-- | The marshalling impls of a user-written payload-bearing @data@ type
-- whose arms are tuple variants, one field per morloc field, each holding
-- its morloc type as is, in a Box or in an Arc (an optional field: in an
-- Option of one of those). Every field is walked at morloc's type, from the
-- value itself.
printUserVariantImpls :: MDoc -> [(T.Text, [UserField], Maybe ArmWire)] -> MDoc
printUserVariantImpls name arms3 = vsep [toImpl, "", fromImpl]
  where
    idxArms = indexed arms3
    vars fs = ["mlc_f" <> pretty j | (j, _) <- indexed fs]
    pat c fs = userArmPattern "Self" c (vars fs)
    badTag = "t => panic!(\"" <> name <> ": no constructor for tag {}\", t),"
    -- A closure-bearing arm crosses as a wire tuple, built from its fields
    -- by reference (an optional behind a pointer, by copy).
    copied fs = tupled1 [viewOf f v | (f, v) <- zipSame fs (vars fs)]
    viewOf f@(_, False) v = userFieldRef f v
    viewOf f v = userFieldCopy f v
    sizeArm i c fs mw = case (fs, mw) of
      ([], _) -> pat c fs <+> "=> w.total += schema.width as isize,"
      (_, Just aw) ->
        pat c fs <+> "=> w.variant_payload_converted(schema, &schema.parameters["
          <> pretty i <> "], self as *const Self as usize, " <> pretty i <> ", || { let mlc_t = " <> copied fs <> "; " <> awReify aw "mlc_t" <> " }),"
      _ ->
        pat c fs <+> "=> { let a = w.variant_arm(schema, &schema.parameters[" <> pretty i <> "]);"
          <+> hsep ["w.child(" <> userFieldRef f v <> ", &a.parameters[" <> pretty j <> "], true);" | (j, (f, v)) <- indexed (zipSame fs (vars fs))]
          <+> "}"
    writeArm i c fs mw = case (fs, mw) of
      ([], _) -> pat c fs <+> "=> ::rustmorloc::write_variant_nullary(dest, " <> pretty i <> "u8),"
      (_, Just aw) ->
        pat c fs <+> "=> w.variant_payload_converted(dest, &schema.parameters["
          <> pretty i <> "], " <> pretty i <> "u8, self as *const Self as usize, " <> pretty i <> ", || { let mlc_t = " <> copied fs <> "; " <> awReify aw "mlc_t" <> " }),"
      _ ->
        pat c fs <+> "=> { let (slot, a) = w.variant_arm(dest, &schema.parameters[" <> pretty i <> "], " <> pretty i <> "u8);"
          <+> hsep ["w.child(" <> userFieldRef f v <> ", slot.add(a.offsets[" <> pretty j <> "]), &a.parameters[" <> pretty j <> "]);" | (j, (f, v)) <- indexed (zipSame fs (vars fs))]
          <+> "}"
    stepArm i fs mw = case (fs, mw) of
      ([], _) -> pretty i <+> "=> {}"
      (_, Just aw) ->
        pretty i <+> "=> { let p = w.payload_ptr(data, &schema.parameters[" <> pretty i <> "]); w.child_step::<"
          <> awType aw <> ">(&schema.parameters[" <> pretty i <> "], p); }"
      _ ->
        pretty i <+> "=> { let p = w.payload_ptr(data, &schema.parameters[" <> pretty i <> "]); let a = ::rustmorloc::resolve_recur(&schema.parameters[" <> pretty i <> "]);"
          <+> hsep ["w.child_step::<" <> userFieldReadTy f <> ">(&a.parameters[" <> pretty j <> "], p.add(a.offsets[" <> pretty j <> "]));" | (j, f) <- indexed fs]
          <+> "}"
    finishArm i c fs mw = case (fs, mw) of
      ([], _) -> pretty i <+> "=> Self::" <> pretty c <> ","
      (_, Just aw) ->
        pretty i <+> "=> { let p = w.payload_ptr(data, &schema.parameters[" <> pretty i <> "]); let mlc_t = "
          <> awReflect aw ("w.child_read::<" <> awType aw <> ">(&schema.parameters[" <> pretty i <> "], p)")
          <> "; Self::" <> pretty c <> tupled [userFieldFrom f ("mlc_t." <> pretty j) | (j, f) <- indexed fs] <> " }"
      _ ->
        pretty i <+> "=> { let p = w.payload_ptr(data, &schema.parameters[" <> pretty i <> "]); let a = ::rustmorloc::resolve_recur(&schema.parameters[" <> pretty i <> "]); Self::" <> pretty c
          <> tupled [userFieldFrom f ("w.child_read::<" <> userFieldReadTy f <> ">(&a.parameters[" <> pretty j <> "], p.add(a.offsets[" <> pretty j <> "]))") | (j, f) <- indexed fs]
          <> " }"
    toImpl =
      vsep
        [ "impl ::rustmorloc::ToVoidstar for" <+> name <+> "{"
        , indent 4 $ vsep
            [ "fn size_step(&self, w: &mut ::rustmorloc::SizeWalk, schema: &::rustmorloc::Schema, _idx: usize) {"
            , indent 4 $ vsep ["match self {", indent 4 (vsep [sizeArm i c fs mw | (i, (c, fs, mw)) <- idxArms]), "}"]
            , "}"
            , "unsafe fn write_step(&self, w: &mut ::rustmorloc::WriteWalk, dest: *mut u8, schema: &::rustmorloc::Schema, _idx: usize) {"
            , indent 4 $ vsep ["match self {", indent 4 (vsep [writeArm i c fs mw | (i, (c, fs, mw)) <- idxArms]), "}"]
            , "}"
            ]
        , "}"
        ]
    fromImpl =
      vsep
        [ "impl ::rustmorloc::FromVoidstar for" <+> name <+> "{"
        , indent 4 $ vsep
            [ "unsafe fn read_step(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8, _idx: usize) {"
            , indent 4 $ vsep
                [ "w.push_finish::<Self>(schema, data);"
                , "match ::rustmorloc::read_variant_tag(data) {"
                , indent 4 (vsep ([stepArm i fs mw | (i, (_, fs, mw)) <- idxArms] <> [badTag]))
                , "}"
                ]
            , "}"
            , "unsafe fn read_finish(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) -> Self {"
            , indent 4 $ vsep
                [ "match ::rustmorloc::read_variant_tag(data) {"
                , indent 4 (vsep ([finishArm i c fs mw | (i, (c, fs, mw)) <- idxArms] <> [badTag]))
                , "}"
                ]
            , "}"
            ]
        , "}"
        ]

-- | The marshalling impls of a generated payload-bearing @data@ type, whose
-- arms are its generated arm types.
printVariantImpls :: [MDoc] -> MDoc -> (T.Text -> MDoc) -> [(T.Text, [MDoc], Maybe ArmWire)] -> MDoc
printVariantImpls params name armRef arms3 = vsep [toImpl, "", fromImpl]
  where
    idxArms = indexed arms3
    payload = "(**mlc_b)"
    readTy c ts mw = maybe (armTy c ts) awType mw
    readArm i c ts mw p = case mw of
      Nothing -> "w.child_read::<" <> armTy c ts <> ">(&schema.parameters[" <> pretty i <> "], " <> p <> ")"
      Just aw -> armCtor c <> parens (awReflect aw ("w.child_read::<" <> awType aw <> ">(&schema.parameters[" <> pretty i <> "], " <> p <> ")"))

    -- Constructors are spelled through `Self` inside the impl: it is legal
    -- in expression and pattern position alike, for a plain name and for a
    -- generic instantiation the impl is written for.
    armPat c ts = "Self::" <> pretty c <> (if null ts then "" else "(mlc_b)")
    -- The payload's native type, as the enum declares it, and its constructor.
    armTy c _ = armRef c
    armCtor c = armHead (armRef c) <> "::new"
    -- A generic arm type's constructor is reached through its path alone.
    armHead d = pretty (T.takeWhile (/= '<') (render d))
    -- The runtime range-checks a tag at the wire boundary, so reaching this
    -- arm means the value and its schema disagree: a bug, not bad input.
    -- Here the tag would otherwise index the arm list.
    badTag = "t => panic!(\"" <> name <> ": no constructor for tag {}\", t),"

    -- A generic enum's impls hold at every instance whose arguments do.
    bounded tr = if null params then "impl" else "impl" <> paramList [p <> ": " <> tr | p <- params]
    toImpl =
      vsep
        [ bounded "::rustmorloc::ToVoidstar" <+> "::rustmorloc::ToVoidstar for" <+> name <+> "{"
        , indent 4 $ vsep
            [ "fn size_step(&self, w: &mut ::rustmorloc::SizeWalk, schema: &::rustmorloc::Schema, _idx: usize) {"
            , indent 4 $ vsep
                [ "match self {"
                , indent 4 $ vsep
                    [ armPat c ts <+> "=>" <+>
                        (if null ts
                           then "w.total += schema.width as isize,"
                           else case mw of
                             Nothing -> "w.variant_payload(schema, &schema.parameters["
                                          <> pretty i <> "], mlc_b),"
                             Just aw -> "w.variant_payload_converted(schema, &schema.parameters["
                                          <> pretty i <> "], self as *const Self as usize, " <> pretty i <> ", || " <> awReify aw payload <> "),")
                    | (i, (c, ts, mw)) <- idxArms ]
                , "}"
                ]
            , "}"
            , "unsafe fn write_step(&self, w: &mut ::rustmorloc::WriteWalk, dest: *mut u8, schema: &::rustmorloc::Schema, _idx: usize) {"
            , indent 4 $ vsep
                [ "match self {"
                , indent 4 $ vsep
                    [ armPat c ts <+> "=>" <+>
                        (if null ts
                           then "::rustmorloc::write_variant_nullary(dest, " <> pretty i <> "u8),"
                           else case mw of
                             Nothing -> "w.variant_payload(dest, &schema.parameters["
                                          <> pretty i <> "], " <> pretty i <> "u8, mlc_b),"
                             Just aw -> "w.variant_payload_converted(dest, &schema.parameters["
                                          <> pretty i <> "], " <> pretty i <> "u8, self as *const Self as usize, " <> pretty i <> ", || " <> awReify aw payload <> "),")
                    | (i, (c, ts, mw)) <- idxArms ]
                , "}"
                ]
            , "}"
            ]
        , "}"
        ]

    fromImpl =
      vsep
        [ bounded "::rustmorloc::FromVoidstar" <+> "::rustmorloc::FromVoidstar for" <+> name <+> "{"
        , indent 4 $ vsep
            [ "unsafe fn read_step(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8, _idx: usize) {"
            , indent 4 $ vsep
                [ "w.push_finish::<Self>(schema, data);"
                , "match ::rustmorloc::read_variant_tag(data) {"
                , indent 4 $ vsep
                    ( [ pretty i <+> "=>" <+>
                          (if null ts
                             then "{}"
                             else "{ let p = w.payload_ptr(data, &schema.parameters[" <> pretty i <> "]); w.child_step::<"
                                    <> readTy c ts mw <> ">(&schema.parameters[" <> pretty i <> "], p); }")
                      | (i, (c, ts, mw)) <- idxArms ]
                      <> [badTag]
                    )
                , "}"
                ]
            , "}"
            , "unsafe fn read_finish(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) -> Self {"
            , indent 4 $ vsep
                [ "match ::rustmorloc::read_variant_tag(data) {"
                , indent 4 $ vsep
                    ( [ pretty i <+> "=>" <+>
                          (if null ts
                             then "Self::" <> pretty c <> ","
                             else "{ let p = w.payload_ptr(data, &schema.parameters[" <> pretty i <> "]); Self::" <> pretty c
                                    <> parens (readArm i c ts mw "p") <> " }")
                      | (i, (c, ts, mw)) <- idxArms ]
                      <> [badTag]
                    )
                , "}"
                ]
            , "}"
            ]
        , "}"
        ]

-- | Emit @ToVoidstar@/@FromVoidstar@ for a @data@ type.
--
-- The value is one byte and fixed-width, so @shm_size@ is the schema width
-- and @write@ is a single store -- no cursor advance, because there is no
-- out-of-line payload.
printEnumImpls :: MDoc -> [T.Text] -> MDoc
printEnumImpls name ctors = vsep [toImpl, "", fromImpl]
  where
    toImpl =
      vsep
        [ "impl ::rustmorloc::ToVoidstar for" <+> name <+> "{"
        , indent 4 $ vsep
            [ "const IS_LEAF: bool = true;"
            , "fn shm_size(&self, schema: &::rustmorloc::Schema) -> usize {"
            , indent 4 "::rustmorloc::resolve_recur(schema).width"
            , "}"
            , "unsafe fn write(&self, dest: *mut u8, _cursor: &mut *mut u8, _schema: &::rustmorloc::Schema) {"
            , indent 4 "*dest = *self as u8;"
            , "}"
            ]
        , "}"
        ]
    fromImpl =
      vsep
        [ "impl ::rustmorloc::FromVoidstar for" <+> name <+> "{"
        , indent 4 $ vsep
            [ "const IS_LEAF: bool = true;"
            , "unsafe fn read(_schema: &::rustmorloc::Schema, data: *const u8, _space: ::rustmorloc::MorlocSpace) -> Self {"
            , indent 4 $ vsep
                [ "match *data {"
                , indent 4 $ vsep
                    ( [pretty i <+> "=>" <+> "Self::" <> pretty c <> "," | (i, c) <- indexed ctors]
                        ++ [ -- The runtime range-checks every tag at the wire
                             -- boundary, so reaching this arm means the value
                             -- and its schema disagree: a bug, not bad input.
                             "t => panic!(\"" <> name <> ": no constructor for tag {}\", t),"
                           ]
                    )
                , "}"
                ]
            , "}"
            ]
        , "}"
        ]

-- | Per-field marshalling strategy for a record function field. A closure field
-- is stored natively as a function value (no @ToVoidstar@), so it is reified
-- to its @ClosureOrigin@ wire tuple on write/shm_size and reflected back on read.
data ClosureMarshal
  = ClosureMarshal
      { cmReflect :: MDoc -> MDoc    -- ^ a @ClosureOrigin@ slot-read expression -> the reflected function value
      }
  | -- | A field written in converted form (GEN-14): the type it is written
    -- as, and the functions converting the field to it and back.
    PackMarshal
      { pmRaw :: MDoc
      , pmTo :: MDoc
      , pmFrom :: MDoc
      }
  | -- | A field whose conversion depends on the record's arguments,
    -- marshalled through the trait for that field, implemented by the
    -- field's type at each instance (GEN-14).
    ViaTrait
      { vtTrait :: MDoc
      }

-- | Whose field types a record's reader names: a generated struct's are the
-- rendered ones; a user-written struct's are whatever the user declared, so
-- the reader takes each from the struct itself (a user may hold a recursive
-- field in a plain 'Box' or in the runtime's deferred-release box).
data RecordFieldTypes = RenderedFieldTypes | StructFieldTypes

-- | The trait a field whose conversion depends on its record's arguments
-- is marshalled through: implemented by the field's type at each instance,
-- written as it is held or converted (GEN-14).
printFieldTrait :: MDoc -> MDoc
printFieldTrait tr =
  vsep
    [ "#[allow(non_camel_case_types)]"
    , "pub trait" <+> tr <+> "{"
    , "    fn mlc_size(&self, w: &mut ::rustmorloc::SizeWalk, schema: &::rustmorloc::Schema);"
    , "    unsafe fn mlc_write(&self, w: &mut ::rustmorloc::WriteWalk, dest: *mut u8, schema: &::rustmorloc::Schema);"
    , "    unsafe fn mlc_step(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8);"
    , "    unsafe fn mlc_read(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) -> Self;"
    , "}"
    ]

-- | How one instance's field type implements its field trait.
data FieldTraitImpl
  = TraitPlain
  -- ^ written as it is held
  | TraitPack MDoc MDoc MDoc
  -- ^ converted: the type written, and the functions to it and back
  | TraitClosure (MDoc -> MDoc)
  -- ^ a function, reified to its origin and reflected back

-- | The field trait's impl for one instance's field type, a converted value
-- kept in the walk's table of converted values, keyed by the field index.
printFieldTraitImpl :: MDoc -> MDoc -> Int -> FieldTraitImpl -> MDoc
printFieldTraitImpl tr ty i conv =
  vsep
    [ "impl" <+> tr <+> "for" <+> ty <+> "{"
    , indent 4 $ vsep $ case conv of
        TraitPlain ->
          [ "fn mlc_size(&self, w: &mut ::rustmorloc::SizeWalk, schema: &::rustmorloc::Schema) { w.child(self, schema, true); }"
          , "unsafe fn mlc_write(&self, w: &mut ::rustmorloc::WriteWalk, dest: *mut u8, schema: &::rustmorloc::Schema) { w.child(self, dest, schema); }"
          , "unsafe fn mlc_step(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) { w.child_step::<Self>(schema, data); }"
          , "unsafe fn mlc_read(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) -> Self { w.child_read::<Self>(schema, data) }"
          ]
        TraitClosure reflect ->
          [ "fn mlc_size(&self, w: &mut ::rustmorloc::SizeWalk, schema: &::rustmorloc::Schema) {"
          , "    w.child_converted(self as *const _ as usize, ::std::any::TypeId::of::<::rustmorloc::ClosureOrigin>(), " <> pretty i <> ", || ::rustmorloc::require_origin(::rustmorloc::reify(self)), schema, true);"
          , "}"
          , "unsafe fn mlc_write(&self, w: &mut ::rustmorloc::WriteWalk, dest: *mut u8, schema: &::rustmorloc::Schema) {"
          , "    w.child_converted(self as *const _ as usize, ::std::any::TypeId::of::<::rustmorloc::ClosureOrigin>(), " <> pretty i <> ", || ::rustmorloc::require_origin(::rustmorloc::reify(self)), dest, schema);"
          , "}"
          , "unsafe fn mlc_step(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) { w.child_step::<::rustmorloc::ClosureOrigin>(schema, data); }"
          , "unsafe fn mlc_read(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) -> Self { " <> reflect "w.child_read::<::rustmorloc::ClosureOrigin>(schema, data)" <> " }"
          ]
        TraitPack raw to from ->
          [ "fn mlc_size(&self, w: &mut ::rustmorloc::SizeWalk, schema: &::rustmorloc::Schema) {"
          , "    w.child_converted(self as *const _ as usize, ::std::any::TypeId::of::<" <> raw <> ">(), " <> pretty i <> ", || " <> to <> "(self), schema, true);"
          , "}"
          , "unsafe fn mlc_write(&self, w: &mut ::rustmorloc::WriteWalk, dest: *mut u8, schema: &::rustmorloc::Schema) {"
          , "    w.child_converted(self as *const _ as usize, ::std::any::TypeId::of::<" <> raw <> ">(), " <> pretty i <> ", || " <> to <> "(self), dest, schema);"
          , "}"
          , "unsafe fn mlc_step(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) { w.child_step::<" <> raw <> ">(schema, data); }"
          , "unsafe fn mlc_read(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) -> Self { " <> from <> "(w.child_read::<" <> raw <> ">(schema, data)) }"
          ]
    , "}"
    ]

-- | Emit @impl ToVoidstar/FromVoidstar@ for a record, marshalling each field in
-- place at its schema offset (mirrors the hand-written LL template). Fields are
-- (escaped-name, rendered-type, is-variable-width, closure-marshal). @params@ are
-- the generic type parameters; each is bounded by the marshalling trait the impl
-- needs (`impl<T1: ToVoidstar> ToVoidstar for S<T1>`). Fully fixed-width records
-- short-circuit the size step to @schema.width@. A function field carries a
-- 'ClosureMarshal' and is reified/reflected instead of marshalled directly.
printRecordImpls :: RecordFieldTypes -> MDoc -> [MDoc] -> [(MDoc, MDoc, Bool, Maybe ClosureMarshal)] -> MDoc
printRecordImpls = printRecordImplsWith False

-- | 'printRecordImpls', for a handle when the flag is set: it is read as
-- its fields and then wrapped.
printRecordImplsWith :: Bool -> RecordFieldTypes -> MDoc -> [MDoc] -> [(MDoc, MDoc, Bool, Maybe ClosureMarshal)] -> MDoc
printRecordImplsWith handle fieldTypes name params fields =
  printRecordImplsWhere handle fieldTypes name params Nothing fields

-- | 'printRecordImplsWith', with the impls' bounds given as `where`
-- clauses (one list for writing, one for reading) in place of a bound on
-- each parameter.
printRecordImplsWhere ::
  Bool -> RecordFieldTypes -> MDoc -> [MDoc] -> Maybe ([MDoc], [MDoc]) -> [(MDoc, MDoc, Bool, Maybe ClosureMarshal)] -> MDoc
printRecordImplsWhere handle fieldTypes name params wheres fields = vsep [toImpl, "", fromImpl]
  where
    idx = indexed fields
    -- A closure field's wire form (ClosureOrigin) is variable-width, so a record
    -- with any closure field cannot short-circuit the size to schema.width.
    allFixed = all (\(_, _, v, cm) -> not v && isPlain cm) fields
    isPlain Nothing = True
    isPlain (Just _) = False
    ty = name <> paramList params
    -- Bound every generic param by `bound` for the impl's `impl<..>` header,
    -- unless the bounds are given as `where` clauses.
    boundedParams bound = case wheres of
      Nothing -> paramList [p <> ":" <+> bound | p <- params]
      Just _ -> paramList params
    whereOf sel = case wheres of
      Just ws | not (null (sel ws)) -> " where" <+> hcat (punctuate ", " (sel ws))
      _ -> ""

    fieldSchema i = "&schema.parameters[" <> pretty i <> "]"
    -- GEN-14: the field's identity in the walk's table of converted values,
    -- and how to convert it.
    converted i f raw to =
      "&self." <> f <> " as *const _ as usize, ::std::any::TypeId::of::<" <> raw <> ">(), " <> pretty i
        <> ", || " <> to <> "(&self." <> f <> ")"
    -- A closure field reifies once per write.
    closureKey i f =
      "&self." <> f <> " as *const _ as usize, ::std::any::TypeId::of::<::rustmorloc::ClosureOrigin>(), " <> pretty i
        <> ", || ::rustmorloc::require_origin(::rustmorloc::reify(&self." <> f <> "))"
    fieldSlot i = "dest.add(schema.offsets[" <> pretty i <> "])"
    fieldData i = "data.add(schema.offsets[" <> pretty i <> "])"

    -- Hand a field to the walk: a plain field by reference; a closure field
    -- reifies to its ClosureOrigin wire tuple (which IS ToVoidstar), owned by
    -- the walk since the step's own frame does not outlive the call.
    sizeField (i, (f, _, _, Nothing)) =
      "w.child(&self." <> f <> ", " <> fieldSchema i <> ", true);"
    sizeField (i, (f, _, _, Just (PackMarshal raw to _))) =
      "w.child_converted(" <> converted i f raw to <> ", " <> fieldSchema i <> ", true);"
    sizeField (i, (f, t, _, Just (ViaTrait tr))) =
      "<" <> t <> " as " <> tr <> ">::mlc_size(&self." <> f <> ", w, " <> fieldSchema i <> ");"
    sizeField (i, (f, _, _, Just _)) =
      "w.child_converted(" <> closureKey i f <> ", " <> fieldSchema i <> ", true);"
    writeField (i, (f, _, _, Nothing)) =
      "w.child(&self." <> f <> ", " <> fieldSlot i <> ", " <> fieldSchema i <> ");"
    writeField (i, (f, _, _, Just (PackMarshal raw to _))) =
      "w.child_converted(" <> converted i f raw to <> ", " <> fieldSlot i <> ", " <> fieldSchema i <> ");"
    writeField (i, (f, t, _, Just (ViaTrait tr))) =
      "<" <> t <> " as " <> tr <> ">::mlc_write(&self." <> f <> ", w, " <> fieldSlot i <> ", " <> fieldSchema i <> ");"
    writeField (i, (f, _, _, Just _)) =
      "w.child_converted(" <> closureKey i f <> ", " <> fieldSlot i <> ", " <> fieldSchema i <> ");"

    toImpl =
      vsep
        [ "impl" <> boundedParams "::rustmorloc::ToVoidstar" <+> "::rustmorloc::ToVoidstar for" <+> ty <> whereOf fst <+> "{"
        , indent 4 $ vsep
            [ "fn size_step(&self, w: &mut ::rustmorloc::SizeWalk, schema: &::rustmorloc::Schema, _idx: usize) {"
            , indent 4 sizeBody
            , "}"
            , "unsafe fn write_step(&self, w: &mut ::rustmorloc::WriteWalk, dest: *mut u8, schema: &::rustmorloc::Schema, _idx: usize) {"
            , indent 4 (vsep (map writeField idx))
            , "}"
            ]
        , "}"
        ]
    sizeBody
      | allFixed = "w.total += schema.width as isize;"
      | otherwise = vsep ("w.total += schema.width as isize;" : map sizeField idx)

    fromImpl =
      vsep
        [ "impl" <> boundedParams "::rustmorloc::FromVoidstar" <+> "::rustmorloc::FromVoidstar for" <+> ty <> whereOf snd <+> "{"
        , indent 4 $ vsep
            [ "unsafe fn read_step(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8, _idx: usize) {"
            , indent 4 $ vsep ("w.push_finish::<Self>(schema, data);" : map stepField idx)
            , "}"
            , "unsafe fn read_finish(w: &mut ::rustmorloc::ReadWalk, schema: &::rustmorloc::Schema, data: *const u8) -> Self {"
            , indent 4 $ vsep
                [ if handle then "Self::new(" <> name <> "Fields {" else "Self {"
                , indent 4 $ vsep (map readField idx)
                , if handle then "})" else "}"
                ]
            , "}"
            ]
        , "}"
        ]
    -- A plain field's frame is for its own type; a closure field's is for the
    -- ClosureOrigin wire tuple its slot holds.
    stepField (i, (f, t, _, Nothing)) = case fieldTypes of
      RenderedFieldTypes ->
        "w.child_step::<" <> t <> ">(" <> fieldSchema i <> ", " <> fieldData i <> ");"
      StructFieldTypes ->
        "w.child_step_for(" <> fieldSchema i <> ", " <> fieldData i <> ", |x: &Self| &x." <> f <> ");"
    stepField (i, (_, t, _, Just (ViaTrait tr))) =
      "<" <> t <> " as " <> tr <> ">::mlc_step(w, " <> fieldSchema i <> ", " <> fieldData i <> ");"
    stepField (i, (_, _, _, Just (PackMarshal raw _ _))) =
      "w.child_step::<" <> raw <> ">(" <> fieldSchema i <> ", " <> fieldData i <> ");"
    stepField (i, (_, _, _, Just _)) =
      "w.child_step::<::rustmorloc::ClosureOrigin>(" <> fieldSchema i <> ", " <> fieldData i <> ");"
    -- A plain field reads via the walk directly; a closure field reads its
    -- ClosureOrigin wire tuple off the slot and reflects it into a callable.
    readField (i, (f, t, _, Nothing)) = case fieldTypes of
      RenderedFieldTypes ->
        f <> ": w.child_read::<" <> t <> ">(" <> fieldSchema i <> ", " <> fieldData i <> "),"
      StructFieldTypes ->
        f <> ": w.child_read_for(" <> fieldSchema i <> ", " <> fieldData i <> ", |x: &Self| &x." <> f <> "),"
    readField (i, (f, t, _, Just (ViaTrait tr))) =
      f <> ": <" <> t <> " as " <> tr <> ">::mlc_read(w, " <> fieldSchema i <> ", " <> fieldData i <> "),"
    readField (i, (f, _, _, Just (PackMarshal raw _ from))) =
      f <> ": " <> from <> "(w.child_read::<" <> raw <> ">(" <> fieldSchema i <> ", " <> fieldData i <> ")),"
    readField (i, (f, _, _, Just (ClosureMarshal reflect))) =
      let slotRead =
            "w.child_read::<::rustmorloc::ClosureOrigin>(" <> fieldSchema i <> ", " <> fieldData i <> ")"
       in f <> ": " <> reflect slotRead <> ","
