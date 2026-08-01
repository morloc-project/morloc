{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Rust
Description : Translate 'SerialManifold' trees into Rust pool source code
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The Rust CAbi pool member: a full member with its own native marshaller
(@rustmorloc@) and dispatch, mirroring the C++ member but far simpler for v1.
Lowers each 'SerialManifold' through the shared 'LowerConfig'/'Imperative' IR,
then prints via 'RustPrinter'. Concrete Rust type names live in the morloc
stdlib as @type Rust => X = "..."@ declarations and reach the translator
pre-resolved in each 'TypeF' CVar slot, so 'rustTypeOf' mostly renders them
directly (mirroring 'cppTypeOf').

v1 scope: scalars, Str, Vector, tuples, @?T@, records (including recursive),
and cross-pool foreign calls. Closures/partial application, remote calls,
caching, and pattern evaluation raise a clear "unsupported in Rust v1" error.
-}
module Morloc.CodeGenerator.Pools.CAbi.Members.Rust
  ( translate
  , rustLang
  ) where

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import qualified Control.Monad.State.Strict as CMS
import Data.Function (on)
import Data.List (nubBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Morloc.CodeGenerator.Grammars.Common
import Morloc.CodeGenerator.Grammars.Macro (expandMacro)
import Morloc.CodeGenerator.Grammars.Translator.Imperative
  ( ArgSite (..)
  , IOwnership (..)
  , LowerConfig (..)
  , buildProgramM
  , defaultDeserialize
  , defaultFoldRules
  , defaultSerialize
  , toIType
  )
import Morloc.CodeGenerator.Namespace
import qualified Morloc.CodeGenerator.Pools.CAbi.Members.RustPrinter as RP
import Morloc.CodeGenerator.Serial (serialAstToType, shallowType)
import Morloc.Typecheck.Internal (unqualify)
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS
import qualified Morloc.Version as MV
import Morloc.Quasi

-- | Duplicated here (as in Cpp.hs) to match data/lang/rust/lang.yaml. The
-- second field is the source extension and must match lang.yaml's @extension@
-- so pool naming (@pool.rs@) and the registry's Lang agree.
rustLang :: ML.Lang
rustLang = ML.Lang "rust" "rs"

data RustState = RustState
  { rsCounter :: Int
  , rsSchemas :: Map.Map Text Int
  , rsLocalSet :: Set.Set Int
  , rsRemoteSet :: Set.Set Int
  , rsDebugInfo :: Int -> (Text, Text)
  -- ^ Per-manifold @(userName, srcloc)@ (from 'makeManifoldDebugInfoLookup'),
  -- baked into each manifold's 'FrameGuard' frame line for error tracebacks.
  , rsRecmap :: RecMap
  -- ^ Unified record types used in this pool; drives struct generation, the
  -- concrete struct name in 'rustTypeOf', and per-record marshalling impls.
  , rsCScope :: Scope
  -- ^ The merged Rust concrete typedef scope; resolves a recursive back-ref
  -- (@RecF@) to its concrete struct name (as C++'s translatorCScope does).
  , rsSrcTypeVarMask :: Map.Map SrcName [(Bool, Bool)]
  -- ^ Per sourced function, per parameter position: @(isBareTypeVar,
  -- isFunctionParam)@ from the declared morloc signature. @isBareTypeVar@: the
  -- parameter is a BARE type variable, so it is passed by reference (@&A@) even
  -- at a Copy instantiation (the sourced Rust fn is generic over @&A@).
  -- @isFunctionParam@: the parameter is itself function-typed (@F: Fn@), so a
  -- function-valued argument is a genuine closure passed BY VALUE; when False,
  -- a function-typed argument is a fully-applied sub-manifold VALUE, passed like
  -- data (borrowed when non-'Copy').
  }

instance Defaultable RustState where
  defaultValue = RustState 0 Map.empty Set.empty Set.empty (\_ -> ("", "")) [] Map.empty Map.empty

-- | The ownership environment: the borrowed (@&T@) parameter indices of the
-- manifold whose body is currently being lowered ('oeCurrent') and of its
-- enclosing caller ('oeParent'). This is a lexically scoped Reader environment,
-- extended by 'local' at each manifold boundary -- NOT mutable state -- so a
-- variable's ownership is a pure function of the lexical context, immune to the
-- fold's evaluation order. A manifold-call argument or captured context
-- argument is named by index-aliasing after the caller's variables, so its
-- ownership is read from 'oeParent' rather than 'oeCurrent'.
data OwnEnv = OwnEnv
  { oeCurrent :: Set.Set Int
  , oeParent :: Set.Set Int
  , oeShared :: Set.Set Int
  -- ^ Indices used at more than one point in the current manifold (the "shared"
  -- set, see 'sharedIndicesSM'). A non-'Copy' shared local must be BORROWED at
  -- reference sinks and CLONED at owned sinks so it survives every use -- never
  -- moved. Kept SEPARATE from 'oeCurrent' (a shared owned local is a value, not
  -- a @&T@ reference: tagging it 'BorrowedRef' would miscompile reference sinks).
  , oeParentShared :: Set.Set Int
  -- ^ The enclosing (caller) manifold's shared set. A captured context argument
  -- names a caller variable, so a SHARED non-'Copy' capture must be cloned into
  -- the @move||@ closure (the move happens at closure formation, so the clone is
  -- hoisted before it) to leave the original live for the caller's other uses.
  }

emptyOwnEnv :: OwnEnv
emptyOwnEnv = OwnEnv Set.empty Set.empty Set.empty Set.empty

type RustM = ReaderT OwnEnv (CMS.StateT RustState Identity)

getCounter :: RustM Int
getCounter = do
  s <- CMS.get
  CMS.put s {rsCounter = rsCounter s + 1}
  return (rsCounter s)

resetCounter :: RustM ()
resetCounter = CMS.modify $ \s -> s {rsCounter = 0}

rustRegisterSchema :: Text -> RustM Int
rustRegisterSchema schema = do
  s <- CMS.get
  case Map.lookup schema (rsSchemas s) of
    Just sid -> return sid
    Nothing -> do
      let sid = Map.size (rsSchemas s)
      CMS.put s {rsSchemas = Map.insert schema sid (rsSchemas s)}
      return sid

getRustSchemaTable :: RustM [Text]
getRustSchemaTable = do
  m <- CMS.gets rsSchemas
  return $ map fst $ sortBy (comparing snd) $ Map.toList m

-- | Render a native (non-serialized) 'TypeF' to a Rust type string. The
-- concrete name has already been resolved into the CVar slot by the Realize
-- pass against the Rust concrete scope, so terminal variables render directly;
-- parameterized types expand their @$N@ macro from the stdlib declaration.
rustTypeOf :: TypeF -> RustM MDoc
rustTypeOf = f
  where
    f :: TypeF -> RustM MDoc
    f (UnkF (FV _ x)) = return (pretty x)
    f (VarF (FV _ x)) = return (pretty x)
    f (AppF t ts) = do
      t' <- f t
      let (typeTs, kindCount) = partitionKindArgsF ts
      ts' <- mapM f typeTs
      return . pretty $ expandMacro (render t') (map render ts') kindCount
    -- A recursive optional (?T where T points back to a containing record)
    -- must break the cycle with Box (an Option<T> of an infinite-size T won't
    -- compile). None == absent matches the voidstar single-relptr Optional.
    f (OptionalF t@(RecF _)) = do
      t' <- f t
      return $ "Option<Box<" <> t' <> ">>"
    f (OptionalF t) = do
      t' <- f t
      return $ "Option<" <> t' <> ">"
    f (NatLitF _) = return mempty
    f NatVoidF = return mempty
    f (StrLitF _) = return mempty
    f StrVoidF = return mempty
    -- A function type renders as a monomorphic `impl Fn` closure (by-reference
    -- arguments, by-value result), matching 'rustArgType' on a 'Function'.
    f (FunF ts t) = do
      argTs <- mapM (\a -> ("&" <>) <$> f a) ts
      retT <- f t
      return $ "impl Fn" <> tupled argTs <+> "->" <+> retT
    -- Effects erase for type rendering: an effectful value's rendered type is
    -- its result type. Effect sequencing is carried by do-block thunks + eval,
    -- not by the type.
    f (EffectF _ t) = f t
    -- Autogenerated record: resolve the concrete struct name from the recmap
    -- (keyed by the record FVar + field keys).
    f (NamF _ v@(FV _ (CV "struct")) _ rs) = do
      recmap <- CMS.gets rsRecmap
      case lookup (v, map fst rs) recmap of
        Just rec -> return (recName rec)
        Nothing -> error $ "Rust: record missing from recmap: " <> show v
    -- User-mapped record: the concrete struct name is the CVar text.
    f (NamF _ (FV _ (CV s)) _ _) = return (pretty s)
    -- Back-reference to a recursive record: resolve the concrete struct name
    -- via the concrete scope (the CVar slot is unreliable after weave).
    f (RecF (FV gv@(TV gvText) (CV cv)))
      | cv /= "struct" && cv /= gvText = return (pretty cv)
      | otherwise = do
          cscope <- CMS.gets rsCScope
          case Map.lookup gv cscope of
            Just ((_, body, _, _, _) : _) | Just name <- bodyName body -> return (pretty name)
            _ -> error $ "Rust: recursive record `" <> T.unpack gvText <> "` has no concrete mapping"

    -- Outer name of a concrete-scope typedef body, when it contributes a name.
    bodyName :: TypeU -> Maybe Text
    bodyName (VarU (TV n)) = Just n
    bodyName (AppU (VarU (TV n)) _) = Just n
    bodyName (NamU _ (TV n) _ _) = Just n
    bodyName _ = Nothing

-- | Whether a native type is Rust @Copy@ (freely duplicable, so a sourced-call
-- argument is passed by value rather than borrowed). @Copy@: scalar numerics,
-- @bool@, unit, and tuples/optionals whose leaves are all @Copy@. Not @Copy@:
-- @Str@ (String), lists (Vec), records. Must be EXACT: borrowing a @Copy@ value
-- (@&i64@ into an @i64@ parameter) is a type error, so @Copy@-ness is decided
-- from the resolved concrete type name, which is robust to type aliases.
rustIsCopy :: TypeF -> Bool
rustIsCopy (OptionalF t) = rustIsCopy t
rustIsCopy (AppF (VarF (FV (TV gv) _)) ts)
  | T.isPrefixOf "Tuple" gv = all rustIsCopy (fst (partitionKindArgsF ts))
rustIsCopy (VarF (FV _ (CV cv))) = cv `elem` copyScalars
rustIsCopy (UnkF (FV _ (CV cv))) = cv `elem` copyScalars
rustIsCopy _ = False

-- Concrete Rust @Copy@ scalar type names (from the root-rust @type Rust => ...@
-- mappings). A custom mapping to a non-standard @Copy@ type would be treated as
-- non-@Copy@ (borrowed) -- acceptable for the standard numeric/bool set.
copyScalars :: [Text]
copyScalars =
  [ "i8", "i16", "i32", "i64"
  , "u8", "u16", "u32", "u64"
  , "f32", "f64", "bool", "()"
  ]

-- | The Rust type of a manifold argument: a serial arg is an incoming packet
-- pointer; a native arg is its rendered native type.
rustArgType :: TypeM -> RustM MDoc
rustArgType (Serial _) = return "*const u8"
rustArgType Passthrough = return "*const u8"
rustArgType (Native t) = rustTypeOf (typeFof t)
-- A function-typed parameter (a morloc-defined combinator abstracting over a
-- function, e.g. `flip`) becomes a monomorphic `impl Fn` closure parameter:
-- every closure argument is a shared reference (the HOF calling convention),
-- the result is by value. morloc monomorphizes manifolds, so the argument and
-- result types are concrete -- no generic manifold parameters are needed.
-- `impl Fn` is zero-cost (rustc re-monomorphizes per closure type at each call
-- site, matching morloc's signature-level manifold dedup).
rustArgType (Function ts t) = do
  argTs <- mapM closureParamType ts
  retT <- rustReturnType t
  return $ "impl Fn" <> tupled argTs <+> "->" <+> retT

-- | The Rust return type of a manifold: a serial result is an owned packet.
rustReturnType :: TypeM -> RustM MDoc
rustReturnType (Function _ o) = rustReturnType o
rustReturnType (Serial _) = return "*mut u8"
rustReturnType Passthrough = return "*mut u8"
rustReturnType (Native t) = rustTypeOf (typeFof t)

rustArgOf :: Arg TypeM -> RustM MDoc
rustArgOf a@(Arg _ t) = do
  ts <- rustArgType t
  -- Idiomatic asymmetric passing: a Copy scalar parameter is by value (`i64`);
  -- a non-Copy parameter (Str/Vec/record) is a shared reference (`&T`). Both
  -- are `Copy` at the manifold-param level (`&T` is Copy), so a value fans out
  -- to several callees with no move. Serial/passthrough args are Copy pointers.
  let ts' = case t of
        Native tf | not (rustIsCopy tf) -> "&" <> ts
        _ -> ts
  return $ argNamer a <> ":" <+> ts'

-- | Adapt a higher-order-function closure's by-reference argument to a
-- manifold parameter: a Copy scalar parameter is taken by value, so deref the
-- reference; a non-Copy parameter is taken by reference, so forward it
-- unchanged. Used to bridge a uniformly-by-reference HOF closure to a
-- manifold's asymmetric parameter convention.
rustBridgeArg :: TypeM -> MDoc -> MDoc
rustBridgeArg (Native tf) name | rustIsCopy tf = "*" <> name
rustBridgeArg _ name = name

-- | The Rust type of a HOF closure parameter: a shared reference to the
-- element/accumuland value type (a HOF passes every closure argument by ref).
closureParamType :: TypeM -> RustM MDoc
closureParamType (Native tf) = ("&" <>) <$> rustTypeOf tf
closureParamType _ = return "_"

-- | Render a getter/bracket pattern. A getter (@.0@/@.field@, possibly chained
-- or multi-sibling) becomes native field access; a bracket index/slice calls
-- the sourced @__access_index__@/@__get_slice__@ (@morloc_at@/@morloc_slice@),
-- borrowing the list receiver. Setters, string interpolation, and brackets
-- nested inside a getter chain are not yet supported (unused by the stdlib).
rustEvalPattern :: TypeF -> Pattern -> [MDoc] -> RustM MDoc
rustEvalPattern _ (PatternStruct sel) [m] =
  return $ case ungroup sel of
    [ss] -> writeSelectorRust m ss
    sss -> tupled (map (writeSelectorRust m) sss)
rustEvalPattern _ PatternBracketIndex [i, m] =
  return $ "morloc_at" <> tupled [i, "&(" <> m <> ")"]
rustEvalPattern _ PatternBracketSlice [start, stop, step, m] =
  return $ "morloc_slice" <> tupled [start, stop, step, "&(" <> m <> ")"]
rustEvalPattern _ p args =
  error $ "Rust v1: unsupported pattern " <> show p <> " with " <> show (length args) <> " args"

-- | Walk an (ungrouped) selector, emitting Rust field access: @.i@ for a tuple
-- index, @.field@ for a record key (keyword-escaped to match the struct).
writeSelectorRust :: MDoc -> [Either Int Text] -> MDoc
writeSelectorRust d [] = d
writeSelectorRust d (Right k : rs) = writeSelectorRust (d <> "." <> rustFieldIdent (Key k)) rs
writeSelectorRust d (Left i : rs) = writeSelectorRust (d <> "." <> pretty i) rs

-- | Adapt a partial application's captured context argument to the closure
-- manifold's parameter. The captured value is a variable in the ENCLOSING
-- (caller) scope, so its ownership is read from 'oeParent': a non-'Copy' context
-- parameter is a reference sink, a 'Copy' one is a by-value (owned) sink --
-- matching how a manifold call passes its arguments.
rustBridgeContext :: Int -> TypeM -> MDoc -> RustM MDoc
-- A captured FUNCTION value (a combinator like `any`/`all` whose inner lambda
-- closes over a function argument) needs by-reference capture AND a by-reference
-- inner manifold parameter (`&F: Fn` stays re-callable across the per-element
-- calls; a by-value `impl Fn` would move out of an `Fn` closure). That param
-- form is not yet emitted, so reject it rather than emit code that miscompiles.
rustBridgeContext _ (Function _ _) _ =
  error "Rust v1: a captured function value (e.g. a combinator closing over a function argument, like `any`/`all`) is not yet supported"
rustBridgeContext i (Native tf) name = do
  parent <- asks oeParent
  let own = if Set.member i parent then BorrowedRef else Owned
  return $ if rustIsCopy tf then rustOwn own tf name else rustRef own name
rustBridgeContext _ _ name = return $ "&(" <> name <> ")"

-- | Per-index variable-use counts over a manifold, for the "shared" (used at
-- more than one point) determination. ALL uses are counted (borrows INCLUDED --
-- excluding them is unsound: an owned use followed by a later borrow would slip
-- through to a use-after-move); the two arms of an 'IfN' are combined by MAX
-- (they are mutually exclusive at runtime, so a value used once per arm is one
-- use per run); a captured value (a nested manifold's context arg) counts as a
-- use. Descends into nested manifolds; any resulting over-count is safe (an
-- extra clone, never a missed one).
varUseCountOps :: FoldManifoldM Identity (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int) (Map.Map Int Int)
varUseCountOps =
  FoldManifoldM
    { opSerialManifoldM = \full@(SerialManifold_ _ _ form _ _) -> return (ctxOf form `add` foldlSM add mempty full)
    , opNativeManifoldM = \full@(NativeManifold_ _ _ form _) -> return (ctxOf form `add` foldlNM add mempty full)
    , opSerialExprM = \node -> return (foldlSE add mempty node)
    , opNativeExprM = \node -> return $ case node of
        BndVarN_ _ i -> Map.singleton i 1
        LetVarN_ _ i -> Map.singleton i 1
        IfN_ _ c t e -> c `add` Map.unionWith max t e
        _ -> foldlNE add mempty node
    , opSerialArgM = \node -> return (foldlSA add mempty node)
    , opNativeArgM = \node -> return (foldlNA add mempty node)
    }
  where
    add = Map.unionWith (+)
    ctxOf form = Map.fromList [(i, 1) | Arg i _ <- manifoldContext form]

sharedOf :: Map.Map Int Int -> Set.Set Int
sharedOf = Map.keysSet . Map.filter (>= 2)

-- | The shared indices of a serial manifold: variables used at more than one
-- point in its body (see 'varUseCountOps').
sharedIndicesSM :: SerialManifold -> Set.Set Int
sharedIndicesSM = sharedOf . runIdentity . foldSerialManifoldM varUseCountOps

-- | The shared indices of a native manifold (used as an argument). Counting its
-- own context args (captures from the enclosing scope) only over-clones (safe).
sharedIndicesNM :: NativeManifold -> Set.Set Int
sharedIndicesNM = sharedOf . runIdentity . foldNativeManifoldM varUseCountOps

-- | The spec's @owner(e, Gamma)@ as a PURE function: the ownership of a native
-- expression given @Gamma@ (the borrowed @&T@ parameter indices) and @Shared@
-- (indices used at more than one point). Because both are explicit arguments
-- (not fold state), the result is correct regardless of the fold's evaluation
-- order. A bound variable is a @&T@ reference only when it is a borrowed
-- parameter; a shared value is a place (borrowed at reference sinks, cloned at
-- owned sinks -- never moved); a field of a borrowed value is a place;
-- everything else is an owned value.
ownerPure :: Set.Set Int -> Set.Set Int -> NativeExpr -> IOwnership
-- A bound variable and a let variable both render as `n<i>` and share the index
-- namespace, so both classify the same way ('classifyIndex').
ownerPure g s (BndVarN _ i) = classifyIndex g s i
ownerPure g s (LetVarN _ i) = classifyIndex g s i
ownerPure g s (ReturnN e) = ownerPure g s e
ownerPure g s (NativeLetN _ _ e) = ownerPure g s e
ownerPure g s (SerialLetN _ _ e) = ownerPure g s e
ownerPure g s (AppExeN _ (PatCallP (PatternStruct _)) [recv]) =
  case argOwnerPure g s recv of
    Owned -> Owned
    _ -> BorrowedPlace
ownerPure _ _ _ = Owned

-- | Classify a variable index: a borrowed parameter (in @Gamma@) is a @&T@
-- reference; a shared value (in @Shared@, used at more than one point) is a place
-- (borrowed at reference sinks, cloned at owned sinks, never moved); otherwise it
-- is an owned value that may be moved.
classifyIndex :: Set.Set Int -> Set.Set Int -> Int -> IOwnership
classifyIndex g s i
  | Set.member i g = BorrowedRef
  | Set.member i s = BorrowedPlace
  | otherwise = Owned

-- | Ownership of an argument's value. An expression argument is read in the
-- current scope (@Gamma@, @Shared@); a manifold argument renders as a CALL that
-- materializes an owned value (a manifold's return type is always the owned @T@,
-- and the return sink clones a borrowed body -- see 'ReturnN_'), so it is 'Owned'
-- regardless of the sub-manifold body's internal ownership.
argOwnerPure :: Set.Set Int -> Set.Set Int -> NativeArg -> IOwnership
argOwnerPure g s (NativeArgExpr e) = ownerPure g s e
argOwnerPure _ _ (NativeArgManifold _) = Owned

rustOwnership :: NativeExpr -> RustM IOwnership
rustOwnership e = do
  g <- asks oeCurrent
  s <- asks oeShared
  return (ownerPure g s e)

-- | Adapt an expression to an owned value at an owned sink (a container element,
-- a return, a let RHS, a by-value parameter): clone a borrowed non-'Copy' value,
-- dereference a borrowed 'Copy' reference, read a 'Copy' place as-is.
rustOwn :: IOwnership -> TypeF -> MDoc -> MDoc
rustOwn Owned _ x = x
rustOwn BorrowedRef tf x
  | rustIsCopy tf = "*" <> x
  | otherwise = x <> ".clone()"
rustOwn BorrowedPlace tf x
  | rustIsCopy tf = x
  | otherwise = x <> ".clone()"

-- | Adapt an expression to a shared reference at a reference sink (a @&T@
-- parameter or a closure application): an owned value or a borrowed place is
-- borrowed; a value that is already a reference is forwarded unchanged.
rustRef :: IOwnership -> MDoc -> MDoc
rustRef BorrowedRef x = x
rustRef _ x = "&(" <> x <> ")"

-- | The safe by-reference closure that adapts a manifold to a higher-order
-- function's @F: Fn@ (a bare @unsafe fn@ does not implement @Fn@). Remaining
-- (bound) parameters become the closure's typed @&T@ parameters -- explicit
-- types are required so rustc infers the higher-ranked @for<'a> Fn(&'a T)@
-- signature -- and are bridged to the manifold's parameter form; captured
-- context args are applied. Shared by 'lcMakePass' (no context) and
-- 'lcMakeLambda' (a partial application).
rustClosureWrapper :: MDoc -> [Arg TypeM] -> [Arg TypeM] -> RustM MDoc
rustClosureWrapper mname ctxArgs boundArgs = do
  boundTyped <- mapM (\a@(Arg _ t) -> do
                        base <- closureParamType t
                        return (argNamer a <> ":" <+> base)) boundArgs
  ctxParts <- mapM bridgeCapture ctxArgs
  let cloneLets = concatMap fst ctxParts
      ctxDocs = map snd ctxParts
      boundDocs = [rustBridgeArg t (argNamer a) | a@(Arg _ t) <- boundArgs]
      call = mname <> tupled (ctxDocs ++ boundDocs)
      closure = "move |" <> hcat (punctuate ", " boundTyped) <> "| unsafe {" <+> call <+> "}"
  -- A shared non-Copy capture is cloned in a block that brackets the closure, so
  -- the `move` captures the clone and the original stays live for the caller.
  return $ case cloneLets of
    [] -> closure
    _ -> "{" <+> hsep cloneLets <+> closure <+> "}"

-- | Bridge one captured context argument: returns any hoisted clone bindings and
-- the value passed into the inner manifold call. A SHARED non-'Copy' owned
-- capture (used elsewhere by the caller) is cloned to a fresh local and the call
-- takes a reference to the clone; everything else defers to 'rustBridgeContext'.
bridgeCapture :: Arg TypeM -> RustM ([MDoc], MDoc)
bridgeCapture a@(Arg i t) = do
  parentBorrowed <- asks oeParent
  parentShared <- asks oeParentShared
  case t of
    Native tf
      | Set.member i parentShared
          && not (rustIsCopy tf)
          && not (Set.member i parentBorrowed) ->
          let name = argNamer a
              cloneVar = name <> "_c"
           in return (["let" <+> cloneVar <+> "=" <+> name <> ".clone();"], rustRef Owned cloneVar)
    _ -> do
      d <- rustBridgeContext i t (argNamer a)
      return ([], d)

-- | Per sourced Rust function, the per-parameter "is a bare type variable"
-- mask from the DECLARED morloc signature. A concrete parameter (Int, Str,
-- [a], record) is False; a bare @a@ is True. Read from 'stateSignatures'
-- (still live at codegen): a plain sourced function is 'Monomorphic' with its
-- type variables 'ForallU'-quantified; a typeclass method is 'Polymorphic',
-- whose per-instance 'termGeneral' has been monomorphized, so the class-level
-- signature + the instance's 'classVars' carry the real type variables.
buildSrcTypeVarMask :: MorlocMonad (Map.Map SrcName [(Bool, Bool)])
buildSrcTypeVarMask = do
  GMap _ sigmap <- MM.gets stateSignatures
  tcls <- MM.gets stateTypeclasses
  return . Map.fromList . concatMap (fromSig tcls) . Map.elems $ sigmap
  where
    fromSig _ (Monomorphic (TermTypes (Just et) srcs _)) =
      let (qs, t') = unqualify (etype et)
       in entries (Set.fromList qs) t' srcs
    fromSig _ (Monomorphic (TermTypes Nothing _ _)) = []
    fromSig tcls (Polymorphic _ methodEV classET tts) =
      let cvars = maybe [] classVars (Map.lookup methodEV tcls)
          (fs, t') = unqualify (etype classET)
          qs = Set.fromList (fs <> cvars)
       in concat [entries qs t' srcs | TermTypes _ srcs _ <- tts]

    entries qs t' srcs =
      [(srcName s, paramMask qs t') | (_, isrc) <- srcs, let s = val isrc, srcLang s == rustLang]

    paramMask qs (FunU args _) = map (\a -> (isBareVar qs a, isFunParam a)) args
    paramMask _ _ = []

    isBareVar qs (VarU v) = Set.member v qs
    isBareVar _ _ = False

    isFunParam (FunU _ _) = True
    isFunParam _ = False

translate :: [Source] -> [SerialManifold] -> MorlocMonad Script
translate srcs es = do
  let rustSrcs = unique $ mapMaybe srcPath [s | s <- srcs, srcLang s == rustLang]
  includeDocs <- mapM rustSourceInclude rustSrcs

  debugInfo <- makeManifoldDebugInfoLookup

  -- Merge the general typedef scope into the Rust concrete scope so a record
  -- field declared via a general alias resolves through its Rust mapping (as
  -- the C++ member does). Concrete entries win on collision.
  universalScopeMap <- MM.gets stateUniversalConcreteTypedefs
  generalScope <- MM.gets stateUniversalGeneralTypedefs
  let rustScope = fromMaybe Map.empty (Map.lookup rustLang universalScopeMap)
      mergedRustScope = Map.union rustScope generalScope
      recmap = unifyRecords . concatMap collectRecords $ es

  srcTypeVarMask <- buildSrcTypeVarMask
  let st0 = defaultValue {rsDebugInfo = debugInfo, rsRecmap = recmap, rsCScope = mergedRustScope, rsSrcTypeVarMask = srcTypeVarMask}
      code = CMS.evalState (runReaderT (makeRustCode includeDocs es) emptyOwnEnv) st0

  maker <- makeTheMaker
  poolSubdir <- MM.getModuleName

  return $
    Script
      { scriptBase = "pool"
      , scriptLang = rustLang
      , scriptCode = "." :/ Dir "pools" [Dir poolSubdir [File "pool.rs" (Code (subVersion (render code)))]]
      , scriptMake = maker
      }

-- | Emit an @include!@ of a sourced Rust file at the pool crate root, so its
-- @pub fn@s become directly callable by name (mirroring C++ @#include@).
rustSourceInclude :: Path -> MorlocMonad MDoc
rustSourceInclude p = do
  absPath <- liftIO $ MS.canonicalizePath p
  return $ "include!(" <> dquotes (pretty absPath) <> ");"

subVersion :: Text -> Text
subVersion = T.replace "__MORLOC_VERSION__" (MT.pack MV.versionStr)

makeRustCode :: [MDoc] -> [SerialManifold] -> RustM MDoc
makeRustCode includeDocs es = do
  structDocs <- generateRustStructs es
  program <- buildProgramM Map.empty Map.empty includeDocs es translateSegment getRustSchemaTable Map.empty
  -- structDocs (struct defs + marshalling impls) go in the schema-table section.
  return $ RP.printProgram structDocs [] [] program

-- | Collect every record type used in these manifolds, keyed by its FVar, with
-- one representative field list (from a use site). Unlike the shared recmap
-- (which only collects @= "struct"@ records), this also collects user-mapped
-- records (@record Rust => X = "Name"@) so their marshalling impls are emitted.
collectRustRecords :: [SerialManifold] -> [(FVar, [(Key, TypeF)])]
collectRustRecords =
  -- One entry per record (keyed by general TVar); keep the first field list.
  nubBy ((==) `on` \(FV gv _, _) -> gv)
    . concatMap (runIdentity . foldWithSerialManifoldM fm)
  where
    fm = defaultValue {opFoldWithNativeExprM = ne, opFoldWithSerialExprM = se}
    ne _ (DeserializeN_ t s xs) = return $ xs <> seek t <> seek (serialAstToType s)
    ne efull e = return $ foldlNE (<>) (seek (typeFof efull)) e
    se _ (SerializeS_ s xs) = return $ seek (serialAstToType s) <> xs
    se _ e = return $ foldlSE (<>) [] e

    seek :: TypeF -> [(FVar, [(Key, TypeF)])]
    seek (NamF _ v _ rs) = (v, rs) : concatMap (seek . snd) rs
    seek (AppF t ts) = concatMap seek (t : ts)
    seek (FunF ts t) = concatMap seek (t : ts)
    seek (OptionalF t) = seek t
    seek (EffectF _ t) = seek t
    seek _ = []

-- | Emit a struct definition (only for autogenerated @= "struct"@ records) plus
-- @ToVoidstar/FromVoidstar@ impls for every record used in the pool. User-mapped
-- records provide their own struct (in sourced Rust), so only the impls are
-- emitted for them.
generateRustStructs :: [SerialManifold] -> RustM [MDoc]
generateRustStructs es = concat <$> mapM makeOne (collectRustRecords es)
  where
    makeOne :: (FVar, [(Key, TypeF)]) -> RustM [MDoc]
    makeOne (v@(FV gv _), rs) = do
      (name, isAutogen) <- case v of
        FV _ (CV "struct") -> do
          recmap <- CMS.gets rsRecmap
          case lookup (v, map fst rs) recmap of
            Just rec -> return (recName rec, True)
            Nothing -> error $ "Rust: autogenerated record missing from recmap: " <> show v
        FV _ (CV s) -> return (pretty s, False)
      fields <- mapM (oneField gv name) rs
      let impls = RP.printRecordImpls name fields
      return $
        if isAutogen
          then [RP.printRustStruct name [(fld, ty) | (fld, ty, _) <- fields], impls]
          else [impls]

    -- Render one field's type, Box'ing a self-optional (?self) so the recursive
    -- cycle is Sized (a `[self]` field is already broken by Vec's indirection).
    oneField :: TVar -> MDoc -> (Key, TypeF) -> RustM (MDoc, MDoc, Bool)
    oneField selfGv selfName (k, ty) = do
      ty' <- case ty of
        OptionalF inner | refsRecord selfGv inner -> return $ "Option<Box<" <> selfName <> ">>"
        _ -> rustTypeOf ty
      return (rustFieldIdent k, ty', isVarWidthF ty)

    -- True when a type is an immediate reference back to the enclosing record.
    refsRecord :: TVar -> TypeF -> Bool
    refsRecord g (NamF _ (FV g' _) _ _) = g == g'
    refsRecord g (VarF (FV g' _)) = g == g'
    refsRecord g (RecF (FV g' _)) = g == g'
    refsRecord _ _ = False

-- | Conservative: only pure fixed-width scalar (and all-fixed tuple) fields let
-- a record short-circuit shm_size to schema.width. Anything variable-region
-- (Str/Vec/Option/record) or unknown counts as variable (always safe -- a false
-- \"variable\" just uses the correct per-field sum).
isVarWidthF :: TypeF -> Bool
isVarWidthF (VarF (FV (TV gv) _)) = gv == "Str"
isVarWidthF (AppF (VarF (FV (TV gv) _)) ts)
  | T.isPrefixOf "Tuple" gv = any isVarWidthF (fst (partitionKindArgsF ts))
isVarWidthF _ = True

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

-- | The bound-variable indices that are borrowed (@&T@) parameters of a
-- manifold: its non-'Copy' NATIVE parameters. Serial (dispatch) parameters
-- deserialize to owned locals, so they are excluded.
borrowedIndicesOfForm :: (HasTypeM t) => ManifoldForm (Or TypeS TypeF) t -> Set.Set Int
borrowedIndicesOfForm form =
  Set.fromList [i | Arg i tm <- typeMofForm form, isBorrowed tm]
  where
    isBorrowed (Native tf) = not (rustIsCopy tf)
    isBorrowed _ = False

-- | Lower a manifold body in its own ownership scope: 'oeCurrent' becomes the
-- manifold's borrowed parameter indices, 'oeParent' the enclosing manifold's
-- ('oeCurrent' from before), and 'oeShared' the manifold's used-more-than-once
-- indices. Because this extends the Reader environment with 'local' rather than
-- mutating state, the scope is purely lexical -- sibling and nested manifolds
-- cannot corrupt each other's view, and the answer does not depend on the fold's
-- evaluation order.
withManifoldScope :: Set.Set Int -> Set.Set Int -> RustM a -> RustM a
withManifoldScope borrowed shared =
  local (\e -> OwnEnv {oeCurrent = borrowed, oeParent = oeCurrent e, oeShared = shared, oeParentShared = oeShared e})

-- | Run an action in the caller's ownership scope, by making 'oeCurrent' the
-- caller's set ('oeParent'). Used when rendering a manifold call whose arguments
-- are named by index-aliasing after the caller's variables.
rustWithCallerScope :: RustM a -> RustM a
rustWithCallerScope = local (\e -> e {oeCurrent = oeParent e, oeShared = oeParentShared e})

-- | Track each manifold's borrowed parameters as the body is folded, so
-- 'rustOwnership' can tell a borrowed @&T@ parameter from an owned value.
rustSurround :: SurroundManifoldM RustM PoolDocs PoolDocs PoolDocs PoolDocs (TypeS, PoolDocs) (TypeM, PoolDocs)
rustSurround =
  defaultValue
    { surroundSerialManifoldM = \recurse sm@(SerialManifold _ _ form _ _) ->
        withManifoldScope (borrowedIndicesOfForm form) (sharedIndicesSM sm) (recurse sm)
    , surroundNativeManifoldM = \recurse nm@(NativeManifold _ _ form _) ->
        withManifoldScope (borrowedIndicesOfForm form) (sharedIndicesNM nm) (recurse nm)
    }

translateSegment :: SerialManifold -> RustM MDoc
translateSegment m0 = do
  resetCounter
  mask <- CMS.gets rsSrcTypeVarMask
  e <- surroundFoldSerialManifoldM rustSurround (defaultFoldRules (rustLowerConfig mask)) m0
  return $ renderPoolDocs e

-- | The single bare-@rustc@ build command (no cargo): compile pool.rs against
-- the prebuilt @rustmorloc@/@morloc_runtime_types@ rlibs (+ transitive deps in
-- @rust-deps@) and link @libmorloc.so@ with an embedded rpath.
makeTheMaker :: MorlocMonad [SysCommand]
makeTheMaker = do
  home <- MM.asks configHome
  poolSubdir <- MM.getModuleName
  let outfile = pretty $ "pools" </> poolSubdir </> ML.makeExecutablePoolName rustLang
      src = pretty $ "pools" </> poolSubdir </> ML.makeSourcePoolName rustLang
      libDir = pretty (home </> "lib")
      rustRelease = home </> "lib" </> "rust-build" </> "release"
      depDir = pretty (rustRelease </> "deps")
      rustmorlocRlib = pretty (rustRelease </> "librustmorloc.rlib")
      -- pool.rs has exactly one direct rlib dep (rustmorloc, pinned by path);
      -- morloc_runtime_types and every other transitive dep resolve by exact
      -- metadata hash from the isolated rust-build/release/deps dir.
      cmd =
        SysRun . Code . render $
          [idoc|rustc -O --edition 2021 -L dependency=#{depDir} --extern rustmorloc=#{rustmorlocRlib} -L native=#{libDir} -l dylib=morloc -C link-arg=-Wl,-rpath,#{libDir} -o #{outfile} #{src}|]
  return [cmd]

-- | The lowering configuration. The core fields are real; the fields for
-- closures/partial application, remote calls, caching, and pattern evaluation
-- raise a clear v1-unsupported error and are unreachable for the pool shapes
-- v1 supports.
rustLowerConfig :: Map.Map SrcName [(Bool, Bool)] -> LowerConfig RustM
rustLowerConfig mask =
  LowerConfig
    { lcSrcName = \src -> pretty (srcName src)
    , lcSourcedArg = \site own tm x ->
        -- Pass each argument to match how the callee's parameter is written,
        -- adapting by the argument's ownership so no unnecessary copy is made.
        -- A non-Copy parameter (and a sourced type-variable parameter, which is
        -- generic over `&A`) is a reference sink; a Copy parameter is a by-value
        -- (owned) sink; a closure application borrows every argument to match its
        -- `Fn(&T, ...)` signature. A function argument passes BY VALUE (the
        -- callee takes it as `F: Fn`; borrowing `&closure` breaks higher-ranked
        -- closure inference).
        let (isVar, isFunParam) = case site of
              SourcedArg src i ->
                maybe (False, False) (\bs -> if i < length bs then bs !! i else (False, False)) (Map.lookup (srcName src) mask)
              _ -> (False, False)
            -- A value parameter: a Copy type is a by-value (owned) sink, a
            -- non-Copy type is a reference sink.
            byType tf = if rustIsCopy tf then rustOwn own tf x else rustRef own x
         in case tm of
              -- A function-TYPED argument is one of two things: a GENUINE closure
              -- passed to an `F: Fn` callee parameter (`isFunParam`), which goes
              -- BY VALUE (borrowing `&closure` breaks higher-ranked inference);
              -- or a fully-applied sub-manifold VALUE, whose Function type is a
              -- misnomer -- it renders as a call and the callee wants its RESULT.
              -- A sub-manifold value to a bare type-variable parameter (`isVar`,
              -- e.g. `(/) :: a -> a -> a`) is generic over `&A`, so borrow it; to
              -- any other value parameter, pass it like a Native arg of its result.
              Function _ resultT
                | isFunParam -> x
                | isVar -> rustRef own x
                | otherwise -> case resultT of
                    Native tf -> byType tf
                    _ -> rustRef own x
              -- A closure application and a sourced type-variable parameter are
              -- reference sinks; otherwise pass by the value type. (`isVar` is only
              -- ever True under 'SourcedArg'.)
              Native tf
                | ClosureArg <- site -> rustRef own x
                | isVar -> rustRef own x
                | otherwise -> byType tf
              _ -> x
    , lcOwnership = rustOwnership
    , lcArgManifoldOwnership = \_ -> return Owned
    , lcOwnArg = rustOwn
    , lcWithCallerScope = rustWithCallerScope
    -- `?T` is `Option<T>`, whose wire layout is type-driven; a widened value must
    -- be a real `Some(..)` (never a bare `T`) or put_value serializes the wrong
    -- shape. The coercion only ever wraps a non-optional inner, so this is safe.
    , lcCoerceOptional = \x -> "Some(" <> x <> ")"
    , lcTypeOf = \t -> Just . toIType <$> rustTypeOf t
    , lcSerialAstType = \s -> Just . toIType <$> rustTypeOf (serialAstToType s)
    , lcDeserialAstType = \s -> Just . toIType <$> rustTypeOf (shallowType s)
    , lcRawDeserialAstType = \s -> Just . toIType <$> rustTypeOf (serialAstToType s)
    , lcTypeMOf = \_ -> return Nothing
    , lcPackerName = \src -> pretty (srcName src)
    , lcUnpackerName = \src -> pretty (srcName src)
    , lcRecordAccessor = \_ _ record field -> record <> "." <> field
    , lcDeserialRecordAccessor = \_ k v -> v <> "." <> pretty k
    , lcTupleAccessor = \i v -> v <> "." <> pretty i
    , lcNewIndex = getCounter
    , lcPrintExpr = RP.printExpr
    , lcPrintStmt = RP.printStmt
    , lcEvalPattern = rustEvalPattern
    , lcListConstructor = \_ _ es -> "vec![" <> hcat (punctuate ", " es) <> "]"
    , lcTupleConstructor = \_ _ es -> tupled es
    , lcRecordConstructor = \recType _ _ _ rs -> do
        name <- rustTypeOf recType
        let fields = hcat (punctuate ", " [rustFieldIdent k <> ":" <+> v | (k, v) <- rs])
        return $ defaultValue {poolExpr = name <+> "{" <+> fields <+> "}"}
    , lcForeignCall = \socketFile mid args ->
        let argList = "&[" <> hcat (punctuate ", " [a <+> "as *const u8" | a <- args]) <> "]"
         in [idoc|rustmorloc::foreign_call(#{dquotes socketFile}, #{pretty mid}, #{argList})|]
    , lcRemoteCall = \_ _ _ _ -> error "Rust v1: remote calls are unsupported"
    , lcCacheBody = \_ _ _ _ _ -> error "Rust v1: cache wrapping is unsupported"
    , lcDebugWrap = \_ _ body -> return body
    , lcMakeLet = rustMakeLet
    , lcReleaseStmt = \_ -> ""
    , lcReturn = \e -> "return" <+> e <> ";"
    , lcMakeIf = rustMakeIf
    , lcMakeDoBlock = \_ stmts expr ->
        return
          ( []
          , case stmts of
              [] -> "move || {" <+> expr <+> "}"
              _ -> "move || {" <> nest 4 (line <> vsep (stmts ++ [expr])) <> line <> "}"
          )
    , lcSerialize = defaultSerialize (rustLowerConfig mask)
    , lcDeserialize = \_ -> defaultDeserialize (rustLowerConfig mask)
    , lcReifyClosure = \_ -> error "Rust v1: closure reification is unsupported"
    , lcReflectClosure = \_ _ -> error "Rust v1: closure reflection is unsupported"
    , lcMakeFunction = rustMakeFunction
    -- A bare function passed to a HOF is a closure over all its params with no
    -- captured context; a lambda/section is a closure over its remaining params
    -- with the applied args captured. Both are the same safe wrapper.
    , lcMakePass = \mname params -> rustClosureWrapper mname [] params
    , lcMakeLambda = \_sig mname contextArgs boundArgs -> rustClosureWrapper mname contextArgs boundArgs
    , lcClosureSig = \_ -> return ""
    , lcRegisterSchema = rustRegisterSchema
    }

-- | Assemble a @let@ binding at the PoolDocs level. A serialize let (mt =
-- Nothing) binds an owned packet pointer; a native let binds its native type.
-- | Does a type render as a closure (@impl Fn@)? Such a type cannot annotate a
-- @let@ binding (@impl Trait@ is only legal in argument/return position), so a
-- function-valued binding must omit its type and let Rust infer the closure.
isFunctionTypeF :: TypeF -> Bool
isFunctionTypeF t = case stripEffectF t of
  FunF _ _ -> True
  _ -> False

rustMakeLet :: (Int -> MDoc) -> Int -> Maybe TypeF -> PoolDocs -> PoolDocs -> RustM PoolDocs
rustMakeLet namer letIndex mt p1 p2 = do
  ann <- case mt of
    Just t | isFunctionTypeF t -> return ""
    Just t -> do ts <- rustTypeOf t; return (":" <+> ts)
    Nothing -> return (":" <+> "*mut u8")
  let letLine = "let" <+> namer letIndex <> ann <+> "=" <+> poolExpr p1 <> ";"
      rs = poolPriorLines p1 <> [letLine] <> poolPriorLines p2
  return $
    PoolDocs
      { poolCompleteManifolds = poolCompleteManifolds p1 <> poolCompleteManifolds p2
      , poolExpr = poolExpr p2
      , poolPriorLines = rs
      , poolPriorExprs = poolPriorExprs p1 <> poolPriorExprs p2
      , poolReturnFlag = poolReturnFlag p1 || poolReturnFlag p2
      }

-- | Native @if@ expression, bound to a fresh temp so it composes as a value.
-- The arms are already adapted to the owned result type by the shared 'IfN_'
-- lowering (via 'adaptOwnedElem'), so they splice directly into the owned let.
rustMakeIf :: NativeExpr -> PoolDocs -> PoolDocs -> PoolDocs -> RustM PoolDocs
rustMakeIf origExpr condDocs thenDocs elseDocs = do
  idx <- getCounter
  let v = helperNamer idx
  typeStr <- rustTypeOf (typeFof origExpr)
  let ifStmt =
        vsep
          [ "let" <+> v <> ":" <+> typeStr <+> "= if" <+> parens (poolExpr condDocs) <+> "{"
          , indent 4 (vsep (poolPriorLines thenDocs ++ [poolExpr thenDocs]))
          , "} else {"
          , indent 4 (vsep (poolPriorLines elseDocs ++ [poolExpr elseDocs]))
          , "};"
          ]
  return $
    PoolDocs
      { poolCompleteManifolds =
          poolCompleteManifolds condDocs <> poolCompleteManifolds thenDocs <> poolCompleteManifolds elseDocs
      , poolExpr = v
      , poolPriorLines = poolPriorLines condDocs <> [ifStmt]
      , poolPriorExprs = poolPriorExprs condDocs <> poolPriorExprs thenDocs <> poolPriorExprs elseDocs
      , poolReturnFlag = poolReturnFlag condDocs || poolReturnFlag thenDocs || poolReturnFlag elseDocs
      }

-- | Emit a manifold function definition (@unsafe fn m{i}(..) -> ..@).
-- Deduplicated per (local/remote) so a manifold visited from multiple sites is
-- defined once, matching the C++ member.
rustMakeFunction ::
  Int -> MDoc -> [Arg TypeM] -> TypeM -> [MDoc] -> MDoc -> Maybe HeadManifoldForm -> RustM (Maybe MDoc)
rustMakeFunction callIndex mname args manifoldType priorLines body headForm = do
  st <- CMS.get
  let isRemote = case headForm of
        Just HeadManifoldFormRemoteWorker -> True
        _ -> False
      done =
        if isRemote
          then Set.member callIndex (rsRemoteSet st)
          else Set.member callIndex (rsLocalSet st)
  if done
    then return Nothing
    else do
      CMS.modify $ \s ->
        if isRemote
          then s {rsRemoteSet = Set.insert callIndex (rsRemoteSet s)}
          else s {rsLocalSet = Set.insert callIndex (rsLocalSet s)}
      retStr <- rustReturnType manifoldType
      typedArgs <- mapM rustArgOf args
      let fullName = mname <> if isRemote then "_remote" else ""
          decl = "unsafe fn" <+> fullName <> tupled typedArgs <+> "->" <+> retStr
          -- Per-manifold traceback frame: on a panic unwind the FrameGuard
          -- appends this line to the thread-local trace, which dispatch_guard
          -- folds onto the throw message (mirrors the C++ member's frame).
          (userName, srclocStr) = rsDebugInfo st callIndex
          nameOut = if T.null userName then "_" else userName
          srclocSuffix = if T.null srclocStr then "" else ", " <> srclocStr
          frameText =
            "\n  at " <> nameOut <> " [rust] (mid=" <> T.pack (show callIndex) <> srclocSuffix <> ")"
          frameStmt =
            "let _mlc_frame = rustmorloc::FrameGuard::new("
              <> dquotes (pretty (RP.rustEscape frameText)) <> ");"
      return . Just $
        vsep [decl <+> "{", indent 4 (vsep (frameStmt : priorLines ++ [body])), "}"]
