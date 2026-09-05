{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Morloc.CodeGenerator.Serial
Description : Build serialization ASTs that describe how to pack\/unpack types
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Constructs 'SerialAST' trees that describe the serialization and
deserialization plan for each type. Handles format selection (JSON
vs MessagePack), packer resolution via typeclass instances, and
serializability checking. Distinct from 'Serialize' which inserts
pack\/unpack calls into the manifold tree.
-}
module Morloc.CodeGenerator.Serial
  ( makeSerialAST
  , checkPackerCoherence
  , findPackerInstances
  , PackerInstance (..)
  , wireSerialAstToType
  , containsFunT
  , serialAstHasString
  , chooseSerializationCycle
  , isSerializable
  , hasArrowHint
  , prettySerialOne
  , serialAstToType
  , shallowType
  , serialAstToMsgpackSchema
  , serialAstToGeneralSchema
  , encode64
  , decode64
  ) where

import qualified Data.Char as C
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.Text as DT
import qualified Morloc.BaseTypes as BT
import Morloc.CodeGenerator.Infer
import Morloc.CodeGenerator.Namespace
import Morloc.Data.Doc
import qualified Morloc.Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import Morloc.Typecheck.Internal (apply, qualify, substitute, subtype, unqualify)

-- | Classification of how an aliased outer type maps onto the wire form.
-- Computed by 'makeSerialAST'' from the gscope alias body's outer head.
-- The outer fv (runtime identity) is preserved separately on List/Tuple
-- shapes so the user's concrete mapping survives onto the schema hint.
data AliasShape
  = AliasIsList TypeU      -- ^ body is `List elemT`; element type carried
  | AliasIsTuple [TypeU]   -- ^ body is `Tuple_n a b ...`; field types carried
  | AliasIsOther TypeU     -- ^ body is some other AppU; route via Packable
  | AliasIsNone            -- ^ outer is its own head (no aliasing)

-- | Recurse all the way to a serializable type, parameterized by how a
-- 'SerialClosure' leaf is rendered. Only the closure leaf differs between the
-- native ('serialAstToType') and wire ('wireSerialAstToType') forms; the
-- container recursion is shared here so a new container constructor need only be
-- added once.
--
-- For SerialList, the optional dim slot is reified as a leading
-- kind-kinded (Nat) arg when present. This preserves dimensional
-- metadata in the wire schema. The macro layer (@expandMacro@) is
-- independent: macro indices @$N@ count TYPE args only, so the dim
-- slot here doesn't shift @$N@ positions in the per-language form.
serialAstToTypeWith :: ([SerialAST] -> SerialAST -> TypeF) -> SerialAST -> TypeF
serialAstToTypeWith onClosure = go
  where
    go (SerialPack _ (_, s)) = go s
    go (SerialList v (Just d) s) = AppF (VarF v) [d, go s]
    go (SerialList v Nothing s) = AppF (VarF v) [go s]
    go (SerialTuple v ss) = AppF (VarF v) (map go ss)
    go (SerialObject o n ps rs) = NamF o n ps (zip (map fst rs) (map (go . snd) rs))
    go (SerialRec v) = RecF v
    go (SerialReal x) = VarF x
    go (SerialFloat32 x) = VarF x
    go (SerialFloat64 x) = VarF x
    go (SerialInt x) = VarF x
    go (SerialInt8 x) = VarF x
    go (SerialInt16 x) = VarF x
    go (SerialInt32 x) = VarF x
    go (SerialInt64 x) = VarF x
    go (SerialUInt x) = VarF x
    go (SerialUInt8 x) = VarF x
    go (SerialUInt16 x) = VarF x
    go (SerialUInt32 x) = VarF x
    go (SerialUInt64 x) = VarF x
    go (SerialBool x) = VarF x
    go (SerialString x) = VarF x
    go (SerialIFile x) = VarF x
    go (SerialOStream x) = VarF x
    go (SerialIStream x) = VarF x
    go (SerialClosure ins out) = onClosure ins out
    go (SerialNull x) = VarF x
    go (SerialOptional _ s) = OptionalF (go s)
    -- passthrough type, it cannot be deserialized or serialized, only passed in from a different language
    go (SerialUnknown v) = UnkF v

serialAstToType :: SerialAST -> TypeF
serialAstToType =
  serialAstToTypeWith (\ins out -> FunF (map serialAstToType ins) (serialAstToType out))

-- | Like 'serialAstToType', but a closure is rendered as the given wire-tuple
-- leaf type instead of its native callable type ('FunF'). A defunctionalized
-- closure travels as the fixed tuple @(home_language, manifold_id, captured)@,
-- so a static backend serializing/raw-deserializing an aggregate that CONTAINS
-- a closure must type the closure slot as that wire tuple (the reified value),
-- not as the native function. Each backend supplies its own concrete wire-tuple
-- leaf (e.g. the C++ @std::tuple<...>@ or Rust @(String,i64,Vec<Vec<u8>>)@
-- injected via the leaf's concrete name).
wireSerialAstToType :: TypeF -> SerialAST -> TypeF
wireSerialAstToType wire = serialAstToTypeWith (\_ _ -> wire)

-- | True when a function type appears anywhere in the type (a bare function, or
-- one nested in a list/tuple/record/optional/effect). Used both by the nexus
-- (which cannot serialize a higher-order value) and the C++ member (which skips
-- generating a native struct (de)serializer for a record with a function field).
containsFunT :: Type -> Bool
containsFunT (FunT _ _) = True
containsFunT (AppT t ts) = containsFunT t || any containsFunT ts
containsFunT (NamT _ _ ts rs) = any containsFunT ts || any (containsFunT . snd) rs
containsFunT (EffectT _ t) = containsFunT t
containsFunT (OptionalT t) = containsFunT t
containsFunT _ = False

-- | Whether a serialized value can carry a native string anywhere inside it.
--
-- Used by codegen to decide, per deserialization, whether the receiving pool
-- needs the cross-pool NUL guard. A type with no string in it cannot carry an
-- interior NUL, so the check is not emitted at all and costs nothing -- which
-- is why this is answered at compile time rather than by a runtime branch.
--
-- Stream handles are excluded deliberately. Their wire form is a tagged union
-- carrying either a filesystem path, which cannot hold a NUL by POSIX rule, or
-- a bare slot id, which holds no string bytes.
serialAstHasString :: SerialAST -> Bool
serialAstHasString (SerialString _) = True
serialAstHasString (SerialList _ _ s) = serialAstHasString s
serialAstHasString (SerialTuple _ ss) = any serialAstHasString ss
serialAstHasString (SerialObject _ _ _ rs) = any (serialAstHasString . snd) rs
serialAstHasString (SerialPack _ (_, s)) = serialAstHasString s
serialAstHasString (SerialOptional _ s) = serialAstHasString s
serialAstHasString _ = False

encode64 :: Int -> String
encode64 i
  | i < 0 = error "Negative size - not in this universe my dear"
  | i < 10 = [C.chr (C.ord '0' + i)] -- 0-9
  | i < 36 = [C.chr (C.ord 'a' + i - 10)]
  | i < 62 = [C.chr (C.ord 'A' + i - 36)]
  | i == 62 = "+"
  | i == 63 = "/"
  | otherwise = "=" <> (encode64 (mod i 64)) <> (encode64 (div i 64))

decode64 :: String -> Int
decode64 (x : xs)
  | x >= '0' && x <= '9' = C.ord x - C.ord '0'
  | x >= 'a' && x <= 'z' = C.ord x - C.ord 'a' + C.ord '0'
  | x >= 'A' && x <= 'Z' = C.ord x - C.ord 'A' + C.ord 'a' + C.ord '0'
  | x == '+' = 62
  | x == '/' = 63
  | x == '=' = decode64 [head xs] + 64 * decode64 (tail xs)
  | otherwise = error "illegal character"
decode64 [] = 0

encode64D :: Int -> MDoc
encode64D i = pretty (encode64 i)

-- | Emit the msgpack schema string for a SerialAST.
--
-- Recursive records introduce two new tokens:
--
--   * @&<klen><name>@ -- declare a named schema. Emitted as a prefix on
--     the first SerialObject of a recursive record. The bytes that
--     immediately follow are the schema's body.
--   * @^<klen><name>@ -- back-reference. Emitted in place of a
--     SerialRec; the runtime parser resolves the name to the previously
--     declared Schema.
--
-- Names use the general type variable string (the FVar's TVar). The
-- declaration prefix is only emitted for records that are actually
-- back-referenced somewhere in the SerialAST -- the pre-scan
-- 'collectRecursiveNames' computes that set so non-recursive records
-- keep their previous, unprefixed wire form (no regression).
serialAstToMsgpackSchema :: SerialAST -> MDoc
serialAstToMsgpackSchema = serialAstToSchemaWith addHint

-- | The same wire schema with every concrete-type hint omitted.
--
-- A hint (@\<dict\>@, @\<Point\>@) names the container the *pool's*
-- language builds, so it is a property of the implementation rather
-- than of the interface: recompiling the same signature against a
-- different @root-*@ changes it while changing nothing a caller can
-- observe. The dispatch path needs the concrete form -- that is what
-- the pool speaks -- but anything published as the program's data
-- contract needs this one.
serialAstToGeneralSchema :: SerialAST -> MDoc
serialAstToGeneralSchema = serialAstToSchemaWith (const "")

-- | Shared schema emitter. 'addHint' is the only clause that differs
-- between the concrete and general forms, so it is the only parameter.
serialAstToSchemaWith :: (FVar -> MDoc) -> SerialAST -> MDoc
serialAstToSchemaWith addHint ast = emit ast
  where
    recNames :: Set.Set TVar
    recNames = collectRecursiveNames ast

    emit :: SerialAST -> MDoc
    emit (SerialPack v (_, s)) = addHint v <> emit s
    emit (SerialList v@(FV (TV name) _) dim s) =
      recDecl name <> addHint v <> "a" <> encodeDim dim <> emit s
      where
        encodeDim (Just (NatLitF n)) = ":" <> pretty n
        encodeDim _                  = ""  -- no slot or void-valued slot
    emit (SerialTuple v@(FV (TV name) _) ss) =
      recDecl name <> addHint v <> "t" <> encode64D (length ss) <> foldl (<>) "" (map emit ss)
    -- Table primitive (Arrow IPC buffer). Open semantics: declared columns
    -- are a *lower bound*; the runtime accepts any Arrow buffer whose schema
    -- contains at least these columns of at least these types.
    emit (SerialObject NamTable _ _ []) = "T"
    emit (SerialObject NamTable _ _ rs) =
      "T:" <> encode64D (length rs) <> foldl (<>) "" (map keypair rs)
    emit (SerialObject _ v@(FV (TV name) _) _ rs) =
      recDecl name <> addHint v <> "m" <> encode64D (length rs)
        <> foldl (<>) "" (map keypair rs)
    emit (SerialRec (FV (TV name) _)) = "^" <> encodeKey name
    emit (SerialReal v) = addHint v <> "f8" -- 64 bit float
    emit (SerialFloat32 v) = addHint v <> "f4"
    emit (SerialFloat64 v) = addHint v <> "f8"
    emit (SerialInt v) = addHint v <> "j"
    emit (SerialInt8 v) = addHint v <> "i1"
    emit (SerialInt16 v) = addHint v <> "i2"
    emit (SerialInt32 v) = addHint v <> "i4"
    emit (SerialInt64 v) = addHint v <> "i8"
    emit (SerialUInt v) = addHint v <> "u8"
    emit (SerialUInt8 v) = addHint v <> "u1"
    emit (SerialUInt16 v) = addHint v <> "u2"
    emit (SerialUInt32 v) = addHint v <> "u4"
    emit (SerialUInt64 v) = addHint v <> "u8"
    emit (SerialBool v) = addHint v <> "b"
    emit (SerialString v) = addHint v <> "s"
    -- F/O/I share a 16-byte tagged-union wire form (see
    -- morloc-runtime-types::stream_handle). The schema code selects the
    -- morloc-level type (IFile / OStream / IStream); the per-instance
    -- tag byte at the wire boundary picks the encoding (path or
    -- handle). The hint preserves the user's per-language native
    -- (uint64_t, int, bit64) so generated foreign code keeps its typed
    -- surface.
    emit (SerialIFile v) = addHint v <> "F"
    emit (SerialOStream v) = addHint v <> "O"
    emit (SerialIStream v) = addHint v <> "I"
    -- A defunctionalized closure travels as a fixed-shape tuple, independent of
    -- the closure's signature: (home_language:str, manifold_id:int,
    -- captured_arg_packets:[bytes]). The language is a string (its lang name)
    -- so the receiver builds the callback socket "pipe-<name>" directly, with
    -- no integer language enum synchronized across pools. Captured packets are
    -- opaque pre-serialized bytes (an array of uint8 arrays). Reified/reflected
    -- by the pool at the serialize boundary; the reified value IS this tuple, so
    -- the existing generic tuple/string/int/array codec carries it with no
    -- runtime changes.
    -- This literal MUST equal the schema of the wire tuple
    -- (String, Int, [[UInt8]]) as the other 'emit' clauses would render it:
    -- tuple-of-3 ("t" <> encode64D 3), then String ("s"), Int ("j"), and a
    -- variable-length list-of-list-of-UInt8 ("a" <> "a" <> "u1", empty dims).
    -- The pool reifies a real such tuple through the generic codec, so this
    -- string and the runtime bytes must stay in lockstep; keep it in sync if
    -- any tuple/list/leaf code below changes.
    emit (SerialClosure _ _) =
      "t" <> encode64D (3 :: Int) <> "s" <> "j" <> "a" <> "a" <> "u1"
    emit (SerialNull v) = addHint v <> "z"
    emit (SerialOptional v s) = addHint v <> "?" <> emit s
    emit (SerialUnknown v) = addHint v <> "*"

    keypair :: (Key, SerialAST) -> MDoc
    keypair (k, s) = encodeKey (unKey k) <> emit s

    encodeKey :: DT.Text -> MDoc
    encodeKey k = (encode64D . DT.length $ k) <> pretty k

    -- Emit @&<klen><name>@ iff this record name is the target of a
    -- SerialRec elsewhere in the tree. Otherwise the prefix is empty
    -- and the wire form is identical to the pre-recursion encoding.
    recDecl :: DT.Text -> MDoc
    recDecl name
      | Set.member (TV name) recNames = "&" <> encodeKey name
      | otherwise = ""

-- | Walk the SerialAST and collect every FVar that appears as a
-- SerialRec. The result drives the @&@ prefix emission in 'emit': we
-- only declare a name on a SerialObject when something below it
-- references that name, which keeps non-recursive records on their
-- original (unprefixed) wire form.
collectRecursiveNames :: SerialAST -> Set.Set TVar
collectRecursiveNames = go
  where
    go (SerialRec (FV v _)) = Set.singleton v
    go (SerialPack _ (_, s)) = go s
    go (SerialList _ _ s) = go s
    go (SerialTuple _ ss) = Set.unions (map go ss)
    go (SerialObject _ _ _ rs) = Set.unions (map (go . snd) rs)
    go (SerialOptional _ s) = go s
    go _ = Set.empty

-- | Emit a schema hint for a newtype boundary. For a default primitive
-- (gv is one of the built-in base types like Int, Real, Str, etc.) the
-- runtime already knows the native type from the schema tag (`f8`, `j`,
-- `i1`, ...) so no hint is needed; emitting one would clutter the schema
-- with redundant information. A non-base gv comes from a newtype or
-- alias-with-custom-form, where the runtime needs the hint to route to
-- the right packer/unpacker.
addHint :: FVar -> MDoc
addHint (FV _ (CV "")) = "" -- no hint if no concrete type is defined
addHint (FV gv (CV v))
  | isBuiltinBaseType gv = ""
  | otherwise = "<" <> pretty v <> ">"

-- | True iff @v@ names one of the hardwired base types whose default
-- per-language native form is implied by the schema tag. Newtypes and
-- user-defined aliases fall through and get a hint.
isBuiltinBaseType :: TVar -> Bool
isBuiltinBaseType v = v `elem`
  [ BT.unit, BT.bool, BT.str
  , BT.real, BT.f32, BT.f64
  , BT.int, BT.i8, BT.i16, BT.i32, BT.i64
  , BT.uint, BT.u8, BT.u16, BT.u32, BT.u64
  , BT.list
  ] || isTupleBaseType v
  where
    isTupleBaseType (TV name) = any (\k -> BT.tuple k == TV name) [2 .. 12]

-- | get only the toplevel type
--
-- See 'serialAstToType' for the dim-as-NatLitF convention used to keep macro
-- indices aligned with concrete-type bodies.
shallowType :: SerialAST -> TypeF
shallowType (SerialPack _ (p, _)) = typePackerPacked p
shallowType (SerialList v (Just d) s) = AppF (VarF v) [d, shallowType s]
shallowType (SerialList v Nothing s) = AppF (VarF v) [shallowType s]
shallowType (SerialTuple v ss) = AppF (VarF v) $ map shallowType ss
shallowType (SerialObject o n ps rs) =
  let ts = map (shallowType . snd) rs
   in NamF o n ps (zip (map fst rs) ts)
shallowType (SerialReal x) = VarF x
shallowType (SerialFloat32 x) = VarF x
shallowType (SerialFloat64 x) = VarF x
shallowType (SerialInt x) = VarF x
shallowType (SerialInt8 x) = VarF x
shallowType (SerialInt16 x) = VarF x
shallowType (SerialInt32 x) = VarF x
shallowType (SerialInt64 x) = VarF x
shallowType (SerialUInt x) = VarF x
shallowType (SerialUInt8 x) = VarF x
shallowType (SerialUInt16 x) = VarF x
shallowType (SerialUInt32 x) = VarF x
shallowType (SerialUInt64 x) = VarF x
shallowType (SerialBool x) = VarF x
shallowType (SerialString x) = VarF x
shallowType (SerialIFile x) = VarF x
shallowType (SerialOStream x) = VarF x
shallowType (SerialIStream x) = VarF x
shallowType (SerialClosure ins out) = FunF (map shallowType ins) (shallowType out)
shallowType (SerialNull x) = VarF x
shallowType (SerialOptional _ s) = OptionalF (shallowType s)
-- A back-reference re-uses the ancestor's NamF identity; downstream
-- consumers resolve it structurally via the FVar's general TVar.
-- The FVar's CVar slot is unreliable -- it carries whatever bnd-
-- protected text @weave@ produced for the position. Consumers that
-- need the language-side name look it up in cscope (e.g. C++ does
-- this in 'CppTranslator.hs' to distinguish legitimate user mappings
-- from pairEval bnd-protect leaks).
shallowType (SerialRec v) = RecF v
shallowType (SerialUnknown v) = UnkF v

-- | One @Packable@ instance: the type it packs, the wire form it packs to,
-- and the pack/unpack pair each language implements it with.
--
-- The wire form belongs to the instance rather than to a language: every
-- language that implements the instance serializes through the same form. Only
-- the sources differ.
data PackerInstance = PackerInstance
  { piHead :: TypeU
  , piWire :: TypeU
  , piSources :: Map.Map Lang (Source, Source)
  }

-- | Every @Packable@ instance in the program, with its two methods paired.
--
-- @Packable a b@ declares @pack :: a -> b@ and @unpack :: b -> a@. Which
-- instance a method was declared in is not recorded, so the pairing is
-- recovered from the signatures: a pack @a -> b@ belongs with the unpack whose
-- argument and result are those same two types the other way round. Both
-- signatures are built from one declaration and so name their variables alike,
-- which is what lets them be compared directly.
--
-- A pack matched against the wrong unpack yields a serializer that is well
-- typed and wrong, so this pairing is deliberately strict: an unmatched method
-- contributes no instance, and the type it would have served then reports that
-- no instance covers it.
-- | Reject a constructor whose instances disagree about its wire form.
--
-- A type's wire form is what two pools agree on when they share a value. Two
-- instances constrain each other only where their heads can be instantiated to
-- one type: heads with no common instance describe disjoint sets of use sites
-- and say nothing about each other. Where they do meet, the form that type
-- serializes to must be the same under both, or a value written by one language
-- is read by another under a form it was never written with.
checkPackerCoherence :: MorlocMonad ()
checkPackerCoherence = do
  instances <- findPackerInstances
  let byKey =
        Map.fromListWith
          (<>)
          [(extractKey (piHead pin), [pin]) | pin <- instances]
  mapM_ checkGroup (Map.elems byKey)
  where
    checkGroup :: [PackerInstance] -> MorlocMonad ()
    checkGroup pins = mapM_ (uncurry checkPair) (pairs pins)

    pairs :: [a] -> [(a, a)]
    pairs [] = []
    pairs (x : xs) = [(x, y) | y <- xs] <> pairs xs

    -- Head and wire are qualified over the same variables, so peeling either
    -- gives the list both were built with.
    parts :: PackerInstance -> ([TVar], TypeU, TypeU)
    parts pin =
      let (vs, h) = unqualify (piHead pin)
          (_, w) = unqualify (piWire pin)
       in (vs, h, w)

    checkPair :: PackerInstance -> PackerInstance -> MorlocMonad ()
    checkPair a b
      | wireFormsAgree (parts a) (parts b) = return ()
      | otherwise =
          MM.throwSystemError $
            "Packable instances disagree on the wire form of"
              <+> squotes (pretty (extractKey (piHead a))) <> ":"
              <> "\n" <> indent 2 (pretty (piHead a) <+> "->" <+> pretty (piWire a))
              <> "\n" <> indent 2 (pretty (piHead b) <+> "->" <+> pretty (piWire b))
              <> "\nBoth cover a common type, so a value of it would be"
              <> "\nwritten under one form and read under the other."
              <> "\nGive the type one wire form."

findPackerInstances :: MorlocMonad [PackerInstance]
findPackerInstances = do
  sigmap <- MM.gets stateTypeclasses
  let packs = methodSignatures (EV "pack") sigmap
      unpacks = methodSignatures (EV "unpack") sigmap
  return
    [ PackerInstance
        { piHead = qualify vs1 b1
        , piWire = qualify vs1 a1
        , piSources = pairByLang packSrcs unpackSrcs
        }
    | ((vs1, FunU [a1] b1), packSrcs) <- packs
    , ((vs2, FunU [a2] b2), unpackSrcs) <- unpacks
    , length vs1 == length vs2
    , a1 == b2
    , b1 == a2
    ]
  where
    methodSignatures :: EVar -> Map.Map EVar Instance -> [(([TVar], TypeU), [Source])]
    methodSignatures name sigmap = case Map.lookup name sigmap of
      (Just (Instance _ _ _ ts)) -> concatMap sigOf ts
      Nothing -> []

    sigOf :: TermTypes -> [(([TVar], TypeU), [Source])]
    sigOf (TermTypes (Just et) (map (val . snd) -> srcs) _) =
      [(unqualify (etype et), srcs)]
    sigOf (TermTypes Nothing _ _) = []

    -- A language declares at most one source per method; two are rejected
    -- earlier as ambiguous source declarations.
    pairByLang :: [Source] -> [Source] -> Map.Map Lang (Source, Source)
    pairByLang ps us =
      Map.fromList
        [(srcLang p, (p, u)) | p <- ps, u <- us, srcLang p == srcLang u]

-- Takes a map of packers with concrete type names as keys. A single concrete
-- type name may map to many single types. For example, the python type "dict"
-- might represent a Map with homogenous keys and values or many things that
-- might be objects in other languages. Similarly, the python "tuple" type maps
-- to tuples of all sizes -- each of which is a different type in both the
-- morloc general type system and many other languages. So the map contains a
-- list of possible packers. Matching the concrete type name to the right packer
-- will be done through subtyping.
makeSerialAST :: Int -> Lang -> TypeF -> MorlocMonad SerialAST
makeSerialAST m lang t0 = do
  instances <- findPackerInstances

  (_, gscope) <- getScope m lang

  -- The instances this language can serialize with, grouped by the constructor
  -- they pack.
  let typepackers =
        Map.fromListWith
          (<>)
          [ (extractKey (piHead pin), [pin])
          | pin <- instances
          , Map.member lang (piSources pin)
          ]

  -- Reset the recursion-tracking state so this invocation starts with
  -- an empty ancestor set. The state is also locally saved/restored
  -- around every NamF descent below, so this reset only matters as a
  -- safety net against state-leak across distinct entry points.
  MM.modify (\s -> s { stateSerialAncestors = Set.empty })
  makeSerialAST' gscope typepackers t0
  where
    makeSerialAST' ::
      Scope ->
      Map.Map TVar [PackerInstance] ->
      TypeF ->
      MorlocMonad SerialAST
    -- If the type is unknown in this language, then it must be a passthrough
    -- type. So it will only be represented in the serialization form. As a
    -- string, for now.
    makeSerialAST' _ _ (UnkF (FV gv _)) = do
      registry <- MM.gets stateLangRegistry |>> lrEntries
      serialType <- case Map.lookup (langName lang) registry of
        Nothing -> MM.throwSourcedError m "Unsupported language"
        (Just langRegistry) -> return $ CV (lreSerialType langRegistry)
      return $ SerialUnknown (FV gv serialType)
    makeSerialAST' gscope typepackers ft@(VarF v@(FV gv cv)) = do
      anc <- MM.gets stateSerialAncestors
      -- Cycle detection: a bare reference to a record currently being
      -- lowered is a guarded self-recursive back-edge (see seciont 3 in the
      -- recursive-fields plan -- TypeEval leaves these as VarU/VarF).
      -- Emit SerialRec instead of trying to look up a packer.
      if Set.member gv anc
        then return $ SerialRec v
        else dispatchVarF anc
      where
        -- Push the outer name onto the ancestor set for the scope of
        -- the body recursion, then restore. Mirrors the @withAncestor@
        -- in the AppF branch and the NamF branch. Required for
        -- self-recursive type aliases used without args (e.g.
        -- @type Pat = [Pat]@): without it the inner @Pat@ reference
        -- inside the list element type is walked with an empty
        -- ancestor set, recursed into again, and loops.
        withAncestorVar :: Set.Set TVar -> MorlocMonad a -> MorlocMonad a
        withAncestorVar anc action = do
          MM.modify (\s -> s { stateSerialAncestors = Set.insert gv anc })
          r <- action
          MM.modify (\s -> s { stateSerialAncestors = anc })
          return r

        dispatchVarF anc
          | finalType == BT.tableU = return $ SerialObject NamTable v [] []
          | finalType == BT.unitU = return $ SerialNull v
          | finalType == BT.boolU = return $ SerialBool v
          | finalType == BT.strU = return $ SerialString v
          | finalType == BT.realU = return $ SerialReal v
          | finalType == BT.f32U = return $ SerialFloat32 v
          | finalType == BT.f64U = return $ SerialFloat64 v
          | finalType == BT.intU = return $ SerialInt v
          | finalType == BT.i8U = return $ SerialInt8 v
          | finalType == BT.i16U = return $ SerialInt16 v
          | finalType == BT.i32U = return $ SerialInt32 v
          | finalType == BT.i64U = return $ SerialInt64 v
          | finalType == BT.uintU = return $ SerialUInt v
          | finalType == BT.u8U = return $ SerialUInt8 v
          | finalType == BT.u16U = return $ SerialUInt16 v
          | finalType == BT.u32U = return $ SerialUInt32 v
          | finalType == BT.u64U = return $ SerialUInt64 v
          | otherwise = do
              (cscope, _) <- getScope m lang
              case aliasShape (Map.member gv cscope) of
                -- @type X = [E]@: bare alias whose body is list-shaped.
                -- The emitted SerialList carries an FVar whose GV is the
                -- outer alias name (so the @&X@ declaration matches any
                -- @^X@ back-reference inside) but whose CV is the
                -- LANGUAGE-LEVEL list constructor name (so the runtime
                -- schema hint -- e.g. @list@ in Python -- routes to the
                -- right container path). Using the alias's own CV (which
                -- defaults to the alias name itself when there is no
                -- explicit @type Lang => Alias = "..."@ mapping) would
                -- surface as an "Unexpected array hint" at deserialize
                -- time. Same rationale applies to the tuple arm below.
                AliasIsList elemU -> do
                  FV _ listCv <- inferConcreteVar lang (Idx m BT.list)
                  elemTf <- inferConcreteType lang (Idx m (typeOf elemU))
                  elemAST <- withAncestorVar anc (makeSerialAST' gscope typepackers elemTf)
                  return $ SerialList (FV gv listCv) Nothing elemAST
                -- @type X = (A, B, ...)@: bare alias whose body is
                -- tuple-shaped. Hint comes from the n-tuple constructor
                -- (Tuple2/Tuple3/...), GV stays as the alias name.
                AliasIsTuple bodyArgs -> do
                  FV _ tupleCv <- inferConcreteVar lang (Idx m (BT.tuple (length bodyArgs)))
                  elemTfs <- mapM (inferConcreteType lang . Idx m . typeOf) bodyArgs
                  elemASTs <- withAncestorVar anc (mapM (makeSerialAST' gscope typepackers) elemTfs)
                  return $ SerialTuple (FV gv tupleCv) elemASTs
                -- @type X = SomePackedT a@: forward through the body's
                -- expansion under the ancestor scope so any recursive
                -- references back to @X@ are caught as @SerialRec@.
                AliasIsOther expanded -> do
                  expandedTf <- inferConcreteType lang (Idx m (typeOf expanded))
                  withAncestorVar anc (makeSerialAST' gscope typepackers expandedTf)
                -- No alias expansion available; fall back to Packable
                -- lookup the same way this branch did before.
                AliasIsNone -> case Map.lookup gv typepackers of
                  (Just ps) -> do
                    packers <- mapM makeTypePacker ps
                    unpacked <- mapM (makeSerialAST' gscope typepackers . typePackerUnpacked) packers
                    selection <- selectPacker (zip packers unpacked)
                    return $ SerialPack v selection
                  Nothing ->
                    MM.throwSourcedError m $
                      "Cannot find constructor in VarF" <+> dquotes (pretty v) <+> " finalType=" <> pretty finalType

        -- Bare alias body shape classifier. Mirrors the AppF branch's
        -- @aliasShape@ but for a zero-arg @VarF@: we ask gscope for
        -- the alias body and inspect its outer constructor.
        -- Walk through newtype bodies as well as transparent aliases.
        -- A newtype like @newtype Vector (n :: Nat) a = List a@ should
        -- have its list-shape recognised here so the codegen routes
        -- through the list dispatch instead of demanding a Packable
        -- instance. Newtypes whose body is itself a leaf (e.g.
        -- @newtype Bytes = Str@) still fall through to the
        -- Packable-lookup path -- the @aliasShape@ matcher only
        -- recognises @AppU@ shapes (List / Tuple), and a bare @VarU@
        -- body falls under @AliasIsNone@.
        -- When the outer type has a registered @Packable@ instance,
        -- defer to that instance regardless of body shape (see the
        -- AppF branch's @aliasShape@ for the rationale).
        -- 'hasOwnForm' says the type declares a per-language form of its own.
        -- Such a type is not simply its parent natively, so expanding through
        -- the parent would discard the declaration.
        aliasShape hasOwnForm
          | Map.member gv typepackers = AliasIsNone
          | otherwise = case TE.expandWireParent gscope (VarU gv) of
              Just expanded@(AppU (VarU h) bodyArgs)
                | h == gv -> AliasIsNone
                | h == BT.list, [elemU] <- bodyArgs -> AliasIsList elemU
                | let bodyRT = bodyArgs
                , h == BT.tuple (length bodyRT) -> AliasIsTuple bodyRT
                | otherwise -> AliasIsOther expanded
              -- A newtype over a leaf (@newtype Name = Str@) is its parent on
              -- the wire, so route through the parent's serializer rather than
              -- demanding a Packable instance. The parameterised branch above
              -- already does this; without the arm here a zero-parameter
              -- newtype over a primitive cannot cross a boundary at all.
              Just expanded@(VarU h)
                | h == gv -> AliasIsNone
                | hasOwnForm -> AliasIsNone
                | otherwise -> AliasIsOther expanded
              _ -> AliasIsNone

        -- Evaluate type aliases step-by-step, stopping at known serialization
        -- base types. This prevents aliases like Int64 = Int from collapsing
        -- to Int, which would lose width information for serialization.
        finalType =
          let t = fst $ unweaveTypeF ft
           in resolveToSerialBaseType gscope t

        resolveToSerialBaseType scope t
          | Set.member t serialBaseTypes = t
          | otherwise = case TE.reduceType scope t of
              Just t' -> resolveToSerialBaseType scope t'
              Nothing -> t

        serialBaseTypes = Set.fromList
          [ BT.unitU, BT.boolU, BT.strU, BT.realU
          , BT.f32U, BT.f64U
          , BT.intU, BT.i8U, BT.i16U, BT.i32U, BT.i64U
          , BT.uintU, BT.u8U, BT.u16U, BT.u32U, BT.u64U
          ]

        makeTypePacker :: PackerInstance -> MorlocMonad TypePacker
        makeTypePacker pin = do
          (forwardSource, reverseSource) <- packerSources lang m pin
          packedType <- inferConcreteType lang (Idx m (typeOf (piHead pin)))
          unpackedType <- inferConcreteType lang (Idx m (typeOf (piWire pin)))
          return $
            TypePacker
              { typePackerPacked = packedType
              , typePackerUnpacked = unpackedType
              , typePackerForward = forwardSource
              , typePackerReverse = reverseSource
              }

        -- A parameterless constructor gives selection nothing to discriminate
        -- on: every instance for it has the same head, so more than one is
        -- ambiguous by construction.
        selectPacker :: [(TypePacker, SerialAST)] -> MorlocMonad (TypePacker, SerialAST)
        selectPacker [] =
          MM.throwSourcedError m $ "No Packable instance for" <+> squotes (pretty cv) <> "."
        selectPacker [x] = return x
        selectPacker xs =
          MM.throwSourcedError m $
            "Ambiguous Packable instances for" <+> squotes (pretty cv) <> ":"
              <> "\n" <> indent 2 (vsep [pretty (typePackerUnpacked tp) | (tp, _) <- xs])
              <> "\nA type with no parameters admits only one instance; these"
              <+> "differ only in the form they serialize to."
    -- A function value crossing a pool boundary is defunctionalized: it travels
    -- as a closure datum (home language, manifold id, captured argument packets).
    -- The argument and result schemas shape only the native callable on each
    -- side; the wire form itself is signature-independent (a tuple emitted by
    -- 'SerialClosure'). Recurse so the arg/result native types are available.
    makeSerialAST' gscope typepackers (FunF ins out) =
      SerialClosure
        <$> mapM (makeSerialAST' gscope typepackers) ins
        <*> makeSerialAST' gscope typepackers out
    -- Wire-form construction for an applied type `Foo a b ...`.
    --
    -- Two pieces of information drive dispatch here:
    --   * fv -- the OUTER (general, concrete) name pair from the woven TypeF.
    --     Carries the user's concrete mapping (e.g. std::deque, numpy.ndarray)
    --     and is the runtime identity that survives onto the wire schema hint.
    --   * The gscope alias body of the outer type (one-step expansion). This
    --     tells us the WIRE STRUCTURE -- list-shaped, tuple-shaped, or
    --     opaque-and-Packable-routed -- independent of the concrete mapping.
    --
    -- These are two different facts about the same type and must come from
    -- different scopes: cscope is authoritative for runtime identity (already
    -- resolved by inferConcreteType via pairEval); gscope is authoritative for
    -- wire structure (only the alias body says "Deque is list-shaped").
    makeSerialAST' gscope typepackers ft@(AppF (VarF fv@(FV generalTypeName _)) ts0) = do
      anc <- MM.gets stateSerialAncestors
      -- Cycle detection: a parameterized self-reference like @T7 Int@
      -- inside @T7 a@'s own body. The head FVar's general name matches
      -- an ancestor that's currently being lowered.
      if Set.member generalTypeName anc
        then return $ SerialRec fv
        else dispatchAppF anc
      where
        -- Add the outer alias name to the ancestor set for the scope of
        -- the body recursion, then restore. Mirrors the NamF branch.
        -- Required for self-recursive type aliases (`type Pair a = (a,
        -- ?(Pair a))`): without it the inner `Pair a` reference is
        -- walked with an empty ancestor set, re-expanded, and loops.
        withAncestor :: Set.Set TVar -> MorlocMonad a -> MorlocMonad a
        withAncestor anc action = do
          MM.modify (\s -> s { stateSerialAncestors = Set.insert generalTypeName anc })
          r <- action
          MM.modify (\s -> s { stateSerialAncestors = anc })
          return r

        dispatchAppF anc
          | null runtimeTs = makeSerialAST' gscope typepackers (VarF fv)
          -- Stream-handle types (IFile / OStream / IStream) share one
          -- 16-byte tagged-union wire form. Intercepting all three here
          -- keeps them from collapsing onto their underlying newtype's
          -- bare UInt64 -- the codec primitives dispatch on the schema
          -- code (F/O/I) plus the per-instance tag byte to pick the
          -- right open kind (IFILE / OSTREAM / ISTREAM) on receive.
          | generalTypeName == BT.ifileVar = return $ SerialIFile fv
          | generalTypeName == BT.ostreamVar = return $ SerialOStream fv
          | generalTypeName == BT.istreamVar = return $ SerialIStream fv
          -- Typed `Table n r`: when the Rec arg lowered to a ground record
          -- (NamF NamRecord ...), surface it as a SerialObject NamTable
          -- carrying the column schema. The wire encoder emits @T:K<entries>@
          -- (or bare @T@ for empty / polymorphic-row); no concrete-type hint
          -- is needed because @T@ is itself the dispatch token to the Arrow
          -- C Data Interface path.
          | generalTypeName == BT.table = case runtimeTs of
              [NamF _ _ _ recRs] -> do
                colASTs <- mapM (\(k, tf) -> (,) k <$> makeSerialAST' gscope typepackers tf) recRs
                return $ SerialObject NamTable (FV BT.table (CV "")) [] colASTs
              _ ->
                return $ SerialObject NamTable (FV BT.table (CV "")) [] []
          | otherwise = case aliasShape of
              -- Outer alias body is list-shaped (`type Deque a = List a`,
              -- `type Vector n a = List a`, user-defined `type MyArr a = [a]`).
              -- Emit SerialList carrying the outer fv; recurse on the body's
              -- element. Inner element types are passed through unmodified so
              -- specialized variants (Int32, Float32) keep their schema width.
              AliasIsList elemU -> do
                elemTf <- inferConcreteType lang (Idx m (typeOf elemU))
                elemAST <- withAncestor anc (makeSerialAST' gscope typepackers elemTf)
                return $ applyDimsToList dims (SerialList fv Nothing elemAST)

              -- Outer alias body is tuple-shaped (`type Pair a b = (a, b)`).
              -- Same pattern as list: outer fv on the SerialTuple, recurse on
              -- each body arg.
              AliasIsTuple bodyArgs -> do
                elemTfs <- mapM (inferConcreteType lang . Idx m . typeOf) bodyArgs
                elemASTs <- withAncestor anc (mapM (makeSerialAST' gscope typepackers) elemTfs)
                return $ SerialTuple fv elemASTs

              -- Outer alias body is something else (`type Foo a = SomePackedT a`).
              -- The user wants serialization to route through SomePackedT's
              -- Packable instance, so we deliberately drop the outer fv: recurse
              -- on the expanded form and let it find the Packable below.
              AliasIsOther expanded -> do
                expandedTf <- inferConcreteType lang (Idx m (typeOf expanded))
                ast <- withAncestor anc (makeSerialAST' gscope typepackers expandedTf)
                return $ applyDimsToList dims ast

              -- No outer aliasing: the type is its own head. Dispatch on it
              -- directly: list / tuple / Packable lookup.
              --
              -- When the outer type is in @typepackers@ but its wire
              -- form is list-shaped (e.g. @Vector n a@ with a Packable
              -- bridge whose wire is @List a@), use 'applyDimsToList'
              -- to re-attach the phantom Nat slots so the wire schema
              -- carries the dim. Macro indices are unaffected (they
              -- count type args only).
              AliasIsNone
                | finalVar == Just BT.list ->
                    applyDimsToList dims . SerialList fv Nothing <$> makeSerialAST' gscope typepackers (head runtimeTs)
                | finalVar == Just (BT.tuple (length runtimeTs)) ->
                    SerialTuple fv <$> mapM (makeSerialAST' gscope typepackers) runtimeTs
                | otherwise -> packableFallback
        -- Classify the outer alias body's wire shape. Empty when the type
        -- is its own head (no aliasing); otherwise tagged by the body's
        -- structural head (List, Tuple, or anything else).
        --
        -- When the outer type has a registered @Packable@ instance,
        -- defer to that instance regardless of body shape: Packable
        -- exists precisely because the native runtime type differs
        -- from the body's structural form (e.g. @Matrix@ packs to
        -- @mlc::Tensor2<$>@, not a tuple). Without this guard the
        -- structural walk would emit the body shape and the
        -- (de)serialiser would skip the pack/unpack hop.
        aliasShape
          | Map.member generalTypeName typepackers = AliasIsNone
          | otherwise = case evaluatedType of
              Just expanded@(AppU (VarU h) bodyArgs)
                | h == generalTypeName -> AliasIsNone   -- alias reduced to itself
                | h == BT.list
                , [elemU] <- filter (not . isKindTypeU) bodyArgs ->
                    AliasIsList elemU
                | let bodyRT = filter (not . isKindTypeU) bodyArgs
                , h == BT.tuple (length bodyRT) ->
                    AliasIsTuple bodyRT
                | otherwise -> AliasIsOther expanded
              -- Newtype-to-primitive wire form (e.g. @newtype IFile a = UInt64@):
              -- expandWireParent peels through the newtype to a bare VarU.
              -- Route through the body's serializer so IFile handles marshal
              -- as their wire type without needing a redundant Packable
              -- instance for every newtype with phantom params.
              Just expanded@(VarU _) -> AliasIsOther expanded
              _ -> AliasIsNone

        -- Look up a Packable instance for the outer type and emit a
        -- SerialPack, or raise a helpful error if no instance is registered.
        packableFallback = case Map.lookup generalTypeName typepackers of
          (Just ps) -> do
            -- Carry each instance's general packed type alongside the
            -- resolved packer. Selection ranks instances by that head, and
            -- 'resolvePacker' has already rewritten it to the concrete form.
            (failures, matches) <- partitionEithers <$> mapM resolveWithHead ps
            selection <- selectPacker failures matches
            return $ SerialPack fv selection
          Nothing ->
            let (gt, ct) = unweaveTypeF ft
            in MM.throwSourcedError m $
                 "No Packable instance for type" <+> squotes (pretty gt)
                   <> " (concrete:" <+> pretty ct <> ")."
                   <> "\nDefine an instance, e.g.:"
                   <> "\n  instance Packable <unpacked> (" <> pretty generalTypeName <+> "...) where ..."
                   <> "\nor map" <+> pretty generalTypeName
                   <+> "to a primitive list / tuple / sourced type."

        -- Resolve one instance against the use site, keeping its general
        -- packed type so the candidates can be ordered by specificity.
        resolveWithHead ::
          PackerInstance ->
          MorlocMonad (Either (TypeU, MDoc) (TypeU, TypePacker))
        resolveWithHead pin = do
          resolved <- resolvePacker lang m ft pin
          return $ case resolved of
            (Right packer) -> Right (piHead pin, packer)
            (Left reason) -> Left (piHead pin, reason)

        -- Extract dimension constraints from kind-kinded type args.
        -- Every Nat position is reified (including NatLitF 0, a valid empty
        -- dimension, and NatVoidF for polymorphic-dim sites like @Vector n a@).
        -- These feed @applyDimsToList@ to annotate each SerialList nesting
        -- level with its size constraint; the wire schema encodes them
        -- via @encodeDim@. Macro indices are independent (they count type
        -- args only via 'partitionKindArgsF').
        dims :: [Maybe TypeF]
        dims = [Just t | t <- ts0, isKindTypeF t]

        -- Apply dimension constraints to nested SerialList nodes.
        -- Each dim in the list is consumed by one nesting level.
        applyDimsToList :: [Maybe TypeF] -> SerialAST -> SerialAST
        applyDimsToList (d:ds) (SerialList v _ inner) = SerialList v d (applyDimsToList ds inner)
        applyDimsToList _ ast = ast

        runtimeTs = filter (not . isKindTypeF) ts0

        basevar :: TypeU -> Maybe TVar
        basevar (VarU v) = Just v
        basevar (NatVarU _) = Nothing
        basevar (ExistU _ _ _) = Nothing
        basevar (ForallU _ _) = Nothing
        basevar (FunU _ _) = Nothing
        basevar (AppU t _) = basevar t
        basevar (NamU _ v _ _) = Just v
        basevar (EffectU _ _) = Nothing
        basevar (OptionalU _) = Nothing
        basevar (NatLitU _) = Nothing
        basevar (NatAddU _ _) = Nothing
        basevar (NatMulU _ _) = Nothing
        basevar (NatSubU _ _) = Nothing
        basevar (NatDivU _ _) = Nothing
        basevar NatVoidU = Nothing
        basevar (StrVarU _) = Nothing
        basevar (StrLitU _) = Nothing
        basevar (StrConcatU _ _) = Nothing
        basevar StrVoidU = Nothing
        basevar (RecVarU _) = Nothing
        basevar RecEmptyU = Nothing
        basevar (RecExtendU _ _ _) = Nothing
        basevar (RecUnionU _ _) = Nothing
        basevar (RecDiffU _ _) = Nothing
        basevar (RecIntersectU _ _) = Nothing
        basevar (RecRestrictU _ _) = Nothing
        basevar (RecDiffListU _ _) = Nothing
        basevar RecVoidU = Nothing
        basevar (ListVarU _) = Nothing
        basevar (ListLitU _) = Nothing
        basevar (ListAppU _ _) = Nothing
        basevar ListVoidU = Nothing
        basevar (SetVarU _) = Nothing
        basevar SetEmptyU = Nothing
        basevar (SetLitU _) = Nothing
        basevar (SetUnionU _ _) = Nothing
        basevar (SetInterU _ _) = Nothing
        basevar (SetDiffU _ _) = Nothing
        basevar SetVoidU = Nothing
        basevar (KeysU _) = Nothing
        basevar (ListToSetU _) = Nothing
        basevar (SizeU _) = Nothing
        basevar (ProjectFieldU _ _) = Nothing
        basevar (RecSingletonU _ _) = Nothing
        basevar (LabeledU _ t) = basevar t

        generalType = fst $ unweaveTypeF ft

        -- Expand only the outer alias, preserving inner type args. Full
        -- evaluation (TE.evaluateType) would chase inner aliases too,
        -- collapsing element types like Int32 into Int and producing the
        -- wrong serialization width. The downstream inferConcreteType call
        -- will resolve concrete types through both scopes correctly.
        --
        -- Walk through newtype bodies as well as transparent aliases:
        -- a newtype like @newtype Vector (n :: Nat) a = List a@ has
        -- @List a@ as its wire form, and the codegen needs to see the
        -- list shape (so @AliasIsList@ fires) rather than demanding a
        -- Packable instance for Vector. Newtypes whose body is a leaf
        -- (e.g. @newtype Bytes = Str@) still fall through to the
        -- Packable-lookup path -- the @aliasShape@ matcher only
        -- recognises @AppU@ list/tuple shapes; a bare @VarU@ body
        -- lands in @AliasIsNone@ which routes to @packableFallback@.
        evaluatedType = TE.expandWireParent gscope generalType

        finalVar = basevar $ maybe generalType id evaluatedType

        -- Choose among the instances that match this use site. Instance
        -- heads are ordered by subsumption, so a well-formed instance set
        -- gives the matches a unique greatest element: the most specific
        -- instance, which is the one to apply. Several incomparable maxima
        -- mean the heads overlap without either specializing the other, and
        -- no choice between them is defensible.
        selectPacker ::
          [(TypeU, MDoc)] ->
          [(TypeU, TypePacker)] ->
          MorlocMonad (TypePacker, SerialAST)
        selectPacker failures [] =
          let (gt, ct) = unweaveTypeF ft
           in MM.throwSourcedError m $
                "No Packable instance covers" <+> squotes (pretty gt)
                  <> " (concrete:" <+> pretty ct <> ")."
                  <> "\nInstances declared for" <+> pretty generalTypeName
                  <> ", and why each was rejected:"
                  <> "\n" <> indent 2 (vsep [pretty h <> "\n  " <> reason | (h, reason) <- failures])
        selectPacker _ matches =
          let maxima = mostSpecific (map fst matches)
           in case [packer | (h, packer) <- matches, any (equivalent h) maxima] of
                [packer] -> do
                  ast <- makeSerialAST' gscope typepackers (typePackerUnpacked packer)
                  return (packer, ast)
                _ ->
                  MM.throwSourcedError m $
                    "Ambiguous Packable instances for"
                      <+> squotes (pretty (fst (unweaveTypeF ft))) <> ":"
                      <> "\n" <> indent 2 (vsep (map pretty maxima))
                      <> "\nEach matches, and none is more specific than the others."
                      <> "\nDeclare an instance for their common specialization to"
                      <+> "disambiguate, or remove one of them."
    makeSerialAST' gscope typepackers (NamF o n@(FV gv _) ps rs) = do
      anc <- MM.gets stateSerialAncestors
      -- Cycle detection at the NamF entry. If we are already lowering
      -- a record with this name on the path above us, emit a back-ref.
      -- Otherwise add the name to the ancestor set, recurse on fields,
      -- and restore the previous set.
      if Set.member gv anc
        then return $ SerialRec n
        else do
          MM.modify (\s -> s { stateSerialAncestors = Set.insert gv anc })
          ts <- mapM (makeSerialAST' gscope typepackers . snd) rs
          MM.modify (\s -> s { stateSerialAncestors = anc })
          let entries = zip (map fst rs) ts
          return $ SerialObject o n ps entries
    makeSerialAST' gscope typepackers (EffectF _ t) = makeSerialAST' gscope typepackers t
    makeSerialAST' gscope typepackers (OptionalF t) = do
      inner <- makeSerialAST' gscope typepackers t
      let v = case t of
                VarF fv -> fv
                AppF (VarF fv) _ -> fv
                NamF _ fv _ _ -> fv
                _ -> FV (TV "Optional") (CV "optional")
      return $ SerialOptional v inner
    makeSerialAST' _ _ t = MM.throwSourcedError m $ "makeSerialAST' error on type:" <+> pretty t

-- | The pack/unpack pair a language implements an instance with.
--
-- The table is filtered to the language before use, so a miss here means the
-- filter and this lookup have gone out of step.
emptyGamma :: Gamma
emptyGamma =
  Gamma 0 0 IntMap.empty Map.empty Map.empty [] Map.empty Map.empty [] Nothing Map.empty [] Set.empty

packerSources :: Lang -> Int -> PackerInstance -> MorlocMonad (Source, Source)
packerSources lang m0 pin = case Map.lookup lang (piSources pin) of
  (Just srcs) -> return srcs
  Nothing ->
    MM.throwSourcedError m0 $
      "No" <+> pretty lang <+> "implementation of the Packable instance for"
        <+> squotes (pretty (piHead pin))

resolvePacker ::
  Lang ->
  Int ->
  TypeF ->
  PackerInstance ->
  MorlocMonad (Either MDoc TypePacker)
resolvePacker lang m0 resolvedType@(AppF _ _) pin = do
  -- The wire form is written as whatever names the instance used, so a record
  -- wire form arrives as the record's name. 'inferConcreteTypeU' resolves the
  -- concrete side to the record body, so the general side has to be resolved
  -- too; weaving a name against an expanded body has no meaning.
  (_, gscope) <- getScope m0 lang
  let unpackedGeneralType = either (const (piWire pin)) id (TE.evaluateType gscope (piWire pin))
      packedGeneralType = piHead pin
  (srcPacked, srcUnpacked) <- packerSources lang m0 pin
  packedConcreteType <- inferConcreteTypeU lang (Idx m0 packedGeneralType)
  unpackedConcreteType <- inferConcreteTypeU lang (Idx m0 unpackedGeneralType)
  maybeUnpackedType <-
    resolveP
      resolvedType
      packedConcreteType
      unpackedConcreteType
      (packedGeneralType, unpackedGeneralType)

  case maybeUnpackedType of
    (Right unpackedType) ->
      return . Right $
        TypePacker
          { typePackerPacked = resolvedType
          , typePackerUnpacked = unpackedType
          , typePackerForward = srcPacked
          , typePackerReverse = srcUnpacked
          }
    (Left reason) -> return (Left reason)
  where
    -- Both sides of the packer function are guaranteed to have the same
    -- generic values, this is guaranteed by the implementation of
    -- Restructure.hs. So it is sufficient to resolve the generics in the packed
    -- type and map them to the unpacked type.
    --
    -- Example:
    --
    --  resolveP ("dict" "str" "int") ("dict" a b) ("list" ("list" a b) --> ("list" ("list" "str" "int"))
    --                    x_r             x_u                y_u                       y_r
    --
    -- x_u is the unresolved packed type that is extracted before typechecking
    -- x_r is equal to x_u after type inference
    --
    -- () |- x_u <: x_y -| g
    -- y_r = apply g y_u
    --
    -- y_u is the unresolved unpacked type that is extracted with x_u
    --
    -- y_u and y_r are both processed by Restructure.hs and are both guaranteed
    -- to share the same set of generics. We can find the identity of these
    -- generics by subtyping x_u against x_y. The produced context contains
    -- the types for each generic variable. The context can be applied to
    -- y_u to get the final desired y_r.
    resolveP ::
      TypeF -> -- resolved packed type (e.g., "dict" "str" "int")
      TypeU -> -- unresolved packed type (e.g., "dict" a b)
      TypeU -> -- unresolved unpacked type (e.g., "list" ("list" a b))
      (TypeU, TypeU) -> -- The general unresolved packed and unpacked types
      MorlocMonad (Either MDoc TypeF) -- the resolved unpacked type, or why this instance does not apply
    resolveP a b c generalTypes = do
      let (ga, ca) = unweaveTypeF a
      -- Both steps are match tests, not assertions. An instance whose head does
      -- not cover this use site is simply not a candidate here; only the caller
      -- knows whether some other instance matched, so neither step may abort.
      case subtype Map.empty b ca emptyGamma of
        (Left typeErr) ->
          return . Left $
            "its concrete form" <+> pretty b <+> "does not cover" <+> pretty ca <> ":" <+> typeErr
        (Right g) -> do
          let unpackedConcreteType = apply g (existential c)
          -- u  is the unresolved general packed type stored in Desugar.hs
          -- gc is the unresolved general unpacked type
          case generalTypes of
            (u, gc) ->
              case subtype Map.empty u ga emptyGamma of
                (Left typeErr) ->
                  return . Left $
                    "its general form" <+> pretty u <+> "does not cover" <+> pretty ga <> ":" <+> typeErr
                (Right g2) ->
                  return . Right $
                    weaveTypeF (apply g2 (existential gc)) unpackedConcreteType

    -- Replaces each generic term with an existential term of the same name
    existential :: TypeU -> TypeU
    existential (ForallU v t0) = substitute v (existential t0)
    existential t0 = t0
resolvePacker _ m0 _ _ = MM.throwSourcedError m0 $ "No packer found for this type"

cv2tv :: CVar -> TVar
cv2tv (CV x) = TV x

tv2cv :: TVar -> CVar
tv2cv (TV x) = CV x

unweaveTypeF :: TypeF -> (TypeU, TypeU)
unweaveTypeF (UnkF (FV gv cv)) = (VarU gv, VarU (cv2tv cv))
unweaveTypeF (VarF (FV gv cv)) = (VarU gv, VarU (cv2tv cv))
unweaveTypeF (FunF ts t) =
  let (gt, ct) = unweaveTypeF t
      (gts, cts) = unzip $ map unweaveTypeF ts
   in (FunU gts gt, FunU cts ct)
unweaveTypeF (AppF t ts) =
  let (gt, ct) = unweaveTypeF t
      (gts, cts) = unzip $ map unweaveTypeF ts
   in (AppU gt gts, AppU ct cts)
unweaveTypeF (NamF n (FV gv cv) ps rs) =
  let (psg, psc) = unzip $ map unweaveTypeF ps
      keys = map fst rs
      (vsg, vsc) = unzip $ map (unweaveTypeF . snd) rs
   in (NamU n gv psg (zip keys vsg), NamU n (cv2tv cv) psc (zip keys vsc))
unweaveTypeF (EffectF effs t) =
  let (gt, ct) = unweaveTypeF t
   in (mkEffectU (EffectSet effs) gt, mkEffectU (EffectSet effs) ct)
unweaveTypeF (OptionalF t) =
  let (gt, ct) = unweaveTypeF t
   in (OptionalU gt, OptionalU ct)

-- A back-reference unweaves into a bare VarU for each side; the
-- referenced NamF appears elsewhere in the surrounding TypeU/TypeF
-- and supplies the structural identity.
unweaveTypeF (RecF (FV gv cv)) = (VarU gv, VarU (cv2tv cv))

-- Nat / Str types have no concrete/general distinction; duplicate as-is
unweaveTypeF (NatLitF n) = (NatLitU n, NatLitU n)
unweaveTypeF NatVoidF = (NatVoidU, NatVoidU)
unweaveTypeF (StrLitF s) = (StrLitU s, StrLitU s)
unweaveTypeF StrVoidF = (StrVoidU, StrVoidU)

weaveTypeF :: TypeU -> TypeU -> TypeF
weaveTypeF (VarU gv) (VarU cv) = VarF (FV gv (tv2cv cv))
weaveTypeF (FunU tsg tg) (FunU tsc tc) = FunF (zipWith weaveTypeF tsg tsc) (weaveTypeF tg tc)
-- AppU arg-list weaving is arity-aware: the concrete side may be
-- shorter than the general side when @pairEval@ expanded a newtype
-- body whose body has a smaller arity than the outer (e.g.
-- @newtype Vector n a = List a@ where Vector has 2 params but the
-- expanded body has 1). Each general kind arg consumes a concrete
-- kind arg if present, otherwise produces an erased sentinel without
-- advancing the concrete cursor. Mirrors @Infer.weaveArgs@ at the
-- Serial.hs entry point.
weaveTypeF (AppU tg tsg) (AppU tc tsc) = AppF (weaveTypeF tg tc) (weaveTypeFArgs tsg tsc)
weaveTypeF (NamU n gv psg rsg) (NamU _ cv psc rsc) =
  NamF
    n
    (FV gv (tv2cv cv))
    (zipWith weaveTypeF psg psc)
    ( zip
        (map fst rsg)
        (zipWith weaveTypeF (map snd rsg) (map snd rsc))
    )
weaveTypeF (EffectU effs gt) (EffectU _ ct) = mkEffectF (resolveEffectSet effs) (weaveTypeF gt ct)
weaveTypeF (OptionalU gt) (OptionalU ct) = OptionalF (weaveTypeF gt ct)
weaveTypeF ((ExistU gv _ _)) (ExistU cv _ _) = UnkF (FV gv (tv2cv cv))
weaveTypeF (NatLitU n) (NatLitU _) = NatLitF n
weaveTypeF (NatLitU n) _ = NatLitF n  -- Nat params may be erased in concrete type
weaveTypeF NatVoidU _ = NatVoidF
weaveTypeF (NatVarU _) _ = NatVoidF  -- Nat vars erased in concrete type
-- Nat arithmetic: try to reduce to a ground Integer. If every leaf is a
-- NatLit, produce a NatLitF; otherwise produce the NatVoidF sentinel
-- (the dimension is symbolic / unresolvable at this site, not 0).
weaveTypeF nat@(NatAddU _ _) _ = maybe NatVoidF NatLitF (reduceNat nat)
weaveTypeF nat@(NatMulU _ _) _ = maybe NatVoidF NatLitF (reduceNat nat)
weaveTypeF nat@(NatSubU _ _) _ = maybe NatVoidF NatLitF (reduceNat nat)
weaveTypeF nat@(NatDivU _ _) _ = maybe NatVoidF NatLitF (reduceNat nat)
weaveTypeF (LabeledU _ gt) ct = weaveTypeF gt ct
weaveTypeF gt (LabeledU _ ct) = weaveTypeF gt ct
weaveTypeF gt ct = error . show $ (gt, ct)

-- | Weave two arg lists, allowing the concrete list to be SHORTER
-- than the general list when the missing positions are kind-kinded
-- (the body's per-language form may have dropped them during
-- newtype expansion). Each general kind arg consumes a concrete
-- kind head if present, otherwise produces the erased sentinel
-- without advancing the concrete cursor.
weaveTypeFArgs :: [TypeU] -> [TypeU] -> [TypeF]
weaveTypeFArgs [] _ = []
weaveTypeFArgs (NatLitU n : gs) cs = NatLitF n : weaveTypeFArgs gs (dropKindHeadU cs)
weaveTypeFArgs (NatVoidU : gs) cs = NatVoidF : weaveTypeFArgs gs (dropKindHeadU cs)
weaveTypeFArgs (NatVarU _ : gs) cs = NatVoidF : weaveTypeFArgs gs (dropKindHeadU cs)
weaveTypeFArgs (nat@(NatAddU _ _) : gs) cs =
  maybe NatVoidF NatLitF (reduceNat nat) : weaveTypeFArgs gs (dropKindHeadU cs)
weaveTypeFArgs (nat@(NatMulU _ _) : gs) cs =
  maybe NatVoidF NatLitF (reduceNat nat) : weaveTypeFArgs gs (dropKindHeadU cs)
weaveTypeFArgs (nat@(NatSubU _ _) : gs) cs =
  maybe NatVoidF NatLitF (reduceNat nat) : weaveTypeFArgs gs (dropKindHeadU cs)
weaveTypeFArgs (nat@(NatDivU _ _) : gs) cs =
  maybe NatVoidF NatLitF (reduceNat nat) : weaveTypeFArgs gs (dropKindHeadU cs)
weaveTypeFArgs (g : gs) (c : cs) = weaveTypeF g c : weaveTypeFArgs gs cs
weaveTypeFArgs gs [] =
  error $ "weaveTypeFArgs: concrete arg list exhausted with non-kind general args remaining: " <> show gs

-- | Drop a leading kind-kinded arg from the concrete cursor, if
-- present. Non-kind heads leave the cursor alone.
dropKindHeadU :: [TypeU] -> [TypeU]
dropKindHeadU (c : cs) | isKindTypeU c = cs
dropKindHeadU cs = cs

-- Recursively evaluate a Nat expression to an Integer. Returns Nothing if
-- the expression contains an unresolvable leaf (NatVar / NatVoid / non-Nat
-- shape). Callers turn Nothing into the NatVoidF sentinel rather than
-- collapsing to a fake 0 value.
reduceNat :: TypeU -> Maybe Integer
reduceNat (NatLitU n) = Just n
reduceNat (NatAddU a b) = (+) <$> reduceNat a <*> reduceNat b
reduceNat (NatMulU a b) = (*) <$> reduceNat a <*> reduceNat b
reduceNat (NatSubU a b) = (-) <$> reduceNat a <*> reduceNat b
reduceNat (NatDivU a b) = do
  x <- reduceNat a
  y <- reduceNat b
  if y == 0 then Nothing else Just (x `div` y)
reduceNat _ = Nothing

-- | True iff this SerialAST root is a Table (Arrow IPC primitive).
-- Used at codegen sites that need to route through the Arrow C Data
-- Interface rather than the general msgpack path. Identity is the
-- structural NamTable tag; the old @<arrow>@ concrete-type hint has
-- been retired (the wire-form @T@ marker now carries the dispatch).
hasArrowHint :: SerialAST -> Bool
hasArrowHint (SerialObject NamTable _ _ _) = True
hasArrowHint _ = False

{- | Given a list of possible ways to (de)serialize data between two languages,
choose one (or none if the list is empty). Currently I just take the first
in the list, but different cycles may have very different performance, so
this will be an important optimization step later on.
-}
chooseSerializationCycle ::
  [(SerialAST, SerialAST)] ->
  Maybe (SerialAST, SerialAST)
chooseSerializationCycle [] = Nothing
chooseSerializationCycle (x : _) = Just x

{- | Determine if a SerialAST can be directly translated to JSON, if not it
will need to be further reduced.
-}
isSerializable :: SerialAST -> Bool
isSerializable (SerialPack _ _) = False
isSerializable (SerialList _ _ x) = isSerializable x
isSerializable (SerialTuple _ xs) = all isSerializable xs
isSerializable (SerialObject _ _ _ rs) = all (isSerializable . snd) rs
isSerializable (SerialReal _) = True
isSerializable (SerialFloat32 _) = True
isSerializable (SerialFloat64 _) = True
isSerializable (SerialInt _) = True
isSerializable (SerialInt8 _) = True
isSerializable (SerialInt16 _) = True
isSerializable (SerialInt32 _) = True
isSerializable (SerialInt64 _) = True
isSerializable (SerialUInt _) = True
isSerializable (SerialUInt8 _) = True
isSerializable (SerialUInt16 _) = True
isSerializable (SerialUInt32 _) = True
isSerializable (SerialUInt64 _) = True
isSerializable (SerialBool _) = True
isSerializable (SerialString _) = True
isSerializable (SerialIFile _) = True
isSerializable (SerialOStream _) = True
isSerializable (SerialIStream _) = True
isSerializable (SerialClosure _ _) = True
isSerializable (SerialNull _) = True
isSerializable (SerialOptional _ x) = isSerializable x
-- A back-reference is serializable iff its referenced object is.
-- We cannot inspect the referenced object structurally from a
-- back-ref, but it is guaranteed by construction to be a
-- SerialObject (which is serializable) -- otherwise no recursion
-- would have been introduced. Return True.
isSerializable (SerialRec _) = True
isSerializable (SerialUnknown _) = True -- are you feeling lucky?

prettySerialOne :: SerialAST -> MDoc
prettySerialOne (SerialPack _ _) = "SerialPack"
prettySerialOne (SerialList v _ x) = "SerialList" <> angles (pretty v) <> parens (prettySerialOne x)
prettySerialOne (SerialTuple v xs) = "SerialTuple" <> angles (pretty v) <> tupled (map prettySerialOne xs)
prettySerialOne (SerialObject r _ _ rs) =
  block 4 ("SerialObject@" <> viaShow r) $
    vsep (map (\(k, v) -> parens (viaShow k) <> "=" <> prettySerialOne v) rs)
prettySerialOne (SerialReal _) = "SerialReal"
prettySerialOne (SerialFloat32 _) = "SerialFloat32"
prettySerialOne (SerialFloat64 _) = "SerialFloat64"
prettySerialOne (SerialInt _) = "SerialInt"
prettySerialOne (SerialInt8 _) = "SerialInt8"
prettySerialOne (SerialInt16 _) = "SerialInt16"
prettySerialOne (SerialInt32 _) = "SerialInt32"
prettySerialOne (SerialInt64 _) = "SerialInt64"
prettySerialOne (SerialUInt _) = "SerialUInt"
prettySerialOne (SerialUInt8 _) = "SerialUInt8"
prettySerialOne (SerialUInt16 _) = "SerialUInt16"
prettySerialOne (SerialUInt32 _) = "SerialUInt32"
prettySerialOne (SerialUInt64 _) = "SerialUInt64"
prettySerialOne (SerialBool _) = "SerialBool"
prettySerialOne (SerialString _) = "SerialString"
prettySerialOne (SerialIFile _) = "SerialIFile"
prettySerialOne (SerialOStream _) = "SerialOStream"
prettySerialOne (SerialIStream _) = "SerialIStream"
prettySerialOne (SerialClosure _ _) = "SerialClosure"
prettySerialOne (SerialNull _) = "SerialNull"
prettySerialOne (SerialOptional _ x) = "SerialOptional" <> parens (prettySerialOne x)
prettySerialOne (SerialRec v) = "SerialRec" <> angles (pretty v)
prettySerialOne (SerialUnknown _) = "SerialUnknown"
