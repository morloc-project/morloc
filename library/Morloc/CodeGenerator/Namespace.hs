{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Morloc.CodeGenerator.Namespace
Description : All code generator types and datastructures
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Namespace
  ( module Morloc.Namespace
  -- ** Types used in final translations
  , PVar(..)
  , TypeP(..)
  , TypeM(..)
  , FVar(..)
  , TypeF(..)
  , TypeS(..)
  , pvar2fvar
  , fvar2pvar
  -- ** Typeclasses
  , HasTypeF(..)
  , MayHaveTypeF(..)
  , HasTypeS(..)
  , HasTypeM(..)
  , typeMofRs
  , typeMofForm
  -- ** 
  , Arg
  , ArgGeneral(..)
  , JsonType(..)
  , JsonPath
  , JsonAccessor(..)
  , NexusCommand(..)
  , ManifoldForm(..)
  , manifoldContext
  , manifoldBound
  , ArgTypes(..)
  , argTypesToTypeM
  -- ** Manifold data types
  , PolyHead(..)
  , PolyExpr(..)
  , MonoHead(..)
  , MonoExpr(..)
  , PoolCall(..)
  , MFunctor(..)
  , GateMap(..)
  , ManifoldMap(..)
  , NativeManifold(..)
  , SerialManifold(..)
  , SerialArg(..)
  , NativeArg(..)
  , SerialExpr(..)
  , NativeExpr(..)
  -- unrecursive types
  , FoldManifoldM(..)
  , SurroundManifoldM(..)
  , NativeManifold_(..)
  , SerialManifold_(..)
  , SerialArg_(..)
  , NativeArg_(..)
  , SerialExpr_(..)
  , NativeExpr_(..)
  , foldlSM
  , foldlNM
  , foldlSE
  , foldlNE
  , foldlSA
  , foldlNA
  -- ** Serialization AST
  , SerialAST(..)
  , TypePacker(..)
  , ResolvedPacker(..)
  -- ** Other
  , generalTypeOf
  , prettyGenTypeP
  -- ** Simple fold over expressions
  , foldSerialManifoldM
  , foldNativeManifoldM
  , foldSerialExprM
  , foldNativeExprM
  , foldNativeArgM
  , foldSerialArgM
  -- ** Contextual fold over expressions
  , surroundFoldSerialManifoldM
  , surroundFoldNativeManifoldM
  , surroundFoldSerialExprM
  , surroundFoldNativeExprM
  , surroundFoldNativeArgM
  , surroundFoldSerialArgM
  -- ** fold withs
  , FoldWithManifoldM(..)
  , foldWithSerialManifoldM
  , foldWithNativeManifoldM
  , foldWithSerialArgM
  , foldWithNativeArgM
  , foldWithSerialExprM
  , foldWithNativeExprM
  -- ** arg magic
  , abimapM
  , abilistM
  , afirstM
  , asecondM
  , abilist
  , abimap
  , afirst
  , asecond
  , abiappendM
  , abiappend
  -- ** weird baby schemes
  , MonoidFold(..)
  , makeMonoidFoldDefault
  ) where

import Morloc.Namespace
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Set as Set
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import Morloc.Pretty ()
import Control.Monad.Identity (runIdentity)

-- | Stores the language, general name and concrete name for a type expression
data PVar
  = PV
    Lang
    (Maybe Text) -- ^ general name of the type variable (if known)
    Text -- ^ concrete name of type variable
  deriving (Show, Eq, Ord)

-- | A solved type coupling a language specific form to an optional general form
data TypeP
  = UnkP PVar
  | VarP PVar
  | FunP [TypeP] TypeP
  | AppP TypeP [TypeP] -- FIXME: this allows representation of things that cannot be applied
  | NamP NamType PVar [TypeP] [(PVar, TypeP)]
  deriving (Show, Ord, Eq)

-- The final types used in code generation. The language annotation is removed,
-- since the language for all types within a pool are the same.
--
-- The general type annotation will be used for documentation only
data FVar = FV Text -- general type
               Text -- concrete type
  deriving (Show, Ord, Eq)

-- The most minimal type that contains both general and concrete types
data TypeF
  = UnkF FVar
  | VarF FVar
  | FunF [TypeF] TypeF
  | AppF TypeF [TypeF]
  | NamF NamType FVar [TypeF] [(FVar, TypeF)]
  deriving (Show, Ord, Eq)

-- Represents types that may contain serialized elements.
data TypeC
  = SerC (Maybe TypeF)
  | VarC FVar
  | FunC [TypeC] TypeC
  | AppC TypeF [TypeF]
  | NamC NamType FVar [TypeF] [(FVar, TypeF)]
  deriving (Show, Ord, Eq)

pvar2fvar :: PVar -> FVar
pvar2fvar (PV _ (Just g) c) = FV g c
pvar2fvar (PV _ Nothing c) = FV "<undefined>" c

fvar2pvar :: Lang -> FVar -> PVar
fvar2pvar l (FV g c) = PV l (Just g) c

type JsonPath = [JsonAccessor]
data JsonAccessor
  = JsonIndex Int
  | JsonKey Text

data NexusCommand = NexusCommand
  { commandName :: EVar -- ^ user-exposed subcommand name in the nexus
  , commandType :: Type -- ^ the general type of the expression
  , commandJson :: MDoc -- ^ JSON output with null's where values will be replaced
  , commandArgs :: [EVar] -- ^ list of function arguments
  , commandSubs :: [(JsonPath, Text, JsonPath)]
  -- ^ list of tuples with values 1) path in JSON to value needs to be replaced
  -- 2) the function argument from which to pull replacement value and 3) the
  -- path to the replacement value
  }

instance Typelike TypeP where
  typeOf (UnkP v) = UnkT (pvar2tvar v)
  typeOf (VarP v) = VarT (pvar2tvar v)
  typeOf (FunP ts t) = FunT (map typeOf ts) (typeOf t)
  typeOf (AppP v ts) = AppT (typeOf v) (map typeOf ts)
  typeOf (NamP o n ps es) =
    let n' = pvar2tvar n
        ps' = map typeOf ps
        es' = [(v, typeOf t) | (PV _ _ v, t) <- es]
    in NamT o n' ps' es'

  -- | substitute all appearances of a given variable with a given new type
  substituteTVar v0 r0 t0 = sub t0
    where
      sub :: TypeP -> TypeP
      sub t@(UnkP _) = t
      sub t@(VarP (PV lang _ v))
        | v0 == TV (Just lang) v = r0
        | otherwise = t
      sub (FunP ts t) = FunP (map sub ts) (sub t)
      sub (AppP v ts) = AppP (sub v) (map sub ts)
      sub (NamP o n ps es) = NamP o n (map sub ps) [(k, sub t) | (k, t) <- es]

  free v@(VarP _) = Set.singleton v
  free (FunP ts t) = Set.unions (map free (t:ts))
  free (AppP t ts) = Set.unions (map free (t:ts))
  free (NamP _ _ ps es) = Set.unions (map free (map snd es <> ps))
  free (UnkP _) = Set.empty -- are UnkP free?

  normalizeType (FunP ts1 (FunP ts2 t)) = normalizeType $ FunP (ts1 <> ts2) t
  normalizeType (AppP t ts) = AppP (normalizeType t) (map normalizeType ts)
  normalizeType (NamP t v ds rs) = NamP t v (map normalizeType ds) (map (second normalizeType) rs)
  normalizeType t = t

pvar2tvar :: PVar -> TVar
pvar2tvar (PV lang _ v) = TV (Just lang) v

generalTypeOf :: TypeP -> Maybe Type
generalTypeOf (UnkP v) = UnkT <$> pvar2genTVar v
generalTypeOf (VarP v) = VarT <$> pvar2genTVar v
generalTypeOf (FunP ts t) = FunT <$> mapM generalTypeOf ts <*> generalTypeOf t
generalTypeOf (AppP v ts) = AppT <$> generalTypeOf v <*> mapM generalTypeOf ts
generalTypeOf (NamP o n ps es)
    = NamT o
    <$> pvar2genTVar n
    <*> mapM generalTypeOf ps
    <*> mapM typeOfEntry es
    where
        typeOfEntry :: (PVar, TypeP) -> Maybe (MT.Text, Type)
        typeOfEntry (PV _ v _, t) = (,) <$> v <*> generalTypeOf t

pvar2genTVar :: PVar -> Maybe TVar
pvar2genTVar (PV _ v _) = TV Nothing <$> v

-- | A tree describing how to (de)serialize an object
data SerialAST
  = SerialPack FVar (TypePacker, SerialAST) -- ^ use an (un)pack function to simplify an object
  | SerialList FVar SerialAST
  | SerialTuple FVar [SerialAST]
  | SerialObject NamType FVar [TypeF] [(FVar, SerialAST)]
  -- ^ Make a record, table, or object. The parameters indicate
  --   1) NamType - record/table/object
  --   2) FVar - telling the name of the object (e.g., "Person")
  --   3) [TypeF] - the types of the parameters (used as parameters in C++ templates, e.g., map<int, map<int,string>>)
  --   4) [(FVar, SerialAST)] - entries with keys for concrete and general cases
  | SerialReal FVar
  | SerialInt FVar
  | SerialBool FVar
  | SerialString FVar
  | SerialNull FVar
  | SerialUnknown FVar
  -- ^ depending on the language, this may or may not raise an error down the
  -- line, the parameter contains the variable name, which is useful only for
  -- source code comments.
  deriving(Ord, Eq, Show)


instance Pretty SerialAST where
  pretty (SerialPack v (packer, s)) = parens
    $ "SerialPack" <+> pretty v
    <+> braces (vsep [pretty packer, pretty s])
  pretty (SerialList _ ef) = parens $ "SerialList" <+> pretty ef 
  pretty (SerialTuple _ efs) = parens $ "SerialTuple" <+> tupled (map pretty efs)
  pretty (SerialObject o _ vs rs) = parens
    $ "SerialObject" <+> pretty o <+> tupled (map pretty vs)
    <+> encloseSep "{" "}" "," [pretty k <+> "=" <+> pretty p | (k, p) <- rs]
  pretty (SerialReal v) = parens ("SerialReal" <+> pretty v)
  pretty (SerialInt v) = parens ("SerialInt" <+> pretty v)
  pretty (SerialBool v) = parens ("SerialBool" <+> pretty v)
  pretty (SerialString v) = parens ("SerialString" <+> pretty v)
  pretty (SerialNull v) = parens ("SerialNull" <+> pretty v)
  pretty (SerialUnknown v) = parens ("SerialUnknown" <+> pretty v)

data ResolvedPacker =
  ResolvedPacker
    { resolvedPackerTerm :: Maybe EVar
    , resolvedPackedType :: TypeU
    , resolvedUnpackedType :: TypeU
    , resolvedPackerForward :: Source
    , resolvedPackerReverse :: Source
    , resolvedPackerGeneralTypes :: Maybe (TypeU, TypeU)
    }
  deriving (Show, Ord, Eq)

data TypePacker = TypePacker
  { typePackerPacked    :: TypeF
  , typePackerUnpacked  :: TypeF
  , typePackerForward :: Source
  , typePackerReverse :: Source
  }
  deriving (Show, Ord, Eq)

instance Pretty TypePacker where
  pretty p = "TypePacker" <+> encloseSep "{" "}" ","
    [ "typePackerPacked" <+> "=" <+> pretty (typePackerPacked p)
    , "typePackerUnpacked" <+> "=" <+> pretty (typePackerUnpacked p)
    , "typePackerForward" <+> "=" <+> pretty (typePackerForward p)
    , "typePackerReverse" <+> "=" <+> pretty (typePackerReverse p)
    ]

-- | A simplified subset of the Type record where functions, existentials,
-- universals and language-specific info are removed
data JsonType
  = VarJ Text
  -- ^ {"int"}
  | ArrJ Text [JsonType]
  -- ^ {"list":["int"]}
  | NamJ Text [(Text, JsonType)]
  -- ^ {"Foo":{"bar":"A","baz":"B"}}
  deriving (Show, Ord, Eq)

type Arg = ArgGeneral Int

data ArgGeneral k a = Arg k a
    deriving(Show, Eq, Ord)

instance Annotated ArgGeneral where
  val (Arg _ x) = x
  ann (Arg i _) = i
  annotate i x = Arg i x

instance Functor (ArgGeneral k) where
  fmap f (Arg i x) = Arg i (f x)

instance Bifunctor ArgGeneral where
  bimapM f g (Arg k x) = Arg <$> f k <*> g x

data TypeM
  = Passthrough -- ^ serialized data that is not deserialized (and may not be representable) in this segment
  | Serial TypeF -- ^ serialized data that may be deserialized in this language
  | Native TypeF -- ^ an unserialized native data type
  | Function [TypeM] TypeM -- ^ a function of n inputs and one output (cannot be serialized)
  deriving(Show, Eq, Ord)

-- | TypeS is a subset of TypeM that does not allow native types
data TypeS
  = PassthroughS
  | SerialS TypeF
  | FunctionS [TypeM] TypeS -- This is the type of a manifold
  deriving(Show, Eq, Ord)

instance HasTypeM TypeS where
  typeMof PassthroughS = Passthrough
  typeMof (SerialS t) = Serial t
  typeMof (FunctionS ts t) = Function (map typeMof ts) (typeMof t)

class HasTypeS a where
  typeSof :: a -> TypeS

-- | This type stores
--
-- Examples:
--
-- #1: ManifoldFull [x1 = (runif 0 1), x2 = var "x"]
--   source py "foo.py" ("add", "runif")
--   foo x = add (runif 0 1) x
--
-- Add and runif are both fully applied and their arguments are associated with
-- expressions.
--
-- #2: for `add`:  ManifoldPass [x1 : "float", x2 : "float"]
--   source py "foo.py" ("add")
--   foo xs = zipWith add xs
--
-- x1 and x2 are supplied by the source function
-- 
-- #3: ManifoldPart [x1 = (runif 0 1), x2 = var "x"] [x2 : "float"]
--   source py "foo.py" ("add", "runif")
--   foo xs = map (add (runif 0 1)) xs
--
-- add takes one expression and one bound argument.
--
-- The "context" comes first here, it can be partially applied
data (ManifoldForm context bound)
  = ManifoldPass [Arg bound]
  -- ^ Unapplied function passed as argument.
  | ManifoldFull [Arg context]
  -- ^ Fully applied function.
  | ManifoldPart [Arg context] [Arg bound]
  -- ^ Partially applied function
  deriving(Show, Eq, Ord)

manifoldContext :: ManifoldForm a b -> [Arg a]
manifoldContext (ManifoldFull xs) = xs
manifoldContext (ManifoldPass _) = []
manifoldContext (ManifoldPart xs _) = xs

manifoldBound :: ManifoldForm a b -> [Arg b]
manifoldBound (ManifoldFull _) = []
manifoldBound (ManifoldPass xs) = xs
manifoldBound (ManifoldPart _ ys) = ys

instance Bifunctor ManifoldForm where
  bimapM f _ (ManifoldFull xs) = ManifoldFull <$> mapM (\(Arg i x) -> Arg i <$> f x) xs
  bimapM _ g (ManifoldPass xs) = ManifoldPass <$> mapM (\(Arg i x) -> Arg i <$> g x) xs
  bimapM f g (ManifoldPart xs ys) = ManifoldPart <$> mapM (\(Arg i x) -> Arg i <$> f x) xs
                                                 <*> mapM (\(Arg i x) -> Arg i <$> g x) ys

instance Bifoldable ManifoldForm where
  bilistM f _ (ManifoldFull xs) = mapM (f . val) xs
  bilistM _ g (ManifoldPass xs) = mapM (g . val) xs
  bilistM f g (ManifoldPart xs ys) = (<>) <$> mapM (f . val) xs <*> mapM (g . val) ys


abimapM :: (Monad m) => (Int -> a -> m a') -> (Int -> b -> m b') -> ManifoldForm a b -> m (ManifoldForm a' b')
abimapM f _ (ManifoldFull xs) = ManifoldFull <$> mapM (\t -> annotate (ann t) <$> f (ann t) (val t)) xs
abimapM _ g (ManifoldPass xs) = ManifoldPass <$> mapM (\t -> annotate (ann t) <$> g (ann t) (val t)) xs
abimapM f g (ManifoldPart xs ys)
  = ManifoldPart <$> mapM (\t -> annotate (ann t) <$> f (ann t) (val t)) xs
                 <*> mapM (\t -> annotate (ann t) <$> g (ann t) (val t)) ys

afirstM :: (Monad m) => (Int -> a -> m a') -> ManifoldForm a b -> m (ManifoldForm a' b)
afirstM f = abimapM f (return2 seq)

asecondM :: (Monad m) => (Int -> b -> m b') -> ManifoldForm a b -> m (ManifoldForm a b')
asecondM = abimapM (return2 seq)

abimap :: (Int -> a -> a') -> (Int -> b -> b') -> ManifoldForm a b -> ManifoldForm a' b'
abimap f g = runIdentity . abimapM (return2 f) (return2 g)

afirst :: (Int -> a -> a') -> ManifoldForm a b -> ManifoldForm a' b
afirst f = runIdentity . afirstM (return2 f)

asecond :: (Int -> b -> b') -> ManifoldForm a b -> ManifoldForm a b'
asecond f = runIdentity . asecondM (return2 f)

abilistM :: Monad m => (Int -> a -> m c) -> (Int -> b -> m c) -> ManifoldForm a b -> m [c]
abilistM f _ (ManifoldFull xs) = mapM (annappM f) xs
abilistM _ g (ManifoldPass xs) = mapM (annappM g) xs
abilistM f g (ManifoldPart xs ys) = (<>) <$> mapM (annappM f) xs <*> mapM (annappM g) ys

abilist :: (Int -> a -> c) -> (Int -> b -> c) -> ManifoldForm a b -> [c] 
abilist f g = runIdentity . abilistM (return2 f) (return2 g)

abiappendM :: (Monad m, Monoid c) => (Int -> a -> m c) -> (Int -> b -> m c) -> ManifoldForm a b -> m c
abiappendM f g = fmap mconcat . abilistM f g

abiappend :: (Monoid c) => (Int -> a -> c) -> (Int -> b -> c) -> ManifoldForm a b -> c
abiappend f g = runIdentity . abiappendM (return2 f) (return2 g) 

instance Pretty FVar where
    pretty (FV _ c) = pretty c

data PolyHead = PolyHead Int [Arg None] PolyExpr

-- no serialization and no argument types
data PolyExpr
  -- organizational terms that may have undefined types
  = PolyManifold Int (ManifoldForm None (Maybe TypeF)) PolyExpr
  | PolyForeignInterface
      TypeP    -- return type in calling language
      [Int]    -- argument ids
      PolyExpr -- foreign expression
  | PolyLet Int PolyExpr PolyExpr
  | PolyReturn PolyExpr
  | PolyApp PolyExpr [PolyExpr]
  -- variables in the original tree will all be typed
  -- but I also may need to generate passthrough terms
  | PolyBndVar (Either Lang TypeP) Int
  -- The Let variables are generated only in partialExpress, where the type is known
  | PolyLetVar TypeP Int
  -- terms that map 1:1 versus SAnno; have defined types in one language
  | PolySrc    TypeP Source
  | PolyAcc    TypeP NamType PVar PolyExpr Text
  -- data types
  | PolyList   PVar TypeP [PolyExpr]
  | PolyTuple  PVar [(TypeP, PolyExpr)]
  | PolyRecord NamType PVar [TypeP] [(PVar, (TypeP, PolyExpr))]
  | PolyLog    PVar Bool
  | PolyReal   PVar Scientific
  | PolyInt    PVar Integer
  | PolyStr    PVar Text
  | PolyNull   PVar

data MonoHead = MonoHead Lang Int [Arg None] MonoExpr

data MonoExpr
  -- organizational terms that may have undefined types
  = MonoManifold Int (ManifoldForm None (Maybe TypeF)) MonoExpr
  | MonoPoolCall
      TypeF     -- return type in calling language
      Int       -- foreign manifold id
      [MDoc]    -- shell command components that preceed the passed data
      [Arg None] -- arguments
  | MonoLet Int MonoExpr MonoExpr
  | MonoLetVar TypeF Int
  | MonoReturn MonoExpr
  | MonoApp MonoExpr [MonoExpr]
  -- terms that map 1:1 versus SAnno; have defined types in one language
  | MonoSrc    TypeF Source
  | MonoBndVar (Maybe TypeF) Int
  | MonoAcc    TypeF NamType FVar MonoExpr Text
  -- data types
  | MonoList   FVar TypeF [MonoExpr]
  | MonoTuple  FVar [(TypeF, MonoExpr)]
  | MonoRecord NamType FVar [TypeF] [(FVar, (TypeF, MonoExpr))]
  | MonoLog    FVar Bool
  | MonoReal   FVar Scientific
  | MonoInt    FVar Integer
  | MonoStr    FVar Text
  | MonoNull   FVar

data PoolCall = PoolCall
  Int -- foreign manifold id
  [MDoc]
  [Arg TypeM] -- contextual argument that are passed to the foreign function
              -- (not the main arguments to the foreign function)
  deriving(Show)

-- | Represents a single data value that may be passed as an argument through a
-- pool. It may be serialized, native, or both. If it is serialized only, then
-- it may be a passthrough type, in whch case its type is not known.
data ArgTypes
  = SerialOnly TypeS
  | NativeOnly TypeF
  | SerialAndNative TypeF
  deriving(Show)

data NativeManifold = NativeManifold Int Lang (ManifoldForm (Or TypeS TypeF) TypeF) NativeExpr
  deriving(Show)

data SerialManifold = SerialManifold Int Lang (ManifoldForm (Or TypeS TypeF) TypeS) SerialExpr
  deriving(Show)

data SerialArg = SerialArgManifold SerialManifold | SerialArgExpr SerialExpr
  deriving(Show)

data NativeArg = NativeArgManifold NativeManifold | NativeArgExpr NativeExpr
  deriving(Show)

data SerialExpr
  = ManS SerialManifold
  | AppPoolS TypeF PoolCall [SerialArg]
  | ReturnS SerialExpr
  | SerialLetS Int SerialExpr SerialExpr
  | NativeLetS Int NativeExpr SerialExpr
  | LetVarS (Maybe TypeF) Int
  | BndVarS (Maybe TypeF) Int
  | SerializeS SerialAST NativeExpr
  deriving(Show)

data NativeExpr
  = ManN         NativeManifold
  | AppSrcN      TypeF Source [NativeArg]
  | ReturnN      NativeExpr
  | SerialLetN   Int SerialExpr NativeExpr
  | NativeLetN   Int NativeExpr NativeExpr
  | LetVarN      TypeF Int
  | BndVarN      TypeF Int
  | DeserializeN TypeF SerialAST SerialExpr
  | AccN         NamType FVar NativeExpr Text
  | SrcN         TypeF Source
  -- data types
  | ListN        FVar TypeF [NativeExpr]
  | TupleN       FVar [NativeExpr]
  | RecordN      NamType FVar [TypeF] [(FVar, NativeExpr)]
  | LogN         FVar Bool
  | RealN        FVar Scientific
  | IntN         FVar Integer
  | StrN         FVar Text
  | NullN        FVar
  deriving(Show)

foldlSM :: (b -> a -> b) -> b -> SerialManifold_ a -> b
foldlSM f b (SerialManifold_ _ _ _ se) = f b se

foldlNM :: (b -> a -> b) -> b -> NativeManifold_ a -> b
foldlNM f b (NativeManifold_ _ _ _ ne) = f b ne

foldlSA :: (b -> a -> b) -> b -> SerialArg_ a a -> b
foldlSA f b (SerialArgManifold_ sm) = f b sm
foldlSA f b (SerialArgExpr_ se) = f b se

foldlNA :: (b -> a -> b) -> b -> NativeArg_ a a -> b
foldlNA f b (NativeArgManifold_ nm) = f b nm
foldlNA f b (NativeArgExpr_ ne) = f b ne

foldlSE :: (b -> a -> b) -> b -> SerialExpr_ a a a a a -> b
foldlSE f b (ManS_ x) = f b x
foldlSE f b (AppPoolS_ _ _ xs) = foldl f b xs
foldlSE f b (ReturnS_ x) = f b x
foldlSE f b (SerialLetS_ _ x1 x2) = foldl f b [x1, x2]
foldlSE f b (NativeLetS_ _ x1 x2) = foldl f b [x1, x2]
foldlSE _ b (LetVarS_ _ _) = b
foldlSE _ b (BndVarS_ _ _) = b
foldlSE f b (SerializeS_ _ x) = f b x

foldlNE :: (b -> a -> b) -> b -> NativeExpr_ a a a a a -> b
foldlNE f b (AppSrcN_    _ _ xs) = foldl f b xs
foldlNE f b (ManN_            x) = f b x
foldlNE f b (ReturnN_       x) = f b x
foldlNE f b (SerialLetN_   _ x1 x2) = foldl f b [x1, x2]
foldlNE f b (NativeLetN_   _ x1 x2) = foldl f b [x1, x2]
foldlNE _ b (LetVarN_      _ _) = b
foldlNE _ b (BndVarN_      _ _) = b
foldlNE f b (DeserializeN_ _ _ x) = f b x 
foldlNE f b (AccN_         _ _ x _) =  f b x
foldlNE _ b (SrcN_         _ _) = b
foldlNE f b (ListN_        _ _ xs) = foldl f b xs
foldlNE f b (TupleN_       _ xs) = foldl f b xs
foldlNE f b (RecordN_      _ _ _ rs) = foldl (\b' (_, a') -> f b' a') b rs
foldlNE _ b (LogN_         _ _) = b
foldlNE _ b (RealN_        _ _) = b
foldlNE _ b (IntN_         _ _) = b
foldlNE _ b (StrN_         _ _) = b
foldlNE _ b (NullN_        _) = b


data (MonoidFold m a) = MonoidFold
  { monoidSerialManifold :: SerialManifold_ (a, SerialExpr) -> m (a, SerialManifold)
  , monoidNativeManifold :: NativeManifold_ (a, NativeExpr) -> m (a, NativeManifold)
  , monoidSerialArg :: SerialArg_ (a, SerialManifold) (a, SerialExpr) -> m (a, SerialArg)
  , monoidNativeArg :: NativeArg_ (a, NativeManifold) (a, NativeExpr) -> m (a, NativeArg)
  , monoidSerialExpr :: SerialExpr_ (a, SerialManifold) (a, SerialExpr) (a, NativeExpr) (a, SerialArg) (a, NativeArg) -> m (a, SerialExpr)
  , monoidNativeExpr :: NativeExpr_ (a, NativeManifold) (a, SerialExpr) (a, NativeExpr) (a, SerialArg) (a, NativeArg) -> m (a, NativeExpr)
  }

makeMonoidFoldDefault :: (Monad m) => a -> (a -> a -> a) -> MonoidFold m a
makeMonoidFoldDefault mempty' mappend' =
  MonoidFold
    { monoidSerialManifold = monoidSerialManifold'
    , monoidNativeManifold = monoidNativeManifold'
    , monoidSerialArg      = monoidSerialArg'
    , monoidNativeArg      = monoidNativeArg'
    , monoidSerialExpr     = monoidSerialExpr'
    , monoidNativeExpr     = monoidNativeExpr'
    }
  where

  monoidSerialManifold' (SerialManifold_ m lang form (req, ne)) = do
    return (req, SerialManifold m lang form ne)

  monoidNativeManifold' (NativeManifold_ m lang form (req, ne)) = do
    return (req, NativeManifold m lang form ne)

  monoidSerialArg' (SerialArgManifold_ (req, sm)) = return (req, SerialArgManifold sm)
  monoidSerialArg' (SerialArgExpr_ (req, se)) = return (req, SerialArgExpr se)

  monoidNativeArg' (NativeArgManifold_ (req, nm)) = return (req, NativeArgManifold nm)
  monoidNativeArg' (NativeArgExpr_ (req, ne)) = return (req, NativeArgExpr ne)

  monoidSerialExpr' (ManS_ (req, sm)) = return (req, ManS sm)
  monoidSerialExpr' (AppPoolS_ t p (unzip -> (reqs, es))) = return (foldl mappend' mempty' reqs, AppPoolS t p es)
  monoidSerialExpr' (ReturnS_ (req, se)) = return (req, ReturnS se)
  monoidSerialExpr' (SerialLetS_ i (req1, se1) (req2, se2)) = return (mappend' req1 req2, SerialLetS i se1 se2)
  monoidSerialExpr' (NativeLetS_ i (req1, ne) (req2, se)) = return (mappend' req1 req2, NativeLetS i ne se)
  monoidSerialExpr' (LetVarS_ mayT i) = return (mempty', LetVarS mayT i)
  monoidSerialExpr' (BndVarS_ mayT i) = return (mempty', BndVarS mayT i)
  monoidSerialExpr' (SerializeS_ s (req, ne)) = return (req, SerializeS s ne)

  monoidNativeExpr' (ManN_ (req, nm)) = return (req, ManN nm)
  monoidNativeExpr' (AppSrcN_ t src (unzip -> (reqs, es))) = return (foldl mappend' mempty' reqs, AppSrcN t src es)
  monoidNativeExpr' (ReturnN_ (req, ne)) = return (req, ReturnN ne)
  monoidNativeExpr' (SerialLetN_ i (req1, se) (req2, ne)) = return (mappend' req1 req2, SerialLetN i se ne)
  monoidNativeExpr' (NativeLetN_ i (req1, ne1) (req2, ne2)) = return (mappend' req1 req2, NativeLetN i ne1 ne2)
  monoidNativeExpr' (LetVarN_ t i) = return (mempty', LetVarN t i)
  monoidNativeExpr' (BndVarN_ t i) = return (mempty', BndVarN t i)
  monoidNativeExpr' (DeserializeN_ t s (req, e)) = return (req, DeserializeN t s e)
  monoidNativeExpr' (AccN_ o v (req, e) k) = return (req, AccN o v e k)
  monoidNativeExpr' (SrcN_ t src) = return (mempty', SrcN t src)
  monoidNativeExpr' (ListN_ v t xs) = return (foldl mappend' mempty' (map fst xs), ListN v t (map snd xs))
  monoidNativeExpr' (TupleN_ v xs) = return (foldl mappend' mempty' (map fst xs), TupleN v $ map snd xs) 
  monoidNativeExpr' (RecordN_ o v ps rs)
    = return (foldl mappend' mempty' $ map (fst . snd) rs
             , RecordN o v ps (map (second snd) rs) )
  monoidNativeExpr' (LogN_ v x) = return (mempty', LogN v x)
  monoidNativeExpr' (RealN_ v x) = return (mempty', RealN v x)
  monoidNativeExpr' (IntN_ v x) = return (mempty', IntN v x)
  monoidNativeExpr' (StrN_ v x) = return (mempty', StrN v x)
  monoidNativeExpr' (NullN_ v) = return (mempty', NullN v)



-- where
--  * m - monad
--  * sm - SerialManifold folded type
--  * nm - NativeManifold
--  * se - SerialExpr
--  * ne - NativeExpr
--  * sr - SerialArg
--  * nr - NativeArg
data FoldManifoldM m sm nm se ne sr nr = FoldManifoldM
  { opSerialManifoldM :: SerialManifold_ se -> m sm
  , opNativeManifoldM :: NativeManifold_ ne -> m nm
  , opSerialExprM :: SerialExpr_ sm se ne sr nr -> m se
  , opNativeExprM :: NativeExpr_ nm se ne sr nr -> m ne
  , opSerialArgM  :: SerialArg_ sm se -> m sr
  , opNativeArgM  :: NativeArg_ nm ne -> m nr
  }

instance (Monoid a, Monad m, a ~ b, a ~ c, a ~ d, a ~ e, a ~ f) => Defaultable (FoldManifoldM m a b c d e f) where
  defaultValue = FoldManifoldM
    { opSerialManifoldM = return . foldlSM mappend mempty
    , opNativeManifoldM = return . foldlNM mappend mempty
    , opSerialExprM = return . foldlSE mappend mempty
    , opNativeExprM = return . foldlNE mappend mempty
    , opSerialArgM = return . foldlSA mappend mempty
    , opNativeArgM = return . foldlNA mappend mempty
    }

instance (Monoid a, Monad m, a ~ b, a ~ c, a ~ d, a ~ e, a ~ f) => Defaultable (FoldWithManifoldM m a b c d e f) where
  defaultValue = FoldWithManifoldM
    { opFoldWithSerialManifoldM = \_ e -> return . foldlSM mappend mempty $ e
    , opFoldWithNativeManifoldM = \_ e -> return . foldlNM mappend mempty $ e
    , opFoldWithSerialExprM = \_ e -> return . foldlSE mappend mempty $ e
    , opFoldWithNativeExprM = \_ e -> return . foldlNE mappend mempty $ e
    , opFoldWithSerialArgM = \_ e -> return . foldlSA mappend mempty $ e
    , opFoldWithNativeArgM = \_ e -> return . foldlNA mappend mempty $ e
    }

data FoldWithManifoldM m sm nm se ne sr nr = FoldWithManifoldM
  { opFoldWithSerialManifoldM :: SerialManifold -> SerialManifold_ se -> m sm
  , opFoldWithNativeManifoldM :: NativeManifold -> NativeManifold_ ne -> m nm
  , opFoldWithSerialExprM :: SerialExpr -> SerialExpr_ sm se ne sr nr -> m se
  , opFoldWithNativeExprM :: NativeExpr -> NativeExpr_ nm se ne sr nr -> m ne
  , opFoldWithSerialArgM  :: SerialArg -> SerialArg_ sm se -> m sr
  , opFoldWithNativeArgM  :: NativeArg -> NativeArg_ nm ne -> m nr
  }

data SurroundManifoldM m sm nm se ne sr nr = SurroundManifoldM
  { surroundSerialManifoldM :: ( SerialManifold -> m sm ) -> SerialManifold -> m sm
  , surroundNativeManifoldM :: ( NativeManifold -> m nm ) -> NativeManifold -> m nm
  , surroundSerialExprM     :: ( SerialExpr -> m se )     -> SerialExpr     -> m se
  , surroundNativeExprM     :: ( NativeExpr -> m ne )     -> NativeExpr     -> m ne
  , surroundSerialArgM      :: ( SerialArg -> m sr )      -> SerialArg      -> m sr
  , surroundNativeArgM      :: ( NativeArg -> m nr )      -> NativeArg      -> m nr
  }

instance Defaultable (SurroundManifoldM m sm nm se ne sr nr) where
  defaultValue = SurroundManifoldM
    { surroundSerialManifoldM = \ f x -> f x
    , surroundNativeManifoldM = \ f x -> f x
    , surroundSerialExprM     = \ f x -> f x
    , surroundNativeExprM     = \ f x -> f x
    , surroundSerialArgM      = \ f x -> f x
    , surroundNativeArgM      = \ f x -> f x
    }

instance HasTypeF a => HasTypeM (Maybe a) where
  typeMof (Just x) = Serial (typeFof x)
  typeMof Nothing = Passthrough

class MayHaveTypeF a where
  mayHaveTypeF :: a -> Maybe TypeF

instance MayHaveTypeF TypeF where
  mayHaveTypeF = Just

instance MayHaveTypeF TypeS where
  mayHaveTypeF PassthroughS = Nothing
  mayHaveTypeF (SerialS t) = Just t
  mayHaveTypeF (FunctionS ts t) = FunF <$> mapM mayHaveTypeF ts <*> mayHaveTypeF t

instance MayHaveTypeF TypeM where
  mayHaveTypeF Passthrough = Nothing
  mayHaveTypeF (Serial t) = Just t
  mayHaveTypeF (Native t) = Just t
  mayHaveTypeF (Function ts t) = FunF <$> mapM mayHaveTypeF ts <*> mayHaveTypeF t

data NativeManifold_ ne = NativeManifold_ Int Lang (ManifoldForm (Or TypeS TypeF) TypeF) ne
data SerialManifold_ se = SerialManifold_ Int Lang (ManifoldForm (Or TypeS TypeF) TypeS) se
data SerialArg_ sm se = SerialArgManifold_ sm | SerialArgExpr_ se
data NativeArg_ nm ne = NativeArgManifold_ nm | NativeArgExpr_ ne

typeMofRs :: [Arg (Or TypeS TypeF)] -> [Arg TypeM]
typeMofRs rs = concat [[Arg i t | t <- bilist typeMof typeMof orT] | (Arg i orT) <- rs]

typeMofForm :: HasTypeM t => ManifoldForm (Or TypeS TypeF) t -> [Arg TypeM]
typeMofForm = concat . abilist (\i r -> [Arg i t | t <- bilist typeMof typeMof r]) (\i r -> [Arg i (typeMof r)])

data SerialExpr_ sm se ne sr nr
  = ManS_ sm
  | AppPoolS_ TypeF PoolCall [sr]
  | ReturnS_ se
  | SerialLetS_ Int se se
  | NativeLetS_ Int ne se
  | LetVarS_ (Maybe TypeF) Int
  | BndVarS_ (Maybe TypeF) Int
  | SerializeS_ SerialAST ne

data NativeExpr_ nm se ne sr nr
  = AppSrcN_      TypeF Source [nr]
  | ManN_         nm
  | ReturnN_      ne 
  | SerialLetN_   Int se ne
  | NativeLetN_   Int ne ne
  | LetVarN_      TypeF Int
  | BndVarN_      TypeF Int
  | DeserializeN_ TypeF SerialAST se
  | AccN_         NamType FVar ne Text
  | SrcN_         TypeF Source
  -- data types
  | ListN_        FVar TypeF [ne]
  | TupleN_       FVar [ne]
  | RecordN_      NamType FVar [TypeF] [(FVar, ne)]
  | LogN_         FVar Bool
  | RealN_        FVar Scientific
  | IntN_         FVar Integer
  | StrN_         FVar Text
  | NullN_        FVar

manifoldFoldToFoldWith :: FoldManifoldM m sm nm se ne sr nr -> FoldWithManifoldM m sm nm se ne sr nr
manifoldFoldToFoldWith fm = FoldWithManifoldM
  { opFoldWithSerialManifoldM = \_ e -> opSerialManifoldM fm e
  , opFoldWithNativeManifoldM = \_ e -> opNativeManifoldM fm e
  , opFoldWithSerialExprM = \_ e -> opSerialExprM fm e
  , opFoldWithNativeExprM = \_ e -> opNativeExprM fm e
  , opFoldWithSerialArgM = \_ e -> opSerialArgM fm e
  , opFoldWithNativeArgM = \_ e -> opNativeArgM fm e
  }

foldSerialManifoldM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> SerialManifold -> m sm
foldSerialManifoldM = surroundFoldSerialManifoldM defaultValue . manifoldFoldToFoldWith

foldNativeManifoldM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> NativeManifold -> m nm
foldNativeManifoldM = surroundFoldNativeManifoldM defaultValue . manifoldFoldToFoldWith

foldSerialArgM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> SerialArg -> m sr
foldSerialArgM = surroundFoldSerialArgM defaultValue . manifoldFoldToFoldWith

foldNativeArgM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> NativeArg -> m nr
foldNativeArgM = surroundFoldNativeArgM defaultValue . manifoldFoldToFoldWith

foldSerialExprM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> SerialExpr -> m se
foldSerialExprM = surroundFoldSerialExprM defaultValue . manifoldFoldToFoldWith

foldNativeExprM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> NativeExpr -> m ne
foldNativeExprM = surroundFoldNativeExprM defaultValue . manifoldFoldToFoldWith


foldWithSerialManifoldM :: Monad m => FoldWithManifoldM m sm nm se ne sr nr -> SerialManifold -> m sm
foldWithSerialManifoldM = surroundFoldSerialManifoldM defaultValue

foldWithNativeManifoldM :: Monad m => FoldWithManifoldM m sm nm se ne sr nr -> NativeManifold -> m nm
foldWithNativeManifoldM = surroundFoldNativeManifoldM defaultValue

foldWithSerialArgM :: Monad m => FoldWithManifoldM m sm nm se ne sr nr -> SerialArg -> m sr
foldWithSerialArgM = surroundFoldSerialArgM defaultValue

foldWithNativeArgM :: Monad m => FoldWithManifoldM m sm nm se ne sr nr -> NativeArg -> m nr
foldWithNativeArgM = surroundFoldNativeArgM defaultValue

foldWithSerialExprM :: Monad m => FoldWithManifoldM m sm nm se ne sr nr -> SerialExpr -> m se
foldWithSerialExprM = surroundFoldSerialExprM defaultValue

foldWithNativeExprM :: Monad m => FoldWithManifoldM m sm nm se ne sr nr -> NativeExpr -> m ne
foldWithNativeExprM = surroundFoldNativeExprM defaultValue


surroundFoldSerialManifoldM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldWithManifoldM m sm nm se ne sr nr -> SerialManifold -> m sm
surroundFoldSerialManifoldM sfm fm = surroundSerialManifoldM sfm f
  where
  f full@(SerialManifold m lang form e) = do
    e' <- surroundFoldSerialExprM sfm fm e
    opFoldWithSerialManifoldM fm full $ SerialManifold_ m lang form e'

surroundFoldNativeManifoldM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldWithManifoldM m sm nm se ne sr nr -> NativeManifold -> m nm
surroundFoldNativeManifoldM sfm fm = surroundNativeManifoldM sfm f
  where
  f full@(NativeManifold m lang form e) = do
    e' <- surroundFoldNativeExprM sfm fm e
    opFoldWithNativeManifoldM fm full $ NativeManifold_ m lang form e'

surroundFoldSerialArgM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldWithManifoldM m sm nm se ne sr nr -> SerialArg -> m sr
surroundFoldSerialArgM sfm fm = surroundSerialArgM sfm f
  where
  f full@(SerialArgManifold sm) = do
    sm' <- surroundFoldSerialManifoldM sfm fm sm 
    opFoldWithSerialArgM fm full $ SerialArgManifold_ sm'
  f full@(SerialArgExpr se) = do
    se' <- surroundFoldSerialExprM sfm fm se 
    opFoldWithSerialArgM fm full $ SerialArgExpr_ se'

surroundFoldNativeArgM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldWithManifoldM m sm nm se ne sr nr -> NativeArg -> m nr
surroundFoldNativeArgM sfm fm = surroundNativeArgM sfm f
  where
  f full@(NativeArgManifold nm) = do
      nm' <- surroundFoldNativeManifoldM sfm fm nm 
      opFoldWithNativeArgM fm full $ NativeArgManifold_ nm'
  f full@(NativeArgExpr ne) = do
      ne' <- surroundFoldNativeExprM sfm fm ne 
      opFoldWithNativeArgM fm full $ NativeArgExpr_ ne'

surroundFoldSerialExprM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldWithManifoldM m sm nm se ne sr nr -> SerialExpr -> m se
surroundFoldSerialExprM sfm fm = surroundSerialExprM sfm f
  where
  f full@(ManS e) = do
    e' <- surroundFoldSerialManifoldM sfm fm e
    opFoldWithSerialExprM fm full $ ManS_ e'
  f full@(AppPoolS t pool es) = do
    es' <- mapM (surroundFoldSerialArgM sfm fm) es
    opFoldWithSerialExprM fm full $ AppPoolS_ t pool es'
  f full@(ReturnS e) = do
    e' <- surroundFoldSerialExprM sfm fm e
    opFoldWithSerialExprM fm full $ ReturnS_ e'
  f full@(SerialLetS i sa sb) = do
    sa' <- surroundFoldSerialExprM sfm fm sa
    sb' <- surroundFoldSerialExprM sfm fm sb
    opFoldWithSerialExprM fm full $ SerialLetS_ i sa' sb'
  f full@(NativeLetS i na sb) = do
    sa' <- surroundFoldNativeExprM sfm fm na
    nb' <- surroundFoldSerialExprM sfm fm sb
    opFoldWithSerialExprM fm full $ NativeLetS_ i sa' nb'
  f full@(LetVarS t i) = opFoldWithSerialExprM fm full (LetVarS_ t i)
  f full@(BndVarS t i) = opFoldWithSerialExprM fm full (BndVarS_ t i)
  f full@(SerializeS s e) = do
    e' <- surroundFoldNativeExprM sfm fm e
    opFoldWithSerialExprM fm full $ SerializeS_ s e'

surroundFoldNativeExprM
  :: Monad m
  => SurroundManifoldM m sm nm se ne sr nr
  -> FoldWithManifoldM m sm nm se ne sr nr
  -> NativeExpr
  -> m ne
surroundFoldNativeExprM sfm fm = surroundNativeExprM sfm f
  where
  f full@(AppSrcN t src nativeArgs) = do
      nativeArgs' <- mapM (surroundFoldNativeArgM sfm fm) nativeArgs
      opFoldWithNativeExprM fm full $ AppSrcN_ t src nativeArgs'
  f full@(ManN nativeManifold) = do
      nativeManifold' <- surroundFoldNativeManifoldM sfm fm nativeManifold 
      opFoldWithNativeExprM fm full $ ManN_ nativeManifold'
  f full@(ReturnN ne) = do
      ne' <- surroundFoldNativeExprM sfm fm ne
      opFoldWithNativeExprM fm full $ ReturnN_ ne'
  f full@(SerialLetN i se1 ne2) = do
      se1' <- surroundFoldSerialExprM sfm fm se1
      ne2' <- surroundFoldNativeExprM sfm fm ne2
      opFoldWithNativeExprM fm full (SerialLetN_ i se1' ne2')
  f full@(NativeLetN i ne1 ne2) = do
      ne1' <- surroundFoldNativeExprM sfm fm ne1
      ne2' <- surroundFoldNativeExprM sfm fm ne2
      opFoldWithNativeExprM fm full (NativeLetN_ i ne1' ne2')
  f full@(LetVarN t i) = opFoldWithNativeExprM fm full (LetVarN_ t i)
  f full@(BndVarN t i) = opFoldWithNativeExprM fm full (BndVarN_ t i)
  f full@(DeserializeN t s se) = do
      se' <- surroundFoldSerialExprM sfm fm se
      opFoldWithNativeExprM fm full (DeserializeN_ t s se')
  f full@(AccN n v ne key) = do
      ne' <- surroundFoldNativeExprM sfm fm ne
      opFoldWithNativeExprM fm full (AccN_ n v ne' key)
  f full@(SrcN t src) = opFoldWithNativeExprM fm full (SrcN_ t src)
  f full@(ListN v t nes) = do
      nes' <- mapM (surroundFoldNativeExprM sfm fm) nes
      opFoldWithNativeExprM fm full (ListN_ v t nes')
  f full@(TupleN t nes) = do
      nes' <- mapM (surroundFoldNativeExprM sfm fm) nes
      opFoldWithNativeExprM fm full (TupleN_ t nes')
  f full@(RecordN o n ps rs) = do
      rs' <- mapM (onSndM (surroundFoldNativeExprM sfm fm)) rs
      opFoldWithNativeExprM fm full (RecordN_ o n ps rs')
      where
        onSndM :: Monad m => (b -> m b') -> (a, b) -> m (a, b')
        onSndM g (a, b) = (,) a <$> g b 
  f full@(LogN t x)  = opFoldWithNativeExprM fm full (LogN_ t x)
  f full@(RealN t x) = opFoldWithNativeExprM fm full (RealN_ t x)
  f full@(IntN t x)  = opFoldWithNativeExprM fm full (IntN_ t x)
  f full@(StrN t x)  = opFoldWithNativeExprM fm full (StrN_ t x)
  f full@(NullN t)   = opFoldWithNativeExprM fm full (NullN_ t)


class HasTypeF a where
  typeFof :: a -> TypeF

instance HasTypeF TypeF where
  typeFof = id

instance HasTypeF NativeExpr where
  typeFof (ManN nm) = typeFof nm
  typeFof (AppSrcN      t _ _) = t
  typeFof (ReturnN      e) = typeFof e
  typeFof (SerialLetN   _ _ e) = typeFof e
  typeFof (NativeLetN   _ _ e) = typeFof e
  typeFof (LetVarN      t _) = t
  typeFof (BndVarN      t _) = t
  typeFof (DeserializeN t _ _) = t
  typeFof (AccN           _ _ (typeFof -> NamF _ _ _ rs) key) =
    -- NOTE: This will fail if the key does not exist. However, non-existence of
    -- a key should have been caught by the typechecker. So such non-existence
    -- here indicates a but in the compiler and should die immediately.
    fromJust . listToMaybe $ [t | (FV _ key', t) <- rs, key' == key]
  typeFof AccN{} = error "Bug - illegal key access should have been caught in the typechecker"
  typeFof (SrcN         t _) = t
  typeFof (ListN        v p _) = AppF (VarF v) [p]
  typeFof (TupleN       v (map typeFof -> ps)) = AppF (VarF v) ps
  typeFof (RecordN      o n ps (map (second typeFof) -> rs)) = NamF o n ps rs
  typeFof (LogN         v _) = VarF v
  typeFof (RealN        v _) = VarF v
  typeFof (IntN         v _) = VarF v
  typeFof (StrN         v _) = VarF v
  typeFof (NullN        v  ) = VarF v

instance HasTypeF TypeP where
  typeFof (UnkP v) = UnkF (pvar2fvar v)
  typeFof (VarP v) = VarF (pvar2fvar v)
  typeFof (FunP ts t) = FunF (map typeFof ts) (typeFof t)
  typeFof (AppP t ts) = AppF (typeFof t) (map typeFof ts)
  typeFof (NamP o v ds rs) = NamF o (pvar2fvar v) (map typeFof ds) (map (bimap pvar2fvar typeFof) rs)

class HasTypeM e where
  typeMof :: e -> TypeM

instance HasTypeM TypeM where
  typeMof = id

instance HasTypeM TypeP where
  typeMof (UnkP _) = Passthrough
  typeMof (FunP ts t) = Function (map typeMof ts) (typeMof t)
  typeMof t = Native (typeFof t)

instance HasTypeM TypeF where
  typeMof (FunF ts t) = Function (map typeMof ts) (typeMof t)
  typeMof (UnkF _) = Passthrough
  typeMof t = Native t

instance HasTypeM NativeExpr where
  typeMof = typeMof . typeFof

instance HasTypeS TypeF where
  typeSof (FunF ts t) = FunctionS (map typeMof ts) (typeSof t)
  typeSof t = SerialS t

instance HasTypeS (Maybe TypeF) where
  typeSof (Just t) = typeSof t
  typeSof Nothing = PassthroughS

-- TODO: fix this - the type of a native manifold should be the full function
-- type, but the manifold function type may not be entirely native
instance HasTypeF NativeManifold where
  typeFof (NativeManifold _ _ _ ne) = typeFof ne

instance HasTypeS SerialExpr where
  typeSof (ManS sm) = typeSof sm
  typeSof (AppPoolS t _ sargs) = FunctionS (map typeMof sargs) (SerialS t) 
  typeSof (ReturnS e) = typeSof e
  typeSof (SerialLetS _ _ e) = typeSof e
  typeSof (NativeLetS _ _ e) = typeSof e
  typeSof (LetVarS t _) = maybe PassthroughS SerialS t
  typeSof (BndVarS t _) = maybe PassthroughS SerialS t
  typeSof (SerializeS _ e) = SerialS (typeFof e)

instance HasTypeM SerialExpr where
  typeMof = typeMof . typeSof

instance HasTypeM NativeManifold where
  typeMof (NativeManifold _ _ form e) = typeOfManifold form (typeMof e)

instance HasTypeM SerialManifold where
  typeMof (SerialManifold _ _ form e) = typeOfManifold form (typeMof e)

instance HasTypeS SerialManifold where
  typeSof (SerialManifold _ _ form e) =
    let inputTypes = concat $ bilist (bilist typeMof typeMof) (return . typeMof) form
    in case inputTypes of
        [] -> typeSof e
        _ -> FunctionS inputTypes (typeSof e)

instance HasTypeS SerialArg where
  typeSof (SerialArgManifold x) = typeSof x
  typeSof (SerialArgExpr x) = typeSof x

typeOfManifold :: (HasTypeM e) => ManifoldForm (Or TypeS TypeF) e -> TypeM -> TypeM
typeOfManifold form outputType =
    let inputTypes = concat $ bilist (bilist typeMof typeMof) (return . typeMof) form
    in case inputTypes of
        [] -> outputType
        _ -> Function inputTypes outputType

instance HasTypeM SerialArg where 
  typeMof (SerialArgManifold sm) = typeMof sm
  typeMof (SerialArgExpr e) = typeMof e

instance HasTypeM NativeArg where 
  typeMof (NativeArgManifold sm) = typeMof sm
  typeMof (NativeArgExpr e) = typeMof e

-- | Generate one or two types from an ArgType. These types may be native,
-- serial, or (serial, native) (in that order). The serial types are rendered in
-- the serial form, currently strings. Note that this operation erases the type
-- annotation for the serial type, if it exists.
argTypesToTypeM :: ArgTypes -> [TypeM]
argTypesToTypeM (SerialOnly (typeMof -> t)) = [t]
argTypesToTypeM (NativeOnly (typeMof -> t)) = [t]
argTypesToTypeM (SerialAndNative t) = argTypesToTypeM (SerialOnly (SerialS t)) <> argTypesToTypeM (NativeOnly t)

data ManifoldMap = ManifoldMap
 { mapSerialManifold :: SerialManifold -> SerialManifold
 , mapNativeManifold :: NativeManifold -> NativeManifold
 , mapSerialExpr :: SerialExpr -> SerialExpr
 , mapNativeExpr :: NativeExpr -> NativeExpr
 , mapSerialArg :: SerialArg -> SerialArg
 , mapNativeArg :: NativeArg -> NativeArg
 }

instance Defaultable ManifoldMap where
  defaultValue = ManifoldMap
    { mapSerialManifold = id
    , mapNativeManifold = id
    , mapSerialExpr = id
    , mapNativeExpr = id
    , mapSerialArg = id
    , mapNativeArg = id
    }

data GateMap = GateMap
 { gateSerialManifold :: SerialManifold -> Bool
 , gateNativeManifold :: NativeManifold -> Bool
 , gateSerialExpr :: SerialExpr -> Bool
 , gateNativeExpr :: NativeExpr -> Bool
 , gateSerialArg :: SerialArg -> Bool
 , gateNativeArg :: NativeArg -> Bool
 }

instance Defaultable GateMap where
  defaultValue = GateMap
    { gateSerialManifold = const True
    , gateNativeManifold = const True
    , gateSerialExpr = const True
    , gateNativeExpr = const True
    , gateSerialArg = const True
    , gateNativeArg = const True
    }

class MFunctor a where
    mgatedMap :: GateMap -> ManifoldMap -> a -> a

    mmap :: ManifoldMap -> a -> a
    mmap = mgatedMap defaultValue

instance MFunctor NativeManifold where
    mgatedMap g f nm@(NativeManifold m l form ne)
      | gateNativeManifold g nm = mapNativeManifold f $ NativeManifold m l form (mgatedMap g f ne)
      | otherwise = mapNativeManifold f nm
        

instance MFunctor SerialManifold where
    mgatedMap g f sm@(SerialManifold m l form se)
      | gateSerialManifold g sm = mapSerialManifold f $ SerialManifold m l form (mgatedMap g f se)
      | otherwise = mapSerialManifold f sm
        

instance MFunctor SerialArg where
  mgatedMap g f sr
    | gateSerialArg g sr = case sr of
        (SerialArgManifold sm) -> mapSerialArg f $ SerialArgManifold (mgatedMap g f sm)
        (SerialArgExpr se) -> mapSerialArg f $ SerialArgExpr (mgatedMap g f se)
    | otherwise = mapSerialArg f sr

instance MFunctor NativeArg where
  mgatedMap g f nr
    | gateNativeArg g nr = case nr of
        (NativeArgManifold nm) -> mapNativeArg f $ NativeArgManifold (mgatedMap g f nm)
        (NativeArgExpr ne) -> mapNativeArg f $ NativeArgExpr (mgatedMap g f ne)
    | otherwise = mapNativeArg f nr

instance MFunctor SerialExpr where
  mgatedMap g f se0
    | gateSerialExpr g se0 = case se0 of
        (ManS sm) -> mapSerialExpr f $ ManS (mgatedMap g f sm)
        (AppPoolS t p serialArgs) -> mapSerialExpr f $ AppPoolS t p (map (mgatedMap g f) serialArgs)
        (ReturnS se) -> mapSerialExpr f $ ReturnS (mgatedMap g f se)
        (SerialLetS i se1 se2) -> mapSerialExpr f $ SerialLetS i (mgatedMap g f se1) (mgatedMap g f se2)
        (NativeLetS i ne1 se2) -> mapSerialExpr f $ NativeLetS i (mgatedMap g f ne1) (mgatedMap g f se2)
        e@(LetVarS _ _) -> mapSerialExpr f e
        e@(BndVarS _ _) -> mapSerialExpr f e
        (SerializeS s ne) -> mapSerialExpr f $ SerializeS s (mgatedMap g f ne)
    | otherwise = mapSerialExpr f se0

-- WARNING - mapping must not change the type of any argument
instance MFunctor NativeExpr where
  mgatedMap g f ne0
    | gateNativeExpr g ne0 = case ne0 of
        (AppSrcN t src nativeArgs) -> mapNativeExpr f $ AppSrcN t src (map (mgatedMap g f) nativeArgs)
        (ManN nm) -> mapNativeExpr f $ ManN (mgatedMap g f nm)
        (ReturnN ne) -> mapNativeExpr f $ ReturnN (mgatedMap g f ne)
        (SerialLetN i se ne) -> mapNativeExpr f $ SerialLetN i (mgatedMap g f se) (mgatedMap g f ne)
        (NativeLetN i ne1 ne2) -> mapNativeExpr f $ NativeLetN i (mgatedMap g f ne1) (mgatedMap g f ne2)
        e@(LetVarN _ _) -> mapNativeExpr f e
        e@(BndVarN _ _) -> mapNativeExpr f e
        (DeserializeN t s se ) -> mapNativeExpr f $ DeserializeN t s (mgatedMap g f se)
        (AccN o v ne key) -> mapNativeExpr f $ AccN o v (mgatedMap g f ne) key
        e@(SrcN _ _) -> mapNativeExpr f e
        (ListN v t nes) -> mapNativeExpr f $ ListN v t (map (mgatedMap g f) nes)
        (TupleN v xs) -> mapNativeExpr f $ TupleN v (map (mgatedMap g f) xs)
        (RecordN o v ps rs) -> mapNativeExpr f $ RecordN o v ps (map (second (mgatedMap g f)) rs)
        e@(LogN _ _) -> mapNativeExpr f e
        e@(RealN _ _) -> mapNativeExpr f e
        e@(IntN _ _) -> mapNativeExpr f e
        e@(StrN _ _) -> mapNativeExpr f e
        e@(NullN _) -> mapNativeExpr f e
    | otherwise = mapNativeExpr f ne0


instance HasOneLanguage PVar where
  langOf' (PV lang _ _) = lang

instance HasOneLanguage TypeP where
  langOf' (UnkP (PV lang _ _)) = lang
  langOf' (VarP (PV lang _ _)) = lang
  langOf' (FunP _ t) = langOf' t 
  langOf' (AppP t _) = langOf' t
  langOf' (NamP _ (PV lang _ _) _ _) = lang

instance (Pretty a) => Pretty (Arg a) where
  pretty (Arg i x) = "x" <> pretty i <> braces (pretty x)

instance Pretty PVar where
  pretty (PV lang (Just g) t) = parens (pretty g <+> "|" <+> pretty t <> "@" <> pretty lang)
  pretty (PV lang Nothing t) = parens ("*" <+> "|" <+> pretty t <> "@" <> pretty lang)

instance Pretty TypeP where
  pretty = pretty . typeOf

instance Pretty TypeF where
  pretty = viaShow

instance Pretty TypeM where
  pretty Passthrough = "Passthrough"
  pretty (Serial c) = "Serial{" <> pretty c <> "}"
  pretty (Native c) = "Native{" <> pretty c <> "}"
  pretty (Function ts t) =
    nest 4 (vsep $ ["Function{"] <> map (\x -> pretty x <+> "->") ts <> [pretty t <> "}"] )

instance Pretty PolyHead where
    pretty _ = "PolyHead stub"

instance Pretty PolyExpr where
    pretty _ = "PolyExpr stub"

instance Pretty MonoExpr where
    pretty (MonoManifold i form e)
      = block 4 ("m" <> pretty i <> tupled (abilist contextArg boundArg form)) (pretty e) where
        contextArg j _ = "c" <> pretty j
        boundArg j _ = "b" <> pretty j
    pretty (MonoPoolCall t i _ _) =  "PoolCall" <> parens (pretty i) <> parens (pretty t)
    pretty (MonoLet i e1 e2) = vsep ["let" <+> "x" <> pretty i <+> "=" <+> pretty e1, pretty e2]
    pretty (MonoLetVar t i) = parens $ "x" <> pretty i <> " :: " <> pretty t
    pretty (MonoReturn e) = "return" <> parens (pretty e)
    pretty (MonoApp e es) = parens (pretty e) <+> hsep (map (parens . pretty) es)
    pretty (MonoSrc    _ src) = pretty src
    pretty (MonoBndVar Nothing i) = parens $ "x" <> pretty i <+> ":" <+> "<unknown>"
    pretty (MonoBndVar (Just t) i) = parens $ "x" <> pretty i <+> ":" <+> pretty t
    pretty (MonoAcc    t n v e k) = parens (pretty e) <> "@" <> pretty k
    pretty (MonoList   _ _ es) = list (map pretty es)
    pretty (MonoTuple  v (map fst -> es)) = tupled (map pretty es)
    pretty (MonoRecord o v fs rs)
        = block 4 (pretty o <+> pretty v <> encloseSep "<" ">" "," (map pretty fs)) "manifold record stub"
    pretty (MonoLog    _ x) = viaShow x
    pretty (MonoReal   _ x) = viaShow x
    pretty (MonoInt    _ x) = viaShow x
    pretty (MonoStr    _ x) = viaShow x
    pretty (MonoNull   _) = "NULL"


instance Pretty PoolCall where
    pretty _ = "PoolCall stub"


prettyGenTypeP :: TypeP -> MDoc
prettyGenTypeP _ = "prettyGenTypeP stub" 
