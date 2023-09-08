{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

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
  , typeFof
  , pvar2fvar
  , fvar2pvar
  , typeP2typeF
  , typeF2typeP
  , typeF2typeM
  -- ** 
  , Arg(..), argId, unArg
  , JsonType(..)
  , JsonPath
  , JsonAccessor(..)
  , NexusCommand(..)
  , ManifoldForm(..)
  -- ** Manifold data types
  , PolyHead(..)
  , PolyExpr(..)
  , MonoHead(..)
  , MonoExpr(..)
  , PoolCall(..)

  , MFunctor(..)
  , ManifoldMap(..)
  , defaultManifoldMap
  , NativeManifold(..)
  , SerialManifold(..)
  , SerialArg(..)
  , NativeArg(..)
  , SerialExpr(..)
  , NativeExpr(..)
  -- unrecursive types
  , NativeManifold_(..)
  , SerialManifold_(..)
  , SerialArg_(..)
  , NativeArg_(..)
  , SerialExpr_(..)
  , NativeExpr_(..)
  -- fully recursive
  , NativeManifold6(..)
  , SerialManifold6(..)
  , SerialArg6(..)
  , NativeArg6(..)
  , SerialExpr6(..)
  , NativeExpr6(..)
  , FoldManifold6M(..)
  , foldSerialManifold6M
  , foldNativeManifold6M
  , foldSerialExpr6M
  , foldNativeExpr6M
  , foldNativeArg6M
  , foldSerialArg6M
  -- ** Serialization AST
  , SerialAST(..)
  , TypePacker(..)
  , ResolvedPacker(..)
  -- ** Other
  , manifoldArgs
  , mapManifoldArgs
  , mapManifoldArgsM
  , generalTypeOf
  , prettyGenTypeP
  -- ** weird folds
  , mapTo
  , FoldManifoldM(..)
  , foldSerialManifoldM
  , foldNativeManifoldM
  , foldSerialExprM
  , foldNativeExprM
  , foldNativeArgM
  , foldSerialArgM
  ) where

import Morloc.Namespace
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Set as Set
import Morloc.Data.Doc
import qualified Morloc.Data.Text as MT
import Morloc.Pretty ()
import qualified Data.Foldable as DF
import qualified Morloc.Language as ML

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

-- The concrete only type, the only problem is that I do not ever want this.
-- Even in the pool generators, I still want to be able to generate docs strings
-- with the general types. Though currently I am not doing so.
data TypeC
  = UnkC Text
  | VarC Text
  | FunC [TypeC] TypeC
  | AppC TypeC [TypeC]
  | NamC NamType Text [TypeC] [(Text, TypeC)]
  deriving (Show, Ord, Eq)

pvar2fvar :: PVar -> FVar
pvar2fvar (PV _ (Just g) c) = FV g c
pvar2fvar (PV _ Nothing c) = FV "<undefined>" c

fvar2pvar :: Lang -> FVar -> PVar
fvar2pvar l (FV g c) = PV l (Just g) c

typeP2typeF :: TypeP -> TypeF
typeP2typeF (UnkP v) = UnkF (pvar2fvar v)
typeP2typeF (VarP v) = VarF (pvar2fvar v)
typeP2typeF (FunP ts t) = FunF (map typeP2typeF ts) (typeP2typeF t)
typeP2typeF (AppP t ts) = AppF (typeP2typeF t) (map typeP2typeF ts)
typeP2typeF (NamP o v ds rs) = NamF o (pvar2fvar v) (map typeP2typeF ds) (map (bimap pvar2fvar typeP2typeF) rs)

typeF2typeP :: Lang -> TypeF -> TypeP
typeF2typeP l (UnkF v) = UnkP (fvar2pvar l v)
typeF2typeP l (VarF v) = VarP (fvar2pvar l v)
typeF2typeP l (FunF ts t) = FunP (map (typeF2typeP l) ts) (typeF2typeP l t)
typeF2typeP l (AppF t ts) = AppP (typeF2typeP l t) (map (typeF2typeP l) ts)
typeF2typeP l (NamF o v ds rs) = NamP o (fvar2pvar l v) (map (typeF2typeP l) ds) (map (bimap (fvar2pvar l) (typeF2typeP l)) rs)

typeF2typeM :: TypeF -> TypeM
typeF2typeM (FunF ts t) = Function (map typeF2typeM ts) (typeF2typeM t)
typeF2typeM (UnkF _) = Passthrough
typeF2typeM t = Native t

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


data Arg a = Arg Int a
    deriving(Show, Eq, Ord)

instance Functor Arg where
    fmap f (Arg i x) = Arg i (f x)

unArg :: Arg a -> a
unArg (Arg _ x) = x

argId :: Arg a -> Int
argId (Arg i _) = i

data TypeM
  = Passthrough -- ^ serialized data that is not deserialized (and may not be representable) in this segment
  | Serial TypeF -- ^ serialized data that may be deserialized in this language
  | Native TypeF -- ^ an unserialized native data type
  | Function [TypeM] TypeM -- ^ a function of n inputs and one output (cannot be serialized)
  deriving(Show, Eq, Ord)


data (ManifoldForm a)
  = ManifoldPass [Arg a]
  -- ^ Unapplied function passed as argument
  | ManifoldFull [Arg a]
  -- ^ Fully applied function
  | ManifoldPart [Arg a] [Arg a]
  -- ^ Partially applied function
  deriving(Show, Eq, Ord)

instance Functor ManifoldForm where
  fmap f (ManifoldPass xs) = ManifoldPass (map (fmap f) xs)
  fmap f (ManifoldFull xs) = ManifoldFull (map (fmap f) xs)
  fmap f (ManifoldPart xs ys) = ManifoldPart (map (fmap f) xs) (map (fmap f) ys)

instance Foldable ManifoldForm where
  foldr f b (ManifoldPass xs) = foldr f b [x | Arg _ x <- xs]
  foldr f b (ManifoldFull xs) = foldr f b [x | Arg _ x <- xs]
  foldr f b (ManifoldPart xs ys) = foldr f b [x | Arg _ x <- xs <> ys]


instance Pretty t => Pretty (ManifoldForm t) where
    pretty (ManifoldPass args) = "ManifoldPass" <> tupled (map pretty args)
    pretty (ManifoldFull args) = "ManifoldFull"  <> tupled (map pretty args)
    pretty (ManifoldPart contextArgs boundArgs)
        = "ManifoldPart" <> tupled
            [ "context:" <+> list (map pretty contextArgs)
            , "bound:"   <+> list (map pretty boundArgs)
            ]

instance Pretty FVar where
    pretty (FV _ c) = pretty c

manifoldArgs :: ManifoldForm t -> [Arg t]
manifoldArgs (ManifoldPass args) = args
manifoldArgs (ManifoldFull args) = args
manifoldArgs (ManifoldPart contextArgs boundArgs) = contextArgs <> boundArgs

mapManifoldArgs :: (Arg a -> Arg b) -> ManifoldForm a -> ManifoldForm b
mapManifoldArgs f (ManifoldPass args) = ManifoldPass (map f args)
mapManifoldArgs f (ManifoldFull args) = ManifoldFull (map f args)
mapManifoldArgs f (ManifoldPart contextArgs boundArgs) = ManifoldPart (map f contextArgs) (map f boundArgs)

mapManifoldArgsM :: Monad m => (Arg a -> m (Arg b)) -> ManifoldForm a -> m (ManifoldForm b)
mapManifoldArgsM f (ManifoldPass args) = ManifoldPass <$> mapM f args
mapManifoldArgsM f (ManifoldFull args) = ManifoldFull <$> mapM f args
mapManifoldArgsM f (ManifoldPart contextArgs boundArgs) = ManifoldPart <$> mapM f contextArgs <*> mapM f boundArgs

data PolyHead = PolyHead Int [Arg None] PolyExpr

-- no serialization and no argument types
data PolyExpr
  -- organizational terms that may have undefined types
  = PolyManifold Int (ManifoldForm None) PolyExpr
  | PolyForeignInterface
      Lang     -- calling lang
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
  = MonoManifold Int (ManifoldForm None) MonoExpr
  | MonoPoolCall
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

data NativeManifold = NativeManifold Int Lang (ManifoldForm TypeM) (TypeF, NativeExpr)
data SerialManifold = SerialManifold Int Lang (ManifoldForm TypeM) SerialExpr
data SerialArg = SerialArgManifold SerialManifold | SerialArgExpr SerialExpr
data NativeArg = NativeArgManifold NativeManifold | NativeArgExpr NativeExpr

data SerialExpr
  = AppManS SerialManifold [Either SerialArg NativeArg]
  | AppPoolS PoolCall [SerialArg]
  | ReturnS SerialExpr
  | SerialLetS Int SerialExpr SerialExpr
  | NativeLetS Int (TypeF, NativeExpr) SerialExpr
  | LetVarS Int
  | BndVarS Int
  | SerializeS SerialAST NativeExpr

data NativeExpr
  = AppSrcN      TypeF Source [NativeArg]
  | AppManN      TypeF NativeManifold [Either SerialArg NativeArg]
  | ReturnN      TypeF NativeExpr
  | SerialLetN   Int SerialExpr (TypeF, NativeExpr)
  | NativeLetN   Int (TypeF, NativeExpr) (TypeF, NativeExpr)
  | LetVarN      TypeF Int
  | BndVarN      TypeF Int
  | DeserializeN TypeF SerialAST SerialExpr
  | AccN         TypeF NamType FVar NativeExpr Text
  | SrcN         TypeF Source
  -- data types
  | ListN        FVar TypeF [NativeExpr]
  | TupleN       FVar [(TypeF, NativeExpr)]
  | RecordN      NamType FVar [TypeF] [(FVar, (TypeF, NativeExpr))]
  | LogN         FVar Bool
  | RealN        FVar Scientific
  | IntN         FVar Integer
  | StrN         FVar Text
  | NullN        FVar

data NativeManifold_ a = NativeManifold_ Int Lang (ManifoldForm TypeM) (TypeF, a)
  deriving(Functor, Foldable)

data SerialManifold_ a = SerialManifold_ Int Lang (ManifoldForm TypeM) a
  deriving(Functor, Foldable)

data SerialArg_ a = SerialArgManifold_ a | SerialArgExpr_ a
  deriving(Functor, Foldable)

data NativeArg_ a = NativeArgManifold_ a | NativeArgExpr_ a
  deriving(Functor, Foldable)

data SerialExpr_ a
  = AppManS_ a [Either a a]
  | AppPoolS_ PoolCall [a]
  | ReturnS_ a
  | SerialLetS_ Int a a
  | NativeLetS_ Int (TypeF, a) a
  | LetVarS_ Int
  | BndVarS_ Int
  | SerializeS_ SerialAST a

data NativeExpr_ a
  = AppSrcN_      TypeF Source [a]
  | AppManN_      TypeF a [Either a a]
  | ReturnN_      TypeF a 
  | SerialLetN_   Int a (TypeF, a)
  | NativeLetN_   Int (TypeF, a) (TypeF, a)
  | LetVarN_      TypeF Int
  | BndVarN_      TypeF Int
  | DeserializeN_ TypeF SerialAST a
  | AccN_         TypeF NamType FVar a Text
  | SrcN_         TypeF Source
  -- data types
  | ListN_        FVar TypeF [a]
  | TupleN_       FVar [(TypeF, a)]
  | RecordN_      NamType FVar [TypeF] [(FVar, (TypeF, a))]
  | LogN_         FVar Bool
  | RealN_        FVar Scientific
  | IntN_         FVar Integer
  | StrN_         FVar Text
  | NullN_        FVar

instance Foldable SerialExpr_ where
  foldr f b (AppManS_ x eitherXs) = foldr f b (x: map catEither eitherXs)
  foldr f b (AppPoolS_ _ xs) = foldr f b xs
  foldr f b (ReturnS_ x) = f x b
  foldr f b (SerialLetS_ _ x1 x2) = foldr f b [x1, x2]
  foldr f b (NativeLetS_ _ (_, x1) x2) = foldr f b [x1, x2]
  foldr _ b (LetVarS_ _) = b
  foldr _ b (BndVarS_ _) = b
  foldr f b (SerializeS_ _ x) = f x b

instance Foldable NativeExpr_ where
  foldr f b (AppSrcN_      _ _ xs) = foldr f b xs
  foldr f b (AppManN_      _ x eitherXs) = foldr f b (x : map catEither eitherXs)
  foldr f b (ReturnN_      _ x ) = f x b
  foldr f b (SerialLetN_   _ x1 (_, x2)) = foldr f b [x1, x2]
  foldr f b (NativeLetN_   _ (_, x1) (_, x2)) = foldr f b [x1, x2]
  foldr _ b (LetVarN_      _ _) = b
  foldr _ b (BndVarN_      _ _) = b
  foldr f b (DeserializeN_ _ _ x) = f x b 
  foldr f b (AccN_         _ _ _ x _) =  f x b
  foldr _ b (SrcN_         _ _) = b
  foldr f b (ListN_        _ _ xs) = foldr f b xs
  foldr f b (TupleN_       _ xs) = foldr (f . snd) b xs
  foldr f b (RecordN_      _ _ _ rs) = foldr f b (map (snd . snd) rs)
  foldr _ b (LogN_         _ _) = b
  foldr _ b (RealN_        _ _) = b
  foldr _ b (IntN_         _ _) = b
  foldr _ b (StrN_         _ _) = b
  foldr _ b (NullN_        _) = b

foldSerialManifoldM :: Monad m => FoldManifoldM m a -> SerialManifold -> m a
foldSerialManifoldM fm (SerialManifold m lang form e) = do
  e' <- foldSerialExprM fm e
  opSerialManifoldM fm $ SerialManifold_ m lang form e'

foldNativeManifoldM :: Monad m => FoldManifoldM m a -> NativeManifold -> m a
foldNativeManifoldM fm (NativeManifold m lang form (t, e)) = do
  e' <- foldNativeExprM fm e
  opNativeManifoldM fm $ NativeManifold_ m lang form (t, e')

foldNativeArgM :: Monad m => FoldManifoldM m a -> NativeArg -> m a
foldNativeArgM fm (NativeArgManifold e) = do
  e' <- foldNativeManifoldM fm e
  opNativeArgM fm $ NativeArgManifold_ e'
foldNativeArgM fm (NativeArgExpr e) = do
  e' <- foldNativeExprM fm e
  opNativeArgM fm $ NativeArgExpr_ e'

foldSerialArgM :: Monad m => FoldManifoldM m a -> SerialArg -> m a
foldSerialArgM fm (SerialArgManifold e) = do
  e' <- foldSerialManifoldM fm e
  opSerialArgM fm $ SerialArgManifold_ e'
foldSerialArgM fm (SerialArgExpr e) = do
  e' <- foldSerialExprM fm e
  opSerialArgM fm $ SerialArgExpr_ e'

foldSerialExprM :: Monad m => FoldManifoldM m a -> SerialExpr -> m a
foldSerialExprM fm (AppManS e es) = do
    e' <- foldSerialManifoldM fm e
    es' <- mapM (mapEitherM (foldSerialArgM fm) (foldNativeArgM fm)) es
    opSerialExprM fm $ AppManS_ e' es'
foldSerialExprM fm (AppPoolS pool es) = do
    es' <- mapM (foldSerialArgM fm) es
    opSerialExprM fm $ AppPoolS_ pool es'
foldSerialExprM fm (ReturnS e) = do
    e' <- foldSerialExprM fm e
    opSerialExprM fm $ ReturnS_ e'
foldSerialExprM fm (SerialLetS i sa sb) = do
    sa' <- foldSerialExprM fm sa
    sb' <- foldSerialExprM fm sb
    opSerialExprM fm $ SerialLetS_ i sa' sb'
foldSerialExprM fm (NativeLetS i (t, na) sb) = do
    sa' <- foldNativeExprM fm na
    nb' <- foldSerialExprM fm sb
    opSerialExprM fm $ NativeLetS_ i (t, sa') nb'
foldSerialExprM fm (LetVarS i) = opSerialExprM fm (LetVarS_ i)
foldSerialExprM fm (BndVarS i) = opSerialExprM fm (BndVarS_ i)
foldSerialExprM fm (SerializeS s e) = do
    e' <- foldNativeExprM fm e
    opSerialExprM fm $ SerializeS_ s e'

foldNativeExprM :: Monad m => FoldManifoldM m a -> NativeExpr -> m a
foldNativeExprM fm (AppSrcN t src nativeArgs) = do
    nativeArgs' <- mapM (foldNativeArgM fm) nativeArgs
    opNativeExprM fm $ AppSrcN_ t src nativeArgs'
foldNativeExprM fm (AppManN t nativeManifold eargs) = do
    nativeManifold' <- foldNativeManifoldM fm nativeManifold 
    eargs' <- mapM (mapEitherM (foldSerialArgM fm) (foldNativeArgM fm)) eargs
    opNativeExprM fm $ AppManN_ t nativeManifold' eargs'
foldNativeExprM fm (ReturnN t ne) = do
    ne' <- foldNativeExprM fm ne
    opNativeExprM fm $ ReturnN_ t ne'
foldNativeExprM fm (SerialLetN i se1 (t, ne2)) = do
    se1' <- foldSerialExprM fm se1
    ne2' <- foldNativeExprM fm ne2
    opNativeExprM fm (SerialLetN_ i se1' (t, ne2'))
foldNativeExprM fm (NativeLetN i (t1, ne1) (t2, ne2)) = do
    ne1' <- foldNativeExprM fm ne1
    ne2' <- foldNativeExprM fm ne2
    opNativeExprM fm (NativeLetN_ i (t1, ne1') (t2, ne2'))
foldNativeExprM fm (LetVarN t i) = opNativeExprM fm (LetVarN_ t i)
foldNativeExprM fm (BndVarN t i) = opNativeExprM fm (BndVarN_ t i)
foldNativeExprM fm (DeserializeN t s se) = do
    se' <- foldSerialExprM fm se
    opNativeExprM fm (DeserializeN_ t s se')
foldNativeExprM fm (AccN t n v ne key) = do
    ne' <- foldNativeExprM fm ne
    opNativeExprM fm (AccN_ t n v ne' key)
foldNativeExprM fm (SrcN t src) = opNativeExprM fm (SrcN_ t src)
foldNativeExprM fm (ListN v t nes) = do
    nes' <- mapM (foldNativeExprM fm) nes
    opNativeExprM fm (ListN_ v t nes')
foldNativeExprM fm (TupleN t nes) = do
    nes' <- mapM (onSndM (foldNativeExprM fm)) nes
    opNativeExprM fm (TupleN_ t nes')
    where
    onSndM :: Monad m => (b -> m b') -> (a, b) -> m (a, b')
    onSndM f (a, b) = (,) a <$> f b 
foldNativeExprM fm (RecordN o n ps rs) = do
    rs' <- mapM (onValM (foldNativeExprM fm)) rs
    opNativeExprM fm (RecordN_ o n ps rs')
    where
    onValM :: Monad m => (c -> m c') -> (a, (b, c)) -> m (a, (b, c')) 
    onValM f (a, (b, c)) = do
        c' <- f c
        return (a, (b, c'))

foldNativeExprM fm (LogN t x)  = opNativeExprM fm (LogN_ t x)
foldNativeExprM fm (RealN t x) = opNativeExprM fm (RealN_ t x)
foldNativeExprM fm (IntN t x)  = opNativeExprM fm (IntN_ t x)
foldNativeExprM fm (StrN t x)  = opNativeExprM fm (StrN_ t x)
foldNativeExprM fm (NullN t)   = opNativeExprM fm (NullN_ t)


-- where
--  * m - monad
--  * sm - SerialManifold folded type
--  * nm - NativeManifold
--  * se - SerialExpr
--  * ne - NativeExpr
--  * sr - SerialArg
--  * nr - NativeArg
data FoldManifold6M m sm nm se ne sr nr = FoldManifold6M
 { opSerialManifold6M :: SerialManifold6 se -> m sm
 , opNativeManifold6M :: NativeManifold6 ne -> m nm
 , opSerialExpr6M :: SerialExpr6 sm se ne sr nr -> m se
 , opNativeExpr6M :: NativeExpr6 nm se ne sr nr -> m ne
 , opSerialArg6M :: SerialArg6 sm se -> m sr
 , opNativeArg6M :: NativeArg6 nm ne -> m nr
 }

data NativeManifold6 ne = NativeManifold6 Int Lang (ManifoldForm TypeM) (TypeF, ne)
  deriving(Functor, Foldable)

data SerialManifold6 se = SerialManifold6 Int Lang (ManifoldForm TypeM) se
  deriving(Functor, Foldable)

data SerialArg6 sm se = SerialArgManifold6 sm | SerialArgExpr6 se
  deriving(Functor, Foldable)

data NativeArg6 nm ne = NativeArgManifold6 nm | NativeArgExpr6 ne
  deriving(Functor, Foldable)

data SerialExpr6 sm se ne sr nr
  = AppManS6 sm [Either sr nr]
  | AppPoolS6 PoolCall [sr]
  | ReturnS6 se
  | SerialLetS6 Int se se
  | NativeLetS6 Int (TypeF, ne) se
  | LetVarS6 Int
  | BndVarS6 Int
  | SerializeS6 SerialAST ne

data NativeExpr6 nm se ne sr nr
  = AppSrcN6      TypeF Source [nr]
  | AppManN6      TypeF nm [Either sr nr]
  | ReturnN6      TypeF ne 
  | SerialLetN6   Int se (TypeF, ne)
  | NativeLetN6   Int (TypeF, ne) (TypeF, ne)
  | LetVarN6      TypeF Int
  | BndVarN6      TypeF Int
  | DeserializeN6 TypeF SerialAST se
  | AccN6         TypeF NamType FVar ne Text
  | SrcN6         TypeF Source
  -- data types
  | ListN6        FVar TypeF [ne]
  | TupleN6       FVar [(TypeF, ne)]
  | RecordN6      NamType FVar [TypeF] [(FVar, (TypeF, ne))]
  | LogN6         FVar Bool
  | RealN6        FVar Scientific
  | IntN6         FVar Integer
  | StrN6         FVar Text
  | NullN6        FVar


foldSerialManifold6M :: Monad m => FoldManifold6M m sm nm se ne sr nr -> SerialManifold -> m sm
foldSerialManifold6M fm (SerialManifold m lang form e) = do
  e' <- foldSerialExpr6M fm e
  opSerialManifold6M fm $ SerialManifold6 m lang form e'

foldNativeManifold6M :: Monad m => FoldManifold6M m sm nm se ne sr nr -> NativeManifold -> m nm
foldNativeManifold6M fm (NativeManifold m lang form (t, e)) = do
  e' <- foldNativeExpr6M fm e
  opNativeManifold6M fm $ NativeManifold6 m lang form (t, e')

foldSerialArg6M :: Monad m => FoldManifold6M m sm nm se ne sr nr -> SerialArg -> m sr
foldSerialArg6M fm (SerialArgManifold sm) = do
  sm' <- foldSerialManifold6M fm sm 
  opSerialArg6M fm $ SerialArgManifold6 sm'
foldSerialArg6M fm (SerialArgExpr se) = do
  se' <- foldSerialExpr6M fm se 
  opSerialArg6M fm $ SerialArgExpr6 se'

foldNativeArg6M :: Monad m => FoldManifold6M m sm nm se ne sr nr -> NativeArg -> m nr
foldNativeArg6M fm (NativeArgManifold nm) = do
  nm' <- foldNativeManifold6M fm nm 
  opNativeArg6M fm $ NativeArgManifold6 nm'
foldNativeArg6M fm (NativeArgExpr ne) = do
  ne' <- foldNativeExpr6M fm ne 
  opNativeArg6M fm $ NativeArgExpr6 ne'

foldSerialExpr6M :: Monad m => FoldManifold6M m sm nm se ne sr nr -> SerialExpr -> m se
foldSerialExpr6M fm (AppManS e es) = do
    e' <- foldSerialManifold6M fm e
    es' <- mapM (mapEitherM (foldSerialArg6M fm) (foldNativeArg6M fm)) es
    opSerialExpr6M fm $ AppManS6 e' es'
foldSerialExpr6M fm (AppPoolS pool es) = do
    es' <- mapM (foldSerialArg6M fm) es
    opSerialExpr6M fm $ AppPoolS6 pool es'
foldSerialExpr6M fm (ReturnS e) = do
    e' <- foldSerialExpr6M fm e
    opSerialExpr6M fm $ ReturnS6 e'
foldSerialExpr6M fm (SerialLetS i sa sb) = do
    sa' <- foldSerialExpr6M fm sa
    sb' <- foldSerialExpr6M fm sb
    opSerialExpr6M fm $ SerialLetS6 i sa' sb'
foldSerialExpr6M fm (NativeLetS i (t, na) sb) = do
    sa' <- foldNativeExpr6M fm na
    nb' <- foldSerialExpr6M fm sb
    opSerialExpr6M fm $ NativeLetS6 i (t, sa') nb'
foldSerialExpr6M fm (LetVarS i) = opSerialExpr6M fm (LetVarS6 i)
foldSerialExpr6M fm (BndVarS i) = opSerialExpr6M fm (BndVarS6 i)
foldSerialExpr6M fm (SerializeS s e) = do
    e' <- foldNativeExpr6M fm e
    opSerialExpr6M fm $ SerializeS6 s e'

foldNativeExpr6M :: Monad m => FoldManifold6M m sm nm se ne sr nr -> NativeExpr -> m ne
foldNativeExpr6M fm (AppSrcN t src nativeArgs) = do
    nativeArgs' <- mapM (foldNativeArg6M fm) nativeArgs
    opNativeExpr6M fm $ AppSrcN6 t src nativeArgs'
foldNativeExpr6M fm (AppManN t nativeManifold eargs) = do
    nativeManifold' <- foldNativeManifold6M fm nativeManifold 
    eargs' <- mapM (mapEitherM (foldSerialArg6M fm) (foldNativeArg6M fm)) eargs
    opNativeExpr6M fm $ AppManN6 t nativeManifold' eargs'
foldNativeExpr6M fm (ReturnN t ne) = do
    ne' <- foldNativeExpr6M fm ne
    opNativeExpr6M fm $ ReturnN6 t ne'
foldNativeExpr6M fm (SerialLetN i se1 (t, ne2)) = do
    se1' <- foldSerialExpr6M fm se1
    ne2' <- foldNativeExpr6M fm ne2
    opNativeExpr6M fm (SerialLetN6 i se1' (t, ne2'))
foldNativeExpr6M fm (NativeLetN i (t1, ne1) (t2, ne2)) = do
    ne1' <- foldNativeExpr6M fm ne1
    ne2' <- foldNativeExpr6M fm ne2
    opNativeExpr6M fm (NativeLetN6 i (t1, ne1') (t2, ne2'))
foldNativeExpr6M fm (LetVarN t i) = opNativeExpr6M fm (LetVarN6 t i)
foldNativeExpr6M fm (BndVarN t i) = opNativeExpr6M fm (BndVarN6 t i)
foldNativeExpr6M fm (DeserializeN t s se) = do
    se' <- foldSerialExpr6M fm se
    opNativeExpr6M fm (DeserializeN6 t s se')
foldNativeExpr6M fm (AccN t n v ne key) = do
    ne' <- foldNativeExpr6M fm ne
    opNativeExpr6M fm (AccN6 t n v ne' key)
foldNativeExpr6M fm (SrcN t src) = opNativeExpr6M fm (SrcN6 t src)
foldNativeExpr6M fm (ListN v t nes) = do
    nes' <- mapM (foldNativeExpr6M fm) nes
    opNativeExpr6M fm (ListN6 v t nes')
foldNativeExpr6M fm (TupleN t nes) = do
    nes' <- mapM (onSndM (foldNativeExpr6M fm)) nes
    opNativeExpr6M fm (TupleN6 t nes')
    where
    onSndM :: Monad m => (b -> m b') -> (a, b) -> m (a, b')
    onSndM f (a, b) = (,) a <$> f b 
foldNativeExpr6M fm (RecordN o n ps rs) = do
    rs' <- mapM (onValM (foldNativeExpr6M fm)) rs
    opNativeExpr6M fm (RecordN6 o n ps rs')
    where
    onValM :: Monad m => (c -> m c') -> (a, (b, c)) -> m (a, (b, c')) 
    onValM f (a, (b, c)) = do
        c' <- f c
        return (a, (b, c'))
foldNativeExpr6M fm (LogN t x)  = opNativeExpr6M fm (LogN6 t x)
foldNativeExpr6M fm (RealN t x) = opNativeExpr6M fm (RealN6 t x)
foldNativeExpr6M fm (IntN t x)  = opNativeExpr6M fm (IntN6 t x)
foldNativeExpr6M fm (StrN t x)  = opNativeExpr6M fm (StrN6 t x)
foldNativeExpr6M fm (NullN t)   = opNativeExpr6M fm (NullN6 t)



class HasTypeF a where
  typeFof :: a -> TypeF

instance HasTypeF (NativeManifold_ a) where
  typeFof (NativeManifold_ _ lang form (outputType, _)) = typeOfNativeManifold lang form outputType 

instance HasTypeF (SerialManifold_ a) where
  typeFof (SerialManifold_ _ lang form _) =
    let outputType = VarF (FV ML.generalSerialType (ML.serialType lang))
    in typeOfNativeManifold lang form outputType 

instance HasTypeF NativeManifold where
  typeFof (NativeManifold _ lang form (outputType, _)) = typeOfNativeManifold lang form outputType

typeOfNativeManifold :: Lang -> ManifoldForm TypeM -> TypeF -> TypeF
typeOfNativeManifold lang form outputType =
    let inputTypes = [typeOfTypeM t | Arg _ t <- manifoldArgs form]
    in case inputTypes of
        [] -> outputType
        _ -> FunF inputTypes outputType
    where
        typeOfTypeM :: TypeM -> TypeF
        typeOfTypeM Passthrough = VarF (FV ML.generalSerialType (ML.serialType lang))
        typeOfTypeM (Serial _) = VarF (FV ML.generalSerialType (ML.serialType lang))
        typeOfTypeM (Native t) = t
        typeOfTypeM (Function ts t) = FunF (map typeOfTypeM ts) (typeOfTypeM t)

instance HasTypeF NativeExpr where
  typeFof (AppSrcN      t _ _) = t
  typeFof (AppManN      t _ _) = t
  typeFof (ReturnN      t _) = t
  typeFof (SerialLetN   _ _ (t, _)) = t
  typeFof (NativeLetN   _ _ (t, _)) = t
  typeFof (LetVarN      t _) = t
  typeFof (BndVarN      t _) = t
  typeFof (DeserializeN t _ _) = t
  typeFof (AccN         t _ _ _ _) = t
  typeFof (SrcN         t _) = t
  typeFof (ListN        v p _) = AppF (VarF v) [p]
  typeFof (TupleN       v (map fst -> ps)) = AppF (VarF v) ps
  typeFof (RecordN      o n ps (map (second fst) -> rs)) = NamF o n ps rs
  typeFof (LogN         v _) = VarF v
  typeFof (RealN        v _) = VarF v
  typeFof (IntN         v _) = VarF v
  typeFof (StrN         v _) = VarF v
  typeFof (NullN        v  ) = VarF v

instance HasTypeF (NativeExpr_ a) where
  typeFof (AppSrcN_      t _ _) = t
  typeFof (AppManN_      t _ _) = t
  typeFof (ReturnN_      t _) = t
  typeFof (SerialLetN_   _ _ (t, _)) = t
  typeFof (NativeLetN_   _ _ (t, _)) = t
  typeFof (LetVarN_      t _) = t
  typeFof (BndVarN_      t _) = t
  typeFof (DeserializeN_ t _ _) = t
  typeFof (AccN_         t _ _ _ _) = t
  typeFof (SrcN_         t _) = t
  typeFof (ListN_        v p _) = AppF (VarF v) [p]
  typeFof (TupleN_       v (map fst -> ps)) = AppF (VarF v) ps
  typeFof (RecordN_      o n ps (map (second fst) -> rs)) = NamF o n ps rs
  typeFof (LogN_         v _) = VarF v
  typeFof (RealN_        v _) = VarF v
  typeFof (IntN_         v _) = VarF v
  typeFof (StrN_         v _) = VarF v
  typeFof (NullN_        v  ) = VarF v

mapEitherM :: Monad m => (a -> m a') -> (b -> m b') -> Either a b -> m (Either a' b') 
mapEitherM f _ (Left x) = Left <$> f x
mapEitherM _ f (Right x) = Right <$> f x

mapTo :: (a -> c) -> (b -> c) -> Either a b -> c
mapTo f _ (Left  x) = f x
mapTo _ f (Right x) = f x


data FoldManifoldM m a = FoldManifoldM
 { opSerialManifoldM :: SerialManifold_ a -> m a
 , opNativeManifoldM :: NativeManifold_ a -> m a
 , opSerialExprM :: SerialExpr_ a -> m a
 , opNativeExprM :: NativeExpr_ a -> m a
 , opSerialArgM :: SerialArg_ a -> m a
 , opNativeArgM :: NativeArg_ a -> m a
 }

data ManifoldMap = ManifoldMap
 { mapSerialManifold :: SerialManifold -> SerialManifold
 , mapNativeManifold :: NativeManifold -> NativeManifold
 , mapSerialExpr :: SerialExpr -> SerialExpr
 , mapNativeExpr :: NativeExpr -> NativeExpr
 , mapSerialArg :: SerialArg -> SerialArg
 , mapNativeArg :: NativeArg -> NativeArg
 }

defaultManifoldMap = ManifoldMap
 { mapSerialManifold = id
 , mapNativeManifold = id
 , mapSerialExpr = id
 , mapNativeExpr = id
 , mapSerialArg = id
 , mapNativeArg = id
 }

class MFunctor a where
    mmap :: ManifoldMap -> a -> a

instance MFunctor NativeManifold where
    mmap f (NativeManifold m l form (t, ne)) = 
        mapNativeManifold f $ NativeManifold m l form (t, mmap f ne)

instance MFunctor SerialManifold where
    mmap f (SerialManifold m l form se) = 
        mapSerialManifold f $ SerialManifold m l form (mmap f se)

instance MFunctor SerialArg where
    mmap f (SerialArgManifold sm) =
        mapSerialArg f $ SerialArgManifold (mmap f sm)
    mmap f (SerialArgExpr se) =
        mapSerialArg f $ SerialArgExpr (mmap f se)

instance MFunctor NativeArg where
    mmap f (NativeArgManifold nm) =
        mapNativeArg f $ NativeArgManifold (mmap f nm)
    mmap f (NativeArgExpr ne) =
        mapNativeArg f $ NativeArgExpr (mmap f ne)

instance MFunctor SerialExpr where
    mmap f (AppManS sm eitherArgs)
        = mapSerialExpr f
        . AppManS (mmap f sm)
        $ map (bimap (mmap f) (mmap f)) eitherArgs
    mmap f (AppPoolS p serialArgs) = mapSerialExpr f $ AppPoolS p (map (mmap f) serialArgs)
    mmap f (ReturnS se) = mapSerialExpr f $ ReturnS (mmap f se)
    mmap f (SerialLetS i se1 se2) = mapSerialExpr f $ SerialLetS i (mmap f se1) (mmap f se2)
    mmap f (NativeLetS i (t, ne1) se2) = mapSerialExpr f $ NativeLetS i (t, mmap f ne1) (mmap f se2)
    mmap f e@(LetVarS _) = mapSerialExpr f e
    mmap f e@(BndVarS _) = mapSerialExpr f e
    mmap f (SerializeS s ne) = mapSerialExpr f $ SerializeS s (mmap f ne)

-- WARNING - mapping must not change the type of any argument
instance MFunctor NativeExpr where
    mmap f (AppSrcN t src nativeArgs) = mapNativeExpr f $ AppSrcN t src (map (mmap f) nativeArgs)
    mmap f (AppManN t nm eitherArgs) = mapNativeExpr f . AppManN t (mmap f nm) $ map (bimap (mmap f) (mmap f)) eitherArgs
    mmap f (ReturnN t ne) = mapNativeExpr f $ ReturnN t (mmap f ne)
    mmap f (SerialLetN i se (t, ne)) = mapNativeExpr f $ SerialLetN i (mmap f se) (t, mmap f ne)
    mmap f (NativeLetN i (t1, ne1) (t2, ne2)) = mapNativeExpr f $ NativeLetN i (t1, mmap f ne1) (t2, mmap f ne2)
    mmap f e@(LetVarN _ _) = mapNativeExpr f e
    mmap f e@(BndVarN _ _) = mapNativeExpr f e
    mmap f (DeserializeN t s se ) = mapNativeExpr f $ DeserializeN t s (mmap f se)
    mmap f (AccN t o v ne key) = mapNativeExpr f $ AccN t o v (mmap f ne) key
    mmap f e@(SrcN _ _) = mapNativeExpr f e
    mmap f (ListN v t nes) = mapNativeExpr f $ ListN v t (map (mmap f) nes)
    mmap f (TupleN v xs) = mapNativeExpr f $ TupleN v [(t, mmap f e) | (t, e) <- xs]
    mmap f (RecordN o v ps rs) = mapNativeExpr f $ RecordN o v ps [(v', (t', mmap f e')) | (v', (t', e')) <- rs]
    mmap f e@(LogN _ _) = mapNativeExpr f e
    mmap f e@(RealN _ _) = mapNativeExpr f e
    mmap f e@(IntN _ _) = mapNativeExpr f e
    mmap f e@(StrN _ _) = mapNativeExpr f e
    mmap f e@(NullN _) = mapNativeExpr f e

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
    pretty (MonoManifold i form e) = block 4 ("m" <> pretty i <> parens (pretty form)) (pretty e)
    pretty (MonoPoolCall i _ _) =  "PoolCall" <> parens (pretty i)
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
