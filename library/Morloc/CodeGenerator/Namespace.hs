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
  , typeP2typeM
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
  , foldlSE
  , foldlNE
  , foldlSA
  , foldlNA
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

typeP2typeM :: TypeP -> TypeM
typeP2typeM (UnkP _) = Passthrough
typeP2typeM (FunP ts t) = Function (map typeP2typeM ts) (typeP2typeM t)
typeP2typeM t = Native (typeP2typeF t)

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

foldlSA :: (b -> a -> b) -> b -> SerialArg_ a a -> b
foldlSA f b (SerialArgManifold_ sm) = f b sm
foldlSA f b (SerialArgExpr_ se) = f b se

foldlNA :: (b -> a -> b) -> b -> NativeArg_ a a -> b
foldlNA f b (NativeArgManifold_ nm) = f b nm
foldlNA f b (NativeArgExpr_ ne) = f b ne

foldlSE :: (b -> a -> b) -> b -> SerialExpr_ a a a a a -> b
foldlSE f b (AppManS_ x eitherXs) = foldl f b (x: map catEither eitherXs)
foldlSE f b (AppPoolS_ _ xs) = foldl f b xs
foldlSE f b (ReturnS_ x) = f b x
foldlSE f b (SerialLetS_ _ x1 x2) = foldl f b [x1, x2]
foldlSE f b (NativeLetS_ _ (_, x1) x2) = foldl f b [x1, x2]
foldlSE _ b (LetVarS_ _) = b
foldlSE _ b (BndVarS_ _) = b
foldlSE f b (SerializeS_ _ x) = f b x

foldlNE :: (b -> a -> b) -> b -> NativeExpr_ a a a a a -> b
foldlNE f b (AppSrcN_      _ _ xs) = foldl f b xs
foldlNE f b (AppManN_      _ x eitherXs) = foldl f b (x : map catEither eitherXs)
foldlNE f b (ReturnN_      _ x ) = f b x
foldlNE f b (SerialLetN_   _ x1 (_, x2)) = foldl f b [x1, x2]
foldlNE f b (NativeLetN_   _ (_, x1) (_, x2)) = foldl f b [x1, x2]
foldlNE _ b (LetVarN_      _ _) = b
foldlNE _ b (BndVarN_      _ _) = b
foldlNE f b (DeserializeN_ _ _ x) = f b x 
foldlNE f b (AccN_         _ _ _ x _) =  f b x
foldlNE _ b (SrcN_         _ _) = b
foldlNE f b (ListN_        _ _ xs) = foldl f b xs
foldlNE f b (TupleN_       _ xs) = foldl (\b' (_, a') -> f b' a') b xs
foldlNE f b (RecordN_      _ _ _ rs) = foldl (\b' (_, (_, a')) -> f b' a') b rs
foldlNE _ b (LogN_         _ _) = b
foldlNE _ b (RealN_        _ _) = b
foldlNE _ b (IntN_         _ _) = b
foldlNE _ b (StrN_         _ _) = b
foldlNE _ b (NullN_        _) = b

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

data NativeManifold_ ne = NativeManifold_ Int Lang (ManifoldForm TypeM) (TypeF, ne)
  deriving(Functor, Foldable)

data SerialManifold_ se = SerialManifold_ Int Lang (ManifoldForm TypeM) se
  deriving(Functor, Foldable)

data SerialArg_ sm se = SerialArgManifold_ sm | SerialArgExpr_ se

data NativeArg_ nm ne = NativeArgManifold_ nm | NativeArgExpr_ ne

data SerialExpr_ sm se ne sr nr
  = AppManS_ sm [Either sr nr]
  | AppPoolS_ PoolCall [sr]
  | ReturnS_ se
  | SerialLetS_ Int se se
  | NativeLetS_ Int (TypeF, ne) se
  | LetVarS_ Int
  | BndVarS_ Int
  | SerializeS_ SerialAST ne

data NativeExpr_ nm se ne sr nr
  = AppSrcN_      TypeF Source [nr]
  | AppManN_      TypeF nm [Either sr nr]
  | ReturnN_      TypeF ne 
  | SerialLetN_   Int se (TypeF, ne)
  | NativeLetN_   Int (TypeF, ne) (TypeF, ne)
  | LetVarN_      TypeF Int
  | BndVarN_      TypeF Int
  | DeserializeN_ TypeF SerialAST se
  | AccN_         TypeF NamType FVar ne Text
  | SrcN_         TypeF Source
  -- data types
  | ListN_        FVar TypeF [ne]
  | TupleN_       FVar [(TypeF, ne)]
  | RecordN_      NamType FVar [TypeF] [(FVar, (TypeF, ne))]
  | LogN_         FVar Bool
  | RealN_        FVar Scientific
  | IntN_         FVar Integer
  | StrN_         FVar Text
  | NullN_        FVar

foldSerialManifoldM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> SerialManifold -> m sm
foldSerialManifoldM = surroundFoldSerialManifoldM defaultValue

foldNativeManifoldM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> NativeManifold -> m nm
foldNativeManifoldM = surroundFoldNativeManifoldM defaultValue

foldSerialArgM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> SerialArg -> m sr
foldSerialArgM = surroundFoldSerialArgM defaultValue

foldNativeArgM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> NativeArg -> m nr
foldNativeArgM = surroundFoldNativeArgM defaultValue

foldSerialExprM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> SerialExpr -> m se
foldSerialExprM = surroundFoldSerialExprM defaultValue

foldNativeExprM :: Monad m => FoldManifoldM m sm nm se ne sr nr -> NativeExpr -> m ne
foldNativeExprM = surroundFoldNativeExprM defaultValue

surroundFoldSerialManifoldM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldManifoldM m sm nm se ne sr nr -> SerialManifold -> m sm
surroundFoldSerialManifoldM sfm fm = surroundSerialManifoldM sfm f
  where
  f (SerialManifold m lang form e) = do
    e' <- surroundFoldSerialExprM sfm fm e
    opSerialManifoldM fm $ SerialManifold_ m lang form e'

surroundFoldNativeManifoldM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldManifoldM m sm nm se ne sr nr -> NativeManifold -> m nm
surroundFoldNativeManifoldM sfm fm = surroundNativeManifoldM sfm f
  where
  f (NativeManifold m lang form (t, e)) = do
    e' <- surroundFoldNativeExprM sfm fm e
    opNativeManifoldM fm $ NativeManifold_ m lang form (t, e')

surroundFoldSerialArgM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldManifoldM m sm nm se ne sr nr -> SerialArg -> m sr
surroundFoldSerialArgM sfm fm = surroundSerialArgM sfm f
  where
  f (SerialArgManifold sm) = do
    sm' <- surroundFoldSerialManifoldM sfm fm sm 
    opSerialArgM fm $ SerialArgManifold_ sm'
  f (SerialArgExpr se) = do
    se' <- surroundFoldSerialExprM sfm fm se 
    opSerialArgM fm $ SerialArgExpr_ se'

surroundFoldNativeArgM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldManifoldM m sm nm se ne sr nr -> NativeArg -> m nr
surroundFoldNativeArgM sfm fm = surroundNativeArgM sfm f
  where
  f (NativeArgManifold nm) = do
      nm' <- surroundFoldNativeManifoldM sfm fm nm 
      opNativeArgM fm $ NativeArgManifold_ nm'
  f (NativeArgExpr ne) = do
      ne' <- surroundFoldNativeExprM sfm fm ne 
      opNativeArgM fm $ NativeArgExpr_ ne'

surroundFoldSerialExprM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldManifoldM m sm nm se ne sr nr -> SerialExpr -> m se
surroundFoldSerialExprM sfm fm = surroundSerialExprM sfm f
  where
  f (AppManS e es) = do
    e' <- surroundFoldSerialManifoldM sfm fm e
    es' <- mapM (mapEitherM (surroundFoldSerialArgM sfm fm) (surroundFoldNativeArgM sfm fm)) es
    opSerialExprM fm $ AppManS_ e' es'
  f (AppPoolS pool es) = do
    es' <- mapM (surroundFoldSerialArgM sfm fm) es
    opSerialExprM fm $ AppPoolS_ pool es'
  f (ReturnS e) = do
    e' <- surroundFoldSerialExprM sfm fm e
    opSerialExprM fm $ ReturnS_ e'
  f (SerialLetS i sa sb) = do
    sa' <- surroundFoldSerialExprM sfm fm sa
    sb' <- surroundFoldSerialExprM sfm fm sb
    opSerialExprM fm $ SerialLetS_ i sa' sb'
  f (NativeLetS i (t, na) sb) = do
    sa' <- surroundFoldNativeExprM sfm fm na
    nb' <- surroundFoldSerialExprM sfm fm sb
    opSerialExprM fm $ NativeLetS_ i (t, sa') nb'
  f (LetVarS i) = opSerialExprM fm (LetVarS_ i)
  f (BndVarS i) = opSerialExprM fm (BndVarS_ i)
  f (SerializeS s e) = do
    e' <- surroundFoldNativeExprM sfm fm e
    opSerialExprM fm $ SerializeS_ s e'

surroundFoldNativeExprM :: Monad m => SurroundManifoldM m sm nm se ne sr nr -> FoldManifoldM m sm nm se ne sr nr -> NativeExpr -> m ne
surroundFoldNativeExprM sfm fm = surroundNativeExprM sfm f
  where
  f (AppSrcN t src nativeArgs) = do
      nativeArgs' <- mapM (surroundFoldNativeArgM sfm fm) nativeArgs
      opNativeExprM fm $ AppSrcN_ t src nativeArgs'
  f (AppManN t nativeManifold eargs) = do
      nativeManifold' <- surroundFoldNativeManifoldM sfm fm nativeManifold 
      eargs' <- mapM (mapEitherM (surroundFoldSerialArgM sfm fm) (surroundFoldNativeArgM sfm fm)) eargs
      opNativeExprM fm $ AppManN_ t nativeManifold' eargs'
  f (ReturnN t ne) = do
      ne' <- surroundFoldNativeExprM sfm fm ne
      opNativeExprM fm $ ReturnN_ t ne'
  f (SerialLetN i se1 (t, ne2)) = do
      se1' <- surroundFoldSerialExprM sfm fm se1
      ne2' <- surroundFoldNativeExprM sfm fm ne2
      opNativeExprM fm (SerialLetN_ i se1' (t, ne2'))
  f (NativeLetN i (t1, ne1) (t2, ne2)) = do
      ne1' <- surroundFoldNativeExprM sfm fm ne1
      ne2' <- surroundFoldNativeExprM sfm fm ne2
      opNativeExprM fm (NativeLetN_ i (t1, ne1') (t2, ne2'))
  f (LetVarN t i) = opNativeExprM fm (LetVarN_ t i)
  f (BndVarN t i) = opNativeExprM fm (BndVarN_ t i)
  f (DeserializeN t s se) = do
      se' <- surroundFoldSerialExprM sfm fm se
      opNativeExprM fm (DeserializeN_ t s se')
  f (AccN t n v ne key) = do
      ne' <- surroundFoldNativeExprM sfm fm ne
      opNativeExprM fm (AccN_ t n v ne' key)
  f (SrcN t src) = opNativeExprM fm (SrcN_ t src)
  f (ListN v t nes) = do
      nes' <- mapM (surroundFoldNativeExprM sfm fm) nes
      opNativeExprM fm (ListN_ v t nes')
  f (TupleN t nes) = do
      nes' <- mapM (onSndM (surroundFoldNativeExprM sfm fm)) nes
      opNativeExprM fm (TupleN_ t nes')
      where
        onSndM :: Monad m => (b -> m b') -> (a, b) -> m (a, b')
        onSndM g (a, b) = (,) a <$> g b 
  f (RecordN o n ps rs) = do
      rs' <- mapM (onValM (surroundFoldNativeExprM sfm fm)) rs
      opNativeExprM fm (RecordN_ o n ps rs')
      where
        onValM :: Monad m => (c -> m c') -> (a, (b, c)) -> m (a, (b, c')) 
        onValM g (a, (b, c)) = do
          c' <- g c
          return (a, (b, c'))
  f (LogN t x)  = opNativeExprM fm (LogN_ t x)
  f (RealN t x) = opNativeExprM fm (RealN_ t x)
  f (IntN t x)  = opNativeExprM fm (IntN_ t x)
  f (StrN t x)  = opNativeExprM fm (StrN_ t x)
  f (NullN t)   = opNativeExprM fm (NullN_ t)



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

instance HasTypeF (NativeExpr_ a b c d e) where
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
    mgatedMap g f nm@(NativeManifold m l form (t, ne))
      | gateNativeManifold g nm = mapNativeManifold f $ NativeManifold m l form (t, mgatedMap g f ne)
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
        (AppManS sm eitherArgs)
            -> mapSerialExpr f
            . AppManS (mgatedMap g f sm)
            $ map (bimap (mgatedMap g f) (mgatedMap g f)) eitherArgs
        (AppPoolS p serialArgs) -> mapSerialExpr f $ AppPoolS p (map (mgatedMap g f) serialArgs)
        (ReturnS se) -> mapSerialExpr f $ ReturnS (mgatedMap g f se)
        (SerialLetS i se1 se2) -> mapSerialExpr f $ SerialLetS i (mgatedMap g f se1) (mgatedMap g f se2)
        (NativeLetS i (t, ne1) se2) -> mapSerialExpr f $ NativeLetS i (t, mgatedMap g f ne1) (mgatedMap g f se2)
        e@(LetVarS _) -> mapSerialExpr f e
        e@(BndVarS _) -> mapSerialExpr f e
        (SerializeS s ne) -> mapSerialExpr f $ SerializeS s (mgatedMap g f ne)
    | otherwise = mapSerialExpr f se0

-- WARNING - mapping must not change the type of any argument
instance MFunctor NativeExpr where
  mgatedMap g f ne0
    | gateNativeExpr g ne0 = case ne0 of
        (AppSrcN t src nativeArgs) -> mapNativeExpr f $ AppSrcN t src (map (mgatedMap g f) nativeArgs)
        (AppManN t nm eitherArgs) -> mapNativeExpr f . AppManN t (mgatedMap g f nm) $ map (bimap (mgatedMap g f) (mgatedMap g f)) eitherArgs
        (ReturnN t ne) -> mapNativeExpr f $ ReturnN t (mgatedMap g f ne)
        (SerialLetN i se (t, ne)) -> mapNativeExpr f $ SerialLetN i (mgatedMap g f se) (t, mgatedMap g f ne)
        (NativeLetN i (t1, ne1) (t2, ne2)) -> mapNativeExpr f $ NativeLetN i (t1, mgatedMap g f ne1) (t2, mgatedMap g f ne2)
        e@(LetVarN _ _) -> mapNativeExpr f e
        e@(BndVarN _ _) -> mapNativeExpr f e
        (DeserializeN t s se ) -> mapNativeExpr f $ DeserializeN t s (mgatedMap g f se)
        (AccN t o v ne key) -> mapNativeExpr f $ AccN t o v (mgatedMap g f ne) key
        e@(SrcN _ _) -> mapNativeExpr f e
        (ListN v t nes) -> mapNativeExpr f $ ListN v t (map (mgatedMap g f) nes)
        (TupleN v xs) -> mapNativeExpr f $ TupleN v [(t, mgatedMap g f e) | (t, e) <- xs]
        (RecordN o v ps rs) -> mapNativeExpr f $ RecordN o v ps [(v', (t', mgatedMap g f e')) | (v', (t', e')) <- rs]
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
