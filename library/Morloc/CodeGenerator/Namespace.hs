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
  , TypeM(..)
  , ExprM(..)
  , Argument(..)
  , JsonType(..)
  , PVar(..)
  , TypeP(..)
  , arrP
  , recP
  , JsonPath
  , JsonAccessor(..)
  , NexusCommand(..)
  -- ** Serialization AST
  , SerialAST(..)
  , TypePacker(..)
  ) where

import Morloc.Namespace
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Set as Set

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
  | AppP TypeP [TypeP]
  | RecP NamType [(PVar, TypeP)]
  deriving (Show, Ord, Eq)

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
  typeOf NulP = NulT
  typeOf (UnkP (PV lang _ t)) = UnkT (TV (Just lang) t)
  typeOf (VarP (PV lang _ t)) = VarT (TV (Just lang) t)
  typeOf (FunP t1 t2) = FunT (typeOf t1) (typeOf t2)
  typeOf (AppP t1 t2) = AppT (typeOf t1) (typeOf t2)
  typeOf (RecP r t1 (PV l _ v) t2) = RecP r (typeOf t1) (TV (Just l) v) (typeOf t2)


  -- | substitute all appearances of a given variable with a given new type
  substituteTVar v0 r0 t0 = sub t0
    where
      sub :: TypeP -> TypeP
      sub NulP = NulP
      sub t'@(UnkP _) = t'
      sub t'@(VarP (PV lang _ v'))
        | v0 == (TV (Just lang) v') = r0
        | otherwise = t'
      sub (FunP t1 t2) = FunT (sub t1) (sub t2)
      sub (AppP t1 t2) = AppT (sub t1) (sub t2)
      sub (RecP r t1 k t2) = RecP r (sub t1) (TV k (sub t2)

  free v@(VarP _) = Set.singleton v
  free v@(UnkP _) = Set.singleton v
  free (FunP t1 t2) = Set.union (free t1) (free t2)
  free (AppP t1 t2) = Set.union (free t1) (free t2)
  free (RecP _ t1 _ t2) = Set.union (free t1) (free t2)
  free NulP = Set.empty

-- -- Decomposition is betrayal of structure, the elegant solution should not need it
-- instance Decomposable TypeP where
--   -- split a composite type into its inputs and outputs
--   -- * functions are separated into intpus and output
--   --   a -> b -> c  ==>  ([a, b], c)
--   -- * parameterized types are type functions that construct the base type
--   --   (a, (b, c))  ==> ([a, (b, c)], Tuple2)
--   --   Map a (a -> b)  ==>  ([a, a -> b], Map)
--   -- * records/objects/tables are broken into key/value pairs
--   --   Person {name :: Str, age :: Int}  ==> ([("name", Str), ("age", Int)], Person)
--   -- * type variables have no input:
--   --   Bool  ==>  ([], Bool)
--   decompose x0@(CatP k _ _) = f x0 where
--     f t@(CatP k' t1 t2)
--       | k == k' = case f t2 of
--         (ts, finalType) -> (t1:ts, finalType)
--       | otherwise = ([], t)
--     f t = ([], t)

-- | A tree describing how to (de)serialize an object
data SerialAST f
  = SerialPack PVar (f (TypePacker, SerialAST f)) -- ^ use an (un)pack function to simplify an object
  | SerialList (SerialAST f)
  | SerialTuple [SerialAST f]
  | SerialObject NamType PVar [PVar] [(PVar, SerialAST f)]
  -- ^ Make a record, table, or object. The parameters indicate
  --   1) NamType - record/table/object
  --   2) PVar - telling the name of the object (e.g., "Person")
  --   3) [PVar] - listing the generic metavariable names
  --   4) [(PVar, SerialAST f)] - entries with keys for concrete and general cases
  | SerialNum PVar
  | SerialBool PVar
  | SerialString PVar
  | SerialNull PVar
  | SerialUnknown PVar
  -- ^ depending on the language, this may or may not raise an error down the
  -- line, the parameter contains the variable name, which is useful only for
  -- source code comments.

data TypePacker = TypePacker
  { typePackerType    :: TypeP
  , typePackerFrom    :: TypeP
  , typePackerForward :: [Source]
  , typePackerReverse :: [Source]
  } deriving (Show, Ord, Eq)

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

-- | An argument that is passed to a manifold
data Argument
  = SerialArgument Int TypeP
  -- ^ A serialized (e.g., JSON string) argument.  The parameters are 1)
  -- argument name (e.g., x), and 2) argument type (e.g., double). Some types
  -- may not be serializable. This is OK, so long as they are only used in
  -- functions of the same language.
  | NativeArgument Int TypeP
  -- ^ A native argument with the same parameters as above
  | PassThroughArgument Int
  -- ^ A serialized argument that is untyped in the current language. It cannot
  -- be deserialized, but will be passed eventually to a foreign argument where it
  -- does have a concrete type.
  deriving (Show, Ord, Eq)

data TypeM
  = Passthrough -- ^ serialized data that cannot be deserialized in this language
  | Serial TypeP -- ^ serialized data that may be deserialized in this language
  | Native TypeP -- ^ an unserialized native data type
  | Function [TypeM] TypeM -- ^ a function of n inputs and one output (cannot be serialized)
  deriving(Show, Eq, Ord)


-- | A grammar that describes the implementation of the pools. Expressions in
-- this grammar will be directly translated into concrete code.
data ExprM f
  = ManifoldM Int [Argument] (ExprM f)
  -- ^ A wrapper around a single source call or (in some cases) a container.

  | ForeignInterfaceM
      TypeM -- required type in the calling language
      (ExprM f) -- expression in the foreign language
  -- ^ A generic interface to an expression in another language. Currently it
  -- will be resolved only to the specfic pool call interface type, where
  -- system calls pass serialized information between pools in different
  -- languages. Eventually, better methods will be added for certain pairs of
  -- languages.

  | PoolCallM
      TypeM -- serialized return data
      Int -- foreign manifold id
      [MDoc] -- shell command components that preceed the passed data
      [Argument] -- argument passed to the foreign function (must be serialized)
  -- ^ Make a system call to another language

  | LetM Int (ExprM f) (ExprM f)
  -- ^ let syntax allows fine control over order of operations in the generated
  -- code. The Int is an index for a LetVarM. It is also important in languages
  -- such as C++ where values need to be declared with explicit types and
  -- special constructors.

  | AppM
      (ExprM f) -- ManifoldM | SrcM | LamM
      [(ExprM f)]

  | SrcM TypeM Source
  -- ^ a within pool function call (cis)

  | LamM [Argument] (ExprM f)
  -- ^ Nothing Evar will be auto generated

  | BndVarM TypeM Int
  -- ^ A lambda-bound variable. BndVarM only describes variables bound as positional
  -- arguments in a manifold. The are represented as integers since the name
  -- will be language-specific.
  --
  -- In the rewrite step, morloc declarations are removed. So the expression:
  --   x = 5
  --   foo y = mul x y
  -- Is rewritten as:
  --   \y -> mul 5 y
  -- So BndVarM does NOT include variables defined in the morloc script. It only
  -- includes lambda-bound variables. The only BndVarM is `y` (`mul` is SrcM). The
  -- literal name "y" is replaced, though, with the integer 1. This is required in
  -- order to avoid name conflicts in concrete languages, for example consider
  -- the following (perfectly legal) morloc function:
  --   foo for = mul for 2
  -- If the string "for" were retained as the variable name, this would fail in
  -- many language where "for" is a keyword.

  | AccM (ExprM f) Text
  -- ^ Access a field in record ExprM

  | LetVarM TypeM Int
  -- ^ An internally generated variable id used in let assignments. When
  -- translated into a language, the integer will be used to generate a unique
  -- variable name (e.g. [a0,a1,...] or [a,b,c,...]).

  -- containers
  | ListM TypeM [(ExprM f)]
  | TupleM TypeM [(ExprM f)]
  | RecordM TypeM [(Text, (ExprM f))]

  -- primitives
  | LogM TypeM Bool
  | NumM TypeM Scientific
  | StrM TypeM Text
  | NullM TypeM

  -- serialization
  | SerializeM (SerialAST f) (ExprM f)
  | DeserializeM (SerialAST f) (ExprM f)

  | ReturnM (ExprM f)
  -- ^ The return value of a manifold. I need this to distinguish between the
  -- values assigned in let expressions and the final return value. In some
  -- languages, this may not be necessary (e.g., R).


instance HasOneLanguage (TypeP) where
  langOf' NulP = error "The empty type has no concrete language - though it probably should"
  langOf' (UnkP (PV lang _ _)) = lang
  langOf' (VarP (PV lang _ _)) = lang
  langOf' (FunP t _) = langOf' t 
  langOf' (AppP t _) = langOf' t
  langOf' (RecP _ t _) = langOf' t


instance HasOneLanguage (TypeM) where
  langOf Passthrough = Nothing 
  langOf (Serial t) = langOf t
  langOf (Native t) = langOf t
  langOf (Function _ t) = langOf t


instance HasOneLanguage (ExprM f) where
  -- langOf :: a -> Maybe Lang
  langOf' (ManifoldM _ _ e) = langOf' e
  langOf' (ForeignInterfaceM t _) = langOf' t
  langOf' (PoolCallM t _ _ _) = langOf' t
  langOf' (LetM _ _ e2) = langOf' e2
  langOf' (AppM e _) = langOf' e
  langOf' (SrcM _ src) = srcLang src
  langOf' (LamM _ e) = langOf' e
  langOf' (BndVarM t _) = langOf' t
  langOf' (LetVarM t _) = langOf' t
  langOf' (AccM e _) = langOf' e
  langOf' (ListM t _) = langOf' t
  langOf' (TupleM t _) = langOf' t
  langOf' (RecordM t _) = langOf' t
  langOf' (LogM t _) = langOf' t
  langOf' (NumM t _) = langOf' t
  langOf' (StrM t _) = langOf' t
  langOf' (NullM t) = langOf' t
  langOf' (SerializeM _ e) = langOf' e
  langOf' (DeserializeM _ e) = langOf' e
  langOf' (ReturnM e) = langOf' e
