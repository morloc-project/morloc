{-|
Module      : Morloc.CodeGenerator.Namespace
Description : All code generator types and datastructures
Copyright   : (c) Zebulun Arendsee, 2020
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
  , MData(..)
  -- ** Serialization AST
  , SerialAST(..)
  , TypePacker(..)
  ) where

import Morloc.Namespace
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)

data SerialAST f
  = SerialPack (f (TypePacker, SerialAST f))
  | SerialList (SerialAST f)
  | SerialTuple [SerialAST f]
  | SerialObject TVar [(Text, SerialAST f)]
  | SerialVar Text
  -- ^ this should be a type that is recognized by the default json serializer
  -- for example, "double" or "std::string" in C++
  | SerialUnknown Text
  -- ^ depending on the language, this may or may not raise an error down the
  -- line, the parameter contains the variable name, which is useful only for
  -- source code comments.

data TypePacker = TypePacker
  { typePackerCType   :: Type
  , typePackerForward :: [Source]
  , typePackerReverse :: [Source]
  }

-- | A simplified subset of the Type record
-- functions, existential, and universal types are removed
-- language-specific info is removed
data JsonType
  = VarJ Text
  -- ^ {"int"}
  | ArrJ Text [JsonType]
  -- ^ {"list":["int"]}
  | NamJ Text [(Text, JsonType)]
  -- ^ {"Foo":{"bar":"A","baz":"B"}}
  deriving (Show, Ord, Eq)

-- | The values are left unparsed, since they will be used as text
data MData
  = Num Text
  | Str Text
  | Log Bool -- booleans are parsed, since representation depend on language
  | Lst [MData]
  | Rec [(Text, MData)]
  | Tup [MData]
  deriving (Show, Eq, Ord)

-- | An argument that is passed to a manifold
data Argument
  = SerialArgument Int CType
  -- ^ A serialized (e.g., JSON string) argument.  The parameters are 1)
  -- argument name (e.g., x), and 2) argument type (e.g., double). Some types
  -- may not be serializable. This is OK, so long as they are only used in
  -- functions of the same language.
  | NativeArgument Int CType
  -- ^ A native argument with the same parameters as above
  | PassThroughArgument Int 
  -- ^ A serialized argument that is untyped in the current language. It cannot
  -- be deserialized, but will be passed eventually to a foreign argument where it
  -- does have a concrete type.
  deriving (Show, Ord, Eq)

instance HasOneLanguage Argument where
  langOf (SerialArgument _ c) = langOf c
  langOf (NativeArgument _ c) = langOf c
  langOf (PassThroughArgument _) = Nothing

data TypeM
  = Passthrough -- ^ serialized data that cannot be deserialized in this language
  | Serial CType -- ^ serialized data that may be deserialized in this language
  | Native CType
  | Function [TypeM] TypeM -- ^ a function of n inputs and one output (cannot be serialized)
  deriving(Show, Eq, Ord)

instance HasOneLanguage TypeM where
  langOf Passthrough = Nothing
  langOf (Serial c) = langOf c
  langOf (Native c) = langOf c
  langOf (Function xs f) = listToMaybe $ catMaybes (map langOf (f:xs))


-- | A grammar that describes the implementation of the pools. Expressions in
-- this grammar will be directly translated into concrete code.
data ExprM
  = ManifoldM Int [Argument] ExprM
  -- ^ A wrapper around a single source call or (in some cases) a container.

  | ForeignInterfaceM
      TypeM -- required type in the calling language
      ExprM -- expression in the foreign language
  -- ^ A generic interface to an expression in another language. Currently it
  -- will be resolved only to the specfic pool call interface type, where
  -- system calls pass serialized information between pools in different
  -- languages. Eventually, better methods will be added for certain pairs of
  -- languages.

  | PoolCallM
      TypeM -- serialized return data
      [MDoc] -- shell command components that preceed the passed data
      [Argument] -- argument passed to the foreign function (must be serialized)
  -- ^ Make a system call to another language

  | LetM Int ExprM ExprM
  -- ^ let syntax allows fine control over order of operations in the generated
  -- code. The Int is an index for a LetVarM. It is also important in languages
  -- such as C++ where values need to be declared with explicit types and
  -- special constructors.

  | AppM
      ExprM -- ManifoldM | SrcM | LamM
      [ExprM] 

  | SrcM TypeM Source
  -- ^ a within pool function call (cis)

  | LamM [Argument] ExprM
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
  -- literal name "y" is replaced, though, with the integer 1. This required in
  -- order to avoid name conflicts in concrete languages, for example consider
  -- the following (perfectly legal) morloc function:
  --   foo for = mul for 2
  -- If the string "for" were retained as the variable name, this would fail in
  -- many language where "for" is a keyword.

  | LetVarM TypeM Int
  -- ^ An internally generated variable id used in let assignments. When
  -- translated into a language, the integer will be used to generate a unique
  -- variable name (e.g. [a0,a1,...] or [a,b,c,...]).

  -- containers
  | ListM TypeM [ExprM]
  | TupleM TypeM [ExprM]
  | RecordM TypeM [(EVar, ExprM)]

  -- primitives
  | LogM TypeM Bool
  | NumM TypeM Scientific
  | StrM TypeM Text
  | NullM TypeM

  -- serialization - these must remain abstract, since required arguments
  -- will vary between languages.
  | SerializeM ExprM
  | DeserializeM ExprM

  | ReturnM ExprM
  -- ^ The return value of a manifold. I need this to distinguish between the
  -- values assigned in let expressions and the final return value. In some
  -- languages, this may not be necessary (e.g., R).
  deriving(Show)

instance HasOneLanguage ExprM where
  -- langOf :: a -> Maybe Lang
  langOf (ManifoldM _ _ e) = langOf e
  langOf (ForeignInterfaceM t _) = langOf t
  langOf (PoolCallM t _ _) = langOf t
  langOf (LetM _ _ e2) = langOf e2
  langOf (AppM e _) = langOf e
  langOf (SrcM _ src) = Just (srcLang src) 
  langOf (LamM _ e) = langOf e
  langOf (BndVarM t _) = langOf t
  langOf (LetVarM t _) = langOf t
  langOf (ListM t _) = langOf t
  langOf (TupleM t _) = langOf t
  langOf (RecordM t _) = langOf t
  langOf (LogM t _) = langOf t
  langOf (NumM t _) = langOf t
  langOf (StrM t _) = langOf t
  langOf (NullM t) = langOf t
  langOf (SerializeM e) = langOf e
  langOf (DeserializeM e) = langOf e
  langOf (ReturnM e) = langOf e
