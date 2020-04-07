{-|
Module      : Morloc.CodeGenerator.Grammars.Common
Description : A common set of utility functions for language templates
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Grammars.Common
  ( SAnno(..)
  , SExpr(..)
  , GMeta(..)
  , Argument(..)
  , argName
  , argType
  , unpackArgument
  , prettyArgument
  , One(..)
  , Many(..)
  , ReturnValue(..)
  , Manifold(..)
  , MAnno(..)
  , MExpr(..)
  ) where

import Morloc.Data.Doc
import Morloc.Namespace
import Morloc.Pretty (prettyType)
import qualified Data.Map.Strict as Map
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS

import Data.Scientific (Scientific)
import qualified Data.Set as Set

-- g: an annotation for the group of child trees (what they have in common)
-- f: a collection - before realization this will probably be Set
--                 - after realization it will be One
-- c: an annotation for the specific child tree
data SAnno g f c = SAnno (f (SExpr g f c, c)) g

data One a = One a
data Many a = Many [a]

instance Functor One where
  fmap f (One x) = One (f x)

data SExpr g f c
  = UniS
  | VarS EVar
  | ListS [SAnno g f c]
  | TupleS [SAnno g f c]
  | LamS [EVar] (SAnno g f c)
  | AppS (SAnno g f c) [SAnno g f c]
  | NumS Scientific
  | LogS Bool
  | StrS MT.Text
  | RecS [(EVar, SAnno g f c)]
  | CallS Source
  | ForeignS Int Lang [EVar]

-- | Description of the general manifold
data GMeta = GMeta {
    metaId :: Int
  , metaGType :: Maybe GType
  , metaName :: Maybe EVar -- the name, if relevant
  , metaProperties :: Set.Set Property
  , metaConstraints :: Set.Set Constraint
} deriving (Show, Ord, Eq)

-- | An argument that is passed to a manifold
data Argument
  = PackedArgument EVar CType
  -- ^ A serialized (e.g., JSON string) argument.  The parameters are 1)
  -- argument name (e.g., x), and 2) argument type (e.g., double). Some types
  -- may not be serializable. This is OK, so long as they are only used in
  -- functions of the same language.
  | UnpackedArgument EVar CType
  -- ^ A native argument with the same parameters as above
  | PassThroughArgument EVar
  -- ^ A serialized argument that is untyped in the current language. It cannot
  -- be unpacked, but will be passed eventually to a foreign argument where it
  -- does have a concrete type.
  deriving (Show, Ord, Eq)

-- | Similar to Argument, but for values returned from a manifold. The EVar
-- parameter stores the names of the manifold. The CType stores the return type
-- of the manifold, which may serialized.
data ReturnValue
  = PackedReturn EVar CType
  | UnpackedReturn EVar CType
  | PassThroughReturn EVar
  deriving (Show, Ord, Eq)

prettyArgument :: Argument -> MDoc
prettyArgument (PackedArgument v c) =
  "Packed" <+> pretty v <+> parens (prettyType c)
prettyArgument (UnpackedArgument v c) =
  "Unpacked" <+> pretty v <+> parens (prettyType c)
prettyArgument (PassThroughArgument v) =
  "PassThrough" <+> pretty v

argName :: Argument -> EVar
argName (PackedArgument v _) = v
argName (UnpackedArgument v _) = v
argName (PassThroughArgument v) = v

argType :: Argument -> Maybe CType
argType (PackedArgument _ t) = Just t
argType (UnpackedArgument _ t) = Just t
argType (PassThroughArgument _) = Nothing

unpackArgument :: Argument -> Argument
unpackArgument (PackedArgument v t) = UnpackedArgument v t
unpackArgument x = x

data Manifold = Manifold ReturnValue [Argument] [MAnno]

data MAnno = MAnno MExpr CType

  -- | ForeignS Int Lang [EVar]
data MExpr
  = MTAssign EVar MAnno
  | MTCall EVar [MAnno]
  | MTForeignCall Int Lang [EVar] -- | cmd [parameters]
  | MTReturn MAnno
  | MTVar EVar
  -- containers
  | MTList [MAnno]
  | MTTuple [MAnno]
  | MTRecord [(EVar, MAnno)]
  -- primitives
  | MTLog Bool
  | MTNum Scientific 
  | MTStr MT.Text
  | MTNull
