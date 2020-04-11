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
  , CallTree(..)
  , prettyCallTree
  , Manifold(..)
  , ExprM(..)
  , serializeCallTree
  , typeOfExprM
  , returnId
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
  = PackedReturn Int CType
  | UnpackedReturn Int CType
  | PassThroughReturn Int 
  deriving (Show, Ord, Eq)

returnId :: ReturnValue -> Int
returnId (PackedReturn i _) = i
returnId (UnpackedReturn i _) = i
returnId (PassThroughReturn i) = i

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

data CallTree = CallTree Manifold [Manifold]
  deriving(Show, Ord, Eq)

data Manifold = Manifold ReturnValue [Argument] [ExprM]
  deriving(Show, Ord, Eq)

data ExprM
  = AssignM EVar ExprM
  | SrcCallM CType ExprM [ExprM] -- always return unpacked object
  | ManCallM CType Int [ExprM] -- always return unpacked object
  | ForeignCallM CType Int Lang [EVar] -- always returns packed object
  | PartialM CType Int ExprM
  | ReturnM ExprM
  | VarM CType EVar
  -- containers
  | ListM CType [ExprM]
  | TupleM CType [ExprM]
  | RecordM CType [(EVar, ExprM)]
  -- primitives
  | LogM CType Bool
  | NumM CType Scientific 
  | StrM CType MT.Text
  | NullM CType
  -- serialization - these must remain abstract, since required arguments
  -- will vary between languages.
  | PackM ExprM
  | UnpackM ExprM
  deriving(Show, Ord, Eq)

prettyCallTree :: CallTree -> MDoc
prettyCallTree (CallTree m ms) = vsep (map prettyManifold (m:ms))

prettyManifold :: Manifold -> MDoc
prettyManifold (Manifold v args es) =
  block 4 (rval v) (vsep $ map prettyExprM es)
  where
    rval :: ReturnValue -> MDoc
    rval (PackedReturn i t)
      = "packed(" <> prettyType t <> ")"
      <+> "m" <> pretty i
      <> tupled (map prettyArgument args)
    rval (UnpackedReturn i t)
      = "unpacked(" <> prettyType t <> ")"
      <+> "m" <> pretty i
      <> tupled (map prettyArgument args)
    rval (PassThroughReturn i)
      = "passthrough" 
      <+> "m" <> pretty i
      <> tupled (map prettyArgument args)

prettyExprM :: ExprM -> MDoc
prettyExprM (AssignM v e) = pretty v <+> "=" <+> prettyExprM e
prettyExprM (SrcCallM c v es) = prettyExprM v <> tupled (map prettyExprM es)
prettyExprM (ManCallM c i es) = "m" <> pretty i <> tupled (map prettyExprM es)
prettyExprM (ForeignCallM c i lang vs) =
  "foreign_call" <> tupled ([pretty i, viaShow lang] ++ map pretty vs)
prettyExprM (ReturnM e) = "return(" <> prettyExprM e <> ")"
prettyExprM (VarM c v) = pretty v
prettyExprM (ListM c es) = encloseSep "[" "]" "," (map prettyExprM es)
prettyExprM (TupleM c es) = tupled (map prettyExprM es)
prettyExprM (RecordM c entries) =
  encloseSep "{" "}" ";" (map (\(k,e) -> pretty k <+> "=" <+> prettyExprM e) entries) 
prettyExprM (LogM c x) = if x then "true" else "false"
prettyExprM (NumM c x) = viaShow x
prettyExprM (StrM c x) = dquotes (pretty x)
prettyExprM (NullM c) = "Null"
prettyExprM (PackM e) = "PACK(" <> prettyExprM e <> ")"
prettyExprM (UnpackM e) = "UNPACK(" <> prettyExprM e <> ")"
prettyExprM (PartialM _ i e) = "Partial(" <> viaShow i <> ", " <> prettyExprM e <> ")"

serializeCallTree :: CallTree -> MorlocMonad CallTree
serializeCallTree x@(CallTree m ms) = do
  let m' = packHead m
  (m'':ms') <- mapM serialize (m':ms)
  return (CallTree m'' ms')
  where
    -- rewrite the head manifold to unpack its return value
    packHead :: Manifold -> Manifold
    packHead m@(Manifold (PackedReturn _ _) _ _) = m
    packHead (Manifold (UnpackedReturn i c) args ms) =
      Manifold (PackedReturn i c) args (map packOutput ms)
    packHead m@(Manifold (PassThroughReturn _) _ _) = m

    packOutput :: ExprM -> ExprM
    packOutput (ReturnM x) = ReturnM (PackM x)
    packOutput m = m

    -- add serialization handling downstream as needed
    serialize :: Manifold -> MorlocMonad Manifold
    serialize (Manifold v args es) =
      return $ Manifold v args (map (serializeExpr args) es) 

    serializeExpr :: [Argument] -> ExprM -> ExprM
    serializeExpr args (ReturnM e) = ReturnM (serializeExpr args e)
    serializeExpr args (PackM e) = PackM (serializeExpr args e)
    serializeExpr args (PartialM i c e) = PartialM i c (serializeExpr args e)
    serializeExpr args (SrcCallM c f es) =
      SrcCallM c (serializeExpr args f) (map (unpackMay args) es) 
    serializeExpr args (TupleM c es) = TupleM c (map (unpackMay args) es)
    serializeExpr args (ListM c es) = ListM c (map (unpackMay args) es)
    serializeExpr args (RecordM c xs) =
      RecordM c (map (\(k,v)->(k, unpackMay args v)) xs)
    serializeExpr _ e = e 

    unpackMay :: [Argument] -> ExprM -> ExprM
    unpackMay args e@(VarM c v) = case unCType c of
      (FunT _ _) -> e -- do not unpack functions
      _ -> case lookupArg v args of
        -- unpack only if the variable is a packed manifold argument
        (Just (PackedArgument _ _)) -> UnpackM e
        -- otherwise leave it alone
        _ -> e
    unpackMay _ e = e

    lookupArg :: EVar -> [Argument] -> Maybe Argument
    lookupArg v args = listToMaybe [r | r <- args, argName r == v]

-- Get the type of an expression
-- Serialization is ignored
typeOfExprM :: ExprM -> CType
typeOfExprM (AssignM _ e) = typeOfExprM e
typeOfExprM (ManCallM c _ _) = c
typeOfExprM (SrcCallM c _ _) = c
typeOfExprM (ForeignCallM c _ _ _) = c
typeOfExprM (ReturnM e) = typeOfExprM e
typeOfExprM (VarM c _) = c
typeOfExprM (ListM c _) = c
typeOfExprM (TupleM c _) = c
typeOfExprM (RecordM c _) = c
typeOfExprM (LogM c _ ) = c
typeOfExprM (NumM c _) = c
typeOfExprM (StrM c _) = c
typeOfExprM (NullM c) = c
typeOfExprM (PackM e) = typeOfExprM e
typeOfExprM (UnpackM e) = typeOfExprM e
typeOfExprM (PartialM c _ _) = c
