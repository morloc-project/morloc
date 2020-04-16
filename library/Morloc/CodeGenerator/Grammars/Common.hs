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
  , ExprM(..)
  , TypeM(..)
  , prettyExprM
  , typeOfExprM
  , invertExprM
  , pack
  , unpack
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

data TypeM
  = Null -- ^ null/void, e.g., the return type for a void function in C++
  | Passthrough -- ^ serialized data that cannot be unpacked in this language
  | Packed CType -- ^ serialized data that may be unpacked in this language
  | Unpacked CType
  | Function [TypeM] TypeM -- ^ a function of n inputs and one output (cannot be serialized)
  deriving(Show, Eq, Ord)

data ExprM
  = Manifold TypeM [Argument] Int ExprM

  -- structural elements
  | LetM EVar ExprM ExprM
  | CisAppM TypeM Source [ExprM] -- ^ a within pool function call (cis)
  | TrsAppM TypeM Int Lang [ExprM] -- ^ a foreign call (trans)
  | LamM TypeM [Maybe EVar] ExprM -- ^ Nothing Evar will be auto generated

  -- may be a function name or a passed variable
  | VarM TypeM EVar

  -- containers
  | ListM TypeM [ExprM]
  | TupleM TypeM [ExprM]
  | RecordM TypeM [(EVar, ExprM)]

  -- primitives
  | LogM TypeM Bool
  | NumM TypeM Scientific
  | StrM TypeM MT.Text
  | NullM TypeM

  -- serialization - these must remain abstract, since required arguments
  -- will vary between languages.
  | PackM ExprM
  | UnpackM ExprM

  -- return
  | ReturnM ExprM -- I need this to distinguish between the values in assigned
                  -- in let expressions and the final return value. In some
                  -- languages, this may not be necessary.
  deriving(Show, Ord, Eq)

prettyExprM :: ExprM -> MDoc
prettyExprM = undefined
-- prettyExprM (Manifold _ args i e) = undefined
-- prettyExprM (LetM v e1 e2) = pretty v <+> "=" <+> prettyExprM e1 <> line <> prettyExprM e2
-- prettyExprM (CisAppM _ f xs) = prettyExprM f <+> tupled (map prettyExprM xs)
-- prettyExprM (TrsAppM _ _ f xs) = "foreign_call" <> tupled (prettyExprM f : map prettyExprM xs)
-- prettyExprM (LamM _ mvs e) = "\\" <> hsep prettyVs <+> "->" <+> prettyExprM e
--   where
--     prettyVs = zipWith (\i v -> maybe ("_" <> viaShow i) pretty v) [0..] mvs
-- prettyExprM (VarM _ v) = pretty v
-- prettyExprM (ListM c es) = encloseSep "[" "]" "," (map prettyExprM es)
-- prettyExprM (TupleM c es) = tupled (map prettyExprM es)
-- prettyExprM (RecordM c entries) =
--   encloseSep "{" "}" ";" (map (\(k,e) -> pretty k <+> "=" <+> prettyExprM e) entries)
-- prettyExprM (LogM c x) = if x then "true" else "false"
-- prettyExprM (NumM c x) = viaShow x
-- prettyExprM (StrM c x) = dquotes (pretty x)
-- prettyExprM (NullM c) = "Null"
-- prettyExprM (PackM e) = "PACK(" <> prettyExprM e <> ")"
-- prettyExprM (UnpackM e) = "UNPACK(" <> prettyExprM e <> ")"
-- prettyExprM (ReturnM e) = "RETURN(" <> prettyExprM e <> ")"

invertExprM :: (Int -> EVar) -> ExprM -> MorlocMonad ExprM
invertExprM = undefined
-- invertExprM namer e = invert e where
--   invert (Manifold t args i e) = do
--     MM.startCounter
--     e' <- invert e
--     return $ Manifold t args i e'
--   invert e@(LetM v e1 e2) = return e
--   invert (AppM c f es) = do
--     f' <- invert f
--     es' <- mapM invert es
--     v <- MM.getCounter |>> namer
--     let e = LetM v (AppM c (terminalOf f') (map terminalOf es')) (VarM c v)
--         e' = foldl (\x y -> dependsOn x y) e (f' : es')
--     return e'
--   -- you can't pull the body of the lambda out into a let statement
--   invert f@(LamM _ _ _) = return f
--   invert (ListM c es) = do
--     es' <- mapM invert es
--     v <- MM.getCounter |>> namer
--     let e = LetM v (ListM c (map terminalOf es')) (VarM c v)
--         e' = foldl (\x y -> dependsOn x y) e es'
--     return e'
--   invert (TupleM c es) = do
--     es' <- mapM invert es
--     v <- MM.getCounter |>> namer
--     let e = LetM v (TupleM c (map terminalOf es')) (VarM c v)
--         e' = foldl (\x y -> dependsOn x y) e es'
--     return e'
--   invert (RecordM c entries) = do
--     es' <- mapM invert (map snd entries)
--     v <- MM.getCounter |>> namer
--     let entries' = zip (map fst entries) (map terminalOf es')
--         e = LetM v (RecordM c entries') (VarM c v)
--         e' = foldl (\x y -> dependsOn x y) e es'
--     return e'
--   invert (PackM e) = do
--     e' <- invert e
--     v <- MM.getCounter |>> namer
--     return $ dependsOn (LetM v (PackM (terminalOf e')) (VarM (typeOfExprM e) v)) e'
--   invert (UnpackM e) = do
--     e' <- invert e
--     v <- MM.getCounter |>> namer
--     return $ dependsOn (LetM v (UnpackM (terminalOf e')) (VarM (typeOfExprM e) v)) e'
--   invert (ReturnM e) = do
--     e' <- invert e
--     return $ dependsOn (ReturnM (terminalOf e')) e'
--   -- VarM LogM NumM StrM NullM
--   invert e = return e

-- transfer all let-dependencies from y to x
--
-- Technically, I should check for variable reuse in the let-chain and
-- resolve conflicts be substituting in fresh variable names. However, for
-- now, I will trust that my name generator created names that are unique
-- within the manifold.
dependsOn :: ExprM -> ExprM -> ExprM
dependsOn x (LetM v e y) = LetM v e (dependsOn x y)
dependsOn x _ = x

-- get the rightmost expression in a let-tree
terminalOf :: ExprM -> ExprM
terminalOf (LetM _ _ e) = terminalOf e
terminalOf e = e

-- Get the type of an expression
-- Serialization is ignored
typeOfExprM :: ExprM -> MorlocMonad TypeM
typeOfExprM (Manifold t _ _ _) = return t
typeOfExprM (LetM _ _ e) = typeOfExprM e
typeOfExprM (CisAppM t _ _) = return t
typeOfExprM (TrsAppM t _ _ _) = return t
typeOfExprM (LamM t _ _) = return t
typeOfExprM (VarM t _) = return t
typeOfExprM (ListM t _) = return t
typeOfExprM (TupleM t _) = return t
typeOfExprM (RecordM t _) = return t
typeOfExprM (LogM t _) = return t
typeOfExprM (NumM t _) = return t
typeOfExprM (StrM t _) = return t
typeOfExprM (NullM t) = return t
typeOfExprM (PackM e) = typeOfExprM e >>= packTypeM
typeOfExprM (UnpackM e) = typeOfExprM e >>= unpackTypeM
typeOfExprM (ReturnM e) = typeOfExprM e 


packTypeM :: TypeM -> MorlocMonad TypeM
packTypeM (Unpacked t) = return (Packed t)
packTypeM (Function ts t) = MM.throwError . OtherError $
  "Canno pack a function"
packTypeM t = return t


unpackTypeM :: TypeM -> MorlocMonad TypeM
unpackTypeM Passthrough = MM.throwError . OtherError $
  "Cannot unpack a passthrough type"
unpackTypeM (Packed t) = return $ Unpacked t
unpackTypeM t = return t 


unpack :: ExprM -> MorlocMonad ExprM
unpack e = do
  t <- typeOfExprM e
  case t of
    (Packed _) -> return (UnpackM e)
    (Passthrough) -> MM.throwError . OtherError $
      "Cannot unpack a passthrough type argument"
    _ -> return e

pack :: ExprM -> MorlocMonad ExprM
pack e = do
  t <- typeOfExprM e
  case t of
    (Unpacked _) -> return (PackM e)
    (Function _ _) -> MM.throwError . OtherError $ "Cannot pack a function"
    _ -> return e
