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
  , argType
  , unpackArgument
  , prettyArgument
  , argId
  , One(..)
  , Many(..)
  , prettyExprM
  , typeOfExprM
  , invertExprM
  , typeParts
  , ctype2typeM
  , packTypeM
  , packExprM
  , unpackExprM
  , unpackTypeM
  , nargsTypeM
  , arg2typeM
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

-- | Description of the general manifold
data GMeta = GMeta {
    metaId :: Int
  , metaGType :: Maybe GType
  , metaName :: Maybe EVar -- the name, if relevant
  , metaProperties :: Set.Set Property
  , metaConstraints :: Set.Set Constraint
} deriving (Show, Ord, Eq)


prettyArgument :: Argument -> MDoc
prettyArgument (PackedArgument i c) =
  "Packed" <+> "x" <> pretty i <+> parens (prettyType c)
prettyArgument (UnpackedArgument i c) =
  "Unpacked" <+> "x" <> pretty i <+> parens (prettyType c)
prettyArgument (PassThroughArgument i) =
  "PassThrough" <+> "x" <> pretty i

argId :: Argument -> Int
argId (PackedArgument i _) = i
argId (UnpackedArgument i _) = i
argId (PassThroughArgument i ) = i

argType :: Argument -> Maybe CType
argType (PackedArgument _ t) = Just t
argType (UnpackedArgument _ t) = Just t
argType (PassThroughArgument _) = Nothing

unpackArgument :: Argument -> Argument
unpackArgument (PackedArgument i t) = UnpackedArgument i t
unpackArgument x = x

nargsTypeM :: TypeM -> Int
nargsTypeM (Function ts _) = length ts
nargsTypeM _ = 0

prettyExprM :: ExprM -> MDoc
prettyExprM e = (vsep . punctuate line . fst $ f e) <> line where
  manNamer :: Int -> MDoc
  manNamer i = "m" <> pretty i

  f :: ExprM -> ([MDoc], MDoc)
  f (ManifoldM i args e) =
    let (ms', body) = f e
        head = manNamer i <> tupled (map prettyArgument args)
        mdoc = block 4 head body
    in (mdoc : ms', manNamer i)
  f (PoolCallM t cmds) = ([], "PoolCall" <> tupled cmds) 
  f (ForeignInterfaceM t e) =
    let (ms, e') = f e
    in (ms, "ForeignInterface")
  f (LetM v e1 e2) =
    let (ms1', e1') = f e1
        (ms2', e2') = f e2
    in (ms1' ++ ms2', "a" <> pretty v <+> "=" <+> e1' <> line <> e2')
  f (AppM fun xs) =
    let (ms', fun') = f fun
        (mss', xs') = unzip $ map f xs
    in (ms' ++ concat mss', fun' <> tupled xs')
  f (SrcM c src) = ([], pretty (srcName src))
  f (LamM args e) =
    let (ms', e') = f e
        vsFull = map prettyArgument args
        vsNames = map (\r -> "x" <> pretty (argId r)) args
    in (ms', "\\ " <+> hsep (punctuate "," vsFull) <> "->" <+> e' <> tupled vsNames)
  f (BndVarM c i) = ([], "x" <> pretty i)
  f (LetVarM c i) = ([], "a" <> pretty i)
  f (ListM _ es) =
    let (mss', es') = unzip $ map f es
    in (concat mss', list es')
  f (TupleM _ es) =
    let (mss', es') = unzip $ map f es
    in (concat mss', tupled es')
  f (RecordM c entries) =
    let (mss', es') = unzip $ map (f . snd) entries
        entries' = zipWith (\k v -> pretty k <> "=" <> v) (map fst entries) es'
    in (concat mss', "{" <> tupled entries' <> "}")
  f (LogM _ x) = ([], if x then "true" else "false")
  f (NumM _ x) = ([], viaShow x)
  f (StrM _ x) = ([], dquotes $ pretty x)
  f (NullM _) = ([], "null")
  f (PackM e) =
    let (ms, e') = f e
    in (ms, "PACK" <> tupled [e'])
  f (UnpackM e) =
    let (ms, e') = f e
    in (ms, "UNPACK" <> tupled [e'])
  f (ReturnM e) =
    let (ms, e') = f e
    in (ms, "RETURN(" <> e' <> ")")

invertExprM :: ExprM -> MorlocMonad ExprM
invertExprM (ManifoldM i args e) = do
  MM.startCounter
  e' <- invertExprM e
  return $ ManifoldM i args e'
invertExprM (LetM v e1 e2) = do
  e2' <- invertExprM e2
  return $ LetM v e1 e2'
invertExprM e@(AppM f es) = do
  f' <- invertExprM f
  es' <- mapM invertExprM es
  v <- MM.getCounter
  let t = typeOfExprM e
      appM' = LetM v (AppM (terminalOf f') (map terminalOf es')) (LetVarM t v)
  return $ foldl dependsOn appM' (f':es')
-- you can't pull the body of the lambda out into a let statement
invertExprM f@(LamM _ _) = return f
invertExprM (ListM c es) = do
  es' <- mapM invertExprM es
  v <- MM.getCounter
  let e = LetM v (ListM c (map terminalOf es')) (LetVarM c v)
      e' = foldl (\x y -> dependsOn x y) e es'
  return e'
invertExprM (TupleM c es) = do
  es' <- mapM invertExprM es
  v <- MM.getCounter
  let e = LetM v (TupleM c (map terminalOf es')) (LetVarM c v)
      e' = foldl (\x y -> dependsOn x y) e es'
  return e'
invertExprM (RecordM c entries) = do
  es' <- mapM invertExprM (map snd entries)
  v <- MM.getCounter
  let entries' = zip (map fst entries) (map terminalOf es')
      e = LetM v (RecordM c entries') (LetVarM c v)
      e' = foldl (\x y -> dependsOn x y) e es'
  return e'
invertExprM (PackM e) = do
  e' <- invertExprM e
  v <- MM.getCounter
  let t' = typeOfExprM e
  return $ dependsOn (LetM v (PackM (terminalOf e')) (LetVarM t' v)) e'
invertExprM (UnpackM e) = do
  e' <- invertExprM e
  v <- MM.getCounter
  let t' = typeOfExprM e
  return $ dependsOn (LetM v (UnpackM (terminalOf e')) (LetVarM t' v)) e'
invertExprM (ReturnM e) = do
  e' <- invertExprM e
  return $ dependsOn (ReturnM (terminalOf e')) e'
invertExprM e = return e

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

typeOfTypeM :: TypeM -> Maybe CType 
typeOfTypeM t = fmap CType (typeOfTypeM' t) where
  typeOfTypeM' Passthrough = Nothing
  typeOfTypeM' (Packed c) = Just (typeOf c)
  typeOfTypeM' (Unpacked c) = Just (typeOf c)
  typeOfTypeM' (Function [] t) = typeOfTypeM' t
  typeOfTypeM' (Function (ti:ts) to) = FunT <$> typeOfTypeM' ti <*> typeOfTypeM' (Function ts to)  

arg2typeM :: Argument -> TypeM
arg2typeM (PackedArgument _ c) = Packed c
arg2typeM (UnpackedArgument _ c) = Unpacked c
arg2typeM (PassThroughArgument _) = Passthrough

-- | Get the manifold type of an expression
--
-- The ExprM must have exactly enough type information to infer the type of any
-- element without reference to the element's parent.
typeOfExprM :: ExprM -> TypeM
typeOfExprM (ManifoldM _ args e) = Function (map arg2typeM args) (typeOfExprM e)
typeOfExprM (ForeignInterfaceM t _) = t
typeOfExprM (PoolCallM t _) = t
typeOfExprM (LetM _ _ e2) = typeOfExprM e2
typeOfExprM (AppM f xs) = case typeOfExprM f of
  (Function inputs output) -> case drop (length xs) inputs of
    [] -> output
    inputs' -> Function inputs' output
  _ -> error "COMPILER BUG: application of non-function"
typeOfExprM (SrcM t _) = t
typeOfExprM (LamM args x) = Function (map arg2typeM args) (typeOfExprM x)
typeOfExprM (BndVarM t _) = t
typeOfExprM (LetVarM t _) = t
typeOfExprM (ListM t _) = t
typeOfExprM (TupleM t _) = t
typeOfExprM (RecordM t _) = t
typeOfExprM (LogM t _) = t
typeOfExprM (NumM t _) = t
typeOfExprM (StrM t _) = t
typeOfExprM (NullM t) = t
typeOfExprM (PackM e) = packTypeM (typeOfExprM e)
typeOfExprM (UnpackM e) = unpackTypeM (typeOfExprM e)
typeOfExprM (ReturnM e) = typeOfExprM e

packTypeM :: TypeM -> TypeM
packTypeM (Unpacked t) = Packed t
packTypeM (Function ts t) = error $ "BUG: Cannot pack a function"
packTypeM t = t

ctype2typeM :: CType -> TypeM
ctype2typeM f@(CType (FunT _ _)) = case typeParts f of
  (inputs, output) -> Function (map ctype2typeM inputs) (ctype2typeM output)
ctype2typeM c = Unpacked c

-- get input types to a function type
typeParts :: CType -> ([CType], CType)
typeParts c = case reverse . map CType $ typeArgs (unCType c) of
  (t:ts) -> (reverse ts, t)
  where
    typeArgs (FunT t1 t2) = t1 : typeArgs t2
    typeArgs t = [t]


unpackTypeM :: TypeM -> TypeM
unpackTypeM Passthrough = error $ "BUG: Cannot unpack a passthrough type"
unpackTypeM (Packed t) = Unpacked t
unpackTypeM t = t 

unpackExprM :: ExprM -> ExprM
unpackExprM e = case typeOfExprM e of
  (Packed _) -> UnpackM e
  (Passthrough) -> error "Cannot unpack a passthrough type argument"
  _ -> e

packExprM :: ExprM -> ExprM
packExprM e = case typeOfExprM e of
  (Unpacked _) -> PackM e
  (Function _ _) -> error "Cannot pack a function"
  _ -> e
