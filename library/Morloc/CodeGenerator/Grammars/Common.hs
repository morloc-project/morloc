{-|
Module      : Morloc.CodeGenerator.Grammars.Common
Description : A common set of utility functions for language templates
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Grammars.Common
  ( argType
  , unpackArgument
  , prettyArgument
  , argId
  , prettyExprM
  , prettyTypeM
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
  , jsontype2json
  , type2jsontype
  ) where

import Morloc.Data.Doc
import Morloc.CodeGenerator.Namespace
import Morloc.Pretty (prettyType)
import qualified Data.Map.Strict as Map
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.System as MS

import Data.Scientific (Scientific)
import qualified Data.Set as Set


prettyArgument :: Argument -> MDoc
prettyArgument (SerialArgument i c) =
  "Serial" <+> "x" <> pretty i <+> parens (prettyType c)
prettyArgument (NativeArgument i c) =
  "Native" <+> "x" <> pretty i <+> parens (prettyType c)
prettyArgument (PassThroughArgument i) =
  "PassThrough" <+> "x" <> pretty i

argId :: Argument -> Int
argId (SerialArgument i _) = i
argId (NativeArgument i _) = i
argId (PassThroughArgument i ) = i

argType :: Argument -> Maybe CType
argType (SerialArgument _ t) = Just t
argType (NativeArgument _ t) = Just t
argType (PassThroughArgument _) = Nothing

unpackArgument :: Argument -> Argument
unpackArgument (SerialArgument i t) = NativeArgument i t
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
  f (PoolCallM t cmds args) =
    let poolArgs = cmds ++ map prettyArgument args
    in ([], "PoolCallM" <> list (poolArgs) <+> "::" <+> prettyTypeM t) 
  f (ForeignInterfaceM t e) =
    let (ms, e') = f e
    in (ms, "ForeignInterface :: " <> prettyTypeM t)
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
  f (SerializeM e) =
    let (ms, e') = f e
    in (ms, "PACK" <> tupled [e'])
  f (DeserializeM e) =
    let (ms, e') = f e
    in (ms, "UNPACK" <> tupled [e'])
  f (ReturnM e) =
    let (ms, e') = f e
    in (ms, "RETURN(" <> e' <> ")")

prettyTypeM :: TypeM -> MDoc
prettyTypeM Passthrough = "Passthrough"
prettyTypeM (Serial c) = "Serial<" <> prettyType c <> ">"
prettyTypeM (Native c) = "Native<" <> prettyType c <> ">"
prettyTypeM (Function ts t) =
  "Function<" <> hsep (punctuate "->" (map prettyTypeM (ts ++ [t]))) <> ">"

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
invertExprM (SerializeM e) = do
  e' <- invertExprM e
  v <- MM.getCounter
  let t' = packTypeM $ typeOfExprM e
  return $ dependsOn (LetM v (SerializeM (terminalOf e')) (LetVarM t' v)) e'
invertExprM (DeserializeM e) = do
  e' <- invertExprM e
  v <- MM.getCounter
  let t' = unpackTypeM $ typeOfExprM e
  return $ dependsOn (LetM v (DeserializeM (terminalOf e')) (LetVarM t' v)) e'
invertExprM (ReturnM e) = do
  e' <- invertExprM e
  return $ dependsOn (ReturnM (terminalOf e')) e'
invertExprM (PoolCallM t cmds args) = do
  v <- MM.getCounter
  return $ LetM v (PoolCallM t cmds args) (LetVarM t v)
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
  typeOfTypeM' (Serial c) = Just (typeOf c)
  typeOfTypeM' (Native c) = Just (typeOf c)
  typeOfTypeM' (Function [] t) = typeOfTypeM' t
  typeOfTypeM' (Function (ti:ts) to) = FunT <$> typeOfTypeM' ti <*> typeOfTypeM' (Function ts to)  

arg2typeM :: Argument -> TypeM
arg2typeM (SerialArgument _ c) = Serial c
arg2typeM (NativeArgument _ c) = Native c
arg2typeM (PassThroughArgument _) = Passthrough

-- | Get the manifold type of an expression
--
-- The ExprM must have exactly enough type information to infer the type of any
-- element without reference to the element's parent.
typeOfExprM :: ExprM -> TypeM
typeOfExprM (ManifoldM _ args e) = Function (map arg2typeM args) (typeOfExprM e)
typeOfExprM (ForeignInterfaceM t _) = t
typeOfExprM (PoolCallM t _ _) = t
typeOfExprM (LetM _ _ e2) = typeOfExprM e2
typeOfExprM (AppM f xs) = case typeOfExprM f of
  (Function inputs output) -> case drop (length xs) inputs of
    [] -> output
    inputs' -> Function inputs' output
  _ -> error . MT.unpack . render $ "COMPILER BUG: application of non-function" <+> parens (prettyTypeM $ typeOfExprM f)
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
typeOfExprM (SerializeM e) = packTypeM (typeOfExprM e)
typeOfExprM (DeserializeM e) = unpackTypeM (typeOfExprM e)
typeOfExprM (ReturnM e) = typeOfExprM e

packTypeM :: TypeM -> TypeM
packTypeM (Native t) = Serial t
packTypeM (Function ts t) = error $ "BUG: Cannot pack a function"
packTypeM t = t

unpackTypeM :: TypeM -> TypeM
unpackTypeM (Serial t) = Native t
unpackTypeM Passthrough = error $ "BUG: Cannot unpack a passthrough type"
unpackTypeM t = t 

ctype2typeM :: CType -> TypeM
ctype2typeM f@(CType (FunT _ _)) = case typeParts f of
  (inputs, output) -> Function (map ctype2typeM inputs) (ctype2typeM output)
ctype2typeM (CType (UnkT _)) = Passthrough
ctype2typeM c = Native c

-- get input types to a function type
typeParts :: CType -> ([CType], CType)
typeParts c = case reverse . map CType $ typeArgs (unCType c) of
  (t:ts) -> (reverse ts, t)
  where
    typeArgs (FunT t1 t2) = t1 : typeArgs t2
    typeArgs t = [t]

unpackExprM :: ExprM -> ExprM
unpackExprM e = case typeOfExprM e of
  (Serial _) -> DeserializeM e
  (Passthrough) -> error "Cannot unpack a passthrough typed expression"
  _ -> e

packExprM :: ExprM -> ExprM
packExprM e = case typeOfExprM e of
  (Native _) -> SerializeM e
  -- (Function _ _) -> error "Cannot pack a function"
  _ -> e

type2jsontype :: Type -> MorlocMonad JsonType
type2jsontype (UnkT _) = MM.throwError . CallTheMonkeys $ "Invalid JSON type: UnkT"
type2jsontype (VarT (TV _ v)) = return $ VarJ v
type2jsontype (ArrT (TV _ v) ts) = ArrJ v <$> mapM type2jsontype ts
type2jsontype (FunT _ _) = MM.throwError . CallTheMonkeys $ "Invalid JSON type: FunT"
type2jsontype (NamT (TV _ v) rs) = do
  vs <- mapM type2jsontype (map snd rs)
  return $ NamJ v (zip (map fst rs) vs)

jsontype2json :: JsonType -> MDoc
jsontype2json (VarJ v) = dquotes (pretty v)
jsontype2json (ArrJ v ts) = "{" <> key <> ":" <> val <> "}" where
  key = dquotes (pretty v)
  val = encloseSep "[" "]" "," (map jsontype2json ts)
jsontype2json (NamJ v rs) = "{" <> dquotes (pretty v) <> ":" <> encloseSep "{" "}" "," rs' <> "}" where
  keys = map (dquotes . pretty) (map fst rs) 
  vals = map jsontype2json (map snd rs)
  rs' = zipWith (\k v -> k <> ":" <> v) keys vals
