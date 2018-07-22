module Morloc.Processor (process) where

import Morloc.Error
import Morloc.Data
import Morloc.Util (repeated, conmap, ifelse)
import Data.List ((\\))
import Control.Monad (join)

process :: Program -> ThrowsError Program
process p
  =   return p
  >>= validateSources
  >>= variablesAreDefined -- ensure all variables come from somewhere
  >>= functionsHaveTypes  -- ensure all declared functions have assigned types
  >>= uniqueDeclarations  -- ensure that there are no reassignments
  >>= typecheckAll        -- typecheck with resolutions

validateSources :: Program -> ThrowsError Program
validateSources p = return p >>= uniquePaths >>= uniqueImports
  where
    paths = [n | (Source _ (Just n) _) <- programSources p]

    uniquePaths :: Program -> ThrowsError Program
    uniquePaths p' = case repeated paths of
      [] -> Right p'
      xs -> Left (NameConflict ("Non-unique import paths: " ++ unwords xs))

    uniqueImports :: Program -> ThrowsError Program
    uniqueImports p' = case (repeated (sourceNames p')) of
      [] -> Right p'
      xs -> Left (NameConflict ("Non-unique imports: " ++ unwords xs))

variablesAreDefined :: Program -> ThrowsError Program
variablesAreDefined p = case (varNames p \\ definedNames p) of
  [] -> Right p
  xs -> Left (UndefinedValue xs)
  where
    definedNames p' = sourceNames p' ++ dataDeclarationNames p'

functionsHaveTypes :: Program -> ThrowsError Program
functionsHaveTypes p = case (functionNames p \\ typeDeclarationNames p) of
  [] -> Right p
  _  -> Left (TypeError "No type signature found for this function")

uniqueDeclarations :: Program -> ThrowsError Program
uniqueDeclarations p = return p >>= uniqueData >>= uniqueType
  where
    uniqueData :: Program -> ThrowsError Program
    uniqueData p' = case (repeated (dataDeclarationNames p')) of
      [] -> Right p'
      xs -> Left (NameConflict ("Repeated data declaration: " ++ unwords xs))

    uniqueType :: Program -> ThrowsError Program
    uniqueType p' = case (repeated (typeDeclarationNames p')) of
      [] -> Right p'
      xs -> Left (NameConflict ("Repeated type declaration: " ++ unwords xs))

typecheckAll :: Program -> ThrowsError Program
typecheckAll p = (join . fmap (sequence . map checkPair) $ obsVsExp) >> return p
  where

    -- get all calls
    cs = calls p 

    -- transform each call to an observed type signature
    obsTypes :: ThrowsError [TypeDecl]
    obsTypes = sequence . map namedData2TypeDecl $ cs

    namedData2TypeDecl :: (Name, [MData]) -> ThrowsError TypeDecl
    namedData2TypeDecl (n,xs)
      =   TypeDecl
      <$> pure n
      <*> (     TypeFun
            <$> pure Nothing
            <*> (sequence . map (mdata2mtype p) $ xs)
            <*> pure TypeUnk
          )
      <*> pure []

    -- expected type signatures
    expTypes :: [TypeDecl]
    expTypes = programTypes p

    -- map observed to expected types
    obsVsExp :: ThrowsError [(MType, MType)]
    obsVsExp = getObsVsExp <$> obsTypes <*> pure expTypes

    getObsVsExp :: [TypeDecl] -> [TypeDecl] -> [(MType, MType)]
    getObsVsExp os es = [(o,e) |
          (TypeDecl m o _) <- os
        , (TypeDecl n e _) <- es
        , m == n
      ] 

    -- compare observed to expected types
    checkPair :: (MType, MType) -> ThrowsError ()
    checkPair (o, e)
      | o == e = Right ()
      | otherwise = Left (TypeError "Observed and expected types differ")

mdata2mtype :: Program -> MData -> ThrowsError MType
mdata2mtype _ (DataInt _) = Right $ TypeSpc Nothing "Int" [] 
mdata2mtype _ (DataNum _) = Right $ TypeSpc Nothing "Num" [] 
mdata2mtype _ (DataLog _) = Right $ TypeSpc Nothing "Bool" []
mdata2mtype p (DataLst xs) =
  fmap (TypeSpc Nothing "List") (sequence [findListType p xs])
mdata2mtype p (DataTup xs) =
  fmap (TypeSpc Nothing "Tuple") (sequence . map (mdata2mtype p) $ xs)
mdata2mtype p (DataRec xs) =
  fmap (TypeSpc Nothing "Record") (sequence . map (kwd2mtype p) $ xs)
    where
      kwd2mtype :: Program -> (Name, MData) -> ThrowsError MType
      kwd2mtype p' (n, x) = TypeKwd <$> pure n <*> mdata2mtype p' x
mdata2mtype _ (DataStr _)   = Right $ TypeSpc Nothing "String" []
mdata2mtype p (DataFun x _) = getFunctionOutput p x
mdata2mtype p (DataVar x) = getTypeByName p x

getFunctionOutput :: Program -> String -> ThrowsError MType
getFunctionOutput p s = case outs of
  []  -> Left (CouldNotFind s)
  [x] -> Right x
  xs -> Left (NameConflict ("multiple definitions for " ++ s))
  where
    outs = [t | (TypeDecl n (TypeFun _ _ t) []) <- programTypes p, n == s] 

getTypeByName :: Program -> String -> ThrowsError MType
getTypeByName p s = case [t | (TypeDecl n t _) <- programTypes p, n == s] of
  [ ] -> Left (CouldNotFind s)
  [x] -> Right x
  _   -> Left (NameConflict ("multiple type signatures for " ++ s))

findListType :: Program -> [MData] -> ThrowsError MType
findListType _ [] = Right TypeUnk
findListType p (x:[]) = mdata2mtype p x
findListType p (x:xs)
  | all ((==) x) xs = mdata2mtype p x
  | otherwise = Left (TypeError "Heterogenous lists are not supported")

-- Require [a] and [b] be of the same length
zipWithError :: (a -> b -> ThrowsError c) -> [a] -> [b] -> ThrowsError [c]
zipWithError f xs ys
  | length xs == length ys = sequence $ zipWith f xs ys
  | otherwise = Left (TypeError "Expected equal length vectors")

findType :: [TypeDecl] -> Name -> ThrowsError MType
findType ((TypeDecl n t _):ts) m = ifelse (n == m) (Right t) (findType ts m)
findType [] m = Left (TypeError ("No type signature found for " ++ m))

-- resolve :: MType -> MType -> ThrowsError (MType -> MType)
-- resolve _ _ = Left (TypeError "Unresolvable mismatch")

sourceNames :: Program -> [String]
sourceNames p = concat [sourceNames' s | s <- programSources p] where
  sourceNames' :: Source -> [String]
  sourceNames' (Source _ _ ns) = [unalias n | n <- ns]

  unalias :: (Name, Maybe Alias) -> String
  unalias (_, Just x) = x
  unalias (x, _     ) = x

dataDeclarationNames :: Program -> [String]
dataDeclarationNames p = [n | (DataDecl n _ _) <- programData p]

typeDeclarationNames :: Program -> [String]
typeDeclarationNames p = [n | (TypeDecl n _ _) <- programTypes p]

varNames :: Program -> [String]
varNames p = concat [n:(f t \\ args) | (DataDecl n args t) <- programData p]
  where
    f :: MData -> [String]
    f (DataLst xs) = conmap f xs
    f (DataTup xs) = conmap f xs
    f (DataRec xs) = conmap f (map snd xs)
    f (DataFun n xs) = n:(conmap f xs)
    f (DataVar n) = [n]
    f _ = []

functionNames :: Program -> [String]
functionNames p = concat [n:(f t \\ args) | (DataDecl n args t) <- programData p]
  where
    f :: MData -> [String]
    f (DataLst xs) = conmap f xs
    f (DataTup xs) = conmap f xs
    f (DataRec xs) = conmap f (map snd xs)
    f (DataFun n xs) = n:(conmap f xs)
    f _ = []

calls :: Program -> [(Name, [MData])]
calls p = concat [winnow args (f t) | (DataDecl n args t) <- programData p]
  where
    f :: MData -> [(Name, [MData])]
    f (DataLst xs) = conmap f xs
    f (DataTup xs) = conmap f xs
    f (DataRec xs) = conmap f (map snd xs)
    f (DataFun n xs) = (n, xs):(conmap f xs)
    f _ = []

    winnow :: (Eq a, Eq b) => [a] -> [(a, b)] -> [(a, b)]
    winnow ns = filter (not . (flip elem) ns . fst)
