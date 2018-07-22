module Morloc.Processor (process) where

import Morloc.Error
import Morloc.Data
import Morloc.Util (repeated, conmap)
import Data.List ((\\))

-- Things to verify
-- [x] source names are unique
-- [x] source paths are unique
-- [x] each function is from a source
-- [x] each source has a type declaration 
-- [x] data declaration names are unique
-- [x] type declaration names are unique
-- [x] each function declaration has a type declaration
-- [ ] each argument to a function is correctly typed

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
typecheckAll = return
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
