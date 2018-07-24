module Morloc.Processor (process) where

import qualified Morloc.Error as ME
import qualified Morloc.Data as MD
import qualified Morloc.Util as MU
import Data.List ((\\))
import qualified Control.Monad as CM

process :: MD.Program -> ME.ThrowsError MD.Program
process p
  =   return p
  >>= validateSources
  >>= variablesAreDefined -- ensure all variables come from somewhere
  >>= functionsHaveTypes  -- ensure all declared functions have assigned types
  >>= uniqueDeclarations  -- ensure that there are no reassignments
  >>= typecheckAll        -- typecheck with resolutions

validateSources :: MD.Program -> ME.ThrowsError MD.Program
validateSources p = return p >>= uniquePaths >>= uniqueImports
  where
    paths = [n | (MD.Source _ (Just n) _) <- MD.programSources p]

    uniquePaths :: MD.Program -> ME.ThrowsError MD.Program
    uniquePaths p' = case MU.repeated paths of
      [] -> Right p'
      xs -> Left (ME.NameConflict ("Non-unique import paths: " ++ unwords xs))

    uniqueImports :: MD.Program -> ME.ThrowsError MD.Program
    uniqueImports p' = case (MU.repeated (sourceNames p')) of
      [] -> Right p'
      xs -> Left (ME.NameConflict ("Non-unique imports: " ++ unwords xs))

variablesAreDefined :: MD.Program -> ME.ThrowsError MD.Program
variablesAreDefined p = case (varNames p \\ definedNames p) of
  [] -> Right p
  xs -> Left (ME.UndefinedValue xs)
  where
    definedNames p' = sourceNames p' ++ dataDeclarationNames p'

functionsHaveTypes :: MD.Program -> ME.ThrowsError MD.Program
functionsHaveTypes p = case (functionNames p \\ typeDeclarationNames p) of
  [] -> Right p
  _  -> Left (ME.TypeError "No type signature found for this function")

uniqueDeclarations :: MD.Program -> ME.ThrowsError MD.Program
uniqueDeclarations p = return p >>= uniqueData >>= uniqueType
  where
    uniqueData :: MD.Program -> ME.ThrowsError MD.Program
    uniqueData p' = case (MU.repeated (dataDeclarationNames p')) of
      [] -> Right p'
      xs -> Left (ME.NameConflict ("Repeated data declaration: " ++ unwords xs))

    uniqueType :: MD.Program -> ME.ThrowsError MD.Program
    uniqueType p' = case (MU.repeated (typeDeclarationNames p')) of
      [] -> Right p'
      xs -> Left (ME.NameConflict ("Repeated type declaration: " ++ unwords xs))

typecheckAll :: MD.Program -> ME.ThrowsError MD.Program
typecheckAll p = (CM.join . fmap (sequence . map checkPair) $ obsVsExp) >> return p
  where

    -- get all calls
    cs = calls p 

    -- transform each call to an observed type signature
    obsTypes :: ME.ThrowsError [MD.TypeDecl]
    obsTypes = sequence . map namedData2TypeDecl $ cs

    namedData2TypeDecl :: (MD.Name, [MD.MData]) -> ME.ThrowsError MD.TypeDecl
    namedData2TypeDecl (n,xs)
      =   MD.TypeDecl
      <$> pure n
      <*> (     MD.TypeFun
            <$> pure Nothing
            <*> (sequence . map (mdata2mtype p) $ xs)
            <*> pure MD.TypeUnk
          )
      <*> pure []

    -- expected type signatures
    expTypes :: [MD.TypeDecl]
    expTypes = MD.programTypes p

    -- map observed to expected types
    obsVsExp :: ME.ThrowsError [(MD.MType, MD.MType)]
    obsVsExp = getObsVsExp <$> obsTypes <*> pure expTypes

    getObsVsExp :: [MD.TypeDecl] -> [MD.TypeDecl] -> [(MD.MType, MD.MType)]
    getObsVsExp os es = [(o,e) |
          (MD.TypeDecl m o _) <- os
        , (MD.TypeDecl n e _) <- es
        , m == n
      ] 

    -- compare observed to expected types
    checkPair :: (MD.MType, MD.MType) -> ME.ThrowsError ()
    checkPair (o, e)
      | o == e = Right ()
      | otherwise = Left (ME.TypeError (unlines [
            "Observed and expected types differ"
          , "Expected:"
          , "> " ++ show e
          , "Observed:"
          , "> " ++ show o
          ])
        )

mdata2mtype :: MD.Program -> MD.MData -> ME.ThrowsError MD.MType
mdata2mtype _ (MD.DataInt _) = Right $ MD.TypeSpc Nothing "Int" [] 
mdata2mtype _ (MD.DataNum _) = Right $ MD.TypeSpc Nothing "Num" [] 
mdata2mtype _ (MD.DataLog _) = Right $ MD.TypeSpc Nothing "Bool" []
mdata2mtype p (MD.DataLst xs) =
  fmap (MD.TypeSpc Nothing "List") (sequence [findListType p xs])
mdata2mtype p (MD.DataTup xs) =
  fmap (MD.TypeSpc Nothing "Tuple") (sequence . map (mdata2mtype p) $ xs)
mdata2mtype p (MD.DataRec xs) =
  fmap (MD.TypeSpc Nothing "Record") (sequence . map (kwd2mtype p) $ xs)
    where
      kwd2mtype :: MD.Program -> (MD.Name, MD.MData) -> ME.ThrowsError MD.MType
      kwd2mtype p' (n, x) = MD.TypeKwd <$> pure n <*> mdata2mtype p' x
mdata2mtype _ (MD.DataStr _)   = Right $ MD.TypeSpc Nothing "String" []
mdata2mtype p (MD.DataFun x _) = getFunctionOutput p x
mdata2mtype p (MD.DataVar x) = getTypeByName p x

getFunctionOutput :: MD.Program -> String -> ME.ThrowsError MD.MType
getFunctionOutput p s = case outs of
  []  -> Left (ME.CouldNotFind s)
  [x] -> Right x
  xs -> Left (ME.NameConflict ("multiple definitions for " ++ s))
  where
    outs = [t | (MD.TypeDecl n (MD.TypeFun _ _ t) []) <- MD.programTypes p, n == s] 

getTypeByName :: MD.Program -> String -> ME.ThrowsError MD.MType
getTypeByName p s = case [t | (MD.TypeDecl n t _) <- MD.programTypes p, n == s] of
  [ ] -> Left (ME.CouldNotFind s)
  [x] -> Right x
  _   -> Left (ME.NameConflict ("multiple type signatures for " ++ s))

findListType :: MD.Program -> [MD.MData] -> ME.ThrowsError MD.MType
findListType _ [] = Right MD.TypeUnk
findListType p (x:[]) = mdata2mtype p x
findListType p (x:xs)
  | all ((==) x) xs = mdata2mtype p x
  | otherwise = Left (ME.TypeError "Heterogenous lists are not supported")

-- Require [a] and [b] be of the same length
zipWithError :: (a -> b -> ME.ThrowsError c) -> [a] -> [b] -> ME.ThrowsError [c]
zipWithError f xs ys
  | length xs == length ys = sequence $ zipWith f xs ys
  | otherwise = Left (ME.TypeError "Expected equal length vectors")

findType :: [MD.TypeDecl] -> MD.Name -> ME.ThrowsError MD.MType
findType ((MD.TypeDecl n t _):ts) m = MU.ifelse (n == m) (Right t) (findType ts m)
findType [] m = Left (ME.TypeError ("No type signature found for " ++ m))

-- resolve :: MD.MType -> MD.MType -> ME.ThrowsError (MD.MType -> MD.MType)
-- resolve _ _ = Left (ME.TypeError "Unresolvable mismatch")

sourceNames :: MD.Program -> [String]
sourceNames p = concat [sourceNames' s | s <- MD.programSources p] where
  sourceNames' :: MD.Source -> [String]
  sourceNames' (MD.Source _ _ ns) = [unalias n | n <- ns]

  unalias :: (MD.Name, Maybe MD.Alias) -> String
  unalias (_, Just x) = x
  unalias (x, _     ) = x

dataDeclarationNames :: MD.Program -> [String]
dataDeclarationNames p = [n | (MD.DataDecl n _ _) <- MD.programData p]

typeDeclarationNames :: MD.Program -> [String]
typeDeclarationNames p = [n | (MD.TypeDecl n _ _) <- MD.programTypes p]

varNames :: MD.Program -> [String]
varNames p = concat [n:(f t \\ args) | (MD.DataDecl n args t) <- MD.programData p]
  where
    f :: MD.MData -> [String]
    f (MD.DataLst xs) = MU.conmap f xs
    f (MD.DataTup xs) = MU.conmap f xs
    f (MD.DataRec xs) = MU.conmap f (map snd xs)
    f (MD.DataFun n xs) = n:(MU.conmap f xs)
    f (MD.DataVar n) = [n]
    f _ = []

functionNames :: MD.Program -> [String]
functionNames p = concat [n:(f t \\ args) | (MD.DataDecl n args t) <- MD.programData p]
  where
    f :: MD.MData -> [String]
    f (MD.DataLst xs) = MU.conmap f xs
    f (MD.DataTup xs) = MU.conmap f xs
    f (MD.DataRec xs) = MU.conmap f (map snd xs)
    f (MD.DataFun n xs) = n:(MU.conmap f xs)
    f _ = []

calls :: MD.Program -> [(MD.Name, [MD.MData])]
calls p = concat [winnow args (f t) | (MD.DataDecl n args t) <- MD.programData p]
  where
    f :: MD.MData -> [(MD.Name, [MD.MData])]
    f (MD.DataLst xs) = MU.conmap f xs
    f (MD.DataTup xs) = MU.conmap f xs
    f (MD.DataRec xs) = MU.conmap f (map snd xs)
    f (MD.DataFun n xs) = (n, xs):(MU.conmap f xs)
    f _ = []

    winnow :: (Eq a, Eq b) => [a] -> [(a, b)] -> [(a, b)]
    winnow ns = filter (not . (flip elem) ns . fst)
