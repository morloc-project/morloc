module Morloc.Evaluator (tree2program) where

import qualified Morloc.Data as MD
import qualified Morloc.Tree as MT
import qualified Morloc.Error as ME
import qualified Morloc.Triple as M3

tree2program :: MT.Tree -> ME.ThrowsError MD.Program
tree2program t
  =   MD.Program 
  <$> getTypeDeclarations t
  <*> getDataDeclarations t
  <*> getSources t

toOne :: [a] -> ME.ThrowsError a
toOne [x] = Right x 
toOne [] = Left (ME.InvalidRDF "Expected 1 object, found 0")
toOne _ = Left (ME.InvalidRDF "Expected 1 object for this relation, found many")

toMaybe :: [a] -> ME.ThrowsError (Maybe a)
toMaybe [ ] = Right Nothing
toMaybe [x] = Right (Just x)
toMaybe  _  = Left (ME.InvalidRDF "Expected 0 or 1 elements")

getString :: M3.Relation -> MT.Tree -> ME.ThrowsError String
getString r t = MT.getStrings r t >>= toOne

getStringMaybe :: M3.Relation -> MT.Tree -> ME.ThrowsError (Maybe String)
getStringMaybe r t = MT.getStrings r t >>= toMaybe

getKid :: M3.Relation -> MT.Tree -> ME.ThrowsError MT.Tree
getKid r t = toOne $ MT.getKids r t

getTypeDeclarations :: MT.Tree -> ME.ThrowsError [MD.TypeDecl]
getTypeDeclarations = sequence . MT.recursiveApply cond' fun'
  where
    cond' = MT.hasRelation ":isa" (MT.Leaf $ M3.Str' ":typeDeclaration")
    fun' t
      =   MD.TypeDecl
      <$> (getKid ":lhs" t >>= getString ":value") 
      <*> (getKid ":rhs" t >>= tree2mtype)
      <*> pure []

getDataDeclarations :: MT.Tree -> ME.ThrowsError [MD.DataDecl]
getDataDeclarations = sequence . MT.recursiveApply cond' fun'
  where
    cond' = MT.hasRelation ":isa" (MT.Leaf $ M3.Str' ":dataDeclaration")
    fun' t
      =   MD.DataDecl
      <$> (getKid ":lhs" t >>= getString ":value") 
      <*> (sequence . map (getString ":value") $ MT.getKids ":parameter" t)
      <*> (getKid ":rhs" t >>= tree2mdata)

getSources :: MT.Tree -> ME.ThrowsError [MD.Source]
getSources = sequence . MT.recursiveApply cond' fun'
  where
    cond' = MT.hasRelation ":isa" (MT.Leaf $ M3.Str' ":source")
    fun' t = MD.Source <$> language' t <*> path' t <*> imports' t

    language' :: MT.Tree -> ME.ThrowsError MD.Language
    language' = getString ":lang"

    path' :: MT.Tree -> ME.ThrowsError (Maybe MD.Path)
    path' = getStringMaybe ":path"

    imports' :: MT.Tree -> ME.ThrowsError [(MD.Name, Maybe MD.Alias)]
    imports' t = sequence $ map getImport' (MT.getKids ":import" t)

    getImport' :: MT.Tree -> ME.ThrowsError (MD.Name, Maybe MD.Alias)
    getImport' t = (,) <$> getString ":name" t <*> getStringMaybe ":alias" t

tree2mtype :: MT.Tree -> ME.ThrowsError MD.MType
tree2mtype t = case getString ":isa" t of
  (Right ":atomicType"           ) -> parsetype' t MD.TypeSpc
  (Right ":atomicGeneric"        ) -> parsetype' t MD.TypeGen
  (Right ":parameterizedType"    ) -> parsetype' t MD.TypeSpc
  (Right ":parameterizedGeneric" ) -> parsetype' t MD.TypeGen
  (Right ":namedType"            ) -> namedtype' t
  (Right ":functionType"         ) -> function' t
  (Left err) -> Left err
  _ -> Left (ME.InvalidRDF "Expected type, none found")
  where

    parsetype'
      :: MT.Tree
      -> (MD.Tag -> MD.Name -> [MD.MType] -> MD.MType)
      -> ME.ThrowsError MD.MType
    parsetype' t' f = f
      <$> pure Nothing
      <*> getString ":value" t'
      <*> (sequence . map tree2mtype $ MT.getKids ":parameter" t')

    namedtype' :: MT.Tree -> ME.ThrowsError MD.MType
    namedtype' t'
      =   MD.TypeKwd
      <$> (getString ":name" t')
      <*> (getKid ":value" t' >>= tree2mtype)

    function' :: MT.Tree -> ME.ThrowsError MD.MType
    function' t' = MD.TypeFun
      <$> pure Nothing
      <*> (sequence . map tree2mtype $ MT.getKids ":input" t')
      <*> (getKid ":output" t' >>= tree2mtype)

tree2mdata :: MT.Tree -> ME.ThrowsError MD.MData
tree2mdata t = case (getString ":isa" t) of
  (Right ":integer"  ) -> getKid ":value" t >>= getInt
  (Right ":number"   ) -> getKid ":value" t >>= getNum
  (Right ":boolean"  ) -> getKid ":value" t >>= getLog
  (Right ":string"   ) -> getKid ":value" t >>= getStr
  (Right ":name"     ) -> getKid ":value" t >>= getStr
  (Right ":list"     ) -> fmap MD.DataLst (list' t)
  (Right ":tuple"    ) -> fmap MD.DataTup (list' t)
  (Right ":record"   ) -> record' t
  (Right ":call"     ) -> call' t
  _                    -> Left (ME.InvalidRDF "Expected MData")
  where

    getInt :: MT.Tree -> ME.ThrowsError MD.MData
    getInt (MT.Leaf (M3.Int' x)) = Right (MD.DataInt x)
    getInt _ = Left (ME.InvalidRDF "Expected an integer")

    getNum :: MT.Tree -> ME.ThrowsError MD.MData
    getNum (MT.Leaf (M3.Num' x)) = Right (MD.DataNum x)
    getNum _ = Left (ME.InvalidRDF "Expected an number")

    getLog :: MT.Tree -> ME.ThrowsError MD.MData
    getLog (MT.Leaf (M3.Log' x)) = Right (MD.DataLog x)
    getLog _ = Left (ME.InvalidRDF "Expected an boolean")

    getStr :: MT.Tree -> ME.ThrowsError MD.MData
    getStr (MT.Leaf (M3.Str' x)) = Right (MD.DataStr x)
    getStr _ = Left (ME.InvalidRDF "Expected an string")

    asMData :: MT.Tree -> ME.ThrowsError MD.MData
    asMData (MT.Leaf o) = case o of
      (M3.Int' x)   -> Right (MD.DataInt x)
      (M3.Num' x)   -> Right (MD.DataNum x)
      (M3.Log' x)   -> Right (MD.DataLog x)
      (M3.Str' x)   -> Right (MD.DataStr x)
      -- (M3.Str' x)   -> Right (MD.DataVar x) -- FIXME: distinguish between Str and Var
      _ -> Left (ME.InvalidRDF "Oh no Mr. Wizard!")
    asMData node = tree2mdata node

    list' :: MT.Tree -> ME.ThrowsError [MD.MData]
    list' = sequence . map asMData . MT.getKids ":contains"

    -- convert one record entry
    record' :: MT.Tree -> ME.ThrowsError MD.MData
    record' t = MD.DataRec <$> (sequence . map recordEntry' $ MT.getKids ":contains" t)
      where
        recordEntry' t
          =   (,)
          <$> getString ":lhs" t
          <*> (getKid ":rhs" t >>= asMData)

    call' :: MT.Tree -> ME.ThrowsError MD.MData
    call' t = MD.DataFun 
      <$> (getKid ":value" t >>= getString ":value")
      <*> (sequence . map asMData $ MT.getKids ":argument" t)
