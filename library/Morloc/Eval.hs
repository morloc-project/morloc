module Morloc.Eval (buildProgram) where

import Morloc.Data
import Morloc.Tree
import Morloc.Error
import Morloc.Triple

buildProgram :: Tree -> ThrowsError Program
buildProgram t
  =   Program 
  <$> getTypeDeclarations t
  <*> getDataDeclarations t
  <*> getSources t

toOne :: [a] -> ThrowsError a
toOne [x] = Right x 
toOne [] = Left (InvalidRDF "Expected 1 object, found 0")
toOne _ = Left (InvalidRDF "Expected 1 object for this relation, found many")

toMaybe :: [a] -> ThrowsError (Maybe a)
toMaybe [ ] = Right Nothing
toMaybe [x] = Right (Just x)
toMaybe  _  = Left (InvalidRDF "Expected 0 or 1 elements")

getString :: Relation -> Tree -> ThrowsError String
getString r t = getStrings r t >>= toOne

getStringMaybe :: Relation -> Tree -> ThrowsError (Maybe String)
getStringMaybe r t = getStrings r t >>= toMaybe

getKid :: Relation -> Tree -> ThrowsError Tree
getKid r t = toOne $ getKids r t

getTypeDeclarations :: Tree -> ThrowsError [TypeDecl]
getTypeDeclarations = sequence . recursiveApply cond' fun'
  where
    cond' = hasRelation ":isa" (Leaf $ Str' ":typeDeclaration")
    fun' t
      =   TypeDecl
      <$> (getKid ":lhs" t >>= getString ":value") 
      <*> (getKid ":rhs" t >>= tree2mtype)
      <*> pure []

getDataDeclarations :: Tree -> ThrowsError [DataDecl]
getDataDeclarations = sequence . recursiveApply cond' fun'
  where
    cond' = hasRelation ":isa" (Leaf $ Str' ":dataDeclaration")
    fun' t
      =   DataDecl
      <$> (getKid ":lhs" t >>= getString ":value") 
      <*> (sequence . map (getString ":value") $ getKids ":parameter" t)
      <*> (getKid ":rhs" t >>= tree2mdata)

getSources :: Tree -> ThrowsError [Source]
getSources = sequence . recursiveApply cond' fun'
  where
    cond' = hasRelation ":isa" (Leaf $ Str' ":source")
    fun' t = Source <$> language' t <*> path' t <*> imports' t

    language' :: Tree -> ThrowsError Language
    language' = getString ":lang"

    path' :: Tree -> ThrowsError (Maybe Path)
    path' = getStringMaybe ":path"

    imports' :: Tree -> ThrowsError [(Name, Maybe Alias)]
    imports' t = sequence $ map getImport' (getKids ":import" t)

    getImport' :: Tree -> ThrowsError (Name, Maybe Alias)
    getImport' t = (,) <$> getString ":name" t <*> getStringMaybe ":alias" t

tree2mtype :: Tree -> ThrowsError MType
tree2mtype t = case getString ":isa" t of
  (Right ":atomicType"           ) -> parsetype' t TypeSpc
  (Right ":atomicGeneric"        ) -> parsetype' t TypeGen
  (Right ":parameterizedType"    ) -> parsetype' t TypeSpc
  (Right ":parameterizedGeneric" ) -> parsetype' t TypeGen
  (Right ":namedType"            ) -> namedtype' t
  (Right ":functionType"         ) -> function' t
  (Left err) -> Left err
  _ -> Left (InvalidRDF "Expected type, none found")
  where

    parsetype' :: Tree -> (Tag -> Name -> [MType] -> MType) -> ThrowsError MType
    parsetype' t' f = f
      <$> pure Nothing
      <*> getString ":value" t'
      <*> (sequence . map tree2mtype $ getKids ":parameter" t')

    namedtype' :: Tree -> ThrowsError MType
    namedtype' t'
      =   TypeKwd
      <$> (getString ":name" t')
      <*> (getKid ":value" t' >>= tree2mtype)

    function' :: Tree -> ThrowsError MType
    function' t' = TypeFun
      <$> pure Nothing
      <*> (sequence . map tree2mtype $ getKids ":input" t')
      <*> (getKid ":output" t' >>= tree2mtype)

tree2mdata :: Tree -> ThrowsError MData
tree2mdata t = case (getString ":isa" t) of
  (Right ":integer"  ) -> getKid ":value" t >>= getInt
  (Right ":number"   ) -> getKid ":value" t >>= getNum
  (Right ":boolean"  ) -> getKid ":value" t >>= getLog
  (Right ":string"   ) -> getKid ":value" t >>= getStr
  (Right ":name"     ) -> getKid ":value" t >>= getStr
  (Right ":list"     ) -> fmap DataLst (list' t)
  (Right ":tuple"    ) -> fmap DataTup (list' t)
  (Right ":record"   ) -> record' t
  (Right ":call"     ) -> call' t
  _                    -> Left (InvalidRDF "Expected MData")
  where

    getInt :: Tree -> ThrowsError MData
    getInt (Leaf (Int' x)) = Right (DataInt x)
    getInt _ = Left (InvalidRDF "Expected an integer")

    getNum :: Tree -> ThrowsError MData
    getNum (Leaf (Num' x)) = Right (DataNum x)
    getNum _ = Left (InvalidRDF "Expected an number")

    getLog :: Tree -> ThrowsError MData
    getLog (Leaf (Log' x)) = Right (DataLog x)
    getLog _ = Left (InvalidRDF "Expected an boolean")

    getStr :: Tree -> ThrowsError MData
    getStr (Leaf (Str' x)) = Right (DataStr x)
    getStr _ = Left (InvalidRDF "Expected an string")

    asMData :: Tree -> ThrowsError MData
    asMData (Leaf o) = case o of
      (Int' x)   -> Right (DataInt x)
      (Num' x)   -> Right (DataNum x)
      (Log' x)   -> Right (DataLog x)
      (Str' x)   -> Right (DataStr x)
      -- (Str' x)   -> Right (DataVar x) -- FIXME: distinguish between Str and Var
      _ -> Left (InvalidRDF "Oh no Mr. Wizard!")
    asMData node = tree2mdata node

    list' :: Tree -> ThrowsError [MData]
    list' = sequence . map asMData . getKids ":contains"

    -- convert one record entry
    record' :: Tree -> ThrowsError MData
    record' t = DataRec <$> (sequence . map recordEntry' $ getKids ":contains" t)
      where
        recordEntry' t
          =   (,)
          <$> getString ":lhs" t
          <*> (getKid ":rhs" t >>= asMData)

    call' :: Tree -> ThrowsError MData
    call' t = DataFun 
      <$> (getKid ":value" t >>= getString ":value")
      <*> (sequence . map asMData $ getKids ":argument" t)
