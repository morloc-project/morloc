module Morloc.Tree
(
    Tree(..)
  , rdf2tree
  , getTypeDeclarations
) where

import Morloc.Triple
import Morloc.Error
import Morloc.Data

data Tree
  = Node Subject [(Relation, Tree)]
  | Leaf Object
  deriving(Ord, Eq, Show)

rdf2tree :: RDF -> Tree
rdf2tree (RDF i ts) = Node i [(r, f ts o) | (j,r,o) <- ts, j == i]
  where
    f :: [Triple] -> Object -> Tree
    f ts' (Id' j) = rdf2tree (RDF j ts')
    f _    o'     = Leaf o'

ifelse :: Bool -> a -> a -> a
ifelse True  x _ = x
ifelse False _ y = y

getChild :: Relation -> Tree -> [Tree]
getChild r (Node _ xs) = [t | (r', t) <- xs, r == r']
getChild _ _ = []

getStr :: String -> Tree -> ThrowsError [String]
getStr s' t' = sequence . map getStr' $ getChild s' t' where
  getStr' :: Tree -> ThrowsError String
  getStr' (Leaf (Str' s)) = Right s
  getStr' _ = Left (InvalidRDF ("Expected a string"))
    
toOne :: [a] -> ThrowsError a
toOne [x] = Right x 
toOne [] = Left (InvalidRDF "Expected one object, found none")
toOne _ = Left (InvalidRDF "Expected one object for this relation, found many")

recursiveApply
  :: (Tree -> Bool) -- Is this the subtree we are looking for?
  -> (Tree -> a)    -- If yes, use this to process the tree
  -> Tree           -- This is the tree we see at the moment 
  -> [a]            -- Results from all the trees we've processed
recursiveApply _ _ (Leaf _) = [] 
recursiveApply cond f (Node i xs)
  = ifelse
      (cond (Node i xs))
      [f (Node i xs)]
      (concat . map (recursiveApply cond f . snd) $ xs)

hasRelation :: Relation -> Tree -> Tree -> Bool
hasRelation r o (Node _ xs) = any (\(r', o') -> r' == r && o' == o) xs
hasRelation _ _ _ = False

getTypeDeclarations :: Tree -> ThrowsError [TypeDecl]
getTypeDeclarations = sequence . recursiveApply cond' fun'
  where
    cond' = hasRelation ":isa" (Leaf $ Str' ":typeDeclaration")
    fun' t = TypeDecl <$> (toOne (getChild ":lhs" t) >>= getStr ":value" >>= toOne) 
                      <*> (toOne (getChild ":rhs" t) >>= tree2mtype)

getDataDeclarations :: Tree -> ThrowsError [DataDecl]
getDataDeclarations = sequence . recursiveApply cond' fun'
  where
    cond' = hasRelation ":isa" (Leaf $ Str' ":dataDeclaration")
    fun' t = DataDecl <$> (toOne (getChild ":lhs" t) >>= getStr ":value" >>= toOne) 
                      <*> (sequence . map (\t' -> getStr ":value" t >>= toOne) $ getChild ":parameter" t)
                      <*> (toOne (getChild ":rhs" t) >>= tree2mdata)

tree2mtype :: Tree -> ThrowsError MType
tree2mtype t = case isa' t of
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
      <*> val' t'
      <*> (sequence . map tree2mtype $ getChild ":parameter" t')

    namedtype' :: Tree -> ThrowsError MType
    namedtype' t'
      =   TypeKwd
      <$> (getStr ":name" t' >>= toOne)
      <*> (toOne (getChild ":value" t') >>= tree2mtype)

    function' :: Tree -> ThrowsError MType
    function' t' = TypeFun
      <$> pure Nothing
      <*> (sequence . map tree2mtype $ getChild ":input" t')
      <*> ((sequence . map tree2mtype $ getChild ":output" t') >>= toOne)

    isa'  t' = getStr ":isa" t' >>= toOne
    val'  t' = getStr ":value" t' >>= toOne

tree2mdata :: Tree -> ThrowsError MData
tree2mdata = undefined
