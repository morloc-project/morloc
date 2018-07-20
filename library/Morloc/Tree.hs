module Morloc.Tree 
(
    Tree(..)
  , rdf2tree
  , getTypes
) where

import Morloc.Triple
import Morloc.Error

data Tree
  = Node Subject [(Relation, Tree)]
  | Leaf Object
  deriving(Ord, Eq, Show)

type Tag = Maybe String
type Name = String

data MType
  = TypeSpc Tag Name [MType]
  | TypeGen Tag Name [MType]
  | TypeFun Tag [MType] MType
  | TypeKwd Name MType
  | TypeEmp Tag
  deriving(Show, Ord, Eq)

data Constraint
  = Constraint String
  deriving(Show, Ord, Eq)

data MData
  = DataInt Integer
  | DataNum Double
  | DataLog Bool
  | DataLst [MData]
  | DataTup [MData]
  | DataRec [(Name, MData)]
  | DataStr String
  | DataFun Name [MData]
  | DataVar Name
  deriving(Show, Ord, Eq)

data MDecl
  = MDecl Name [String] MData 
  deriving(Show, Ord, Eq)

ifelse :: Bool -> a -> a -> a
ifelse True  x _ = x
ifelse False _ y = y

rdf2tree :: RDF -> Tree
rdf2tree (RDF i ts) = Node i [(r, f ts o) | (j,r,o) <- ts, j == i]
  where
    f :: [Triple] -> Object -> Tree
    f ts' (Id' j) = rdf2tree (RDF j ts')
    f _    o'     = Leaf o'

toOne :: [a] -> ThrowsError a
toOne [a] = Right a 
toOne _ = Left (InvalidRDF "Expected only one object for this relation")

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

getTypes :: Tree -> ThrowsError [MType]
getTypes = sequence . recursiveApply cond' f
  where
    f :: Tree -> ThrowsError MType
    f t = getRhs t >>= tree2mtype
    
    cond' :: Tree -> Bool
    cond' (Leaf _) = False
    cond' (Node _ xs) = any cond'' xs

    cond'' :: (Relation, Tree) -> Bool
    cond'' (":isa", Leaf (Str' ":typeDeclaration")) = True 
    cond'' _ = False

    getRhs :: Tree -> ThrowsError Tree
    getRhs (Node _ xs) = toOne [o | (r,o) <- xs, r == ":rhs"]
    getRhs _ = Left (InvalidRDF "Expected a node")

getChild :: Relation -> Tree -> [Tree]
getChild r (Node _ xs) = [t | (r', t) <- xs, r == r']
getChild _ _ = []

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
      <$> (getStr t' ":name" >>= toOne)
      <*> (toOne (getChild ":value" t') >>= tree2mtype)

    function' :: Tree -> ThrowsError MType
    function' t' = TypeFun
      <$> pure Nothing
      <*> (sequence . map tree2mtype $ getChild ":input" t')
      <*> ((sequence . map tree2mtype $ getChild ":output" t') >>= toOne)

    getStr :: Tree -> String -> ThrowsError [String]
    getStr t' s' = sequence . map getStr' $ getChild s' t'

    getStr' :: Tree -> ThrowsError String
    getStr' (Leaf (Str' s)) = Right s
    getStr' _ = Left (InvalidRDF ("Expected a string"))
    
    isa'  t' = getStr t' ":isa" >>= toOne
    val'  t' = getStr t' ":value" >>= toOne
