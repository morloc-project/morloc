module Morloc.Check (typecheck) where

import Morloc.Error
import Morloc.Data
import Morloc.Tree
import Morloc.Syntax

typecheck :: Program -> ThrowsError Program
typecheck p = do
  sequence (map (typecheck' (ontology p)) (workflow p))
  return p
    
  where

    typecheck' :: [(String, TNode)] -> Function WNode -> ThrowsError (Tree [Bool])
    typecheck' types (Function _ args tree) = sequence $ familyMap (foo types args) tree

    foo
      :: [(String, TNode)]   -- list of type signatures by name
      -> [String]            -- list of bound variables for this function
      -> WNode               -- the downstream node
      -> [WNode]             -- the input nodes
      -> ThrowsError [Bool]
    foo types vars (WNode _ n _) ws
      = case lookup n types of
        Nothing -> Right [] -- maybe?
        Just (TNodeType _)                     -> Right []
        Just (TNodeSignature _   Nothing    _) -> Right []
        Just (TNodeSignature ins (Just out) _) ->
          case sequence (map (wnode2otype types vars) ws) of
            Right outs -> sequence (bar n ins outs)
            Left err -> Left err
    foo _ _ _ _ = Right []

    wnode2otype :: [(String, TNode)] -> [String] -> WNode -> ThrowsError MType
    wnode2otype types vars (WNode _ n' _) = case lookup n' types of
      Nothing -> if   (elem n' vars)
                 then (Right MBound) -- is a variable passed to the function
                 else (Left . TypeError $ "Variable '" ++ n' ++ "' is not defined")
      (Just (TNodeType              m   )) -> Right m -- get constant value
      (Just (TNodeSignature _ (Just m) _)) -> Right m -- get function output
      (Just (TNodeSignature _ Nothing _))
        -> Left . TypeError $ "'" ++ n' ++ "' has no output"
    wnode2otype _ _ (WLeaf _ d   ) = mdata2type d

    -- TODO: This data to type conversion is VERY important to Morloc and needs
    -- a "place of honor". Also, it should be customizeable.
    mdata2type :: MData -> ThrowsError MType
    mdata2type (MInt _) = Right $ MSpecific "Int"  [] ""
    mdata2type (MNum _) = Right $ MSpecific "Num"  [] ""
    mdata2type (MLog _) = Right $ MSpecific "Bool" [] ""
    mdata2type (MLst []) = Left $ TypeError "Cannot infer type of empty list"
    mdata2type (MLst xs) = do
      ts <- sequence $ map mdata2type xs
      allEqual ts
      return $ MList (head ts) ""

    allEqual :: [MType] -> ThrowsError [MType]
    allEqual [] = Right []
    allEqual (x:xs) = case (and $ map ((==) x) xs) of
      True -> Right (x:xs)
      False -> Left . TypeError $
        "Heterogenous lists are not supported (try using a tuple or record)"

    bar :: String -> [MType] -> [MType] -> [ThrowsError Bool]
    bar n (i:is) (o:os) = (bar' n i o):(bar n is os)
    bar _ [] [] = [] -- empty is fine
    bar _ _  [] = [] -- more function arguments than inputs is fine
                   -- this represents partial application
    bar n [] _  = [Left . TypeError $ "'" ++ n ++ "' has too many inputs"]

    bar' :: String -> MType -> MType -> ThrowsError Bool
    bar' _ _ MBound = Right True
    bar' name input output
      | same input output = Right True
      | otherwise = Left $ TypeMismatch name (show input) (show output)

-- Test sameness of types. The default (==) doesn't work since I don't want
-- equality to depend on the type tag or local constraints (for now).
same
  :: MType -- input  type (to)
  -> MType -- output type (from)
  -> Bool
-- Here I autocast an int to a real
same (MSpecific "Num" _ _) (MSpecific "Int" _ _) = True
same (MSpecific n xs _) (MSpecific n' xs' _)
  =  n == n'          -- base types are the same
  && sameList xs xs'  -- parameters are the same
-- generic variables are equal so long as their parameters are equal (for now I
-- am ignoring constraints and do not have typeclasses, FIXME: this will need
-- to change
same (MGeneric  n xs _) (MGeneric  n' xs' _) = sameList xs xs' 
same (MList m  _) (MList  m' _) = same m m'
same (MTuple m _) (MTuple m' _) = sameList m m'
same (MRecord n ns _) (MRecord n' ns' _) = n == n' && same_ns ns ns' where
  same_ns :: [(String, MType)] -> [(String, MType)] -> Bool
  same_ns ((s,t):ns) ((s',t'):ns') = s == s' && same t t' && same_ns ns ns'
  same_ns [] [] = True
  same_ns _ _ = False
same MEmpty MEmpty = True -- TODO, should this be true?
same _ _ = False

sameList :: [MType] -> [MType] -> Bool
sameList (t:ts) (t':ts') = same t t' && sameList ts ts'
sameList [] [] = True
sameList _ _ = False
