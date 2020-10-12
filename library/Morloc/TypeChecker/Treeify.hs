{-|
Module      : Morloc.TypeChecker.Treeify
Description : I am groot
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.TypeChecker.Treeify (treeify) where

import Morloc.Namespace
import Morloc.Data.Doc
import Morloc.TypeChecker.PartialOrder
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.Data.DAG as MDD
import qualified Data.Map as Map
import qualified Data.Set as Set

data TermOrigin = Declared Expr | Sourced Source
  deriving(Show, Ord, Eq)

treeify
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> MorlocMonad [SAnno GMeta Many [CType]]
treeify d
  | Map.size d == 0 = return []
  | otherwise = case MDD.roots d of
    [] -> MM.throwError CyclicDependency
    (_:_) -> MM.throwError . CallTheMonkeys $ "How did you end up with so many roots?"
    [k] -> case MDD.lookupNode k d of
      Nothing -> MM.throwError . DagMissingKey . render $ pretty k
      (Just n) -> do
        -- initialize state counter to 0, used to index manifolds
        MM.startCounter
        mapM (collect d k n) (Set.toList (typedNodeExports n))

-- -- | Build the call tree for a single nexus command. The result is ambiguous,
-- -- with 1 or more possible tree topologies, each with one or more possible for
-- -- each function.
collect
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> MVar
  -> TypedNode
  -> EVar
  -> MorlocMonad (SAnno GMeta Many [CType])
collect d k n v = do
  -- Just look at one x, since any should emit the same GMeta (if not, then
  -- something is broken upstream of GMeta is not general enough)
  gmeta <- makeGMeta (Just v) (typedNodeTypeMap n) Nothing

  -- DAG MVar None (EVar, (TypedNode, [TermOrigin]))
  let termTree = MDD.lookupAliasedTerm v k (makeTermOrigin v) d

  -- DAG MVar None [(SExpr GMeta Many [CType], [CType])]
  sexprTree <- MDD.mapNodeM (\(v,(n,ts)) -> collectTerms d v n ts) termTree

  -- [(SExpr GMeta Many [CType], [CType])]
  let trees = concat . MDD.nodes $ sexprTree

  return $ SAnno (Many trees) gmeta


-- | Find info common across realizations of a given term in a given module
makeGMeta
  :: Maybe EVar
  -> Map.Map EVar TypeSet
  -> Maybe GType
  -> MorlocMonad GMeta
makeGMeta name typemap gtype = do
  i <- MM.getCounter
  case name >>= (flip Map.lookup) typemap of
    (Just (TypeSet (Just e) _)) -> do
      return $ GMeta
        { metaId = i
        , metaName = name
        , metaGType = maybe (Just . GType $ etype e) Just gtype
        , metaProperties = eprop e
        , metaConstraints = econs e
        }
    _ -> do
      return $ GMeta
        { metaId = i
        , metaName = name
        , metaGType = gtype
        , metaProperties = Set.empty
        , metaConstraints = Set.empty
        }


makeTermOrigin
  :: EVar
  -> TypedNode
  -> (TypedNode, [TermOrigin])
makeTermOrigin v n = (n, declared ++ sourced) where
  declared = [Declared e | (Declaration v' e) <- typedNodeBody n, v' == v]
  sourced = map Sourced
          $ filter (\s -> srcAlias s == v) (Map.elems $ typedNodeSourceMap n)


collectTerms
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> EVar
  -> TypedNode
  -> [TermOrigin]
  -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
collectTerms d v n ts = mapM (collectTerm d v n) ts


-- Notice that `args` is NOT an input to collectTerm. Morloc uses lexical
-- scoping, and the input to collectTerm is the origin of a term, so the
-- definition of the term is outside the scope of the parent expression.
collectTerm
  :: DAG MVar [(EVar, EVar)] TypedNode
  -> EVar
  -> TypedNode
  -> TermOrigin
  -> MorlocMonad (SExpr GMeta Many [CType], [CType])
collectTerm d v n (Declared (AnnE x ts)) = undefined
collectTerm _ _ _ (Declared _) = MM.throwError . GeneratorError $
  "Invalid expression in CollectTerm Declared, expected AnnE"
collectTerm _ v n (Sourced src) = undefined
