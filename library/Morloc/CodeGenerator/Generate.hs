{-|
Module      : Morloc.CodeGenerator.Generate
Description : Translate AST forests into target language source code
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

The single @generate@ function wraps the entire AST forest to source code
translation process.

The input the @generate@ is of type @[SAnno GMeta Many [CType]]@. The @SAnno
GMeta Many [CType]@ elements each represent a single command exported from the
main function. The @GMeta@ type stores all general information about a given
"manifold" (a node in the function graph and all its wrappings). The term
@Many@ states that there may be one of more AST describing each expression. The
term @[CType]@ states that there may be multiple concrete, language-specific
types associated with any term.

The @generate@ function converts the @SAnno GMeta Many [CType]@ types into
@SAnno GMeta One CType@ unambiguous ASTs. This step is an important
optimization step in the morloc build pipeline. Currently the compiler uses a
flat scoring matrix for the cost of interop between languages (e.g., 0 for C++
to C++, 1000 for anything to R, 5 for R to R since there is a function call
cost, etc). Replacing this algorithm with an empirically parameterized
performance model is a major goal.

Additional manipulations of the AST can reduce the number of required foreign
calls, (de)serialization calls, and duplicate computation.

The @SAnno GMeta One CType@ expression is ultimately translated into a simple
@ExprM@ type that is then passed to a language-specific translator.

-}

module Morloc.CodeGenerator.Generate
(
  generate
) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Internal
import Morloc.Data.Doc
import Morloc.Pretty (prettyType)
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as Lang
import qualified Morloc.Monad as MM
import Morloc.CodeGenerator.Grammars.Common
import qualified Morloc.CodeGenerator.Nexus as Nexus
import qualified Morloc.Module as Mod
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Morloc.CodeGenerator.Grammars.Translator.Cpp as Cpp
import qualified Morloc.CodeGenerator.Grammars.Translator.R as R
import qualified Morloc.CodeGenerator.Grammars.Translator.Python3 as Python3

-- | Translate typed, abstract syntax forests into compilable code
generate ::
  [SAnno GMeta Many [CType]]
  -- ^ one AST forest for each command exported from main
  -> MorlocMonad (Script, [Script]) 
  -- ^ the nexus code and the source code for each language pool
generate ms = do
  -- translate modules into bitrees
  (gASTs, rASTs)
    -- eliminate morloc composition abstractions
    <-  mapM rewrite ms
    -- select a single instance at each node in the tree
    >>= mapM realize   -- [Either (SAnno GMeta One CType) (SAnno GMeta One CType)]
    -- separate unrealized (general) ASTs (uASTs) from realized ASTs (rASTs)
    |>> partitionEithers

  -- Collect all call-free data
  gSerial <- mapM generalSerial gASTs

  -- build nexus
  -- -----------
  -- Each nexus subcommand calls one function from one one pool.
  -- The call passes the pool an index for the function (manifold) that will be called.
  nexus <- Nexus.generate
    gSerial
    [ (t, poolId m x, metaName m)
    | SAnno (One (x, t)) m <- rASTs
    ]

  -- find all sources files
  let srcs = unique . concat . conmap (unpackSAnno getSrcs) $ rASTs

  -- for each language, collect all functions into one "pool"
  pools
    -- thread arguments across the tree
    <- mapM parameterize rASTs
    -- convert from AST to manifold tree
    >>= mapM express
    -- rewrite lets to minimize the number of foreign calls
    >>= mapM letOptimize
    -- Separate the call trees into mono-lingual segments terminated in
    -- primitives or foreign calls.
    >>= mapM segment |>> concat
    -- Cast each call tree root as a manifold
    >>= mapM rehead
    -- Gather segments into pools, currently this entails gathering all
    -- segments from a given language into one pool. Later it may be more
    -- nuanced.
    >>= pool
    -- Generate the code for each pool
    >>= mapM (encode srcs)

  -- return the nexus script and each pool script
  return (nexus, pools)
  where
    -- map from nexus id to pool id
    -- these differ when a declared variable is exported
    poolId :: GMeta -> SExpr GMeta One TypeP -> Int
    poolId _ (LamS _ (SAnno _ meta)) = metaId meta
    poolId meta _ = metaId meta

    -- this is grossly inefficient ... but I'll deal with it later
    getSrcs :: SExpr GMeta One c -> GMeta -> c -> [Source]
    getSrcs (CallS src) g _ = src : getSrcsFromGmeta g
    getSrcs _ g _ = getSrcsFromGmeta g

    getSrcsFromGmeta :: GMeta -> [Source]
    getSrcsFromGmeta g
      = concat [unresolvedPackerForward p ++ unresolvedPackerReverse p
               | p <- (concat . Map.elems . metaPackers) g]
      ++ Map.elems (metaConstructors g)


-- | Eliminate morloc function calls
-- For example:
--    foo x y = bar x (baz y)
--    bar x y = add x y
--    baz x = div x 5
-- Can be rewritten as:
--    foo x y = add x (div y 5)
-- Notice that no morloc abstractions appear on the right hand side.
rewrite
  :: SAnno GMeta Many [CType]
  -> MorlocMonad (SAnno GMeta Many [CType])
rewrite (SAnno (Many es0) g0) = do
  es0' <- concat <$> mapM rewriteL0 es0
  return $ SAnno (Many es0') g0
  where
    rewriteL0
      :: (SExpr GMeta Many [CType], [CType])
      -> MorlocMonad [(SExpr GMeta Many [CType], [CType])]
    rewriteL0 (AppS (SAnno (Many es1) g1) args, c1) = do
      args' <- mapM rewrite args
      -- originally es1 consists of a list of CallS and LamS constructors
      --  - CallS are irreducible source functions
      --  - LamS are Morloc abstractions that can be reduced
      -- separate LamS expressions from all others
      let (es1LamS, es1CallS) = partitionEithers (map sepLamS es1)
      -- rewrite the LamS expressions, each expression will yields 1 or more
      es1LamS' <- concat <$> mapM (rewriteL1 args') es1LamS
      return $ (AppS (SAnno (Many es1CallS) g1) args', c1) : es1LamS'
      where
        sepLamS
          :: (SExpr g Many c, c)
          -> Either ([EVar], SAnno g Many c)
                    (SExpr g Many c, c)
        sepLamS (LamS vs body, _) = Left (vs, body)
        sepLamS x = Right x
    rewriteL0 (AccS x k, c) = do
      x' <- rewrite x
      return [(AccS x' k, c)]
    rewriteL0 (ListS xs, c) = do
      xs' <- mapM rewrite xs
      return [(ListS xs', c)]
    rewriteL0 (TupleS xs, c) = do
      xs' <- mapM rewrite xs
      return [(TupleS xs', c)]
    rewriteL0 (RecS entries, c) = do
      xs' <- mapM (rewrite . snd) entries
      return [(RecS $ zip (map fst entries) xs', c)]
    rewriteL0 (LamS vs x, c) = do
      x' <- rewrite x
      return [(LamS vs x', c)]
    -- VarS UniS NumS LogS StrS CallS
    rewriteL0 x = return [x]

    rewriteL1
      :: [SAnno g Many c]
      -> ([EVar], SAnno g Many c) -- lambda variables and body
      -> MorlocMonad [(SExpr g Many c, c)]
    rewriteL1 args (vs, SAnno (Many es2) _)
      | length vs == length args =
          concat <$> mapM (substituteExprs (zip vs args)) es2
      | length vs > length args = MM.throwError . NotImplemented $
          "Partial function application not yet implemented (coming soon)"
      | length vs < length args = MM.throwError . TypeError $
          "Type error: too many arguments applied to lambda"
    rewriteL1 _ (_, SAnno (Many _) _) = error "GHC warnings tell me this is a missing case, but why?"


    substituteExprs
      :: [(EVar, SAnno g Many c)]
      -> (SExpr g Many c, c) -- body
      -> MorlocMonad [(SExpr g Many c, c)] -- substituted bodies
    substituteExprs [] x = return [x]
    substituteExprs ((v, r):rs) x = do
      xs' <- substituteExpr v r x
      concat <$> mapM (substituteExprs rs) xs'

    substituteExpr
      :: EVar
      -> SAnno g Many c -- replacement
      -> (SExpr g Many c, c) -- expression
      -> MorlocMonad [(SExpr g Many c, c)]
    substituteExpr v (SAnno (Many xs) _) x@(VarS v', _)
      | v == v' = return xs
      | otherwise = return [x]
    substituteExpr v r (AccS x k, c) = do
      x' <- substituteAnno v r x
      return [(AccS x' k, c)]
    substituteExpr v r (ListS xs, c) = do
      xs' <- mapM (substituteAnno v r) xs
      return [(ListS xs', c)]
    substituteExpr v r (TupleS xs, c) = do
      xs' <- mapM (substituteAnno v r) xs
      return [(TupleS xs', c)]
    substituteExpr v r (RecS entries, c) = do
      xs' <- mapM (substituteAnno v r . snd) entries
      return [(RecS (zip (map fst entries) xs'), c)]
    substituteExpr v r (LamS vs x, c) = do
      x' <- substituteAnno v r x
      return [(LamS vs x', c)]
    substituteExpr v r (AppS f xs, c) = do
      f' <- substituteAnno v r f
      xs' <- mapM (substituteAnno v r) xs
      return [(AppS f' xs', c)]
    -- UniS NumS LogS StrS CallS
    substituteExpr _ _ x = return [x]

    substituteAnno
      :: EVar -- variable to replace
      -> SAnno g Many c -- replacement branch set
      -> SAnno g Many c -- search branch
      -> MorlocMonad (SAnno g Many c)
    substituteAnno v r (SAnno (Many xs) g) = do
      xs' <- concat <$> mapM (substituteExpr v r) xs
      return $ SAnno (Many xs') g

-- | Select a single concrete language for each sub-expression.  Store the
-- concrete type and the general type (if available).  Select pack/unpack
-- functions.
realize
  :: SAnno GMeta Many [CType]
  -> MorlocMonad (Either (SAnno GMeta One ()) (SAnno GMeta One TypeP))
realize x0 = do
  -- MM.say $ " --- realize ---"
  -- MM.say $ prettySAnnoMany x
  -- MM.say $ " ---------------"
  realizationMay <- realizeAnno 0 Nothing x0
  case realizationMay of
    Nothing -> makeGAST x0 |>> Left
    (Just (_, realization)) -> do
       mapGCM weaveTypesGCP realization >>= rewritePartials |>> Right
  where
    realizeAnno
      :: Int
      -> Maybe Lang
      -> SAnno GMeta Many [CType]
      -> MorlocMonad (Maybe (Int, SAnno GMeta One CType))
    realizeAnno depth langMay (SAnno (Many xs) m) = do
      asts <- mapM (\(x, cs) -> mapM (realizeExpr (depth+1) langMay x) cs) xs |>> concat
      case minimumOnMay (\(s,_,_) -> s) (catMaybes asts) of
        Just (i, x, c) -> do
          return $ Just (i, SAnno (One (x, c)) m)
        Nothing -> do
          return Nothing

    realizeExpr
      :: Int
      -> Maybe Lang
      -> SExpr GMeta Many [CType]
      -> CType
      -> MorlocMonad (Maybe (Int, SExpr GMeta One CType, CType))
    realizeExpr depth lang x c = do
      let lang' = if isJust lang then lang else langOf c
      realizeExpr' depth lang' x c

    realizeExpr'
      :: Int
      -> Maybe Lang
      -> SExpr GMeta Many [CType]
      -> CType
      -> MorlocMonad (Maybe (Int, SExpr GMeta One CType, CType))
    -- always choose the primitive that is in the same language as the parent
    realizeExpr' _ lang UniS c
      | lang == langOf c = return $ Just (0, UniS, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (NumS x) c
      | lang == langOf c = return $ Just (0, NumS x, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (LogS x) c
      | lang == langOf c = return $ Just (0, LogS x, c)
      | otherwise = return Nothing
    realizeExpr' _ lang (StrS x) c
      | lang == langOf c = return $ Just (0, StrS x, c)
      | otherwise = return Nothing
    -- Q: a call should also be of the same language as the parent, shouldn't it?
    -- A: not necessarily, specifically if the parent includes many child calls, say in a list
    realizeExpr' _ lang (CallS src) c
      -- FIXME: assuming function calls have 0 cost is perhaps not realistic
      | lang == langOf c = return $ Just (0, CallS src, c)
      | otherwise = return Nothing
    -- and a var?
    realizeExpr' _ lang (VarS x) c
      | lang == langOf c = return $ Just (0, VarS x, c)
      | otherwise = return Nothing
    realizeExpr' depth lang (AccS x k) c
      | lang == langOf c = do
        xMay <- realizeAnno depth lang x
        case xMay of
          Nothing -> return Nothing
          (Just (i, x')) -> return $ Just (i, AccS x' k, c)
      | otherwise = return Nothing
    -- simple recursion into ListS, TupleS, and RecS
    realizeExpr' depth lang (ListS xs) c
      | lang == langOf c = do
        xsMay <- mapM (realizeAnno depth lang) xs
        case (fmap unzip . sequence) xsMay of
          (Just (scores, xs')) -> return $ Just (sum scores, ListS xs', c)
          Nothing -> return Nothing
      | otherwise = return Nothing
    realizeExpr' depth lang (TupleS xs) c
      | lang == langOf c = do
        xsMay <- mapM (realizeAnno depth lang) xs
        case (fmap unzip . sequence) xsMay of
          (Just (scores, xs')) -> return $ Just (sum scores, TupleS xs', c)
          Nothing -> return Nothing
      | otherwise = return Nothing
    realizeExpr' depth lang (RecS entries) c
      | lang == langOf c = do
          xsMay <- mapM (realizeAnno depth lang . snd) entries
          case (fmap unzip . sequence) xsMay of
            (Just (scores, vals)) -> return $ Just (sum scores, RecS (zip (map fst entries) vals), c)
            Nothing -> return Nothing
      | otherwise = return Nothing
    --
    realizeExpr' depth _ (LamS vs x) c = do
      xMay <- realizeAnno depth (langOf c) x
      case xMay of
        (Just (score, x')) -> return $ Just (score, LamS vs x', c)
        Nothing -> return Nothing
    -- AppS
    realizeExpr' _ Nothing _ _ = MM.throwError . OtherError $ "Expected concrete type"
    realizeExpr' depth (Just lang) (AppS f xs) c = do
      let lang' = (fromJust . langOf) c 
      fMay <- realizeAnno depth (Just lang') f
      xsMay <- mapM (realizeAnno depth (Just lang')) xs
      case (fMay, (fmap unzip . sequence) xsMay, Lang.pairwiseCost lang lang') of
        (Just (fscore, f'), Just (scores, xs'), Just interopCost) ->
          return $ Just (fscore + sum scores + interopCost, AppS f' xs', c)
        _ -> return Nothing


-- | This function is called on trees that contain no language-specific
-- components.  "GAST" refers to General Abstract Syntax Tree. The most common
-- GAST case, and the only one that is currently supported, is a expression
-- that merely rearranges data structures without calling any functions. Here
-- are a few examples:
--
--  Constant values and containters (currently supported):
--  f1 = 5
--  f2 = [1,2,3]
--
--  Variable values and containers (coming soon):
--  f3 x = x
--
--  f4 x = [1,2,x]
--
--  Combinations of transformations on containers (possible, but not coming soon):
--  f5 :: forall a b . (a, b) -> (b, a)
--  f6 (x,y) = (y,x)
--
-- The idea could be elaborated into a full-fledged language.
makeGAST :: SAnno GMeta Many [CType] -> MorlocMonad (SAnno GMeta One ())
makeGAST (SAnno (Many []) m) = case metaGType m of
  (Just (GType t)) -> MM.throwError . CallTheMonkeys . render
    $ "Cannot build general value from type" <+> dquotes (prettyType t)
  Nothing -> MM.throwError . CallTheMonkeys . render
          $ "Cannot build general value from type."
          <+> "You probably tried to build a module that is meant to be imported."
makeGAST (SAnno (Many ((UniS, _):_)) m) = return (SAnno (One (UniS, ())) m)
makeGAST (SAnno (Many ((VarS x, _):_)) m) = return (SAnno (One (VarS x, ())) m)
makeGAST (SAnno (Many ((NumS x, _):_)) m) = return (SAnno (One (NumS x, ())) m)
makeGAST (SAnno (Many ((LogS x, _):_)) m) = return (SAnno (One (LogS x, ())) m)
makeGAST (SAnno (Many ((StrS x, _):_)) m) = return (SAnno (One (StrS x, ())) m)
makeGAST (SAnno (Many ((AccS x k, _):_)) m) = do
  x' <- makeGAST x
  return (SAnno (One (AccS x' k, ())) m)
makeGAST (SAnno (Many ((ListS ss, _):_)) m) = do
  ss' <- mapM makeGAST ss
  return $ SAnno (One (ListS ss', ())) m
makeGAST (SAnno (Many ((TupleS ss, _):_)) m) = do
  ss' <- mapM makeGAST ss
  return $ SAnno (One (TupleS ss', ())) m
makeGAST (SAnno (Many ((LamS vs s, _):_)) m) = do
  s' <- makeGAST s
  return $ SAnno (One (LamS vs s', ())) m
makeGAST (SAnno (Many ((AppS f xs, _):_)) m) = do
  f' <- makeGAST f
  xs' <- mapM makeGAST xs
  return $ SAnno (One (AppS f' xs', ())) m
makeGAST (SAnno (Many ((RecS es, _):_)) m) = do
  vs <- mapM (makeGAST . snd) es
  return $ SAnno (One (RecS (zip (map fst es) vs), ())) m
makeGAST (SAnno (Many ((CallS src, _):_)) _)
  = MM.throwError . OtherError . render
  $ "Function calls cannot be used in general code:" <+> pretty (srcName src)


-- | Serialize a simple, general data type. This type can consists only of JSON
-- primitives and containers (lists, tuples, and records) and accessors.
generalSerial :: SAnno GMeta One () -> MorlocMonad NexusCommand
generalSerial (SAnno _ GMeta{metaName = Nothing})
  = MM.throwError . OtherError $ "No general type found for call-free function"
generalSerial (SAnno _ GMeta{metaGType = Nothing})
  = MM.throwError . OtherError $ "No name found for call-free function"
generalSerial x0@(SAnno _ GMeta{ metaName = Just subcmd
                               , metaGType = Just (GType cmdtype)}) = generalSerial' [] x0
  where
    base = NexusCommand subcmd cmdtype (dquotes "_") [] []

    generalSerial' :: JsonPath -> SAnno GMeta One () -> MorlocMonad NexusCommand
    generalSerial' _ (SAnno (One (UniS,   _)) _)
      = return $ base { commandJson = "null" }
    generalSerial' _ (SAnno (One (NumS x, _)) _)
      = return $ base { commandJson = viaShow x }
    generalSerial' _ (SAnno (One (LogS x, _)) _)
      = return $ base { commandJson = if x then "true" else "false" }
    generalSerial' _ (SAnno (One (StrS x, _)) _)
      = return $ base { commandJson = dquotes (pretty x) }
    -- if a nested accessor is observed, evaluate the nested expression and
    -- append the path 
    generalSerial' ps (SAnno (One (AccS x@(SAnno (One (AccS _ _, _)) _) k, _)) _) = do
      ncmd <- generalSerial' ps x
      case commandSubs ncmd of
        [(ps1, arg, ps2)] ->
          return $ ncmd { commandSubs = [(ps1, arg, JsonKey (unEVar k) : ps2)] }
        _ -> error "Bad record access"
    -- record the path to and from a record access, leave the value as null, it
    -- will be set in the nexus
    generalSerial' ps (SAnno (One (AccS (SAnno (One (VarS v, _)) g) k, _)) _) =
      case g of
        (metaGType->(Just (GType (NamT {})))) ->
          return $ base { commandSubs = [(ps, unEVar v, [JsonKey (unEVar k)])] }
        _ -> error "Attempted to use key access to non-record"
    generalSerial' ps (SAnno (One (ListS xs, _)) _) = do
      ncmds <- zipWithM generalSerial'
                        [ps ++ [JsonIndex i] | i <- [0..]] xs
      return $ base 
        { commandJson = list (map commandJson ncmds)
        , commandSubs = conmap commandSubs ncmds
        }
    generalSerial' ps (SAnno (One (TupleS xs, _)) _) = do
      ncmds <- zipWithM generalSerial'
                        [ps ++ [JsonIndex i] | i <- [0..]] xs
      return $ base
        { commandJson = list (map commandJson ncmds)
        , commandSubs = conmap commandSubs ncmds
        }
    generalSerial' ps (SAnno (One (RecS es, _)) _) = do
      ncmds <- zipWithM generalSerial'
                        [ps ++ [JsonKey (unEVar k)] | k <- map fst es]
                        (map snd es)
      let entries = zip (map fst es) (map commandJson ncmds)
          obj = encloseSep "{" "}" ","
                (map (\(k, v) -> dquotes (pretty k) <> ":" <> v) entries)
      return $ base
        { commandJson = obj
        , commandSubs = conmap commandSubs ncmds
        }
    generalSerial' ps (SAnno (One (LamS vs x, _)) _) = do
      ncmd <- generalSerial' ps x
      return $ ncmd { commandArgs = vs }
    generalSerial' ps (SAnno (One (VarS (EVar v), _)) _) =
      return $ base { commandSubs = [(ps, v, [])] }
    generalSerial' _ (SAnno (One _) m) = do
      MM.throwError . OtherError . render $
        "Cannot serialize general type:" <+> prettyType (fromJust $ metaGType m)

rewritePartials
  :: SAnno GMeta One TypeP
  -> MorlocMonad (SAnno GMeta One TypeP)
rewritePartials (SAnno (One (AppS f xs, ftype@(FunP _ _))) m) = do
  let gTypeArgs = maybe (repeat Nothing) (map Just . decomposeFull) (metaGType m)
  f' <- rewritePartials f
  xs' <- mapM rewritePartials xs
  lamGType <- makeGType $ [metaGType g | (SAnno _ g) <- xs'] ++ gTypeArgs
  let vs = map EVar . take (nargs ftype) $ freshVarsAZ [] -- TODO: exclude existing arguments
      ys = zipWith3 makeVar vs (decomposeFull ftype) gTypeArgs
      -- unsafe, but should not fail for well-typed input
      appType = last . decomposeFull $ ftype
      appMeta = m {metaGType = metaGType m >>= (last . map Just . decomposeFull)}
      lamMeta = m {metaGType = Just lamGType}
      lamCType = ftype

  return $ SAnno (One (LamS vs (SAnno (One (AppS f' (xs' ++ ys), appType)) appMeta), lamCType)) lamMeta
  where
    makeGType :: [Maybe GType] -> MorlocMonad GType
    makeGType ts = fmap GType . makeType . map (unGType . fromJust) $ ts

    -- make an sanno variable from variable name and type info
    makeVar :: EVar -> TypeP -> Maybe GType -> SAnno GMeta One TypeP
    makeVar v c g = SAnno (One (VarS v, c))
      ( m { metaGType = g
          , metaName = Nothing
          , metaProperties = Set.empty
          , metaConstraints = Set.empty
          }
      )
-- apply the pattern above down the AST
rewritePartials (SAnno (One (AppS f xs, t)) m) = do
  xs' <- mapM rewritePartials xs
  f' <- rewritePartials f
  return $ SAnno (One (AppS f' xs', t)) m
rewritePartials (SAnno (One (LamS vs x, t)) m) = do
  x' <- rewritePartials x
  return $ SAnno (One (LamS vs x', t)) m
rewritePartials (SAnno (One (AccS x k, t)) m) = do
  x' <- rewritePartials x
  return $ SAnno (One (AccS x' k, t)) m
rewritePartials (SAnno (One (ListS xs, t)) m) = do
  xs' <- mapM rewritePartials xs
  return $ SAnno (One (ListS xs', t)) m
rewritePartials (SAnno (One (TupleS xs, t)) m) = do
  xs' <- mapM rewritePartials xs
  return $ SAnno (One (TupleS xs', t)) m
rewritePartials (SAnno (One (RecS entries, t)) m) = do
  let keys = map fst entries
  vals <- mapM (rewritePartials . snd) entries
  return $ SAnno (One (RecS (zip keys vals), t)) m
rewritePartials x = return x

-- | Add arguments that are required for each term. Unneeded arguments are
-- removed at each step.
parameterize
  :: SAnno GMeta One TypeP
  -> MorlocMonad (SAnno GMeta One (TypeP, [(EVar, Argument)]))
parameterize (SAnno (One (LamS vs x, t)) m) = do
  let args0 = zip vs $ zipWith makeArgument [0..] (decomposeFull t)
  x' <- parameterize' args0 x
  return $ SAnno (One (LamS vs x', (t, args0))) m
parameterize (SAnno (One (CallS src, t)) m) = do
  let ts = init . decomposeFull $ t
      vs = map EVar (freshVarsAZ [])
      args0 = zipWith makeArgument [0..] ts
  return $ SAnno (One (CallS src, (t, zip vs args0))) m
parameterize x = parameterize' [] x

-- TODO: the arguments coupled to every term should be the arguments USED
-- (not inherited) by the term. I need to ensure the argument threading
-- leads to correct passing of serialized/unserialized arguments. AppS should
-- "know" that it needs to deserialize functions that are passed to a foreign
-- call, for instance.
parameterize'
  :: [(EVar, Argument)] -- arguments in parental scope (child needn't retain them)
  -> SAnno GMeta One TypeP
  -> MorlocMonad (SAnno GMeta One (TypeP, [(EVar, Argument)]))
-- primitives, no arguments are required for a primitive, so empty lists
parameterize' _ (SAnno (One (UniS, c)) m) = return $ SAnno (One (UniS, (c, []))) m
parameterize' _ (SAnno (One (NumS x, c)) m) = return $ SAnno (One (NumS x, (c, []))) m
parameterize' _ (SAnno (One (LogS x, c)) m) = return $ SAnno (One (LogS x, (c, []))) m
parameterize' _ (SAnno (One (StrS x, c)) m) = return $ SAnno (One (StrS x, (c, []))) m
-- VarS EVar
parameterize' args (SAnno (One (VarS v, c)) m) = do
  let args' = [(v', r) | (v', r) <- args, v' == v]
  return $ SAnno (One (VarS v, (c, args'))) m
-- CallS Source
parameterize' _ (SAnno (One (CallS src, c)) m) = do
  return $ SAnno (One (CallS src, (c, []))) m
-- record access
parameterize' args (SAnno (One (AccS x k, c)) m) = do
  x' <- parameterize' args x
  return $ SAnno (One (AccS x' k, (c, args))) m
-- containers
parameterize' args (SAnno (One (ListS xs, c)) m) = do
  xs' <- mapM (parameterize' args) xs
  let usedArgs = map fst . unique . concatMap sannoSnd $ xs'
      args' = [(v, r) | (v, r) <- args, elem v usedArgs] 
  return $ SAnno (One (ListS xs', (c, args'))) m
parameterize' args (SAnno (One (TupleS xs, c)) m) = do
  xs' <- mapM (parameterize' args) xs
  let usedArgs = map fst . unique . concatMap sannoSnd $ xs'
      args' = [(v, r) | (v, r) <- args, elem v usedArgs] 
  return $ SAnno (One (TupleS xs', (c, args'))) m
parameterize' args (SAnno (One (RecS entries, c)) m) = do
  vs' <- mapM (parameterize' args . snd) entries
  let usedArgs = map fst . unique . concatMap sannoSnd $ vs'
      args' = [(v, r) | (v, r) <- args, elem v usedArgs] 
  return $ SAnno (One (RecS (zip (map fst entries) vs'), (c, args'))) m
parameterize' args (SAnno (One (LamS vs x, c)) m) = do
  let args' = [(v, r) | (v, r) <- args, notElem v vs]
      startId = maximum (map (argId . snd) args) + 1
      args0 = zip vs $ map unpackArgument $ zipWith makeArgument [startId..] (decomposeFull c)
  x' <- parameterize' (args' ++ args0) x
  return $ SAnno (One (LamS vs x', (c, args'))) m
parameterize' args (SAnno (One (AppS x xs, c)) m) = do
  x' <- parameterize' args x
  xs' <- mapM (parameterize' args) xs
  let usedArgs = map fst $ sannoSnd x' ++ (unique . concatMap sannoSnd $ xs')
      args' = [(v, r) | (v, r) <- args, elem v usedArgs] 
  return $ SAnno (One (AppS x' xs', (c, args'))) m

makeArgument :: Int -> TypeP -> Argument
makeArgument i (UnkP _) = PassThroughArgument i
makeArgument i t = SerialArgument i t


-- convert from unambiguous tree to non-segmented ExprM
express :: SAnno GMeta One (TypeP, [(EVar, Argument)]) -> MorlocMonad (ExprM Many)
express s0@(SAnno (One (_, (c0, _))) _) = express' True c0 s0 where

  express' :: Bool -> TypeP -> SAnno GMeta One (TypeP, [(EVar, Argument)]) -> MorlocMonad (ExprM Many)

  -- primitives
  express' _ _ (SAnno (One (NumS x, (c, _))) _) = return $ NumM (Native c) x
  express' _ _ (SAnno (One (LogS x, (c, _))) _) = return $ LogM (Native c) x
  express' _ _ (SAnno (One (StrS x, (c, _))) _) = return $ StrM (Native c) x
  express' _ _ (SAnno (One (UniS, (c, _))) _) = return $ NullM (Native c)

  -- record access
  express' isTop pc (SAnno (One (AccS x k, _)) m) = do
    x' <- express' isTop pc x >>= unpackExprM m
    return (AccM x' k)

  -- containers
  express' isTop _ (SAnno (One (ListS xs, (c@(ArrP _ [t]), args))) m) = do
    xs' <- mapM (express' False t) xs >>= mapM (unpackExprM m)
    let x = ListM (Native c) xs'
    if isTop
      then do
        x' <- packExprM m x
        return $ ManifoldM m (map snd args) (ReturnM x')
      else return x
  express' _ _ (SAnno (One (ListS _, _)) _) = MM.throwError . CallTheMonkeys $ "ListS can only be ArrP type"

  express' isTop _ (SAnno (One (TupleS xs, (c@(ArrP _ ts), args))) m) = do
    xs' <- zipWithM (express' False) ts xs >>= mapM (unpackExprM m)
    let x = TupleM (Native c) xs'
    if isTop
      then do
        x' <- packExprM m x
        return $ ManifoldM m (map snd args) (ReturnM x')
      else return x

  express' isTop _ (SAnno (One (RecS entries, (c@(NamP _ _ _ rs), args))) m) = do
    xs' <- zipWithM (express' False) (map snd rs) (map snd entries) >>= mapM (unpackExprM m)
    let x = RecordM (Native c) (zip (map fst entries) xs')
    if isTop
      then do
        x' <- packExprM m x
        return $ ManifoldM m (map snd args) (ReturnM x')
      else return x

  -- lambda
  express' isTop _ (SAnno (One (LamS _ x@(SAnno (One (_, (c,_))) _), _)) _) = express' isTop c x

  -- var
  express' _ _ (SAnno (One (VarS v, (c, rs))) _) =
    case [r | (v', r) <- rs, v == v'] of
      [r] -> case r of
        (SerialArgument i _) -> return $ BndVarM (Serial c) i
        (NativeArgument i _) -> return $ BndVarM (Native c) i
        -- NOT passthrough, since it doesn't
        -- After segmentation, this type will be used to resolve passthroughs everywhere
        (PassThroughArgument i) -> return $ BndVarM (Serial c) i
      _ -> MM.throwError . OtherError $ "Expected VarS to match exactly one argument"

  -- Apply arguments to a sourced function
  -- The CallS object may be in a foreign language. These inter-language
  -- connections will be snapped apart in the segment step.
  express' _ pc (SAnno (One (AppS (SAnno (One (CallS src, (fc, _))) _) xs, (_, args))) m)
    -- case #1
    | sameLanguage && fullyApplied = do
        xs' <- zipWithM (express' False) inputs xs >>= mapM (unpackExprM m)
        return . ManifoldM m (map snd args) $
          ReturnM (AppM f xs')

    -- case #2
    | sameLanguage && not fullyApplied = do
        xs' <- zipWithM (express' False) inputs xs >>= mapM (unpackExprM m)
        let startId = maximum (map (argId . snd) args) + 1
            lambdaTypes = drop (length xs) (map typeP2typeM inputs)
            lambdaArgs = zipWith NativeArgument [startId ..] inputs
            lambdaVals = zipWith BndVarM          lambdaTypes [startId ..]
        return . ManifoldM m (map snd args) $
          ReturnM (LamM lambdaArgs (AppM f (xs' ++ lambdaVals)))

    -- case #3
    | not sameLanguage && fullyApplied = do
          xs' <- zipWithM (express' False) inputs xs >>= mapM (unpackExprM m)
          return . ForeignInterfaceM (packTypeM (typeP2typeM pc)) . ManifoldM m (map snd args) $
            ReturnM (AppM f xs')

    -- case #4
    | not sameLanguage && not fullyApplied = do
        xs' <- zipWithM (express' False) inputs xs >>= mapM (unpackExprM m)
        let startId = maximum (map (argId . snd) args) + 1
            lambdaTypes = drop (length xs) (map typeP2typeM inputs)
            lambdaArgs = zipWith NativeArgument [startId ..] inputs
            lambdaVals = zipWith BndVarM lambdaTypes [startId ..]
        return . ForeignInterfaceM (packTypeM (typeP2typeM pc))
               . ManifoldM m (map snd args)
               $ ReturnM (LamM lambdaArgs (AppM f (xs' ++ lambdaVals)))
    where
      (inputs, _) = decompose fc
      sameLanguage = langOf pc == langOf fc
      fullyApplied = length inputs == length xs
      f = SrcM (typeP2typeM fc) src

  -- CallS - direct export of a sourced function, e.g.:
  express' True _ (SAnno (One (CallS src, (c, _))) m) = do
    let (inputs, _) = decompose c
        lambdaArgs = zipWith SerialArgument [0 ..] inputs
        lambdaTypes = map (packTypeM . typeP2typeM) inputs
        f = SrcM (typeP2typeM c) src
    lambdaVals <- mapM (unpackExprM m) $ zipWith BndVarM lambdaTypes [0 ..]
    return $ ManifoldM m lambdaArgs (ReturnM $ AppM f lambdaVals)

  -- An un-applied source call
  express' False pc (SAnno (One (CallS src, (c, _))) m) = do
    let (inputs, _) = decompose c
        lambdaTypes = map typeP2typeM inputs
        lambdaArgs = zipWith NativeArgument [0 ..] inputs
        lambdaVals = zipWith BndVarM lambdaTypes [0 ..]
        f = SrcM (typeP2typeM c) src
        manifold = ManifoldM m lambdaArgs (ReturnM $ AppM f lambdaVals)

    if langOf pc == langOf c
      then return manifold
      else return $ ForeignInterfaceM (typeP2typeM pc) manifold

  express' _ _ (SAnno (One (_, (t, _))) m) = MM.throwError . CallTheMonkeys . render $
    "Invalid input to express' in module (" <> viaShow (metaName m) <> ") - type: " <> prettyTypeP t

-- | Move let assignments to minimize number of foreign calls.  This step
-- should be integrated with the optimizations performed in the realize step.
-- FIXME: replace stub
letOptimize :: ExprM Many -> MorlocMonad (ExprM Many)
letOptimize = return

segment :: ExprM Many -> MorlocMonad [ExprM Many]
segment e0
  = segment' (gmetaOf e0) (argsOf e0) e0
  |>> (\(ms,e) -> e:ms)
  |>> map reparameterize where

  -- This is where segmentation happens, every other match is just traversal
  segment' _ args (ForeignInterfaceM t e@(ManifoldM m args' _)) = do
    (ms, e') <- segment' m args' e
    config <- MM.ask
    case MC.buildPoolCallBase config (langOf e') (metaId m) of
      (Just cmds) -> return (e':ms, PoolCallM (packTypeM t) (metaId m) cmds args)
      Nothing -> MM.throwError . OtherError $ "Unsupported language: " <> MT.show' (langOf e')

  segment' m args (SerializeM _ (AppM e@(ForeignInterfaceM _ _) es)) = do
    (ms, e') <- segment' m args e
    (mss, es') <- mapM (segment' m args) es |>> unzip
    es'' <- mapM (packExprM m) es'
    return (ms ++ concat mss, AppM e' es'')

  segment' _ _ (ManifoldM m args e) = do
    (ms, e') <- segment' m args e
    return (ms, ManifoldM m args e')

  segment' m args (AppM e es) = do
    (ms, e') <- segment' m args e
    (mss, es') <- mapM (segment' m args) es |>> unzip
    return (ms ++ concat mss, AppM e' es')

  segment' m args0 (LamM args1 e) = do
    (ms, e') <- segment' m (args0 ++ args1) e
    return (ms, LamM args1 e')

  segment' m args (LetM i e1 e2) = do
    (ms1, e1') <- segment' m args e1
    (ms2, e2') <- segment' m args e2
    return (ms1 ++ ms2, LetM i e1' e2')

  segment' m args (AccM e k) = do
    (ms, e') <- segment' m args e
    return (ms, AccM e' k)

  segment' m args (ListM t es) = do
    (mss, es') <- mapM (segment' m args) es |>> unzip
    return (concat mss, ListM t es')

  segment' m args (TupleM t es) = do
    (mss, es') <- mapM (segment' m args) es |>> unzip
    return (concat mss, TupleM t es')

  segment' m args (RecordM t entries) = do
    (mss, es') <- mapM (segment' m args . snd) entries |>> unzip
    return (concat mss, RecordM t (zip (map fst entries) es'))

  segment' m args (SerializeM s e) = do
    (ms, e') <- segment' m args e
    return (ms, SerializeM s e')

  segment' m args (DeserializeM s e) = do
    (ms, e') <- segment' m args e
    return (ms, DeserializeM s e')

  segment' m args (ReturnM e) = do
    (ms, e') <- segment' m args e
    return (ms, ReturnM e')

  segment' _ _ e = return ([], e)

-- Now that the AST is segmented by language, we can resolve passed-through
-- arguments where possible.
reparameterize :: (ExprM Many) -> (ExprM Many)
reparameterize e0 = snd (substituteBndArgs e0) where 
  substituteBndArgs :: (ExprM Many) -> ([(Int, TypeM)], ExprM Many) 
  substituteBndArgs (ForeignInterfaceM i e) =
    let (vs, e') = substituteBndArgs e
    in (vs, ForeignInterfaceM i (snd $ substituteBndArgs e'))
  substituteBndArgs (ManifoldM m args e) =
    let (vs, e') = substituteBndArgs e
    in (vs, ManifoldM m (map (sub vs) args) e')
  substituteBndArgs (AppM e es) =
    let (vs, e') = substituteBndArgs e
        (vss, es') = unzip $ map substituteBndArgs es
    in (vs ++ concat vss, AppM e' es')
  substituteBndArgs (LamM args e) =
    let (vs, e') = substituteBndArgs e
    in (vs, LamM (map (sub vs) args) e')
  substituteBndArgs (LetM i e1 e2) =
    let (vs1, e1') = substituteBndArgs e1
        (vs2, e2') = substituteBndArgs e2
    in (vs1 ++ vs2, LetM i e1' e2')
  substituteBndArgs (AccM e k) =
    let (vs, e') = substituteBndArgs e
    in (vs, AccM e' k)
  substituteBndArgs (ListM t es) =
    let (vss, es') = unzip $ map substituteBndArgs es
    in (concat vss, ListM t es')
  substituteBndArgs (TupleM t es) =
    let (vss, es') = unzip $ map substituteBndArgs es
    in (concat vss, TupleM t es')
  substituteBndArgs (RecordM t entries) =
    let (vss, es') = unzip $ map substituteBndArgs (map snd entries)
    in (concat vss, RecordM t (zip (map fst entries) es'))
  substituteBndArgs (SerializeM s e) =
    let (vs, e') = substituteBndArgs e
    in (vs, SerializeM s e')
  substituteBndArgs (DeserializeM s e) =
    let (vs, e') = substituteBndArgs e
    in (vs, DeserializeM s e')
  substituteBndArgs (ReturnM e) =
    let (vs, e') = substituteBndArgs e
    in (vs, ReturnM e')
  substituteBndArgs e@(BndVarM t i) = ([(i, t)], e)
  substituteBndArgs e = ([], e)

  sub :: [(Int, TypeM)] -> Argument -> Argument
  sub bnds r@(PassThroughArgument i) = case [t | (i', t) <- bnds, i == i'] of
    ((Serial t):_) -> SerialArgument i t 
    ((Native t):_) -> NativeArgument i t 
    ((Function _ _):_) -> error "You don't need to pass functions as manifold arguments"
    (Passthrough : _) -> error "What about 'Passthrough' do you not understand?"
    _ -> r 
  sub _ r = r

rehead :: ExprM Many -> MorlocMonad (ExprM Many)
rehead (LamM _ e) = rehead e
rehead (ManifoldM m args (ReturnM e)) = do
  e' <- packExprM m e
  return $ ManifoldM m args (ReturnM e')
rehead _ = MM.throwError $ CallTheMonkeys "Bad Head"

-- Sort manifolds into pools. Within pools, group manifolds into call sets.
pool :: [ExprM Many] -> MorlocMonad [(Lang, [ExprM Many])]
pool = return . groupSort . map (\e -> (fromJust $ langOf e, e))

encode
  :: [Source]
  -> (Lang, [ExprM Many])
  -> MorlocMonad Script
encode srcs (lang, xs) = do
  state <- MM.get

  -- this function cleans up source names (if needed) and generates compiler flags and paths to search
  (sources, flags, includes) <- Mod.handleFlagsAndPaths lang
    $ unique [s | s <- srcs, srcLang s == lang]

  xs' <- mapM (preprocess lang) xs >>= chooseSerializer
  -- translate each node in the AST to code
  code <- translate lang sources xs'

  return $ Script
    { scriptBase = "pool"
    , scriptLang = lang
    , scriptCode = Code . render $ code
    , scriptCompilerFlags = flags
    , scriptInclude = includes
    }

preprocess :: Lang -> ExprM Many -> MorlocMonad (ExprM Many)
preprocess CppLang es = Cpp.preprocess es
preprocess RLang es = R.preprocess es
preprocess Python3Lang es = Python3.preprocess es
preprocess l _ = MM.throwError . PoolBuildError . render
               $ "Language '" <> viaShow l <> "' has no translator"

chooseSerializer :: [ExprM Many] -> MorlocMonad [ExprM One]
chooseSerializer xs = mapM chooseSerializer' xs where
  chooseSerializer' :: ExprM Many -> MorlocMonad (ExprM One)
  -- This is where the magic happens, the rest is just plumbing
  chooseSerializer' (SerializeM s e) = SerializeM <$> oneSerial s <*> chooseSerializer' e
  chooseSerializer' (DeserializeM s e) = DeserializeM <$> oneSerial s <*> chooseSerializer' e
  -- plumbing
  chooseSerializer' (ManifoldM g args e) = ManifoldM g args <$> chooseSerializer' e
  chooseSerializer' (ForeignInterfaceM t e) = ForeignInterfaceM t <$> chooseSerializer' e
  chooseSerializer' (LetM i e1 e2) = LetM i <$> chooseSerializer' e1 <*> chooseSerializer' e2
  chooseSerializer' (AppM e es) = AppM <$> chooseSerializer' e <*> mapM chooseSerializer' es
  chooseSerializer' (LamM args e) = LamM args <$> chooseSerializer' e
  chooseSerializer' (AccM e k) = AccM <$> chooseSerializer' e <*> pure k
  chooseSerializer' (ListM t es) = ListM t <$> mapM chooseSerializer' es
  chooseSerializer' (TupleM t es) = TupleM t <$> mapM chooseSerializer' es
  chooseSerializer' (RecordM t rs) = do
    ts <- mapM (chooseSerializer' . snd) rs
    return $ RecordM t (zip (map fst rs) ts)
  chooseSerializer' (ReturnM e ) = ReturnM <$> chooseSerializer' e
  chooseSerializer' (SrcM t s) = return $ SrcM t s
  chooseSerializer' (PoolCallM t i d args) = return $ PoolCallM t i d args
  chooseSerializer' (BndVarM t i ) = return $ BndVarM t i
  chooseSerializer' (LetVarM t i) = return $ LetVarM t i
  chooseSerializer' (LogM t x) = return $ LogM t x
  chooseSerializer' (NumM t x) = return $ NumM t x
  chooseSerializer' (StrM t x) = return $ StrM t x
  chooseSerializer' (NullM t) = return $ NullM t

  oneSerial :: SerialAST Many -> MorlocMonad (SerialAST One)
  oneSerial (SerialPack _ (Many [])) = MM.throwError . SerializationError $ "No valid serializer found"
  oneSerial (SerialPack v (Many ((p,s):_))) = do
    s' <- oneSerial s
    return $ SerialPack v (One (p, s'))
  oneSerial (SerialList s) = SerialList <$> oneSerial s
  oneSerial (SerialTuple ss) = SerialTuple <$> mapM oneSerial ss
  oneSerial (SerialObject r v ps rs) = do
    ts <- mapM (oneSerial . snd) rs
    return $ SerialObject r v ps (zip (map fst rs) ts)
  oneSerial (SerialNum t) = return $ SerialNum t
  oneSerial (SerialBool t) = return $ SerialBool t
  oneSerial (SerialString t) = return $ SerialString t
  oneSerial (SerialNull t) = return $ SerialNull t
  oneSerial (SerialUnknown t) = return $ SerialUnknown t

translate :: Lang -> [Source] -> [ExprM One] -> MorlocMonad MDoc
translate lang srcs es = do
  case lang of
    CppLang -> Cpp.translate srcs es
    RLang -> R.translate srcs es
    Python3Lang -> Python3.translate srcs es
    x -> MM.throwError . PoolBuildError . render
      $ "Language '" <> viaShow x <> "' has no translator"


-------- Utility and lookup functions ----------------------------------------

unpackSAnno :: (SExpr g One c -> g -> c -> a) -> SAnno g One c -> [a]
unpackSAnno f (SAnno (One (e@(AccS x _),     c)) g) = f e g c : unpackSAnno f x
unpackSAnno f (SAnno (One (e@(ListS xs),     c)) g) = f e g c : conmap (unpackSAnno f) xs
unpackSAnno f (SAnno (One (e@(TupleS xs),    c)) g) = f e g c : conmap (unpackSAnno f) xs
unpackSAnno f (SAnno (One (e@(RecS entries), c)) g) = f e g c : conmap (unpackSAnno f) (map snd entries)
unpackSAnno f (SAnno (One (e@(LamS _ x),     c)) g) = f e g c : unpackSAnno f x
unpackSAnno f (SAnno (One (e@(AppS x xs),    c)) g) = f e g c : conmap (unpackSAnno f) (x:xs)
unpackSAnno f (SAnno (One (e, c)) g)                = [f e g c]

mapGCM :: (g -> c -> MorlocMonad c') -> SAnno g One c -> MorlocMonad (SAnno g One c')
mapGCM f (SAnno (One (AccS x k, c)) g) = do
  x' <- mapGCM f x
  c' <- f g c
  return $ SAnno (One (AccS x' k, c')) g
mapGCM f (SAnno (One (ListS xs, c)) g) = do
  xs' <- mapM (mapGCM f) xs
  c' <- f g c
  return $ SAnno (One (ListS xs', c')) g
mapGCM f (SAnno (One (TupleS xs, c)) g) = do
  xs' <- mapM (mapGCM f) xs
  c' <- f g c
  return $ SAnno (One (TupleS xs', c')) g
mapGCM f (SAnno (One (RecS entries, c)) g) = do
  xs' <- mapM (mapGCM f) (map snd entries)
  c' <- f g c
  return $ SAnno (One (RecS (zip (map fst entries) xs'), c')) g
mapGCM f (SAnno (One (LamS vs x, c)) g) = do
  x' <- mapGCM f x
  c' <- f g c
  return $ SAnno (One (LamS vs x', c')) g
mapGCM f (SAnno (One (AppS x xs, c)) g) = do
  x' <- mapGCM f x
  xs' <- mapM (mapGCM f) xs
  c' <- f g c
  return $ SAnno (One (AppS x' xs', c')) g
mapGCM f (SAnno (One (VarS x, c)) g) = do
  c' <- f g c
  return $ SAnno (One (VarS x, c')) g
mapGCM f (SAnno (One (CallS src, c)) g) = do
  c' <- f g c
  return $ SAnno (One (CallS src, c')) g
mapGCM f (SAnno (One (UniS, c)) g) = do
  c' <- f g c
  return $ SAnno (One (UniS, c')) g
mapGCM f (SAnno (One (NumS x, c)) g) = do
  c' <- f g c
  return $ SAnno (One (NumS x, c')) g
mapGCM f (SAnno (One (LogS x, c)) g) = do
  c' <- f g c
  return $ SAnno (One (LogS x, c')) g
mapGCM f (SAnno (One (StrS x, c)) g) = do
  c' <- f g c
  return $ SAnno (One (StrS x, c')) g

sannoSnd :: SAnno g One (a, b) -> b
sannoSnd (SAnno (One (_, (_, x))) _) = x

-- generate infinite list of fresh variables of form
-- ['a','b',...,'z','aa','ab',...,'zz',...]
freshVarsAZ
  :: [MT.Text] -- variables to exclude
  -> [MT.Text]
freshVarsAZ exclude =
  filter
    (\x -> not (elem x exclude))
    ([1 ..] >>= flip replicateM ['a' .. 'z'] |>> MT.pack)

-- turn type list into a function
makeType :: [Type] -> MorlocMonad Type
makeType [] = MM.throwError . TypeError $ "empty type"
makeType [t] = return t
makeType (t:ts) = pure (FunT t) <*> makeType ts
