{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.CodeGenerator.DimContract
Description : The dimension relations a command's input must meet (DIM-3)
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

A command's input is outside data, so every relation its signature states
between the lengths and label values of its arguments is checked when the
command is called (DIM-3, the data level): a Nat two arguments share must
be the same length in both, and every Nat position of the signature must
be non-negative (KIND-4) at the values the input gives. The nexus reads
the relations from the command's manifest entry as a @dims@ constraint.

A Nat is bound where it is a Vector's length (DIM-8), found by following
an argument's type through lists, tuples, records, optionals and aliases,
or where a label binds it to an argument's value.
-}
module Morloc.CodeGenerator.DimContract
  ( dimConstraints
  ) where

import Morloc.Data.Zip (indexed, zipExact)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Morloc.BaseTypes as MBT
import Morloc.CodeGenerator.Namespace
import qualified Morloc.Data.GMap as GMap
import Morloc.Data.Json (jsonArr, jsonInt, jsonObj, jsonStr)
import qualified Morloc.Data.Text as MT
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import Morloc.Typecheck.Internal (natPositions, typeUToNatExpr, unqualify)
import qualified Morloc.Typecheck.NatSolver as NS

-- | One step from a value toward the value a binding reads.
data Step = Each | Field Int | Some

-- | Where a Nat is read: the argument, the path into it, and whether it is
-- that value's length or its integer value. A length that is an expression
-- binds a variable named for it, which must equal the expression.
data Binding = Binding
  { bindVar :: TVar
  , bindArg :: Int
  , bindPath :: [Step]
  , bindValue :: Bool
  , bindEq :: Maybe NS.NatExpr
  , bindHeader :: Maybe (Text, Int)
  -- ^ For a newtype header's value: what it is, and how many trailing
  -- steps of the path lie inside the header
  }

-- | The @dims@ constraint of the command exported as the term at @i@, if
-- its signature relates the lengths or label values of its arguments.
dimConstraints :: Int -> MorlocMonad [Text]
dimConstraints i = do
  sgmap <- MM.gets stateSignatures
  scope <- MM.gets stateGeneralTypedefs
  headers <- MM.gets stateNewtypeHeaders
  name <- MM.metaName i
  inferred <- MM.gets (Map.lookup i . stateExportTypes)
  -- An export without a signature is held to the type inferred for it.
  let ets = case GMap.lookup i sgmap of
        GMapJust (Monomorphic (TermTypes (Just et) _ _)) -> [(etype et, enatLabels et)]
        GMapJust (Polymorphic _ _ et) -> [(etype et, enatLabels et)]
        _ -> [(t, Map.empty) | Just t <- [inferred]]
  return $ concatMap (contract scope headers (maybe "" unEVar name)) ets

contract :: Scope -> Headers -> Text -> (TypeU, Map.Map TVar Int) -> [Text]
contract scope headers name (et, labels)
  | null conds && null eqs && null multiples && null roots && not (any shared vars) = []
  | otherwise =
      [ jsonObj
          [ ("type", jsonStr "dims")
          , ( "value"
            , jsonObj
                [ ("vars", jsonArr [jsonStr (display v) | v <- allVars])
                , ("bind", jsonArr (map bindJson binds))
                , ("conds", jsonArr (map rootJson roots <> map condJson conds <> map eqJson eqs <> map multipleJson multiples))
                , ("name", jsonStr name)
                ]
            )
          ]
      ]
  where
    t = snd (unqualify et)
    args = fst (uncurryU t)
    labelArgs = Map.fromList [(k, v) | (v, k) <- Map.toList labels]
    binds =
      concat
        [ case Map.lookup k labelArgs of
            Just v | intLabel a -> [Binding v k [] True Nothing Nothing]
            _ -> lengths scope headers k [] a
        | (k, a) <- indexed args
        ]
    vars = List.nub (map bindVar binds)
    -- A length or header that is an expression with a variable no slot
    -- binds gives that expression a value, so it stands for the expression
    -- wherever the expression occurs.
    known =
      List.nub
        [ (e, NS.NatVar v)
        | Binding v _ _ _ (Just e) _ <- binds
        , not (NS.isGround e)
        , not (all (`elem` vars) (NS.freeNatVars e))
        ]
    -- A variable an expression alone determines is solved from the first
    -- such expression: `v + c`, `c - v`, `v - c` and `c * v` (which also
    -- needs a multiple of `c`) directly, and any other of `+` and `*` in
    -- that variable by the checker's search, since such an expression
    -- never decreases as the variable grows. Any other expression in the
    -- variable is then checked as an equation.
    -- Each solution can leave another expression with one variable to
    -- solve, so they are taken one at a time until none is left.
    chosen = choose vars known
    choose given pending =
      case [ (v, w, e, inv)
           | (e, x@(NS.NatVar w)) <- pending
           , [v] <- [filter (`notElem` given) (Set.toList (NS.freeNatVars e))]
           , let inv = invert v e x
           , isJust inv || monotone e
           ] of
        [] -> []
        (c@(v, w, _, _) : _) -> c : choose (v : given) [k | k@(_, x) <- pending, x /= NS.NatVar w]
    solved = [(v, x') | (v, _, _, Just (x', _)) <- chosen]
    -- The variables of the expressions a variable was solved from: their
    -- equations hold by construction.
    solving = [w | (_, w, _, _) <- chosen]
    roots = [(w, v, e) | (v, w, e, Nothing) <- chosen]
    monotone e = case e of
      NS.NatAdd a b -> monotone a && monotone b
      NS.NatMul a b -> monotone a && monotone b
      NS.NatLit c -> c >= 0
      NS.NatVar _ -> True
      _ -> False
    allVars = vars <> List.nub [v | (_, v, _) <- roots]
    multiples = [(w, c, e) | (_, w, e, Just (_, Just c)) <- chosen]
    -- `e = x` solved for `v`, its one variable no slot binds, when `e` is
    -- linear in `v`; a division by a number needs `x` a multiple of it.
    invert v e x = case e of
      NS.NatVar u | u == v -> Just (x, Nothing)
      NS.NatAdd a b
        | has a && not (has b) -> fst' (invert v a (NS.NatSub x b))
        | has b && not (has a) -> fst' (invert v b (NS.NatSub x a))
      NS.NatSub a b
        | has a && not (has b) -> fst' (invert v a (NS.NatAdd x b))
        | has b && not (has a) -> fst' (invert v b (NS.NatSub a x))
      NS.NatMul (NS.NatLit c) a | c > 0, NS.NatVar u <- a, u == v -> Just (NS.NatDiv x (NS.NatLit c), Just c)
      NS.NatMul a (NS.NatLit c) | c > 0, NS.NatVar u <- a, u == v -> Just (NS.NatDiv x (NS.NatLit c), Just c)
      _ -> Nothing
      where
        has y = Set.member v (NS.freeNatVars y)
        -- A multiple is checked only of a length or header itself.
        fst' r = case r of
          Just (y, Nothing) -> Just (y, Nothing)
          _ -> Nothing
    sub = subExcept Nothing
    -- The substitution for the binding of `self`, which its own
    -- expression does not stand for.
    subExcept self e = case lookup e [k | k@(_, x) <- known, Just x /= self] of
      Just x -> x
      Nothing -> case e of
        NS.NatVar v | Just x <- lookup v solved -> subExcept self x
        NS.NatAdd a b -> NS.NatAdd (subExcept self a) (subExcept self b)
        NS.NatSub a b -> NS.NatSub (subExcept self a) (subExcept self b)
        NS.NatMul a b -> NS.NatMul (subExcept self a) (subExcept self b)
        NS.NatDiv a b -> NS.NatDiv (subExcept self a) (subExcept self b)
        _ -> e
    eqs =
      List.nub
        [ (v, e')
        | b <- binds
        , Just e <- [bindEq b]
        , let v = bindVar b
        , v `notElem` solving
        , let e' = subExcept (Just (NS.NatVar v)) e
        , e' /= NS.NatVar v
        , bound e'
        ]
    -- A header value is a Nat, which is never negative (KIND-4).
    headerNats =
      List.nub
        [ NS.NatVar v
        | Binding v _ _ True eq (Just _) <- binds
        , maybe True (not . NS.isGround) eq
        , Map.notMember v labels
        ]
    index v = maybe 0 id (List.elemIndex v allVars)
    shared v = length [() | b <- binds, bindVar b == v] > 1
    bound e = all (`elem` allVars) (NS.freeNatVars e)
    -- Each Nat position whose variables the input binds and which is not
    -- non-negative for every value, and each divisor in one.
    conds =
      List.nub
        [ e
        | p <- natPositions t
        , Just e0 <- [sub <$> typeUToNatExpr p]
        , e <- e0 : divisors e0
        , bound e
        , not (NS.isGround e)
        , not (NS.nonNegativeEverywhere e)
        ]
        <> headerNats
        <> map snd solved
    divisors e = case e of
      NS.NatDiv a b -> NS.NatSub b (NS.NatLit 1) : divisors a <> divisors b
      NS.NatAdd a b -> divisors a <> divisors b
      NS.NatSub a b -> divisors a <> divisors b
      NS.NatMul a b -> divisors a <> divisors b
      _ -> []
    bindJson b =
      jsonObj $
        [ ("var", jsonInt (index (bindVar b)))
        , ("arg", jsonInt (bindArg b))
        , ("path", jsonArr (map (jsonStr . stepText) (bindPath b)))
        , ("of", jsonStr (if bindValue b then "value" else "length"))
        ]
          <> [ ("header", jsonObj [("desc", jsonStr d), ("depth", jsonInt n)])
             | Just (d, n) <- [bindHeader b]
             ]
    eqJson (v, e) =
      jsonObj
        [ ("len", jsonInt (index v))
        , ("expr", jsonArr (map jsonStr (postfix e)))
        , ("text", jsonStr (natText e))
        ]
    rootJson (w, v, e) =
      jsonObj
        [ ("len", jsonInt (index w))
        , ("root", jsonInt (index v))
        , ("expr", jsonArr (map jsonStr (postfix (subExcept (Just (NS.NatVar w)) e))))
        , ("text", jsonStr (natText e))
        ]
    multipleJson (w, c, e) =
      jsonObj
        [ ("len", jsonInt (index w))
        , ("multiple", jsonInt (fromIntegral c))
        , ("expr", jsonArr [])
        , ("text", jsonStr (natText e))
        ]
    condJson e =
      jsonObj
        [ ("expr", jsonArr (map jsonStr (postfix e)))
        , ("text", jsonStr (maybe (natText e) display (lookup e [(x, v) | (v, x) <- solved])))
        ]
    postfix e = case e of
      NS.NatLit n -> [MT.show' n]
      NS.NatVar v -> ["v" <> MT.show' (index v)]
      NS.IntVar v -> ["v" <> MT.show' (index v)]
      NS.NatAdd a b -> postfix a <> postfix b <> ["+"]
      NS.NatSub a b -> postfix a <> postfix b <> ["-"]
      NS.NatMul a b -> postfix a <> postfix b <> ["*"]
      NS.NatDiv a b -> postfix a <> postfix b <> ["/"]
    -- A label of an integer argument binds a Nat or an Int; any other
    -- binds a value no Nat position holds.
    intLabel = integral (8 :: Int)
    integral 0 _ = False
    integral fuel a = case a of
      VarU v | v `elem` [MBT.int, MBT.i8, MBT.i16, MBT.i32, MBT.i64, MBT.uint, MBT.u8, MBT.u16, MBT.u32, MBT.u64] -> True
      _ -> case TE.evaluateStep scope a of
        Just a' | a' /= a -> integral (fuel - 1) a'
        _ -> False

-- | A variable as the signature writes it. One that stands for an
-- expression is named for it.
display :: TVar -> Text
display (TV v) = MT.takeWhile (/= '@') v

-- | A Nat expression as a user would write it: the terms of a sum with a
-- positive coefficient first, then those subtracted, then the constant;
-- with no positive term, the constant comes first.
natText :: NS.NatExpr -> Text
natText e0 = case vars' <> (if null vars' then lit <> negs else negs <> lit) of
  [] -> "0"
  ((s, x) : rest) -> (if s then x else "-" <> x) <> mconcat [(if s' then " + " else " - ") <> y | (s', y) <- rest]
  where
    ts = flat True e0
    vars' = [(True, atom x) | (True, x) <- ts, not (isLit x)]
    negs = [(False, atom x) | (False, x) <- ts, not (isLit x)]
    c = sum [if s then n else negate n | (s, NS.NatLit n) <- ts]
    lit = [(c >= 0, MT.show' (abs c)) | c /= 0]
    isLit (NS.NatLit _) = True
    isLit _ = False
    flat s e = case e of
      NS.NatAdd a b -> flat s a <> flat s b
      NS.NatSub a b -> flat s a <> flat (not s) b
      NS.NatMul (NS.NatLit (-1)) x -> flat (not s) x
      NS.NatMul x (NS.NatLit (-1)) -> flat (not s) x
      NS.NatMul (NS.NatLit k) x | k < 0 -> [(not s, NS.NatMul (NS.NatLit (negate k)) x)]
      NS.NatLit k | k < 0 -> [(not s, NS.NatLit (negate k))]
      _ -> [(s, e)]
    atom e = case e of
      NS.NatLit n -> MT.show' n
      NS.NatVar v -> named v
      NS.IntVar v -> named v
      NS.NatMul a b -> factor a <> " * " <> factor b
      NS.NatDiv a b -> factor a <> " / " <> factor b
      _ -> natText e
    named v = case MT.uncons (display v) of
      Just ('#', x) -> "(" <> x <> ")"
      _ -> display v
    factor e = case e of
      NS.NatAdd _ _ -> "(" <> natText e <> ")"
      NS.NatSub _ _ -> "(" <> natText e <> ")"
      NS.NatLit n | n < 0 -> "(" <> MT.show' n <> ")"
      _ -> atom e

-- | A newtype's representation at the given arguments, with each header
-- label's description, the tuple fields that reach it and the argument it
-- holds.
representation :: Scope -> Headers -> TypeU -> Maybe ([(Text, [Int], TypeU)], TypeU)
representation scope headers t = case t of
  AppU (VarU v) ts -> at v ts
  VarU v -> at v []
  _ -> Nothing
  where
    at v ts = case Map.lookup v scope of
      Just ((ps, body, _, _, TypedefNewtype) : _)
        | NamU{} <- body -> Nothing
        | params <- [p | Left (p, _) <- ps]
        , length params == length ps
        , Just args <- zipExact params ts ->
            let hs =
                  [ (display l <> " in the " <> unTVar v <> " header", fields, n)
                  | (l, fields) <- Map.findWithDefault [] v headers
                  , Just n <- [lookup l args]
                  ]
             in Just (hs, TE.parsubAll args body)
      _ -> Nothing

-- | The variable a length or value that is an expression binds. One that
-- is a number gets a variable of its own slot, so that two slots holding
-- the same number are not taken to share it.
synthetic :: Int -> [Step] -> NS.NatExpr -> TVar
synthetic k path e
  | NS.isGround e = TV ("#" <> natText e <> "@" <> MT.show' k <> mconcat (map stepText path))
  | otherwise = TV ("#" <> natText e)

stepText :: Step -> Text
stepText Each = "*"
stepText (Field k) = "f" <> MT.show' k
stepText Some = "?"

-- | Each newtype's header labels: the parameter and the tuple fields that
-- reach its integer.
type Headers = Map.Map TVar [(TVar, [Int])]

-- | The Vector lengths and header values a value of a type binds, by path
-- from the value. A literal length is left to the decoders; a literal
-- header value is not, since no decoder reads it.
lengths :: Scope -> Headers -> Int -> [Step] -> TypeU -> [Binding]
lengths scope headers = go (8 :: Int)
  where
    go 0 _ _ _ = []
    go fuel k path t = case t of
      AppU (VarU v) [n, a]
        | v == MBT.vector ->
            lengthOf k path n
              <> go fuel k (path <> [Each]) a
      AppU (VarU v) [a]
        | v == MBT.list -> go fuel k (path <> [Each]) a
      AppU (VarU v) ts
        | v == MBT.tuple (length ts) -> concat [go fuel k (path <> [Field j]) x | (j, x) <- indexed ts]
      AppU (VarU v) [n, _]
        | MBT.isTableVar v -> rowsOf k path n
      NamU _ _ _ rs -> concat [go fuel k (path <> [Field j]) x | (j, (_, x)) <- indexed rs]
      OptionalU a -> go fuel k (path <> [Some]) a
      _ -> case representation scope headers t of
        Just (hs, body) -> concatMap (header k path) hs <> go (fuel - 1) k path body
        Nothing -> case TE.evaluateStep scope t of
          Just t' | t' /= t -> go (fuel - 1) k path t'
          _ -> []
    header k path (desc, fields, n) =
      let at = path <> map Field fields
          h = Just (desc, length fields)
       in case n of
            NatVarU v -> [Binding v k at True Nothing h]
            IntVarU v -> [Binding v k at True Nothing h]
            _ -> case typeUToNatExpr n of
              Just e -> [Binding (synthetic k at e) k at True (Just e) h]
              Nothing -> []
    -- No decoder reads a table's row count, so a literal one is checked
    -- here too.
    rowsOf k path n = case typeUToNatExpr n of
      Just e | NS.isGround e -> [Binding (synthetic k path e) k path False (Just e) Nothing]
      _ -> lengthOf k path n
    lengthOf k path n = case n of
      NatVarU v -> [Binding v k path False Nothing Nothing]
      IntVarU v -> [Binding v k path False Nothing Nothing]
      _ -> case typeUToNatExpr n of
        Just e | not (NS.isGround e) -> [Binding (synthetic k path e) k path False (Just e) Nothing]
        _ -> []
