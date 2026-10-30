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

import qualified Data.List as List
import qualified Data.Map as Map
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
  }

-- | The @dims@ constraint of the command exported as the term at @i@, if
-- its signature relates the lengths or label values of its arguments.
dimConstraints :: Int -> MorlocMonad [Text]
dimConstraints i = do
  sgmap <- MM.gets stateSignatures
  scope <- MM.gets stateGeneralTypedefs
  name <- MM.metaName i
  let ets = case GMap.lookup i sgmap of
        GMapJust (Monomorphic (TermTypes (Just et) _ _)) -> [et]
        GMapJust (Polymorphic _ _ et _) -> [et]
        _ -> []
  return $ concatMap (contract scope (maybe "" unEVar name)) ets

contract :: Scope -> Text -> EType -> [Text]
contract scope name et
  | null conds && null eqs && not (any shared vars) = []
  | otherwise =
      [ jsonObj
          [ ("type", jsonStr "dims")
          , ( "value"
            , jsonObj
                [ ("vars", jsonArr [jsonStr (display v) | v <- vars])
                , ("bind", jsonArr (map bindJson binds))
                , ("conds", jsonArr (map condJson conds <> map eqJson eqs))
                , ("name", jsonStr name)
                ]
            )
          ]
      ]
  where
    t = snd (unqualify (etype et))
    args = case t of
      FunU as _ -> as
      _ -> []
    labelArgs = Map.fromList [(k, v) | (v, k) <- Map.toList (enatLabels et)]
    binds =
      concat
        [ case Map.lookup k labelArgs of
            Just v | intLabel a -> [Binding v k [] True Nothing]
            _ -> lengths scope k [] a
        | (k, a) <- zip [0 ..] args
        ]
    vars = List.nub (map bindVar binds)
    eqs = List.nub [(bindVar b, e) | b <- binds, Just e <- [bindEq b], bound e]
    index v = maybe 0 id (List.elemIndex v vars)
    shared v = length [() | b <- binds, bindVar b == v] > 1
    bound e = all (`elem` vars) (NS.freeNatVars e)
    -- Each Nat position whose variables the input binds and which is not
    -- non-negative for every value, and each divisor in one.
    conds =
      List.nub
        [ e
        | p <- natPositions t
        , Just e0 <- [typeUToNatExpr p]
        , e <- e0 : divisors e0
        , bound e
        , not (NS.isGround e)
        , not (NS.nonNegativeEverywhere e)
        ]
    divisors e = case e of
      NS.NatDiv a b -> NS.NatSub b (NS.NatLit 1) : divisors a <> divisors b
      NS.NatAdd a b -> divisors a <> divisors b
      NS.NatSub a b -> divisors a <> divisors b
      NS.NatMul a b -> divisors a <> divisors b
      _ -> []
    bindJson b =
      jsonObj
        [ ("var", jsonInt (index (bindVar b)))
        , ("arg", jsonInt (bindArg b))
        , ("path", jsonArr (map (jsonStr . stepText) (bindPath b)))
        , ("of", jsonStr (if bindValue b then "value" else "length"))
        ]
    eqJson (v, e) =
      jsonObj
        [ ("len", jsonInt (index v))
        , ("expr", jsonArr (map jsonStr (postfix e)))
        , ("text", jsonStr (natText e))
        ]
    condJson e =
      jsonObj
        [ ("expr", jsonArr (map jsonStr (postfix e)))
        , ("text", jsonStr (natText e))
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

display :: TVar -> Text
display (TV v) = MT.takeWhile (/= '@') v

-- | A Nat expression as a user would write it: the terms of a sum with a
-- positive coefficient first, then those subtracted, then the constant.
natText :: NS.NatExpr -> Text
natText e0 = case vars' <> negs <> lit of
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
      NS.NatVar v -> display v
      NS.IntVar v -> display v
      NS.NatMul a b -> factor a <> " * " <> factor b
      NS.NatDiv a b -> factor a <> " / " <> factor b
      _ -> natText e
    factor e = case e of
      NS.NatAdd _ _ -> "(" <> natText e <> ")"
      NS.NatSub _ _ -> "(" <> natText e <> ")"
      NS.NatLit n | n < 0 -> "(" <> MT.show' n <> ")"
      _ -> atom e

stepText :: Step -> Text
stepText Each = "*"
stepText (Field k) = "f" <> MT.show' k
stepText Some = "?"

-- | The Vector lengths a value of a type binds, by path from the value.
-- A literal length is left to the decoders.
lengths :: Scope -> Int -> [Step] -> TypeU -> [Binding]
lengths scope = go (8 :: Int)
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
        | v == MBT.tuple (length ts) -> concat [go fuel k (path <> [Field j]) x | (j, x) <- zip [0 ..] ts]
      NamU _ _ _ rs -> concat [go fuel k (path <> [Field j]) x | (j, (_, x)) <- zip [0 ..] rs]
      OptionalU a -> go fuel k (path <> [Some]) a
      _ -> case TE.evaluateStep scope t of
        Just t' | t' /= t -> go (fuel - 1) k path t'
        _ -> []
    lengthOf k path n = case n of
      NatVarU v -> [Binding v k path False Nothing]
      IntVarU v -> [Binding v k path False Nothing]
      _ -> case typeUToNatExpr n of
        Just e | not (NS.isGround e) -> [Binding (TV ("#" <> natText e)) k path False (Just e)]
        _ -> []
