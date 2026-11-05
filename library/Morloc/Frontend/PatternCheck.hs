{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.PatternCheck
Description : Check pattern matches against the types they match
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Desugar records the clauses of every pattern match by the name it binds the
receiver to ('stateMatchSites'). Here each match is found in the typed trees
by that name, its patterns are checked against the receiver's type, and its
clauses are checked for coverage and reachability with
'Morloc.Data.Usefulness' (PAT-1, PAT-3, SUM-3).
-}
module Morloc.Frontend.PatternCheck
  ( checkPatterns
  , receiverShape
  , rowFields
  , tupleWidthError
  , missingKeyError
  ) where

import Morloc.Data.Zip (zipPrefix, zipSame, zipWithSame)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as DT
import qualified Morloc.BaseTypes as BT
import Morloc.Data.Doc
import qualified Morloc.Data.Usefulness as U
import Morloc.Frontend.Namespace
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE

-- | A constructor head: a `data` constructor with its type's constructors
-- and their arities, a tuple of a width, a record with its type's keys, a
-- literal, or a list of a length.
data Head
  = HCon DT.Text [(DT.Text, Int)]
  | HTuple Int
  | HRecord [Key]
  | HLit SiteLit
  | HList Int
  deriving (Eq)

-- | Why a pattern was not converted: a type the check cannot see into,
-- which leaves the match to its run-time throw (a receiver whose type is
-- still unknown in a typed tree is never given a value there), a tuple
-- pattern of the wrong width, or a record field the value lacks.
data Problem
  = Unknown
  | TupleWidth Int Int
  | MissingKey Key

-- | PAT-1, PAT-3, PAT-8: each match, at every type it is used at.
checkPatterns :: [AnnoS (Indexed TypeU) Many Int] -> MorlocMonad ()
checkPatterns trees = do
  sites <- MM.gets stateMatchSites
  scope <- MM.getGeneralScope
  let found = Set.toList (Set.fromList (concatMap (foldAnnoS (receivers sites)) trees))
  foldM_
      (\done (k, ts) -> case Map.lookup k sites of
          Just site -> do
            warnable <- checkSite scope ts site
            unless (Set.member k done) (mapM_ (`MM.warnAt` unreachable) warnable)
            return (Set.insert k done)
          Nothing -> return done)
      Set.empty
      found
  where
    receivers sites (AnnoS (Idx _ t) _ e) = case e of
      LetS v (AnnoS (Idx _ t1) _ _) _
        | Map.member (base v) sites -> [(base v, [t1])]
      LamS vs _ ->
        let ts = argTypes t
         in [ (k, take (siteWidth site) (drop i ts))
            | (i, v) <- zipPrefix [0 ..] vs
            , let k = base v
            , Just site <- [Map.lookup k sites]
            ]
      _ -> []

    siteWidth site = case siteRows site of
      ((_, ps) : _) -> length ps
      [] -> 0

    base (EV v) = DT.takeWhile (\c -> c /= '@' && c /= '`') v

    argTypes (FunU ts r) = ts <> argTypes r
    argTypes (ForallU _ t) = argTypes t
    argTypes _ = []

-- | Check one match at one receiver type, returning its unreachable
-- clauses.
checkSite :: Scope -> [TypeU] -> MatchSite -> MorlocMonad [Int]
checkSite scope ts (MatchSite i rows total)
  | any ((/= length ts) . length . snd) rows = return []
  | otherwise = do
      let converted = [(ri, [(pi', toPat scope t p) | (t, (pi', p)) <- zipSame ts ps]) | (ri, ps) <- rows]
      sequence_ [problem pi' q | (_, cols) <- converted, (pi', Left q) <- cols]
      case mapM (\(ri, cols) -> (,) ri <$> combine (map snd cols)) converted of
        Left _ -> return []
        Right matrix
          | not total -> return []
          | otherwise -> do
              case U.missing signature 3 (length ts) (map snd matrix) of
                [] -> return ()
                ws ->
                  MM.throwSourcedError i $
                    "These clauses do not match every value; not matched:"
                      <+> hsep (punctuate ";" (map (pretty . showRow) ws))
              let dead = U.redundant signature [(False, r) | (_, r) <- matrix]
              return [ri | (k, (ri, _)) <- zipPrefix [0 :: Int ..] matrix, k `elem` dead]
  where
    problem :: Int -> Problem -> MorlocMonad ()
    problem at (TupleWidth given wanted) = tupleWidthError at given (pretty wanted)
    problem at (MissingKey k) = missingKeyError at k
    problem _ Unknown = return ()

-- | PAT-1: a tuple pattern of this width on a tuple of another.
tupleWidthError :: Int -> Int -> MDoc -> MorlocMonad a
tupleWidthError at given wanted =
  MM.throwSourcedError at $
    "This tuple pattern has" <+> pretty given
      <+> "elements, but the value it matches is a tuple of" <+> wanted

-- | PAT-1: a record pattern naming a field its value does not have.
missingKeyError :: Int -> Key -> MorlocMonad a
missingKeyError at k =
  MM.throwSourcedError at $
    "This record pattern names the field" <+> squotes (pretty (unKey k))
      <> ", which the value it matches does not have"

unreachable :: MDoc
unreachable = "This clause is unreachable: the clauses above it match every value it matches"

signature :: U.Signature Head
signature = U.Signature headArity headCoverage
  where
    headArity h = case h of
      HCon c cs -> maybe 0 id (lookup c cs)
      HTuple n -> n
      HRecord ks -> length ks
      HLit _ -> 0
      HList n -> n
    headCoverage hs = case hs of
      (HCon _ cs : _) -> U.Absent [HCon c cs | (c, _) <- cs, HCon c cs `notElem` hs]
      (HLit (SiteBool _) : _) -> U.Absent [h | h <- [HLit (SiteBool True), HLit (SiteBool False)], h `notElem` hs]
      (HTuple _ : _) -> U.Complete
      (HRecord _ : _) -> U.Complete
      _ -> U.Unbounded

-- | A pattern against the type of the value it matches. A type the check
-- cannot see into leaves a pattern unconverted, which is decisive only for
-- the tuples inside it.
toPat :: Scope -> TypeU -> SitePat -> Either Problem (U.Pat Head)
toPat scope t0 = toPat' scope (either (const t0) id (TE.evaluateType scope t0))

-- | The outermost form of the type of a value a pattern destructures.
receiverShape :: Scope -> TypeU -> TypeU
receiverShape scope t0 = TE.unWhnf (TE.whnf scope (either (const t0) id (TE.evaluateType scope t0)))

-- | The fields of a record type, and whether it can have no others.
rowFields :: TypeU -> Maybe ([(Key, TypeU)], Bool)
rowFields (NamU _ _ _ fs) = Just (fs, True)
rowFields (LitU (LRec fs)) = Just ([(Key k, t) | (k, t) <- fs], True)
rowFields (RecExtendU k t rest) = Just $ case rowFields rest of
  Just (fs, closed) -> ((Key k, t) : fs, closed)
  Nothing -> ([(Key k, t)], False)
rowFields _ = Nothing

toPat' :: Scope -> TypeU -> SitePat -> Either Problem (U.Pat Head)
toPat' scope t p = case p of
  SitePatWild -> Right U.Wild
  SitePatLit l -> Right (U.Con (HLit l) [])
  SitePatTuple ps -> case shape of
    AppU (VarU v) fs
      | v == BT.tuple (length fs) ->
          if length fs == length ps
            then U.Con (HTuple (length ps)) <$> combine (zipWithSame (toPat scope) fs ps)
            else Left (TupleWidth (length ps) (length fs))
    _ -> Left Unknown
  SitePatCon c ps -> case TE.dataCtorsAt scope (TE.whnf scope t) of
    Just cs
      | Just fs <- lookup c cs, length fs == length ps ->
          U.Con (HCon c [(n, length xs) | (n, xs) <- cs]) <$> combine (zipWithSame (toPat scope) fs ps)
    _ -> Left Unknown
  SitePatRecord kps -> case rowFields shape of
    Just (fs, closed) -> case [k | (k, _) <- kps, k `notElem` map fst fs] of
      (k : _) | closed -> Left (MissingKey k)
      [] | closed ->
        U.Con (HRecord (map fst fs))
          <$> combine [maybe (Right U.Wild) (toPat scope ft) (lookup k kps) | (k, ft) <- fs]
      _ -> Left Unknown
    Nothing -> Left Unknown
  SitePatList ps -> case shape of
    AppU (VarU v) [a]
      | v == BT.list -> U.Con (HList (length ps)) <$> combine (map (toPat scope a) ps)
    _ -> Left Unknown
  where
    shape = TE.unWhnf (TE.whnf scope t)

-- | All converted, or the problem that matters most: a definite one before
-- an unknown type.
combine :: [Either Problem a] -> Either Problem [a]
combine xs = case [p | Left p <- xs] of
  [] -> Right [x | Right x <- xs]
  ps -> Left (foldr1 worse ps)
  where
    worse Unknown b = b
    worse a _ = a

showRow :: [U.Witness Head] -> String
showRow ws = intercalate ", " (map (showWitness False) ws)

showWitness :: Bool -> U.Witness Head -> String
showWitness nested w = case w of
  U.Any -> "_"
  U.Other hs -> "<" <> intercalate " or " (map showHead hs) <> " excluded>"
  U.WCon (HCon c _) [] -> DT.unpack c
  U.WCon (HCon c _) xs -> paren (unwords (DT.unpack c : map (showWitness True) xs))
  U.WCon (HTuple _) xs -> "(" <> intercalate ", " (map (showWitness False) xs) <> ")"
  U.WCon (HRecord ks) xs ->
    "{" <> intercalate ", " [DT.unpack (unKey k) <> " = " <> showWitness False x | (k, x) <- zipSame ks xs] <> "}"
  U.WCon (HList _) xs -> "[" <> intercalate ", " (map (showWitness False) xs) <> "]"
  U.WCon h _ -> showHead h
  where
    paren s = if nested then "(" <> s <> ")" else s

showHead :: Head -> String
showHead h = case h of
  HCon c _ -> DT.unpack c
  HTuple n -> "a " <> show n <> "-tuple"
  HRecord _ -> "a record"
  HLit (SiteInt n) -> show n
  HLit (SiteReal x) -> showRealLit x
  HLit (SiteStr s) -> show s
  HLit (SiteBool b) -> if b then "True" else "False"
  HList n -> "a list of " <> show n
