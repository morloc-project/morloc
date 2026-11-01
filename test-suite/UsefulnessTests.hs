module UsefulnessTests (usefulnessTests) where

import Morloc.Data.Usefulness
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

-- A small type universe, checked against brute-force enumeration of its
-- values. Int literals in patterns stay below 3 and Int values reach 3, so no
-- set of literals covers Int, as no set of literals covers an unbounded type.
data Ty = TBool | TInt | TPair Ty Ty | TOpt Ty
  deriving (Show, Eq)

data H = HTrue | HFalse | HPair | HNone | HSome | HLit Int
  deriving (Show, Eq)

data V = VB Bool | VI Int | VP V V | VN | VS V
  deriving (Show, Eq)

sig :: Signature H
sig = Signature ar cov
  where
    ar HPair = 2
    ar HSome = 1
    ar _ = 0
    cov hs = case hs of
      (h : _) | isBool h -> absent [HTrue, HFalse]
      (h : _) | isOpt h -> absent [HNone, HSome]
      (HPair : _) -> Complete
      _ -> Unbounded
      where
        absent all' = case [h | h <- all', h `notElem` hs] of
          [] -> Complete
          xs -> Absent xs
    isBool h = h == HTrue || h == HFalse
    isOpt h = h == HNone || h == HSome

values :: Ty -> [V]
values TBool = [VB True, VB False]
values TInt = map VI [0 .. 3]
values (TPair a b) = [VP x y | x <- values a, y <- values b]
values (TOpt a) = VN : map VS (values a)

matches :: Pat H -> V -> Bool
matches Wild _ = True
matches (Con HTrue []) (VB True) = True
matches (Con HFalse []) (VB False) = True
matches (Con (HLit n) []) (VI m) = n == m
matches (Con HPair [p, q]) (VP x y) = matches p x && matches q y
matches (Con HNone []) VN = True
matches (Con HSome [p]) (VS x) = matches p x
matches _ _ = False

rowMatches :: [Pat H] -> [V] -> Bool
rowMatches ps vs = and (zipWith matches ps vs)

headOf :: V -> H
headOf (VB True) = HTrue
headOf (VB False) = HFalse
headOf (VI n) = HLit n
headOf (VP _ _) = HPair
headOf VN = HNone
headOf (VS _) = HSome

fields :: V -> [V]
fields (VP x y) = [x, y]
fields (VS x) = [x]
fields _ = []

witnessMatches :: Witness H -> V -> Bool
witnessMatches Any _ = True
witnessMatches (Other hs) v = headOf v `notElem` hs
witnessMatches (WCon h ws) v = headOf v == h && and (zipWith witnessMatches ws (fields v))

vectorMatches :: [Witness H] -> [V] -> Bool
vectorMatches ws vs = and (zipWith witnessMatches ws vs)

vectors :: [Ty] -> [[V]]
vectors = mapM values

genTy :: Int -> Gen Ty
genTy 0 = elements [TBool, TInt]
genTy d =
  frequency
    [ (2, elements [TBool, TInt])
    , (1, TPair <$> genTy (d - 1) <*> genTy (d - 1))
    , (1, TOpt <$> genTy (d - 1))
    ]

genPat :: Ty -> Gen (Pat H)
genPat t = frequency [(1, pure Wild), (3, con t)]
  where
    con TBool = elements [Con HTrue [], Con HFalse []]
    con TInt = Con . HLit <$> choose (0, 2) <*> pure []
    con (TPair a b) = (\p q -> Con HPair [p, q]) <$> genPat a <*> genPat b
    con (TOpt a) = oneof [pure (Con HNone []), (\p -> Con HSome [p]) <$> genPat a]

data Case = Case [Ty] [(Bool, [Pat H])]
  deriving (Show)

instance Arbitrary Case where
  arbitrary = do
    width <- choose (1, 2)
    tys <- vectorOf width (genTy 2)
    n <- choose (0, 6)
    rows <- vectorOf n ((,) <$> frequency [(4, pure False), (1, pure True)] <*> mapM genPat tys)
    return (Case tys rows)

unguarded :: [(Bool, [Pat H])] -> [[Pat H]]
unguarded rows = [r | (False, r) <- rows]

usefulnessTests :: TestTree
usefulnessTests =
  testGroup
    "usefulness"
    [ testProperty "a value is unmatched exactly when a witness holds it" $
        \(Case tys rows) ->
          let rs = unguarded rows
              ws = missing sig maxBound (length tys) rs
           in all
                (\vs -> not (any (`rowMatches` vs) rs) == any (`vectorMatches` vs) ws)
                (vectors tys)
    , testProperty "no value is held by two witnesses" $
        \(Case tys rows) ->
          let ws = missing sig maxBound (length tys) (unguarded rows)
           in all (\vs -> length (filter (`vectorMatches` vs) ws) <= 1) (vectors tys)
    , testProperty "a limit keeps the first witnesses" $
        \(Case tys rows) (Small k) ->
          let rs = unguarded rows
           in missing sig k (length tys) rs === take k (missing sig maxBound (length tys) rs)
    , testProperty "a useful witness holds a value no row matches" $
        \(Case tys rows) ->
          let rs = unguarded rows
           in case useful sig rs (map (const Wild) tys) of
                Nothing -> property (all (\vs -> any (`rowMatches` vs) rs) (vectors tys))
                Just w -> property (any (\vs -> vectorMatches w vs && not (any (`rowMatches` vs) rs)) (vectors tys))
    , testProperty "a row is redundant exactly when earlier rows match all it matches" $
        \(Case tys rows) ->
          let earlier i = unguarded (take i rows)
              dead i r = all (\vs -> any (`rowMatches` vs) (earlier i)) (filter (rowMatches r) (vectors tys))
           in redundant sig rows === [i | (i, (_, r)) <- zip [0 ..] rows, dead i r]
    , testCase "nested list patterns are exhaustive" $
        -- Cons x (Cons y rest) | Cons x Nil | Nil, over a list modelled as
        -- Opt (Pair elem list)
        let cons p q = Con HSome [Con HPair [p, q]]
            nil = Con HNone []
         in missing sig 1 1 [[cons Wild (cons Wild Wild)], [cons Wild nil], [nil]] @?= []
    , testCase "a clause after a catch-all is redundant" $
        redundant sig [(False, [Con HTrue []]), (False, [Wild]), (False, [Con HFalse []])] @?= [2]
    , testCase "the missing constructor is the witness" $
        missing sig 3 1 [[Con HSome [Wild]]] @?= [[WCon HNone []]]
    , testCase "an unbounded column names the other values and the uncovered heads" $
        missing sig 4 2 [[Con (HLit 1) [], Con HTrue []]]
          @?= [[Other [HLit 1], Any], [WCon (HLit 1) [], WCon HFalse []]]
    , testCase "a complete absent list is complete" $
        missing (sig {coverage = const (Absent [])}) 4 1 [[Con HTrue []]] @?= []
    ]
