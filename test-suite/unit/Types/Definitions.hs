{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Definitions
  ( whitespaceTests
  , whereTests
  , orderInvarianceTests
  , signatureContractTests
  , constraintContractTests
  , definitionLadderTests
  , infixOperatorTests
  , accessorInWhereTests
  , definitionArityTests
  , letBindingTests
  ) where

import Morloc.Frontend.Namespace
import Text.RawString.QQ

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Types.Common

whitespaceTests :: TestTree
whitespaceTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Tests whitespace handling for modules"
      [ assertGeneralType
          "module indent == 1 and top indent == module indent"
          "module foo (y)\nx = 1\ny = 2"
          int
      , assertGeneralType
          "module indent == 1 and top indent > module indent"
          "module foo (y)\n  x = 1\n  y = 2"
          int
      , assertGeneralType
          "module indent > 1 and top indent > module indent"
          " module foo (y)\n   x = 1\n   y = 2"
          int
      , assertGeneralType
          "module indent > 1 and top indent = module indent"
          "  module foo (y)\n  x = 1\n  y = 2"
          int
      , -- indenting main
        assertGeneralType
          "main indent == 1"
          "module main (y)\nx = 1\ny = 2"
          int
      , assertGeneralType
          "main indent > 1"
          "module main (y)\n  x = 1\n  y = 2"
          int
      , -- multiple modules
        assertGeneralType
          "multiple modules at pos 1 with pos > 1 exprs"
          [r|
module foo (x)
  x = True
module bar (y)
  import .foo
  y = True
module main (z)
  import .bar
  z = 1
      |]
          int
      ]

whereTests :: TestTree
whereTests =
  localOption (mkTimeout 2000000) $
    testGroup
      "Test of where statements"
      [ -- A `where` binding is a declaration, not a local: it is elaborated
        -- afresh at each use site, so one unsigned binding may serve two
        -- types. A `let` pins a single type, so lowering a `where` into one
        -- loses this.
        assertGeneralType
          "polymorphic where-binding applied at two types"
          [r|
            id :: a -> a
            f = (me 1, me "a") where
                me = id
            f
        |]
          (AppU (VarU (TV "Tuple2")) [int, str])
      , assertGeneralType
          "polymorphic where-binding applied at two container types"
          [r|
            id :: a -> a
            f = (me [1], me ["a"]) where
                me = id
            f
        |]
          (AppU (VarU (TV "Tuple2")) [lst int, lst str])
      , -- A where-binding's scope covers its siblings, not only the body.
        -- Here `unused` is the only reader of `g` and the body never names
        -- it; `g` must still resolve.
        assertGeneralType
          "where-binding read only by an unread sibling"
          [r|
            inc :: Int -> Int
            f = g 1 where
                g y = inc y
                unused = g 2
            f
        |]
          int
      , assertGeneralType
          "chain of where-bindings behind a single reader"
          [r|
            inc :: Int -> Int
            f = c where
                a = 1
                b = inc a
                c = inc b
            f
        |]
          int
      , assertGeneralType
          "simple where"
          [r|
            f :: Int
            f = z where
                z = 42
            f
        |]
          int
      , assertGeneralType
          "calling simple where"
          [r|
            inc :: Int -> Int
            f = inc z where
                z = 42
            f
        |]
          int
      , assertGeneralType
          "calling deeper where"
          [r|
            id :: a -> a
            inc :: Int -> Int
            f = id z where
                z = inc y where
                  y = 42
            f
        |]
          int
      , -- Names bound by a parameter pattern are in scope in the where-block,
        -- exactly as a plain parameter is.
        assertGeneralType
          "tuple-pattern parameters are visible in where"
          [r|
            add :: Int -> Int -> Int
            f :: (Int, Int) -> Int
            f (a, b) = g where
                g = add a b
            f
        |]
          (fun [tuple [int, int], int])
      , -- A where-binding is a pure value: running an effect in it has no
        -- do-block to sequence it in.
        expectError
          "where-binding right-hand side cannot force an effect"
          [r|
           module main (f)
           effect IO
           readIt :: Int -> <IO> Int
           addInt :: Int -> Int -> Int
           f :: Int -> <IO> Int
           f n = do
             addInt x 1
             where
               x = !(readIt n)
        |]
      ]

orderInvarianceTests :: TestTree
orderInvarianceTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Test order invariance"
      [ assertGeneralType
          "definitions work"
          "x = 42\nx"
          int
      , assertGeneralType
          "terms may be defined before they are used"
          "y = 42\nx = y\nx"
          int
      , assertGeneralType
          "long chains of substitution are OK too"
          "z = 42\ny = z\nx = y\nx"
          int
      ]

-- A signature is a contract on the definition: its type variables are rigid,
-- so a body that only works at some instance of them is rejected, whether the
-- term is used at that instance, used elsewhere, or not used at all.
--
signatureContractTests :: TestTree
signatureContractTests =
  localOption (mkTimeout 2000000) $
    testGroup
      "Signatures are contracts"
      [ expectError
          "a body cannot fix a length its signature leaves free"
          [r|
module main (f)
sum3 :: Vector 3 Real -> Real
f :: Vector n Real -> Real
f v = sum3 v
|]
      , expectError
          "a body cannot equate two lengths its signature keeps apart"
          [r|
module main (f)
dot :: Vector n Real -> Vector n Real -> Real
f :: Vector n Real -> Vector m Real -> Real
f a b = dot a b
|]
      , expectPass
          "a length-polymorphic body passes its length through"
          [r|
module main (f)
inc :: Real -> Real
vmap :: (a -> b) -> Vector n a -> Vector n b
f :: Vector n Real -> Vector n Real
f v = vmap inc v
|]
      , expectError
          "a body cannot fix an effect its signature leaves free"
          [r|
module main (f)
effect IO
ioOp :: Int -> <IO> Int
f :: Int -> <e> Int
f x = ioOp x
|]
      , expectPass
          "an effect-polymorphic body passes its effect through"
          [r|
module main (f)
apply :: (Int -> <e> Int) -> Int -> <e> Int
f :: (Int -> <e> Int) -> Int -> <e> Int
f g x = apply g x
|]
      , expectError
          "a local signature's own type variable is not the enclosing one"
          [r|
module main (f)
pair :: a -> a -> [a]
f :: a -> [a]
f x = g x
  where
    g :: b -> [b]
    g y = pair y x
|]
      , expectError
          "a local signature's type variable is its own, whatever its name (POLY-4)"
          [r|
module main (f)
pair :: a -> a -> [a]
f :: a -> [a]
f x = g x
  where
    g :: a -> [a]
    g y = pair y x
|]
      , expectError
          "body narrower than signature, used at the narrow type"
          [r|
           module main (f)
           addInt :: Int -> Int -> Int
           bad :: a -> a
           bad x = addInt x 1
           f :: Int
           f = bad 5
        |]
      , expectError
          "body narrower than signature, never used"
          [r|
           module main (f)
           addInt :: Int -> Int -> Int
           bad :: a -> a
           bad x = addInt x 1
           f :: Int
           f = 5
        |]
      , expectError
          "body narrower than signature, exported"
          [r|
           module main (bad)
           addInt :: Int -> Int -> Int
           bad :: a -> a
           bad x = addInt x 1
        |]
      , expectError
          "body returns the wrong type variable"
          [r|
           module main (f)
           swapBad :: a -> b -> a
           swapBad x y = y
           f :: Int
           f = swapBad 1 2
        |]
      , -- a recursive call instantiates the signature afresh
        expectPass
          "recursive definition checked against its polymorphic signature"
          [r|
           module main (f)
           le :: Int -> Int -> Bool
           sub :: Int -> Int -> Int
           keep :: a -> Int -> a
           keep x n
             ? le n 0 = x
             : keep x (sub n 1)
           f :: Int -> Str
           f n = keep "a" n
        |]
      , assertGeneralType
          "body as general as its signature, used at two types"
          [r|
           good :: a -> a
           good x = x
           (good 1, good "a")
        |]
          (AppU (VarU (TV "Tuple2")) [int, str])
      ]

-- A signature's class constraints are part of its contract: each names the
-- type it constrains, the body's method uses must follow from them (a
-- superclass follows from its subclass), and an instance exists only where
-- its superclass instances exist.
constraintContractTests :: TestTree
constraintContractTests =
  localOption (mkTimeout 2000000) $
    testGroup
      "Constraints are contracts"
      [ expectError
          "an instance body using a method at a type its context does not cover"
          [r|
module main (f)
class Sz a where
  sz :: a -> Int
add :: Int -> Int -> Int
fold :: (b -> a -> b) -> b -> [a] -> b
instance Sz Int where
  sz x = 1
instance Sz (List a) where
  sz xs = fold (\acc x -> add acc (sz x)) 0 xs
f :: [Int] -> Int
f xs = sz xs
|]
      , expectPass
          "an instance body using a method at a type its context covers"
          [r|
module main (f)
class Sz a where
  sz :: a -> Int
add :: Int -> Int -> Int
fold :: (b -> a -> b) -> b -> [a] -> b
instance Sz Int where
  sz x = 1
instance Sz a => Sz (List a) where
  sz xs = fold (\acc x -> add acc (sz x)) 0 xs
f :: [Int] -> Int
f xs = sz xs
|]
      , expectPass
          "an instance context entails its superclasses"
          [r|
module main (f)
class Eqq a where
  eqq :: a -> a -> Bool
class Eqq a => Ordd a where
  lte :: a -> a -> Bool
class Sm a where
  same :: a -> a -> Bool
instance Eqq Int where
  eqq x y = True
instance Ordd Int where
  lte x y = True
instance Sm Int where
  same x y = True
pick :: [a] -> a
instance (Ordd a) => Sm (List a) where
  same xs ys = eqq (pick xs) (pick ys)
f :: [Int] -> Bool
f xs = same xs xs
|]
      , expectError
          "class constraint without a type argument"
          [r|
           module main (f)
           class Ord a where
             (<=) :: a -> a -> Bool
           instance Ord Int where
             (<=) x y = True
           lt :: Ord => a -> a -> Bool
           lt x y = x <= y
           f :: Int -> Bool
           f x = lt x x
        |]
      , expectError
          "body uses a method its signature does not constrain"
          [r|
           module main (f)
           class Eq a where
             (==) :: a -> a -> Bool
           instance Eq Int where
             (==) x y = True
           same :: a -> a -> Bool
           same x y = x == y
           f :: Int -> Bool
           f x = same x x
        |]
      , expectError
          "instance without an instance of its superclass"
          [r|
           module main (f)
           class Sg a where
             cat :: a -> a -> a
           class Sg a => Mn a where
             emp :: a
           instance Sg Str where
             cat x y = x
           instance Mn Int where
             emp = 0
           f :: Int -> Int
           f x = emp
        |]
      , -- each constraint of a parenthesized list is one given
        expectPass
          "method of the second of two declared constraints"
          [r|
           module main (f)
           class Sz a where
             size :: a -> Int
           class Eq a where
             (==) :: a -> a -> Bool
           instance Sz Int where
             size x = 1
           instance Eq Int where
             (==) x y = True
           same :: (Sz a, Eq a) => a -> a -> Bool
           same x y = x == y
           f :: Int -> Bool
           f x = same x x
        |]
      , -- a literal inhabits a rigid variable only under a numeric class
        expectPass
          "integer literal at a variable constrained by Integral"
          [r|
           module main (f)
           class Integral a where
             (+) :: a -> a -> a
           instance Integral Int where
             (+) x y = x
           inc :: Integral a => a -> a
           inc x = x + 1
           f :: Int -> Int
           f x = inc x
        |]
      , expectError
          "integer literal at an unconstrained variable"
          [r|
           module main (f)
           one :: a -> a
           one x = 1
           f :: Int -> Int
           f x = one x
        |]
      , expectPass
          "superclass method used under the subclass constraint"
          [r|
           module main (f)
           class Sg a where
             cat :: a -> a -> a
           class Sg a => Mn a where
             emp :: a
           instance Sg Int where
             cat x y = x
           instance Mn Int where
             emp = 0
           padded :: Mn a => a -> a
           padded x = cat x emp
           f :: Int -> Int
           f x = padded x
        |]
      ]

-- A definition is checked once, however many times it is named. In the
-- ladder every level names the one below it twice, so any per-use
-- re-elaboration costs 2^depth; checking each definition once is linear.
--
definitionLadderTests :: TestTree
definitionLadderTests =
  localOption (mkTimeout 500000) $
    testGroup
      "Definitions are checked once"
      [ expectPass "ladder of definitions each naming the one below twice" (definitionLadder 8)
      , expectError "recursion through two definitions at a growing type is rejected"
          [r|
module main (start)
f :: a -> Int
g :: [a] -> Int
f x = g [x]
g xs = f xs
start :: Int
start = f 1
|]
      , expectPass "a record constraint passes through two layers of definitions"
          [r|
module main (outer)
type TBox (r :: Rec)
hasZ :: (Subset (ListToSet ['z]) (Keys r)) => TBox r -> TBox r
mid :: (Subset (ListToSet ['z]) (Keys r)) => TBox r -> TBox r
mid t = hasZ t
outer :: (Subset (ListToSet ['z]) (Keys r)) => TBox r -> TBox r
outer t = mid t
|]
      , expectPass "mutual recursion over a list of a type variable"
          [r|
module main (evenLen)
isNil :: [a] -> Bool
rest :: [a] -> [a]
evenLen :: [a] -> Bool
oddLen :: [a] -> Bool
evenLen xs = ? isNil xs = True : oddLen (rest xs)
oddLen xs = ? isNil xs = False : evenLen (rest xs)
|]
      , expectPass "mutual recursion over a vector of any length"
          [r|
module main (start)
dec :: Int -> Int
isZero :: Int -> Bool
evenV :: Int -> Vector n Int -> Bool
oddV :: Int -> Vector n Int -> Bool
evenV k v = ? isZero k = True : oddV (dec k) v
oddV k v = ? isZero k = False : evenV (dec k) v
start :: Int -> Vector n Int -> Bool
start k v = evenV k v
|]
      , expectError "two definitions fixing one length variable differently are rejected"
          [r|
module main (both)
add :: Real -> Real -> Real
sum3 :: Vector 3 Real -> Real
sum4 :: Vector 4 Real -> Real
f :: Vector n Real -> Real
f v = sum3 v
g :: Vector n Real -> Real
g v = sum4 v
both :: Vector n Real -> Real
both v = add (f v) (g v)
|]
      , expectPass "a subclass of Numeric admits a real literal"
          [r|
module main (half)
class Integral a where
  add :: a -> a -> a
  mul :: a -> a -> a
class Integral a => Numeric a where
  divide :: a -> a -> a
class Numeric a => Fancy a where
  fancy :: a -> a
instance Integral Real
instance Numeric Real
instance Fancy Real
half :: Fancy a => a -> a
half x = mul x 0.5
|]
      , expectPass "recursion through two definitions at one type"
          [r|
module main (start)
dec :: Int -> Int
dec n = n
isZero :: Int -> Bool
isZero n = True
f :: Int -> Int
g :: Int -> Int
f x = g (dec x)
g x = f (dec x)
start :: Int
start = f 1
|]
      ]

{- | Tests for infix operator functionality
All tests have a 1-second timeout to prevent infinite loops
-}
infixOperatorTests :: TestTree
infixOperatorTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout in microseconds
    testGroup
      "Infix operator tests"
      [ -- Basic precedence tests
        assertGeneralType
          "default precedence: multiplication before addition"
          [r|
          infixl 6 +
          infixl 7 *
          (+) :: Int -> Int -> Int
          (+) x y = x
          (*) :: Int -> Int -> Int
          (*) x y = y
          z = 1 + 2 * 3
          z
        |]
          int
      , assertGeneralType
          "custom precedence: higher binds tighter"
          [r|
          infixl 3 #
          infixl 8 @
          (#) :: Int -> Int -> Int
          (#) x y = x
          (@) :: Int -> Int -> Int
          (@) x y = y
          x = 1 # 2 @ 3
          x
        |]
          int
      , -- Associativity tests
        assertGeneralType
          "left associative operators"
          [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = 1 + 2 + 3
          x
        |]
          int
      , assertGeneralType
          "right associative operators"
          [r|
          infixr 5 ++
          (++) :: [Int] -> [Int] -> [Int]
          (++) xs ys = xs
          x = [1] ++ [2] ++ [3]
          x
        |]
          (lst int)
      , -- Operators in prefix position
        assertGeneralType
          "operator used prefix"
          [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = (+) 1 2
          x
        |]
          int
      , assertGeneralType
          "operator in lambda"
          [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          f :: Int -> Int -> Int
          f = \x y -> x + y
          f
        |]
          (fun [int, int, int])
      , -- Default precedence tests
        assertGeneralType
          "default * has precedence 7"
          [r|
          infixl 6 +
          (*) :: Int -> Int -> Int
          (*) x y = y
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = 1 + 2 * 3
          x
        |]
          int
      , assertGeneralType
          "default + has precedence 6"
          [r|
          infixl 7 *
          (*) :: Int -> Int -> Int
          (*) x y = y
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = 1 + 2 * 3
          x
        |]
          int
      , -- Multiple operators in one declaration
        assertGeneralType
          "multiple operators same fixity"
          [r|
          infixl 6 +, -
          (+) :: Int -> Int -> Int
          (-) :: Int -> Int -> Int
          x = 1 + 2 - 3
          x
        |]
          int
      , -- Polymorphic operators
        assertGeneralType
          "polymorphic operator"
          [r|
          infixl 9 .
          infixl 6 +
          infixr 0 $
          (.) :: (b -> c) -> (a -> b) -> a -> c
          ($) :: (a -> b) -> a -> b
          (+) :: Int -> Int -> Int
          show :: a -> Str
          x = show . (+) 9 $ 5
          x
        |]
          str
      , assertGeneralType
          "polymorphic list append"
          [r|
          infixl 6 ++
          (++) :: [a] -> [a] -> [a]
          (++) xs ys = xs
          x = [1] ++ [2]
          x
        |]
          (lst int)
      , -- Complex expressions
        assertGeneralType
          "nested operations with parens"
          [r|
          infixl 6 +
          infixl 7 *
          (+) :: Int -> Int -> Int
          (+) x y = x
          (*) :: Int -> Int -> Int
          (*) x y = y
          x = (1 + 2) * (3 + 4)
          x
        |]
          int
      , -- Operators in different contexts
        assertGeneralType
          "operator in where clause"
          [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = y + z where
            y = 1
            z = 2
          x
        |]
          int
      , assertGeneralType
          "operator in list"
          [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          xs = [1 + 2, 3 + 4]
          xs
        |]
          (lst int)
      , assertGeneralType
          "operator in tuple"
          [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = (1 + 2, "hi")
          x
        |]
          (AppU (VarU (TV "Tuple2")) [int, str])
      , -- Edge cases
        assertGeneralType
          "operator precedence 0 (lowest)"
          [r|
          infixr 0 $
          ($) :: (Int -> Int) -> Int -> Int
          ($) f x = f x
          g :: Int -> Int
          x = g $ 5
          x
        |]
          int
      , assertGeneralType
          "operator precedence 9 (highest)"
          [r|
          infixl 9 !!!
          (!!!) :: Int -> Int -> Int
          (!!!) x y = x
          x = 1 !!! 2
          x
        |]
          int
      , -- Operators with both parens and bare syntax in fixity decls
        assertGeneralType
          "fixity with parentheses"
          [r|
          infixl 6 (+)
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = 1 + 2
          x
        |]
          int
      , assertGeneralType
          "fixity without parentheses"
          [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          (+) x y = x
          x = 1 + 2
          x
        |]
          int
      , -- Type-verified precedence: asymmetric operator types ensure
        -- only the correct parse tree typechecks
        assertGeneralType
          "type-verified: * at 7 binds tighter than + at 6"
          [r|
          infixl 6 +
          infixl 7 *
          (+) :: Str -> Int -> Str
          (*) :: Int -> Int -> Int
          x = "a" + 1 * 2
          x
        |]
          str
      , assertGeneralType
          "type-verified: @ at 8 binds tighter than # at 3"
          [r|
          infixl 3 #
          infixl 8 @
          (#) :: Str -> Int -> Str
          (@) :: Int -> Int -> Int
          x = "a" # 1 @ 2
          x
        |]
          str
      , assertGeneralType
          "type-verified: three-operator precedence chain"
          [r|
          infixl 3 <$>
          infixl 6 +
          infixl 9 *
          (<$>) :: Str -> Str -> Int
          (+) :: Str -> Int -> Str
          (*) :: Int -> Int -> Int
          x = "a" <$> "b" + 1 * 2
          x
        |]
          int
      , -- Type-verified associativity: asymmetric operator types ensure
        -- only the correct associativity typechecks
        assertGeneralType
          "type-verified: left-assoc chain"
          [r|
          infixl 6 +
          (+) :: Int -> Str -> Int
          x = 1 + "a" + "b"
          x
        |]
          int
      , assertGeneralType
          "type-verified: right-assoc chain"
          [r|
          infixr 5 ++
          (++) :: Str -> Int -> Int
          x = "a" ++ "b" ++ 1
          x
        |]
          int
      , -- Application operator ($)
        assertGeneralType
          "$ applies function to argument"
          [r|
          infixr 0 $
          ($) :: (a -> b) -> a -> b
          f :: Int -> Str
          x = f $ 1
          x
        |]
          str
      , assertGeneralType
          "nested $ is right-associative (type-verified)"
          [r|
          infixr 0 $
          ($) :: (a -> b) -> a -> b
          f :: Int -> Str
          g :: Str -> Int
          x = g $ f $ 1
          x
        |]
          int
      , assertGeneralType
          "$ binds looser than + (type-verified)"
          [r|
          infixr 0 $
          infixl 6 +
          ($) :: (a -> b) -> a -> b
          (+) :: Int -> Int -> Int
          f :: Int -> Str
          x = f $ 1 + 2
          x
        |]
          str
      , -- Composition operator (.)
        assertGeneralType
          "composition of two functions"
          [r|
          infixr 9 .
          (.) :: (b -> c) -> (a -> b) -> a -> c
          g :: Str -> Int
          f :: Int -> Str
          x = g . f
          x
        |]
          (fun [int, int])
      , assertGeneralType
          "composition chain of three functions"
          [r|
          infixr 9 .
          (.) :: (b -> c) -> (a -> b) -> a -> c
          h :: Str -> Int
          g :: Int -> Str
          f :: Bool -> Int
          x = h . g . f
          x
        |]
          (fun [bool, int])
      , assertGeneralType
          "composition binds tighter than $ (type-verified)"
          [r|
          infixr 9 .
          infixr 0 $
          (.) :: (b -> c) -> (a -> b) -> a -> c
          ($) :: (a -> b) -> a -> b
          f :: Int -> Int
          g :: Int -> Str
          x = g . f $ 5
          x
        |]
          str
      , -- Position independence of fixity declarations
        assertGeneralType
          "fixity declared after usage"
          [r|
          (+) :: Int -> Str -> Int
          x = 1 + "a"
          infixl 6 +
          x
        |]
          int
      , assertGeneralType
          "fixity and type sig both declared after usage"
          [r|
          x = 1 + "a"
          infixl 6 +
          (+) :: Int -> Str -> Int
          x
        |]
          int
      , assertGeneralType
          "both fixities at end, precedence still works"
          [r|
          (+) :: Str -> Int -> Str
          (*) :: Int -> Int -> Int
          x = "a" + 1 * 2
          infixl 6 +
          infixl 7 *
          x
        |]
          str
      , -- Default fixity is infixl 9
        assertGeneralType
          "undeclared operator defaults to prec 9 (type-verified)"
          [r|
          infixl 6 +
          (+) :: Str -> Int -> Str
          (*) :: Int -> Int -> Int
          x = "a" + 1 * 2
          x
        |]
          str
      , assertGeneralType
          "undeclared operator defaults to left-associative (type-verified)"
          [r|
          (+) :: Int -> Str -> Int
          x = 1 + "a" + "b"
          x
        |]
          int
      , -- Where-clause bindings with infix operators
        assertGeneralType
          "infix operator in where binding (type-verified)"
          [r|
          infixl 6 +
          (+) :: Int -> Str -> Int
          x = y where
            y = 1 + "a"
          x
        |]
          int
      , assertGeneralType
          "multiple where bindings with different operators"
          [r|
          infixl 6 +
          infixl 7 *
          (+) :: Str -> Int -> Str
          (*) :: Int -> Int -> Int
          x = (y, z) where
            y = "hello" + 3
            z = 2 * 4
          x
        |]
          (tuple [str, int])
      , -- Ambiguity and conflict errors
        exprTestBad
          "non-associative operator chained"
          [r|
          infix 6 ~~
          (~~) :: Int -> Int -> Int
          x = 1 ~~ 2 ~~ 3
          x
        |]
      , exprTestBad
          "two non-associative operators at same precedence"
          [r|
          infix 6 ~~
          infix 6 @@
          (~~) :: Int -> Int -> Int
          (@@) :: Int -> Int -> Int
          x = 1 ~~ 2 @@ 3
          x
        |]
      , exprTestBad
          "left-assoc and right-assoc at same precedence"
          [r|
          infixl 6 +
          infixr 6 ++
          (+) :: Int -> Int -> Int
          (++) :: Int -> Int -> Int
          x = 1 + 2 ++ 3
          x
        |]
      , exprTestBad
          "conflicting fixity declarations for same operator"
          [r|
          infixl 6 +
          infixr 7 +
          (+) :: Int -> Int -> Int
          x = 1 + 2
          x
        |]
      , -- Operators in various expression contexts
        assertGeneralType
          "infix in parenthesized function argument"
          [r|
          infixl 6 +
          (+) :: Int -> Int -> Int
          f :: Int -> Str
          x = f (1 + 2)
          x
        |]
          str
      , assertGeneralType
          "infix in multiple function arguments"
          [r|
          infixl 6 +
          infixl 7 *
          (+) :: Int -> Int -> Int
          (*) :: Int -> Int -> Int
          f :: Int -> Int -> Str
          x = f (1 + 2) (3 * 4)
          x
        |]
          str
      , assertGeneralType
          "infix expressions as applied functions"
          [r|
          infixl 6 +
          infixl 7 *
          infixr 9 .
          (+) :: Int -> Int -> Int
          (*) :: Int -> Int -> Int
          (.) :: (b -> c) -> (a -> b) -> a -> c
          foo x = ((+) 1 . (*) 2) x
          foo
        |]
          (fun [int, int])
      , assertGeneralType
          "infix in lambda body with asymmetric types"
          [r|
          infixl 6 +
          (+) :: Int -> Str -> Int
          f = \x y -> x + y
          f
        |]
          (fun [int, str, int])
      , assertGeneralType
          "infix across tuple elements with different result types"
          [r|
          infixl 6 +
          infixl 7 *
          (+) :: Int -> Int -> Str
          (*) :: Int -> Int -> Int
          x = (1 + 2, 3 * 4)
          x
        |]
          (tuple [str, int])
      , assertGeneralType
          "mixed infix operators across list elements"
          [r|
          infixl 6 +
          infixl 7 *
          (+) :: Int -> Int -> Int
          (*) :: Int -> Int -> Int
          xs = [1 + 2, 3 * 4, 5 + 6 * 7]
          xs
        |]
          (lst int)
      , -- C7: infix precedence must be in [0,9]
        exprTestBad
          "infixl negative precedence is rejected"
          [r|
          module main (z)
          infixl -1 +
          (+) :: Int -> Int -> Int
          z = 1 + 2
        |]
      , exprTestBad
          "infixr negative precedence is rejected"
          [r|
          module main (z)
          infixr -3 +
          (+) :: Int -> Int -> Int
          z = 1 + 2
        |]
      , exprTestBad
          "infix (non-associative) negative precedence is rejected"
          [r|
          module main (z)
          infix -1 +
          (+) :: Int -> Int -> Int
          z = 1 + 2
        |]
      , exprTestBad
          "infix precedence above 9 is rejected"
          [r|
          module main (z)
          infixl 10 +
          (+) :: Int -> Int -> Int
          z = 1 + 2
        |]
      , exprTestBad
          "infix precedence far above 9 is rejected"
          [r|
          module main (z)
          infixl 100 +
          (+) :: Int -> Int -> Int
          z = 1 + 2
        |]
      ]

{- | C3: Record literal field order is by-name. Permuted literals must
typecheck against the declared record type, while literals with mismatched
key sets (missing / unknown fields) must be rejected.
-}
-- A tuple accessor inside an unannotated `where` helper. The helper's
-- parameter existential is solved to another existential before that one
-- goes ground, and the slot constraints from the accessor have to survive
-- the chain. When they did not, the slot type was never checked: a Str
-- slot could be returned where a Real was declared, and the program only
-- failed at run time inside the pool.
accessorInWhereTests :: TestTree
accessorInWhereTests =
  localOption (mkTimeout 2000000) $
    testGroup
      "A tuple accessor in an unannotated where binding"
      [ expectPass
          "an accessor whose slot type matches the signature"
          [r|
module main (top)
mapL :: (a -> b) -> [a] -> [b]
counts :: [(Str, Int)]
top :: [Int]
top = mapL score counts
  where
    score p = .1 p
|]
      , exprTestBad
          "an accessor whose slot type contradicts the signature"
          [r|
module main (top)
mapL :: (a -> b) -> [a] -> [b]
counts :: [(Str, Int)]
top :: [Real]
top = mapL score counts
  where
    score p = .0 p
|]
      , exprTestBad
          "the other slot, contradicted the other way"
          [r|
module main (top)
mapL :: (a -> b) -> [a] -> [b]
counts :: [(Str, Int)]
top :: [Str]
top = mapL score counts
  where
    score p = .1 p
|]
      , expectPass
          "an accessor result passed on to a second unannotated helper"
          [r|
module main (top)
mapL :: (a -> b) -> [a] -> [b]
counts :: [(Str, Int)]
top :: [Int]
top = mapL score counts
  where
    score p = bump (.1 p)
    bump c = c
|]
      ]

{- | Tests for typechecker complexity - these would timeout with O(2^n) behavior
All tests have a 0.1-second timeout to catch exponential blowup
-}
-- | Arity agreement between a signature and its definition.
--
-- Two behaviours are covered, and both were broken by the same defect: the
-- lambda-vs-'FunU' check compared the lambda's parameter count against the
-- OUTERMOST argument list of the expected type, then handed the difference to
-- 'expand'. A curried signature such as @Int -> (Int -> Int)@ presents one
-- outer argument, so a legal two-parameter definition looked over-applied and
-- 'expand' was called with a negative count, which appends a parameter and
-- recurses without a base case. The compiler never returned.
--
-- The timeout on this group is the real assertion for the negative cases: a
-- regression does not produce a wrong type, it diverges. Every case here
-- completes in milliseconds once the check normalizes the expected type and
-- rejects a genuine over-application.
definitionArityTests :: TestTree
definitionArityTests =
  localOption (mkTimeout 2000000) $ -- 2 second timeout; the divergence guard
    testGroup
      "Definition arity"
      [ -- Curried spellings must accept a definition of the flattened arity.
        assertGeneralType
          "curried signature, two parameters"
          [r|
          foo :: Int -> (Int -> Int)
          foo x y = x
          foo
        |]
          (fun [int, int, int])
      , assertGeneralType
          "curried signature, three parameters"
          [r|
          foo :: Int -> (Int -> (Int -> Int))
          foo x y z = x
          foo
        |]
          (fun [int, int, int, int])
      , assertGeneralType
          "curried in the middle"
          [r|
          foo :: Int -> (Int -> Int) -> Int
          foo x f = f x
          foo
        |]
          (fun [int, fun [int, int], int])
      , assertGeneralType
          "curried result whose argument is itself a function"
          [r|
          foo :: (Int -> Int) -> (Int -> Int)
          foo f x = f x
          foo
        |]
          (fun [fun [int, int], int, int])
      , -- Flat spellings keep working, including under-application (which
        -- eta-expands rather than erroring).
        assertGeneralType
          "flat signature, matching arity"
          [r|
          foo :: Int -> Int -> Int
          foo x y = x
          foo
        |]
          (fun [int, int, int])
      , assertGeneralType
          "fewer parameters than the type: eta-expansion"
          [r|
          add :: Int -> Int -> Int
          foo :: Int -> Int -> Int
          foo = add
          foo
        |]
          (fun [int, int, int])
      , -- Genuine over-application is an error, and must terminate.
        expectError
          "one parameter too many"
          [r|
          foo :: Int -> Int
          foo x y = x
          foo
        |]
      , expectError
          "two parameters too many"
          [r|
          foo :: Int -> Int -> Int -> Int
          foo x y z w = x
          foo
        |]
      , expectError
          "too many, where the first argument is a function"
          [r|
          foo :: (Int -> Int) -> Int
          foo f x = f x
          foo
        |]
      , expectError
          "too many against a non-function result"
          [r|
          foo :: Int -> [Int]
          foo x y = [x]
          foo
        |]
      , expectError
          "too many, polymorphic signature"
          [r|
          foo :: a -> a
          foo x y = x
          foo
        |]
      ]

-- ============================================================
-- Let binding syntax tests
-- ============================================================

letBindingTests :: TestTree
letBindingTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Let binding syntax"
      [ -- === Regular let expressions ===
        assertGeneralType
          "single let binding"
          [r|
        module main (x)
        x = let y = 42 in y
          |]
          int

      , assertGeneralType
          "let with multiple bindings (omit repeated let)"
          [r|
        module main (x)
        x =
          let a = 1
              b = 2
          in b
          |]
          int

      , assertGeneralType
          "let with repeated let keywords"
          [r|
        module main (x)
        x =
          let a = 1
          let b = 2
          in b
          |]
          int

      , assertGeneralType
          "nested let expressions"
          [r|
        module main (x)
        x =
          let a = 1
          in let b = 2
             in b
          |]
          int

      , assertGeneralType
          "let binding used in body"
          [r|
        module main (x)
        add :: Int -> Int -> Int
        x =
          let a = 1
              b = 2
          in add a b
          |]
          int

      -- === Semicolon-delimited let (for inline / morloc run) ===

      , assertGeneralType
          "let with semicolons (inline form)"
          "module main (x)\nx = let { a = 1; b = 2 } in b"
          int

      -- === Do-block let bindings ===

      , assertGeneralType
          "do-block with single let"
          [r|
        module main (x)
        x = do
            let y = 42
            y
          |]
          (emptyEff int)

      , assertGeneralType
          "do-block with multi-binding let (omit repeated let)"
          [r|
        module main (x)
        x = do
            let a = 1
                b = 2
            b
          |]
          (emptyEff int)

      , assertGeneralType
          "do-block with separate let statements"
          [r|
        module main (x)
        x = do
            let a = 1
            let b = 2
            b
          |]
          (emptyEff int)

      , assertGeneralType
          "do-block let interleaved with bind"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        x = do
            let a = 1
            b <- f a
            let c = 2
            b
          |]
          (ioEff int)

      -- === Semicolon-delimited do-block (explicit braces) ===

      , assertGeneralType
          "do with explicit braces and semicolons"
          "module main (x)\nx = do { 42 }"
          (emptyEff int)

      , assertGeneralType
          "do with explicit braces, let, and semicolons"
          "module main (x)\nx = do { let a = 1; a }"
          (emptyEff int)

      , assertGeneralType
          "do with explicit braces, multiple lets"
          "module main (x)\nx = do { let a = 1; let b = 2; b }"
          (emptyEff int)
      ]
