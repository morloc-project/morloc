{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : UnitTypeTests
Description : Unit tests for type operations, subtyping, typechecking, and codegen
-}
module UnitTypeTests
  ( subtypeTests
  , substituteTVarTests
  , unitTypeTests
  , unitValuecheckTests
  , typeOrderTests
  , typeAliasTests
  , numericLiteralAliasTests
  , pendingNumLitTests
  , packerTests
  , whereTests
  , orderInvarianceTests
  , whitespaceTests
  , infixOperatorTests
  , recordLiteralOrderTests
  , complexityRegressionTests
  , effectSubtypeTests
  , effectSynthesisTests
  , effectErrorTests
  , evalSugarTests
  , effectEscapabilityTests
  , effectPartialApplicationTests
  , polymorphicEffectRowTests
  , catchRowInheritTests
  , effectCoverageMessageTests
  , namespaceErrorTests
  , typeclassTests
  , natErrorTests
  , natArithTests
  , natLabelTests
  , natKindPromotionTests
  , natDimTests
  , gradualDesugarTests
  , letBindingTests
  , irrefutablePatternTests
  , aliasConstructorTests
  , newtypeTests
  , literalDispatchTests
  , typedefKindVarTests
  , recursiveRecordTests
  , bidirectionalAppCheckTests
  , postArgPropagationTests
  , withDocstringTests
  ) where

import Morloc (typecheck, typecheckFrontend)
import Morloc.Frontend.Namespace
import Morloc.Frontend.Typecheck (evaluateAnnoSTypes)
import qualified Morloc.Monad as MM
import qualified Morloc.TypeEval as TE
import qualified Morloc.Typecheck.Internal as MTI
import qualified Morloc.Typecheck.NatSolver as NS
import qualified System.Directory as SD
import Text.RawString.QQ

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as MT
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit

-- get the toplevel general type of a typechecked expression
gtypeof :: AnnoS (Indexed TypeU) f c -> TypeU
gtypeof (AnnoS (Idx _ t) _ _) = t

runFront :: MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int])
runFront code = do
  config <- emptyConfig
  ((x, _), _) <-
    MM.runMorlocMonad
      Nothing
      0
      config
      defaultValue
      (typecheckFrontend Nothing (Code code) >>= mapM evaluateAnnoSTypes)
  return x

-- | Like runFront but without type alias evaluation, so nat dimensions are preserved.
runFrontRaw :: MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int])
runFrontRaw code = do
  config <- emptyConfig
  ((x, _), _) <-
    MM.runMorlocMonad
      Nothing
      0
      config
      defaultValue
      (typecheckFrontend Nothing (Code code))
  return x

runMiddle ::
  MT.Text ->
  IO
    ( Either
        MorlocError
        ( [AnnoS (Indexed Type) One ()]
        , [AnnoS (Indexed Type) One (Indexed Lang)]
        )
    )
runMiddle code = do
  config <- emptyConfig
  ((x, _), _) <- MM.runMorlocMonad Nothing 0 config defaultValue (typecheck Nothing (Code code))
  return x

emptyConfig :: IO Config
emptyConfig = do
  home <- SD.getHomeDirectory
  return $
    Config
      { configHome = home <> "/.local/share/morloc"
      , configLibrary = home <> "/.local/share/src/morloc"
      , configPlane = "default"
      , configPlaneCore = "morloclib"
      , configTmpDir = home <> "/.morloc/tmp"
      , configBuildConfig = home <> "/.morloc/.build-config.yaml"
      , configLangOverrides = mempty
      , configRegistry = Nothing
      }

assertGeneralType :: String -> MT.Text -> TypeU -> TestTree
assertGeneralType msg code t = testCase msg $ do
  result <- runFront code
  case result of
    (Right [x]) -> assertEqual "" (closeExistentials . MTI.cleanTypeName $ t) (closeExistentials . MTI.cleanTypeName . renameExistentials . gtypeof $ x)
    (Right _) -> error "Expected exactly one export from main for assertGeneralType"
    (Left e) ->
      error $
        "The following error was raised: " <> show e <> "\nin:\n" <> show code

renameExistentials :: TypeU -> TypeU
renameExistentials = snd . f (0 :: Int, Map.empty)
  where
    f s (VarU v) = (s, VarU v)
    f (i, m) (ExistU v (ps, pc) (rs, rc)) =
      case Map.lookup v m of
        (Just v') -> ((i, m), ExistU v' (ps, pc) (rs, rc))
        Nothing ->
          let v' = TV ("e" <> MT.pack (show i))
              i' = i + 1
              m' = Map.insert v v' m
              (s', ps') = statefulMap f (i', m') ps
              (s'', vs') = statefulMap f s' (map snd rs)
           in (s'', ExistU v' (ps', pc) (zip (map fst rs) vs', rc))
    f s (ForallU v t) =
      let (s', t') = f s t
       in (s', ForallU v t')
    f s t@(NatVarU _) = (s, t)
    f s (FunU ts t) =
      let (s', ts') = statefulMap f s ts
          (s'', t') = f s' t
       in (s'', FunU ts' t')
    f s (AppU t ts) =
      let (s', t') = f s t
          (s'', ts') = statefulMap f s' ts
       in (s'', AppU t' ts')
    f s (NamU o n vs rs) =
      let (s', ts') = statefulMap f s (map snd rs)
       in (s', NamU o n vs (zip (map fst rs) ts'))
    f s (EffectU effs t) =
      let (s', t') = f s t
       in (s', EffectU effs t')
    f s (OptionalU t) =
      let (s', t') = f s t
       in (s', OptionalU t')
    f s t@(NatLitU _) = (s, t)
    f s (NatAddU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', NatAddU a' b')
    f s (NatMulU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', NatMulU a' b')
    f s (NatSubU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', NatSubU a' b')
    f s (NatDivU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', NatDivU a' b')
    f s t@NatVoidU = (s, t)
    f s t@(StrVarU _) = (s, t)
    f s t@(StrLitU _) = (s, t)
    f s (StrConcatU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', StrConcatU a' b')
    f s t@StrVoidU = (s, t)
    f s t@(RecVarU _) = (s, t)
    f s t@RecEmptyU = (s, t)
    f s (RecExtendU k a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', RecExtendU k a' b')
    f s (RecUnionU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', RecUnionU a' b')
    f s (RecDiffU a ks) = let (s', a') = f s a in (s', RecDiffU a' ks)
    f s (RecIntersectU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', RecIntersectU a' b')
    f s (RecRestrictU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', RecRestrictU a' b')
    f s (RecDiffListU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', RecDiffListU a' b')
    f s t@RecVoidU = (s, t)
    f s t@(ListVarU _) = (s, t)
    f s (ListLitU es) = let (s', es') = statefulMap f s es in (s', ListLitU es')
    f s (ListAppU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', ListAppU a' b')
    f s t@ListVoidU = (s, t)
    f s t@(SetVarU _) = (s, t)
    f s t@SetEmptyU = (s, t)
    f s (SetLitU es) = let (s', es') = statefulMap f s es in (s', SetLitU es')
    f s (SetUnionU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', SetUnionU a' b')
    f s (SetInterU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', SetInterU a' b')
    f s (SetDiffU a b) = let (s', a') = f s a; (s'', b') = f s' b in (s'', SetDiffU a' b')
    f s t@SetVoidU = (s, t)
    f s (KeysU rec_) = let (s', r') = f s rec_ in (s', KeysU r')
    f s (ListToSetU l) = let (s', l') = f s l in (s', ListToSetU l')
    f s (SizeU c) = let (s', c') = f s c in (s', SizeU c')
    f s (ProjectFieldU rec_ fld) = let (s', r') = f s rec_; (s'', f') = f s' fld in (s'', ProjectFieldU r' f')
    f s (RecSingletonU k v) = let (s', k') = f s k; (s'', v') = f s' v in (s'', RecSingletonU k' v')
    f s (LabeledU n t) = let (s', t') = f s t in (s', LabeledU n t')

closeExistentials :: TypeU -> TypeU
closeExistentials = f
  where
    f (ExistU v (ts, _) (rs, _)) = ExistU v (map f ts, Closed) (map (second f) rs, Closed)
    f t@(VarU _) = t
    f t@(NatVarU _) = t
    f (ForallU v t) = ForallU v (f t)
    f (FunU ts t) = FunU (map f ts) (f t)
    f (AppU t ts) = AppU (f t) (map f ts)
    f (NamU o v ts rs) = NamU o v (map f ts) (map (second f) rs)
    f (EffectU effs t) = EffectU effs (f t)
    f (OptionalU t) = OptionalU (f t)
    f t@(NatLitU _) = t
    f (NatAddU a b) = NatAddU (f a) (f b)
    f (NatMulU a b) = NatMulU (f a) (f b)
    f (NatSubU a b) = NatSubU (f a) (f b)
    f (NatDivU a b) = NatDivU (f a) (f b)
    f t@NatVoidU = t
    f t@(StrVarU _) = t
    f t@(StrLitU _) = t
    f (StrConcatU a b) = StrConcatU (f a) (f b)
    f t@StrVoidU = t
    f t@(RecVarU _) = t
    f t@RecEmptyU = t
    f (RecExtendU k a b) = RecExtendU k (f a) (f b)
    f (RecUnionU a b) = RecUnionU (f a) (f b)
    f (RecDiffU a ks) = RecDiffU (f a) ks
    f (RecIntersectU a b) = RecIntersectU (f a) (f b)
    f (RecRestrictU a b) = RecRestrictU (f a) (f b)
    f (RecDiffListU a b) = RecDiffListU (f a) (f b)
    f t@RecVoidU = t
    f t@(ListVarU _) = t
    f (ListLitU es) = ListLitU (map f es)
    f (ListAppU a b) = ListAppU (f a) (f b)
    f t@ListVoidU = t
    f t@(SetVarU _) = t
    f t@SetEmptyU = t
    f (SetLitU es) = SetLitU (map f es)
    f (SetUnionU a b) = SetUnionU (f a) (f b)
    f (SetInterU a b) = SetInterU (f a) (f b)
    f (SetDiffU a b) = SetDiffU (f a) (f b)
    f t@SetVoidU = t
    f (KeysU rec_) = KeysU (f rec_)
    f (ListToSetU l) = ListToSetU (f l)
    f (SizeU c) = SizeU (f c)
    f (ProjectFieldU rec_ fld) = ProjectFieldU (f rec_) (f fld)
    f (RecSingletonU k v) = RecSingletonU (f k) (f v)
    f (LabeledU n t) = LabeledU n (f t)

-- | Assert the general type before alias evaluation (preserves nat dimensions).
assertRawType :: String -> MT.Text -> TypeU -> TestTree
assertRawType msg code t = testCase msg $ do
  result <- runFrontRaw code
  case result of
    (Right [x]) -> assertEqual "" (closeExistentials . MTI.cleanTypeName $ t) (closeExistentials . MTI.cleanTypeName . renameExistentials . gtypeof $ x)
    (Right _) -> error "Expected exactly one export from main for assertRawType"
    (Left e) ->
      error $
        "The following error was raised: " <> show e <> "\nin:\n" <> show code

assertSubtypeGamma :: String -> [GammaIndex] -> TypeU -> TypeU -> [GammaIndex] -> TestTree
assertSubtypeGamma msg gs1 a b gs2 = testCase msg $ do
  let g0 = listToGamma gs1
  case MTI.subtype Map.empty a b g0 of
    Left e -> error $ show e
    Right g -> assertEqual "" gs2 (MTI.gammaContextList g)

-- | Assert that a subtype check is rejected. Used to lock in negative
-- rules (e.g. effect narrowing) where the absence of an error would
-- previously have been a silent soundness hole.
assertSubtypeBad :: String -> [GammaIndex] -> TypeU -> TypeU -> TestTree
assertSubtypeBad msg gs a b = testCase msg $ do
  let g0 = listToGamma gs
  case MTI.subtype Map.empty a b g0 of
    Left _ -> return ()
    Right _ -> assertFailure $ "Expected subtype rejection for " <> show a <> " <: " <> show b

-- | Convert a list of GammaIndex (newest first) to a Gamma with IntMap.
-- Uses slot spacing of 256 to match production code.
listToGamma :: [GammaIndex] -> Gamma
listToGamma gs =
  let spacing = 256
      n = length gs
      -- Newest entry gets highest slot
      indexed = zip [spacing * (n - 1), spacing * (n - 2) .. 0] gs
      ctx = IntMap.fromList indexed
      existMap = Map.fromList [(v, s) | (s, ExistG v _ _) <- indexed]
  in Gamma
    { gammaCounter = 0
    , gammaSlot = spacing * n
    , gammaContext = ctx
    , gammaExist = existMap
    , gammaSolved = Map.empty
    , gammaDeferred = []
    , gammaKindSubs = Map.empty
    , gammaEffSubs = Map.empty
    , gammaIntVals = Map.empty
    , gammaConstraints = []
    , gammaAssumedConstraints = Nothing
    , gammaPendingNumLits = []
    }

exprTestBad :: String -> MT.Text -> TestTree
exprTestBad msg code =
  testCase msg $ do
    result <- runFront code
    case result of
      (Right _) -> assertFailure . MT.unpack $ "Expected '" <> code <> "' to fail"
      (Left _) -> return ()

valuecheckFail :: String -> MT.Text -> TestTree
valuecheckFail msg code =
  testCase msg $ do
    result <- runMiddle code
    case result of
      (Right _) -> assertFailure . MT.unpack $ "Expected '" <> code <> "' to fail"
      (Left _) -> return ()

valuecheckPass :: String -> MT.Text -> TestTree
valuecheckPass msg code =
  testCase msg $ do
    result <- runMiddle code
    case result of
      (Right _) -> return ()
      (Left _) -> assertFailure . MT.unpack $ "Expected '" <> code <> "' to pass"

-- Don't test the type of error message, that would incur too much fiddly
-- overhead as the messages and such are tweaked.
expectError :: String -> MT.Text -> TestTree
expectError msg code =
  testCase msg $ do
    result <- runFront code
    case result of
      (Right _) -> assertFailure . MT.unpack $ "Expected failure"
      (Left _) -> return ()

-- Asserts the frontend pipeline (parse, restructure, typecheck, alias
-- evaluation) completes successfully. Used when the structure of the
-- resulting type is uninteresting and we only care that no error was
-- raised and the pipeline did not diverge (the surrounding group's
-- localOption mkTimeout supplies the divergence guard).
expectPass :: String -> MT.Text -> TestTree
expectPass msg code =
  testCase msg $ do
    result <- runFront code
    case result of
      (Right _) -> return ()
      (Left e) -> assertFailure $ "Expected pass but got error: " <> show e

testEqual :: (Eq a, Show a) => String -> a -> a -> TestTree
testEqual msg x y =
  testCase msg $ assertEqual "" x y

testTrue :: String -> Bool -> TestTree
testTrue msg x =
  testCase msg $ assertEqual "" x True

testFalse :: String -> Bool -> TestTree
testFalse msg x =
  testCase msg $ assertEqual "" x False

bool :: TypeU
bool = VarU (TV "Bool")

real :: TypeU
real = VarU (TV "Real")

int :: TypeU
int = VarU (TV "Int")

str :: TypeU
str = VarU (TV "Str")

fun :: [TypeU] -> TypeU
fun [] = error "Cannot infer type of empty list"
fun [t] = FunU [] t
fun ts = FunU (init ts) (last ts)

forallu :: [MT.Text] -> TypeU -> TypeU
forallu ss t = foldr (\s -> ForallU (TV s)) t ss

exist :: MT.Text -> TypeU
exist v = ExistU (TV v) ([], Open) ([], Open)

existP v ts rs = ExistU (TV v) (ts, Open) (rs, Open)

var :: MT.Text -> TypeU
var s = VarU (TV s)

arr :: MT.Text -> [TypeU] -> TypeU
arr s = AppU (VarU (TV s))

lst :: TypeU -> TypeU
lst t = arr "List" [t]

tuple :: [TypeU] -> TypeU
tuple ts = AppU v ts
  where
    v = VarU . TV . MT.pack $ "Tuple" ++ show (length ts)

record' :: MT.Text -> [(Key, TypeU)] -> TypeU
record' n = NamU NamRecord (TV n) []

subtypeTests :: TestTree
subtypeTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Test subtype within context"
      [ -- basic general cases
        assertSubtypeGamma "G -| A <: A |- G" [] a a []
      , assertSubtypeGamma "<a>, <b> -| <a> <: <b> |- <a>:<b>, <b>" [eag, ebg] ea eb [solvedA eb, ebg]
      , assertSubtypeGamma "<a>, <b> -| <b> <: <a> |- <a>:<b>, <b>" [eag, ebg] ea eb [solvedA eb, ebg]
      , assertSubtypeGamma "G -| (A -> B) <: (A -> B) |- G" [] (fun [a, b]) (fun [a, b]) []
      , assertSubtypeGamma "G -| [A] <: [A] |- G" [] (lst a) (lst a) []
      , assertSubtypeGamma
          "G -| {K :: a, L :: b} <: {K :: a, L :: b}"
          []
          (record' "Foo" [(Key "K", a), (Key "L", b)])
          (record' "Foo" [(Key "K", a), (Key "L", b)])
          []
      , assertSubtypeGamma "<a> -| <a> <: A |- <a>:A" [eag] ea a [solvedA a]
      , assertSubtypeGamma "<a> -| A <: <a> |- <a>:A" [eag] a ea [solvedA a]
      , assertSubtypeGamma "<b> -| [A] <: <b> |- <b>:[A]" [ebg] (lst a) (eb) [solvedB (lst a)]
      , assertSubtypeGamma "<a> -| <a> <: [B] |- <a>:[B]" [eag] (lst b) (ea) [solvedA (lst b)]
      , assertSubtypeGamma
          "<a>, <b> -| <a> <b> <: [C] |- <a>:[C], <b>:C"
          [eag, ebg]
          (existP "x1" [eb] [])
          (lst c)
          [solvedA (lst c), solvedB c]
      , assertSubtypeGamma
          "<a>, <b> -|[C] <: <a> <b> |- <a>:[C], <b>:C"
          [eag, ebg]
          (lst c)
          (existP "x1" [eb] [])
          [solvedA (lst c), solvedB c]
      , assertSubtypeGamma
          "[] -| forall a . a <: A -| a:A"
          []
          (forallu ["a"] (var "a"))
          a
          [SolvedG (TV "a") a]
      , -- nested types
        assertSubtypeGamma "<b> -| [A] <: [<b>] |- <b>:A" [ebg] (lst a) (lst eb) [solvedB a]
      , assertSubtypeGamma "<a> -| [<a>] <: [B] |- <a>:B" [eag] (lst b) (lst ea) [solvedA b]
      , assertSubtypeGamma
          "<a>, <b> -| (A, B) <: (<a>, <b>) |- <a>:A, <b>:B"
          [eag, ebg]
          (tuple [a, b])
          (tuple [ea, eb])
          [solvedA a, solvedB b]
      , assertSubtypeGamma
          "<a>, <b> -| (<a>, <b>) <: (A, B) |- <a>:A, <b>:B"
          [eag, ebg]
          (tuple [ea, eb])
          (tuple [a, b])
          [solvedA a, solvedB b]
      , assertSubtypeGamma
          "<a>, <b>, <c>, <d> -| (<a>, <b>) <: (<c>, <d>) -| <a>:<c>, <b>:<d>, <c>, <d>"
          [eag, ebg, ecg, edg]
          (tuple [ea, eb])
          (tuple [ec, ed])
          [solvedA ec, solvedB ed, ecg, edg]
      ]
  where
    a = var "A"
    b = var "B"
    c = var "C"
    ea = exist "x1"
    eb = exist "x2"
    ec = exist "x3"
    ed = exist "x4"
    eag = ExistG (TV "x1") ([], Open) ([], Open)
    ebg = ExistG (TV "x2") ([], Open) ([], Open)
    ecg = ExistG (TV "x3") ([], Open) ([], Open)
    edg = ExistG (TV "x4") ([], Open) ([], Open)
    solvedA t = SolvedG (TV "x1") t
    solvedB t = SolvedG (TV "x2") t

substituteTVarTests :: TestTree
substituteTVarTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "test variable substitution"
      [ testEqual "[x/y]Int" (substituteTVar (TV "x") (var "y") int) int
      , testEqual
          "[y/x]([x] -> x)"
          (substituteTVar (TV "x") (var "y") (fun [lst (var "x"), var "x"]))
          (fun [lst (var "y"), var "y"])
      ]

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
  import foo
  y = True
module main (z)
  import bar
  z = 1
      |]
          int
      ]

packerTests :: TestTree
packerTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Test building of packer maps"
      [testEqual "packer test" (1 :: Int) 1]

typeAliasTests :: TestTree
typeAliasTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Test type alias substitutions"
      [ assertGeneralType
          "general type alias"
          [r|
        module main (f)
        type Foo = A
        f :: Foo
        |]
          (var "A")
      , assertGeneralType
          "parameterized generic"
          [r|
        module main (f)
        f :: m (a -> b)
        |]
          (forallu ["m@q0", "a@q1", "b@q2"] (arr "m@q0" [fun [var "a@q1", var "b@q2"]]))
      , assertGeneralType
          "non-parametric, general type alias"
          [r|
        module main (f)
        type Foo = A
        f :: Foo -> B
        |]
          (fun [var "A", var "B"])
      , assertGeneralType
          "deep type substitution: `[Foo] -> B`"
          [r|
        module main (f)
        type Foo = A
        f :: [Foo] -> B
        |]
          (fun [lst (var "A"), var "B"])
      , assertGeneralType
          "deep type substitution: `[Foo] -> Foo`"
          [r|
        module main (f)
        type Foo = A
        f :: [Foo] -> Foo
        |]
          (fun [lst (var "A"), var "A"])
      , assertGeneralType
          "parametric alias, general type alias"
          [r|
        module main (f)
        type (Foo a b) = (a,b)
        f :: Foo X Y -> Z
        |]
          (fun [tuple [var "X", var "Y"], var "Z"])
      , assertGeneralType
          "nested types"
          [r|
           module main (foo)
           type A = B
           type B = C
           foo :: A -> B -> C
        |]
          (fun [var "C", var "C", var "C"])
      , assertGeneralType
          "state is preserved across binding"
          [r|
           module main (f)
           type Foo = A
           g :: Foo -> Int
           f = g
        |]
          (fun [var "A", var "Int"])
      , assertGeneralType
          "state is inherited across binding"
          [r|
           module main (f)
           type Foo = A
           g :: a -> b
           f :: Foo -> Int
           f = g  {- yes, g isn't defined -}
        |]
          (fun [var "A", var "Int"])
      , expectError
          "fail on too many type aliases parameters"
          [r|
           type A = B
           foo :: A Int -> C
           foo
        |]
      , expectError
          "fail on too few type aliases parameters"
          [r|
           type (A a) = (a,a)
           foo :: A -> C
           foo
        |]
      , expectError
          "fail on conflicting types (Int vs Str)"
          [r|
           type A = Int
         
           module b (A)
           type A = Str
         
           module main (foo)
           import a (A)
           import b (A)
         
           foo :: A -> A -> A
        |]
      , expectError
          "fail on conflicting types (Map vs List)"
          [r|
           module a (A)
           type A a b = Map a b
           
           module b (A)
           type A a b = List (Tuple2 a b)
           
           module main (foo)
           import a (A)
           import b (A)
           
           foo :: A a b -> A a b -> A a b
        |]
      , -- import tests ---------------------------------------
        assertGeneralType
          "non-parametric, general type alias, imported"
          [r|
           module m1 (Foo)
             type Foo = A
           module main (f)
             import m1 (Foo)
             f :: Foo -> B
        |]
          (fun [var "A", var "B"])
      , assertGeneralType
          "non-parametric, general type alias, reimported"
          [r|
           module m3 (Foo)
             type Foo = A
           module m2 (Foo)
             import m3 (Foo)
           module m1 (Foo)
             import m2 (Foo)
           module main (f)
             import m1 (Foo)
             f :: Foo -> B
        |]
          (fun [var "A", var "B"])
      , assertGeneralType
          "non-parametric, general type alias, imported aliased"
          [r|
           module m1 (Foo)
             type Foo = A
           module main (f)
             import m1 (Foo as Bar)
             f :: Bar -> B
        |]
          (fun [var "A", var "B"])
      , assertGeneralType
          "non-parametric, general type alias, reimported aliased"
          [r|
           module m3 (Foo1)
             type Foo1 = A

           module m2 (Foo2)
             import m3 (Foo1 as Foo2)

           module m1 (Foo3)
             import m2 (Foo2 as Foo3)

           module main (f)
             import m1 (Foo3 as Foo4)
             f :: Foo4 -> B
        |]
          (fun [var "A", var "B"])
      , assertGeneralType
          "non-parametric, general type alias, duplicate import"
          [r|
           module m2 (Foo)
             type Foo = A

           module m1 (Foo)
             type Foo = A

           module main (f)
             import m1 (Foo)
             import m2 (Foo)
             f :: Foo -> B
        |]
          (fun [var "A", var "B"])
      , assertGeneralType
          "parametric alias, general type alias, duplicate import"
          [r|
           module m2 (Foo)
             type (Foo a b) = (a,b)

           module m1 (Foo)
             type (Foo c d) = (c,d)

           module main (f)
             import m1 (Foo)
             import m2 (Foo)
             f :: Foo X Y -> Z
        |]
          (fun [tuple [var "X", var "Y"], var "Z"])
      -- Type-level record literals use `=` to bind fields (mirroring
      -- morloc's term-level `{x = 3, y = "a"}` syntax). Mistakenly using
      -- `::` (the declaration separator) raises a specific parser error
      -- rather than a generic 'unexpected ::'.
      , expectError
          "type-level record literal: `{x :: Int}` raises a parse error"
          [r|
        module main (f)
        type R = {x :: Int, y :: Str}
        f :: R -> R
        |]
      , expectError
          "type-level record literal: `::` inside a tuple type annotation"
          [r|
        module main (c)
        c :: ({x :: Int, y :: Int}, Int)
        c = undefined
        |]
      -- Invariant 3 (R1): an @instance@ may only be declared on the root of
      -- an alias tree. A transparent @type@ alias inherits the root's
      -- instances; declaring an instance on the leaf would collide with the
      -- root's.
      , expectError
          "instance on transparent alias is rejected (simple)"
          [r|
        module main (f)
        type MyStr = Str
        class Eq a where
          (==) :: a -> a -> Bool
        instance Eq MyStr where
          (==) x y = True
        f :: MyStr -> MyStr -> Bool
        f x y = x == y
        |]
      , expectError
          "instance on transparent alias is rejected (parameterized)"
          [r|
        module main (f)
        type MyList a = List a
        class Functor f where
          map :: (a -> b) -> f a -> f b
        instance Functor MyList where
          map = undefined
        f :: MyList Int -> MyList Int
        f xs = map (\x -> x) xs
        |]
      , expectError
          "instance on transparent alias is rejected (deep chain)"
          [r|
        module main (f)
        type A = Str
        type B = A
        class Foo a where
          tag :: a -> Str
        instance Foo B where
          tag x = x
        f :: B -> Str
        f x = tag x
        |]
      -- R1 positive: a @newtype@ legitimately owns its own instances.
      , expectPass
          "instance on newtype is accepted"
          [r|
        module main (f)
        newtype MyStr = Str
        class Tag a where
          tag :: a -> Str
        instance Tag MyStr where
          tag x = "tagged"
        f :: MyStr -> Str
        f x = tag x
        |]
      -- R2: cousin transparent aliases unify and share the root's
      -- instance. Both @B@ and @C@ reduce to @Str@, so the root's
      -- @Default@ method dispatches at every call site.
      , expectPass
          "cousin transparent aliases share the root's instance"
          [r|
        module main (f)
        type B = Str
        type C = Str
        class Default a where
          def :: a
        instance Default Str where
          def = ""
        f :: B -> C
        f x = def
        |]
      , assertGeneralType
          "cousin transparent aliases are interchangeable (subtype)"
          [r|
        module main (f)
        type Cousin1 = Str
        type Cousin2 = Str
        g :: Cousin1 -> Cousin1
        f :: Cousin2 -> Cousin1
        f x = g x
        |]
          (fun [var "Str", var "Str"])
      ]

-- | Tests for integer/real literal defaulting through type aliases.
-- A literal `65` checked against a type alias such as `type Char = U8`
-- must take on the aliased base integer type instead of synthesizing as
-- `Int` and failing the `Int <: U8` subtype step. Same for real
-- literals and F32/F64 aliases. Covers single-depth and multi-hop
-- alias chains, and every fixed-width primitive in the integer/real
-- families.
numericLiteralAliasTests :: TestTree
numericLiteralAliasTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "Numeric literal defaulting through type aliases"
      [ -- single-depth integer aliases: every fixed-width family member
        assertGeneralType
          "int literal :: Char  (Char = U8)"
          [r|
        module main (x)
        type Char = U8
        x :: Char
        x = 65
        |]
          (var "U8")
      , assertGeneralType
          "int literal :: Word16  (Word16 = U16)"
          [r|
        module main (x)
        type Word16 = U16
        x :: Word16
        x = 65000
        |]
          (var "U16")
      , assertGeneralType
          "int literal :: Word32  (Word32 = U32)"
          [r|
        module main (x)
        type Word32 = U32
        x :: Word32
        x = 1
        |]
          (var "U32")
      , assertGeneralType
          "int literal :: Word64  (Word64 = U64)"
          [r|
        module main (x)
        type Word64 = U64
        x :: Word64
        x = 1
        |]
          (var "U64")
      , assertGeneralType
          "int literal :: Byte  (Byte = I8)"
          [r|
        module main (x)
        type Byte = I8
        x :: Byte
        x = 1
        |]
          (var "I8")
      , assertGeneralType
          "int literal :: Short  (Short = I16)"
          [r|
        module main (x)
        type Short = I16
        x :: Short
        x = 1
        |]
          (var "I16")
      , assertGeneralType
          "int literal :: I32  (I32 = I32)"
          [r|
        module main (x)
        type Int32 = I32
        x :: Int32
        x = 1
        |]
          (var "I32")
      , assertGeneralType
          "int literal :: I64  (I64 = I64)"
          [r|
        module main (x)
        type Int64 = I64
        x :: Int64
        x = 1
        |]
          (var "I64")
      , assertGeneralType
          "int literal :: Word  (Word = UInt)"
          [r|
        module main (x)
        type Word = UInt
        x :: Word
        x = 1
        |]
          (var "UInt")
        -- multi-hop alias chains: literal must default through the chain
      , assertGeneralType
          "int literal :: FooChar  (FooChar = Char = U8)"
          [r|
        module main (x)
        type Char = U8
        type FooChar = Char
        x :: FooChar
        x = 65
        |]
          (var "U8")
      , assertGeneralType
          "int literal :: A  (A = B = C = U32)"
          [r|
        module main (x)
        type A = B
        type B = C
        type C = U32
        x :: A
        x = 1
        |]
          (var "U32")
        -- list literal flowing each element through the alias:
        -- mirrors the encode/decode test in stdlib char-cpp that
        -- triggered the original bug report.
      , assertGeneralType
          "list of int literals :: [Char]  (Char = U8)"
          [r|
        module main (xs)
        type Char = U8
        xs :: [Char]
        xs = [65, 66, 67]
        |]
          (lst (var "U8"))
      , assertGeneralType
          "list of int literals :: [FooChar]  (chain to U8)"
          [r|
        module main (xs)
        type Char = U8
        type FooChar = Char
        xs :: [FooChar]
        xs = [65, 66, 67]
        |]
          (lst (var "U8"))
        -- single-depth real aliases
      , assertGeneralType
          "real literal :: Mass  (Mass = F32)"
          [r|
        module main (x)
        type Mass = F32
        x :: Mass
        x = 1.5
        |]
          (var "F32")
      , assertGeneralType
          "real literal :: Distance  (Distance = F64)"
          [r|
        module main (x)
        type Distance = F64
        x :: Distance
        x = 1.5
        |]
          (var "F64")
      , assertGeneralType
          "real literal :: Quantity  (Quantity = Real)"
          [r|
        module main (x)
        type Quantity = Real
        x :: Quantity
        x = 1.5
        |]
          (var "Real")
        -- multi-hop real alias chain
      , assertGeneralType
          "real literal :: A  (A = B = F32)"
          [r|
        module main (x)
        type A = B
        type B = F32
        x :: A
        x = 1.5
        |]
          (var "F32")
      , assertGeneralType
          "list of real literals :: [Mass]  (Mass = F32)"
          [r|
        module main (xs)
        type Mass = F32
        xs :: [Mass]
        xs = [1.0, 2.0, 3.0]
        |]
          (lst (var "F32"))
        -- negative: an integer literal still cannot inhabit a non-numeric
        -- alias. Guards against the fix accidentally letting any alias
        -- swallow integer literals.
      , expectError
          "int literal :: Flag (Flag = Bool) must fail"
          [r|
        module main (x)
        type Flag = Bool
        x :: Flag
        x = 65
        |]
        -- negative: a real literal still cannot inhabit an integer alias.
      , expectError
          "real literal :: Char (Char = U8) must fail"
          [r|
        module main (x)
        type Char = U8
        x :: Char
        x = 1.5
        |]
        -- Nat-parameterized list aliases: `Vector 4 I32` reduces via
        -- `type Vector (n :: Nat) a = List a` to `List I32`. The list
        -- literal's elements must take on the reduced element type,
        -- not synthesize as `Int` and fail the subsequent subtype check.
        -- This is the user's original reproducer from the bug report.
      , assertGeneralType
          "list literal :: Vector 4 I32  (Vector (n::Nat) a = List a)"
          [r|
        module main (x)
        type Vector (n :: Nat) a = List a
        x :: Vector 4 I32
        x = [1, 2, 3, 4]
        |]
          (lst (var "I32"))
      , assertGeneralType
          "list literal :: Vector 3 I8  (other fixed-width int)"
          [r|
        module main (x)
        type Vector (n :: Nat) a = List a
        x :: Vector 3 I8
        x = [1, 2, 3]
        |]
          (lst (var "I8"))
      , assertGeneralType
          "list literal :: Vector 2 U16  (unsigned fixed-width int)"
          [r|
        module main (x)
        type Vector (n :: Nat) a = List a
        x :: Vector 2 U16
        x = [1, 2]
        |]
          (lst (var "U16"))
        -- Nested nat-parameterized alias: Matrix m n a = [[a]] requires
        -- the element-type propagation to recurse through both layers.
      , assertGeneralType
          "nested list literal :: Matrix 2 2 I32"
          [r|
        module main (x)
        type Matrix (m :: Nat) (n :: Nat) a = List (List a)
        x :: Matrix 2 2 I32
        x = [[1, 2], [3, 4]]
        |]
          (lst (lst (var "I32")))
        -- Real literals through nat-parameterized aliases use the same
        -- dispatch — confirm the RealS path is unaffected.
      , assertGeneralType
          "real list literal :: Vector 2 F32"
          [r|
        module main (x)
        type Vector (n :: Nat) a = List a
        x :: Vector 2 F32
        x = [1.5, 2.5]
        |]
          (lst (var "F32"))
        -- Negative: Nat-dimension mismatch must still fail. The element
        -- type was successfully propagated (I32 accepted into the
        -- literals), but length 3 does not satisfy Nat dimension 4.
        -- This guards against the fix bypassing the nat-dim check.
      , expectError
          "list literal :: Vector 4 I32 with wrong length must fail"
          [r|
        module main (x)
        type Vector (n :: Nat) a = List a
        x :: Vector 4 I32
        x = [1, 2, 3]
        |]
      ]

-- Numeric literals appearing in argument positions where the
-- function's parameter type is an unsolved existential. The fix in
-- @checkE (IntS / RealS)@ defers the literal binding until a later
-- arg pins the existential. If nothing pins it, the kind-appropriate
-- default (Int / Real) is applied at end-of-typecheck.
--
-- @IntS@ also accepts real base types (Int-to-Real promotion), so
-- expressions like @4 + 2.3@ where the @+@ wants @Real@ on both sides
-- let the @4@ adopt @Real@ rather than locking the existential to
-- @Int@ and failing.
pendingNumLitTests :: TestTree
pendingNumLitTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "Numeric literals in polymorphic argument positions"
      [ -- ----- Bare top-level: synth-mode default -----
        assertGeneralType
          "bare int literal defaults to Int"
          [r|
        module main (x)
        x = 42
        |]
          int
      , assertGeneralType
          "bare real literal defaults to Real"
          [r|
        module main (x)
        x = 4.2
        |]
          real

      -- ----- Direct integer-to-real promotion at the literal site -----
      , assertGeneralType
          "int literal :: Real (promotes)"
          [r|
        module main (x)
        x :: Real
        x = 42
        |]
          real
      , assertGeneralType
          "int literal :: F32 (promotes)"
          [r|
        module main (x)
        x :: F32
        x = 42
        |]
          (var "F32")
      , assertGeneralType
          "int literal :: F64 (promotes)"
          [r|
        module main (x)
        x :: F64
        x = 42
        |]
          (var "F64")

      -- ----- Mixed-numeric list literal -----
      , assertGeneralType
          "list of int + real literals at [Real]"
          [r|
        module main (x)
        x :: List Real
        x = [1, 2, 3.0]
        |]
          (lst real)

      -- ----- Polymorphic identity: literal flows through, default applies -----
      , assertGeneralType
          "id of int literal defaults to Int"
          [r|
        module main (x)
        id :: a -> a
        x = id 42
        |]
          int
      , assertGeneralType
          "id of real literal defaults to Real"
          [r|
        module main (x)
        id :: a -> a
        x = id 4.2
        |]
          real

      -- ----- Polymorphic binary function: both args share the existential -----
      , assertGeneralType
          "add of two int literals defaults to Int"
          [r|
        module main (x)
        add :: a -> a -> a
        x = add 1 1
        |]
          int
      , assertGeneralType
          "add of two real literals defaults to Real"
          [r|
        module main (x)
        add :: a -> a -> a
        x = add 1.5 2.5
        |]
          real
      , assertGeneralType
          "add of int + real literals -- int promotes (1 + 2.3)"
          [r|
        module main (x)
        add :: a -> a -> a
        x = add 1 2.3
        |]
          real
      , assertGeneralType
          "add of real + int literals -- int promotes (2.3 + 1)"
          [r|
        module main (x)
        add :: a -> a -> a
        x = add 2.3 1
        |]
          real

      -- ----- Annotated arg pins the existential, literal adopts the width -----
      , assertGeneralType
          "add of int literal + I8-annotated -- literal adopts I8"
          [r|
        module main (x)
        add :: a -> a -> a
        y :: I8
        y = 5
        x :: I8
        x = add 1 y
        |]
          (var "I8")

      -- ----- Negative: numeric literal in a non-numeric slot fails -----
      , exprTestBad
          "int literal in Bool slot rejected"
          [r|
        module main (x)
        x :: Bool
        x = 42
        |]
      , exprTestBad
          "real literal in Int slot rejected (no demotion)"
          [r|
        module main (x)
        x :: Int
        x = 4.2
        |]
      ]

whereTests :: TestTree
whereTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Test of where statements"
      [ assertGeneralType
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

typeOrderTests :: TestTree
typeOrderTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Tests of type partial ordering (subtype)"
      [ testFalse
          "Str !< Real"
          (isSubtypeOf str real)
      , testFalse
          "Real !< Str"
          (isSubtypeOf real str)
      , testFalse
          "[Real] !< [Str]"
          (isSubtypeOf (lst real) (lst str))
      , testFalse
          "[Str] !< [Real]"
          (isSubtypeOf (lst str) (lst real))
      , testFalse
          "Str -> Str -> Str !< Real -> Real -> Real"
          (isSubtypeOf (fun [str, str, str]) (fun [real, real, real]))
      , testFalse
          "Real -> Real -> Real !< Str -> Str -> Str"
          (isSubtypeOf (fun [real, real, real]) (fun [str, str, str]))
      , testFalse
          "Str -> Str !< Int -> Int -> Int"
          (isSubtypeOf (fun [str, str]) (fun [int, int, int]))
      , testTrue
          "a <: Int"
          (isSubtypeOf (forallu ["a"] (var "a")) int)
      , testFalse
          "Int !< forall a . a"
          (isSubtypeOf int (forallu ["a"] (var "a")))
      , testTrue
          "forall a . (Int, a) <: (Int, Str)"
          (isSubtypeOf (forallu ["a"] (tuple [int, var "a"])) (tuple [int, str]))
      , testTrue
          "forall a b . (a, b) <: (Int, Str)"
          (isSubtypeOf (forallu ["a", "b"] (tuple [var "a", var "b"])) (tuple [int, str]))
      , testTrue
          "forall a . (Int, a) <: forall b . (Int, b)"
          ( isSubtypeOf
              (forallu ["a"] (tuple [int, var "a"]))
              (forallu ["b"] (tuple [int, var "b"]))
          )
      , testTrue
          "forall a . a <: (Int, Str)"
          (isSubtypeOf (forallu ["a"] (var "a")) (tuple [int, str]))
      , testTrue
          "forall a . a <: forall a b . (a, b)"
          (isSubtypeOf (forallu ["a"] (var "a")) (forallu ["a", "b"] (tuple [var "a", var "b"])))
      , -- cannot compare
        testFalse
          "[Int] !< Int"
          (isSubtypeOf (lst int) int)
      , testFalse
          "Int !< [Int]"
          (isSubtypeOf int (lst int))
      , -- partial order of types
        testTrue
          "forall a . [a] <= [Int]"
          ((forallu ["a"] (lst (var "a"))) <= (lst (var "a")))
      , testFalse
          "[Int] !< forall a . [a]"
          ((lst (var "a")) <= (forallu ["a"] (lst (var "a"))))
      , testTrue
          "forall a . (Int, a) <= (Int, Bool)"
          ((forallu ["a"] (tuple [int, var "a"])) <= (tuple [int, bool]))
      , testFalse
          "(Int, Bool) !<= forall a . (Int, a)"
          ((tuple [int, bool]) <= (forallu ["a"] (tuple [int, var "a"])))
      , testTrue
          "forall a b . (a, b) <= forall c . (Int, c)"
          ((forallu ["a", "b"] (tuple [var "a", var "b"])) <= (forallu ["c"] (tuple [int, var "c"])))
      , testFalse
          "forall c . (Int, c) !<= forall a b . (a, b)"
          ((forallu ["c"] (tuple [int, var "c"])) <= (forallu ["a", "b"] (tuple [var "a", var "b"])))
      , testTrue
          "forall a . a <= forall a b . (a, b)"
          ((forallu ["a"] (var "a")) <= (forallu ["a", "b"] (tuple [var "a", var "b"])))
      , -- test "mostSpecific"
        testEqual
          "mostSpecific [Int, Str, forall a . a] = [Int, Str]"
          (mostSpecific [int, str, forallu ["a"] (var "a")])
          [int, str]
      , -- test "mostGeneral"
        testEqual
          "mostGeneral [Int, Str, forall a . a] = forall a . a"
          (mostGeneral [int, str, forallu ["a"] (var "a")])
          [forallu ["a"] (var "a")]
      , -- test mostSpecificSubtypes
        testEqual
          "mostSpecificSubtypes: Int against [forall a . a]"
          (mostSpecificSubtypes int [forallu ["a"] (var "a")])
          [forallu ["a"] (var "a")]
      , testEqual
          "mostSpecificSubtypes: (Int -> Int)"
          ( mostSpecificSubtypes
              (fun [int, int])
              [fun [str, str], fun [int, int], forallu ["a"] (fun [var "a", var "a"])]
          )
          [fun [int, int]]
      , testEqual
          "mostSpecificSubtypes: empty"
          (mostSpecificSubtypes (fun [str, str, str]) [fun [real, real, real]])
          []
      , -- test mostSpecificSubtypes for tuples
        testEqual
          "mostSpecificSubtypes: tuples"
          ( mostSpecificSubtypes
              (tuple [int, int])
              [ forallu ["a"] (var "a")
              , forallu ["a", "b"] (tuple [var "a", var "b"])
              , forallu ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
              ]
          )
          [forallu ["a", "b"] (tuple [var "a", var "b"])]
      , -- test mostSpecificSubtypes for tuples
        testEqual
          "mostSpecificSubtypes: with partially generic tuples"
          ( mostSpecificSubtypes
              (forallu ["a"] (tuple [int, var "a"]))
              [ forallu ["a"] (var "a")
              , forallu ["a", "b"] (tuple [var "a", var "b"])
              , forallu ["a"] (tuple [int, var "a"])
              , forallu ["a"] (tuple [int, bool])
              , forallu ["a", "b", "c"] (tuple [var "a", var "b", var "c"])
              ]
          )
          [forallu ["a"] (tuple [int, var "a"])]
      ]

unitTypeTests :: TestTree
unitTypeTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Typechecker unit tests"
      -- comments
      [ assertGeneralType "block comments (1)" "{- -} 42" int
      , assertGeneralType "block comments (2)" " {--} 42{-   foo -} " int
      , assertGeneralType "line comments (3)" "-- foo\n 42" int
      , -- reals versus integers
        assertGeneralType "0 is an int" "0" int
      , assertGeneralType "42 is an int" "42" int
      , assertGeneralType "-42 is an int" "-42" int
      , assertGeneralType "big integers are OK" "123456789123456789123456789123456789123456789123456789" int
      , assertGeneralType
          "big negative integers are OK"
          "-123456789123456789123456789123456789123456789123456789"
          int
      , assertGeneralType "0.0 is a real" "0.0" real
      , assertGeneralType "4.2 is a real" "4.2" real
      , assertGeneralType "-4.2 is a real" "-4.2" real
      , assertGeneralType "4e1 is a real (scientific notation is real)" "4e1" real
      , assertGeneralType "-4e1 is a real" "-4e1" real
      , assertGeneralType "-4e-1 is a real" "-4e-1" real
      , assertGeneralType "4.2e3000 is a real" "4.2e3000" real
      , assertGeneralType "irregular scientific notation is OK" "123456789123456789123456789e-3000" real
      , assertGeneralType "reals may be big" "123456789123456789123456789.123456789123456789123456789" real
      , -- other primitives
        assertGeneralType "primitive boolean" "True" bool
      , assertGeneralType "primitive string" "\"this is a string literal\"" str
      , -- The `\0` escape is intentionally legal at the source-level so
        --   that NUL-tolerant pools (Python, C++, Julia) and the pure
        --   morloc nexus can carry interior NULs. The dispatch-layer
        --   guard rejects NUL-bearing values only at boundaries into
        --   NUL-intolerant languages (R, C). See features-strings.asc.
        assertGeneralType "string with embedded NUL escape" "\"abc\\0def\"" str
      , assertGeneralType "primitive integer annotation" "42 :: Int" int
      , assertGeneralType "primitive boolean annotation" "True :: Bool" bool
      , assertGeneralType "primitive double annotation" "4.2 :: Real" real
      , assertGeneralType
          "primitive string annotation"
          "\"this is a string literal\" :: Str"
          str
      , assertGeneralType "primitive declaration" "x = True\n4.2" real
      , -- containers
        -- - lists
        assertGeneralType "list of one primitive" "[1]" (lst int)
      , assertGeneralType "list of many primitives" "[1,2,3]" (lst int)
      , assertGeneralType "list of many containers" "[(True,1),(False,2)]" (lst (tuple [bool, int]))
      , -- - tuples
        assertGeneralType "tuple of primitives" "(1,2,True)" (tuple [int, int, bool])
      , assertGeneralType "tuple with containers" "(1,(2,True))" (tuple [int, tuple [int, bool]])
      , -- - records
        assertGeneralType
          "primitive record statement"
          [r|
        {x=42, y="yolo"}
        |]
          (existP "e0" [] [(Key "x", int), (Key "y", str)])
      , assertGeneralType
          "primitive record signature"
          [r|
        record Foo = Foo {x :: Int, y :: Str}
        f :: Int -> Foo
        f 42
        |]
          (record' "Foo" [(Key "x", int), (Key "y", str)])
      , assertGeneralType
          "primitive record declaration"
          [r|
        foo = {x = 42, y = "yolo"}
        foo
        |]
          (existP "e0" [] [(Key "x", int), (Key "y", str)])
      , assertGeneralType
          "nested records"
          [r|
        {x = 42, y = {bob = 24601, tod = "listen now closely and hear how I've planned it"}}
        |]
          (existP "e0" [] [(Key "x", int), (Key "y", existP "e1" [] [(Key "bob", int), (Key "tod", str)])])
      , assertGeneralType
          "records with bound variables"
          [r|
        foo a = {x=a, y="yolo"}
        foo 42
        |]
          (existP "e0" [] [(Key "x", int), (Key "y", str)])
      , testGroup
          "anonymous record row-form vs NamU-form reconciliation"
          [ expectPass
              "field accessor on an anonymous-record-typed argument"
              [r|
        f :: {x = Int} -> Int
        f r = .x r
        f
        |]
          , expectPass
              "record construction checked against an anonymous-record annotation"
              [r|
        f :: Int -> {x = Int}
        f n = {x = n}
        f
        |]
          , expectPass
              "constructed record passed to an anonymous-record-typed parameter"
              [r|
        g :: {x = Int} -> Int
        g r = 0
        g {x = 5}
        |]
          , expectPass
              "irrefutable record pattern binding an anonymous-record field"
              [r|
        f :: {x = Int} -> Int
        f r = let ({x = a}) = r in a
        f
        |]
          , expectPass
              "refutable `|`-pattern destructuring an anonymous record"
              [r|
        f :: {x = Int} -> Int
        f | {x = a} = a
        f
        |]
          ]
      , -- functions
        assertGeneralType
          "1-arg function declaration without signature"
          [r|
        f x = True
        f 42
        |]
          bool
      , assertGeneralType
          "2-arg function declaration without signature"
          [r|
        f x y = True
        f 42 True
        |]
          bool
      , assertGeneralType
          "1-arg function signature without declaration"
          [r|
        f :: Int -> Bool
        f 42
        |]
          bool
      , assertGeneralType
          "2-arg function signature without declaration"
          [r|
        f :: Int -> Bool -> Str
        f 42 True
        |]
          str
      , assertGeneralType
          "partial 1-2 function signature without declaration"
          [r|
        f :: Int -> Bool -> Str
        f 42
        |]
          (fun [bool, str])
      , assertGeneralType
          "identity function declaration and application"
          [r|
        f x = x
        f 42
        |]
          int
      , assertGeneralType
          "const declared function"
          [r|
        const x y = x
        const 42 True
        |]
          int
      , assertGeneralType
          "identity signature function"
          [r|
        id :: a -> a
        id 42
        |]
          int
      , assertGeneralType
          "const signature function"
          [r|
        const :: a -> b -> a
        const 42 True
        |]
          int
      , assertGeneralType
          "fst signature function"
          [r|
        fst :: (a,b) -> a
        fst (42,True)
        |]
          int
      , assertGeneralType
          "value to list function"
          [r|
        single :: a -> [a]
        single 42
        |]
          (lst int)
      , assertGeneralType
          "head function"
          [r|
        head :: [a] -> a
        head [1,2,3]
        |]
          int
      , assertGeneralType
          "make list function"
          [r|
        f :: a -> [a]
        f 1
        |]
          (lst int)
      , assertGeneralType
          "make list function"
          [r|
        single :: a -> [a]
        single 1
        |]
          (lst int)
      , assertGeneralType
          "existential function passing"
          [r|
        module main (g)
        g f = f True
        |]
          (fun [fun [bool, exist "e0"], exist "e0"])
      , assertGeneralType
          "app single function"
          [r|
        app :: (a -> b) -> a -> b
        f :: a -> [a]
        app f 42
        |]
          (lst int)
      , assertGeneralType
          "app head function"
          [r|
        app :: (a -> b) -> a -> b
        f :: [a] -> a
        app f [42]
        |]
          int
      , assertGeneralType
          "simple nested call"
          [r|
      f x = x
      g x = f x
      g 1
      |]
          int
      , assertGeneralType
          "nested calls"
          [r|
      f x y = (x, y)
      g x y = (x, f 1 y)
      g True "hi"
      |]
          (tuple [bool, tuple [int, str]])
      , assertGeneralType
          "zip pair"
          [r|
      pair x y = (x, y)
      zip :: (x -> y -> z) -> [x] -> [y] -> [z]
      zip pair [1,2] [True, False]
      |]
          (lst (tuple [int, bool]))
      , assertGeneralType
          "nested identity"
          [r|
      id :: a -> a
      id (id (id 1))
      |]
          int
      , assertGeneralType
          "head (head [[1]])"
          [r|
      head :: [a] -> a
      head (head [[42]])
      |]
          int
      , assertGeneralType
          "snd (snd (1,(1,True)))"
          [r|
      snd :: (a, b) -> b
      snd (snd (1, (1, True)))
      |]
          bool
      , assertGeneralType
          "f x y = [x, y]"
          [r|
        f x y = [x, y]
        f 1
        |]
          (fun [int, lst int])
      , assertGeneralType
          "map head function"
          [r|
        map :: (a -> b) -> [a] -> [b]
        head :: [a] -> a
        map head [[1],[1,2,3]]
        |]
          (lst int)
      , assertGeneralType
          "t a -> a"
          [r|
        gify :: a -> G a
        out :: f a -> a
        out (gify 1)
        |]
          int
      , assertGeneralType
          "f a b -> b"
          [r|
        gify :: a -> b -> G a b
        snd :: f a b -> b
        snd (gify 1 True)
        |]
          bool
      , assertGeneralType
          "map id over number list"
          [r|
        map :: (a -> b) -> [a] -> [b]
        id :: a -> a
        map id [1,2,3]
        |]
          (lst int)
      , assertGeneralType
          "map fst over tuple list"
          [r|
        map :: (a -> b) -> [a] -> [b]
        fst :: (a,b) -> a
        map fst [(1,True),(2,False)]
        |]
          (lst int)
      , assertGeneralType
          "map fstG over (G a b) list"
          [r|
        gify :: a -> b -> G a b
        map :: (a -> b) -> [a] -> [b]
        fstF :: f a b -> a
        map fstF [gify 1 True, gify 2 False]
        |]
          (lst int)
      , assertGeneralType
          "fmap generic fst over functor"
          [r|
        gify :: a -> G a
        fmap :: (a -> b) -> f a -> f b
        out :: f a -> a
        fmap out (gify [1])
        |]
          (arr "G" [int])
      , assertGeneralType
          "generic parameter reordering"
          [r|
        module m (biz)
        type M a b c = R b a c
        foo :: M a b c -> N b c
        bar :: a -> b -> c -> R a b c
        da :: Int -> X
        db :: Int -> Y
        dc :: Int -> Z
        baz a b c = foo (bar a b c)
        -- biz :: N X Z
        biz = baz (da 1) (db 2) (dc 3)
        |]
          (arr "N" [var "X", var "Z"])
      , assertGeneralType
          "variable annotation"
          [r|
        module main (f)
        f :: Foo
        |]
          (var "Foo")
      , -- lambdas
        assertGeneralType
          "function with parameterized types"
          [r|
        module main (f)
        f :: A B -> C
        |]
          (fun [arr "A" [var "B"], var "C"])
      , assertGeneralType "fully applied lambda (1)" "(\\x y -> x) 1 True" int
      , assertGeneralType "fully applied lambda (2)" "(\\x -> True) 42" bool
      , assertGeneralType "fully applied lambda (3)" "(\\x -> (\\y -> True) x) 42" bool
      , assertGeneralType "fully applied lambda (4)" "(\\x -> (\\y -> x) True) 42" int
      , assertGeneralType
          "unapplied lambda, polymorphic (1)"
          [r|\x -> True|]
          (fun [exist "e0", bool])
      , assertGeneralType
          "unapplied lambda, polymorphic (2)"
          "(\\x y -> x) :: a -> b -> a"
          (fun [exist "e0", exist "e1", exist "e0"])
      , assertGeneralType
          "annotated, fully applied lambda"
          "((\\x -> x) :: a -> a) True"
          bool
      , assertGeneralType
          "annotated, partially applied lambda"
          "((\\x y -> x) :: a -> b -> a) True"
          (fun [exist "e0", bool])
      , assertGeneralType
          "recursive functions are A-OK"
          "\\f -> f 5"
          (fun [fun [int, exist "e0"], exist "e0"])
      , -- applications
        assertGeneralType
          "primitive variable in application"
          [r|
        x = True
        (\y -> y) x
        |]
          bool
      , assertGeneralType
          "function variable in application"
          [r|
        f x y = x
        f 42 True
        |]
          int
      , assertGeneralType
          "partially applied function variable in application"
          [r|
        f x y = x
        x = f 42
        x
        |]
          (fun [exist "e0", int])
      , exprTestBad
          "applications with too many arguments fail"
          [r|
        f :: a -> a
        f True 12
        |]
      , exprTestBad
          "applications with mismatched types fail (1)"
          [r|
        abs :: Int -> Int
        abs True
        |]
      , exprTestBad
          "applications with mismatched types fail (2)"
          [r|
        f = 14
        g = \x h -> h x
        (g True) f
        |]
      , expectError
          "applications of non-functions should fail (1)"
          [r|
        f = 5
        g = \x -> f x
        g 12
        |]
      , expectError
          "applications of non-functions should fail (2)"
          [r|
        f = 5
        g = \h -> h 5
        g f
        |]
      , -- evaluation within containers
        expectError
          "arguments to a function are monotypes"
          [r|
        f :: a -> a
        g = \h -> (h 42, h True)
        g f
        |]
      , assertGeneralType
          "polymorphism under lambdas (203f8c) (1)"
          [r|
        f :: a -> a
        g = \h -> (h 42, h 1234)
        g f
        |]
          (tuple [int, int])
      , assertGeneralType
          "polymorphism under lambdas (203f8c) (2)"
          [r|
        f :: a -> a
        g = \h -> [h 42, h 1234]
        g f
        |]
          (lst int)
      , -- binding
        assertGeneralType
          "annotated variables without definition are legal"
          [r|
        module main (x)
        x :: Int
        |]
          int
      , assertGeneralType
          "unannotated variables with definition are legal"
          [r|
        x = 42
        x
        |]
          int
      , -- , exprTestBad
        --     "unannotated variables without definitions are illegal ('x')"
        --     "x"

        -- parameterized types
        assertGeneralType
          "parameterized type (n=1)"
          [r|
        module main (xs)
        xs :: Foo A
        |]
          (arr "Foo" [var "A"])
      , assertGeneralType
          "parameterized type (n=2)"
          [r|
        module main (xs)
        xs :: Foo A B
        |]
          (arr "Foo" [var "A", var "B"])
      , assertGeneralType
          "nested parameterized type"
          [r|
        module main (xs)
        xs :: Foo (Bar A) [B]
        |]
          (arr "Foo" [arr "Bar" [var "A"], arr "List" [var "B"]])
      , -- type signatures and higher-order functions
        assertGeneralType
          "type signature: identity function"
          [r|
        f :: a -> a
        f 42
        |]
          int
      , assertGeneralType
          "type signature: apply function with primitives"
          [r|
        apply :: (Int -> Bool) -> Int -> Bool
        f :: Int -> Bool
        apply f 42
        |]
          bool
      , assertGeneralType
          "type signature: generic apply function"
          [r|
        apply :: (a->b) -> a -> b
        f :: Int -> Bool
        apply f 42
        |]
          bool
      , assertGeneralType
          "type signature: map"
          [r|
        map :: (a->b) -> [a] -> [b]
        f :: Int -> Bool
        map f [5,2]
        |]
          (lst bool)
      , -- shadowing
        assertGeneralType
          "name shadowing in lambda expressions"
          [r|
        f x = (14, x)
        g x f = f x
        g True f
        |]
          (tuple [int, bool])
      , assertGeneralType
          "function passing without shadowing"
          [r|
        f x = (14, x)
        g foo = foo True
        g f
        |]
          (tuple [int, bool])
      , assertGeneralType
          "shadowed qualified type variables (7ffd52a)"
          [r|
        f :: a -> a
        g :: a -> Int
        g f
        |]
          int
      , assertGeneralType
          "non-shadowed qualified type variables (7ffd52a)"
          [r|
        f :: a -> a
        g :: b -> Int
        g f
        |]
          int
      , -- lists
        assertGeneralType "list of primitives" "[1,2,3]" (lst int)
      , assertGeneralType
          "list containing an applied variable"
          [r|
        f :: a -> a
        [53, f 34]
        |]
          (lst int)
      , -- NOTE: this test relies on internal renaming implementation
        assertGeneralType "empty list" "[]" (lst (exist "e0"))
      , assertGeneralType
          "list in function signature and application"
          [r|
        f :: [Int] -> Bool
        f [1]
        |]
          bool
      , -- , assertGeneralType
        --     "list in generic function signature and application"
        --     "f :: [a] -> Bool\nf [1]"
        --     [bool]
        -- , exprTestBad "failure on heterogenous list" "[1,2,True]"

        -- tuples
        assertGeneralType
          "tuple of primitives"
          [r|
        (4.2, True)
        |]
          (tuple [real, bool])
      , assertGeneralType
          "tuple containing an applied variable"
          [r|
        f :: a -> a
        (f 53, True)
        |]
          (tuple [int, bool])
      , assertGeneralType
          "check 2-tuples type signature"
          [r|
        module main (f)
        f :: (Int, Str)
        |]
          (tuple [int, str])
      , assertGeneralType "1-tuples are just for grouping" "module main (f)\nf :: (Int)" int
      , -- unit type
        assertGeneralType
          "unit as input"
          [r|
        module main (f)
        f :: () -> Bool
        |]
          (fun [VarU (TV "Unit"), bool])
      , assertGeneralType
          "unit as 2rd input"
          [r|
        module main (f)
        f :: Int -> () -> Bool
        |]
          (fun [int, VarU (TV "Unit"), bool])
      , assertGeneralType
          "unit as output"
          [r|
        module main (f)
        f :: Bool -> ()
        |]
          (fun [bool, VarU (TV "Unit")])
      , -- FIXME - I really don't like "Unit" being a normal var ...
        -- I am inclined to cast it as the unit type
        assertGeneralType "empty tuples are of unit type" "module main (f)\nf :: ()" (var "Unit")
      , -- extra space
        assertGeneralType "leading space" " 42" int
      , assertGeneralType "trailing space" "42 " int
      , -- adding signatures to declarations
        assertGeneralType
          "declaration with a signature (1)"
          [r|
        f :: a -> a
        f x = x
        f 42
        |]
          int
      , assertGeneralType
          "declaration with a signature (2)"
          [r|
        f :: Int -> Bool
        f x = True
        f 42
        |]
          bool
      , assertGeneralType
          "declaration with a signature (3)"
          [r|
        f :: Int -> Bool
        f x = True
        f
        |]
          (fun [int, bool])
      , expectError
          "primitive type mismatch should raise error"
          [r|
        module main (f)
        f :: Int -> Bool
        f x = 9999
        |]
      , expectError
          "catch infinite recursion of list"
          [r|
        module main (f)
        g :: [a] -> a
        f :: a -> a
        f x = g x
        |]
      , expectError
          "catch infinite recursion of tuple"
          [r|
        module main (f)
        g :: (a, b) -> a
        f :: a -> a
        f x = g x
        |]
      , expectError
          "check signatures under supposed identity"
          [r|
        module main (f)
        g :: (a -> b) -> a
        f :: a -> a
        f x = g x
        |]
      ,

        -- constraint syntax (implicit quantification wraps free vars in ForallU)
        assertGeneralType
          "constraint syntax (1)"
          "module main (f)\nf :: (Ord a) => a -> a -> a"
          (forallu ["a"] (fun [var "a", var "a", var "a"]))
      , assertGeneralType
          "constraint syntax (2)"
          "module main (f)\nf :: Ord a => a -> a -> a"
          (forallu ["a"] (fun [var "a", var "a", var "a"]))
      , assertGeneralType
          "constraint syntax (3)"
          "module main (f)\nf :: (Ord a, Eq b) => a -> b -> Bool"
          (forallu ["a", "b"] (fun [var "a", var "b", VarU (TV "Bool")]))
      , -- tests modules
        assertGeneralType
          "basic main module"
          [r|
          module main(x)
          x = [1,2,3]
        |]
          (lst int)
      , (flip $ assertGeneralType "import/export") (lst int) $
          [r|
          module foo (x)
            x = 42
          module bar (f)
            f :: a -> [a]
          module main (z)
            import foo (x)
            import bar (f)
            z = f x
        |]
      , (flip $ assertGeneralType "complex parse (1)") int $
          [r|
         module foo (x)
           add :: Int -> Int -> Int
           x = add a y where
             a = 1
             y = add b z where
               b = 42
           z = 19
      |]
      ]

unitValuecheckTests :: TestTree
unitValuecheckTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Valuechecker unit tests"
      [ valuecheckFail
          "unequal primitives fail"
          -- primitives
          [r|
         module foo (x)
           x = 1
           x = 2
      |]
      , valuecheckPass
          "equal primitives pass"
          [r|
         module foo (x)
           x = 1
           x = 1
      |]
      , -- containers
        valuecheckFail
          "lists with unequal values fail"
          [r|
         module foo (x)
           x = [1,3]
           x = [1,2]
      |]
      , valuecheckFail
          "lists of unequal length fail"
          [r|
         module foo (x)
           x = [1]
           x = [1,2]
      |]
      , valuecheckPass
          "identical lists pass"
          [r|
         module foo (x)
           x = [1,2]
           x = [1,2]
      |]
      , -- bound terms in simple expressions
        valuecheckFail
          "argument constraints"
          [r|
         module foo (f)
           f x y = x
           f a b = b
      |]
      , valuecheckFail
          "lambda var mismatches"
          [r|
         module foo (f)
           f x y = [x,y]
           f a b = [b,a]
      |]
      , valuecheckPass
          "identical lambda passes"
          [r|
         module foo (f)
           f x y = [x,y]
           f a b = [a,b]
      |]
      , -- comparisons of simple and non-simple
        valuecheckFail
          "constrained values fail"
          [r|
         module foo (x)
           source Py ("sum")
           sum :: [Int] -> Int
           x = sum [1, 2]
           x = 3
      |]
      , valuecheckFail
          "unequal types"
          [r|
         module foo (f)
           source Py ("sum")
           sum :: [Int] -> Int
           f xs = [1, sum xs]
           f xs = [2, sum xs]
      |]
      , -- non-export terms must also be value-checked
        valuecheckFail
          "non-export term value contradiction"
          [r|
         module foo (a)
           a = y
           y = 1
           y = 2
      |]
      , valuecheckFail
          "where-clause value contradiction"
          [r|
         module foo (a)
           a = y where
             y = 1
             y = 2
      |]
      , valuecheckFail
          "where-clause contradiction inside lambda body"
          [r|
         module foo (f)
           f x = [x, y] where
             y = 1
             y = 2
      |]
      , valuecheckFail
          "where with equivalent values is illegal (duplicate binding)"
          [r|
         module foo (a)
           a = y where
             y = 1
             y = 1
      |]
      , -- self-referential / cyclic bindings without concrete sources
        valuecheckFail
          "direct self-referential binding"
          [r|
         module foo (omega)
           omega = omega
      |]
      , valuecheckFail
          "mutual self-reference cycle"
          [r|
         module foo (a)
           a = b
           b = a
      |]
      , valuecheckFail
          "three-cycle self-reference"
          [r|
         module foo (a)
           a = b
           b = c
           c = a
      |]
      , -- duplicate record fields (caught at parse time)
        valuecheckFail
          "duplicate record field literal"
          [r|
         module foo (r)
           r = {a = 1, b = 2, a = 3}
      |]
      , valuecheckPass
          "distinct record fields are legal"
          [r|
         module foo (r)
           r = {a = 1, b = 2, c = 3}
      |]
      , valuecheckFail
          "duplicate field in record type (where form)"
          [r|
         module foo (Foo)
           record Foo where
             a :: Int
             b :: Bool
             a :: Real
      |]
      , valuecheckFail
          "duplicate field in record type (legacy form)"
          [r|
         module foo (Foo)
           record Foo = MkFoo {a :: Int, b :: Bool, a :: Real}
      |]
      , valuecheckPass
          "distinct fields in record type (where form)"
          [r|
         module foo (Foo)
           record Foo where
             a :: Int
             b :: Bool
             c :: Real
      |]
      , -- let blocks intentionally allow shadowing (non-recursive sequential
        -- let): `let { x = 1 ; x = 2 } in x` is the layout-free spelling of
        -- `let x = 1 in let x = 2 in x` and returns 2. Only where-clauses
        -- (which are order-invariant) reject duplicates.
        valuecheckPass
          "nested let shadowing is legal"
          [r|
         module foo (a)
           a = let x = 1 in let x = 2 in x
      |]
      , valuecheckPass
          "multi-binding let with same name shadows (sequential)"
          [r|
         module foo (a)
           a = let { x = 1 ; x = 2 } in x
      |]
      , -- where-clause shadows function parameter
        expectError
          "where-clause binding shadows parameter"
          [r|
         module foo (g)
           g x = y where
             x = 100
             y = x + 1
      |]
      , -- duplicate names in a single where-clause
        expectError
          "duplicate where-clause binding"
          [r|
         module foo (g)
           g n = y where
             y = n + 1
             y = n + 2
      |]
      , -- A polymorphic export that reaches a typeclass method backed by
        -- instances with differing literal bodies (e.g. Integral Int zero=0
        -- vs Integral Real zero=0.0). The instances are type-disjoint and
        -- never co-selected, so this must not be flagged as a value conflict.
        valuecheckPass
          "polymorphic export through multi-instance typeclass with literal bodies"
          [r|
         module foo (f)
           class Foo a where
             bar :: a
           instance Foo Int where
             bar = 5
           instance Foo Real where
             bar = 5.0
           f :: Foo a => a
           f = bar
      |]
      , -- Conflicting bodies for the SAME instance type must still fail:
        -- type-disjointness is the only thing that licenses skipping the
        -- comparison, and here both alternatives are Int.
        valuecheckFail
          "duplicate same-type instance bodies still flagged"
          [r|
         module foo (f)
           class Foo a where
             bar :: a
           instance Foo Int where
             bar = 5
           instance Foo Int where
             bar = 6
           f :: Foo a => a
           f = bar
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
recordLiteralOrderTests :: TestTree
recordLiteralOrderTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Record literal field-order tests"
      [ assertGeneralType
          "permuted literal (mixed types) typechecks against declared record"
          [r|
          record Person = Person { name :: Str, age :: Int }
          b :: Person
          b = { age = 30, name = "Alice" }
          b
        |]
          (record' "Person" [(Key "name", str), (Key "age", int)])
      , assertGeneralType
          "permuted literal (same-typed fields) typechecks"
          [r|
          record Point = Point { x :: Int, y :: Int }
          p :: Point
          p = { y = 2, x = 1 }
          p
        |]
          (record' "Point" [(Key "x", int), (Key "y", int)])
      , assertGeneralType
          "fully-reversed literal typechecks"
          [r|
          record T = T { a :: Int, b :: Int, c :: Int }
          v :: T
          v = { c = 3, b = 2, a = 1 }
          v
        |]
          (record' "T" [(Key "a", int), (Key "b", int), (Key "c", int)])
      , exprTestBad
          "literal missing a declared field is rejected"
          [r|
          module main (b)
          record Person = Person { name :: Str, age :: Int }
          b :: Person
          b = { name = "Alice" }
        |]
      , exprTestBad
          "literal with extra unknown field is rejected"
          [r|
          module main (b)
          record Person = Person { name :: Str, age :: Int }
          b :: Person
          b = { name = "Alice", age = 30, weight = 65 }
        |]
      , exprTestBad
          "literal with completely disjoint keys is rejected"
          [r|
          module main (b)
          record Person = Person { name :: Str, age :: Int }
          b :: Person
          b = { foo = "x", bar = 1 }
        |]
      , exprTestBad
          "literal with one matching and one wrong key is rejected"
          [r|
          module main (b)
          record Person = Person { name :: Str, age :: Int }
          b :: Person
          b = { name = "Alice", years = 30 }
        |]
      ]

{- | Tests for typechecker complexity - these would timeout with O(2^n) behavior
All tests have a 0.1-second timeout to catch exponential blowup
-}
complexityRegressionTests :: TestTree
complexityRegressionTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout
    testGroup
      "Complexity regression tests"
      [ -- Deep function composition - tests batch subtype optimization
        assertGeneralType
          "deep identity composition"
          [r|
          id :: a -> a
          f = id (id (id (id (id (id (id (id (id (id 42)))))))))
          f
        |]
          int
      , assertGeneralType
          "deep function composition chain"
          [r|
          id :: a -> a
          (.) :: (b -> c) -> (a -> b) -> a -> c
          f = id . id . id . id . id . id . id . id . id . id
          f 42
        |]
          int
      , -- Eta expansion - tests avoiding re-inference
        assertGeneralType
          "nested lambdas returning functions"
          [r|
          add :: Int -> Int -> Int
          f = \x -> add x
          f
        |]
          (fun [int, int, int])
      , assertGeneralType
          "deeply nested partial application"
          [r|
          add3 :: Int -> Int -> Int -> Int
          f = \x -> \y -> add3 x y
          f
        |]
          (fun [int, int, int, int])
      , assertGeneralType
          "lambda with multi-arg function body"
          [r|
          add4 :: Int -> Int -> Int -> Int -> Int
          g = \a -> \b -> add4 a b
          g 1 2 3 4
        |]
          int
      , -- Multi-argument function subtyping
        assertGeneralType
          "many-argument function"
          [r|
          f :: Int -> Int -> Int -> Int -> Int -> Int -> Int
          f 1 2 3 4 5 6
        |]
          int
      , assertGeneralType
          "polymorphic many-argument function"
          [r|
          f :: a -> b -> c -> d -> e -> (a, b, c, d, e)
          f 1 True "x" 2.0 [1]
        |]
          (tuple [int, bool, str, real, lst int])
      , -- HOF shared type variable enforcement
        exprTestBad
          "fold with (==) should fail: shared var c gets Bool and Str"
          [r|
          fold :: (b -> a -> b) -> b -> [a] -> b
          (==) :: c -> c -> Bool
          test = fold (==) True ["hello", "hello"]
          test
        |]
      , assertGeneralType
          "fold with (+) should succeed: shared var resolved consistently"
          [r|
          fold :: (b -> a -> b) -> b -> [a] -> b
          (+) :: Int -> Int -> Int
          test = fold (+) 0 [1, 2, 3]
          test
        |]
          int
      , assertGeneralType
          "map with lambda using (==) should succeed: same type both args"
          [r|
          map :: (a -> b) -> [a] -> [b]
          (==) :: c -> c -> Bool
          test = map (\x -> x == x) ["hello"]
          test
        |]
          (lst bool)
      , -- zipSubtype path: type constructor with repeated variable
        exprTestBad
          "zipSubtype: Pair a a cannot unify with Pair Bool Str"
          [r|
          mkPair :: a -> Pair a a
          consume :: Pair Bool Str -> Int
          test = consume (mkPair True)
          test
        |]
      , assertGeneralType
          "zipSubtype: Pair a a consistent with Pair Int Int"
          [r|
          mkPair :: a -> Pair a a
          fst :: Pair a b -> a
          test = fst (mkPair 42)
          test
        |]
          int
      , -- Shared var in return: id passed where Bool -> Str expected
        exprTestBad
          "shared var via HOF: id cannot satisfy Bool -> Str"
          [r|
          apply :: (a -> b) -> a -> b
          id :: a -> a
          asStr :: Str -> Str
          test = asStr (apply id True)
          test
        |]
      , -- Triple-shared variable through HOF
        exprTestBad
          "triple-shared var forced to different types through fold"
          [r|
          fold :: (b -> a -> b) -> b -> [a] -> b
          choose :: c -> c -> c
          test = fold choose "hello" [1, 2]
          test
        |]
      , -- Shared return var conflicts with arguments
        exprTestBad
          "shared var return type conflicts with argument through fold"
          [r|
          fold :: (b -> a -> b) -> b -> [a] -> b
          weirdEq :: c -> c -> Str
          test = fold weirdEq "start" [1, 2]
          test
        |]
      , -- Two distinct shared vars both violated
        exprTestBad
          "two distinct shared vars both inconsistent through HOF"
          [r|
          hof :: (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
          f :: x -> y -> x -> y -> Bool
          test = hof f 1 "hi" True 42.0
          test
        |]
      , -- Nested HOF shared var conflict
        exprTestBad
          "nested HOF: shared var conflict through double application"
          [r|
          apply :: (a -> b) -> a -> b
          (==) :: c -> c -> Bool
          test = apply (apply (==) True) "hello"
          test
        |]
      , -- Regression: fold with (==) on consistent types should pass
        assertGeneralType
          "fold with (==) consistent types: all Bool"
          [r|
          fold :: (b -> a -> b) -> b -> [a] -> b
          (==) :: c -> c -> Bool
          test = fold (==) True [True, False]
          test
        |]
          bool
      , -- Regression: multiple shared vars all consistent
        assertGeneralType
          "multiple shared vars consistent through HOF"
          [r|
          hof :: (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
          f :: x -> y -> x -> y -> Bool
          test = hof f 1 2 3 4
          test
        |]
          bool
      ]

-- Effect type helpers used throughout the effect test groups.
ioEff :: TypeU -> TypeU
ioEff = EffectU ioEffectSet

errEff :: TypeU -> TypeU
errEff = EffectU (EffectSet (Set.singleton "Error"))

ioErrEff :: TypeU -> TypeU
ioErrEff = EffectU (EffectSet (Set.fromList ["IO", "Error"]))

randEff :: TypeU -> TypeU
randEff = EffectU (EffectSet (Set.singleton "Rand"))

emptyEff :: TypeU -> TypeU
emptyEff = EffectU emptyEffectSet

-- | An effect set containing an unsolved EffectVar.  EffectVars are
-- introduced internally by the typechecker when forcing non-final
-- statements of a do-block and are not yet solved; subtyping defers
-- judgement in their presence rather than rejecting outright.
effVar :: MT.Text -> TypeU -> TypeU
effVar v = EffectU (EffectVar (TV v))

-- | Effect subtyping covers four cases the spec lays out
-- ('spec/types/effects.md' under "Subtyping" and "Effect Checking"):
--
--   1. Widening accepted: fewer effects can be used where more are
--      expected.  This is the one direction the rule permits.
--   2. Narrowing rejected: a value with more effects cannot satisfy a
--      slot with fewer.  Previously this was silently accepted; the
--      stricter rule turns it into a hard subtype error.
--   3. Effectful-to-pure rejected: an effectful type cannot satisfy a
--      pure slot.  Effects do not silently drop.
--   4. EffectVar deferral: when either side mentions an unsolved
--      EffectVar, judgement is deferred (the variable may later be
--      solved to anything).  This is a known TODO; the deferral is
--      tested here so the loophole is visible.
effectSubtypeTests :: TestTree
effectSubtypeTests =
  localOption (mkTimeout 100000) $ -- 0.1 second timeout
    testGroup
      "Effect subtype tests"
      [ -- === Reflexivity ===
        -- A type is always a subtype of itself, regardless of its effect set.
        assertSubtypeGamma "reflex: <IO> A <: <IO> A"
          [] (ioEff a) (ioEff a) []
      , assertSubtypeGamma "reflex: <> A <: <> A"
          [] (emptyEff a) (emptyEff a) []
      , assertSubtypeGamma "reflex: <IO,Error> A <: <IO,Error> A"
          [] (ioErrEff a) (ioErrEff a) []

        -- === Widening accepted (covariant subset) ===
        -- A value producing fewer effects may be used where more are
        -- expected.  This is the only allowed direction.
      , assertSubtypeGamma "widening: <IO> A <: <IO,Error> A"
          [] (ioEff a) (ioErrEff a) []
      , assertSubtypeGamma "widening: <> A <: <IO> A"
          [] (emptyEff a) (ioEff a) []
      , assertSubtypeGamma "widening: <> A <: <IO,Error> A"
          [] (emptyEff a) (ioErrEff a) []
      , assertSubtypeGamma "widening: <Error> A <: <IO,Error> A"
          [] (errEff a) (ioErrEff a) []

        -- === Narrowing rejected ===
        -- The signature `a :: <IO> Int = (rint :: <IO,Error> Int)`
        -- previously compiled, silently losing the Error tag.  The new
        -- rule rejects it as a subtype error.
      , assertSubtypeBad "narrowing: <IO,Error> A </: <IO> A"
          [] (ioErrEff a) (ioEff a)
      , assertSubtypeBad "narrowing: <IO> A </: <Error> A"
          [] (ioEff a) (errEff a)
      , assertSubtypeBad "narrowing: <IO> A </: <> A"
          [] (ioEff a) (emptyEff a)
      , assertSubtypeBad "narrowing: <IO,Error> A </: <Rand> A (disjoint)"
          [] (ioErrEff a) (randEff a)

        -- === Effectful-to-pure rejected ===
        -- Effects do not silently drop.  An effectful value cannot
        -- inhabit a plain type slot; the caller must declare the
        -- effect or force it in a do-block.
      , assertSubtypeBad "drop effect: <IO> A </: A"
          [] (ioEff a) a
      , assertSubtypeBad "drop effect: <IO,Error> A </: A"
          [] (ioErrEff a) a

        -- === Empty effect is the monoid identity: <> A == A ===
        -- The empty effect set is NOT a suspended computation; it is
        -- definitionally the inner type.  Effect coercion was removed,
        -- so this holds through the subtype relation alone, in both
        -- directions -- there is no `tryCoerce` step to involve.
      , assertSubtypeGamma "empty effect identity: <> A <: A"
          [] (emptyEff a) a []
      , assertSubtypeGamma "empty effect identity: A <: <> A"
          [] a (emptyEff a) []

        -- === Pure-to-effect is plain subsumption, NOT coercion ===
        -- The old pure-to-effect lift (`tryCoerce` / CoerceToEffect)
        -- was removed.  A pure value satisfies an effectful slot
        -- directly through subtyping: <> A == A and {} is a subset of
        -- any effect row, so `A <: <IO> A` holds with no coercion step.
      , assertSubtypeGamma "pure <: <IO> via subsumption (no coercion)"
          [] a (ioEff a) []

        -- === Recursion through inner type ===
        -- The subtype rule recurses on the inner types after the
        -- effect-subset check passes, so structural mismatches inside
        -- are caught normally.
      , assertSubtypeGamma "inner fun: <IO> (A -> B) <: <IO> (A -> B)"
          [] (ioEff (fun [a, b])) (ioEff (fun [a, b])) []
      , assertSubtypeGamma "inner optional: <IO> ?A <: <IO> ?A"
          [] (ioEff (OptionalU a)) (ioEff (OptionalU a)) []
      , assertSubtypeGamma "inner list: <IO> [A] <: <IO> [A]"
          [] (ioEff (lst a)) (ioEff (lst a)) []
      , assertSubtypeBad "inner type mismatch: <IO> A </: <IO> B"
          [] (ioEff a) (ioEff b)

        -- === Existential solving inside effects ===
        -- Existentials on either side of the effect wrapper are solved
        -- by recursing into the inner-type subtype check.  This
        -- behaviour is unchanged by the strictness fix.
      , assertSubtypeGamma "existential inside: <a> -| <IO> <a> <: <IO> A |- <a>:A"
          [eag] (ioEff ea) (ioEff a) [solvedA a]
      , assertSubtypeGamma "existential inside reverse: <a> -| <IO> A <: <IO> <a> |- <a>:A"
          [eag] (ioEff a) (ioEff ea) [solvedA a]

        -- === EffectVar deferral (known loophole) ===
        -- EffectVars are introduced internally when a do-block forces
        -- a non-final statement.  They are not yet solved, so subtype
        -- relaxes when either side mentions one rather than rejecting.
        -- Locking this in keeps the loophole visible; tightening it
        -- requires implementing effect-variable solving (TODO).
      , assertSubtypeGamma "deferral: <e1> A <: <IO> A passes (var on left)"
          [] (effVar "e1" a) (ioEff a) []
      , assertSubtypeGamma "deferral: <IO> A <: <e1> A passes (var on right)"
          [] (ioEff a) (effVar "e1" a) []
      , assertSubtypeGamma "deferral: <IO,Error> A <: <e1> A passes (var on right)"
          [] (ioErrEff a) (effVar "e1" a) []

        -- ...but a genuinely effectful value still cannot satisfy a
        -- concrete non-effect type (that rejection is unchanged).
      , assertSubtypeBad "effect into concrete still rejected: <Error> A </: A"
          [] (errEff a) a
      ]
  where
    a = var "A"
    b = var "B"
    ea = exist "x1"
    eag = ExistG (TV "x1") ([], Open) ([], Open)
    solvedA t = SolvedG (TV "x1") t

-- | Effect synthesis tests verify that the inferred effect set on a
-- top-level export matches the spec's structural propagation rule
-- ('spec/types/effects.md' under "Effect Inference"):
effectSynthesisTests :: TestTree
effectSynthesisTests =
  localOption (mkTimeout 100000) $ -- 0.1 second timeout
    testGroup
      "Effect synthesis tests"
      [ -- pure do-block with no effects infers empty effect set
        assertGeneralType
          "pure do-block"
          [r|
        module main (x)
        x = do 42
          |]
          int
      , assertGeneralType
          "do-block with with one function call"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        x = do { f 1 }
          |]
          (ioEff int)
      , assertGeneralType
          "do-block with tuple of effectful elements"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        x = do
          x <- f 1
          y <- f 2
          (x, y)
          |]
          (ioEff (tuple [int, int]))
      , -- let with pure RHS in do-block infers empty effect
        assertGeneralType
          "do-block with pure let binding"
          [r|
        module main (x)
        x = do
            let y = 1
            y
          |]
          int
      , assertGeneralType
          "do-block with bind and let"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        add :: Int -> Int -> Int
        x = do
            y <- f 1
            let z = add y 1
            z
          |]
          (ioEff int)
      , -- pure expression in do-block produces empty effects
        assertGeneralType
          "pure expression in do-block"
          [r|
        module main (x)
        add :: Int -> Int -> Int
        x = do add 1 2
          |]
          int
      , assertGeneralType
          "do-block with multiple effect labels"
          [r|
        module main (x)
        effect IO
        effect Error
        f :: Int -> <IO> Int
        g :: Int -> <Error> Int
        add :: Int -> Int -> Int
        x = do
          x <- f 1
          y <- g 2
          add x y
          |]
          (ioErrEff int)
      , -- chained binds feeding results forward
        assertGeneralType
          "do-block with chained dependent binds"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        add :: Int -> Int -> Int
        x = do
            a <- f 1
            b <- f a
            add a b
          |]
          (ioEff int)
      , -- do-block with effect annotation matching inferred effects
        assertGeneralType
          "annotated do-block matches inferred effects"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        x :: <IO> Int
        x = do
            y <- f 1
            y
          |]
          (ioEff int)
      , -- polymorphic function applied inside do-block
        assertGeneralType
          "polymorphic function in do-block"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        id :: a -> a
        x = do
          f (id 42)
          |]
          (ioEff int)
      , -- do-block returning a list with forces
        assertGeneralType
          "do-block returning list"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        x = do
          x <- f 1
          y <- f 2
          [x, y]
          |]
          (ioEff (lst int))

        -- Polymorphic effectful typeclass method used TWICE inside a
        -- nested do-block that is bound with '<-'. The inner do-block's
        -- final synths to a concrete type (Int, driven by 'add'), so the
        -- inner DoBlockS returns 'EffectU collected Int'. Without the
        -- ForallU peel in 'effectOfAnno', 'collected' comes back empty
        -- because 'random :: forall a. <IO> a' is not a bare EffectU;
        -- then 'mkEffectU' collapses '<> Int' to 'Int', and the outer
        -- '<-' EvalS throws "Cannot force a non-effectful value (got
        -- type Int)."
      , assertGeneralType
          "polymorphic method twice inside nested '<-' do-block"
          [r|
        module main (foo)
        effect IO
        class C a where
          random :: <IO> a
        instance C Int where
          source Py from "helpers.py" ("r" as random)
        add :: Int -> Int -> Int
        foo :: <IO> Int
        foo = do
          a <- (do
            v1 <- random
            v2 <- random
            add v1 v2)
          a
          |]
          (ioEff int)

        -- The same shape via '!' sugar. Under Option A, 'a <- add !v1 !v2'
        -- desugars to 'a <- (do; x <- v1; y <- v2; add x y)', so this
        -- exercises the identical typechecker path with a different
        -- surface syntax.
      , assertGeneralType
          "polymorphic method twice through '!' in a '<-' bind RHS"
          [r|
        module main (foo)
        effect IO
        class C a where
          random :: <IO> a
        instance C Int where
          source Py from "helpers.py" ("r" as random)
        add :: Int -> Int -> Int
        foo :: <IO> Int
        foo = do
          a <- add !random !random
          a
          |]
          (ioEff int)
      ]

-- | Program-level effect rejection tests.  Each test is a complete
-- morloc module that exercises a specific spec rule and asserts the
-- compiler refuses it.  The structure mirrors the spec subsections:
--
--   * Force misuse: '!' applied to a non-suspended value.
--   * Type mismatch inside a forced expression.
--   * Effectful value flowing into a pure parameter slot.
--   * Effect narrowing at a binding boundary (assigning a more-effectful
--     value to a less-effectful annotation).
--   * Effect narrowing at an application boundary (passing a more-
--     effectful argument to a less-effectful parameter).
--   * Effectful value flowing out of a function with a pure return type.
--   * Disjoint-effect mismatch (Rand passed where IO expected).
--
-- When the inference pass for declared-vs-inferred mismatches lands
-- (a separate item in the effects plan), additional widening-in-body
-- tests belong here.
effectErrorTests :: TestTree
effectErrorTests =
  localOption (mkTimeout 100000) $ -- 0.1 second timeout
    testGroup
      "Effect error tests"
      [ -- A function `g :: Int -> Int` requires a pure Int.  Passing
        -- the effectful application `f 1 :: <IO> Int` directly (no
        -- force) must be rejected: effects do not silently drop at
        -- the argument boundary.
        exprTestBad
          "effectful arg passed to pure parameter"
          [r|
        module main (x)
        effect IO
        f :: Int -> <IO> Int
        g :: Int -> Int
        x = g (f 1)
          |]

        -- Narrowing at a binding: rint produces <IO,Error> Int, but
        -- the annotation requests <IO> Int.  The Error tag would be
        -- silently dropped under the old permissive rule.  The strict
        -- rule rejects.
      , exprTestBad
          "narrowing at binding: <IO,Error> bound to <IO>"
          [r|
        module main (x)
        effect IO
        effect Error
        rint :: <IO, Error> Int
        x :: <IO> Int
        x = rint
          |]

        -- Narrowing at a binding to a pure type: dropping all effects
        -- in one step is rejected for the same reason.
      , exprTestBad
          "narrowing at binding: <IO> bound to pure"
          [r|
        module main (x)
        effect IO
        rint :: <IO> Int
        x :: Int
        x = rint
          |]

        -- Narrowing at an application boundary: `consume` accepts an
        -- <IO> Int parameter; passing an <IO,Error> Int would discard
        -- the Error effect at the call site.  Rejected.
      , exprTestBad
          "narrowing at application: <IO,Error> arg to <IO> param"
          [r|
        module main (x)
        effect IO
        effect Error
        rint :: <IO, Error> Int
        consume :: <IO> Int -> Int
        x = consume rint
          |]

        -- Disjoint effects between argument and parameter.  Rand is
        -- not a subset of IO; the call cannot be accepted under any
        -- interpretation of the rule.
      , exprTestBad
          "disjoint effects: <Rand> arg to <IO> param"
          [r|
        module main (x)
        effect Rand
        effect IO
        rrand :: <Rand> Int
        consume :: <IO> Int -> Int
        x = consume rrand
          |]
      ]

-- | Eval-sugar ('!' prefix) desugars to do-block binds pre-typecheck.
-- The typechecker never sees a '!'. These tests confirm:
--   * Types match the equivalent explicit do-block form.
--   * '!' at various expression positions (tuple, application, lambda,
--     let, guard branches) sequences into the enclosing scope, with
--     effect propagating outward (nothing is discharged).
--   * '!' at redundant positions ('<-' RHS, bare do stmt) is rejected
--     during desugaring.
--   * '!' on a pure value is rejected during typechecking as it would
--     be for an explicit do-bind.
evalSugarTests :: TestTree
evalSugarTests =
  localOption (mkTimeout 100000) $ -- 0.1 second timeout
    testGroup
      "Eval-sugar '!' desugar tests"
      [ -- '!' inside a tuple: hoist per-element to a wrapping do-block.
        -- Type must match the explicit do-block form.
        assertGeneralType
          "'!' in tuple: (!x, !y)"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        y :: <IO> Int
        foo = (!x, !y)
          |]
          (ioEff (tuple [int, int]))

        -- Reference: the equivalent explicit do-block has the same type.
      , assertGeneralType
          "do-block form of (!x, !y)"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        y :: <IO> Int
        foo = do
          a <- x
          b <- y
          (a, b)
          |]
          (ioEff (tuple [int, int]))

        -- '!' as function arguments: bar (!x) (!y).
      , assertGeneralType
          "'!' as function arguments"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo = add !x !y
          |]
          (ioEff int)

        -- '!' inside a lambda body: hoists INSIDE the body (per-call).
      , assertGeneralType
          "'!' in lambda body: \\x -> !(f x)"
          [r|
        module main (foo)
        effect IO
        f :: Int -> <IO> Int
        foo = \x -> !(f x)
          |]
          (fun [int, ioEff int])


        -- '!' in guard branches (branch-local): each branch is a boundary.
      , assertGeneralType
          "'!' in guard branches"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        y :: <IO> Int
        foo b
          ? b = !x
          : !y
          |]
          (fun [bool, ioEff int])

        -- '!' inside an existing do-block: hoist per-statement to a fresh
        -- bind inserted BEFORE the containing statement, preserving order.
      , assertGeneralType
          "'!' inside a do-block bind RHS"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo = do
          a <- add !x !y
          a
          |]
          (ioEff int)

        -- '!' inside a nested do-block: the inner do is a boundary; its
        -- effect does not leak out (the outer only sees the inner block's
        -- return type wrapped in its own inferred effect).
      , assertGeneralType
          "'!' in a nested do-block"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        y :: <IO> Int
        foo = do
          a <- do
            x
            x
          c <- do
            y
            y
          (a, c)
          |]
          (ioEff (tuple [int, int]))

        -- '!' with an AnnE: the ascription applies to the do-block wrapper.
      , assertGeneralType
          "'!' with type annotation"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo = (!x) :: <IO> Int
          |]
          (ioEff int)

        -- '!' on a pure value is rejected: EvalS on a non-effectful type.
      , exprTestBad
          "'!' on a pure value"
          [r|
        module main (foo)
        x :: Int
        foo :: Int
        foo = !x
          |]

        -- Redundant '!' on the RHS of '<-' is a desugar error.
      , exprTestBad
          "redundant '!' on RHS of '<-'"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo :: <IO> Int
        foo = do
          y <- !x
          y
          |]

        -- Redundant '!' as a bare do statement is a desugar error.
      , exprTestBad
          "redundant '!' as bare do statement"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo :: <IO> Int
        foo = do
          !x
          x
          |]

        -- '!' at the RHS of a let in a do-block is rejected. 'let'
        -- should bind a pure value; a bang at a hoistable position
        -- would silently rewrite the let into an effectful bind chain
        -- followed by a pure let, making the surface line read as pure
        -- while the effect fires above it. Bare form.
      , exprTestBad
          "bare '!' as RHS of let in do-block"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo :: <IO> Int
        foo = do
          let y = !x
          y
          |]

        -- Same rejection for a composite RHS: any '!' at a hoistable
        -- position inside the let RHS is rejected, not just a bare
        -- '!expr'.
      , exprTestBad
          "composite let RHS with '!' at hoistable position"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo :: <IO> Int
        foo = do
          let s = add !x !y
          s
          |]

        -- '!' inside a lambda body under a let is fine: the lambda body
        -- is a boundary, so the bang stays local (the effect fires when
        -- the function is applied, not at the let itself). The let
        -- correctly binds a function value.
      , assertGeneralType
          "'!' inside a lambda body under a let stays legal"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo :: <IO> Int
        foo = do
          let f = \z -> !x
          y <- f 42
          y
          |]
          (ioEff int)

        -- '!' inside the RHS of a '<-' bind is legal: the RHS is a
        -- boundary, so the bangs seal into a nested do-block wrapping
        -- the residual expression. This keeps the effect firing at the
        -- '<-' line rather than silently hoisting above it.
      , assertGeneralType
          "'!' inside a '<-' bind RHS with pure residual"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo :: <IO> Int
        foo = do
          a <- add !x !y
          a
          |]
          (ioEff int)

        -- Same for a bare non-final do statement: the RHS is a boundary.
      , assertGeneralType
          "'!' inside a bare non-final do statement"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo :: <IO> Int
        foo = do
          add !x !y
          x
          |]
          (ioEff int)

        -- The same rejection applies OUTSIDE a do-block: 'let x = !expr
        -- in body' is rejected. Keeps the mental model uniform (let
        -- always binds a pure value; effect firing goes through '<-')
        -- and avoids the compiler silently synthesizing a LetE with an
        -- effect-firing binding from a user 'let'.
      , exprTestBad
          "'!' at RHS of an expression-level 'let ... in ...'"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        foo :: <IO> Int
        foo = let z = !x in add z 1
          |]

        -- Same in composite form.
      , exprTestBad
          "composite '!' in expression-level let RHS"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        y :: <IO> Int
        foo :: <IO> Int
        foo = let s = add !x !y in s
          |]

        -- '!' inside a lambda body under an expression-level let is
        -- still legal (bang sealed by lambda boundary).
      , assertGeneralType
          "'!' inside a lambda body under expression-level let"
          [r|
        module main (foo)
        effect IO
        x :: <IO> Int
        foo :: Int -> <IO> Int
        foo = let f = \z -> !x in f
          |]
          (fun [int, ioEff int])

        -- '!' inside the BODY of an expression-level let is legal:
        -- the bang hoists into the LetE's continuation (bindings still
        -- in scope), and the whole let becomes an effectful expression.
      , assertGeneralType
          "'!' inside the body of an expression-level let"
          [r|
        module main (foo)
        effect IO
        add :: Int -> Int -> Int
        x :: <IO> Int
        foo :: <IO> Int
        foo = let n = 5 in add n !x
          |]
          (ioEff int)
      ]

-- | Escapability tests.  An effect is inescapable by default
-- (`effect E`); `escapable effect E` opts it into being
-- dischargeable.  Two rules interact:
--   * inescapable-propagation (signature-level): every inescapable
--     concrete label in a parameter's effect row must also appear in
--     the result row, so an inescapable effect is never silently
--     consumed -- not even by a handler-shaped signature.
--   * sourced-discharge (value-level): an escapable effect is exempt
--     from propagation, but only a SOURCED handler may actually drop
--     it; a defined function that carries the effect cannot.
-- The handlers are APPLIED and the positive cases assert the
-- discharged/propagated result type, so the assertion itself proves
-- the rule.  The escapable-keyword contrast is two otherwise-identical
-- applied programs differing only by the `escapable` keyword.
effectEscapabilityTests :: TestTree
effectEscapabilityTests =
  localOption (mkTimeout 100000) $ -- 0.1 second timeout
    testGroup
      "Effect escapability tests"
      [ -- === Escapable effect discharged by a SOURCED handler, applied ===
        -- foo performs the escapable Error; the sourced handler recover
        -- consumes <Error> Int and returns a pure Int.  Applying it
        -- must yield Int -- the assertion itself proves the Error was
        -- discharged at the call site, not merely that some unrelated
        -- binding is well typed.
        assertGeneralType
          "escapable Error discharged by sourced handler (applied)"
          [r|
        module main (x)
        escapable effect Error
        source Py ("foo")
        source Py ("recover")
        foo :: Int -> <Error> Int
        recover :: <Error> Int -> Int
        x :: Int
        x = recover (foo 1)
          |]
          int
      , -- Tail-variable handler <Error, e> a -> <e> a applied: Error is
        -- discharged and the row variable solves to the empty row, so
        -- the applied result reduces to a pure Int.
        assertGeneralType
          "escapable Error discharged, row variable solved (applied)"
          [r|
        module main (x)
        escapable effect Error
        source Py ("foo")
        source Py ("handle")
        foo :: Int -> <Error> Int
        handle :: <Error, e> a -> <e> a
        x :: Int
        x = handle (foo 1)
          |]
          int
      , -- An inescapable effect propagates THROUGH application: passt
        -- keeps Cap in its result, so applying it to an effectful
        -- argument yields <Cap> Int (the effect is not dropped).
        assertGeneralType
          "inescapable Cap propagates through application"
          [r|
        module main (x)
        effect Cap
        source Py ("foo")
        source Py ("passt")
        foo :: Int -> <Cap> Int
        passt :: <Cap> Int -> <Cap> Int
        x :: <Cap> Int
        x = passt (foo 1)
          |]
          (EffectU (EffectSet (Set.singleton "Cap")) int)

        -- === Escapable-keyword contrast (same applied program) ===
        -- Identical to the first test EXCEPT `escapable` is dropped, so
        -- Error is inescapable.  The handler is now rejected by the
        -- inescapable-propagation rule -- isolating exactly what the
        -- `escapable` keyword controls, with the handler applied.
      , exprTestBad
          "inescapable Error: the same applied program is rejected"
          [r|
        module main (x)
        effect Error
        source Py ("foo")
        source Py ("recover")
        foo :: Int -> <Error> Int
        recover :: <Error> Int -> Int
        x :: Int
        x = recover (foo 1)
          |]

        -- === Only a SOURCED handler may discharge an escapable effect ===
        -- recover is now DEFINED (recover t = t), not sourced.  Even
        -- though Error is escapable, a defined function that carries the
        -- effect cannot claim a pure result -- the discharge privilege
        -- belongs to sourced handlers alone.
      , exprTestBad
          "escapable Error: a defined (non-sourced) handler cannot discharge it"
          [r|
        module main (x)
        escapable effect Error
        source Py ("foo")
        foo :: Int -> <Error> Int
        recover :: <Error> Int -> Int
        recover t = t
        x :: Int
        x = recover (foo 1)
          |]

        -- === Inescapable effect cannot escape through application ===
        -- consume takes a pure Int; passing the inescapable effectful
        -- `foo 1 :: <Cap> Int` would silently drop Cap at the argument
        -- boundary.  Rejected.
      , exprTestBad
          "inescapable Cap cannot escape into a pure parameter (applied)"
          [r|
        module main (x)
        effect Cap
        source Py ("foo")
        source Py ("consume")
        foo :: Int -> <Cap> Int
        consume :: Int -> Int
        x :: Int
        x = consume (foo 1)
          |]

        -- === Signature-level rejections (illegal before any use) ===
        -- These declarations are rejected at the point of declaration
        -- by the inescapable-propagation rule, so no application is
        -- even reachable.
      , exprTestBad
          "signature: inescapable Cap in parameter absent from result"
          [r|
        module main (ok)
        effect Cap
        consume :: <Cap> Int -> Int
        ok :: Int
        ok = 42
          |]
      , exprTestBad
          "signature: handler-shaped <Cap,e> a -> <e> a rejected (Cap inescapable)"
          [r|
        module main (ok)
        effect Cap
        bad :: <Cap, e> a -> <e> a
        ok :: Int
        ok = 42
          |]
      , exprTestBad
          "signature: escapable Error droppable but inescapable Cap in same row leaks"
          [r|
        module main (ok)
        escapable effect Error
        effect Cap
        mixed :: <Error, Cap> Int -> <Error> Int
        ok :: Int
        ok = 42
          |]
      ]

-- | Effect propagation under partial application.
--
-- The hard invariant: any effect at the top level of an applied
-- argument's type must reach the terminal output of the enclosing
-- application. The bare-existential subtyping rule (instantiate
-- ExistU<->EffectU) solves a bare ?b to <IO> ?b' and stores the effect
-- inside the gamma binding for the consumed slot. Without the
-- application-site lift, the effect drops out of the user-visible spine
-- whenever it lands in a slot whose type variable does not appear in
-- the result. These tests pin down the post-fix behavior in five
-- positive shapes plus a negative case that locks in non-interference
-- with row-polymorphic handler-style signatures.
effectPartialApplicationTests :: TestTree
effectPartialApplicationTests =
  localOption (mkTimeout 200000) $ -- 0.2 second timeout
    testGroup
      "Effect propagation under partial application"
      [ -- Control: the consumed slot IS the result slot.  The instantiate
        -- path stores <IO> in ?a's binding, and ?a is the terminal, so
        -- the effect already reaches the output without the lift.
        assertGeneralType
          "f1 control: foo (bar str) -- consumed slot equals result slot"
          [r|
        module main (f1)
        effect IO
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        f1 y z = do
          x <- bar "hi"
          foo x y z
          |]
          (fun [exist "a", exist "b", ioEff str])

        -- Partial application: effectful arg goes into ?b (NOT the
        -- result slot).  Pre-fix the <IO> tag silently dropped because
        -- ?b is consumed.  Post-fix the lift places <IO> on the
        -- terminal Int.
      , assertGeneralType
          "f2 partial app: foo (int) (bar str) -- effect into non-result slot"
          [r|
        module main (f2)
        effect IO
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        f2 y = do
          x <- bar "hi"
          foo 42 x y
          |]
          (fun [exist "a", ioEff int])

        -- Full application: every slot consumed.  Pre-fix the function
        -- returns plain Int and the <IO> tag vanishes entirely.
        -- Post-fix the terminal is wrapped as <IO> Int.
      , assertGeneralType
          "f3 full app: foo (int) (bar str) (bool) -- effect must reach pure-Int result"
          [r|
        module main (f3)
        effect IO
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        f3 = do
          x <- bar "hi"
          foo 42 x True
          |]
          (ioEff int)

        -- Two effectful args, each in a bare-existential slot.  Both
        -- top-level effect sets must union onto the terminal output.
        -- Here only IO is involved, but two independent absorbing slots
        -- exercise the foldr-union path in liftAbsorbedEffects.
      , assertGeneralType
          "f4 two effectful args -- both contribute to terminal"
          [r|
        module main (f4)
        effect IO
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        qux :: Str -> <IO> Str
        f4 z = do
          x <- bar "hi"
          y <- qux "by"
          foo x y z
          |]
          (fun [exist "a", ioEff str])

        -- Different effects from different bare-existential slots must
        -- UNION into the terminal output, not be lost or pick one side.
        -- Exercises the foldr unionEffectSet path with non-equal sets.
      , assertGeneralType
          "f6 multi-effect union: <IO> arg and <Error> arg combine on terminal"
          [r|
        module main (f6)
        effect IO
        effect Error
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        qux :: Str -> <Error> Str
        f6 = do
          x <- bar "hi"
          y <- qux "by"
          foo 1 x y 
          |]
          (ioErrEff int)

        -- Let-bound effectful value flowing into a bare slot.  The
        -- let-RHS still synthesizes to <IO> Str and is checked against
        -- the bare existential through the same instantiate path, so
        -- the lift must fire identically.
      , assertGeneralType
          "f5 let-bound effectful value into non-result slot"
          [r|
        module main (f5)
        effect IO
        foo :: a -> b -> c -> a
        bar :: Str -> <IO> Str
        f5 = do
          x <- bar "hi"
          foo 42 x True
          |]
          (ioEff int)

        -- Negative: row-polymorphic handler signature.  The first
        -- parameter is <Error, e> a, an EffectU -- NOT a bare
        -- ExistU -- so isAbsorbing rejects it and the lift must not
        -- fire.  The Error effect is escapable and discharged by the
        -- handler; the result must be pure Int with no spurious <IO>
        -- or <Error> manufactured by the lift.
      , assertGeneralType
          "handleEsc: row-poly handler discharges escapable Error, lift does not interfere"
          [r|
        module main (h)
        escapable effect Error
        source Py ("foo")
        source Py ("handle")
        foo :: Int -> <Error> Int
        handle :: <Error, e> a -> <e> a
        h = handle (foo 1)
          |]
          int
      ]

-- | Pure values filling `<e> T` slots must recurse on the inner type
-- only. Solving the RHS row (via subtypeEffRows with a synthetic empty
-- LHS) would over-constrain any open row-variable to empty; adding an
-- existential-LHS guard would spuriously trip the occurs check when
-- the RHS's inner referenced the same existential.
polymorphicEffectRowTests :: TestTree
polymorphicEffectRowTests =
  localOption (mkTimeout 200000) $
    testGroup
      "Polymorphic effect row / pure-into-effect subtype"
      [ -- A helper that @catches on a polymorphic fallible must
        -- typecheck (guards against InstantiateL firing to solve
        -- `b := <e> b` and tripping the occurs check).
        expectPass
          "1. @catch on polymorphic <e, Err> b with bare fallback"
          [r|
        module main (withCatch)
        escapable effect Err
        withCatch :: (a -> <e, Err> b) -> a -> b -> <e> b
        withCatch f x fb = @catch (f x) fb
          |]

        -- A pure value filling a `<e> T` slot must NOT solve e := empty.
        -- The surrounding <IO> constraint is the real solver.
      , expectPass
          "2. pure Int in <e> Int slot does not pin e to empty"
          [r|
        module main (pureRoot)
        effect IO
        passThrough :: <e> Int -> <e> Int
        passThrough x = x
        pureRoot :: <IO> Int
        pureRoot = passThrough 42
          |]

        -- Trivial concrete-into-concrete pure lift still works.
      , expectPass
          "3. pure Int in <IO, Err> Int slot"
          [r|
        module main (x)
        effect IO
        escapable effect Err
        x :: <IO, Err> Int
        x = 42
          |]

        -- === Nested polymorphic @catch ===
        -- Outer @catch's fallback is itself an @catch, both polymorphic
        -- in e. Each layer independently exercises the pure-into-
        -- existential-EffectU rule when its bare fallback is checked.
      , expectPass
          "4. nested @catch, both layers polymorphic in e"
          [r|
        module main (withTwoCatches)
        escapable effect Err
        withTwoCatches :: (a -> <e, Err> b) -> (a -> <e, Err> b) -> a -> b -> <e> b
        withTwoCatches f g x fb = @catch (f x) (@catch (g x) fb)
          |]

        -- Two pure arguments: solving e := empty on the first would
        -- fail to unify against the export's <IO> on the second.
      , expectPass
          "5. row-var survives two pure arguments"
          [r|
        module main (run)
        effect IO
        pick :: <e> Int -> <e> Int -> <e> Int
        pick x _ = x
        run :: <IO> Int
        run = pick 42 99
          |]

        -- === Pure-then-effectful ordering ===
        -- First arg pure, second arg concretely effectful. The row
        -- var must be solvable to <IO> by the second arg despite the
        -- first arg not constraining it.
      , expectPass
          "6. row-var solved by effectful second arg after pure first"
          [r|
        module main (run)
        effect IO
        source Py ("saveInt")
        saveInt :: Int -> <IO> Int
        seq2 :: <e> Int -> <e> Int -> <e> Int
        seq2 _ y = y
        run :: <IO> Int
        run = seq2 42 (saveInt 99)
          |]

        -- === Negative: effect cannot drop when caller expects pure ===
        -- The pure-into-EffectU rule must NOT be bidirectional; the
        -- reverse (EffectU-into-pure) is unsound and must still reject.
      , expectError
          "7. <Err> Int in Int slot rejected (wrong subtype direction)"
          [r|
        module main (f)
        escapable effect Err
        f :: Int
        f = @throw "x"
          |]

      , expectError
          "8. <IO> Int in Int slot rejected via direct assignment"
          [r|
        module main (bad)
        effect IO
        source Py ("readInt")
        readInt :: <IO> Int
        bad :: Int
        bad = readInt
          |]

        -- === Negative: @catch on non-Err fallible still rejected ===
        -- Loosening the pure-into-EffectU rule must NOT loosen
        -- @catch's requirement that its fallible have Err in its row.
      , expectError
          "9. @catch on <IO>-only (no Err) fallible rejected"
          [r|
        module main (bad)
        effect IO
        source Py ("readInt")
        readInt :: <IO> Int
        bad :: <IO> Int
        bad = @catch readInt 0
          |]
      ]

-- | Row-inheritance of @catch. The fallback declares its own effect row
-- and the whole @catch inherits it, so the same operator handles both
-- "recover to pure" (fallback is <>) and "fall through to another
-- fallible attempt" (fallback keeps Err). The primary's non-Err effects
-- propagate too.
catchRowInheritTests :: TestTree
catchRowInheritTests =
  localOption (mkTimeout 200000) $
    testGroup
      "@catch row-inheritance"
      [ expectPass
          "chained @catch: fallback raises Err, result is <Err>"
          [r|
        module main (chained)
        escapable effect Err
        source Py ("thrower1", "thrower2")
        thrower1 :: <Err> Int
        thrower2 :: <Err> Int
        chained :: <Err> Int
        chained = @catch thrower1 thrower2
          |]

      , expectPass
          "nested @catch chain terminates in pure default -> stripped"
          [r|
        module main (safe)
        escapable effect Err
        source Py ("thrower1", "thrower2", "thrower3")
        thrower1 :: <Err> Int
        thrower2 :: <Err> Int
        thrower3 :: <Err> Int
        safe :: Int
        safe = @catch thrower1 (@catch thrower2 (@catch thrower3 0))
          |]

      , expectPass
          "primary <IO, Err>, pure fallback -> <IO>"
          [r|
        module main (recovered)
        effect IO
        escapable effect Err
        source Py ("readIntOrFail")
        readIntOrFail :: <IO, Err> Int
        recovered :: <IO> Int
        recovered = @catch readIntOrFail 0
          |]

      , expectPass
          "primary <IO, Err>, <Err> fallback -> <IO, Err>"
          [r|
        module main (retried)
        effect IO
        escapable effect Err
        source Py ("readIntOrFail", "retryOrFail")
        readIntOrFail :: <IO, Err> Int
        retryOrFail :: <Err> Int
        retried :: <IO, Err> Int
        retried = @catch readIntOrFail retryOrFail
          |]

      , expectPass
          "concrete <Err> primary + pure fallback strips to plain type"
          [r|
        module main (safe)
        escapable effect Err
        source Py ("thrower")
        thrower :: <Err> Int
        safe :: Int
        safe = @catch thrower 0
          |]

      , expectError
          "two independent open effect rows in @catch rejected"
          [r|
        module main (twoTail)
        escapable effect Err
        twoTail :: (a -> <e, Err> b) -> (a -> <f> b) -> a -> b
        twoTail f g x = @catch (f x) (g x)
          |]
      ]

-- | Effect-coverage error message shape.
--
-- After the message rewrite, effect-coverage failures name the
-- specific missing effects and adapt the fix suggestion based on
-- escapability (Err → mention @catch; other escapable → mention
-- handler function; all non-escapable → suggest declaration only).
--
-- Per the workspace convention we do NOT assert on exact message
-- text (it drifts as wording is tuned). We assert that these
-- programs are rejected (they must remain rejected under any future
-- message improvement) and rely on running the tests interactively
-- to eyeball the message quality.
effectCoverageMessageTests :: TestTree
effectCoverageMessageTests =
  localOption (mkTimeout 200000) $
    testGroup
      "Effect-coverage error messages (rejection-only, message quality checked manually)"
      [ -- Err missing → message should suggest declare + mention @catch.
        expectError
          "Err in body, sig declares only <IO> → rejected"
          [r|
        module main (bad)
        effect IO
        escapable effect Err
        bad :: <IO> Int
        bad = do
          @throw "oops"
          0
          |]

        -- Non-Err escapable missing → message should suggest declare
        -- + mention handler function (no @catch mention).
      , expectError
          "escapable non-Err in body, sig pure → rejected"
          [r|
        module main (bad)
        escapable effect Log
        source Py ("plog")
        plog :: Str -> <Log> ()
        bad :: Int
        bad = do
          plog "hi"
          0
          |]

        -- Non-escapable missing → message should suggest declaration
        -- ONLY (no handler-function mention).
      , expectError
          "inescapable effect in body, sig pure → rejected"
          [r|
        module main (bad)
        effect IO
        source Py ("psideEffect")
        psideEffect :: Int -> <IO> Int
        bad :: Int
        bad = do
          x <- psideEffect 1
          x
          |]

        -- Mixed escapable + inescapable missing → message should
        -- suggest declaration for all, mention handler for escapable
        -- ones, and specifically @catch for Err.
      , expectError
          "Err + inescapable both missing → rejected"
          [r|
        module main (bad)
        effect IO
        escapable effect Err
        source Py ("psideEffect")
        psideEffect :: Int -> <IO> Int
        bad :: Int
        bad = do
          x <- psideEffect 1
          @throw "boom"
          x
          |]
      ]

namespaceErrorTests :: TestTree
namespaceErrorTests =
  localOption (mkTimeout 2000000) $ -- 2 second timeout
    testGroup
      "Tests for namespace import error cases"
      [ -- chained namespace dots should be a parse error
        exprTestBad
          "chained namespace dots a.b.c"
          [r|
        module main (x)
        x :: Int
        x = a.b.c
          |]
      , -- keyword used as namespace should fail
        exprTestBad
          "keyword as namespace name (let)"
          [r|
        module foo (y)
        y :: Int
        y = 1
        module main (x)
        import foo as let
        x :: Int
        x = let.y
          |]
      , -- keyword used as namespace should fail
        exprTestBad
          "keyword as namespace name (do)"
          [r|
        module foo (y)
        y :: Int
        y = 1
        module main (x)
        import foo as do
        x :: Int
        x = do.y
          |]
      , -- undefined namespace prefix should fail
        exprTestBad
          "undefined namespace prefix"
          [r|
        module main (x)
        x :: Int
        x = noexist.foo 5
          |]
      , -- namespace-qualified name used with wrong arg type
        exprTestBad
          "namespace qualified name type mismatch"
          [r|
        module helpers (double)
        double :: Int -> Int
        double x = x
        module main (x)
        import helpers as h
        x :: Int
        x = h.double "hello"
          |]
      , -- bare name should fail when imported with namespace
        exprTestBad
          "bare name fails with namespace import"
          [r|
        module helpers (double)
        double :: Int -> Int
        double x = x
        module main (x)
        import helpers as h
        x :: Int
        x = double 5
          |]
      , -- typeclass methods are not exported as standalone symbols and
        -- cannot be selectively imported; this should raise an error
        -- (the test asserts only the presence of an error, not its text)
        exprTestBad
          "typeclass method cannot be selectively imported"
          [r|
        module eq (*)
        class Eq a where
          (==) :: a -> a -> Bool
        module main (test)
        import eq ((==))
        test :: Bool
        test = True
          |]
      ]

typeclassTests :: TestTree
typeclassTests =
  localOption (mkTimeout 200000) $ -- 0.2 second timeout
    testGroup
      "Typeclass tests"
      [ -- === ANNOTATION PROPAGATION FIX (the core bug) ===
        -- Annotation leaked through copyState/reindexExprI to implementation
        -- indices, causing checkG to wrongly constrain non-matching instances.

        -- Instance declaration order must not matter
        assertGeneralType
          "annotation selects Str instance (Str declared first)"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x :: Str
        x = mempty :: Str
          |]
          str

      , assertGeneralType
          "annotation selects Str instance (List declared first)"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid (List a) where
          mempty = []
        instance Monoid Str where
          mempty = ""
        x :: Str
        x = mempty :: Str
          |]
          str

      , -- Annotation selects the parametric instance
        assertGeneralType
          "annotation selects List instance"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x :: [Int]
        x = mempty :: [Int]
          |]
          (lst int)

      , -- Export signature alone (no inline annotation) resolves the instance
        assertGeneralType
          "export signature resolves instance without inline annotation"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x :: Str
        x = mempty
          |]
          str

      , -- === MONOMORPHIC ANNOTATION FIX ===
        -- Annotation on standalone polymorphic functions (not typeclass methods)
        -- leaked via copyState to MonomorphicExpr implementation indices.

        assertGeneralType
          "annotation on standalone polymorphic function with args"
          [r|
        module main (foo)
        type Py => Int = "int"
        myId :: a -> a
        source Py ("myId")
        foo :: Int
        foo = (myId :: Int -> Int) 42
          |]
          int

      , assertGeneralType
          "annotation on standalone polymorphic nullary function"
          [r|
        module main (foo)
        type Py => Real = "float"
        myVal :: a
        source Py ("myVal")
        foo :: Real
        foo = myVal :: Real
          |]
          real

      , -- === SUPERCLASS CONSTRAINTS ===

        assertGeneralType
          "superclass method usable with subclass instance"
          [r|
        module main (x)
        type Py => Str = "str"
        class Semigroup a where
          append :: a -> a -> a
        class Semigroup a => Monoid a where
          mempty :: a
        instance Semigroup Str where
          source Py from "foo.py" ("appendStr" as append)
        instance Monoid Str where
          mempty = ""
        x :: Str
        x = append "" ""
          |]
          str

      , -- === NEGATIVE TESTS ===

        exprTestBad
          "ambiguous: multiple instances, no annotation"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x = mempty
          |]

      , exprTestBad
          "no matching instance for annotated type"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        x :: Real
        x = mempty :: Real
          |]

      , exprTestBad
          "no instances defined"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        x :: Str
        x = mempty
          |]

      , exprTestBad
          "annotation contradicts export signature"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x :: Int
        x = mempty :: Str
          |]

      , -- === COERCION-AWARE INSTANCE RESOLUTION ===

        assertGeneralType
          "instance resolved through optional coercion"
          [r|
        module main (x)
        class Default a where
          def :: a
        instance Default Int where
          def = 0
        x :: ?Int
        x = def
          |]
          (OptionalU int)

      , assertGeneralType
          "typeclass method in effectful do-block"
          [r|
        module main (x)
        effect IO
        class Default a where
          def :: a
        instance Default Int where
          def = 0
        f :: Int -> <IO> Int
        x = do
          f def
          |]
          (ioEff int)

      , -- === INTERACTION WITH OTHER FEATURES ===

        assertGeneralType
          "typeclass method in let binding"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        x :: Str
        x =
          let y = (mempty :: Str)
          in y
          |]
          str

      , assertGeneralType
          "typeclass method resolved by function argument context"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid Str where
          mempty = ""
        instance Monoid (List a) where
          mempty = []
        f :: Str -> Str
        x :: Str
        x = f mempty
          |]
          str

      , assertGeneralType
          "class with multiple nullary methods"
          [r|
        module main (x)
        class Bounded a where
          minBound :: a
          maxBound :: a
        instance Bounded Int where
          minBound = 0
          maxBound = 100
        x :: Int
        x = minBound
          |]
          int

      , assertGeneralType
          "nested parametric instance"
          [r|
        module main (x)
        class Monoid a where
          mempty :: a
        instance Monoid (List a) where
          mempty = []
        x :: [[Int]]
        x = mempty
          |]
          (lst (lst int))
      ]

natErrorTests :: TestTree
natErrorTests =
  testGroup
    "nat typecheck errors"
    [ expectError
        "add dimension mismatch (4 != 5)"
        [r|
      module main (x)
      type Tensor2 d1 d2 a
      add :: Tensor2 m n Real -> Tensor2 m n Real -> Tensor2 m n Real
      a :: Tensor2 3 4 Real
      b :: Tensor2 3 5 Real
      x = add a b
        |]
    , expectError
        "matmul inner dimension mismatch (4 != 5)"
        [r|
      module main (x)
      type Tensor2 d1 d2 a
      matmul :: Tensor2 m k Real -> Tensor2 k n Real -> Tensor2 m n Real
      a :: Tensor2 3 4 Real
      b :: Tensor2 5 6 Real
      x = matmul a b
        |]
    , expectError
        "trace requires square matrix (3 != 4)"
        [r|
      module main (x)
      type Tensor2 d1 d2 a
      trace :: Tensor2 n n Real -> Real
      a :: Tensor2 3 4 Real
      x = trace a
        |]
    , expectError
        "dot product length mismatch (3 != 5)"
        [r|
      module main (x)
      type Tensor1 d1 a
      dot :: Tensor1 n Real -> Tensor1 n Real -> Real
      a :: Tensor1 3 Real
      b :: Tensor1 5 Real
      x = dot a b
        |]
    , expectError
        "vstack column dimension mismatch (3 != 4)"
        [r|
      module main (x)
      type Tensor2 d1 d2 a
      vstack :: Tensor2 m n Real -> Tensor2 p n Real -> Tensor2 m n Real
      a :: Tensor2 2 3 Real
      b :: Tensor2 4 4 Real
      x = vstack a b
        |]
    , expectError
        "nat arithmetic mismatch: (2+3) != 4"
        [r|
      module main (x)
      type SizedList n a = [a]
      append :: SizedList m a -> SizedList n a -> SizedList (m + n) a
      a :: SizedList 2 Int
      b :: SizedList 3 Int
      x :: SizedList 4 Int
      x = append a b
        |]
    ]

natDimTests :: TestTree
natDimTests =
  testGroup
    "nat dimension checking on list literals"
    [ expectError
        "list literal dimension mismatch (3 != 4)"
        [r|
      module main (foo)
      type Matrix (m :: Nat) (n :: Nat) a = [[a]]
      foo = [[1,2,3],[4,5,6]] :: Matrix 2 4 Int
        |]
    , testCase "list literal dimensions match" $ do
        result <- runFront [r|
          module main (foo)
          type Matrix (m :: Nat) (n :: Nat) a = [[a]]
          foo = [[1,2,3],[4,5,6]] :: Matrix 2 3 Int
            |]
        case result of
          Right _ -> return ()
          Left e -> assertFailure $ "Expected success, got: " <> show e
    , testCase "infer nat dimensions from list literal" $ do
        result <- runFrontRaw [r|
          module main (foo)
          type Matrix (m :: Nat) (n :: Nat) a = [[a]]
          foo :: Matrix m n Int
          foo = [[1,2,3],[4,5,6]]
            |]
        case result of
          Right [x] ->
            let t = gtypeof x
            in case t of
              AppU (VarU (TV "Matrix")) [NatLitU 2, NatLitU 3, _] -> return ()
              _ -> assertFailure $ "Expected Matrix 2 3 Int, got: " <> show t
          Right _ -> assertFailure "Expected exactly one export"
          Left e -> assertFailure $ "Expected success, got: " <> show e
    ]

gradualDesugarTests :: TestTree
gradualDesugarTests =
  testGroup
    "gradual desugar of missing kind arguments"
    [ -- Baseline: pre-existing annotation forms still typecheck.
      expectPass "concrete-Nat: `Vector 3 Int`"
        [r|
      module main (foo)
      type Vector (n :: Nat) a = [a]
      foo :: Vector 3 Int -> Int
      foo _ = 0
        |]
    , expectPass "polymorphic-Nat: `Vector n Int`"
        [r|
      module main (foo)
      type Vector (n :: Nat) a = [a]
      foo :: Vector n Int -> Int
      foo _ = 0
        |]
    , expectPass "Nat arithmetic: `Vector (2+3) Int`"
        [r|
      module main (foo)
      type Vector (n :: Nat) a = [a]
      foo :: Vector (2+3) Int -> Int
      foo _ = 0
        |]

    -- Single-Nat gradual form.
    , expectPass "gradual `Vector Int` typechecks"
        [r|
      module main (foo)
      type Vector (n :: Nat) a = [a]
      foo :: Vector Int -> Int
      foo _ = 0
        |]
    , expectPass "gradual `Vector Int` accepts list literal of any length"
        [r|
      module main (xs)
      type Vector (n :: Nat) a = [a]
      xs :: Vector Int
      xs = [1, 2, 3, 4, 5]
        |]
    , expectPass "gradual `[Vector Int]` accepts het-length elements"
        [r|
      module main (xs)
      type Vector (n :: Nat) a = [a]
      xs :: [Vector Int]
      xs = [[1, 2], [3, 4, 5], [6]]
        |]

    -- Structural check on the desugar output. After alias reduction
    -- `Vector NatVoidU Int` may collapse to `[Int]`; both encode the
    -- same runtime shape.
    , testCase "gradual `Vector Int` desugars to `AppU Vector [NatVoidU, Int]`" $ do
        result <- runFrontRaw [r|
          module main (foo)
          type Vector (n :: Nat) a = [a]
          foo :: Vector Int -> Int
          foo _ = 0
            |]
        case result of
          Right [x] -> case stripForalls (gtypeof x) of
            FunU [AppU (VarU (TV "Vector")) [NatVoidU, VarU (TV "Int")]] _ -> return ()
            FunU [AppU (VarU (TV "List")) [VarU (TV "Int")]] _ -> return ()
            t -> assertFailure $
                   "Expected `Vector NatVoidU Int -> _` or `[Int] -> _`, got: " <> show t
          Right _ -> assertFailure "Expected exactly one export"
          Left e  -> assertFailure $ "Expected success, got: " <> show e

    -- Concrete <-> gradual subtype flows in both directions.
    , expectPass "concrete `Vector 3 Int` flows into `Vector Int` arg"
        [r|
      module main (result)
      type Vector (n :: Nat) a = [a]
      f :: Vector Int -> Int
      f _ = 0
      xs :: Vector 3 Int
      xs = [1, 2, 3]
      result :: Int
      result = f xs
        |]
    , expectPass "gradual `Vector Int` flows into `Vector 3 Int` arg"
        [r|
      module main (result)
      type Vector (n :: Nat) a = [a]
      f :: Vector 3 Int -> Int
      f _ = 0
      xs :: Vector Int
      xs = [1, 2, 3]
      result :: Int
      result = f xs
        |]

    -- Multi-Nat: Tensor3-style with 3 Nat params + 1 Type.
    , expectPass "gradual `Tensor3 Int` (3 Nat positions missing)"
        [r|
      module main (foo)
      type Tensor3 (i :: Nat) (j :: Nat) (k :: Nat) a = [a]
      foo :: Tensor3 Int -> Int
      foo _ = 0
        |]
    , expectPass "gradual `Tensor3 h Int` (2 Nat positions missing)"
        [r|
      module main (foo)
      type Tensor3 (i :: Nat) (j :: Nat) (k :: Nat) a = [a]
      foo :: Tensor3 h Int -> Int
      foo _ = 0
        |]
    , expectPass "gradual `Tensor3 h w Int` (1 Nat position missing)"
        [r|
      module main (foo)
      type Tensor3 (i :: Nat) (j :: Nat) (k :: Nat) a = [a]
      foo :: Tensor3 h w Int -> Int
      foo _ = 0
        |]
    , expectPass "fully-specified `Tensor3 2 3 5 Int` (no desugar)"
        [r|
      module main (foo)
      type Tensor3 (i :: Nat) (j :: Nat) (k :: Nat) a = [a]
      foo :: Tensor3 2 3 5 Int -> Int
      foo _ = 0
        |]

    -- Multi-kind: Table with Nat + Rec params.
    , expectPass "gradual `Table` (both Nat and Rec missing)"
        [r|
      module main (foo)
      type Table (n :: Nat) (r :: Rec) = Int
      foo :: Table -> Int
      foo _ = 0
        |]
    , expectPass "partial `Table 100` (only Rec missing)"
        [r|
      module main (foo)
      type Table (n :: Nat) (r :: Rec) = Int
      foo :: Table 100 -> Int
      foo _ = 0
        |]

    -- Type-position under-application is a normal arity error.
    , expectError "bare `Vector` (missing Type arg) is rejected"
        [r|
      module main (foo)
      type Vector (n :: Nat) a = [a]
      foo :: Vector -> Int
      foo _ = 0
        |]

    -- Regression: polymorphic-Nat call-through via NatSolver's poly extractor.
    , expectPass "`g v = f v` with `Vector n Int` on both sides"
        [r|
      module main (g)
      type Vector (n :: Nat) a = [a]
      f :: Vector n Int -> Int
      f _ = 0
      g :: Vector n Int -> Int
      g v = f v
        |]

    -- Regression: gradual under-applied type constructor inside a
    -- type-alias body (e.g. `type Read a = (a, Vector U8)`) must also
    -- get its missing Nat position padded. Without walking TypE bodies
    -- through the fillMissingKindArgs pass, the alias would keep the
    -- unpadded `AppU Vector [U8]` and downstream newtype reduction
    -- couldn't match the 2-arg Vector definition.
    , expectPass "gradual `Vector U8` under a type alias reduces cleanly"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = List a
      type Read a = (a, Vector U8)
      foo :: Read Str -> Read Str
      foo x = x
        |]
    , expectPass "gradual `Vector U8` under a paramless type alias"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = List a
      type Bytes = Vector U8
      foo :: Bytes -> Bytes
      foo x = x
        |]

    , expectPass "gradual `Vector U8` in an instance head"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = List a
      class Fooable a where fooOf :: a -> Int
      instance Fooable (Vector U8) where
        fooOf _ = 0
      foo :: Vector U8 -> Int
      foo v = fooOf v
        |]

    , expectPass "gradual `Vector U8` in an expression annotation"
        [r|
      module main (foo)
      newtype Vector (n :: Nat) a = List a
      foo :: Int
      foo = let v = ([1, 2, 3] :: Vector U8) in 0
        |]

    , exprTestBad
        "unknown kind name is rejected with a source-located error"
        [r|
      module main (foo)
      type Vector (n :: Nut) a = [a]
      foo :: Vector 3 Int -> Int
      foo _ = 0
        |]

    , expectPass "partial gradual with Rec literal preserves the schema"
        [r|
      module main (foo)
      newtype Table (n :: Nat) (r :: Rec)
      foo :: Table {x = Int} -> Int
      foo _ = 0
        |]
    ]
  where
    stripForalls (ForallU _ t) = stripForalls t
    stripForalls t = t

natArithTests :: TestTree
natArithTests =
  testGroup
    "nat arithmetic (sub, div, solver fix)"
    [ -- NatSolver unit tests
      testCase "ground subtraction: (10 - 3) ~ 7" $
        let e1 = NS.NatSub (NS.NatLit 10) (NS.NatLit 3)
            e2 = NS.NatLit 7
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" subs Map.empty
             Left err -> assertFailure $ "Expected success, got: " ++ show err
    , testCase "ground division: (12 / 4) ~ 3" $
        let e1 = NS.NatDiv (NS.NatLit 12) (NS.NatLit 4)
            e2 = NS.NatLit 3
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" subs Map.empty
             Left err -> assertFailure $ "Expected success, got: " ++ show err
    , testCase "subtraction mismatch: (10 - 3) ~ 8" $
        let e1 = NS.NatSub (NS.NatLit 10) (NS.NatLit 3)
            e2 = NS.NatLit 8
        in case NS.solveNat e1 e2 of
             Left NS.Contradiction -> return ()
             other -> assertFailure $ "Expected Contradiction, got: " ++ show other
    , testCase "division mismatch: (12 / 4) ~ 4" $
        let e1 = NS.NatDiv (NS.NatLit 12) (NS.NatLit 4)
            e2 = NS.NatLit 4
        in case NS.solveNat e1 e2 of
             Left NS.Contradiction -> return ()
             other -> assertFailure $ "Expected Contradiction, got: " ++ show other
    , testCase "subtraction with variable: n - 3 ~ 5 => n = 8" $
        let e1 = NS.NatSub (NS.NatVar (TV "n")) (NS.NatLit 3)
            e2 = NS.NatLit 5
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" (Map.singleton (TV "n") (NS.NatLit 8)) subs
             Left err -> assertFailure $ "Expected n=8, got: " ++ show err
    , testCase "division by constant: n / 3 with n = 9 / 3 ~ 3" $
        let e1 = NS.NatDiv (NS.NatLit 9) (NS.NatLit 3)
            e2 = NS.NatLit 3
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" subs Map.empty
             Left err -> assertFailure $ "Expected success, got: " ++ show err
    -- Polynomial substitution. When the caller passes a @(v, rhs)@ the
    -- assertion also checks that @v@ is the chosen substituted variable;
    -- passing @Nothing@ checks only correctness (satisfaction), useful
    -- when the direction depends on sort-order tie-breaking.
    , testCase "polynomial: i*j ~ n => n = i*j" $
        assertSolvesTo (nMul (nVar "i") (nVar "j")) (nVar "n")
                       (Just (TV "n", nMul (nVar "i") (nVar "j")))
    , testCase "polynomial: n ~ i + j => n = i + j" $
        assertSolvesTo (nVar "n") (nAdd (nVar "i") (nVar "j"))
                       (Just (TV "n", nAdd (nVar "i") (nVar "j")))
    , testCase "polynomial: n ~ 2*i + 3 => n = 2*i + 3" $
        assertSolvesTo (nVar "n") (nAdd (nMul (nLit 2) (nVar "i")) (nLit 3))
                       (Just (TV "n", nAdd (nMul (nLit 2) (nVar "i")) (nLit 3)))
    , testCase "asymmetric: 2*n ~ i => i = 2*n (only division-exact direction)" $
        assertSolvesTo (nMul (nLit 2) (nVar "n")) (nVar "i")
                       (Just (TV "i", nMul (nLit 2) (nVar "n")))
    , testCase "variable unify: n1 ~ n2 (correctness only; direction ties)" $
        assertSolvesTo (nVar "n1") (nVar "n2") Nothing
    , testCase "divisible: 2*n ~ 2*i (correctness only; direction ties)" $
        assertSolvesTo (nMul (nLit 2) (nVar "n")) (nMul (nLit 2) (nVar "i")) Nothing
    -- Genuinely undecidable cases: no linear substitution captures them.
    , testCase "undecidable: n*m ~ 5 is Deferred" $
        assertDeferred (nMul (nVar "n") (nVar "m")) (nLit 5)
    , testCase "undecidable Diophantine: 2*n + 3*m ~ 5 is Deferred" $
        assertDeferred (nAdd (nMul (nLit 2) (nVar "n")) (nMul (nLit 3) (nVar "m"))) (nLit 5)
    , testCase "linear solving still works: n + 3 ~ 8 => n = 5" $
        let e1 = NS.NatAdd (NS.NatVar (TV "n")) (NS.NatLit 3)
            e2 = NS.NatLit 8
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" (Map.singleton (TV "n") (NS.NatLit 5)) subs
             Left err -> assertFailure $ "Expected n=5, got: " ++ show err
    , testCase "simple variable solving: n ~ 5" $
        let e1 = NS.NatVar (TV "n")
            e2 = NS.NatLit 5
        in case NS.solveNat e1 e2 of
             Right subs -> assertEqual "" (Map.singleton (TV "n") (NS.NatLit 5)) subs
             Left err -> assertFailure $ "Expected n=5, got: " ++ show err
    -- Typechecker integration tests for sub/div syntax
    , expectError
        "ground subtraction mismatch: (10-3) != 8 in type annotation"
        [r|
      module main (x)
      type SizedList n a = [a]
      a :: SizedList (10 - 3) Int
      x :: SizedList 8 Int
      x = a
        |]
    , expectError
        "ground division mismatch: (12/4) != 4 in type annotation"
        [r|
      module main (x)
      type SizedList n a = [a]
      a :: SizedList (12 / 4) Int
      x :: SizedList 4 Int
      x = a
        |]
    -- deferred constraint re-checking: variable arithmetic caught after solving
    , expectError
        "deferred subtraction mismatch: m=8, n=3, but m-n used as 7"
        [r|
      module main (x)
      type SizedList n a = [a]
      take :: SizedList (m - n) a -> SizedList n a -> SizedList m a
      a :: SizedList 7 Int
      b :: SizedList 3 Int
      x :: SizedList 8 Int
      x = take a b
        |]
    , expectError
        "deferred multiplication mismatch: n*m=12 but n=5 (no integer m)"
        [r|
      module main (x)
      type SizedList n a = [a]
      split :: SizedList (n * m) a -> SizedList n a
      a :: SizedList 12 Int
      x :: SizedList 5 Int
      x = split a
        |]
    ]
  where
    nVar s = NS.NatVar (TV s)
    nLit  = NS.NatLit
    nAdd  = NS.NatAdd
    nMul  = NS.NatMul

    assertSolvesTo :: NS.NatExpr -> NS.NatExpr -> Maybe (TVar, NS.NatExpr) -> Assertion
    assertSolvesTo e1 e2 mExpected = case NS.solveNat e1 e2 of
      Left err -> assertFailure $ "Expected solve, got: " ++ show err
      Right subs -> do
        assertBool ("Substitution does not satisfy: " ++ show subs)
          (NS.natEqual (NS.substituteNat subs e1) (NS.substituteNat subs e2))
        case mExpected of
          Nothing -> return ()
          Just (v, rhs) -> case Map.lookup v subs of
            Just e | NS.natEqual e rhs -> return ()
            Just e  -> assertFailure $ "Direction: expected " ++ show v ++ " = "
                                     ++ show rhs ++ ", got " ++ show e
            Nothing -> assertFailure $ "Direction: expected " ++ show v
                                     ++ " in subs, got: " ++ show subs

    assertDeferred :: NS.NatExpr -> NS.NatExpr -> Assertion
    assertDeferred e1 e2 = case NS.solveNat e1 e2 of
      Left (NS.Deferred _) -> return ()
      Right subs -> assertFailure $ "Expected Deferred, got solved: " ++ show subs
      Left NS.Contradiction -> assertFailure "Expected Deferred, got Contradiction"

-- | Tests that typedef expansion correctly substitutes kind-specific
-- variables (NatVarU/StrVarU/RecVarU/ListVarU/SetVarU) into the body.
-- Regression coverage for the parsub fix: previously parsub treated
-- these as inert (parallel to its NatVarU base case), so typedefs of
-- the form @type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int@ would
-- leave the body's promoted Nat variables unbound after expansion. The
-- function-call substitution path (Internal.apply / gammaNatSubs) was
-- unaffected, which is why polymorphic function signatures worked but
-- typedef-style nat arithmetic did not.
typedefKindVarTests :: TestTree
typedefKindVarTests =
  testGroup
    "typedef expansion substitutes kind-specific variables"
    [ -- === Direct evaluateType tests (no morloc source, no frontend) ===
      testCase "evaluateType: Nat typedef param substitutes NatLitU into NatAddU" $
        -- Scope: type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatAddU (NatVarU (TV "n")) (NatVarU (TV "m")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat), Left (TV "m", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 3, NatLitU 2]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatAddU (NatLitU 3) (NatLitU 2), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: Nat typedef param leaves unrelated NatVarU intact" $
        -- type Foo (n :: Nat) = Vector (k + n) Int
        -- Foo 3 should give Vector (k + 3) Int (k stays NatVarU)
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatAddU (NatVarU (TV "k")) (NatVarU (TV "n")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 3]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatAddU (NatVarU (TV "k")) (NatLitU 3), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: Nat typedef param substitutes through NatSubU" $
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatSubU (NatVarU (TV "n")) (NatVarU (TV "m")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat), Left (TV "m", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 5, NatLitU 3]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatSubU (NatLitU 5) (NatLitU 3), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: Nat typedef param substitutes through NatMulU" $
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatMulU (NatVarU (TV "n")) (NatVarU (TV "m")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat), Left (TV "m", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 3, NatLitU 2]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatMulU (NatLitU 3) (NatLitU 2), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: Nat typedef param substitutes through NatDivU" $
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatDivU (NatVarU (TV "n")) (NatVarU (TV "m")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat), Left (TV "m", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 6, NatLitU 2]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatDivU (NatLitU 6) (NatLitU 2), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: Str typedef param substitutes through StrConcatU" $
        let bodyT = StrConcatU (StrVarU (TV "s")) (StrLitU "_suffix")
            params = [Left (TV "s", KindStr)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [StrLitU "prefix"]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (StrConcatU (StrLitU "prefix") (StrLitU "_suffix"))
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: substitution at one position does NOT leak to other vars" $
        -- type Foo (n :: Nat) = Vector (n + n) Int
        -- Foo 4 should give Vector (4 + 4) Int (both occurrences substituted)
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatAddU (NatVarU (TV "n")) (NatVarU (TV "n")), VarU (TV "Int")]
            params = [Left (TV "n", KindNat)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 4]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatAddU (NatLitU 4) (NatLitU 4), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e
    , testCase "evaluateType: mixed Type and Nat typedef params" $
        -- type Foo (n :: Nat) a = Vector (n + 1) a
        let bodyT = AppU (VarU (TV "Vector"))
                      [NatAddU (NatVarU (TV "n")) (NatLitU 1), VarU (TV "a")]
            params = [Left (TV "n", KindNat), Left (TV "a", KindType)]
            scope = Map.singleton (TV "Foo")
                      [(params, bodyT, ArgDocAlias defaultValue, False, TypedefAlias)]
            input = AppU (VarU (TV "Foo")) [NatLitU 4, VarU (TV "Int")]
        in case TE.evaluateType scope input of
             Right t -> assertEqual ""
                          (AppU (VarU (TV "Vector"))
                            [NatAddU (NatLitU 4) (NatLitU 1), VarU (TV "Int")])
                          t
             Left e -> assertFailure $ "Expected expansion, got: " ++ show e

      -- === End-to-end morloc-source tests ===
      -- The user's original reproducer: typechecking succeeds when the
      -- typedef-parameter substitution correctly threads through Nat ops.
    , assertGeneralType
        "Foo n m = Vector (n + m) Int; Foo 3 2 = [1..5]"
        [r|
      module main (x)
      type Vector (n :: Nat) a = List a
      type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
      x :: Foo 3 2
      x = [1, 2, 3, 4, 5]
        |]
        (lst (var "Int"))
    , assertGeneralType
        "Foo n m = Vector (n - m) Int; Foo 5 3 = [1, 2]"
        [r|
      module main (x)
      type Vector (n :: Nat) a = List a
      type Foo (n :: Nat) (m :: Nat) = Vector (n - m) Int
      x :: Foo 5 3
      x = [1, 2]
        |]
        (lst (var "Int"))
    , assertGeneralType
        "Foo n m = Vector (n * m) Int; Foo 3 2 = [1..6]"
        [r|
      module main (x)
      type Vector (n :: Nat) a = List a
      type Foo (n :: Nat) (m :: Nat) = Vector (n * m) Int
      x :: Foo 3 2
      x = [1, 2, 3, 4, 5, 6]
        |]
        (lst (var "Int"))
    , assertGeneralType
        "Foo n m = Vector (n / m) Int; Foo 6 2 = [1..3]"
        [r|
      module main (x)
      type Vector (n :: Nat) a = List a
      type Foo (n :: Nat) (m :: Nat) = Vector (n / m) Int
      x :: Foo 6 2
      x = [1, 2, 3]
        |]
        (lst (var "Int"))
    , expectError
        "Foo n m = Vector (n + m) Int; Foo 3 2 with wrong-length value fails"
        [r|
      module main (x)
      type Vector (n :: Nat) a = List a
      type Foo (n :: Nat) (m :: Nat) = Vector (n + m) Int
      x :: Foo 3 2
      x = [1, 2, 3, 4, 5, 6]
        |]
    , assertGeneralType
        "Mixed Type/Nat params: Foo (n :: Nat) a = Vector (n + 1) a; Foo 4 Int = [1..5]"
        [r|
      module main (x)
      type Vector (n :: Nat) a = List a
      type Foo (n :: Nat) a = Vector (n + 1) a
      x :: Foo 4 Int
      x = [1, 2, 3, 4, 5]
        |]
        (lst (var "Int"))
    , assertGeneralType
        "Repeated Nat param: Foo (n :: Nat) = Vector (n + n) Int; Foo 3 = [1..6]"
        [r|
      module main (x)
      type Vector (n :: Nat) a = List a
      type Foo (n :: Nat) = Vector (n + n) Int
      x :: Foo 3
      x = [1, 2, 3, 4, 5, 6]
        |]
        (lst (var "Int"))
    , assertGeneralType
        "Single Nat param: Foo (n :: Nat) = Vector (n + 1) Int; Foo 4 = [1..5]"
        [r|
      module main (x)
      type Vector (n :: Nat) a = List a
      type Foo (n :: Nat) = Vector (n + 1) Int
      x :: Foo 4
      x = [1, 2, 3, 4, 5]
        |]
        (lst (var "Int"))

    -- Bare VarU classifies as a Nat expression (see typeUToNatExpr's
    -- VarU case in Typecheck/Internal.hs), so a synthetic pair that
    -- classifies as NONE of Nat/Str/Rec/List/Set must use a shape no
    -- classifier accepts: FunU here.
    , testCase "recheckDeferred rejects an unclassifiable pair" $
        let bad = FunU [] (VarU (TV "a"))
            g0 = (listToGamma []) { gammaDeferred = [(bad, bad)] }
        in case MTI.recheckDeferred g0 of
             Left msg ->
               assertBool ("expected 'unclassifiable' in error, got: " ++ show msg)
                 ("unclassifiable" `MT.isInfixOf` MT.pack (show msg))
             Right _ ->
               assertFailure "expected Left, got Right (classifier let unknown-kind pair through)"
    ]

natLabelTests :: TestTree
natLabelTests =
  testGroup
    "nat labeled params (m@Int syntax)"
    [ -- === Positive: literal int args resolve nat vars ===
      assertRawType
        "labeled literal resolves dimension: makeVec 5 :: Tensor1 5 Real"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec 5
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 5, VarU (TV "Real")])
    , assertRawType
        "labeled literal zero dimension: makeVec 0 :: Tensor1 0 Real"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec 0
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 0, VarU (TV "Real")])
    , assertRawType
        "two labeled params resolve: makeMat 3 4 :: Tensor2 3 4 Real"
        [r|
      module main (x)
      type Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      makeMat :: m@Int -> n@Int -> Tensor2 m n Real
      x = makeMat 3 4
        |]
        (AppU (VarU (TV "Tensor2")) [NatLitU 3, NatLitU 4, VarU (TV "Real")])
    , assertRawType
        "labeled dims flow through generic op: id_ (makeVec 7) :: Tensor1 7 Real"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      id_ :: Tensor1 n Real -> Tensor1 n Real
      x = id_ (makeVec 7)
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 7, VarU (TV "Real")])
    , assertRawType
        "labeled dims with nat arithmetic: conv output dims computed"
        [r|
      module main (x)
      type Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      type Tensor3 (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) a
      makeImg :: h@Int -> w@Int -> Tensor2 h w Real
      makeK :: k@Int -> fh@Int -> fw@Int -> Tensor3 k fh fw Real
      type Tensor1 (d :: Nat) a = [a]
      makeB :: k@Int -> Tensor1 k Real
      conv :: Tensor2 h w Real -> Tensor3 k fh fw Real -> Tensor1 k Real -> Tensor3 k (h - fh + 1) (w - fw + 1) Real
      x = conv (makeImg 5 5) (makeK 2 3 3) (makeB 2)
        |]
        (AppU (VarU (TV "Tensor3")) [NatLitU 2, NatLitU 3, NatLitU 3, VarU (TV "Real")])
    , assertRawType
        "labeled + flatten nat arithmetic: 2*3*3 = 18"
        [r|
      module main (x)
      type Tensor3 (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) a
      type Tensor1 (d :: Nat) a = [a]
      makeT :: a@Int -> b@Int -> c@Int -> Tensor3 a b c Real
      flatten :: Tensor3 a b c Real -> Tensor1 (a * b * c) Real
      x = flatten (makeT 2 3 3)
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 18, VarU (TV "Real")])
    , assertRawType
        "mixed labeled and unlabeled args"
        [r|
      module main (x)
      type Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      makeT :: m@Int -> n@Int -> Tensor2 m n Real
      scale :: Real -> Tensor2 m n Real -> Tensor2 m n Real
      x = scale 2.0 (makeT 3 4)
        |]
        (AppU (VarU (TV "Tensor2")) [NatLitU 3, NatLitU 4, VarU (TV "Real")])
    , assertRawType
        "same label var used in two positions (diagonal)"
        [r|
      module main (x)
      type Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      eye :: n@Int -> Tensor2 n n Real
      x = eye 4
        |]
        (AppU (VarU (TV "Tensor2")) [NatLitU 4, NatLitU 4, VarU (TV "Real")])

    -- === Positive: let-bound integers resolve nat labels ===
    , assertRawType
        "let-bound int resolves label: let n = 5 in makeVec n"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = let n = 5 in makeVec n
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 5, VarU (TV "Real")])
    , assertRawType
        "chained let-bound: let a = 7 in let b = a in makeVec b"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = let a = 7 in let b = a in makeVec b
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 7, VarU (TV "Real")])
    , assertRawType
        "multiple let-bound dims: let m=3, n=4 in makeMat m n"
        [r|
      module main (x)
      type Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      makeMat :: m@Int -> n@Int -> Tensor2 m n Real
      x = let m = 3 in let n = 4 in makeMat m n
        |]
        (AppU (VarU (TV "Tensor2")) [NatLitU 3, NatLitU 4, VarU (TV "Real")])

    -- === Positive: tuple accessor evaluation ===
    , assertRawType
        "tuple accessor resolves: makeVec (.0 (5, 6))"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec (.0 (5, 6))
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 5, VarU (TV "Real")])
    , assertRawType
        "let-bound tuple + accessor: let dims = (3,4) in makeMat (.0 dims) (.1 dims)"
        [r|
      module main (x)
      type Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      makeMat :: m@Int -> n@Int -> Tensor2 m n Real
      x = let dims = (3, 4) in makeMat (.0 dims) (.1 dims)
        |]
        (AppU (VarU (TV "Tensor2")) [NatLitU 3, NatLitU 4, VarU (TV "Real")])
    , assertRawType
        "chained let + accessor: let d=(8,9); let n=.0 d in makeVec n"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = let d = (8, 9) in let n = .0 d in makeVec n
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 8, VarU (TV "Real")])

    -- === Positive: lambda application evaluation ===
    , assertRawType
        "identity lambda: makeVec ((\\x -> x) 5)"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec ((\n -> n) 5)
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 5, VarU (TV "Real")])
    , assertRawType
        "lambda + accessor: makeVec ((\\t -> .1 t) (1,2,3))"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec ((\t -> .1 t) (1, 2, 3))
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 2, VarU (TV "Real")])
    , assertRawType
        "lambda selects first of two args: (\\x y -> x) 7 99"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x = makeVec ((\a b -> a) 7 99)
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 7, VarU (TV "Real")])

    -- === Negative: dimension mismatches caught despite labels ===
    , expectError
        "labeled dim mismatch: add (makeT 3 4) (makeT 3 5) fails"
        [r|
      module main (x)
      type Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      makeT :: m@Int -> n@Int -> Tensor2 m n Real
      add :: Tensor2 m n Real -> Tensor2 m n Real -> Tensor2 m n Real
      x = add (makeT 3 4) (makeT 3 5)
        |]
    , expectError
        "labeled dim mismatch: dot product length mismatch"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      dot :: Tensor1 n Real -> Tensor1 n Real -> Real
      x = dot (makeVec 3) (makeVec 5)
        |]
    , expectError
        "labeled dim mismatch through arithmetic: conv wrong kernel size"
        [r|
      module main (x)
      type Tensor2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      type Tensor3 (d1 :: Nat) (d2 :: Nat) (d3 :: Nat) a
      type Tensor1 (d :: Nat) a = [a]
      makeImg :: h@Int -> w@Int -> Tensor2 h w Real
      makeK :: k@Int -> fh@Int -> fw@Int -> Tensor3 k fh fw Real
      makeB :: k@Int -> Tensor1 k Real
      conv :: Tensor2 h w Real -> Tensor3 k fh fw Real -> Tensor1 k Real -> Tensor3 k (h - fh + 1) (w - fw + 1) Real
      x :: Tensor3 2 3 4 Real
      x = conv (makeImg 5 5) (makeK 2 3 3) (makeB 2)
        |]
    , expectError
        "annotated return type contradicts labeled resolution"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeVec :: n@Int -> Tensor1 n Real
      x :: Tensor1 99 Real
      x = makeVec 5
        |]

    -- === Interesting edge cases ===
    , assertRawType
        "no labels: plain nat vars remain generic"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      id_ :: Tensor1 n Real -> Tensor1 n Real
      a :: Tensor1 5 Real
      x = id_ a
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 5, VarU (TV "Real")])
    , assertRawType
        "label on non-first param position"
        [r|
      module main (x)
      type Tensor1 (d :: Nat) a = [a]
      makeFrom :: Real -> n@Int -> Tensor1 n Real
      x = makeFrom 1.0 10
        |]
        (AppU (VarU (TV "Tensor1")) [NatLitU 10, VarU (TV "Real")])
    ]

natKindPromotionTests :: TestTree
natKindPromotionTests =
  testGroup
    "nat kind promotion and cross-feature interaction"
    [
    -- ================================================================
    -- (:: Nat) annotation effects on type variable promotion
    -- ================================================================
    -- When a typedef has (d :: Nat), variables at that position MUST be
    -- promoted from VarU to NatVarU. Without this promotion, nat label
    -- resolution and nat constraint solving both silently fail.

      assertRawType
        "with (:: Nat): labels resolve to concrete dimensions"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      makeVec :: n@Int -> T1 n Real
      x = makeVec 5
        |]
        (AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")])

    , assertRawType
        "without (:: Nat): labels do NOT resolve (dim stays generic)"
        [r|
      module main (x)
      type T1 d a = [a]
      makeVec :: n@Int -> T1 n Real
      x = makeVec 5
        |]
        -- Without :: Nat, n is VarU not NatVarU, so resolveNatLabels cannot
        -- find nat vars to solve. The type stays generic (existential).
        (AppU (VarU (TV "T1")) [ExistU (TV "a") ([], Open) ([], Open), VarU (TV "Real")])

    -- ================================================================
    -- Nat constraint solving requires (:: Nat) promotion
    -- ================================================================

    , expectError
        "with (:: Nat): dimension mismatch is caught (4 != 5)"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      dot :: T1 n Real -> T1 n Real -> Real
      a :: T1 4 Real
      b :: T1 5 Real
      x = dot a b
        |]

    -- Without (:: Nat), the params are still NatLitU from the literal syntax.
    -- The typechecker catches the mismatch through existential solving since
    -- the NatLit values 4 and 5 cannot unify. This is actually sound.
    , expectError
        "without (:: Nat): dimension mismatch still caught via existentials"
        [r|
      module main (x)
      type T1 d a = [a]
      dot :: T1 n Real -> T1 n Real -> Real
      a :: T1 4 Real
      b :: T1 5 Real
      x = dot a b
        |]

    -- ================================================================
    -- Multi-step composition: labels propagate through chains
    -- ================================================================

    , assertRawType
        "labeled dims propagate through 3-function chain"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 n Real
      f :: T1 n Real -> T1 n Real
      g :: T1 n Real -> T1 n Real
      x = g (f (makeVec 9))
      makeVec :: n@Int -> T1 n Real
      x = g (f (make 9))
        |]
        (AppU (VarU (TV "T1")) [NatLitU 9, VarU (TV "Real")])

    , assertRawType
        "labeled dims propagate through let chain"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 n Real
      f :: T1 n Real -> T1 n Real
      x = let a = make 4
          in let b = f a
          in f b
        |]
        (AppU (VarU (TV "T1")) [NatLitU 4, VarU (TV "Real")])

    -- ================================================================
    -- Nat arithmetic with labeled params
    -- ================================================================

    , assertRawType
        "labeled subtraction: h=10 w=3, h-w+1 = 8"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: h@Int -> w@Int -> T1 (h - w + 1) Real
      x = make 10 3
        |]
        (AppU (VarU (TV "T1")) [NatLitU 8, VarU (TV "Real")])

    , assertRawType
        "labeled multiplication: m=3 n=4, m*n = 12"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: m@Int -> n@Int -> T1 (m * n) Real
      x = make 3 4
        |]
        (AppU (VarU (TV "T1")) [NatLitU 12, VarU (TV "Real")])

    , assertRawType
        "labeled division: n=12 d=4, n/d = 3"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: n@Int -> d@Int -> T1 (n / d) Real
      x = make 12 4
        |]
        (AppU (VarU (TV "T1")) [NatLitU 3, VarU (TV "Real")])

    , assertRawType
        "compound arithmetic: a=6 b=2 c=1, (a*b)-c = 11"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: a@Int -> b@Int -> c@Int -> T1 (a * b - c) Real
      x = make 6 2 1
        |]
        (AppU (VarU (TV "T1")) [NatLitU 11, VarU (TV "Real")])

    -- ================================================================
    -- Negative: arithmetic mismatches with labels
    -- ================================================================

    , expectError
        "labeled arithmetic mismatch: 10-3+1=8 but annotated as 7"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: h@Int -> w@Int -> T1 (h - w + 1) Real
      x :: T1 7 Real
      x = make 10 3
        |]

    , expectError
        "labeled multiplication mismatch: 3*4=12 but used where 11 expected"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: m@Int -> n@Int -> T1 (m * n) Real
      consume :: T1 11 Real -> Int
      x = consume (make 3 4)
        |]

    -- ================================================================
    -- Cross-feature: nat dims with optional types
    -- ================================================================

    , assertRawType
        "optional tensor: ?(T1 n Real) with labeled dim"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      tryMake :: n@Int -> ?(T1 n Real)
      x = tryMake 5
        |]
        (OptionalU (AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")]))

    -- ================================================================
    -- Cross-feature: nat dims with effect types
    -- ================================================================

    , assertRawType
        "effectful tensor: <IO> T1 n Real with labeled dim"
        [r|
      module main (x)
      effect IO
      type T1 (d :: Nat) a = [a]
      ioMake :: n@Int -> <IO> T1 n Real
      x = ioMake 5
        |]
        (EffectU (EffectSet (Set.singleton "IO"))
          (AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")]))

    -- ================================================================
    -- Nat dims nested inside other type constructors
    -- ================================================================

    , assertRawType
        "list of nat-parameterized type: [T1 n Real]"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: n@Int -> [T1 n Real]
      x = make 5
        |]
        (AppU (VarU (TV "List")) [AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")]])

    , assertRawType
        "tuple of nat-parameterized types"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: m@Int -> n@Int -> (T1 m Real, T1 n Real)
      x = make 3 7
        |]
        (AppU (VarU (TV "Tuple2"))
          [ AppU (VarU (TV "T1")) [NatLitU 3, VarU (TV "Real")]
          , AppU (VarU (TV "T1")) [NatLitU 7, VarU (TV "Real")]
          ])

    -- ================================================================
    -- Multiple nat-parameterized typedefs interacting
    -- ================================================================

    , assertRawType
        "conversion between two nat-parameterized types"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      type T2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      flatten :: T2 m n Real -> T1 (m * n) Real
      make :: m@Int -> n@Int -> T2 m n Real
      x = flatten (make 3 4)
        |]
        (AppU (VarU (TV "T1")) [NatLitU 12, VarU (TV "Real")])

    , expectError
        "cross-type dim mismatch: flatten 3x4=12 but consume expects 11"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      type T2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
      flatten :: T2 m n Real -> T1 (m * n) Real
      make :: m@Int -> n@Int -> T2 m n Real
      consume :: T1 11 Real -> Int
      x = consume (flatten (make 3 4))
        |]

    -- ================================================================
    -- Partial nat annotation: some params Nat, some not
    -- ================================================================

    , assertRawType
        "mixed params: first is Nat, second is Type"
        [r|
      module main (x)
      type Sized (n :: Nat) a = [a]
      make :: n@Int -> Sized n Int
      x = make 10
        |]
        (AppU (VarU (TV "Sized")) [NatLitU 10, VarU (TV "Int")])

    -- ================================================================
    -- Negative: bad label syntax and semantics
    -- ================================================================

    -- Label n resolves nothing in return type (m is a different var), so
    -- m stays generic. This is valid — the label just has no effect.
    -- m is promoted to NatVarU (it's in a :: Nat position) but stays unresolved.
    , assertRawType
        "label on param that doesn't appear in return type: dim stays generic"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 m Real
      x = make 5
        |]
        (AppU (VarU (TV "T1")) [NatVarU (TV "a"), VarU (TV "Real")])

    -- ================================================================
    -- Edge: nat literal 0 in arithmetic
    -- ================================================================

    , assertRawType
        "nat literal 0 in subtraction: n-0 = n"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 (n - 0) Real
      x = make 7
        |]
        (AppU (VarU (TV "T1")) [NatLitU 7, VarU (TV "Real")])

    , assertRawType
        "nat literal 1 in multiplication: n*1 = n"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 (n * 1) Real
      x = make 7
        |]
        (AppU (VarU (TV "T1")) [NatLitU 7, VarU (TV "Real")])

    , assertRawType
        "nat literal 0 in multiplication: n*0 = 0"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 (n * 0) Real
      x = make 7
        |]
        (AppU (VarU (TV "T1")) [NatLitU 0, VarU (TV "Real")])

    -- ================================================================
    -- Regression: multiple exports with nat dims
    -- ================================================================

    , testCase "multiple exports all resolve independently" $ do
        result <- runFrontRaw [r|
      module main (x, y)
      type T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 n Real
      x = make 3
      y = make 7
          |]
        case result of
          Right [xExpr, yExpr] -> do
            let xt = MTI.cleanTypeName . renameExistentials . gtypeof $ xExpr
                yt = MTI.cleanTypeName . renameExistentials . gtypeof $ yExpr
                expected3 = MTI.cleanTypeName $ AppU (VarU (TV "T1")) [NatLitU 3, VarU (TV "Real")]
                expected7 = MTI.cleanTypeName $ AppU (VarU (TV "T1")) [NatLitU 7, VarU (TV "Real")]
            assertEqual "first export" (closeExistentials expected3) (closeExistentials xt)
            assertEqual "second export" (closeExistentials expected7) (closeExistentials yt)
          Right other -> assertFailure $ "Expected 2 exports, got " ++ show (length other)
          Left e -> assertFailure $ "Unexpected error: " ++ show e

    -- ================================================================
    -- Regression: large nat literals
    -- ================================================================

    , assertRawType
        "large nat literal: 1000000"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      make :: n@Int -> T1 n Real
      x = make 1000000
        |]
        (AppU (VarU (TV "T1")) [NatLitU 1000000, VarU (TV "Real")])

    -- ================================================================
    -- Negative: nat constraint contradictions caught through labels
    -- ================================================================

    , expectError
        "labeled: same label used for different values"
        [r|
      module main (x)
      type T1 (d :: Nat) a = [a]
      combine :: T1 n Real -> T1 n Real -> T1 n Real
      make :: n@Int -> T1 n Real
      x = combine (make 3) (make 5)
        |]

    -- ================================================================
    -- Forward-declared type (no RHS) with :: Nat
    -- ================================================================

    , assertRawType
        "forward-declared type with :: Nat resolves labels"
        [r|
      module main (x)
      type Opaque (d :: Nat) a
      make :: n@Int -> Opaque n Real
      x = make 42
        |]
        (AppU (VarU (TV "Opaque")) [NatLitU 42, VarU (TV "Real")])

    , expectError
        "forward-declared type: dim mismatch caught"
        [r|
      module main (x)
      type Opaque (d :: Nat) a
      make :: n@Int -> Opaque n Real
      consume :: Opaque 10 Real -> Int
      x = consume (make 5)
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
          int

      , assertGeneralType
          "do-block with multi-binding let (omit repeated let)"
          [r|
        module main (x)
        x = do
            let a = 1
                b = 2
            b
          |]
          int

      , assertGeneralType
          "do-block with separate let statements"
          [r|
        module main (x)
        x = do
            let a = 1
            let b = 2
            b
          |]
          int

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
          int

      , assertGeneralType
          "do with explicit braces, let, and semicolons"
          "module main (x)\nx = do { let a = 1; a }"
          int

      , assertGeneralType
          "do with explicit braces, multiple lets"
          "module main (x)\nx = do { let a = 1; let b = 2; b }"
          int
      ]

-- ============================================================
-- Irrefutable-pattern binding syntax
-- ============================================================

irrefutablePatternTests :: TestTree
irrefutablePatternTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "Irrefutable pattern bindings"
      [ -- === Tuple destructuring in let ===
        assertGeneralType
          "let with tuple pattern picks first component"
          "module main (x)\nx = let (a, b) = (1, True) in a"
          int

      , assertGeneralType
          "let with tuple pattern picks second component"
          "module main (x)\nx = let (a, b) = (1, True) in b"
          bool

      , assertGeneralType
          "let with wildcard in tuple ignores component"
          "module main (x)\nx = let (_, b) = (1, True) in b"
          bool

      , assertGeneralType
          "let with nested tuple pattern"
          "module main (x)\nx = let (a, (b, c)) = (1, (True, 3)) in b"
          bool

      -- === Tuple destructuring in lambda ===
      -- These use signatures because inferring through a lambda-projected
      -- irrefutable pattern still hits the existential-leak bug tracked as
      -- a separate TODO. The desugar produces the correct AST; the tests
      -- confirm end-to-end synth+check works when the outer type is fixed.

      , assertGeneralType
          "lambda with tuple pattern picks first"
          "module main (x)\nx :: Int\nx = (\\ (a, b) -> a) (7, True)"
          int

      , assertGeneralType
          "lambda with wildcard in tuple pattern"
          "module main (x)\nx :: Bool\nx = (\\ (_, b) -> b) (7, True)"
          bool

      -- === Tuple destructuring in function args ===

      , assertGeneralType
          "function-arg tuple pattern picks first"
          [r|
        fst2 :: (Int, Bool) -> Int
        fst2 (a, b) = a
        fst2 (42, True)
          |]
          int

      , assertGeneralType
          "function-arg tuple pattern picks second"
          [r|
        snd2 :: (Int, Bool) -> Bool
        snd2 (a, b) = b
        snd2 (42, True)
          |]
          bool

      , assertGeneralType
          "function-arg wildcard in tuple"
          [r|
        second :: (Int, Bool, Int) -> Bool
        second (_, b, _) = b
        second (1, True, 3)
          |]
          bool

      -- === Record destructuring (field-polymorphic on structural records) ===
      -- NOTE: `let {...}` alone is ambiguous with explicit let-blocks, so
      -- record patterns on a let LHS must be parenthesized.

      , assertGeneralType
          "let with record pattern extracts named field"
          "module main (x)\nx = let ({a=p, b=q}) = {a=1, b=True} in p"
          int

      , assertGeneralType
          "record pattern extracts second field"
          "module main (x)\nx = let ({a=p, b=q}) = {a=1, b=True} in q"
          bool

      , assertGeneralType
          "record pattern field order does not matter"
          "module main (x)\nx = let ({b=q, a=p}) = {a=1, b=True} in p"
          int

      -- === As-patterns ===

      , assertGeneralType
          "as-pattern binds label to whole receiver"
          [r|
        first p@(a, b) = p
        first (1, True)
          |]
          (tuple [int, bool])

      , assertGeneralType
          "as-pattern binds label and inner components together"
          [r|
        firstElem :: (Int, Bool) -> Int
        firstElem p@(a, b) = a
        firstElem (1, True)
          |]
          int

      -- === Do-block pattern binds ===

      , expectPass
          "do-block tuple bind unpacks and returns first component"
          [r|
        module main (x)
        effect IO
        f :: <IO> (Int, Bool)
        x = do
            (a, b) <- f
            a
          |]

      , expectPass
          "do-block wildcard bind runs effect and returns other"
          [r|
        module main (x)
        effect IO
        f :: <IO> (Int, Bool)
        x = do
            (_, b) <- f
            b
          |]

      -- === Nested / complex patterns ===

      , assertGeneralType
          "function-arg mixing tuple, record, wildcard"
          [r|
        record R = R {a :: Bool, b :: Real}
        pick :: (Int, R) -> Real
        pick (n, {a=_, b=q}) = q
        pick (1, {a=True, b=3.5})
          |]
          real

      , assertGeneralType
          "wildcard in let followed by identity use"
          "module main (x)\nx = let _ = 42 in True"
          bool

      -- === Error cases ===

      , expectError
          "duplicate binder in tuple pattern is rejected"
          [r|
        module main (x)
        x = let (a, a) = (1, 2) in a
          |]

      , expectError
          "duplicate binder across as-pattern is rejected"
          [r|
        module main (x)
        x = let a@(a, b) = (1, 2) in b
          |]
      ]

-- | Tests for typeclass instance resolution across newtype boundaries.
-- @newtype Deque a = List a@ and @newtype Array a = List a@ are nominally
-- distinct from List and from each other, but share its wire format. Each
-- needs its own typeclass instances; an instance on List does not apply
-- to Deque, and a Deque value cannot be passed where an Array is expected.
aliasConstructorTests :: TestTree
aliasConstructorTests =
  localOption (mkTimeout 2000000) $ -- 2 second timeout
    testGroup
      "Typeclass resolution with newtype families"
      [
        -- === POSITIVE: newtypes used as type constructors with their own instances ===

        -- Two newtypes of the same wire-parent, each with its own Foldable
        -- instance. concat needs (Foldable f, Monoid a); the call site fixes
        -- f to a specific newtype which routes to the matching instance.
        assertGeneralType
          "fold over newtype: Deque and List each carry their own Foldable instance"
          [r|
        module main (f)
        class Semigroup a where
          append :: a -> a -> a
        class Semigroup a => Monoid a where
          mempty :: a
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        newtype Deque a = List a
        instance Semigroup (List a)
        instance Semigroup (Deque a)
        instance Monoid (List a)
        instance Monoid (Deque a)
        instance Foldable List
        instance Foldable Deque
        concat :: (Foldable f, Monoid a) => f (f a) -> f a
        concat = fold append mempty
        f :: [[Int]] -> [Int]
        f = concat
          |]
          (fun [lst (lst int), lst int])

      , -- Three newtypes sharing a wire-parent, each with its own Functor instance
        assertGeneralType
          "three newtypes (List, Deque, Array) each carry their own Functor instance"
          [r|
        module main (f)
        class Functor f where
          fmap :: (a -> b) -> f a -> f b
        newtype Deque a = List a
        newtype Array a = List a
        instance Functor List
        instance Functor Deque
        instance Functor Array
        f :: [Int] -> [Int]
        f = fmap (\x -> x)
          |]
          (fun [lst int, lst int])

      , -- Chained newtype wire-parents: MyList -> Deque -> List
        assertGeneralType
          "chained newtypes: MyList wire-parent Deque, Deque wire-parent List"
          [r|
        module main (f)
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        newtype Deque a = List a
        newtype MyList a = Deque a
        instance Foldable List
        instance Foldable Deque
        instance Foldable MyList
        f :: [Int] -> Int
        f = fold (\a x -> a) 0
          |]
          (fun [lst int, int])

      , -- Multi-parameter newtype: same arity as wire-parent
        assertGeneralType
          "two-parameter newtype with its own instance"
          [r|
        module main (f)
        class MyClass f where
          myMethod :: f a b -> f a b
        newtype MyMap a b = Map a b
        instance MyClass Map
        instance MyClass MyMap
        f :: Map Int Str -> Map Int Str
        f = myMethod
          |]
          (fun [arr "Map" [int, str], arr "Map" [int, str]])

      , -- === ANNOTATION-DRIVEN NEWTYPE SELECTION ===
        -- A return-type annotation propagates the newtype through the
        -- expression: both append arguments must be Deque Int because
        -- the signature says so. Demonstrates that newtype identity
        -- flows through bidirectional checking without alias reduction.

        assertRawType
          "newtype annotation on right propagates to result"
          [r|
        module main (bar)
        class Semigroup a where
          append :: a -> a -> a
        newtype Deque a = List a
        instance Semigroup (List a)
        instance Semigroup (Deque a)
        bar :: Deque Int
        bar = append [1,2,3] ([4,5,6] :: Deque Int)
          |]
          (arr "Deque" [int])

      , assertRawType
          "newtype annotation on left propagates to result"
          [r|
        module main (baz)
        class Semigroup a where
          append :: a -> a -> a
        newtype Deque a = List a
        instance Semigroup (List a)
        instance Semigroup (Deque a)
        baz :: Deque Int
        baz = append ([4,5,6] :: Deque Int) [1,2,3]
          |]
          (arr "Deque" [int])

      , -- Annotation across a chain: MyList -> Deque -> List wire-parents
        assertRawType
          "newtype annotation propagates through chained wire-parents"
          [r|
        module main (bar)
        class Semigroup a where
          append :: a -> a -> a
        newtype Deque a = List a
        newtype MyList a = Deque a
        instance Semigroup (List a)
        instance Semigroup (Deque a)
        instance Semigroup (MyList a)
        bar :: MyList Int
        bar = append [1,2,3] ([4,5,6] :: MyList Int)
          |]
          (arr "MyList" [int])

      , -- concat used over a List value with full Semigroup/Monoid/Foldable
        -- instances on the List newtype. Demonstrates polymorphic typeclass
        -- dispatch when each newtype carries its own instance set.
        assertGeneralType
          "concat typechecks over List with full instance set"
          [r|
        module main (f)
        class Semigroup a where
          append :: a -> a -> a
        class Semigroup a => Monoid a where
          mempty :: a
        class Foldable f where
          fold :: (b -> a -> b) -> b -> f a -> b
        newtype Deque a = List a
        newtype Array a = List a
        instance Semigroup (List a)
        instance Semigroup (Deque a)
        instance Semigroup (Array a)
        instance Monoid (List a)
        instance Monoid (Deque a)
        instance Monoid (Array a)
        instance Foldable List
        instance Foldable Deque
        instance Foldable Array
        concat :: (Foldable f, Monoid a) => f (f a) -> f a
        concat = fold append mempty
        f :: [[Int]] -> [Int]
        f = concat
          |]
          (fun [lst (lst int), lst int])

      , -- === NEGATIVE: NEWTYPE NOMINAL DISTINCTION ===

        -- Newtypes of the same wire-parent are nominally distinct.
        -- A binary operator with shared type variable cannot mix them.
        exprTestBad
          "newtypes Array and Deque are nominally distinct"
          [r|
        module main (bad)
        class Semigroup a where
          append :: a -> a -> a
        newtype Deque a = List a
        newtype Array a = List a
        instance Semigroup (Deque a)
        instance Semigroup (Array a)
        bad = append ([1,2,3] :: Array Int) ([4,5,6] :: Deque Int)
          |]

      , -- A function expecting one newtype must reject a value of a
        -- different newtype, even when both share a wire-parent.
        exprTestBad
          "function expecting Deque rejects Array argument"
          [r|
        module main (bad)
        newtype Deque a = List a
        newtype Array a = List a
        f :: Deque Int -> Int
        bad :: Int
        bad = f ([1,2,3] :: Array Int)
          |]

      , -- Non-alias types remain incompatible
        exprTestBad
          "non-alias types Int vs Str remain incompatible"
          [r|
        module main (f)
        f :: Int
        f = ("hello" :: Str)
          |]
      ]

-- | Tests for the @newtype@ declaration's nominal-distinct, wire-equivalent
-- semantics. @newtype Foo = Bar@ inherits only the wire format from Bar;
-- typeclass instances, per-language overrides, and type identity are Foo's
-- own. Covers basic acceptance, instance non-inheritance, frontend
-- invariants (Invariant 1: transparent @type@ alias cannot carry a
-- per-language form; Invariant 2: cycle in the newtype wire-parent graph
-- is rejected), and composition with @?@/@<E>@ wrappers.
newtypeTests :: TestTree
newtypeTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "newtype declaration"
      [ -- A bare newtype with no inherited instances should parse and
        -- typecheck. The wire-parent identity (List) is preserved in the
        -- general scope; the newtype identity (MyList) is what the
        -- typechecker sees at the boundary.
        assertGeneralType
          "bare newtype parses and is distinct from wire-parent"
          [r|
        module main (f)
        newtype MyList a = List a
        f :: MyList Int -> MyList Int
        f x = x
          |]
          (fun [arr "MyList" [int], arr "MyList" [int]])

      , -- A newtype with an explicit typeclass instance is usable through
        -- that instance. The wire-parent's instance is irrelevant here:
        -- only the newtype's own instance routes to the dispatch.
        assertGeneralType
          "newtype with explicit Functor instance routes through it"
          [r|
        module main (f)
        class Functor f where
          fmap :: (a -> b) -> f a -> f b
        newtype MyList a = List a
        instance Functor MyList
        f :: MyList Int -> MyList Int
        f = fmap (\x -> x)
          |]
          (fun [arr "MyList" [int], arr "MyList" [int]])

      , -- Without an explicit instance on the newtype, a typeclass method
        -- declared on the wire-parent is not visible. Instance resolution
        -- stops at the newtype boundary.
        exprTestBad
          "newtype does NOT inherit Functor instance from wire-parent"
          [r|
        module main (f)
        class Functor f where
          fmap :: (a -> b) -> f a -> f b
        newtype MyList a = List a
        instance Functor List
        f :: MyList Int -> MyList Int
        f = fmap (\x -> x)
          |]

      , -- Invariant 1: a transparent @type@ alias cannot carry a per-language
        -- form. The right alternative is to declare @Foo@ as a @newtype@.
        exprTestBad
          "type alias with per-language form is rejected"
          [r|
        module main (f)
        type Foo = Int
        type Py => Foo = "my_foo"
        f :: Foo -> Foo
        f x = x
          |]

      , -- Invariant 2: a cycle in the newtype wire-parent graph is rejected.
        exprTestBad
          "cyclic newtype chain is rejected"
          [r|
        module main (f)
        newtype A = B
        newtype B = A
        f :: A -> A
        f x = x
          |]

      , -- Optional composes structurally with newtype: ?(MyList Int) is
        -- a distinct nominal type whose wire format is Optional (List Int).
        assertGeneralType
          "newtype composes with Optional wrapper"
          [r|
        module main (f)
        newtype MyList a = List a
        f :: ?(MyList Int) -> ?(MyList Int)
        f x = x
          |]
          (fun [OptionalU (arr "MyList" [int]), OptionalU (arr "MyList" [int])])

      , -- Effect wrappers compose with newtype the same way.
        assertGeneralType
          "newtype composes with effect wrapper"
          [r|
        module main (f)
        effect IO
        newtype MyList a = List a
        f :: MyList Int -> <IO> MyList Int
        f x = x
          |]
          (fun [arr "MyList" [int], ioEff (arr "MyList" [int])])

      , -- A newtype with its own per-language form. The Cpp form is
        -- registered alongside the general declaration; the typechecker
        -- treats MyInt as nominally distinct from Int even though both
        -- compile to @int@ in C++ here. (No instance inheritance.)
        assertGeneralType
          "newtype with explicit per-language form"
          [r|
        module main (f)
        newtype MyInt = Int
        type Cpp => MyInt = "int"
        f :: MyInt -> MyInt
        f x = x
          |]
          (fun [var "MyInt", var "MyInt"])

      , -- A newtype whose wire-parent is another newtype: instance and
        -- per-language lookup walk the wire-parent chain in code-gen but
        -- treat each level as a distinct nominal type at the type level.
        assertGeneralType
          "chained newtypes: distinct identities, shared wire format"
          [r|
        module main (f)
        newtype Inner = Int
        newtype Outer = Inner
        f :: Outer -> Outer
        f x = x
          |]
          (fun [var "Outer", var "Outer"])

      , -- A vacuous newtype body (Foo = Foo) is rejected by the same
        -- self-reference check that rejects vacuous type aliases.
        exprTestBad
          "vacuous newtype body is rejected"
          [r|
        module main (f)
        newtype Foo = Foo
        f :: Foo -> Foo
        f x = x
          |]
      ]

-- | Tests for value-level literals being accepted at newtype-typed
-- positions. The typechecker walks the newtype's wire-parent chain via
-- @wireParentRoot@: if the chain reaches the literal's natural primitive
-- (e.g. @newtype Foo = Int@ for an integer literal), the literal is
-- accepted at the newtype's identity. Codegen handles per-language
-- conversion via @*Like@ instances when the newtype's effective
-- per-language form diverges from the wire-parent's; the typechecker
-- itself doesn't consult @*Like@ instances.
literalDispatchTests :: TestTree
literalDispatchTests =
  localOption (mkTimeout 1000000) $
    testGroup
      "Literal acceptance at newtype types"
      [ -- An integer literal inhabits a newtype whose wire-parent
        -- chain reaches @Int@.
        assertGeneralType
          "integer literal at newtype Foo (Foo's wire-parent is Int)"
          [r|
        module main (x)
        newtype Foo = Int
        x :: Foo
        x = 5
          |]
          (var "Foo")

      , -- A string literal at a newtype whose wire-parent is Str.
        assertGeneralType
          "string literal at newtype Name (Name's wire-parent is Str)"
          [r|
        module main (x)
        newtype Name = Str
        x :: Name
        x = "Alice"
          |]
          (var "Name")

      , -- A boolean literal at a newtype whose wire-parent is Bool.
        assertGeneralType
          "bool literal at newtype Flag (Flag's wire-parent is Bool)"
          [r|
        module main (x)
        newtype Flag = Bool
        x :: Flag
        x = True
          |]
          (var "Flag")

      , -- A real literal at a newtype whose wire-parent is Real.
        assertGeneralType
          "real literal at newtype Speed (Speed's wire-parent is Real)"
          [r|
        module main (x)
        newtype Speed = Real
        x :: Speed
        x = 3.14
          |]
          (var "Speed")

      , -- A list literal at a Nat-parameterised newtype container whose
        -- wire-parent is List. Length-checks against the Nat dimension.
        assertGeneralType
          "list literal at newtype Vec n a (wire-parent is List a)"
          [r|
        module main (x)
        newtype Vec (n :: Nat) a = List a
        x :: Vec 3 Int
        x = [1, 2, 3]
          |]
          (arr "Vec" [NatLitU 3, var "Int"])

      , -- A literal whose natural primitive doesn't match the newtype's
        -- wire-parent chain is rejected. Foo here has wire-parent Str,
        -- so an integer literal cannot inhabit it.
        exprTestBad
          "integer literal rejected when newtype's wire-parent is not Int"
          [r|
        module main (x)
        newtype Foo = Str
        x :: Foo
        x = 5
          |]

      , -- Length mismatch against a Nat-parameterised newtype: the
        -- structural shape check accepts the list, then the existing
        -- nat-dim check rejects the wrong length.
        exprTestBad
          "list literal length mismatch at Vec 3 Int"
          [r|
        module main (x)
        newtype Vec (n :: Nat) a = List a
        x :: Vec 3 Int
        x = [1, 2]
          |]

      , -- Bare-VarT newtype-list: an unparameterised newtype that aliases
        -- a fully-applied list (e.g. @newtype Bytes = List U8@).
        -- A list literal at @Bytes@ is accepted because the wire-parent
        -- chain reaches a list shape; the children are typed at the
        -- wire-parent's element type.
        assertGeneralType
          "list literal at bare-VarT newtype Bytes (Bytes = List U8)"
          [r|
        module main (x)
        newtype Bytes = List U8
        x :: Bytes
        x = [1, 2, 3]
          |]
          (var "Bytes")

      , -- The same shape applied to a newtype whose body is a list of a
        -- different primitive. Confirms the element type comes from the
        -- wire-parent body, not from the user-facing type's args (which
        -- are empty here).
        assertGeneralType
          "list literal at bare-VarT newtype Reals (Reals = List Real)"
          [r|
        module main (x)
        newtype Reals = List Real
        x :: Reals
        x = [1.0, 2.0, 3.0]
          |]
          (var "Reals")

      , -- Chained newtype: a list literal at an outer newtype whose
        -- wire-parent walks through another newtype to reach List. The
        -- iterated 'wireParentRoot' must follow the full chain.
        assertGeneralType
          "list literal at chained newtype-of-newtype-of-List"
          [r|
        module main (x)
        newtype Inner = List Int
        newtype Outer = Inner
        x :: Outer
        x = [1, 2, 3]
          |]
          (var "Outer")

      , -- Bracket-index pattern on a Nat-parameterized container returns
        -- the element type (Vec n a -> a). The test inlines a stub
        -- __to_index__ since the desugarer wraps user-supplied bounds.
        assertGeneralType
          "bracket index on Vec n a returns a"
          [r|
        module main (f)
        class IndexLike i where
          __to_index__ :: i -> I64
        instance IndexLike Int where
          source Py ("identity" as __to_index__)
        newtype Vec (n :: Nat) a = List a
        f :: Vec 5 Int -> Int
        f x = .[0] x
          |]
          (FunU [arr "Vec" [NatLitU 5, var "Int"]] (var "Int"))

      , -- Bracket slice on a List preserves the shape.
        assertGeneralType
          "bracket slice on List a returns List a"
          [r|
        module main (f)
        class IndexLike i where
          __to_index__ :: i -> I64
        instance IndexLike Int where
          source Py ("identity" as __to_index__)
        f :: [Int] -> [Int]
        f x = .[1:5] x
          |]
          (FunU [arr "List" [var "Int"]] (arr "List" [var "Int"]))
      ]

-- Recursive record types. Self-recursive references through @[_]@ or @?_@
-- are legal (the data is heap-indirected and base-case-terminating).
-- Bare self-recursion (under tuple, function, named app, or directly) and
-- any mutual recursion remain illegal. Divergence is treated as failure
-- via the per-group timeout: if alias expansion or any later phase loops
-- on a recursive typedef, the offending test fires the timeout.
--
-- Syntax notes:
--   * Record types use @record T where { f :: t; ... }@; type-level
--     record literals @{f :: t}@ on the RHS of @type ... = ...@ are
--     rejected by the parser (CLAUDE.md: the @::@-form is reserved for
--     decls, not literals).
--   * Self-recursion on a non-record type alias (@type B3 = B3 -> Int@,
--     @type B2 = (B2, Int)@) parses and is exercised below for the
--     non-record bare-self cases.
recursiveRecordTests :: TestTree
recursiveRecordTests =
  localOption (mkTimeout 1000000) $ -- 1 second timeout; divergence appears as failure
    testGroup
      "Recursive record types"
      [
        -- POSITIVE: guarded self-recursion is accepted

        expectPass
          "self-recursion through list field"
          [r|
        module main (f)
        record T1 where
          a :: [T1]
        f :: T1 -> T1
        |]
      , expectPass
          "self-recursion through optional field"
          [r|
        module main (f)
        record T2 where
          a :: ?T2
        f :: T2 -> T2
        |]
      , expectPass
          "self-recursion through optional of list"
          [r|
        module main (f)
        record T3 where
          a :: ?[T3]
        f :: T3 -> T3
        |]
      , expectPass
          "self-recursion through list of optional"
          [r|
        module main (f)
        record T4 where
          a :: [?T4]
        f :: T4 -> T4
        |]
      , expectPass
          "self-recursion through nested lists"
          [r|
        module main (f)
        record T5 where
          a :: [[T5]]
        f :: T5 -> T5
        |]
      , expectPass
          "self-recursion through optional of nested lists"
          [r|
        module main (f)
        record T6 where
          a :: ?[[T6]]
        f :: T6 -> T6
        |]
      , expectPass
          "parameterized self-recursion"
          [r|
        module main (f)
        record T7 a where
          value :: a
          children :: [T7 a]
        f :: T7 Int -> T7 Int
        |]

        -- NEGATIVE: bare self-recursion is rejected
      , expectError
          "bare self under record field is rejected"
          [r|
        module main (f)
        record B1 where
          a :: B1
        f :: B1 -> B1
        |]
      , expectError
          "bare self under tuple alias is rejected"
          [r|
        module main (f)
        type B2 = (B2, Int)
        f :: B2 -> B2
        |]
      , expectError
          "bare self under function argument is rejected"
          [r|
        module main (f)
        type B3 = B3 -> Int
        f :: B3 -> Int
        |]
      , expectError
          "bare self under function return is rejected"
          [r|
        module main (f)
        type B4 = Int -> B4
        f :: B4
        |]
      , expectError
          "bare self under non-list non-optional app is rejected"
          [r|
        module main (f)
        type Pair a b = (a, b)
        record B5 where
          a :: Pair B5 Int
        f :: B5 -> B5
        |]
      , expectError
          "mixed good-and-bad fields: rejection wins"
          [r|
        module main (f)
        record B6 where
          ok :: [B6]
          bad :: B6
        f :: B6 -> B6
        |]

        -- NEGATIVE: mutual recursion is rejected (out of scope this pass)
      , expectError
          "mutual recursion 2-cycle, unguarded, is rejected"
          [r|
        module main (f)
        record M1 where
          x :: M2
        record M2 where
          y :: M1
        f :: M1 -> M1
        |]
      , expectError
          "mutual recursion 2-cycle, both sides guarded, is rejected"
          [r|
        module main (f)
        record N1 where
          x :: [N2]
        record N2 where
          y :: [N1]
        f :: N1 -> N1
        |]
      , expectError
          "mutual recursion 3-cycle is rejected"
          [r|
        module main (f)
        record C1 where
          x :: C2
        record C2 where
          y :: C3
        record C3 where
          z :: C1
        f :: C1 -> C1
        |]
      ]

-- | Bidirectional checking for function application against structurally
-- recursive types. The new `checkE (AppS f xs) t` rule propagates the
-- expected type into the function's return position BEFORE checking the
-- args. Without it, a literal at a recursive alias gets synthesised to
-- a finite tuple shape and subtype fails when it tries to roll the
-- finite type back into the recursive alias.
--
-- The cases here are typecheck-only (no codegen, no pool build) so the
-- whole group runs in milliseconds. The deep tests below pin the
-- linear-complexity claim from the plan.
bidirectionalAppCheckTests :: TestTree
bidirectionalAppCheckTests =
  localOption (mkTimeout 2000000) $ -- 2 second timeout
    testGroup
      "Bidirectional App-check against recursive types"
      [ -- Bug repro: monomorphic id-shaped call at a recursive alias.
        -- Without the new rule, the literal synthesises to
        -- `(Str, (Str, ?ex))` and subtype against `Pair Str` fails at
        -- the inner `(Str, ?ex) <: ?(Pair Str)`.
        expectPass
          "monomorphic id at recursive Pair alias"
          [r|
        module main (foo)
        type Pair a = (a, ?(Pair a))
        idPair :: Pair Str -> Pair Str
        foo :: Pair Str
        foo = idPair ("a", ("b", Null))
        |]
      , -- Polymorphic id: forces stripForallU + propagation through the
        -- shared `a` existential.
        expectPass
          "polymorphic id at recursive Pair alias"
          [r|
        module main (foo)
        type Pair a = (a, ?(Pair a))
        id :: a -> a
        foo :: Pair Str
        foo = id ("a", ("b", Null))
        |]
      , -- Multi-arg application: exercises the
        -- `length xs == length paramTypes` arm beyond unary.
        expectPass
          "multi-arg polymorphic function at recursive alias"
          [r|
        module main (foo)
        type Pair a = (a, ?(Pair a))
        pick :: Bool -> a -> a -> a
        foo :: Pair Str
        foo = pick True ("a", Null) ("b", Null)
        |]
      , -- Nested AppS: argument is itself a function application, so
        -- the new rule must recurse through checkG -> checkE -> AppS.
        expectPass
          "nested polymorphic id at recursive alias"
          [r|
        module main (foo)
        type Pair a = (a, ?(Pair a))
        id :: a -> a
        foo :: Pair Str
        foo = id (id ("a", ("b", Null)))
        |]
      , -- Partial application: falls back cleanly via checkEFallback.
        -- The new rule's `_ -> checkEFallback` arm is exercised.
        expectPass
          "partial application falls through cleanly"
          [r|
        module main (bar)
        add :: Int -> Int -> Int
        bar :: Int -> Int
        bar = add 1
        |]
      , -- Genuine type error must still be rejected: the propagation
        -- step solves `a := Int`, then the argument check `"x" :: Int`
        -- fails inside checkG (same diagnostic surface as today).
        exprTestBad
          "wrong-typed argument still rejected"
          [r|
        module main (foo)
        id :: a -> a
        foo :: Int
        foo = id "hello"
        |]
      , -- Function-as-argument: confirms the rule doesn't over-eagerly
        -- fire on FunU-returning calls where the existing logic was
        -- already correct.
        expectPass
          "function-as-argument keeps working"
          [r|
        module main (foo)
        apply :: (a -> b) -> a -> b
        inc :: Int -> Int
        foo :: Int
        foo = apply inc 3
        |]
      , -- Linear-complexity probe: depth-20 recursive literal under a
        -- monomorphic id at a recursive alias. With the linear
        -- algorithm this is microseconds; with an O(N^2) regression
        -- depth-20 is 400x slower, still well inside 2 s but visible
        -- if cubic or exponential.
        expectPass
          "deep-20 recursive literal, monomorphic id"
          (deepLiteralProgram False 20)
      , -- Depth-20 literal under polymorphic id: forces stripForallU +
        -- propagation at the recursive type.
        expectPass
          "deep-20 recursive literal, polymorphic id"
          (deepLiteralProgram True 20)
      , -- Depth-20 chain of id applications around a small literal:
        -- exercises the application-nesting axis orthogonally to
        -- literal depth.
        expectPass
          "deep-20 chained id applications"
          (deepIdChainProgram 20)
      , -- Combined axes: depth-20 literal nested inside a depth-20 id
        -- chain. If complexity is O(M*N) this is 400 work units, still
        -- linear-ish in 2 s but a regression to O((M*N)^k) for k>=2
        -- would blow up here.
        expectPass
          "deep-20 literal under deep-20 id chain"
          (deepCombinedProgram 20)
      , -- Negative deep case: depth-20 literal at a wrong type. The
        -- failure path must also be linear (no pathological cost when
        -- the rule's propagation succeeds but the argument check
        -- rejects).
        exprTestBad
          "deep-20 recursive literal at wrong type still rejected"
          (deepWrongTypeProgram 20)
      , -- Regression: a polymorphic indexer (`at :: Int -> [a] -> a`)
        -- whose return is the bare element type, applied in a
        -- position that expects `?a`. The App-Check shortcut was
        -- pre-pinning the function's return existential to the FULL
        -- `?a`, which propagated through the polymorphic arg and
        -- forced `xs :: [?a]` against the actual `xs :: [a]`,
        -- producing an occurs check `a <: ?a`. The fix bails to
        -- checkEFallback when the expected type is OptionalU so the
        -- coercion fires at the application boundary.
        expectPass
          "indexer at ?a return position coerces at boundary"
          [r|
        module main (foo)
        listAt :: Int -> [a] -> a
        foo :: [a] -> ?a
        foo xs = listAt 0 xs
        |]
      , -- The full listToMaybe pattern from the maybe stdlib:
        -- explicit null-guard on one branch, polymorphic indexer on
        -- the other. The else-branch must coerce `a` to `?a` cleanly.
        expectPass
          "guarded safeHead pattern with polymorphic indexer"
          [r|
        module main (foo)
        listAt :: Int -> [a] -> a
        isEmpty :: [a] -> Bool
        foo :: [a] -> ?a
        foo xs
          ? isEmpty xs = Null
          : listAt 0 xs
        |]
      , -- Negative: applying a function whose return is NOT the
        -- expected `?a`'s inner type must still be rejected. `f ::
        -- a -> a` applied to `xs : [a]` returns `[a]`, which cannot
        -- coerce to `?a`. Confirms the OptionalU bypass doesn't
        -- accidentally permit wrong-shape coercions.
        exprTestBad
          "wrong-shape return at ?a position still rejected"
          [r|
        module main (foo)
        f :: a -> a
        foo :: [a] -> ?a
        foo xs = f xs
        |]
      ]

-- | Build a morloc source program with a depth-N recursive literal
-- assigned to `foo :: Pair Str` via `idPair` (mono) or `id` (poly).
-- The literal nests as ("s1", ("s2", ..., ("sN", Null) ... )).
deepLiteralProgram :: Bool -> Int -> MT.Text
deepLiteralProgram poly n = MT.pack $ unlines
  [ "module main (foo)"
  , "type Pair a = (a, ?(Pair a))"
  , if poly then "id :: a -> a" else "idPair :: Pair Str -> Pair Str"
  , "foo :: Pair Str"
  , "foo = " <> (if poly then "id " else "idPair ") <> deepLit n
  ]

-- | depth-N nested literal: ("s1", ("s2", ..., ("sN", Null) ... ))
deepLit :: Int -> String
deepLit n = go 1
  where
    go k
      | k > n     = "Null"
      | otherwise = "(\"s" <> show k <> "\", " <> go (k + 1) <> ")"

-- | depth-N chain of `id` applications: id (id (id ... ("a", Null)))
deepIdChainProgram :: Int -> MT.Text
deepIdChainProgram n = MT.pack $ unlines
  [ "module main (foo)"
  , "type Pair a = (a, ?(Pair a))"
  , "id :: a -> a"
  , "foo :: Pair Str"
  , "foo = " <> chain n <> "(\"s1\", (\"s2\", Null))" <> replicate n ')'
  ]
  where
    chain k = concat (replicate k "id (")

-- | Combined: depth-N id chain wrapped around depth-N literal.
deepCombinedProgram :: Int -> MT.Text
deepCombinedProgram n = MT.pack $ unlines
  [ "module main (foo)"
  , "type Pair a = (a, ?(Pair a))"
  , "id :: a -> a"
  , "foo :: Pair Str"
  , "foo = " <> chain n <> deepLit n <> replicate n ')'
  ]
  where
    chain k = concat (replicate k "id (")

-- | depth-N literal annotated at the wrong type (Int) so the
-- argument check must fail.
deepWrongTypeProgram :: Int -> MT.Text
deepWrongTypeProgram n = MT.pack $ unlines
  [ "module main (foo)"
  , "id :: a -> a"
  , "foo :: Int"
  , "foo = id " <> deepLit n
  ]

-- | Coverage for the class of nat/str/list-label propagation
-- patterns: information that flows from a literal argument OUT
-- through one or more carrier functions to satisfy an outer
-- expected type. The "labeled dims propagate through 3-function
-- chain" case in natKindPromotionTests was the canonical example;
-- these tests broaden the coverage along several axes:
--
--   * different chain depths (1, 2, 4) on the same nat-label axis
--   * monomorphic vs polymorphic carrier functions
--   * multi-label functions where multiple pinnings must propagate
--   * multi-arg functions where one arg pins a shared variable
--   * cross-existential pinning via a tuple's first element
--   * negative cases: a downstream constraint must reject a wrong
--     literal seen at any depth in the chain
--
-- Most cases here are handled by the OLD synth+subtype path -- the
-- new checkE AppS rule is gated to bare-existential return types,
-- which excludes the parameterized-carrier functions used in chains.
-- That gating is itself under test: a regression that broadened the
-- rule's trigger (e.g. dropping the isBareExistU guard) would let it
-- over-commit on the structured-return cases below and surface as
-- either a stranded NatVar or a Rec-key-set mismatch.
postArgPropagationTests :: TestTree
postArgPropagationTests =
  localOption (mkTimeout 2000000) $ -- 2 second timeout
    testGroup
      "App-check post-arg propagation of inner solutions"
      [ -- One level of indirection: monomorphic carrier with a single
        -- nat var. The pre-subtype defers (NatVar n_f vs NatVar n_g),
        -- the labelled `make` arg pins n_f := 5, and the post-subtype
        -- has to push n_f's value out to n_g.
        assertRawType
          "depth-1 nat label chain through monomorphic id_"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        id_ :: T1 n Real -> T1 n Real
        x = id_ (make 5)
          |]
          (AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")])
      , -- Polymorphic carrier (forall a. a -> a) instead of the
        -- nat-typed id_. Exercises the stripForallU + label-pinning
        -- combination through the new rule.
        assertRawType
          "depth-1 nat label chain through polymorphic id"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        id :: a -> a
        x = id (make 6)
          |]
          (AppU (VarU (TV "T1")) [NatLitU 6, VarU (TV "Real")])
      , -- Depth-2 chain: same direction as the 3-function chain, but
        -- one level shorter. The post-subtype has to chain through
        -- two carriers (the inner f and the outer g_ both leave the
        -- nat var unsolved by themselves).
        assertRawType
          "depth-2 nat label chain"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        f :: T1 n Real -> T1 n Real
        g :: T1 n Real -> T1 n Real
        x = g (f (make 7))
          |]
          (AppU (VarU (TV "T1")) [NatLitU 7, VarU (TV "Real")])
      , -- Depth-4 chain: confirms the post-subtype propagation works
        -- at arbitrary depth. If the propagation lost information at
        -- some level the result would be a stranded NatVar like the
        -- bug report.
        assertRawType
          "depth-4 nat label chain"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        f1 :: T1 n Real -> T1 n Real
        f2 :: T1 n Real -> T1 n Real
        f3 :: T1 n Real -> T1 n Real
        f4 :: T1 n Real -> T1 n Real
        x = f4 (f3 (f2 (f1 (make 11))))
          |]
          (AppU (VarU (TV "T1")) [NatLitU 11, VarU (TV "Real")])
      , -- Two labels in the same function, both must propagate
        -- through one level of indirection. Without the post-subtype
        -- one of the nat vars (typically the second) would stay
        -- stranded because the deferred constraint never re-fires.
        assertRawType
          "two-label function chained through id_"
          [r|
        module main (x)
        type T2 (d1 :: Nat) (d2 :: Nat) a = [[a]]
        make :: m@Int -> n@Int -> T2 m n Real
        id_ :: T2 m n Real -> T2 m n Real
        x = id_ (make 3 4)
          |]
          (AppU (VarU (TV "T2")) [NatLitU 3, NatLitU 4, VarU (TV "Real")])
      , -- Multi-arg outer function where the recursive position is one
        -- of several args. The other arg (a plain Real) must not
        -- interfere with the post-subtype's nat-var propagation.
        assertRawType
          "label propagates through second arg of multi-arg function"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        scale :: Real -> T1 n Real -> T1 n Real
        x = scale 2.0 (make 8)
          |]
          (AppU (VarU (TV "T1")) [NatLitU 8, VarU (TV "Real")])
      , -- Both args of a multi-arg function contribute the same label
        -- value. The pre-subtype against the expected type defers,
        -- but the first arg's labelled-nat pins one side; the second
        -- arg then has to be checked against the now-pinned type,
        -- and the post-subtype propagates the result back to the
        -- expected nat var.
        assertRawType
          "shared label resolved by both args of binary op"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        add :: T1 n Real -> T1 n Real -> T1 n Real
        x = add (make 4) (make 4)
          |]
          (AppU (VarU (TV "T1")) [NatLitU 4, VarU (TV "Real")])
      , -- Negative: depth-3 chain ending in a consumer that demands a
        -- specific dimension. The mismatch must be caught, not
        -- silently accepted by a stranded NatVar.
        exprTestBad
          "wrong-dim labelled arg at end of chain still rejected"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        f :: T1 n Real -> T1 n Real
        g :: T1 n Real -> T1 n Real
        consume :: T1 5 Real -> Int
        x = consume (g (f (make 7)))
          |]
      , -- Negative: shared label disagreement across two arg positions.
        -- Without correct propagation a stranded NatVar would let the
        -- mismatch slip through.
        exprTestBad
          "label disagreement across shared-var binary op rejected"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        add :: T1 n Real -> T1 n Real -> T1 n Real
        x = add (make 3) (make 5)
          |]
      , -- Nat arithmetic in the return type, propagated through a
        -- chain. `make 6 2` returns `T1 (6*2) Real = T1 12 Real`; the
        -- outer carriers must not lose the arithmetic result during
        -- propagation.
        assertRawType
          "labelled nat arithmetic propagates through chain"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: m@Int -> n@Int -> T1 (m * n) Real
        f :: T1 n Real -> T1 n Real
        g :: T1 n Real -> T1 n Real
        x = g (f (make 6 2))
          |]
          (AppU (VarU (TV "T1")) [NatLitU 12, VarU (TV "Real")])
      , -- Polymorphic id wraps a labelled call inside a multi-arg
        -- outer. Combines the Forall-instantiation path with the
        -- label-resolution path and exercises propagation at both
        -- layers.
        assertRawType
          "polymorphic id wrapping labelled call inside outer scale"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        id :: a -> a
        scale :: Real -> T1 n Real -> T1 n Real
        x = scale 1.0 (id (make 9))
          |]
          (AppU (VarU (TV "T1")) [NatLitU 9, VarU (TV "Real")])
      , -- Pin a `forall a` from inside a TupS literal: the first
        -- element of the tuple arg carries a labelled-nat value, and
        -- the function's polymorphism `forall a.` must be solved by
        -- that arg. Exercises the post-subtype when the carrier is a
        -- non-nat existential.
        assertRawType
          "tuple-arg's labelled element pins outer existential"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        fst_ :: (a, Bool) -> a
        x = fst_ (make 5, True)
          |]
          (AppU (VarU (TV "T1")) [NatLitU 5, VarU (TV "Real")])
      , -- The 3-function chain (the originally-failing test) as a
        -- regression marker in the same group so the whole class is
        -- co-located. Duplicated from natKindPromotionTests on
        -- purpose: if someone later moves or renames the original
        -- test, this group keeps locking in the contract.
        assertRawType
          "regression: labelled dims propagate through 3-function chain"
          [r|
        module main (x)
        type T1 (d :: Nat) a = [a]
        make :: n@Int -> T1 n Real
        f :: T1 n Real -> T1 n Real
        g :: T1 n Real -> T1 n Real
        x = g (f (make 9))
          |]
          (AppU (VarU (TV "T1")) [NatLitU 9, VarU (TV "Real")])
      ]

-- | Frontend validation of `--' with:` docstring atoms (terminal
-- actions on CLI-exported commands). One positive sanity check plus
-- coverage of the rejection paths in Frontend/Desugar.hs. Type-level
-- shape is verified by the golden test-suite; here we only care that
-- a legal shape passes and every illegal shape fails.
withDocstringTests :: TestTree
withDocstringTests =
  localOption (mkTimeout 1000000) $ -- 1s
    testGroup
      "with: docstring validation"
      [ -- Sanity: a legal single-atom `with:` composes cleanly.
        expectPass
          "single with: on effectful command with matching formatter"
          [r|
        module main (foo)
        effect IO
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: -l/--lines=fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- Multiple `with:` lines with distinct short/long are legal.
      , expectPass
          "two with: lines with distinct short and long"
          [r|
        module main (foo)
        effect IO
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        alt :: Int -> <IO> ()
        alt x = alt x
        --' with: -l/--lines=fmt
        --' with: -a/--alt=alt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- A pure terminal (no `<IO>`) on an effectful parent is legal.
        -- The synthesized entry inherits the parent's effect via the
        -- do-block; the terminal itself contributes no effect.
      , expectPass
          "pure terminal on effectful parent"
          [r|
        module main (foo)
        effect IO
        showInt :: Int -> Str
        showInt x = showInt x
        --' with: -s/--show=showInt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- A pure terminal on a pure parent is legal. Composed entry is
        -- pure `Int -> Str`; no effect is invented.
      , expectPass
          "pure terminal on pure parent"
          [r|
        module main (foo)
        showInt :: Int -> Str
        showInt x = showInt x
        --' with: -s/--show=showInt
        foo :: Int -> Int
        foo x = x
          |]

        -- Pure terminal returning () is fine too -- the "sink" shape
        -- doesn't require `<IO>` in the signature; a pure `A -> ()` is
        -- an unusual choice but should typecheck against any parent.
      , expectPass
          "pure terminal returning ()"
          [r|
        module main (foo)
        drop :: Int -> ()
        drop x = ()
        --' with: -d/--drop=drop
        foo :: Int -> Int
        foo x = x
          |]

        -- Duplicate long -> reject.
      , expectError
          "duplicate with: long name"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: -l/--lines=fmt
        --' with: -a/--lines=fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- Duplicate short -> reject.
      , expectError
          "duplicate with: short name"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: -l/--lines=fmt
        --' with: -l/--alt=fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- Short-only spec (`-l` without `--long`) -> reject.
      , expectError
          "with: short-only spec is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: -l=fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- Reserved long `--help` -> reject.
      , expectError
          "with: --help collides with reserved flag"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: --help=fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- Reserved short `-h` -> reject.
      , expectError
          "with: -h collides with reserved flag"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: -h/--halt=fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- `with:` on argument-level docstring -> reject (command-only).
      , expectError
          "with: on argument-level docstring is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        foo ::
          --' with: -l/--lines=fmt
          Int ->
          <IO> Int
        foo x = x
          |]

        -- `with:` on export without a signature -> reject.
      , expectError
          "with: on export without signature is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: -l/--lines=fmt
        foo = 42
          |]

        -- Malformed value (missing `=` between flag and term) -> reject.
      , expectError
          "with: missing = separator is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: -l/--lines fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- Empty term after `=` -> reject.
      , expectError
          "with: empty term name is rejected"
          [r|
        module main (foo)
        --' with: -l/--lines=
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- Long form must be lowercase-kebab. UPPER-cased long is rejected.
      , expectError
          "with: uppercased long name is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: --Lines=fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- Digit short is rejected (would collide with negative-number args).
      , expectError
          "with: digit short is rejected"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: -1/--lines=fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- `with:` long clashes with an argument-declared long. Both
        -- would land on the same clap Command; clap panics at build
        -- time. Frontend must reject first.
      , expectError
          "with: long collides with arg-declared long"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: --lines=fmt
        foo ::
          --' arg: --lines
          Int ->
          <IO> Int
        foo x = x
          |]

        -- Same for short.
      , expectError
          "with: short collides with arg-declared short"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: -x/--lines=fmt
        foo ::
          --' arg: -x/--count
          Int ->
          <IO> Int
        foo x = x
          |]

        -- Two `with:` long flags that mangle to the same internal name
        -- (hyphen -> underscore collapse) must be caught, or the
        -- synthesized bindings would clash silently.
      , expectError
          "with: two flags mangling to the same internal name"
          [r|
        module main (foo)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        --' with: --bar-baz=fmt
        --' with: --bar_baz=fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]

        -- Synthesized name collides with a user-defined top-level
        -- identifier in the same module.
      , expectError
          "with: mangled entry collides with user identifier"
          [r|
        module main (foo, mlcp_foo_lines)
        fmt :: Int -> <IO> ()
        fmt x = fmt x
        mlcp_foo_lines :: Int -> Int
        mlcp_foo_lines n = n
        --' with: -l/--lines=fmt
        foo :: Int -> <IO> Int
        foo x = x
          |]
      ]
