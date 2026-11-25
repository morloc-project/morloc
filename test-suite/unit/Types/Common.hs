{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Types.Common where

import Morloc (typecheck, typecheckFrontend, generatePools)
import Morloc.Frontend.Namespace
import Morloc.Frontend.Typecheck (evaluateAnnoSTypes)
import qualified Morloc.Monad as MM
import qualified Morloc.Typecheck.Internal as MTI
import qualified Morloc.Typecheck.NatDecide as ND
import qualified System.Directory as SD
import Text.RawString.QQ

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as MT
import Test.Tasty (TestTree)
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

-- | 'runFront' with the final state, to tell a rejection from a defect.
runFrontState :: MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int], MorlocState)
runFrontState code = do
  config <- emptyConfig
  ((x, _), st) <-
    MM.runMorlocMonad
      Nothing
      0
      config
      defaultValue
      (typecheckFrontend Nothing (Code code) >>= mapM evaluateAnnoSTypes)
  return (x, st)

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

-- | Run the frontend with a 'MorlocState' seed applied first, sharing the
-- monad scaffold. Backs the eval-sandbox runners below.
runFrontWith :: (MorlocState -> MorlocState) -> MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int])
runFrontWith seed code = do
  config <- emptyConfig
  ((x, _), _) <-
    MM.runMorlocMonad Nothing 0 config defaultValue $ do
      MM.modify seed
      typecheckFrontend Nothing (Code code)
  return x

-- | Run the frontend under a sandboxed eval (as `morloc eval --eval-sandbox`
-- does): seed eval mode + the module allow-list and pin local modules off,
-- then run the same checkEvalRestrictions -> typecheck path.
runEvalSandbox :: [MT.Text] -> MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int])
runEvalSandbox allow =
  runFrontWith $ \s -> s
    { stateEvalMode = True
    , stateAllowLocalModules = False
    , stateEvalSandbox = Just (Set.fromList (map MV allow))
    }

-- | Run the frontend under trusted (non-sandboxed) eval (as the dev CLI
-- `morloc eval`): eval mode on, but the sandbox gates are inert.
runEvalTrusted :: MT.Text -> IO (Either MorlocError [AnnoS (Indexed TypeU) Many Int])
runEvalTrusted = runFrontWith (\s -> s {stateEvalMode = True})

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

-- | Run the full code-generation pipeline ('typecheck' then 'generatePools',
-- which is where 'applyLambdas' runs) and force the result to a manifold
-- count. Used by the complexity guard to catch 'applyLambdas' rebuilding an
-- exponentially large tree: a divergent/blowup run overruns the group's
-- 'mkTimeout' (or exhausts memory) rather than returning.
runGen :: MT.Text -> IO (Either MorlocError Int)
runGen code = do
  config <- emptyConfig
  ((x, _), _) <-
    MM.runMorlocMonad Nothing 0 config defaultValue $ do
      (_, rasts) <- typecheck Nothing (Code code)
      pools <- generatePools rasts
      return (sum (map (length . snd) pools))
  return x

-- | Assert that a program lowers all the way to pools without diverging (the
-- complexity guards only care that 'runGen' returns before the timeout).
assertGenerates :: MT.Text -> Assertion
assertGenerates code = do
  result <- runGen code
  case result of
    Right _ -> return ()
    Left e -> assertFailure ("expected pool generation to succeed: " <> show e)

-- | Assert that a program is refused during pool generation: the frontend
-- accepts it, and lowering to the wire form is where it fails.
assertGenerationFails :: MT.Text -> Assertion
assertGenerationFails code = do
  result <- runGen code
  case result of
    Right _ -> assertFailure "expected pool generation to fail"
    Left _ -> return ()

emptyConfig :: IO Config
emptyConfig = do
  home <- SD.getHomeDirectory
  return $
    Config
      { configHome = home <> "/.local/share/morloc"
      , configState = home <> "/.local/share/morloc"
      , configLibrary = home <> "/.local/share/src/morloc"
      , configTmpDir = home <> "/.morloc/tmp"
      , configBuildConfig = home <> "/.morloc/.build-config.yaml"
      , configLangOverrides = mempty
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
    f s t@(IntVarU _) = (s, t)
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
    f t@(IntVarU _) = t
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

-- | Every unsolved existential the index names is the entry at its slot,
-- every cached solution is an entry of the context, and the annotation
-- index holds exactly the annotations of the context, newest first.
assertSubtypeIndexed :: String -> [GammaIndex] -> TypeU -> TypeU -> TestTree
assertSubtypeIndexed msg gs a b = testCase msg $
  case MTI.subtype Map.empty a b (listToGamma gs) of
    Left e -> assertFailure (show e)
    Right g -> do
      mapM_
        (\(v, s) -> case IntMap.lookup s (gammaContext g) of
            Just (ExistG v' _ _) | v' == v -> return ()
            other -> assertFailure $ show v <> " is indexed at a slot holding " <> show other)
        (Map.toList (gammaExist g))
      let solvedInContext = Set.fromList [v | SolvedG v _ <- IntMap.elems (gammaContext g)]
      mapM_
        (\v -> assertBool (show v <> " is solved but its entry is gone") (Set.member v solvedInContext))
        (Map.keys (gammaSolved g))
      let annsInContext = Map.fromListWith (flip (<>)) [(v, [s]) | (s, AnnG v _) <- IntMap.toDescList (gammaContext g)]
      assertEqual "annotation index" annsInContext (gammaAnn g)

-- | A Gamma holding the given entries at the given slots.
slotGamma :: [(Int, GammaIndex)] -> Gamma
slotGamma entries =
  (listToGamma [])
    { gammaSlot = MTI.slotSpacing * (1 + maximum (0 : map ((`div` MTI.slotSpacing) . fst) entries))
    , gammaContext = IntMap.fromList entries
    , gammaExist = Map.fromList [(v, s) | (s, ExistG v _ _) <- entries]
    , gammaAnn = Map.fromListWith (flip (<>)) [(v, [s]) | (s, AnnG v _) <- IntMap.toDescList (IntMap.fromList entries)]
    }

-- | Articulating an existential whose free slots are used up keeps every
-- entry indexed and in order, with the new entries just older than it.
assertArticulatedInOrder :: String -> [(Int, GammaIndex)] -> TVar -> [TVar] -> TestTree
assertArticulatedInOrder msg entries v older = testCase msg $
  case MTI.subtype Map.empty (ExistU v ([], Open) ([], Open)) (FunU [VarU (TV "A")] (VarU (TV "B"))) (slotGamma entries) of
    Left e -> assertFailure (show e)
    Right g -> do
      mapM_
        (\(x, s) -> case IntMap.lookup s (gammaContext g) of
            Just (ExistG x' _ _) | x' == x -> return ()
            other -> assertFailure $ show x <> " is indexed at a slot holding " <> show other)
        (Map.toList (gammaExist g))
      let names = [case e of ExistG x _ _ -> x; SolvedG x _ -> x; _ -> TV "?" | e <- IntMap.elems (gammaContext g)]
          below = takeWhile (/= v) names
      assertEqual "entry count" (length entries + 2) (IntMap.size (gammaContext g))
      assertEqual "older entries keep their order" older (filter (`elem` older) below)
      assertBool "new entries are just older than the articulated entry" $
        all (`notElem` older) (take 2 (reverse below))

-- | Convert a list of GammaIndex (newest first) to a Gamma with IntMap.
listToGamma :: [GammaIndex] -> Gamma
listToGamma gs =
  let spacing = MTI.slotSpacing
      n = length gs
      -- Newest entry gets highest slot
      indexed = zip [spacing * (n - 1), spacing * (n - 2) .. 0] gs
      ctx = IntMap.fromList indexed
      existMap = Map.fromList [(v, s) | (s, ExistG v _ _) <- indexed]
      annMap = Map.fromListWith (flip (<>)) [(v, [s]) | (s, AnnG v _) <- indexed]
  in Gamma
    { gammaCounter = 0
    , gammaSlot = spacing * n
    , gammaContext = ctx
    , gammaAnn = annMap
    , gammaExist = existMap
    , gammaSolved = Map.empty
    , gammaDeferred = []
    , gammaDeferredAt = Map.empty
    , gammaKindSubs = Map.empty
    , gammaEffSubs = Map.empty
    , gammaIntVals = Map.empty
    , gammaConstraints = []
    , gammaWanteds = []
    , gammaNestedUses = []
    , gammaAssumedConstraints = Nothing
    , gammaUnfolded = Set.empty
    , gammaPendingNumLits = []
    , gammaRigid = Nothing
    , gammaRigidNats = Set.empty
    , gammaRigidSlots = Set.empty
    , gammaRigidDomain = []
    , gammaLabelVars = Map.empty
    , gammaObserved = Set.empty
    , gammaDefinitionLambda = Nothing
    , gammaScoped = Map.empty

    , gammaPositionalReceivers = Set.empty
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
    (result, st) <- runFrontState code
    case result of
      (Right _) -> assertFailure . MT.unpack $ "Expected failure"
      (Left e) | MM.isDefect st e -> assertFailure $ "Expected a user error, got a compiler error: " <> show e
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

unifies :: [MT.Text] -> TypeU -> TypeU -> Bool
unifies vs a b = maybe False (const True) (unifyU (map TV vs) a b)

wireAgrees :: ([MT.Text], TypeU, TypeU) -> ([MT.Text], TypeU, TypeU) -> Bool
wireAgrees (vs1, h1, w1) (vs2, h2, w2) =
  wireFormsAgree (map TV vs1, h1, w1) (map TV vs2, h2, w2)

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

-- | @p_k n = add (p_{k-1} n) (p_{k-1} n)@, @k@ levels of signed definitions.
definitionLadder :: Int -> MT.Text
definitionLadder k =
  MT.unlines $
    [ "module main (p" <> num k <> ")"
    , "add :: Int -> Int -> Int"
    , "p0 :: Int -> Int"
    , "p0 n = add n 1"
    ]
      <> concat
        [ [ "p" <> num i <> " :: Int -> Int"
          , "p" <> num i <> " n = add (p" <> num (i - 1) <> " n) (p" <> num (i - 1) <> " n)"
          ]
        | i <- [1 .. k]
        ]
  where
    num = MT.pack . show

-- | @k@ uses of a method whose class has fourteen sourced instances, in
-- one definition.
manyMethodUses :: Int -> MT.Text
manyMethodUses k =
  "module main (f)\n"
    <> "class Gt a where\n  gtm :: a -> a -> Bool\n"
    <> mconcat
      [ "instance Gt " <> t <> " where\n  source Py from \"g.py\" (\"gt\" as gtm)\n"
      | t <- ["Int", "Str", "Real", "Bool", "I8", "I16", "I32", "I64", "U8", "U16", "U32", "U64", "F32", "F64"]
      ]
    <> "f :: Int -> Int -> [Bool]\n"
    <> "f x y = [" <> MT.intercalate ", " (replicate k "gtm x y") <> "]\n"

-- | @k@ uses of a method in one signed definition. The method's class is
-- the first of three; each has fourteen sourced instances and tuple
-- instances whose bodies use the next class's method once per component.
nestedMethodUses :: Int -> MT.Text
nestedMethodUses k =
  "module main (f)\n"
    <> mconcat [classOf l | l <- [0 .. layers - 1]]
    <> "both :: Bool -> Bool -> Bool\nboth a b = a\n"
    <> mconcat [tupleInstance l n | l <- [0 .. layers - 1], n <- [2, 3, 4]]
    <> "f :: Int -> Int -> [Bool]\n"
    <> "f x y = [" <> MT.intercalate ", " (replicate k "m0 x y") <> "]\n"
  where
    layers = 3 :: Int
    showT = MT.pack . show
    method l = "m" <> showT l
    classOf l =
      "class C" <> showT l <> " a where\n  " <> method l <> " :: a -> a -> Bool\n"
        <> mconcat
          [ "instance C" <> showT l <> " " <> t <> " where\n  source Py from \"g.py\" (\"eq\" as " <> method l <> ")\n"
          | t <- ["Int", "Str", "Real", "Bool", "I8", "I16", "I32", "I64", "U8", "U16", "U32", "U64", "F32", "F64"]
          ]
    tupleInstance l n =
      let vs = take n ["a", "b", "c", "d"]
          next = if l + 1 < layers then l + 1 else l
          ctx = MT.intercalate ", " [cls next v | v <- vs]
          cls c v = "C" <> showT c <> " " <> v
          use i = method next <> " (." <> showT i <> " x) (." <> showT i <> " y)"
          body = foldl (\acc i -> "both (" <> use i <> ") (" <> acc <> ")") (use (0 :: Int)) [1 .. n - 1]
       in "instance (" <> ctx <> ") => C" <> showT l <> " (" <> MT.intercalate ", " vs <> ") where\n  "
            <> method l <> " x y = " <> body <> "\n"

-- | A comparison class with container instances whose bodies use the class
-- again, and a newtype with its Packable instance.
classComparisons :: MT.Text
classComparisons =
  "module main (f)\n"
    <> "class Cmp a where\n  lt :: a -> a -> Bool\n  le :: a -> a -> Bool\n"
    <> "instance Cmp Int, Cmp Real, Cmp Str, Cmp Bool where\n  lt x y = False\n  le x y = True\n"
    <> "pick :: Bool -> Int -> Int -> Int\npick b x y\n  ? b = x\n  : y\n"
    <> "cmpC :: Cmp a => a -> a -> Int\ncmpC x y = pick (lt x y) 0 (pick (le x y) 1 2)\n"
    <> "instance Cmp a => Cmp [a] where\n  lt xs ys = lt (cmpC xs ys) 0\n  le xs ys = le (cmpC xs ys) 0\n"
    <> "instance (Cmp a, Cmp b) => Cmp (a, b) where\n"
    <> "  lt x y = lt (pick (lt (cmpC (.0 x) (.0 y)) 0) 0 (cmpC (.1 x) (.1 y))) 0\n"
    <> "  le x y = le (pick (lt (cmpC (.0 x) (.0 y)) 0) 0 (cmpC (.1 x) (.1 y))) 0\n"
    <> "instance (Cmp a, Cmp b, Cmp c) => Cmp (a, b, c) where\n"
    <> "  lt x y = lt (pick (lt (cmpC (.0 x) (.0 y)) 0) 0 (pick (lt (cmpC (.1 x) (.1 y)) 0) 0 (cmpC (.2 x) (.2 y)))) 0\n"
    <> "  le x y = le (pick (lt (cmpC (.0 x) (.0 y)) 0) 0 (pick (lt (cmpC (.1 x) (.1 y)) 0) 0 (cmpC (.2 x) (.2 y)))) 0\n"
    <> "class Packable a b where\n  pack :: a -> b\n  unpack :: b -> a\n"
    <> "newtype Tag = Int\n"
    <> "mapL :: (a -> b) -> [a] -> [b]\nmapL g xs = []\n"
    <> "sizeL :: [a] -> Int\nsizeL xs = 0\n"

-- | @k@ tuple comparisons and a use whose result type only the selected
-- Packable instance determines (CLS-20).
soleInstanceComparisons :: Int -> MT.Text
soleInstanceComparisons k =
  classComparisons
    <> "f :: [Tag] -> Int\n"
    <> "f ts = " <> foldr (\i e -> "(pick (lt (" <> MT.pack (show i) <> ", 2.0, \"a\") (3, 4.0, \"b\")) 1 " <> e <> ")") "(sizeL (mapL unpack ts))" [1 .. k] <> "\n"

-- | @(inc . inc . ... . inc) x@, @k@ compositions deep, with @(.)@ defined
-- in morloc and @inc@ sourced in Python and C++ so the realizer has a
-- choice at every call; every composition passes function values to a
-- morloc head.
nestedComposeChain :: Int -> MT.Text
nestedComposeChain k =
  "module main (run)\n"
    <> "type Py => Int = \"int\"\n"
    <> "type Cpp => Int = \"int\"\n"
    <> "source Py from \"c.py\" (\"inc\")\n"
    <> "source Cpp from \"c.hpp\" (\"inc\")\n"
    <> "inc :: Int -> Int\n"
    <> "(.) :: (b -> c) -> (a -> b) -> a -> c\n"
    <> "(.) f g x = f (g x)\n"
    <> "run :: Int -> Int\n"
    <> "run x = (" <> MT.intercalate " . " (replicate k "inc") <> ") x\n"

-- | @id . id . ... . id@ applied to an @Int@, @k@ compositions deep, with
-- no tuple/destructuring anywhere. Used to catch a per-solve full scan of
-- the solved context reintroducing quadratic typechecking on plain
-- composition chains.
deepIdComposition :: Int -> MT.Text
deepIdComposition k =
  "module main (r)\n"
    <> "id :: a -> a\n"
    <> "(.) :: (b -> c) -> (a -> b) -> a -> c\n"
    <> "r = (" <> MT.intercalate " . " (replicate k "id") <> ") 42\n"

-- | @map (\\(k, v) -> (k, dbl v)) . ... @ applied to @[(Str, Int)]@, @k@
-- compositions deep. Exercises a long chain in which every stage
-- destructures a tuple, so the receiver index is populated and the scan is
-- actually taken.
tuplePatternComposition :: Int -> MT.Text
tuplePatternComposition k =
  "module main (r)\n"
    <> "map :: (a -> b) -> [a] -> [b]\n"
    <> "dbl :: Int -> Int\n"
    <> "(.) :: (b -> c) -> (a -> b) -> a -> c\n"
    <> "xs :: [(Str, Int)]\n"
    <> "xs = [(\"a\", 1)]\n"
    <> "r = (" <> MT.intercalate " . " (replicate k stage) <> ") xs\n"
  where
    stage = "map (\\(k, v) -> (k, dbl v))"

-- | @dd (dd (... (big x)))@, @k@ deep, where @dd a = add a a@ uses its
-- parameter twice. Each application is a beta-redex @(\\a -> add a a) arg@
-- whose parameter is multiply-used, so substitution would clone @arg@ twice
-- and a chain doubles it (2^k). Sourced so it lands on the pool path. Used to
-- verify 'applyLambdas' shares such a parameter rather than cloning it.
dupParamChain :: Int -> MT.Text
dupParamChain k =
  "module main (run)\n"
    <> "type Py => Int = \"int\"\n"
    <> "source Py from \"dup.py\" (\"add\", \"big\")\n"
    <> "add :: Int -> Int -> Int\n"
    <> "big :: Int -> Int\n"
    <> "dd :: Int -> Int\n"
    <> "dd a = add a a\n"
    <> "run :: Int -> Int\n"
    <> "run x = " <> nest <> "\n"
  where
    nest = foldr (\_ acc -> "dd (" <> acc <> ")") "big x" [1 .. k]

-- | @mid (mid (... (seed x)))@, @k@ deep, where @mid a = inc a@ uses its
-- parameter exactly ONCE. Each application is a beta-redex @(\\a -> inc a) arg@
-- whose parameter is singly-used: the fix MOVES @arg@ (no copy), so the chain
-- is linear. Cloning instead re-copies the growing rest-of-chain at every
-- level -- O(k^2) -- and a deep chain blows up. First-order @Int -> Int@ only,
-- so realize does no function-argument re-scoring and this isolates the
-- 'applyLambdas' move/clone decision. Sourced so it lands on the pool path.
singleUseChain :: Int -> MT.Text
singleUseChain k =
  "module main (run)\n"
    <> "type Py => Int = \"int\"\n"
    <> "source Py from \"m.py\" (\"inc\", \"seed\")\n"
    <> "inc :: Int -> Int\n"
    <> "seed :: Int -> Int\n"
    <> "mid :: Int -> Int\n"
    <> "mid a = inc a\n"
    <> "run :: Int -> Int\n"
    <> "run x = " <> nest <> "\n"
  where
    nest = foldr (\_ acc -> "mid (" <> acc <> ")") "seed x" [1 .. k]

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

-- Shared abstract environment for the destructuring-propagation tests.
-- Every case exports a single binding `r`; it references only the
-- signatures/data it needs and ignores the rest.
tplEnv :: MT.Text
tplEnv =
  [r|
map :: (a -> b) -> [a] -> [b]
dbl :: Int -> Int
proj :: (Str, Int) -> Str
proj (k, v) = k
ys :: [Int]
ys = [1, 2, 3]
n :: Int
n = 5
t :: (Str, Int)
t = ("a", 1)
xs :: [(Str, Int)]
xs = [("a", 1)]
sxs :: [(Str, Str)]
sxs = [("a", "b")]
ts :: [(Str, Int, Bool)]
ts = [("a", 1, True)]
us :: [((Str, Int), Bool)]
us = [(("a", 1), True)]
|]

tplProg :: MT.Text -> MT.Text
tplProg body = "module main (r)\n" <> tplEnv <> "\nr = " <> body <> "\n"

-- | Like 'expectError', and also refusing an environment failure:
-- the program must be rejected as a user error.
expectUserError :: String -> MT.Text -> TestTree
expectUserError msg code =
  testCase msg $ do
    (result, st) <- runFrontState code
    case result of
      (Right _) -> assertFailure "Expected failure"
      (Left e) | MM.isDefect st e -> assertFailure $ "Expected a user error, got a compiler error: " <> show e
      (Left e@(EnvFailed _)) -> assertFailure $ "Expected a user error, got an environment failure: " <> show e
      (Left _) -> return ()

-- | The general type of the single export, with aliases as inference left
-- them (no alias evaluation).
rawGeneralType :: MT.Text -> IO (Either MorlocError TypeU)
rawGeneralType code = do
  result <- runFrontRaw code
  return $ case result of
    Right [x] -> Right (closeExistentials . MTI.cleanTypeName . renameExistentials . gtypeof $ x)
    Right _ -> Left (Fault Nothing "expected exactly one export")
    Left e -> Left e

-- | A program written with a transparent alias and the same program with the
-- alias written out must get the same verdict and the same evaluated type.
sameAsExpanded :: String -> MT.Text -> MT.Text -> TestTree
sameAsExpanded msg aliased expanded =
  testCase msg $ do
    a <- runFront aliased
    e <- runFront expanded
    case (a, e) of
      (Right [x], Right [y]) ->
        assertEqual "" (norm y) (norm x)
      (Left err, Right _) -> assertFailure $ "aliased program rejected: " <> show err
      (Right _, Left err) -> assertFailure $ "expanded program rejected: " <> show err
      (Left _, Left _) -> return ()
      _ -> assertFailure "expected exactly one export"
  where
    norm = closeExistentials . MTI.cleanTypeName . renameExistentials . gtypeof

holds, refuted :: ND.Decision -> Bool
holds (ND.Holds _) = True
holds _ = False
refuted (ND.Refuted _) = True
refuted _ = False
