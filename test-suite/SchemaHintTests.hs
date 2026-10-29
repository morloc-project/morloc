{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : SchemaHintTests
Description : Wire shapes and the schema strings the runtime parses
-}
module SchemaHintTests (schemaHintTests) where

import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Serial
import Morloc.Data.Doc (render)

schemaHintTests :: TestTree
schemaHintTests = testGroup "wire schema"
  [ wireShapeTests
  , keyTests
  , corpusTest
  ]

fv :: T.Text -> FVar
fv g = FV (TV g) (CV "")

hinted :: T.Text -> T.Text -> FVar
hinted g c = FV (TV g) (CV c)

concrete :: SerialAST -> T.Text
concrete = render . serialAstToMsgpackSchema

general :: SerialAST -> T.Text
general = render . serialAstToGeneralSchema

tree :: T.Text -> SerialAST
tree name =
  SerialObject NamRecord (fv name) []
    [ (Key "kids", SerialList (fv "List") Nothing (SerialRec (fv name) []))
    , (Key "v", SerialInt (fv "Int")) ]

wireShapeTests :: TestTree
wireShapeTests = testGroup "wireShape"
  [ testCase "a hinted list of reals" $
      wireShape (SerialList (hinted "Vec" "std::vector<$1>") Nothing (SerialReal (hinted "Q" "units::q<double>")))
        @?= WList (WScalar (WFloat 8))
  , testCase "an optional declared list" $
      wireShape (SerialOptional (fv "?") (SerialList (fv "List") (Just (NatLitF 3)) (SerialString (fv "Str"))))
        @?= WOptional (WList WStr)
  , testCase "a recursive record" $
      wireShape (tree "Tree") @?= WCompound
  , testCase "an enum" $
      wireShape (SerialEnum (fv "Ton") [] ["Hell", "Dunkel"]) @?= WEnum
  , testCase "a table" $
      wireShape (SerialObject NamTable (fv "Table") [] [(Key "x", SerialInt (fv "Int"))]) @?= WTable
  , testCase "fixed width excludes Int and Null" $
      map isFixedWidthShape [WScalar WBool, WScalar (WUInt 1), WScalar WBigInt, WScalar WNull, WStr]
        @?= [True, True, False, False, False]
  ]

keyTests :: TestTree
keyTests = testGroup "keys"
  [ testCase "a key's length prefix counts UTF-8 bytes" $
      general (SerialObject NamRecord (fv "U") [] [(Key "gr\252n", SerialInt (fv "Int"))])
        @?= "m15gr\252nj"
  , testCase "the closure tuple" $
      closureWireSchema @?= "t3sjaau1"
  ]

-- Schemas the printer emits, each beside its hint-free form. The runtime's
-- tests parse every line (morloc-runtime-types/tests/schema_corpus.rs).
corpusPath :: FilePath
corpusPath = "data/rust/morloc-runtime-types/tests/schema-corpus.txt"

corpusTest :: TestTree
corpusTest = testCase "the printed corpus matches the one the runtime parses" $ do
  let rendered = T.unlines [concrete s <> "\t" <> general s | s <- corpus]
  rewrite <- lookupEnv "MORLOC_WRITE_SCHEMA_CORPUS"
  when (rewrite == Just "1") (BS.writeFile corpusPath (TE.encodeUtf8 rendered))
  stored <- BS.readFile corpusPath
  unless (stored == TE.encodeUtf8 rendered) $
    assertFailure $
      corpusPath <> " is not what the printer emits; regenerate it with"
        <> " MORLOC_WRITE_SCHEMA_CORPUS=1 and run the runtime-types cargo tests"

corpus :: [SerialAST]
corpus = handWritten <> take 400 (generated 2026)

handWritten :: [SerialAST]
handWritten =
  [ tree "Tree"
  , tree "B\228um"
  , SerialList (hinted "Map" "std::map<$1,$2>") Nothing
      (SerialTuple (fv "Tuple2") [SerialString (fv "Str"), SerialInt (fv "Int")])
  , SerialOptional (hinted "Opt" "std::optional<$1>") (SerialReal (hinted "Q" "units::q<double>"))
  , SerialList (fv "List") (Just (NatLitF 0)) (SerialInt8 (fv "I8"))
  , SerialList (fv "List") (Just NatVoidF) (SerialUInt8 (fv "U8"))
  , SerialEnum (fv "Ton") [] ["Hell", "Mittelgro\223", "\26085\26412"]
  , SerialVariant (fv "Shape") []
      [ ("Circle", [SerialReal (fv "Real")])
      , ("Rect\937", [SerialReal (fv "Real"), SerialReal (fv "Real")])
      , ("Empty", []) ]
  , SerialObject NamTable (fv "Table") [] []
  , SerialObject NamTable (fv "Table") [] [(Key "gr\252n", SerialInt (fv "Int")), (Key "b", SerialString (fv "Str"))]
  , SerialTuple (fv "Tuple3") [SerialIFile (fv "IFile"), SerialOStream (fv "OStream"), SerialIStream (fv "IStream")]
  , SerialObject NamRecord (fv "Wide") [] [(Key (T.pack ("k" <> show i)), SerialBool (fv "Bool")) | i <- [1 .. 70 :: Int]]
  , SerialObject NamRecord (fv "Long") [] [(Key (T.replicate 100 "\252"), SerialNull (fv "Unit"))]
  ]

-- A fixed pseudo-random stream of trees, so the corpus file is stable.
generated :: Int -> [SerialAST]
generated seed = go (lcg seed)
  where
    go s = let (t, s') = gen 3 s in t : go s'

lcg :: Int -> Int
lcg x = (x * 1103515245 + 12345) `mod` 2147483648

pick :: Int -> Int -> (Int, Int)
pick n s = let s' = lcg s in ((s' `div` 65536) `mod` n, s')

keys :: [T.Text]
keys = ["a", "b", "name", "gr\252n", "\26085\26412", "x_1", "\937mega", "Stra\223e"]

hints :: [FVar]
hints = [fv "T", hinted "Vec" "std::vector<$1>", hinted "Map" "std::map<$1,$2>", hinted "N" "numpy.ndarray"]

gen :: Int -> Int -> (SerialAST, Int)
gen depth s0 =
  let (k, s1) = pick (if depth <= 0 then 12 else 18) s0
  in case k of
    0 -> (SerialBool (fv "Bool"), s1)
    1 -> (SerialInt (fv "Int"), s1)
    2 -> (SerialReal (fv "Real"), s1)
    3 -> (SerialFloat32 (fv "F32"), s1)
    4 -> (SerialString (fv "Str"), s1)
    5 -> (SerialUInt8 (fv "U8"), s1)
    6 -> (SerialInt64 (fv "I64"), s1)
    7 -> (SerialNull (fv "Unit"), s1)
    8 -> (SerialUInt (fv "UInt"), s1)
    9 -> (SerialIFile (fv "IFile"), s1)
    10 -> let (n, s2) = pick (length keys) s1 in (SerialEnum (fv "E") [] (take (n + 1) keys), s2)
    11 -> (SerialInt16 (fv "I16"), s1)
    12 -> let (h, s2) = pick (length hints) s1
              (d, s3) = pick 3 s2
              (e, s4) = gen (depth - 1) s3
              dim = [Nothing, Just (NatLitF 4), Just NatVoidF] !! d
          in (SerialList (hints !! h) dim e, s4)
    13 -> let (n, s2) = pick 4 s1
              (es, s3) = gens (n + 1) s2
          in (SerialTuple (fv "Tuple") es, s3)
    14 -> let (n, s2) = pick (length keys) s1
              (es, s3) = gens (n + 1) s2
          in (SerialObject NamRecord (fv "R") [] (zip (map Key keys) es), s3)
    15 -> let (e, s2) = gen (depth - 1) s1 in (SerialOptional (fv "?") e, s2)
    16 -> let (n, s2) = pick 3 s1
              (es, s3) = gens n s2
          in (SerialVariant (fv "V") [] [("A", es), ("Ctor\252", [])], s3)
    _ -> let (h, s2) = pick (length hints) s1 in (SerialFloat64 (hints !! h), s2)
  where
    gens 0 s = ([], s)
    gens n s = let (e, s') = gen (depth - 1) s
                   (es, s'') = gens (n - 1 :: Int) s'
               in (e : es, s'')
