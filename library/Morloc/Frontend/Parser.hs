{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}

module Morloc.Frontend.Parser
  ( readProgram
  , readType
  , PState (..)
  , emptyPState
  ) where

import qualified Control.Monad.State.Strict as State
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Scientific as DS
import Morloc.Frontend.Token
import Morloc.Frontend.Lexer (lexMorloc, showLexError)
import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import Morloc.Namespace.Expr
import qualified Morloc.BaseTypes as BT
import qualified Morloc.Language as ML
import Data.List (sortBy, foldl')
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Located)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn6 ([ExprI])
	| HappyAbsSyn7 (TypeU)
	| HappyAbsSyn8 (ExprI)
	| HappyAbsSyn15 (Text)
	| HappyAbsSyn16 ([Text])
	| HappyAbsSyn18 (Export)
	| HappyAbsSyn19 ([(Int, Symbol)])
	| HappyAbsSyn20 ((Int, Symbol))
	| HappyAbsSyn21 (Located)
	| HappyAbsSyn23 (Maybe [AliasedSymbol])
	| HappyAbsSyn24 ([AliasedSymbol])
	| HappyAbsSyn25 (AliasedSymbol)
	| HappyAbsSyn27 (NamType)
	| HappyAbsSyn28 (Lang)
	| HappyAbsSyn29 ((TVar, [Either TVar TypeU]))
	| HappyAbsSyn30 ([Either TVar TypeU])
	| HappyAbsSyn31 ((Key, TypeU))
	| HappyAbsSyn32 ([(Key, TypeU)])
	| HappyAbsSyn35 (([Constraint], ClassName, [TVar]))
	| HappyAbsSyn36 ([Constraint])
	| HappyAbsSyn37 ([Signature])
	| HappyAbsSyn38 (Signature)
	| HappyAbsSyn40 ([[ExprI]])
	| HappyAbsSyn43 ([EVar])
	| HappyAbsSyn44 (EVar)
	| HappyAbsSyn46 (Maybe Text)
	| HappyAbsSyn47 ([(Text, Maybe Text)])
	| HappyAbsSyn48 ((Text, Maybe Text))
	| HappyAbsSyn53 ([(EVar, ExprI)])
	| HappyAbsSyn54 ((EVar, ExprI))
	| HappyAbsSyn65 ([(Key, ExprI)])
	| HappyAbsSyn66 ((Key, ExprI))
	| HappyAbsSyn69 ([DoStmt])
	| HappyAbsSyn70 (DoStmt)
	| HappyAbsSyn73 ([(Text, ExprI)])
	| HappyAbsSyn74 ((Text, ExprI))
	| HappyAbsSyn81 (([ExprI], [Text]))
	| HappyAbsSyn87 ([TypeU])
	| HappyAbsSyn89 (([Constraint], TypeU))
	| HappyAbsSyn90 (Constraint)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Located)
	-> HappyState (Located) (HappyStk HappyAbsSyn -> [(Located)] -> m HappyAbsSyn)
	-> [HappyState (Located) (HappyStk HappyAbsSyn -> [(Located)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Located)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316,
 action_317,
 action_318,
 action_319,
 action_320,
 action_321,
 action_322,
 action_323,
 action_324,
 action_325,
 action_326,
 action_327,
 action_328,
 action_329,
 action_330,
 action_331,
 action_332,
 action_333,
 action_334,
 action_335,
 action_336,
 action_337,
 action_338,
 action_339,
 action_340,
 action_341,
 action_342,
 action_343,
 action_344,
 action_345,
 action_346,
 action_347,
 action_348,
 action_349,
 action_350,
 action_351,
 action_352,
 action_353,
 action_354,
 action_355,
 action_356,
 action_357,
 action_358,
 action_359,
 action_360,
 action_361,
 action_362,
 action_363,
 action_364,
 action_365,
 action_366,
 action_367,
 action_368,
 action_369,
 action_370,
 action_371,
 action_372,
 action_373,
 action_374,
 action_375,
 action_376,
 action_377,
 action_378,
 action_379,
 action_380,
 action_381,
 action_382,
 action_383,
 action_384,
 action_385,
 action_386,
 action_387,
 action_388,
 action_389,
 action_390,
 action_391,
 action_392,
 action_393,
 action_394,
 action_395,
 action_396,
 action_397,
 action_398,
 action_399,
 action_400,
 action_401,
 action_402,
 action_403 :: () => Prelude.Int -> ({-HappyReduction (P) = -}
	   Prelude.Int 
	-> (Located)
	-> HappyState (Located) (HappyStk HappyAbsSyn -> [(Located)] -> (P) HappyAbsSyn)
	-> [HappyState (Located) (HappyStk HappyAbsSyn -> [(Located)] -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Located)] -> (P) HappyAbsSyn)

happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164,
 happyReduce_165,
 happyReduce_166,
 happyReduce_167,
 happyReduce_168,
 happyReduce_169,
 happyReduce_170,
 happyReduce_171,
 happyReduce_172,
 happyReduce_173,
 happyReduce_174,
 happyReduce_175,
 happyReduce_176,
 happyReduce_177,
 happyReduce_178,
 happyReduce_179,
 happyReduce_180,
 happyReduce_181,
 happyReduce_182,
 happyReduce_183,
 happyReduce_184,
 happyReduce_185,
 happyReduce_186,
 happyReduce_187,
 happyReduce_188,
 happyReduce_189,
 happyReduce_190,
 happyReduce_191,
 happyReduce_192,
 happyReduce_193,
 happyReduce_194,
 happyReduce_195,
 happyReduce_196,
 happyReduce_197,
 happyReduce_198,
 happyReduce_199,
 happyReduce_200,
 happyReduce_201,
 happyReduce_202,
 happyReduce_203,
 happyReduce_204,
 happyReduce_205,
 happyReduce_206,
 happyReduce_207,
 happyReduce_208,
 happyReduce_209,
 happyReduce_210,
 happyReduce_211,
 happyReduce_212,
 happyReduce_213,
 happyReduce_214,
 happyReduce_215 :: () => ({-HappyReduction (P) = -}
	   Prelude.Int 
	-> (Located)
	-> HappyState (Located) (HappyStk HappyAbsSyn -> [(Located)] -> (P) HappyAbsSyn)
	-> [HappyState (Located) (HappyStk HappyAbsSyn -> [(Located)] -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Located)] -> (P) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,1345) ([0,0,0,0,0,0,2,256,0,0,0,0,0,0,0,40960,10,0,1120,0,0,0,0,0,0,34112,519,26627,30,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,4096,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17152,48,4096,0,0,0,0,0,0,43008,96,96,972,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,47104,24822,96,1005,0,0,0,0,0,0,57808,49281,39424,7,0,0,0,0,0,40960,962,385,3892,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5376,2076,32780,121,0,0,0,0,0,0,2,0,4608,0,0,0,0,0,0,0,0,0,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,8192,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,5376,0,0,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,23552,1,0,140,0,0,0,0,0,0,680,0,6144,1,0,0,0,0,0,20480,5,0,560,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,2560,0,65411,8,0,0,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4288,12,1024,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,3072,0,0,0,0,0,0,2048,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,10,0,4480,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,1024,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1360,0,12288,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96,0,0,0,0,0,32768,3850,1540,15568,0,0,0,0,0,0,5376,2078,40972,121,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,4608,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,4,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,32768,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57680,49281,39424,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34112,519,26627,30,0,0,0,0,0,32768,42,0,4480,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15402,6160,62272,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,2048,8192,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61608,24640,52480,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49824,33027,13313,15,0,0,0,0,0,16384,1925,770,7784,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30804,12320,59008,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,32768,3850,1540,15568,0,0,0,0,0,0,5376,2078,40972,121,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,340,0,35840,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12352,260,16384,1,0,0,0,0,0,32768,2144,2,640,0,0,0,0,0,0,49408,1040,0,5,0,0,0,0,0,0,42,0,17920,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,168,8,6144,1,0,0,0,0,0,28672,5,0,560,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,3072,0,32,0,0,0,0,0,0,0,16392,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,32768,32705,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,16,0,0,0,0,0,0,32,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,1,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,10752,4156,16408,243,0,0,0,0,0,0,340,0,35840,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,384,0,0,0,0,0,0,256,0,1,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,1024,128,0,4,0,0,0,0,0,0,8,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21,0,8960,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,43008,0,8,280,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2144,6,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,16,0,0,0,0,0,0,0,0,49824,33027,13313,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2688,1039,53254,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10752,4156,16408,243,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57680,49281,39424,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,16,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,2081,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,256,0,0,3,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,43520,0,0,70,0,0,0,0,0,0,17156,16,5120,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,32768,0,0,128,0,0,0,0,0,0,5376,0,0,35,0,0,0,0,0,0,42,0,17920,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,12288,0,0,16,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10880,0,32768,17,0,0,0,0,0,0,85,0,8960,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,16384,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2144,2,512,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,3,0,256,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24576,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,680,0,6144,1,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,768,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,1032,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,32768,42,0,4480,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21504,0,0,140,0,0,0,0,0,0,168,0,6144,1,0,0,0,0,0,3072,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,8192,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,64,0,0,0,0,0,0,2688,0,32768,17,0,0,0,0,0,0,85,0,8960,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,64,0,0,0,0,0,0,10880,0,32768,17,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,0,0,0,0,0,0,0,0,4608,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24710,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,42,0,4480,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,128,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseProgram","%start_parseTypeOnly","%start_parseExprOnly","program","type_eof","expr_eof","modules","module","top_body","top_decls","top_decl","sig_or_ass","module_name","module_parts","module_comp","exports","export_list","export_item","symbol","import_decl","opt_import_list","import_items","import_item","typedef_decl","nam_type","lang_name","typedef_term","typedef_params","nam_entry","nam_entry_list","nam_entries","typeclass_decl","class_head","class_constraints","sig_list","signature","instance_decl","instance_items","instance_item","fixity_decl","operator_names","operator_ref","source_decl","opt_from","source_items","source_item","source_new_items","source_new_item","expr","let_expr","let_bindings","let_binding","lambda_expr","infix_expr","operand","app_expr","atom_expr","force_expr","paren_expr","expr_list1","suspend_expr","record_expr","record_entries","record_entry","list_expr","do_expr","do_stmts","do_stmt","getter_expr","setter_expr","setter_entries","setter_entry","var_expr","hole_expr","bool_expr","num_expr","string_expr","interp_string","interp_body","type","fun_type","non_fun_type","app_type","atom_type","type_list1","types1","sig_type","single_constraint","operator_name","evar_or_op","opt_where_decls","where_items","where_item","lower_names","lower_names1","VLBRACE","VRBRACE","VSEMI","'('","')'","'['","']'","'{'","'}'","'<'","'>'","','","'\\\\'","'_'","'!'","'.'","'='","'::'","'->'","'=>'","'<-'","'*'","'-'","'module'","'import'","'source'","'from'","'where'","'as'","'True'","'False'","'type'","'record'","'object'","'table'","'class'","'instance'","'infixl'","'infixr'","'infix'","'let'","'in'","'do'","LOWER","UPPER","OPERATOR","INTEGER","FLOAT","STRING","STRSTART","STRMID","STREND","INTERPOPEN","INTERPCLOSE","EOF","%eof"]
        bit_start = st Prelude.* 153
        bit_end = (st Prelude.+ 1) Prelude.* 153
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..152]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (98) = happyShift action_63
action_0 (121) = happyShift action_6
action_0 (6) = happyGoto action_61
action_0 (9) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 (11) = happyGoto action_62
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (101) = happyShift action_54
action_1 (103) = happyShift action_55
action_1 (105) = happyShift action_56
action_1 (107) = happyShift action_57
action_1 (141) = happyShift action_58
action_1 (142) = happyShift action_59
action_1 (146) = happyShift action_60
action_1 (7) = happyGoto action_48
action_1 (82) = happyGoto action_49
action_1 (83) = happyGoto action_50
action_1 (84) = happyGoto action_51
action_1 (85) = happyGoto action_52
action_1 (86) = happyGoto action_53
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (101) = happyShift action_31
action_2 (103) = happyShift action_32
action_2 (105) = happyShift action_33
action_2 (110) = happyShift action_34
action_2 (111) = happyShift action_35
action_2 (112) = happyShift action_36
action_2 (113) = happyShift action_37
action_2 (120) = happyShift action_38
action_2 (127) = happyShift action_39
action_2 (128) = happyShift action_40
action_2 (138) = happyShift action_41
action_2 (140) = happyShift action_42
action_2 (141) = happyShift action_43
action_2 (144) = happyShift action_44
action_2 (145) = happyShift action_45
action_2 (146) = happyShift action_46
action_2 (147) = happyShift action_47
action_2 (8) = happyGoto action_7
action_2 (51) = happyGoto action_8
action_2 (52) = happyGoto action_9
action_2 (53) = happyGoto action_10
action_2 (54) = happyGoto action_11
action_2 (55) = happyGoto action_12
action_2 (56) = happyGoto action_13
action_2 (57) = happyGoto action_14
action_2 (58) = happyGoto action_15
action_2 (59) = happyGoto action_16
action_2 (60) = happyGoto action_17
action_2 (61) = happyGoto action_18
action_2 (63) = happyGoto action_19
action_2 (64) = happyGoto action_20
action_2 (67) = happyGoto action_21
action_2 (68) = happyGoto action_22
action_2 (71) = happyGoto action_23
action_2 (72) = happyGoto action_24
action_2 (75) = happyGoto action_25
action_2 (76) = happyGoto action_26
action_2 (77) = happyGoto action_27
action_2 (78) = happyGoto action_28
action_2 (79) = happyGoto action_29
action_2 (80) = happyGoto action_30
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (121) = happyShift action_6
action_3 (9) = happyGoto action_4
action_3 (10) = happyGoto action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (121) = happyShift action_6
action_4 (152) = happyShift action_140
action_4 (10) = happyGoto action_139
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_7

action_6 (141) = happyShift action_138
action_6 (15) = happyGoto action_135
action_6 (16) = happyGoto action_136
action_6 (17) = happyGoto action_137
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (153) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (152) = happyShift action_134
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_109

action_10 (138) = happyShift action_41
action_10 (139) = happyShift action_133
action_10 (54) = happyGoto action_132
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_114

action_12 _ = happyReduce_110

action_13 (115) = happyShift action_131
action_13 _ = happyReduce_111

action_14 (107) = happyShift action_121
action_14 (108) = happyShift action_122
action_14 (113) = happyShift action_129
action_14 (119) = happyShift action_124
action_14 (120) = happyShift action_130
action_14 (143) = happyShift action_126
action_14 (91) = happyGoto action_128
action_14 _ = happyReduce_119

action_15 (101) = happyShift action_31
action_15 (103) = happyShift action_32
action_15 (105) = happyShift action_33
action_15 (111) = happyShift action_35
action_15 (112) = happyShift action_36
action_15 (113) = happyShift action_37
action_15 (120) = happyShift action_38
action_15 (127) = happyShift action_39
action_15 (128) = happyShift action_40
action_15 (140) = happyShift action_42
action_15 (141) = happyShift action_43
action_15 (144) = happyShift action_44
action_15 (145) = happyShift action_45
action_15 (146) = happyShift action_46
action_15 (147) = happyShift action_47
action_15 (59) = happyGoto action_127
action_15 (60) = happyGoto action_17
action_15 (61) = happyGoto action_18
action_15 (63) = happyGoto action_19
action_15 (64) = happyGoto action_20
action_15 (67) = happyGoto action_21
action_15 (68) = happyGoto action_22
action_15 (71) = happyGoto action_23
action_15 (72) = happyGoto action_24
action_15 (75) = happyGoto action_25
action_15 (76) = happyGoto action_26
action_15 (77) = happyGoto action_27
action_15 (78) = happyGoto action_28
action_15 (79) = happyGoto action_29
action_15 (80) = happyGoto action_30
action_15 _ = happyReduce_122

action_16 _ = happyReduce_123

action_17 _ = happyReduce_125

action_18 _ = happyReduce_126

action_19 _ = happyReduce_133

action_20 _ = happyReduce_134

action_21 _ = happyReduce_132

action_22 _ = happyReduce_137

action_23 _ = happyReduce_128

action_24 _ = happyReduce_127

action_25 _ = happyReduce_135

action_26 _ = happyReduce_136

action_27 _ = happyReduce_130

action_28 _ = happyReduce_131

action_29 _ = happyReduce_129

action_30 _ = happyReduce_173

action_31 (101) = happyShift action_31
action_31 (102) = happyShift action_120
action_31 (103) = happyShift action_32
action_31 (105) = happyShift action_33
action_31 (107) = happyShift action_121
action_31 (108) = happyShift action_122
action_31 (110) = happyShift action_34
action_31 (111) = happyShift action_35
action_31 (112) = happyShift action_36
action_31 (113) = happyShift action_123
action_31 (119) = happyShift action_124
action_31 (120) = happyShift action_125
action_31 (127) = happyShift action_39
action_31 (128) = happyShift action_40
action_31 (138) = happyShift action_41
action_31 (140) = happyShift action_42
action_31 (141) = happyShift action_43
action_31 (143) = happyShift action_126
action_31 (144) = happyShift action_44
action_31 (145) = happyShift action_45
action_31 (146) = happyShift action_46
action_31 (147) = happyShift action_47
action_31 (51) = happyGoto action_118
action_31 (52) = happyGoto action_9
action_31 (53) = happyGoto action_10
action_31 (54) = happyGoto action_11
action_31 (55) = happyGoto action_12
action_31 (56) = happyGoto action_13
action_31 (57) = happyGoto action_14
action_31 (58) = happyGoto action_15
action_31 (59) = happyGoto action_16
action_31 (60) = happyGoto action_17
action_31 (61) = happyGoto action_18
action_31 (63) = happyGoto action_19
action_31 (64) = happyGoto action_20
action_31 (67) = happyGoto action_21
action_31 (68) = happyGoto action_22
action_31 (71) = happyGoto action_23
action_31 (72) = happyGoto action_24
action_31 (75) = happyGoto action_25
action_31 (76) = happyGoto action_26
action_31 (77) = happyGoto action_27
action_31 (78) = happyGoto action_28
action_31 (79) = happyGoto action_29
action_31 (80) = happyGoto action_30
action_31 (91) = happyGoto action_119
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (101) = happyShift action_31
action_32 (103) = happyShift action_32
action_32 (104) = happyShift action_117
action_32 (105) = happyShift action_33
action_32 (110) = happyShift action_34
action_32 (111) = happyShift action_35
action_32 (112) = happyShift action_36
action_32 (113) = happyShift action_37
action_32 (120) = happyShift action_38
action_32 (127) = happyShift action_39
action_32 (128) = happyShift action_40
action_32 (138) = happyShift action_41
action_32 (140) = happyShift action_42
action_32 (141) = happyShift action_43
action_32 (144) = happyShift action_44
action_32 (145) = happyShift action_45
action_32 (146) = happyShift action_46
action_32 (147) = happyShift action_47
action_32 (51) = happyGoto action_115
action_32 (52) = happyGoto action_9
action_32 (53) = happyGoto action_10
action_32 (54) = happyGoto action_11
action_32 (55) = happyGoto action_12
action_32 (56) = happyGoto action_13
action_32 (57) = happyGoto action_14
action_32 (58) = happyGoto action_15
action_32 (59) = happyGoto action_16
action_32 (60) = happyGoto action_17
action_32 (61) = happyGoto action_18
action_32 (62) = happyGoto action_116
action_32 (63) = happyGoto action_19
action_32 (64) = happyGoto action_20
action_32 (67) = happyGoto action_21
action_32 (68) = happyGoto action_22
action_32 (71) = happyGoto action_23
action_32 (72) = happyGoto action_24
action_32 (75) = happyGoto action_25
action_32 (76) = happyGoto action_26
action_32 (77) = happyGoto action_27
action_32 (78) = happyGoto action_28
action_32 (79) = happyGoto action_29
action_32 (80) = happyGoto action_30
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (101) = happyShift action_31
action_33 (103) = happyShift action_32
action_33 (105) = happyShift action_33
action_33 (110) = happyShift action_34
action_33 (111) = happyShift action_35
action_33 (112) = happyShift action_36
action_33 (113) = happyShift action_37
action_33 (120) = happyShift action_38
action_33 (127) = happyShift action_39
action_33 (128) = happyShift action_40
action_33 (138) = happyShift action_41
action_33 (140) = happyShift action_42
action_33 (141) = happyShift action_114
action_33 (144) = happyShift action_44
action_33 (145) = happyShift action_45
action_33 (146) = happyShift action_46
action_33 (147) = happyShift action_47
action_33 (51) = happyGoto action_111
action_33 (52) = happyGoto action_9
action_33 (53) = happyGoto action_10
action_33 (54) = happyGoto action_11
action_33 (55) = happyGoto action_12
action_33 (56) = happyGoto action_13
action_33 (57) = happyGoto action_14
action_33 (58) = happyGoto action_15
action_33 (59) = happyGoto action_16
action_33 (60) = happyGoto action_17
action_33 (61) = happyGoto action_18
action_33 (63) = happyGoto action_19
action_33 (64) = happyGoto action_20
action_33 (65) = happyGoto action_112
action_33 (66) = happyGoto action_113
action_33 (67) = happyGoto action_21
action_33 (68) = happyGoto action_22
action_33 (71) = happyGoto action_23
action_33 (72) = happyGoto action_24
action_33 (75) = happyGoto action_25
action_33 (76) = happyGoto action_26
action_33 (77) = happyGoto action_27
action_33 (78) = happyGoto action_28
action_33 (79) = happyGoto action_29
action_33 (80) = happyGoto action_30
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (141) = happyShift action_110
action_34 (97) = happyGoto action_109
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_165

action_36 (101) = happyShift action_31
action_36 (103) = happyShift action_32
action_36 (105) = happyShift action_33
action_36 (111) = happyShift action_35
action_36 (112) = happyShift action_36
action_36 (113) = happyShift action_37
action_36 (120) = happyShift action_38
action_36 (127) = happyShift action_39
action_36 (128) = happyShift action_40
action_36 (140) = happyShift action_42
action_36 (141) = happyShift action_43
action_36 (144) = happyShift action_44
action_36 (145) = happyShift action_45
action_36 (146) = happyShift action_46
action_36 (147) = happyShift action_47
action_36 (59) = happyGoto action_108
action_36 (60) = happyGoto action_17
action_36 (61) = happyGoto action_18
action_36 (63) = happyGoto action_19
action_36 (64) = happyGoto action_20
action_36 (67) = happyGoto action_21
action_36 (68) = happyGoto action_22
action_36 (71) = happyGoto action_23
action_36 (72) = happyGoto action_24
action_36 (75) = happyGoto action_25
action_36 (76) = happyGoto action_26
action_36 (77) = happyGoto action_27
action_36 (78) = happyGoto action_28
action_36 (79) = happyGoto action_29
action_36 (80) = happyGoto action_30
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (101) = happyShift action_105
action_37 (141) = happyShift action_106
action_37 (144) = happyShift action_107
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (144) = happyShift action_103
action_38 (145) = happyShift action_104
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_166

action_40 _ = happyReduce_167

action_41 (111) = happyShift action_101
action_41 (141) = happyShift action_102
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (98) = happyShift action_100
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_164

action_44 _ = happyReduce_168

action_45 _ = happyReduce_169

action_46 _ = happyReduce_172

action_47 (150) = happyShift action_99
action_47 (81) = happyGoto action_98
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (153) = happyAccept
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (152) = happyShift action_97
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_177

action_51 (116) = happyShift action_96
action_51 _ = happyReduce_178

action_52 (101) = happyShift action_54
action_52 (103) = happyShift action_55
action_52 (105) = happyShift action_56
action_52 (141) = happyShift action_58
action_52 (142) = happyShift action_59
action_52 (146) = happyShift action_60
action_52 (86) = happyGoto action_95
action_52 _ = happyReduce_181

action_53 _ = happyReduce_183

action_54 (101) = happyShift action_54
action_54 (102) = happyShift action_94
action_54 (103) = happyShift action_55
action_54 (105) = happyShift action_56
action_54 (107) = happyShift action_57
action_54 (141) = happyShift action_58
action_54 (142) = happyShift action_59
action_54 (146) = happyShift action_60
action_54 (82) = happyGoto action_93
action_54 (83) = happyGoto action_50
action_54 (84) = happyGoto action_51
action_54 (85) = happyGoto action_52
action_54 (86) = happyGoto action_53
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (101) = happyShift action_54
action_55 (103) = happyShift action_55
action_55 (105) = happyShift action_56
action_55 (107) = happyShift action_57
action_55 (141) = happyShift action_58
action_55 (142) = happyShift action_59
action_55 (146) = happyShift action_60
action_55 (82) = happyGoto action_92
action_55 (83) = happyGoto action_50
action_55 (84) = happyGoto action_51
action_55 (85) = happyGoto action_52
action_55 (86) = happyGoto action_53
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (101) = happyShift action_54
action_56 (103) = happyShift action_55
action_56 (105) = happyShift action_56
action_56 (107) = happyShift action_57
action_56 (141) = happyShift action_58
action_56 (142) = happyShift action_59
action_56 (146) = happyShift action_60
action_56 (82) = happyGoto action_91
action_56 (83) = happyGoto action_50
action_56 (84) = happyGoto action_51
action_56 (85) = happyGoto action_52
action_56 (86) = happyGoto action_53
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (141) = happyShift action_90
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_190

action_59 _ = happyReduce_189

action_60 _ = happyReduce_191

action_61 (153) = happyAccept
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (152) = happyShift action_89
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (99) = happyShift action_75
action_63 (101) = happyShift action_76
action_63 (122) = happyShift action_77
action_63 (123) = happyShift action_78
action_63 (129) = happyShift action_79
action_63 (130) = happyShift action_80
action_63 (131) = happyShift action_81
action_63 (132) = happyShift action_82
action_63 (133) = happyShift action_83
action_63 (134) = happyShift action_84
action_63 (135) = happyShift action_85
action_63 (136) = happyShift action_86
action_63 (137) = happyShift action_87
action_63 (141) = happyShift action_88
action_63 (12) = happyGoto action_64
action_63 (13) = happyGoto action_65
action_63 (14) = happyGoto action_66
action_63 (22) = happyGoto action_67
action_63 (26) = happyGoto action_68
action_63 (27) = happyGoto action_69
action_63 (34) = happyGoto action_70
action_63 (39) = happyGoto action_71
action_63 (42) = happyGoto action_72
action_63 (45) = happyGoto action_73
action_63 (92) = happyGoto action_74
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (99) = happyShift action_197
action_64 (100) = happyShift action_198
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_12

action_66 _ = happyReduce_20

action_67 _ = happyReduce_14

action_68 _ = happyReduce_15

action_69 (101) = happyShift action_195
action_69 (142) = happyShift action_196
action_69 (29) = happyGoto action_194
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_16

action_71 _ = happyReduce_17

action_72 _ = happyReduce_18

action_73 _ = happyReduce_19

action_74 (96) = happyGoto action_193
action_74 _ = happyReduce_212

action_75 _ = happyReduce_11

action_76 (107) = happyShift action_121
action_76 (108) = happyShift action_122
action_76 (113) = happyShift action_129
action_76 (119) = happyShift action_124
action_76 (120) = happyShift action_192
action_76 (143) = happyShift action_126
action_76 (91) = happyGoto action_191
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (141) = happyShift action_138
action_77 (15) = happyGoto action_190
action_77 (16) = happyGoto action_136
action_77 (17) = happyGoto action_137
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (141) = happyShift action_188
action_78 (142) = happyShift action_189
action_78 (28) = happyGoto action_187
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (101) = happyShift action_185
action_79 (142) = happyShift action_186
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_56

action_81 _ = happyReduce_57

action_82 _ = happyReduce_58

action_83 (101) = happyShift action_184
action_83 (103) = happyShift action_55
action_83 (105) = happyShift action_56
action_83 (141) = happyShift action_58
action_83 (142) = happyShift action_59
action_83 (146) = happyShift action_60
action_83 (35) = happyGoto action_182
action_83 (85) = happyGoto action_183
action_83 (86) = happyGoto action_53
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (142) = happyShift action_181
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (144) = happyShift action_180
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (144) = happyShift action_179
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (144) = happyShift action_178
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_204

action_89 _ = happyReduce_4

action_90 (108) = happyShift action_177
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (106) = happyShift action_176
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (104) = happyShift action_175
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (102) = happyShift action_173
action_93 (109) = happyShift action_174
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_184

action_95 _ = happyReduce_182

action_96 (101) = happyShift action_54
action_96 (103) = happyShift action_55
action_96 (105) = happyShift action_56
action_96 (107) = happyShift action_57
action_96 (141) = happyShift action_58
action_96 (142) = happyShift action_59
action_96 (146) = happyShift action_60
action_96 (82) = happyGoto action_172
action_96 (83) = happyGoto action_50
action_96 (84) = happyGoto action_51
action_96 (85) = happyGoto action_52
action_96 (86) = happyGoto action_53
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_5

action_98 (148) = happyShift action_170
action_98 (149) = happyShift action_171
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (101) = happyShift action_31
action_99 (103) = happyShift action_32
action_99 (105) = happyShift action_33
action_99 (110) = happyShift action_34
action_99 (111) = happyShift action_35
action_99 (112) = happyShift action_36
action_99 (113) = happyShift action_37
action_99 (120) = happyShift action_38
action_99 (127) = happyShift action_39
action_99 (128) = happyShift action_40
action_99 (138) = happyShift action_41
action_99 (140) = happyShift action_42
action_99 (141) = happyShift action_43
action_99 (144) = happyShift action_44
action_99 (145) = happyShift action_45
action_99 (146) = happyShift action_46
action_99 (147) = happyShift action_47
action_99 (51) = happyGoto action_169
action_99 (52) = happyGoto action_9
action_99 (53) = happyGoto action_10
action_99 (54) = happyGoto action_11
action_99 (55) = happyGoto action_12
action_99 (56) = happyGoto action_13
action_99 (57) = happyGoto action_14
action_99 (58) = happyGoto action_15
action_99 (59) = happyGoto action_16
action_99 (60) = happyGoto action_17
action_99 (61) = happyGoto action_18
action_99 (63) = happyGoto action_19
action_99 (64) = happyGoto action_20
action_99 (67) = happyGoto action_21
action_99 (68) = happyGoto action_22
action_99 (71) = happyGoto action_23
action_99 (72) = happyGoto action_24
action_99 (75) = happyGoto action_25
action_99 (76) = happyGoto action_26
action_99 (77) = happyGoto action_27
action_99 (78) = happyGoto action_28
action_99 (79) = happyGoto action_29
action_99 (80) = happyGoto action_30
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (101) = happyShift action_31
action_100 (103) = happyShift action_32
action_100 (105) = happyShift action_33
action_100 (110) = happyShift action_34
action_100 (111) = happyShift action_35
action_100 (112) = happyShift action_36
action_100 (113) = happyShift action_37
action_100 (120) = happyShift action_38
action_100 (127) = happyShift action_39
action_100 (128) = happyShift action_40
action_100 (138) = happyShift action_41
action_100 (140) = happyShift action_42
action_100 (141) = happyShift action_168
action_100 (144) = happyShift action_44
action_100 (145) = happyShift action_45
action_100 (146) = happyShift action_46
action_100 (147) = happyShift action_47
action_100 (51) = happyGoto action_165
action_100 (52) = happyGoto action_9
action_100 (53) = happyGoto action_10
action_100 (54) = happyGoto action_11
action_100 (55) = happyGoto action_12
action_100 (56) = happyGoto action_13
action_100 (57) = happyGoto action_14
action_100 (58) = happyGoto action_15
action_100 (59) = happyGoto action_16
action_100 (60) = happyGoto action_17
action_100 (61) = happyGoto action_18
action_100 (63) = happyGoto action_19
action_100 (64) = happyGoto action_20
action_100 (67) = happyGoto action_21
action_100 (68) = happyGoto action_22
action_100 (69) = happyGoto action_166
action_100 (70) = happyGoto action_167
action_100 (71) = happyGoto action_23
action_100 (72) = happyGoto action_24
action_100 (75) = happyGoto action_25
action_100 (76) = happyGoto action_26
action_100 (77) = happyGoto action_27
action_100 (78) = happyGoto action_28
action_100 (79) = happyGoto action_29
action_100 (80) = happyGoto action_30
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (114) = happyShift action_164
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (114) = happyShift action_163
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_170

action_104 _ = happyReduce_171

action_105 (141) = happyShift action_162
action_105 (73) = happyGoto action_160
action_105 (74) = happyGoto action_161
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_158

action_107 _ = happyReduce_159

action_108 _ = happyReduce_138

action_109 (116) = happyShift action_158
action_109 (141) = happyShift action_159
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_214

action_111 (106) = happyShift action_157
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (106) = happyShift action_155
action_112 (109) = happyShift action_156
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_148

action_114 (114) = happyShift action_154
action_114 _ = happyReduce_164

action_115 _ = happyReduce_144

action_116 (104) = happyShift action_152
action_116 (109) = happyShift action_153
action_116 _ = happyFail (happyExpListPerState 116)

action_117 _ = happyReduce_151

action_118 (102) = happyShift action_150
action_118 (109) = happyShift action_151
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (102) = happyShift action_149
action_119 _ = happyFail (happyExpListPerState 119)

action_120 _ = happyReduce_139

action_121 _ = happyReduce_202

action_122 _ = happyReduce_203

action_123 (101) = happyShift action_105
action_123 (141) = happyShift action_106
action_123 (144) = happyShift action_107
action_123 _ = happyReduce_201

action_124 _ = happyReduce_200

action_125 (102) = happyShift action_148
action_125 (144) = happyShift action_103
action_125 (145) = happyShift action_104
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_199

action_127 _ = happyReduce_124

action_128 (101) = happyShift action_31
action_128 (103) = happyShift action_32
action_128 (105) = happyShift action_33
action_128 (110) = happyShift action_34
action_128 (111) = happyShift action_35
action_128 (112) = happyShift action_36
action_128 (113) = happyShift action_37
action_128 (120) = happyShift action_38
action_128 (127) = happyShift action_39
action_128 (128) = happyShift action_40
action_128 (138) = happyShift action_41
action_128 (140) = happyShift action_42
action_128 (141) = happyShift action_43
action_128 (144) = happyShift action_44
action_128 (145) = happyShift action_45
action_128 (146) = happyShift action_46
action_128 (147) = happyShift action_47
action_128 (51) = happyGoto action_147
action_128 (52) = happyGoto action_9
action_128 (53) = happyGoto action_10
action_128 (54) = happyGoto action_11
action_128 (55) = happyGoto action_12
action_128 (56) = happyGoto action_13
action_128 (57) = happyGoto action_14
action_128 (58) = happyGoto action_15
action_128 (59) = happyGoto action_16
action_128 (60) = happyGoto action_17
action_128 (61) = happyGoto action_18
action_128 (63) = happyGoto action_19
action_128 (64) = happyGoto action_20
action_128 (67) = happyGoto action_21
action_128 (68) = happyGoto action_22
action_128 (71) = happyGoto action_23
action_128 (72) = happyGoto action_24
action_128 (75) = happyGoto action_25
action_128 (76) = happyGoto action_26
action_128 (77) = happyGoto action_27
action_128 (78) = happyGoto action_28
action_128 (79) = happyGoto action_29
action_128 (80) = happyGoto action_30
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_201

action_130 (101) = happyShift action_31
action_130 (103) = happyShift action_32
action_130 (105) = happyShift action_33
action_130 (110) = happyShift action_34
action_130 (111) = happyShift action_35
action_130 (112) = happyShift action_36
action_130 (113) = happyShift action_37
action_130 (120) = happyShift action_38
action_130 (127) = happyShift action_39
action_130 (128) = happyShift action_40
action_130 (138) = happyShift action_41
action_130 (140) = happyShift action_42
action_130 (141) = happyShift action_43
action_130 (144) = happyShift action_44
action_130 (145) = happyShift action_45
action_130 (146) = happyShift action_46
action_130 (147) = happyShift action_47
action_130 (51) = happyGoto action_146
action_130 (52) = happyGoto action_9
action_130 (53) = happyGoto action_10
action_130 (54) = happyGoto action_11
action_130 (55) = happyGoto action_12
action_130 (56) = happyGoto action_13
action_130 (57) = happyGoto action_14
action_130 (58) = happyGoto action_15
action_130 (59) = happyGoto action_16
action_130 (60) = happyGoto action_17
action_130 (61) = happyGoto action_18
action_130 (63) = happyGoto action_19
action_130 (64) = happyGoto action_20
action_130 (67) = happyGoto action_21
action_130 (68) = happyGoto action_22
action_130 (71) = happyGoto action_23
action_130 (72) = happyGoto action_24
action_130 (75) = happyGoto action_25
action_130 (76) = happyGoto action_26
action_130 (77) = happyGoto action_27
action_130 (78) = happyGoto action_28
action_130 (79) = happyGoto action_29
action_130 (80) = happyGoto action_30
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (101) = happyShift action_54
action_131 (103) = happyShift action_55
action_131 (105) = happyShift action_56
action_131 (107) = happyShift action_57
action_131 (141) = happyShift action_58
action_131 (142) = happyShift action_59
action_131 (146) = happyShift action_60
action_131 (82) = happyGoto action_145
action_131 (83) = happyGoto action_50
action_131 (84) = happyGoto action_51
action_131 (85) = happyGoto action_52
action_131 (86) = happyGoto action_53
action_131 _ = happyFail (happyExpListPerState 131)

action_132 _ = happyReduce_115

action_133 (101) = happyShift action_31
action_133 (103) = happyShift action_32
action_133 (105) = happyShift action_33
action_133 (110) = happyShift action_34
action_133 (111) = happyShift action_35
action_133 (112) = happyShift action_36
action_133 (113) = happyShift action_37
action_133 (120) = happyShift action_38
action_133 (127) = happyShift action_39
action_133 (128) = happyShift action_40
action_133 (138) = happyShift action_41
action_133 (140) = happyShift action_42
action_133 (141) = happyShift action_43
action_133 (144) = happyShift action_44
action_133 (145) = happyShift action_45
action_133 (146) = happyShift action_46
action_133 (147) = happyShift action_47
action_133 (51) = happyGoto action_144
action_133 (52) = happyGoto action_9
action_133 (53) = happyGoto action_10
action_133 (54) = happyGoto action_11
action_133 (55) = happyGoto action_12
action_133 (56) = happyGoto action_13
action_133 (57) = happyGoto action_14
action_133 (58) = happyGoto action_15
action_133 (59) = happyGoto action_16
action_133 (60) = happyGoto action_17
action_133 (61) = happyGoto action_18
action_133 (63) = happyGoto action_19
action_133 (64) = happyGoto action_20
action_133 (67) = happyGoto action_21
action_133 (68) = happyGoto action_22
action_133 (71) = happyGoto action_23
action_133 (72) = happyGoto action_24
action_133 (75) = happyGoto action_25
action_133 (76) = happyGoto action_26
action_133 (77) = happyGoto action_27
action_133 (78) = happyGoto action_28
action_133 (79) = happyGoto action_29
action_133 (80) = happyGoto action_30
action_133 _ = happyFail (happyExpListPerState 133)

action_134 _ = happyReduce_6

action_135 (101) = happyShift action_143
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (113) = happyShift action_142
action_136 _ = happyReduce_23

action_137 (120) = happyShift action_141
action_137 _ = happyReduce_24

action_138 _ = happyReduce_26

action_139 _ = happyReduce_8

action_140 _ = happyReduce_3

action_141 (141) = happyShift action_258
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (141) = happyShift action_138
action_142 (17) = happyGoto action_257
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (101) = happyShift action_253
action_143 (119) = happyShift action_254
action_143 (141) = happyShift action_255
action_143 (142) = happyShift action_256
action_143 (18) = happyGoto action_249
action_143 (19) = happyGoto action_250
action_143 (20) = happyGoto action_251
action_143 (21) = happyGoto action_252
action_143 _ = happyFail (happyExpListPerState 143)

action_144 _ = happyReduce_113

action_145 _ = happyReduce_112

action_146 _ = happyReduce_121

action_147 _ = happyReduce_120

action_148 _ = happyReduce_141

action_149 _ = happyReduce_140

action_150 _ = happyReduce_142

action_151 (101) = happyShift action_31
action_151 (103) = happyShift action_32
action_151 (105) = happyShift action_33
action_151 (110) = happyShift action_34
action_151 (111) = happyShift action_35
action_151 (112) = happyShift action_36
action_151 (113) = happyShift action_37
action_151 (120) = happyShift action_38
action_151 (127) = happyShift action_39
action_151 (128) = happyShift action_40
action_151 (138) = happyShift action_41
action_151 (140) = happyShift action_42
action_151 (141) = happyShift action_43
action_151 (144) = happyShift action_44
action_151 (145) = happyShift action_45
action_151 (146) = happyShift action_46
action_151 (147) = happyShift action_47
action_151 (51) = happyGoto action_115
action_151 (52) = happyGoto action_9
action_151 (53) = happyGoto action_10
action_151 (54) = happyGoto action_11
action_151 (55) = happyGoto action_12
action_151 (56) = happyGoto action_13
action_151 (57) = happyGoto action_14
action_151 (58) = happyGoto action_15
action_151 (59) = happyGoto action_16
action_151 (60) = happyGoto action_17
action_151 (61) = happyGoto action_18
action_151 (62) = happyGoto action_248
action_151 (63) = happyGoto action_19
action_151 (64) = happyGoto action_20
action_151 (67) = happyGoto action_21
action_151 (68) = happyGoto action_22
action_151 (71) = happyGoto action_23
action_151 (72) = happyGoto action_24
action_151 (75) = happyGoto action_25
action_151 (76) = happyGoto action_26
action_151 (77) = happyGoto action_27
action_151 (78) = happyGoto action_28
action_151 (79) = happyGoto action_29
action_151 (80) = happyGoto action_30
action_151 _ = happyFail (happyExpListPerState 151)

action_152 _ = happyReduce_152

action_153 (101) = happyShift action_31
action_153 (103) = happyShift action_32
action_153 (105) = happyShift action_33
action_153 (110) = happyShift action_34
action_153 (111) = happyShift action_35
action_153 (112) = happyShift action_36
action_153 (113) = happyShift action_37
action_153 (120) = happyShift action_38
action_153 (127) = happyShift action_39
action_153 (128) = happyShift action_40
action_153 (138) = happyShift action_41
action_153 (140) = happyShift action_42
action_153 (141) = happyShift action_43
action_153 (144) = happyShift action_44
action_153 (145) = happyShift action_45
action_153 (146) = happyShift action_46
action_153 (147) = happyShift action_47
action_153 (51) = happyGoto action_247
action_153 (52) = happyGoto action_9
action_153 (53) = happyGoto action_10
action_153 (54) = happyGoto action_11
action_153 (55) = happyGoto action_12
action_153 (56) = happyGoto action_13
action_153 (57) = happyGoto action_14
action_153 (58) = happyGoto action_15
action_153 (59) = happyGoto action_16
action_153 (60) = happyGoto action_17
action_153 (61) = happyGoto action_18
action_153 (63) = happyGoto action_19
action_153 (64) = happyGoto action_20
action_153 (67) = happyGoto action_21
action_153 (68) = happyGoto action_22
action_153 (71) = happyGoto action_23
action_153 (72) = happyGoto action_24
action_153 (75) = happyGoto action_25
action_153 (76) = happyGoto action_26
action_153 (77) = happyGoto action_27
action_153 (78) = happyGoto action_28
action_153 (79) = happyGoto action_29
action_153 (80) = happyGoto action_30
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (101) = happyShift action_31
action_154 (103) = happyShift action_32
action_154 (105) = happyShift action_33
action_154 (110) = happyShift action_34
action_154 (111) = happyShift action_35
action_154 (112) = happyShift action_36
action_154 (113) = happyShift action_37
action_154 (120) = happyShift action_38
action_154 (127) = happyShift action_39
action_154 (128) = happyShift action_40
action_154 (138) = happyShift action_41
action_154 (140) = happyShift action_42
action_154 (141) = happyShift action_43
action_154 (144) = happyShift action_44
action_154 (145) = happyShift action_45
action_154 (146) = happyShift action_46
action_154 (147) = happyShift action_47
action_154 (51) = happyGoto action_246
action_154 (52) = happyGoto action_9
action_154 (53) = happyGoto action_10
action_154 (54) = happyGoto action_11
action_154 (55) = happyGoto action_12
action_154 (56) = happyGoto action_13
action_154 (57) = happyGoto action_14
action_154 (58) = happyGoto action_15
action_154 (59) = happyGoto action_16
action_154 (60) = happyGoto action_17
action_154 (61) = happyGoto action_18
action_154 (63) = happyGoto action_19
action_154 (64) = happyGoto action_20
action_154 (67) = happyGoto action_21
action_154 (68) = happyGoto action_22
action_154 (71) = happyGoto action_23
action_154 (72) = happyGoto action_24
action_154 (75) = happyGoto action_25
action_154 (76) = happyGoto action_26
action_154 (77) = happyGoto action_27
action_154 (78) = happyGoto action_28
action_154 (79) = happyGoto action_29
action_154 (80) = happyGoto action_30
action_154 _ = happyFail (happyExpListPerState 154)

action_155 _ = happyReduce_147

action_156 (141) = happyShift action_245
action_156 (66) = happyGoto action_244
action_156 _ = happyFail (happyExpListPerState 156)

action_157 _ = happyReduce_146

action_158 (101) = happyShift action_31
action_158 (103) = happyShift action_32
action_158 (105) = happyShift action_33
action_158 (110) = happyShift action_34
action_158 (111) = happyShift action_35
action_158 (112) = happyShift action_36
action_158 (113) = happyShift action_37
action_158 (120) = happyShift action_38
action_158 (127) = happyShift action_39
action_158 (128) = happyShift action_40
action_158 (138) = happyShift action_41
action_158 (140) = happyShift action_42
action_158 (141) = happyShift action_43
action_158 (144) = happyShift action_44
action_158 (145) = happyShift action_45
action_158 (146) = happyShift action_46
action_158 (147) = happyShift action_47
action_158 (51) = happyGoto action_243
action_158 (52) = happyGoto action_9
action_158 (53) = happyGoto action_10
action_158 (54) = happyGoto action_11
action_158 (55) = happyGoto action_12
action_158 (56) = happyGoto action_13
action_158 (57) = happyGoto action_14
action_158 (58) = happyGoto action_15
action_158 (59) = happyGoto action_16
action_158 (60) = happyGoto action_17
action_158 (61) = happyGoto action_18
action_158 (63) = happyGoto action_19
action_158 (64) = happyGoto action_20
action_158 (67) = happyGoto action_21
action_158 (68) = happyGoto action_22
action_158 (71) = happyGoto action_23
action_158 (72) = happyGoto action_24
action_158 (75) = happyGoto action_25
action_158 (76) = happyGoto action_26
action_158 (77) = happyGoto action_27
action_158 (78) = happyGoto action_28
action_158 (79) = happyGoto action_29
action_158 (80) = happyGoto action_30
action_158 _ = happyFail (happyExpListPerState 158)

action_159 _ = happyReduce_215

action_160 (102) = happyShift action_241
action_160 (109) = happyShift action_242
action_160 _ = happyFail (happyExpListPerState 160)

action_161 _ = happyReduce_161

action_162 (114) = happyShift action_240
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (101) = happyShift action_31
action_163 (103) = happyShift action_32
action_163 (105) = happyShift action_33
action_163 (110) = happyShift action_34
action_163 (111) = happyShift action_35
action_163 (112) = happyShift action_36
action_163 (113) = happyShift action_37
action_163 (120) = happyShift action_38
action_163 (127) = happyShift action_39
action_163 (128) = happyShift action_40
action_163 (138) = happyShift action_41
action_163 (140) = happyShift action_42
action_163 (141) = happyShift action_43
action_163 (144) = happyShift action_44
action_163 (145) = happyShift action_45
action_163 (146) = happyShift action_46
action_163 (147) = happyShift action_47
action_163 (51) = happyGoto action_239
action_163 (52) = happyGoto action_9
action_163 (53) = happyGoto action_10
action_163 (54) = happyGoto action_11
action_163 (55) = happyGoto action_12
action_163 (56) = happyGoto action_13
action_163 (57) = happyGoto action_14
action_163 (58) = happyGoto action_15
action_163 (59) = happyGoto action_16
action_163 (60) = happyGoto action_17
action_163 (61) = happyGoto action_18
action_163 (63) = happyGoto action_19
action_163 (64) = happyGoto action_20
action_163 (67) = happyGoto action_21
action_163 (68) = happyGoto action_22
action_163 (71) = happyGoto action_23
action_163 (72) = happyGoto action_24
action_163 (75) = happyGoto action_25
action_163 (76) = happyGoto action_26
action_163 (77) = happyGoto action_27
action_163 (78) = happyGoto action_28
action_163 (79) = happyGoto action_29
action_163 (80) = happyGoto action_30
action_163 _ = happyFail (happyExpListPerState 163)

action_164 (101) = happyShift action_31
action_164 (103) = happyShift action_32
action_164 (105) = happyShift action_33
action_164 (110) = happyShift action_34
action_164 (111) = happyShift action_35
action_164 (112) = happyShift action_36
action_164 (113) = happyShift action_37
action_164 (120) = happyShift action_38
action_164 (127) = happyShift action_39
action_164 (128) = happyShift action_40
action_164 (138) = happyShift action_41
action_164 (140) = happyShift action_42
action_164 (141) = happyShift action_43
action_164 (144) = happyShift action_44
action_164 (145) = happyShift action_45
action_164 (146) = happyShift action_46
action_164 (147) = happyShift action_47
action_164 (51) = happyGoto action_238
action_164 (52) = happyGoto action_9
action_164 (53) = happyGoto action_10
action_164 (54) = happyGoto action_11
action_164 (55) = happyGoto action_12
action_164 (56) = happyGoto action_13
action_164 (57) = happyGoto action_14
action_164 (58) = happyGoto action_15
action_164 (59) = happyGoto action_16
action_164 (60) = happyGoto action_17
action_164 (61) = happyGoto action_18
action_164 (63) = happyGoto action_19
action_164 (64) = happyGoto action_20
action_164 (67) = happyGoto action_21
action_164 (68) = happyGoto action_22
action_164 (71) = happyGoto action_23
action_164 (72) = happyGoto action_24
action_164 (75) = happyGoto action_25
action_164 (76) = happyGoto action_26
action_164 (77) = happyGoto action_27
action_164 (78) = happyGoto action_28
action_164 (79) = happyGoto action_29
action_164 (80) = happyGoto action_30
action_164 _ = happyFail (happyExpListPerState 164)

action_165 _ = happyReduce_157

action_166 (99) = happyShift action_236
action_166 (100) = happyShift action_237
action_166 _ = happyFail (happyExpListPerState 166)

action_167 _ = happyReduce_154

action_168 (118) = happyShift action_235
action_168 _ = happyReduce_164

action_169 (151) = happyShift action_234
action_169 _ = happyFail (happyExpListPerState 169)

action_170 (150) = happyShift action_233
action_170 _ = happyFail (happyExpListPerState 170)

action_171 _ = happyReduce_174

action_172 _ = happyReduce_179

action_173 _ = happyReduce_185

action_174 (101) = happyShift action_54
action_174 (103) = happyShift action_55
action_174 (105) = happyShift action_56
action_174 (107) = happyShift action_57
action_174 (141) = happyShift action_58
action_174 (142) = happyShift action_59
action_174 (146) = happyShift action_60
action_174 (82) = happyGoto action_231
action_174 (83) = happyGoto action_50
action_174 (84) = happyGoto action_51
action_174 (85) = happyGoto action_52
action_174 (86) = happyGoto action_53
action_174 (87) = happyGoto action_232
action_174 _ = happyFail (happyExpListPerState 174)

action_175 _ = happyReduce_187

action_176 _ = happyReduce_188

action_177 _ = happyReduce_180

action_178 (101) = happyShift action_227
action_178 (107) = happyShift action_121
action_178 (108) = happyShift action_122
action_178 (113) = happyShift action_129
action_178 (119) = happyShift action_124
action_178 (141) = happyShift action_228
action_178 (143) = happyShift action_126
action_178 (43) = happyGoto action_230
action_178 (44) = happyGoto action_225
action_178 (91) = happyGoto action_226
action_178 _ = happyFail (happyExpListPerState 178)

action_179 (101) = happyShift action_227
action_179 (107) = happyShift action_121
action_179 (108) = happyShift action_122
action_179 (113) = happyShift action_129
action_179 (119) = happyShift action_124
action_179 (141) = happyShift action_228
action_179 (143) = happyShift action_126
action_179 (43) = happyGoto action_229
action_179 (44) = happyGoto action_225
action_179 (91) = happyGoto action_226
action_179 _ = happyFail (happyExpListPerState 179)

action_180 (101) = happyShift action_227
action_180 (107) = happyShift action_121
action_180 (108) = happyShift action_122
action_180 (113) = happyShift action_129
action_180 (119) = happyShift action_124
action_180 (141) = happyShift action_228
action_180 (143) = happyShift action_126
action_180 (43) = happyGoto action_224
action_180 (44) = happyGoto action_225
action_180 (91) = happyGoto action_226
action_180 _ = happyFail (happyExpListPerState 180)

action_181 (101) = happyShift action_54
action_181 (103) = happyShift action_55
action_181 (105) = happyShift action_56
action_181 (141) = happyShift action_58
action_181 (142) = happyShift action_59
action_181 (146) = happyShift action_60
action_181 (86) = happyGoto action_222
action_181 (88) = happyGoto action_223
action_181 _ = happyFail (happyExpListPerState 181)

action_182 (125) = happyShift action_221
action_182 _ = happyReduce_72

action_183 (101) = happyShift action_54
action_183 (103) = happyShift action_55
action_183 (105) = happyShift action_56
action_183 (117) = happyShift action_220
action_183 (141) = happyShift action_58
action_183 (142) = happyShift action_59
action_183 (146) = happyShift action_60
action_183 (86) = happyGoto action_95
action_183 _ = happyReduce_75

action_184 (101) = happyShift action_54
action_184 (102) = happyShift action_94
action_184 (103) = happyShift action_55
action_184 (105) = happyShift action_56
action_184 (107) = happyShift action_57
action_184 (141) = happyShift action_58
action_184 (142) = happyShift action_219
action_184 (146) = happyShift action_60
action_184 (36) = happyGoto action_217
action_184 (82) = happyGoto action_93
action_184 (83) = happyGoto action_50
action_184 (84) = happyGoto action_51
action_184 (85) = happyGoto action_52
action_184 (86) = happyGoto action_53
action_184 (90) = happyGoto action_218
action_184 _ = happyFail (happyExpListPerState 184)

action_185 (142) = happyShift action_216
action_185 _ = happyFail (happyExpListPerState 185)

action_186 (117) = happyShift action_215
action_186 (30) = happyGoto action_214
action_186 _ = happyReduce_63

action_187 (124) = happyShift action_213
action_187 (46) = happyGoto action_212
action_187 _ = happyReduce_98

action_188 _ = happyReduce_60

action_189 _ = happyReduce_59

action_190 (101) = happyShift action_211
action_190 (23) = happyGoto action_210
action_190 _ = happyReduce_37

action_191 (102) = happyShift action_209
action_191 _ = happyFail (happyExpListPerState 191)

action_192 (102) = happyShift action_208
action_192 _ = happyFail (happyExpListPerState 192)

action_193 (114) = happyShift action_205
action_193 (115) = happyShift action_206
action_193 (141) = happyShift action_207
action_193 _ = happyFail (happyExpListPerState 193)

action_194 (114) = happyShift action_203
action_194 (125) = happyShift action_204
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (142) = happyShift action_202
action_195 _ = happyFail (happyExpListPerState 195)

action_196 (117) = happyShift action_201
action_196 (30) = happyGoto action_200
action_196 _ = happyReduce_63

action_197 _ = happyReduce_10

action_198 (101) = happyShift action_76
action_198 (122) = happyShift action_77
action_198 (123) = happyShift action_78
action_198 (129) = happyShift action_79
action_198 (130) = happyShift action_80
action_198 (131) = happyShift action_81
action_198 (132) = happyShift action_82
action_198 (133) = happyShift action_83
action_198 (134) = happyShift action_84
action_198 (135) = happyShift action_85
action_198 (136) = happyShift action_86
action_198 (137) = happyShift action_87
action_198 (141) = happyShift action_88
action_198 (13) = happyGoto action_199
action_198 (14) = happyGoto action_66
action_198 (22) = happyGoto action_67
action_198 (26) = happyGoto action_68
action_198 (27) = happyGoto action_69
action_198 (34) = happyGoto action_70
action_198 (39) = happyGoto action_71
action_198 (42) = happyGoto action_72
action_198 (45) = happyGoto action_73
action_198 (92) = happyGoto action_74
action_198 _ = happyFail (happyExpListPerState 198)

action_199 _ = happyReduce_13

action_200 (101) = happyShift action_283
action_200 (141) = happyShift action_285
action_200 _ = happyReduce_61

action_201 (101) = happyShift action_195
action_201 (142) = happyShift action_282
action_201 (29) = happyGoto action_301
action_201 _ = happyFail (happyExpListPerState 201)

action_202 (30) = happyGoto action_300
action_202 _ = happyReduce_63

action_203 (141) = happyShift action_298
action_203 (142) = happyShift action_299
action_203 _ = happyFail (happyExpListPerState 203)

action_204 (98) = happyShift action_297
action_204 _ = happyFail (happyExpListPerState 204)

action_205 (101) = happyShift action_31
action_205 (103) = happyShift action_32
action_205 (105) = happyShift action_33
action_205 (110) = happyShift action_34
action_205 (111) = happyShift action_35
action_205 (112) = happyShift action_36
action_205 (113) = happyShift action_37
action_205 (120) = happyShift action_38
action_205 (127) = happyShift action_39
action_205 (128) = happyShift action_40
action_205 (138) = happyShift action_41
action_205 (140) = happyShift action_42
action_205 (141) = happyShift action_43
action_205 (144) = happyShift action_44
action_205 (145) = happyShift action_45
action_205 (146) = happyShift action_46
action_205 (147) = happyShift action_47
action_205 (51) = happyGoto action_296
action_205 (52) = happyGoto action_9
action_205 (53) = happyGoto action_10
action_205 (54) = happyGoto action_11
action_205 (55) = happyGoto action_12
action_205 (56) = happyGoto action_13
action_205 (57) = happyGoto action_14
action_205 (58) = happyGoto action_15
action_205 (59) = happyGoto action_16
action_205 (60) = happyGoto action_17
action_205 (61) = happyGoto action_18
action_205 (63) = happyGoto action_19
action_205 (64) = happyGoto action_20
action_205 (67) = happyGoto action_21
action_205 (68) = happyGoto action_22
action_205 (71) = happyGoto action_23
action_205 (72) = happyGoto action_24
action_205 (75) = happyGoto action_25
action_205 (76) = happyGoto action_26
action_205 (77) = happyGoto action_27
action_205 (78) = happyGoto action_28
action_205 (79) = happyGoto action_29
action_205 (80) = happyGoto action_30
action_205 _ = happyFail (happyExpListPerState 205)

action_206 (101) = happyShift action_54
action_206 (103) = happyShift action_55
action_206 (105) = happyShift action_56
action_206 (107) = happyShift action_57
action_206 (141) = happyShift action_58
action_206 (142) = happyShift action_59
action_206 (146) = happyShift action_60
action_206 (82) = happyGoto action_294
action_206 (83) = happyGoto action_50
action_206 (84) = happyGoto action_51
action_206 (85) = happyGoto action_52
action_206 (86) = happyGoto action_53
action_206 (89) = happyGoto action_295
action_206 _ = happyFail (happyExpListPerState 206)

action_207 _ = happyReduce_213

action_208 _ = happyReduce_206

action_209 _ = happyReduce_205

action_210 _ = happyReduce_36

action_211 (101) = happyShift action_291
action_211 (141) = happyShift action_292
action_211 (142) = happyShift action_293
action_211 (24) = happyGoto action_289
action_211 (25) = happyGoto action_290
action_211 _ = happyFail (happyExpListPerState 211)

action_212 (101) = happyShift action_287
action_212 (125) = happyShift action_288
action_212 _ = happyFail (happyExpListPerState 212)

action_213 (146) = happyShift action_286
action_213 _ = happyFail (happyExpListPerState 213)

action_214 (101) = happyShift action_283
action_214 (114) = happyShift action_284
action_214 (141) = happyShift action_285
action_214 _ = happyReduce_49

action_215 (101) = happyShift action_195
action_215 (142) = happyShift action_282
action_215 (29) = happyGoto action_281
action_215 _ = happyFail (happyExpListPerState 215)

action_216 (30) = happyGoto action_280
action_216 _ = happyReduce_63

action_217 (102) = happyShift action_278
action_217 (109) = happyShift action_279
action_217 _ = happyFail (happyExpListPerState 217)

action_218 _ = happyReduce_76

action_219 (101) = happyShift action_54
action_219 (103) = happyShift action_55
action_219 (105) = happyShift action_56
action_219 (141) = happyShift action_58
action_219 (142) = happyShift action_59
action_219 (146) = happyShift action_60
action_219 (86) = happyGoto action_222
action_219 (88) = happyGoto action_277
action_219 _ = happyReduce_189

action_220 (101) = happyShift action_54
action_220 (103) = happyShift action_55
action_220 (105) = happyShift action_56
action_220 (141) = happyShift action_58
action_220 (142) = happyShift action_59
action_220 (146) = happyShift action_60
action_220 (85) = happyGoto action_276
action_220 (86) = happyGoto action_53
action_220 _ = happyFail (happyExpListPerState 220)

action_221 (98) = happyShift action_275
action_221 _ = happyFail (happyExpListPerState 221)

action_222 _ = happyReduce_194

action_223 (101) = happyShift action_54
action_223 (103) = happyShift action_55
action_223 (105) = happyShift action_56
action_223 (125) = happyShift action_274
action_223 (141) = happyShift action_58
action_223 (142) = happyShift action_59
action_223 (146) = happyShift action_60
action_223 (86) = happyGoto action_273
action_223 _ = happyReduce_82

action_224 (109) = happyShift action_270
action_224 _ = happyReduce_87

action_225 _ = happyReduce_90

action_226 _ = happyReduce_94

action_227 (107) = happyShift action_121
action_227 (108) = happyShift action_122
action_227 (113) = happyShift action_129
action_227 (119) = happyShift action_124
action_227 (120) = happyShift action_272
action_227 (143) = happyShift action_126
action_227 (91) = happyGoto action_271
action_227 _ = happyFail (happyExpListPerState 227)

action_228 _ = happyReduce_95

action_229 (109) = happyShift action_270
action_229 _ = happyReduce_88

action_230 (109) = happyShift action_270
action_230 _ = happyReduce_89

action_231 _ = happyReduce_192

action_232 (102) = happyShift action_268
action_232 (109) = happyShift action_269
action_232 _ = happyFail (happyExpListPerState 232)

action_233 (101) = happyShift action_31
action_233 (103) = happyShift action_32
action_233 (105) = happyShift action_33
action_233 (110) = happyShift action_34
action_233 (111) = happyShift action_35
action_233 (112) = happyShift action_36
action_233 (113) = happyShift action_37
action_233 (120) = happyShift action_38
action_233 (127) = happyShift action_39
action_233 (128) = happyShift action_40
action_233 (138) = happyShift action_41
action_233 (140) = happyShift action_42
action_233 (141) = happyShift action_43
action_233 (144) = happyShift action_44
action_233 (145) = happyShift action_45
action_233 (146) = happyShift action_46
action_233 (147) = happyShift action_47
action_233 (51) = happyGoto action_267
action_233 (52) = happyGoto action_9
action_233 (53) = happyGoto action_10
action_233 (54) = happyGoto action_11
action_233 (55) = happyGoto action_12
action_233 (56) = happyGoto action_13
action_233 (57) = happyGoto action_14
action_233 (58) = happyGoto action_15
action_233 (59) = happyGoto action_16
action_233 (60) = happyGoto action_17
action_233 (61) = happyGoto action_18
action_233 (63) = happyGoto action_19
action_233 (64) = happyGoto action_20
action_233 (67) = happyGoto action_21
action_233 (68) = happyGoto action_22
action_233 (71) = happyGoto action_23
action_233 (72) = happyGoto action_24
action_233 (75) = happyGoto action_25
action_233 (76) = happyGoto action_26
action_233 (77) = happyGoto action_27
action_233 (78) = happyGoto action_28
action_233 (79) = happyGoto action_29
action_233 (80) = happyGoto action_30
action_233 _ = happyFail (happyExpListPerState 233)

action_234 _ = happyReduce_175

action_235 (101) = happyShift action_31
action_235 (103) = happyShift action_32
action_235 (105) = happyShift action_33
action_235 (110) = happyShift action_34
action_235 (111) = happyShift action_35
action_235 (112) = happyShift action_36
action_235 (113) = happyShift action_37
action_235 (120) = happyShift action_38
action_235 (127) = happyShift action_39
action_235 (128) = happyShift action_40
action_235 (138) = happyShift action_41
action_235 (140) = happyShift action_42
action_235 (141) = happyShift action_43
action_235 (144) = happyShift action_44
action_235 (145) = happyShift action_45
action_235 (146) = happyShift action_46
action_235 (147) = happyShift action_47
action_235 (51) = happyGoto action_266
action_235 (52) = happyGoto action_9
action_235 (53) = happyGoto action_10
action_235 (54) = happyGoto action_11
action_235 (55) = happyGoto action_12
action_235 (56) = happyGoto action_13
action_235 (57) = happyGoto action_14
action_235 (58) = happyGoto action_15
action_235 (59) = happyGoto action_16
action_235 (60) = happyGoto action_17
action_235 (61) = happyGoto action_18
action_235 (63) = happyGoto action_19
action_235 (64) = happyGoto action_20
action_235 (67) = happyGoto action_21
action_235 (68) = happyGoto action_22
action_235 (71) = happyGoto action_23
action_235 (72) = happyGoto action_24
action_235 (75) = happyGoto action_25
action_235 (76) = happyGoto action_26
action_235 (77) = happyGoto action_27
action_235 (78) = happyGoto action_28
action_235 (79) = happyGoto action_29
action_235 (80) = happyGoto action_30
action_235 _ = happyFail (happyExpListPerState 235)

action_236 _ = happyReduce_153

action_237 (101) = happyShift action_31
action_237 (103) = happyShift action_32
action_237 (105) = happyShift action_33
action_237 (110) = happyShift action_34
action_237 (111) = happyShift action_35
action_237 (112) = happyShift action_36
action_237 (113) = happyShift action_37
action_237 (120) = happyShift action_38
action_237 (127) = happyShift action_39
action_237 (128) = happyShift action_40
action_237 (138) = happyShift action_41
action_237 (140) = happyShift action_42
action_237 (141) = happyShift action_168
action_237 (144) = happyShift action_44
action_237 (145) = happyShift action_45
action_237 (146) = happyShift action_46
action_237 (147) = happyShift action_47
action_237 (51) = happyGoto action_165
action_237 (52) = happyGoto action_9
action_237 (53) = happyGoto action_10
action_237 (54) = happyGoto action_11
action_237 (55) = happyGoto action_12
action_237 (56) = happyGoto action_13
action_237 (57) = happyGoto action_14
action_237 (58) = happyGoto action_15
action_237 (59) = happyGoto action_16
action_237 (60) = happyGoto action_17
action_237 (61) = happyGoto action_18
action_237 (63) = happyGoto action_19
action_237 (64) = happyGoto action_20
action_237 (67) = happyGoto action_21
action_237 (68) = happyGoto action_22
action_237 (70) = happyGoto action_265
action_237 (71) = happyGoto action_23
action_237 (72) = happyGoto action_24
action_237 (75) = happyGoto action_25
action_237 (76) = happyGoto action_26
action_237 (77) = happyGoto action_27
action_237 (78) = happyGoto action_28
action_237 (79) = happyGoto action_29
action_237 (80) = happyGoto action_30
action_237 _ = happyFail (happyExpListPerState 237)

action_238 _ = happyReduce_117

action_239 _ = happyReduce_116

action_240 (101) = happyShift action_31
action_240 (103) = happyShift action_32
action_240 (105) = happyShift action_33
action_240 (110) = happyShift action_34
action_240 (111) = happyShift action_35
action_240 (112) = happyShift action_36
action_240 (113) = happyShift action_37
action_240 (120) = happyShift action_38
action_240 (127) = happyShift action_39
action_240 (128) = happyShift action_40
action_240 (138) = happyShift action_41
action_240 (140) = happyShift action_42
action_240 (141) = happyShift action_43
action_240 (144) = happyShift action_44
action_240 (145) = happyShift action_45
action_240 (146) = happyShift action_46
action_240 (147) = happyShift action_47
action_240 (51) = happyGoto action_264
action_240 (52) = happyGoto action_9
action_240 (53) = happyGoto action_10
action_240 (54) = happyGoto action_11
action_240 (55) = happyGoto action_12
action_240 (56) = happyGoto action_13
action_240 (57) = happyGoto action_14
action_240 (58) = happyGoto action_15
action_240 (59) = happyGoto action_16
action_240 (60) = happyGoto action_17
action_240 (61) = happyGoto action_18
action_240 (63) = happyGoto action_19
action_240 (64) = happyGoto action_20
action_240 (67) = happyGoto action_21
action_240 (68) = happyGoto action_22
action_240 (71) = happyGoto action_23
action_240 (72) = happyGoto action_24
action_240 (75) = happyGoto action_25
action_240 (76) = happyGoto action_26
action_240 (77) = happyGoto action_27
action_240 (78) = happyGoto action_28
action_240 (79) = happyGoto action_29
action_240 (80) = happyGoto action_30
action_240 _ = happyFail (happyExpListPerState 240)

action_241 _ = happyReduce_160

action_242 (141) = happyShift action_162
action_242 (74) = happyGoto action_263
action_242 _ = happyFail (happyExpListPerState 242)

action_243 _ = happyReduce_118

action_244 _ = happyReduce_149

action_245 (114) = happyShift action_154
action_245 _ = happyFail (happyExpListPerState 245)

action_246 _ = happyReduce_150

action_247 _ = happyReduce_145

action_248 (102) = happyShift action_262
action_248 (109) = happyShift action_153
action_248 _ = happyFail (happyExpListPerState 248)

action_249 (102) = happyShift action_261
action_249 _ = happyFail (happyExpListPerState 249)

action_250 (109) = happyShift action_260
action_250 _ = happyReduce_29

action_251 _ = happyReduce_30

action_252 _ = happyReduce_32

action_253 (107) = happyShift action_121
action_253 (108) = happyShift action_122
action_253 (113) = happyShift action_129
action_253 (119) = happyShift action_124
action_253 (143) = happyShift action_126
action_253 (91) = happyGoto action_259
action_253 _ = happyFail (happyExpListPerState 253)

action_254 _ = happyReduce_28

action_255 _ = happyReduce_33

action_256 _ = happyReduce_35

action_257 (120) = happyShift action_141
action_257 _ = happyReduce_25

action_258 _ = happyReduce_27

action_259 (102) = happyShift action_339
action_259 _ = happyFail (happyExpListPerState 259)

action_260 (101) = happyShift action_253
action_260 (141) = happyShift action_255
action_260 (142) = happyShift action_256
action_260 (20) = happyGoto action_338
action_260 (21) = happyGoto action_252
action_260 _ = happyFail (happyExpListPerState 260)

action_261 (98) = happyShift action_63
action_261 (11) = happyGoto action_337
action_261 _ = happyFail (happyExpListPerState 261)

action_262 _ = happyReduce_143

action_263 _ = happyReduce_162

action_264 _ = happyReduce_163

action_265 _ = happyReduce_155

action_266 _ = happyReduce_156

action_267 (151) = happyShift action_336
action_267 _ = happyFail (happyExpListPerState 267)

action_268 _ = happyReduce_186

action_269 (101) = happyShift action_54
action_269 (103) = happyShift action_55
action_269 (105) = happyShift action_56
action_269 (107) = happyShift action_57
action_269 (141) = happyShift action_58
action_269 (142) = happyShift action_59
action_269 (146) = happyShift action_60
action_269 (82) = happyGoto action_335
action_269 (83) = happyGoto action_50
action_269 (84) = happyGoto action_51
action_269 (85) = happyGoto action_52
action_269 (86) = happyGoto action_53
action_269 _ = happyFail (happyExpListPerState 269)

action_270 (101) = happyShift action_227
action_270 (107) = happyShift action_121
action_270 (108) = happyShift action_122
action_270 (113) = happyShift action_129
action_270 (119) = happyShift action_124
action_270 (141) = happyShift action_228
action_270 (143) = happyShift action_126
action_270 (44) = happyGoto action_334
action_270 (91) = happyGoto action_226
action_270 _ = happyFail (happyExpListPerState 270)

action_271 (102) = happyShift action_333
action_271 _ = happyFail (happyExpListPerState 271)

action_272 (102) = happyShift action_332
action_272 _ = happyFail (happyExpListPerState 272)

action_273 _ = happyReduce_195

action_274 (98) = happyShift action_331
action_274 _ = happyFail (happyExpListPerState 274)

action_275 (101) = happyShift action_76
action_275 (141) = happyShift action_88
action_275 (37) = happyGoto action_328
action_275 (38) = happyGoto action_329
action_275 (92) = happyGoto action_330
action_275 _ = happyFail (happyExpListPerState 275)

action_276 (101) = happyShift action_54
action_276 (103) = happyShift action_55
action_276 (105) = happyShift action_56
action_276 (141) = happyShift action_58
action_276 (142) = happyShift action_59
action_276 (146) = happyShift action_60
action_276 (86) = happyGoto action_95
action_276 _ = happyReduce_73

action_277 (101) = happyShift action_54
action_277 (103) = happyShift action_55
action_277 (105) = happyShift action_56
action_277 (141) = happyShift action_58
action_277 (142) = happyShift action_59
action_277 (146) = happyShift action_60
action_277 (86) = happyGoto action_273
action_277 _ = happyReduce_198

action_278 (117) = happyShift action_327
action_278 _ = happyFail (happyExpListPerState 278)

action_279 (142) = happyShift action_326
action_279 (90) = happyGoto action_325
action_279 _ = happyFail (happyExpListPerState 279)

action_280 (101) = happyShift action_283
action_280 (102) = happyShift action_324
action_280 (141) = happyShift action_285
action_280 _ = happyFail (happyExpListPerState 280)

action_281 (114) = happyShift action_323
action_281 _ = happyFail (happyExpListPerState 281)

action_282 (30) = happyGoto action_200
action_282 _ = happyReduce_63

action_283 (101) = happyShift action_54
action_283 (103) = happyShift action_55
action_283 (105) = happyShift action_56
action_283 (107) = happyShift action_57
action_283 (141) = happyShift action_58
action_283 (142) = happyShift action_59
action_283 (146) = happyShift action_60
action_283 (82) = happyGoto action_322
action_283 (83) = happyGoto action_50
action_283 (84) = happyGoto action_51
action_283 (85) = happyGoto action_52
action_283 (86) = happyGoto action_53
action_283 _ = happyFail (happyExpListPerState 283)

action_284 (101) = happyShift action_54
action_284 (103) = happyShift action_55
action_284 (105) = happyShift action_56
action_284 (107) = happyShift action_57
action_284 (141) = happyShift action_58
action_284 (142) = happyShift action_59
action_284 (146) = happyShift action_60
action_284 (82) = happyGoto action_321
action_284 (83) = happyGoto action_50
action_284 (84) = happyGoto action_51
action_284 (85) = happyGoto action_52
action_284 (86) = happyGoto action_53
action_284 _ = happyFail (happyExpListPerState 284)

action_285 _ = happyReduce_64

action_286 _ = happyReduce_99

action_287 (146) = happyShift action_320
action_287 (47) = happyGoto action_318
action_287 (48) = happyGoto action_319
action_287 _ = happyFail (happyExpListPerState 287)

action_288 (98) = happyShift action_317
action_288 _ = happyFail (happyExpListPerState 288)

action_289 (102) = happyShift action_315
action_289 (109) = happyShift action_316
action_289 _ = happyFail (happyExpListPerState 289)

action_290 _ = happyReduce_39

action_291 (107) = happyShift action_121
action_291 (108) = happyShift action_122
action_291 (113) = happyShift action_129
action_291 (119) = happyShift action_124
action_291 (143) = happyShift action_126
action_291 (91) = happyGoto action_314
action_291 _ = happyFail (happyExpListPerState 291)

action_292 (126) = happyShift action_313
action_292 _ = happyReduce_41

action_293 (126) = happyShift action_312
action_293 _ = happyReduce_45

action_294 (117) = happyShift action_311
action_294 _ = happyReduce_197

action_295 _ = happyReduce_21

action_296 (125) = happyShift action_310
action_296 (93) = happyGoto action_309
action_296 _ = happyReduce_207

action_297 (141) = happyShift action_308
action_297 (31) = happyGoto action_306
action_297 (32) = happyGoto action_307
action_297 _ = happyFail (happyExpListPerState 297)

action_298 (105) = happyShift action_305
action_298 _ = happyFail (happyExpListPerState 298)

action_299 (105) = happyShift action_304
action_299 _ = happyFail (happyExpListPerState 299)

action_300 (101) = happyShift action_283
action_300 (102) = happyShift action_303
action_300 (141) = happyShift action_285
action_300 _ = happyFail (happyExpListPerState 300)

action_301 (114) = happyShift action_302
action_301 _ = happyFail (happyExpListPerState 301)

action_302 (142) = happyShift action_369
action_302 _ = happyFail (happyExpListPerState 302)

action_303 _ = happyReduce_62

action_304 (141) = happyShift action_308
action_304 (31) = happyGoto action_366
action_304 (33) = happyGoto action_368
action_304 _ = happyFail (happyExpListPerState 304)

action_305 (141) = happyShift action_308
action_305 (31) = happyGoto action_366
action_305 (33) = happyGoto action_367
action_305 _ = happyFail (happyExpListPerState 305)

action_306 _ = happyReduce_67

action_307 (99) = happyShift action_364
action_307 (100) = happyShift action_365
action_307 _ = happyFail (happyExpListPerState 307)

action_308 (115) = happyShift action_363
action_308 _ = happyFail (happyExpListPerState 308)

action_309 _ = happyReduce_22

action_310 (98) = happyShift action_362
action_310 _ = happyFail (happyExpListPerState 310)

action_311 (101) = happyShift action_54
action_311 (103) = happyShift action_55
action_311 (105) = happyShift action_56
action_311 (107) = happyShift action_57
action_311 (141) = happyShift action_58
action_311 (142) = happyShift action_59
action_311 (146) = happyShift action_60
action_311 (82) = happyGoto action_361
action_311 (83) = happyGoto action_50
action_311 (84) = happyGoto action_51
action_311 (85) = happyGoto action_52
action_311 (86) = happyGoto action_53
action_311 _ = happyFail (happyExpListPerState 311)

action_312 (142) = happyShift action_360
action_312 _ = happyFail (happyExpListPerState 312)

action_313 (141) = happyShift action_359
action_313 _ = happyFail (happyExpListPerState 313)

action_314 (102) = happyShift action_358
action_314 _ = happyFail (happyExpListPerState 314)

action_315 _ = happyReduce_38

action_316 (101) = happyShift action_291
action_316 (141) = happyShift action_292
action_316 (142) = happyShift action_293
action_316 (25) = happyGoto action_357
action_316 _ = happyFail (happyExpListPerState 316)

action_317 (141) = happyShift action_356
action_317 (49) = happyGoto action_354
action_317 (50) = happyGoto action_355
action_317 _ = happyFail (happyExpListPerState 317)

action_318 (102) = happyShift action_352
action_318 (109) = happyShift action_353
action_318 _ = happyFail (happyExpListPerState 318)

action_319 _ = happyReduce_100

action_320 (126) = happyShift action_351
action_320 _ = happyReduce_102

action_321 _ = happyReduce_48

action_322 (102) = happyShift action_350
action_322 _ = happyFail (happyExpListPerState 322)

action_323 (101) = happyShift action_54
action_323 (103) = happyShift action_55
action_323 (105) = happyShift action_56
action_323 (107) = happyShift action_57
action_323 (141) = happyShift action_58
action_323 (142) = happyShift action_59
action_323 (146) = happyShift action_60
action_323 (82) = happyGoto action_349
action_323 (83) = happyGoto action_50
action_323 (84) = happyGoto action_51
action_323 (85) = happyGoto action_52
action_323 (86) = happyGoto action_53
action_323 _ = happyFail (happyExpListPerState 323)

action_324 (114) = happyShift action_348
action_324 _ = happyReduce_51

action_325 _ = happyReduce_77

action_326 (101) = happyShift action_54
action_326 (103) = happyShift action_55
action_326 (105) = happyShift action_56
action_326 (141) = happyShift action_58
action_326 (142) = happyShift action_59
action_326 (146) = happyShift action_60
action_326 (86) = happyGoto action_222
action_326 (88) = happyGoto action_277
action_326 _ = happyFail (happyExpListPerState 326)

action_327 (101) = happyShift action_54
action_327 (103) = happyShift action_55
action_327 (105) = happyShift action_56
action_327 (141) = happyShift action_58
action_327 (142) = happyShift action_59
action_327 (146) = happyShift action_60
action_327 (85) = happyGoto action_347
action_327 (86) = happyGoto action_53
action_327 _ = happyFail (happyExpListPerState 327)

action_328 (99) = happyShift action_345
action_328 (100) = happyShift action_346
action_328 _ = happyFail (happyExpListPerState 328)

action_329 _ = happyReduce_78

action_330 (96) = happyGoto action_344
action_330 _ = happyReduce_212

action_331 (101) = happyShift action_76
action_331 (123) = happyShift action_78
action_331 (141) = happyShift action_88
action_331 (14) = happyGoto action_340
action_331 (40) = happyGoto action_341
action_331 (41) = happyGoto action_342
action_331 (45) = happyGoto action_343
action_331 (92) = happyGoto action_74
action_331 _ = happyFail (happyExpListPerState 331)

action_332 _ = happyReduce_93

action_333 _ = happyReduce_92

action_334 _ = happyReduce_91

action_335 _ = happyReduce_193

action_336 _ = happyReduce_176

action_337 _ = happyReduce_9

action_338 _ = happyReduce_31

action_339 _ = happyReduce_34

action_340 _ = happyReduce_86

action_341 (99) = happyShift action_388
action_341 (100) = happyShift action_389
action_341 _ = happyFail (happyExpListPerState 341)

action_342 _ = happyReduce_83

action_343 _ = happyReduce_85

action_344 (115) = happyShift action_387
action_344 (141) = happyShift action_207
action_344 _ = happyFail (happyExpListPerState 344)

action_345 _ = happyReduce_71

action_346 (101) = happyShift action_76
action_346 (141) = happyShift action_88
action_346 (38) = happyGoto action_386
action_346 (92) = happyGoto action_330
action_346 _ = happyFail (happyExpListPerState 346)

action_347 (101) = happyShift action_54
action_347 (103) = happyShift action_55
action_347 (105) = happyShift action_56
action_347 (141) = happyShift action_58
action_347 (142) = happyShift action_59
action_347 (146) = happyShift action_60
action_347 (86) = happyGoto action_95
action_347 _ = happyReduce_74

action_348 (101) = happyShift action_54
action_348 (103) = happyShift action_55
action_348 (105) = happyShift action_56
action_348 (107) = happyShift action_57
action_348 (141) = happyShift action_58
action_348 (142) = happyShift action_59
action_348 (146) = happyShift action_60
action_348 (82) = happyGoto action_385
action_348 (83) = happyGoto action_50
action_348 (84) = happyGoto action_51
action_348 (85) = happyGoto action_52
action_348 (86) = happyGoto action_53
action_348 _ = happyFail (happyExpListPerState 348)

action_349 _ = happyReduce_47

action_350 _ = happyReduce_65

action_351 (101) = happyShift action_383
action_351 (141) = happyShift action_384
action_351 _ = happyFail (happyExpListPerState 351)

action_352 _ = happyReduce_96

action_353 (146) = happyShift action_320
action_353 (48) = happyGoto action_382
action_353 _ = happyFail (happyExpListPerState 353)

action_354 (99) = happyShift action_380
action_354 (100) = happyShift action_381
action_354 _ = happyFail (happyExpListPerState 354)

action_355 _ = happyReduce_106

action_356 _ = happyReduce_108

action_357 _ = happyReduce_40

action_358 (126) = happyShift action_379
action_358 _ = happyReduce_44

action_359 _ = happyReduce_42

action_360 _ = happyReduce_46

action_361 _ = happyReduce_196

action_362 (101) = happyShift action_76
action_362 (141) = happyShift action_88
action_362 (14) = happyGoto action_376
action_362 (92) = happyGoto action_74
action_362 (94) = happyGoto action_377
action_362 (95) = happyGoto action_378
action_362 _ = happyFail (happyExpListPerState 362)

action_363 (101) = happyShift action_54
action_363 (103) = happyShift action_55
action_363 (105) = happyShift action_56
action_363 (107) = happyShift action_57
action_363 (141) = happyShift action_58
action_363 (142) = happyShift action_59
action_363 (146) = happyShift action_60
action_363 (82) = happyGoto action_375
action_363 (83) = happyGoto action_50
action_363 (84) = happyGoto action_51
action_363 (85) = happyGoto action_52
action_363 (86) = happyGoto action_53
action_363 _ = happyFail (happyExpListPerState 363)

action_364 _ = happyReduce_52

action_365 (141) = happyShift action_308
action_365 (31) = happyGoto action_374
action_365 _ = happyFail (happyExpListPerState 365)

action_366 _ = happyReduce_69

action_367 (106) = happyShift action_373
action_367 (109) = happyShift action_372
action_367 _ = happyFail (happyExpListPerState 367)

action_368 (106) = happyShift action_371
action_368 (109) = happyShift action_372
action_368 _ = happyFail (happyExpListPerState 368)

action_369 (105) = happyShift action_370
action_369 _ = happyFail (happyExpListPerState 369)

action_370 (141) = happyShift action_308
action_370 (31) = happyGoto action_366
action_370 (33) = happyGoto action_399
action_370 _ = happyFail (happyExpListPerState 370)

action_371 _ = happyReduce_55

action_372 (141) = happyShift action_308
action_372 (31) = happyGoto action_398
action_372 _ = happyFail (happyExpListPerState 372)

action_373 _ = happyReduce_53

action_374 _ = happyReduce_68

action_375 _ = happyReduce_66

action_376 _ = happyReduce_211

action_377 (99) = happyShift action_396
action_377 (100) = happyShift action_397
action_377 _ = happyFail (happyExpListPerState 377)

action_378 _ = happyReduce_209

action_379 (141) = happyShift action_395
action_379 _ = happyFail (happyExpListPerState 379)

action_380 _ = happyReduce_97

action_381 (141) = happyShift action_356
action_381 (50) = happyGoto action_394
action_381 _ = happyFail (happyExpListPerState 381)

action_382 _ = happyReduce_101

action_383 (107) = happyShift action_121
action_383 (108) = happyShift action_122
action_383 (113) = happyShift action_129
action_383 (119) = happyShift action_124
action_383 (120) = happyShift action_393
action_383 (143) = happyShift action_126
action_383 (91) = happyGoto action_392
action_383 _ = happyFail (happyExpListPerState 383)

action_384 _ = happyReduce_103

action_385 _ = happyReduce_50

action_386 _ = happyReduce_79

action_387 (101) = happyShift action_54
action_387 (103) = happyShift action_55
action_387 (105) = happyShift action_56
action_387 (107) = happyShift action_57
action_387 (141) = happyShift action_58
action_387 (142) = happyShift action_59
action_387 (146) = happyShift action_60
action_387 (82) = happyGoto action_294
action_387 (83) = happyGoto action_50
action_387 (84) = happyGoto action_51
action_387 (85) = happyGoto action_52
action_387 (86) = happyGoto action_53
action_387 (89) = happyGoto action_391
action_387 _ = happyFail (happyExpListPerState 387)

action_388 _ = happyReduce_81

action_389 (101) = happyShift action_76
action_389 (123) = happyShift action_78
action_389 (141) = happyShift action_88
action_389 (14) = happyGoto action_340
action_389 (41) = happyGoto action_390
action_389 (45) = happyGoto action_343
action_389 (92) = happyGoto action_74
action_389 _ = happyFail (happyExpListPerState 389)

action_390 _ = happyReduce_84

action_391 _ = happyReduce_80

action_392 (102) = happyShift action_403
action_392 _ = happyFail (happyExpListPerState 392)

action_393 (102) = happyShift action_402
action_393 _ = happyFail (happyExpListPerState 393)

action_394 _ = happyReduce_107

action_395 _ = happyReduce_43

action_396 _ = happyReduce_208

action_397 (101) = happyShift action_76
action_397 (141) = happyShift action_88
action_397 (14) = happyGoto action_376
action_397 (92) = happyGoto action_74
action_397 (95) = happyGoto action_401
action_397 _ = happyFail (happyExpListPerState 397)

action_398 _ = happyReduce_70

action_399 (106) = happyShift action_400
action_399 (109) = happyShift action_372
action_399 _ = happyFail (happyExpListPerState 399)

action_400 _ = happyReduce_54

action_401 _ = happyReduce_210

action_402 _ = happyReduce_105

action_403 _ = happyReduce_104

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyMonadReduce 2 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkImplicitMain happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  8 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  9 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  9 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happyMonadReduce 6 10 happyReduction_9
happyReduction_9 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { i <- freshId happy_var_1
            ; expI <- freshExprI happy_var_1 (ExpE happy_var_4)
            ; return (ExprI i (ModE (MV happy_var_2) (expI : happy_var_6))) }))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_10 = happySpecReduce_3  11 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  11 happyReduction_11
happyReduction_11 _
	_
	 =  HappyAbsSyn6
		 ([]
	)

happyReduce_12 = happySpecReduce_1  12 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  12 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  13 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyMonadReduce 4 14 happyReduction_21
happyReduction_21 ((HappyAbsSyn89  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { let (cs, t) = happy_var_4
            ; let t' = forallWrap (map TV happy_var_2) t
            ; let et = EType t' (Set.fromList cs) (ArgDocSig defaultValue [] defaultValue)
            ; e <- freshExprI happy_var_1 (SigE (Signature (toEVar happy_var_1) Nothing et))
            ; return [e] }))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_22 = happyMonadReduce 5 14 happyReduction_22
happyReduction_22 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { e <- case happy_var_2 of
                [] -> freshExprI happy_var_1 (AssE (toEVar happy_var_1) happy_var_4 happy_var_5)
                vs -> do { lam <- freshExprI happy_var_3 (LamE (map EV vs) happy_var_4)
                         ; freshExprI happy_var_1 (AssE (toEVar happy_var_1) lam happy_var_5) }
            ; return [e] }))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_23 = happySpecReduce_1  15 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 (T.intercalate "." happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  16 happyReduction_24
happyReduction_24 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  16 happyReduction_25
happyReduction_25 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  17 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (getName happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  17 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 <> "-" <> getName happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  18 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn18
		 (ExportAll
	)

happyReduce_29 = happySpecReduce_1  18 happyReduction_29
happyReduction_29 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (ExportMany (Set.fromList happy_var_1) []
	)
happyReduction_29 _  = notHappyAtAll

happyReduce_30 = happySpecReduce_1  19 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  19 happyReduction_31
happyReduction_31 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happyMonadReduce 1 20 happyReduction_32
happyReduction_32 ((HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { i <- freshId happy_var_1; return (i, symVal happy_var_1) }))
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_33 = happySpecReduce_1  21 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  21 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  21 happyReduction_35
happyReduction_35 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happyMonadReduce 3 22 happyReduction_36
happyReduction_36 ((HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (ImpE (Import (MV happy_var_2) happy_var_3 [] Nothing))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_37 = happySpecReduce_0  23 happyReduction_37
happyReduction_37  =  HappyAbsSyn23
		 (Nothing
	)

happyReduce_38 = happySpecReduce_3  23 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (Just happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  24 happyReduction_39
happyReduction_39 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  24 happyReduction_40
happyReduction_40 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  25 happyReduction_41
happyReduction_41 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (AliasedTerm (EV (getName happy_var_1)) (EV (getName happy_var_1))
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  25 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (AliasedTerm (EV (getName happy_var_1)) (EV (getName happy_var_3))
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 5 25 happyReduction_43
happyReduction_43 ((HappyTerminal happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (AliasedTerm (EV (getOp happy_var_2)) (EV (getName happy_var_5))
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_3  25 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (AliasedTerm (EV (getOp happy_var_2)) (EV (getOp happy_var_2))
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  25 happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (AliasedType (TV (getName happy_var_1)) (TV (getName happy_var_1))
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  25 happyReduction_46
happyReduction_46 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn25
		 (AliasedType (TV (getName happy_var_1)) (TV (getName happy_var_3))
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happyMonadReduce 6 26 happyReduction_47
happyReduction_47 ((HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { lang <- parseLang (getName happy_var_2)
            ; mkTypedef happy_var_1 (Just (lang, False)) happy_var_4 happy_var_6 }))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_48 = happyMonadReduce 5 26 happyReduction_48
happyReduction_48 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkTypedef happy_var_1 Nothing (TV (getName happy_var_2), happy_var_3) happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_49 = happyMonadReduce 3 26 happyReduction_49
happyReduction_49 ((HappyAbsSyn30  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkTypedefAlias happy_var_1 (TV (getName happy_var_2), happy_var_3)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_50 = happyMonadReduce 7 26 happyReduction_50
happyReduction_50 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkTypedef happy_var_1 Nothing (TV (getName happy_var_3), happy_var_4) happy_var_7))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_51 = happyMonadReduce 5 26 happyReduction_51
happyReduction_51 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkTypedefAlias happy_var_1 (TV (getName happy_var_3), happy_var_4)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_52 = happyMonadReduce 6 26 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn32  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkNamTypeWhere happy_var_3 happy_var_1 happy_var_2 happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_53 = happyMonadReduce 7 26 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn32  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkNamTypeLegacy happy_var_3 happy_var_1 happy_var_2 (Just (getName happy_var_4)) Nothing happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_54 = happyMonadReduce 9 26 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn32  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_6) `HappyStk`
	(HappyTerminal happy_var_5) `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { lang <- parseLang (getName happy_var_2)
            ; mkNamTypeLegacy happy_var_5 happy_var_1 happy_var_4 (Just (getName happy_var_6)) (Just (lang, False)) happy_var_8 }))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_55 = happyMonadReduce 7 26 happyReduction_55
happyReduction_55 (_ `HappyStk`
	(HappyAbsSyn32  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	(HappyAbsSyn27  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkNamTypeLegacy happy_var_3 happy_var_1 happy_var_2 (Just (getName happy_var_4)) Nothing happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_56 = happySpecReduce_1  27 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn27
		 (NamRecord
	)

happyReduce_57 = happySpecReduce_1  27 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn27
		 (NamObject
	)

happyReduce_58 = happySpecReduce_1  27 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn27
		 (NamTable
	)

happyReduce_59 = happyMonadReduce 1 28 happyReduction_59
happyReduction_59 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( parseLang (getName happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn28 r))

happyReduce_60 = happyMonadReduce 1 28 happyReduction_60
happyReduction_60 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( parseLang (getName happy_var_1)))
	) (\r -> happyReturn (HappyAbsSyn28 r))

happyReduce_61 = happySpecReduce_2  29 happyReduction_61
happyReduction_61 (HappyAbsSyn30  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 ((TV (getName happy_var_1), happy_var_2)
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happyReduce 4 29 happyReduction_62
happyReduction_62 (_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 ((TV (getName happy_var_2), happy_var_3)
	) `HappyStk` happyRest

happyReduce_63 = happySpecReduce_0  30 happyReduction_63
happyReduction_63  =  HappyAbsSyn30
		 ([]
	)

happyReduce_64 = happySpecReduce_2  30 happyReduction_64
happyReduction_64 (HappyTerminal happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1 ++ [Left (TV (getName happy_var_2))]
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happyReduce 4 30 happyReduction_65
happyReduction_65 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn30
		 (happy_var_1 ++ [Right happy_var_3]
	) `HappyStk` happyRest

happyReduce_66 = happySpecReduce_3  31 happyReduction_66
happyReduction_66 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn31
		 ((Key (getName happy_var_1), happy_var_3)
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  32 happyReduction_67
happyReduction_67 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn32
		 ([happy_var_1]
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  32 happyReduction_68
happyReduction_68 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  33 happyReduction_69
happyReduction_69 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn32
		 ([happy_var_1]
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  33 happyReduction_70
happyReduction_70 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happyMonadReduce 6 34 happyReduction_71
happyReduction_71 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn35  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { let (cs, cn, vs) = happy_var_2
            ; freshExprI happy_var_1 (ClsE (Typeclass cs cn vs happy_var_5)) }))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_72 = happyMonadReduce 2 34 happyReduction_72
happyReduction_72 ((HappyAbsSyn35  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { let (cs, cn, vs) = happy_var_2
            ; freshExprI happy_var_1 (ClsE (Typeclass cs cn vs [])) }))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_73 = happyMonadReduce 3 35 happyReduction_73
happyReduction_73 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { cs <- extractConstraints happy_var_1
            ; (cn, vs) <- extractClassDef happy_var_3
            ; return (cs, cn, vs) }))
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_74 = happyMonadReduce 5 35 happyReduction_74
happyReduction_74 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn36  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( do { (cn, vs) <- extractClassDef happy_var_5
            ; return (happy_var_2, cn, vs) }))
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_75 = happyMonadReduce 1 35 happyReduction_75
happyReduction_75 ((HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { (cn, vs) <- extractClassDef happy_var_1
            ; return ([], cn, vs) }))
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_76 = happySpecReduce_1  36 happyReduction_76
happyReduction_76 (HappyAbsSyn90  happy_var_1)
	 =  HappyAbsSyn36
		 ([happy_var_1]
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  36 happyReduction_77
happyReduction_77 (HappyAbsSyn90  happy_var_3)
	_
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  37 happyReduction_78
happyReduction_78 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn37
		 ([happy_var_1]
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  37 happyReduction_79
happyReduction_79 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happyMonadReduce 4 38 happyReduction_80
happyReduction_80 ((HappyAbsSyn89  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( let (cs, t) = happy_var_4 in mkSignature happy_var_1 happy_var_2 cs t))
	) (\r -> happyReturn (HappyAbsSyn38 r))

happyReduce_81 = happyMonadReduce 7 39 happyReduction_81
happyReduction_81 (_ `HappyStk`
	(HappyAbsSyn40  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn87  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (IstE (ClassName (getName happy_var_2)) happy_var_3 (concat happy_var_6))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_82 = happyMonadReduce 3 39 happyReduction_82
happyReduction_82 ((HappyAbsSyn87  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (IstE (ClassName (getName happy_var_2)) happy_var_3 [])))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_83 = happySpecReduce_1  40 happyReduction_83
happyReduction_83 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  40 happyReduction_84
happyReduction_84 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  41 happyReduction_85
happyReduction_85 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  41 happyReduction_86
happyReduction_86 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happyMonadReduce 3 42 happyReduction_87
happyReduction_87 ((HappyAbsSyn43  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (FixE (Fixity InfixL (fromInteger (getInt happy_var_2)) happy_var_3))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_88 = happyMonadReduce 3 42 happyReduction_88
happyReduction_88 ((HappyAbsSyn43  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (FixE (Fixity InfixR (fromInteger (getInt happy_var_2)) happy_var_3))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_89 = happyMonadReduce 3 42 happyReduction_89
happyReduction_89 ((HappyAbsSyn43  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (FixE (Fixity InfixN (fromInteger (getInt happy_var_2)) happy_var_3))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_90 = happySpecReduce_1  43 happyReduction_90
happyReduction_90 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn43
		 ([happy_var_1]
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  43 happyReduction_91
happyReduction_91 (HappyAbsSyn44  happy_var_3)
	_
	(HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  44 happyReduction_92
happyReduction_92 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn44
		 (EV (getOp happy_var_2)
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  44 happyReduction_93
happyReduction_93 _
	_
	_
	 =  HappyAbsSyn44
		 (EV "-"
	)

happyReduce_94 = happySpecReduce_1  44 happyReduction_94
happyReduction_94 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn44
		 (EV (getOp happy_var_1)
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_1  44 happyReduction_95
happyReduction_95 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn44
		 (EV (getName happy_var_1)
	)
happyReduction_95 _  = notHappyAtAll 

happyReduce_96 = happyMonadReduce 6 45 happyReduction_96
happyReduction_96 (_ `HappyStk`
	(HappyAbsSyn47  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mapM (mkSource happy_var_1 happy_var_2 happy_var_3) happy_var_5))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_97 = happyMonadReduce 7 45 happyReduction_97
happyReduction_97 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mapM (mkSourceNew happy_var_1 happy_var_2 happy_var_3) happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_98 = happySpecReduce_0  46 happyReduction_98
happyReduction_98  =  HappyAbsSyn46
		 (Nothing
	)

happyReduce_99 = happySpecReduce_2  46 happyReduction_99
happyReduction_99 (HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Just (getString happy_var_2)
	)
happyReduction_99 _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  47 happyReduction_100
happyReduction_100 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn47
		 ([happy_var_1]
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_3  47 happyReduction_101
happyReduction_101 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_1  48 happyReduction_102
happyReduction_102 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn48
		 ((getString happy_var_1, Nothing)
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_3  48 happyReduction_103
happyReduction_103 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn48
		 ((getString happy_var_1, Just (getName happy_var_3))
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyReduce_104 = happyReduce 5 48 happyReduction_104
happyReduction_104 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 ((getString happy_var_1, Just (getOp happy_var_4))
	) `HappyStk` happyRest

happyReduce_105 = happyReduce 5 48 happyReduction_105
happyReduction_105 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 ((getString happy_var_1, Just "-")
	) `HappyStk` happyRest

happyReduce_106 = happySpecReduce_1  49 happyReduction_106
happyReduction_106 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  49 happyReduction_107
happyReduction_107 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_107 _ _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  50 happyReduction_108
happyReduction_108 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (getName happy_var_1
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1  51 happyReduction_109
happyReduction_109 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_1  51 happyReduction_110
happyReduction_110 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_110 _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  51 happyReduction_111
happyReduction_111 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happyMonadReduce 3 51 happyReduction_112
happyReduction_112 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_2 (AnnE happy_var_1 happy_var_3)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_113 = happyMonadReduce 3 52 happyReduction_113
happyReduction_113 ((HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn53  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_2 (LetE happy_var_1 happy_var_3)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_114 = happySpecReduce_1  53 happyReduction_114
happyReduction_114 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn53
		 ([happy_var_1]
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_2  53 happyReduction_115
happyReduction_115 (HappyAbsSyn54  happy_var_2)
	(HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn53
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_115 _ _  = notHappyAtAll 

happyReduce_116 = happyReduce 4 54 happyReduction_116
happyReduction_116 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 ((EV (getName happy_var_2), happy_var_4)
	) `HappyStk` happyRest

happyReduce_117 = happyReduce 4 54 happyReduction_117
happyReduction_117 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn54
		 ((EV "_", happy_var_4)
	) `HappyStk` happyRest

happyReduce_118 = happyMonadReduce 4 55 happyReduction_118
happyReduction_118 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (LamE (map EV happy_var_2) happy_var_4)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_119 = happySpecReduce_1  56 happyReduction_119
happyReduction_119 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happyMonadReduce 3 56 happyReduction_120
happyReduction_120 ((HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { opI <- freshId happy_var_2
            ; freshExprI happy_var_2 (BopE happy_var_1 opI (EV (getOp happy_var_2)) happy_var_3) }))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_121 = happyMonadReduce 3 56 happyReduction_121
happyReduction_121 ((HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { opI <- freshId happy_var_2
            ; freshExprI happy_var_2 (BopE happy_var_1 opI (EV "-") happy_var_3) }))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_122 = happySpecReduce_1  57 happyReduction_122
happyReduction_122 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_1  58 happyReduction_123
happyReduction_123 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happyMonadReduce 2 58 happyReduction_124
happyReduction_124 ((HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkApp happy_var_1 happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_125 = happySpecReduce_1  59 happyReduction_125
happyReduction_125 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  59 happyReduction_126
happyReduction_126 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_1  59 happyReduction_127
happyReduction_127 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_127 _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1  59 happyReduction_128
happyReduction_128 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_1  59 happyReduction_129
happyReduction_129 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_1  59 happyReduction_130
happyReduction_130 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  59 happyReduction_131
happyReduction_131 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_1  59 happyReduction_132
happyReduction_132 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_132 _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_1  59 happyReduction_133
happyReduction_133 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_133 _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_1  59 happyReduction_134
happyReduction_134 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_1  59 happyReduction_135
happyReduction_135 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_135 _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_1  59 happyReduction_136
happyReduction_136 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_136 _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_1  59 happyReduction_137
happyReduction_137 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_137 _  = notHappyAtAll 

happyReduce_138 = happyMonadReduce 2 60 happyReduction_138
happyReduction_138 ((HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (ForceE happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_139 = happyMonadReduce 2 61 happyReduction_139
happyReduction_139 (_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 UniE))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_140 = happyMonadReduce 3 61 happyReduction_140
happyReduction_140 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (VarE defaultValue (EV (getOp happy_var_2)))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_141 = happyMonadReduce 3 61 happyReduction_141
happyReduction_141 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (VarE defaultValue (EV "-"))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_142 = happySpecReduce_3  61 happyReduction_142
happyReduction_142 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_142 _ _ _  = notHappyAtAll 

happyReduce_143 = happyMonadReduce 5 61 happyReduction_143
happyReduction_143 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (TupE (happy_var_2 : happy_var_4))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_144 = happySpecReduce_1  62 happyReduction_144
happyReduction_144 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_144 _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_3  62 happyReduction_145
happyReduction_145 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_145 _ _ _  = notHappyAtAll 

happyReduce_146 = happyMonadReduce 3 63 happyReduction_146
happyReduction_146 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (SuspendE happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_147 = happyMonadReduce 3 64 happyReduction_147
happyReduction_147 (_ `HappyStk`
	(HappyAbsSyn65  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (NamE happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_148 = happySpecReduce_1  65 happyReduction_148
happyReduction_148 (HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn65
		 ([happy_var_1]
	)
happyReduction_148 _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_3  65 happyReduction_149
happyReduction_149 (HappyAbsSyn66  happy_var_3)
	_
	(HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn65
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_149 _ _ _  = notHappyAtAll 

happyReduce_150 = happySpecReduce_3  66 happyReduction_150
happyReduction_150 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn66
		 ((Key (getName happy_var_1), happy_var_3)
	)
happyReduction_150 _ _ _  = notHappyAtAll 

happyReduce_151 = happyMonadReduce 2 67 happyReduction_151
happyReduction_151 (_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (LstE [])))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_152 = happyMonadReduce 3 67 happyReduction_152
happyReduction_152 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (LstE happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_153 = happyMonadReduce 4 68 happyReduction_153
happyReduction_153 (_ `HappyStk`
	(HappyAbsSyn69  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( desugarDo happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_154 = happySpecReduce_1  69 happyReduction_154
happyReduction_154 (HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn69
		 ([happy_var_1]
	)
happyReduction_154 _  = notHappyAtAll 

happyReduce_155 = happySpecReduce_3  69 happyReduction_155
happyReduction_155 (HappyAbsSyn70  happy_var_3)
	_
	(HappyAbsSyn69  happy_var_1)
	 =  HappyAbsSyn69
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_155 _ _ _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_3  70 happyReduction_156
happyReduction_156 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn70
		 (DoBind (EV (getName happy_var_1)) happy_var_3
	)
happyReduction_156 _ _ _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_1  70 happyReduction_157
happyReduction_157 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn70
		 (DoBare happy_var_1
	)
happyReduction_157 _  = notHappyAtAll 

happyReduce_158 = happyMonadReduce 2 71 happyReduction_158
happyReduction_158 ((HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (PatE (PatternStruct (SelectorKey (getName happy_var_2, SelectorEnd) [])))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_159 = happyMonadReduce 2 71 happyReduction_159
happyReduction_159 ((HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (PatE (PatternStruct (SelectorIdx (fromInteger (getInt happy_var_2), SelectorEnd) [])))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_160 = happyMonadReduce 4 72 happyReduction_160
happyReduction_160 (_ `HappyStk`
	(HappyAbsSyn73  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkSetter happy_var_1 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_161 = happySpecReduce_1  73 happyReduction_161
happyReduction_161 (HappyAbsSyn74  happy_var_1)
	 =  HappyAbsSyn73
		 ([happy_var_1]
	)
happyReduction_161 _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_3  73 happyReduction_162
happyReduction_162 (HappyAbsSyn74  happy_var_3)
	_
	(HappyAbsSyn73  happy_var_1)
	 =  HappyAbsSyn73
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_162 _ _ _  = notHappyAtAll 

happyReduce_163 = happySpecReduce_3  74 happyReduction_163
happyReduction_163 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn74
		 ((getName happy_var_1, happy_var_3)
	)
happyReduction_163 _ _ _  = notHappyAtAll 

happyReduce_164 = happyMonadReduce 1 75 happyReduction_164
happyReduction_164 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (VarE defaultValue (EV (getName happy_var_1)))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_165 = happyMonadReduce 1 76 happyReduction_165
happyReduction_165 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 HolE))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_166 = happyMonadReduce 1 77 happyReduction_166
happyReduction_166 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (LogE True)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_167 = happyMonadReduce 1 77 happyReduction_167
happyReduction_167 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (LogE False)))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_168 = happyMonadReduce 1 78 happyReduction_168
happyReduction_168 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (IntE (getInt happy_var_1))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_169 = happyMonadReduce 1 78 happyReduction_169
happyReduction_169 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (RealE (DS.fromFloatDigits (getFloat happy_var_1)))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_170 = happyMonadReduce 2 78 happyReduction_170
happyReduction_170 ((HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (IntE (negate (getInt happy_var_2)))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_171 = happyMonadReduce 2 78 happyReduction_171
happyReduction_171 ((HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (RealE (DS.fromFloatDigits (negate (getFloat happy_var_2))))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_172 = happyMonadReduce 1 79 happyReduction_172
happyReduction_172 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( freshExprI happy_var_1 (StrE (getString happy_var_1))))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_173 = happySpecReduce_1  79 happyReduction_173
happyReduction_173 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_173 _  = notHappyAtAll 

happyReduce_174 = happyMonadReduce 3 80 happyReduction_174
happyReduction_174 ((HappyTerminal happy_var_3) `HappyStk`
	(HappyAbsSyn81  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( mkInterpString happy_var_1 happy_var_2 happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_175 = happySpecReduce_3  81 happyReduction_175
happyReduction_175 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn81
		 (([happy_var_2], [])
	)
happyReduction_175 _ _ _  = notHappyAtAll 

happyReduce_176 = happyReduce 5 81 happyReduction_176
happyReduction_176 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn81  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn81
		 (let (es, ms) = happy_var_1 in (es ++ [happy_var_4], ms ++ [getString happy_var_2])
	) `HappyStk` happyRest

happyReduce_177 = happySpecReduce_1  82 happyReduction_177
happyReduction_177 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_177 _  = notHappyAtAll 

happyReduce_178 = happySpecReduce_1  82 happyReduction_178
happyReduction_178 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_178 _  = notHappyAtAll 

happyReduce_179 = happySpecReduce_3  83 happyReduction_179
happyReduction_179 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (case happy_var_3 of { FunU args ret -> FunU (happy_var_1 : args) ret; t -> FunU [happy_var_1] t }
	)
happyReduction_179 _ _ _  = notHappyAtAll 

happyReduce_180 = happySpecReduce_3  84 happyReduction_180
happyReduction_180 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ExistU (TV (getName happy_var_2)) ([], Open) ([], Open)
	)
happyReduction_180 _ _ _  = notHappyAtAll 

happyReduce_181 = happySpecReduce_1  84 happyReduction_181
happyReduction_181 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_181 _  = notHappyAtAll 

happyReduce_182 = happyMonadReduce 2 85 happyReduction_182
happyReduction_182 ((HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( return (applyType happy_var_1 happy_var_2)))
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_183 = happySpecReduce_1  85 happyReduction_183
happyReduction_183 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_183 _  = notHappyAtAll 

happyReduce_184 = happySpecReduce_2  86 happyReduction_184
happyReduction_184 _
	_
	 =  HappyAbsSyn7
		 (BT.unitU
	)

happyReduce_185 = happySpecReduce_3  86 happyReduction_185
happyReduction_185 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_185 _ _ _  = notHappyAtAll 

happyReduce_186 = happyReduce 5 86 happyReduction_186
happyReduction_186 (_ `HappyStk`
	(HappyAbsSyn87  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (BT.tupleU (happy_var_2 : happy_var_4)
	) `HappyStk` happyRest

happyReduce_187 = happySpecReduce_3  86 happyReduction_187
happyReduction_187 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (BT.listU happy_var_2
	)
happyReduction_187 _ _ _  = notHappyAtAll 

happyReduce_188 = happySpecReduce_3  86 happyReduction_188
happyReduction_188 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ThunkU happy_var_2
	)
happyReduction_188 _ _ _  = notHappyAtAll 

happyReduce_189 = happySpecReduce_1  86 happyReduction_189
happyReduction_189 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (VarU (TV (getName happy_var_1))
	)
happyReduction_189 _  = notHappyAtAll 

happyReduce_190 = happySpecReduce_1  86 happyReduction_190
happyReduction_190 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (VarU (TV (getName happy_var_1))
	)
happyReduction_190 _  = notHappyAtAll 

happyReduce_191 = happySpecReduce_1  86 happyReduction_191
happyReduction_191 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (VarU (TV (getString happy_var_1))
	)
happyReduction_191 _  = notHappyAtAll 

happyReduce_192 = happySpecReduce_1  87 happyReduction_192
happyReduction_192 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn87
		 ([happy_var_1]
	)
happyReduction_192 _  = notHappyAtAll 

happyReduce_193 = happySpecReduce_3  87 happyReduction_193
happyReduction_193 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn87
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_193 _ _ _  = notHappyAtAll 

happyReduce_194 = happySpecReduce_1  88 happyReduction_194
happyReduction_194 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn87
		 ([happy_var_1]
	)
happyReduction_194 _  = notHappyAtAll 

happyReduce_195 = happySpecReduce_2  88 happyReduction_195
happyReduction_195 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn87  happy_var_1)
	 =  HappyAbsSyn87
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_195 _ _  = notHappyAtAll 

happyReduce_196 = happyMonadReduce 3 89 happyReduction_196
happyReduction_196 ((HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((( do { cs <- extractConstraints happy_var_1; return (cs, happy_var_3) }))
	) (\r -> happyReturn (HappyAbsSyn89 r))

happyReduce_197 = happySpecReduce_1  89 happyReduction_197
happyReduction_197 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn89
		 (([], happy_var_1)
	)
happyReduction_197 _  = notHappyAtAll 

happyReduce_198 = happySpecReduce_2  90 happyReduction_198
happyReduction_198 (HappyAbsSyn87  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn90
		 (Constraint (ClassName (getName happy_var_1)) happy_var_2
	)
happyReduction_198 _ _  = notHappyAtAll 

happyReduce_199 = happySpecReduce_1  91 happyReduction_199
happyReduction_199 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_199 _  = notHappyAtAll 

happyReduce_200 = happySpecReduce_1  91 happyReduction_200
happyReduction_200 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_200 _  = notHappyAtAll 

happyReduce_201 = happySpecReduce_1  91 happyReduction_201
happyReduction_201 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_201 _  = notHappyAtAll 

happyReduce_202 = happySpecReduce_1  91 happyReduction_202
happyReduction_202 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_202 _  = notHappyAtAll 

happyReduce_203 = happySpecReduce_1  91 happyReduction_203
happyReduction_203 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_203 _  = notHappyAtAll 

happyReduce_204 = happySpecReduce_1  92 happyReduction_204
happyReduction_204 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_204 _  = notHappyAtAll 

happyReduce_205 = happySpecReduce_3  92 happyReduction_205
happyReduction_205 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2
	)
happyReduction_205 _ _ _  = notHappyAtAll 

happyReduce_206 = happySpecReduce_3  92 happyReduction_206
happyReduction_206 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2
	)
happyReduction_206 _ _ _  = notHappyAtAll 

happyReduce_207 = happySpecReduce_0  93 happyReduction_207
happyReduction_207  =  HappyAbsSyn6
		 ([]
	)

happyReduce_208 = happyReduce 4 93 happyReduction_208
happyReduction_208 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_209 = happySpecReduce_1  94 happyReduction_209
happyReduction_209 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_209 _  = notHappyAtAll 

happyReduce_210 = happySpecReduce_3  94 happyReduction_210
happyReduction_210 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_210 _ _ _  = notHappyAtAll 

happyReduce_211 = happySpecReduce_1  95 happyReduction_211
happyReduction_211 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_211 _  = notHappyAtAll 

happyReduce_212 = happySpecReduce_0  96 happyReduction_212
happyReduction_212  =  HappyAbsSyn16
		 ([]
	)

happyReduce_213 = happySpecReduce_2  96 happyReduction_213
happyReduction_213 (HappyTerminal happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [getName happy_var_2]
	)
happyReduction_213 _ _  = notHappyAtAll 

happyReduce_214 = happySpecReduce_1  97 happyReduction_214
happyReduction_214 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 ([getName happy_var_1]
	)
happyReduction_214 _  = notHappyAtAll 

happyReduce_215 = happySpecReduce_2  97 happyReduction_215
happyReduction_215 (HappyTerminal happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 ++ [getName happy_var_2]
	)
happyReduction_215 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 153 153 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Located _ TokVLBrace _ -> cont 98;
	Located _ TokVRBrace _ -> cont 99;
	Located _ TokVSemi _ -> cont 100;
	Located _ TokLParen _ -> cont 101;
	Located _ TokRParen _ -> cont 102;
	Located _ TokLBracket _ -> cont 103;
	Located _ TokRBracket _ -> cont 104;
	Located _ TokLBrace _ -> cont 105;
	Located _ TokRBrace _ -> cont 106;
	Located _ TokLAngle _ -> cont 107;
	Located _ TokRAngle _ -> cont 108;
	Located _ TokComma _ -> cont 109;
	Located _ TokBackslash _ -> cont 110;
	Located _ TokUnderscore _ -> cont 111;
	Located _ TokBang _ -> cont 112;
	Located _ TokDot _ -> cont 113;
	Located _ TokEquals _ -> cont 114;
	Located _ TokDColon _ -> cont 115;
	Located _ TokArrow _ -> cont 116;
	Located _ TokFatArrow _ -> cont 117;
	Located _ TokBind _ -> cont 118;
	Located _ TokStar _ -> cont 119;
	Located _ TokMinus _ -> cont 120;
	Located _ TokModule _ -> cont 121;
	Located _ TokImport _ -> cont 122;
	Located _ TokSource _ -> cont 123;
	Located _ TokFrom _ -> cont 124;
	Located _ TokWhere _ -> cont 125;
	Located _ TokAs _ -> cont 126;
	Located _ TokTrue _ -> cont 127;
	Located _ TokFalse _ -> cont 128;
	Located _ TokType _ -> cont 129;
	Located _ TokRecord _ -> cont 130;
	Located _ TokObject _ -> cont 131;
	Located _ TokTable _ -> cont 132;
	Located _ TokClass _ -> cont 133;
	Located _ TokInstance _ -> cont 134;
	Located _ TokInfixl _ -> cont 135;
	Located _ TokInfixr _ -> cont 136;
	Located _ TokInfix _ -> cont 137;
	Located _ TokLet _ -> cont 138;
	Located _ TokIn _ -> cont 139;
	Located _ TokDo _ -> cont 140;
	Located _ (TokLowerName _) _ -> cont 141;
	Located _ (TokUpperName _) _ -> cont 142;
	Located _ (TokOperator _) _ -> cont 143;
	Located _ (TokInteger _) _ -> cont 144;
	Located _ (TokFloat _) _ -> cont 145;
	Located _ (TokString _) _ -> cont 146;
	Located _ (TokStringStart _) _ -> cont 147;
	Located _ (TokStringMid _) _ -> cont 148;
	Located _ (TokStringEnd _) _ -> cont 149;
	Located _ TokInterpOpen _ -> cont 150;
	Located _ TokInterpClose _ -> cont 151;
	Located _ TokEOF _ -> cont 152;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 153 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = ((>>=))
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> P a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Located)], [Prelude.String]) -> P a
happyError' = (\(tokens, _) -> parseError tokens)
parseProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

parseTypeOnly tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

parseExprOnly tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


--------------------------------------------------------------------
-- Parser monad
--------------------------------------------------------------------

data ParseError = ParseError !Pos !String
  deriving (Show)

showParseError :: String -> ParseError -> String
showParseError filename (ParseError pos msg) =
  filename ++ ":" ++ show (posLine pos) ++ ":" ++ show (posCol pos)
  ++ ": " ++ msg

data PState = PState
  { psExpIndex    :: !Int
  , psSourceMap   :: !(Map.Map Int SrcLoc)
  , psModulePath  :: !(Maybe Path)
  , psModuleConfig :: !ModuleConfig
  , psLangMap :: !(Map.Map T.Text Lang) -- alias -> Lang for all known languages
  }
  deriving (Show)

emptyPState :: PState
emptyPState = PState 1 Map.empty Nothing defaultValue Map.empty

type P a = State.StateT PState (Either ParseError) a

--------------------------------------------------------------------
-- Token extraction helpers
--------------------------------------------------------------------

-- | Extract name from identifier tokens
getName :: Located -> Text
getName (Located _ (TokLowerName n) _) = n
getName (Located _ (TokUpperName n) _) = n
getName (Located _ _ t) = t

-- | Extract integer value
getInt :: Located -> Integer
getInt (Located _ (TokInteger n) _) = n
getInt _ = 0

-- | Extract float value
getFloat :: Located -> Double
getFloat (Located _ (TokFloat d) _) = d
getFloat _ = 0

-- | Extract string value
getString :: Located -> Text
getString (Located _ (TokString s) _) = s
getString (Located _ (TokStringStart s) _) = s
getString (Located _ (TokStringMid s) _) = s
getString (Located _ (TokStringEnd s) _) = s
getString (Located _ _ t) = t

-- | Extract operator text from operator-like tokens
getOp :: Located -> Text
getOp (Located _ (TokOperator t) _) = t
getOp (Located _ TokMinus _) = "-"
getOp (Located _ TokStar _) = "*"
getOp (Located _ TokDot _) = "."
getOp (Located _ TokLAngle _) = "<"
getOp (Located _ TokRAngle _) = ">"
getOp (Located _ _ t) = t

-- | Get the name from a Located token as a Symbol
symVal :: Located -> Symbol
symVal (Located _ (TokLowerName n) _) = TermSymbol (EV n)
symVal (Located _ (TokUpperName n) _) = TypeSymbol (TV n)
symVal (Located _ (TokOperator n) _) = TermSymbol (EV n)
symVal (Located _ TokMinus _) = TermSymbol (EV "-")
symVal (Located _ TokStar _) = TermSymbol (EV "*")
symVal (Located _ TokDot _) = TermSymbol (EV ".")
symVal (Located _ TokLAngle _) = TermSymbol (EV "<")
symVal (Located _ TokRAngle _) = TermSymbol (EV ">")
symVal _ = TermSymbol (EV "?")

-- | Convert Located to EVar
toEVar :: Located -> EVar
toEVar (Located _ (TokLowerName n) _) = EV n
toEVar (Located _ (TokOperator n) _) = EV n
toEVar (Located _ TokMinus _) = EV "-"
toEVar (Located _ TokStar _) = EV "*"
toEVar (Located _ TokDot _) = EV "."
toEVar (Located _ TokLAngle _) = EV "<"
toEVar (Located _ TokRAngle _) = EV ">"
toEVar _ = EV "?"

--------------------------------------------------------------------
-- Expression ID generation
--------------------------------------------------------------------

-- | Generate a fresh expression ID and record source position
freshId :: Located -> P Int
freshId tok = do
  s <- State.get
  let i = psExpIndex s
      p = locPos tok
      loc = SrcLoc (Just (posFile p)) (posLine p) (posCol p) (posLine p) (posCol p)
  State.put s { psExpIndex = i + 1
              , psSourceMap = Map.insert i loc (psSourceMap s) }
  return i

-- | Create a fresh ExprI with position from a Located token
freshExprI :: Located -> Expr -> P ExprI
freshExprI tok e = do
  i <- freshId tok
  return (ExprI i e)

-- | Create a fresh ExprI copying position from an existing ExprI
freshExprIFrom :: ExprI -> Expr -> P ExprI
freshExprIFrom (ExprI refId _) e = do
  s <- State.get
  let i = psExpIndex s
      loc = Map.findWithDefault noSrcLoc refId (psSourceMap s)
  State.put s { psExpIndex = i + 1
              , psSourceMap = Map.insert i loc (psSourceMap s) }
  return (ExprI i e)

noSrcLoc :: SrcLoc
noSrcLoc = SrcLoc Nothing 0 0 0 0

--------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------

-- | Wrap type variables in forall
forallWrap :: [TVar] -> TypeU -> TypeU
forallWrap [] t = t
forallWrap (v : vs) t = ForallU v (forallWrap vs t)

-- | Parse a language name, checking both built-in and registered plugin languages
parseLang :: Text -> P Lang
parseLang t = do
  langs <- State.gets psLangMap
  case Map.lookup (T.toLower t) langs of
    Just lang -> return lang
    Nothing -> pfail (Pos 0 0 "") ("unknown language: " ++ T.unpack t)

pfail :: Pos -> String -> P a
pfail pos msg = State.lift (Left (ParseError pos msg))

-- | Build interpolated string expression
mkInterpString :: Located -> ([ExprI], [Text]) -> Located -> P ExprI
mkInterpString startTok (exprs, mids) endTok = do
  let tails = mids ++ [getString endTok]
  patI <- freshExprI startTok (PatE (PatternText (getString startTok) tails))
  freshExprI startTok (AppE patI exprs)

-- | Flatten left-recursive application into AppE f [args]
mkApp :: ExprI -> ExprI -> P ExprI
mkApp (ExprI _ (AppE f args)) x = freshExprIFrom f (AppE f (args ++ [x]))
mkApp f x = freshExprIFrom f (AppE f [x])

-- | Flatten left-recursive type application into AppU head [args]
applyType :: TypeU -> TypeU -> TypeU
applyType (AppU f args) x = AppU f (args ++ [x])
applyType f x = AppU f [x]

-- | Reinterpret a type that was parsed before '=>' as constraints.
-- Handles: Ord a, (Ord a, Show a), (Ord a)
extractConstraints :: TypeU -> P [Constraint]
extractConstraints (AppU (VarU (TV name)) args) =
  return [Constraint (ClassName name) args]
extractConstraints (VarU (TV name)) =
  return [Constraint (ClassName name) []]
extractConstraints (NamU NamRecord _ _ entries) =
  -- Tuple types parsed as (A a, B b) become tuple-like structures.
  -- We need to handle tuples which use BT.tupleU.
  pfail (Pos 0 0 "") "invalid constraint syntax"
extractConstraints t =
  -- Try to extract from tuple representation (Tuple2 (Ord a) (Show a) etc.)
  case flattenTupleConstraint t of
    Just cs -> return cs
    Nothing -> pfail (Pos 0 0 "") ("invalid constraint: " ++ show t)

flattenTupleConstraint :: TypeU -> Maybe [Constraint]
flattenTupleConstraint (AppU (VarU (TV name)) args)
  | T.isPrefixOf "Tuple" name = mapM typeToConstraint args
  | otherwise = Just [Constraint (ClassName name) args]
flattenTupleConstraint (VarU (TV name)) =
  Just [Constraint (ClassName name) []]
flattenTupleConstraint _ = Nothing

-- | Extract class name and type variables from a type parsed as app_type
extractClassDef :: TypeU -> P (ClassName, [TVar])
extractClassDef (AppU (VarU (TV name)) args) = do
  tvs <- mapM typeToTVar args
  return (ClassName name, tvs)
extractClassDef (VarU (TV name)) =
  return (ClassName name, [])
extractClassDef _ = pfail (Pos 0 0 "") "invalid class head"

typeToTVar :: TypeU -> P TVar
typeToTVar (VarU tv) = return tv
typeToTVar _ = pfail (Pos 0 0 "") "expected type variable in class head"

typeToConstraint :: TypeU -> Maybe Constraint
typeToConstraint (AppU (VarU (TV name)) args) =
  Just (Constraint (ClassName name) args)
typeToConstraint (VarU (TV name)) =
  Just (Constraint (ClassName name) [])
typeToConstraint _ = Nothing

--------------------------------------------------------------------
-- Semantic action helpers
--------------------------------------------------------------------

mkSignature :: Located -> [Text] -> [Constraint] -> TypeU -> P Signature
mkSignature nameTok vs cs t = do
  let wrappedT = forallWrap (map TV vs) t
      et = EType wrappedT (Set.fromList cs) (ArgDocSig defaultValue [] defaultValue)
  return (Signature (toEVar nameTok) Nothing et)

mkTypedef :: Located -> Maybe (Lang, Bool) -> (TVar, [Either TVar TypeU]) -> TypeU -> P ExprI
mkTypedef tok lang (v, vs) t =
  freshExprI tok (TypE (ExprTypeE lang v vs t (ArgDocAlias defaultValue)))

mkTypedefAlias :: Located -> (TVar, [Either TVar TypeU]) -> P ExprI
mkTypedefAlias tok (v, vs) =
  let t = if null vs then VarU v else AppU (VarU v) (map (either VarU id) vs)
  in freshExprI tok (TypE (ExprTypeE Nothing v vs t (ArgDocAlias defaultValue)))

mkNamTypeWhere :: Located -> NamType -> (TVar, [Either TVar TypeU]) -> [(Key, TypeU)] -> P ExprI
mkNamTypeWhere tok nt (v, vs) entries =
  let t = NamU nt v (map (either VarU id) vs) entries
      doc = ArgDocRec defaultValue [(k, defaultValue) | (k, _) <- entries]
  in freshExprI tok (TypE (ExprTypeE Nothing v vs t doc))

mkNamTypeLegacy :: Located -> NamType -> (TVar, [Either TVar TypeU]) -> Maybe Text -> Maybe (Lang, Bool) -> [(Key, TypeU)] -> P ExprI
mkNamTypeLegacy tok nt (v, vs) mayCon lang entries =
  let con = maybe v TV mayCon
      t = NamU nt con (map (either VarU id) vs) entries
      doc = ArgDocRec defaultValue [(k, defaultValue) | (k, _) <- entries]
  in freshExprI tok (TypE (ExprTypeE lang v vs t doc))

mkSetter :: Located -> [(Text, ExprI)] -> P ExprI
mkSetter tok entries = do
  let keys = [(k, SelectorEnd) | (k, _) <- entries]
      vals = map snd entries
      sel = case keys of
        [] -> SelectorEnd
        (k : ks) -> SelectorKey k ks
  patI <- freshExprI tok (PatE (PatternStruct sel))
  lamI <- freshId tok
  let v = EV (".setter_" <> T.pack (show lamI))
  vArg <- freshExprI tok (VarE defaultValue v)
  appI <- freshExprI tok (AppE patI (vArg : vals))
  return (ExprI lamI (LamE [v] appI))

mkSource :: Located -> Lang -> Maybe Text -> (Text, Maybe Text) -> P ExprI
mkSource tok lang srcfile (name, mayAlias) = do
  let alias = maybe name id mayAlias
      path = fmap T.unpack srcfile
  freshExprI tok (SrcE Source
    { srcName = SrcName name
    , srcLang = lang
    , srcPath = path
    , srcAlias = EV alias
    , srcLabel = Nothing
    , srcRsize = []
    , srcNote = []
    })

mkSourceNew :: Located -> Lang -> Maybe Text -> Text -> P ExprI
mkSourceNew tok lang srcfile name =
  freshExprI tok (SrcE Source
    { srcName = SrcName name
    , srcLang = lang
    , srcPath = fmap T.unpack srcfile
    , srcAlias = EV name
    , srcLabel = Nothing
    , srcRsize = []
    , srcNote = []
    })

data DoStmt = DoBind EVar ExprI | DoBare ExprI

desugarDo :: Located -> [DoStmt] -> P ExprI
desugarDo tok [] = pfail (locPos tok) "empty do block"
desugarDo tok [DoBare e] = freshExprI tok (ForceE e)
desugarDo tok [DoBind _ _] = pfail (locPos tok) "do block cannot end with a bind (<-)"
desugarDo tok (DoBind v e : rest) = do
  forceE <- freshExprI tok (ForceE e)
  restE <- desugarDo tok rest
  freshExprI tok (LetE [(v, forceE)] restE)
desugarDo tok (DoBare e : rest) = do
  idx <- freshId tok
  let discardVar = EV ("_do_" <> T.pack (show idx))
  forceE <- freshExprI tok (ForceE e)
  restE <- desugarDo tok rest
  freshExprI tok (LetE [(discardVar, forceE)] restE)

-- | Wrap top-level declarations (without explicit module keyword) in an
-- implicit "main" module. Bare expressions are not supported at the top
-- level; use explicit assignments instead.
mkImplicitMain :: [ExprI] -> P [ExprI]
mkImplicitMain es = do
  let tok = Located (Pos 0 0 "") TokEOF ""
  modI <- freshId tok
  return [ExprI modI (ModE (MV "main") es)]

--------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------

parseError :: [Located] -> P a
parseError [] = pfail (Pos 0 0 "") "unexpected end of input"
parseError (Located pos tok _ : _) =
  pfail pos ("unexpected " ++ showToken tok)

--------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------

readProgram ::
  Maybe MVar ->
  Maybe Path ->
  Text ->
  PState ->
  DAG MVar Import ExprI ->
  Either String (DAG MVar Import ExprI, PState)
readProgram _moduleName modulePath sourceCode pstate dag = do
  let filename = maybe "<expr>" id modulePath
  (tokens, _docMap, groupToks) <- case lexMorloc filename sourceCode of
    Left err -> Left (showLexError err)
    Right result -> Right result
  case State.runStateT (parseProgram tokens) pstate of
    Right (result, finalState) ->
      let dag' = foldl addModule dag result
          dag'' = attachGroupAnnotations tokens groupToks dag'
      in return (dag'', finalState)
    Left err ->
      let exprTokens = stripLayoutTokens tokens
      in case State.runStateT (parseExprOnly exprTokens) pstate of
        Right (exprI, exprState) -> do
          let s = exprState
              i1 = psExpIndex s
              assI = ExprI i1 (AssE (EV "__expr__") exprI [])
              s1 = s { psExpIndex = i1 + 1 }
              i2 = psExpIndex s1
              expI = ExprI i2 (ExpE ExportAll)
              s2 = s1 { psExpIndex = i2 + 1 }
              i3 = psExpIndex s2
              modI = ExprI i3 (ModE (MV "main") [expI, assI])
              finalState = s2 { psExpIndex = i3 + 1 }
              dag' = Map.insert (MV "main") (modI, []) dag
          return (dag', finalState)
        Left _ ->
          Left (showParseError filename err)
  where
    addModule d e@(ExprI _ (ModE n es)) =
      let imports = [(importModuleName i, i) | (ExprI _ (ImpE i)) <- es]
      in Map.insert n (e, imports) d
    addModule _ _ = error "expected a module"

-- | Strip virtual layout tokens for expression fallback parsing
stripLayoutTokens :: [Located] -> [Located]
stripLayoutTokens = filter (not . isLayoutToken)
  where
    isLayoutToken (Located _ TokVLBrace _) = True
    isLayoutToken (Located _ TokVRBrace _) = True
    isLayoutToken (Located _ TokVSemi _) = True
    isLayoutToken _ = False

-- | Post-process the DAG to attach group annotations from --* tokens.
-- Scans the token stream to find export list symbols and their positions,
-- then uses group annotation positions to determine membership.
attachGroupAnnotations :: [Located] -> [Located] -> DAG MVar Import ExprI -> DAG MVar Import ExprI
attachGroupAnnotations _ [] dag = dag
attachGroupAnnotations tokens groupToks dag =
  let groupHeaders = parseGroupHeaders groupToks
      exportSymPositions = findExportSymbolPositions tokens
      membership = buildMembership groupHeaders exportSymPositions
      ghdrMap = Map.fromList [(n, d) | (n, d, _) <- groupHeaders]
  in Map.map (\(e, es) -> (attachToExpr membership ghdrMap e, es)) dag
  where
    attachToExpr :: Map.Map T.Text T.Text -> Map.Map T.Text [T.Text] -> ExprI -> ExprI
    attachToExpr mem ghdrs (ExprI i (ModE m es)) =
      ExprI i (ModE m (map (attachToExpr mem ghdrs) es))
    attachToExpr mem ghdrs (ExprI i (ExpE (ExportMany symbols _))) =
      let groupedSymNames = Map.keysSet mem
          -- preserve order of groups from source
          groupNames = nubOrd [gn | (_, gn) <- Map.toList mem]
          exportGroups =
            [ ExportGroup gn (maybe [] id (Map.lookup gn ghdrs))
                (Set.filter (\(_, sym) -> Map.lookup (symText sym) mem == Just gn) symbols)
            | gn <- groupNames
            ]
          ungrouped = Set.filter (\(_, sym) -> not (Set.member (symText sym) groupedSymNames)) symbols
      in ExprI i (ExpE (ExportMany ungrouped exportGroups))
    attachToExpr _ _ e = e

    nubOrd :: (Eq a) => [a] -> [a]
    nubOrd [] = []
    nubOrd (x:xs) = x : nubOrd (filter (/= x) xs)

    symText :: Symbol -> T.Text
    symText (TermSymbol (EV n)) = n
    symText (TypeSymbol (TV n)) = n
    symText (ClassSymbol (ClassName n)) = n

-- | Parse --* tokens into ordered (name, desc, position) triples
parseGroupHeaders :: [Located] -> [(T.Text, [T.Text], Pos)]
parseGroupHeaders = foldl' accum [] . map extractLine
  where
    extractLine (Located pos (TokGroupLine txt) _) = (pos, T.strip txt)
    extractLine (Located pos _ _) = (pos, T.empty)

    accum :: [(T.Text, [T.Text], Pos)] -> (Pos, T.Text) -> [(T.Text, [T.Text], Pos)]
    accum gs (pos, line) = case T.stripPrefix "name:" line of
      Just name -> gs ++ [(T.strip name, [], pos)]
      Nothing -> case T.stripPrefix "desc:" line of
        Just desc -> case gs of
          [] -> gs
          _ -> init gs ++ [let (n, ds, p) = last gs in (n, ds ++ [T.strip desc], p)]
        Nothing -> gs

-- | Find symbol names and positions in the export list by scanning tokens.
findExportSymbolPositions :: [Located] -> [(T.Text, Pos)]
findExportSymbolPositions = findModule
  where
    findModule (Located _ TokModule _ : rest) = findLParen rest
    findModule (_ : rest) = findModule rest
    findModule [] = []

    findLParen (Located _ TokLParen _ : rest) = scanExports 1 rest
    findLParen (Located _ TokStar _ : _) = []
    findLParen (_ : rest) = findLParen rest
    findLParen [] = []

    scanExports :: Int -> [Located] -> [(T.Text, Pos)]
    scanExports 0 _ = []
    scanExports depth (Located _ TokLParen _ : rest) = scanExports (depth + 1) rest
    scanExports depth (Located _ TokRParen _ : rest)
      | depth <= 1 = []
      | otherwise = scanExports (depth - 1) rest
    scanExports depth (Located pos (TokLowerName n) _ : rest) = (n, pos) : scanExports depth rest
    scanExports depth (Located pos (TokUpperName n) _ : rest) = (n, pos) : scanExports depth rest
    scanExports depth (_ : rest) = scanExports depth rest
    scanExports _ [] = []

-- | Build symbol name -> group name map using positions.
-- For each export symbol, find the most recent group annotation that
-- precedes it (by source position). Symbols before any group are ungrouped.
buildMembership :: [(T.Text, [T.Text], Pos)] -> [(T.Text, Pos)] -> Map.Map T.Text T.Text
buildMembership groupHeaders exportSyms = Map.fromList
  [ (sym, gname)
  | (sym, symPos) <- exportSyms
  , Just gname <- [findGroup symPos]
  ]
  where
    -- Groups sorted by position
    sortedGroups = sortBy (\(_,_,p1) (_,_,p2) -> compare p1 p2) groupHeaders

    -- Find the group that a symbol at the given position belongs to
    findGroup :: Pos -> Maybe T.Text
    findGroup symPos = case filter (\(_,_,gpos) -> gpos < symPos) (reverse sortedGroups) of
      ((gname,_,_):_) -> Just gname
      [] -> Nothing

readType :: Text -> Either String TypeU
readType typeStr = do
  let initState = emptyPState
  (tokens, _, _) <- case lexMorloc "<type>" typeStr of
    Left err -> Left (showLexError err)
    Right result -> Right result
  (result, _) <- case State.runStateT (parseTypeOnly tokens) initState of
    Left err -> Left (showParseError "<type>" err)
    Right r -> Right r
  return result
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
