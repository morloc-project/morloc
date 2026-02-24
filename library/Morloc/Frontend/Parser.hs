{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}

module Morloc.Frontend.Parser
  ( readProgram
  , readType
  , PState (..)
  , emptyPState
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Scientific as DS
import Data.List (sortBy, foldl')
import qualified Control.Monad.State.Strict as State
import Morloc.Frontend.Token
import Morloc.Frontend.Lexer (lexMorloc, showLexError)
import Morloc.Frontend.CST
import Morloc.Frontend.Desugar (DState(..), D, ParseError(..), showParseError, desugarProgram, desugarExpr)
import Morloc.Namespace.Prim
import Morloc.Namespace.Type
import Morloc.Namespace.Expr
import qualified Morloc.BaseTypes as BT
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Located)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn6 (([Loc CstExpr], Bool))
	| HappyAbsSyn7 (TypeU)
	| HappyAbsSyn8 (Loc CstExpr)
	| HappyAbsSyn9 ([Loc CstExpr])
	| HappyAbsSyn15 ([(Loc CstExpr, Loc CstExpr)])
	| HappyAbsSyn16 ((Loc CstExpr, Loc CstExpr))
	| HappyAbsSyn17 (Text)
	| HappyAbsSyn18 ([Text])
	| HappyAbsSyn20 (CstExport)
	| HappyAbsSyn21 ([Located])
	| HappyAbsSyn22 (Located)
	| HappyAbsSyn25 (Maybe [AliasedSymbol])
	| HappyAbsSyn26 ([AliasedSymbol])
	| HappyAbsSyn27 (AliasedSymbol)
	| HappyAbsSyn29 ((Located, NamType))
	| HappyAbsSyn30 ((Text, Bool))
	| HappyAbsSyn31 ([(Key, TypeU)])
	| HappyAbsSyn33 ((TVar, [Either TVar TypeU]))
	| HappyAbsSyn34 ([Either TVar TypeU])
	| HappyAbsSyn35 ((Key, TypeU))
	| HappyAbsSyn36 ((Located, Key, TypeU))
	| HappyAbsSyn37 ([(Located, Key, TypeU)])
	| HappyAbsSyn39 ((TypeU, Bool))
	| HappyAbsSyn40 ([TypeU])
	| HappyAbsSyn46 (CstClassHead)
	| HappyAbsSyn47 ([Constraint])
	| HappyAbsSyn48 ([CstSigItem])
	| HappyAbsSyn49 (CstSigItem)
	| HappyAbsSyn51 ([[Loc CstExpr]])
	| HappyAbsSyn54 ([EVar])
	| HappyAbsSyn55 (EVar)
	| HappyAbsSyn57 (Maybe Text)
	| HappyAbsSyn58 ([(Text, Maybe Text)])
	| HappyAbsSyn59 ((Text, Maybe Text))
	| HappyAbsSyn64 ([(EVar, Loc CstExpr)])
	| HappyAbsSyn65 ((EVar, Loc CstExpr))
	| HappyAbsSyn77 ([(Key, Loc CstExpr)])
	| HappyAbsSyn78 ((Key, Loc CstExpr))
	| HappyAbsSyn81 ([CstDoStmt])
	| HappyAbsSyn82 (CstDoStmt)
	| HappyAbsSyn84 (CstAccessorBody)
	| HappyAbsSyn85 (CstAccessorTail)
	| HappyAbsSyn86 ([CstAccessorBody])
	| HappyAbsSyn94 (([Loc CstExpr], [Text]))
	| HappyAbsSyn102 (CstSigType)
	| HappyAbsSyn103 ([(Pos, TypeU)])
	| HappyAbsSyn104 ((Pos, TypeU))
	| HappyAbsSyn107 (Constraint)

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
 action_403,
 action_404,
 action_405,
 action_406,
 action_407,
 action_408,
 action_409,
 action_410,
 action_411,
 action_412,
 action_413,
 action_414,
 action_415,
 action_416,
 action_417,
 action_418,
 action_419,
 action_420,
 action_421,
 action_422,
 action_423,
 action_424,
 action_425,
 action_426,
 action_427,
 action_428,
 action_429,
 action_430,
 action_431,
 action_432,
 action_433,
 action_434,
 action_435,
 action_436,
 action_437,
 action_438,
 action_439,
 action_440,
 action_441,
 action_442,
 action_443,
 action_444,
 action_445,
 action_446,
 action_447,
 action_448,
 action_449,
 action_450,
 action_451,
 action_452,
 action_453,
 action_454,
 action_455,
 action_456,
 action_457,
 action_458,
 action_459,
 action_460,
 action_461,
 action_462,
 action_463,
 action_464,
 action_465,
 action_466,
 action_467,
 action_468,
 action_469,
 action_470,
 action_471,
 action_472,
 action_473,
 action_474,
 action_475,
 action_476,
 action_477,
 action_478,
 action_479,
 action_480,
 action_481,
 action_482,
 action_483,
 action_484,
 action_485,
 action_486,
 action_487,
 action_488,
 action_489,
 action_490,
 action_491,
 action_492,
 action_493,
 action_494,
 action_495,
 action_496,
 action_497,
 action_498,
 action_499,
 action_500,
 action_501,
 action_502,
 action_503,
 action_504,
 action_505,
 action_506,
 action_507,
 action_508,
 action_509,
 action_510,
 action_511,
 action_512,
 action_513,
 action_514,
 action_515,
 action_516,
 action_517,
 action_518,
 action_519,
 action_520 :: () => Prelude.Int -> ({-HappyReduction (P) = -}
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
 happyReduce_215,
 happyReduce_216,
 happyReduce_217,
 happyReduce_218,
 happyReduce_219,
 happyReduce_220,
 happyReduce_221,
 happyReduce_222,
 happyReduce_223,
 happyReduce_224,
 happyReduce_225,
 happyReduce_226,
 happyReduce_227,
 happyReduce_228,
 happyReduce_229,
 happyReduce_230,
 happyReduce_231,
 happyReduce_232,
 happyReduce_233,
 happyReduce_234,
 happyReduce_235,
 happyReduce_236,
 happyReduce_237,
 happyReduce_238,
 happyReduce_239,
 happyReduce_240,
 happyReduce_241,
 happyReduce_242,
 happyReduce_243,
 happyReduce_244,
 happyReduce_245,
 happyReduce_246,
 happyReduce_247,
 happyReduce_248,
 happyReduce_249,
 happyReduce_250,
 happyReduce_251,
 happyReduce_252,
 happyReduce_253,
 happyReduce_254,
 happyReduce_255,
 happyReduce_256,
 happyReduce_257,
 happyReduce_258,
 happyReduce_259,
 happyReduce_260,
 happyReduce_261,
 happyReduce_262,
 happyReduce_263,
 happyReduce_264,
 happyReduce_265,
 happyReduce_266,
 happyReduce_267,
 happyReduce_268,
 happyReduce_269,
 happyReduce_270,
 happyReduce_271,
 happyReduce_272,
 happyReduce_273,
 happyReduce_274,
 happyReduce_275,
 happyReduce_276,
 happyReduce_277,
 happyReduce_278,
 happyReduce_279,
 happyReduce_280,
 happyReduce_281,
 happyReduce_282 :: () => ({-HappyReduction (P) = -}
	   Prelude.Int 
	-> (Located)
	-> HappyState (Located) (HappyStk HappyAbsSyn -> [(Located)] -> (P) HappyAbsSyn)
	-> [HappyState (Located) (HappyStk HappyAbsSyn -> [(Located)] -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Located)] -> (P) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,1660) ([0,0,0,0,0,0,0,4,4096,0,0,0,0,0,0,0,0,0,340,0,24576,4,0,0,0,0,0,0,32768,9994,12304,59008,1,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,1,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,47104,33654,769,8040,0,0,0,0,0,0,0,7424,8270,96,973,0,0,0,0,0,0,0,49824,1033,40972,121,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20480,1217,1536,15552,0,0,0,0,0,0,0,512,0,0,144,0,0,0,0,0,0,0,0,0,0,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,256,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5568,0,0,70,0,0,0,0,0,0,0,680,0,49152,8,0,0,0,0,0,0,0,85,0,6144,1,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,2560,0,64536,71,0,0,0,0,0,0,0,192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,48,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3072,770,0,2,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,1536,0,0,0,0,0,0,0,2048,0,0,192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,10,0,35840,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21504,1,0,1120,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,32768,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2720,0,0,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,0,0,20480,1249,1538,15568,0,0,0,0,0,0,0,10752,16540,192,1946,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,32768,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,1152,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16896,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,258,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,1536,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,9994,12304,59008,1,0,0,0,0,0,0,20480,1249,1538,15568,0,0,0,0,0,0,0,10752,16540,192,1946,0,0,0,0,0,0,0,5440,0,0,70,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,19989,24608,52480,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,48,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,32768,0,8,3072,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,4997,6152,62272,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5376,8270,96,973,0,0,0,0,0,0,0,49824,1033,40972,121,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10752,16540,192,1946,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,18432,0,0,0,0,0,0,0,40960,2498,3076,31136,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,36864,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5376,8270,96,973,0,0,0,0,0,0,0,49824,1033,40972,121,0,0,0,0,0,0,0,14420,32897,13313,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3072,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,2,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,170,0,12288,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33540,192,40960,0,0,0,0,0,0,0,32768,4192,24,5120,0,0,0,0,0,0,0,4096,524,3,640,0,0,0,0,0,0,0,10752,0,0,560,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,168,32,49152,8,0,0,0,0,0,0,0,87,0,6144,1,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,51200,0,1024,0,0,0,0,0,0,0,0,2048,128,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,61536,287,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,4,0,0,0,0,0,0,0,16,0,0,1,0,0,0,0,0,0,0,2,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2240,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,2,1,0,0,0,0,0,0,0,0,14420,32897,13313,15,0,0,0,0,0,0,32768,42,0,35840,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,768,0,0,0,0,0,0,0,1024,0,32,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,16,8,32768,0,0,0,0,0,0,0,0,2,0,8192,0,0,0,0,0,0,0,16384,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,10,0,35840,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1344,0,2,70,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49283,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8256,0,0,0,0,0,0,0,0,0,0,14420,32897,13313,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20480,1249,1538,15568,0,0,0,0,0,0,0,0,288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,28840,258,26627,30,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16512,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24576,6160,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,512,0,0,48,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40960,2498,3076,31136,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,57680,516,53254,60,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21760,0,0,280,0,0,0,0,0,0,0,6176,1540,0,5,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,8,0,16384,0,0,0,0,0,0,0,0,21,0,6144,1,0,0,0,0,0,0,40960,2,0,8960,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,48,0,32768,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,21760,0,0,280,0,0,0,0,0,0,0,2720,0,0,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,16512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8384,48,8192,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,16384,5,0,17920,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,22272,0,0,280,0,0,0,0,0,0,0,2720,0,0,35,0,0,0,0,0,0,0,340,0,24576,4,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6144,0,0,64,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,24576,4,0,0,0,0,0,0,0,0,0,35840,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,1024,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1360,0,32768,17,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,512,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,42,0,35840,0,0,0,0,0,0,0,20480,5,0,4480,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,12,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,516,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,340,0,24576,4,0,0,0,0,0,0,32768,42,0,35840,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1344,0,0,70,0,0,0,0,0,0,0,168,0,49152,8,0,0,0,0,0,0,49152,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,64,0,0,0,0,0,0,0,5376,0,0,280,0,0,0,0,0,0,0,2720,0,0,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,10752,0,0,560,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,696,0,49152,8,0,0,0,0,0,0,0,85,0,6144,1,0,0,0,0,0,0,40960,10,0,8960,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,24576,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,43008,2,0,2240,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,680,0,49152,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,36864,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2720,0,0,35,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,16384,0,0,0,0,0,0,0,0,258,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33030,1,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,84,0,24576,4,0,0,0,0,0,0,32768,42,0,35840,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,2064,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,21504,1,0,1120,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,170,0,12288,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,16,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20480,5,0,4480,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,258,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseProgram","%start_parseTypeOnly","%start_parseExprOnly","program","type_eof","expr_eof","modules","module","top_body","top_decls","top_decl","sig_or_ass","guard_clauses","guard_clause","module_name","module_parts","module_comp","exports","export_list","export_item","symbol","import_decl","opt_import_list","import_items","import_item","typedef_decl","nam_type","nam_constructor","opt_nam_entries","lang_token","typedef_term","typedef_params","nam_entry","nam_entry_loc","nam_entry_list_loc","nam_entries","concrete_rhs","concrete_rhs_args","non_string_type","non_string_non_fun","non_string_app","non_string_atom","typeclass_decl","class_head","class_constraints","sig_list","signature","instance_decl","instance_items","instance_item","fixity_decl","operator_names","operator_ref","source_decl","opt_from","source_items","source_item","source_new_items","source_new_item","expr","let_expr","let_bindings","let_binding","lambda_expr","infix_expr","operand","app_expr","atom_exprs1","atom_expr","force_expr","paren_expr","expr_list1","suspend_expr","record_expr","record_entries","record_entry","list_expr","do_expr","do_stmts","do_stmt","getter_expr","accessor_body","accessor_tail","grouped_accessors","grouped_accessor","var_expr","hole_expr","bool_expr","num_expr","string_expr","interp_string","interp_body","type","fun_type","non_fun_type","app_type","atom_type","type_list1","types1","sig_type","sig_fun_args","pos_non_fun_type","pos_app_type","pos_atom_type","single_constraint","operator_name","evar_or_op","opt_where_decls","where_items","where_item","lower_names","lower_names1","VLBRACE","VRBRACE","VSEMI","'('","')'","'['","']'","'{'","'}'","'<'","'>'","','","'\\\\'","'_'","'!'","'?'","'.'","GDOT","'='","'::'","'->'","'=>'","'<-'","'*'","'-'","':'","'module'","'import'","'source'","'from'","'where'","'as'","'True'","'False'","'type'","'record'","'object'","'table'","'class'","'instance'","'infixl'","'infixr'","'infix'","'let'","'in'","'do'","LOWER","UPPER","OPERATOR","INTEGER","FLOAT","STRING","STRSTART","STRMID","STREND","INTERPOPEN","INTERPCLOSE","EOF","%eof"]
        bit_start = st Prelude.* 173
        bit_end = (st Prelude.+ 1) Prelude.* 173
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..172]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (115) = happyShift action_62
action_0 (141) = happyShift action_6
action_0 (6) = happyGoto action_60
action_0 (9) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 (11) = happyGoto action_61
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (118) = happyShift action_53
action_1 (120) = happyShift action_54
action_1 (122) = happyShift action_55
action_1 (124) = happyShift action_56
action_1 (161) = happyShift action_57
action_1 (162) = happyShift action_58
action_1 (166) = happyShift action_59
action_1 (7) = happyGoto action_47
action_1 (95) = happyGoto action_48
action_1 (96) = happyGoto action_49
action_1 (97) = happyGoto action_50
action_1 (98) = happyGoto action_51
action_1 (99) = happyGoto action_52
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (118) = happyShift action_30
action_2 (120) = happyShift action_31
action_2 (122) = happyShift action_32
action_2 (127) = happyShift action_33
action_2 (128) = happyShift action_34
action_2 (129) = happyShift action_35
action_2 (132) = happyShift action_36
action_2 (139) = happyShift action_37
action_2 (147) = happyShift action_38
action_2 (148) = happyShift action_39
action_2 (158) = happyShift action_40
action_2 (160) = happyShift action_41
action_2 (161) = happyShift action_42
action_2 (164) = happyShift action_43
action_2 (165) = happyShift action_44
action_2 (166) = happyShift action_45
action_2 (167) = happyShift action_46
action_2 (8) = happyGoto action_7
action_2 (62) = happyGoto action_8
action_2 (63) = happyGoto action_9
action_2 (64) = happyGoto action_10
action_2 (65) = happyGoto action_11
action_2 (66) = happyGoto action_12
action_2 (67) = happyGoto action_13
action_2 (68) = happyGoto action_14
action_2 (69) = happyGoto action_15
action_2 (71) = happyGoto action_16
action_2 (72) = happyGoto action_17
action_2 (73) = happyGoto action_18
action_2 (75) = happyGoto action_19
action_2 (76) = happyGoto action_20
action_2 (79) = happyGoto action_21
action_2 (80) = happyGoto action_22
action_2 (83) = happyGoto action_23
action_2 (88) = happyGoto action_24
action_2 (89) = happyGoto action_25
action_2 (90) = happyGoto action_26
action_2 (91) = happyGoto action_27
action_2 (92) = happyGoto action_28
action_2 (93) = happyGoto action_29
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (141) = happyShift action_6
action_3 (9) = happyGoto action_4
action_3 (10) = happyGoto action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (141) = happyShift action_6
action_4 (172) = happyShift action_142
action_4 (10) = happyGoto action_141
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_7

action_6 (161) = happyShift action_140
action_6 (17) = happyGoto action_137
action_6 (18) = happyGoto action_138
action_6 (19) = happyGoto action_139
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (173) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (172) = happyShift action_136
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_150

action_10 (158) = happyShift action_40
action_10 (159) = happyShift action_135
action_10 (65) = happyGoto action_134
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_155

action_12 _ = happyReduce_151

action_13 (134) = happyShift action_133
action_13 _ = happyReduce_152

action_14 (124) = happyShift action_122
action_14 (125) = happyShift action_123
action_14 (131) = happyShift action_131
action_14 (138) = happyShift action_125
action_14 (139) = happyShift action_132
action_14 (163) = happyShift action_127
action_14 (108) = happyGoto action_130
action_14 _ = happyReduce_161

action_15 _ = happyReduce_165

action_16 (118) = happyShift action_30
action_16 (120) = happyShift action_31
action_16 (122) = happyShift action_32
action_16 (128) = happyShift action_34
action_16 (129) = happyShift action_35
action_16 (132) = happyShift action_36
action_16 (147) = happyShift action_38
action_16 (148) = happyShift action_39
action_16 (160) = happyShift action_41
action_16 (161) = happyShift action_42
action_16 (164) = happyShift action_43
action_16 (165) = happyShift action_44
action_16 (166) = happyShift action_45
action_16 (167) = happyShift action_46
action_16 (70) = happyGoto action_128
action_16 (71) = happyGoto action_129
action_16 (72) = happyGoto action_17
action_16 (73) = happyGoto action_18
action_16 (75) = happyGoto action_19
action_16 (76) = happyGoto action_20
action_16 (79) = happyGoto action_21
action_16 (80) = happyGoto action_22
action_16 (83) = happyGoto action_23
action_16 (88) = happyGoto action_24
action_16 (89) = happyGoto action_25
action_16 (90) = happyGoto action_26
action_16 (91) = happyGoto action_27
action_16 (92) = happyGoto action_28
action_16 (93) = happyGoto action_29
action_16 _ = happyReduce_168

action_17 _ = happyReduce_172

action_18 _ = happyReduce_173

action_19 _ = happyReduce_179

action_20 _ = happyReduce_180

action_21 _ = happyReduce_178

action_22 _ = happyReduce_183

action_23 _ = happyReduce_174

action_24 _ = happyReduce_181

action_25 _ = happyReduce_182

action_26 _ = happyReduce_176

action_27 _ = happyReduce_177

action_28 _ = happyReduce_175

action_29 _ = happyReduce_224

action_30 (118) = happyShift action_30
action_30 (119) = happyShift action_121
action_30 (120) = happyShift action_31
action_30 (122) = happyShift action_32
action_30 (124) = happyShift action_122
action_30 (125) = happyShift action_123
action_30 (127) = happyShift action_33
action_30 (128) = happyShift action_34
action_30 (129) = happyShift action_35
action_30 (131) = happyShift action_124
action_30 (132) = happyShift action_36
action_30 (138) = happyShift action_125
action_30 (139) = happyShift action_126
action_30 (147) = happyShift action_38
action_30 (148) = happyShift action_39
action_30 (158) = happyShift action_40
action_30 (160) = happyShift action_41
action_30 (161) = happyShift action_42
action_30 (163) = happyShift action_127
action_30 (164) = happyShift action_43
action_30 (165) = happyShift action_44
action_30 (166) = happyShift action_45
action_30 (167) = happyShift action_46
action_30 (62) = happyGoto action_119
action_30 (63) = happyGoto action_9
action_30 (64) = happyGoto action_10
action_30 (65) = happyGoto action_11
action_30 (66) = happyGoto action_12
action_30 (67) = happyGoto action_13
action_30 (68) = happyGoto action_14
action_30 (69) = happyGoto action_15
action_30 (71) = happyGoto action_16
action_30 (72) = happyGoto action_17
action_30 (73) = happyGoto action_18
action_30 (75) = happyGoto action_19
action_30 (76) = happyGoto action_20
action_30 (79) = happyGoto action_21
action_30 (80) = happyGoto action_22
action_30 (83) = happyGoto action_23
action_30 (88) = happyGoto action_24
action_30 (89) = happyGoto action_25
action_30 (90) = happyGoto action_26
action_30 (91) = happyGoto action_27
action_30 (92) = happyGoto action_28
action_30 (93) = happyGoto action_29
action_30 (108) = happyGoto action_120
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (118) = happyShift action_30
action_31 (120) = happyShift action_31
action_31 (121) = happyShift action_118
action_31 (122) = happyShift action_32
action_31 (127) = happyShift action_33
action_31 (128) = happyShift action_34
action_31 (129) = happyShift action_35
action_31 (132) = happyShift action_36
action_31 (139) = happyShift action_37
action_31 (147) = happyShift action_38
action_31 (148) = happyShift action_39
action_31 (158) = happyShift action_40
action_31 (160) = happyShift action_41
action_31 (161) = happyShift action_42
action_31 (164) = happyShift action_43
action_31 (165) = happyShift action_44
action_31 (166) = happyShift action_45
action_31 (167) = happyShift action_46
action_31 (62) = happyGoto action_116
action_31 (63) = happyGoto action_9
action_31 (64) = happyGoto action_10
action_31 (65) = happyGoto action_11
action_31 (66) = happyGoto action_12
action_31 (67) = happyGoto action_13
action_31 (68) = happyGoto action_14
action_31 (69) = happyGoto action_15
action_31 (71) = happyGoto action_16
action_31 (72) = happyGoto action_17
action_31 (73) = happyGoto action_18
action_31 (74) = happyGoto action_117
action_31 (75) = happyGoto action_19
action_31 (76) = happyGoto action_20
action_31 (79) = happyGoto action_21
action_31 (80) = happyGoto action_22
action_31 (83) = happyGoto action_23
action_31 (88) = happyGoto action_24
action_31 (89) = happyGoto action_25
action_31 (90) = happyGoto action_26
action_31 (91) = happyGoto action_27
action_31 (92) = happyGoto action_28
action_31 (93) = happyGoto action_29
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (118) = happyShift action_30
action_32 (120) = happyShift action_31
action_32 (122) = happyShift action_32
action_32 (127) = happyShift action_33
action_32 (128) = happyShift action_34
action_32 (129) = happyShift action_35
action_32 (132) = happyShift action_36
action_32 (139) = happyShift action_37
action_32 (147) = happyShift action_38
action_32 (148) = happyShift action_39
action_32 (158) = happyShift action_40
action_32 (160) = happyShift action_41
action_32 (161) = happyShift action_115
action_32 (164) = happyShift action_43
action_32 (165) = happyShift action_44
action_32 (166) = happyShift action_45
action_32 (167) = happyShift action_46
action_32 (62) = happyGoto action_112
action_32 (63) = happyGoto action_9
action_32 (64) = happyGoto action_10
action_32 (65) = happyGoto action_11
action_32 (66) = happyGoto action_12
action_32 (67) = happyGoto action_13
action_32 (68) = happyGoto action_14
action_32 (69) = happyGoto action_15
action_32 (71) = happyGoto action_16
action_32 (72) = happyGoto action_17
action_32 (73) = happyGoto action_18
action_32 (75) = happyGoto action_19
action_32 (76) = happyGoto action_20
action_32 (77) = happyGoto action_113
action_32 (78) = happyGoto action_114
action_32 (79) = happyGoto action_21
action_32 (80) = happyGoto action_22
action_32 (83) = happyGoto action_23
action_32 (88) = happyGoto action_24
action_32 (89) = happyGoto action_25
action_32 (90) = happyGoto action_26
action_32 (91) = happyGoto action_27
action_32 (92) = happyGoto action_28
action_32 (93) = happyGoto action_29
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (161) = happyShift action_111
action_33 (114) = happyGoto action_110
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_218

action_35 (118) = happyShift action_30
action_35 (120) = happyShift action_31
action_35 (122) = happyShift action_32
action_35 (128) = happyShift action_34
action_35 (129) = happyShift action_35
action_35 (132) = happyShift action_36
action_35 (147) = happyShift action_38
action_35 (148) = happyShift action_39
action_35 (160) = happyShift action_41
action_35 (161) = happyShift action_42
action_35 (164) = happyShift action_43
action_35 (165) = happyShift action_44
action_35 (166) = happyShift action_45
action_35 (167) = happyShift action_46
action_35 (71) = happyGoto action_109
action_35 (72) = happyGoto action_17
action_35 (73) = happyGoto action_18
action_35 (75) = happyGoto action_19
action_35 (76) = happyGoto action_20
action_35 (79) = happyGoto action_21
action_35 (80) = happyGoto action_22
action_35 (83) = happyGoto action_23
action_35 (88) = happyGoto action_24
action_35 (89) = happyGoto action_25
action_35 (90) = happyGoto action_26
action_35 (91) = happyGoto action_27
action_35 (92) = happyGoto action_28
action_35 (93) = happyGoto action_29
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (118) = happyShift action_106
action_36 (161) = happyShift action_107
action_36 (164) = happyShift action_108
action_36 (84) = happyGoto action_105
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (164) = happyShift action_103
action_37 (165) = happyShift action_104
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_219

action_39 _ = happyReduce_220

action_40 (128) = happyShift action_101
action_40 (161) = happyShift action_102
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (115) = happyShift action_100
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_217

action_43 _ = happyReduce_221

action_44 _ = happyReduce_222

action_45 _ = happyReduce_223

action_46 (170) = happyShift action_99
action_46 (94) = happyGoto action_98
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (173) = happyAccept
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (172) = happyShift action_97
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_228

action_50 (135) = happyShift action_96
action_50 _ = happyReduce_229

action_51 (118) = happyShift action_53
action_51 (120) = happyShift action_54
action_51 (122) = happyShift action_55
action_51 (161) = happyShift action_57
action_51 (162) = happyShift action_58
action_51 (166) = happyShift action_59
action_51 (99) = happyGoto action_95
action_51 _ = happyReduce_232

action_52 _ = happyReduce_234

action_53 (118) = happyShift action_53
action_53 (119) = happyShift action_94
action_53 (120) = happyShift action_54
action_53 (122) = happyShift action_55
action_53 (124) = happyShift action_56
action_53 (161) = happyShift action_57
action_53 (162) = happyShift action_58
action_53 (166) = happyShift action_59
action_53 (95) = happyGoto action_93
action_53 (96) = happyGoto action_49
action_53 (97) = happyGoto action_50
action_53 (98) = happyGoto action_51
action_53 (99) = happyGoto action_52
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (118) = happyShift action_53
action_54 (120) = happyShift action_54
action_54 (122) = happyShift action_55
action_54 (124) = happyShift action_56
action_54 (161) = happyShift action_57
action_54 (162) = happyShift action_58
action_54 (166) = happyShift action_59
action_54 (95) = happyGoto action_92
action_54 (96) = happyGoto action_49
action_54 (97) = happyGoto action_50
action_54 (98) = happyGoto action_51
action_54 (99) = happyGoto action_52
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (118) = happyShift action_53
action_55 (120) = happyShift action_54
action_55 (122) = happyShift action_55
action_55 (124) = happyShift action_56
action_55 (161) = happyShift action_57
action_55 (162) = happyShift action_58
action_55 (166) = happyShift action_59
action_55 (95) = happyGoto action_91
action_55 (96) = happyGoto action_49
action_55 (97) = happyGoto action_50
action_55 (98) = happyGoto action_51
action_55 (99) = happyGoto action_52
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (161) = happyShift action_90
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (140) = happyShift action_89
action_57 _ = happyReduce_242

action_58 _ = happyReduce_240

action_59 _ = happyReduce_243

action_60 (173) = happyAccept
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (172) = happyShift action_88
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (116) = happyShift action_74
action_62 (118) = happyShift action_75
action_62 (142) = happyShift action_76
action_62 (143) = happyShift action_77
action_62 (149) = happyShift action_78
action_62 (150) = happyShift action_79
action_62 (151) = happyShift action_80
action_62 (152) = happyShift action_81
action_62 (153) = happyShift action_82
action_62 (154) = happyShift action_83
action_62 (155) = happyShift action_84
action_62 (156) = happyShift action_85
action_62 (157) = happyShift action_86
action_62 (161) = happyShift action_87
action_62 (12) = happyGoto action_63
action_62 (13) = happyGoto action_64
action_62 (14) = happyGoto action_65
action_62 (24) = happyGoto action_66
action_62 (28) = happyGoto action_67
action_62 (29) = happyGoto action_68
action_62 (45) = happyGoto action_69
action_62 (50) = happyGoto action_70
action_62 (53) = happyGoto action_71
action_62 (56) = happyGoto action_72
action_62 (109) = happyGoto action_73
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (116) = happyShift action_215
action_63 (117) = happyShift action_216
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_12

action_65 _ = happyReduce_20

action_66 _ = happyReduce_14

action_67 _ = happyReduce_15

action_68 (118) = happyShift action_212
action_68 (161) = happyShift action_213
action_68 (162) = happyShift action_214
action_68 (33) = happyGoto action_211
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_16

action_70 _ = happyReduce_17

action_71 _ = happyReduce_18

action_72 _ = happyReduce_19

action_73 (113) = happyGoto action_210
action_73 _ = happyReduce_279

action_74 _ = happyReduce_11

action_75 (124) = happyShift action_122
action_75 (125) = happyShift action_123
action_75 (131) = happyShift action_208
action_75 (138) = happyShift action_125
action_75 (139) = happyShift action_209
action_75 (163) = happyShift action_127
action_75 (108) = happyGoto action_207
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (161) = happyShift action_140
action_76 (17) = happyGoto action_206
action_76 (18) = happyGoto action_138
action_76 (19) = happyGoto action_139
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (161) = happyShift action_204
action_77 (162) = happyShift action_205
action_77 (32) = happyGoto action_203
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (118) = happyShift action_200
action_78 (161) = happyShift action_201
action_78 (162) = happyShift action_202
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_68

action_80 _ = happyReduce_69

action_81 _ = happyReduce_70

action_82 (118) = happyShift action_199
action_82 (120) = happyShift action_54
action_82 (122) = happyShift action_55
action_82 (161) = happyShift action_57
action_82 (162) = happyShift action_58
action_82 (166) = happyShift action_59
action_82 (46) = happyGoto action_197
action_82 (98) = happyGoto action_198
action_82 (99) = happyGoto action_52
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (162) = happyShift action_196
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (164) = happyShift action_195
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (164) = happyShift action_194
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (164) = happyShift action_193
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_270

action_88 _ = happyReduce_4

action_89 (118) = happyShift action_53
action_89 (120) = happyShift action_54
action_89 (122) = happyShift action_55
action_89 (124) = happyShift action_56
action_89 (161) = happyShift action_57
action_89 (162) = happyShift action_58
action_89 (166) = happyShift action_59
action_89 (97) = happyGoto action_192
action_89 (98) = happyGoto action_51
action_89 (99) = happyGoto action_52
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (125) = happyShift action_191
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (123) = happyShift action_190
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (121) = happyShift action_189
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (119) = happyShift action_187
action_93 (126) = happyShift action_188
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_235

action_95 _ = happyReduce_233

action_96 (118) = happyShift action_53
action_96 (120) = happyShift action_54
action_96 (122) = happyShift action_55
action_96 (124) = happyShift action_56
action_96 (161) = happyShift action_57
action_96 (162) = happyShift action_58
action_96 (166) = happyShift action_59
action_96 (95) = happyGoto action_186
action_96 (96) = happyGoto action_49
action_96 (97) = happyGoto action_50
action_96 (98) = happyGoto action_51
action_96 (99) = happyGoto action_52
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_5

action_98 (168) = happyShift action_184
action_98 (169) = happyShift action_185
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (118) = happyShift action_30
action_99 (120) = happyShift action_31
action_99 (122) = happyShift action_32
action_99 (127) = happyShift action_33
action_99 (128) = happyShift action_34
action_99 (129) = happyShift action_35
action_99 (132) = happyShift action_36
action_99 (139) = happyShift action_37
action_99 (147) = happyShift action_38
action_99 (148) = happyShift action_39
action_99 (158) = happyShift action_40
action_99 (160) = happyShift action_41
action_99 (161) = happyShift action_42
action_99 (164) = happyShift action_43
action_99 (165) = happyShift action_44
action_99 (166) = happyShift action_45
action_99 (167) = happyShift action_46
action_99 (62) = happyGoto action_183
action_99 (63) = happyGoto action_9
action_99 (64) = happyGoto action_10
action_99 (65) = happyGoto action_11
action_99 (66) = happyGoto action_12
action_99 (67) = happyGoto action_13
action_99 (68) = happyGoto action_14
action_99 (69) = happyGoto action_15
action_99 (71) = happyGoto action_16
action_99 (72) = happyGoto action_17
action_99 (73) = happyGoto action_18
action_99 (75) = happyGoto action_19
action_99 (76) = happyGoto action_20
action_99 (79) = happyGoto action_21
action_99 (80) = happyGoto action_22
action_99 (83) = happyGoto action_23
action_99 (88) = happyGoto action_24
action_99 (89) = happyGoto action_25
action_99 (90) = happyGoto action_26
action_99 (91) = happyGoto action_27
action_99 (92) = happyGoto action_28
action_99 (93) = happyGoto action_29
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (118) = happyShift action_30
action_100 (120) = happyShift action_31
action_100 (122) = happyShift action_32
action_100 (127) = happyShift action_33
action_100 (128) = happyShift action_34
action_100 (129) = happyShift action_35
action_100 (132) = happyShift action_36
action_100 (139) = happyShift action_37
action_100 (147) = happyShift action_38
action_100 (148) = happyShift action_39
action_100 (158) = happyShift action_181
action_100 (160) = happyShift action_41
action_100 (161) = happyShift action_182
action_100 (164) = happyShift action_43
action_100 (165) = happyShift action_44
action_100 (166) = happyShift action_45
action_100 (167) = happyShift action_46
action_100 (62) = happyGoto action_178
action_100 (63) = happyGoto action_9
action_100 (64) = happyGoto action_10
action_100 (65) = happyGoto action_11
action_100 (66) = happyGoto action_12
action_100 (67) = happyGoto action_13
action_100 (68) = happyGoto action_14
action_100 (69) = happyGoto action_15
action_100 (71) = happyGoto action_16
action_100 (72) = happyGoto action_17
action_100 (73) = happyGoto action_18
action_100 (75) = happyGoto action_19
action_100 (76) = happyGoto action_20
action_100 (79) = happyGoto action_21
action_100 (80) = happyGoto action_22
action_100 (81) = happyGoto action_179
action_100 (82) = happyGoto action_180
action_100 (83) = happyGoto action_23
action_100 (88) = happyGoto action_24
action_100 (89) = happyGoto action_25
action_100 (90) = happyGoto action_26
action_100 (91) = happyGoto action_27
action_100 (92) = happyGoto action_28
action_100 (93) = happyGoto action_29
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (133) = happyShift action_177
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (130) = happyShift action_175
action_102 (133) = happyShift action_176
action_102 (15) = happyGoto action_173
action_102 (16) = happyGoto action_174
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_166

action_104 _ = happyReduce_167

action_105 _ = happyReduce_207

action_106 (132) = happyShift action_172
action_106 (86) = happyGoto action_170
action_106 (87) = happyGoto action_171
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (132) = happyShift action_167
action_107 (133) = happyShift action_168
action_107 (85) = happyGoto action_169
action_107 _ = happyReduce_211

action_108 (132) = happyShift action_167
action_108 (133) = happyShift action_168
action_108 (85) = happyGoto action_166
action_108 _ = happyReduce_211

action_109 _ = happyReduce_184

action_110 (135) = happyShift action_164
action_110 (161) = happyShift action_165
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_281

action_112 (123) = happyShift action_163
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (123) = happyShift action_161
action_113 (126) = happyShift action_162
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_195

action_115 (133) = happyShift action_160
action_115 _ = happyReduce_217

action_116 _ = happyReduce_191

action_117 (121) = happyShift action_158
action_117 (126) = happyShift action_159
action_117 _ = happyFail (happyExpListPerState 117)

action_118 _ = happyReduce_198

action_119 (119) = happyShift action_156
action_119 (126) = happyShift action_157
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (119) = happyShift action_155
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_185

action_122 _ = happyReduce_268

action_123 _ = happyReduce_269

action_124 (119) = happyShift action_154
action_124 _ = happyFail (happyExpListPerState 124)

action_125 _ = happyReduce_267

action_126 (119) = happyShift action_153
action_126 (164) = happyShift action_103
action_126 (165) = happyShift action_104
action_126 _ = happyFail (happyExpListPerState 126)

action_127 _ = happyReduce_266

action_128 (118) = happyShift action_30
action_128 (120) = happyShift action_31
action_128 (122) = happyShift action_32
action_128 (128) = happyShift action_34
action_128 (129) = happyShift action_35
action_128 (132) = happyShift action_36
action_128 (147) = happyShift action_38
action_128 (148) = happyShift action_39
action_128 (160) = happyShift action_41
action_128 (161) = happyShift action_42
action_128 (164) = happyShift action_43
action_128 (165) = happyShift action_44
action_128 (166) = happyShift action_45
action_128 (167) = happyShift action_46
action_128 (71) = happyGoto action_152
action_128 (72) = happyGoto action_17
action_128 (73) = happyGoto action_18
action_128 (75) = happyGoto action_19
action_128 (76) = happyGoto action_20
action_128 (79) = happyGoto action_21
action_128 (80) = happyGoto action_22
action_128 (83) = happyGoto action_23
action_128 (88) = happyGoto action_24
action_128 (89) = happyGoto action_25
action_128 (90) = happyGoto action_26
action_128 (91) = happyGoto action_27
action_128 (92) = happyGoto action_28
action_128 (93) = happyGoto action_29
action_128 _ = happyReduce_169

action_129 _ = happyReduce_170

action_130 (118) = happyShift action_30
action_130 (120) = happyShift action_31
action_130 (122) = happyShift action_32
action_130 (127) = happyShift action_33
action_130 (128) = happyShift action_34
action_130 (129) = happyShift action_35
action_130 (132) = happyShift action_36
action_130 (139) = happyShift action_37
action_130 (147) = happyShift action_38
action_130 (148) = happyShift action_39
action_130 (158) = happyShift action_40
action_130 (160) = happyShift action_41
action_130 (161) = happyShift action_42
action_130 (164) = happyShift action_43
action_130 (165) = happyShift action_44
action_130 (166) = happyShift action_45
action_130 (167) = happyShift action_46
action_130 (62) = happyGoto action_151
action_130 (63) = happyGoto action_9
action_130 (64) = happyGoto action_10
action_130 (65) = happyGoto action_11
action_130 (66) = happyGoto action_12
action_130 (67) = happyGoto action_13
action_130 (68) = happyGoto action_14
action_130 (69) = happyGoto action_15
action_130 (71) = happyGoto action_16
action_130 (72) = happyGoto action_17
action_130 (73) = happyGoto action_18
action_130 (75) = happyGoto action_19
action_130 (76) = happyGoto action_20
action_130 (79) = happyGoto action_21
action_130 (80) = happyGoto action_22
action_130 (83) = happyGoto action_23
action_130 (88) = happyGoto action_24
action_130 (89) = happyGoto action_25
action_130 (90) = happyGoto action_26
action_130 (91) = happyGoto action_27
action_130 (92) = happyGoto action_28
action_130 (93) = happyGoto action_29
action_130 _ = happyFail (happyExpListPerState 130)

action_131 (118) = happyShift action_30
action_131 (120) = happyShift action_31
action_131 (122) = happyShift action_32
action_131 (127) = happyShift action_33
action_131 (128) = happyShift action_34
action_131 (129) = happyShift action_35
action_131 (132) = happyShift action_36
action_131 (139) = happyShift action_37
action_131 (147) = happyShift action_38
action_131 (148) = happyShift action_39
action_131 (158) = happyShift action_40
action_131 (160) = happyShift action_41
action_131 (161) = happyShift action_42
action_131 (164) = happyShift action_43
action_131 (165) = happyShift action_44
action_131 (166) = happyShift action_45
action_131 (167) = happyShift action_46
action_131 (62) = happyGoto action_150
action_131 (63) = happyGoto action_9
action_131 (64) = happyGoto action_10
action_131 (65) = happyGoto action_11
action_131 (66) = happyGoto action_12
action_131 (67) = happyGoto action_13
action_131 (68) = happyGoto action_14
action_131 (69) = happyGoto action_15
action_131 (71) = happyGoto action_16
action_131 (72) = happyGoto action_17
action_131 (73) = happyGoto action_18
action_131 (75) = happyGoto action_19
action_131 (76) = happyGoto action_20
action_131 (79) = happyGoto action_21
action_131 (80) = happyGoto action_22
action_131 (83) = happyGoto action_23
action_131 (88) = happyGoto action_24
action_131 (89) = happyGoto action_25
action_131 (90) = happyGoto action_26
action_131 (91) = happyGoto action_27
action_131 (92) = happyGoto action_28
action_131 (93) = happyGoto action_29
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (118) = happyShift action_30
action_132 (120) = happyShift action_31
action_132 (122) = happyShift action_32
action_132 (127) = happyShift action_33
action_132 (128) = happyShift action_34
action_132 (129) = happyShift action_35
action_132 (132) = happyShift action_36
action_132 (139) = happyShift action_37
action_132 (147) = happyShift action_38
action_132 (148) = happyShift action_39
action_132 (158) = happyShift action_40
action_132 (160) = happyShift action_41
action_132 (161) = happyShift action_42
action_132 (164) = happyShift action_43
action_132 (165) = happyShift action_44
action_132 (166) = happyShift action_45
action_132 (167) = happyShift action_46
action_132 (62) = happyGoto action_149
action_132 (63) = happyGoto action_9
action_132 (64) = happyGoto action_10
action_132 (65) = happyGoto action_11
action_132 (66) = happyGoto action_12
action_132 (67) = happyGoto action_13
action_132 (68) = happyGoto action_14
action_132 (69) = happyGoto action_15
action_132 (71) = happyGoto action_16
action_132 (72) = happyGoto action_17
action_132 (73) = happyGoto action_18
action_132 (75) = happyGoto action_19
action_132 (76) = happyGoto action_20
action_132 (79) = happyGoto action_21
action_132 (80) = happyGoto action_22
action_132 (83) = happyGoto action_23
action_132 (88) = happyGoto action_24
action_132 (89) = happyGoto action_25
action_132 (90) = happyGoto action_26
action_132 (91) = happyGoto action_27
action_132 (92) = happyGoto action_28
action_132 (93) = happyGoto action_29
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (118) = happyShift action_53
action_133 (120) = happyShift action_54
action_133 (122) = happyShift action_55
action_133 (124) = happyShift action_56
action_133 (161) = happyShift action_57
action_133 (162) = happyShift action_58
action_133 (166) = happyShift action_59
action_133 (95) = happyGoto action_148
action_133 (96) = happyGoto action_49
action_133 (97) = happyGoto action_50
action_133 (98) = happyGoto action_51
action_133 (99) = happyGoto action_52
action_133 _ = happyFail (happyExpListPerState 133)

action_134 _ = happyReduce_156

action_135 (118) = happyShift action_30
action_135 (120) = happyShift action_31
action_135 (122) = happyShift action_32
action_135 (127) = happyShift action_33
action_135 (128) = happyShift action_34
action_135 (129) = happyShift action_35
action_135 (132) = happyShift action_36
action_135 (139) = happyShift action_37
action_135 (147) = happyShift action_38
action_135 (148) = happyShift action_39
action_135 (158) = happyShift action_40
action_135 (160) = happyShift action_41
action_135 (161) = happyShift action_42
action_135 (164) = happyShift action_43
action_135 (165) = happyShift action_44
action_135 (166) = happyShift action_45
action_135 (167) = happyShift action_46
action_135 (62) = happyGoto action_147
action_135 (63) = happyGoto action_9
action_135 (64) = happyGoto action_10
action_135 (65) = happyGoto action_11
action_135 (66) = happyGoto action_12
action_135 (67) = happyGoto action_13
action_135 (68) = happyGoto action_14
action_135 (69) = happyGoto action_15
action_135 (71) = happyGoto action_16
action_135 (72) = happyGoto action_17
action_135 (73) = happyGoto action_18
action_135 (75) = happyGoto action_19
action_135 (76) = happyGoto action_20
action_135 (79) = happyGoto action_21
action_135 (80) = happyGoto action_22
action_135 (83) = happyGoto action_23
action_135 (88) = happyGoto action_24
action_135 (89) = happyGoto action_25
action_135 (90) = happyGoto action_26
action_135 (91) = happyGoto action_27
action_135 (92) = happyGoto action_28
action_135 (93) = happyGoto action_29
action_135 _ = happyFail (happyExpListPerState 135)

action_136 _ = happyReduce_6

action_137 (118) = happyShift action_146
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (131) = happyShift action_144
action_138 (132) = happyShift action_145
action_138 _ = happyReduce_27

action_139 (139) = happyShift action_143
action_139 _ = happyReduce_28

action_140 _ = happyReduce_31

action_141 _ = happyReduce_8

action_142 _ = happyReduce_3

action_143 (161) = happyShift action_288
action_143 _ = happyFail (happyExpListPerState 143)

action_144 (161) = happyShift action_140
action_144 (19) = happyGoto action_287
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (161) = happyShift action_140
action_145 (19) = happyGoto action_286
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (118) = happyShift action_282
action_146 (138) = happyShift action_283
action_146 (161) = happyShift action_284
action_146 (162) = happyShift action_285
action_146 (20) = happyGoto action_278
action_146 (21) = happyGoto action_279
action_146 (22) = happyGoto action_280
action_146 (23) = happyGoto action_281
action_146 _ = happyFail (happyExpListPerState 146)

action_147 _ = happyReduce_154

action_148 _ = happyReduce_153

action_149 _ = happyReduce_163

action_150 _ = happyReduce_164

action_151 _ = happyReduce_162

action_152 _ = happyReduce_171

action_153 _ = happyReduce_187

action_154 _ = happyReduce_188

action_155 _ = happyReduce_186

action_156 _ = happyReduce_189

action_157 (118) = happyShift action_30
action_157 (120) = happyShift action_31
action_157 (122) = happyShift action_32
action_157 (127) = happyShift action_33
action_157 (128) = happyShift action_34
action_157 (129) = happyShift action_35
action_157 (132) = happyShift action_36
action_157 (139) = happyShift action_37
action_157 (147) = happyShift action_38
action_157 (148) = happyShift action_39
action_157 (158) = happyShift action_40
action_157 (160) = happyShift action_41
action_157 (161) = happyShift action_42
action_157 (164) = happyShift action_43
action_157 (165) = happyShift action_44
action_157 (166) = happyShift action_45
action_157 (167) = happyShift action_46
action_157 (62) = happyGoto action_116
action_157 (63) = happyGoto action_9
action_157 (64) = happyGoto action_10
action_157 (65) = happyGoto action_11
action_157 (66) = happyGoto action_12
action_157 (67) = happyGoto action_13
action_157 (68) = happyGoto action_14
action_157 (69) = happyGoto action_15
action_157 (71) = happyGoto action_16
action_157 (72) = happyGoto action_17
action_157 (73) = happyGoto action_18
action_157 (74) = happyGoto action_277
action_157 (75) = happyGoto action_19
action_157 (76) = happyGoto action_20
action_157 (79) = happyGoto action_21
action_157 (80) = happyGoto action_22
action_157 (83) = happyGoto action_23
action_157 (88) = happyGoto action_24
action_157 (89) = happyGoto action_25
action_157 (90) = happyGoto action_26
action_157 (91) = happyGoto action_27
action_157 (92) = happyGoto action_28
action_157 (93) = happyGoto action_29
action_157 _ = happyFail (happyExpListPerState 157)

action_158 _ = happyReduce_199

action_159 (118) = happyShift action_30
action_159 (120) = happyShift action_31
action_159 (122) = happyShift action_32
action_159 (127) = happyShift action_33
action_159 (128) = happyShift action_34
action_159 (129) = happyShift action_35
action_159 (132) = happyShift action_36
action_159 (139) = happyShift action_37
action_159 (147) = happyShift action_38
action_159 (148) = happyShift action_39
action_159 (158) = happyShift action_40
action_159 (160) = happyShift action_41
action_159 (161) = happyShift action_42
action_159 (164) = happyShift action_43
action_159 (165) = happyShift action_44
action_159 (166) = happyShift action_45
action_159 (167) = happyShift action_46
action_159 (62) = happyGoto action_276
action_159 (63) = happyGoto action_9
action_159 (64) = happyGoto action_10
action_159 (65) = happyGoto action_11
action_159 (66) = happyGoto action_12
action_159 (67) = happyGoto action_13
action_159 (68) = happyGoto action_14
action_159 (69) = happyGoto action_15
action_159 (71) = happyGoto action_16
action_159 (72) = happyGoto action_17
action_159 (73) = happyGoto action_18
action_159 (75) = happyGoto action_19
action_159 (76) = happyGoto action_20
action_159 (79) = happyGoto action_21
action_159 (80) = happyGoto action_22
action_159 (83) = happyGoto action_23
action_159 (88) = happyGoto action_24
action_159 (89) = happyGoto action_25
action_159 (90) = happyGoto action_26
action_159 (91) = happyGoto action_27
action_159 (92) = happyGoto action_28
action_159 (93) = happyGoto action_29
action_159 _ = happyFail (happyExpListPerState 159)

action_160 (118) = happyShift action_30
action_160 (120) = happyShift action_31
action_160 (122) = happyShift action_32
action_160 (127) = happyShift action_33
action_160 (128) = happyShift action_34
action_160 (129) = happyShift action_35
action_160 (132) = happyShift action_36
action_160 (139) = happyShift action_37
action_160 (147) = happyShift action_38
action_160 (148) = happyShift action_39
action_160 (158) = happyShift action_40
action_160 (160) = happyShift action_41
action_160 (161) = happyShift action_42
action_160 (164) = happyShift action_43
action_160 (165) = happyShift action_44
action_160 (166) = happyShift action_45
action_160 (167) = happyShift action_46
action_160 (62) = happyGoto action_275
action_160 (63) = happyGoto action_9
action_160 (64) = happyGoto action_10
action_160 (65) = happyGoto action_11
action_160 (66) = happyGoto action_12
action_160 (67) = happyGoto action_13
action_160 (68) = happyGoto action_14
action_160 (69) = happyGoto action_15
action_160 (71) = happyGoto action_16
action_160 (72) = happyGoto action_17
action_160 (73) = happyGoto action_18
action_160 (75) = happyGoto action_19
action_160 (76) = happyGoto action_20
action_160 (79) = happyGoto action_21
action_160 (80) = happyGoto action_22
action_160 (83) = happyGoto action_23
action_160 (88) = happyGoto action_24
action_160 (89) = happyGoto action_25
action_160 (90) = happyGoto action_26
action_160 (91) = happyGoto action_27
action_160 (92) = happyGoto action_28
action_160 (93) = happyGoto action_29
action_160 _ = happyFail (happyExpListPerState 160)

action_161 _ = happyReduce_194

action_162 (161) = happyShift action_274
action_162 (78) = happyGoto action_273
action_162 _ = happyFail (happyExpListPerState 162)

action_163 _ = happyReduce_193

action_164 (118) = happyShift action_30
action_164 (120) = happyShift action_31
action_164 (122) = happyShift action_32
action_164 (127) = happyShift action_33
action_164 (128) = happyShift action_34
action_164 (129) = happyShift action_35
action_164 (132) = happyShift action_36
action_164 (139) = happyShift action_37
action_164 (147) = happyShift action_38
action_164 (148) = happyShift action_39
action_164 (158) = happyShift action_40
action_164 (160) = happyShift action_41
action_164 (161) = happyShift action_42
action_164 (164) = happyShift action_43
action_164 (165) = happyShift action_44
action_164 (166) = happyShift action_45
action_164 (167) = happyShift action_46
action_164 (62) = happyGoto action_272
action_164 (63) = happyGoto action_9
action_164 (64) = happyGoto action_10
action_164 (65) = happyGoto action_11
action_164 (66) = happyGoto action_12
action_164 (67) = happyGoto action_13
action_164 (68) = happyGoto action_14
action_164 (69) = happyGoto action_15
action_164 (71) = happyGoto action_16
action_164 (72) = happyGoto action_17
action_164 (73) = happyGoto action_18
action_164 (75) = happyGoto action_19
action_164 (76) = happyGoto action_20
action_164 (79) = happyGoto action_21
action_164 (80) = happyGoto action_22
action_164 (83) = happyGoto action_23
action_164 (88) = happyGoto action_24
action_164 (89) = happyGoto action_25
action_164 (90) = happyGoto action_26
action_164 (91) = happyGoto action_27
action_164 (92) = happyGoto action_28
action_164 (93) = happyGoto action_29
action_164 _ = happyFail (happyExpListPerState 164)

action_165 _ = happyReduce_282

action_166 _ = happyReduce_209

action_167 (118) = happyShift action_106
action_167 (161) = happyShift action_107
action_167 (164) = happyShift action_108
action_167 (84) = happyGoto action_271
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (118) = happyShift action_30
action_168 (120) = happyShift action_31
action_168 (122) = happyShift action_32
action_168 (127) = happyShift action_33
action_168 (128) = happyShift action_34
action_168 (129) = happyShift action_35
action_168 (132) = happyShift action_36
action_168 (139) = happyShift action_37
action_168 (147) = happyShift action_38
action_168 (148) = happyShift action_39
action_168 (158) = happyShift action_40
action_168 (160) = happyShift action_41
action_168 (161) = happyShift action_42
action_168 (164) = happyShift action_43
action_168 (165) = happyShift action_44
action_168 (166) = happyShift action_45
action_168 (167) = happyShift action_46
action_168 (62) = happyGoto action_270
action_168 (63) = happyGoto action_9
action_168 (64) = happyGoto action_10
action_168 (65) = happyGoto action_11
action_168 (66) = happyGoto action_12
action_168 (67) = happyGoto action_13
action_168 (68) = happyGoto action_14
action_168 (69) = happyGoto action_15
action_168 (71) = happyGoto action_16
action_168 (72) = happyGoto action_17
action_168 (73) = happyGoto action_18
action_168 (75) = happyGoto action_19
action_168 (76) = happyGoto action_20
action_168 (79) = happyGoto action_21
action_168 (80) = happyGoto action_22
action_168 (83) = happyGoto action_23
action_168 (88) = happyGoto action_24
action_168 (89) = happyGoto action_25
action_168 (90) = happyGoto action_26
action_168 (91) = happyGoto action_27
action_168 (92) = happyGoto action_28
action_168 (93) = happyGoto action_29
action_168 _ = happyFail (happyExpListPerState 168)

action_169 _ = happyReduce_208

action_170 (119) = happyShift action_268
action_170 (126) = happyShift action_269
action_170 _ = happyFail (happyExpListPerState 170)

action_171 _ = happyReduce_214

action_172 (118) = happyShift action_106
action_172 (161) = happyShift action_107
action_172 (164) = happyShift action_108
action_172 (84) = happyGoto action_267
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (130) = happyShift action_175
action_173 (16) = happyGoto action_266
action_173 _ = happyReduce_159

action_174 _ = happyReduce_24

action_175 (118) = happyShift action_30
action_175 (120) = happyShift action_31
action_175 (122) = happyShift action_32
action_175 (127) = happyShift action_33
action_175 (128) = happyShift action_34
action_175 (129) = happyShift action_35
action_175 (132) = happyShift action_36
action_175 (139) = happyShift action_37
action_175 (147) = happyShift action_38
action_175 (148) = happyShift action_39
action_175 (158) = happyShift action_40
action_175 (160) = happyShift action_41
action_175 (161) = happyShift action_42
action_175 (164) = happyShift action_43
action_175 (165) = happyShift action_44
action_175 (166) = happyShift action_45
action_175 (167) = happyShift action_46
action_175 (62) = happyGoto action_265
action_175 (63) = happyGoto action_9
action_175 (64) = happyGoto action_10
action_175 (65) = happyGoto action_11
action_175 (66) = happyGoto action_12
action_175 (67) = happyGoto action_13
action_175 (68) = happyGoto action_14
action_175 (69) = happyGoto action_15
action_175 (71) = happyGoto action_16
action_175 (72) = happyGoto action_17
action_175 (73) = happyGoto action_18
action_175 (75) = happyGoto action_19
action_175 (76) = happyGoto action_20
action_175 (79) = happyGoto action_21
action_175 (80) = happyGoto action_22
action_175 (83) = happyGoto action_23
action_175 (88) = happyGoto action_24
action_175 (89) = happyGoto action_25
action_175 (90) = happyGoto action_26
action_175 (91) = happyGoto action_27
action_175 (92) = happyGoto action_28
action_175 (93) = happyGoto action_29
action_175 _ = happyFail (happyExpListPerState 175)

action_176 (118) = happyShift action_30
action_176 (120) = happyShift action_31
action_176 (122) = happyShift action_32
action_176 (127) = happyShift action_33
action_176 (128) = happyShift action_34
action_176 (129) = happyShift action_35
action_176 (132) = happyShift action_36
action_176 (139) = happyShift action_37
action_176 (147) = happyShift action_38
action_176 (148) = happyShift action_39
action_176 (158) = happyShift action_40
action_176 (160) = happyShift action_41
action_176 (161) = happyShift action_42
action_176 (164) = happyShift action_43
action_176 (165) = happyShift action_44
action_176 (166) = happyShift action_45
action_176 (167) = happyShift action_46
action_176 (62) = happyGoto action_264
action_176 (63) = happyGoto action_9
action_176 (64) = happyGoto action_10
action_176 (65) = happyGoto action_11
action_176 (66) = happyGoto action_12
action_176 (67) = happyGoto action_13
action_176 (68) = happyGoto action_14
action_176 (69) = happyGoto action_15
action_176 (71) = happyGoto action_16
action_176 (72) = happyGoto action_17
action_176 (73) = happyGoto action_18
action_176 (75) = happyGoto action_19
action_176 (76) = happyGoto action_20
action_176 (79) = happyGoto action_21
action_176 (80) = happyGoto action_22
action_176 (83) = happyGoto action_23
action_176 (88) = happyGoto action_24
action_176 (89) = happyGoto action_25
action_176 (90) = happyGoto action_26
action_176 (91) = happyGoto action_27
action_176 (92) = happyGoto action_28
action_176 (93) = happyGoto action_29
action_176 _ = happyFail (happyExpListPerState 176)

action_177 (118) = happyShift action_30
action_177 (120) = happyShift action_31
action_177 (122) = happyShift action_32
action_177 (127) = happyShift action_33
action_177 (128) = happyShift action_34
action_177 (129) = happyShift action_35
action_177 (132) = happyShift action_36
action_177 (139) = happyShift action_37
action_177 (147) = happyShift action_38
action_177 (148) = happyShift action_39
action_177 (158) = happyShift action_40
action_177 (160) = happyShift action_41
action_177 (161) = happyShift action_42
action_177 (164) = happyShift action_43
action_177 (165) = happyShift action_44
action_177 (166) = happyShift action_45
action_177 (167) = happyShift action_46
action_177 (62) = happyGoto action_263
action_177 (63) = happyGoto action_9
action_177 (64) = happyGoto action_10
action_177 (65) = happyGoto action_11
action_177 (66) = happyGoto action_12
action_177 (67) = happyGoto action_13
action_177 (68) = happyGoto action_14
action_177 (69) = happyGoto action_15
action_177 (71) = happyGoto action_16
action_177 (72) = happyGoto action_17
action_177 (73) = happyGoto action_18
action_177 (75) = happyGoto action_19
action_177 (76) = happyGoto action_20
action_177 (79) = happyGoto action_21
action_177 (80) = happyGoto action_22
action_177 (83) = happyGoto action_23
action_177 (88) = happyGoto action_24
action_177 (89) = happyGoto action_25
action_177 (90) = happyGoto action_26
action_177 (91) = happyGoto action_27
action_177 (92) = happyGoto action_28
action_177 (93) = happyGoto action_29
action_177 _ = happyFail (happyExpListPerState 177)

action_178 _ = happyReduce_206

action_179 (116) = happyShift action_261
action_179 (117) = happyShift action_262
action_179 _ = happyFail (happyExpListPerState 179)

action_180 _ = happyReduce_201

action_181 (128) = happyShift action_101
action_181 (161) = happyShift action_260
action_181 _ = happyFail (happyExpListPerState 181)

action_182 (137) = happyShift action_259
action_182 _ = happyReduce_217

action_183 (171) = happyShift action_258
action_183 _ = happyFail (happyExpListPerState 183)

action_184 (170) = happyShift action_257
action_184 _ = happyFail (happyExpListPerState 184)

action_185 _ = happyReduce_225

action_186 _ = happyReduce_230

action_187 _ = happyReduce_236

action_188 (118) = happyShift action_53
action_188 (120) = happyShift action_54
action_188 (122) = happyShift action_55
action_188 (124) = happyShift action_56
action_188 (161) = happyShift action_57
action_188 (162) = happyShift action_58
action_188 (166) = happyShift action_59
action_188 (95) = happyGoto action_255
action_188 (96) = happyGoto action_49
action_188 (97) = happyGoto action_50
action_188 (98) = happyGoto action_51
action_188 (99) = happyGoto action_52
action_188 (100) = happyGoto action_256
action_188 _ = happyFail (happyExpListPerState 188)

action_189 _ = happyReduce_238

action_190 _ = happyReduce_239

action_191 _ = happyReduce_231

action_192 _ = happyReduce_241

action_193 (118) = happyShift action_249
action_193 (124) = happyShift action_122
action_193 (125) = happyShift action_123
action_193 (131) = happyShift action_250
action_193 (138) = happyShift action_125
action_193 (139) = happyShift action_251
action_193 (161) = happyShift action_252
action_193 (163) = happyShift action_127
action_193 (54) = happyGoto action_254
action_193 (55) = happyGoto action_247
action_193 (108) = happyGoto action_248
action_193 _ = happyFail (happyExpListPerState 193)

action_194 (118) = happyShift action_249
action_194 (124) = happyShift action_122
action_194 (125) = happyShift action_123
action_194 (131) = happyShift action_250
action_194 (138) = happyShift action_125
action_194 (139) = happyShift action_251
action_194 (161) = happyShift action_252
action_194 (163) = happyShift action_127
action_194 (54) = happyGoto action_253
action_194 (55) = happyGoto action_247
action_194 (108) = happyGoto action_248
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (118) = happyShift action_249
action_195 (124) = happyShift action_122
action_195 (125) = happyShift action_123
action_195 (131) = happyShift action_250
action_195 (138) = happyShift action_125
action_195 (139) = happyShift action_251
action_195 (161) = happyShift action_252
action_195 (163) = happyShift action_127
action_195 (54) = happyGoto action_246
action_195 (55) = happyGoto action_247
action_195 (108) = happyGoto action_248
action_195 _ = happyFail (happyExpListPerState 195)

action_196 (118) = happyShift action_53
action_196 (120) = happyShift action_54
action_196 (122) = happyShift action_55
action_196 (161) = happyShift action_57
action_196 (162) = happyShift action_58
action_196 (166) = happyShift action_59
action_196 (99) = happyGoto action_244
action_196 (101) = happyGoto action_245
action_196 _ = happyFail (happyExpListPerState 196)

action_197 (145) = happyShift action_243
action_197 _ = happyReduce_108

action_198 (118) = happyShift action_53
action_198 (120) = happyShift action_54
action_198 (122) = happyShift action_55
action_198 (136) = happyShift action_242
action_198 (161) = happyShift action_57
action_198 (162) = happyShift action_58
action_198 (166) = happyShift action_59
action_198 (99) = happyGoto action_95
action_198 _ = happyReduce_111

action_199 (118) = happyShift action_53
action_199 (119) = happyShift action_94
action_199 (120) = happyShift action_54
action_199 (122) = happyShift action_55
action_199 (124) = happyShift action_56
action_199 (161) = happyShift action_57
action_199 (162) = happyShift action_241
action_199 (166) = happyShift action_59
action_199 (47) = happyGoto action_239
action_199 (95) = happyGoto action_93
action_199 (96) = happyGoto action_49
action_199 (97) = happyGoto action_50
action_199 (98) = happyGoto action_51
action_199 (99) = happyGoto action_52
action_199 (107) = happyGoto action_240
action_199 _ = happyFail (happyExpListPerState 199)

action_200 (162) = happyShift action_238
action_200 _ = happyFail (happyExpListPerState 200)

action_201 (136) = happyShift action_237
action_201 _ = happyFail (happyExpListPerState 201)

action_202 (136) = happyShift action_236
action_202 (34) = happyGoto action_235
action_202 _ = happyReduce_80

action_203 (144) = happyShift action_234
action_203 (57) = happyGoto action_233
action_203 _ = happyReduce_137

action_204 _ = happyReduce_77

action_205 _ = happyReduce_76

action_206 (118) = happyShift action_232
action_206 (25) = happyGoto action_231
action_206 _ = happyReduce_44

action_207 (119) = happyShift action_230
action_207 _ = happyFail (happyExpListPerState 207)

action_208 (119) = happyShift action_229
action_208 _ = happyFail (happyExpListPerState 208)

action_209 (119) = happyShift action_228
action_209 _ = happyFail (happyExpListPerState 209)

action_210 (130) = happyShift action_175
action_210 (133) = happyShift action_225
action_210 (134) = happyShift action_226
action_210 (161) = happyShift action_227
action_210 (15) = happyGoto action_224
action_210 (16) = happyGoto action_174
action_210 _ = happyFail (happyExpListPerState 210)

action_211 (133) = happyShift action_222
action_211 (145) = happyShift action_223
action_211 _ = happyFail (happyExpListPerState 211)

action_212 (162) = happyShift action_221
action_212 _ = happyFail (happyExpListPerState 212)

action_213 (136) = happyShift action_220
action_213 _ = happyFail (happyExpListPerState 213)

action_214 (136) = happyShift action_219
action_214 (34) = happyGoto action_218
action_214 _ = happyReduce_80

action_215 _ = happyReduce_10

action_216 (118) = happyShift action_75
action_216 (142) = happyShift action_76
action_216 (143) = happyShift action_77
action_216 (149) = happyShift action_78
action_216 (150) = happyShift action_79
action_216 (151) = happyShift action_80
action_216 (152) = happyShift action_81
action_216 (153) = happyShift action_82
action_216 (154) = happyShift action_83
action_216 (155) = happyShift action_84
action_216 (156) = happyShift action_85
action_216 (157) = happyShift action_86
action_216 (161) = happyShift action_87
action_216 (13) = happyGoto action_217
action_216 (14) = happyGoto action_65
action_216 (24) = happyGoto action_66
action_216 (28) = happyGoto action_67
action_216 (29) = happyGoto action_68
action_216 (45) = happyGoto action_69
action_216 (50) = happyGoto action_70
action_216 (53) = happyGoto action_71
action_216 (56) = happyGoto action_72
action_216 (109) = happyGoto action_73
action_216 _ = happyFail (happyExpListPerState 216)

action_217 _ = happyReduce_13

action_218 (118) = happyShift action_319
action_218 (161) = happyShift action_321
action_218 _ = happyReduce_78

action_219 (118) = happyShift action_212
action_219 (162) = happyShift action_317
action_219 (33) = happyGoto action_352
action_219 _ = happyFail (happyExpListPerState 219)

action_220 (118) = happyShift action_212
action_220 (162) = happyShift action_317
action_220 (33) = happyGoto action_351
action_220 _ = happyFail (happyExpListPerState 220)

action_221 (34) = happyGoto action_350
action_221 _ = happyReduce_80

action_222 (161) = happyShift action_347
action_222 (162) = happyShift action_348
action_222 (166) = happyShift action_349
action_222 (30) = happyGoto action_346
action_222 _ = happyFail (happyExpListPerState 222)

action_223 (115) = happyShift action_345
action_223 _ = happyFail (happyExpListPerState 223)

action_224 (130) = happyShift action_175
action_224 (145) = happyShift action_344
action_224 (16) = happyGoto action_266
action_224 (110) = happyGoto action_343
action_224 _ = happyReduce_274

action_225 (118) = happyShift action_30
action_225 (120) = happyShift action_31
action_225 (122) = happyShift action_32
action_225 (127) = happyShift action_33
action_225 (128) = happyShift action_34
action_225 (129) = happyShift action_35
action_225 (132) = happyShift action_36
action_225 (139) = happyShift action_37
action_225 (147) = happyShift action_38
action_225 (148) = happyShift action_39
action_225 (158) = happyShift action_40
action_225 (160) = happyShift action_41
action_225 (161) = happyShift action_42
action_225 (164) = happyShift action_43
action_225 (165) = happyShift action_44
action_225 (166) = happyShift action_45
action_225 (167) = happyShift action_46
action_225 (62) = happyGoto action_342
action_225 (63) = happyGoto action_9
action_225 (64) = happyGoto action_10
action_225 (65) = happyGoto action_11
action_225 (66) = happyGoto action_12
action_225 (67) = happyGoto action_13
action_225 (68) = happyGoto action_14
action_225 (69) = happyGoto action_15
action_225 (71) = happyGoto action_16
action_225 (72) = happyGoto action_17
action_225 (73) = happyGoto action_18
action_225 (75) = happyGoto action_19
action_225 (76) = happyGoto action_20
action_225 (79) = happyGoto action_21
action_225 (80) = happyGoto action_22
action_225 (83) = happyGoto action_23
action_225 (88) = happyGoto action_24
action_225 (89) = happyGoto action_25
action_225 (90) = happyGoto action_26
action_225 (91) = happyGoto action_27
action_225 (92) = happyGoto action_28
action_225 (93) = happyGoto action_29
action_225 _ = happyFail (happyExpListPerState 225)

action_226 (118) = happyShift action_335
action_226 (120) = happyShift action_336
action_226 (122) = happyShift action_337
action_226 (124) = happyShift action_338
action_226 (161) = happyShift action_339
action_226 (162) = happyShift action_340
action_226 (166) = happyShift action_341
action_226 (102) = happyGoto action_330
action_226 (103) = happyGoto action_331
action_226 (104) = happyGoto action_332
action_226 (105) = happyGoto action_333
action_226 (106) = happyGoto action_334
action_226 _ = happyFail (happyExpListPerState 226)

action_227 _ = happyReduce_280

action_228 _ = happyReduce_272

action_229 _ = happyReduce_273

action_230 _ = happyReduce_271

action_231 _ = happyReduce_43

action_232 (118) = happyShift action_327
action_232 (161) = happyShift action_328
action_232 (162) = happyShift action_329
action_232 (26) = happyGoto action_325
action_232 (27) = happyGoto action_326
action_232 _ = happyFail (happyExpListPerState 232)

action_233 (118) = happyShift action_323
action_233 (145) = happyShift action_324
action_233 _ = happyFail (happyExpListPerState 233)

action_234 (166) = happyShift action_322
action_234 _ = happyFail (happyExpListPerState 234)

action_235 (118) = happyShift action_319
action_235 (133) = happyShift action_320
action_235 (161) = happyShift action_321
action_235 _ = happyReduce_61

action_236 (118) = happyShift action_212
action_236 (162) = happyShift action_317
action_236 (33) = happyGoto action_318
action_236 _ = happyFail (happyExpListPerState 236)

action_237 (118) = happyShift action_212
action_237 (162) = happyShift action_317
action_237 (33) = happyGoto action_316
action_237 _ = happyFail (happyExpListPerState 237)

action_238 (34) = happyGoto action_315
action_238 _ = happyReduce_80

action_239 (119) = happyShift action_313
action_239 (126) = happyShift action_314
action_239 _ = happyFail (happyExpListPerState 239)

action_240 _ = happyReduce_112

action_241 (118) = happyShift action_53
action_241 (120) = happyShift action_54
action_241 (122) = happyShift action_55
action_241 (161) = happyShift action_57
action_241 (162) = happyShift action_58
action_241 (166) = happyShift action_59
action_241 (99) = happyGoto action_244
action_241 (101) = happyGoto action_312
action_241 _ = happyReduce_240

action_242 (118) = happyShift action_53
action_242 (120) = happyShift action_54
action_242 (122) = happyShift action_55
action_242 (161) = happyShift action_57
action_242 (162) = happyShift action_58
action_242 (166) = happyShift action_59
action_242 (98) = happyGoto action_311
action_242 (99) = happyGoto action_52
action_242 _ = happyFail (happyExpListPerState 242)

action_243 (115) = happyShift action_310
action_243 _ = happyFail (happyExpListPerState 243)

action_244 _ = happyReduce_246

action_245 (118) = happyShift action_53
action_245 (120) = happyShift action_54
action_245 (122) = happyShift action_55
action_245 (145) = happyShift action_309
action_245 (161) = happyShift action_57
action_245 (162) = happyShift action_58
action_245 (166) = happyShift action_59
action_245 (99) = happyGoto action_308
action_245 _ = happyReduce_118

action_246 (126) = happyShift action_304
action_246 _ = happyReduce_123

action_247 _ = happyReduce_126

action_248 _ = happyReduce_131

action_249 (124) = happyShift action_122
action_249 (125) = happyShift action_123
action_249 (131) = happyShift action_306
action_249 (138) = happyShift action_125
action_249 (139) = happyShift action_307
action_249 (163) = happyShift action_127
action_249 (108) = happyGoto action_305
action_249 _ = happyFail (happyExpListPerState 249)

action_250 _ = happyReduce_132

action_251 _ = happyReduce_133

action_252 _ = happyReduce_134

action_253 (126) = happyShift action_304
action_253 _ = happyReduce_124

action_254 (126) = happyShift action_304
action_254 _ = happyReduce_125

action_255 _ = happyReduce_244

action_256 (119) = happyShift action_302
action_256 (126) = happyShift action_303
action_256 _ = happyFail (happyExpListPerState 256)

action_257 (118) = happyShift action_30
action_257 (120) = happyShift action_31
action_257 (122) = happyShift action_32
action_257 (127) = happyShift action_33
action_257 (128) = happyShift action_34
action_257 (129) = happyShift action_35
action_257 (132) = happyShift action_36
action_257 (139) = happyShift action_37
action_257 (147) = happyShift action_38
action_257 (148) = happyShift action_39
action_257 (158) = happyShift action_40
action_257 (160) = happyShift action_41
action_257 (161) = happyShift action_42
action_257 (164) = happyShift action_43
action_257 (165) = happyShift action_44
action_257 (166) = happyShift action_45
action_257 (167) = happyShift action_46
action_257 (62) = happyGoto action_301
action_257 (63) = happyGoto action_9
action_257 (64) = happyGoto action_10
action_257 (65) = happyGoto action_11
action_257 (66) = happyGoto action_12
action_257 (67) = happyGoto action_13
action_257 (68) = happyGoto action_14
action_257 (69) = happyGoto action_15
action_257 (71) = happyGoto action_16
action_257 (72) = happyGoto action_17
action_257 (73) = happyGoto action_18
action_257 (75) = happyGoto action_19
action_257 (76) = happyGoto action_20
action_257 (79) = happyGoto action_21
action_257 (80) = happyGoto action_22
action_257 (83) = happyGoto action_23
action_257 (88) = happyGoto action_24
action_257 (89) = happyGoto action_25
action_257 (90) = happyGoto action_26
action_257 (91) = happyGoto action_27
action_257 (92) = happyGoto action_28
action_257 (93) = happyGoto action_29
action_257 _ = happyFail (happyExpListPerState 257)

action_258 _ = happyReduce_226

action_259 (118) = happyShift action_30
action_259 (120) = happyShift action_31
action_259 (122) = happyShift action_32
action_259 (127) = happyShift action_33
action_259 (128) = happyShift action_34
action_259 (129) = happyShift action_35
action_259 (132) = happyShift action_36
action_259 (139) = happyShift action_37
action_259 (147) = happyShift action_38
action_259 (148) = happyShift action_39
action_259 (158) = happyShift action_40
action_259 (160) = happyShift action_41
action_259 (161) = happyShift action_42
action_259 (164) = happyShift action_43
action_259 (165) = happyShift action_44
action_259 (166) = happyShift action_45
action_259 (167) = happyShift action_46
action_259 (62) = happyGoto action_300
action_259 (63) = happyGoto action_9
action_259 (64) = happyGoto action_10
action_259 (65) = happyGoto action_11
action_259 (66) = happyGoto action_12
action_259 (67) = happyGoto action_13
action_259 (68) = happyGoto action_14
action_259 (69) = happyGoto action_15
action_259 (71) = happyGoto action_16
action_259 (72) = happyGoto action_17
action_259 (73) = happyGoto action_18
action_259 (75) = happyGoto action_19
action_259 (76) = happyGoto action_20
action_259 (79) = happyGoto action_21
action_259 (80) = happyGoto action_22
action_259 (83) = happyGoto action_23
action_259 (88) = happyGoto action_24
action_259 (89) = happyGoto action_25
action_259 (90) = happyGoto action_26
action_259 (91) = happyGoto action_27
action_259 (92) = happyGoto action_28
action_259 (93) = happyGoto action_29
action_259 _ = happyFail (happyExpListPerState 259)

action_260 (130) = happyShift action_175
action_260 (133) = happyShift action_299
action_260 (15) = happyGoto action_298
action_260 (16) = happyGoto action_174
action_260 _ = happyFail (happyExpListPerState 260)

action_261 _ = happyReduce_200

action_262 (118) = happyShift action_30
action_262 (120) = happyShift action_31
action_262 (122) = happyShift action_32
action_262 (127) = happyShift action_33
action_262 (128) = happyShift action_34
action_262 (129) = happyShift action_35
action_262 (132) = happyShift action_36
action_262 (139) = happyShift action_37
action_262 (147) = happyShift action_38
action_262 (148) = happyShift action_39
action_262 (158) = happyShift action_181
action_262 (160) = happyShift action_41
action_262 (161) = happyShift action_182
action_262 (164) = happyShift action_43
action_262 (165) = happyShift action_44
action_262 (166) = happyShift action_45
action_262 (167) = happyShift action_46
action_262 (62) = happyGoto action_178
action_262 (63) = happyGoto action_9
action_262 (64) = happyGoto action_10
action_262 (65) = happyGoto action_11
action_262 (66) = happyGoto action_12
action_262 (67) = happyGoto action_13
action_262 (68) = happyGoto action_14
action_262 (69) = happyGoto action_15
action_262 (71) = happyGoto action_16
action_262 (72) = happyGoto action_17
action_262 (73) = happyGoto action_18
action_262 (75) = happyGoto action_19
action_262 (76) = happyGoto action_20
action_262 (79) = happyGoto action_21
action_262 (80) = happyGoto action_22
action_262 (82) = happyGoto action_297
action_262 (83) = happyGoto action_23
action_262 (88) = happyGoto action_24
action_262 (89) = happyGoto action_25
action_262 (90) = happyGoto action_26
action_262 (91) = happyGoto action_27
action_262 (92) = happyGoto action_28
action_262 (93) = happyGoto action_29
action_262 _ = happyFail (happyExpListPerState 262)

action_263 _ = happyReduce_158

action_264 _ = happyReduce_157

action_265 (133) = happyShift action_296
action_265 _ = happyFail (happyExpListPerState 265)

action_266 _ = happyReduce_25

action_267 _ = happyReduce_216

action_268 _ = happyReduce_210

action_269 (132) = happyShift action_172
action_269 (87) = happyGoto action_295
action_269 _ = happyFail (happyExpListPerState 269)

action_270 _ = happyReduce_212

action_271 _ = happyReduce_213

action_272 _ = happyReduce_160

action_273 _ = happyReduce_196

action_274 (133) = happyShift action_160
action_274 _ = happyFail (happyExpListPerState 274)

action_275 _ = happyReduce_197

action_276 _ = happyReduce_192

action_277 (119) = happyShift action_294
action_277 (126) = happyShift action_159
action_277 _ = happyFail (happyExpListPerState 277)

action_278 (119) = happyShift action_293
action_278 _ = happyFail (happyExpListPerState 278)

action_279 (126) = happyShift action_292
action_279 _ = happyReduce_34

action_280 _ = happyReduce_35

action_281 _ = happyReduce_37

action_282 (124) = happyShift action_122
action_282 (125) = happyShift action_123
action_282 (131) = happyShift action_290
action_282 (138) = happyShift action_125
action_282 (139) = happyShift action_291
action_282 (163) = happyShift action_127
action_282 (108) = happyGoto action_289
action_282 _ = happyFail (happyExpListPerState 282)

action_283 _ = happyReduce_33

action_284 _ = happyReduce_38

action_285 _ = happyReduce_42

action_286 (139) = happyShift action_143
action_286 _ = happyReduce_30

action_287 (139) = happyShift action_143
action_287 _ = happyReduce_29

action_288 _ = happyReduce_32

action_289 (119) = happyShift action_407
action_289 _ = happyFail (happyExpListPerState 289)

action_290 (119) = happyShift action_406
action_290 _ = happyFail (happyExpListPerState 290)

action_291 (119) = happyShift action_405
action_291 _ = happyFail (happyExpListPerState 291)

action_292 (118) = happyShift action_282
action_292 (161) = happyShift action_284
action_292 (162) = happyShift action_285
action_292 (22) = happyGoto action_404
action_292 (23) = happyGoto action_281
action_292 _ = happyFail (happyExpListPerState 292)

action_293 (115) = happyShift action_62
action_293 (11) = happyGoto action_403
action_293 _ = happyFail (happyExpListPerState 293)

action_294 _ = happyReduce_190

action_295 _ = happyReduce_215

action_296 (118) = happyShift action_30
action_296 (120) = happyShift action_31
action_296 (122) = happyShift action_32
action_296 (127) = happyShift action_33
action_296 (128) = happyShift action_34
action_296 (129) = happyShift action_35
action_296 (132) = happyShift action_36
action_296 (139) = happyShift action_37
action_296 (147) = happyShift action_38
action_296 (148) = happyShift action_39
action_296 (158) = happyShift action_40
action_296 (160) = happyShift action_41
action_296 (161) = happyShift action_42
action_296 (164) = happyShift action_43
action_296 (165) = happyShift action_44
action_296 (166) = happyShift action_45
action_296 (167) = happyShift action_46
action_296 (62) = happyGoto action_402
action_296 (63) = happyGoto action_9
action_296 (64) = happyGoto action_10
action_296 (65) = happyGoto action_11
action_296 (66) = happyGoto action_12
action_296 (67) = happyGoto action_13
action_296 (68) = happyGoto action_14
action_296 (69) = happyGoto action_15
action_296 (71) = happyGoto action_16
action_296 (72) = happyGoto action_17
action_296 (73) = happyGoto action_18
action_296 (75) = happyGoto action_19
action_296 (76) = happyGoto action_20
action_296 (79) = happyGoto action_21
action_296 (80) = happyGoto action_22
action_296 (83) = happyGoto action_23
action_296 (88) = happyGoto action_24
action_296 (89) = happyGoto action_25
action_296 (90) = happyGoto action_26
action_296 (91) = happyGoto action_27
action_296 (92) = happyGoto action_28
action_296 (93) = happyGoto action_29
action_296 _ = happyFail (happyExpListPerState 296)

action_297 _ = happyReduce_202

action_298 (130) = happyShift action_175
action_298 (158) = happyReduce_159
action_298 (159) = happyReduce_159
action_298 (16) = happyGoto action_266
action_298 _ = happyReduce_205

action_299 (118) = happyShift action_30
action_299 (120) = happyShift action_31
action_299 (122) = happyShift action_32
action_299 (127) = happyShift action_33
action_299 (128) = happyShift action_34
action_299 (129) = happyShift action_35
action_299 (132) = happyShift action_36
action_299 (139) = happyShift action_37
action_299 (147) = happyShift action_38
action_299 (148) = happyShift action_39
action_299 (158) = happyShift action_40
action_299 (160) = happyShift action_41
action_299 (161) = happyShift action_42
action_299 (164) = happyShift action_43
action_299 (165) = happyShift action_44
action_299 (166) = happyShift action_45
action_299 (167) = happyShift action_46
action_299 (62) = happyGoto action_401
action_299 (63) = happyGoto action_9
action_299 (64) = happyGoto action_10
action_299 (65) = happyGoto action_11
action_299 (66) = happyGoto action_12
action_299 (67) = happyGoto action_13
action_299 (68) = happyGoto action_14
action_299 (69) = happyGoto action_15
action_299 (71) = happyGoto action_16
action_299 (72) = happyGoto action_17
action_299 (73) = happyGoto action_18
action_299 (75) = happyGoto action_19
action_299 (76) = happyGoto action_20
action_299 (79) = happyGoto action_21
action_299 (80) = happyGoto action_22
action_299 (83) = happyGoto action_23
action_299 (88) = happyGoto action_24
action_299 (89) = happyGoto action_25
action_299 (90) = happyGoto action_26
action_299 (91) = happyGoto action_27
action_299 (92) = happyGoto action_28
action_299 (93) = happyGoto action_29
action_299 _ = happyFail (happyExpListPerState 299)

action_300 _ = happyReduce_203

action_301 (171) = happyShift action_400
action_301 _ = happyFail (happyExpListPerState 301)

action_302 _ = happyReduce_237

action_303 (118) = happyShift action_53
action_303 (120) = happyShift action_54
action_303 (122) = happyShift action_55
action_303 (124) = happyShift action_56
action_303 (161) = happyShift action_57
action_303 (162) = happyShift action_58
action_303 (166) = happyShift action_59
action_303 (95) = happyGoto action_399
action_303 (96) = happyGoto action_49
action_303 (97) = happyGoto action_50
action_303 (98) = happyGoto action_51
action_303 (99) = happyGoto action_52
action_303 _ = happyFail (happyExpListPerState 303)

action_304 (118) = happyShift action_249
action_304 (124) = happyShift action_122
action_304 (125) = happyShift action_123
action_304 (131) = happyShift action_250
action_304 (138) = happyShift action_125
action_304 (139) = happyShift action_251
action_304 (161) = happyShift action_252
action_304 (163) = happyShift action_127
action_304 (55) = happyGoto action_398
action_304 (108) = happyGoto action_248
action_304 _ = happyFail (happyExpListPerState 304)

action_305 (119) = happyShift action_397
action_305 _ = happyFail (happyExpListPerState 305)

action_306 (119) = happyShift action_396
action_306 _ = happyFail (happyExpListPerState 306)

action_307 (119) = happyShift action_395
action_307 _ = happyFail (happyExpListPerState 307)

action_308 _ = happyReduce_247

action_309 (115) = happyShift action_394
action_309 _ = happyFail (happyExpListPerState 309)

action_310 (118) = happyShift action_75
action_310 (161) = happyShift action_87
action_310 (48) = happyGoto action_391
action_310 (49) = happyGoto action_392
action_310 (109) = happyGoto action_393
action_310 _ = happyFail (happyExpListPerState 310)

action_311 (118) = happyShift action_53
action_311 (120) = happyShift action_54
action_311 (122) = happyShift action_55
action_311 (161) = happyShift action_57
action_311 (162) = happyShift action_58
action_311 (166) = happyShift action_59
action_311 (99) = happyGoto action_95
action_311 _ = happyReduce_109

action_312 (118) = happyShift action_53
action_312 (120) = happyShift action_54
action_312 (122) = happyShift action_55
action_312 (161) = happyShift action_57
action_312 (162) = happyShift action_58
action_312 (166) = happyShift action_59
action_312 (99) = happyGoto action_308
action_312 _ = happyReduce_265

action_313 (136) = happyShift action_390
action_313 _ = happyFail (happyExpListPerState 313)

action_314 (162) = happyShift action_389
action_314 (107) = happyGoto action_388
action_314 _ = happyFail (happyExpListPerState 314)

action_315 (118) = happyShift action_319
action_315 (119) = happyShift action_387
action_315 (161) = happyShift action_321
action_315 _ = happyFail (happyExpListPerState 315)

action_316 (133) = happyShift action_386
action_316 _ = happyFail (happyExpListPerState 316)

action_317 (34) = happyGoto action_218
action_317 _ = happyReduce_80

action_318 (133) = happyShift action_385
action_318 _ = happyFail (happyExpListPerState 318)

action_319 (118) = happyShift action_53
action_319 (120) = happyShift action_54
action_319 (122) = happyShift action_55
action_319 (124) = happyShift action_56
action_319 (161) = happyShift action_57
action_319 (162) = happyShift action_58
action_319 (166) = happyShift action_59
action_319 (95) = happyGoto action_384
action_319 (96) = happyGoto action_49
action_319 (97) = happyGoto action_50
action_319 (98) = happyGoto action_51
action_319 (99) = happyGoto action_52
action_319 _ = happyFail (happyExpListPerState 319)

action_320 (118) = happyShift action_53
action_320 (120) = happyShift action_54
action_320 (122) = happyShift action_55
action_320 (124) = happyShift action_56
action_320 (161) = happyShift action_57
action_320 (162) = happyShift action_58
action_320 (166) = happyShift action_59
action_320 (95) = happyGoto action_383
action_320 (96) = happyGoto action_49
action_320 (97) = happyGoto action_50
action_320 (98) = happyGoto action_51
action_320 (99) = happyGoto action_52
action_320 _ = happyFail (happyExpListPerState 320)

action_321 _ = happyReduce_81

action_322 _ = happyReduce_138

action_323 (166) = happyShift action_382
action_323 (58) = happyGoto action_380
action_323 (59) = happyGoto action_381
action_323 _ = happyFail (happyExpListPerState 323)

action_324 (115) = happyShift action_379
action_324 _ = happyFail (happyExpListPerState 324)

action_325 (119) = happyShift action_377
action_325 (126) = happyShift action_378
action_325 _ = happyFail (happyExpListPerState 325)

action_326 _ = happyReduce_46

action_327 (124) = happyShift action_122
action_327 (125) = happyShift action_123
action_327 (131) = happyShift action_375
action_327 (138) = happyShift action_125
action_327 (139) = happyShift action_376
action_327 (163) = happyShift action_127
action_327 (108) = happyGoto action_374
action_327 _ = happyFail (happyExpListPerState 327)

action_328 (146) = happyShift action_373
action_328 _ = happyReduce_48

action_329 (146) = happyShift action_372
action_329 _ = happyReduce_56

action_330 _ = happyReduce_21

action_331 (136) = happyShift action_371
action_331 _ = happyReduce_249

action_332 (135) = happyShift action_370
action_332 _ = happyReduce_251

action_333 (118) = happyShift action_53
action_333 (120) = happyShift action_54
action_333 (122) = happyShift action_55
action_333 (161) = happyShift action_57
action_333 (162) = happyShift action_58
action_333 (166) = happyShift action_59
action_333 (99) = happyGoto action_369
action_333 _ = happyReduce_253

action_334 _ = happyReduce_255

action_335 (118) = happyShift action_53
action_335 (119) = happyShift action_368
action_335 (120) = happyShift action_54
action_335 (122) = happyShift action_55
action_335 (124) = happyShift action_56
action_335 (161) = happyShift action_57
action_335 (162) = happyShift action_58
action_335 (166) = happyShift action_59
action_335 (95) = happyGoto action_367
action_335 (96) = happyGoto action_49
action_335 (97) = happyGoto action_50
action_335 (98) = happyGoto action_51
action_335 (99) = happyGoto action_52
action_335 _ = happyFail (happyExpListPerState 335)

action_336 (118) = happyShift action_53
action_336 (120) = happyShift action_54
action_336 (122) = happyShift action_55
action_336 (124) = happyShift action_56
action_336 (161) = happyShift action_57
action_336 (162) = happyShift action_58
action_336 (166) = happyShift action_59
action_336 (95) = happyGoto action_366
action_336 (96) = happyGoto action_49
action_336 (97) = happyGoto action_50
action_336 (98) = happyGoto action_51
action_336 (99) = happyGoto action_52
action_336 _ = happyFail (happyExpListPerState 336)

action_337 (118) = happyShift action_53
action_337 (120) = happyShift action_54
action_337 (122) = happyShift action_55
action_337 (124) = happyShift action_56
action_337 (161) = happyShift action_57
action_337 (162) = happyShift action_58
action_337 (166) = happyShift action_59
action_337 (95) = happyGoto action_365
action_337 (96) = happyGoto action_49
action_337 (97) = happyGoto action_50
action_337 (98) = happyGoto action_51
action_337 (99) = happyGoto action_52
action_337 _ = happyFail (happyExpListPerState 337)

action_338 (161) = happyShift action_364
action_338 _ = happyFail (happyExpListPerState 338)

action_339 (140) = happyShift action_363
action_339 _ = happyReduce_263

action_340 _ = happyReduce_261

action_341 _ = happyReduce_264

action_342 (145) = happyShift action_344
action_342 (110) = happyGoto action_362
action_342 _ = happyReduce_274

action_343 _ = happyReduce_23

action_344 (115) = happyShift action_361
action_344 _ = happyFail (happyExpListPerState 344)

action_345 (161) = happyShift action_360
action_345 (36) = happyGoto action_358
action_345 (37) = happyGoto action_359
action_345 _ = happyFail (happyExpListPerState 345)

action_346 (122) = happyShift action_357
action_346 (31) = happyGoto action_356
action_346 _ = happyReduce_74

action_347 _ = happyReduce_73

action_348 _ = happyReduce_72

action_349 _ = happyReduce_71

action_350 (118) = happyShift action_319
action_350 (119) = happyShift action_355
action_350 (161) = happyShift action_321
action_350 _ = happyFail (happyExpListPerState 350)

action_351 (133) = happyShift action_354
action_351 _ = happyFail (happyExpListPerState 351)

action_352 (133) = happyShift action_353
action_352 _ = happyFail (happyExpListPerState 352)

action_353 (161) = happyShift action_347
action_353 (162) = happyShift action_348
action_353 (166) = happyShift action_349
action_353 (30) = happyGoto action_461
action_353 _ = happyFail (happyExpListPerState 353)

action_354 (161) = happyShift action_347
action_354 (162) = happyShift action_348
action_354 (166) = happyShift action_349
action_354 (30) = happyGoto action_460
action_354 _ = happyFail (happyExpListPerState 354)

action_355 _ = happyReduce_79

action_356 _ = happyReduce_65

action_357 (161) = happyShift action_459
action_357 (35) = happyGoto action_457
action_357 (38) = happyGoto action_458
action_357 _ = happyFail (happyExpListPerState 357)

action_358 _ = happyReduce_85

action_359 (116) = happyShift action_455
action_359 (117) = happyShift action_456
action_359 _ = happyFail (happyExpListPerState 359)

action_360 (134) = happyShift action_454
action_360 _ = happyFail (happyExpListPerState 360)

action_361 (118) = happyShift action_75
action_361 (161) = happyShift action_87
action_361 (14) = happyGoto action_451
action_361 (109) = happyGoto action_73
action_361 (111) = happyGoto action_452
action_361 (112) = happyGoto action_453
action_361 _ = happyFail (happyExpListPerState 361)

action_362 _ = happyReduce_22

action_363 (118) = happyShift action_53
action_363 (120) = happyShift action_54
action_363 (122) = happyShift action_55
action_363 (124) = happyShift action_56
action_363 (161) = happyShift action_57
action_363 (162) = happyShift action_58
action_363 (166) = happyShift action_59
action_363 (97) = happyGoto action_450
action_363 (98) = happyGoto action_51
action_363 (99) = happyGoto action_52
action_363 _ = happyFail (happyExpListPerState 363)

action_364 (125) = happyShift action_449
action_364 _ = happyFail (happyExpListPerState 364)

action_365 (123) = happyShift action_448
action_365 _ = happyFail (happyExpListPerState 365)

action_366 (121) = happyShift action_447
action_366 _ = happyFail (happyExpListPerState 366)

action_367 (119) = happyShift action_445
action_367 (126) = happyShift action_446
action_367 _ = happyFail (happyExpListPerState 367)

action_368 _ = happyReduce_256

action_369 _ = happyReduce_254

action_370 (118) = happyShift action_335
action_370 (120) = happyShift action_336
action_370 (122) = happyShift action_337
action_370 (124) = happyShift action_338
action_370 (161) = happyShift action_339
action_370 (162) = happyShift action_340
action_370 (166) = happyShift action_341
action_370 (103) = happyGoto action_444
action_370 (104) = happyGoto action_332
action_370 (105) = happyGoto action_333
action_370 (106) = happyGoto action_334
action_370 _ = happyFail (happyExpListPerState 370)

action_371 (118) = happyShift action_335
action_371 (120) = happyShift action_336
action_371 (122) = happyShift action_337
action_371 (124) = happyShift action_338
action_371 (161) = happyShift action_339
action_371 (162) = happyShift action_340
action_371 (166) = happyShift action_341
action_371 (103) = happyGoto action_443
action_371 (104) = happyGoto action_332
action_371 (105) = happyGoto action_333
action_371 (106) = happyGoto action_334
action_371 _ = happyFail (happyExpListPerState 371)

action_372 (162) = happyShift action_442
action_372 _ = happyFail (happyExpListPerState 372)

action_373 (161) = happyShift action_441
action_373 _ = happyFail (happyExpListPerState 373)

action_374 (119) = happyShift action_440
action_374 _ = happyFail (happyExpListPerState 374)

action_375 (119) = happyShift action_439
action_375 _ = happyFail (happyExpListPerState 375)

action_376 (119) = happyShift action_438
action_376 _ = happyFail (happyExpListPerState 376)

action_377 _ = happyReduce_45

action_378 (118) = happyShift action_327
action_378 (161) = happyShift action_328
action_378 (162) = happyShift action_329
action_378 (27) = happyGoto action_437
action_378 _ = happyFail (happyExpListPerState 378)

action_379 (161) = happyShift action_436
action_379 (60) = happyGoto action_434
action_379 (61) = happyGoto action_435
action_379 _ = happyFail (happyExpListPerState 379)

action_380 (119) = happyShift action_432
action_380 (126) = happyShift action_433
action_380 _ = happyFail (happyExpListPerState 380)

action_381 _ = happyReduce_139

action_382 (146) = happyShift action_431
action_382 _ = happyReduce_141

action_383 _ = happyReduce_60

action_384 (119) = happyShift action_430
action_384 _ = happyFail (happyExpListPerState 384)

action_385 (118) = happyShift action_422
action_385 (120) = happyShift action_423
action_385 (122) = happyShift action_424
action_385 (124) = happyShift action_425
action_385 (161) = happyShift action_426
action_385 (162) = happyShift action_427
action_385 (166) = happyShift action_428
action_385 (39) = happyGoto action_429
action_385 (41) = happyGoto action_418
action_385 (42) = happyGoto action_419
action_385 (43) = happyGoto action_420
action_385 (44) = happyGoto action_421
action_385 _ = happyFail (happyExpListPerState 385)

action_386 (118) = happyShift action_422
action_386 (120) = happyShift action_423
action_386 (122) = happyShift action_424
action_386 (124) = happyShift action_425
action_386 (161) = happyShift action_426
action_386 (162) = happyShift action_427
action_386 (166) = happyShift action_428
action_386 (39) = happyGoto action_417
action_386 (41) = happyGoto action_418
action_386 (42) = happyGoto action_419
action_386 (43) = happyGoto action_420
action_386 (44) = happyGoto action_421
action_386 _ = happyFail (happyExpListPerState 386)

action_387 (133) = happyShift action_416
action_387 _ = happyReduce_63

action_388 _ = happyReduce_113

action_389 (118) = happyShift action_53
action_389 (120) = happyShift action_54
action_389 (122) = happyShift action_55
action_389 (161) = happyShift action_57
action_389 (162) = happyShift action_58
action_389 (166) = happyShift action_59
action_389 (99) = happyGoto action_244
action_389 (101) = happyGoto action_312
action_389 _ = happyFail (happyExpListPerState 389)

action_390 (118) = happyShift action_53
action_390 (120) = happyShift action_54
action_390 (122) = happyShift action_55
action_390 (161) = happyShift action_57
action_390 (162) = happyShift action_58
action_390 (166) = happyShift action_59
action_390 (98) = happyGoto action_415
action_390 (99) = happyGoto action_52
action_390 _ = happyFail (happyExpListPerState 390)

action_391 (116) = happyShift action_413
action_391 (117) = happyShift action_414
action_391 _ = happyFail (happyExpListPerState 391)

action_392 _ = happyReduce_114

action_393 (113) = happyGoto action_412
action_393 _ = happyReduce_279

action_394 (118) = happyShift action_75
action_394 (143) = happyShift action_77
action_394 (161) = happyShift action_87
action_394 (14) = happyGoto action_408
action_394 (51) = happyGoto action_409
action_394 (52) = happyGoto action_410
action_394 (56) = happyGoto action_411
action_394 (109) = happyGoto action_73
action_394 _ = happyFail (happyExpListPerState 394)

action_395 _ = happyReduce_129

action_396 _ = happyReduce_130

action_397 _ = happyReduce_128

action_398 _ = happyReduce_127

action_399 _ = happyReduce_245

action_400 _ = happyReduce_227

action_401 (158) = happyReduce_157
action_401 (159) = happyReduce_157
action_401 _ = happyReduce_204

action_402 _ = happyReduce_26

action_403 _ = happyReduce_9

action_404 _ = happyReduce_36

action_405 _ = happyReduce_40

action_406 _ = happyReduce_41

action_407 _ = happyReduce_39

action_408 _ = happyReduce_122

action_409 (116) = happyShift action_493
action_409 (117) = happyShift action_494
action_409 _ = happyFail (happyExpListPerState 409)

action_410 _ = happyReduce_119

action_411 _ = happyReduce_121

action_412 (134) = happyShift action_492
action_412 (161) = happyShift action_227
action_412 _ = happyFail (happyExpListPerState 412)

action_413 _ = happyReduce_107

action_414 (118) = happyShift action_75
action_414 (161) = happyShift action_87
action_414 (49) = happyGoto action_491
action_414 (109) = happyGoto action_393
action_414 _ = happyFail (happyExpListPerState 414)

action_415 (118) = happyShift action_53
action_415 (120) = happyShift action_54
action_415 (122) = happyShift action_55
action_415 (161) = happyShift action_57
action_415 (162) = happyShift action_58
action_415 (166) = happyShift action_59
action_415 (99) = happyGoto action_95
action_415 _ = happyReduce_110

action_416 (118) = happyShift action_53
action_416 (120) = happyShift action_54
action_416 (122) = happyShift action_55
action_416 (124) = happyShift action_56
action_416 (161) = happyShift action_57
action_416 (162) = happyShift action_58
action_416 (166) = happyShift action_59
action_416 (95) = happyGoto action_490
action_416 (96) = happyGoto action_49
action_416 (97) = happyGoto action_50
action_416 (98) = happyGoto action_51
action_416 (99) = happyGoto action_52
action_416 _ = happyFail (happyExpListPerState 416)

action_417 _ = happyReduce_59

action_418 _ = happyReduce_90

action_419 (135) = happyShift action_489
action_419 _ = happyReduce_94

action_420 (118) = happyShift action_53
action_420 (120) = happyShift action_54
action_420 (122) = happyShift action_55
action_420 (161) = happyShift action_57
action_420 (162) = happyShift action_58
action_420 (166) = happyShift action_59
action_420 (99) = happyGoto action_488
action_420 _ = happyReduce_96

action_421 _ = happyReduce_98

action_422 (118) = happyShift action_53
action_422 (119) = happyShift action_487
action_422 (120) = happyShift action_54
action_422 (122) = happyShift action_55
action_422 (124) = happyShift action_56
action_422 (161) = happyShift action_57
action_422 (162) = happyShift action_58
action_422 (166) = happyShift action_59
action_422 (95) = happyGoto action_486
action_422 (96) = happyGoto action_49
action_422 (97) = happyGoto action_50
action_422 (98) = happyGoto action_51
action_422 (99) = happyGoto action_52
action_422 _ = happyFail (happyExpListPerState 422)

action_423 (118) = happyShift action_53
action_423 (120) = happyShift action_54
action_423 (122) = happyShift action_55
action_423 (124) = happyShift action_56
action_423 (161) = happyShift action_57
action_423 (162) = happyShift action_58
action_423 (166) = happyShift action_59
action_423 (95) = happyGoto action_485
action_423 (96) = happyGoto action_49
action_423 (97) = happyGoto action_50
action_423 (98) = happyGoto action_51
action_423 (99) = happyGoto action_52
action_423 _ = happyFail (happyExpListPerState 423)

action_424 (118) = happyShift action_53
action_424 (120) = happyShift action_54
action_424 (122) = happyShift action_55
action_424 (124) = happyShift action_56
action_424 (161) = happyShift action_57
action_424 (162) = happyShift action_58
action_424 (166) = happyShift action_59
action_424 (95) = happyGoto action_484
action_424 (96) = happyGoto action_49
action_424 (97) = happyGoto action_50
action_424 (98) = happyGoto action_51
action_424 (99) = happyGoto action_52
action_424 _ = happyFail (happyExpListPerState 424)

action_425 (161) = happyShift action_483
action_425 _ = happyFail (happyExpListPerState 425)

action_426 (140) = happyShift action_482
action_426 _ = happyReduce_106

action_427 _ = happyReduce_104

action_428 (40) = happyGoto action_481
action_428 _ = happyReduce_91

action_429 _ = happyReduce_58

action_430 _ = happyReduce_82

action_431 (118) = happyShift action_478
action_431 (161) = happyShift action_479
action_431 (162) = happyShift action_480
action_431 _ = happyFail (happyExpListPerState 431)

action_432 _ = happyReduce_135

action_433 (166) = happyShift action_382
action_433 (59) = happyGoto action_477
action_433 _ = happyFail (happyExpListPerState 433)

action_434 (116) = happyShift action_475
action_434 (117) = happyShift action_476
action_434 _ = happyFail (happyExpListPerState 434)

action_435 _ = happyReduce_147

action_436 _ = happyReduce_149

action_437 _ = happyReduce_47

action_438 (146) = happyShift action_474
action_438 _ = happyReduce_53

action_439 (146) = happyShift action_473
action_439 _ = happyReduce_55

action_440 (146) = happyShift action_472
action_440 _ = happyReduce_51

action_441 _ = happyReduce_49

action_442 _ = happyReduce_57

action_443 _ = happyReduce_248

action_444 _ = happyReduce_250

action_445 _ = happyReduce_257

action_446 (118) = happyShift action_53
action_446 (120) = happyShift action_54
action_446 (122) = happyShift action_55
action_446 (124) = happyShift action_56
action_446 (161) = happyShift action_57
action_446 (162) = happyShift action_58
action_446 (166) = happyShift action_59
action_446 (95) = happyGoto action_255
action_446 (96) = happyGoto action_49
action_446 (97) = happyGoto action_50
action_446 (98) = happyGoto action_51
action_446 (99) = happyGoto action_52
action_446 (100) = happyGoto action_471
action_446 _ = happyFail (happyExpListPerState 446)

action_447 _ = happyReduce_259

action_448 _ = happyReduce_260

action_449 _ = happyReduce_252

action_450 _ = happyReduce_262

action_451 _ = happyReduce_278

action_452 (116) = happyShift action_469
action_452 (117) = happyShift action_470
action_452 _ = happyFail (happyExpListPerState 452)

action_453 _ = happyReduce_276

action_454 (118) = happyShift action_53
action_454 (120) = happyShift action_54
action_454 (122) = happyShift action_55
action_454 (124) = happyShift action_56
action_454 (161) = happyShift action_57
action_454 (162) = happyShift action_58
action_454 (166) = happyShift action_59
action_454 (95) = happyGoto action_468
action_454 (96) = happyGoto action_49
action_454 (97) = happyGoto action_50
action_454 (98) = happyGoto action_51
action_454 (99) = happyGoto action_52
action_454 _ = happyFail (happyExpListPerState 454)

action_455 _ = happyReduce_64

action_456 (161) = happyShift action_360
action_456 (36) = happyGoto action_467
action_456 _ = happyFail (happyExpListPerState 456)

action_457 _ = happyReduce_87

action_458 (123) = happyShift action_465
action_458 (126) = happyShift action_466
action_458 _ = happyFail (happyExpListPerState 458)

action_459 (134) = happyShift action_464
action_459 _ = happyFail (happyExpListPerState 459)

action_460 (122) = happyShift action_357
action_460 (31) = happyGoto action_463
action_460 _ = happyReduce_74

action_461 (122) = happyShift action_357
action_461 (31) = happyGoto action_462
action_461 _ = happyReduce_74

action_462 _ = happyReduce_66

action_463 _ = happyReduce_67

action_464 (118) = happyShift action_53
action_464 (120) = happyShift action_54
action_464 (122) = happyShift action_55
action_464 (124) = happyShift action_56
action_464 (161) = happyShift action_57
action_464 (162) = happyShift action_58
action_464 (166) = happyShift action_59
action_464 (95) = happyGoto action_515
action_464 (96) = happyGoto action_49
action_464 (97) = happyGoto action_50
action_464 (98) = happyGoto action_51
action_464 (99) = happyGoto action_52
action_464 _ = happyFail (happyExpListPerState 464)

action_465 _ = happyReduce_75

action_466 (161) = happyShift action_459
action_466 (35) = happyGoto action_514
action_466 _ = happyFail (happyExpListPerState 466)

action_467 _ = happyReduce_86

action_468 _ = happyReduce_84

action_469 _ = happyReduce_275

action_470 (118) = happyShift action_75
action_470 (161) = happyShift action_87
action_470 (14) = happyGoto action_451
action_470 (109) = happyGoto action_73
action_470 (112) = happyGoto action_513
action_470 _ = happyFail (happyExpListPerState 470)

action_471 (119) = happyShift action_512
action_471 (126) = happyShift action_303
action_471 _ = happyFail (happyExpListPerState 471)

action_472 (161) = happyShift action_511
action_472 _ = happyFail (happyExpListPerState 472)

action_473 (161) = happyShift action_510
action_473 _ = happyFail (happyExpListPerState 473)

action_474 (161) = happyShift action_509
action_474 _ = happyFail (happyExpListPerState 474)

action_475 _ = happyReduce_136

action_476 (161) = happyShift action_436
action_476 (61) = happyGoto action_508
action_476 _ = happyFail (happyExpListPerState 476)

action_477 _ = happyReduce_140

action_478 (124) = happyShift action_122
action_478 (125) = happyShift action_123
action_478 (131) = happyShift action_506
action_478 (138) = happyShift action_125
action_478 (139) = happyShift action_507
action_478 (163) = happyShift action_127
action_478 (108) = happyGoto action_505
action_478 _ = happyFail (happyExpListPerState 478)

action_479 _ = happyReduce_142

action_480 _ = happyReduce_143

action_481 (118) = happyShift action_53
action_481 (120) = happyShift action_54
action_481 (122) = happyShift action_55
action_481 (161) = happyShift action_57
action_481 (162) = happyShift action_58
action_481 (166) = happyShift action_59
action_481 (99) = happyGoto action_504
action_481 _ = happyReduce_89

action_482 (118) = happyShift action_53
action_482 (120) = happyShift action_54
action_482 (122) = happyShift action_55
action_482 (124) = happyShift action_56
action_482 (161) = happyShift action_57
action_482 (162) = happyShift action_58
action_482 (166) = happyShift action_59
action_482 (97) = happyGoto action_503
action_482 (98) = happyGoto action_51
action_482 (99) = happyGoto action_52
action_482 _ = happyFail (happyExpListPerState 482)

action_483 (125) = happyShift action_502
action_483 _ = happyFail (happyExpListPerState 483)

action_484 (123) = happyShift action_501
action_484 _ = happyFail (happyExpListPerState 484)

action_485 (121) = happyShift action_500
action_485 _ = happyFail (happyExpListPerState 485)

action_486 (119) = happyShift action_498
action_486 (126) = happyShift action_499
action_486 _ = happyFail (happyExpListPerState 486)

action_487 _ = happyReduce_99

action_488 _ = happyReduce_97

action_489 (118) = happyShift action_53
action_489 (120) = happyShift action_54
action_489 (122) = happyShift action_55
action_489 (124) = happyShift action_56
action_489 (161) = happyShift action_57
action_489 (162) = happyShift action_58
action_489 (166) = happyShift action_59
action_489 (95) = happyGoto action_497
action_489 (96) = happyGoto action_49
action_489 (97) = happyGoto action_50
action_489 (98) = happyGoto action_51
action_489 (99) = happyGoto action_52
action_489 _ = happyFail (happyExpListPerState 489)

action_490 _ = happyReduce_62

action_491 _ = happyReduce_115

action_492 (118) = happyShift action_335
action_492 (120) = happyShift action_336
action_492 (122) = happyShift action_337
action_492 (124) = happyShift action_338
action_492 (161) = happyShift action_339
action_492 (162) = happyShift action_340
action_492 (166) = happyShift action_341
action_492 (102) = happyGoto action_496
action_492 (103) = happyGoto action_331
action_492 (104) = happyGoto action_332
action_492 (105) = happyGoto action_333
action_492 (106) = happyGoto action_334
action_492 _ = happyFail (happyExpListPerState 492)

action_493 _ = happyReduce_117

action_494 (118) = happyShift action_75
action_494 (143) = happyShift action_77
action_494 (161) = happyShift action_87
action_494 (14) = happyGoto action_408
action_494 (52) = happyGoto action_495
action_494 (56) = happyGoto action_411
action_494 (109) = happyGoto action_73
action_494 _ = happyFail (happyExpListPerState 494)

action_495 _ = happyReduce_120

action_496 _ = happyReduce_116

action_497 _ = happyReduce_93

action_498 _ = happyReduce_100

action_499 (118) = happyShift action_53
action_499 (120) = happyShift action_54
action_499 (122) = happyShift action_55
action_499 (124) = happyShift action_56
action_499 (161) = happyShift action_57
action_499 (162) = happyShift action_58
action_499 (166) = happyShift action_59
action_499 (95) = happyGoto action_255
action_499 (96) = happyGoto action_49
action_499 (97) = happyGoto action_50
action_499 (98) = happyGoto action_51
action_499 (99) = happyGoto action_52
action_499 (100) = happyGoto action_519
action_499 _ = happyFail (happyExpListPerState 499)

action_500 _ = happyReduce_102

action_501 _ = happyReduce_103

action_502 _ = happyReduce_95

action_503 _ = happyReduce_105

action_504 _ = happyReduce_92

action_505 (119) = happyShift action_518
action_505 _ = happyFail (happyExpListPerState 505)

action_506 (119) = happyShift action_517
action_506 _ = happyFail (happyExpListPerState 506)

action_507 (119) = happyShift action_516
action_507 _ = happyFail (happyExpListPerState 507)

action_508 _ = happyReduce_148

action_509 _ = happyReduce_52

action_510 _ = happyReduce_54

action_511 _ = happyReduce_50

action_512 _ = happyReduce_258

action_513 _ = happyReduce_277

action_514 _ = happyReduce_88

action_515 _ = happyReduce_83

action_516 _ = happyReduce_145

action_517 _ = happyReduce_146

action_518 _ = happyReduce_144

action_519 (119) = happyShift action_520
action_519 (126) = happyShift action_303
action_519 _ = happyFail (happyExpListPerState 519)

action_520 _ = happyReduce_101

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 ((happy_var_1, False)
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn6
		 ((happy_var_1, True)
	)
happyReduction_4 _ _  = notHappyAtAll 

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
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  9 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 6 10 happyReduction_9
happyReduction_9 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at happy_var_1 (CModE happy_var_2 happy_var_4 happy_var_6)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  11 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  11 happyReduction_11
happyReduction_11 _
	_
	 =  HappyAbsSyn9
		 ([]
	)

happyReduce_12 = happySpecReduce_1  12 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  12 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  13 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  13 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 14 happyReduction_21
happyReduction_21 ((HappyAbsSyn102  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([at happy_var_1 (CSigE (toEVar happy_var_1) happy_var_2 happy_var_4)]
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 5 14 happyReduction_22
happyReduction_22 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([at happy_var_1 (CAssE (toEVar happy_var_1) happy_var_2 happy_var_4 happy_var_5)]
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 14 happyReduction_23
happyReduction_23 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([at happy_var_1 (CGuardedAssE (toEVar happy_var_1) happy_var_2 happy_var_3 happy_var_4)]
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  15 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  15 happyReduction_25
happyReduction_25 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 16 happyReduction_26
happyReduction_26 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (T.intercalate "." happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  18 happyReduction_28
happyReduction_28 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  18 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  18 happyReduction_30
happyReduction_30 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  19 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 (getName happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  19 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 <> "-" <> getName happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  20 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn20
		 (CstExportAll
	)

happyReduce_34 = happySpecReduce_1  20 happyReduction_34
happyReduction_34 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 (CstExportMany happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  21 happyReduction_35
happyReduction_35 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  21 happyReduction_36
happyReduction_36 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  22 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  23 happyReduction_38
happyReduction_38 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  23 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  23 happyReduction_40
happyReduction_40 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  23 happyReduction_41
happyReduction_41 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  23 happyReduction_42
happyReduction_42 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  24 happyReduction_43
happyReduction_43 (HappyAbsSyn25  happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CImpE (Import (MV happy_var_2) happy_var_3 [] Nothing))
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_0  25 happyReduction_44
happyReduction_44  =  HappyAbsSyn25
		 (Nothing
	)

happyReduce_45 = happySpecReduce_3  25 happyReduction_45
happyReduction_45 _
	(HappyAbsSyn26  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (Just happy_var_2
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  26 happyReduction_46
happyReduction_46 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 ([happy_var_1]
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  26 happyReduction_47
happyReduction_47 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  27 happyReduction_48
happyReduction_48 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (AliasedTerm (EV (getName happy_var_1)) (EV (getName happy_var_1))
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  27 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (AliasedTerm (EV (getName happy_var_1)) (EV (getName happy_var_3))
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happyReduce 5 27 happyReduction_50
happyReduction_50 ((HappyTerminal happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (AliasedTerm (EV (getOp happy_var_2)) (EV (getName happy_var_5))
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_3  27 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (AliasedTerm (EV (getOp happy_var_2)) (EV (getOp happy_var_2))
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happyReduce 5 27 happyReduction_52
happyReduction_52 ((HappyTerminal happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (AliasedTerm (EV "-") (EV (getName happy_var_5))
	) `HappyStk` happyRest

happyReduce_53 = happySpecReduce_3  27 happyReduction_53
happyReduction_53 _
	_
	_
	 =  HappyAbsSyn27
		 (AliasedTerm (EV "-") (EV "-")
	)

happyReduce_54 = happyReduce 5 27 happyReduction_54
happyReduction_54 ((HappyTerminal happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (AliasedTerm (EV ".") (EV (getName happy_var_5))
	) `HappyStk` happyRest

happyReduce_55 = happySpecReduce_3  27 happyReduction_55
happyReduction_55 _
	_
	_
	 =  HappyAbsSyn27
		 (AliasedTerm (EV ".") (EV ".")
	)

happyReduce_56 = happySpecReduce_1  27 happyReduction_56
happyReduction_56 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (AliasedType (TV (getName happy_var_1)) (TV (getName happy_var_1))
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  27 happyReduction_57
happyReduction_57 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn27
		 (AliasedType (TV (getName happy_var_1)) (TV (getName happy_var_3))
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happyReduce 6 28 happyReduction_58
happyReduction_58 ((HappyAbsSyn39  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at happy_var_1 (CTypE (CstTypeAlias (Just happy_var_2) happy_var_4 happy_var_6))
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 6 28 happyReduction_59
happyReduction_59 ((HappyAbsSyn39  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at happy_var_1 (CTypE (CstTypeAlias (Just happy_var_2) happy_var_4 happy_var_6))
	) `HappyStk` happyRest

happyReduce_60 = happyReduce 5 28 happyReduction_60
happyReduction_60 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at happy_var_1 (CTypE (CstTypeAlias Nothing (TV (getName happy_var_2), happy_var_3) (happy_var_5, False)))
	) `HappyStk` happyRest

happyReduce_61 = happySpecReduce_3  28 happyReduction_61
happyReduction_61 (HappyAbsSyn34  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CTypE (CstTypeAliasForward (TV (getName happy_var_2), happy_var_3)))
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happyReduce 7 28 happyReduction_62
happyReduction_62 ((HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at happy_var_1 (CTypE (CstTypeAlias Nothing (TV (getName happy_var_3), happy_var_4) (happy_var_7, False)))
	) `HappyStk` happyRest

happyReduce_63 = happyReduce 5 28 happyReduction_63
happyReduction_63 (_ `HappyStk`
	(HappyAbsSyn34  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at happy_var_1 (CTypE (CstTypeAliasForward (TV (getName happy_var_3), happy_var_4)))
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 6 28 happyReduction_64
happyReduction_64 (_ `HappyStk`
	(HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyAbsSyn29  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at (fst happy_var_1) (CTypE (CstNamTypeWhere (snd happy_var_1) happy_var_2 happy_var_5))
	) `HappyStk` happyRest

happyReduce_65 = happyReduce 5 28 happyReduction_65
happyReduction_65 ((HappyAbsSyn31  happy_var_5) `HappyStk`
	(HappyAbsSyn30  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyAbsSyn29  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at (fst happy_var_1) (CTypE (CstNamTypeLegacy Nothing (snd happy_var_1) happy_var_2 happy_var_4 happy_var_5))
	) `HappyStk` happyRest

happyReduce_66 = happyReduce 7 28 happyReduction_66
happyReduction_66 ((HappyAbsSyn31  happy_var_7) `HappyStk`
	(HappyAbsSyn30  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn29  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at (fst happy_var_1) (CTypE (CstNamTypeLegacy (Just happy_var_2) (snd happy_var_1) happy_var_4 happy_var_6 happy_var_7))
	) `HappyStk` happyRest

happyReduce_67 = happyReduce 7 28 happyReduction_67
happyReduction_67 ((HappyAbsSyn31  happy_var_7) `HappyStk`
	(HappyAbsSyn30  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn29  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at (fst happy_var_1) (CTypE (CstNamTypeLegacy (Just happy_var_2) (snd happy_var_1) happy_var_4 happy_var_6 happy_var_7))
	) `HappyStk` happyRest

happyReduce_68 = happySpecReduce_1  29 happyReduction_68
happyReduction_68 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 ((happy_var_1, NamRecord)
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  29 happyReduction_69
happyReduction_69 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 ((happy_var_1, NamObject)
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  29 happyReduction_70
happyReduction_70 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 ((happy_var_1, NamTable)
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  30 happyReduction_71
happyReduction_71 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 ((getString happy_var_1, True)
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  30 happyReduction_72
happyReduction_72 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 ((getName happy_var_1, False)
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  30 happyReduction_73
happyReduction_73 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn30
		 ((getName happy_var_1, False)
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_0  31 happyReduction_74
happyReduction_74  =  HappyAbsSyn31
		 ([]
	)

happyReduce_75 = happySpecReduce_3  31 happyReduction_75
happyReduction_75 _
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (happy_var_2
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  32 happyReduction_76
happyReduction_76 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  32 happyReduction_77
happyReduction_77 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_2  33 happyReduction_78
happyReduction_78 (HappyAbsSyn34  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn33
		 ((TV (getName happy_var_1), happy_var_2)
	)
happyReduction_78 _ _  = notHappyAtAll 

happyReduce_79 = happyReduce 4 33 happyReduction_79
happyReduction_79 (_ `HappyStk`
	(HappyAbsSyn34  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 ((TV (getName happy_var_2), happy_var_3)
	) `HappyStk` happyRest

happyReduce_80 = happySpecReduce_0  34 happyReduction_80
happyReduction_80  =  HappyAbsSyn34
		 ([]
	)

happyReduce_81 = happySpecReduce_2  34 happyReduction_81
happyReduction_81 (HappyTerminal happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (happy_var_1 ++ [Left (TV (getName happy_var_2))]
	)
happyReduction_81 _ _  = notHappyAtAll 

happyReduce_82 = happyReduce 4 34 happyReduction_82
happyReduction_82 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn34
		 (happy_var_1 ++ [Right happy_var_3]
	) `HappyStk` happyRest

happyReduce_83 = happySpecReduce_3  35 happyReduction_83
happyReduction_83 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn35
		 ((Key (getName happy_var_1), happy_var_3)
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_3  36 happyReduction_84
happyReduction_84 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn36
		 ((happy_var_1, Key (getName happy_var_1), happy_var_3)
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  37 happyReduction_85
happyReduction_85 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn37
		 ([happy_var_1]
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_3  37 happyReduction_86
happyReduction_86 (HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  38 happyReduction_87
happyReduction_87 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  38 happyReduction_88
happyReduction_88 (HappyAbsSyn35  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_2  39 happyReduction_89
happyReduction_89 (HappyAbsSyn40  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 ((case happy_var_2 of { [] -> VarU (TV (getString happy_var_1)); ts -> AppU (VarU (TV (getString happy_var_1))) ts }, True)
	)
happyReduction_89 _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  39 happyReduction_90
happyReduction_90 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn39
		 ((happy_var_1, False)
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_0  40 happyReduction_91
happyReduction_91  =  HappyAbsSyn40
		 ([]
	)

happyReduce_92 = happySpecReduce_2  40 happyReduction_92
happyReduction_92 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_92 _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  41 happyReduction_93
happyReduction_93 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (case happy_var_3 of { FunU args ret -> FunU (happy_var_1 : args) ret; t -> FunU [happy_var_1] t }
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  41 happyReduction_94
happyReduction_94 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  42 happyReduction_95
happyReduction_95 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ExistU (TV (getName happy_var_2)) ([], Open) ([], Open)
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_1  42 happyReduction_96
happyReduction_96 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_96 _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_2  43 happyReduction_97
happyReduction_97 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (applyType happy_var_1 happy_var_2
	)
happyReduction_97 _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_1  43 happyReduction_98
happyReduction_98 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_98 _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_2  44 happyReduction_99
happyReduction_99 _
	_
	 =  HappyAbsSyn7
		 (BT.unitU
	)

happyReduce_100 = happySpecReduce_3  44 happyReduction_100
happyReduction_100 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happyReduce 5 44 happyReduction_101
happyReduction_101 (_ `HappyStk`
	(HappyAbsSyn40  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (BT.tupleU (happy_var_2 : happy_var_4)
	) `HappyStk` happyRest

happyReduce_102 = happySpecReduce_3  44 happyReduction_102
happyReduction_102 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (BT.listU happy_var_2
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_3  44 happyReduction_103
happyReduction_103 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ThunkU happy_var_2
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_1  44 happyReduction_104
happyReduction_104 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (VarU (TV (getName happy_var_1))
	)
happyReduction_104 _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_3  44 happyReduction_105
happyReduction_105 (HappyAbsSyn7  happy_var_3)
	_
	_
	 =  HappyAbsSyn7
		 (happy_var_3
	)
happyReduction_105 _ _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1  44 happyReduction_106
happyReduction_106 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (VarU (TV (getName happy_var_1))
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happyReduce 6 45 happyReduction_107
happyReduction_107 (_ `HappyStk`
	(HappyAbsSyn48  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn46  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at happy_var_1 (CClsE happy_var_2 happy_var_5)
	) `HappyStk` happyRest

happyReduce_108 = happySpecReduce_2  45 happyReduction_108
happyReduction_108 (HappyAbsSyn46  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CClsE happy_var_2 [])
	)
happyReduction_108 _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  46 happyReduction_109
happyReduction_109 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn46
		 (CCHConstrained happy_var_1 happy_var_3
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happyReduce 5 46 happyReduction_110
happyReduction_110 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn46
		 (CCHMultiConstrained happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_111 = happySpecReduce_1  46 happyReduction_111
happyReduction_111 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn46
		 (CCHSimple happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_1  47 happyReduction_112
happyReduction_112 (HappyAbsSyn107  happy_var_1)
	 =  HappyAbsSyn47
		 ([happy_var_1]
	)
happyReduction_112 _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_3  47 happyReduction_113
happyReduction_113 (HappyAbsSyn107  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  48 happyReduction_114
happyReduction_114 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn48
		 ([happy_var_1]
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_3  48 happyReduction_115
happyReduction_115 (HappyAbsSyn49  happy_var_3)
	_
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happyReduce 4 49 happyReduction_116
happyReduction_116 ((HappyAbsSyn102  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn49
		 (CstSigItem (toEVar happy_var_1) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_117 = happyReduce 7 50 happyReduction_117
happyReduction_117 (_ `HappyStk`
	(HappyAbsSyn51  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at happy_var_1 (CIstE (ClassName (getName happy_var_2)) happy_var_3 (concat happy_var_6))
	) `HappyStk` happyRest

happyReduce_118 = happySpecReduce_3  50 happyReduction_118
happyReduction_118 (HappyAbsSyn40  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CIstE (ClassName (getName happy_var_2)) happy_var_3 [])
	)
happyReduction_118 _ _ _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  51 happyReduction_119
happyReduction_119 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn51
		 ([happy_var_1]
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_3  51 happyReduction_120
happyReduction_120 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_120 _ _ _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_1  52 happyReduction_121
happyReduction_121 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_121 _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1  52 happyReduction_122
happyReduction_122 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happySpecReduce_3  53 happyReduction_123
happyReduction_123 (HappyAbsSyn54  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CFixE InfixL (fromInteger (getInt happy_var_2)) happy_var_3)
	)
happyReduction_123 _ _ _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_3  53 happyReduction_124
happyReduction_124 (HappyAbsSyn54  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CFixE InfixR (fromInteger (getInt happy_var_2)) happy_var_3)
	)
happyReduction_124 _ _ _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_3  53 happyReduction_125
happyReduction_125 (HappyAbsSyn54  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CFixE InfixN (fromInteger (getInt happy_var_2)) happy_var_3)
	)
happyReduction_125 _ _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1  54 happyReduction_126
happyReduction_126 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn54
		 ([happy_var_1]
	)
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_3  54 happyReduction_127
happyReduction_127 (HappyAbsSyn55  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn54
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_127 _ _ _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_3  55 happyReduction_128
happyReduction_128 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn55
		 (EV (getOp happy_var_2)
	)
happyReduction_128 _ _ _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_3  55 happyReduction_129
happyReduction_129 _
	_
	_
	 =  HappyAbsSyn55
		 (EV "-"
	)

happyReduce_130 = happySpecReduce_3  55 happyReduction_130
happyReduction_130 _
	_
	_
	 =  HappyAbsSyn55
		 (EV "."
	)

happyReduce_131 = happySpecReduce_1  55 happyReduction_131
happyReduction_131 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn55
		 (EV (getOp happy_var_1)
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_1  55 happyReduction_132
happyReduction_132 _
	 =  HappyAbsSyn55
		 (EV "."
	)

happyReduce_133 = happySpecReduce_1  55 happyReduction_133
happyReduction_133 _
	 =  HappyAbsSyn55
		 (EV "-"
	)

happyReduce_134 = happySpecReduce_1  55 happyReduction_134
happyReduction_134 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn55
		 (EV (getName happy_var_1)
	)
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happyReduce 6 56 happyReduction_135
happyReduction_135 (_ `HappyStk`
	(HappyAbsSyn58  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn57  happy_var_3) `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([at happy_var_1 (CSrcOldE happy_var_2 happy_var_3 happy_var_5)]
	) `HappyStk` happyRest

happyReduce_136 = happyReduce 7 56 happyReduction_136
happyReduction_136 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn57  happy_var_3) `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 ([at happy_var_1 (CSrcNewE happy_var_2 happy_var_3 happy_var_6)]
	) `HappyStk` happyRest

happyReduce_137 = happySpecReduce_0  57 happyReduction_137
happyReduction_137  =  HappyAbsSyn57
		 (Nothing
	)

happyReduce_138 = happySpecReduce_2  57 happyReduction_138
happyReduction_138 (HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn57
		 (Just (getString happy_var_2)
	)
happyReduction_138 _ _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_1  58 happyReduction_139
happyReduction_139 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn58
		 ([happy_var_1]
	)
happyReduction_139 _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_3  58 happyReduction_140
happyReduction_140 (HappyAbsSyn59  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_140 _ _ _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_1  59 happyReduction_141
happyReduction_141 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn59
		 ((getString happy_var_1, Nothing)
	)
happyReduction_141 _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_3  59 happyReduction_142
happyReduction_142 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn59
		 ((getString happy_var_1, Just (getName happy_var_3))
	)
happyReduction_142 _ _ _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_3  59 happyReduction_143
happyReduction_143 (HappyTerminal happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn59
		 ((getString happy_var_1, Just (getName happy_var_3))
	)
happyReduction_143 _ _ _  = notHappyAtAll 

happyReduce_144 = happyReduce 5 59 happyReduction_144
happyReduction_144 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn59
		 ((getString happy_var_1, Just (getOp happy_var_4))
	) `HappyStk` happyRest

happyReduce_145 = happyReduce 5 59 happyReduction_145
happyReduction_145 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn59
		 ((getString happy_var_1, Just "-")
	) `HappyStk` happyRest

happyReduce_146 = happyReduce 5 59 happyReduction_146
happyReduction_146 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn59
		 ((getString happy_var_1, Just ".")
	) `HappyStk` happyRest

happyReduce_147 = happySpecReduce_1  60 happyReduction_147
happyReduction_147 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_147 _  = notHappyAtAll 

happyReduce_148 = happySpecReduce_3  60 happyReduction_148
happyReduction_148 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_148 _ _ _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_1  61 happyReduction_149
happyReduction_149 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_149 _  = notHappyAtAll 

happyReduce_150 = happySpecReduce_1  62 happyReduction_150
happyReduction_150 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_150 _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_1  62 happyReduction_151
happyReduction_151 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_151 _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_1  62 happyReduction_152
happyReduction_152 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_152 _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_3  62 happyReduction_153
happyReduction_153 (HappyAbsSyn7  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_2 (CAnnE happy_var_1 happy_var_3)
	)
happyReduction_153 _ _ _  = notHappyAtAll 

happyReduce_154 = happySpecReduce_3  63 happyReduction_154
happyReduction_154 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_2 (CLetE happy_var_1 happy_var_3)
	)
happyReduction_154 _ _ _  = notHappyAtAll 

happyReduce_155 = happySpecReduce_1  64 happyReduction_155
happyReduction_155 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn64
		 ([happy_var_1]
	)
happyReduction_155 _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_2  64 happyReduction_156
happyReduction_156 (HappyAbsSyn65  happy_var_2)
	(HappyAbsSyn64  happy_var_1)
	 =  HappyAbsSyn64
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_156 _ _  = notHappyAtAll 

happyReduce_157 = happyReduce 4 65 happyReduction_157
happyReduction_157 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn65
		 ((EV (getName happy_var_2), happy_var_4)
	) `HappyStk` happyRest

happyReduce_158 = happyReduce 4 65 happyReduction_158
happyReduction_158 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn65
		 ((EV "_", happy_var_4)
	) `HappyStk` happyRest

happyReduce_159 = happySpecReduce_3  65 happyReduction_159
happyReduction_159 (HappyAbsSyn15  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn65
		 ((EV (getName happy_var_2), Loc (happy_var_1 <-> snd (last happy_var_3)) (CGuardExprE happy_var_3))
	)
happyReduction_159 _ _ _  = notHappyAtAll 

happyReduce_160 = happyReduce 4 66 happyReduction_160
happyReduction_160 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (at happy_var_1 (CLamE (map EV happy_var_2) happy_var_4)
	) `HappyStk` happyRest

happyReduce_161 = happySpecReduce_1  67 happyReduction_161
happyReduction_161 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_161 _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_3  67 happyReduction_162
happyReduction_162 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_2 (CBopE happy_var_1 happy_var_2 happy_var_3)
	)
happyReduction_162 _ _ _  = notHappyAtAll 

happyReduce_163 = happySpecReduce_3  67 happyReduction_163
happyReduction_163 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_2 (CBopE happy_var_1 happy_var_2 happy_var_3)
	)
happyReduction_163 _ _ _  = notHappyAtAll 

happyReduce_164 = happySpecReduce_3  67 happyReduction_164
happyReduction_164 (HappyAbsSyn8  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_2 (CBopE happy_var_1 happy_var_2 happy_var_3)
	)
happyReduction_164 _ _ _  = notHappyAtAll 

happyReduce_165 = happySpecReduce_1  68 happyReduction_165
happyReduction_165 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_165 _  = notHappyAtAll 

happyReduce_166 = happySpecReduce_2  68 happyReduction_166
happyReduction_166 (HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CIntE (negate (getInt happy_var_2)))
	)
happyReduction_166 _ _  = notHappyAtAll 

happyReduce_167 = happySpecReduce_2  68 happyReduction_167
happyReduction_167 (HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CRealE (DS.fromFloatDigits (negate (getFloat happy_var_2))))
	)
happyReduction_167 _ _  = notHappyAtAll 

happyReduce_168 = happySpecReduce_1  69 happyReduction_168
happyReduction_168 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_168 _  = notHappyAtAll 

happyReduce_169 = happySpecReduce_2  69 happyReduction_169
happyReduction_169 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Loc (happy_var_1 <-> last happy_var_2) (CAppE happy_var_1 happy_var_2)
	)
happyReduction_169 _ _  = notHappyAtAll 

happyReduce_170 = happySpecReduce_1  70 happyReduction_170
happyReduction_170 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_170 _  = notHappyAtAll 

happyReduce_171 = happySpecReduce_2  70 happyReduction_171
happyReduction_171 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_171 _ _  = notHappyAtAll 

happyReduce_172 = happySpecReduce_1  71 happyReduction_172
happyReduction_172 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_172 _  = notHappyAtAll 

happyReduce_173 = happySpecReduce_1  71 happyReduction_173
happyReduction_173 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_173 _  = notHappyAtAll 

happyReduce_174 = happySpecReduce_1  71 happyReduction_174
happyReduction_174 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_174 _  = notHappyAtAll 

happyReduce_175 = happySpecReduce_1  71 happyReduction_175
happyReduction_175 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_175 _  = notHappyAtAll 

happyReduce_176 = happySpecReduce_1  71 happyReduction_176
happyReduction_176 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_176 _  = notHappyAtAll 

happyReduce_177 = happySpecReduce_1  71 happyReduction_177
happyReduction_177 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_177 _  = notHappyAtAll 

happyReduce_178 = happySpecReduce_1  71 happyReduction_178
happyReduction_178 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_178 _  = notHappyAtAll 

happyReduce_179 = happySpecReduce_1  71 happyReduction_179
happyReduction_179 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_179 _  = notHappyAtAll 

happyReduce_180 = happySpecReduce_1  71 happyReduction_180
happyReduction_180 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_180 _  = notHappyAtAll 

happyReduce_181 = happySpecReduce_1  71 happyReduction_181
happyReduction_181 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_181 _  = notHappyAtAll 

happyReduce_182 = happySpecReduce_1  71 happyReduction_182
happyReduction_182 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_182 _  = notHappyAtAll 

happyReduce_183 = happySpecReduce_1  71 happyReduction_183
happyReduction_183 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_183 _  = notHappyAtAll 

happyReduce_184 = happySpecReduce_2  72 happyReduction_184
happyReduction_184 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CForceE happy_var_2)
	)
happyReduction_184 _ _  = notHappyAtAll 

happyReduce_185 = happySpecReduce_2  73 happyReduction_185
happyReduction_185 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 CUniE
	)
happyReduction_185 _ _  = notHappyAtAll 

happyReduce_186 = happySpecReduce_3  73 happyReduction_186
happyReduction_186 _
	(HappyAbsSyn22  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CVarE (EV (getOp happy_var_2)))
	)
happyReduction_186 _ _ _  = notHappyAtAll 

happyReduce_187 = happySpecReduce_3  73 happyReduction_187
happyReduction_187 _
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CVarE (EV "-"))
	)
happyReduction_187 _ _ _  = notHappyAtAll 

happyReduce_188 = happySpecReduce_3  73 happyReduction_188
happyReduction_188 _
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CVarE (EV "."))
	)
happyReduction_188 _ _ _  = notHappyAtAll 

happyReduce_189 = happySpecReduce_3  73 happyReduction_189
happyReduction_189 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_189 _ _ _  = notHappyAtAll 

happyReduce_190 = happyReduce 5 73 happyReduction_190
happyReduction_190 ((HappyTerminal happy_var_5) `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Loc (happy_var_1 <-> happy_var_5) (CTupE (happy_var_2 : happy_var_4))
	) `HappyStk` happyRest

happyReduce_191 = happySpecReduce_1  74 happyReduction_191
happyReduction_191 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_191 _  = notHappyAtAll 

happyReduce_192 = happySpecReduce_3  74 happyReduction_192
happyReduction_192 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_192 _ _ _  = notHappyAtAll 

happyReduce_193 = happySpecReduce_3  75 happyReduction_193
happyReduction_193 (HappyTerminal happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (Loc (happy_var_1 <-> happy_var_3) (CSuspendE happy_var_2)
	)
happyReduction_193 _ _ _  = notHappyAtAll 

happyReduce_194 = happySpecReduce_3  76 happyReduction_194
happyReduction_194 (HappyTerminal happy_var_3)
	(HappyAbsSyn77  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (Loc (happy_var_1 <-> happy_var_3) (CNamE happy_var_2)
	)
happyReduction_194 _ _ _  = notHappyAtAll 

happyReduce_195 = happySpecReduce_1  77 happyReduction_195
happyReduction_195 (HappyAbsSyn78  happy_var_1)
	 =  HappyAbsSyn77
		 ([happy_var_1]
	)
happyReduction_195 _  = notHappyAtAll 

happyReduce_196 = happySpecReduce_3  77 happyReduction_196
happyReduction_196 (HappyAbsSyn78  happy_var_3)
	_
	(HappyAbsSyn77  happy_var_1)
	 =  HappyAbsSyn77
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_196 _ _ _  = notHappyAtAll 

happyReduce_197 = happySpecReduce_3  78 happyReduction_197
happyReduction_197 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn78
		 ((Key (getName happy_var_1), happy_var_3)
	)
happyReduction_197 _ _ _  = notHappyAtAll 

happyReduce_198 = happySpecReduce_2  79 happyReduction_198
happyReduction_198 (HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (Loc (happy_var_1 <-> happy_var_2) (CLstE [])
	)
happyReduction_198 _ _  = notHappyAtAll 

happyReduce_199 = happySpecReduce_3  79 happyReduction_199
happyReduction_199 (HappyTerminal happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (Loc (happy_var_1 <-> happy_var_3) (CLstE happy_var_2)
	)
happyReduction_199 _ _ _  = notHappyAtAll 

happyReduce_200 = happyReduce 4 80 happyReduction_200
happyReduction_200 ((HappyTerminal happy_var_4) `HappyStk`
	(HappyAbsSyn81  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Loc (happy_var_1 <-> happy_var_4) (CDoE happy_var_3)
	) `HappyStk` happyRest

happyReduce_201 = happySpecReduce_1  81 happyReduction_201
happyReduction_201 (HappyAbsSyn82  happy_var_1)
	 =  HappyAbsSyn81
		 ([happy_var_1]
	)
happyReduction_201 _  = notHappyAtAll 

happyReduce_202 = happySpecReduce_3  81 happyReduction_202
happyReduction_202 (HappyAbsSyn82  happy_var_3)
	_
	(HappyAbsSyn81  happy_var_1)
	 =  HappyAbsSyn81
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_202 _ _ _  = notHappyAtAll 

happyReduce_203 = happySpecReduce_3  82 happyReduction_203
happyReduction_203 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn82
		 (CstDoBind (EV (getName happy_var_1)) happy_var_3
	)
happyReduction_203 _ _ _  = notHappyAtAll 

happyReduce_204 = happyReduce 4 82 happyReduction_204
happyReduction_204 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn82
		 (CstDoLet (EV (getName happy_var_2)) happy_var_4
	) `HappyStk` happyRest

happyReduce_205 = happySpecReduce_3  82 happyReduction_205
happyReduction_205 (HappyAbsSyn15  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn82
		 (CstDoLet (EV (getName happy_var_2)) (Loc (happy_var_1 <-> snd (last happy_var_3)) (CGuardExprE happy_var_3))
	)
happyReduction_205 _ _ _  = notHappyAtAll 

happyReduce_206 = happySpecReduce_1  82 happyReduction_206
happyReduction_206 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn82
		 (CstDoBare happy_var_1
	)
happyReduction_206 _  = notHappyAtAll 

happyReduce_207 = happySpecReduce_2  83 happyReduction_207
happyReduction_207 (HappyAbsSyn84  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CAccessorE happy_var_2)
	)
happyReduction_207 _ _  = notHappyAtAll 

happyReduce_208 = happySpecReduce_2  84 happyReduction_208
happyReduction_208 (HappyAbsSyn85  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn84
		 (CABKey (getName happy_var_1) happy_var_2
	)
happyReduction_208 _ _  = notHappyAtAll 

happyReduce_209 = happySpecReduce_2  84 happyReduction_209
happyReduction_209 (HappyAbsSyn85  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn84
		 (CABIdx (fromInteger (getInt happy_var_1)) happy_var_2
	)
happyReduction_209 _ _  = notHappyAtAll 

happyReduce_210 = happySpecReduce_3  84 happyReduction_210
happyReduction_210 _
	(HappyAbsSyn86  happy_var_2)
	_
	 =  HappyAbsSyn84
		 (CABGroup happy_var_2
	)
happyReduction_210 _ _ _  = notHappyAtAll 

happyReduce_211 = happySpecReduce_0  85 happyReduction_211
happyReduction_211  =  HappyAbsSyn85
		 (CATEnd
	)

happyReduce_212 = happySpecReduce_2  85 happyReduction_212
happyReduction_212 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (CATSet happy_var_2
	)
happyReduction_212 _ _  = notHappyAtAll 

happyReduce_213 = happySpecReduce_2  85 happyReduction_213
happyReduction_213 (HappyAbsSyn84  happy_var_2)
	_
	 =  HappyAbsSyn85
		 (CATChain happy_var_2
	)
happyReduction_213 _ _  = notHappyAtAll 

happyReduce_214 = happySpecReduce_1  86 happyReduction_214
happyReduction_214 (HappyAbsSyn84  happy_var_1)
	 =  HappyAbsSyn86
		 ([happy_var_1]
	)
happyReduction_214 _  = notHappyAtAll 

happyReduce_215 = happySpecReduce_3  86 happyReduction_215
happyReduction_215 (HappyAbsSyn84  happy_var_3)
	_
	(HappyAbsSyn86  happy_var_1)
	 =  HappyAbsSyn86
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_215 _ _ _  = notHappyAtAll 

happyReduce_216 = happySpecReduce_2  87 happyReduction_216
happyReduction_216 (HappyAbsSyn84  happy_var_2)
	_
	 =  HappyAbsSyn84
		 (happy_var_2
	)
happyReduction_216 _ _  = notHappyAtAll 

happyReduce_217 = happySpecReduce_1  88 happyReduction_217
happyReduction_217 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CVarE (EV (getName happy_var_1)))
	)
happyReduction_217 _  = notHappyAtAll 

happyReduce_218 = happySpecReduce_1  89 happyReduction_218
happyReduction_218 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 CHolE
	)
happyReduction_218 _  = notHappyAtAll 

happyReduce_219 = happySpecReduce_1  90 happyReduction_219
happyReduction_219 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CLogE True)
	)
happyReduction_219 _  = notHappyAtAll 

happyReduce_220 = happySpecReduce_1  90 happyReduction_220
happyReduction_220 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CLogE False)
	)
happyReduction_220 _  = notHappyAtAll 

happyReduce_221 = happySpecReduce_1  91 happyReduction_221
happyReduction_221 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CIntE (getInt happy_var_1))
	)
happyReduction_221 _  = notHappyAtAll 

happyReduce_222 = happySpecReduce_1  91 happyReduction_222
happyReduction_222 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CRealE (DS.fromFloatDigits (getFloat happy_var_1)))
	)
happyReduction_222 _  = notHappyAtAll 

happyReduce_223 = happySpecReduce_1  92 happyReduction_223
happyReduction_223 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (at happy_var_1 (CStrE (getString happy_var_1))
	)
happyReduction_223 _  = notHappyAtAll 

happyReduce_224 = happySpecReduce_1  92 happyReduction_224
happyReduction_224 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_224 _  = notHappyAtAll 

happyReduce_225 = happySpecReduce_3  93 happyReduction_225
happyReduction_225 (HappyTerminal happy_var_3)
	(HappyAbsSyn94  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (Loc (happy_var_1 <-> happy_var_3) (CInterpE (getString happy_var_1) (fst happy_var_2) (snd happy_var_2) (getString happy_var_3))
	)
happyReduction_225 _ _ _  = notHappyAtAll 

happyReduce_226 = happySpecReduce_3  94 happyReduction_226
happyReduction_226 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn94
		 (([happy_var_2], [])
	)
happyReduction_226 _ _ _  = notHappyAtAll 

happyReduce_227 = happyReduce 5 94 happyReduction_227
happyReduction_227 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn94  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn94
		 (let (es, ms) = happy_var_1 in (es ++ [happy_var_4], ms ++ [getString happy_var_2])
	) `HappyStk` happyRest

happyReduce_228 = happySpecReduce_1  95 happyReduction_228
happyReduction_228 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_228 _  = notHappyAtAll 

happyReduce_229 = happySpecReduce_1  95 happyReduction_229
happyReduction_229 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_229 _  = notHappyAtAll 

happyReduce_230 = happySpecReduce_3  96 happyReduction_230
happyReduction_230 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (case happy_var_3 of { FunU args ret -> FunU (happy_var_1 : args) ret; t -> FunU [happy_var_1] t }
	)
happyReduction_230 _ _ _  = notHappyAtAll 

happyReduce_231 = happySpecReduce_3  97 happyReduction_231
happyReduction_231 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ExistU (TV (getName happy_var_2)) ([], Open) ([], Open)
	)
happyReduction_231 _ _ _  = notHappyAtAll 

happyReduce_232 = happySpecReduce_1  97 happyReduction_232
happyReduction_232 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_232 _  = notHappyAtAll 

happyReduce_233 = happySpecReduce_2  98 happyReduction_233
happyReduction_233 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (applyType happy_var_1 happy_var_2
	)
happyReduction_233 _ _  = notHappyAtAll 

happyReduce_234 = happySpecReduce_1  98 happyReduction_234
happyReduction_234 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_234 _  = notHappyAtAll 

happyReduce_235 = happySpecReduce_2  99 happyReduction_235
happyReduction_235 _
	_
	 =  HappyAbsSyn7
		 (BT.unitU
	)

happyReduce_236 = happySpecReduce_3  99 happyReduction_236
happyReduction_236 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_236 _ _ _  = notHappyAtAll 

happyReduce_237 = happyReduce 5 99 happyReduction_237
happyReduction_237 (_ `HappyStk`
	(HappyAbsSyn40  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (BT.tupleU (happy_var_2 : happy_var_4)
	) `HappyStk` happyRest

happyReduce_238 = happySpecReduce_3  99 happyReduction_238
happyReduction_238 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (BT.listU happy_var_2
	)
happyReduction_238 _ _ _  = notHappyAtAll 

happyReduce_239 = happySpecReduce_3  99 happyReduction_239
happyReduction_239 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ThunkU happy_var_2
	)
happyReduction_239 _ _ _  = notHappyAtAll 

happyReduce_240 = happySpecReduce_1  99 happyReduction_240
happyReduction_240 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (VarU (TV (getName happy_var_1))
	)
happyReduction_240 _  = notHappyAtAll 

happyReduce_241 = happySpecReduce_3  99 happyReduction_241
happyReduction_241 (HappyAbsSyn7  happy_var_3)
	_
	_
	 =  HappyAbsSyn7
		 (happy_var_3
	)
happyReduction_241 _ _ _  = notHappyAtAll 

happyReduce_242 = happySpecReduce_1  99 happyReduction_242
happyReduction_242 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (VarU (TV (getName happy_var_1))
	)
happyReduction_242 _  = notHappyAtAll 

happyReduce_243 = happySpecReduce_1  99 happyReduction_243
happyReduction_243 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn7
		 (VarU (TV (getString happy_var_1))
	)
happyReduction_243 _  = notHappyAtAll 

happyReduce_244 = happySpecReduce_1  100 happyReduction_244
happyReduction_244 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_244 _  = notHappyAtAll 

happyReduce_245 = happySpecReduce_3  100 happyReduction_245
happyReduction_245 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_245 _ _ _  = notHappyAtAll 

happyReduce_246 = happySpecReduce_1  101 happyReduction_246
happyReduction_246 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn40
		 ([happy_var_1]
	)
happyReduction_246 _  = notHappyAtAll 

happyReduce_247 = happySpecReduce_2  101 happyReduction_247
happyReduction_247 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_247 _ _  = notHappyAtAll 

happyReduce_248 = happySpecReduce_3  102 happyReduction_248
happyReduction_248 (HappyAbsSyn103  happy_var_3)
	_
	(HappyAbsSyn103  happy_var_1)
	 =  HappyAbsSyn102
		 (CstSigType (Just happy_var_1) happy_var_3
	)
happyReduction_248 _ _ _  = notHappyAtAll 

happyReduce_249 = happySpecReduce_1  102 happyReduction_249
happyReduction_249 (HappyAbsSyn103  happy_var_1)
	 =  HappyAbsSyn102
		 (CstSigType Nothing happy_var_1
	)
happyReduction_249 _  = notHappyAtAll 

happyReduce_250 = happySpecReduce_3  103 happyReduction_250
happyReduction_250 (HappyAbsSyn103  happy_var_3)
	_
	(HappyAbsSyn104  happy_var_1)
	 =  HappyAbsSyn103
		 (happy_var_1 : happy_var_3
	)
happyReduction_250 _ _ _  = notHappyAtAll 

happyReduce_251 = happySpecReduce_1  103 happyReduction_251
happyReduction_251 (HappyAbsSyn104  happy_var_1)
	 =  HappyAbsSyn103
		 ([happy_var_1]
	)
happyReduction_251 _  = notHappyAtAll 

happyReduce_252 = happySpecReduce_3  104 happyReduction_252
happyReduction_252 _
	(HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn104
		 ((locPos happy_var_1, ExistU (TV (getName happy_var_2)) ([], Open) ([], Open))
	)
happyReduction_252 _ _ _  = notHappyAtAll 

happyReduce_253 = happySpecReduce_1  104 happyReduction_253
happyReduction_253 (HappyAbsSyn104  happy_var_1)
	 =  HappyAbsSyn104
		 (happy_var_1
	)
happyReduction_253 _  = notHappyAtAll 

happyReduce_254 = happySpecReduce_2  105 happyReduction_254
happyReduction_254 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn104  happy_var_1)
	 =  HappyAbsSyn104
		 ((fst happy_var_1, applyType (snd happy_var_1) happy_var_2)
	)
happyReduction_254 _ _  = notHappyAtAll 

happyReduce_255 = happySpecReduce_1  105 happyReduction_255
happyReduction_255 (HappyAbsSyn104  happy_var_1)
	 =  HappyAbsSyn104
		 (happy_var_1
	)
happyReduction_255 _  = notHappyAtAll 

happyReduce_256 = happySpecReduce_2  106 happyReduction_256
happyReduction_256 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn104
		 ((locPos happy_var_1, BT.unitU)
	)
happyReduction_256 _ _  = notHappyAtAll 

happyReduce_257 = happySpecReduce_3  106 happyReduction_257
happyReduction_257 _
	(HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn104
		 ((locPos happy_var_1, happy_var_2)
	)
happyReduction_257 _ _ _  = notHappyAtAll 

happyReduce_258 = happyReduce 5 106 happyReduction_258
happyReduction_258 (_ `HappyStk`
	(HappyAbsSyn40  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn104
		 ((locPos happy_var_1, BT.tupleU (happy_var_2 : happy_var_4))
	) `HappyStk` happyRest

happyReduce_259 = happySpecReduce_3  106 happyReduction_259
happyReduction_259 _
	(HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn104
		 ((locPos happy_var_1, BT.listU happy_var_2)
	)
happyReduction_259 _ _ _  = notHappyAtAll 

happyReduce_260 = happySpecReduce_3  106 happyReduction_260
happyReduction_260 _
	(HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn104
		 ((locPos happy_var_1, ThunkU happy_var_2)
	)
happyReduction_260 _ _ _  = notHappyAtAll 

happyReduce_261 = happySpecReduce_1  106 happyReduction_261
happyReduction_261 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn104
		 ((locPos happy_var_1, VarU (TV (getName happy_var_1)))
	)
happyReduction_261 _  = notHappyAtAll 

happyReduce_262 = happySpecReduce_3  106 happyReduction_262
happyReduction_262 (HappyAbsSyn7  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn104
		 ((locPos happy_var_1, happy_var_3)
	)
happyReduction_262 _ _ _  = notHappyAtAll 

happyReduce_263 = happySpecReduce_1  106 happyReduction_263
happyReduction_263 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn104
		 ((locPos happy_var_1, VarU (TV (getName happy_var_1)))
	)
happyReduction_263 _  = notHappyAtAll 

happyReduce_264 = happySpecReduce_1  106 happyReduction_264
happyReduction_264 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn104
		 ((locPos happy_var_1, VarU (TV (getString happy_var_1)))
	)
happyReduction_264 _  = notHappyAtAll 

happyReduce_265 = happySpecReduce_2  107 happyReduction_265
happyReduction_265 (HappyAbsSyn40  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn107
		 (Constraint (ClassName (getName happy_var_1)) happy_var_2
	)
happyReduction_265 _ _  = notHappyAtAll 

happyReduce_266 = happySpecReduce_1  108 happyReduction_266
happyReduction_266 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_266 _  = notHappyAtAll 

happyReduce_267 = happySpecReduce_1  108 happyReduction_267
happyReduction_267 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_267 _  = notHappyAtAll 

happyReduce_268 = happySpecReduce_1  108 happyReduction_268
happyReduction_268 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_268 _  = notHappyAtAll 

happyReduce_269 = happySpecReduce_1  108 happyReduction_269
happyReduction_269 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_269 _  = notHappyAtAll 

happyReduce_270 = happySpecReduce_1  109 happyReduction_270
happyReduction_270 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_270 _  = notHappyAtAll 

happyReduce_271 = happySpecReduce_3  109 happyReduction_271
happyReduction_271 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_271 _ _ _  = notHappyAtAll 

happyReduce_272 = happySpecReduce_3  109 happyReduction_272
happyReduction_272 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_272 _ _ _  = notHappyAtAll 

happyReduce_273 = happySpecReduce_3  109 happyReduction_273
happyReduction_273 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_273 _ _ _  = notHappyAtAll 

happyReduce_274 = happySpecReduce_0  110 happyReduction_274
happyReduction_274  =  HappyAbsSyn9
		 ([]
	)

happyReduce_275 = happyReduce 4 110 happyReduction_275
happyReduction_275 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_276 = happySpecReduce_1  111 happyReduction_276
happyReduction_276 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_276 _  = notHappyAtAll 

happyReduce_277 = happySpecReduce_3  111 happyReduction_277
happyReduction_277 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_277 _ _ _  = notHappyAtAll 

happyReduce_278 = happySpecReduce_1  112 happyReduction_278
happyReduction_278 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_278 _  = notHappyAtAll 

happyReduce_279 = happySpecReduce_0  113 happyReduction_279
happyReduction_279  =  HappyAbsSyn18
		 ([]
	)

happyReduce_280 = happySpecReduce_2  113 happyReduction_280
happyReduction_280 (HappyTerminal happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 ++ [getName happy_var_2]
	)
happyReduction_280 _ _  = notHappyAtAll 

happyReduce_281 = happySpecReduce_1  114 happyReduction_281
happyReduction_281 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 ([getName happy_var_1]
	)
happyReduction_281 _  = notHappyAtAll 

happyReduce_282 = happySpecReduce_2  114 happyReduction_282
happyReduction_282 (HappyTerminal happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 ++ [getName happy_var_2]
	)
happyReduction_282 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 173 173 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Located _ TokVLBrace _ -> cont 115;
	Located _ TokVRBrace _ -> cont 116;
	Located _ TokVSemi _ -> cont 117;
	Located _ TokLParen _ -> cont 118;
	Located _ TokRParen _ -> cont 119;
	Located _ TokLBracket _ -> cont 120;
	Located _ TokRBracket _ -> cont 121;
	Located _ TokLBrace _ -> cont 122;
	Located _ TokRBrace _ -> cont 123;
	Located _ TokLAngle _ -> cont 124;
	Located _ TokRAngle _ -> cont 125;
	Located _ TokComma _ -> cont 126;
	Located _ TokBackslash _ -> cont 127;
	Located _ TokUnderscore _ -> cont 128;
	Located _ TokBang _ -> cont 129;
	Located _ TokQuestion _ -> cont 130;
	Located _ TokDot _ -> cont 131;
	Located _ TokGetterDot _ -> cont 132;
	Located _ TokEquals _ -> cont 133;
	Located _ TokDColon _ -> cont 134;
	Located _ TokArrow _ -> cont 135;
	Located _ TokFatArrow _ -> cont 136;
	Located _ TokBind _ -> cont 137;
	Located _ TokStar _ -> cont 138;
	Located _ TokMinus _ -> cont 139;
	Located _ TokColon _ -> cont 140;
	Located _ TokModule _ -> cont 141;
	Located _ TokImport _ -> cont 142;
	Located _ TokSource _ -> cont 143;
	Located _ TokFrom _ -> cont 144;
	Located _ TokWhere _ -> cont 145;
	Located _ TokAs _ -> cont 146;
	Located _ TokTrue _ -> cont 147;
	Located _ TokFalse _ -> cont 148;
	Located _ TokType _ -> cont 149;
	Located _ TokRecord _ -> cont 150;
	Located _ TokObject _ -> cont 151;
	Located _ TokTable _ -> cont 152;
	Located _ TokClass _ -> cont 153;
	Located _ TokInstance _ -> cont 154;
	Located _ TokInfixl _ -> cont 155;
	Located _ TokInfixr _ -> cont 156;
	Located _ TokInfix _ -> cont 157;
	Located _ TokLet _ -> cont 158;
	Located _ TokIn _ -> cont 159;
	Located _ TokDo _ -> cont 160;
	Located _ (TokLowerName _) _ -> cont 161;
	Located _ (TokUpperName _) _ -> cont 162;
	Located _ (TokOperator _) _ -> cont 163;
	Located _ (TokInteger _) _ -> cont 164;
	Located _ (TokFloat _) _ -> cont 165;
	Located _ (TokString _) _ -> cont 166;
	Located _ (TokStringStart _) _ -> cont 167;
	Located _ (TokStringMid _) _ -> cont 168;
	Located _ (TokStringEnd _) _ -> cont 169;
	Located _ TokInterpOpen _ -> cont 170;
	Located _ TokInterpClose _ -> cont 171;
	Located _ TokEOF _ -> cont 172;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 173 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = ((>>=))
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> P a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Located)], [Prelude.String]) -> P a
happyError' = parseError
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

data PState = PState
  { psExpIndex    :: !Int
  , psSourceMap   :: !(Map.Map Int SrcLoc)
  , psModulePath  :: !(Maybe Path)
  , psModuleConfig :: !ModuleConfig
  , psDocMap      :: !(Map.Map Pos [Text])
  , psSourceLines :: ![Text]
  , psLangMap :: !(Map.Map T.Text Lang) -- alias -> Lang for all known languages
  }
  deriving (Show)

emptyPState :: PState
emptyPState = PState 1 Map.empty Nothing defaultValue Map.empty [] Map.empty

type P a = State.StateT PState (Either ParseError) a

--------------------------------------------------------------------
-- Token extraction helpers
--------------------------------------------------------------------

getName :: Located -> Text
getName (Located _ (TokLowerName n) _) = n
getName (Located _ (TokUpperName n) _) = n
getName (Located _ _ t) = t

getInt :: Located -> Integer
getInt (Located _ (TokInteger n) _) = n
getInt _ = 0

getFloat :: Located -> Double
getFloat (Located _ (TokFloat d) _) = d
getFloat _ = 0

getString :: Located -> Text
getString (Located _ (TokString s) _) = s
getString (Located _ (TokStringStart s) _) = s
getString (Located _ (TokStringMid s) _) = s
getString (Located _ (TokStringEnd s) _) = s
getString (Located _ _ t) = t

getOp :: Located -> Text
getOp (Located _ (TokOperator t) _) = t
getOp (Located _ TokMinus _) = "-"
getOp (Located _ TokStar _) = "*"
getOp (Located _ TokDot _) = "."
getOp (Located _ TokLAngle _) = "<"
getOp (Located _ TokRAngle _) = ">"
getOp (Located _ _ t) = t

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
-- Type helper
--------------------------------------------------------------------

applyType :: TypeU -> TypeU -> TypeU
applyType (AppU f args) x = AppU f (args ++ [x])
applyType f x = AppU f [x]

--------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------

parseError :: ([Located], [String]) -> P a
parseError ([], expected) = do
  srcLines <- State.gets psSourceLines
  State.lift (Left (ParseError (Pos 0 0 "") "unexpected end of input" expected srcLines))
parseError (Located pos tok _ : _, expected) = do
  srcLines <- State.gets psSourceLines
  State.lift (Left (ParseError pos ("unexpected " ++ showToken tok) expected srcLines))

--------------------------------------------------------------------
-- Desugar bridge
--------------------------------------------------------------------

toDState :: PState -> DState
toDState ps = DState
  { dsExpIndex = psExpIndex ps
  , dsSourceMap = psSourceMap ps
  , dsDocMap = psDocMap ps
  , dsModulePath = psModulePath ps
  , dsModuleConfig = psModuleConfig ps
  , dsSourceLines = psSourceLines ps
  , dsLangMap = psLangMap ps
  }

fromDState :: PState -> DState -> PState
fromDState ps ds = ps
  { psExpIndex = dsExpIndex ds
  , psSourceMap = dsSourceMap ds
  }

-- | Run parse + desugar
parseAndDesugar :: PState -> [Located] -> Either ParseError ([ExprI], PState)
parseAndDesugar pstate tokens =
  case State.runStateT (parseProgram tokens) pstate of
    Left err -> Left err
    Right ((cstNodes, isImplicitMain), _parseState) ->
      let dstate = toDState pstate
      in case State.runStateT (desugarProgram isImplicitMain cstNodes) dstate of
        Left err -> Left err
        Right (exprIs, finalDState) ->
          Right (exprIs, fromDState pstate finalDState)

-- | Parse and desugar a single expression
parseAndDesugarExpr :: PState -> [Located] -> Either ParseError (ExprI, PState)
parseAndDesugarExpr pstate tokens =
  case State.runStateT (parseExprOnly tokens) pstate of
    Left err -> Left err
    Right (cstExpr, _parseState) ->
      let dstate = toDState pstate
      in case State.runStateT (desugarExpr cstExpr) dstate of
        Left err -> Left err
        Right (exprI, finalDState) ->
          Right (exprI, fromDState pstate finalDState)

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
  (tokens, docMap, groupToks) <- case lexMorloc filename sourceCode of
    Left err -> Left (showLexError err)
    Right r -> Right r
  let srcLines = T.lines sourceCode
      pstate' = pstate { psModulePath = modulePath, psDocMap = docMap, psSourceLines = srcLines }
  -- Strategy 1: parse as-is (code with module declarations)
  case parseAndDesugar pstate' tokens of
    Right (result, finalState) ->
      let dag' = foldl addModule dag result
          dag'' = attachGroupAnnotations tokens groupToks dag'
      in return (dag'', finalState)
    Left err ->
      -- Strategy 2: wrap in module, patch trailing expr as __expr__ assignment.
      let wrappedCode = "module main (*)\n" <> sourceCode
      in case lexMorloc filename wrappedCode of
        Right (wrappedTokens, wrappedDocMap, wrappedGroupToks) ->
          let pstate'' = pstate' { psDocMap = wrappedDocMap, psSourceLines = T.lines wrappedCode }
          in case parseAndDesugar pstate'' wrappedTokens of
            Right (result, finalState) ->
              let dag' = foldl addModule dag result
                  dag'' = attachGroupAnnotations wrappedTokens wrappedGroupToks dag'
              in return (dag'', finalState)
            Left _ ->
              case patchForTrailingExpr wrappedTokens of
                Just patchedTokens ->
                  case parseAndDesugar pstate'' patchedTokens of
                    Right (result, finalState) ->
                      let dag' = foldl addModule dag result
                          dag'' = attachGroupAnnotations patchedTokens wrappedGroupToks dag'
                      in return (dag'', finalState)
                    Left _ -> tryExprFallback tokens pstate' dag filename err
                Nothing -> tryExprFallback tokens pstate' dag filename err
        Left _ -> tryExprFallback tokens pstate' dag filename err
  where
    tryExprFallback tokens' ps dag' filename' origErr =
      let exprTokens = stripLayoutTokens tokens'
      in case parseAndDesugarExpr ps exprTokens of
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
              dag'' = Map.insert (MV "main") (modI, []) dag'
          return (dag'', finalState)
        Left _ ->
          Left (showParseError filename' origErr)

    addModule d e@(ExprI _ (ModE n es)) =
      let imports = [(importModuleName i, i) | (ExprI _ (ImpE i)) <- es]
      in Map.insert n (e, imports) d
    addModule _ _ = error "expected a module"

patchForTrailingExpr :: [Located] -> Maybe [Located]
patchForTrailingExpr tokens = do
  let tokens' = patchExport tokens
  patchLastStmt tokens'

patchExport :: [Located] -> [Located]
patchExport [] = []
patchExport (t@(Located _ TokLParen _) : Located p TokStar _ : rest) =
  t : Located p (TokLowerName "__expr__") "__expr__" : rest
patchExport (t : rest) = t : patchExport rest

patchLastStmt :: [Located] -> Maybe [Located]
patchLastStmt tokens =
  case findLastTopVSemi tokens 0 0 Nothing of
    Just idx ->
      let (before, after) = splitAt (idx + 1) tokens
          dummyPos = Pos 0 0 "<expr>"
          exprTok = Located dummyPos (TokLowerName "__expr__") "__expr__"
          eqTok = Located dummyPos TokEquals "="
      in Just (before ++ [exprTok, eqTok] ++ after)
    Nothing -> Nothing
  where
    findLastTopVSemi :: [Located] -> Int -> Int -> Maybe Int -> Maybe Int
    findLastTopVSemi [] _ _ lastIdx = lastIdx
    findLastTopVSemi (Located _ TokVLBrace _ : rest) depth pos lastIdx =
      findLastTopVSemi rest (depth + 1) (pos + 1) lastIdx
    findLastTopVSemi (Located _ TokVRBrace _ : rest) depth pos lastIdx =
      findLastTopVSemi rest (max 0 (depth - 1)) (pos + 1) lastIdx
    findLastTopVSemi (Located _ TokVSemi _ : rest) depth pos _
      | depth == 1 = findLastTopVSemi rest depth (pos + 1) (Just pos)
    findLastTopVSemi (_ : rest) depth pos lastIdx =
      findLastTopVSemi rest depth (pos + 1) lastIdx

stripLayoutTokens :: [Located] -> [Located]
stripLayoutTokens = filter (not . isLayoutToken)
  where
    isLayoutToken (Located _ TokVLBrace _) = True
    isLayoutToken (Located _ TokVRBrace _) = True
    isLayoutToken (Located _ TokVSemi _) = True
    isLayoutToken _ = False

readType :: Text -> Either String TypeU
readType typeStr = do
  let initState = emptyPState
  (tokens, _, _) <- case lexMorloc "<type>" typeStr of
    Left err -> Left (showLexError err)
    Right r -> Right r
  (result, _) <- case State.runStateT (parseTypeOnly tokens) initState of
    Left err -> Left (showParseError "<type>" err)
    Right r -> Right r
  return result

-- | Post-process the DAG to attach group annotations from --* tokens.
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
          groupNames = nubText [gn | (_, gn) <- Map.toList mem]
          exportGroups =
            [ ExportGroup gn (maybe [] id (Map.lookup gn ghdrs))
                (Set.filter (\(_, sym) -> Map.lookup (symText sym) mem == Just gn) symbols)
            | gn <- groupNames
            ]
          ungrouped = Set.filter (\(_, sym) -> not (Set.member (symText sym) groupedSymNames)) symbols
      in ExprI i (ExpE (ExportMany ungrouped exportGroups))
    attachToExpr _ _ e = e

    nubText :: [T.Text] -> [T.Text]
    nubText [] = []
    nubText (x:xs) = x : nubText (filter (/= x) xs)

    symText :: Symbol -> T.Text
    symText (TermSymbol (EV n)) = n
    symText (TypeSymbol (TV n)) = n
    symText (ClassSymbol (ClassName n)) = n

parseGroupHeaders :: [Located] -> [(T.Text, [T.Text], Pos)]
parseGroupHeaders = foldl' accum [] . map extractLine
  where
    extractLine (Located pos (TokGroupLine txt) _) = (pos, T.strip txt)
    extractLine (Located pos _ _) = (pos, T.empty)

    accum :: [(T.Text, [T.Text], Pos)] -> (Pos, T.Text) -> [(T.Text, [T.Text], Pos)]
    accum gs (pos, line)
      | T.null line = gs ++ [(T.empty, [], pos)]  -- bare --* = group terminator
      | otherwise = case T.stripPrefix "name:" line of
          Just name -> gs ++ [(T.strip name, [], pos)]
          Nothing -> case T.stripPrefix "desc:" line of
            Just desc -> case gs of
              [] -> gs
              _ -> init gs ++ [let (n, ds, p) = last gs in (n, ds ++ [T.strip desc], p)]
            Nothing -> gs

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

buildMembership :: [(T.Text, [T.Text], Pos)] -> [(T.Text, Pos)] -> Map.Map T.Text T.Text
buildMembership groupHeaders exportSyms = Map.fromList
  [ (sym, gname)
  | (sym, symPos) <- exportSyms
  , Just gname <- [findGroup symPos]
  ]
  where
    sortedGroups = sortBy (\(_,_,p1) (_,_,p2) -> compare p1 p2) groupHeaders

    findGroup :: Pos -> Maybe T.Text
    findGroup symPos = case filter (\(_,_,gpos) -> gpos < symPos) (reverse sortedGroups) of
      ((gname,_,_):_)
        | T.null gname -> Nothing  -- empty name = group terminator
        | otherwise -> Just gname
      [] -> Nothing
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
