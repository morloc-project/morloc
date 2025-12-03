{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Module      : Morloc.CodeGenerator.Docstrings
Description : Generate the final docstring records
Copyright   : (c) Zebulun Arendsee, 2016-2025
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.CodeGenerator.Docstrings (processDocstrings) where

import Morloc.Namespace
import qualified Data.Map as Map
import qualified Morloc.Monad as MM
import qualified Morloc.Data.GMap as GMap

processDocstrings :: AnnoS (Indexed Type) One a -> MorlocMonad (AnnoS (Indexed Type) One a, CmdDocSet)
processDocstrings e@(AnnoS (Idx i t) _ _) = do
    sgmap <- MM.gets stateSignatures
    doc <- case GMap.lookup i sgmap of
      (GMapJust (Monomorphic (TermTypes (Just et) _ _))) -> processCmdDocSet i t (edocs et)
      (GMapJust (Polymorphic _ _ et _)) -> processCmdDocSet i t (edocs et)
      _ -> return defaultValue
    return (e, doc)

processCmdDocSet :: Int -> Type -> CmdDocSet -> MorlocMonad CmdDocSet
processCmdDocSet i (FunT ts _) cmd = do
  cmdargs' <- zipWithM (processCmdArg i) ts (cmdDocArgs cmd)
  return $ cmd { cmdDocArgs = cmdargs' }
processCmdDocSet _ _ c = return c

processCmdArg :: Int -> Type -> CmdArg -> MorlocMonad CmdArg
processCmdArg i (VarT v) arg = do
  scope <- MM.getGeneralScope i
  case Map.lookup v scope of
    (Just [(_, typeOf -> parentType, parentArg, _)]) ->
      processCmdArg i parentType (inheritCmdArg arg parentArg)
    (Just xs) -> error $ show xs
    Nothing -> return arg
processCmdArg i (NamT _ _ _ rs) (CmdArgGrp recarg) = do
  args <- zipWithM (processCmdArg i) (map snd rs) (map snd (recDocEntries recarg))
  return . CmdArgGrp $ recarg { recDocEntries = zip (map fst rs) args }
processCmdArg _ (NamT _ _ _ _) _ =  error "Unreachable?"
processCmdArg _ _ r = return r

-- | Define rules for docstring term inheritance and promotion
inheritCmdArg :: CmdArg -> CmdArg -> CmdArg
inheritCmdArg CmdArgDef x =  x
inheritCmdArg x CmdArgDef =  x

-- for two positionals, inherit from parent field when child field is empty
inheritCmdArg (CmdArgPos (ArgPosDocSet d1 m1 l1 u1)) (CmdArgPos (ArgPosDocSet d2 m2 l2 u2)) =
  CmdArgPos $ ArgPosDocSet
    { argPosDocDesc = if (length d1 > 0) then d1 else d2
    , argPosDocMetavar = m1 <|> m2
    , argPosDocLiteral = l1 <|> l2
    , argPosDocUnroll = u1 <|> u2
    }

-- for positional child, with unroll=False, only inherit description
inheritCmdArg (CmdArgPos r1@(ArgPosDocSet d1 _ _ (Just False))) (CmdArgOpt r2) =
  CmdArgPos $ r1
    { argPosDocDesc = if (length d1 > 0) then d1 else argOptDocDesc r2
    }

-- for positional child, with unroll=True, convert
inheritCmdArg (CmdArgPos (ArgPosDocSet d1 m1 l1 _)) (CmdArgOpt r2) =
  CmdArgOpt $ r2
    { argOptDocDesc = if (length d1 > 0) then d1 else argOptDocDesc r2
    , argOptDocMetavar = m1 <|> argOptDocMetavar r2
    , argOptDocLiteral = l1 <|> argOptDocLiteral r2
    }

-- for positional child, with unroll=True, convert
inheritCmdArg (CmdArgPos (ArgPosDocSet d1 m1 _ _)) (CmdArgGrp r2) =
  CmdArgGrp $ r2
    { recDocDesc = if (length d1 > 0) then d1 else recDocDesc r2
    , recDocMetavar = m1 <|> recDocMetavar r2
    }

inheritCmdArg (CmdArgOpt r1) (CmdArgPos r2) =
  CmdArgOpt $ r1
    { argOptDocDesc = if (length (argOptDocDesc r1) > 0) then argOptDocDesc r1 else argPosDocDesc r2
    , argOptDocMetavar = argOptDocMetavar r1 <|> argPosDocMetavar r2
    , argOptDocLiteral = argOptDocLiteral r1 <|> argPosDocLiteral r2
    , argOptDocUnroll  = argOptDocUnroll  r1 <|> argPosDocUnroll  r2
    }

inheritCmdArg (CmdArgOpt r1) (CmdArgOpt r2) = undefined
  CmdArgOpt $ r1
    { argOptDocDesc = if (length (argOptDocDesc r1) > 0) then argOptDocDesc r1 else argOptDocDesc r2
    , argOptDocMetavar = argOptDocMetavar r1 <|> argOptDocMetavar r2
    , argOptDocLiteral = argOptDocLiteral r1 <|> argOptDocLiteral r2
    , argOptDocUnroll  = argOptDocUnroll  r1 <|> argOptDocUnroll  r2
    , argOptDocShort   = argOptDocShort   r1 <|> argOptDocShort   r2
    , argOptDocLong    = argOptDocLong    r1 <|> argOptDocLong    r2
    , argOptDocDefault = argOptDocDefault r1 <|> argOptDocDefault r2
    }

inheritCmdArg (CmdArgOpt r1) (CmdArgGrp r2)
  | argOptDocUnroll r1 == Just False = CmdArgOpt $ r1
    { argOptDocDesc = if (length (argOptDocDesc r1) > 0) then argOptDocDesc r1 else recDocDesc r2
    , argOptDocMetavar = argOptDocMetavar r1 <|> recDocMetavar r2
    , argOptDocShort   = argOptDocShort   r1 <|> recDocShort   r2
    , argOptDocLong    = argOptDocLong    r1 <|> recDocLong    r2
    }
  | otherwise = CmdArgGrp $ r2
    { recDocDesc = if (length (argOptDocDesc r1) > 0) then argOptDocDesc r1 else recDocDesc r2
    , recDocMetavar = argOptDocMetavar r1 <|> recDocMetavar r2
    , recDocUnroll  = argOptDocUnroll  r1 <|> recDocUnroll  r2
    , recDocShort   = argOptDocShort   r1 <|> recDocShort   r2
    , recDocLong    = argOptDocLong    r1 <|> recDocLong    r2
    , recDocEntries = recDocEntries r2
  }

inheritCmdArg (CmdArgGrp r1) (CmdArgPos r2) =
  CmdArgGrp $ r1
    { recDocDesc = if (length (recDocDesc r1) > 0) then recDocDesc r1 else argPosDocDesc r2
    , recDocMetavar = recDocMetavar r1 <|> argPosDocMetavar r2
    }

inheritCmdArg (CmdArgGrp r1) (CmdArgOpt r2) = undefined
  CmdArgGrp $ r1
    { recDocDesc = if (length (recDocDesc r1) > 0) then recDocDesc r1 else argOptDocDesc r2
    , recDocMetavar = recDocMetavar r1 <|> argOptDocMetavar r2
    , recDocShort   = recDocShort   r1 <|> argOptDocShort   r2
    , recDocLong    = recDocLong    r1 <|> argOptDocLong    r2
    }

inheritCmdArg (CmdArgGrp r1) (CmdArgGrp r2) = undefined
  CmdArgGrp $ r1
    { recDocDesc = if (length (recDocDesc r1) > 0) then recDocDesc r1 else recDocDesc r2
    , recDocMetavar = recDocMetavar r1 <|> recDocMetavar r2
    , recDocUnroll  = recDocUnroll  r1 <|> recDocUnroll  r2
    , recDocShort   = recDocShort   r1 <|> recDocShort   r2
    , recDocLong    = recDocLong    r1 <|> recDocLong    r2
    , recDocEntries = [ (k1, inheritCmdArg f1 f2)
                      | (k1, f1) <- recDocEntries r1
                      , (k2, f2) <- recDocEntries r2
                      , k1 == k2
                      ]
    }
