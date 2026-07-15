{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Frontend.CST
Description : Concrete syntax tree types for the Happy parser
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Types for the concrete syntax tree produced by the Happy-generated parser
(@Parser.y@). These preserve source spans and syntactic structure before
desugaring into the internal 'Expr' AST.
-}
module Morloc.Frontend.CST
  ( Span (..)
  , Loc (..)
  , HasPos (..)
  , CstExpr (..)
  , CstExport (..)
  , CstSigType (..)
  , CstTypeDef (..)
  , CstClassHead (..)
  , CstSigItem (..)
  , CstDoStmt (..)
  , CstAccessorBody (..)
  , CstBracketAxis (..)
  , CstAccessorTail (..)
  , CstIrrefPat (..)
  , at
  , (<->)
  , valOf
  ) where

import Data.Text (Text)
import Morloc.Frontend.Token (Located (..), Pos (..))
import Morloc.Namespace.Expr (Associativity (..), Import (..), RealLit (..))
import Morloc.Namespace.Prim
import Morloc.Namespace.Type (Constraint (..), NamType (..), TypeU (..))

-- | Source span: start position to end position
data Span = Span !Pos !Pos
  deriving (Show, Eq)

-- | Span-annotated wrapper
data Loc a = Loc !Span a
  deriving (Show, Eq)

valOf :: Loc a -> a
valOf (Loc _ x) = x

-- | Wrap a value at a single token's position
at :: Located -> a -> Loc a
at tok x = Loc (Span p p) x where p = locPos tok

{- | Build a span between any two things that have positions.
Works with Located tokens and Loc-wrapped values.
-}
class HasPos a where
  startPos :: a -> Pos
  endPos :: a -> Pos

instance HasPos Located where
  startPos = locPos
  endPos = locPos

instance HasPos (Loc a) where
  startPos (Loc (Span s _) _) = s
  endPos (Loc (Span _ e) _) = e

instance HasPos Span where
  startPos (Span s _) = s
  endPos (Span _ e) = e

-- | Build a span from a start-positioned thing to an end-positioned thing
(<->) :: (HasPos a, HasPos b) => a -> b -> Span
a <-> b = Span (startPos a) (endPos b)

infixl 5 <->

--------------------------------------------------------------------
-- CST node types
--------------------------------------------------------------------

data CstExpr
  = -- Top-level declarations
    CModE (Maybe Text) CstExport [Loc CstExpr]
  | CImpE Import
  | CSigE EVar CstSigType
  | CAssE EVar [Loc CstExpr] (Loc CstExpr) [Loc CstExpr]
  | CGuardedAssE EVar [Loc CstExpr] [(Loc CstExpr, Loc CstExpr)] (Loc CstExpr) [Loc CstExpr]
  | CTypE CstTypeDef
  | CClsE CstClassHead [CstSigItem]
  | CIstE ClassName [TypeU] [Loc CstExpr]
  | CEffE Text Bool  -- ^ effect declaration: label, isEscapable (True = escapable)
  | CFixE Associativity Int [EVar]
  -- Source-item tuple: (isBacktick, foreign-name, morloc-alias).
  -- isBacktick=True marks the name as `-quoted; it will bypass ldNamePattern/ldOperatorPattern
  -- validation and be emitted verbatim as an infix operator.
  | CSrcOldE Located (Maybe Text) [(Bool, Text, Maybe Text)]
  -- Source-item tuple: (isInline, isBacktick, foreign-name, name-token).
  | CSrcNewE Located (Maybe Text) [(Bool, Bool, Text, Located)]
  | -- Expressions
    CAppE (Loc CstExpr) [Loc CstExpr]
  | CLamE [Loc CstExpr] (Loc CstExpr)
  | CLetE [(Loc CstExpr, Loc CstExpr)] (Loc CstExpr)
  | CBopE (Loc CstExpr) Located (Loc CstExpr)
  | CLabeledVarE Text EVar  -- label:var (e.g., large:mean)
  | CVarE EVar
  | CIntE Integer
  | CRealE RealLit
  | CStrE Text
  | CLogE Bool
  | CUniE
  | CNullE
  | CLstE [Loc CstExpr]
  | CTupE [Loc CstExpr]
  | CNamE [(Key, Loc CstExpr)]
  | CAnnE (Loc CstExpr) TypeU
  | CDoE [CstDoStmt]
  | CAccessorE CstAccessorBody
  | CInterpE Text [Loc CstExpr] [Text] Text
  | CGuardExprE [(Loc CstExpr, Loc CstExpr)] (Loc CstExpr)
  | CIntrinsicE Text  -- ^ @name intrinsic reference (text is the name without @)
  | CParenE !(Loc CstExpr)  -- ^ parenthesized expression (preserves grouping for BopE chains)
  | CUnderscoreE  -- ^ '_' -- only legal in binding positions, checked in Desugar
  | CAsE EVar (Loc CstExpr)  -- ^ label@expr -- only legal in binding positions, checked in Desugar
  -- Operator sections (Haskell naming: left = left operand given, right = right operand given)
  | CLeftSecE !(Loc CstExpr) !Located  -- ^ (expr op) left section: \x -> expr op x
  | CRightSecE !Located !(Loc CstExpr)  -- ^ (op expr) right section: \x -> x op expr
  | CInlineE (Loc CstExpr) -- ^ %inline wrapper for source declarations
  deriving (Show, Eq)

data CstExport
  = CstExportAll
  | CstExportMany [Located]
  deriving (Show, Eq)

data CstSigType = CstSigType
  { cstSigConstraintArgs :: Maybe [(Pos, TypeU)]
  , cstSigArgs :: [(Pos, TypeU)]
  }
  deriving (Show, Eq)

data CstTypeDef
  = CstTypeAlias (Maybe Located) (TVar, [Either (TVar, Kind) TypeU]) (TypeU, Bool)
  | CstNewtype (TVar, [Either (TVar, Kind) TypeU]) TypeU
  | CstTypeAliasForward (TVar, [Either (TVar, Kind) TypeU])
  | CstNamTypeWhere NamType (TVar, [Either (TVar, Kind) TypeU]) [(Located, Key, TypeU)]
  | CstNamTypeLegacy (Maybe Located) NamType (TVar, [Either (TVar, Kind) TypeU]) (Text, Bool, [TypeU]) [(Key, TypeU)]
  deriving (Show, Eq)

data CstClassHead
  = CCHSimple TypeU
  | CCHConstrained TypeU TypeU
  | CCHMultiConstrained [Constraint] TypeU
  deriving (Show, Eq)

data CstSigItem = CstSigItem EVar CstSigType
  deriving (Show, Eq)

data CstDoStmt
  = CstDoBind (Loc CstExpr) (Loc CstExpr)
  | CstDoBare (Loc CstExpr)
  | CstDoLet (Loc CstExpr) (Loc CstExpr)
  deriving (Show, Eq)

-- | Concrete-syntax irrefutable pattern. Every well-typed receiver
-- matches (no literals, no alternatives) -- hence "irrefutable".
-- Built by 'Morloc.Frontend.Desugar.exprToIrrefPat' from an expression
-- appearing in a binding position, then eliminated into 'LetE' chains
-- that use the standard 'Pattern' / 'Selector' accessor infrastructure.
data CstIrrefPat
  = CIPatVar EVar                            -- ^ x
  | CIPatWild                                -- ^ _
  | CIPatTup [Loc CstIrrefPat]               -- ^ (p1, ..., pn), n >= 2
  | CIPatRec [(Key, Loc CstIrrefPat)]        -- ^ {a=p1, b=p2}
  | CIPatAs EVar (Loc CstIrrefPat)           -- ^ label@pat
  deriving (Show, Eq)

data CstAccessorBody
  = CABKey Text CstAccessorTail
  | CABIdx Int CstAccessorTail
  | CABGroup [CstAccessorBody]
  | CABBracket [CstBracketAxis] CstAccessorTail
  deriving (Show, Eq)

-- A single axis of a bracket accessor body. v1 only honors length-1 lists; the
-- list shape is preserved so the future tensor/matrix round can extend rank
-- without grammar changes.
data CstBracketAxis
  = BAxIdx (Loc CstExpr)
  | BAxSlice (Maybe (Loc CstExpr))   -- start
             (Maybe (Loc CstExpr))   -- stop
             (Maybe (Loc CstExpr))   -- step
  deriving (Show, Eq)

data CstAccessorTail
  = CATEnd
  | CATSet (Loc CstExpr)
  | CATChain CstAccessorBody
  deriving (Show, Eq)
