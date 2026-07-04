{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.PatternChain.Grammar
Description : Canonical grammar and parser for PatternChain strings.
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The PatternAccessible typeclass carries pattern-accessor chains
(@.foo@, @.[i]@, @.[s:e:p]@, @.0@, @.(a, b)@ and any of their nested
compositions) through the compiler as a canonical string wrapped in the
'PatternChain' newtype. This module owns the grammar, the parser, the
renderer, the Selector<->Path bridge, and the type-level walker that
computes the extracted type from the receiver's inner type and the
parsed path.

= Grammar (v1)

@
    path         ::= linear-step* group-step        -- linear prefix, terminal group
                  |  linear-step+                   -- linear only, no group

    linear-step  ::= field-step
                  |  tuple-step
                  |  bracket-index-step
                  |  bracket-slice-step

    field-step         ::= "." field-name
    tuple-step         ::= "." tuple-idx
    bracket-index-step ::= ".[]"
    bracket-slice-step ::= ".[:]"
    group-step         ::= ".(" path (";" path)* ")"

    field-name   ::= /[A-Za-z_][A-Za-z0-9_]*/
    tuple-idx    ::= "0" | /[1-9][0-9]*/
@

Termination: a group step always terminates the enclosing chain.
@.(a;b).c@ is a grammar error; @.(.(x;.y);.z)@ is legal (nested groups).

Placeholder semantics: @.[]@ consumes exactly 1 runtime arg (the index);
@.[:]@ consumes exactly 3 runtime args (start, stop, step). Args are
consumed in strict left-to-right occurrence order across the entire
path, including recursively into group children. Absent slice bounds
are encoded as @Nothing@ in the args list (@has@ byte == 0), not by
omitting placeholders in the string.

The bracket / group syntax matches what the runtime pattern walker
(@shared_ifile_walk@ in @morloc-runtime/src/stream.rs@) already accepts.
Aligning with the runtime lets the class-based dispatch feed the same
walker without a runtime parser rewrite.
-}
module Morloc.PatternChain.Grammar
  ( -- * Grammar version
    grammarVersion
    -- * Path AST
  , Path (..)
  , LinearStep (..)
    -- * Parser / renderer
  , GrammarError (..)
  , parsePath
  , renderPath
    -- * Selector interop
  , selectorToPath
    -- * Placeholder counting
  , placeholderCount
    -- * Type-level walker
  , WalkError (..)
  , walkPathType
  ) where

import qualified Morloc.BaseTypes as BT
import Morloc.Namespace.Expr (Selector (..))
import Morloc.Namespace.Prim (Key (..), TVar (..))
import Morloc.Namespace.Type (TypeU (..))

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec (ParseError, Parsec, (<|>), between, char, choice, digit,
                    eof, letter, lookAhead, many, optionMaybe, parse,
                    sepBy1, try)
import qualified Text.Parsec as P
import Text.Parsec.Text ()

-- | Canonical grammar version. Bumped on any backward-incompatible
-- change to the string form. Advanced instances that parse pattern
-- strings should pin against this constant at build time.
grammarVersion :: Int
grammarVersion = BT.patternChainGrammarVersion

-- | A parsed pattern-chain path.
data Path
  = LinearPath [LinearStep]
    -- ^ Non-empty linear steps, no group terminator. Corresponds to
    -- @linear-step+@ in the grammar.
  | GroupPath [LinearStep] [Path]
    -- ^ Optional linear prefix followed by a terminal group. Corresponds
    -- to @linear-step* group-step@. The child list is non-empty.
  deriving (Show, Ord, Eq)

-- | One linear step of the path.
data LinearStep
  = StepField Text        -- ^ @.foo@
  | StepTupleIdx Int      -- ^ @.0@, @.1@, ...
  | StepBracketIndex      -- ^ @.[]@; consumes 1 runtime arg
  | StepBracketSlice      -- ^ @.[:]@; consumes 3 runtime args
  deriving (Show, Ord, Eq)

-- | Grammar / parse errors surfaced by 'parsePath'.
data GrammarError
  = ParseFailed ParseError
  | EmptyPath
  | GroupFollowedByStep
  deriving (Show)

-- | Placeholder count for a path (sum of @?@ occurrences across every
-- step, including recursively into group children).
placeholderCount :: Path -> Int
placeholderCount (LinearPath steps) = sum (map linearArity steps)
placeholderCount (GroupPath prefix children) =
    sum (map linearArity prefix) + sum (map placeholderCount children)

linearArity :: LinearStep -> Int
linearArity StepBracketIndex = 1
linearArity StepBracketSlice = 3
linearArity _ = 0

-- | Render a Path back to its canonical string form.
renderPath :: Path -> Text
renderPath (LinearPath steps) = T.concat (map renderLinear steps)
renderPath (GroupPath prefix children) =
    T.concat (map renderLinear prefix) <> renderGroup children

renderLinear :: LinearStep -> Text
renderLinear (StepField k) = "." <> k
renderLinear (StepTupleIdx i) = "." <> T.pack (show i)
renderLinear StepBracketIndex = ".[]"
renderLinear StepBracketSlice = ".[:]"

renderGroup :: [Path] -> Text
renderGroup children =
    ".(" <> T.intercalate ";" (map renderPath children) <> ")"

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

type P = Parsec Text ()

parsePath :: Text -> Either GrammarError Path
parsePath src
  | T.null src = Left EmptyPath
  | otherwise = case parse (topPath <* eof) "<pattern-chain>" src of
      Left err -> Left (ParseFailed err)
      Right p -> Right p

topPath :: P Path
topPath = pathInner

pathInner :: P Path
pathInner = do
    -- Look at first char after the leading dot to decide: is this a
    -- group (starts with '(') or a linear step?
    prefix <- many (try linearStep)
    -- After the linear prefix, we may see: EOF, ',' or ')' (terminates
    -- the enclosing context if nested), or '.(' which begins a terminal
    -- group.
    mGrp <- optionMaybe (try groupStep)
    case (prefix, mGrp) of
      ([], Nothing) -> P.parserFail "empty path"
      (_, Just children) -> return (GroupPath prefix children)
      (_, Nothing) -> return (LinearPath prefix)

-- A path within a group child. Same as topPath but stops at ',' or ')'.
-- Group children may themselves end in a terminal group, or be linear.
childPath :: P Path
childPath = pathInner

linearStep :: P LinearStep
linearStep = do
    _ <- char '.'
    -- If the next char is '(', this is a group step, not a linear step.
    -- Reject so try/optionMaybe can backtrack.
    isGroup <- P.optionMaybe (try (lookAhead (char '(')))
    case isGroup of
      Just _ -> P.parserFail "not a linear step"
      Nothing -> choice
        [ bracketStep
        , tupleIdxStep
        , fieldStep
        ]

bracketStep :: P LinearStep
bracketStep = try $ do
    _ <- char '['
    choice
      [ try (P.string "]" >> return StepBracketIndex)
      , P.string ":]" >> return StepBracketSlice
      ]

tupleIdxStep :: P LinearStep
tupleIdxStep = try $ do
    d <- digit
    rest <- if d == '0' then return "" else many digit
    return (StepTupleIdx (read (d : rest)))

fieldStep :: P LinearStep
fieldStep = do
    h <- letter <|> char '_'
    tl <- many (letter <|> digit <|> char '_')
    return (StepField (T.pack (h : tl)))

groupStep :: P [Path]
groupStep = do
    _ <- char '.'
    between (char '(') (char ')') (sepBy1 childPath (char ';'))

--------------------------------------------------------------------------------
-- Selector interop
--------------------------------------------------------------------------------

-- | Convert a 'Selector' into a canonical 'Path'. Returns 'Nothing' for
-- 'SelectorEnd' (a Path has no representation for an empty walk).
selectorToPath :: Selector -> Maybe Path
selectorToPath sel = case sel of
    SelectorEnd -> Nothing
    _ -> Just (go sel)
  where
    -- Convert a Selector known to be non-Empty. Groups (multi-sibling
    -- Key/Idx forms) become GroupPath; single-sibling forms build a
    -- linear chain.
    go :: Selector -> Path
    go = collect []

    -- Collect linear steps head-first into an accumulator; when the
    -- selector splits into siblings, terminate with a GroupPath.
    collect :: [LinearStep] -> Selector -> Path
    collect acc SelectorEnd =
        -- Cannot terminate an empty path; caller guaranteed non-empty
        -- (else we would have returned Nothing at the top).
        LinearPath (reverse acc)
    collect acc SelectorBracketSlice =
        LinearPath (reverse (StepBracketSlice : acc))
    collect acc (SelectorBracketIndex s) =
        collect (StepBracketIndex : acc) s
    collect acc (SelectorKey (k, s) []) =
        collectMaybeContinue acc (StepField k) s
    collect acc (SelectorIdx (i, s) []) =
        collectMaybeContinue acc (StepTupleIdx i) s
    collect acc (SelectorKey hd others) =
        let siblings = hd : others
            children = [ collect [] (siblingPath k s) | (k, s) <- siblings ]
            siblingPath k s = SelectorKey (k, s) []
        in GroupPath (reverse acc) children
    collect acc (SelectorIdx hd others) =
        let siblings = hd : others
            children = [ collect [] (siblingPath i s) | (i, s) <- siblings ]
            siblingPath i s = SelectorIdx (i, s) []
        in GroupPath (reverse acc) children

    collectMaybeContinue acc step SelectorEnd =
        LinearPath (reverse (step : acc))
    collectMaybeContinue acc step s =
        collect (step : acc) s

--------------------------------------------------------------------------------
-- Type-level walker
--------------------------------------------------------------------------------

-- | Errors from walking a Path against a receiver type.
data WalkError
  = WalkFieldNotFound Text TypeU
  | WalkTupleIdxOutOfRange Int TypeU
  | WalkExpectedRecord TypeU
  | WalkExpectedTuple TypeU
  | WalkExpectedIndexable TypeU
  | WalkExpectedSliceable TypeU
  | WalkUnknownType TypeU
  deriving (Show)

-- | Compute the extracted type given the receiver's inner type and a
-- parsed Path. The receiver's inner type is the @a@ from @w a@ (the
-- 'PatternAccessible' instance's @w@ has already been stripped by the
-- caller).
--
-- This walker uses only structural information (record fields, tuple
-- arity, List/Vector head shapes). Instance-based subtyping decisions
-- happen at the call site, not here.
walkPathType :: TypeU -> Path -> Either WalkError TypeU
walkPathType t (LinearPath steps) = walkSteps t steps
walkPathType t (GroupPath prefix children) = do
    t' <- walkSteps t prefix
    -- Post-slice group broadcast: `.[:].(.a;.b)` on `[T]` builds a
    -- Tuple(T.a, T.b) inside the list.
    case (endsWithSlice prefix, t') of
      (True, AppU h@(VarU (TV "List")) [a]) -> do
          childTys <- mapM (walkPathType a) children
          return (AppU h [mkTuple childTys])
      (True, AppU h@(VarU (TV "Vector")) [n, a]) -> do
          childTys <- mapM (walkPathType a) children
          return (AppU h [n, mkTuple childTys])
      _ -> do
          childTys <- mapM (walkPathType t') children
          return (mkTuple childTys)
  where
    endsWithSlice steps = case reverse steps of
      (StepBracketSlice : _) -> True
      _                      -> False

walkSteps :: TypeU -> [LinearStep] -> Either WalkError TypeU
walkSteps t [] = Right t
walkSteps t (s : rest) = do
    t' <- walkStep t s
    -- Post-slice broadcast: `.[:]` followed by a field/tuple-idx tail
    -- walks the tail on the element type and wraps the result in a
    -- list, mirroring the IntrMap desugar.
    case (s, t', rest) of
      (StepBracketSlice, AppU h@(VarU (TV "List")) [a], _:_) -> do
          r <- walkSteps a rest
          return (AppU h [r])
      (StepBracketSlice, AppU h@(VarU (TV "Vector")) [n, a], _:_) -> do
          r <- walkSteps a rest
          return (AppU h [n, r])
      _ -> walkSteps t' rest

walkStep :: TypeU -> LinearStep -> Either WalkError TypeU
walkStep t (StepField k) = case t of
    NamU _ _ _ fields -> case lookup (Key k) fields of
        Just ft -> Right ft
        Nothing -> Left (WalkFieldNotFound k t)
    _ -> Left (WalkExpectedRecord t)
walkStep t (StepTupleIdx i) = case t of
    AppU (VarU (TV name)) params
      | isTupleName name ->
          if i >= 0 && i < length params
             then Right (params !! i)
             else Left (WalkTupleIdxOutOfRange i t)
    _ -> Left (WalkExpectedTuple t)
walkStep t StepBracketIndex = case t of
    AppU (VarU (TV "List")) [a] -> Right a
    AppU (VarU (TV "Vector")) [_, a] -> Right a
    AppU (VarU (TV "Str")) _ -> Right t
    VarU (TV "Str") -> Right t
    _ -> Left (WalkExpectedIndexable t)
walkStep t StepBracketSlice = case t of
    AppU (VarU (TV "List")) _ -> Right t
    AppU (VarU (TV "Vector")) _ -> Right t
    VarU (TV "Str") -> Right t
    _ -> Left (WalkExpectedSliceable t)

isTupleName :: Text -> Bool
isTupleName name =
    T.isPrefixOf "Tuple" name && T.all (`elem` ("0123456789" :: String)) (T.drop 5 name)

-- Build a Tuple type (or a bare type for singleton groups). Multi-child
-- groups produce TupleN with N = length of the child list.
mkTuple :: [TypeU] -> TypeU
mkTuple [] = VarU (TV "Unit")
mkTuple [ty] = ty
mkTuple tys =
    let n = length tys
        tName = "Tuple" <> T.pack (show n)
    in AppU (VarU (TV tName)) tys

