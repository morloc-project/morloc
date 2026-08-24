{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Morloc.Version.Constraint
Description : Compiler-version constraint check for morloc modules
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

Checks a module's declared @morloc-version@ constraint (from its
@package.yaml@) against the running compiler version, at install and make.

Mirror of @morloc-deps/src/constraint.rs@ in the morloc-project/morloc-manager
repo: the two implement the same grammar and ordering, so keep them and their
shared test vectors in sync.

The grammar is the conda match-spec subset morloc already uses for language and
package versions -- ONE grammar for all versions. A constraint is @*@ (or empty)
meaning "any", or a comma-separated conjunction of atoms; each atom is a
comparator (@>=@, @>@, @<=@, @<@), or a bare version / @==v@ / @X.Y.*@ glob,
which all denote the prefix range @[v, next(v))@ (bumping v's last segment). So
@0.98@ means the @0.98.x@ series @[0.98, 0.99)@, and @>=0.98@ is the open floor.

Versions are dotted non-negative integers, compared segment-wise with
zero-padding so @0.9 < 0.10@ and @0.98 == 0.98.0@.
-}
module Morloc.Version.Constraint
  ( gateModuleVersion
    -- * Exposed for testing
  , parseConstraint
  , parseVersion
  , inRange
  , Constraint
  , Version
  ) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | A dotted numeric version, e.g. @0.98.2 -> [0,98,2]@.
newtype Version = Version [Integer]
  deriving (Eq, Show)

-- | The version with its last segment incremented, e.g. @[0,98] -> [0,99]@,
-- @[0,98,2] -> [0,98,3]@. Mirrors @Version::bump(depth=len-1)@ in
-- @constraint.rs@: it turns a bare/@==@/@X.Y.*@ atom into the half-open upper
-- bound of its prefix range. The argument is always non-empty ('parseVersion'
-- rejects empty input).
bumpLast :: Version -> Version
bumpLast (Version xs) = Version (init xs ++ [last xs + 1])

-- | Segment-wise comparison with zero-padding: @0.9 < 0.10@, @0.98 == 0.98.0@.
compareVersion :: Version -> Version -> Ordering
compareVersion (Version a) (Version b) =
  mconcat (zipWith compare (pad a) (pad b))
  where
    n = max (length a) (length b)
    pad xs = xs ++ replicate (n - length xs) 0

-- | One atomic bound. A version satisfies a 'Constraint' iff it satisfies
-- every atom, so comma-separated atoms need no bound-merging.
data Atom
  = AtGe Version
  | AtGt Version
  | AtLe Version
  | AtLt Version
  deriving (Show)

-- | A parsed @morloc-version@ constraint: the conjunction of its atoms
-- (empty = @*@ = any compiler).
newtype Constraint = Constraint [Atom]
  deriving (Show)

satisfiesAtom :: Version -> Atom -> Bool
satisfiesAtom v (AtGe w) = compareVersion v w /= LT
satisfiesAtom v (AtGt w) = compareVersion v w == GT
satisfiesAtom v (AtLe w) = compareVersion v w /= GT
satisfiesAtom v (AtLt w) = compareVersion v w == LT

-- | Whether a concrete version satisfies every atom of a constraint.
inRange :: Constraint -> Version -> Bool
inRange (Constraint atoms) v = all (satisfiesAtom v) atoms

-- | Parse a dotted numeric version. Rejects empty input, empty segments, and
-- any non-digit segment (e.g. a @0.98.2-dev@ suffix), returning 'Nothing'.
parseVersion :: Text -> Maybe Version
parseVersion t =
  let segs = T.splitOn "." (T.strip t)
   in if null segs then Nothing else Version <$> mapM parseSeg segs
  where
    parseSeg s
      | not (T.null s) && T.all isDigit s = Just (read (T.unpack s))
      | otherwise = Nothing

-- | Parse a constraint: @*@/empty = any, else a comma-separated conjunction of
-- atoms. Each atom is a comparator (@>=@, @>@, @<=@, @<@), or a bare version /
-- @==v@ / @X.Y.*@ glob, which expands to the prefix range @[v, next(v))@.
-- Mirrors @VersionRange::parse@ in @constraint.rs@.
parseConstraint :: Text -> Either Text Constraint
parseConstraint raw =
  let spec = T.strip raw
   in if T.null spec || spec == "*"
        then Right (Constraint [])
        else Constraint . concat <$> mapM (parseAtom . T.strip) (T.splitOn "," spec)

parseAtom :: Text -> Either Text [Atom]
parseAtom atom
  | Just rest <- T.stripPrefix ">=" atom = single AtGe rest
  | Just rest <- T.stripPrefix "<=" atom = single AtLe rest
  | Just rest <- T.stripPrefix ">" atom = single AtGt rest
  | Just rest <- T.stripPrefix "<" atom = single AtLt rest
  -- bare version, "==v", or "X.Y.*": the prefix range [v, next(v)).
  | otherwise =
      let base = fromMaybe atom (T.stripPrefix "==" atom)
          base' = fromMaybe base (T.stripSuffix ".*" base)
       in case parseVersion (T.strip base') of
            Just v -> Right [AtGe v, AtLt (bumpLast v)]
            Nothing -> Left (badVersion base')
  where
    single ctor s = case parseVersion (T.strip s) of
      Just v -> Right [ctor v]
      Nothing -> Left (badVersion s)
    badVersion s =
      "invalid version '" <> T.strip s <> "' in morloc-version constraint"

-- | Gate a module's declared @morloc-version@ against the running compiler.
--
--   * @Right Nothing@        -- compatible (or unconstrained): proceed silently.
--   * @Right (Just warning)@ -- could not check (the compiler reports a
--                               non-numeric version, e.g. a dev build):
--                               proceed, but emit the warning.
--   * @Left err@             -- reject: the constraint is malformed or the
--                               running compiler is out of range.
gateModuleVersion
  :: Text        -- ^ module name (for messages)
  -> Text        -- ^ running compiler version ('Morloc.Version.versionStr')
  -> Maybe Text  -- ^ declared @morloc-version@ constraint
  -> Either Text (Maybe Text)
gateModuleVersion _ _ Nothing = Right Nothing
gateModuleVersion modName compilerVer (Just spec) =
  case parseConstraint spec of
    Left e ->
      Left $ "module '" <> modName <> "' has an invalid morloc-version: " <> e
    Right c ->
      case parseVersion (T.strip compilerVer) of
        Nothing ->
          Right . Just $
            "warning: cannot check morloc-version for module '" <> modName
              <> "': compiler version '" <> compilerVer
              <> "' is not a plain numeric version; skipping the check"
        Just v
          | inRange c v -> Right Nothing
          | otherwise ->
              Left $
                "module '" <> modName <> "' requires morloc " <> spec
                  <> ", but the running compiler is " <> compilerVer
                  <> ". Update the compiler (e.g. 'mim update "
                  <> "--morloc-version <version>') or install a compatible "
                  <> "version of the module."
