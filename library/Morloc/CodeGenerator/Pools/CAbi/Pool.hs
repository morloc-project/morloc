{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Pools.CAbi.Pool
Description : The CAbi pool and its members
Copyright   : (c) Zebulun Arendsee, 2016-2026
License     : Apache-2.0
Maintainer  : z@morloc.io

The CAbi pool holds ABI-compatible members that co-reside in one pool
process. C++ is its first member. This module builds the concrete 'Member'
values and routes a language to its member.
-}
module Morloc.CodeGenerator.Pools.CAbi.Pool
  ( memberFor
  ) where

import qualified Morloc.CodeGenerator.Pools.CAbi.Members.Cpp as Cpp
import Morloc.CodeGenerator.Namespace (Lang)
import Morloc.CodeGenerator.Pools.Pool (Member (..))
import qualified Morloc.CodeGenerator.Grammars.Translator.Generic as Generic

-- | The C++ member of the CAbi pool.
cppMember :: Member
cppMember = Member Cpp.translate

-- | A generic member for an interpreted language. Transitional: these become
-- their own single-member pools later.
genericMember :: Lang -> Member
genericMember lang = Member (Generic.translate lang)

-- | Route a language to its member. C++ is the CAbi pool's first member;
-- everything else falls back to the generic translator.
memberFor :: Lang -> Member
memberFor lang
  | lang == Cpp.cppLang = cppMember
  | otherwise = genericMember lang
