{-|
Module      : Morloc.CodeGenerator.Generate
Description : Translate AST forests into target language source code
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental

The single @generate@ function wraps the entire AST forest to source code
translation process.

The input the @generate@ is of type @[SAnno (Indexed Type) Many [Type]]@. The @SAnno
(Indexed Type) Many [Type]@ elements each represent a single command exported from the
main function. The @(Indexed Type)@ type stores all general information about a given
"manifold" (a node in the function graph and all its wrappings). The term
@Many@ states that there may be one of more AST describing each expression. The
term @[Type]@ states that there may be multiple concrete, language-specific
types associated with any term.

The @generate@ function converts the @SAnno (Indexed Type) Many [Type]@ types into
@SAnno (Indexed Type) One Type@ unambiguous ASTs. This step is an important
optimization step in the morloc build pipeline. Currently the compiler uses a
flat scoring matrix for the cost of interop between languages (e.g., 0 for C++
to C++, 1000 for anything to R, 5 for R to R since there is a function call
cost, etc). Replacing this algorithm with an empirically parameterized
performance model is a major goal.

Additional manipulations of the AST can reduce the number of required foreign
calls, (de)serialization calls, and duplicate computation.

The @SAnno (Indexed Type) One Type@ expression is ultimately translated into a simple
@ExprM@ type that is then passed to a language-specific translator.

-}

module Morloc.CodeGenerator.Generate
(
  generate
) where

import Morloc.CodeGenerator.Namespace
import Morloc.CodeGenerator.Internal
import Morloc.Data.Doc
import Morloc.Pretty (prettyType)
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as Lang
import qualified Morloc.Monad as MM
import Morloc.CodeGenerator.Grammars.Common
import qualified Morloc.CodeGenerator.Nexus as Nexus
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Morloc.CodeGenerator.Grammars.Translator.Cpp as Cpp
import qualified Morloc.CodeGenerator.Grammars.Translator.Rust as Rust
import qualified Morloc.CodeGenerator.Grammars.Translator.R as R
import qualified Morloc.CodeGenerator.Grammars.Translator.Python3 as Python3

-- | Translate typed, abstract syntax forests into compilable code
generate ::
  [SAnno (Indexed Type) Many Int]
  -- ^ one AST forest for each command exported from main
  -> MorlocMonad (Script, [Script]) 
  -- ^ the nexus code and the source code for each language pool
generate = undefined

realize
  :: SAnno (Indexed Type) Many Int
  -> MorlocMonad (SAnno (Indexed Type) One (Indexed Type))
realize = undefined
