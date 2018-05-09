{- This file is a temporary suite of non-sense -}

module Morloc.Core () where

import qualified Data.Text as T

-- A standin for an undefined type
type XXX = String

-- the unique name of a function
-- NOTE: this should eventually include the namespace
type FunctionName = String

-- A Object, Predicate, Object logical triple
type Triple = (String, String, String)

-- All the general processed data needed to compile a Morloc program. But not
-- system-specific info. That is, a package should by invariant across systems.
-- It may also contain multiple versions of a function, with the function
-- choice made at compile time.
type Package = XXX

-- A link to a Morloc graph database
type DB = XXX

-- A tar or zip archive, or something like that. It needs to hold a whole
-- directory, for example all the material in an R package.
type Archive = XXX

-- A binary and metadata about it
type BinaryProgram = XXX

-- The source code for a funtion
type Body = T.Text

-- Input code can be binaries (not supported), archives (not supported), or raw
-- text (only supported for R).
data Code = CodeBinary  BinaryProgram
          | CodeArchive Archive
          | CodeFile    T.Text 

-- A Morloc script
type Script = Code

-- The executable produced by the compiler
type Executable = XXX

-- A constraint on a function
type Constraint = XXX

--
type Type = XXX


-- stubs for config records for each configurable thingy
data OptCompile   = OptCompile   { optCompile_foo   :: Int }
data OptSource    = OptSource    { optSource_foo    :: Int }
data OptScript    = OptScript    { optScript_foo    :: Int }
data OptSignature = OptSignature { optSignature_foo :: Int }
data OptTypecheck = OptTypecheck { optTypecheck_foo :: Int }


-- Each language has its own dedicated set of configurations
data OptR         = OptR         { optR_foo         :: Int }
data OptPython    = OptPython    { optPython_foo    :: Int }
data OptHaskell   = OptHaskell   { optHaskell_foo   :: Int }

-- These are options common to all languages, or passed when the language is
-- inferred. The ZZZ is a standin for something more creative. 
data OptZZZ   = OptZZZ   { optZZZ_foo   :: Int }

data Source = SourceR       OptR       Code 
            | SourcePython  OptPython  Code 
            | SourceHaskell OptHaskell Code 
            | SourceOther   OptZZZ     Code

data Signature = Signature FunctionName [Constraint] [Type]

data FileFeature = FileFeatureFunction Signature Body
                 | FileFeatureDocumentation T.Text
                 | FileFeatureRelation Triple

compile :: [OptCompile] -> Package -> IO Executable
compile = undefined

packSource :: [OptSource] -> Source -> Package
packSource = undefined

compileScript :: [OptScript] -> DB -> Script -> Package
compileScript = undefined

typecheck :: [OptTypecheck] -> Package -> Either T.Text T.Text
typecheck = undefined

packageSubset :: Package -> [FunctionName] -> Package
packageSubset = undefined

test :: (a -> b) -> (a, b) -> Either T.Text T.Text  
test = undefined

parseFunctionSignature :: [OptSignature] -> String -> Signature
parseFunctionSignature = undefined

parseFile :: T.Text -> [FileFeature]
parseFile = undefined

-- instance Monoid Package where
--   --
