{- This file is a temporary suite of non-sense -}

module Morloc.Core () where

import qualified Data.Text as T

-- A standin for an undefined type
type XXX = String

-- the unique name of a function
-- NOTE: this should eventually include the namespace
type Name = String

-- A Object, Predicate, Object logical triple
type Triple = (String, String, String)

data Language = LanguageR | LanguageHaskell | LanguageOther

-- A constraint on a function
type Constraint = XXX

-- A Morloc type - fundamentally, this is a list of triples.
type Type = XXX

data Signature = Signature Name [Constraint] [Type]

type Function = (Signature, Language, Name)


-- All the general processed data needed to compile a Morloc program. But not
-- system-specific info. That is, a package should by invariant across systems.
-- It may also contain multiple versions of a function, with the function
-- choice made at compile time.
data Package = Package {
    packageCode      :: Code
  , packageFunctions :: Function
  , packageLanguage  :: Language
}

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


-- stubs for config records for each configurable thingy
newtype OptCompile   = OptCompile   { optCompileFoo   :: Int }
newtype OptSource    = OptSource    { optSourceFoo    :: Int }
newtype OptScript    = OptScript    { optScriptFoo    :: Int }
newtype OptSignature = OptSignature { optSignatureFoo :: Int }
newtype OptTypecheck = OptTypecheck { optTypecheckFoo :: Int }


-- Each language has its own dedicated set of configurations
newtype OptR         = OptR         { optRFoo         :: Int }
newtype OptPython    = OptPython    { optPythonFoo    :: Int }
newtype OptHaskell   = OptHaskell   { optHaskellFoo   :: Int }

-- These are options common to all languages, or passed when the language is
-- inferred. The ZZZ is a standin for something more creative. 
newtype OptZZZ   = OptZZZ   { optZZZFoo   :: Int }

data Source = SourceR       OptR       Code 
            | SourcePython  OptPython  Code 
            | SourceHaskell OptHaskell Code 
            | SourceOther   OptZZZ     Code

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

packageSubset :: Package -> [Name] -> Package
packageSubset = undefined

test :: (a -> b) -> (a, b) -> Either T.Text T.Text  
test = undefined

parseFunctionSignature :: [OptSignature] -> String -> Signature
parseFunctionSignature = undefined

parseFile :: T.Text -> [FileFeature]
parseFile = undefined

-- inferType :: Filename -> Type
-- inferType :: Text -> Type

-- instance Monoid Package where
--   --
