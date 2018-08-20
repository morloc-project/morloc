{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Morloc.Pool
Description : Short description
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Pool (generatePoolCode) where

import qualified Data.RDF as DR
import qualified Data.Text as DT

import qualified Morloc.Walker as MW
import Morloc.Operators
import qualified Morloc.Error as ME
import qualified Morloc.Language as ML
import qualified Morloc.Util as MU

-- RDF node representing a source import
type Source = DR.Node 
-- RDF node representing a function call
type Call = DR.Node
-- RDF node representing a an imported function
type Import = DR.Node

generatePoolCode :: DR.Rdf a => DR.RDF a -> [Source] -> ME.ThrowsError DT.Text
generatePoolCode rdf (s:ss) = case (MU.maybeOne $ MW.lang rdf s >>= MW.valueOf) of
  (Just "R") -> generateWith rdf (s:ss) ML.rCodeGenerator
  (Just l) -> Left $ ME.NotSupported ("No support for " ++ DT.unpack l)
  (Nothing) -> Left $ ME.InvalidRDF "No language specified for source"
generatePoolCode _ [] = Left $ ME.InvalidRDF "No source given for the pool"

generateWith
  :: DR.Rdf a
  => DR.RDF a
  -> [Source]
  -> ML.CodeGenerator
  -> ME.ThrowsError DT.Text
generateWith rdf srcs g = Right $
  (ML.makePool g)
    (generateGlobal rdf srcs g)
    (generateSource rdf srcs g)
    ((generateSourceFunctions rdf srcs g) ++ (generateFunctions rdf srcs g))

generateGlobal :: DR.RDF a -> [Source] -> ML.CodeGenerator -> [DT.Text]
generateGlobal _ _ _ = []

generateSource :: DR.Rdf a => DR.RDF a -> [Source] -> ML.CodeGenerator -> [DT.Text]
generateSource rdf srcs g = srcs >>= MW.path rdf >>= MW.valueOf |>> ML.makeSource g

generateFunctions :: DR.Rdf a => DR.RDF a -> [Source] -> ML.CodeGenerator -> [DT.Text]
generateFunctions rdf srcs g = map (generateFunction rdf srcs g) (MW.getCalls rdf)

generateSourceFunctions :: DR.Rdf a => DR.RDF a -> [Source] -> ML.CodeGenerator -> [DT.Text]
generateSourceFunctions rdf srcs g
  = concat
  $ map (generateSourceFunction rdf g)
        (srcs >>= MW.sourceExports rdf)

generateSourceFunction
  :: DR.Rdf a
  => DR.RDF a
  -> ML.CodeGenerator
  -> Import
  -> [DT.Text]
generateSourceFunction rdf g imp = case (MW.importAlias rdf imp, MW.importName rdf imp) of
  ([alias'], [name']) -> case (
        MW.idOf imp
      , asPositional (MW.getType rdf alias' >>= MW.elements rdf)
    ) of
      ([mid'], args') ->
        [(ML.makeFunction g)
           ((ML.makeManifoldName g) mid')
           args'
           (generateBody rdf g alias' name' args')
        ]
      _ -> []
  _ -> []
  where
    asPositional :: [a] -> [DT.Text]
    asPositional xs = map (\(_,i) -> "x" <> (DT.pack $ show i)) (zip xs [1..])

generateBody :: DR.Rdf a
  => DR.RDF a
  -> ML.CodeGenerator
  -> DT.Text   -- function alias
  -> DT.Text   -- function name
  -> [DT.Text] -- JSON argument inputs
  -> DT.Text   -- function body
generateBody rdf g alias' name' args' =
  DT.unlines
    [ (ML.makeAssignment g) "output" ((ML.makeCall g) name' castFunctions)
    , (ML.makeReturn g) "output"
    ]
  where
    castFunctions = MU.zipWithOrDie
      (ML.makeCall g)
      (seekCasters rdf (ML.languageName g) alias')
      (map (\x -> [x]) args')

seekCasters :: DR.Rdf a
  => DR.RDF a
  -> DT.Text   -- the foreign language
  -> DT.Text   -- the function name
  -> [DT.Text] -- the casting function for each input to the functions
seekCasters rdf lang' func' = case (
      MW.getTypeDeclaration rdf func' "Morloc"
    , MW.getTypeDeclaration rdf func' lang'
  ) of
    -- seek a special caster for each argument
    ([_], [specialForm]) ->
      map (seekCaster rdf lang') (MW.elements rdf specialForm >>= MW.down rdf (MW.o "rdf:type"))
    -- seek a generic caster
    ([generalForm], []) -> case MW.elements rdf generalForm of
      types -> take (length types) (repeat (seekGenericCaster rdf lang'))
    ([], _) -> error ("Cast failure: No general type found for '" ++ DT.unpack func' ++ "'")
    _ -> error "Ambiguous types"

-- | seek `<func> <lang> :: unpack => JSON -> <type>`
seekCaster :: DR.Rdf a => DR.RDF a -> DT.Text -> DR.Node -> DT.Text
seekCaster rdf lang' form' = case MW.getUnpack rdf lang' form' of
  [f] -> f
  [] -> error ("No unpacker found for " ++ DT.unpack lang' ++ ":(" ++ show form' ++ ")")
  _ -> error ("Ambiguous unpacker for " ++ DT.unpack lang' ++ ":(" ++ show form' ++ ")")

-- | seek `<func'> <lang'> :: unpack => JSON -> a`
seekGenericCaster :: DR.Rdf a => DR.RDF a -> DT.Text -> DT.Text
seekGenericCaster rdf lang' = case MW.getUnpack rdf lang' to of
  [f] -> f
  [] -> error ("No generic unpacker found for '" ++ DT.unpack lang' ++ "'")
  _ -> error ("Ambiguous generic unpacker for '" ++ DT.unpack lang' ++ "'")
  where
    to = MW.v (Just "morloc:atomicGeneric") "a"

generateFunction :: DR.Rdf a
  => DR.RDF a
  -> [Source] -- needed to decide whether this is an internal or external function
  -> ML.CodeGenerator
  -> Call 
  -> DT.Text 
-- TODO: use src to separate internal/external functions
generateFunction rdf _ g n =
  case
    (
        MW.value rdf n >>= MW.rdftype rdf >>= MW.valueOf
        -- FIXME: this only works for primitives
      , MW.elements rdf n >>= MW.rdftype rdf >>= MW.valueOf
      , MW.getScope rdf n >>= MW.elements rdf >>= MW.rdftype rdf >>= MW.valueOf 
      , MW.idOf n
    )
  of
  ([name'], args', bndvars', [mid']) ->
    (ML.makeFunction g)
      ((ML.makeManifoldName g) mid')
      bndvars'
      ((ML.makeCall g) name' args')
  ([], _, _, _) -> error "Invalid RDF: no name for function"
  (_, _, _, []) -> error "Invalid RDF: no id for function"
  _ -> error "Invalid RDF: failed to generate function"
