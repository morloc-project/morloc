{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Nexus
Description : Code generators for the user interface
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Nexus (generate) where

import Morloc.Operators
import Morloc.Types
import Morloc.Quasi
import Morloc.Query as Q
import qualified Morloc.System as MS
import qualified Data.Text as DT
import qualified Data.HashMap.Strict as Map
import qualified Data.List as DL
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

-- | Generate the nexus, which is a program that coordinates the execution of
-- the language-specific function pools.
generate :: SparqlEndPoint -> IO Script
generate e
  =   Script
  <$> pure "nexus"
  <*> pure "perl"
  <*> perlNexus e


-- | This query returns the following:
--   1. name - name of an exported Morloc function
--   2. mid - id of the specific morloc manifold (id of the type declaration)
--   3. nargs - number of arguments the function takes
forNexusCallQ :: SparqlEndPoint -> IO [[Maybe DT.Text]]
forNexusCallQ = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  SELECT ?fname ?typedec (count (distinct ?element) as ?nargs)
  WHERE {
    ?typedec rdf:type mlc:typeDeclaration ;
       mlc:lang "Morloc" ;
       mlc:lhs ?fname ;
       mlc:rhs ?type .
    ?type rdf:type mlc:functionType .
    ?arg ?element ?type .
    FILTER(regex(str(?element), "_[0-9]+$", "i"))
    ?e rdf:type mlc:export ;
       rdf:value ?fname .
  }
  GROUP BY ?typedec ?fname ?lang
|]

declaration2lang :: SparqlEndPoint -> IO [[Maybe DT.Text]]
declaration2lang = [sparql|
  PREFIX mlc: <http://www.morloc.io/ontology/000/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  SELECT ?declaredName ?callName ?lang
  WHERE {
    {
      ?typedec rdf:type mlc:dataDeclaration ;
         mlc:lhs ?declaredName ;
         mlc:rhs ?rhs .
      ?rhs rdf:type mlc:call ;
           rdf:value ?callid .
      ?callid rdf:type mlc:name ; 
              rdf:value ?callName .
      OPTIONAL { 
        ?src rdf:type mlc:source ;
             mlc:lang ?lang ;
             mlc:import ?i .
        ?i mlc:alias ?callName .
      }
    } UNION {
      ?typedec rdf:type mlc:typeDeclaration ;
        mlc:lang "Morloc" ;
        mlc:lhs ?declaredName ;
        mlc:rhs ?type .
      ?type rdf:type mlc:functionType .
      ?src rdf:type mlc:source ;
           mlc:lang ?lang ;
           mlc:import ?i .
      ?i mlc:alias ?declaredName .
    }
  }
|]

langMap :: SparqlEndPoint -> IO (Map.HashMap Name (Maybe Name, Lang))
langMap e = fmap (makemap . map toThree) (declaration2lang e) where

  toThree :: [Maybe DT.Text] -> (Name, (Maybe Name, Maybe Lang))
  toThree [Just name, callName, lang] = (name, (callName, lang)) 
  toThree _ = error "Unexpected SPARQL return value"

  makemap
    :: [(Name, (Maybe Name, Maybe Lang))]
    -> Map.HashMap Name (Maybe Name, Lang)
  makemap xs = case
      ( [(x, (y, z)) | (x, (y       , Just z )) <- xs] -- those with lang
      , [(x,  y)     | (x, (Just y  , Nothing)) <- xs] -- those without
      , [ x          | (x, (Nothing , Nothing)) <- xs] -- anomalies
      ) of
    (xs, ys, []) -> fillHash (Map.fromList xs) ys 
    (_, _, xs) -> error ("Anomalous functions: " ++ show xs)

  fillHash
    :: Map.HashMap Name (Maybe Name, Lang)
    -> [(Name, Name)]
    -> Map.HashMap Name (Maybe Name, Lang)
  fillHash hash [] = hash 
  fillHash hash xs =
    case
      DL.partition (\x -> Map.member (snd x) hash) xs 
    of
      ([], unfound) -> error $  "Could not find source language for functions: "
                             ++ show (map fst unfound) 
      (found, unfound) -> fillHash (insert' hash found) unfound

  insert'
    :: Map.HashMap Name (Maybe Name, Lang)
    -> [(Name, Name)]
    -> Map.HashMap Name (Maybe Name, Lang)
  insert' hash xs = Map.union hash (Map.fromList (map (toPair hash) xs))

  toPair
    :: Map.HashMap Name (Maybe Name, Lang)
    -> (Name, Name)
    -> (Name, (Maybe Name, Lang))
  toPair hash (x,y) = case Map.lookup y hash of
    (Just (_, lang)) -> (x, (Just y, lang))
    _ -> error "Should not happen"
     

perlNexus :: SparqlEndPoint -> IO DT.Text
perlNexus ep = fmap render (main <$> f' <*> g') where
  f' :: IO [Doc]
  f' = fmap (map prepf) (Q.exportsQ ep)

  prepf :: [Maybe DT.Text] -> Doc
  prepf [Just x] = text' x
  prepf _ = error "Invalid SPARQL return"

  g' :: IO [(Doc, Int, Doc, Doc, Doc)]
  g' = forNexusCallQ ep >>= doLMap

  doLMap :: [[Maybe DT.Text]] -> IO [(Doc, Int, Doc, Doc, Doc)]
  doLMap xss = fmap (foo xss) (langMap ep)

  foo
    :: [[Maybe DT.Text]]
    -> Map.HashMap Name (Maybe Name, Lang)
    -> [(Doc, Int, Doc, Doc, Doc)]
  foo xss hash = map (prepg hash) xss

  prepg
    :: Map.HashMap Name (Maybe Name, Lang)
    -> [Maybe DT.Text]
    -> (Doc, Int, Doc, Doc, Doc)
  prepg hash [Just name, Just uid, Just nargs] = case (Map.lookup name hash) of
    Just (_, lang) -> ( text' name
                      , (read (DT.unpack nargs) :: Int)
                      , text' (MS.findExecutor lang)
                      , text' (MS.makePoolName lang)
                      , text' (MS.makeManifoldName uid)
                      )
  prepg _ _ = error "Invalid SPARQL return"


main :: [Doc] -> [(Doc, Int, Doc, Doc, Doc)] -> Doc
main names fdata = [idoc|#!/usr/bin/env perl

use strict;
use warnings;

&printResult(&dispatch(@ARGV));

sub printResult {
    my $result = shift;
    print "$result";
}

sub dispatch {
    if(scalar(@_) == 0){
        &usage();
    }

    my $cmd = shift;
    my $result = undef;

    ${hashmapT names}

    if($cmd eq '-h' || $cmd eq '-?' || $cmd eq '--help' || $cmd eq '?'){
        &usage();
    }

    if(exists($cmds{$cmd})){
        $result = $cmds{$cmd}(@_);
    } else {
        print STDERR "Command '$cmd' not found";
        &usage();
    }

    return $result;
}

${usageT names}

${vsep (map functionT fdata)}
|]

hashmapT names = [idoc|my %cmds = ${tupled (map hashmapEntryT names)};|]

hashmapEntryT n = [idoc|${n} => \&call_${n}|]

usageT names = [idoc|
sub usage{
    print STDERR "The following commands are exported:\n";
    ${align $ vsep (map usageLineT names)}
    exit 0;
}
|]

usageLineT n = [idoc|print STDERR "  ${n}\n";|]

functionT (name, nargs, prog, pool, mid) = [idoc|
sub call_${name}{
    if(scalar(@_) != ${int nargs}){
        print STDERR "Expected ${int nargs} arguments to '${name}', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `${prog} ${pool} ${mid} ${hsep $ map argT [0..(nargs-1)]}`
}
|]

argT i = [idoc|'$_[${int i}]'|]
