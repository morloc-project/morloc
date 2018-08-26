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
import Text.PrettyPrint.Leijen.Text hiding ((<$>))


-- | Generate the nexus, which is a program that coordinates the execution of
-- the language-specific function pools.
generate :: SparqlEndPoint -> IO Script
generate e
  =   Script
  <$> pure "nexus"
  <*> pure "perl"
  <*> perlNexus e

perlNexus :: SparqlEndPoint -> IO DT.Text
perlNexus e = perlNexus' e Q.exportsQ Q.forNexusCallQ where

perlNexus'
  :: SparqlEndPoint
  -> (SparqlEndPoint -> IO [[Maybe DT.Text]]) 
      -- ^ SPARQL query to get morloc names
  -> (SparqlEndPoint -> IO [[Maybe DT.Text]]) 
      -- ^ SPARQL query to get data needed to build function calls
  -> IO DT.Text
perlNexus' ep f g = fmap render (main <$> f' <*> g') where
  f' :: IO [Doc]
  f' = fmap (map prepf) (f ep)

  g' :: IO [(Doc, Doc, Doc, Doc, Doc)]
  g' = fmap (map prepg) (g ep)

  prepf :: [Maybe DT.Text] -> Doc
  prepf [Just x] = text' x
  prepf _ = error "Invalid SPARQL return"

  -- The input from the SPARQL query is:
  --   1. name - name of an exported Morloc function
  --   2. lang - the language the function is exported from
  --   3. mid - id of the specific morloc manifold (id of the type declaration)
  --   4. nargs - number of arguments the function takes
  -- The output tuple has the following fields
  --   1. name - name of an exported Morloc function
  --   2. nargs - number of arguments the function takes
  --   3. prog - name of the program the will execute the pool
  --   4. pool - name of the pool
  --   5. mid - id of the specific morloc manifold (id of the type declaration)
  prepg :: [Maybe DT.Text] -> (Doc, Doc, Doc, Doc, Doc)
  prepg [Just name, Just lang, Just uid, Just nargs]
    =   ( text' name
        , text' nargs
        , text' (MS.findExecutor lang)
        , text' (MS.makePoolName lang)
        , text' (MS.makeManifoldName uid)
        )
  prepg _ = error "Invalid SPARQL return"


main :: [Doc] -> [(Doc, Doc, Doc, Doc, Doc)] -> Doc
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
    ${vsep (map usageLineT names)}
    exit 0;
}
|]

usageLineT n = [idoc|print STDERR "  ${n}\n";|]

functionT (name, nargs, prog, pool, mid) = [idoc|
sub call_${name}{
    if(scalar(@_) != ${nargs}){
        print STDERR "Expected ${nargs} arguments to '${name}', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `${prog} ${pool} ${mid} '$_[0]'`
}
|]
