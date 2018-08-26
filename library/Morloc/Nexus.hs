{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Nexus
Description : Code generators for the user interface
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Nexus (
      perlNexus
  ) where

import Morloc.Operators
import Morloc.Types
import Morloc.Quasi
import qualified Morloc.Util as MU
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DL
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

toLazy :: [[Maybe DT.Text]] -> [[Maybe DL.Text]]
toLazy = map (map (fmap DL.fromStrict)) 

perlNexus :: SparqlEndPoint -> IO DT.Text
perlNexus = perlNexus' f g where
  f = undefined
  g = undefined

perlNexus'
  :: (SparqlEndPoint -> IO [[Maybe DT.Text]]) -- SPARQL query to get morloc names
  -> (SparqlEndPoint -> IO [[Maybe DT.Text]]) -- SPARQL query to get data needed to build function calls
  -> SparqlEndPoint
  -> IO DT.Text
perlNexus' f g ep = fmap render (main <$> f' <*> g') where
  f' :: IO [Doc]
  f' = fmap  (map prepf . toLazy) (f ep)

  g' :: IO [(Doc, Doc, Doc, Doc, Doc)]
  g' = fmap (map prepg . toLazy) (g ep)

  prepf :: [Maybe DL.Text] -> Doc
  prepf [Just x] = text x
  prepf _ = error "Invalid SPARQL return"

  prepg :: [Maybe DL.Text] -> (Doc, Doc, Doc, Doc, Doc)
  prepg [Just name, Just nargs, Just prog, Just pool, Just mid]
    =   (text name, text nargs, text prog, text pool, text mid)
  prepg _ = error "Invalid SPARQL return"


main :: [Doc] -> [(Doc, Doc, Doc, Doc, Doc)] -> Doc
main names fdata = [s|
#!/usr/bin/env perl

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

hashmapT names = [s|my %cmds = ${tupled (map hashmapEntryT names)};|]

hashmapEntryT n = [s|${n} => \&call_${n}|]

usageT names = [s|
sub usage{
    print STDERR "The following commands are exported:\n";
    ${vsep (map usageLineT names)}
    exit 0;
}
|]

usageLineT n = [s|print STDERR "  ${n}\n";|]

functionT (name, nargs, prog, pool, mid) = [s|
sub call_${name}{
    if(scalar(@_) != ${nargs}){
        print STDERR "Expected ${nargs} arguments to '${name}', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `${prog} ${pool} ${mid} '$_[0]'`
}
|]
