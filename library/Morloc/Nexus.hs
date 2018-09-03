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
import qualified Morloc.Vortex as V
import Morloc.Util as MU
import qualified Morloc.System as MS
import qualified Data.Text as DT
import qualified Data.HashMap.Strict as Map
import qualified Data.List as DL
import qualified Data.Maybe as DM
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
perlNexus ep = fmap render $ main <$> names <*> fdata where

  manifolds :: IO [V.Manifold]
  manifolds = fmap (filter isExported) (V.buildManifolds ep)

  names :: IO [Doc]
  names = fmap (map (text' . getName)) manifolds

  getName :: V.Manifold -> DT.Text
  getName m = maybe (V.mMorlocName m) id (V.mComposition m)

  getNArgs :: V.Manifold -> Int
  getNArgs m
    | DM.isJust (V.mComposition m) = length (V.mBoundVars m)
    | otherwise = length (V.mArgs m)

  fdata :: IO [(Doc, Int, Doc, Doc, Doc)]
  fdata = fmap (map getFData) manifolds

  getFData :: V.Manifold -> (Doc, Int, Doc, Doc, Doc)
  getFData m = case V.mLang m of
    (Just lang) ->
      ( text' (getName m)
      , getNArgs m
      , text' (MS.findExecutor lang)
      , text' (MS.makePoolName lang)
      , text' (MS.makeManifoldName (V.mCallId m))
      )
    Nothing -> error "A language must be defined"

  isExported :: V.Manifold -> Bool
  isExported m =
    -- shallow wrappers around a source function
    (V.mExported m && not (V.mCalled m) && V.mSourced m)
    || -- compositions
    (V.mExported m && DM.isJust (V.mComposition m))


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

${usageT fdata}

${vsep (map functionT fdata)}
|]

hashmapT names = [idoc|my %cmds = ${tupled (map hashmapEntryT names)};|]

hashmapEntryT n = [idoc|${n} => \&call_${n}|]

usageT fdata = [idoc|
sub usage{
    print STDERR "The following commands are exported:\n";
    ${align $ vsep (map usageLineT fdata)}
    exit 0;
}
|]

usageLineT (name, nargs, _, _, _) = [idoc|print STDERR "  ${name} [${int nargs}]\n";|]

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
