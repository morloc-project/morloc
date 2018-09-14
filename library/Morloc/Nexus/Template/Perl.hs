{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Nexus.Template.Perl
Description : Templates for generating a Perl nexus
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Nexus.Template.Perl (generate) where

import Morloc.Operators
import Morloc.Types
import Morloc.Quasi
import Morloc.Util as MU
import Morloc.Component.Manifold as MCM
import Morloc.Data.Doc hiding ((<$>))
import qualified Morloc.Data.Text as MT
import qualified Morloc.System as MS
import qualified Data.Map.Strict as Map
import qualified Data.List as DL
import qualified Data.Maybe as DM

generate e
  =   Script
  <$> pure "nexus"
  <*> pure "perl"
  <*> makeNexus e

makeNexus :: SparqlDatabaseLike db => db -> IO MT.Text
makeNexus ep = fmap render $ main <$> names <*> fdata where

  manifolds :: IO [Manifold]
  manifolds = fmap (filter isExported) (MCM.fromSparqlDb ep)

  names :: IO [Doc]
  names = fmap (map (text' . getName)) manifolds

  getName :: Manifold -> MT.Text
  getName m = maybe (mMorlocName m) id (mComposition m)

  getNArgs :: Manifold -> Int
  getNArgs m
    | DM.isJust (mComposition m) = length (mBoundVars m)
    | otherwise = length (mArgs m)

  fdata :: IO [(Doc, Int, Doc, Doc, Doc)]
  fdata = fmap (map getFData) manifolds

  getFData :: Manifold -> (Doc, Int, Doc, Doc, Doc)
  getFData m = case mLang m of
    (Just lang) ->
      ( text' (getName m)
      , getNArgs m
      , text' (MS.findExecutor lang)
      , text' (MS.makePoolName lang)
      , text' (MS.makeManifoldName (mCallId m))
      )
    Nothing -> error "A language must be defined"

  isExported :: Manifold -> Bool
  isExported m =
    -- shallow wrappers around a source function
    (mExported m && not (mCalled m) && mSourced m)
    || -- compositions
    (mExported m && DM.isJust (mComposition m))


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

    ${mapT names}

    if($cmd eq '-h' || $cmd eq '-?' || $cmd eq '--help' || $cmd eq '?'){
        &usage();
    }

    if(exists($cmds{$cmd})){
        $result = $cmds{$cmd}(@_);
    } else {
        print STDERR "Command '$cmd' not found\n";
        &usage();
    }

    return $result;
}

${usageT fdata}

${vsep (map functionT fdata)}
|]

mapT names = [idoc|my %cmds = ${tupled (map mapEntryT names)};|]

mapEntryT n = [idoc|${n} => \&call_${n}|]

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
