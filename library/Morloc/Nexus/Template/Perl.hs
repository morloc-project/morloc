{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Nexus.Template.Perl
Description : Templates for generating a Perl nexus
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}

module Morloc.Nexus.Template.Perl (generate) where

import Morloc.Global
import Morloc.Operators
import Morloc.Quasi
import Morloc.Data.Doc hiding ((<$>), (<>))
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Data.Maybe as DM
import qualified Control.Monad as CM

-- | A function for building a pool call
type PoolBuilder
  =  Doc    -- pool name
  -> Doc    -- pool id
  -> [Doc]  -- output list of CLI arguments

type FData = (PoolBuilder, Doc, Int, Doc, Doc)

generate :: [Manifold] -> MorlocMonad Script
generate all_manifolds = do
  let manifolds = filter isExported all_manifolds
      names = map (text' . getName) manifolds     -- [Doc]
  fdata <- CM.mapM getFData manifolds -- [FData]
  return $ Script { scriptBase = "nexus"
                  , scriptLang = ML.PerlLang
                  , scriptCode = render $ main names fdata
                  , scriptCompilerFlags = []
                  }

getName :: Manifold -> MT.Text
getName m = maybe (mMorlocName m) id (mComposition m)

getNArgs :: Manifold -> Int
getNArgs m
  | DM.isJust (mComposition m) = length (mBoundVars m)
  | otherwise = length (mArgs m)

getFData :: Manifold -> MorlocMonad FData
getFData m = do
  config <- MM.ask
  let mid' = text' . MT.show' $ mid m
      lang = mLang m
  case MC.getPoolCallBuilder config lang id of
    (Just call') -> return $
      ( call'
      , text' (getName m)
      , getNArgs m
      , text' (ML.makeExecutableName lang "pool")
      , mid' 
      )
    Nothing -> MM.throwError . GeneratorError $
      "No execution method found for language: " <> ML.showLangName lang

isExported :: Manifold -> Bool
isExported m =
  -- shallow wrappers around a source function
  (mExported m && not (mCalled m) && mSourced m)
  || -- compositions
  (mExported m && DM.isJust (mComposition m))


main :: [Doc] -> [FData] -> Doc
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

    #{mapT names}

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

#{usageT fdata}

#{vsep (map functionT fdata)}
|]

mapT names = [idoc|my %cmds = #{tupled (map mapEntryT names)};|]

mapEntryT n = [idoc|#{n} => \&call_#{n}|]

usageT fdata = [idoc|
sub usage{
    print STDERR "The following commands are exported:\n";
    #{align $ vsep (map usageLineT fdata)}
    exit 0;
}
|]

usageLineT (_, name, nargs, _, _) = [idoc|print STDERR "  #{name} [#{int nargs}]\n";|]

functionT (call', name, nargs, pool, mid') = [idoc|
sub call_#{name}{
    if(scalar(@_) != #{int nargs}){
        print STDERR "Expected #{int nargs} arguments to '#{name}', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `#{poolcall}`
}
|] where
  poolcall = hsep $ (call' pool mid') ++ map argT [0..(nargs-1)]

argT i = [idoc|'$_[#{int i}]'|]
