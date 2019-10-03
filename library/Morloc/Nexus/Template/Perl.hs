{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.Nexus.Template.Perl
Description : Templates for generating a Perl nexus
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.Nexus.Template.Perl
  ( generate
  ) where

import Morloc.Data.Doc
import Morloc.Namespace
import Morloc.Quasi
import qualified Control.Monad as CM
import qualified Morloc.Config as MC
import qualified Morloc.Data.Text as MT
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM

-- | A function for building a pool call
type PoolBuilder
   = MDoc -- pool name
      -> MDoc -- pool id
          -> [MDoc] -- output list of CLI arguments

type FData = (PoolBuilder, MDoc, Int, MDoc, MDoc)

generate :: [Manifold] -> MorlocMonad Script
generate all_manifolds = do
  let manifolds = filter isExported all_manifolds
      names = map (pretty . getName) manifolds -- [MDoc]
  fdata <- CM.mapM getFData manifolds -- [FData]
  return $
    Script
      { scriptBase = "nexus"
      , scriptLang = ML.PerlLang
      , scriptCode = render $ main names fdata
      , scriptCompilerFlags = []
      , scriptInclude = []
      }

getName :: Manifold -> MT.Text
getName m = maybe (mMorlocName m) id (mComposition m)

getNArgs :: Manifold -> Int
getNArgs m
  | isJust (mComposition m) = length (mBoundVars m)
  | otherwise = length (mArgs m)

getFData :: Manifold -> MorlocMonad FData
getFData m = do
  config <- MM.ask
  let mid' = pretty . MT.show' $ mid m
      lang = mLang m
  case MC.getPoolCallBuilder config lang id of
    (Just call') ->
      return $
      ( call'
      , pretty (getName m)
      , getNArgs m
      , pretty (ML.makeExecutableName lang "pool")
      , mid')
    Nothing ->
      MM.throwError . GeneratorError $
      "No execution method found for language: " <> ML.showLangName lang

isExported :: Manifold -> Bool
isExported m
  -- shallow wrappers around a source function
 =
  (mExported m && not (mCalled m) && mSourced m) || -- compositions
  (mExported m && isJust (mComposition m))

main :: [MDoc] -> [FData] -> MDoc
main names fdata =
  [idoc|#!/usr/bin/env perl

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

usageT :: [FData] -> MDoc
usageT fdata =
  [idoc|
sub usage{
    print STDERR "The following commands are exported:\n";
    #{align $ vsep (map usageLineT fdata)}
    exit 0;
}
|]

usageLineT :: FData -> MDoc
usageLineT (_, name, nargs, _, _) =
  [idoc|print STDERR "  #{name} [#{pretty nargs}]\n";|]

functionT :: FData -> MDoc
functionT (call', name, nargs, pool, mid') =
  [idoc|
sub call_#{name}{
    if(scalar(@_) != #{pretty nargs}){
        print STDERR "Expected #{pretty nargs} arguments to '#{name}', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `#{poolcall}`
}
|]
  where
    poolcall = hsep $ (call' pool mid') ++ map argT [0 .. (nargs - 1)]

argT :: Int -> MDoc
argT i = "$_[" <> pretty i <> "]"
