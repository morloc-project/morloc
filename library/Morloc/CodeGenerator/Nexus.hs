{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Nexus
Description : Templates for generating a Perl nexus
Copyright   : (c) Zebulun Arendsee, 2018
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Nexus
  ( generate
  ) where

import Morloc.Data.Doc
import Morloc.Namespace
import Morloc.Quasi
import Morloc.Pretty (prettyType)
import qualified Control.Monad as CM
import qualified Morloc.Config as MC
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM

-- | A function for building a pool call
type PoolBuilder
   = MDoc -- pool name
   -> MDoc -- pool id
   -> [MDoc] -- output list of CLI arguments

type FData = (PoolBuilder, MDoc, Int, MDoc, MDoc)

say :: Doc ann -> MorlocMonad ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

generate :: [(ConcreteType, Int, Maybe EVar)] -> MorlocMonad Script
generate xs = do
  let names = [pretty name | (_, _, Just name) <- xs]
  fdata <- CM.mapM getFData [(t, i, n) | (t, i, Just n) <- xs] -- [FData]
  return $
    Script
      { scriptBase = "nexus"
      , scriptLang = ML.PerlLang
      , scriptCode = Code . render $ main names fdata
      , scriptCompilerFlags = []
      , scriptInclude = []
      }

getFData :: (ConcreteType, Int, EVar) -> MorlocMonad FData
getFData (t, i, n) = do
  config <- MM.ask
  let mid' = pretty i
      lang = langOf' t
  case MC.getPoolCallBuilder config lang id of
    (Just call') ->
      return $
      ( call'
      , pretty n
      , nargs (typeOf t)
      , pretty (ML.makeExecutableName lang "pool")
      , mid')
    Nothing ->
      MM.throwError . GeneratorError $
      "No execution method found for language: " <> ML.showLangName lang

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
