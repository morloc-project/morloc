{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Nexus
Description : Templates for generating a Perl nexus
Copyright   : (c) Zebulun Arendsee, 2020
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Nexus
  ( generate
  ) where

import Morloc.Data.Doc
import Morloc.CodeGenerator.Namespace
import Morloc.Quasi
import Morloc.Pretty (prettyType)
import qualified Control.Monad as CM
import qualified Morloc.Config as MC
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM

type FData =
  ( MDoc -- pool call command, (e.g., "RScript pool.R 4 --")
  , MDoc -- subcommand name
  , Type -- argument type
  )

say :: Doc ann -> MorlocMonad ()
say d = liftIO . putDoc $ " : " <> d <> "\n"

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

generate :: [(EVar, MDoc, [EVar])] -> [(CType, Int, Maybe EVar)] -> MorlocMonad Script
generate cs xs = do
  let names = [pretty name | (_, _, Just name) <- xs] ++ map (pretty . fst3) cs
  fdata <- CM.mapM getFData [(t, i, n) | (t, i, Just n) <- xs] -- [FData]
  return $
    Script
      { scriptBase = "nexus"
      , scriptLang = ML.PerlLang
      , scriptCode = Code . render $ main names fdata cs
      , scriptCompilerFlags = []
      , scriptInclude = []
      }

getFData :: (CType, Int, EVar) -> MorlocMonad FData
getFData (t, i, n) = do
  config <- MM.ask
  let lang = langOf t
  case MC.buildPoolCallBase config lang i of
    (Just cmds) -> return (hsep cmds, pretty n, typeOf t)
    Nothing ->
      MM.throwError . GeneratorError $
      "No execution method found for language: " <> ML.showLangName (fromJust lang)

main :: [MDoc] -> [FData] -> [(EVar, MDoc, [EVar])] -> MDoc
main names fdata cdata =
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

#{usageT fdata cdata}

#{vsep (map functionCT cdata)}

#{vsep (map functionT fdata)}

|]

mapT names = [idoc|my %cmds = #{tupled (map mapEntryT names)};|]

mapEntryT n = [idoc|#{n} => \&call_#{n}|]

usageT :: [FData] -> [(EVar, MDoc, [EVar])] -> MDoc
usageT fdata cdata =
  [idoc|
sub usage{
    print STDERR "The following commands are exported:\n";
    #{align $ vsep (map usageLineT fdata ++ map usageLineConst cdata)}
    exit 0;
}
|]

usageLineT :: FData -> MDoc
usageLineT (_, name, t) =
  [idoc|print STDERR "  #{name} [#{pretty (nargs t)}]\n";|]

usageLineConst :: (EVar, MDoc, [EVar]) -> MDoc
usageLineConst (v, d, _) = [idoc|print STDERR "  #{pretty v} [0]\n";|]

functionT :: FData -> MDoc
functionT (cmd, name, t) =
  [idoc|
sub call_#{name}{
    if(scalar(@_) != #{pretty n}){
        print STDERR "Expected #{pretty n} arguments to '#{name}', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `#{poolcall}`;
}
|]
  where
    n = nargs t
    poolcall = hsep $ cmd : map argT [0 .. (n - 1)]

functionCT :: (EVar, MDoc, [EVar]) -> MDoc
functionCT (v, d, vs) =
  [idoc|
sub call_#{pretty v}{
    if(scalar(@_) != #{pretty $ length vs}){
        print STDERR "Expected #{pretty $ length vs} arguments to '#{pretty v}', given " . scalar(@_) . "\n";
        exit 1;
    }
    my $x = '#{d}';
    #{replacements}
    return ($x . "\n");
}
|]
  where
    replacements = vsep (zipWith (\v i-> [idoc|$x =~ s/\Q<<#{pretty v}>>/$_[#{viaShow i}]/g;|]) vs [0..])

argT :: Int -> MDoc
argT i = "$_[" <> pretty i <> "]"
