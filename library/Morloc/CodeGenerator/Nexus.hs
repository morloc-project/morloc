{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-|
Module      : Morloc.CodeGenerator.Nexus
Description : Templates for generating a Perl nexus
Copyright   : (c) Zebulun Arendsee, 2021
License     : GPL-3
Maintainer  : zbwrnz@gmail.com
Stability   : experimental
-}
module Morloc.CodeGenerator.Nexus
  ( generate
  ) where

import qualified Control.Monad.State as CMS
import Morloc.Data.Doc
import Morloc.CodeGenerator.Namespace
import Morloc.Quasi
import Morloc.Pretty ()
import qualified Morloc.Data.Text as MT
import qualified Control.Monad as CM
import qualified Morloc.Config as MC
import qualified Morloc.Language as ML
import qualified Morloc.Monad as MM

type FData =
  ( MDoc -- pool call command, (e.g., "RScript pool.R 4 --")
  , MDoc -- subcommand name
  , TypeP -- argument type
  )

generate :: [NexusCommand] -> [(TypeP, Int)] -> MorlocMonad Script
generate cs xs = do

  callNames <- mapM MM.metaName (map snd xs) |>> catMaybes |>> map pretty
  let gastNames = map (pretty . commandName) cs
      names = callNames <> gastNames 
  fdata <- CM.mapM getFData xs -- [FData]
  outfile <- fmap (maybe "nexus.pl" id) $ CMS.gets stateOutfile
  return $
    Script
      { scriptBase = outfile
      , scriptLang = ML.PerlLang
      , scriptCode = "." :/ File outfile (Code . render $ main names fdata cs)
      , scriptMake = [SysExe outfile]
      }

getFData :: (TypeP, Int) -> MorlocMonad FData
getFData (t, i) = do
  mayName <- MM.metaName i
  case mayName of
    (Just name') -> do
      config <- MM.ask
      let lang = langOf t
      case MC.buildPoolCallBase config lang i of
        (Just cmds) -> return (hsep cmds, pretty name', t)
        Nothing ->
          MM.throwError . GeneratorError $
          "No execution method found for language: " <> ML.showLangName (fromJust lang)
    (Nothing) -> MM.throwError . GeneratorError $ "No name in FData"

main :: [MDoc] -> [FData] -> [NexusCommand] -> MDoc
main names fdata cdata =
  [idoc|#!/usr/bin/env perl

use strict;
use warnings;

use JSON::XS;

my $json = JSON::XS->new->canonical;

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

#{vsep (map functionCT cdata ++ map functionT fdata)}

|]

mapT names = [idoc|my %cmds = #{tupled (map mapEntryT names)};|]

mapEntryT n = [idoc|#{n} => \&call_#{n}|]

usageT :: [FData] -> [NexusCommand] -> MDoc
usageT fdata cdata =
  [idoc|
sub usage{
    print STDERR "The following commands are exported:\n";
    #{align $ vsep (map usageLineT fdata ++ map usageLineConst cdata)}
    exit 0;
}
|]

usageLineT :: FData -> MDoc
usageLineT (_, name', t) = vsep
  ( [idoc|print STDERR "  #{name'}\n";|]
  : writeTypes (gtypeOf t)
  )

gtypeOf (UnkP (PV _ (Just v) _)) = UnkT (TV Nothing v)
gtypeOf (VarP (PV _ (Just v) _)) = VarT (TV Nothing v)
gtypeOf (FunP ts t) = FunT (map gtypeOf ts) (gtypeOf t)
gtypeOf (AppP t ts) = AppT (gtypeOf t) (map gtypeOf ts)
gtypeOf (NamP o (PV _ (Just n) _) ps rs)
  = NamT o (TV Nothing n)
    (map gtypeOf ps)
    [(k, gtypeOf t) | (PV _ (Just k) _, t) <- rs]
gtypeOf _ = UnkT (TV Nothing "?") -- this shouldn't happen


usageLineConst :: NexusCommand -> MDoc
usageLineConst cmd = vsep
  ( [idoc|print STDERR "  #{pretty (commandName cmd)}\n";|]
  : writeTypes (commandType cmd) 
  )

writeTypes :: Type -> [MDoc]
writeTypes (FunT inputs output)
  = zipWith writeType (map Just [1..]) inputs
  ++ writeTypes output
writeTypes t = [writeType Nothing t]

writeType :: Maybe Int -> Type -> MDoc
writeType (Just i) t  = [idoc|print STDERR q{    param #{pretty i}: #{pretty t}}, "\n";|]
writeType (Nothing) t = [idoc|print STDERR q{    return: #{pretty t}}, "\n";|]


functionT :: FData -> MDoc
functionT (cmd, name', t) =
  [idoc|
sub call_#{name'}{
    if(scalar(@_) != #{pretty n}){
        print STDERR "Expected #{pretty n} arguments to '#{name'}', given " . 
        scalar(@_) . "\n";
        exit 1;
    }
    return `#{poolcall}`;
}
|]
  where
    n = nargs t
    poolcall = hsep $ cmd : map argT [0 .. (n - 1)]

functionCT :: NexusCommand -> MDoc
functionCT (NexusCommand cmd _ json_str args subs) =
  [idoc|
sub call_#{pretty cmd}{
    if(scalar(@_) != #{pretty $ length args}){
        print STDERR "Expected #{pretty $ length args} arguments to '#{pretty cmd}', given " . scalar(@_) . "\n";
        exit 1;
    }
    my $json_obj = $json->decode(q{#{json_str}});
    #{align . vsep $ readArguments ++ replacements}
    return ($json->encode($json_obj) . "\n");
}
|]
  where
    readArguments = zipWith readJsonArg args [1..]
    replacements = map (uncurry3 replaceJson) subs

replaceJson :: JsonPath -> MT.Text -> JsonPath -> MDoc
replaceJson pathTo v pathFrom
  = (access "$json_obj" pathTo)
  <+> "="
  <+> (access ([idoc|$json_#{pretty v}|]) pathFrom)
  <> ";"

access :: MDoc -> JsonPath -> MDoc
access v ps = cat $ punctuate "->" (v : map pathElement ps)  

pathElement :: JsonAccessor -> MDoc
pathElement (JsonIndex i) = brackets (pretty i)
pathElement (JsonKey key) = braces (pretty key)

readJsonArg ::EVar -> Int -> MDoc
readJsonArg (EV v) i = [idoc|my $json_#{pretty v} = $json->decode($ARGV[#{pretty i}]); |]

argT :: Int -> MDoc
argT i = "'$_[" <> pretty i <> "]'" 
