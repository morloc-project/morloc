{-# LANGUAGE OverloadedStrings #-}

module Morloc.Nexus (
      NexusGenerator(..)
    , perlCliNexusGenerator
  ) where

import Morloc.Operators
import qualified Morloc.Util as MU
import qualified Data.Text as DT

data NexusGenerator = NexusGenerator {
    nexusPrologue    
    :: DT.Text
  , nexusPrint
    :: DT.Text
    -> DT.Text
  , nexusDispatch
    :: [DT.Text]
    -> DT.Text
    -- make a help function
  , nexusHelp
    :: [DT.Text] -- TODO: make this more expressive
    -> DT.Text
    -- make a funtion that calls a function in a particular pool 
  , nexusCall
    :: DT.Text -- the command for calling the pool (e.g. Rscript or python)
    -> DT.Text -- the pool name
    -> DT.Text -- function name
    -> DT.Text -- manifold name
    -> Int     -- number of arguments
    -> DT.Text -- call function
  , nexusEpilogue
    :: DT.Text
}

show' :: Show a => a -> DT.Text
show' x = DT.pack (show x)

perlCliNexusGenerator :: NexusGenerator
perlCliNexusGenerator = NexusGenerator {
      nexusPrologue = nexusPrologue'
    , nexusPrint    = nexusPrint'
    , nexusDispatch = nexusDispatch'
    , nexusHelp     = nexusHelp'
    , nexusCall     = nexusCall'
    , nexusEpilogue = nexusEpilogue'
  }
  where
    nexusPrologue' = DT.unlines
      [ "#!/usr/bin/env perl"
      , "use strict;"
      , "use warnings;"
      , ""
      , "&printResult(&dispatch(@ARGV));"
      ]

    nexusPrint' _ = DT.unlines
      [ "sub printResult {"
      , "    my $result = shift;"
      , "    print \"$result\";"
      , "}"
      ]

    nexusDispatch' functions = DT.unlines
      [ "sub dispatch {"
      , "    if(scalar(@_) == 0){"
      , "        &usage();"
      , "    }"
      , ""
      , makeCmdHash functions
      , ""
      , "    my $cmd = shift;"
      , "    my $result = undef;"
      , ""
      , "    if($cmd eq '-h' || $cmd eq '-?' || $cmd eq '--help' || $cmd eq '?'){"
      , "        &usage();"
      , "    }"
      , "    if(exists($cmds{$cmd})){"
      , "        $result = $cmds{$cmd}(@_);"
      , "    } else {"
      , "        print STDERR \"Command '$cmd' not found\";"
      , "        &usage();"
      , "    }"
      , ""
      , "    return $result;"
      , "}"
      ]

    makeCmdHash :: [DT.Text] -> DT.Text
    makeCmdHash fs = MU.indent 4 . DT.unlines $
      [ "my %cmds = ("
      , MU.indent 4 . DT.intercalate ",\n" . map makeHashEntry $ fs
      , ");"
      ]

    makeHashEntry :: DT.Text -> DT.Text
    makeHashEntry f = f <> " => \\&" <> makeCallName f

    makeCallName :: DT.Text -> DT.Text
    makeCallName f = "call_" <> f

    nexusHelp' :: [DT.Text] -> DT.Text
    nexusHelp' _ = makeFunction "usage" $ DT.unlines
      [ "print STDERR \"this is a help message\\n\";"
      , "exit 0;"
      ]

    nexusCall' prog filename name mid nargs =
      makeFunction
        (makeCallName name)
        (DT.unlines
          [ "if(scalar(@_) != " <> show' nargs <> "){"
          , "    print STDERR \"Expected "
            <> show' nargs
            <> " arguments to 'sample_index', given \" . "
          , "    scalar(@_) . \"\\n\";"
          , "    exit 1;"
          , "}"
          , "return `" <> makePoolCall prog filename mid nargs <> "`"
          ]
        )

    makePoolCall prog filename manifold j
      = DT.unwords [prog, filename, manifold, makePoolArgList j]

    makePoolArgList 0 = ""
    makePoolArgList j = DT.unwords ["$_[" <> show' j <> "]" | j <- [0..(j-1)]]

    makeFunction :: DT.Text -> DT.Text -> DT.Text
    makeFunction name body = "sub " <> name <> "{\n" <> MU.indent 4 body <> "\n}"

    nexusEpilogue' = ""
