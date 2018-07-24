module Morloc.Nexus (
      NexusGenerator(..)
    , perlCliNexusGenerator
  ) where

import qualified Morloc.Data as MD
import qualified Morloc.Util as MU
import qualified Data.List as DL 

data NexusGenerator = NexusGenerator {
    nexusPrologue    
    :: String
    -- make a help function
  , nexusPrint
    :: String
    -> String
  , nexusDispatch
    :: [String]
    -> String
  , nexusHelp
    :: [(MD.DataDecl, MD.TypeDecl)]
    -> String
    -- make a funtion that calls a function in a particular pool 
  , nexusCall
    :: String -- the command for calling the pool (e.g. Rscript or python)
    -> String -- the pool name
    -> ( String   -- function name
       , Int      -- manifold id
       , [String] -- arguments
       )
    -> String -- call function
  , nexusEpilogue
    :: String
}

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
    nexusPrologue' = unlines
      [ "#!/usr/bin/env perl"
      , "use strict;"
      , "use warnings;"
      , ""
      , "&printResult(&dispatch(@ARGV));"
      ]

    nexusPrint' _ = unlines
      [ "sub printResult {"
      , "    my $result = shift;"
      , "    print \"$result\";"
      , "}"
      ]


    nexusDispatch' functions = unlines
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

    makeCmdHash fs = MU.indent 4 . unlines $
      [ "my %cmds = ("
      , MU.indent 4 . DL.intercalate ",\n" . map makeHashEntry $ fs
      , ");"
      ]

    makeHashEntry f = f ++ " => \\&" ++ makeCallName f

    makeCallName f = "call_" ++ f

    nexusHelp' _ = makeFunction "usage" $ unlines
      [ "print STDERR \"this is a help message\\n\";"
      , "exit 0;"
      ]

    nexusCall' prog filename (name, i, args) =
      makeFunction (makeCallName name) $ unlines
        [ "if(scalar(@_) != " ++ show (length args) ++ "){"
        , "    print STDERR \"Expected "
          ++ show (length args)
          ++ " arguments to 'sample_index', given \" . "
        , "    scalar(@_) . \"\\n\";"
        , "    exit 1;"
        , "}"
        , "return `" ++ makePoolCall prog filename (makeManifoldName i) (length args) ++ "`"
        ]

    makePoolCall prog filename manifold j
      = unwords [prog, filename, manifold, makePoolArgList j]

    makePoolArgList 0 = ""
    makePoolArgList j = unwords ["$_[" ++ show j ++ "]" | j <- [0..(j-1)]]

    makeManifoldName i = "m" ++ show i

    makeFunction name body = "sub " ++ name ++ "{\n" ++ MU.indent 4 body ++ "\n}"

    nexusEpilogue' = ""
