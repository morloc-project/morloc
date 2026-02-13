# Compiler Invocation

## Current Implementation

### Nexus Compilation (during `morloc init`)
**File**: `library/Morloc/CodeGenerator/SystemConfig.hs`

The nexus is compiled once during `morloc init`, not per-program. `Nexus.hs` now emits a JSON manifest and copies the pre-compiled binary:

```haskell
return $ Script
  { scriptBase = manifestFile
  , scriptLang = ML.CLang
  , scriptCode = "." :/ File manifestFile (Code manifestJson)
  , scriptMake = [ SysRun . Code $ "cp " <> nexusBin <> " " <> outfile
                 , SysExe outfile ]
  }
```

**Init command**: `gcc -O2 -I<includeDir> -o morloc-nexus nexus.c -L<libDir> -Wl,-rpath,<libDir> -lmorloc -lpthread`

### C++ Pool Compilation
**File**: `library/Morloc/CodeGenerator/Grammars/Translator/Cpp.hs:231-247`

```haskell
makeTheMaker :: [Source] -> MorlocMonad [SysCommand]
makeTheMaker srcs = do
  let outfile = pretty $ ML.makeExecutablePoolName CppLang
  let src = pretty (ML.makeSourcePoolName CppLang)
  (_, flags, includes) <- Mod.handleFlagsAndPaths CppLang srcs
  let incs = [pretty ("-I" <> i) | i <- includes]
  let flags' = map pretty flags
  let cmd = SysRun . Code . render $
        [idoc|g++ -O3 --std=c++17 -o #{outfile} #{src} #{hsep flags'} #{hsep incs}|]
  return [cmd]
```

**Command**: `g++ -O3 --std=c++17 -o pool pool.cpp [flags] -I[includes]`

### System Library Compilation
**File**: `library/Morloc/CodeGenerator/SystemConfig.hs:104-106`

```haskell
let morlocOptions = (if slurmSupport then ["-DSLURM_SUPPORT"] else [])
let gccArgs = ["-O", "-shared", "-o", soPath, "-fPIC", tmpCFile] <> morlocOptions
callProcess "gcc" gccArgs
```

**Command**: `gcc -O -shared -o libmorloc.so -fPIC x.c [-DSLURM_SUPPORT]`

### R Library Compilation
**File**: `library/Morloc/CodeGenerator/SystemConfig.hs:166`

```haskell
let compileCommand = "R CMD SHLIB " ++ sourcePath ++ " -o " ++ libPath
callCommand compileCommand
```

**Command**: `R CMD SHLIB rmorloc.c -o librmorloc.so`

### Python Extension Compilation
**File**: `library/Morloc/CodeGenerator/SystemConfig.hs:121`

```haskell
callCommand ("make -C " <> optDir <> " -f " <> DF.embededFileName DF.libpylangMakefile)
```

**Command**: `make -C ~/.local/share/morloc/opt -f Makefile`

Makefile uses: `python3 setup.py build`

## Problems

1. **No fallback**: If gcc/g++ not found, build fails immediately
2. **No version check**: Could invoke incompatible compiler version
3. **No capability detection**: Assumes all flags supported
4. **Platform-specific**: Hardcoded Unix paths, no Windows support
5. **No parallel builds**: Sequential compilation only

## Proposed Solution

### Add Compiler Detection Module

**New file**: `library/Morloc/CodeGenerator/CompilerDetect.hs`

```haskell
module Morloc.CodeGenerator.CompilerDetect
  ( detectCompilers
  , Compilers(..)
  ) where

data Compilers = Compilers
  { ccCompiler :: String      -- "gcc" or "clang"
  , cxxCompiler :: String     -- "g++" or "clang++"
  , ccVersion :: String       -- "11.3.0"
  , cxxVersion :: String      -- "11.3.0"
  , ccPath :: FilePath        -- "/usr/bin/gcc"
  , cxxPath :: FilePath       -- "/usr/bin/g++"
  }

detectCompilers :: IO (Either String Compilers)
detectCompilers = do
  -- Try gcc, fallback to clang
  cc <- tryCompiler ["gcc", "clang", "cc"]
  cxx <- tryCompiler ["g++", "clang++", "c++"]

  case (cc, cxx) of
    (Just c, Just cxx) -> do
      cVersion <- getVersion c
      cxxVersion <- getVersion cxx
      return $ Right $ Compilers c cxx cVersion cxxVersion
    _ -> return $ Left "No C/C++ compiler found. Install gcc or clang."

tryCompiler :: [String] -> IO (Maybe String)
tryCompiler [] = return Nothing
tryCompiler (c:cs) = do
  found <- isExecutable c
  if found then return (Just c) else tryCompiler cs

getVersion :: String -> IO String
getVersion compiler =
  readProcess compiler ["--version"] ""
    |>> lines |>> head |>> parseVersion
```

### Update Config to Store Compilers

**File**: `library/Morloc/Config.hs`

```haskell
data Config = Config
  { ...
  , configCCompiler :: String        -- NEW: detected C compiler
  , configCXXCompiler :: String      -- NEW: detected C++ compiler
  , configCompilerFlags :: [String]  -- NEW: optional user flags
  }
```

### Update SystemConfig.hs to Use Config

Nexus.hs no longer invokes compilers (it just writes JSON). Compiler detection would apply to `SystemConfig.hs` (for `morloc init`) and `Cpp.hs` (for C++ pool compilation).

### Update Cpp.hs to Use Config

```haskell
-- OLD
[idoc|g++ -O3 --std=c++17 -o #{outfile} #{src} #{hsep flags'} #{hsep incs}|]

-- NEW
config <- MM.ask
let cxx = MT.pack (MC.configCXXCompiler config)
[idoc|#{cxx} -O3 --std=c++17 -o #{outfile} #{src} #{hsep flags'} #{hsep incs}|]
```

### Run Detection in `morloc init`

**File**: `library/Morloc/CodeGenerator/SystemConfig.hs`

```haskell
configureAllSteps verbose force slurmSupport config = do
  -- NEW: Detect compilers at init time
  compilers <- detectCompilers
  case compilers of
    Left err -> do
      hPutStrLn stderr $ "\ESC[31m" ++ err ++ "\ESC[0m"
      exitFailure
    Right comps -> do
      say $ "Found C compiler: " ++ ccCompiler comps ++ " " ++ ccVersion comps
      say $ "Found C++ compiler: " ++ cxxCompiler comps ++ " " ++ cxxVersion comps

      -- Store in config file
      let configData = unlines
            [ "c-compiler: " ++ ccCompiler comps
            , "cxx-compiler: " ++ cxxCompiler comps
            , "slurm-support: " ++ show slurmSupport
            ]
      writeFile (configBuildConfig config) configData

      -- Continue with rest of initialization
      ...
```

### Load Compilers at Runtime

**File**: `library/Morloc/Config.hs`

```haskell
readConfig :: IO Config
readConfig = do
  home <- getDefaultMorlocHome
  let configFile = home </> ".build-config.yaml"
  exists <- doesFileExist configFile

  if exists
    then do
      contents <- readFile configFile
      let cc = lookup "c-compiler" (parseYAML contents)
      let cxx = lookup "cxx-compiler" (parseYAML contents)
      -- ... parse other config
      return $ Config { configCCompiler = fromMaybe "gcc" cc, ... }
    else do
      -- Default fallback
      return $ Config { configCCompiler = "gcc", configCXXCompiler = "g++", ... }
```

## Testing Strategy

1. **Unit tests**: Mock `tryCompiler` to test detection logic
2. **Integration tests**: Test with gcc-only, clang-only, both
3. **Error tests**: Test with no compilers installed
4. **Version tests**: Test with old/new compiler versions

## Backward Compatibility

- If `.build-config.yaml` exists but missing compiler fields, default to gcc/g++
- `morloc init -f` forces re-detection
- Old config files still work (SLURM support preserved)

## Future Enhancements

- Detect C++ standard support (c++14, c++17, c++20)
- Detect sanitizer support (ASan, UBSan)
- Detect optimization capabilities (LTO, PGO)
- Cross-compilation support (target triples, sysroots)
- Compiler cache (ccache detection)

See [[BUILD-TODO.md]] for prioritization.
