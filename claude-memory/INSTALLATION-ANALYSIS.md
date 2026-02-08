# Module Installation Analysis

## Installation Flow

### Entry Point: `installModule` (lines 403-422)

1. Parses installation string (e.g., `"github:weena/math@v1.0"` or `"./local-module"`)
2. Routes to appropriate installer:
   - `installLocal` for local directories
   - `installRemote` for git repositories

### Local Installation: `installLocal` (lines 681-722)

**Steps:**
1. **L692**: Convert module path to absolute path via `MS.makeAbsolute`
2. **L695**: Extract module name: `moduleName = MS.takeFileName sourceDir` ⚠️ **ISSUE #3**
3. **L696**: Compute target: `targetDir = libpath </> moduleName`
4. **L699-702**: Verify source directory exists
5. **L705-714**: Handle overwrite protocol (remove existing if ForceOverwrite)
6. **L717-722**: Route based on git repo detection:
   - Git repo → `installLocalGitRepo`
   - Plain directory → `installLocalDirectory`

### Git Repo Installation: `installLocalGitRepo` (lines 725-748)

- If `LatestDefaultBranch`: Copy with `cpDir` (includes uncommitted changes)
- Otherwise: Clone and checkout specific ref
- Removes `.git/` folder after checkout

### Directory Installation: `installLocalDirectory` (lines 763-766)

- Simple recursive copy: `cp -r sourceDir targetDir` ⚠️ **ISSUE #1**

### Remote Installation: `installRemote` (lines 769-799)

- Target dir: `libpath </> gitReponame` (from parsed URL)
- Clones repository and checks out ref

---

## The Three Issues

### Issue #1: Missing space in cp command (L766, L752)

**Location:** `installLocalDirectory` (L766) and `cpDir` (L752)

**Problem:**
```haskell
-- L752
let cmd = "cp -r " <> fromDir <> toDir  -- Missing space!

-- L766
liftIO . callCommand $ "cp -r " ++ sourceDir ++ " " ++ targetDir  -- Has space
```

Actually, L766 is correct (has space), but L752 is missing a space between source and target.

**Expected:** `cp -r /source/path /target/path`
**Generated (L752):** `cp -r /source/path/target/path` ❌

This causes nested directory copies to fail.

---

### Issue #2: Target path creation not ensured (L696)

**Location:** `installLocal` target directory construction

**Problem:**
```haskell
let targetDir = libpath </> moduleName  -- L696
```

No `createDirectoryIfMissing` before copy/clone operations. If:
- `libpath` doesn't exist, OR
- Module name contains nested structure (e.g., "foo/bar")

The installation will fail when `cp` or `git clone` tries to write to non-existent parent directories.

**Fix needed:** Add before L699:
```haskell
liftIO $ createDirectoryIfMissing True libpath
```

Or better, ensure parent of `targetDir` exists before copy operations.

---

### Issue #3: Uses folder name instead of declared module name (L695)

**Location:** Module name extraction in `installLocal`

**Problem:**
```haskell
let moduleName = MS.takeFileName sourceDir  -- L695
```

This extracts the **folder name** from the filesystem path, not the **module name** declared in:
- The `module <name>` declaration in the `.loc` file
- The `package.yaml` metadata

**Example:**
- Folder: `/path/to/foo/`
- Module declaration: `module foobar`
- Current behavior: Installs to `~/.morloc/lib/foo` ❌
- Expected: Install to `~/.morloc/lib/foobar` ✅

**Fix needed:**
1. Find the main `.loc` file in `sourceDir` (look for `main.loc` or `*.loc`)
2. Parse the `module <name>` declaration
3. Use that name for `targetDir` construction
4. Fallback to folder name if no module declaration found

---

## Functions Involved

### Installation Functions
- `installModule` - Main entry, parses and routes
- `installLocal` - Handles local directory installation
- `installLocalGitRepo` - Handles local git repos
- `installLocalDirectory` - Copies plain directories
- `installRemote` - Handles remote git repos
- `cloneRepo` - Executes git clone
- `cpDir` - Copies directories (has bug)
- `checkoutRef` - Checks out git refs

### Parsing Functions
- `moduleInstallParser` - Parses install strings
- `parseModname` - Extracts module name from install string
- `parseLocalModule` - Parses local paths (starts with `.`, `~`, `/`)

### Utility Functions
- `findModule` - Locates modules in search paths
- `findModuleMetadata` - Finds `package.yaml`
- `loadModuleMetadata` - Loads package metadata
- `getModulePaths` - Computes search paths for imports

---

## Related Data Structures

```haskell
data ModuleSource
  = ModuleSourceLocal Text (Maybe GitSnapshotSelector)
  | ModuleSourceRemoteGit GitRemote

data GitSnapshotSelector
  = LatestDefaultBranch
  | LatestOnBranch Text
  | CommitHash Text
  | ReleaseTag Text
```
