# morloc-manager

Container lifecycle manager for Morloc. Handles version management,
dependency layers, and a build/freeze/serve deployment pipeline for
running morloc programs as containerized services.

```
                        morloc-manager
     +---------+---------+---------+---------+
     |         |         |         |         |
  install    run      build     freeze     serve
     |         |         |         |         |
     v         v         v         v         v
  pull img   exec in   mutable   export   immutable
  + setup    container builder   state    service
```


## Prerequisites

A container engine: [Docker](https://docs.docker.com/engine/install/) (v20+)
or [Podman](https://podman.io/docs/installation) (v3+). No Compose required.

When both are installed, morloc-manager prefers **podman** (rootless by default).
Override with `morloc-manager setup --engine docker`.


## Installation

Download the static binary for your platform:

```sh
# Linux x86_64
curl -Lo morloc-manager https://github.com/morloc-project/morloc/releases/latest/download/morloc-manager-linux-x86_64
chmod +x morloc-manager
mv morloc-manager ~/.local/bin/
```

The binary is fully static (no dependencies, runs on any Linux distribution).

Configure the container engine (required before first use):

```sh
morloc-manager setup                    # interactive (auto-detects engines)
morloc-manager setup --engine podman    # non-interactive
```

Then install a morloc version:

```sh
morloc-manager install          # latest version
morloc-manager install 0.68.0   # specific version
```


## Quick start

```sh
# Set up and install morloc
morloc-manager setup
morloc-manager install
morloc-manager run -- morloc install root-py

# Write a program
cat > foo.loc << 'EOF'
module foo (double)
import root-py
double :: Int -> Int
double x = 2 * x
EOF

# Compile and run
morloc-manager run -- morloc make foo.loc
morloc-manager run -- ./foo 21    # output: 42
```


## Commands

### install

Pull a morloc container image and set up the host-side directory structure.

```sh
morloc-manager install              # latest version
morloc-manager install 0.68.0       # specific version
morloc-manager install --system         # system-wide install (requires sudo)
```

Multiple versions can be installed side-by-side. Each version gets its own
data directory under `~/.local/share/morloc/versions/<ver>/`.

### select

Switch the active morloc version.

```sh
morloc-manager select 0.68.0
morloc-manager select 0.67.0
```

### run

Execute a command inside the active morloc container. This is the primary
way to interact with morloc.

```sh
morloc-manager run -- morloc make foo.loc
morloc-manager run -- ./foo 21
morloc-manager run -- morloc install math
morloc-manager run --shell              # interactive shell
```

The container bind-mounts:

```
  Host                                    Container
  ~/.local/share/morloc/versions/<ver> -> ~/.local/share/morloc
  ~/.local/share/morloc/versions/<ver>/bin -> ~/.local/bin
  $PWD                                 -> $PWD (working directory)
```

On SELinux systems (Fedora, RHEL), the `:z` relabel suffix is applied
automatically. You must work in a subdirectory of your home (not `~`
itself, `/tmp`, or other system directories).

### env

Manage custom dependency environments that layer on top of the base
morloc image.

```
  +-------------------------------+
  | Custom environment layer      |  pip install scikit-learn, etc.
  +-------------------------------+
  | morloc-full:<version>         |  Base image (compiler + runtimes)
  +-------------------------------+
```

```sh
# Create a new environment
morloc-manager env init ml

# Edit the generated Dockerfile
vim ~/.local/share/morloc/deps/ml.Dockerfile

# Build and activate
morloc-manager env build ml

# List environments
morloc-manager env list
```

After activating, all `run` commands use the custom environment
automatically.

### info

Show installed versions and current configuration.

```sh
morloc-manager info
```

Output:

```
Installed versions:
  0.68.0
  0.67.0
Active version: 0.68.0
Active scope:   Local
Active env:     base
Engine:         Podman
```


## Deployment: Build / Freeze / Serve

morloc-manager supports a deployment lifecycle that produces immutable
containers serving morloc programs over HTTP/TCP/Unix sockets.

```
  Build Phase            Freeze Phase          Serve Phase
  (mutable)              (artifact)            (immutable)

  +--------------+      +--------------+      +--------------+
  | morloc       |      | state.tar.gz |      | nexus router |
  | compiler     |----->| manifest.json|----->| morloc binary|
  | + GHC        |      |              |      | frozen state |
  | + runtimes   |      | lib/ fdb/    |      | runtimes     |
  +--------------+      | bin/ pools/  |      +--------------+
                        +--------------+
  morloc-manager         morloc-manager        morloc-manager
  build                  freeze                serve
```

### build

Start a mutable builder container for installing modules and compiling
programs.

```sh
# Interactive build session
morloc-manager build --shell

# Scripted build
morloc-manager build --script build.sh
```

Inside the builder, use standard morloc commands:

```sh
morloc install root
morloc install math
morloc make --install -o myservice service.loc
morloc eval 'import math; math.add 1 2'   # test
```

### freeze

Export the installed morloc state as a portable artifact.

```sh
morloc-manager freeze                       # output to ./morloc-freeze/
morloc-manager freeze --output /tmp/state   # custom output directory
```

Produces:

- `state.tar.gz` -- compressed archive of lib/, fdb/, bin/, and pool files
- `freeze-manifest.json` -- auditable record of installed modules (with
  SHA-256 checksums), compiled programs, and base image reference

What gets frozen:

```
  INCLUDED                    EXCLUDED
  lib/     installed modules  GHC toolchain
  fdb/     program manifests  Stack / Cabal
  bin/     compiled programs  Package caches
  pools    language pool code Build temporaries
  morloc   compiler binary
```

### unfreeze

Build a minimal serve image from the frozen state.

```sh
morloc-manager unfreeze \
  --from ./morloc-freeze/state.tar.gz \
  --tag myservice:v1
```

The resulting image contains:

- The morloc-nexus router (serves programs over HTTP/TCP/sockets)
- Language runtimes (Python, R, C++)
- The frozen morloc state (read-only)
- The morloc compiler binary (for `/eval` endpoint)
- NO GHC, NO Stack, NO build tools

### start

Run a serve container.

```sh
morloc-manager start myservice:v1                   # default: port 8080
morloc-manager start myservice:v1 -p 9090:8080      # custom port
morloc-manager start myservice:v1 -p 8080:8080 -p 9001:9001  # multiple ports
```

The container starts the morloc-nexus router, which exposes:

```
  GET  /programs            List all programs and their commands
  GET  /discover/<program>  Command signatures, types, and schemas
  POST /call/<prog>/<cmd>   Invoke a function
  GET  /health              Pool liveness status
  POST /eval                Compose and run a morloc expression
```

Example:

```sh
# List available programs
curl -s localhost:8080/programs | jq .

# Call a function
curl -s -X POST localhost:8080/call/myservice/hello -d '["world"]'
# {"status":"ok","result":"hello, world!"}

# Compose an expression over installed modules
curl -s -X POST localhost:8080/eval -d '{"expr":"import math; math.add 1 2"}'
# {"status":"ok","result":"3"}
```

The serve container runs with `--read-only` (immutable root filesystem).
`morloc install` cannot modify the frozen state. `morloc eval` can only
compose functions from modules that were installed during the build phase.


## Safety model

The immutable serve container provides defense-in-depth for agentic use:

```
  Layer                  Prevents                    Enforced by
  --------------------   -------------------------   ----------------
  Morloc parser          Inline foreign source code  Compile time
  Module resolution      Uninstalled modules         Compile time
  Read-only filesystem   Installing new modules      Container flag
  Type system            Invalid compositions        Compile time
  Resource limits        Runaway eval                setrlimit()
  Container isolation    Syscall-level escape         Namespaces
```

An agent can:

- Discover all available functions and their types via `/discover`
- Compose any type-valid expression over them via `/eval`
- Call any pre-compiled function via `/call`

An agent cannot:

- Introduce foreign source code (parser rejects it structurally)
- Import modules not present in the frozen state
- Install new modules (filesystem is read-only)
- Bypass the type system
- Escape the container


## Global options

```
--scope SCOPE     local or system (default: local)
-v, --verbose     print container commands to stderr before executing
```

`--system` and `--local` are mutually exclusive. When neither is specified,
the scope is read from the config file (which defaults to local).


## Configuration

morloc-manager stores configuration as JSON files following the XDG Base
Directory Specification.

```
  Local scope:                          System scope:
  ~/.config/morloc/                     /etc/morloc/
    config.json                           config.json
    versions/                             versions/
      0.68.0/                               0.68.0/
        config.json                           config.json
        environments/                         environments/
          ml.json                               ml.json

  ~/.local/share/morloc/                /usr/local/share/morloc/
    versions/                             versions/
      0.68.0/                               0.68.0/
        bin/   lib/   fdb/                    bin/   lib/   fdb/
        include/  opt/  tmp/                  include/  opt/  tmp/
    deps/                                 deps/
      ml.Dockerfile                         ml.Dockerfile
      ml.flags                              ml.flags
    morloc.flags                          morloc.flags
```

Config writes are atomic (write to temp file, rename) with advisory file
locking to prevent concurrent corruption.


## Migration from the shell script

The Haskell morloc-manager replaces the POSIX shell script
(`morloc-manager.sh`). Key differences:

| Aspect | Shell script | Haskell rewrite |
|--------|-------------|----------------|
| Distribution | curl pipe to bash | Static binary (34 MB, no deps) |
| Config format | key=value text files | JSON with schema |
| Config safety | No locking, no atomic writes | flock + atomic rename |
| Error handling | Manual return code checks | Typed errors (ManagerError ADT) |
| Testing | Agent-driven VM exploration | 34 unit tests + property tests |
| Deployment | Not supported | build/freeze/serve lifecycle |
| Eval endpoint | Not supported | POST /eval via nexus router |
| Container flags | Untyped string concatenation | Typed RunConfig/BuildConfig |
| Lines of code | ~2500 (24% business logic) | ~1200 (mostly business logic) |

### What changed

- `morloc-manager.sh` is replaced by the `morloc-manager` binary
- Config files are now JSON (not key=value). Existing configs from the
  shell script will not be read -- a fresh `install` is needed.
- The `update` subcommand is removed. Use your package manager or
  download a new release binary.
- The `clean` and `uninstall` subcommands are not yet ported.
- New subcommands: `build`, `freeze`, `serve-image`, `serve`.

### What stayed the same

- Same container engine support (docker and podman, prefers podman)
- Same directory layout conventions (XDG paths)
- Same bind mount strategy (version data + bin + working directory)
- Same SELinux handling (`:z` suffix, unsafe path validation)
- Same `run --shell` for interactive access
- Same environment layering (Dockerfile stubs, hash-based rebuild)


## Flags files

Extra container flags can be specified in flags files (one flag per line,
`#` for comments):

```
# ~/.local/share/morloc/morloc.flags (applied to all runs)
--gpus all
-v /data/models:/models

# ~/.config/morloc/versions/0.68.0/environments/ml.flags (per-environment)
--network host
```

Flags are aggregated: global flags + environment flags.


## Troubleshooting

### "No container engine found"

Install [Docker](https://docs.docker.com/engine/install/) or
[Podman](https://podman.io/docs/installation).

### "No morloc version selected"

Run `morloc-manager install` to install a version, or
`morloc-manager select <version>` if one is already installed.

### SELinux bind-mount errors (Fedora/RHEL)

Work in a subdirectory of your home directory:

```sh
mkdir -p ~/project && cd ~/project
morloc-manager run -- morloc make foo.loc
```

Do not work from `~`, `/tmp`, or `/var/tmp`.

### Permission denied (root-owned files)

Container processes may create root-owned files. Fix with:

```sh
sudo chown -R $(id -u):$(id -g) ~/.local/share/morloc/
```

### "morloc-nexus: not found"

The morloc runtime libraries were not initialized. Run:

```sh
morloc-manager run -- morloc init -f
```
