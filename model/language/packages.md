# Packages

Installable packages: their identity, where they are installed, where they
come from, and how dependencies are pinned. Prefix: PKG.

## Identity

### PKG-1 A package is named by its library and its name
Intent: ruled 2026-11-14
Tests: spec-pkg-1-1, spec-pkg-1-2, spec-pkg-1-3, spec-pkg-1-4
Code: conforms 2026-11-18

`package.yaml` gives `name` and `library`. Installing a package requires
both. Standard library packages declare `library: std`. A program that is only
built with `morloc make` needs neither, and its modules belong to no
library. Exception, for packages frozen before the field existed: a
package with no `library` fetched from the origin of a library that ships
with every environment (PKG-5), or installed from a local clone of one of
its repositories, belongs to that library.

### PKG-2 Library and module names are lower-case words
Intent: ruled 2026-11-14
Tests: spec-pkg-2-1, spec-pkg-2-2, spec-pkg-2-3
Code: conforms 2026-11-18

A library or installed module name matches `[a-z][a-z0-9-]*`. `std` is
reserved for the standard library's packages. Names of program-local modules are not
restricted by this item.

### PKG-3 A package is installed at `<plane>/<library>/<name>`
Intent: ruled 2026-11-15
Tests: spec-pkg-3-1, spec-pkg-3-2, spec-pkg-3-3
Code: conforms 2026-11-18

The install path is taken from the installed package's own `library` and
`name`, never from the install string, the repository name or a directory
name. `morloc install` refuses a package whose name differs from the
repository it was fetched from. Its entry file is `main.loc`, else
`<name>.loc`. Every per-module store (module record,
lock, pins, exposed resources) is keyed by library and name.

### PKG-4 Installing a commit gives one result, whatever the route
Intent: ruled 2026-11-14
Code: deviates (../compiler/issues/install.md)

For any commit H of repository R, `morloc install <location>@H` and
cloning R, checking out H and running `morloc install ./R` give the same
installed tree, exposed files and module record, except the fields that
record the source, time and reason of the install.

## Origins

### PKG-5 Every library has one origin
Intent: ruled 2026-11-15
Tests: spec-pkg-5-1, spec-pkg-5-2, spec-pkg-5-3, spec-pkg-5-4, spec-pkg-5-5, spec-pkg-5-6
Code: conforms 2026-11-18

An origin is `host:owner`; module `name` of a library with origin
`host:owner` lives at `host:owner/name`. `std` (`github:morloclib`) and
`bio` (`github:morloc-bio`) ship with every environment and cannot be
changed. Other libraries are recorded in the environment's origins file.
First come, first served: `morloc install host:owner/name` of a package
whose library is unrecorded records it when the library equals the owner
(compared without case); any other pairing is recorded only by `morloc
library add`. A package fetched from anywhere but its library's origin is
refused. A local install (`morloc install ./x`) is always accepted and
records nothing.

### PKG-6 A dependency may name its exact source
Intent: ruled 2026-11-15
Tests: spec-pkg-6-1, spec-pkg-6-2, spec-pkg-6-3
Code: conforms 2026-11-18

A `morloc-deps` entry has `name`, `library` (default `std`), a quoted
`git-hash`, and an optional `source: host:owner/name`. `library` is
required when `source` is given. The fetched package's library and name
must equal the entry's. A `source` never records an origin; one that
disagrees with a recorded origin is an error. A dependency with no
`source` whose library has no origin is a load error that names the
command to record one.

### PKG-7 A snapshot line names the library and, if needed, the source
Intent: ruled 2026-11-15
Tests: spec-pkg-7-1, spec-pkg-7-2, spec-pkg-7-3
Code: deviates (issues/packages.md)

A snapshot line is `name hash` for a standard library module and `library/name hash
[host:owner/name]` otherwise; the source is required for a library that
does not ship with the environment. A snapshot pin outranks a package's
pin, and an install of a snapshot-pinned module fetches from the
snapshot's source. A freeze refuses a locally installed module.

### PKG-8 Imports of a package's dependencies are found in all its files
Intent: ruled 2026-11-15
Tests: spec-pkg-8-1
Code: conforms 2026-11-18

The modules a package needs are the installed modules imported by its
entry file and by every local module the entry file reaches, directly or
through other local modules; files nothing imports (tests, examples) add
none. Installing a package locally warns
about each imported module whose library does not ship with the
environment and that has no `source` entry, since others could not fetch
it.

### PKG-9 Installed programs of two libraries may not share a name
Intent: ruled 2026-11-14
Code: deviates (issues/packages.md)

Installing a program whose name an installed program of another library
already holds is refused, naming both.

### PKG-10 The package server is a transport, not an identity
Intent: ruled 2026-11-15
Code: deviates (issues/packages.md)

A package server answers a request for (library, name, git hash) with the
tree of that commit, so PKG-4 covers it. The identity is still read from
the fetched `package.yaml` and checked against the library's origin.

### PKG-11 An install commits a whole module or nothing
Intent: proposed
Tests: spec-pkg-11-1, spec-pkg-11-2
Code: conforms 2026-11-18

A module is installed once its record is written, after its dependencies,
exposed files, setup script and typecheck have succeeded. A module
directory without a record is not installed: imports do not see it, and
the next install replaces it. An install that fails leaves the module as it
was before (a replaced module is put back) and records no origin;
dependencies it installed stay installed. Installs and origin changes in one
environment run one at a time.

## Native dependencies

A package lives in the environment's prefix P (`CONDA_PREFIX` of a managed
build, one where `MORLOC_ENV` is set): headers in P/include, libraries in
P/lib, programs in P/bin. A build outside a managed environment has no P.

### PKG-12 A cpp-deps entry declares a package, its headers and its link names
Intent: ruled 2026-11-19
Tests: spec-pkg-12-1, spec-pkg-12-2, spec-pkg-12-3, spec-pkg-12-4, spec-pkg-12-5, spec-pkg-12-6
Code: conforms 2026-11-19

A `cpp-deps` entry names a package the environment provides, with keys
`version`, `source`, `channel`, `namespace` (GEN-7), `import` (its headers)
and `libs` (its link names, each one `-l<name>` on a C++ pool's link line);
any other key is refused. A link name is a bare name: no leading `-`, no
`lib` prefix, no library suffix, no whitespace. A header is ASCII without
`<`, `>`, `"` or whitespace. `libs` reaches C++ pools only; Rust names its
links in its own source. The `dependencies` key is refused, empty or not,
naming its replacements: a package's `libs`, `tool-deps` for a program, and
`cxx-flags` for a library the environment does not provide.

Why: one field meant both a link name and a package to install, so a link
name that is not a package name (`hts`, `boost_filesystem`) failed the
solve.

### PKG-13 A native pool build sees the environment, whatever the compiler
Intent: ruled 2026-11-19
Tests: spec-pkg-13-1, spec-pkg-13-2
Code: conforms 2026-11-19

In a managed build every native compile (a C++ pool, a Rust pool, a guest's
C) searches P/include for headers and P/lib for libraries, after morloc's
own directories, and the pool records P/lib as a run path (DT_RPATH on
Linux, so a caller's `LD_LIBRARY_PATH` does not replace the environment's
libraries). A managed build whose `CONDA_PREFIX` is unset, empty or not a
directory, or whose P/lib holds a libmorloc, is refused. A pool built this
way runs only where P exists at the same path, as in serve and frozen
images.

### PKG-14 A declared header that does not resolve is refused by name
Intent: ruled 2026-11-19
Tests: spec-pkg-14-1
Code: conforms 2026-11-19

A C++ pool build checks every `import` header of every cpp-deps entry of
the program, managed or not, and a header that does not resolve is refused
naming the dependency and the header. Only headers are checked: a missing
library is the linker's `cannot find -l<name>`, versions are not checked,
and a header found outside P passes.

### PKG-15 Each cxx-flags entry is one argument
Intent: ruled 2026-11-19
Tests: spec-pkg-15-1
Code: conforms 2026-11-19

Each entry of `cxx-flags` and of `-X cpp:flags` reaches the compiler as
exactly one argument, never split or expanded by a shell.

### PKG-16 A tool's declared executables must resolve at build
Intent: ruled 2026-11-19
Tests: spec-pkg-16-1, spec-pkg-16-2
Code: conforms 2026-11-19

A `tool-deps` entry may name its executables under `bin`, as plain names
without `/` or whitespace; any key other than `version`, `source`,
`channel` and `bin` is refused. Each executable must resolve at build, in
P/bin when managed and on `PATH` otherwise, or the build is refused naming
the dependency and the executable. An entry without `bin` is not checked: a
package name is not an executable name (`star` installs `STAR`).

### PKG-17 A pool finds the environment's programs first
Intent: ruled 2026-11-19
Tests: spec-pkg-17-1
Code: conforms 2026-11-19

A program built in a managed environment records P, and the nexus starts
every pool with P/bin first on `PATH`, so declared tools resolve to the
environment's whatever `PATH` the program was started from. A program built
outside one records nothing, and its pools inherit `PATH` unchanged.
