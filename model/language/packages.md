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
