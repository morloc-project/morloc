# Packages

A package is a distributable collection of morloc modules. Packages provide metadata for dependency management, versioning, and installation.

## Package Metadata

Each package contains a `package.yaml` file in its root directory:

```yaml
name: math
version: 0.1.0
homepage: https://github.com/morloc-project/math
synopsis: Mathematical functions for morloc
description: Provides trigonometric, exponential, and other math functions
bug-reports: https://github.com/morloc-project/math/issues
license: MIT
author: Morloc Project
dependencies:
  - root
```

## Fields

| Field | Required | Description |
|-------|----------|-------------|
| `name` | Yes | Package name (must match directory name) |
| `version` | Yes | Semantic version string |
| `homepage` | No | URL to the project page |
| `synopsis` | No | One-line description |
| `description` | No | Longer description |
| `license` | No | License identifier |
| `author` | No | Package author |
| `dependencies` | No | List of required packages |

## Installation

Packages are installed with `morloc install`:

```bash
morloc install math                          -- from default repository
morloc install github:user/repo              -- from GitHub
morloc install /path/to/local/package        -- from local path
```

Installation copies the package contents to the module library:

```
~/.local/share/morloc/src/morloc/plane/<plane>/<package-name>/
```

## Dependencies

When a package declares dependencies, `morloc install` ensures those dependencies are also installed. Dependencies are resolved transitively: if `A` depends on `B` and `B` depends on `C`, installing `A` also installs `B` and `C`.

## Standard Library Packages

The standard library is distributed as a set of packages:

| Package | Purpose |
|---------|---------|
| `internal` | Compiler internal definitions |
| `root` | Language-agnostic core signatures |
| `root-py` | Python implementations of core functions |
| `root-cpp` | C++ implementations of core functions |
| `root-r` | R implementations of core functions |
| `math` | Mathematical functions |

These are installed during `morloc init`:

```bash
morloc init -f
morloc install internal root root-py root-cpp root-r math
```

## Versioning

Packages use semantic versioning (MAJOR.MINOR.PATCH). The current module system does not enforce version constraints at resolution time; dependency version fields are informational.
