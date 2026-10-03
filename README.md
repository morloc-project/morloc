<p align="center">
  <a href="https://github.com/morloc-project/morloc/releases"><img src="https://img.shields.io/github/release/morloc-project/morloc.svg?label=current+release" alt="github release"></a>&nbsp;&nbsp;
  <a href="https://www.apache.org/licenses/LICENSE-2.0"><img src="https://img.shields.io/badge/License-Apache%202.0-blue.svg" alt="license: Apache 2.0"></a>&nbsp;&nbsp;
  <a href="https://morloc-project.github.io/docs"><img src="https://img.shields.io/badge/Manual-000000?style=flat&logo=readthedocs&logoColor=white" alt="Manual"></a>&nbsp;&nbsp;
  <a href="https://discord.gg/dyhKd9sJfF"><img src="https://img.shields.io/badge/Discord-5865F2?style=flat&logo=discord&logoColor=white" alt="Discord"></a>&nbsp;&nbsp;
  <a href="https://peerj.com/articles/cs-3435/"><img src="https://img.shields.io/badge/Paper-02A98D?style=flat&logo=googlescholar&logoColor=white" alt="Paper"></a>&nbsp;&nbsp;
  <a href="https://x.com/morlocproject"><img src="https://img.shields.io/badge/X-000000?style=flat&logo=x&logoColor=white" alt="X"></a>&nbsp;&nbsp;
  <a href="https://bsky.app/profile/morloc-project.bsky.social"><img src="https://img.shields.io/badge/Bluesky-0285FF?style=flat&logo=bluesky&logoColor=white" alt="BlueSky"></a>&nbsp;&nbsp;
  <a href="mailto:z@morloc.io"><img src="https://img.shields.io/badge/Email-D14836?style=flat&logo=gmail&logoColor=white" alt="Email"></a>
</p>

<p align="center">
  <img src="./assets/comp-fig.svg" alt="Composition" width="100%" />
</p>

<div align="center">
<h1>Morloc</h1>
<h2>Make the function, not the application, the unit of composition</h2>
</div>

Morloc composes functions across languages under a common type system
and transforms modules into rich CLIs, APIs, and MCPs.

Write ordinary code in ordinary languages, source it into a Morloc script, and
provide a type. These functions are then first class citizens. They can be used
freely within a rich functional language. The Morloc compiler generates the CLI,
API, MCP model, and usage statements from the types and declarative docstring
instructions. 

## Install

Morloc runs natively on Linux, macOS (new ARM chips), and Windows WSL.

Run this to install the Morloc Installation Manager (`mim`):

```sh
curl -fsSL https://raw.githubusercontent.com/morloc-project/morloc-manager/main/scripts/install.sh | sh
mim new     # bulid the environment
mim shell   # drop into a shell
mim demo    # pull cool morloc demos
```

The build is native by default, add the `--engine docker` flag to build in a
container (or `podman` or `apptainer`). The environment automatically handles
dependencies for Morloc builds via the Pixi package manager. See the manual
section [Getting Started](https://morloc-project.github.io/docs/#_getting_started) for more info.

You may freeze Morloc environments into Docker images with `mim freeze`.

## An example

Two functions, in two languages, neither aware of the other. A C++ sum:

```cpp
// foo.hpp
#pragma once
#include <vector>

double sum(const std::vector<double>& vec) {
    double sum = 0.0;
    for (double value : vec) {
        sum += value;
    }
    return sum;
}
```

and a parallel map in Python:

```python
# foo.py
import multiprocessing as mp

def pmap(f, xs):
    with mp.Pool() as pool:
        results = pool.map(f, xs)
    return results
```

Neither file imports anything from Morloc. The Morloc module gives each a type
and composes them:

```morloc
-- sums.loc
module m (sum, sumOfSums)

import root-py
import root-cpp

source Py from "foo.py" ("pmap")
source Cpp from "foo.hpp" ("sum")

pmap :: (a -> b) -> [a] -> [b]

--' Add up a list of numbers
sum :: [Real] -> Real

--' Add up a list of lists, summing each in parallel
sumOfSums :: [[Real]] -> Real
sumOfSums = sum . pmap sum
```

`.` is function composition, so `sumOfSums` reads right to left: `pmap sum` sums
each inner list in parallel, and the outer `sum` adds the results.

```console
$ morloc make sums.loc
$ ./sums sumOfSums '[[1,2],[3,4,5]]'
15
```

A Python function called a C++ function across a process boundary, and you
wrote no binding, no serializer, and no argument parser.

### Free stuff that the compiler generates based on the type

Building the program creats a CLI tool from the given types and docstrings:

```console
$ ./sums -h
Usage: ./sums <nexus_options> <command> <command_options>

Commands:
  sum        Add up a list of numbers
  sumOfSums  Add up a list of lists, summing each in parallel

General Options:
  -h, --help  Print help; -hh adds details and examples, -hhh adds schemas
              (nexus options: -h @)
```

`./sums sum -h` goes further and prints the argument types the compiler derived.

It can also serve as an MCP server (`./sums --mcp-tools` emits JSON Schema tool
definitions), an HTTP, TCP, and Unix-socket service, and a module that another
Morloc module can import and re-export. None of these are separate builds or a
separate descriptions, so none of them can drift from the functions: changing
a return type changes all interfaces.

## Where the project stands

Morloc has been in development for about ten years and is in beta. I use it for
my own work and am currently trying to build a community.

The test suite is massive and growing, but many bugs remain. The core feature is
slowly stabilizing, but I still make regular backwards incompatible changes. Play
with the cutting edge and you might bleed. Track your blood over here:
[issue tracker](https://github.com/morloc-project/morloc/issues).

## Getting involved

The hard part is finished. The ongoing work is the long tail of bug fixes and
ecosystem development. That is where I need people. Here's what I'm looking for:

- **Report what breaks.** Try it out, tell me what breaks
- **Write a module.** Take something you already maintain, give it types, and
  share it. I would be happy to collaborate.
- **Fix the editor tooling.** Add support for your favorite editor.
- **Bring a language.** Every new language brings fun design questions, I would
  be happy to work with you in bringing your language into the Morloc ecosystem.
- **Tell me the right way to build a type system.** There is a lot of theory to
  hash through and I've half-assed more than half of it. If you're a theorist,
  I'd love to hear from you. There's a paper or two buried somewhere in all of
  this.

If you are interested in playing with Morloc (or want to drown my in money),
feel free to contact me at [z@morloc.io](z@morloc.io).

## License

Apache 2.0. See [LICENSE](LICENSE).
