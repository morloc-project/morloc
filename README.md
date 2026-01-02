<p align="center">
  <a href="https://github.com/morloc-project/morloc/actions/workflows/.test.yml">
    <img src="https://github.com/morloc-project/morloc/actions/workflows/.test.yml/badge.svg" alt="build status">
  </a>
  <a href="https://github.com/morloc-project/morloc/releases">
    <img src="https://img.shields.io/github/release/morloc-project/morloc.svg?label=current+release" alt="github release">
  </a>
  <a href="https://www.apache.org/licenses/LICENSE-2.0">
      <img src="https://img.shields.io/badge/License-Apache%202.0-blue.svg" alt="license: Apache 2.0">
</a>
</p>

<p align="center">
  <a href="https://morloc-project.github.io/docs">Manual</a> |
  <a href="https://discord.gg/dyhKd9sJfF">Discord</a> |
  <a href="https://peerj.com/articles/cs-3435/">Paper</a> |
  <a href="https://x.com/morlocproject">X</a> |
  <a href="https://bsky.app/profile/morloc-project.bsky.social">BlueSky</a> |
  <a href= "mailto:z@morloc.io">Email</a>
</p>



<div align="center">
<h1>Morloc</h1>
<h2>compose functions across languages under a common type system</h2>
</div>

**Why use Morloc?**

 * Universal function composition: Import functions from multiple languages and
   compose them together under a unified, strongly-typed functional framework.

 * Polyglot without boilerplate: Use the best language for each task with no
   manual bindings or interop code.

 * Type-directed CLI generation: Write concrete function signatures once and
   automatically generate elegant command-line interfaces with argument
   parsing, validation, help text, and documentation.

 * Composable CLI tools: Morloc CLI programs can be composed by simply importing
   them into a new Morloc module and re-exporting their functions.

 * Seamless benchmarking and testing: Swap implementations and run the same
   benchmarks/tests across languages with consistent type signatures and data
   representation.

 * Design universal libraries: Build abstract, type-driven libraries and
   populate them with foreign language implementations, enabling rigorous code
   organization and reuse.

 * Smarter workflows: Replace brittle application/file-based pipelines with
   faster, more maintainable pipelines made from functions acting on structured
   data.


Below is a simple example, for installation details and more examples, see the
[Manual](https://morloc-project.github.io/docs).

A Morloc module can import functions from foreign languages, assign them general
types, and compose new functions:


```morloc
-- Morloc code, in "main.loc"
module m (vsum)

import root-py
import root-cpp

source Py from "foo.py" ("pmap")
pmap a b :: (a -> b) -> [a] -> [b] 

source Cpp from "foo.hpp" ("sum")
sum :: [Real] -> Real

--' Input numeric vectors that will be summed in parallel
--' metavar: VECTORS
type Vectors = [[Real]]

--' Sum a list of numeric vectors
--' return: Final sum of all elements in all vectors
vsum :: Vectors -> Real
vsum = sum . pmap sum 
```

The imported code is natural code with no Morloc-specific dependencies.

Below is the C++ code that defines `sum` as a function of a standard C++ vector
of `double`s that returns a `double`:

```C++
// C++ code, in "foo.hpp"

#pragma once

#include <vector>
#include <numeric>

double sum(std::vector<double> xs) {
    return std::accumulate(
       xs.begin(), xs.end(), 0.0);
}
```

Below is Python code that defines a parallel map function:

```python
# Python code, in "foo.py"

import multiprocessing as mp

# Parallel map function
def pmap(f, xs):
    with mp.Pool() as pool:
        results = pool.map(f, xs)
    return results
```

This program can be compiled and run as below:

```
$ menv morloc make main.loc

$ menv ./nexus vsum -h
Usage: ./nexus vsum VECTORS

Sum a list of numeric vectors

Positional arguments:
  VECTORS  Input numeric vectors that will be summed in parallel
           type: [[Real]]

Return: Real
  Final sum of all elements in all vectors

$ menv ./nexus vsum [[1.2],[0,0.1]]
1.3
```
