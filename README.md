<p align="center">
  <a href="https://github.com/morloc-project/morloc/actions/workflows/.test.yml">
    <img src="https://github.com/morloc-project/morloc/actions/workflows/.test.yml/badge.svg" alt="build status">
  </a>
  <a href="https://github.com/morloc-project/morloc/releases">
    <img src="https://img.shields.io/github/release/morloc-project/morloc.svg?label=current+release" alt="github release">
  </a>
  <a href="https://www.gnu.org/licenses/gpl-3.0">
    <img src="https://img.shields.io/badge/License-GPL%20v3-blue.svg" alt="license: GPL v3">
  </a>
</p>

<p align="center">
  <a href="https://morloc-project.github.io/docs">Manual</a> |
  <a href="https://discord.gg/dyhKd9sJfF">Discord</a> |
  <a href="https://www.zebulun-arendsee.com/images/morloc-paper-001.pdf">Paper Draft</a> |
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

 * Seamless benchmarking and testing: Swap implementations and run the same
   benchmarks/tests across languages with consistent type signatures and data
   representation.

 * Design universal libraries: Build abstract, type-driven libraries and
   populate them with foreign language implementations, enabling rigorous code
   organization and reuse.

 * Smarter workflows: Replace brittle application/file-based pipelines with more
   fast, more maintainable pipelines made from functions acting on structured
   data.


Below is a simple example, for installation details and more examples, see the
[Manual](https://morloc-project.github.io/docs).

A Morloc module can import functions from foreign languages, assign them general
types, and compose new functions:


```morloc
-- Morloc code, in "main.loc"
module m (sumOfSums)

import types

source Py from "foo.py" ("pmap")
pmap a b :: (a -> b) -> [a] -> [b] 

source Cpp from "foo.hpp" ("sum")
sum :: [Int] -> Int

--' Sum a list of lists of numbers
sumOfSums = sum . pmap sum 
```

The imported code is is natural code with no Morloc-specific dependencies.

Below is the C++ code that defines `sum` as a function of a standard C++ vector
of `int`s that returns an `int`:

```C++
// C++ code, in "foo.hpp"

#pragma once

#include <vector>
#include <numeric>

int sum(std::vector<int> xs) {
    return std::accumulate(
       xs.begin(), xs.end(), 0);
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

$ menv ./nexus -h
Usage: ./nexus [OPTION]... COMMAND [ARG]...

Nexus Options:
 -h, --help            Print this help message
 -o, --output-file     Print to this file instead of STDOUT
 -f, --output-format   Output format [json|mpk|voidstar]

Exported Commands:
  sumOfSums   Sum a list of lists of numbers
                param 1: [[Int]]
                return: Int

$ menv ./nexus sumOfSums [[1,2,3],[4]]
10
```
