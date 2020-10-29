1.0.0 [202x.xx.xx]

The first stable. It will be a product that I expect other people to use for
important projects, therefore backwards compatibility will be important. The
whole system needs extensive testing in real applications. Much of this will be
done in the development of the core libraries. We will also need to add
handling for several very different languages (proofs-of-concept).

 - [ ] typeclasses
 - [ ] semantic types - test in bioinformatics applications
 - [ ] constraints - refined types?
 - [ ] manifold hooks - caching, documentation, logging, effects
 - [ ] logic engine (z3?) - from typechecking to architecture design and debugging
 - [ ] ecosystem (test suite, linter, package tools, vim plugin)
 - [ ] language support (Python3, R, C++, Java, Haskell, Scheme, Prolog)
 - [ ] well tested core libraries


0.33.0 [2020.11.08]

First hackage release

 - [ ] Print general types after subcommands
 - [ ] Allow spaces and no unecessary escape in input JSON data
 - [ ] Remove semicolon requirement
 - [ ] Haddock documentation
 - [ ] Update README

0.32.0 [2020.10.xx]

 - [ ] Add record/table field access
 - [ ] Add record/table arithmetic sugar

0.31.0 [2020.10.29]

 * Fix anonymous records in C++
 * Distinguish 'record', 'object', and 'table'
 * Add object handling
 * Add table handling

0.30.0 [2020.10.23]
-------------------

 * Add `object` keyword for defining record types
 * Add full record serialization handling (C++, py, R)

0.29.0 [2020.10.21]
-------------------

 * Add AST directed (de)serialization framework
 * Add type constructors for parameterized types

0.28.0 [2020.10.12]
-------------------

 * Allow import/export of type aliases
 * Refactor with DAGs all through the parser and typechecker

0.27.0 [2020.10.04]
-------------------

 * Add systematic tests for data serialization
 * Fix bug in C++ serialization
 * Move to serialize to dedicated libraries that require no import

0.26.0 [2020.09.27]
-------------------

Add `type` keyword for defining type aliases

0.25.0 [2020.09.26]
-------------------

No explicit forall. Instead use Haskell convention of generics being lowercase
and non-generics being uppercase. 

 * no more explicit "forall"
 * generics are lowercase in type signatures
 * non-generic types are uppercase
 * normal functions are lowercase
 * class constructors are uppercase (though handling for this is not yet implemented)

0.24.0 [2020.09.22]
-------------------

Allow integration of many instances

0.23.0 [2020.05.14]

Bug fixes and code cleanup

Bug fixes / tests
 - [x] [x] github issue #7 - new Var=> typechecking rule
 - [x] [x] github issue #9 - rewire container type inference
 - [x] [x] github issue #10
 - [x] [x] github issue #11


0.22.0 [2020.04.28]
-------------------

Implement a schema-directed composable serialization system

Major changes
 * Fully composable serialization over containers and primitives
 * Improved C++ support of generic functions
 * Record support for R and Python3 (not C++ yet)
 * Refactor generator - replace old grammar system
 * Allow arguments to be passed to general functions
   (e.g., `foo x = [x]`, where no specific language is needed) 

Minor changes
 * change default python3 interpreter from "python" to "python3"
 * add default library and tmp paths to config handler
 * test composable serialization functions in all supported languages
 * allow wrapped comments in R

Testing - grammar directed testing
 * test record handling
 * remove and replace out-of-date golden tests
 * systematic argument handling tests
 * systematic manifold form tests
 * systematic interop testing

0.21.0 [2020.03.31]
-------------------

Major - add handling and test for many many corner cases
 * Allow export of data statements
 * Allow export of non-functions
 * Allow functions with containers at the root
 * Allow export of 0-argument functions 

Minor
 * proof-of-concept composable serialization functions in C++ (cppbase)
 * add python tests
 * make the test output look pretty (no weird whitespace)

0.20.0 [2020.03.23]
-------------------

 * Add composable default types

0.19.1 [2020.02.22]
-------------------

 * bug fixes

0.19.0 [2020.02.20]
-------------------

Major changes
 * Allow currying
 * Add realization optimizations
 * Refactor generator into series of clear transformations
 * Added handling for dealing with ambiguous ASTs

Minor bug fixes and updates
 * Prettier code generation for C++, Python and R
 * More detailed comments in generated code
 * Allow tags on parenthesized types
 * Fix bug in functions with multiple parameters 
 * Fix bug preventing loading of package metadata 

0.18.1 [2019.11.08]
-------------------

 * Fix travis
 * Use C++11 for C++ builds
 * Make .morloc/config optional
 * Fix bug in parsing unit type: `()`

0.18.0 [2019.11.04]
-------------------

 * Add bidirectional type system
 * Allow parameterized concrete types
 * Allow higher-order functions
 * Allow properties to contain multiple terms 
 * Add many tests
 * Add module system
 * Allow non-primitive types in lists, tuples, and records
 * Removed arq and SPARQL dependency (very fast compilation)

0.17.4 [2019.06.29]
-------------------

 * Add C and C++ handling
 * Define Ord intance for MTypeMeta
 * Allow pools to be called as executables
 * Add type handling to generators
 * Remove redundant SPARQL queries (better performance)
 * New RDF list semantics
 * Use strings to represent concrete types (e.g. "char\*")
 * Write pretty-printed diagnostic files to `$MORLOC_HOME/tmp` 
 * Handling for multiple concrete type signatures (e.g., definition of
   a function in multiple languages).
 * Handling for multiple abstract type signatures
 * Handling for multiple function declarations

0.17.3 [2019.06.14]
-------------------

 * Partial C support
   - execution of sourced functions
   - no composition
   - no foreign calls

 * Partial transition to typed generators
   - bound arguments are still not typed correctly

 * Use integer IDs to identify manifolds in pools and the nexus (can to make
   calls between them) instead of long, mangled names.

 * Replace string names of languages (e.g., "python") with a sum type.

0.17.2 [2019.05.05]
-------------------

  Pycon release

0.17.1 [2019.04.26]
-------------------

 * Fix output serialization in generate code
 * Fix module linking in generated code

0.17.0 [2019.04.16]
-------------------

 * Add morloc home
 * Load modules from `$MORLOCHOME/lib`
 * Create monad stack

0.16.2 [2018.03.05]
-------------------

 * Add Zenodo badge making the project citable
 * Move to `morloc-project/morloc` github repo

0.16.1 [2018.09.24]
-------------------

Minor release consisting of internal refactoring

 * Pruned unnecessary code
 * Pruned unnecessary imports
 * Compliance with stricter compile flags

0.16.0 [2018.09.14]
-------------------

 * Write RDF bools in lowercase ("true", rather than "True"), as per specs
 * Stricter node typing (replace ad hoc names with elements from an ADT)
 * Add very rudimentary typechecking
 * Remove SPARQL server dependency (now there's a sluggish Jena dependency)

0.15.1 [2018.09.10]
-------------------

 * Add error handling and reporting to pools
 * Add type signature comments to generated pools 
 * Richer internal data structures

0.15.0 [2018.09.05]
-------------------

 * Generalize code generators using grammar records
 * Add Python compatibility
 * Replace unit tests with golden tests
 * Use docopt and USAGE template for argument handling
 * Report number of arguments in nexus usage statements
