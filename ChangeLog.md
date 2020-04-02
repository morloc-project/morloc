x.xx.x [xxxx.xx.xx]
-------------------

Major changes
 - [ ] Remove semicolon requirement
 - [ ] Rewrite perl manifold nexus in C++
 - [ ] Place pools and nexus in ~/.morloc/tmp and create a script in the
       working directory that calls them
 - [ ] Add a verbosity flag to `morloc make`

Minor changes
 - [x] change default python3 interpreter from "python" to "python3"
 - [x] add default library and tmp paths to config handler
 - [ ] test composable serialization functions in all supported languages
 - [ ] add type annotations to printed help
 - [ ] generate error handling in pools
 - [ ] meaningful error messages
 - [ ] resurrect property tests

Testing - grammar directed testing
 - [x] test record handling
 - [ ] test record interop
 - [ ] test list interop
 - [ ] test tuple interop

Fixes
 - [ ] allow wrapped comments in R
 - [ ] fix record handling
 - [ ] fix container interop

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
