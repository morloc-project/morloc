x.xx.x-pre [xxxx.xx.xx]
-------------------

New build system and and compiler options

Major changes
 - [ ] Remove semicolon requirement (1 day)
 - [ ] Rewrite perl manifold nexus in C++ (3 days)
       - [ ] add type annotations to printed help
 - [ ] New build system (7 days)
       - [ ] Place pools and nexus in ~/.morloc/tmp
       - [ ] Create scripts in the working directory that calls them
       - [ ] Add compiler options for executable name, deletion, cache clearing, etc
       - [ ] Allow stdin to replace '-' argument
 - [ ] Add a verbosity flag to `morloc make` (1 day)
 - [ ] New compiler commands for managing modules (1 day)
       - [ ] `morloc uninstall` command for deleting modules
       - [ ] `morloc update` command for updating modules (i.e., git pull)
       - [ ] `morloc list` command for listing all available modules

Minor changes
 - [ ] generate error handling in pools
 - [ ] meaningful error messages
 - [ ] resurrect property tests
 - [ ] generate error handling in pools
 - [ ] address all compiler warnings
 - [ ] add linter
 - [ ] resurrect property tests


0.22.0-pre [2020.04.xx]
-------------------

Implement a schema-directed composable serialization system

  * * * temp * * *

 - [ ] substitute in idocs in translators for better readability
 - [ ] fix the argument-form-3 and manifold-form-7 problem
 - [ ] resurrect interop (and add testing)
 - [ ] add IO tests
 - [ ] add C++ record handling
       Implementing them as tuples would be the easiest option, since I would
       not have to add much no machinery (e.g., serialization already works for
       tuples). It would be better in both performance (probably) and
       readability to use structures. Before going too far, though, I need to
       decide in general how records will work in morloc. Do I want extensible
       records? Does order matter?

Major changes
 - [x] Fully composable serialization over containers and primitives
 - [x] Improved C++ support of generic functions
 - [ ] Record support
       - [x] R
       - [x] Python3
       - [ ] C++
 - [x] Refactor generator - replace old grammar system

Minor changes
 - [x] change default python3 interpreter from "python" to "python3"
 - [x] add default library and tmp paths to config handler
 - [x] test composable serialization functions in all supported languages
 - [x] allow wrapped comments in R

Testing - grammar directed testing
 - [x] test record handling
 - [x] remove and replace out-of-date golden tests
 - [x] systematic argument handling tests
 - [x] systematic manifold form tests
 - [.] systematic interop testing

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
