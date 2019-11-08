0.18.1 [2019.11.08]

 * fix travis
 * use C++11 for C++ builds
 * make .morloc/config optional
 * fix bug in parsing unit type: `()`

0.18.0 [2019.11.04]

 * Add bidirectional type system
 * Allow parameterized concrete types
 * Allow higher-order functions
 * Allow properties to contain multiple terms 
 * Add many tests
 * Add module system
 * Allow non-primitive types in lists, tuples, and records
 * Removed arq and SPARQL dependency (very fast compilation)

0.17.4 [2019.06.29]

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
