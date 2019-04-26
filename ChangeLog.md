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
