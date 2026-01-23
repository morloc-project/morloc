0.59.0 [2026-01-23]
-------------------
 * allow functions in data structures
 * allow source functions to return functions
 * allow application of expressions that evaluate to functions
 * fix bug in C++ bool deserialization

0.58.3 [2026-01-03]
-------------------
 * fix record docstring inheritance

0.58.2 [2025-12-29]
-------------------
 * fix minor bug optional versus positional

0.58.1 [2025-12-28]
-------------------
 * fix minor bug in record unrolling

0.58.0 [2025-12-28]
-------------------
 * add hex, octal, and binary numeric representations
 * new record syntax
 * generate CLI from docstrings
     * resolve records into groups of arguments
     * allow literal strings without the extra JSON quoting
 * change to Apache-2.0

0.57.1 [2025-11-12]
-------------------
 * minor bug fixes

0.57.0 [2025-11-11]
-------------------
 * re-allow underscores in variable names
 * add placeholder syntax / lambda lifting from holes
 * multi-line strings
 * string interpolation
 * getter patterns
 * setter patterns
 * write full Morloc nexus evaluator
 * Fix bug in local installs with "."

0.56.0 [2025-10-08]
-------------------

New file organization
 * Change file layout to conform to XDG spec; 
   replaced `~/.morloc` with `~/.local/share/morloc`
 * Move standard library to `$MORLOC_HOME/src/morloc/plane/default/morloclib`
   * src/ - stores any source code morloc needs, not just modules 
   * morloc/ - stores morloc modules
   * plane/ - stores morloc modules that are part of a plane
   * default/ - the current default plane
   * morloclib/ - the org name for the core modules in the default plane

New `morloc install` subcommand functionality
 * Multiple imports may be chained on one command
 * Source and ref can be included per install, for example:
   * `root` - install a core module
   * `root@hash:abcdef1234` - specific core instance
   * `root@tag:v1.0.0` - specific tag/version
   * `codeberg:weena/calendar@hash:abcdef1234` - 3rd party with source and ref
 * Support for install of local modules
   

0.55.1 [2025-09-29]
-------------------

 * Bug fixes

0.55.0 [2025-09-27]
-------------------

 * Allow general types to be declared and imported/exported
 * Allow undirected dependencies
 * Allow dashes in module names
 * Fix many bugs in native Morloc code handling
 * Fix bug in certain higher order foreign functions
 * Simplify internal unique naming conventions
 * Move all tests to use the new root module (rather than base)
 * Slightly improved error messages

Breaking changes:
 * Move to reverse tree model of dependency handling
 * Allow exactly one type signature for each term and class
 * Require explicit typeclass import/export
 * Fix handling of Python builtin imports
    Now builtins must not be imported from Python source. Instead
    import directly from Python, for example: `source Py ("len")`
 * More efficient Haskell Map usage

0.54.2 [2025-08-09]
-------------------

 * Enforce lowercase module name rule
 * Fix handling of executable file name option
 * Fix record handling

0.54.1 [2025-07-26]
-------------------

 * Fix pickle bug in Python multiprocessing
 * Replace asprintf non-standard C function 
 * Partially fix interop for C structs
 * Fix bug in unqualified imports

0.54.0 [2025-07-13]
-------------------

 * Add full MessagePack and VoidStar IO to nexus
 * Fixed type pretty printing in usage and typechecking output
 * Add nexus option and usage info
 * Add support for one-line command docstrings

0.53.7 [2025-05-31]
-------------------

 * Fix bug in parameterized type definition concrete type inference

0.53.6 [2025-05-31]
-------------------

 * Improve container instructions in README
 * Fix all README examples
 * Make Dockerfiles more portable
 * Fix bug in implicit typeclass instance inheritance

0.53.5 [2025-05-12]
-------------------

 * Fixed unnecessary copying in C libs
 * Fixed double let-binding in code gen

0.53.4 [2025-05-08]
-------------------

 * Replace C daemon forking with thread pooling (4X speedup)
 * Fix bugs related to 0-length array memory allocation

0.53.3 [2025-05-06]
-------------------

 * Fix handling of empty vectors

0.53.2 [2025-05-06]
-------------------

 * Fix bug in JSON parsing

0.53.1 [2025-05-06]
-------------------

 * Update containers
 * Update github actions
 * Fix bug in type scoping (sort of)

0.53.0 [2025-05-04]
-------------------

Unify backend under shared C library

 * Replace python nexus with C nexus
 * Move packets to little-endian format
 * Move all packet handling, binary protocol specs, socket handling, JSON
   parsing from pool code into the shared morloc.h library
 * Replace R `future` parallelism scheme with builtin `parallel` library

Add remote worker and resource management support

 * Add conventions for specifying caching and evaluation strategy
 * Add xxhash hashing for caching
 * Add remote handling with experimental slurm support

Other

 * `morloc` commands now return proper exit codes

0.52.1 [2025-02-16]
-------------------
 * Add python native bytes and bytearray support

0.52.0 [2025-02-09]
-------------------

Type evaluation and specialization
 * Delayed general alias evaluation
 * Add strict numeric types (`int8`, `uint8`, `int16` etc)
 * Allow concrete alias specialization
 * Add type hints allowing concrete type conservation across foreign calls

Bug fixes
 * Fix bug in opening existing shared memory volumes

Language support
  * Add C++ support for std template list-like types (`list`, `forward_list`,
     `deque`, `stack`, and `set` (I know, they're a tad different)
  * Add Python support for `numpy` vectors and arrays
  * Allow raw R vectors to be interpreted as strings
  * Nexus no longer creates python objects from returned data, instead writes
    results directly through C library (`morloc.h`)

0.51.1 [2024-12-04]
-------------------

 * Do not catch STDOUT and STDERR
 * Fix NULL return errors
 * Fix container setup

0.51.0 [2024-12-02]
------------------

Shared memory

 * Allow processes to communicate through shared memory

Setup

 * `morloc init` command will now build the `morloc` ecosystem, writing all
   required headers to `morloc` home and compiling any required shared libraries.

Other

 * Fix cases where morloc stalls when errors are transmitted across languages
 * Moved demos to the dedicated example repo


0.50.0 [2024-11-08]
-------------------

Better installation

 * `morloc install --commit 45d8df12` - for github retrieval by commit hash
 * `morloc install --branch dev` - to retrieve latest from specific branch
 * `morloc install --tag 0.1.0` - to retrieve specific tag

Better containers
 * Use podman rather than docker in Makefile
 * `morloc-tiny:<version>` - everything needed to compile morloc projects
 * `morloc-full:<version>` - an environment for running projects


0.49.0 [2024-11-04]
-------------------

New backend with better performance

 * Mediate inter-process communication with UNIX domain sockets
 * Transmit data with MessagePack rather than JSON
 * Added a benchmarking suite
 * Added `morloc init` command to configure morloc ecosystem

0.48.0 [2024-05-10]
-------------------

Second PeerJ submission (after return by editor for technical reasons)

 * Made type parameters explicit as type arguments:
   `id a :: a -> a`
    rather than either of
   `id :: a -> a`
   `id :: forall a . a -> a`.
 * Pass-by-reference in generated C++ code
 * Simplify generated C++ code by using function template arguments rather
   than type casting.


0.47.2 [2024-04-28]
-------------------

 * made the build static
 * fixed the dockerfile
 * exported the vim-syntax file out to its own repo
 * added a --version option
 * upgraded to LTS 22.18

0.47.2 [2024-04-28]
-------------------

First PeerJ submission

0.47.0 [2024.03.10]
-------------------
 * Add value checker
 * Raise error when implementations have equal score (no arbitrary choice)

0.46.0 [2024.03.06]
-------------------

 * Fix precedence of '@' operator - it binds tightest
 * Update flu demo
 * Fix github actions
 * Fix Dockerfile
 * Address all compiler warnings

0.45.0 [2024.02.14]
-------------------

 * Allow explicit import of polymorphic terms
 * Fix infinite loop bug when two many arguments are applied to a function
 * Synchronise tests with new core libraries type classes

0.44.0 [2024.02.08]
-------------------

Add support for ad hoc polymorphism.
  * Support sources and declarations in classes 
  * Support multiple parameters
  * Support overlapping instances
  * Packers are now implemented through the `Packable` typeclass

Some missing features:
  * No support typeclass constraints in the type signatures.
  * No support for parameterized class variables
  * No support for polymorphic recursion (does anyone want that?)

0.43.0 [2024.01.14]
-------------------

New features
 * Allow a module to explicitly export packers
 * Show pool pseudocode for `typecheck -r` 
 * Add `typecheck dump` subcommand to show expressions and indices
 * Allow nexus inputs to be files
 * Remove concrete type signatures - always infer
 * Make fields in language-specific table decs optional
     Rather than this:
       table (Person a) = Person {name :: Str, info :: a}
       table R (Person a) = "data.frame" {name :: Str, info :: a}
     Allow this:
       table (Person a) = Person {name :: Str, info :: a}
       table R (Person a) = "data.frame"
     Really, I need to totally redo the table/record/object handling.
 * Remove support for anonymous records in type signatures
     I will re-add this possibly at a future time when I refactor

Infrastructure changes
 * Pass all data between pools as files rather than string arguments
 * Raise an error if any general type cannot be translated to a concrete type

Fixes
 * Fix record type inference 
 * Fix bug in collecting packers (missed packers required by root manifold)
 * Fix C++ handling of quotes and special characters in JSON strings

0.42.0 [2023.10.11]
-------------------

 * Infer concrete types from inferred general types when necessary and possible
 * More informative error messages 
 * Fix template resolution in generated C++ code
 * Fix include name shadowing conflict in generated C++ code
 * Partially fix naming conflict in Python and R pools caused by use of "f". My
   solution was name mangling, though we need a more permanent solution to our
   problem.
 * Let user write correct "list" R types for lists, tuples, and records
 * Fix bug in code generation of lets

## Internal
 * For `ForeignInterfaceM` constructor of ExprM, store the full call type, not
   just the return type
 * Parameterize `ExprM` with the type type (e.g., `Maybe TypeP` or `TypeM`)
 * Replace `Argument` and `PreArgument` with a parameterized `Arg` type.
 * Fix broken `ForeignInterfaceM` and `PoolCallM` cases in `typeOfExprM`
 * Refactor backend

 Testing and documentation:
 * Complete flu demo - it builds now, just need to implement the library code
   for align and other functions.


0.41.1 [2023.05.26]
-------------------

 * Print nothing if "null" is returned
 * Fix the import of working directory modules
 * Resolve bug in occur check
 * Streamline github actions script
 * Fix `morloc install` path download for core modules
 * Raise type error for untyped concrete term
 * Fix bug in concrete type synthesis

0.41.0 [2023.04.16]
-------------------

Language updates
 * Add check to avoid infinite typechecker recursion
 * Synthesize concrete types from general types when possible
 * Improve exports
   * Move exports to module list (as in Haskell)
   * Add `*` wildcard to export every top-level named term or type
   * Raise an error if a non-existing term is exported from a module
 * Allow concrete (un)packers to vary in number of generic terms
 * Set functions with empty inputs (e.g., `()`) to have empty lists of arguments
 * Replace the `Null` literal term with `()`


Package updates
 * Default to c++17, rather than c++11
 * Restructure library to avoid name conflicts with pre-existing packages
 * Replace Perl nexus with Python nexus and remove Perl dependencies

Better error messages and logs
 * Resolve "shit output" error message (map index to export name)
 * Tidy up the verbose diagnostics
 * Print general and concrete types for typecheck with -r option
 * Add multiple levels of verbosity (quiet by default)

Bug fixes
 * Typechecking bug in record access
 * Fix bug allowing undefined things in module export lists
 * Fix cousin module imports
 * Fix unparameterized (un)packer serialization
 * Fix error raised when a module exporting a type is compiled 
 * Fix out of order record fields in nexus output

0.40.0 [2023.02.04]
-------------------

 * Infer types of records
 * Fix bug in concrete type inference
 * Fix bugs in foreign higher order function code generation
 * Simplify generator code
 * Add many tests

0.39.0 [2023.01.03]
-------------------

 * Add compose operator
 * Allow eta-reduction

0.38.0 [2022.12.23]
-------------------

 * Choose casing convention
   * camel case for terms (for now, underscore is illegal)
   * pascal case for types
 * Fix sub-module handling
 * Fix import/export of type definitions
 * Better error messages for import/export errors
 * Somewhat formatted `typecheck` subcommand output 
 * Add option to typecheck backend (concrete types and language selection)

0.37.0 [2022.12.11]
-------------------

 * Remove extra space printed at the end of R JSON
 * Clarify error message for missing exports
 * Clarify error message for missing concrete signature
 * Fix exponential time bug in parser
 * Allow prime characters in names after first position
 * Allow '.' to separate namespaces in imports/modules
 * Fix infinite loop bug when module name != import name

0.36.0 [2022.02.17]
-------------------

 * Separate reals from integers
 * Remove global haskell extensions from package.yaml

0.36.0 [2022.02.17]
-------------------

 * Separate reals from integers
 * Remove global haskell extensions from package.yaml

0.35.0 [2021.10.24]
-------------------

Where scoping and a total architecture refactor

 * Fix handling for generic parameterized types
 * Improve whitespace handling
 * Statements are order invariant
 * Thread link from expression to source expression down to generators 
 * Typecheck over final abstract syntax trees rather than expressions
 * Separate general and concrete typechecking
 * Pretty instances for all data types
 * More testing
 * Agonized over deep and wide structures

0.34.0 [2021.03.05]
-------------------

 * Add handling for C++ float primitives
 * Let C++ programs be imported through a header and shared library
 * Remove semicolon requirement
 * Add hie.yaml for compatibility with hsl
 * Update dependency set to LTS-17.4
 * Add subparsers to CLI with pcapriotti/optparse-applicative 
 * Remove brace requirement for modules and `where` statements
 * Add `-o` option to compiler to specify output executable names
 * Acceptable syntax error messages

0.33.0 [2020.11.03]
-------------------

First hackage release

 * Haddock documentation
 * Update README
 * In help statements write universal, not concrete, types
 * Make default containers non-existential (probably a bad decision?)

0.32.0 [2020.11.01]
-------------------

 * Add record/table field access
 * Fix JSON handling in nexus
 * Fix nexus bug necessitated escaping quotations and braces
 * Print general types in nexus help
 * Resolve most GHC warnings

0.31.0 [2020.10.29]
-------------------

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
