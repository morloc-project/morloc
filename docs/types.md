The goal of Morloc is to unify all languages under a common type system. This
will allow all pure functions to be ordered into multi-lingual databases. These
functions can then be assembled into pipelines with automatic data reading,
writing, checking, and inter-lingual transer.

The type system is the essential core of Morloc. It is the matrix in which all
other functionallity is nested. It is the interface between Morloc script and
function database. The types are the contract between functions.

Components of a type

 1. Semantic/ontological types - Orthogonal to the base type, is the semantic
    type system. It is an ontology. I will likely base it on OWL. The semantic
    type system expands upon the base type. For example, the base type might be
    `Double`. But `Double` really is not very informative. The semantic type
    can specialize this into, for example, `Meter`, `Second`, or `RadianAngle`.
    Relations can be set between semantic types (e.g. `is_a Meter Distance`).
    Properties can be added to them, to set the range of the value, add
    a distribution, etc. 

 2. Base type - This will consist of a large subset of the Haskell type system.
    Including algebraic types, type classes (will need to include functions
    which are implemented in each language), and parameterized types. The
    purpose of the base type is to allow rigorous type checking and code reuse
    according to property (e.g. all the functor, monoid, foldable, etc wonders
    of Haskell).

 3. Invariants/runtime dependencies - This includes dependencies between
    sub-units of a type (e.g. arguments in a functional signature or members of
    a tuple). It is probably not practical to have true, compile-time checked,
    dependent types in Morloc, since functions are black boxes. So these
    dependencies will be checked at runtime (unless compiled without checking).

 4. Performance expectation - There are two layers to this "type" (we are not
    really in type-world anymore, perhaps). First is a performance contract for
    a function signature, where all implementations of the function must meet
    the specified standards. A second layer is a parameterized function for
    modeling time and space usage. This is closer to an annotation. It can be
    inferred through simulation. A major use of this would be to predict
    runtime requirements from the inputs and system data. A programmer may put
    limits on the time and space of the program and halt if a node is predicted
    to exceed them.

 5. Distribution - A random model of the data. All data should be modeled. This
    allows random values to be generated and used in automatic testing. The
    distribution can also be handled in real-time to find outliers and possibly
    pathological behaviour.

Making all of this work together is a tricky task. The semantic types can be
used to promote semantic types according to rules, insert conversion functions
(e.g. Meter to Foot), raise errors on unresolvable conflicts, etc. The
dependencies, performance, and distribution types all provide information the
compiler can use to add functions to the graph. Once all this preprocessing and
checking is done, the base type can be checked at compile-time using GHC, along
with a Morloc-side check that typeclass functions are implemented where
expected. 




OWL as a type system, beyond correctness. Imposing semantic meaning on
functions. This allows them to be organized and searched in databases.

Google is trying to organize all of human knowledge. The semantic web.

The base type can be derived from the semantic type and vice versa. The
semantic type is more specific and contains much information that would never
go into the base type (and that would not be used in type checking).

Ontologies have been springing up all over. Perhaps the largest and most
developed are the biology ones, but there are many more:
[physics](https://www.astro.umd.edu/~eshaya/astro-onto/ontologies/physics.html),
[statistics](http://stato-ontology.org/),
[chemistry](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2867191/).
