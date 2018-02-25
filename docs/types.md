The goal of Morloc is to unify all languages under a common type system. This
will allow all pure functions to be ordered into multi-lingual databases. These
functions can then be assembled into pipelines with automatic data reading,
writing, checking, and inter-lingual transer.

The type system is the essential core of Morloc. It is the matrix in which all
other functionallity is nested. It is the interface between Morloc script and
function database. The types are the contract between functions.

Components of a type

## Check at compile time

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

## Check at runtime

 3. Invariants/runtime dependencies - This includes dependencies between
    sub-units of a type (e.g. arguments in a functional signature or members of
    a tuple). It is probably not practical to have true, compile-time checked,
    dependent types in Morloc, since functions are black boxes. So these
    dependencies will be checked at runtime (unless compiled without checking).

 4. Distribution - A random model of the data. All data should be modeled. This
    allows random values to be generated and used in automatic testing. The
    distribution can also be handled in real-time to find outliers and possibly
    pathological behaviour.

## Annotation

 5. Performance expectation - There are two layers to this "type" (we are not
    really in type-world anymore, perhaps). First is a performance contract for
    a function signature, where all implementations of the function must meet
    the specified standards. A second layer is a parameterized function for
    modeling time and space usage. This is closer to an annotation. It can be
    inferred through simulation. A major use of this would be to predict
    runtime requirements from the inputs and system data. A programmer may put
    limits on the time and space of the program and halt if a node is predicted
    to exceed them.



The right way and the fast way. 

 1. semantic type system
    - **Right Way**: Consider category theory (groups, semigroups, algebras),
      search literature, base the ontology on theory. Prove that everything
      works.
    - **Fast Way**: Implement the top-level, less-controversial types. Implement
      deeper ontologies for case studies (e.g. in bioinformatics).
    - Make the type system robust. Equivalent types. Assume graph `is_a`
      relations rather than trees. This allows multiple hierarchies to coexist.

 2. base type
    - **Fast Way**: Follow Haskell conventions. Implement the safe stuff. Use
      the GHC typechecker for handling containers.
    - **Right Way**: Learn from Haskell mistakes. Do Prelude right. Use richer
      algebras. Do not use GHC. Do everything through logical inference. Encode
      containers through the knowledge system. The latter transition from the
      "Fast Way" to the "Right Way" here should be mostly in the backend.

 3. Invariants/runtime dependencies
    - **Fast Way**: Make these annotations, which link to functions that can be
      run in the common language to test data.
    - **Right Way**: Find a formal representation of dependencies. Allow the
      reasoner to work on them.

 4. Distribution
    - **Fast Way**: As with runtime dependencies, add distributions as links to
      generative functions.
    - **Right Way**: Find a formal representation for models. Build a statistical
      reasoning machine to work with them. Test consistency between functions
      across the type system. Allow function instances to specialize the
      models.

 5. Performance expectation
    - **Fast Way**: Add as an informal note in the Annotation
    - **Right Way**: Formally express as a mathematical functions of the
      features derived from the inputs (and the input distributions) and
      possibly the architecture. If the distributions for the inputs are
      specified, then inputs can be sampled and the function timed for each
      input set. The resulting data can be fed into a symbolic regression
      machine to infer an analytic solution to the time and space behaviour.


Making all of this work together is a tricky task. The semantic types can be
used to promote semantic types according to rules, insert conversion functions
(e.g. Meter to Foot), raise errors on unresolvable conflicts, etc. The
dependencies, performance, and distribution types all provide information the
compiler can use to add functions to the graph. Once all this preprocessing and
checking is done, the base type can be checked at compile-time using GHC, along
with a Morloc-side check that typeclass functions are implemented where
expected. 

The type properties (Haskell typeclasses) describe what can be done to a type.
For example: Orderable, Foldable, Traversable, Monoid, Semigroup.

Machines need to learn how to use tools. The statistical deel learning approach
gives them data, and then trains a network to recognize patterns. But whether
this will ever be a substitute for knowledge is uncertain. There are other
approaches to machine learning. Symbolic regression. Logic. Knowledge
representation systems allow them to reason and give them common sense. But how
can they use tools and other resources? How can they find them? They need to be
able to reason about functions. Deep learning can be used to create predicates
in a knowledge representation system.

`[Image] -> PhysicalObject -> [Bool]`
`[(Image, Bool)] -> ([Image] -> PhysicalObject -> [Bool])`

Functions are premises. The reasoner can prove that the program is correct, but
the functions are blackboxes, they may not do what their types suggest. The
functions are premises. If the premises are invalid, then the program is
invalid.

Training data, but for testing. Alternatively, have a generative model.

Advantages of a semantic type system.

OWL2 is based on a formal logic system, Description Logics (DL). It is good for
describing the relations between types and what properties types have. The
types and the type constraints (corresponding to Haskell typeclasses) are
axioms. Morloc code can be translated into a list of facts. If the facts are
consistent, the type checker passes. This will allow highly expressive types.
Strictly supersetting, I believe, the Haskell system (need a proof of this,
does DL superset Hask?).

Individuals are "instances" from the type "classes" (not to be confused with
typeclasses ala Haskell, which are class properties).

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

## Mapping to conventional types

A minimal functionallity of ST is to emulate conventional primitive types and
containers. What information do I need to store about each type?

### Number

 * bounds

### String

 * encoding
 * length

### Boolean

### Enum

### Maybe

 * type

Syntactically, I will probably use a '?' for Maybe, for example: `?Int`

### List

 * length
 * element type

### Tuple

 * discrete number of types

### Matrix

 * dimension
 * element type

### Structure

 * list of (name,type) pairs (recursive)

### Table

 * dimension
 * column types
 * ? column dependencies
 * ? column names
 * ? row names
 * ? table metadata
 * ? column metadata
 * ? row metadata
 * ? cell metadata

### Parameterized Boolean

This is a odd type, but something important I want to express: a boolean with
semantic annotation. For example:

```
Filename -> Is Readable
Image    -> Is Dog
String   -> Is Integer
Integer  -> Is Odd
Audio    -> Is HipHop
```

```
-- general
filterImage :: (Image -> Is Thing) -> [Image] -> [Image]

-- more specific
filterImage :: (Image -> Is Airplane) -> [Image] -> [Image]

-- most specific
filterImage :: (Image -> Is Boeing747) -> [Image] -> [Image]
```

These would have the more general types:

```
Filename -> Bool
Image    -> Bool
String   -> Bool
Integer  -> Bool
Audio    -> Bool
```

But these lack semantic meaning.

'Is' implies an equivalence of some sort. 'Has' implies a `has_part` relation.
For example: 

```
[Integer] -> Has 45
Image -> Has Dog
```

### Parameterized Probability

First there is the `ChanceOf` type

```
FeatureTable -> ChanceOf Win
```
