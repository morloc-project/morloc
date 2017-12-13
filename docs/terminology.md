 * node

A wrapper around a function or composition of functions. It may carry state and
effects can be attached to it. For example, arbitrary functions of its output
can be attached to it (leading to out-of-plane branching). Or conditions can be
set that must be met before the contained function is run. Or caching functions
may be added.


 * programmer versus composer

Where I say `programmer`, I mean the one who writes the functions in a
non-Morloc language. Where I say `composer`, I mean the person who writes
Morloc code (or more generally, any pure scripting language).


 * double-blind

Morloc is designed to allow a double-blind relation between composers and
programmers.

The blind programmer doesn't how their code is being used by Morloc. They write
as they like using the data types that make sense for their problem. They only
know the general function type signature, which contains nothing specific to
their language. 

The blind composer builds workflows from nodes. They know the type of the nodes
and, based on documentation, know what they do. But they know nothing about the
implementation, including the language.

A doubly blind system is a system where all programmers and all composers are
blind. Only the function signatures are known. The signatures play the role of
an API.


 * common form

A data form that is used everywhere between functions, and is independent of
any particular language. It is a data representation such as JSON, YAML, ASCII
text, or whatever (the details depend on the implementation and should not
normally be the concern of either the programmer or composer).


 * cis and trans connections

A *cis* connection is a call between two functions of the same language.
A *trans* connection is between two different languages.

*cis* connections are completely idiomatic. They compile to something like
`foo(bar())` in the target language. They can use language-specific features
(like laziness). They can also use types that are not supported by Morloc. No
types even need to be specified for them. However, this allows
language-inflexibility, where two nodes become coupled such that they have to
be in the same language. This goes against the Morloc philosophy of double
blindness, where neither programer or composer needs to know about the other.

*trans* calls are easy to control. The results always pass through the common
type (barring future optimizations). In a *trans* connection, `foo` would have
to make a system call to the program that has `bar` (optimizations aside). This
is likely slow. Also the generated code is less concise.

 * strict versus non-strict mode

In strict mode, all connections must be *trans*. This provides fine granularity
of errors, you can know exactly where the failure occurs and handle it in
a clean uniform way. This also forces double-blindness by preventing reliance
on language specific features.


 * functions of nodes

This is a composition that is indistinguishable from a normal node, with the
exception that its language cannot be set (since it is in Morloc). This allows
hiding of the particular nodes used inside. It also allows inputs to be freely
distributed inside the internal composition.


 * caching

The output of node with caching toggled will run exactly once. Any subsequent
calls to it will receive only the cache result. The caching is persistent.


 * early and late validator

Validators come before (early) or after (late) a function is called within
a node. Validators are themselves nodes. They must be in a layer that inherits
from the node they validate.


 * output and input filter

These filters take the data flowing through an edge, and access it, possibly
modifying it. They could be used to create log messages. Or they could be used
to sample from or subset the data flow.


 * layer

A layer is an environment where nodes can be composed. They can link only to
other nodes in the layer and to nodes in any layer their parent layer inherits
from.

Layers define scope.

Layers are important when adding modifications to a node, for examle:

```
A = (foo . bar) x y z 

after A :: baz
```

Which calls baz on the output of `foo . bar` after it has take the inputs `x`,
`y`, `z`.


 * anonymous layer

Layers don't have to have names. If they do not have names, then nothing can
inherit from them.


 * master nodes, slave node, and slave layers

A master node is a node that takes other nodes as input and uses them
internally as functions. The slave is the node so used. Masters can pass
arguments to slaves. They may call a slave many times with different arguments.
The entire graph of nodes downstream of the slaves, all filters, listeners, and
anything touching the slave, is wrapped into a slave layer. Nodes in the slave
layer link to each other or to arguments passed from the master.

A new slave layer is generated every time the top slave node is called. Each
slave layer possesses a globally unique id. 

Slaves may have slaves. In which case, the unique id of a slave of a slave
would contain two elements.


 * import

Import a set of nodes from a module. These nodes behave exactly as if they were
defined locally. But nodes that were not exported by the module cannot be
altered in any way. Naming conflicts result in compile time errors, which can
be resolved by specifying the namespace (name of the imported module).


 * module

A module exports a specific set of nodes and specifies which parameters may be
set. Node attributes cannot be overridden.


 * node signature

The general type signature for the function in the node. It is general, in that
this signature is language-independent. See *type system* for details.

A node signature plays the role of a specification. The signature, along with
a human language description of what it should do, should be all that is needed
to implement the function. But there are additional formal constraints that
could reasonably be added. For example, time and space runtime properties.

In languages like Haskell (or other ML variants), type signatures are optional.
Though these languages are statically typed, explicit signatures are not
necessary since the type can be inferred from the code. However, in Morloc the
code is inside foreign languages, and thus are black boxes. This makes type
inference impossible in general. So explicit types are required (except in
non-strict *cis* calls).

 * type system

The type system is under development. It is the core of the Morloc ecosystem,
but so far is modeled heavily after the Haskell system. I want type classes so
that I can specify the properties of a type. However, I also want to be able to
layer on "soft" types. Whereas the "hard" types would be formally checked at
compile time, the soft-types would have more runtime use. Or be used to testing
specification and testing. Or used to generate random instances of the type.
Soft types would include type dependencies, dependencies between types in
a function signature, and the distribution of a type. 

 * type random model 

For every type there is a random generative model. These random models may be
automatically generated. Or they may be specified in detail in the "soft" type.
They may be simple, such as a uniform distribution between 2^-63 and 2^+63, or
complex, such as Markov model for generating random English text. The
generative models are used to test and optimize a workflow.


 * parameter

A parameter is an keyword argument to a target language function and the may be
optional. Not all languages have parameters. R and python do; Haskell doesn't.
UNIX commands do (as `-*` flags). Parameterization can be modularized into
configuration scripts.


 * parameter type

This is experimental. Currently, in old Morloc, parameters are expressed freely
without compile time checking. But this is pretty loose and highly language
dependent. The parameter typing allows tighter control of the functions and may
be used to provide automated documentation of parameters.


 * specialization

Ideally the type signature is all that is needed to generate required IO. But
often more is needed. Should a table passed to R, for example, be
a `data.frame`, a `data.table`, a list of vectors, a `matrix`, a `tibble`, etc.
Or should a string sent to Haskell be a String, ByteString or Text? In such
cases, hints must be passed to the compiler. The specialization syntax needs to
be general enough to allow great flexibility in meeting the needs of wildly
different languages. Perhaps it can be a YAML block.

Providing this information is the responsibility of the programmer, since it is
language specific. The composer can pass hints to the programmer in the form of
soft types (such as the distribution of data and time/space complexity).


 * primitives

Primitives are terminal values in a workflow that are not modifiable, e.g.
numbers, strings.


 * Workflow Intermediate Language

This is a simple language designed to be easy to parse by a machine. It
contains all the information needed to build the Morloc executable, but is
abstracted away from the particular syntax used for specification.


 * Void, wells and sinks

A node that either writes to or reads from something outside the program will
have type `Void`. Functions with a void input are *wells*, they produce data,
but take nothing from other nodes. Functions that have a void output, take data
and do something with it, passing nothing on. Since functions are black boxes
in Morloc, there is no guarantee that a function without a void in its type
signature doesn't interact impurely with the system.
