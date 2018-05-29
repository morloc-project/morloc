Monolithic tools often do not resolve cleanly into a single type. This is
especially true of standalone tools. But it is also true of many functions in
dynamic languages. The problem is that they may have many possible types of
input and output.

 * input type depends on parameters
 * input type depends on input value (ad hoc polymorphism)
 * output type depends on parameters
 * output type depends on input value
 * output type depends on output value
 * cardinality of input depends on parameters
 * and more

Enumerating all combinations of types would be a naming nightmare. 

Allowing function overloading reduces this problem somewhat.

We could infer the output type, then attempt to cast it to the expected output
type. For example, suppose we have a function that returns a list of integers
if the list is longer than 1 and a single integer otherwise. Morloc could
generate a code in the manifold that checks the type of the output function,
then casts it as needed.

Some of these issues may need to be solved by workarounds in the wrapper (such
as hard-coding certain parameters or processing the output or input).

One way to solve this is to consider the tool's function signature as a graph
of types, rather than a singular entity. ... elaborate tomorrow ... 
