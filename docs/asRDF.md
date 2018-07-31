A Morloc program and all imported modules and ontologies, needs to be encoded
as a RDF triple store. Here I will work out the rules for how elements of
a Morloc script should be translated into RDF.

The system needs to define the

 1. connectivity of the graph (what functions call what)
 2. language-specific type hints
 3. type ontology (including the function types)

Object
 1. WUID - workflow unique identifier, used to identify elements, such as nodes
    in the application graph, that are unique to the workflow.
 2. UUID - universally unique identifiers, such as functions or types from Morloc.IO.
