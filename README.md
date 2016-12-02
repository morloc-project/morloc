# Rats

A typed dataflow language that supports loops and conditionals

# Preprocessor

`rats` will be designed to work with the C preprossessor. This will allow macro
usage, ifdef's, include statements, and C-style comments.

# Examples

```
// this is a comment
GetA  :: NA -> a
GetM  :: NA -> m
PrepA :: a -> b
PrepM :: m -> n
Merge :: b -> n -> x

(GetA PrepA) (GetM PrepM) Merge
```

An example with parameter setting (and leaving off type declarations for
brevity)

```
(GetA[x=1] PrepA) (GetM PrepM) Merge
```

Define a dimension to the pipeline, basically a loop

```
dim (xs ys) zs
(GetA[x=xs] PrepA[z=zs]) (GetM[y=ys] PrepM) Merge
```

There are two dimensions in the above pipeline: `(xs ys)` and `zs`. Presumably,
`xs` and `ys` have the same length (but this is a backend problem). The
pipeline will evaluate every combination of `(xs ys)` pairs and `zs` induvidual
scores.

# What it does

Checks type compatibility
