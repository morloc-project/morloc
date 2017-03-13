Stars lead to self-reference.

For example:

```
@path
foo

@hook
* :: hahaha
```

The hook `hahaha` calls the hook `hahaha`, forever

Possible solutions:
 1. only apply the rhs to manifolds in @path section.
 2. apply to all manifolds other than self
 3. allow infinite loops: it's a feature, not a bug
