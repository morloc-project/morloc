# Manifold templates

```
# Where x and y are outside arguments coming in as JSON
# The third argument comes from a downstream argument
m1(x, y) =
    @effect1
    if (isCached) {
        @effect2
        x = getCache
        @effect3
    } else {
        @effect4
        x_ = unpackX(x)
        y_ = unpackY(y)
        z_ = m2(y_)
        assert(constraint1(x_))
        assert(constraint2(x_, z_))
        @effect5
        x = foo(x_, z_)
        @effect6
    }
    @effect7
    return(x)
}
```
