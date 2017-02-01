def m1():
    # @0
    if(cache_chk("m1")):
        # @8
        b = cache_get("m1")
        # @9
    else:
        # @2
        if( [check] ):
            # @4
            b = fun([input], [arg])
            cache_put("m1", b)
            # @5
        else:
            # @6
            b = fail()
            cache_put("m1", b)
            # @7
        # @3
    # @1
    return b
