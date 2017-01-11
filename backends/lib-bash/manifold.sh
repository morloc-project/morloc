# This is a sample manifold written in bash. I write this mostly just to hash
# out my thoughts on the bash backend.


m1 (){
    if cache_chk $0
    then
        cache_get $0
    else
        if check1 && check2
        then
            run fun args <(open1 m4) <(open2 m5) |
            tee >(null effect1) |
            tee >(null effect2) |
            pack | cache_put $0
        else
            fail | cache_put $0
        fi
        null hook1
        null hook2
    fi
}


MID (){
    if CACHE_CHK $0
    then
        CACHE_GET $0
    else
        if CHECK
        then
            PASS FUN ARGS POSITIONALS |
            EFFECT 
            PACK | CACHE_PUT $0
        else
            FAIL | CACHE_PUT $0
        fi
        HOOK
    fi
}

# where
# CHECK(f)  -->  f && CHECK(f)
# ARGS(a:args) --> a ARGS(args)
# POSITIONAL(a:pos) --> <(OPEN(o1) a) POSITIONAL(pos)
# OPEN(f) --> f
# PACK(f) --> f
# CACHE_PUT --> 
