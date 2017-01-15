m4_define(`PROLOGUE', )

m4_define(`NATIVE_MANIFOLD',
    $1(){
        CACHE_$1
        RETURN
    }
)

m4_dnl this currently expands to be the same as the native manifold. I distinguish
m4_dnl between the two because I may eventually use pooled caches for universal
m4_dnl manifolds.
m4_define(`UNIVERSAL_MANIFOLD',
    $1(){
        CACHE_$1
        RETURN
    }
)

m4_define(`FOREIGN_MANIFOLD',
    $2(){
        ./call.$1 $2
    }
)

m4_define(`RETURN', )

m4_define(`DO_CACHE',
    if BASECACHE_$1``_chk'' $1
    then
        BASECACHE_$1``_get'' $1
    else
        VALIDATE_$1
    fi
)

m4_define(`NO_CACHE',
    VALIDATE_$1
    HOOK_$1
)

m4_define(`DO_VALIDATE',
    if CHECK_$1
    then
        CORE($1)
    else
        FAIL_$1$2
    fi
)

m4_define(`NOTHING', `echo -n')

m4_define(`SIMPLE_FAIL', `return 1')

m4_define(`NO_VALIDATE', CORE($1))

m4_define(`CHECK', $1)

m4_define(`CORE', RUN_$1 EFFECT_$1 PACK_$1 CACHE_PUT_$1)

m4_define(`DO_PACK', PACKFUN_$1)

m4_define(`NO_PACK', )

m4_define(`DO_PASS', PASS_$1 FUNC_$1 ARG_$1 INPUT_$1)

m4_define(`NO_PASS', FUNC_$1 ARG_$1 INPUT_$1)

m4_define(`CALL', <($1) )

m4_define(`EFFECT', `| tee >(NULL($1))')

m4_define(`HOOK', `| NULL($1)')

m4_define(`NULL', `$1 > /dev/null')

m4_define(`NO_PUT', )

m4_define(`DO_PUT', `| BASECACHE_$1``_put'' $1')

m4_define(`EPILOGUE',
`
manifold_exists() {
    type $`1' | grep -q function
}
if manifold_exists $`1'
then
    $`1' 
else
    exit 1 
fi'
)
