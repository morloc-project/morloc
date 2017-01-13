define(PROLOGUE, `')

define(NATIVE_MANIFOLD,
    $1(){
        CACHE_$1
        RETURN
    }
)

dnl this currently expands to be the same as the native manifold. I distinguish
dnl between the two because I may eventually use pooled caches for universal
dnl manifolds.
define(UNIVERSAL_MANIFOLD,
    $1(){
        CACHE_$1
        RETURN
    }
)

define(FOREIGN_MANIFOLD,
    $1(){
        call_$2 $1
    }
)

define(RETURN, `')

define(DO_CACHE,
    if BASECACHE_$1`_chk' $1
    then
        BASECACHE_$1`_get' $1
    else
        VALIDATE_$1
    fi
)

define(NO_CACHE,
    VALIDATE_$1
    HOOK_$1
)

define(DO_VALIDATE,
    if CHECK_$1
    then
        CORE($1)
    else
        FAIL_$1$2
    fi
)

define(NOTHING, `echo -n')

define(SIMPLE_FAIL, `return 1')

define(NO_VALIDATE, CORE($1))

define(CHECK, $1)

define(CORE,
    PASS_$1 FUNC_$1 ARG_$1 INPUT_$1
    EFFECT_$1
    PACK_$1 CACHE_PUT_$1
)

define(CALL, <($1) )

define(EFFECT, `| tee >(NULL($1))\n')

define(HOOK, `| NULL($1)')

define(NULL, `$1 > /dev/null')

define(NO_PUT, `')

define(DO_PUT, `| BASECACHE_$1`_put' $1')

define(EPILOGUE,
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
