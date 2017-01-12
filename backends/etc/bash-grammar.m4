define(PROLOGUE, `')

define(MANIFOLD,
    $1(){
        CACHE_$1
        RETURN
    }
)

define(FOREIGN_MANIFOLD,
    $1(){
        CACHE_$1
        RETURN
    }
)

define(RETURN, `')

define(DO_CACHE,
    if CACHE_CHK_$1 $1
    then
        CACHE_GET_$1 $1
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

define(NO_VALIDATE, CORE($1))

define(CORE,
    PASS_$1 FUNC_$1 INPUT_$1 ARG_$1
    EFFECT_$1
    PACK_$1 CACHE_PUT_$1
)

define(EPILOGUE,
`if manifold_exists $`1'
then
    $`1' 
else
    exit 1 
fi'
)
