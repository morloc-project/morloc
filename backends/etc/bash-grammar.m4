define(PROLOGUE, `')

define(EPILOGUE,
`if manifold_exists $`1'
then
    $`1' 
else
    exit 1 
fi'
)

define(MANIFOLD,
    $1(){
        CACHE_$1
    }
)

define(FOREIGN_MANIFOLD,
    $1(){
        CACHE_$1
    }
)

define(RETURN, `Mb')

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
)

define(DO_VALIDATE,
    if CHECK_$1
    then
        PASS_$1 FUNC_$1 INPUT_$1 ARG_$1
        EFFECT_$1
        PACK_$1 CACHE_PUT_$1
    else
        FAIL_$1$2
    fi
    HOOK_$1
)

define(NO_VALIDATE,
    PASS_$1 FUNC_$1 INPUT_$1 ARG_$1
    EFFECT_$1
    PACK_$1 CACHE_PUT_$1
    HOOK_$1
)
