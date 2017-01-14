define(PROLOGUE, `')

define(NATIVE_MANIFOLD,
    $1 <- function(){
        CACHE_$1
        RETURN
    }
)

define(UNIVERSAL_MANIFOLD,
    $1 <- function(){
        CACHE_$1
        RETURN
    }
)

define(FOREIGN_MANIFOLD,
    $1 <- function(){
        system("call.$2 $1", intern = TRUE)
    }
)

define(RETURN, `Mb')


define(DO_CACHE,
    if`(BASECACHE_$1`_chk'($1))'{
        BASECACHE_$1`_get'($1)
    } else {
        VALIDATE_$1
    }
)

define(NO_CACHE,
    VALIDATE_$1
    HOOK_$1
)


define(DO_VALIDATE,
`
    if(CHECK_$1){
        CORE($1)
    } else {
        Mb <- FAIL_$1`()'
        dnl missing cache
    }
'
)

define(NO_VALIDATE, `CORE($1)')

define(CHECK, $1`()')

define(CORE,
`
    b <- RUN_$1
    EFFECT_$1
    PACK_$1
    CACHE_PUT_$1
'
)

define(DO_CACHE_PUT, BASECACHE_$1`("$1")')

define(NO_CACHE_PUT, `')

define(DO_PACK, Mb <- PACKFUN_$1`(b)')

define(NO_PACK, Mb <- b)

define(DO_PASS, `PASS_$1`(FUNC_$1 ARG_$1 INPUT_$1)'')

define(NO_PASS, `FUNC_$1`(ARG_$1 INPUT_$1)'')

define(CALL, $1`()')

define(EFFECT, `$1(b)\n')

define(HOOK, `$1()\n')

define(NOTHING, `NULL')

define(SIMPLE_FAIL, `return NULL')

define(NO_PUT, `')

define(DO_PUT, `BASECACHE_$1`_put'($1)')

define(EPILOGUE, `')
