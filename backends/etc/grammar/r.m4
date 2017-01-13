define(PROLOGUE, `')

define(MANIFOLD,
    $1 <- function(){
        CACHE_$1
        RETURN
    }
)

define(FOREIGN_MANIFOLD,
    $1 <- function(){
        CACHE_$1
        RETURN
    }
)

define(RETURN, `return Mb')

define(DO_CACHE,
    if(CACHE_CHK_$1("$1")){
        CACHE_GET_$1("$1")
    } else {
        VALIDATE_$1
    }
)

define(NO_CACHE,
    VALIDATE_$1
    HOOK_$1
)

define(DO_VALIDATE,
    if(CHECK_$1("$1")){
        CORE($1)
    } else {
        Mb <- FAIL_$1()
        $2("m1")
    }
)

define(NO_VALIDATE, CORE($1))

define(CORE,
    b <- PASS_$1(FUNC_$1 INPUT_$1 ARG_$1)
    EFFECT_$1
    Mb <- PACK_$1(b)
    CACHE_PUT_$1("$1")
)

define(EPILOGUE, `')
