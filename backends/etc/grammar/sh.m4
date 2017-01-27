m4_changequote(`<[', `]>')

m4_define(<[__SEP__]>, <[ ]>)
m4_define(<[__BIND__]>, <[ ]>)
m4_define(<[__AND__]>, <[&&]>)

m4_define(<[ARG_LIST]>, <[m4_ifelse($1, 0, <[]>, <[ARG_LIST(m4_decr($1)) $$1]>)]>)

m4_define(<[UID_ARG]>, <[m4_ifelse(NARG_$1, 0, <[]>, <[<[ $uid]>]>)]>)

m4_define(<[UID_INP]>, <[m4_ifelse($1, 0, <[]>, <[uid=$<[m4_incr($1)]>]>)]>)

m4_define(<[MAKE_UID]>, 
$1_uid=0
wrap_$1 () {
    $1_uid=$(( $1_uid + 1 ))
    uid=$$1_uid
    $1 <[ARG_LIST(NARG_$1) $uid]>
}
)

m4_define(<[UID_WRAP]>, wrap_$1 )

m4_define(<[NTH_ARG]>, $$1)

m4_define(<[PROLOGUE]>,
<[to_stderr () {
    $<[]>@ 1>&2
}]>
)

m4_define(<[NATIVE_MANIFOLD]>,
    $1(){
        <[UID_INP(NARG_$1)]>
        HOOK0_$1
        CACHE_$1
        HOOK1_$1
    }
)

m4_dnl this currently expands to be the same as the native manifold. I distinguish
m4_dnl between the two because I may eventually use pooled caches for universal
m4_dnl manifolds.
m4_define(<[UNIVERSAL_MANIFOLD]>,
    $1(){
        <[UID_INP(NARG_$1)]>
        HOOK0_$1
        CACHE_$1
        HOOK1_$1
    }
)

m4_define(<[FOREIGN_MANIFOLD]>,
    $2(){
        ./call.$1 $2
    }
)

m4_define(<[DO_CACHE]>,
    if BASECACHE_$1<[<[_chk]>]> $1 <[UID_ARG($1)]>
    then
        HOOK8_$1
        BASECACHE_$1<[<[_get]>]> $1 <[UID_ARG($1)]>
        HOOK9_$1
    else
        HOOK2_$1
        VALIDATE_$1
        HOOK3_$1
        ( cat $1_tmp ; rm $1_tmp )
    fi
)

m4_define(<[NO_CACHE]>,
    HOOK2_$1
    VALIDATE_$1
    HOOK3_$1
    ( cat $1_tmp ; rm $1_tmp )
)

m4_define(<[DO_VALIDATE]>,
    if CHECK_$1
    then
        CORE($1)
    else
        HOOK6_$1
        FAIL_$1$2
        HOOK7_$1
    fi
)

m4_define(<[NOTHING]>, <[echo -n]>)

m4_define(<[SIMPLE_FAIL]>, <[return 1]>)

m4_define(<[NO_VALIDATE]>, CORE($1))

m4_define(<[CORE]>,
    HOOK4_$1
    FUNC_$1 ARG_$1 INPUT_$1 > $1_tmp
    CACHE_PUT_$1
    HOOK5_$1
)

m4_define(<[CALL]>, <($1 <[ARG_LIST(NARG_$2) UID_ARG($2) ]>) )

m4_define(<[CHECK]>, $1 <[ARG_LIST(NARG_$2) UID_ARG($2) ]> )

m4_define(<[HOOK]>, to_stderr $1 <[ARG_LIST(NARG_$2) UID_ARG($2)]>)

m4_define(<[NO_PUT]>, )

m4_define(<[DO_PUT]>, <[BASECACHE_$1<[<[_put]>]> $1 $1_tmp UID_ARG($1)]>)

m4_define(<[EPILOGUE]>,
<[
manifold_exists() {
    type $<[1]> | grep -q function
}
if manifold_exists $<[1]>
then
    if [[ -f $<[1]>_tmp ]]
    then
        cat $<[1]>_tmp
    else
        $<[1]> 
    fi
else
    exit 1 
fi]>
)
