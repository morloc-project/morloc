#!/bin/awk -f 

BEGIN {
    seps["bash"] = " "    
    seps["R"] = ", "

    binds["bash"] = " "
    binds["R"] = "="

    ands["bash"] = "&&"
    ands["R"] = "&&"

    printf("define(call_r, `call-r.R')\n")        >> rules
    printf("define(call_bash, `call-bash.sh')\n") >> rules

    printf("define(SEP, `%s')\n", seps[lang])   >> rules
    printf("define(BIND, `%s')\n", binds[lang]) >> rules
    printf("define(AND, `%s')\n", ands[lang])   >> rules
}

$1 == "EMIT"  { m[$2]["lang"]      = $3 ; next }
$1 == "CACHE" { m[$2]["cache"]     = $3 ; next }
$1 == "CHECK" { m[$2]["check"][$3] = 1  ; next }
$1 == "FUNC"  { m[$2]["func"]      = $3 ; next }
$1 == "PASS"  { m[$2]["pass"]      = $3 ; next }
$1 == "FAIL"  { m[$2]["fail"]      = $3 ; next }
$1 == "PACK"  { m[$2]["pack"]      = $3 ; next }
$1 == "OPEN"  { m[$2]["open"]      = $3 ; next }
$1 == "EFCT"  { m[$2]["efct"][$3]  = 1  ; next }
$1 == "HOOK"  { m[$2]["hook"][$3]  = 1  ; next }
$1 == "INPM"  { m[$2]["m"][$3]     = $4 ; next }
$1 == "INPP"  { m[$2]["p"][$3]     = $4 ; next }

$1 == "ARG" {
    if($4) { arg = $3 " BIND " $4 } else { arg = $3 }
    m[$2]["arg"][arg] = 1
    next
}

END{

    printf "PROLOGUE\n" >> body

    for(i in m){

        printf "MANIFOLD_%s\n", i >> body

        if(m[i]["lang"] == lang){
            printf "define(MANIFOLD_%s, NATIVE_MANIFOLD(%s))\n", i, i >> rules
        } else if(m[i]["lang"] == "*"){
            printf "define(MANIFOLD_%s, UNIVERSAL_MANIFOLD(%s))\n", i, i >> rules
        } else {
            printf "define(MANIFOLD_%s, FOREIGN_MANIFOLD(%s,%s))\n", i, m[i]["lang"], i >> rules
        }

        if(m[i]["cache"]){
            cache = m[i]["cache"]
            printf "define(BASECACHE_%s, %s)\n", i, cache >> rules
            printf "define(CACHE_%s, DO_CACHE(%s))\n", i, i >> rules
            printf "define(PUT_%s, DO_PUT(%s))\n", i, i >> rules
        } else {
            printf "define(CACHE_%s, NO_CACHE(%s))\n", i, i >> rules
            printf "define(CACHE_PUT_%s, NO_PUT(%s))\n", i, i >> rules
        }

        if(length(m[i]["check"]) > 0){
            printf "define(VALIDATE_%s, DO_VALIDATE(%s))\n", i, i >> rules
            check=""
            for(k in m[i]["check"]){
                check = sprintf("%s AND CHECK(%s)", check, k)
            }
            gsub(/^ AND /, "", check) # remove the last sep
            printf "define(CHECK_%s, %s)\n", i, check >> rules
        } else {
            printf "define(VALIDATE_%s, NO_VALIDATE(%s))\n", i, i >> rules
        }

        if( "m" in m[i] || "p" in m[i] ){
            k=0
            input=""
            while(1) {
                if(k in m[i]["m"]){
                    input = sprintf("%s SEP CALL(%s)", input, m[i]["m"][k])
                }
                else if(m[i]["p"][k]){
                    input = input " " m[i]["p"][k]
                    input = sprintf("%s SEP %s", input, m[i]["p"][k])
                }
                else {
                    break
                }
                k = k + 1
            }
            printf "define(INPUT_%s, %s)\n", i, input >> rules
        } else {
            printf "define(INPUT_%s, %s%s)\n", i, L, R >> rules
        }

        if(length(m[i]["arg"]) > 0){
            arg=""
            for(k in m[i]["arg"]){
                arg = sprintf("%s SEP %s", arg, k)
            }
            printf "define(ARG_%s, %s)\n", i, arg >> rules
        } else {
            printf "define(ARG_%s, %s%s)\n", i, L, R >> rules
        }

        if(length(m[i]["efct"]) > 0){
            effect=""
            for(k in m[i]["efct"]){
                effect = sprintf("%s EFFECT(%s) \n", effect, k)
            }
            printf "define(EFFECT_%s, %s)\n", i, effect >> rules
        } else {
            printf "define(EFFECT_%s, %s%s)\n", i, L, R >> rules
        }

        if(length(m[i]["hook"]) > 0){
            hook=""
            for(k in m[i]["hook"]){
                hook = sprintf("%s HOOK(%s)\n", hook, k)
            }
            printf "define(HOOK_%s, %s)\n", i, hook >> rules
        } else {
            printf "define(HOOK_%s, %s%s)\n", i, L, R >> rules
        }

        if(m[i]["func"]){
            printf "define(FUNC_%s, %s)\n", i, m[i]["func"] >> rules
        } else {
            printf "define(FUNC_%s, nothing)\n", i >> rules
        }

        if(m[i]["pass"]){
            printf "define(PASS_%s, %s)\n", i, m[i]["pass"] >> rules
        } else {
            printf "define(PASS_%s, %s%s)\n", i,L,R >> rules
        }

        if(m[i]["open"]){
            print "WARNING: `open` is not yet supported" >> "/dev/stderr"
            # printf "define(OPEN_%s, OPEN(%s))\n", i, i, m[i]["open"] >> rules
        } else {
            # printf "define(OPEN_%s, %s%s)\n", i,L,R >> rules
        }

        if(m[i]["fail"]){
            printf "define(FAIL_%s, %s)\n", i, m[i]["fail"] >> rules
        } else {
            printf "define(FAIL_%s, nothing)\n", i >> rules
        }

        if(m[i]["pack"]){
            printf "define(PACK_%s, %s)\n", i, m[i]["pack"] >> rules
        } else {
            printf "define(PACK_%s, %s%s)\n", i, L, R >> rules
        }

    }

    printf "EPILOGUE\n" >> body

}
