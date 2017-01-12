#!/bin/awk -f 

$1 == "EMIT"  { m[$2]["lang"]      = $3 ; next }
$1 == "CACHE" { m[$2]["cache"]     = $3 ; next }
$1 == "CHECK" { m[$2]["check"][$3] = 1  ; next }
$1 == "FUNC"  { m[$2]["func"]      = $3 ; next }
$1 == "PASS"  { m[$2]["pass"]      = $3 ; next }
$1 == "FAIL"  { m[$2]["fail"]      = $3 ; next }
$1 == "PACK"  { m[$2]["pack"]      = $3 ; next }
$1 == "EFCT"  { m[$2]["efct"][$3]  = 1  ; next }
$1 == "HOOK"  { m[$2]["hook"][$3]  = 1  ; next }
$1 == "ARG"   { m[$2]["arg"][$3]   = $4 ; next }

$1 == "INPM"  { m[$2]["m"][$3] = $4 ; next } 
$1 == "INPP"  { m[$2]["p"][$3] = $4 ; next } 
{ }

END{
    printf "PROLOGUE\n" >> body
    for(i in m){

        if(m[i]["lang"] == "bash"){
            printf "MANIFOLD(%s)\n", i >> body
            printf "define(OPEN_%s, <(run %s))\n", i, i >> rules
        } else {
            printf "FOREIGN_MANIFOLD(%s)\n", i >> body
            if(m[i]["lang"]){
                printf "define(OPEN_%s, <(call-%s.sh %s))\n", i, m[i]["lang"], i >> rules
            } else {
                printf "define(OPEN_%s, <(call-unknown.sh %s))\n", i, m[i]["lang"], i >> rules
            }
        }

        if(m[i]["cache"]){
            cache = m[i]["cache"]
            printf "define(CACHE_%s, DO_CACHE(%s) )\n", i, i, cache >> rules
            printf "define(CACHE_PUT_%s, %s| %s_put %s%s)\n", i, L, cache, i, R >> rules
            printf "define(CACHE_CHK_%s, %s_chk)\n", i, cache >> rules
            printf "define(CACHE_GET_%s, %s_get)\n", i, cache >> rules
        } else {
            printf "define(CACHE_%s, NO_CACHE(%s))\n", i, i >> rules
            printf "define(CACHE_PUT_%s, %s%s)\n", i, L, R >> rules
        }

        if(length(m[i]["check"]) > 0){
            printf "define(VALIDATE_%s, DO_VALIDATE(%s))\n", i, i >> rules
            chech=""
            for(k in m[i]["check"]){
                check = check " && " k
            }
            gsub(/^ && /, "", check) # remove the last sep
            printf "define(CHECK_%s, %s)\n", i, check >> rules
        } else {
            printf "define(VALIDATE_%s, NO_VALIDATE(%s))\n", i, i >> rules
        }

        if( "m" in m[i] || "p" in m[i] ){
            k=0
            input=""
            while(1) {
                if(k in m[i]["m"]){
                    input = input " OPEN_" m[i]["m"][k]
                }
                else if(m[i]["p"][k]){
                    input = input " " m[i]["p"][k] 
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
                arg = arg " " k " " m[i]["arg"][k]
            }
            printf "define(ARG_%s, %s)\n", i, arg >> rules

        } else {
            printf "define(ARG_%s, %s%s)\n", i, L, R >> rules
        }

        if(length(m[i]["efct"]) > 0){
            effect=""
            for(k in m[i]["efct"]){
                effect = effect "| tee >(null "k")\n"
            }
            printf "define(EFFECT_%s, %s)\n", i, effect >> rules

        } else {
            printf "define(EFFECT_%s, %s%s)\n", i, L, R >> rules
        }

        if(length(m[i]["hook"]) > 0){
            hook=""
            for(k in m[i]["hook"]){
                hook = sprintf("%s| null %s\n", hook, k)
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
