#!/bin/awk -f 

$1 == "EMIT"  { m[$3]["lang"]      = $2 ; next }
$1 == "CACHE" { m[$2]["cache"]     = $3 ; next }
$1 == "CHECK" { m[$2]["check"][$3] = 1  ; next }
$1 == "FUNC"  { m[$2]["func"]      = $3 ; next }
$1 == "PASS"  { m[$2]["pass"]      = $3 ; next }
$1 == "FAIL"  { m[$2]["fail"]      = $3 ; next }
$1 == "PACK"  { m[$2]["pack"]      = $3 ; next }
$1 == "EFCT"  { m[$2]["efct"][$3]  = 1  ; next }
$1 == "ARG"   { m[$2]["arg"][$3]   = $4 ; next }

$1 == "INPM"  { m[$2]["m"][$3] = $4 ; next } 
$1 == "INPP"  { m[$2]["p"][$3] = $4 ; next } 
{ }

END{
    for(i in m){

        if(m[i]["lang"] == "bash"){
            printf "MANIFOLD(%s)", i >> body
            printf "define(OPEN_%s, <(run %s))", i, i >> body
        } else {
            printf "FOREIGN_MANIFOLD(%s)", i >> body
            if(m[i]["lang"]){
                printf "define(OPEN_%s, <(exe-%s.sh %s))", i, m[i]["lang"], i >> body
            } else {
                printf "define(OPEN_%s, <(exe-unknown.sh %s))", i, m[i]["lang"], i >> body
            }
        }

        if(m[i]["cache"]){
            cache = m[i]["cache"]
            printf "define(CACHE_%s, DO_CACHE(%s) )", i, i, cache >> rules
            printf "define(CACHE_PUT_%s, %s| %s_put $mid%s)", i, L, cache, R >> rules
            printf "define(CACHE_CHK_%s, %s_chk)", i, cache >> rules
            printf "define(CACHE_GET_%s, %s_get)", i, cache >> rules
        } else {
            printf "define(CACHE_%s, NO_CACHE(%s))", i, i >> rules
            printf "define(CACHE_PUT_%s, %s%s)", i, L, R >> rules
        }

        if(length(m[i]["check"]) > 0){
            printf "define(VALIDATE_%s, DO_VALIDATE(%s))", i, i >> rules
            for(k in m[i]["check"]){
                check = check " && " k
            }
            gsub(/^ && /, "", check) # remove the last sep
            printf "define(CHECK_%s, %s)", i, check >> rules
        } else {
            printf "define(VALIDATE_%s, NO_VALIDATE(%s))", i, i >> rules
        }

        if( "m" in m[i] || "p" in m[i] ){
            k=0
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
            printf "define(INPUT_%s, %s)", i, input >> rules
        } else {
            printf "define(INPUT_%s, %s%s)", i, L, R >> rules
        }

        if(length(m[i]["arg"]) > 0){
            for(k in m[i]["arg"]){
                arg = arg " " k " " m[i]["arg"][k]
            }
            printf "define(ARG_%s, %s)", i, arg >> rules

        } else {
            printf "define(ARG_%s, %s%s)", i, L, R >> rules
        }

        if(length(m[i]["efct"]) > 0){
            for(k in m[i]["efct"]){
                effect = effect "tee >(null "k") |"
            }
            printf "define(EFFECT_%s, %s)", i, effect >> rules

        } else {
            printf "define(EFFECT_%s, %s%s)", i, L, R >> rules
        }

        if(length(m[i]["hook"]) > 0){
            for(k in m[i]["hook"]){
                hook = hook "tee >(null "k") |"
            }
            printf "define(HOOK_%s, %s)", i, hook >> rules

        } else {
            printf "define(HOOK_%s, %s%s)", i, L, R >> rules
        }

        if(m[i]["func"]){
            printf "define(FUNC_%s, %s)", i, m[i]["func"] >> rules
        } else {
            printf "define(FUNC_%s, nothing)", i >> rules
        }

        if(m[i]["pass"]){
            printf "define(PASS_%s, %s)", i, m[i]["pass"] >> rules
        } else {
            printf "define(PASS_%s, run)", i >> rules
        }

        if(m[i]["fail"]){
            printf "define(FAIL_%s, %s)", i, m[i]["fail"] >> rules
        } else {
            printf "define(FAIL_%s, nothing)", i >> rules
        }

        if(m[i]["pack"]){
            printf "define(PACK_%s, %s)", i, m[i]["pack"] >> rules
        } else {
            printf "define(PACK_%s, id)", i >> rules
        }

    }

}
