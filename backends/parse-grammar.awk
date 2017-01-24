#!/bin/awk -f 

BEGIN {

    FS="\t"

    printf("m4_define(<[OUTDIR]>, %s)\n", dir) >> rules

    seps["sh"] = " "    
    seps["R"] = ", "

    binds["sh"] = " "
    binds["R"] = "="

    ands["sh"] = "&&"
    ands["R"] = "&&"

    printf("m4_define(<[SEP]>, <[%s]>)\n",  seps[lang])  >> rules
    printf("m4_define(<[BIND]>, <[%s]>)\n", binds[lang]) >> rules
    printf("m4_define(<[AND]>, <[%s]>)\n",  ands[lang])  >> rules
}

$1 == "EMIT" { m[$2]["lang"]      = $3 ; next }
$1 == "CACH" { m[$2]["cache"]     = $3 ; next }
$1 == "CHEK" { m[$2]["check"][$3] = 1  ; next }
$1 == "FUNC" { m[$2]["func"]      = $3 ; next }
$1 == "PASS" { m[$2]["pass"]      = $3 ; next }
$1 == "FAIL" { m[$2]["fail"]      = $3 ; next }
$1 == "PACK" { m[$2]["pack"]      = $3 ; next }
$1 == "OPEN" { m[$2]["open"]      = $3 ; next }
$1 == "EFCT" { m[$2]["efct"][$3]  = 1  ; next }
$1 == "HOOK" { m[$2]["hook"][$3]  = 1  ; next }
$1 == "INPM" { m[$2]["m"][$3]     = $4 ; next }
$1 == "INPP" { m[$2]["p"][$3]     = $4 ; next }
$1 == "INPA" { m[$2]["a"][$3]     = $4 ; next }
$1 == "INPF" { m[$2]["f"][$3]     = $4 ; next }
$1 == "NARG" { m[$2]["narg"]      = $3 ; next }

$1 == "FARG" {
    if($5 != "") { arg = $4 " BIND " $5 } else { arg = $4 }
    m[$2]["arg"][$3] = arg
    next
}

END{

    for(i in m){

        if(m[i]["narg"]){
            printf "m4_define(<[NARG_%s]>, %s)\n", i, m[i]["narg"] >> rules
        } else {
            printf "m4_define(<[NARG_%s]>, 0)\n", i >> rules
        }

        if(m[i]["lang"] == lang){
            printf "m4_define(<[MANIFOLD_%s]>, NATIVE_MANIFOLD(%s)) \n", i, i >> rules
        } else if(m[i]["lang"] == "*"){
            printf "m4_define(<[MANIFOLD_%s]>, UNIVERSAL_MANIFOLD(%s)) \n", i, i >> rules
        } else {
            printf "m4_define(<[MANIFOLD_%s]>, FOREIGN_MANIFOLD(%s,%s)) \n", i, m[i]["lang"], i >> rules
        }

        if(m[i]["cache"]){
            cache = m[i]["cache"]
            printf "m4_define(<[BASECACHE_%s]>, %s)\n", i, cache >> rules
            printf "m4_define(<[CACHE_%s]>, DO_CACHE(%s))\n", i, i >> rules
            printf "m4_define(<[CACHE_PUT_%s]>, DO_PUT(%s))\n", i, i >> rules
        } else {
            printf "m4_define(<[CACHE_%s]>, NO_CACHE(%s))\n", i, i >> rules
            printf "m4_define(<[CACHE_PUT_%s]>, NO_PUT(%s))\n", i, i >> rules
        }

        if(length(m[i]["check"]) > 0){
            printf "m4_define(<[VALIDATE_%s]>, DO_VALIDATE(%s)) \n", i, i >> rules
            check=""
            for(k in m[i]["check"]){
                check = sprintf("%s AND CHECK(%s, %s)\n", check, k, i)
            }
            gsub(/^ AND /, "", check) # remove the last sep
            printf "m4_define(<[CHECK_%s]>, %s) \n", i, check >> rules
        } else {
            printf "m4_define(<[VALIDATE_%s]>, NO_VALIDATE(%s)) \n", i, i >> rules
        }

        if( "m" in m[i] || "p" in m[i] || "a" in m[i] || "f" in m[i]){
            k=0
            input=""
            while(1) {
                if(m[i]["m"][k]){
                    input = sprintf("%s SEP <[CALL(%s, %s)]>", input, m[i]["m"][k], i)
                }
                else if(m[i]["p"][k]){
                    input = sprintf("%s SEP %s", input, m[i]["p"][k])
                }
                else if(m[i]["f"][k]){
                    wrappings[m[i]["f"][k]] = 1
                    input = sprintf("%s SEP <[UID_WRAP(%s)]>", input, m[i]["f"][k])
                }
                else if(m[i]["a"][k]){
                    input = sprintf("%s SEP <[NTH_ARG(%s)]>", input, m[i]["a"][k])
                }
                else {
                    break
                }
                k = k + 1
            }
            gsub(/^ SEP /, "", input) # remove the last sep
            printf "m4_define(<[INPUT_%s]>, <[XXLEFT %s XXRIGHT]>)\n", i, input >> rules
        } else {
            printf "m4_define(<[INPUT_%s]>, <[]>)\n", i >> rules
        }

        if(length(m[i]["arg"]) > 0){
            arg=""
            for(k=0; k<length(m[i]["arg"]); k++){
                arg = sprintf("%s SEP %s", arg, m[i]["arg"][k])
            }
            gsub(/^ SEP /, "", arg) # remove the initial sep
            printf "m4_define(<[ARG_%s]>, <[XXLEFT %s XXRIGHT]>)\n", i, arg >> rules
        } else {
            printf "m4_define(<[ARG_%s]>, <[]>)\n", i >> rules
        }

        if(length(m[i]["efct"]) > 0){
            effect=""
            for(k in m[i]["efct"]){
                effect = sprintf("%s EFFECT(%s,%s) \n", effect, k, i)
            }
            printf "m4_define(<[EFFECT_%s]>, %s)\n", i, effect >> rules
        } else {
            printf "m4_define(<[EFFECT_%s]>, <[]>)\n", i >> rules
        }

        if(length(m[i]["hook"]) > 0){
            hook=""
            for(k in m[i]["hook"]){
                hook = sprintf("%s HOOK(%s, %s) ", hook, k, i)
            }
            printf "m4_define(<[HOOK_%s]>, %s) \n", i, hook >> rules
        } else {
            printf "m4_define(<[HOOK_%s]>, <[]>) \n", i >> rules
        }

        if(m[i]["func"]){
            printf "m4_define(<[FUNC_%s]>, %s)\n", i, m[i]["func"] >> rules
        } else {
            printf "m4_define(<[FUNC_%s]>, NOTHING)\n", i >> rules
        }

        if(m[i]["pass"]){
            printf "m4_define(<[PASS_%s]>, %s)\n", i, m[i]["pass"] 
            printf "m4_define(<[RUN_%s]>, DO_PASS(%s))\n", i, i >> rules
        } else {
            printf "m4_define(<[RUN_%s]>, NO_PASS(%s))\n", i, i >> rules
        }

        if(m[i]["open"]){
            print "WARNING: `open` is not yet supported" >> "/dev/stderr"
            # printf "m4_define(<[OPEN_%s]>, OPEN(%s)) ", i, i, m[i]["open"] >> rules
        } else {
            # printf "m4_define(<[OPEN_%s]> <[]>) ", i >> rules
        }

        if(m[i]["fail"]){
            printf "m4_define(<[FAIL_%s]>, %s)\n", i, m[i]["fail"] >> rules
        } else {
            printf "m4_define(<[FAIL_%s]>, SIMPLE_FAIL)\n", i >> rules
        }

        if(m[i]["pack"]){
            printf "m4_define(<[PACKFUN_%s]>, %s)\n", m[i]["pack"] >> rules
            printf "m4_define(<[PACK_%s]>, DO_PACK(%s))\n", i >> rules
        } else {
            printf "m4_define(<[PACK_%s]>, NO_PACK)\n", i >> rules
        }

    }

    printf "PROLOGUE " >> body
    printf "m4_include(%s)\n", src >> body

    for(i in m){
        if(i in wrappings){
            printf sprintf("MAKE_UID(%s)\n", i) >> body
        }
        printf "MANIFOLD_%s \n", i >> body
    }

    printf "EPILOGUE\n" >> body

}
