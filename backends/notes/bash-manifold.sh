# version 1
m1 (){
    if cache_chk "m1"
    then
        cache_get "m1"
    else
        if check1 && check2
        then
            run fun args <(m4 | o1) <(m5 | o2) |
            tee >(effect1 | null) |
            tee >(effect2 | null) |
            pack | cache_put "m1"
        else
            fail | cache_put "m1"
        fi
        hook1 | null 
        hook2 | null 
    fi
}

# version 2
m1_tmp=$(gentmp)
m1 (){
    begin1 | stderr
    begin2 | stderr
    if cache_chk "m1"
    then
        cat $(cache_get "m1")
    else
        prehook1 | stderr 
        prehook2 | stderr
        if check1 && check2
        then
            fun args <(m4) <(m5) > $m1_tmp
            pass_effect1 | stderr
            pass_effect2 | stderr
        else
            fail_output2 > $m1_tmp
            fail_effect1 | stderr
            fail_effect2 | stderr
        fi
        cache_put "m1" <(cat $m1_tmp) | stderr
        posthook1 | stderr 
        posthook2 | stderr 
        cat $m1_tmp
        rm $m1_tmp
    fi
    end1 | stderr
    end2 | stderr
}
