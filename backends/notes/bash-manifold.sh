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
