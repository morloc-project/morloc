m1_tmp=$(gentmp)
m1 (){
    # @0
    if cache_chk "m1"
    then
        # @8
        cat $(cache_get "m1")
        # @9
    else
        # @2
        if check1 && check2
        then
            # @4
            fun args <(m4) <(m5) > $m1_tmp
            # @5
        else
            # @6
            fail_output2 > $m1_tmp
            # @7
        fi
        cache_put "m1" <(cat $m1_tmp)
        # @3
        cat $m1_tmp
        rm $m1_tmp
    fi
    # @1
}


# stderr hook 
