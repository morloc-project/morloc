m1 <- function (){
    if(cache_chk("m1")){
        cache_get("m1")
    }
    else{
        if( check1() && check2() ){
            b <- pass(fun, open1(in1), open2(in2), arg1, arg2)
            effect1(b)
            effect2(b)
            Mb <- pack(b)
            cache_put("m1")
        } else {
            Mb = fail()
            cache_put("m1")
        }
        hook1()
        hook2()
    }
    return Mb
}

# version 2
m1 <- function (){
    begin1()
    begin2()
    if(cache_chk("m1")){
        b <- cache_get("m1")
    }
    else{
        prehook1()
        prehook2()
        if( check1() && check2() ){
            pass_prehook1()
            pass_prehook2()
            b <- fun(in1(), in2(), arg1, arg2)
            cache_put("m1", b)
            pass_posthook()
            pass_posthook()
        } else {
            fail_prehook()
            fail_prehook()
            b <- fail()
            cache_put("m1", b)
            fail_posthook()
            fail_posthook()
        }
        posthook1()
        posthook2()
    }
    end1()
    end2()
    return b
}


# numbered effects
m1 <- function (){
    # @0
    if(cache_chk("m1")){
        # @8
        b = cache_get("m1")
        # @9
    }
    else{
        # @2
        if( [check] ){
            # @4
            b = fun([input], [arg])
            cache_put("m1", b)
            # @5
        } else {
            # @6
            b = fail()
            cache_put("m1", b)
            # @7
        }
        # @3
    }
    # @1
    return b
}
