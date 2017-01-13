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
