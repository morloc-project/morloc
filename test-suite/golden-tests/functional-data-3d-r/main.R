bar <- function(cond1, cond2){
    if(cond1){
        function(x){ 2*x }
    } else {
        function(x){ 3*x + cond2 }
    }
}

baz <- function(cond1){
    function(num){
        function(cond2){
            cond1 + num + 3*cond2
        }
    }
}
