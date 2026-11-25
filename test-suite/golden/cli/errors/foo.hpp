template <typename T>
T cid(T x){
    return x;
}

double cdiv(double x){
    if(x == 0){
       throw std::runtime_error("Cannot divide by zero"); 
    } else {
        return 1 / x;
    }
}
