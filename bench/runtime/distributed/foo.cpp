#include <vector>

double cmean(std::vector<double> xs){
    if(xs.size() == 0){
        return 0;
    }
    double total = 0;
    for(size_t i = 0; i < xs.size(); i++){
        total += xs[i];
    }
    return total / xs.size();
}

double cdouble(double x){
    return x * 2;
}
