#ifndef __FOO_HPP__
#define __FOO_HPP__

#include <string>
#include <vector>
#include <functional>

int addInt(int x, int y){
  return (x + y);
}

double addReal(double x, double y){
  return (x + y);
}

std::string addStr(std::string x, std::string y){
  return (x + y);
}

template <class A, class B>
B fold(std::function<B(B,A)> f, B y, std::vector<A> xs){
    for(std::size_t i=0; i < xs.size(); i++){
        y = f(y, xs[i]);
    }
    return y;
}

#endif
