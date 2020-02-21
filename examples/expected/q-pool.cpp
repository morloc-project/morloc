#include <string>
#include <iostream>
#include <functional>

#include "core.hpp"
#include "serializers.hpp"
#include "gsl.h"
#include "cpp_math.hpp"
#include "optimization.hpp"
#include "cpp_math.hpp"
#include "optimization.hpp"

double m0(std::string a, std::string b, std::string c, std::string x);
double m1(std::string a, std::string b, std::string c, std::string x);
double m2(std::string a, std::string b, std::string c, std::string x);
double m3(std::string a, std::string b, std::string c, std::string x);
double m4(std::string a, std::string b, std::string c, std::string x);
double m5( std::string a
, std::string b
, std::string c
, std::string rbnd
, std::string lbnd );
double m6(std::string a, std::string b, std::string c, double x);

/* cis manifold */
/* quadraticEq */
/* add :: (Num, Num -> Num) */
double m0(std::string a, std::string b, std::string c, std::string x){
    double a0 = m1(a, b, c, x);
    double a1 = m3(a, b, c, x);
    return add(a0, a1);
}
/* cis manifold */
/* mul */
/* mul :: (Num, Num -> Num) */
double m1(std::string a, std::string b, std::string c, std::string x){
    double a0 = unpackDouble(a);
    double a1 = m2(a, b, c, x);
    return mul(a0, a1);
}
/* cis manifold */
/* mul */
/* mul :: (Num, Num -> Num) */
double m2(std::string a, std::string b, std::string c, std::string x){
    double a0 = unpackDouble(x);
    double a1 = unpackDouble(x);
    return mul(a0, a1);
}
/* cis manifold */
/* add */
/* add :: (Num, Num -> Num) */
double m3(std::string a, std::string b, std::string c, std::string x){
    double a0 = m4(a, b, c, x);
    double a1 = unpackDouble(c);
    return add(a0, a1);
}
/* cis manifold */
/* mul */
/* mul :: (Num, Num -> Num) */
double m4(std::string a, std::string b, std::string c, std::string x){
    double a0 = unpackDouble(b);
    double a1 = unpackDouble(x);
    return mul(a0, a1);
}
/* cis manifold */
/* optimalQuadratic */
/* optimize1D :: ((Num -> Num), Num, Num, Num -> Num) */
double m5( std::string a
, std::string b
, std::string c
, std::string lbnd
, std::string rbnd ){
    auto a0 = std::bind(m6, a, b, c, std::placeholders::_1);
    double a1 = unpackDouble(lbnd);
    double a2 = unpackDouble(rbnd);
    return optimize1D(a0, a1, a2);
}
/* passed function*/
// FIXME: this is inefficient since a/b/c will have to be deserialized many
// times. If all downstream functions are in the same language, the
// deserialization step should be done in the std::bind call. For now, the
// current behavior is OK.
double m6(std::string a, std::string b, std::string c, double x){
    double a0 = m1(a, b, c, packDouble(x));
    double a1 = m3(a, b, c, packDouble(x));
    return add(a0, a1);
}

int main(int argc, char * argv[]){
  int cmdID;
  std::string result;
  cmdID = std::stoi(argv[1]);
  switch(cmdID){
    case 0:
      result = packDouble(m0(argv[2], argv[3], argv[4], argv[5]));
      break;
    case 5:
      result = packDouble(m5(argv[2], argv[3], argv[4], argv[5], argv[6]));
      break;
    default:
      std::cerr << "Unexpected argument passed to C++ pool" << std::endl;
      return 1; 
  }
  std::cout << result << std::endl;
  return 0;
}
