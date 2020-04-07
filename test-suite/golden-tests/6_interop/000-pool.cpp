#include <string>
#include <iostream>
#include <functional>
#include "core.hpp"
#include "serial.hpp"
#include "interop.hpp"
#include "cpp_math.hpp"
#include "gsl.h"

/* sqrt :: Num -> Num */
double m1(std::string n);
/*  */
double m3(std::string n);
/* fibonacci :: Num -> List Num */
std::vector<double> m5(std::string n);

/* From B
CallS sqrt <CppLang>
sqrt :: Num -> Num
sqrt CppLang :: double@CppLang -> double@CppLang */
double m1(std::string n)
{
    double t1;
    std::string x0 = foreign_call("Rscript pool.R 3 " +  n);
    double x1 = unpack(x0, t1); 
    return morloc_sqrt(x1);
}

/* From B
CallS fibonacci <CppLang>
fibonacci :: Num -> List Num
fibonacci CppLang :: size_t@CppLang -> std::vector<$1>@CppLang double@CppLang */
std::vector<double> m5(std::string n)
{
    size_t t0;
    size_t x0 = unpack(n, t0);
    return fibonacci(x0);
}

int main(int argc, char * argv[])
{
    int cmdID;
    std::string result;
    cmdID = std::stoi(argv[1]);
    double t1;
std::vector<double> t5;
switch(cmdID)
{
    case 1:
        result = pack(m1(argv[2]), t1);
        break;
    case 5:
        result = pack(m5(argv[2]), t5);
        break;
}
    std::cout << result << std::endl;
    return 0;
}
