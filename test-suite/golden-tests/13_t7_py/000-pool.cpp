#include <string>
#include <iostream>
#include <functional>
#include "serializers.hpp"
#include "cpp_math.hpp"

/* sum :: List Num -> Num */
double m1(std::string x);
/* sqrt :: Num -> Num */
double m7(std::string x);

/* From B
CallS sum <CppLang>
sum :: List Num -> Num
sum CppLang :: std::vector<$1>@CppLang double@CppLang -> double@CppLang */
double m1(std::string x)
{
    std::vector<double> x0 = {1.0, 2.0, 3.0, m7(x)};
    return sum(x0);
}

/* From B
CallS sqrt <CppLang>
sqrt :: Num -> Num
sqrt CppLang :: double@CppLang -> double@CppLang */
double m7(std::string x)
{
    double x0 = unpackDouble(x);
    return morloc_sqrt(x0);
}

int main(int argc, char * argv[])
{
    int cmdID;
    std::string result;
    cmdID = std::stoi(argv[1]);
    switch(cmdID)
    {
        case 1:
            result = packDouble(m1(argv[2]));
            break;
    }
    std::cout << result << std::endl;
    return 0;
}
