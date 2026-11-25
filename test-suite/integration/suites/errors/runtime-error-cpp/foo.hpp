#include <stdexcept>

int cpp_fail(int x){
    throw std::runtime_error("CPPERR: something went wrong in cpp");
    return x;
}
