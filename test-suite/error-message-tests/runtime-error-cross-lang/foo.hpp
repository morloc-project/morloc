#include <stdexcept>
#include <string>

std::string cpp_fail(std::string x){
    throw std::runtime_error("CPPERR: cross-lang failure in cpp");
    return x;
}

std::string cpp_id(std::string x){
    return x;
}
