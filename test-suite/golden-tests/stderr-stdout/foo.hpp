#include <iostream>
#include <string>
#include "mlccpptypes/prelude.hpp"

mlc::Unit cfoo(const std::string& name) {
    std::cerr << "STDERR Hello " << name << std::endl;
    std::cout << "STDOUT Hello " << name << std::endl;
    return mlc::_Unit;
}
