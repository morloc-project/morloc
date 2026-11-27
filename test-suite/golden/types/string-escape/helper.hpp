#ifndef __HELPER_HPP__
#define __HELPER_HPP__

#include <string>

std::string identity(std::string x) {
    return x;
}

int slen(std::string x) {
    return static_cast<int>(x.size());
}

#endif
