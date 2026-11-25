#pragma once
#include <stdexcept>

inline double crashy_cpp(double x) {
    throw std::runtime_error("INTENTIONAL_TEST_FAILURE");
}
