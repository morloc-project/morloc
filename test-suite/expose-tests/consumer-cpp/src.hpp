#ifndef CONSUMER_CPP_SRC_HPP
#define CONSUMER_CPP_SRC_HPP

#include "morloc-test-expose-cpp/foo.hpp"

inline int compute(int x) {
    return mlc_test::foo(x);
}

#endif
