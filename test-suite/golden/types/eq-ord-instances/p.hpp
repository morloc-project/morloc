#ifndef __EQ_ORD_P_HPP__
#define __EQ_ORD_P_HPP__
#include <string>
#include <cstdint>
struct P {
    int64_t a;
    std::string b;
};

P p_id(const P& x) { return x; }
#endif
