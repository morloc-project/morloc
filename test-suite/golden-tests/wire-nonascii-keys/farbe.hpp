#ifndef MORLOC_WIRE_NONASCII_KEYS_HPP
#define MORLOC_WIRE_NONASCII_KEYS_HPP

#include <string>

struct farbe_t {
    int grün;
    std::string 名前;
};

template <class T>
inline T same(T x) { return x; }

#endif
