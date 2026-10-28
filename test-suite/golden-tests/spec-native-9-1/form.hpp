#pragma once
#include <string>

struct P {
    int px;
    double py;
};

// Both arguments have one static type, which is their native form.
template <class T>
std::string same(const std::string& name, const T& a, const T& b) {
    (void)a; (void)b;
    return name + " ok";
}

template <class T>
T echo(const T& x) { return x; }
