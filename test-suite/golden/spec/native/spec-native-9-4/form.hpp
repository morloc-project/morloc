#pragma once
#include <string>

struct P {
    int px;
    double py;
};

#include <set>
struct Q {
    std::set<int> qs;
    int qn;
};

// Both arguments have one static type, which is their native form.
template <class T>
std::string same(const std::string& name, const T& a, const T& b) {
    (void)a; (void)b;
    return name + " ok";
}

template <class T>
T echo(const T& x) { return x; }
