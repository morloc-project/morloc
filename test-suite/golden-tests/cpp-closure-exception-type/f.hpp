#pragma once
#include <stdexcept>
#include <string>

int boom(int x) { throw std::out_of_range("boom " + std::to_string(x)); }

int cboom(int) { throw "cboom"; }

template <typename F>
std::string catchRange(F f) {
    try {
        f(0);
        return "returned";
    } catch (const std::out_of_range& e) {
        return std::string("caught out_of_range: ") + e.what();
    } catch (...) {
        return "caught another type";
    }
}

template <typename F>
int callIt(F f) { return f(0); }
