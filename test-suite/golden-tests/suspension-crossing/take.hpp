#pragma once
#include <functional>
#include <iostream>

int takeThunk(std::function<int()> t) {
    std::cerr << "takeThunk received a callable" << std::endl;
    return t() + t();
}

int cppDec(int n) { return n - 1; }
bool cppDone(int n) { return n <= 0; }

#include <string>
#include <optional>
struct Job { std::function<int()> run; std::string name; };
int runJobTwice(const Job& j) { return j.run() + j.run(); }
int runOpt(std::optional<std::function<int()>> t) { return t ? (*t)() : -1; }
