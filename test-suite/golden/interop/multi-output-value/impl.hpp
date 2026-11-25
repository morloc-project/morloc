#ifndef __MULTI_OUTPUT_VALUE_HPP__
#define __MULTI_OUTPUT_VALUE_HPP__

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

inline std::vector<int> build(const std::string& log, int n) {
    std::ofstream(log, std::ios::app) << "ran\n";
    std::cout << "building " << n << std::endl;
    std::vector<int> out;
    for (int i = 0; i < n; i++) out.push_back(i * i);
    return out;
}

inline std::string asLines(const std::vector<int>& xs) {
    std::string out;
    for (int x : xs) out += std::to_string(x) + "\n";
    return out;
}

inline std::string pad(int width, const std::vector<int>& xs) {
    std::string out;
    for (int x : xs) {
        std::string s = std::to_string(x);
        while ((int)s.size() < width) s = " " + s;
        out += s + "\n";
    }
    return out;
}

inline void sinkLines(const std::vector<int>& xs) {
    for (int x : xs) std::cout << "sink " << x << "\n";
    std::cout.flush();
}

inline int summary(const std::vector<int>& xs) {
    return (int)xs.size();
}

#endif
