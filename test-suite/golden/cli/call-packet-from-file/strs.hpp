#pragma once
#include <string>
#include <vector>
inline std::vector<std::string> bad(int) { return {std::string("ok"), std::string("a\xff" "b")}; }
inline std::vector<std::string> good(int) { return {std::string("ok"), std::string("ab")}; }
inline int count(const std::vector<std::string>& xs) { return (int)xs.size(); }
