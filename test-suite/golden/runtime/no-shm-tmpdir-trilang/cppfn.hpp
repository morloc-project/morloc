#pragma once
#include <string>
#include <vector>

inline std::string cpp_id_str(const std::string& x) {
    return x;
}

inline std::vector<int> cpp_id_list(const std::vector<int>& x) {
    return x;
}

inline std::vector<std::vector<int>> cpp_id_nested(const std::vector<std::vector<int>>& x) {
    return x;
}
