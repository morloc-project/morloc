#include <string>
#include <vector>

static std::string join_cpp(const std::vector<std::string>& xs) {
    std::string out;
    for (size_t i = 0; i < xs.size(); i++) {
        if (i) out += ",";
        out += xs[i];
    }
    return out;
}

std::string show2_cpp(const std::vector<std::string>& a, const std::vector<std::string>& b) {
    return join_cpp(a) + "|" + join_cpp(b);
}

std::string tmp_cpp(const std::string& x) { return x; }
