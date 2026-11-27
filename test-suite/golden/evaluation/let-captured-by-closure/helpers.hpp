#include <string>

inline int cpp_bump(const std::string& s, int x) { return static_cast<int>(s.size()) + x; }
