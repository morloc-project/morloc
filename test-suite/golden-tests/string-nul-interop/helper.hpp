#ifndef HELPER_HPP
#define HELPER_HPP

#include <string>

inline std::string make_nul() {
    // Build "abc\0def\0!" with explicit length so the NUL bytes survive.
    return std::string("abc\000def\000!", 9);
}

inline std::string echo(std::string s) { return s; }

inline int slen(std::string s) { return static_cast<int>(s.size()); }

#endif
