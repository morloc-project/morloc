#ifndef __SPEC_CPP_3_1_VIEW__
#define __SPEC_CPP_3_1_VIEW__

#include <string_view>
using Name = std::string_view;
inline Name mkName(int n) { static const char s[] = "abcdefgh"; return Name(s, n); }
inline int nameLen(const Name& x) { return (int)x.size(); }

#endif
