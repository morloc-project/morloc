#ifndef __SPEC_CPP_3_1_FNONLY__
#define __SPEC_CPP_3_1_FNONLY__

#include <functional>
#include <string_view>

using Name = std::string_view;
inline int withName(std::function<int(Name)> f) { return f(Name("abc")); }
inline int nameLen(Name x) { return (int)x.size(); }

#endif
