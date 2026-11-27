#ifndef __SPEC_CPP_3_1_FIELD__
#define __SPEC_CPP_3_1_FIELD__

#include <string>
#include <string_view>
struct doc_t { std::string_view name; int k; };
inline int docLen(const doc_t& d) { return (int)d.name.size() + d.k; }
inline std::string mkStr(int n) { return std::string(n, 'x'); }

#endif
