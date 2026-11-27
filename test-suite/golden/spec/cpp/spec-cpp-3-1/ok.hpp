#ifndef __SPEC_CPP_3_1_OK__
#define __SPEC_CPP_3_1_OK__

#include <memory>
#include <string>

using Name = std::string;
inline Name mkName(int n) { return Name(n, 'a'); }
inline int nameLen(const Name& x) { return (int)x.size(); }

using Shared = std::shared_ptr<const int>;
inline Shared mkShared(int n) { return std::make_shared<const int>(n); }
inline int sharedAt(const Shared& p) { return *p; }

struct doc_t { std::string name; int k; };
inline int docLen(const doc_t& d) { return (int)d.name.size() + d.k; }
inline std::string mkStr(int n) { return std::string(n, 'x'); }

#endif
