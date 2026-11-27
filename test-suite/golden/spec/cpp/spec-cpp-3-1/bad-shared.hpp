#ifndef __SPEC_CPP_3_1_SHARED__
#define __SPEC_CPP_3_1_SHARED__

#include <memory>
using Shared = std::shared_ptr<int>;
inline Shared mkShared(int n) { return std::make_shared<int>(n); }
inline int sharedAt(const Shared& p) { return *p; }

#endif
