#ifndef __SPEC_CPP_3_1_WEAK__
#define __SPEC_CPP_3_1_WEAK__

#include <memory>
using Weak = std::weak_ptr<const int>;
inline Weak mkWeak(int) { return Weak(); }
inline int weakOk(const Weak& w) { return (int)w.expired(); }

#endif
