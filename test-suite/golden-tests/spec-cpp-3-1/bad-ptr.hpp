#ifndef __SPEC_CPP_3_1_PTR__
#define __SPEC_CPP_3_1_PTR__

#include <cstdio>
using Handle = int*;
inline Handle mkHandle(int n) { static int v[8]; return v + n; }
inline int handleAt(const Handle& h) { return (int)(h != nullptr); }

#endif
