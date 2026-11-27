#pragma once
using Handle = int*;
inline Handle mkH(int n) { static int v[8]; return v + n; }
inline int useH(const Handle& h) { return (int)(h != nullptr); }
