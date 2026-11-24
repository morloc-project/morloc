#pragma once
struct r_t { int* h; };
inline r_t mkR(int k) { static int v[8]; return r_t{v + k}; }
inline int useR(const r_t& r) { return (int)(r.h != nullptr); }
