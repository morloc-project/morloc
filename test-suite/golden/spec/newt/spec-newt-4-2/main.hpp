#pragma once
#include <tuple>
struct Pt { int x; int y; };
inline int ptSum(const Pt& p) { return p.x * 10 + p.y; }
inline Pt packPt(const std::tuple<int, int>& t) { return Pt{std::get<0>(t), std::get<1>(t)}; }
inline std::tuple<int, int> unpackPt(const Pt& p) { return std::make_tuple(p.x, p.y); }
