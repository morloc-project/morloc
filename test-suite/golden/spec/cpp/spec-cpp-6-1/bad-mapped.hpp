#ifndef __SPEC_CPP_6_1_BAD_MAPPED_HPP__
#define __SPEC_CPP_6_1_BAD_MAPPED_HPP__

#include <memory>
#include <variant>
#include <vector>

inline std::vector<int> mk(int n) { return std::vector<int>(n, 1); }
struct Pt_P;
struct Pt { std::variant<std::shared_ptr<Pt_P>> v; };
struct Pt_P { std::vector<int> f0; };
inline int ptSize(const Pt& p) { return (int)std::get<0>(p.v)->f0.size(); }

#endif
