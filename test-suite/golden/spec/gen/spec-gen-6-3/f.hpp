#pragma once
#include "mlc/types/_local/main.hpp"
namespace t = mlc::types::_local::main;
inline int fb(const t::B& b) { return b.y; }
inline int fe(t::E e) { return e == t::E::Green ? 1 : 0; }
inline int fbox(const t::Wrap<int>&) { return 0; }
inline int fv(const t::V&) { return 0; }
inline int ft(const t::T& x) { return x->v; }
inline int fh(const t::H& h) { return h.v.v.index(); }
inline int fn_(const t::N& n) { return n->v; }
inline int fa(const t::A& a) { return a.x + fb(t::B{2}) + fe(t::E::Green) + (t::B{2} == t::B{2} ? 0 : 100); }
