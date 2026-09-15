#pragma once
#include <tuple>
#include "mlccpptypes/prelude.hpp"

std::tuple<mlc::Unit, bool> mk_ub(bool b) { return {mlc::Unit{}, b}; }
std::tuple<mlc::Unit, int64_t> mk_ui(int64_t n) { return {mlc::Unit{}, n}; }
std::tuple<int64_t, mlc::Unit> mk_iu(int64_t n) { return {n, mlc::Unit{}}; }
std::tuple<mlc::Unit, bool> flip_ub(std::tuple<mlc::Unit, bool> t) { return {mlc::Unit{}, !std::get<1>(t)}; }
