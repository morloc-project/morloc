#pragma once
#include "mlc/types/_local/main.hpp"
int personAge(const mlc::types::_local::main::Person& p) { return p.age; }
double shapeArea(const mlc::types::_local::main::Shape& s) {
  using namespace mlc::types::_local::main;
  if (auto c = std::get_if<std::shared_ptr<const Shape_Circle>>(&s.v)) return 3.0 * (*c)->f0 * (*c)->f0;
  auto r = std::get<std::shared_ptr<const Shape_Rect>>(s.v);
  return r->f0 * r->f1;
}
