#include <iostream>
#include "mlc/types/_local/main.hpp"
using namespace mlc::types::_local::main;
int main() {
  P p{3, "a"};
  Shape s{std::make_shared<const Shape_Rect>(Shape_Rect{2.0, 3.0})};
  Chain c{{1, Chain{{2, std::nullopt}}}};
  int n = 0;
  for (const std::optional<Chain>* q = &c->next; *q; q = &(**q)->next) n++;
  std::cout << p.x << " " << p.name << " " << std::get<1>(s.v)->f0 * std::get<1>(s.v)->f1 << " " << n << std::endl;
}
