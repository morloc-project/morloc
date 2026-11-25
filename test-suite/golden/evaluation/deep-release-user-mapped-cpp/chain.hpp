#pragma once
// The layout sum-types.md documents for a user-mapped data type.
#include <memory>
#include <variant>

struct Chain_End;
struct Chain_Link;
struct Chain {
  std::variant<std::shared_ptr<const Chain_End>, std::shared_ptr<const Chain_Link>> v;
};
struct Chain_End {};
struct Chain_Link { int f0; Chain f1; };

inline int chainLength(const Chain& c) {
  int len = 0;
  for (const Chain* p = &c; auto l = std::get_if<std::shared_ptr<const Chain_Link>>(&p->v); p = &(*l)->f1) len++;
  return len;
}
