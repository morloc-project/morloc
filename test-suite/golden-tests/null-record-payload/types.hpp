#include <optional>
#include <string>
struct PInner { std::string ann; int x; };
struct POuter { std::string ann; std::optional<PInner> inner; };
template <class C> struct Inner { C ann; int x; };
template <class T, class C> struct Outer { T ann; std::optional<Inner<C>> inner; };
#include <vector>
template <class T, class C>
int count_inner(const std::vector<Outer<T, C>>& xs) { int n = 0; for (auto& o : xs) n += o.inner ? 1 : 0; return n; }
inline int count_pinner(const std::vector<POuter>& xs) { int n = 0; for (auto& o : xs) n += o.inner ? 1 : 0; return n; }
struct PNested { std::string ann; std::optional<std::optional<PInner>> inner; };
inline int count_nested(const std::vector<PNested>& xs) { int n = 0; for (auto& o : xs) n += o.inner ? 1 : 0; return n; }
inline std::string nest_state(const std::optional<std::optional<int>>& x) { return x ? "present" : "absent"; }
