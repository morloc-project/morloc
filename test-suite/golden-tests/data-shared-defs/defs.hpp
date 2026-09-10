#pragma once
#include <memory>
#include <string>
#include <variant>
#include <cstdint>

// A sourced C++ header is included BEFORE the compiler's own declarations, so
// it cannot name a generated type. Everything C++ speaks is therefore declared
// here and mapped in `types.loc`. Rust has no such ordering constraint and
// uses the generated forms for the same three types.
//
// Each arm lives behind a shared_ptr inside the variant, which is morloc's
// convention for a payload-bearing `data`.
struct Action_Mkdir;
struct Action_Rename;
struct Action_Chmod;
struct Action_Noop;

struct Action {
    std::variant<std::shared_ptr<Action_Mkdir>,
                 std::shared_ptr<Action_Rename>,
                 std::shared_ptr<Action_Chmod>,
                 std::shared_ptr<Action_Noop>> v;
};

struct Action_Mkdir  { std::string f0; };
struct Action_Rename { std::string f0; std::string f1; };
struct Action_Chmod  { std::string f0; int64_t f1; };
struct Action_Noop   { };

enum class Level : uint8_t { Info = 0, Warn = 1, Fail = 2 };

struct Step { int64_t n; Action action; };

struct Plan { std::string label; Action first; Level level; };

inline std::string cpp_describe(const Action& a) {
    if (auto p = std::get_if<std::shared_ptr<Action_Mkdir>>(&a.v))
        return "mkdir " + (*p)->f0;
    if (auto p = std::get_if<std::shared_ptr<Action_Rename>>(&a.v))
        return "rename " + (*p)->f0 + " " + (*p)->f1;
    if (auto p = std::get_if<std::shared_ptr<Action_Chmod>>(&a.v))
        return "chmod " + (*p)->f0 + " " + std::to_string((*p)->f1);
    return "noop";
}

inline std::string cpp_step_desc(const Step& s) {
    return std::to_string(s.n) + ":" + cpp_describe(s.action);
}

inline std::string cpp_plan_desc(const Plan& p) {
    std::string lv = p.level == Level::Info ? "info"
                   : (p.level == Level::Warn ? "warn" : "fail");
    return p.label + "/" + lv + "/" + cpp_describe(p.first);
}
