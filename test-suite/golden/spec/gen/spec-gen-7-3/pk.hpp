#pragma once
#include <string>
#include <nlohmann/json.hpp>
inline nlohmann::json parseJson(const std::string& s) { return nlohmann::json::parse(s); }
inline std::string dumpJson(const nlohmann::json& j) { return j.dump(); }
