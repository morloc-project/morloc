#ifndef MORLOC_OPTIONAL_RECORDS_HPP
#define MORLOC_OPTIONAL_RECORDS_HPP

#include <optional>
#include <string>
#include <vector>

struct person_t {
    std::string name;
    std::optional<int> age;
};

person_t makePerson(const std::string& name, const std::optional<int>& age) {
    return person_t{name, age};
}

std::string getName(const person_t& p) {
    return p.name;
}

std::optional<int> getAge(const person_t& p) {
    return p.age;
}

template <typename T>
std::optional<T> toNull(const T& x) {
    return std::optional<T>(x);
}

std::optional<person_t> findPerson(const std::string& name, const std::vector<person_t>& people) {
    for (const auto& p : people) {
        if (p.name == name) return std::optional<person_t>(p);
    }
    return std::nullopt;
}

#endif
