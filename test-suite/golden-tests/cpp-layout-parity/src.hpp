#ifndef __SRC_HPP__
#define __SRC_HPP__

#include "cppmorloc.hpp"
#include <string>
#include <vector>

// Every schema on which the C++ pool's layout rules disagree with the
// runtime's: the element alignment and an array's data alignment.
inline std::vector<std::string> layoutMismatches(int) {
    const char* schemas[] = {
        "b", "u1", "i2", "i4", "f4", "i8", "f8", "j", "s", "z",
        "?b", "?i4", "?f8", "?s",
        "e22E02E1",
        "m11ab", "m11au1", "m21ab1bu1", "m11ai8",
        "t2bb", "t2b?b", "t2si4",
        "v21A1b1B0", "v21A2b?b1B0",
        "ab", "a?i4", "at2b?b",
        "F", "T",
    };
    std::vector<std::string> out;
    for (const char* s : schemas) {
        char* err = nullptr;
        Schema* schema = parse_schema(s, &err);
        if (err) { out.push_back(std::string(s) + ": " + err); free(err); continue; }
        if (schema_alignment_cpp(schema) != schema_alignment(schema)) {
            out.push_back(std::string(s) + " alignment");
        }
        if (array_data_alignment_cpp(schema) != array_data_alignment(schema)) {
            out.push_back(std::string(s) + " array data alignment");
        }
        free_schema(schema);
    }
    return out;
}

#endif
