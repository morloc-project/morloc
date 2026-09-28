#ifndef __INLINE_PAYLOAD_BOUNDS_PROBE_HPP__
#define __INLINE_PAYLOAD_BOUNDS_PROBE_HPP__

#include <cstring>
#include <string>
#include <vector>
#include "cppmorloc.hpp"

std::string probe(int64_t size, int64_t data) {
    char* err = nullptr;
    Schema* schema = parse_schema("au1", &err);
    std::vector<uint8_t> value = {1, 2, 3};
    void* voidstar = to_voidstar(schema, value);
    uint8_t* made = make_inline_data_packet(voidstar, schema, &err);
    const morloc_packet_header_t* h = (const morloc_packet_header_t*)made;
    std::vector<uint8_t> p(made, made + sizeof(morloc_packet_header_t) + h->offset + h->length);
    free(made);
    if (p[13] != 0) return "not inline";
    size_t base = sizeof(morloc_packet_header_t) + h->offset;
    if (size >= 0) { uint64_t v = (uint64_t)size; std::memcpy(&p[base], &v, 8); }
    if (data >= 0) { int64_t v = data; std::memcpy(&p[base + 8], &v, 8); }
    try {
        auto out = mlc_read_inline_packet<std::vector<uint8_t>>(p.data(), schema);
        return "ok " + std::to_string(out.size());
    } catch (const std::exception&) {
        return "refused";
    }
}

#endif
