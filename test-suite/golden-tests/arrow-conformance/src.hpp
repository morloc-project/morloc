#ifndef ARROW_CONFORMANCE_SRC_HPP
#define ARROW_CONFORMANCE_SRC_HPP

// Canonical Arrow table fixture and describer, C++ side. Mirrors src.py:
// same columns, same values, same ASCII description, built and read with
// nanoarrow so no Arrow C++ library is needed.

#include "mlc_arrow.hpp"
#include <nanoarrow/nanoarrow.h>

#include <cstdint>
#include <cstdio>
#include <cstring>
#include <stdexcept>
#include <string>
#include <vector>

namespace conformance {

inline void ck(int rc, const char* what) {
    if (rc != NANOARROW_OK) {
        throw std::runtime_error(std::string("nanoarrow: ") + what + " failed (" + std::to_string(rc) + ")");
    }
}

// One column of the fixture: a type and five optional values. Null is the
// slot whose `present` flag is false.
struct Cell {
    bool present;
    int64_t i;
    uint64_t u;
    double d;
    std::string bytes;            // utf8 / large_utf8 / binary payload
    std::vector<int64_t> items;   // list<int64> payload
};

inline Cell null_cell() { return Cell{false, 0, 0, 0.0, "", {}}; }
inline Cell ci(int64_t v) { Cell c = null_cell(); c.present = true; c.i = v; return c; }
inline Cell cu(uint64_t v) { Cell c = null_cell(); c.present = true; c.u = v; return c; }
inline Cell cd(double v) { Cell c = null_cell(); c.present = true; c.d = v; return c; }
inline Cell cs(std::string v) { Cell c = null_cell(); c.present = true; c.bytes = std::move(v); return c; }
inline Cell cl(std::vector<int64_t> v) { Cell c = null_cell(); c.present = true; c.items = std::move(v); return c; }

struct Col {
    const char* name;
    const char* format;   // Arrow C Data Interface format string
    std::vector<Cell> cells;
};

inline std::vector<Col> spec() {
    const int64_t I64_MIN = INT64_MIN;
    const int64_t I64_MAX = INT64_MAX;
    const uint64_t U64_MAX = UINT64_MAX;
    std::vector<Col> cols;
    cols.push_back({"b",   "b",       {ci(1), null_cell(), ci(0), ci(1), ci(0)}});
    cols.push_back({"i8",  "c",       {ci(-128), null_cell(), ci(0), ci(127), ci(1)}});
    cols.push_back({"i16", "s",       {ci(-32768), null_cell(), ci(0), ci(32767), ci(2)}});
    cols.push_back({"i32", "i",       {ci(-2147483648LL), null_cell(), ci(0), ci(2147483647), ci(3)}});
    cols.push_back({"i64", "l",       {ci(I64_MIN), null_cell(), ci(0), ci(I64_MAX), ci(4)}});
    cols.push_back({"u8",  "C",       {cu(0), null_cell(), cu(255), cu(7), cu(5)}});
    cols.push_back({"u16", "S",       {cu(0), null_cell(), cu(65535), cu(7), cu(6)}});
    cols.push_back({"u32", "I",       {cu(0), null_cell(), cu(4294967295ULL), cu(7), cu(7)}});
    cols.push_back({"u64", "L",       {cu(0), null_cell(), cu(U64_MAX), cu(7), cu(8)}});
    cols.push_back({"f32", "f",       {cd(0.5), null_cell(), cd(-1.5), cd(100.125), cd(3.0)}});
    cols.push_back({"f64", "g",       {cd(0.5), null_cell(), cd(-1.5), cd(100.125), cd(0.001)}});
    cols.push_back({"s",   "u",       {cs(""), null_cell(), cs("abc"), cs("h\xc3\xa9llo"), cs("x")}});
    cols.push_back({"ls",  "U",       {cs(""), null_cell(), cs("abc"), cs("\xe6\x97\xa5\xe6\x9c\xac"), cs("y")}});
    cols.push_back({"bin", "z",       {cs(""), null_cell(), cs(std::string("\x00\x01", 2)), cs("ab"), cs("\xff")}});
    cols.push_back({"d",   "tdD",     {ci(0), null_cell(), ci(18262), ci(-1), ci(1)}});
    cols.push_back({"ts",  "tsu:UTC", {ci(0), null_cell(), ci(1577836800000000LL), ci(-1), ci(1)}});
    cols.push_back({"dur", "tDu",     {ci(0), null_cell(), ci(86400000000LL), ci(-1), ci(1)}});
    cols.push_back({"li",  "+l",      {cl({}), null_cell(), cl({1, 2, 3}), cl({4}), cl({5, 6})}});
    return cols;
}

inline void set_child_type(ArrowSchema* child, const char* format) {
    std::string f(format);
    if (f == "b") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_BOOL), "SetType");
    else if (f == "c") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_INT8), "SetType");
    else if (f == "s") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_INT16), "SetType");
    else if (f == "i") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_INT32), "SetType");
    else if (f == "l") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_INT64), "SetType");
    else if (f == "C") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_UINT8), "SetType");
    else if (f == "S") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_UINT16), "SetType");
    else if (f == "I") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_UINT32), "SetType");
    else if (f == "L") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_UINT64), "SetType");
    else if (f == "f") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_FLOAT), "SetType");
    else if (f == "g") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_DOUBLE), "SetType");
    else if (f == "u") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_STRING), "SetType");
    else if (f == "U") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_LARGE_STRING), "SetType");
    else if (f == "z") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_BINARY), "SetType");
    else if (f == "tdD") ck(ArrowSchemaSetType(child, NANOARROW_TYPE_DATE32), "SetType");
    else if (f == "tsu:UTC") ck(ArrowSchemaSetTypeDateTime(child, NANOARROW_TYPE_TIMESTAMP, NANOARROW_TIME_UNIT_MICRO, "UTC"), "SetTypeDateTime");
    else if (f == "tDu") ck(ArrowSchemaSetTypeDateTime(child, NANOARROW_TYPE_DURATION, NANOARROW_TIME_UNIT_MICRO, nullptr), "SetTypeDateTime");
    else if (f == "+l") {
        ck(ArrowSchemaSetType(child, NANOARROW_TYPE_LIST), "SetType list");
        ck(ArrowSchemaSetType(child->children[0], NANOARROW_TYPE_INT64), "SetType item");
    } else throw std::runtime_error("unknown format " + f);
}

inline void append_cell(ArrowArray* col, const char* format, const Cell& c) {
    if (!c.present) { ck(ArrowArrayAppendNull(col, 1), "AppendNull"); return; }
    std::string f(format);
    if (f == "b" || f == "c" || f == "s" || f == "i" || f == "l" || f == "tdD" || f == "tsu:UTC" || f == "tDu") {
        ck(ArrowArrayAppendInt(col, c.i), "AppendInt");
    } else if (f == "C" || f == "S" || f == "I" || f == "L") {
        ck(ArrowArrayAppendUInt(col, c.u), "AppendUInt");
    } else if (f == "f" || f == "g") {
        ck(ArrowArrayAppendDouble(col, c.d), "AppendDouble");
    } else if (f == "u" || f == "U" || f == "z") {
        ArrowBufferView bv;
        bv.data.as_char = c.bytes.data();
        bv.size_bytes = (int64_t)c.bytes.size();
        ck(ArrowArrayAppendBytes(col, bv), "AppendBytes");
    } else if (f == "+l") {
        for (int64_t v : c.items) ck(ArrowArrayAppendInt(col->children[0], v), "AppendInt item");
        ck(ArrowArrayFinishElement(col), "FinishElement list");
    } else throw std::runtime_error("unknown format " + f);
}

// Build the fixture. case: full | one | empty | allnull.
inline mlc::ArrowTable mk(const std::string& which) {
    std::vector<Col> cols = spec();
    int64_t nrows;
    if (which == "full") nrows = 5;
    else if (which == "one") nrows = 1;
    else if (which == "empty") nrows = 0;
    else if (which == "allnull") nrows = 5;
    else throw std::runtime_error("unknown case " + which);

    ArrowSchema schema;
    ArrowSchemaInit(&schema);
    ck(ArrowSchemaSetTypeStruct(&schema, (int64_t)cols.size()), "SetTypeStruct");
    for (size_t j = 0; j < cols.size(); ++j) {
        set_child_type(schema.children[j], cols[j].format);
        ck(ArrowSchemaSetName(schema.children[j], cols[j].name), "SetName");
    }

    ArrowArray array;
    ArrowError err;
    ck(ArrowArrayInitFromSchema(&array, &schema, &err), "InitFromSchema");
    ck(ArrowArrayStartAppending(&array), "StartAppending");
    for (int64_t r = 0; r < nrows; ++r) {
        for (size_t j = 0; j < cols.size(); ++j) {
            const Cell& c = (which == "allnull") ? cols[j].cells[0] : cols[j].cells[(size_t)r];
            if (which == "allnull") append_cell(array.children[j], cols[j].format, null_cell());
            else append_cell(array.children[j], cols[j].format, c);
        }
        ck(ArrowArrayFinishElement(&array), "FinishElement struct");
    }
    ck(ArrowArrayFinishBuildingDefault(&array, &err), "FinishBuilding");
    return mlc::ArrowTable(std::move(schema), std::move(array));
}

inline std::string esc(const char* p, int64_t n) {
    std::string out;
    for (int64_t k = 0; k < n; ++k) {
        unsigned char ch = (unsigned char)p[k];
        if (ch >= 0x20 && ch <= 0x7e && ch != 0x5c) out.push_back((char)ch);
        else { char buf[8]; snprintf(buf, sizeof buf, "\\x%02x", ch); out += buf; }
    }
    return out;
}

inline std::string tname(const ArrowSchema* s) {
    std::string f(s->format ? s->format : "");
    if (f == "b") return "bool";
    if (f == "c") return "i8";
    if (f == "s") return "i16";
    if (f == "i") return "i32";
    if (f == "l") return "i64";
    if (f == "C") return "u8";
    if (f == "S") return "u16";
    if (f == "I") return "u32";
    if (f == "L") return "u64";
    if (f == "f") return "f32";
    if (f == "g") return "f64";
    if (f == "u") return "utf8";
    if (f == "U") return "large_utf8";
    if (f == "z") return "binary";
    if (f == "tdD") return "date32";
    if (f == "tsu:UTC") return "ts_us_UTC";
    if (f == "tDu") return "dur_us";
    if (f == "+l") return "list_" + tname(s->children[0]);
    return "?" + f;
}

inline std::string fmt_value(const ArrowArrayView* v, const ArrowSchema* s, int64_t row) {
    if (ArrowArrayViewIsNull(v, row)) return "null";
    std::string f(s->format);
    char buf[64];
    if (f == "b") return ArrowArrayViewGetIntUnsafe(v, row) ? "true" : "false";
    if (f == "c" || f == "s" || f == "i" || f == "l" || f == "tdD" || f == "tsu:UTC" || f == "tDu") {
        snprintf(buf, sizeof buf, "%lld", (long long)ArrowArrayViewGetIntUnsafe(v, row));
        return buf;
    }
    if (f == "C" || f == "S" || f == "I" || f == "L") {
        snprintf(buf, sizeof buf, "%llu", (unsigned long long)ArrowArrayViewGetUIntUnsafe(v, row));
        return buf;
    }
    if (f == "f" || f == "g") {
        snprintf(buf, sizeof buf, "%g", ArrowArrayViewGetDoubleUnsafe(v, row));
        return buf;
    }
    if (f == "u" || f == "U" || f == "z") {
        ArrowBufferView bv = ArrowArrayViewGetBytesUnsafe(v, row);
        return esc(bv.data.as_char, bv.size_bytes);
    }
    if (f == "+l") {
        int64_t start = v->buffer_views[1].data.as_int32[v->offset + row];
        int64_t end = v->buffer_views[1].data.as_int32[v->offset + row + 1];
        std::string out = "[";
        for (int64_t k = start; k < end; ++k) {
            if (k > start) out += ",";
            out += fmt_value(v->children[0], s->children[0], k);
        }
        return out + "]";
    }
    return "?";
}

inline std::string describe(const mlc::ArrowTable& t) {
    const ArrowSchema* schema = t.schema();
    const ArrowArray* array = t.array();
    ArrowArrayView view;
    ArrowError err;
    ck(ArrowArrayViewInitFromSchema(&view, schema, &err), "ViewInitFromSchema");
    ck(ArrowArrayViewSetArray(&view, array, &err), "ViewSetArray");

    std::string out = "rows=" + std::to_string(array->length) + " cols=" + std::to_string(schema->n_children);
    for (int64_t j = 0; j < schema->n_children; ++j) {
        const ArrowSchema* cs_ = schema->children[j];
        const ArrowArrayView* cv = view.children[j];
        std::string line = std::string("\n") + (cs_->name ? cs_->name : "") + " " + tname(cs_)
            + " n=" + std::to_string(cv->length)
            + " nulls=" + std::to_string(array->children[j]->null_count) + " [";
        for (int64_t r = 0; r < cv->length; ++r) {
            if (r > 0) line += ",";
            line += fmt_value(cv, cs_, r);
        }
        out += line + "]";
    }
    ArrowArrayViewReset(&view);
    return out;
}

} // namespace conformance

#endif
