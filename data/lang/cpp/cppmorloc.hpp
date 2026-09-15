#ifndef __CPPMORLOC_HPP__
#define __CPPMORLOC_HPP__

#include <vector>
#include <memory>
#include <stack>
#include <list>
#include <forward_list>
#include <queue>
#include <deque>
#include <optional>
#include <variant>

#include <algorithm>
#include <tuple>
#include <stdexcept>
#include <cstring>
#include <string>
#include <sstream>
#include <limits>
#include <type_traits>

#include "morloc.h"

// ============================================================
// Type traits for container dispatch
// ============================================================

template<typename T> struct is_std_vector : std::false_type {};
template<typename T, typename A> struct is_std_vector<std::vector<T, A>> : std::true_type {};

template<typename T> struct is_std_list : std::false_type {};
template<typename T, typename A> struct is_std_list<std::list<T, A>> : std::true_type {};

template<typename T> struct is_std_forward_list : std::false_type {};
template<typename T, typename A> struct is_std_forward_list<std::forward_list<T, A>> : std::true_type {};

template<typename T> struct is_std_deque : std::false_type {};
template<typename T, typename A> struct is_std_deque<std::deque<T, A>> : std::true_type {};

template<typename T> struct is_std_stack : std::false_type {};
template<typename T, typename C> struct is_std_stack<std::stack<T, C>> : std::true_type {};

template<typename T> struct is_std_queue : std::false_type {};
template<typename T, typename C> struct is_std_queue<std::queue<T, C>> : std::true_type {};

template<typename T> struct is_std_tuple : std::false_type {};
template<typename... Args> struct is_std_tuple<std::tuple<Args...>> : std::true_type {};

template<typename T> struct is_std_pair : std::false_type {};
template<typename A, typename B> struct is_std_pair<std::pair<A, B>> : std::true_type {};

template<typename T> struct is_std_optional : std::false_type {};
template<typename T> struct is_std_optional<std::optional<T>> : std::true_type {};

// shared_ptr<T> is the C++ surface form for a `?T` field at a recursive
// cycle position (T's body would be incomplete inside its own
// definition). `nullptr == absent`. The marshalling layer detects this
// shape and handles the wire-format relptr at the field's slot.
template<typename T> struct is_std_shared_ptr : std::false_type {};
template<typename T> struct is_std_shared_ptr<std::shared_ptr<T>> : std::true_type {};

template<typename T>
inline constexpr bool is_non_vector_container_v =
    is_std_list<T>::value || is_std_forward_list<T>::value ||
    is_std_deque<T>::value || is_std_stack<T>::value ||
    is_std_queue<T>::value;

// ============================================================
// Field representation dispatch (D1)
// ============================================================
//
// The morloc encoding rule for `?T` at C++ field positions:
//   - `?T` -> std::optional<T> everywhere sizeof(T) is known.
//   - `?T` -> some pointer-shape (null == absent) at recursive-cycle
//     positions where sizeof(T) is not yet known.
//
// Pointer-shape is polymorphic within a closed set (shared_ptr,
// unique_ptr, raw T*). The codegen emits generic wrap/deref calls and
// the C++ template machinery resolves which wrapper to use at compile
// time. The value/pointer axis is enforced by static_assert: a user-
// supplied struct that puts optional<T> where the rule says pointer
// (or vice versa) gets a clear error.

namespace mlc {

template<typename T> struct is_pointer_shape : std::false_type {};
template<typename T> struct is_pointer_shape<std::shared_ptr<T>> : std::true_type {};
template<typename T> struct is_pointer_shape<std::unique_ptr<T>> : std::true_type {};
template<typename T> struct is_pointer_shape<T*>                 : std::true_type {};

// Pointer-shape traits. Primary intentionally undefined; only
// recognized pointer kinds get specializations.
template<typename FieldT> struct pointer_traits;

template<typename T>
struct pointer_traits<std::shared_ptr<T>> {
    using inner = T;
    static std::shared_ptr<T> wrap(T&& v)              { return std::make_shared<T>(std::move(v)); }
    static std::shared_ptr<T> wrap(const T& v)         { return std::make_shared<T>(v); }
    static std::shared_ptr<T> absent()                 { return nullptr; }
    static const T& deref(const std::shared_ptr<T>& p) { return *p; }
    static bool has_value(const std::shared_ptr<T>& p) { return p != nullptr; }
};

template<typename T>
struct pointer_traits<std::unique_ptr<T>> {
    using inner = T;
    static std::unique_ptr<T> wrap(T&& v)              { return std::make_unique<T>(std::move(v)); }
    static std::unique_ptr<T> wrap(const T& v)         { return std::make_unique<T>(v); }
    static std::unique_ptr<T> absent()                 { return nullptr; }
    static const T& deref(const std::unique_ptr<T>& p) { return *p; }
    static bool has_value(const std::unique_ptr<T>& p) { return p != nullptr; }
};

// Raw pointer T*: READ-ONLY at field positions. The codegen has no
// way to manage the pointee's lifetime when constructing a value, so
// `wrap` is armed with a static_assert that fires at any call site
// the codegen tries to materialize a T* field from a fresh value.
// Useful for FFI consumer-side scenarios where the user's struct
// already holds a T* owned by external code.
template<typename T>
struct pointer_traits<T*> {
    using inner = T;
    static T* wrap(T&&) {
        static_assert(sizeof(T*) == 0,
            "Cannot construct a field of type T* natively in C++. "
            "Raw pointers carry no ownership information, so the morloc "
            "codegen has no way to manage the pointee's lifetime. Use "
            "std::shared_ptr or std::unique_ptr if native construction "
            "is needed; or restrict the enclosing record to consumer-"
            "side use only.");
        return nullptr;
    }
    static T* absent()                  { return nullptr; }
    static const T& deref(T* p)         { return *p; }
    static bool has_value(T* p)         { return p != nullptr; }
};

template<typename FieldT>
FieldT wrap_field_absent() {
    if constexpr (is_pointer_shape<FieldT>::value) {
        return pointer_traits<FieldT>::absent();
    } else if constexpr (is_std_optional<FieldT>::value) {
        return std::nullopt;
    } else {
        static_assert(sizeof(FieldT) == 0,
            "wrap_field_absent called on a non-nullable field type. "
            "Only pointer-shapes and std::optional<T> can be absent.");
        return FieldT{};
    }
}

// Generic field-construction entry point: the codegen emits
//   {a, mlc::wrap_field<decltype(rec_t::field)>(value)}
// at every aggregate-init / field-assign site. The static_assert
// enforces the value/pointer axis of the encoding rule: ?T at a
// recursive-cycle position must be pointer-shaped; ?T everywhere
// else must be std::optional<T>; non-optional fields pass through.
//
// The `Inner = std::optional<T>` branch handles the case where the
// codegen emits a typed empty optional (e.g. `std::optional<T>{}`
// for a Null literal). At a pointer-shape field that means "absent";
// at an optional field it's a direct copy. The codegen uses the
// typed empty-optional form so foreign-call argument positions
// retain template-argument deducibility (bare `std::nullopt` has
// type `nullopt_t` and cannot pin a template parameter).
template<typename FieldT, typename Inner>
FieldT wrap_field(Inner&& v) {
    using InnerBare = std::remove_cv_t<std::remove_reference_t<Inner>>;
    static_assert(
        is_pointer_shape<FieldT>::value
            || std::is_same_v<FieldT, std::optional<InnerBare>>
            || is_std_optional<InnerBare>::value
            || std::is_constructible_v<FieldT, Inner>,
        "Field type doesn't match the morloc encoding rule. "
        "?T at a recursive-cycle position expects a pointer-shape "
        "(std::shared_ptr<T>, std::unique_ptr<T>, or T*); "
        "?T everywhere else expects std::optional<T>; "
        "non-optional fields expect T directly."
    );
    if constexpr (is_std_optional<InnerBare>::value) {
        if (!v.has_value()) return wrap_field_absent<FieldT>();
        if constexpr (is_pointer_shape<FieldT>::value) {
            return pointer_traits<FieldT>::wrap(std::move(*v));
        } else if constexpr (is_std_optional<FieldT>::value) {
            return FieldT(std::move(*v));
        } else {
            return FieldT(std::move(*v));
        }
    } else if constexpr (is_pointer_shape<FieldT>::value) {
        return pointer_traits<FieldT>::wrap(std::forward<Inner>(v));
    } else if constexpr (is_std_optional<FieldT>::value) {
        return FieldT(std::forward<Inner>(v));
    } else {
        return FieldT(std::forward<Inner>(v));
    }
}

// Null-literal overload: morloc's `Null` codegens to `std::nullopt`,
// which then flows into the field-construction site. For an optional
// field that's just an assignment; for a pointer-shape field it must
// resolve to nullptr. This overload routes both through
// wrap_field_absent so the right "absent" value is constructed for
// the field's actual type.
template<typename FieldT>
FieldT wrap_field(std::nullopt_t) {
    return wrap_field_absent<FieldT>();
}

// Generic field-read entry point. Returns the underlying value by
// const-reference, dispatching on whether the field is wrapped.
template<typename FieldT>
decltype(auto) deref_field(const FieldT& f) {
    if constexpr (is_pointer_shape<FieldT>::value) {
        return pointer_traits<FieldT>::deref(f);
    } else if constexpr (is_std_optional<FieldT>::value) {
        return (*f);
    } else {
        return (f);
    }
}

template<typename FieldT>
bool field_has_value(const FieldT& f) {
    if constexpr (is_pointer_shape<FieldT>::value) {
        return pointer_traits<FieldT>::has_value(f);
    } else if constexpr (is_std_optional<FieldT>::value) {
        return f.has_value();
    } else {
        return true;
    }
}

} // namespace mlc


// ============================================================
// Container-to-vector conversion
// ============================================================

template<typename Container>
auto to_vector(const Container& c) {
    using T = typename Container::value_type;
    if constexpr (is_std_stack<Container>::value) {
        std::vector<T> v;
        auto copy = c;
        while (!copy.empty()) { v.push_back(copy.top()); copy.pop(); }
        std::reverse(v.begin(), v.end());
        return v;
    } else if constexpr (is_std_queue<Container>::value) {
        std::vector<T> v;
        auto copy = c;
        while (!copy.empty()) { v.push_back(copy.front()); copy.pop(); }
        return v;
    } else {
        return std::vector<T>(c.begin(), c.end());
    }
}


// ============================================================
// C runtime wrappers (implementations in cppmorloc.cpp)
// ============================================================

absptr_t rel2abs_cpp(relptr_t ptr);
relptr_t abs2rel_cpp(absptr_t ptr);

// Resolve a relative pointer.
//
// When `base_ptr` is non-null the relptr is a buffer-relative offset
// into a malloc'd inline packet payload (the inline MESG+VOIDSTAR
// path); resolution is a single add. Otherwise we route through the
// inline `resolve_relptr` in morloc.h which consults the per-process
// exposed volume table for a lock-free fast path, falling through to
// the SHM FFI only when the slot has not yet been populated in this
// process.
static inline void* resolve_relptr_cpp(relptr_t relptr, const void* base_ptr) {
    if (base_ptr) {
        return (char*)base_ptr + relptr_offset_bits(relptr);
    }
    return resolve_relptr(relptr, NULL, NULL);
}
bool shfree_cpp(absptr_t ptr);
Schema* parse_schema_cpp(const char* schema_ptr);
void* shmalloc_cpp(size_t size);
shm_t* shinit_cpp(const char* shm_basename, size_t volume_index, size_t shm_size);
int pack_with_schema_cpp(const void* mlc, const Schema* schema, char** mpk, size_t* mpk_size);
int unpack_with_schema_cpp(const char* mgk, size_t mgk_size, const Schema* schema, void** mlcptr);


// ============================================================
// Recursive-record env (named-schema stack)
// ============================================================
//
// Mirror of the C-side stack in pymorloc.c / rmorloc.c: when walking
// a Schema that may contain MORLOC_RECUR back-references, every
// declaration (a non-Recur schema with `name != nullptr`) is pushed
// onto a thread-local stack while its subtree is walked, and a
// back-reference resolves to the nearest declaration of its name. The
// marshalling walks below push and pop as they visit frames.
struct CppRecurEntry {
    const char* name;
    const Schema* schema;
};

inline std::vector<CppRecurEntry>& recur_env() {
    thread_local std::vector<CppRecurEntry> stack;
    return stack;
}

inline const Schema* recur_env_lookup(const char* name) {
    if (name == nullptr) return nullptr;
    auto& stack = recur_env();
    for (auto it = stack.rbegin(); it != stack.rend(); ++it) {
        if (it->name != nullptr && std::strcmp(it->name, name) == 0) {
            return it->schema;
        }
    }
    return nullptr;
}

// Resolve a Recur schema to the named declaration on the env stack.
// Returns the original schema unchanged when not a Recur (so callers
// can use it as an unconditional normaliser at the top of a walker).
// Throws when a Recur is unresolvable, since downstream reads would
// otherwise dereference an empty parameter list.
inline const Schema* resolve_recur(const Schema* schema) {
    if (schema == nullptr || schema->type != MORLOC_RECUR) {
        return schema;
    }
    const Schema* target = recur_env_lookup(schema->name);
    if (target == nullptr) {
        std::ostringstream oss;
        oss << "Recur back-reference to undeclared schema name '"
            << (schema->name ? schema->name : "?") << "'";
        throw std::runtime_error(oss.str());
    }
    return target;
}

// ============================================================
// mpk_pack / mpk_unpack declarations
// ============================================================

template<typename T>
std::vector<char> mpk_pack(const T& data, const std::string& schema_str);

template<typename T>
T mpk_unpack(const std::vector<char>& packed_data, const std::string& schema_str);


// ============================================================
// schema_alignment (C++ mirror of the C function in schema.c)
// ============================================================

inline size_t schema_alignment_cpp(const Schema* schema) {
    switch (schema->type) {
        case MORLOC_NIL: case MORLOC_BOOL: case MORLOC_SINT8: case MORLOC_UINT8: return 1;
        case MORLOC_SINT16: case MORLOC_UINT16: return 2;
        case MORLOC_SINT32: case MORLOC_UINT32: case MORLOC_FLOAT32: return 4;
        case MORLOC_SINT64: case MORLOC_UINT64: case MORLOC_FLOAT64:
        case MORLOC_STRING: case MORLOC_ARRAY:
        case MORLOC_INT: return alignof(size_t);
        case MORLOC_TUPLE: case MORLOC_MAP: {
            size_t max_align = 1;
            for (size_t i = 0; i < schema->size; i++) {
                size_t a = schema_alignment_cpp(schema->parameters[i]);
                if (a > max_align) max_align = a;
            }
            return max_align;
        }
        case MORLOC_OPTIONAL: return schema_alignment_cpp(schema->parameters[0]);
        default: return alignof(size_t);
    }
}

// SIMD/BLAS-friendly alignment for Array data buffers when the element type
// is a primitive numeric. Fixed 64-byte constant in the wire format spec --
// covers SSE/AVX/AVX-512 + cache lines on every common architecture, and the
// per-array slack overhead (<= 63 bytes) is negligible for large arrays.
#define MORLOC_ARRAY_DATA_ALIGN 64

inline bool is_primitive_numeric_cpp(const Schema* schema) {
    switch (schema->type) {
        case MORLOC_SINT8:  case MORLOC_SINT16: case MORLOC_SINT32: case MORLOC_SINT64:
        case MORLOC_UINT8:  case MORLOC_UINT16: case MORLOC_UINT32: case MORLOC_UINT64:
        case MORLOC_FLOAT32: case MORLOC_FLOAT64:
            return true;
        default:
            return false;
    }
}

// True iff a std::vector<T> with this element schema is byte-identical to the
// voidstar element layout, so the whole data region can be bulk-copied. Both
// T's numeric KIND (float / signed-int / unsigned-int) AND width must match the
// schema element -- not merely "both are equal-size primitive numerics", which
// would let a float-vs-int or signed-vs-unsigned pairing memcpy raw bits and
// skip the per-element value-cast / range-check the scalar to_voidstar applies.
// Excludes the variable-width BigInt (MORLOC_INT) and non-arithmetic T.
template <typename T>
inline bool vector_is_bulk_copyable(const Schema* elem) {
    if (static_cast<size_t>(elem->width) != sizeof(T)) return false;
    if constexpr (std::is_floating_point_v<T>) {
        return elem->type == MORLOC_FLOAT32 || elem->type == MORLOC_FLOAT64;
    } else if constexpr (std::is_integral_v<T> && std::is_signed_v<T>) {
        return elem->type == MORLOC_SINT8 || elem->type == MORLOC_SINT16
            || elem->type == MORLOC_SINT32 || elem->type == MORLOC_SINT64;
    } else if constexpr (std::is_integral_v<T> && std::is_unsigned_v<T>) {
        return elem->type == MORLOC_UINT8 || elem->type == MORLOC_UINT16
            || elem->type == MORLOC_UINT32 || elem->type == MORLOC_UINT64;
    } else {
        return false;
    }
}

// Alignment for an Array's element data buffer in SHM. For primitive numerics
// we bump to MORLOC_ARRAY_DATA_ALIGN (SIMD/BLAS); otherwise use the element's
// natural alignment.
inline size_t array_data_alignment_cpp(const Schema* elem) {
    size_t natural = schema_alignment_cpp(elem);
    return is_primitive_numeric_cpp(elem)
        ? (MORLOC_ARRAY_DATA_ALIGN > natural ? MORLOC_ARRAY_DATA_ALIGN : natural)
        : natural;
}


#define MORLOC_VARIANT_PAYLOAD 8

// ============================================================
// Marshalling walks
// ============================================================
//
// A value is marshalled in three passes -- size, write, read -- and each is
// an explicit-stack traversal rather than a recursive one, so a value's depth
// is bounded by memory and not by the worker thread's stack. A frame names
// one compound node (the schema, the native object, and for write/read the
// wire slot); the driver pops a frame, keeps the recursion environment in
// step with it, and calls the node's step, which does the node's own work
// and pushes its compound children. Leaves -- primitives, enums, strings --
// are handled inline by the parent's step.
//
// Every marshallable type has an MlcNode<T> with three steps. The primary
// template covers the standard containers with `if constexpr`; generated
// records, variant wrappers and arm structs are explicit specializations
// emitted by the code generator. An unsupported type fails to compile.
//
// Arrays are walked one element per visit: a step that has more elements
// re-pushes its own frame beneath the element's, so the frame stack stays
// proportional to depth rather than to element count.
//
// A frame is only needed where the schema can describe a value of unbounded
// depth, which is where it holds a back-reference. A child whose schema has
// none is stepped by calling its step from the parent's: the depth of that
// call chain is the schema's own height. A walk whose root has none frames
// nothing at all.

struct MlcSizeWalk;
struct MlcWriteWalk;
struct MlcReadWalk;

template<typename T> struct MlcNode;

// A leaf is handled by the parent's step without a frame of its own.
template<typename T>
inline constexpr bool mlc_is_leaf_v =
    std::is_arithmetic_v<T> || std::is_enum_v<T>
    || std::is_same_v<T, std::nullptr_t> || std::is_same_v<T, std::string>
    || std::is_same_v<T, const char*>;

// A compound schema must reach a container node. A scalar-sized leaf at a
// compound schema means a Packable value was serialized without its pack
// wrapper -- fail loudly instead of under-allocating shared memory (which
// writes the payload past the block and crashes the reader).
inline void guard_scalar_schema(const Schema* schema, const char* fn) {
    if (schema->type == MORLOC_ARRAY
     || schema->type == MORLOC_TUPLE
     || schema->type == MORLOC_MAP) {
        throw std::runtime_error(
            std::string(fn) + ": compound schema reached a scalar-sized type -- "
            "a Packable value was serialized without its pack wrapper");
    }
}

// Range-check a value before narrowing to a fixed-width wire integer.
// Catches the case where a C++ source produces a value too large for the
// declared morloc type (e.g. `int` returning 200 declared as `Int8`),
// which would otherwise truncate silently into the wire payload.
template<typename Wire, typename Src>
Wire check_range_narrow(const Src& data, const char* name) {
    if constexpr (std::is_arithmetic_v<Src>) {
        using SLim = std::numeric_limits<Wire>;
        if constexpr (std::is_floating_point_v<Src>) {
            // Narrowing a non-finite float (Inf/NaN) to an integer via
            // static_cast is UB -- Apple-clang on ARM64 may trap or yield
            // garbage. Reject it cleanly.
            if (!std::isfinite(data)) {
                std::ostringstream oss;
                oss << "Cannot convert non-finite value to integer type " << name;
                throw std::overflow_error(oss.str());
            }
            // A FINITE but out-of-range float is ALSO UB to static_cast into an
            // integer, so range-check in floating point BEFORE the narrowing
            // cast, against exact power-of-two bounds (representable in long
            // double even where it is 64-bit): the valid signed range is
            // [-2^(w-1), 2^(w-1)) and unsigned is [0, 2^w).
            long double d = static_cast<long double>(data);
            long double hi_excl = std::ldexp(1.0L, std::numeric_limits<Wire>::digits);
            long double lo = std::is_signed_v<Wire> ? -hi_excl : 0.0L;
            if (!(d >= lo && d < hi_excl)) {
                std::ostringstream oss;
                oss << "Integer overflow: value " << data << " out of range for " << name;
                throw std::overflow_error(oss.str());
            }
        } else if constexpr (std::is_signed_v<Src>) {
            int64_t v = static_cast<int64_t>(data);
            int64_t lo = static_cast<int64_t>(SLim::min());
            int64_t hi = static_cast<int64_t>(SLim::max());
            if (v < lo || v > hi) {
                std::ostringstream oss;
                oss << "Integer overflow: value " << v << " out of range for "
                    << name << " (range " << lo << " to " << hi << ")";
                throw std::overflow_error(oss.str());
            }
        } else {
            uint64_t v = static_cast<uint64_t>(data);
            uint64_t hi = static_cast<uint64_t>(SLim::max());
            if (v > hi) {
                std::ostringstream oss;
                oss << "Integer overflow: value " << v << " out of range for "
                    << name << " (range 0 to " << hi << ")";
                throw std::overflow_error(oss.str());
            }
        }
    }
    return static_cast<Wire>(data);
}

// ------------------------------------------------------------
// Leaves
// ------------------------------------------------------------

// Size of a leaf, including any variable-length tail it appends.
template<typename T>
size_t mlc_leaf_size(const Schema* schema, const T& data) {
    if (schema->type == MORLOC_NIL) {
        return schema->width;
    }
    if constexpr (std::is_same_v<T, std::string>) {
        return schema->width + data.size();
    } else if constexpr (std::is_same_v<T, const char*>) {
        return schema->width + strlen(data);
    } else if constexpr (std::is_same_v<T, std::nullptr_t>) {
        return schema->width;
    } else {
        if (schema->type == MORLOC_INT) {
            // Inline BigInt: [size, value] = 16 bytes (C++ values always fit inline)
            return 16;
        }
        if (schema->type == MORLOC_IFILE
         || schema->type == MORLOC_OSTREAM
         || schema->type == MORLOC_ISTREAM) {
            // Stream-handle field: look up the exact suballoc cost via the
            // registry (returns 8 + path_len for TAG_PATH, 0 for empty).
            if constexpr (std::is_arithmetic_v<T>) {
                char* err = NULL;
                int64_t n = mlc_handle_path_len(static_cast<int64_t>(data), &err);
                if (err) {
                    std::string msg(err);
                    free(err);
                    throw std::runtime_error("mlc_handle_path_len: " + msg);
                }
                return schema->width + static_cast<size_t>(n);
            } else {
                throw std::runtime_error(
                    "get_shm_size: stream-handle schema requires an arithmetic handle type"
                );
            }
        }
        guard_scalar_schema(schema, "get_shm_size");
        return schema->width;
    }
}

// Write raw bytes as an array: the header at dest, the bytes at the cursor.
inline void* bytes_to_voidstar(void* dest, void** cursor, const Schema* schema, const uint8_t* data, size_t size) {
    Array* result = static_cast<Array*>(dest);
    result->size = size;
    if(size == 0){
        result->data = RELNULL;
        return dest;
    }
    absptr_t data_ptr = static_cast<absptr_t>(*cursor);
    result->data = abs2rel_cpp(data_ptr);
    *cursor = static_cast<char*>(*cursor) + size * schema->parameters[0]->width;
    memcpy(data_ptr, data, size);
    return dest;
}

// Write a leaf at schema width, so the wire form matches the morloc type
// regardless of the C++ concrete type's width.
template<typename T>
void mlc_leaf_write(void* dest, void** cursor, const Schema* schema, const T& data) {
    // A nil slot has no width; the wire carries nothing for it.
    if (schema->type == MORLOC_NIL) {
        return;
    }
    if constexpr (std::is_same_v<T, std::string>) {
        bytes_to_voidstar(dest, cursor, schema, (const uint8_t*)data.c_str(), data.size());
    } else if constexpr (std::is_same_v<T, const char*>) {
        bytes_to_voidstar(dest, cursor, schema, (const uint8_t*)data, strlen(data));
    } else if constexpr (std::is_same_v<T, std::nullptr_t>) {
        // Nothing else has a nil schema.
    } else if constexpr (std::is_enum_v<T>) {
        // A morloc enum is its one-byte wire tag; a host enum standing in
        // for an integer is written at the integer's width.
        if (schema->type == MORLOC_ENUM) {
            *((uint8_t*)dest) = static_cast<uint8_t>(data);
        } else {
            mlc_leaf_write(dest, cursor, schema, static_cast<std::underlying_type_t<T>>(data));
        }
    } else {
        guard_scalar_schema(schema, "to_voidstar");
        switch(schema->type) {
            case MORLOC_IFILE:
            case MORLOC_OSTREAM:
            case MORLOC_ISTREAM: {
                char* err = NULL;
                if (mlc_write_handle_voidstar(
                        static_cast<int64_t>(data), dest, cursor, &err) != 0) {
                    std::string msg = err ? err : "mlc_write_handle_voidstar failed";
                    free(err);
                    throw std::runtime_error(msg);
                }
                break;
            }
            case MORLOC_SINT8:   *(int8_t*)dest   = check_range_narrow<int8_t>(data, "I8");    break;
            case MORLOC_SINT16:  *(int16_t*)dest  = check_range_narrow<int16_t>(data, "I16");  break;
            case MORLOC_SINT32:  *(int32_t*)dest  = check_range_narrow<int32_t>(data, "I32");  break;
            case MORLOC_SINT64:  *(int64_t*)dest  = check_range_narrow<int64_t>(data, "I64");  break;
            case MORLOC_UINT8:   *(uint8_t*)dest  = check_range_narrow<uint8_t>(data, "U8");   break;
            case MORLOC_UINT16:  *(uint16_t*)dest = check_range_narrow<uint16_t>(data, "U16"); break;
            case MORLOC_UINT32:  *(uint32_t*)dest = check_range_narrow<uint32_t>(data, "U32"); break;
            case MORLOC_UINT64:  *(uint64_t*)dest = check_range_narrow<uint64_t>(data, "U64"); break;
            case MORLOC_FLOAT32: *(float*)dest    = static_cast<float>(data);    break;
            case MORLOC_FLOAT64: *(double*)dest   = static_cast<double>(data);   break;
            case MORLOC_INT: {
                // Inline BigInt: [size=1, value] -- no allocation, no relptr
                int64_t* fields = static_cast<int64_t*>(dest);
                fields[0] = 1;
                fields[1] = check_range_narrow<int64_t>(data, "Int");
                break;
            }
            default: *(T*)dest = data; break;
        }
    }
}

// Read a leaf at schema width and convert to the C++ type, so a narrow
// concrete type (e.g. `int` for Int) works with a wider schema.
template<typename T>
T mlc_leaf_read(const Schema* schema, const void* data, const void* base_ptr) {
    if (schema->type == MORLOC_NIL) {
        return T{};
    }
    if constexpr (std::is_same_v<T, bool>) {
        // NOTE: do NOT load as bool: a wire byte outside {0,1} is UB
        return *(const uint8_t*)data == 1;
    } else if constexpr (std::is_same_v<T, std::string>) {
        const Array* array = (const Array*)data;
        if(array->size > 0){
            return std::string((char*)resolve_relptr_cpp(array->data, base_ptr), array->size);
        }
        return std::string("");
    } else if constexpr (std::is_same_v<T, std::nullptr_t>) {
        return nullptr;
    } else if constexpr (std::is_enum_v<T>) {
        if (schema->type == MORLOC_ENUM) {
            return static_cast<T>(*(const uint8_t*)data);
        }
        return static_cast<T>(mlc_leaf_read<std::underlying_type_t<T>>(schema, data, base_ptr));
    } else {
        switch(schema->type) {
            case MORLOC_IFILE:
            case MORLOC_OSTREAM:
            case MORLOC_ISTREAM: {
                char* err = NULL;
                uint8_t kind = (schema->type == MORLOC_IFILE)   ? MLC_KIND_IFILE
                             : (schema->type == MORLOC_OSTREAM) ? MLC_KIND_OSTREAM
                             :                                    MLC_KIND_ISTREAM;
                int64_t handle = mlc_read_handle_voidstar(
                    data, base_ptr, kind, &err);
                if (err || handle < 0) {
                    std::string msg = err ? err : "mlc_read_handle_voidstar failed";
                    free(err);
                    throw std::runtime_error(msg);
                }
                return static_cast<T>(handle);
            }
            case MORLOC_SINT8:   return static_cast<T>(*(const int8_t*)data);
            case MORLOC_SINT16:  return static_cast<T>(*(const int16_t*)data);
            case MORLOC_SINT32:  return static_cast<T>(*(const int32_t*)data);
            case MORLOC_SINT64:  return static_cast<T>(*(const int64_t*)data);
            case MORLOC_UINT8:   return static_cast<T>(*(const uint8_t*)data);
            case MORLOC_UINT16:  return static_cast<T>(*(const uint16_t*)data);
            case MORLOC_UINT32:  return static_cast<T>(*(const uint32_t*)data);
            case MORLOC_UINT64:  return static_cast<T>(*(const uint64_t*)data);
            case MORLOC_FLOAT32: return static_cast<T>(*(const float*)data);
            case MORLOC_FLOAT64: return static_cast<T>(*(const double*)data);
            case MORLOC_INT: {
                // Inline BigInt: [size, value_or_relptr]
                const int64_t* fields = (const int64_t*)data;
                int64_t size = fields[0];
                if (size <= 1) {
                    int64_t val = (size == 0) ? 0 : fields[1];
                    // An integral target must hold the value exactly; a
                    // floating target takes the nearest representable value.
                    if constexpr (std::is_integral_v<T>) {
                        bool fits;
                        if constexpr (std::is_signed_v<T>) {
                            fits = val >= static_cast<int64_t>(std::numeric_limits<T>::min())
                                && val <= static_cast<int64_t>(std::numeric_limits<T>::max());
                        } else {
                            fits = val >= 0
                                && static_cast<uint64_t>(val) <= static_cast<uint64_t>(std::numeric_limits<T>::max());
                        }
                        if (!fits) {
                            std::ostringstream oss;
                            oss << "Integer overflow: value " << val
                                << " does not fit in " << (sizeof(T) * 8) << "-bit type"
                                << " (range " << +std::numeric_limits<T>::min()
                                << " to " << +std::numeric_limits<T>::max() << ")";
                            throw std::overflow_error(oss.str());
                        }
                    }
                    return static_cast<T>(val);
                } else {
                    // Multi-limb integer, cannot fit in any C++ primitive
                    std::ostringstream oss;
                    oss << "Integer overflow: " << size << "-limb integer"
                        << " (" << (size * 64) << " bits)"
                        << " does not fit in " << (sizeof(T) * 8) << "-bit type";
                    if constexpr (std::is_integral_v<T>) {
                        oss << " (range " << +std::numeric_limits<T>::min()
                            << " to " << +std::numeric_limits<T>::max() << ")";
                    }
                    throw std::overflow_error(oss.str());
                }
            }
            default: return *(const T*)data;
        }
    }
}

// True iff a back-reference occurs anywhere under `schema`. A schema without
// one has a fixed height, so a value of it can be stepped by direct call
// (each compound child's step invoked from its parent's) with no frame and
// no recursion environment: the walk then costs what a plain recursive walk
// does. A schema with one may describe a value of any depth, and only a
// framed walk keeps its stack bounded.
inline bool mlc_schema_has_recur(const Schema* schema) {
    if (schema == nullptr) return false;
    if (schema->type == MORLOC_RECUR) return true;
    // An enum's size counts constructors and it has no parameters.
    if (schema->parameters == nullptr) return false;
    for (size_t i = 0; i < schema->size; i++) {
        if (mlc_schema_has_recur(schema->parameters[i])) return true;
    }
    return false;
}

// ------------------------------------------------------------
// Walk drivers
// ------------------------------------------------------------

// The recursion environment is kept in step with the walk: when a frame for
// a named declaration is first visited its name is pushed, and a sentinel
// frame (null step) pushed beneath the node's children pops it once the
// subtree is done. An exception unwinds the env to its depth at entry.
#define MLC_WALK_RUN(BODY)                                                    \
    size_t env_depth_ = recur_env().size();                                   \
    try {                                                                     \
        while (!stack.empty()) {                                              \
            Frame f = stack.back();                                           \
            stack.pop_back();                                                 \
            if (f.step == nullptr) {                                          \
                recur_env().pop_back();                                       \
                continue;                                                     \
            }                                                                 \
            const Schema* s = resolve_recur(f.schema);                        \
            if (!f.env_pushed && s->name != nullptr) {                        \
                recur_env().push_back({s->name, s});                          \
                stack.push_back(Frame{});                                     \
                f.env_pushed = true;                                          \
            }                                                                 \
            cur = f;                                                          \
            BODY                                                              \
        }                                                                     \
    } catch (...) {                                                           \
        recur_env().resize(env_depth_);                                       \
        stack.clear();                                                        \
        throw;                                                                \
    }

struct MlcSizeWalk {
    using Step = void (*)(MlcSizeWalk&, const Schema*, const void*, size_t);
    struct Frame {
        Step step = nullptr;
        const Schema* schema = nullptr;
        const void* obj = nullptr;
        size_t idx = 0;
        bool inline_slot = false;   // the parent already counted this node's width
        bool env_pushed = false;
    };
    std::vector<Frame> stack;
    Frame cur;
    std::vector<std::shared_ptr<void>> keep;   // values a step made, alive until the walk ends
    int64_t total = 0;
    bool direct;   // the root has no back-reference: nothing needs a frame

    explicit MlcSizeWalk(const Schema* root) : direct(!mlc_schema_has_recur(root)) {}

    // A child of a schema that cannot describe unbounded depth is stepped
    // by call.
    bool flat(const Schema* schema) const {
        return direct || !mlc_schema_has_recur(schema);
    }

    template<typename T>
    static void thunk(MlcSizeWalk& w, const Schema* s, const void* obj, size_t idx) {
        MlcNode<T>::size_step(w, s, *static_cast<const T*>(obj), idx);
    }

    // Account for a child. A leaf is summed here; a compound child is
    // stepped by call or by frame. `inline_slot` says the child's fixed
    // width lies inside the parent's (tuple and record fields), so only its
    // tail is added.
    template<typename T>
    void child(const Schema* schema, const T& v, bool inline_slot) {
        if constexpr (mlc_is_leaf_v<T>) {
            const Schema* s = resolve_recur(schema);
            total += static_cast<int64_t>(mlc_leaf_size(s, v));
            if (inline_slot) total -= static_cast<int64_t>(s->width);
        } else if (flat(schema)) {
            if (inline_slot) total -= static_cast<int64_t>(schema->width);
            MlcNode<T>::size_step(*this, schema, v, 0);
        } else {
            stack.push_back(Frame{&thunk<T>, schema, &v, 0, inline_slot, false});
        }
    }

    // Visit the current node again for element `idx`.
    void resume(size_t idx) {
        Frame f = cur;
        f.idx = idx;
        f.inline_slot = false;
        f.env_pushed = true;
        stack.push_back(f);
    }

    // A variant slot plus the out-of-line payload of one arm.
    template<typename T>
    void variant_payload(const Schema* schema, const Schema* arm, const T& payload) {
        const Schema* a = resolve_recur(arm);
        size_t align = schema_alignment_cpp(a);
        if (align == 0) align = 1;
        total += static_cast<int64_t>(schema->width + (align - 1));
        child(a, payload, false);
    }

    int64_t run() {
        MLC_WALK_RUN({
            if (f.inline_slot) total -= static_cast<int64_t>(s->width);
            f.step(*this, s, f.obj, f.idx);
        })
        return total;
    }
};

struct MlcWriteWalk {
    using Step = void (*)(MlcWriteWalk&, const Schema*, void*, const void*, size_t);
    struct Frame {
        Step step = nullptr;
        const Schema* schema = nullptr;
        void* dest = nullptr;
        const void* obj = nullptr;
        size_t idx = 0;
        bool env_pushed = false;
    };
    std::vector<Frame> stack;
    Frame cur;
    std::vector<std::shared_ptr<void>> keep;
    void** cursor;
    bool direct;

    MlcWriteWalk(const Schema* root, void** cursor_)
        : cursor(cursor_), direct(!mlc_schema_has_recur(root)) {}

    bool flat(const Schema* schema) const {
        return direct || !mlc_schema_has_recur(schema);
    }

    template<typename T>
    static void thunk(MlcWriteWalk& w, const Schema* s, void* dest, const void* obj, size_t idx) {
        MlcNode<T>::write_step(w, s, dest, *static_cast<const T*>(obj), idx);
    }

    // Write a child into its slot: a leaf now, a compound node by call or
    // by frame.
    template<typename T>
    void child(const Schema* schema, void* dest, const T& v) {
        if constexpr (mlc_is_leaf_v<T>) {
            mlc_leaf_write(dest, cursor, resolve_recur(schema), v);
        } else if (flat(schema)) {
            MlcNode<T>::write_step(*this, schema, dest, v, 0);
        } else {
            stack.push_back(Frame{&thunk<T>, schema, dest, &v, 0, false});
        }
    }

    void resume(size_t idx) {
        Frame f = cur;
        f.idx = idx;
        f.env_pushed = true;
        stack.push_back(f);
    }

    // Take an aligned slot of `inner`'s width from the cursor.
    void* alloc(const Schema* inner) {
        size_t align = schema_alignment_cpp(inner);
        if (align == 0) align = 1;
        *cursor = reinterpret_cast<void*>(ALIGN_UP(reinterpret_cast<uintptr_t>(*cursor), align));
        void* slot = *cursor;
        *cursor = static_cast<char*>(slot) + inner->width;
        return slot;
    }

    // A variant slot for an arm with fields: the tag, determined padding,
    // and a pointer to the payload written at the cursor.
    template<typename T>
    void variant_payload(void* dest, const Schema* arm, uint8_t tag, const T& payload) {
        *((uint8_t*)dest) = tag;
        memset((char*)dest + 1, 0, MORLOC_VARIANT_PAYLOAD - 1);
        const Schema* a = resolve_recur(arm);
        void* slot = alloc(a);
        *((relptr_t*)((char*)dest + MORLOC_VARIANT_PAYLOAD)) =
            abs2rel_cpp(static_cast<absptr_t>(slot));
        child(a, slot, payload);
    }

    void run() {
        MLC_WALK_RUN({
            f.step(*this, s, f.dest, f.obj, f.idx);
        })
    }
};

struct MlcReadWalk {
    using Step = void (*)(MlcReadWalk&, const Schema*, const void*, void*, size_t);
    struct Frame {
        Step step = nullptr;
        const Schema* schema = nullptr;
        const void* data = nullptr;
        void* out = nullptr;
        size_t idx = 0;
        bool env_pushed = false;
    };
    std::vector<Frame> stack;
    Frame cur;
    std::vector<std::shared_ptr<void>> keep;
    const void* base_ptr;
    bool direct;

    MlcReadWalk(const Schema* root, const void* base)
        : base_ptr(base), direct(!mlc_schema_has_recur(root)) {}

    bool flat(const Schema* schema) const {
        return direct || !mlc_schema_has_recur(schema);
    }

    // Run `step` on (data, out) once every frame above it is done: pushed
    // before the children it waits for.
    void after(Step step, const Schema* schema, const void* data, void* out) {
        stack.push_back(Frame{step, schema, data, out, 0, false});
    }

    template<typename T>
    static void thunk(MlcReadWalk& w, const Schema* s, const void* data, void* out, size_t idx) {
        MlcNode<T>::read_step(w, s, data, static_cast<T*>(out), idx);
    }

    // Read a child into a destination that already exists: a leaf now, a
    // compound node by call or by frame.
    template<typename T>
    void child(const Schema* schema, const void* data, T* out) {
        if constexpr (mlc_is_leaf_v<T>) {
            *out = mlc_leaf_read<T>(resolve_recur(schema), data, base_ptr);
        } else if (flat(schema)) {
            MlcNode<T>::read_step(*this, schema, data, out, 0);
        } else {
            stack.push_back(Frame{&thunk<T>, schema, data, out, 0, false});
        }
    }

    void resume(size_t idx) {
        Frame f = cur;
        f.idx = idx;
        f.env_pushed = true;
        stack.push_back(f);
    }

    // The payload of a variant slot whose tag selected arm `arm`.
    template<typename T>
    void variant_payload(const Schema* arm, const void* data, T* out) {
        relptr_t rel = *(const relptr_t*)((const char*)data + MORLOC_VARIANT_PAYLOAD);
        child(arm, resolve_relptr_cpp(rel, base_ptr), out);
    }

    void run() {
        MLC_WALK_RUN({
            f.step(*this, s, f.data, f.out, f.idx);
        })
    }
};

#undef MLC_WALK_RUN

// ---- Variant slots (payload-bearing `data`) --------------------------------
//
// A variant is a tag byte, seven bytes of padding, and a relative pointer to
// the arm's fields -- Optional's slot shape with a tag in front. The walks
// above carry the payload; these cover the nullary arm and the tag.

inline void write_variant_nullary(void* dest, uint8_t tag) {
    *((uint8_t*)dest) = tag;
    memset((char*)dest + 1, 0, MORLOC_VARIANT_PAYLOAD - 1);
    *((relptr_t*)((char*)dest + MORLOC_VARIANT_PAYLOAD)) = RELNULL;
}

inline uint8_t read_variant_tag(const void* data) {
    return *((const uint8_t*)data);
}

// ------------------------------------------------------------
// Public entry points
// ------------------------------------------------------------

template<typename T>
size_t get_shm_size(const Schema* schema, const T& data) {
    MlcSizeWalk w(schema);
    w.child(schema, data, false);
    return static_cast<size_t>(w.run());
}

// Write `data` into the slot at `dest`, appending variable-length parts at
// the cursor.
template<typename T>
void* to_voidstar(void* dest, void** cursor, const Schema* schema, const T& data) {
    MlcWriteWalk w(schema, cursor);
    w.child(schema, dest, data);
    w.run();
    return dest;
}

// Allocate a block for `data` in shared memory and write it there.
template<typename T>
void* to_voidstar(const Schema* schema, const T& data){
    size_t total_size = get_shm_size(schema, data);
    void* dest = shmalloc_cpp(total_size);
    void* cursor = (void*)((char*)dest + schema->width);
    try {
        return to_voidstar(dest, &cursor, schema, data);
    } catch (...) {
        shfree_cpp(dest);
        throw;
    }
}

template<typename T>
T from_voidstar(const Schema* schema, const void* data, T* = nullptr, const void* base_ptr = nullptr) {
    if(data == NULL){
        throw std::runtime_error("Void error in from_voidstar");
    }
    T out{};
    MlcReadWalk w(schema, base_ptr);
    w.child(schema, data, &out);
    w.run();
    return out;
}

// ------------------------------------------------------------
// Standard-library nodes
// ------------------------------------------------------------

// Fixed-width element schema: the array's data region is n * width bytes.
inline bool mlc_elem_fixed_width(const Schema* elem) {
    switch (elem->type) {
        case MORLOC_NIL: case MORLOC_BOOL: case MORLOC_ENUM:
        case MORLOC_SINT8: case MORLOC_SINT16: case MORLOC_SINT32: case MORLOC_SINT64:
        case MORLOC_UINT8: case MORLOC_UINT16: case MORLOC_UINT32: case MORLOC_UINT64:
        case MORLOC_FLOAT32: case MORLOC_FLOAT64:
            return true;
        default:
            return false;
    }
}

inline bool mlc_elem_is_handle(const Schema* elem) {
    return elem->type == MORLOC_IFILE
        || elem->type == MORLOC_OSTREAM
        || elem->type == MORLOC_ISTREAM;
}

template<typename Tuple, size_t... Is>
void mlc_tuple_size(MlcSizeWalk& w, const Schema* schema, const Tuple& data, std::index_sequence<Is...>) {
    w.total += static_cast<int64_t>(schema->width);
    (w.child(schema->parameters[Is], std::get<Is>(data), true), ...);
}

template<typename Tuple, size_t... Is>
void mlc_tuple_write(MlcWriteWalk& w, const Schema* schema, void* dest, const Tuple& data, std::index_sequence<Is...>) {
    (w.child(schema->parameters[Is], (char*)dest + schema->offsets[Is], std::get<Is>(data)), ...);
}

template<typename Tuple, size_t... Is>
void mlc_tuple_read(MlcReadWalk& w, const Schema* schema, const void* data, Tuple* out, std::index_sequence<Is...>) {
    (w.child(schema->parameters[Is], (const char*)data + schema->offsets[Is], &std::get<Is>(*out)), ...);
}

// A type with no node reached the walk. Generated code can name a marshaller
// it never calls (a record whose Packable field crosses field-wise, as a
// tuple), so this is a runtime failure rather than a compile-time one.
[[noreturn]] inline void mlc_no_marshaller(const Schema* schema) {
    std::ostringstream oss;
    oss << "no marshaller for a value of this C++ type (schema type "
        << (schema ? (int)schema->type : -1) << ")";
    throw std::runtime_error(oss.str());
}

// Move the elements read into a vector into a non-vector container.
template<typename T>
void mlc_container_fill(MlcReadWalk&, const Schema*, const void* data, void* out, size_t) {
    using ElemT = typename T::value_type;
    auto& elems = *static_cast<std::vector<ElemT>*>(const_cast<void*>(data));
    T result;
    constexpr bool reverse = is_std_stack<T>::value || is_std_forward_list<T>::value;
    if constexpr (reverse) {
        for (size_t i = elems.size(); i > 0; --i) {
            if constexpr (is_std_stack<T>::value) result.push(std::move(elems[i-1]));
            else result.push_front(std::move(elems[i-1]));
        }
    } else {
        for (size_t i = 0; i < elems.size(); ++i) {
            if constexpr (is_std_queue<T>::value) result.push(std::move(elems[i]));
            else result.push_back(std::move(elems[i]));
        }
    }
    *static_cast<T*>(out) = std::move(result);
}

template<typename T>
struct MlcNode {
    static_assert(!mlc_is_leaf_v<T>, "a leaf has no node");

    static void size_step(MlcSizeWalk& w, const Schema* schema, const T& data, size_t idx) {
        if constexpr (is_std_vector<T>::value) {
            using ElemT = typename T::value_type;
            const Schema* elem = resolve_recur(schema->parameters[0]);
            if (idx == 0) {
                // The header, worst-case cursor alignment for the data
                // region, and the fixed part of every element.
                w.total += static_cast<int64_t>(schema->width + array_data_alignment_cpp(elem) - 1);
                if (mlc_elem_fixed_width(elem)) {
                    w.total += static_cast<int64_t>(data.size() * elem->width);
                    return;
                }
                if (mlc_elem_is_handle(elem)) {
                    // Batched suballoc-size lookup amortises the registry
                    // lock across N handles.
                    if constexpr (std::is_same_v<ElemT, int64_t>) {
                        char* err = NULL;
                        int64_t paths_total = mlc_handles_path_lens(
                            data.data(), data.size(), nullptr, &err);
                        if (paths_total < 0) {
                            std::string msg = err ? err : "mlc_handles_path_lens failed";
                            free(err);
                            throw std::runtime_error(msg);
                        }
                        w.total += static_cast<int64_t>(data.size() * elem->width
                                                        + static_cast<size_t>(paths_total));
                        return;
                    }
                }
                if constexpr (mlc_is_leaf_v<ElemT>) {
                    for (size_t i = 0; i < data.size(); ++i) {
                        w.total += static_cast<int64_t>(mlc_leaf_size(elem, data[i]));
                    }
                    return;
                }
            }
            if constexpr (!mlc_is_leaf_v<ElemT>) {
                if (w.flat(elem)) {
                    for (size_t i = 0; i < data.size(); ++i) w.child(elem, data[i], false);
                } else if (idx < data.size()) {
                    if (idx + 1 < data.size()) w.resume(idx + 1);
                    w.child(elem, data[idx], false);
                }
            }
        } else if constexpr (is_non_vector_container_v<T>) {
            // The container is walked as the vector of its elements, which
            // the walk keeps alive for as long as its frames refer to it.
            auto v = std::make_shared<decltype(to_vector(data))>(to_vector(data));
            w.keep.push_back(v);
            w.child(schema, *v, false);
        } else if constexpr (is_std_tuple<T>::value) {
            mlc_tuple_size(w, schema, data, std::make_index_sequence<std::tuple_size_v<T>>{});
        } else if constexpr (is_std_pair<T>::value) {
            mlc_tuple_size(w, schema, data, std::index_sequence<0, 1>{});
        } else if constexpr (is_std_optional<T>::value || is_std_shared_ptr<T>::value) {
            // The slot is a relptr. Present: the slot, worst-case padding
            // for the inner T, and T's full size.
            if (!data) {
                w.total += static_cast<int64_t>(schema->width);
            } else {
                const Schema* inner = resolve_recur(schema->parameters[0]);
                size_t align = schema_alignment_cpp(inner);
                if (align == 0) align = 1;
                w.total += static_cast<int64_t>(schema->width + (align - 1));
                w.child(inner, *data, false);
            }
        } else {
            mlc_no_marshaller(schema);
        }
    }

    static void write_step(MlcWriteWalk& w, const Schema* schema, void* dest, const T& data, size_t idx) {
        if constexpr (is_std_vector<T>::value) {
            using ElemT = typename T::value_type;
            Array* result = static_cast<Array*>(dest);
            const Schema* elem = resolve_recur(schema->parameters[0]);
            size_t width = elem->width;
            if (idx == 0) {
                result->size = data.size();
                if (data.size() == 0) {
                    result->data = RELNULL;
                    return;
                }
                // The data region: aligned (64 for primitive numerics), one
                // fixed slot per element; tails follow at the cursor.
                *w.cursor = reinterpret_cast<void*>(ALIGN_UP(reinterpret_cast<uintptr_t>(*w.cursor), array_data_alignment_cpp(elem)));
                result->data = abs2rel_cpp(static_cast<absptr_t>(*w.cursor));
                *w.cursor = static_cast<char*>(*w.cursor) + data.size() * width;
                char* start = (char*)rel2abs_cpp(result->data);
                if (mlc_elem_is_handle(elem)) {
                    // Batched stream-handle write: one registry lock for all N handles.
                    if constexpr (std::is_same_v<ElemT, int64_t>) {
                        char* err = NULL;
                        if (mlc_write_handles_voidstar(
                                data.data(), data.size(), start, width, w.cursor, &err) != 0) {
                            std::string msg = err ? err : "mlc_write_handles_voidstar failed";
                            free(err);
                            throw std::runtime_error(msg);
                        }
                        return;
                    }
                }
                if constexpr (std::is_arithmetic_v<ElemT> && !std::is_same_v<ElemT, bool>) {
                    // A vector of a fixed-width primitive packs identically to
                    // the wire layout: one memcpy instead of a call per element.
                    if (vector_is_bulk_copyable<ElemT>(elem)) {
                        std::memcpy(start, data.data(), data.size() * width);
                        return;
                    }
                }
                if constexpr (mlc_is_leaf_v<ElemT>) {
                    for (size_t i = 0; i < data.size(); ++i) {
                        if constexpr (std::is_same_v<ElemT, bool>) {
                            // std::vector<bool>::operator[] is a bit proxy.
                            mlc_leaf_write(start + width * i, w.cursor, elem, static_cast<bool>(data[i]));
                        } else {
                            mlc_leaf_write(start + width * i, w.cursor, elem, data[i]);
                        }
                    }
                    return;
                }
            }
            if constexpr (!mlc_is_leaf_v<ElemT>) {
                char* start = (char*)rel2abs_cpp(result->data);
                if (w.flat(elem)) {
                    for (size_t i = 0; i < data.size(); ++i) w.child(elem, start + width * i, data[i]);
                } else {
                    if (idx + 1 < data.size()) w.resume(idx + 1);
                    w.child(elem, start + width * idx, data[idx]);
                }
            }
        } else if constexpr (is_non_vector_container_v<T>) {
            auto v = std::make_shared<decltype(to_vector(data))>(to_vector(data));
            w.keep.push_back(v);
            w.child(schema, dest, *v);
        } else if constexpr (is_std_tuple<T>::value) {
            mlc_tuple_write(w, schema, dest, data, std::make_index_sequence<std::tuple_size_v<T>>{});
        } else if constexpr (is_std_pair<T>::value) {
            mlc_tuple_write(w, schema, dest, data, std::index_sequence<0, 1>{});
        } else if constexpr (is_std_optional<T>::value || is_std_shared_ptr<T>::value) {
            // Absent -> RELNULL. Present -> an aligned slot for the inner T at
            // the cursor, its relptr in this slot, then T's body.
            if (!data) {
                *((relptr_t*)dest) = RELNULL;
            } else {
                const Schema* inner = resolve_recur(schema->parameters[0]);
                void* slot = w.alloc(inner);
                *(relptr_t*)dest = abs2rel_cpp(static_cast<absptr_t>(slot));
                w.child(inner, slot, *data);
            }
        } else {
            mlc_no_marshaller(schema);
        }
    }

    static void read_step(MlcReadWalk& w, const Schema* schema, const void* data, T* out, size_t idx) {
        if constexpr (is_std_vector<T>::value) {
            using ElemT = typename T::value_type;
            const Array* array = (const Array*)data;
            const Schema* elem = resolve_recur(schema->parameters[0]);
            if (idx == 0) {
                if (array->size == 0) {
                    out->clear();
                    return;
                }
                const char* start = (const char*)resolve_relptr_cpp(array->data, w.base_ptr);
                if constexpr (std::is_arithmetic_v<ElemT> && !std::is_same_v<ElemT, bool>) {
                    // Fixed-width primitives whose C++ width matches the
                    // wire width are one bulk copy; bool is excluded
                    // because a wire byte outside {0,1} must be normalised.
                    if (mlc_elem_fixed_width(elem) && sizeof(ElemT) == elem->width) {
                        const ElemT* first = (const ElemT*)start;
                        out->assign(first, first + array->size);
                        return;
                    }
                }
                out->resize(array->size);
                if constexpr (mlc_is_leaf_v<ElemT>) {
                    for (size_t i = 0; i < array->size; i++) {
                        (*out)[i] = mlc_leaf_read<ElemT>(elem, start + i * elem->width, w.base_ptr);
                    }
                    return;
                }
            }
            if constexpr (!mlc_is_leaf_v<ElemT>) {
                const char* start = (const char*)resolve_relptr_cpp(array->data, w.base_ptr);
                if (w.flat(elem)) {
                    for (size_t i = 0; i < array->size; ++i) w.child(elem, start + i * elem->width, &(*out)[i]);
                } else {
                    if (idx + 1 < array->size) w.resume(idx + 1);
                    w.child(elem, start + idx * elem->width, &(*out)[idx]);
                }
            }
        } else if constexpr (is_non_vector_container_v<T>) {
            // Read into a kept vector, then move its elements into the
            // container once they are all in.
            using ElemT = typename T::value_type;
            auto v = std::make_shared<std::vector<ElemT>>();
            w.keep.push_back(v);
            w.after(&mlc_container_fill<T>, schema, v.get(), out);
            w.child(schema, data, v.get());
        } else if constexpr (is_std_tuple<T>::value) {
            mlc_tuple_read(w, schema, data, out, std::make_index_sequence<std::tuple_size_v<T>>{});
        } else if constexpr (is_std_pair<T>::value) {
            mlc_tuple_read(w, schema, data, out, std::index_sequence<0, 1>{});
        } else if constexpr (is_std_optional<T>::value) {
            relptr_t relptr = *(const relptr_t*)data;
            if (relptr == RELNULL) {
                out->reset();
            } else {
                out->emplace();
                w.child(schema->parameters[0], resolve_relptr_cpp(relptr, w.base_ptr), &**out);
            }
        } else if constexpr (is_std_shared_ptr<T>::value) {
            // shared_ptr<T> is the C++ surface form for `?T` at a recursive
            // cycle. The wire slot is a single relptr (RELNULL == absent).
            using PointeeT = typename T::element_type;
            relptr_t relptr = *(const relptr_t*)data;
            if (relptr == RELNULL) {
                out->reset();
            } else {
                *out = std::make_shared<PointeeT>();
                w.child(schema->parameters[0], resolve_relptr_cpp(relptr, w.base_ptr), out->get());
            }
        } else {
            mlc_no_marshaller(schema);
        }
    }
};

// ============================================================
// mpk_pack / mpk_unpack
// ============================================================

template<typename T>
std::vector<char> mpk_pack(const T& data, const std::string& schema_str) {
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema_cpp(schema_ptr);

    void* voidstar = nullptr;
    char* msgpack_data = NULL;
    size_t msg_size = 0;

    try {
        voidstar = to_voidstar(schema, data);
        pack_with_schema_cpp(voidstar, schema, &msgpack_data, &msg_size);
    } catch (...) {
        if (voidstar) shfree_cpp(voidstar);
        free(msgpack_data);
        free_schema(schema);
        throw;
    }

    shfree_cpp(voidstar);

    std::vector<char> result(msgpack_data, msgpack_data + msg_size);
    free(msgpack_data);
    free_schema(schema);

    return result;
}

template<typename T>
T mpk_unpack(const std::vector<char>& packed_data, const std::string& schema_str) {
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema_cpp(schema_ptr);

    void* voidstar = nullptr;
    int unpack_result = unpack_with_schema_cpp(packed_data.data(), packed_data.size(), schema, &voidstar);
    if (unpack_result != 0) {
        free_schema(schema);
        throw std::runtime_error("Unpacking failed");
    }

    T x;
    try {
        x = from_voidstar(schema, voidstar, static_cast<T*>(nullptr));
    } catch (...) {
        free_schema(schema);
        shfree_cpp(voidstar);
        throw;
    }

    free_schema(schema);
    shfree_cpp(voidstar);

    return x;
}

#endif
