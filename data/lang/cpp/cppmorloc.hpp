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

absptr_t cpp_rel2abs(relptr_t ptr);
relptr_t abs2rel_cpp(absptr_t ptr);

// Resolve a relative pointer using either base-pointer arithmetic (inline data)
// or SHM. When base_ptr is non-null, data lives in a contiguous malloc'd blob.
static inline void* resolve_relptr_cpp(relptr_t relptr, const void* base_ptr) {
    if (base_ptr) {
        return (char*)base_ptr + relptr;
    }
    return cpp_rel2abs(relptr);
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
// onto a thread-local stack. RecurEnvScope provides RAII push/pop so
// nested entries (vectors, optionals, tuples) compose cleanly with
// exception unwinding.
struct CppRecurEntry {
    const char* name;
    const Schema* schema;
};

inline std::vector<CppRecurEntry>& cpp_recur_env() {
    thread_local std::vector<CppRecurEntry> stack;
    return stack;
}

inline const Schema* recur_env_lookup_cpp(const char* name) {
    if (name == nullptr) return nullptr;
    auto& stack = cpp_recur_env();
    for (auto it = stack.rbegin(); it != stack.rend(); ++it) {
        if (it->name != nullptr && std::strcmp(it->name, name) == 0) {
            return it->schema;
        }
    }
    return nullptr;
}

struct RecurEnvScope {
    bool pushed;
    explicit RecurEnvScope(const Schema* schema) : pushed(false) {
        if (schema != nullptr
            && schema->name != nullptr
            && schema->type != MORLOC_RECUR) {
            cpp_recur_env().push_back({schema->name, schema});
            pushed = true;
        }
    }
    ~RecurEnvScope() {
        if (pushed) {
            auto& s = cpp_recur_env();
            if (!s.empty()) s.pop_back();
        }
    }
    RecurEnvScope(const RecurEnvScope&) = delete;
    RecurEnvScope& operator=(const RecurEnvScope&) = delete;
};

// Resolve a Recur schema to the named declaration on the env stack.
// Returns the original schema unchanged when not a Recur (so callers
// can use it as an unconditional normaliser at the top of a walker).
// Throws when a Recur is unresolvable, since downstream reads would
// otherwise dereference an empty parameter list.
inline const Schema* resolve_recur_cpp(const Schema* schema) {
    if (schema == nullptr || schema->type != MORLOC_RECUR) {
        return schema;
    }
    const Schema* target = recur_env_lookup_cpp(schema->name);
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

// Alignment for an Array's element data buffer in SHM. For primitive numerics
// we bump to MORLOC_ARRAY_DATA_ALIGN (SIMD/BLAS); otherwise use the element's
// natural alignment.
inline size_t array_data_alignment_cpp(const Schema* elem) {
    size_t natural = schema_alignment_cpp(elem);
    return is_primitive_numeric_cpp(elem)
        ? (MORLOC_ARRAY_DATA_ALIGN > natural ? MORLOC_ARRAY_DATA_ALIGN : natural)
        : natural;
}


// ============================================================
// get_shm_size
// ============================================================

// Forward declaration
template<typename T>
size_t get_shm_size(const Schema* schema, const T& data);

size_t get_shm_size(const Schema* schema, const std::nullptr_t&) {
    return sizeof(int8_t);
}

// Primitives
template<typename Primitive>
size_t get_shm_size(const Schema* schema, const Primitive& data) {
    if (schema->type == MORLOC_INT) {
        // Inline BigInt: [size, value] = 16 bytes (C++ values always fit inline)
        return 16;
    }
    return schema->width;
}

template<typename T>
size_t get_shm_size(const Schema* schema, const std::vector<T>& data) {
    size_t total_size = schema->width;
    const Schema* elem_schema = resolve_recur_cpp(schema->parameters[0]);
    // worst-case cursor alignment padding for element data
    total_size += array_data_alignment_cpp(elem_schema) - 1;
    switch(elem_schema->type){
        case MORLOC_NIL:
        case MORLOC_BOOL:
        case MORLOC_SINT8:
        case MORLOC_SINT16:
        case MORLOC_SINT32:
        case MORLOC_SINT64:
        case MORLOC_UINT8:
        case MORLOC_UINT16:
        case MORLOC_UINT32:
        case MORLOC_UINT64:
        case MORLOC_FLOAT32:
        case MORLOC_FLOAT64:
            total_size += data.size() * elem_schema->width;
            break;
        case MORLOC_INT:
        case MORLOC_STRING:
        case MORLOC_ARRAY:
        case MORLOC_TUPLE:
        case MORLOC_MAP:
        case MORLOC_OPTIONAL:
            for(size_t i = 0; i < data.size(); i++){
               total_size += get_shm_size(elem_schema, data[i]);
            }
            break;
        default:
            // Recur / Table / unhandled: size depends on element kind that
            // we cannot pre-compute structurally here. Fall back to the
            // per-element walker which will resolve and recurse properly.
            for(size_t i = 0; i < data.size(); i++){
               total_size += get_shm_size(elem_schema, data[i]);
            }
            break;
    }
    return total_size;
}

// Optional: slot is a relptr (schema->width = sizeof(relptr_t)). Absent
// values contribute only the slot. Present values contribute the slot,
// worst-case alignment padding for the inner T, and T's full size
// (including any of T's own variable-length sub-data).
template<typename T>
size_t get_shm_size(const Schema* schema, const std::optional<T>& data) {
    if (!data.has_value()) {
        return schema->width;
    }
    const Schema* inner_schema = resolve_recur_cpp(schema->parameters[0]);
    size_t inner_size = get_shm_size(inner_schema, *data);
    size_t inner_align = schema_alignment_cpp(inner_schema);
    if (inner_align == 0) inner_align = 1;
    return schema->width + (inner_align - 1) + inner_size;
}

// shared_ptr<T>: the C++ surface form for `?T` at a recursive cycle
// position. Mirrors the optional<T> wire-format handling exactly --
// the schema is still the `?T` schema, and the slot is still a single
// relptr. nullptr == absent (RELNULL); non-null == present with the
// pointee occupying the inner slot.
template<typename T>
size_t get_shm_size(const Schema* schema, const std::shared_ptr<T>& data) {
    if (!data) {
        return schema->width;
    }
    const Schema* inner_schema = resolve_recur_cpp(schema->parameters[0]);
    size_t inner_size = get_shm_size(inner_schema, *data);
    size_t inner_align = schema_alignment_cpp(inner_schema);
    if (inner_align == 0) inner_align = 1;
    return schema->width + (inner_align - 1) + inner_size;
}

size_t get_shm_size(const Schema* schema, const std::string& data) {
    return schema->width + data.size();
}

size_t get_shm_size(void* dest, const Schema* schema, const char* data) {
    return schema->width + strlen(data);
}

template<typename Tuple, size_t... Is>
size_t createTupleShmSizeHelper(const Schema* schema, const Tuple& data, std::index_sequence<Is...>) {
    size_t total_size = schema->width;
    (void)std::initializer_list<int>{(
        [&](){
            const Schema* field_schema = resolve_recur_cpp(schema->parameters[Is]);
            size_t elem = get_shm_size(field_schema, std::get<Is>(data));
            if (elem > field_schema->width) {
                total_size += elem - field_schema->width;
            }
        }(),
        0
    )...};
    return total_size;
}

template<typename... Args>
size_t get_shm_size(const Schema* schema, const std::tuple<Args...>& data) {
    return createTupleShmSizeHelper(schema, data, std::index_sequence_for<Args...>{});
}

// Non-vector containers: convert to vector and delegate
template<typename T>
size_t get_shm_size(const Schema* schema, const std::list<T>& data) {
    return get_shm_size(schema, to_vector(data));
}

template<typename T>
size_t get_shm_size(const Schema* schema, const std::forward_list<T>& data) {
    return get_shm_size(schema, to_vector(data));
}

template<typename T>
size_t get_shm_size(const Schema* schema, const std::deque<T>& data) {
    return get_shm_size(schema, to_vector(data));
}

template<typename T>
size_t get_shm_size(const Schema* schema, const std::stack<T>& data) {
    return get_shm_size(schema, to_vector(data));
}

template<typename T>
size_t get_shm_size(const Schema* schema, const std::queue<T>& data) {
    return get_shm_size(schema, to_vector(data));
}


// ============================================================
// toAnything - top-level (allocating)
// ============================================================

// Generic top-level: compute size, allocate, serialize
template<typename T>
void* toAnything(const Schema* schema, const T& data){
    // Push the top schema's name onto the recur env so any back-ref
    // encountered during the walk resolves to the matching declaration.
    // RAII pops on every return path.
    RecurEnvScope _recur_top(schema);
    size_t total_size = get_shm_size(schema, data);
    void* dest = shmalloc_cpp(total_size);
    void* cursor = (void*)((char*)dest + schema->width);
    try {
        return toAnything(dest, &cursor, schema, data);
    } catch (...) {
        shfree_cpp(dest);
        throw;
    }
}

// Non-vector containers: convert to vector and delegate
template<typename T>
void* toAnything(const Schema* schema, const std::stack<T>& data) {
    return toAnything(schema, to_vector(data));
}

template<typename T>
void* toAnything(const Schema* schema, const std::forward_list<T>& data) {
    return toAnything(schema, to_vector(data));
}

template<typename T>
void* toAnything(const Schema* schema, const std::queue<T>& data) {
    return toAnything(schema, to_vector(data));
}

template<typename T>
void* toAnything(const Schema* schema, const std::deque<T>& data) {
    return toAnything(schema, to_vector(data));
}

template<typename T>
void* toAnything(const Schema* schema, const std::list<T>& data) {
    return toAnything(schema, to_vector(data));
}


// ============================================================
// toAnything - cursor-based (recursive)
// ============================================================

// Forward declaration
template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const T& data);

// Write raw binary data as an array
void* binarytoAnything(void* dest, void** cursor, const Schema* schema, const uint8_t* data, size_t size) {
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

void* toAnything(void* dest, void** cursor, const Schema* schema, const std::nullptr_t&) {
    *((int8_t*)dest) = (int8_t)0;
    return dest;
}

// Range-check a value before narrowing to a fixed-width wire integer.
// Catches the case where a C++ source produces a value too large for the
// declared morloc type (e.g. `int` returning 200 declared as `Int8`),
// which would otherwise truncate silently into the wire payload.
template<typename Wire, typename Src>
Wire checkRangeNarrow(const Src& data, const char* name) {
    if constexpr (std::is_arithmetic_v<Src>) {
        using SLim = std::numeric_limits<Wire>;
        if constexpr (std::is_signed_v<Src>) {
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

// Primitives — always write at schema width so the wire format matches
// the morloc type regardless of the C++ concrete type width.
// Also instantiated for record types (which fall through to default).
template<typename Primitive>
void* toAnything(void* dest, void** cursor, const Schema* schema, const Primitive& data) {
    if constexpr (std::is_arithmetic_v<Primitive>) {
        switch(schema->type) {
            case MORLOC_SINT8:   *(int8_t*)dest   = checkRangeNarrow<int8_t>(data, "Int8");    break;
            case MORLOC_SINT16:  *(int16_t*)dest  = checkRangeNarrow<int16_t>(data, "Int16");  break;
            case MORLOC_SINT32:  *(int32_t*)dest  = checkRangeNarrow<int32_t>(data, "Int32");  break;
            case MORLOC_SINT64:  *(int64_t*)dest  = static_cast<int64_t>(data);   break;
            case MORLOC_UINT8:   *(uint8_t*)dest  = checkRangeNarrow<uint8_t>(data, "UInt8");   break;
            case MORLOC_UINT16:  *(uint16_t*)dest = checkRangeNarrow<uint16_t>(data, "UInt16"); break;
            case MORLOC_UINT32:  *(uint32_t*)dest = checkRangeNarrow<uint32_t>(data, "UInt32"); break;
            case MORLOC_UINT64:  *(uint64_t*)dest = static_cast<uint64_t>(data);  break;
            case MORLOC_FLOAT32: *(float*)dest    = static_cast<float>(data);    break;
            case MORLOC_FLOAT64: *(double*)dest   = static_cast<double>(data);   break;
            case MORLOC_INT: {
                // Inline BigInt: [size=1, value] — no allocation, no relptr
                int64_t* fields = static_cast<int64_t*>(dest);
                fields[0] = 1;
                fields[1] = static_cast<int64_t>(data);
                break;
            }
            default: *(Primitive*)dest = data; break;
        }
    } else {
        *(Primitive*)dest = data;
    }
    return dest;
}

// Vector (primary array implementation)
template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::vector<T>& data) {
    Array* result = static_cast<Array*>(dest);
    result->size = data.size();
    if(data.size() == 0){
        result->data = RELNULL;
        return dest;
    }
    // Resolve a Recur element schema before descending so any
    // user-generated struct overload sees the named declaration's
    // parameters / offsets, not the empty back-ref node.
    const Schema* elem_schema = resolve_recur_cpp(schema->parameters[0]);
    // align cursor for element data placement (bumps to 64 for primitive numerics)
    *cursor = reinterpret_cast<void*>(ALIGN_UP(reinterpret_cast<uintptr_t>(*cursor), array_data_alignment_cpp(elem_schema)));
    result->data = abs2rel_cpp(static_cast<absptr_t>(*cursor));
    *cursor = static_cast<char*>(*cursor) + data.size() * elem_schema->width;
    char* start = (char*)cpp_rel2abs(result->data);
    size_t width = elem_schema->width;
    for (size_t i = 0; i < data.size(); ++i) {
         toAnything(start + width * i, cursor, elem_schema, data[i]);
    }
    return dest;
}

// Shared helper for iterable containers (list, forward_list, deque)
template<typename Container>
void* toAnything_seq(void* dest, void** cursor, const Schema* schema, const Container& data, size_t size) {
    Array* result = static_cast<Array*>(dest);
    result->size = size;
    if(size == 0){
        result->data = RELNULL;
        return dest;
    }
    const Schema* elem_schema = resolve_recur_cpp(schema->parameters[0]);
    // align cursor for element data placement (bumps to 64 for primitive numerics)
    *cursor = reinterpret_cast<void*>(ALIGN_UP(reinterpret_cast<uintptr_t>(*cursor), array_data_alignment_cpp(elem_schema)));
    result->data = abs2rel_cpp(static_cast<absptr_t>(*cursor));
    *cursor = static_cast<char*>(*cursor) + size * elem_schema->width;
    char* start = (char*)cpp_rel2abs(result->data);
    size_t width = elem_schema->width;
    size_t i = 0;
    for (const auto& item : data) {
        toAnything(start + width * i, cursor, elem_schema, item);
        ++i;
    }
    return dest;
}

template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::list<T>& data) {
    return toAnything_seq(dest, cursor, schema, data, data.size());
}

template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::forward_list<T>& data) {
    return toAnything_seq(dest, cursor, schema, data, std::distance(data.begin(), data.end()));
}

template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::deque<T>& data) {
    return toAnything_seq(dest, cursor, schema, data, data.size());
}

// Stack and queue: convert to vector and delegate
template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::queue<T>& data) {
    return toAnything(dest, cursor, schema, to_vector(data));
}

template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::stack<T>& data) {
    return toAnything(dest, cursor, schema, to_vector(data));
}

// String and C string
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::string& data) {
    return binarytoAnything(dest, cursor, schema, (const uint8_t*)data.c_str(), data.size());
}

void* toAnything(void* dest, void** cursor, const Schema* schema, const char* data) {
    return binarytoAnything(dest, cursor, schema, (const uint8_t*)data, strlen(data));
}

// Tuple
template<typename Tuple, size_t... Is>
void* createTupleAnythingHelper(void* dest, const Schema* schema, void** cursor, const Tuple& data, std::index_sequence<Is...>) {
    // Each element's schema may be a Recur back-reference; resolve
    // before dispatch so user-generated overloads see the target.
    (void)std::initializer_list<int>{(
        toAnything((char*)dest + schema->offsets[Is], cursor, resolve_recur_cpp(schema->parameters[Is]), std::get<Is>(data)),
        0
    )...};
    return dest;
}

template<typename... Args>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::tuple<Args...>& data) {
    return createTupleAnythingHelper(dest, schema, cursor, data, std::index_sequence_for<Args...>{});
}

// Pair (reuses tuple helper since std::pair supports std::get)
template<typename A, typename B>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::pair<A, B>& data) {
    return createTupleAnythingHelper(dest, schema, cursor, data, std::index_sequence<0, 1>{});
}

// Optional
//
// The Optional slot is a single relptr. Absent → RELNULL. Present →
// align the cursor for the inner T, write its relptr into the slot,
// advance the cursor past T's width, then recurse to populate T.
template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::optional<T>& data) {
    if (!data.has_value()) {
        *((relptr_t*)dest) = RELNULL;
    } else {
        const Schema* inner_schema = resolve_recur_cpp(schema->parameters[0]);
        size_t inner_align = schema_alignment_cpp(inner_schema);
        if (inner_align == 0) inner_align = 1;
        *cursor = reinterpret_cast<void*>(ALIGN_UP(reinterpret_cast<uintptr_t>(*cursor), inner_align));
        *(relptr_t*)dest = abs2rel_cpp(static_cast<absptr_t>(*cursor));
        void* inner_dest = *cursor;
        *cursor = static_cast<char*>(*cursor) + inner_schema->width;
        toAnything(inner_dest, cursor, inner_schema, *data);
    }
    return dest;
}

// shared_ptr<T>: the C++ surface form for `?T` at a recursive cycle.
// Writes the wire-format relptr at dest (RELNULL when absent),
// allocates the inner slot when present, and recurses into the
// pointee. Mirrors the optional<T> overload above exactly.
template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::shared_ptr<T>& data) {
    if (!data) {
        *((relptr_t*)dest) = RELNULL;
    } else {
        const Schema* inner_schema = resolve_recur_cpp(schema->parameters[0]);
        size_t inner_align = schema_alignment_cpp(inner_schema);
        if (inner_align == 0) inner_align = 1;
        *cursor = reinterpret_cast<void*>(ALIGN_UP(reinterpret_cast<uintptr_t>(*cursor), inner_align));
        *(relptr_t*)dest = abs2rel_cpp(static_cast<absptr_t>(*cursor));
        void* inner_dest = *cursor;
        *cursor = static_cast<char*>(*cursor) + inner_schema->width;
        toAnything(inner_dest, cursor, inner_schema, *data);
    }
    return dest;
}


// ============================================================
// fromAnything - single template with if constexpr dispatch
// ============================================================

// Forward declaration for recursive calls
template<typename T>
T fromAnything(const Schema* schema, const void* data, T* = nullptr, const void* base_ptr = nullptr);

// Tuple helper (needs forward declaration of fromAnything)
template<typename Tuple, size_t... Is>
Tuple fromTupleAnythingHelper(
  const Schema* schema,
  const void* anything,
  std::index_sequence<Is...>,
  Tuple* = nullptr,
  const void* base_ptr = nullptr
) {
    // Resolve each element's schema (it may be a Recur back-reference)
    // so any user-defined struct overload sees the named target's
    // parameters/offsets rather than the empty back-ref node.
    return Tuple(fromAnything(resolve_recur_cpp(schema->parameters[Is]),
                              (char*)anything + schema->offsets[Is],
                              static_cast<std::tuple_element_t<Is, Tuple>*>(nullptr),
                              base_ptr)...);
}

template<typename T>
T fromAnything(const Schema* schema, const void* data, T*, const void* base_ptr) {
    if(data == NULL){
        throw std::runtime_error("Void error in fromAnything");
    }

    // Resolve a back-reference to the named declaration on the env
    // stack before descending. Recur nodes have no parameters/keys, so
    // every downstream read needs the resolved target. Push the
    // resolved schema's name (if any) onto the env so a sub-walk's
    // Recur back-ref finds it.
    schema = resolve_recur_cpp(schema);
    RecurEnvScope _recur_scope(schema);

    if constexpr (std::is_same_v<T, bool>) {
        // NOTE: do NOT use bool here since its width is often not 1 byte
        return *(uint8_t*)data == 1;
    }
    else if constexpr (std::is_same_v<T, std::string>) {
        Array* array = (Array*)data;
        if(array->size > 0){
            return std::string((char*)resolve_relptr_cpp(array->data, base_ptr), array->size);
        }
        return std::string("");
    }
    else if constexpr (is_std_vector<T>::value) {
        using ElemT = typename T::value_type;
        std::vector<ElemT> result;
        Array* array = (Array*)data;
        if(array->size == 0) return result;

        // Resolve a Recur element schema (e.g. vector<tree_t> with
        // schema Array<Recur(Tree)>) to the named declaration before
        // descending. User-generated struct overloads access
        // schema->offsets[] directly and would crash on a Recur.
        const Schema* elem_schema = resolve_recur_cpp(schema->parameters[0]);

        // Fast path for primitive arrays — only when C++ type width
        // matches schema width (e.g. both 8 bytes). When they differ
        // (e.g. int=4 bytes vs i8 schema=8 bytes), fall through to
        // the element-by-element slow path which converts per element.
        switch(elem_schema->type){
            case MORLOC_NIL:
            case MORLOC_BOOL:
            case MORLOC_SINT8:
            case MORLOC_SINT16:
            case MORLOC_SINT32:
            case MORLOC_SINT64:
            case MORLOC_UINT8:
            case MORLOC_UINT16:
            case MORLOC_UINT32:
            case MORLOC_UINT64:
            case MORLOC_FLOAT32:
            case MORLOC_FLOAT64:
                if (sizeof(ElemT) == elem_schema->width) {
                    ElemT* arr_start = (ElemT*)resolve_relptr_cpp(array->data, base_ptr);
                    std::vector<ElemT> pv(arr_start, arr_start + array->size);
                    return pv;
                }
                break;
        }

        // Complex element types
        result.reserve(array->size);
        char* start = (char*)resolve_relptr_cpp(array->data, base_ptr);
        for(size_t i = 0; i < array->size; i++){
            result.push_back(fromAnything(elem_schema, (void*)(start + i * elem_schema->width), static_cast<ElemT*>(nullptr), base_ptr));
        }
        return result;
    }
    else if constexpr (is_non_vector_container_v<T>) {
        using ElemT = typename T::value_type;
        Array* array = (Array*)data;
        T result;
        if(array->size == 0) return result;

        const Schema* elem_schema = resolve_recur_cpp(schema->parameters[0]);
        char* start = (char*)resolve_relptr_cpp(array->data, base_ptr);

        constexpr bool reverse = is_std_stack<T>::value || is_std_forward_list<T>::value;

        if constexpr (reverse) {
            for (size_t i = array->size; i > 0; --i) {
                auto elem = fromAnything(elem_schema, (void*)(start + (i-1) * elem_schema->width), static_cast<ElemT*>(nullptr), base_ptr);
                if constexpr (is_std_stack<T>::value) result.push(std::move(elem));
                else result.push_front(std::move(elem));
            }
        } else {
            for (size_t i = 0; i < array->size; ++i) {
                auto elem = fromAnything(elem_schema, (void*)(start + i * elem_schema->width), static_cast<ElemT*>(nullptr), base_ptr);
                if constexpr (is_std_queue<T>::value) result.push(std::move(elem));
                else result.push_back(std::move(elem));
            }
        }
        return result;
    }
    else if constexpr (is_std_tuple<T>::value) {
        return fromTupleAnythingHelper(
            schema, data,
            std::make_index_sequence<std::tuple_size_v<T>>{},
            static_cast<T*>(nullptr),
            base_ptr
        );
    }
    else if constexpr (is_std_pair<T>::value) {
        return fromTupleAnythingHelper(
            schema, data,
            std::index_sequence<0, 1>{},
            static_cast<T*>(nullptr),
            base_ptr
        );
    }
    else if constexpr (is_std_optional<T>::value) {
        using InnerT = typename T::value_type;
        // The Optional slot is a relptr (RELNULL = absent). Resolve and
        // recurse into the inner T's body when present.
        relptr_t relptr = *(const relptr_t*)data;
        if (relptr == RELNULL) {
            return std::nullopt;
        }
        const Schema* inner_schema = resolve_recur_cpp(schema->parameters[0]);
        const void* inner_data = resolve_relptr_cpp(relptr, base_ptr);
        return std::optional<InnerT>(
            fromAnything(inner_schema, inner_data, static_cast<InnerT*>(nullptr), base_ptr));
    }
    else if constexpr (is_std_shared_ptr<T>::value) {
        // shared_ptr<T> is the C++ surface form for `?T` at a recursive
        // cycle. The wire slot is a single relptr (RELNULL == absent).
        // Resolve it, then construct shared_ptr<T> wrapping the
        // recursive recurse-into-pointee result.
        using PointeeT = typename T::element_type;
        relptr_t relptr = *(const relptr_t*)data;
        if (relptr == RELNULL) {
            return std::shared_ptr<PointeeT>(nullptr);
        }
        const Schema* inner_schema = resolve_recur_cpp(schema->parameters[0]);
        const void* inner_data = resolve_relptr_cpp(relptr, base_ptr);
        return std::make_shared<PointeeT>(
            fromAnything(inner_schema, inner_data, static_cast<PointeeT*>(nullptr), base_ptr));
    }
    else if constexpr (std::is_arithmetic_v<T>) {
        // Primitives (int, double, float, etc.) — read at schema width and
        // convert to the C++ type so narrow concrete types (e.g. int for Int)
        // work correctly with wider morloc schemas (e.g. i8).
        switch(schema->type) {
            case MORLOC_SINT8:   return static_cast<T>(*(int8_t*)data);
            case MORLOC_SINT16:  return static_cast<T>(*(int16_t*)data);
            case MORLOC_SINT32:  return static_cast<T>(*(int32_t*)data);
            case MORLOC_SINT64:  return static_cast<T>(*(int64_t*)data);
            case MORLOC_UINT8:   return static_cast<T>(*(uint8_t*)data);
            case MORLOC_UINT16:  return static_cast<T>(*(uint16_t*)data);
            case MORLOC_UINT32:  return static_cast<T>(*(uint32_t*)data);
            case MORLOC_UINT64:  return static_cast<T>(*(uint64_t*)data);
            case MORLOC_FLOAT32: return static_cast<T>(*(float*)data);
            case MORLOC_FLOAT64: return static_cast<T>(*(double*)data);
            case MORLOC_INT: {
                // Inline BigInt: [size, value_or_relptr]
                const int64_t* fields = (const int64_t*)data;
                int64_t size = fields[0];
                if (size <= 1) {
                    // Inline: second field is the value directly
                    int64_t val = (size == 0) ? 0 : fields[1];
                    // Check if the value fits in the target C++ type
                    if (sizeof(T) < 8) {
                        int64_t tmin = std::numeric_limits<T>::min();
                        int64_t tmax = std::numeric_limits<T>::max();
                        if (val < tmin || val > tmax) {
                            std::ostringstream oss;
                            oss << "Integer overflow: value " << val
                                << " does not fit in " << (sizeof(T) * 8) << "-bit type"
                                << " (range " << tmin << " to " << tmax << ")";
                            throw std::overflow_error(oss.str());
                        }
                    }
                    return static_cast<T>(val);
                } else {
                    // Overflow: multi-limb integer, cannot fit in any C++ primitive
                    std::ostringstream oss;
                    oss << "Integer overflow: " << size << "-limb integer"
                        << " (" << (size * 64) << " bits)"
                        << " does not fit in " << (sizeof(T) * 8) << "-bit type"
                        << " (range " << static_cast<int64_t>(std::numeric_limits<T>::min())
                        << " to " << static_cast<int64_t>(std::numeric_limits<T>::max()) << ")";
                    throw std::overflow_error(oss.str());
                }
            }
            default: return *(T*)data;
        }
    }
    else {
        // Non-arithmetic types (records, etc.) — generated overloads are
        // preferred by overload resolution, but if we reach here, just
        // reinterpret the bytes directly.
        return *(T*)data;
    }
}


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
        voidstar = toAnything(schema, data);
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
        x = fromAnything(schema, voidstar, static_cast<T*>(nullptr));
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
