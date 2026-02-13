#ifndef __CPPMORLOC_HPP__
#define __CPPMORLOC_HPP__

#include <vector>
#include <stack>
#include <list>
#include <forward_list>
#include <queue>
#include <deque>

#include <algorithm>
#include <tuple>
#include <stdexcept>
#include <cstring>
#include <string>
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

template<typename T>
inline constexpr bool is_non_vector_container_v =
    is_std_list<T>::value || is_std_forward_list<T>::value ||
    is_std_deque<T>::value || is_std_stack<T>::value ||
    is_std_queue<T>::value;


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
bool shfree_cpp(absptr_t ptr);
Schema* parse_schema_cpp(const char* schema_ptr);
void* shmalloc_cpp(size_t size);
shm_t* shinit_cpp(const char* shm_basename, size_t volume_index, size_t shm_size);
int pack_with_schema_cpp(const void* mlc, const Schema* schema, char** mpk, size_t* mpk_size);
int unpack_with_schema_cpp(const char* mgk, size_t mgk_size, const Schema* schema, void** mlcptr);


// ============================================================
// mpk_pack / mpk_unpack declarations
// ============================================================

template<typename T>
std::vector<char> mpk_pack(const T& data, const std::string& schema_str);

template<typename T>
T mpk_unpack(const std::vector<char>& packed_data, const std::string& schema_str);


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
    return schema->width;
}

template<typename T>
size_t get_shm_size(const Schema* schema, const std::vector<T>& data) {
    size_t total_size = schema->width;
    switch(schema->parameters[0]->type){
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
            total_size += data.size() * schema->parameters[0]->width;
            break;
        case MORLOC_STRING:
        case MORLOC_ARRAY:
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            for(size_t i = 0; i < data.size(); i++){
               total_size += get_shm_size(schema->parameters[0], data[i]);
            }
            break;
    }
    return total_size;
}

size_t get_shm_size(const Schema* schema, const std::string& data) {
    return schema->width + data.size();
}

size_t get_shm_size(void* dest, const Schema* schema, const char* data) {
    return schema->width + strlen(data);
}

template<typename Tuple, size_t... Is>
size_t createTupleShmSizeHelper(const Schema* schema, const Tuple& data, std::index_sequence<Is...>) {
    size_t total_size = 0;
    (void)std::initializer_list<int>{(
        total_size += get_shm_size(schema->parameters[Is], std::get<Is>(data)),
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

// Primitives
template<typename Primitive>
void* toAnything(void* dest, void** cursor, const Schema* schema, const Primitive& data) {
    *((Primitive*)dest) = data;
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
    result->data = abs2rel_cpp(static_cast<absptr_t>(*cursor));
    *cursor = static_cast<char*>(*cursor) + data.size() * schema->parameters[0]->width;
    char* start = (char*)cpp_rel2abs(result->data);
    size_t width = schema->parameters[0]->width;
    for (size_t i = 0; i < data.size(); ++i) {
         toAnything(start + width * i, cursor, schema->parameters[0], data[i]);
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
    result->data = abs2rel_cpp(static_cast<absptr_t>(*cursor));
    *cursor = static_cast<char*>(*cursor) + size * schema->parameters[0]->width;
    char* start = (char*)cpp_rel2abs(result->data);
    size_t width = schema->parameters[0]->width;
    size_t i = 0;
    for (const auto& item : data) {
        toAnything(start + width * i, cursor, schema->parameters[0], item);
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
    (void)std::initializer_list<int>{(
        toAnything((char*)dest + schema->offsets[Is], cursor, schema->parameters[Is], std::get<Is>(data)),
        0
    )...};
    return dest;
}

template<typename... Args>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::tuple<Args...>& data) {
    return createTupleAnythingHelper(dest, schema, cursor, data, std::index_sequence_for<Args...>{});
}


// ============================================================
// fromAnything - single template with if constexpr dispatch
// ============================================================

// Forward declaration for recursive calls
template<typename T>
T fromAnything(const Schema* schema, const void* data, T* = nullptr);

// Tuple helper (needs forward declaration of fromAnything)
template<typename Tuple, size_t... Is>
Tuple fromTupleAnythingHelper(
  const Schema* schema,
  const void* anything,
  std::index_sequence<Is...>,
  Tuple* = nullptr
) {
    return Tuple(fromAnything(schema->parameters[Is],
                              (char*)anything + schema->offsets[Is],
                              static_cast<std::tuple_element_t<Is, Tuple>*>(nullptr))...);
}

template<typename T>
T fromAnything(const Schema* schema, const void* data, T*) {
    if(data == NULL){
        throw std::runtime_error("Void error in fromAnything");
    }

    if constexpr (std::is_same_v<T, bool>) {
        // NOTE: do NOT use bool here since its width is often not 1 byte
        return *(uint8_t*)data == 1;
    }
    else if constexpr (std::is_same_v<T, std::string>) {
        Array* array = (Array*)data;
        if(array->size > 0){
            return std::string((char*)cpp_rel2abs(array->data), array->size);
        }
        return std::string("");
    }
    else if constexpr (is_std_vector<T>::value) {
        using ElemT = typename T::value_type;
        std::vector<ElemT> result;
        Array* array = (Array*)data;
        if(array->size == 0) return result;

        // Fast path for primitive arrays
        switch(schema->parameters[0]->type){
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
            case MORLOC_FLOAT64: {
                std::vector<ElemT> pv(
                    (ElemT*)(cpp_rel2abs(array->data)),
                    (ElemT*)(cpp_rel2abs(array->data)) + array->size
                );
                return pv;
            }
        }

        // Complex element types
        result.reserve(array->size);
        const Schema* elem_schema = schema->parameters[0];
        char* start = (char*)cpp_rel2abs(array->data);
        for(size_t i = 0; i < array->size; i++){
            result.push_back(fromAnything(elem_schema, (void*)(start + i * elem_schema->width), static_cast<ElemT*>(nullptr)));
        }
        return result;
    }
    else if constexpr (is_non_vector_container_v<T>) {
        using ElemT = typename T::value_type;
        Array* array = (Array*)data;
        T result;
        if(array->size == 0) return result;

        const Schema* elem_schema = schema->parameters[0];
        char* start = (char*)cpp_rel2abs(array->data);

        constexpr bool reverse = is_std_stack<T>::value || is_std_forward_list<T>::value;

        if constexpr (reverse) {
            for (size_t i = array->size; i > 0; --i) {
                auto elem = fromAnything(elem_schema, (void*)(start + (i-1) * elem_schema->width), static_cast<ElemT*>(nullptr));
                if constexpr (is_std_stack<T>::value) result.push(std::move(elem));
                else result.push_front(std::move(elem));
            }
        } else {
            for (size_t i = 0; i < array->size; ++i) {
                auto elem = fromAnything(elem_schema, (void*)(start + i * elem_schema->width), static_cast<ElemT*>(nullptr));
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
            static_cast<T*>(nullptr)
        );
    }
    else {
        // Primitives (int, double, float, etc.)
        // Record types are handled by generated overloads which are preferred
        // by overload resolution over this template.
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
