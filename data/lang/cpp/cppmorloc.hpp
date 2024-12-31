#ifndef __CPPMORLOC_HPP__
#define __CPPMORLOC_HPP__

#include <vector>
#include <tuple>
#include <stdexcept>
#include <cstring>
#include <iostream>
#include <string>
#include <cstring>

#include "morloc.h"

// The two main exported functions are mpk_pack and mpk_unpack. These translate
// to and from MessagePack format given a schema.

template<typename T>
std::vector<char> mpk_pack(const T& data, const std::string& schema_str);

template<typename T>
T mpk_unpack(const std::vector<char>& packed_data, const std::string& schema_str);



// Forward declarations
template<typename T>
size_t get_shm_size(const Schema* schema, const T& data);

// Specialization for nullptr_t (NIL)
size_t get_shm_size(const Schema* schema, const std::nullptr_t&) {
    return sizeof(int8_t);
}

// Primitives
template<typename Primitive>
size_t get_shm_size(const Schema* schema, const Primitive& data) {
    return schema->width;
}

// Specialization for std::vector (array)
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

template<typename... Args>
size_t get_shm_size(const Schema* schema, const std::tuple<Args...>& data) {
    return createTupleShmSizeHelper(schema, data, std::index_sequence_for<Args...>{});
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



template<typename T>
void* toAnything(const Schema* schema, const T& data){
    // Calculate the total required memory space
    size_t total_size = get_shm_size(schema, data);

    // Allocate this space in shared memory
    void* dest = shmalloc(total_size);

    void* cursor = (void*)((char*)dest + schema->width);

    // Recurse
    return toAnything(dest, &cursor, schema, data);
}

// Forward declarations
template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const T& data);

// Specialization for nullptr_t (NIL)
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

// Specialization for std::vector (array)
template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::vector<T>& data) {
    // The fixed length array wrapper is written to the destincation
    Array* result = static_cast<Array*>(dest);
    result->size = data.size();

    // The array data is written to the cursor location
    // The N fixed-size elements will be written here
    result->data = abs2rel(static_cast<absptr_t>(*cursor));

    // The cursor is mutated to point to the location after the children
    *cursor = static_cast<char*>(*cursor) + data.size() * schema->parameters[0]->width;

    size_t width = schema->parameters[0]->width;
    for (size_t i = 0; i < data.size(); ++i) {
        // Any child variable data will be written to the cursor
         toAnything(rel2abs(result->data + width * i), cursor, schema->parameters[0], data[i]);
    }

    return dest;
}

// Specialization for string, casts a string to a uint8_t vector and recalls
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::string& data) {
    // Create a vector<uint8_t> from the string's data without copying
    std::vector<uint8_t> vec(
        reinterpret_cast<const uint8_t*>(data.data()),
        reinterpret_cast<const uint8_t*>(data.data() + data.size())
    );

    // Move the vector into the function call
    return toAnything(dest, cursor, schema, std::move(vec));
}

// Specialization for C strings, casts to uint8_t vector and recalls
void* toAnything(void* dest, void** cursor, const Schema* schema, const char* data) {
    std::vector<uint8_t> vec(
        reinterpret_cast<const uint8_t*>(data),
        reinterpret_cast<const uint8_t*>(data) + strlen(data)
    );

    // Move the vector into the function call
    return toAnything(dest, cursor, schema, std::move(vec));
}

// Specialization for std::tuple
template<typename... Args>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::tuple<Args...>& data) {
    return createTupleAnythingHelper(dest, schema, cursor, data, std::index_sequence_for<Args...>{});
}

// Helper function for tuple creation
template<typename Tuple, size_t... Is>
void* createTupleAnythingHelper(void* dest, const Schema* schema, void** cursor, const Tuple& data, std::index_sequence<Is...>) {
    (void)std::initializer_list<int>{(
        toAnything((char*)dest + schema->offsets[Is], cursor, schema->parameters[Is], std::get<Is>(data)),
        0
    )...};
    return dest;
}



template<typename Primitive>
Primitive fromAnything(const Schema* schema, const void* data, Primitive* dumby = nullptr) {
    return *(Primitive*)data;
}

std::string fromAnything(const Schema* schema, const void* data, std::string* dumby = nullptr) {
    Array* array = (Array*)data;
    return std::string((char*)rel2abs(array->data), array->size);
}

template<typename T>
std::vector<T> fromAnything(const Schema* schema, const void* data, std::vector<T>* dumby = nullptr){
  Array* array = (Array*) data;

  // Directly use memory for constant width primitives arrays
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
      std::vector<T> result((T*)(rel2abs(array->data)), (T*)(rel2abs(array->data)) + array->size);
      return result;
  }

  // Other data types require some rearrangement
  std::vector<T> result;
  result.reserve(array->size);
  const Schema* elemental_schema = schema->parameters[0];
  T* elemental_dumby = nullptr;
  for(size_t i = 0; i < array->size; i++){
    result.push_back(fromAnything(elemental_schema, rel2abs(array->data + i * elemental_schema->width), elemental_dumby));
  }
  return result;
}


template<typename... Args>
std::tuple<Args...> fromAnything(const Schema* schema, const void* anything, std::tuple<Args...>* = nullptr) {
    return fromTupleAnythingHelper(
      schema,
      anything,
      std::index_sequence_for<Args...>{},
      static_cast<std::tuple<Args...>*>(nullptr)
    );
}

// Helper function for tuple conversion
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
std::vector<char> mpk_pack(const T& data, const std::string& schema_str) {
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema(&schema_ptr);

    // Create Anything* from schema and data
    void* voidstar = toAnything(schema, data);
    char* msgpack_data = NULL;
    size_t msg_size = 0;

    int pack_result = pack(voidstar, schema_str.c_str(), &msgpack_data, &msg_size);

    if (pack_result != 0) {
        free_schema(schema);
        throw std::runtime_error("Packing failed");
    }

    std::vector<char> result(msgpack_data, msgpack_data + msg_size);

    free_schema(schema);

    return result;
}

template<typename T>
T mpk_unpack(const std::vector<char>& packed_data, const std::string& schema_str) {
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema(&schema_ptr);

    void* voidstar = nullptr;
    int unpack_result = unpack_with_schema(packed_data.data(), packed_data.size(), schema, &voidstar);
    if (unpack_result != 0) {
        // free_schema(schema);
        throw std::runtime_error("Unpacking failed");
    }

    T x = fromAnything(schema, voidstar, static_cast<T*>(nullptr));

    free_schema(schema);
    shfree(voidstar);

    return x;
}

#endif
