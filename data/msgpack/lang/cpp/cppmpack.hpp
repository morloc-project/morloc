#ifndef __MLCMPACK_CPP_BINDINGS_HPP__
#define __MLCMPACK_CPP_BINDINGS_HPP__

#include <vector>
#include <tuple>
#include <stdexcept>
#include <cstring>
#include <iostream>
#include <string>
#include <cstring>

#include "mlcmpack.h"

// The two main exported functions are mpk_pack and mpk_unpack. These translate
// to and from MessagePack format given a schema.

template<typename T>
std::vector<char> mpk_pack(const T& data, const std::string& schema_str);

template<typename T>
T mpk_unpack(const std::vector<char>& packed_data, const std::string& schema_str);




// Forward declarations
template<typename T>
void* toAnything(void* dest, const Schema* schema, const T& data);

// Specialization for nullptr_t (NIL)
void* toAnything(void* dest, const Schema* schema, const std::nullptr_t&) {
    if(!dest){
        dest = get_ptr(schema);
    }

    *((int8_t*)dest) = (int8_t)0; 

    return dest;
}


// Primitives
template<typename Primitive>
void* toAnything(void* dest, const Schema* schema, const Primitive& data) {
    if(!dest){
        dest = get_ptr(schema);
    }

    *((Primitive*)dest) = data;

    return dest;
}

// Specialization for std::vector (array)
template<typename T>
void* toAnything(void* dest, const Schema* schema, const std::vector<T>& data) {
    size_t width = schema->parameters[0]->width;
    Array* result = array_data(dest, width, data.size());

    for (size_t i = 0; i < data.size(); ++i) {
         toAnything((char*)result->data + width * i, schema->parameters[0], data[i]);
    }

    return (void*)result;
}

// Specialization for string
void* toAnything(void* dest, const Schema* schema, const std::string& data) {
    // Create a vector<uint8_t> from the string's data without copying
    std::vector<uint8_t> vec(
        reinterpret_cast<const uint8_t*>(data.data()),
        reinterpret_cast<const uint8_t*>(data.data() + data.size())
    );

    // Move the vector into the function call
    return toAnything(dest, schema, std::move(vec));
}

void* toAnything(void* dest, const Schema* schema, const char* data) {
    std::vector<uint8_t> vec(
        reinterpret_cast<const uint8_t*>(data),
        reinterpret_cast<const uint8_t*>(data) + strlen(data)
    );

    // Move the vector into the function call
    return toAnything(dest, schema, std::move(vec));
}

// Specialization for std::tuple
template<typename... Args>
void* toAnything(void* dest, const Schema* schema, const std::tuple<Args...>& data) {
    return createTupleAnythingHelper(dest, schema, data, std::index_sequence_for<Args...>{});
}

// Helper function for tuple creation
template<typename Tuple, size_t... Is>
void* createTupleAnythingHelper(void* dest, const Schema* schema, const Tuple& data, std::index_sequence<Is...>) {
    if(!dest){
        dest = tuple_data_(schema->parameters, schema->size, schema->width);
    }

    (void)std::initializer_list<int>{(
        toAnything((char*)dest + schema->offsets[Is], schema->parameters[Is], std::get<Is>(data)),
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
    return std::string((char*)array->data, array->size);
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
      std::vector<T> result((T*)array->data, (T*)array->data + array->size);
      return result;
  }

  // Other data types require some rearrangement
  std::vector<T> result;
  result.reserve(array->size);
  const Schema* elemental_schema = schema->parameters[0];
  T* elemental_dumby = nullptr;
  for(size_t i = 0; i < result.size(); i++){
    result.push_back(fromAnything(elemental_schema, (char*)array->data + i * elemental_schema->width, elemental_dumby));
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
    void* data_obj_1 = toAnything(NULL, schema, data);
    char* msgpack_data = nullptr;
    size_t msg_size = 0;

    int pack_result = pack(data_obj_1, schema_str.c_str(), &msgpack_data, &msg_size);
    if (pack_result != 0) {
        // free_schema(schema);
        throw std::runtime_error("Packing failed");
    }

    std::vector<char> result(msgpack_data, msgpack_data + msg_size);

    // free_schema(schema);
    // TODO: free the memories

    return result;
}

template<typename T>
T mpk_unpack(const std::vector<char>& packed_data, const std::string& schema_str) {
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema(&schema_ptr);

    void* data_obj_2 = nullptr;
    int unpack_result = unpack(packed_data.data(), packed_data.size(), schema_str.c_str(), &data_obj_2);
    if (unpack_result != 0) {
        // free_schema(schema);
        throw std::runtime_error("Unpacking failed");
    }

    T x = fromAnything(schema, data_obj_2, static_cast<T*>(nullptr));

    // free_schema(schema);
    // free_parsed_data(data_obj_2);

    return x;
}

#endif
