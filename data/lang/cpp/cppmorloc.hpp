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
#include <iostream>
#include <string>
#include <cstring>

#include "morloc.h"

absptr_t cpp_rel2abs(relptr_t ptr){
    char* errmsg = NULL;
    absptr_t absptr = rel2abs(ptr, &errmsg);
    if(errmsg != NULL){
        throw std::runtime_error(errmsg);
    }
    return absptr;
}

relptr_t abs2rel_cpp(absptr_t ptr){
    char* errmsg = NULL;
    relptr_t relptr = abs2rel(ptr, &errmsg);
    if(errmsg != NULL){
        throw std::runtime_error(errmsg);
    }
    return relptr;
}

bool shfree_cpp(absptr_t ptr){
    char* errmsg = NULL;
    bool success = shfree(ptr, &errmsg);
    if(errmsg != NULL){
        throw std::runtime_error(errmsg);
    }
    return success;
}


Schema* parse_schema_cpp(const char** schema_ptr){
    char* errmsg = NULL;
    Schema* schema = parse_schema(schema_ptr, &errmsg);
    if(errmsg != NULL){
        throw std::runtime_error(errmsg);
    }
    return schema;
}

void* shmalloc_cpp(size_t size){
    char* errmsg = NULL;
    void* new_ptr = shmalloc(size, &errmsg);
    if(errmsg != NULL){
        throw std::runtime_error(errmsg);
    }
    return new_ptr;

}

shm_t* shinit_cpp(const char* shm_basename, size_t volume_index, size_t shm_size) {
    char* errmsg = NULL;
    shm_t* new_ptr = shinit(shm_basename, volume_index, shm_size, &errmsg);
    if(errmsg != NULL){
        throw std::runtime_error(errmsg);
    }
    return new_ptr;

}

int pack_with_schema_cpp(const void* mlc, const Schema* schema, char** mpk, size_t* mpk_size){
    char* errmsg = NULL;
    int exitcode = pack_with_schema(mlc, schema, mpk, mpk_size, &errmsg);
    if(errmsg != NULL){
        throw std::runtime_error(errmsg);
    }
    return exitcode; 
}

int unpack_with_schema_cpp(const char* mgk, size_t mgk_size, const Schema* schema, void** mlcptr){
    char* errmsg = NULL;
    int exitcode = unpack_with_schema(mgk, mgk_size, schema, mlcptr, &errmsg);
    if(errmsg != NULL){
        throw std::runtime_error(errmsg);
    }
    return exitcode; 
}

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
    void* dest = shmalloc_cpp(total_size);

    void* cursor = (void*)((char*)dest + schema->width);

    // Recurse
    return toAnything(dest, &cursor, schema, data);
}

// Convert STL type isomorphic to vectors to vectors
template <typename T>
void* toAnything(const Schema* schema, const std::stack<T>& data) {
    std::vector<T> temp;
    std::stack<T> tempStack = data;
    while (!tempStack.empty()) {
        temp.push_back(tempStack.top());
        tempStack.pop();
    }

    std::reverse(temp.begin(), temp.end());

    // Use the vector implementation to write the data
    return toAnything(schema, temp);
}

template <typename T>
void* toAnything(const Schema* schema, const std::forward_list<T>& data) {
    std::vector<T> temp(data.begin(), data.end()); // Copy to vector

    return toAnything(schema, temp);
}

template <typename T>
void* toAnything(const Schema* schema, const std::queue<T>& data) {
    std::vector<T> temp;
    std::queue<T> tempQueue = data; // Copy the queue
    while (!tempQueue.empty()) {
        temp.push_back(tempQueue.front()); // Note: front() for queue
        tempQueue.pop();
    }

    return toAnything(schema, temp);
}

template <typename T>
void* toAnything(const Schema* schema, const std::deque<T>& data) {
    std::vector<T> temp(data.begin(), data.end()); // Directly copy to vector

    return toAnything(schema, temp);
}

template <typename T>
void* toAnything(const Schema* schema, const std::list<T>& data) {
    std::vector<T> temp(data.begin(), data.end()); // Directly copy to vector

    return toAnything(schema, temp);
}


// Forward declarations
template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const T& data);

// write a raw data array
void* binarytoAnything(void* dest, void** cursor, const Schema* schema, const uint8_t* data, size_t size) {
    // The fixed length array wrapper is written to the destination
    Array* result = static_cast<Array*>(dest);
    result->size = size;

    if(size == 0){
        result->data = RELNULL;
        return dest;
    }

    absptr_t data_ptr = static_cast<absptr_t>(*cursor);

    // The array data is written to the cursor location
    // The N fixed-size elements will be written here
    result->data = abs2rel_cpp(data_ptr);

    // The cursor is mutated to point to the location after the children
    *cursor = static_cast<char*>(*cursor) + size * schema->parameters[0]->width;

    memcpy(data_ptr, data, size);

    return dest;
}


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

    if(data.size() == 0){
        result->data = RELNULL;
        return dest;
    }

    // The array data is written to the cursor location
    // The N fixed-size elements will be written here
    result->data = abs2rel_cpp(static_cast<absptr_t>(*cursor));

    // The cursor is mutated to point to the location after the children
    *cursor = static_cast<char*>(*cursor) + data.size() * schema->parameters[0]->width;

    char* start = (char*)cpp_rel2abs(result->data);
    size_t width = schema->parameters[0]->width;
    for (size_t i = 0; i < data.size(); ++i) {
        // Any child variable data will be written to the cursor
         toAnything(start + width * i, cursor, schema->parameters[0], data[i]);
    }

    return dest;
}

template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::list<T>& data) {
    // The fixed length array wrapper is written to the destination
    Array* result = static_cast<Array*>(dest);
    result->size = data.size();

    if(data.size() == 0){
        result->data = RELNULL;
        return dest;
    }

    // The array data is written to the cursor location
    result->data = abs2rel_cpp(static_cast<absptr_t>(*cursor));

    // The cursor is mutated to point to the location after the children
    *cursor = static_cast<char*>(*cursor) + data.size() * schema->parameters[0]->width;

    char* start = (char*)cpp_rel2abs(result->data);
    size_t width = schema->parameters[0]->width;
    size_t i = 0;
    for (const auto& item : data) {
        // Any child variable data will be written to the cursor
        toAnything(start + width * i, cursor, schema->parameters[0], item);
        ++i;
    }

    return dest;
}

template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::forward_list<T>& data) {
    // Count the elements in the forward_list
    size_t size = std::distance(data.begin(), data.end());

    // The fixed length array wrapper is written to the destination
    Array* result = static_cast<Array*>(dest);
    result->size = size;

    if(size == 0){
        result->data = RELNULL;
        return dest;
    }

    // The array data is written to the cursor location
    result->data = abs2rel_cpp(static_cast<absptr_t>(*cursor));

    // The cursor is mutated to point to the location after the children
    *cursor = static_cast<char*>(*cursor) + size * schema->parameters[0]->width;

    char* start = (char*)cpp_rel2abs(result->data);
    size_t width = schema->parameters[0]->width;
    size_t i = 0;
    for (const auto& item : data) {
        // Any child variable data will be written to the cursor
        toAnything(start + width * i, cursor, schema->parameters[0], item);
        ++i;
    }

    return dest;
}

template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::queue<T>& data) {
    // Create a temporary vector to store the queue elements
    std::vector<T> temp;
    std::queue<T> tempQueue = data;
    while (!tempQueue.empty()) {
        temp.push_back(tempQueue.front());
        tempQueue.pop();
    }

    // Use the vector implementation to write the data
    return toAnything(dest, cursor, schema, temp);
}

template<typename T>
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::deque<T>& data) {
    // The fixed length array wrapper is written to the destination
    Array* result = static_cast<Array*>(dest);
    result->size = data.size();

    if(data.size() == 0){
        result->data = RELNULL;
        return dest;
    }

    // The array data is written to the cursor location
    result->data = abs2rel_cpp(static_cast<absptr_t>(*cursor));

    // The cursor is mutated to point to the location after the children
    *cursor = static_cast<char*>(*cursor) + data.size() * schema->parameters[0]->width;

    char* start = (char*)cpp_rel2abs(result->data);
    size_t width = schema->parameters[0]->width;
    for (size_t i = 0; i < data.size(); ++i) {
        // Any child variable data will be written to the cursor
        toAnything(start + width * i, cursor, schema->parameters[0], data[i]);
    }

    return dest;
}

// Specialization for string, casts a string to a uint8_t vector and recalls
void* toAnything(void* dest, void** cursor, const Schema* schema, const std::string& data) {
    return binarytoAnything(dest, cursor, schema, (const uint8_t*)data.c_str(), data.size());
}

// Specialization for C strings, casts to uint8_t vector and recalls
void* toAnything(void* dest, void** cursor, const Schema* schema, const char* data) {
    return binarytoAnything(dest, cursor, schema, (const uint8_t*)data, strlen(data));
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
    if(array->size > 0){
        return std::string((char*)cpp_rel2abs(array->data), array->size);
    } else {
        return std::string("");
    }
}

template<typename T>
std::vector<T> fromAnything(const Schema* schema, const void* data, std::vector<T>* dumby = nullptr){
  std::vector<T> result;
  Array* array = (Array*) data;

  if(array->size == 0){
      return result;
  }

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
      std::vector<T> primitive_vector((T*)(cpp_rel2abs(array->data)), (T*)(cpp_rel2abs(array->data)) + array->size);
      return primitive_vector;
  }

  // Other data types require some rearrangement
  result.reserve(array->size);
  if(array->size > 0){
      const Schema* elemental_schema = schema->parameters[0];
      T* elemental_dumby = nullptr;
      char* start = (char*)cpp_rel2abs(array->data);
      for(size_t i = 0; i < array->size; i++){
        result.push_back(fromAnything(elemental_schema, (void*)(start + i * elemental_schema->width), elemental_dumby));
      }
  }
  return result;
}

template<typename T>
std::stack<T> fromAnything(const Schema* schema, const void* data, std::stack<T>* dumby = nullptr) {
    Array* array = (Array*)data;
    std::stack<T> result;
    if(array->size > 0){
        const Schema* elemental_schema = schema->parameters[0];
        T* elemental_dumby = nullptr;
        char* start = (char*)cpp_rel2abs(array->data);
        
        // We need to push elements in reverse order to maintain the original stack order
        for (size_t i = array->size; i > 0; --i) {
            result.push(fromAnything(elemental_schema, (void*)(start + (i-1) * elemental_schema->width), elemental_dumby));
        }
    }
    return result;
}

template<typename T>
std::list<T> fromAnything(const Schema* schema, const void* data, std::list<T>* dumby = nullptr) {
    Array* array = (Array*)data;
    std::list<T> result;
    if(array->size > 0){
        const Schema* elemental_schema = schema->parameters[0];
        T* elemental_dumby = nullptr;
        char* start = (char*)cpp_rel2abs(array->data);
        
        for (size_t i = 0; i < array->size; ++i) {
            result.push_back(fromAnything(elemental_schema, (void*)(start + i * elemental_schema->width), elemental_dumby));
        }
    }
    return result;
}

template<typename T>
std::forward_list<T> fromAnything(const Schema* schema, const void* data, std::forward_list<T>* dumby = nullptr) {
    Array* array = (Array*)data;
    std::forward_list<T> result;
    if(array->size > 0){
        const Schema* elemental_schema = schema->parameters[0];
        T* elemental_dumby = nullptr;
        char* start = (char*)cpp_rel2abs(array->data);
        
        // We need to insert elements in reverse order to maintain the original list order
        for (size_t i = array->size; i > 0; --i) {
            result.push_front(fromAnything(elemental_schema, (void*)(start + (i-1) * elemental_schema->width), elemental_dumby));
        }
    }
    return result;
}

template<typename T>
std::queue<T> fromAnything(const Schema* schema, const void* data, std::queue<T>* dumby = nullptr) {
    Array* array = (Array*)data;
    std::queue<T> result;
    if(array->size > 0){
        const Schema* elemental_schema = schema->parameters[0];
        T* elemental_dumby = nullptr;
        char* start = (char*)cpp_rel2abs(array->data);
        
        for (size_t i = 0; i < array->size; ++i) {
            result.push(fromAnything(elemental_schema, (void*)(start + i * elemental_schema->width), elemental_dumby));
        }
    }
    return result;
}

template<typename T>
std::deque<T> fromAnything(const Schema* schema, const void* data, std::deque<T>* dumby = nullptr) {
    Array* array = (Array*)data;
    std::deque<T> result;
    if(array->size > 0){
        const Schema* elemental_schema = schema->parameters[0];
        T* elemental_dumby = nullptr;
        char* start = (char*)cpp_rel2abs(array->data);
        
        for (size_t i = 0; i < array->size; ++i) {
            result.push_back(fromAnything(elemental_schema, (void*)(start + i * elemental_schema->width), elemental_dumby));
        }
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
    Schema* schema = parse_schema_cpp(&schema_ptr);

    // Create Anything* from schema and data
    void* voidstar = toAnything(schema, data);
    char* msgpack_data = NULL;
    size_t msg_size = 0;

    int pack_result = pack_with_schema_cpp(voidstar, schema, &msgpack_data, &msg_size);

    std::vector<char> result(msgpack_data, msgpack_data + msg_size);

    free_schema(schema);

    return result;
}

template<typename T>
T mpk_unpack(const std::vector<char>& packed_data, const std::string& schema_str) {
    char* errmsg = NULL;
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema_cpp(&schema_ptr);

    void* voidstar = nullptr;
    int unpack_result = unpack_with_schema_cpp(packed_data.data(), packed_data.size(), schema, &voidstar);
    if (unpack_result != 0) {
        // free_schema(schema);
        throw std::runtime_error("Unpacking failed");
    }

    T x = fromAnything(schema, voidstar, static_cast<T*>(nullptr));

    free_schema(schema);
    shfree_cpp(voidstar);

    return x;
}

#endif
