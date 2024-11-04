#ifndef __MLCMPACK_CPP_BINDINGS_HPP__
#define __MLCMPACK_CPP_BINDINGS_HPP__

#include <vector>
#include <tuple>
#include <stdexcept>
#include <cstring>
#include <iostream>
#include <string>

#include "mlcmpack.h"

// The two main exported functions are mpk_pack and mpk_unpack. These translate
// to and from MessagePack format given a schema.

template<typename T>
std::vector<char> mpk_pack(const T& data, const std::string& schema_str);

template<typename T>
T mpk_unpack(const std::vector<char>& packed_data, const std::string& schema_str);




// Helper function to check schema type
void checkSchemaType(const Schema* schema, morloc_serial_type expected_type) {
    if (schema->type != expected_type) {
        fprintf(stderr, "Schema type mismatch. Expected %d but got %d\n", expected_type, schema->type);
        
        throw std::runtime_error("Schema mismatch\n");
    }
}

// Forward declarations
template<typename T>
Anything* toAnything(const Schema* schema, const T& data);

// Specialization for nullptr_t (NIL)
Anything* toAnything(const Schema* schema, const std::nullptr_t&) {
    checkSchemaType(schema, MORLOC_NIL);
    return nil_data();
}

// Specialization for bool
Anything* toAnything(const Schema* schema, const bool& data) {
    checkSchemaType(schema, MORLOC_BOOL);
    return bool_data(data);
}

// Specialization for int
Anything* toAnything(const Schema* schema, const int& data) {
    checkSchemaType(schema, MORLOC_INT);
    return int_data(data);
}

// Specialization for double
Anything* toAnything(const Schema* schema, const double& data) {
    checkSchemaType(schema, MORLOC_FLOAT);
    return float_data(data);
}

// Specialization for const char*
Anything* toAnything(const Schema* schema, const std::string& data) {
    checkSchemaType(schema, MORLOC_STRING);
    return string_data(data.c_str(), data.size());
}

// Specialization for std::vector (array)
template<typename T>
Anything* toAnything(const Schema* schema, const std::vector<T>& data) {
    checkSchemaType(schema, MORLOC_ARRAY);
    Anything* result = array_data_(data.size());
    for (size_t i = 0; i < data.size(); ++i) {
        result->data.obj_arr[i] = toAnything(schema->parameters[0], data[i]);
    }
    return result;
}

// Specialization for std::vector<bool>
template<>
Anything* toAnything<bool>(const Schema* schema, const std::vector<bool>& data) {
    checkSchemaType(schema, MORLOC_BOOL_ARRAY);
    // std::vector<bool> is a special case that does not guarantee contiguous
    // memory, so we have to copy here
    Anything* result = array_bool_data_(data.size());
    for (size_t i = 0; i < data.size(); ++i) {
        result->data.bool_arr[i] = data[i];
    }
    return result;
}

// Specialization for const char*, i.e., binary
Anything* toAnything(const Schema* schema, const std::vector<char>& data) {
    checkSchemaType(schema, MORLOC_BINARY);
    Anything* result = new Anything;
    result->type = MORLOC_BINARY;
    result->size = data.size();
    result->data.char_arr = const_cast<char*>(data.data());
    return result;
}

// Specialization for std::vector<int>
template<>
Anything* toAnything<int>(const Schema* schema, const std::vector<int>& data) {
    checkSchemaType(schema, MORLOC_INT_ARRAY);
    Anything* result = new Anything;
    result->type = MORLOC_INT_ARRAY;
    result->size = data.size();
    result->data.int_arr = const_cast<int*>(data.data());
    return result;
}

// Specialization for std::vector<double>
template<>
Anything* toAnything<double>(const Schema* schema, const std::vector<double>& data) {
    checkSchemaType(schema, MORLOC_FLOAT_ARRAY);
    Anything* result = new Anything;
    result->type = MORLOC_FLOAT_ARRAY;
    result->size = data.size();
    result->data.float_arr = const_cast<double*>(data.data());
    return result;
}

// Specialization for std::tuple
template<typename... Args>
Anything* toAnything(const Schema* schema, const std::tuple<Args...>& data) {
    checkSchemaType(schema, MORLOC_TUPLE);
    return createTupleAnythingHelper(schema, data, std::index_sequence_for<Args...>{});
}

// Helper function for tuple creation
template<typename Tuple, size_t... Is>
Anything* createTupleAnythingHelper(const Schema* schema, const Tuple& data, std::index_sequence<Is...>) {
    Anything* result = tuple_data_(sizeof...(Is));
    (void)std::initializer_list<int>{(
        result->data.obj_arr[Is] = toAnything(schema->parameters[Is], std::get<Is>(data)),
        0
    )...};
    return result;
}


bool fromAnything(const Schema* schema, const Anything* data, bool* dumby = nullptr) {
    return data->data.bool_val;
}

double fromAnything(const Schema* schema, const Anything* data, double* dumby = nullptr) {
    return data->data.double_val;
}

int fromAnything(const Schema* schema, const Anything* data, int* dumby = nullptr) {
    return data->data.int_val;
}

std::string fromAnything(const Schema* schema, const Anything* data, std::string* dumby = nullptr) {
    return std::string(data->data.char_arr);
}


std::vector<char> fromAnything(const Schema* schema, const Anything* data, std::vector<char>* dumby = nullptr) {
    return std::vector<char>(const_cast<char*>(data->data.char_arr), 
                             const_cast<char*>(data->data.char_arr) + data->size);
}

std::vector<bool> fromAnything(const Schema* schema, const Anything* data, std::vector<bool>* dumby = nullptr) {
    return std::vector<bool>(const_cast<bool*>(data->data.bool_arr), 
                             const_cast<bool*>(data->data.bool_arr) + data->size);
}

std::vector<int> fromAnything(const Schema* schema, const Anything* data, std::vector<int>* dumby = nullptr) {
    return std::vector<int>(const_cast<int*>(data->data.int_arr), 
                             const_cast<int*>(data->data.int_arr) + data->size);
}

std::vector<double> fromAnything(const Schema* schema, const Anything* data, std::vector<double>* dumby = nullptr) {
    return std::vector<double>(const_cast<double*>(data->data.float_arr), 
                             const_cast<double*>(data->data.float_arr) + data->size);
}

template<typename T>
std::vector<T> fromAnything(const Schema* schema, const Anything* anything, std::vector<T>* dumby = nullptr) {
    checkSchemaType(schema, MORLOC_ARRAY); // Ensure schema type matches for an array
    std::vector<T> result;
    result.reserve(anything->size); // Reserve space based on the size in Anything

    const Schema* element_schema = schema->parameters[0]; // Get the schema for the element type
    T* elemental_dumby = nullptr;

    // Loop over elements in "Anything** obj_arr" and call fromAnything on each
    for (size_t i = 0; i < anything->size; ++i) {
        result.push_back(fromAnything(element_schema, anything->data.obj_arr[i], elemental_dumby));
    }

    return result;
}

template<typename... Args>
std::tuple<Args...> fromAnything(const Schema* schema, const Anything* anything, std::tuple<Args...>* = nullptr) {
    checkSchemaType(schema, MORLOC_TUPLE);
    return fromTupleAnythingHelper(schema, anything, std::index_sequence_for<Args...>{}, static_cast<std::tuple<Args...>*>(nullptr));
}

// Helper function for tuple conversion
template<typename Tuple, size_t... Is>
Tuple fromTupleAnythingHelper(const Schema* schema, const Anything* anything, std::index_sequence<Is...>, Tuple* = nullptr) {
    return Tuple(fromAnything(schema->parameters[Is], 
                              anything->data.obj_arr[Is], 
                              static_cast<std::tuple_element_t<Is, Tuple>*>(nullptr))...);
}


template<typename T>
std::vector<char> mpk_pack(const T& data, const std::string& schema_str) {
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema(&schema_ptr);

    // Create Anything* from schema and data
    Anything* data_obj_1 = toAnything(schema, data);
    char* msgpack_data = nullptr;
    size_t msg_size = 0;

    int pack_result = pack(data_obj_1, schema_str.c_str(), &msgpack_data, &msg_size);
    if (pack_result != 0) {
        free_schema(schema);
        free_parsed_data(data_obj_1);
        throw std::runtime_error("Packing failed");
    }

    std::vector<char> result(msgpack_data, msgpack_data + msg_size);

    free_schema(schema);
    // free_parsed_data(data_obj_1);
    free(msgpack_data);

    return result;
}

template<typename T>
T mpk_unpack(const std::vector<char>& packed_data, const std::string& schema_str) {
    const char* schema_ptr = schema_str.c_str();
    Schema* schema = parse_schema(&schema_ptr);

    Anything* data_obj_2 = nullptr;
    int unpack_result = unpack(packed_data.data(), packed_data.size(), schema_str.c_str(), &data_obj_2);
    if (unpack_result != 0) {
        free_schema(schema);
        throw std::runtime_error("Unpacking failed");
    }

    T x = fromAnything(schema, data_obj_2, static_cast<T*>(nullptr));

    free_schema(schema);
    // free_parsed_data(data_obj_2);

    return x;
}

#endif
