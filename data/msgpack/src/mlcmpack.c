#include "mlcmpack.h"

// utility ####

void print_hex(const char* data, size_t size) {
    for (size_t i = 0; i < size; i++) {
        fprintf(stderr, "%02x", (unsigned char)data[i]);
        if (i < size - 1) {
            fprintf(stderr, " ");
        }
    }
}

// Helper function to create a schema with parameters
Schema* create_schema_with_params(morloc_serial_type type, int size, Schema** params, char** keys) {
    Schema* schema = (Schema*)malloc(sizeof(Schema));
    if (!schema) return NULL;

    schema->type = type;
    schema->size = size;
    schema->parameters = params;
    schema->keys = keys;

    return schema;
}

Schema* nil_schema() {
    return create_schema_with_params(MORLOC_NIL, 0, NULL, NULL);
}

Schema* bool_schema() {
    return create_schema_with_params(MORLOC_BOOL, 0, NULL, NULL);
}

Schema* int_schema() {
    return create_schema_with_params(MORLOC_INT, 0, NULL, NULL);
}

Schema* float_schema() {
    return create_schema_with_params(MORLOC_FLOAT, 0, NULL, NULL);
}

Schema* string_schema() {
    return create_schema_with_params(MORLOC_STRING, 0, NULL, NULL);
}

Schema* binary_schema() {
    return create_schema_with_params(MORLOC_BINARY, 0, NULL, NULL);
}

Schema* bool_array_schema() {
    Schema** params = (Schema**)malloc(sizeof(Schema*));
    params[0] = bool_schema();
    return create_schema_with_params(MORLOC_BOOL_ARRAY, 1, params, NULL);
}

Schema* int_array_schema() {
    Schema** params = (Schema**)malloc(sizeof(Schema*));
    params[0] = int_schema();
    return create_schema_with_params(MORLOC_INT_ARRAY, 1, params, NULL);
}

Schema* float_array_schema() {
    Schema** params = (Schema**)malloc(sizeof(Schema*));
    params[0] = float_schema();
    return create_schema_with_params(MORLOC_FLOAT_ARRAY, 1, params, NULL);
}

Schema* tuple_schema_(size_t size) {

    Schema** params = (Schema**)calloc(size, sizeof(Schema*));
    if (!params) {
        return NULL;
    }

    return create_schema_with_params(MORLOC_TUPLE, size, params, NULL);
}

Schema* array_schema(Schema* array_type) {
    switch(array_type->type){
      case MORLOC_BOOL:
        return bool_array_schema();
      case MORLOC_INT:
        return int_array_schema();
      case MORLOC_FLOAT:
        return float_array_schema();
      default:
        Schema** params = (Schema**)malloc(sizeof(Schema*));
        if (!params) return NULL;
        
        params[0] = array_type;
        
        return create_schema_with_params(MORLOC_ARRAY, 1, params, NULL);
    }
}

Schema* map_schema_(size_t size) {
    Schema** params = (Schema**)calloc(size, sizeof(Schema*));
    char** keys = (char**)calloc(size, sizeof(char*));

    if (!params || !keys) {
        free(params);
        free(keys);
        return NULL;
    }

    return create_schema_with_params(MORLOC_MAP, size, params, keys);
}

// Free a Schema and its contents
void free_schema(Schema* schema) {
    if (!schema) return;

    if (schema->parameters) {
        for (int i = 0; i < schema->size; i++) {
            if(schema->parameters[i]){
              free_schema(schema->parameters[i]);
            }
        }
        free(schema->parameters);
    }

    if (schema->keys) {
        for (int i = 0; i < schema->size; i++) {
            if (schema->keys[i]) {
                free(schema->keys[i]);
            }
        }
        free(schema->keys);
    }

    free(schema);
}



// Helper function to create a Anything with allocated memory
Anything* create_parsed_data(morloc_serial_type type, size_t size) {
    Anything* data = (Anything*)malloc(sizeof(Anything));
    if (!data) return NULL;
    data->type = type;
    data->size = size;
    data->key = NULL;
    return data;
}

// Helper functions for primitive types
Anything* nil_data() {
    Anything* data = create_parsed_data(MORLOC_NIL, 0);
    // I won't actually use this value, so I keep the msgpck value
    if (data) data->data.nil_val = 0xc0;
    return data;
}

Anything* bool_data(bool value) {
    Anything* data = create_parsed_data(MORLOC_BOOL, 0);
    if (data) data->data.bool_val = value;
    return data;
}

Anything* int_data(int value) {
    Anything* data = create_parsed_data(MORLOC_INT, 0);
    if (data) data->data.int_val = value;
    return data;
}

Anything* float_data(double value) {
    Anything* data = create_parsed_data(MORLOC_FLOAT, 0);
    if (data) data->data.double_val = value;
    return data;
}


Anything* char_data_(size_t size, morloc_serial_type mtype) {
    Anything* data = create_parsed_data(mtype, size);
    if (data) {
        data->data.char_arr = (char*)calloc(size, sizeof(char));
        if (!data->data.char_arr) {
            free(data);
            return NULL;
        }
    }
    return data;
}

Anything* string_data_(size_t size) {
    return char_data_(size, MORLOC_STRING);
}

Anything* binary_data_(size_t size) {
    return char_data_(size, MORLOC_BINARY);
}

Anything* ext_data_(size_t size, int ext_type) {
    // the ext_type is ignored since no support is currently offered
    return char_data_(size, MORLOC_EXT);
}


Anything* obj_array_data_(size_t size, morloc_serial_type mtype) {
    Anything* data = create_parsed_data(mtype, size);
    if (data) {
        data->data.obj_arr = (Anything**)calloc(size, sizeof(Anything*));
        if (!data->data.obj_arr) {
            free(data);
            return NULL;
        }
    }
    return data;
}

// Helper function for arrays
Anything* array_data_(size_t size) {
    return obj_array_data_(size, MORLOC_ARRAY);
}

// Helper function for tuples
Anything* tuple_data_(size_t size) {
    return obj_array_data_(size, MORLOC_TUPLE);
}

// Helper function for maps
Anything* map_data_(size_t size) {
    Anything* data = create_parsed_data(MORLOC_MAP, size);
    if (data) {
        data->data.obj_arr = (Anything**)calloc(size, sizeof(Anything*));
        if (!data->data.obj_arr) {
            free(data->data.obj_arr);
            free(data);
            return NULL;
        }
    }
    return data;
}

// Helper function to set a key-value pair in a map
void set_map_element(Anything* map, size_t pos, const char* key, Anything* value) {
    value->key = strdup(key);
    map->data.obj_arr[pos] = value;
}

Anything* array_bool_data_(size_t size) {
    Anything* data = create_parsed_data(MORLOC_BOOL_ARRAY, size);
    data->data.bool_arr = (bool*)calloc(size, sizeof(bool));
    if (!data->data.bool_arr) {
        free(data);
        return NULL;
    }
    return data;
}

Anything* array_int_data_(size_t size) {
    Anything* data = create_parsed_data(MORLOC_INT_ARRAY, size);
    data->data.int_arr = (int*)malloc(size * sizeof(int));
    if (!data->data.int_arr) {
        free(data);
        return NULL;
    }

    return data;
}

Anything* array_float_data_(size_t size) {
    Anything* data = create_parsed_data(MORLOC_FLOAT_ARRAY, size);
    data->data.float_arr = (double*)malloc(size * sizeof(double));
    if (!data->data.float_arr) {
        free(data);
        return NULL;
    }

    return data;
}

// Helper function for array of booleans
Anything* array_bool_data(const bool* values, size_t size) {
    Anything* data = array_bool_data_(size);
    if (data) {
        memcpy(data->data.bool_arr, values, size * sizeof(bool));
    }
    return data;
}

// Helper function for array of signed integers
Anything* array_int_data(const int* values, size_t size) {
    Anything* data = array_int_data_(size);
    if (data) {
        memcpy(data->data.int_arr, values, size * sizeof(int));
    }
    return data;
}

// Helper function for array of floats (doubles)
Anything* array_float_data(const double* values, size_t size) {
    Anything* data = array_float_data_(size);
    if (data) {
        memcpy(data->data.float_arr, values, size * sizeof(double));
    }
    return data;
}

Anything* string_data(const char* value, size_t size) {
    Anything* data = string_data_(size);
    if (data) {
        memcpy(data->data.char_arr, value, size);
    }
    return data;
}

Anything* binary_data(const char* value, size_t size) {
    Anything* data = binary_data_(size);
    if (data) {
        memcpy(data->data.char_arr, value, size);
    }
    return data;
}


void free_parsed_data(Anything* data) {
    if (!data) return;

    switch (data->type) {
        case MORLOC_NIL:
            // No additional freeing needed
            break;
        case MORLOC_BOOL:
            // For a single boolean, no additional freeing needed
            break;
        case MORLOC_INT:
        case MORLOC_FLOAT:
            // For single primitives, no additional freeing needed
            break;
        case MORLOC_BINARY:
        case MORLOC_STRING:
            free(data->data.char_arr);
            break;
        case MORLOC_MAP:
        case MORLOC_ARRAY:
            if (data->data.obj_arr) {
                for (size_t i = 0; i < data->size; i++) {
                    free_parsed_data(data->data.obj_arr[i]);
                }
                free(data->data.obj_arr);
            }
            break;
        case MORLOC_BOOL_ARRAY:
            free(data->data.bool_arr);
            break;
        case MORLOC_INT_ARRAY:
            free(data->data.int_arr);
            break;
        case MORLOC_FLOAT_ARRAY:
            free(data->data.float_arr);
            break;
        default:
            // Unknown type, do nothing
            break;
    }

    free(data);
}

// packing ####


void upsize(
  char** data,            // data that will be resized
  char** data_ptr,        // pointer that will be updated to preserve offset
  size_t* remaining_size, // remaining data size
  size_t added_size       // the number of bytes that need to be added
){
    // check if any action is needed
    if (added_size <= *remaining_size) {
        return;
    }

    size_t used_size = *data_ptr - *data;
    size_t buffer_size = used_size + *remaining_size;

    // find an appropriate size for the new data
    while (added_size > *remaining_size) {
        if (buffer_size > SIZE_MAX / 2) {
            buffer_size += BUFFER_SIZE;
        } else {
            buffer_size *= 2;
        }
        *remaining_size = buffer_size - used_size;
    }

    // allocate memory for the new data
    *data = (char*)realloc(*data, buffer_size);

    // point old pointer to the same offset in the new data
    *data_ptr = *data + used_size;
}


void write_to_packet(
  void* src,                // source data
  char** packet,            // destination
  char** packet_ptr,        // location in the destination that will be written to
  size_t* packet_remaining, // remaining data size
  size_t size               // the number of bytes to write

){
    upsize(packet, packet_ptr, packet_remaining, size);
    memcpy(*packet_ptr, src, size);
    *packet_ptr += size;
    *packet_remaining -= size;
}


int dynamic_mpack_write(
  mpack_tokbuf_t* tokbuf,
  char** packet,
  char** packet_ptr,
  size_t* packet_remaining,
  mpack_token_t* token,
  size_t extra_size
) {
    int result = 0;
    if(*packet_remaining <= 0){
        upsize(packet, packet_ptr, packet_remaining, 1);
    }
    result = mpack_write(tokbuf, packet_ptr, packet_remaining, token);
    if (result == MPACK_EOF || *packet_remaining == 0) {
        upsize(packet, packet_ptr, packet_remaining, token->length + extra_size);
        if (result == MPACK_EOF) {
            mpack_write(tokbuf, packet_ptr, packet_remaining, token);
        }
    }
    return result;
}

size_t parse_schema_size(const char** schema_ptr){
  char c = **schema_ptr; 
  size_t size =
    // characters 0-9 are integers 0-9
    (c >= 0x30 && c <= 0x39) * (c - 0x30) +
    // characters a-z are integers 10-35
    (c >= 0x61 && c <= 0x7a) * (c - 0x61 + 10) +
    // characters A-Z are integers 36-61
    (c >= 0x41 && c <= 0x5a) * (c - 0x41 + 36) +
    // '+' is 62
    (c == '+') * 62 +
    // '/' is 63
    (c == '/') * 63;
  (*schema_ptr)++;
  return size;
}

char* parse_schema_key(const char** schema_ptr){
  size_t key_size = parse_schema_size(schema_ptr);
  char* key = (char*)calloc(key_size+1, sizeof(char));
  memcpy(key, *schema_ptr, key_size); 
  *schema_ptr += key_size;
  return key;
}

Schema* parse_schema(const char** schema_ptr){
  Schema* schema = 0;
  char c = **schema_ptr;
  (*schema_ptr)++;
  size_t size;

  switch(c){
    case SCHEMA_ARRAY:
      return array_schema(parse_schema(schema_ptr));
    case SCHEMA_TUPLE:
      size = parse_schema_size(schema_ptr);
      schema = tuple_schema_(size);
      for(size_t i = 0; i < size; i++){
        schema->parameters[i] = parse_schema(schema_ptr);
      }
      return schema;
    case SCHEMA_MAP:
      size = parse_schema_size(schema_ptr);
      schema = map_schema_(size);
      for(size_t i = 0; i < size; i++){
        schema->keys[i] = parse_schema_key(schema_ptr);
        schema->parameters[i] = parse_schema(schema_ptr);
      }
      return schema;
    case SCHEMA_NIL:
      return nil_schema();
    case SCHEMA_BOOL:
      return bool_schema();
    case SCHEMA_SINT:
      size = parse_schema_size(schema_ptr);
      return int_schema();
    case SCHEMA_UINT:
      size = parse_schema_size(schema_ptr);
      return int_schema();
    case SCHEMA_FLOAT:
      size = parse_schema_size(schema_ptr);
      return float_schema();
    case SCHEMA_STRING:
      return string_schema();
    case SCHEMA_BINARY:
      return binary_schema();
    default:
      fprintf(stderr, "Unrecognized schema type '%c'\n", c);
      return 0;
  }
}

//  The main function for writing MessagePack
int pack_data(
  const Anything* data,    // input data structure
  const Schema* schema,      // input data schema
  char** packet,             // a pointer to the messagepack data
  char** packet_ptr,         // the current position in the buffer
  size_t* packet_remaining,  // bytes from current position to the packet end
  mpack_tokbuf_t* tokbuf
) {
    mpack_token_t token;
    int result;

    switch (schema->type) {
        case MORLOC_NIL:
            token = mpack_pack_nil();
            break;
        case MORLOC_BOOL:
            token = mpack_pack_boolean(data->data.bool_val);
            break;
        case MORLOC_INT:
            token = mpack_pack_int32(data->data.int_val);
            break;
        case MORLOC_FLOAT:
            token = mpack_pack_float(data->data.double_val);
            break;
        case MORLOC_STRING:
            token = mpack_pack_str(data->size);
            break;
        case MORLOC_BINARY:
            token = mpack_pack_bin(data->size);
            break;
        case MORLOC_BOOL_ARRAY:
        case MORLOC_INT_ARRAY:
        case MORLOC_FLOAT_ARRAY:
        case MORLOC_ARRAY:
        case MORLOC_TUPLE:
            // these all start with the same array bytes
            token = mpack_pack_array(data->size);
            break;
        case MORLOC_MAP:
            token = mpack_pack_map(data->size);
            break;
        default:
            fprintf(stderr, "Unexpected morloc type\n");
            return 1;
    }

    dynamic_mpack_write(tokbuf, packet, packet_ptr, packet_remaining, &token, 0);

    size_t array_length;
    Schema* array_schema;

    switch(schema->type){
      case MORLOC_BINARY:
      case MORLOC_STRING:
        write_to_packet((void*)data->data.char_arr, packet, packet_ptr, packet_remaining, data->size);
        break;
      case MORLOC_ARRAY:
        array_length = data->size;
        array_schema = schema->parameters[0];
        for (size_t i = 0; i < array_length; i++) {
            pack_data(
              data->data.obj_arr[i],
              array_schema,
              packet,
              packet_ptr,
              packet_remaining,
              tokbuf
            );
        }
        break;
      case MORLOC_BOOL_ARRAY:
      case MORLOC_INT_ARRAY:
      case MORLOC_FLOAT_ARRAY:

        for (size_t i = 0; i < data->size; i++){

            switch(schema->type){
              case MORLOC_BOOL_ARRAY:
                token = mpack_pack_boolean(data->data.bool_arr[i]);
                break;
              case MORLOC_INT_ARRAY:
                token = mpack_pack_int32(data->data.int_arr[i]);
                break;
              case MORLOC_FLOAT_ARRAY:
                token = mpack_pack_float(data->data.float_arr[i]);
                break;
              default:
                fprintf(stderr, "Unexpected token: %d\n", schema->type);
                break;
            }

            // token.length isn't set for the primitives other that floats.
            // so I'm hard setting the required bases to 8, which is enough
            // for any supported number. In rare edge cases, this could lead
            // to the buffer being unnecessarily resized. But this will only
            // be a minor performance cost. mpack_pack_sint is about 10%
            // faster than mpack_pack_number, so usually sint is better.
            dynamic_mpack_write(tokbuf, packet, packet_ptr, packet_remaining, &token, 8);

        }
        break;
      case MORLOC_TUPLE:
        for (size_t i = 0; i < schema->size; i++) {
            pack_data(
              data->data.obj_arr[i],
              schema->parameters[i],
              packet,
              packet_ptr,
              packet_remaining,
              tokbuf
            );
        }
        break;
      case MORLOC_MAP:
        for (size_t i = 0; i < data->size; i++) {

            char* key = data->data.obj_arr[i]->key;
            size_t key_len = strlen(key);

            // write key string token
            token = mpack_pack_str(key_len);
            dynamic_mpack_write(tokbuf, packet, packet_ptr, packet_remaining, &token, 8);

            // write string bytes
            write_to_packet((void*)key, packet, packet_ptr, packet_remaining, key_len);

            // write value
            pack_data(data->data.obj_arr[i], schema->parameters[i], packet, packet_ptr, packet_remaining, tokbuf);
        }
        break;
    }

    return 0;
}



int pack_with_schema(const Anything* data, const Schema* schema, char** packet, size_t* packet_size) {
    *packet_size = 0;

    *packet = (char*)malloc(BUFFER_SIZE * sizeof(char));
    if (*packet == NULL) return 1;
    size_t packet_remaining = BUFFER_SIZE;
    char* packet_ptr = *packet;

    mpack_tokbuf_t tokbuf;
    mpack_tokbuf_init(&tokbuf);

    int pack_result = pack_data(data, schema, packet, &packet_ptr, &packet_remaining, &tokbuf);

    // mutate packet_size (will be used outside)
    *packet_size = packet_ptr - *packet;

    // Trim the output buffer to the exact size needed
    if (packet_remaining > 0) {
        *packet = (char*)realloc(*packet, *packet_size);
    }

    return pack_result;
}

int pack(const Anything* data, const char* schema_str, char** out_data, size_t* out_size) {
    Schema* schema = parse_schema(&schema_str);
    return pack_with_schema(data, schema, out_data, out_size);
}



void write_token(mpack_token_t token){
    switch(token.type){
        case MPACK_TOKEN_NIL:
            fprintf(stderr, "NIL");
            break;
        case MPACK_TOKEN_BOOLEAN:
            fprintf(stderr, "BOOLEAN");
            break;
        case MPACK_TOKEN_SINT:
            fprintf(stderr, "SINT(%d)", token.length);
            break;
        case MPACK_TOKEN_UINT:
            fprintf(stderr, "UINT(%d)", token.length);
            break;
        case MPACK_TOKEN_FLOAT:
            fprintf(stderr, "FLOAT");
            break;
        case MPACK_TOKEN_CHUNK:
            fprintf(stderr, "CHUNK(%d): ", token.length);
            if(token.data.chunk_ptr)
                print_hex(token.data.chunk_ptr, token.length);
            break;
        case MPACK_TOKEN_ARRAY:
            fprintf(stderr, "ARRAY(%d)", token.length);
            break;
        case MPACK_TOKEN_MAP:
            fprintf(stderr, "MAP(%d)", token.length);
            break;
        case MPACK_TOKEN_BIN:
            fprintf(stderr, "BIN(%d)", token.length);
            break;
        case MPACK_TOKEN_STR:
            fprintf(stderr, "STR(%d)", token.length);
            break;
        case MPACK_TOKEN_EXT:
            fprintf(stderr, "EXT(%d)", token.length);
            break;
      default:
        break;
    }
}

// terminal parsers
Anything* parse_binary(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_bool(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_bool_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_float(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_float_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_int(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_int_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_nil(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_string(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);

// nested parsers
Anything* parse_array(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_map(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_tuple(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
Anything* parse_obj(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);

Anything* parse_nil(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    return nil_data();
}

Anything* parse_bool(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    return bool_data(mpack_unpack_boolean(*token));
}

Anything* parse_int(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    /* fprintf(stderr, "Is data being decoded incorrectly?\n"); */
    /* print_hex(*buf_ptr, *buf_remaining);            */
    /* fprintf(stderr, "\n");                                   */

    int result = -999;

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    switch(token->type){
      case MPACK_TOKEN_UINT:
        result = (int)mpack_unpack_uint32(*token);
        break;
      case MPACK_TOKEN_SINT:
        result = (int)mpack_unpack_sint32(*token);
        break;
      case MPACK_TOKEN_FLOAT:
        result = (int)(mpack_unpack_float(*token));
        break;
      default:
        fprintf(stderr, "Bad token %d\n", token->type);
        break;
    }

    return int_data(result);
}

Anything* parse_float(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    return float_data((double)mpack_unpack_float(*token));
}


char* parse_key(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    char* key = (char*)calloc(token->length + 1, sizeof(char));
    
    size_t str_idx = 0;

    while(size - str_idx > 0){
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);
        memcpy(
          key + str_idx,
          token->data.chunk_ptr,
          token->length * sizeof(char)
        );
        str_idx += token->length;
    }

    return key;
}

Anything* parse_string(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = string_data_(size);
    size_t str_idx = 0;

    while(size - str_idx > 0){
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);
        memcpy(
          result->data.char_arr + str_idx,
          token->data.chunk_ptr,
          token->length * sizeof(char)
        );
        str_idx += token->length;
    }

    return result;
}

Anything* parse_binary(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = binary_data_(size);
    size_t str_idx = 0;

    while(size - str_idx > 0){
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);
        memcpy(
          result->data.char_arr + str_idx,
          token->data.chunk_ptr,
          token->length * sizeof(char)
        );
        str_idx += token->length;
    }

    return result;
}

Anything* parse_bool_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = array_bool_data_(size);
    for(size_t i = 0; i < size; i++){
      mpack_read(tokbuf, buf_ptr, buf_remaining, token);
      result->data.bool_arr[i] = mpack_unpack_boolean(*token);
    }
    return result;
}

Anything* parse_int_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = array_int_data_(size);
    for(size_t i = 0; i < size; i++){
      mpack_read(tokbuf, buf_ptr, buf_remaining, token);
      switch(token->type){
        case MPACK_TOKEN_UINT:
          result->data.int_arr[i] = (int)mpack_unpack_uint(*token);
          break;
        case MPACK_TOKEN_SINT:
          result->data.int_arr[i] = (int)mpack_unpack_sint(*token);
          break;
        case MPACK_TOKEN_FLOAT:
          result->data.int_arr[i] = (int)(mpack_unpack_float(*token));
          break;
        default:
          fprintf(stderr, "Bad token %d\n", token->type);
          break;
      }
    }
    return result;
}

Anything* parse_float_array(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = array_float_data_(size);
    for(size_t i = 0; i < size; i++){
      mpack_read(tokbuf, buf_ptr, buf_remaining, token);
      result->data.float_arr[i] = (double)mpack_unpack_float(*token);
    }
    return result;
}

Anything* parse_array(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = array_data_(size);

    for(size_t i = 0; i < size; i++){
        result->data.obj_arr[i] = parse_obj(schema, tokbuf, buf_ptr, buf_remaining, token);
    }

    return result;
}

Anything* parse_tuple(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = tuple_data_(size);

    for(size_t i = 0; i < size; i++){
        result->data.obj_arr[i] = parse_obj(schema->parameters[i], tokbuf, buf_ptr, buf_remaining, token);
    }

    return result;
}

Anything* parse_map(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t size = token->length;
    Anything* result = map_data_(size);
    char* key;

    for(size_t i = 0; i < size; i++){
        key = parse_key(tokbuf, buf_ptr, buf_remaining, token);;
        result->data.obj_arr[i] = parse_obj(schema->parameters[i], tokbuf, buf_ptr, buf_remaining, token);
        result->data.obj_arr[i]->key = key;
    }

    return result;
}

Anything* parse_obj(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    switch(schema->type){
      case MORLOC_NIL:
        return parse_nil(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_BOOL:
        return parse_bool(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_INT:
        return parse_int(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_FLOAT:
        return parse_float(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_STRING:
        return parse_string(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_BINARY:
        return parse_binary(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_ARRAY:
        return parse_array(schema->parameters[0], tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_MAP:
        return parse_map(schema, tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_TUPLE:
        return parse_tuple(schema, tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_BOOL_ARRAY:
        return parse_bool_array(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_INT_ARRAY:
        return parse_int_array(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_FLOAT_ARRAY:
        return parse_float_array(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_EXT:
        fprintf(stderr, "Unexpected schema type in parse_obj\n");
        break;
      default:
        break;
    }
    return NULL;
}

void write_tokens(const char** buf_ptr, size_t* buf_remaining){
    mpack_tokbuf_t tokbuf;
    mpack_tokbuf_init(&tokbuf);
    mpack_token_t token;

    int read_result;

    do {
      read_result = mpack_read(&tokbuf, buf_ptr, buf_remaining, &token);
      write_token(token);
      fprintf(stderr, "\n");
    } while(read_result == MPACK_OK);
}


int unpack_with_schema(const char* data, size_t data_size, const Schema* schema, Anything** out_data) {
    // Use the existing unpack_with_schema function, but adapt it to the new prototype
    const char* buf = data;
    size_t buf_remaining = data_size;

    mpack_tokbuf_t tokbuf;
    mpack_tokbuf_init(&tokbuf);
    mpack_token_t token;

    *out_data = parse_obj(schema, &tokbuf, &data, &buf_remaining, &token);

    // Return 0 for success, or an error code if unpack_with_schema fails
    return (*out_data != NULL) ? 0 : -1;
}

int unpack(const char* data, size_t data_size, const char* schema_str, Anything** out_data) {
    const Schema* schema = parse_schema(&schema_str);
    return unpack_with_schema(data, data_size, schema, out_data);
}



// Function to get the string representation of morloc_serial_type
const char* get_type_string(morloc_serial_type type) {
    switch (type) {
        case MORLOC_NIL: return "NIL";
        case MORLOC_BOOL: return "BOOL";
        case MORLOC_INT: return "INT";
        case MORLOC_FLOAT: return "FLOAT";
        case MORLOC_STRING: return "STRING";
        case MORLOC_BINARY: return "BINARY";
        case MORLOC_ARRAY: return "ARRAY";
        case MORLOC_MAP: return "MAP";
        case MORLOC_TUPLE: return "TUPLE";
        case MORLOC_BOOL_ARRAY: return "BOOL_ARRAY";
        case MORLOC_INT_ARRAY: return "INT_ARRAY";
        case MORLOC_FLOAT_ARRAY: return "FLOAT_ARRAY";
        case MORLOC_EXT: return "EXT";
        default: return "UNKNOWN";
    }
}

// Function to print Schema
void print_schema_r(const Schema* schema, int indent) {
    if (schema == NULL) {
        fprintf(stderr, "null");
        return;
    }

    fprintf(stderr, "{\n");
    fprintf(stderr, "%*s\"type\": \"%s\",\n", indent + 2, "", get_type_string(schema->type));
    fprintf(stderr, "%*s\"size\": %zu", indent + 2, "", schema->size);

    if (schema->size > 0) {
        fprintf(stderr, ",\n%*s\"parameters\": [\n", indent + 2, "");
        for (size_t i = 0; i < schema->size; i++) {
            fprintf(stderr, "%*s", indent + 4, "");
            print_schema_r(schema->parameters[i], indent + 4);
            if (i < schema->size - 1) fprintf(stderr, ",");
            fprintf(stderr, "\n");
        }
        fprintf(stderr, "%*s]", indent + 2, "");

        if (schema->keys != NULL) {
            fprintf(stderr, ",\n%*s\"keys\": [", indent + 2, "");
            for (size_t i = 0; i < schema->size; i++) {
                fprintf(stderr, "\"%s\"", schema->keys[i]);
                if (i < schema->size - 1) fprintf(stderr, ", ");
            }
            fprintf(stderr, "]");
        }
    }

    fprintf(stderr, "\n%*s}", indent, "");
}

void print_schema(const Schema* schema){
  print_schema_r(schema, 0);
}



// Function to print Anything
void print_parsed_data_r(const Anything* data, int indent) {
    if (data == NULL) {
        fprintf(stderr, "null");
        return;
    }

    fprintf(stderr, "{\n");
    fprintf(stderr, "%*s\"type\": \"%s\",\n", indent + 2, "", get_type_string(data->type));
    fprintf(stderr, "%*s\"size\": %zu,\n", indent + 2, "", data->size);
    
    if (data->key != NULL) {
        fprintf(stderr, "%*s\"key\": \"%s\",\n", indent + 2, "", data->key);
    }

    fprintf(stderr, "%*s\"data\": ", indent + 2, "");

    switch (data->type) {
        case MORLOC_NIL:
            fprintf(stderr, "null");
            break;
        case MORLOC_BOOL:
            fprintf(stderr, "%s", data->data.bool_val ? "true" : "false");
            break;
        case MORLOC_INT:
            fprintf(stderr, "%d", data->data.int_val);
            break;
        case MORLOC_FLOAT:
            fprintf(stderr, "%f", data->data.double_val);
            break;
        case MORLOC_STRING:
            fprintf(stderr, "\"%s\"", data->data.char_arr);
            break;
        case MORLOC_BINARY:
            fprintf(stderr, "\"<binary data>\"");
            break;
        case MORLOC_ARRAY:
        case MORLOC_MAP:
        case MORLOC_TUPLE:
            fprintf(stderr, "[\n");
            for (size_t i = 0; i < data->size; i++) {
                fprintf(stderr, "%*s", indent + 4, "");
                print_parsed_data_r(data->data.obj_arr[i], indent + 4);
                if (i < data->size - 1) fprintf(stderr, ",");
                fprintf(stderr, "\n");
            }
            fprintf(stderr, "%*s]", indent + 2, "");
            break;
        case MORLOC_BOOL_ARRAY:
            fprintf(stderr, "[");
            for (size_t i = 0; i < data->size; i++) {
                fprintf(stderr, "%s", data->data.bool_arr[i] ? "true" : "false");
                if (i < data->size - 1) fprintf(stderr, ", ");
            }
            fprintf(stderr, "]");
            break;
        case MORLOC_INT_ARRAY:
            fprintf(stderr, "[");
            for (size_t i = 0; i < data->size; i++) {
                fprintf(stderr, "%d", data->data.int_arr[i]);
                if (i < data->size - 1) fprintf(stderr, ", ");
            }
            fprintf(stderr, "]");
            break;
        case MORLOC_FLOAT_ARRAY:
            fprintf(stderr, "[");
            for (size_t i = 0; i < data->size; i++) {
                fprintf(stderr, "%f", data->data.float_arr[i]);
                if (i < data->size - 1) fprintf(stderr, ", ");
            }
            fprintf(stderr, "]");
            break;
        case MORLOC_EXT:
            fprintf(stderr, "\"<extension data>\"");
            break;
    }

    fprintf(stderr, "\n%*s}", indent, "");
}

void print_parsed_data(const Anything* data){
  print_parsed_data_r(data, 0);
}
