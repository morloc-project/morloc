#include "morloc.h"

static char schema_size_to_string(size_t size) {
  if (size < 10) {
      // characters 0-9 are integers 0-9
      return (char)(0x30 + size);
  } else if (size < 36) {
      // characters a-z are integers 10-35
      return (char)(0x61 + (size - 10));
  } else if (size < 62) {
      // characters A-Z are integers 36-61
      return (char)(0x41 + (size - 36));
  } else if (size == 62) {
      // '+' is 62
      return '+';
  } else if (size == 63) {
      // '/' is 63
      return '/';
  } else {
      // TODO: update this to encode arbitrary size
      return '\a'; // error - ding!!!
  }
}

static char* resize_schema_buffer(char* schema_str, size_t pos, size_t* buffer_size, size_t requirement){
    if(pos + requirement + 1 >= *buffer_size){
        *buffer_size += 32 * (1 + (*buffer_size - pos + requirement) / 32);
        char* new_str = (char*)realloc(schema_str, *buffer_size);
        if(new_str == NULL){
            fprintf(stderr, "Out of memory in resize_schema_buffer (requested %zu bytes)\n", *buffer_size);
            abort();
        }
        schema_str = new_str;
    }
    return schema_str;
}

static char* write_generic_schema_r(const Schema* schema, char* schema_str, size_t* pos, size_t* buffer_size){
    schema_str = resize_schema_buffer(schema_str, *pos, buffer_size, 2);
    switch (schema->type) {
        case MORLOC_NIL:
            schema_str[*pos] = SCHEMA_NIL; (*pos)++;
            break;
        case MORLOC_BOOL:
            schema_str[*pos] = SCHEMA_BOOL; (*pos)++;
            break;
        case MORLOC_UINT8:
            schema_str[*pos] = SCHEMA_UINT; (*pos)++;
            schema_str[*pos] = '1'; (*pos)++;
            break;
        case MORLOC_UINT16:
            schema_str[*pos] = SCHEMA_UINT; (*pos)++;
            schema_str[*pos] = '2'; (*pos)++;
            break;
        case MORLOC_UINT32:
            schema_str[*pos] = SCHEMA_UINT; (*pos)++;
            schema_str[*pos] = '4'; (*pos)++;
            break;
        case MORLOC_UINT64:
            schema_str[*pos] = SCHEMA_UINT; (*pos)++;
            schema_str[*pos] = '8'; (*pos)++;
            break;
        case MORLOC_SINT8:
            schema_str[*pos] = SCHEMA_SINT; (*pos)++;
            schema_str[*pos] = '1'; (*pos)++;
            break;
        case MORLOC_SINT16:
            schema_str[*pos] = SCHEMA_SINT; (*pos)++;
            schema_str[*pos] = '2'; (*pos)++;
            break;
        case MORLOC_SINT32:
            schema_str[*pos] = SCHEMA_SINT; (*pos)++;
            schema_str[*pos] = '4'; (*pos)++;
            break;
        case MORLOC_SINT64:
            schema_str[*pos] = SCHEMA_SINT; (*pos)++;
            schema_str[*pos] = '8'; (*pos)++;
            break;
        case MORLOC_FLOAT32:
            schema_str[*pos] = SCHEMA_FLOAT; (*pos)++;
            schema_str[*pos] = '4'; (*pos)++;
            break;
        case MORLOC_FLOAT64:
            schema_str[*pos] = SCHEMA_FLOAT; (*pos)++;
            schema_str[*pos] = '8'; (*pos)++;
            break;
        case MORLOC_STRING:
            schema_str[*pos] = SCHEMA_STRING; (*pos)++;
            break;
        case MORLOC_ARRAY:
            schema_str[*pos] = SCHEMA_ARRAY; (*pos)++;
            schema_str = write_generic_schema_r(schema->parameters[0], schema_str, pos, buffer_size);
            break;
        case MORLOC_MAP:
            schema_str[*pos] = SCHEMA_MAP; (*pos)++;
            schema_str[*pos] = schema_size_to_string(schema->size); (*pos)++;
            for(size_t i = 0; i < schema->size; i++){
                // write key
                size_t key_len = strlen(schema->keys[i]);
                schema_str = resize_schema_buffer(schema_str, *pos, buffer_size, key_len + 1);
                schema_str[*pos] = schema_size_to_string(key_len); (*pos)++;
                strncpy(schema_str + *pos, schema->keys[i], key_len); *pos += key_len;
                // write value
                schema_str = write_generic_schema_r(schema->parameters[i], schema_str, pos, buffer_size);
            }
            break;

        case MORLOC_TUPLE:
            schema_str[*pos] = SCHEMA_TUPLE; (*pos)++;
            schema_str[*pos] = schema_size_to_string(schema->size); (*pos)++;
            for(size_t i = 0; i < schema->size; i++){
                schema_str = write_generic_schema_r(schema->parameters[i], schema_str, pos, buffer_size);
            }
            break;
        default:
            // This should be unreachable
            fprintf(stderr, "Missing case in morloc schema");
    }

    return schema_str;
}

char* schema_to_string(const Schema* schema){
    char* schema_str = (char*)calloc(32, sizeof(char));
    size_t pos = 0;
    size_t buffer_size = 32;
    schema_str = write_generic_schema_r(schema, schema_str, &pos, &buffer_size);
    schema_str[pos] = '\0';
    return schema_str;
}

void* get_ptr(const Schema* schema, ERRMSG){
    PTR_RETURN_SETUP(void)
    void* ptr = TRY((void*)shmalloc, schema->width);
    return ptr;
}

bool schema_is_fixed_width(const Schema* schema){
    switch(schema->type){
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            return false;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            for(size_t i = 0; i < schema->size; i++){
                if (! schema_is_fixed_width(schema->parameters[i])){
                    return false;
                }
            }
            break;
        default:
            return true;
            break;
    }
    return true;
}

size_t calculate_voidstar_size(const void* data, const Schema* schema, ERRMSG){
    VAL_RETURN_SETUP(size_t, 0)

    size_t size = 0;

    switch(schema->type){
        case MORLOC_STRING:
            {
                Array* array = (Array*)data;
                size = sizeof(Array) + array->size * schema->parameters[0]->width;
            }
            break;
        case MORLOC_ARRAY:
            {
                Array* array = (Array*)data;
                size_t element_width = schema->parameters[0]->width;

                uint8_t* element_data = TRY((uint8_t*)rel2abs, array->data);

                size = sizeof(Array);

                if (schema_is_fixed_width(schema)){
                    size += element_width * array->size;
                } else {
                    for(size_t i = 0; i < array->size; i++){
                        size += TRY(calculate_voidstar_size, element_data + i * element_width, schema->parameters[0]);
                    }
                }
            }
            break;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            {
                if (schema_is_fixed_width(schema)){
                    size = schema->width;
                } else {
                    uint8_t* element_data = (uint8_t*)data;
                    for(size_t i = 0; i < schema->size; i++){
                        size += TRY(calculate_voidstar_size, element_data + schema->offsets[i], schema->parameters[i]);
                    }
                }
            }
            break;
        default:
            size = schema->width;
            break;
    }
    return size;
}

static Schema* create_schema_with_params(morloc_serial_type type, size_t width, size_t size, Schema** params, char** keys) {
    Schema* schema = (Schema*)calloc(1, sizeof(Schema));
    if (!schema) return NULL;

    schema->type = type;
    schema->size = size;
    schema->width = width;
    if(size > 0){
        schema->offsets = (size_t*)calloc(size, sizeof(size_t));;
    } else {
        schema->offsets = NULL;
    }
    schema->parameters = params;
    schema->keys = keys;

    // for tuples and maps, generate the element offsets
    if(params){
      for(size_t i = 1; i < size; i++){
        schema->offsets[i] = schema->offsets[i-1] + params[i-1]->width;
      }
    }

    return schema;
}

static Schema* nil_schema() {
    return create_schema_with_params(MORLOC_NIL, 1, 0, NULL, NULL);
}

static Schema* bool_schema() {
    return create_schema_with_params(MORLOC_BOOL, 1, 0, NULL, NULL);
}

static Schema* uint_schema(size_t width) {
    switch(width){
      case 1:
        return create_schema_with_params(MORLOC_UINT8, width, 0, NULL, NULL);
      case 2:
        return create_schema_with_params(MORLOC_UINT16, width, 0, NULL, NULL);
      case 4:
        return create_schema_with_params(MORLOC_UINT32, width, 0, NULL, NULL);
      case 8:
        return create_schema_with_params(MORLOC_UINT64, width, 0, NULL, NULL);
      default:
        fprintf(stderr, "Integers may only have widths of 1, 2, 4, or 8 bytes; found %lu", width);
        return NULL;
    }
}

static Schema* sint_schema(size_t width) {
    switch(width){
      case 1:
        return create_schema_with_params(MORLOC_SINT8, width, 0, NULL, NULL);
      case 2:
        return create_schema_with_params(MORLOC_SINT16, width, 0, NULL, NULL);
      case 4:
        return create_schema_with_params(MORLOC_SINT32, width, 0, NULL, NULL);
      case 8:
        return create_schema_with_params(MORLOC_SINT64, width, 0, NULL, NULL);
      default:
        fprintf(stderr, "Integers may only have widths of 1, 2, 4, or 8 bytes; found %lu", width);
        return NULL;
    }
}

static Schema* float_schema(size_t width) {
    switch(width){
      case 4:
        return create_schema_with_params(MORLOC_FLOAT32, width, 0, NULL, NULL);
      case 8:
        return create_schema_with_params(MORLOC_FLOAT64, width, 0, NULL, NULL);
      default:
        fprintf(stderr, "Floats may only have widths of 4 or 8 bytes, found %lu", width);
        return NULL;
    }
}

static Schema* string_schema() {
    Schema** params = (Schema**)calloc(1, sizeof(Schema*));
    if (!params) return NULL;

    // This parameter is needed for compatibility with arrays
    params[0] = uint_schema(1);

    return create_schema_with_params(MORLOC_STRING, sizeof(Array), 1, params, NULL);
}

static Schema* tuple_schema(Schema** params, size_t size) {
    size_t width = 0;
    for(size_t i = 0; i < size; i++){
      width += params[i]->width;
    }
    return create_schema_with_params(MORLOC_TUPLE, width, size, params, NULL);
}

static Schema* array_schema(Schema* array_type) {
    Schema** params = (Schema**)calloc(1, sizeof(Schema*));
    if (!params) return NULL;

    params[0] = array_type;

    return create_schema_with_params(MORLOC_ARRAY, sizeof(Array), 1, params, NULL);
}

static Schema* map_schema(size_t size, char** keys, Schema** params) {
    size_t width = 0;
    for(size_t i = 0; i < size; i++){
      width += params[i]->width;
    }
    return create_schema_with_params(MORLOC_MAP, width, size, params, keys);
}

static size_t parse_schema_size(char** schema_ptr){
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

static char* parse_schema_key(char** schema_ptr){
  size_t key_size = parse_schema_size(schema_ptr);
  char* key = (char*)calloc(key_size+1, sizeof(char));
  if(key == NULL) return NULL;
  memcpy(key, *schema_ptr, key_size);
  *schema_ptr += key_size;
  return key;
}

static char* parse_hint(char** schema_ptr) {
  char* hint = NULL;

  if (!schema_ptr || !*schema_ptr) {
    return hint;
  }

  size_t depth = 1;
  size_t buffer_size = 128;
  size_t buffer_index = 0;
  hint = (char*)calloc(1, buffer_size);
  if (!hint) return NULL;

  while (**schema_ptr != '\0') {
    if (**schema_ptr == '<') {
      depth++;
    } else if (**schema_ptr == '>') {
      depth--;
      if (depth == 0) {
        (*schema_ptr)++; // Move past the closing '>'
        break;
      }
    }

    if (buffer_index >= buffer_size - 1) {
      buffer_size *= 2;
      char* new_hint = (char*)realloc(hint, buffer_size);
      if (!new_hint) {
        FREE(hint);
        return NULL;
      }
      hint = new_hint;
    }

    hint[buffer_index++] = **schema_ptr;
    (*schema_ptr)++;
  }

  if (depth != 0) {
    // Unmatched '<', free memory and return NULL
    FREE(hint);
    return NULL;
  }

  hint[buffer_index] = '\0';
  return hint;
}

Schema* parse_schema(const char* schema_str, ERRMSG){
  PTR_RETURN_SETUP(Schema)

  char* schema_copy = strdup(schema_str);
  RAISE_IF(schema_copy == NULL, "Failed to allocate schema string")

  char* parse_ptr = schema_copy;  // This pointer will be modified
  Schema* schema = TRY_WITH(free(schema_copy), parse_schema_r, &parse_ptr);

  free(schema_copy);
  return schema;
}

Schema* parse_schema_r(char** schema_ptr, ERRMSG){
  PTR_RETURN_SETUP(Schema)

  Schema** params;
  char c = **schema_ptr;
  (*schema_ptr)++;
  size_t size;
  char** keys;
  Schema* child_schema = NULL;

  Schema* schema = NULL;
  char* hint = NULL;

  switch(c){
    case SCHEMA_ARRAY:
      child_schema = parse_schema_r(schema_ptr, &CHILD_ERRMSG);
      RAISE_IF(child_schema == NULL, "\n%s", CHILD_ERRMSG)
      schema = array_schema(child_schema);
      break;
    case SCHEMA_TUPLE:
      size = parse_schema_size(schema_ptr);
      params = (Schema**)calloc(size, sizeof(Schema*));
      for(size_t i = 0; i < size; i++){
        params[i] = parse_schema_r(schema_ptr, &CHILD_ERRMSG);
        if(params[i] == NULL){
          for(size_t j = 0; j < i; j++) free_schema(params[j]);
          free(params);
          RAISE("\n%s", CHILD_ERRMSG)
        }
      }
      schema = tuple_schema(params, size);
      break;
    case SCHEMA_MAP:
      size = parse_schema_size(schema_ptr);
      keys = (char**)calloc(size, sizeof(char*));
      params = (Schema**)calloc(size, sizeof(Schema*));
      for(size_t i = 0; i < size; i++){
        keys[i] = parse_schema_key(schema_ptr);
        if(keys[i] == NULL){
          for(size_t j = 0; j < i; j++){ free_schema(params[j]); free(keys[j]); }
          free(params);
          free(keys);
          RAISE("Failed to allocate schema key")
        }
        params[i] = parse_schema_r(schema_ptr, &CHILD_ERRMSG);
        if(params[i] == NULL){
          for(size_t j = 0; j < i; j++) free_schema(params[j]);
          for(size_t j = 0; j <= i; j++) free(keys[j]);
          free(params);
          free(keys);
          RAISE("\n%s", CHILD_ERRMSG)
        }
      }
      schema = map_schema(size, keys, params);
      break;
    case SCHEMA_NIL:
      schema = nil_schema();
      break;
    case SCHEMA_BOOL:
      schema = bool_schema();
      break;
    case SCHEMA_SINT:
      size = parse_schema_size(schema_ptr);
      schema = sint_schema(size);
      break;
    case SCHEMA_UINT:
      size = parse_schema_size(schema_ptr);
      schema = uint_schema(size);
      break;
    case SCHEMA_FLOAT:
      size = parse_schema_size(schema_ptr);
      schema = float_schema(size);
      break;
    case SCHEMA_STRING:
      schema = string_schema();
      break;
    case '<':
      {
          hint = parse_hint(schema_ptr);
          schema = parse_schema_r(schema_ptr, &CHILD_ERRMSG);
          RAISE_IF(schema == NULL, "\n%s", CHILD_ERRMSG)
      }
      break;
    default:
      RAISE("Unrecognized schema type '%c'", c);
  }

  schema->hint = hint;

  return schema;
}

void free_schema(Schema* schema) {
    if (schema == NULL) {
        return;
    }

    FREE(schema->hint);
    FREE(schema->offsets);

    // Free the parameters and their contents
    if (schema->parameters != NULL) {
        for (size_t i = 0; i < schema->size; i++) {
            free_schema(schema->parameters[i]);
        }
        free(schema->parameters);
    }

    // Free the keys and their contents
    if (schema->keys != NULL) {
        for (size_t i = 0; i < schema->size; i++) {
            FREE(schema->keys[i]);
        }
        free(schema->keys);
    }

    // Finally, free the schema itself
    FREE(schema);
}
