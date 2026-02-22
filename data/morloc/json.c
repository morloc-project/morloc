#include "morloc.h"
#include "json.h"

char* quoted(const char* input){
  size_t len = strlen(input);
  char* quoted_string = (char*)calloc(len + 3, sizeof(char));
  quoted_string[0] = '"';
  quoted_string[len + 1] = '"';
  memcpy(quoted_string + 1, input, len);
  return quoted_string;
}

static char* json_escape_string(const char* input, size_t input_len) {
    size_t output_len = 0;
    size_t i;

    // First pass: calculate the required length of the escaped string
    for (i = 0; i < input_len; i++) {
        switch (input[i]) {
            case '\"': case '\\': case '/': case '\b':
            case '\f': case '\n': case '\r': case '\t':
                output_len += 2;
                break;
            default:
                if ((unsigned char)input[i] < 32) {
                    output_len += 6; // \u00xx
                } else {
                    output_len++;
                }
        }
    }

    char* output = (char*)calloc(output_len + 1, sizeof(char)); // +1 for null terminator
    if (!output) return NULL;

    size_t j = 0;
    // Second pass: copy and escape characters
    for (i = 0; i < input_len; i++) {
        switch (input[i]) {
            case '\"': output[j++] = '\\'; output[j++] = '\"'; break;
            case '\\': output[j++] = '\\'; output[j++] = '\\'; break;
            case '/':  output[j++] = '\\'; output[j++] = '/';  break;
            case '\b': output[j++] = '\\'; output[j++] = 'b';  break;
            case '\f': output[j++] = '\\'; output[j++] = 'f';  break;
            case '\n': output[j++] = '\\'; output[j++] = 'n';  break;
            case '\r': output[j++] = '\\'; output[j++] = 'r';  break;
            case '\t': output[j++] = '\\'; output[j++] = 't';  break;
            default:
                if ((unsigned char)input[i] < 32) {
                    sprintf(&output[j], "\\u%04x", input[i]);
                    j += 6;
                } else {
                    output[j++] = input[i];
                }
        }
    }
    output[j] = '\0';

    return output;
}

static void consume_whitespace(char** json_data){
    while(**json_data == ' ' || **json_data == '\t' || **json_data == '\n' || **json_data == '\r'){
        (*json_data)++;
    }
}

static bool consume_char(char c, char** json_ptr, ERRMSG){
    BOOL_RETURN_SETUP
    consume_whitespace(json_ptr);
    RAISE_IF(**json_ptr != c, "\n  Unexpected character: expected '%c' at '%s'", c, *json_ptr)
    (*json_ptr)++;
    return true;
}

static bool match_json_boolean(char** json_data, ERRMSG){
    BOOL_RETURN_SETUP
    consume_whitespace(json_data);
    if(**json_data == 't' && *(*json_data + 1) == 'r' && *(*json_data + 2) == 'u' && *(*json_data + 3) == 'e') {
        *json_data += 4;
        return true;
    } else if(**json_data == 'f' && *(*json_data + 1) == 'a' && *(*json_data + 2) == 'l' && *(*json_data + 3) == 's' && *(*json_data + 4) == 'e') {
        *json_data += 5;
        return false;
    } else {
        RAISE("\n  Expected (true/false) value in JSON")
    }
}

static int64_t parse_json_int(char** json_ptr, ERRMSG) {
    VAL_RETURN_SETUP(uint64_t, 0)
    consume_whitespace(json_ptr);
    char* end;
    int64_t val = strtoll(*json_ptr, &end, 10);
    RAISE_IF(*json_ptr == end, "\n  Not a valid integer: '%c'", **json_ptr);
    *json_ptr = end;
    return val;
}

static uint64_t parse_json_uint(char** json_ptr, ERRMSG) {
    VAL_RETURN_SETUP(uint64_t, 0)
    consume_whitespace(json_ptr);
    char* end;
    uint64_t val = strtoull(*json_ptr, &end, 10);
    RAISE_IF(*json_ptr == end, "\n  Not a valid integer: '%c'", **json_ptr);
    *json_ptr = end;
    return val;
}

static double parse_json_double(char** json_ptr, ERRMSG) {
    VAL_RETURN_SETUP(double, 0)
    consume_whitespace(json_ptr);
    char* end;
    double val = strtod(*json_ptr, &end);
    RAISE_IF(*json_ptr == end, "\n  Not a valid floating-point number: '%c'", **json_ptr);
    *json_ptr = end;
    return val;
}

static int json_string_size(char* ptr, size_t* json_size, size_t* c_size, ERRMSG) {
    INT_RETURN_SETUP
    RAISE_IF(*ptr != '"', "\n  Expected string, but no initial quote found (observed '%c')", *ptr)

    char* json_start = ptr;
    size_t c_size_ = 0;

    ptr++; // Skip opening quote
    while (*ptr != '\0' && *ptr != '"') {
        if (*ptr == '\\') {
            ptr++; // Move past backslash
            RAISE_IF(*ptr == '\0', "\n  Unexpected end of string in escape sequence");

            switch (*ptr) {
                case '"':
                case '\\':
                case '/':
                case 'b':
                case 'f':
                case 'n':
                case 'r':
                case 't':
                    // Valid single-character escapes
                    c_size_ += 1;
                    ptr++;
                    break;

                case 'u': {
                    // Unicode escape sequence \uXXXX
                    ptr++; // Move past 'u'
                    // Validate 4 hex digits
                    for (int i = 0; i < 4; i++) {
                        RAISE_IF(*ptr == '\0', "\n  Truncated Unicode escape");
                        RAISE_IF(!isxdigit(*ptr), "\n  Invalid hex digit in Unicode escape");
                        ptr++;
                    }
                    c_size_ += 4; // Count as one character
                    break;
                }

                default:
                    RAISE("\n  Invalid escape character");
            }
        } else {
            // Regular character
            c_size_ += 1;
            ptr++;
        }
    }

    RAISE_IF(*ptr != '"', "\n  Unterminated string, missing closing quote");
    ptr++;

    *json_size = (size_t)(ptr - json_start - 2);
    *c_size = c_size_;

    return EXIT_PASS;
}

static int write_json_string(char** json_ptr, char* dest, ERRMSG){
    INT_RETURN_SETUP
    consume_whitespace(json_ptr);

    char* ptr = *json_ptr;

    RAISE_IF(*ptr != '"', "\n  Expected string, but no initial quote found")
    ptr++; // Skip opening quote

    while (*ptr != '\0' && *ptr != '"') {
        if (*ptr == '\\') {
            ptr++; // Move past backslash
            RAISE_IF(*ptr == '\0', "\n  Unexpected end of string in escape sequence");

            switch (*ptr) {
                case '"':
                    *dest = '"'; dest++; break;
                case '\\':
                    *dest = '\\'; dest++; break;
                case '/':
                    *dest = '/'; dest++; break;
                case 'b':
                    *dest = '\b'; dest++; break;
                case 'f':
                    *dest = '\f'; dest++; break;
                case 'n':
                    *dest = '\n'; dest++; break;
                case 'r':
                    *dest = '\r'; dest++; break;
                case 't':
                    *dest = '\t'; dest++; break;
                case 'u': {
                    // Unicode escape sequence \uXXXX
                    ptr++; // Move past 'u'
                    // Validate 4 hex digits
                    for (int i = 0; i < 4; i++) {
                        RAISE_IF(*ptr == '\0', "\n  Truncated Unicode escape");
                        RAISE_IF(!isxdigit(*ptr), "\n  Invalid hex digit in Unicode escape");
                        *dest = *ptr;
                        dest++;
                        ptr++;
                    }
                    break;
                }
                default:
                    RAISE("\n  Invalid escape character");
            }
            ptr++;
        } else {
            // Regular character
            *dest = *ptr;
            ptr++;
            dest++;
        }
    }

    RAISE_IF(*ptr != '"', "\n  Unterminated string, missing closing quote");
    ptr++; // move past closing quote

    // consume the string
    *json_ptr = ptr;

    return EXIT_PASS;

}

static char* read_json_key(char** json_ptr, ERRMSG){
    PTR_RETURN_SETUP(char)

    size_t j_string_size = 0;
    size_t c_string_size = 0;

    consume_whitespace(json_ptr);
    TRY(json_string_size, *json_ptr, &j_string_size, &c_string_size);

    char* key = (char*)calloc(c_string_size + 1, sizeof(char));

    // If the C and JSON strings are the same length, then there are
    // no special characters to consider and we can simply memcpy.
    if(c_string_size == j_string_size){
        // copy from the position AFTER the initial quote
        memcpy(key, *json_ptr + 1, c_string_size);
        // consume the string and quotes
        *json_ptr += j_string_size + 2;
    }
    // Otherwise we need to loop through the string and handle escapees
    else {
        TRY_WITH(free(key), write_json_string, json_ptr, key);
    }

    return key;
}

static size_t json_array_size(char* ptr, ERRMSG) {
    VAL_RETURN_SETUP(size_t, 0)

    RAISE_IF(*ptr != '[', "\n  Failed to parse input JSON. Expected an array, but found no starting bracket\n");

    size_t size = 0;
    size_t depth = 0;
    bool in_string = false;
    bool in_escape = false;

    while(*ptr != '\0'){
        if(in_string){
            switch(*ptr){
                case '\\':
                    in_escape = !in_escape;
                    break;
                case '"':
                    if(!in_escape){
                        in_string = false;
                    }
                // fall through
                default:
                    in_escape = false;
                    break;
            }
        } else {
            switch(*ptr){
                case '[': {
                  depth++;
                  // handle the first element
                  if(depth == 1){
                    consume_whitespace(&ptr);
                    if(*(ptr+1) != ']'){
                        size = 1;
                    }
                  }
                  break;
                }
                case '{': depth++; break;
                case ']': depth--; break;
                case '}': depth--; break;
                case '"':
                    in_string = true;
                    in_escape = false;
                    break;
                case ',':
                    size += depth == 1;
                    break;
                default:
                    break;
            }
        }

        if(depth == 0){
            return size;
        }

        ptr++;
    }

    RAISE("\n  Failed to parse JSON: missing closing bracket on array\n")
}

static int read_json_with_schema_r(
    uint8_t* voidstar, // pre-allocated and zeroed space that will be mutated
    char** json_ptr, // a pointer to the current location in the json string
    const Schema* schema,
    ERRMSG
){

    INT_RETURN_SETUP
    consume_whitespace(json_ptr);

    switch(schema->type){
        case MORLOC_NIL: {
            *((uint8_t*)voidstar) = (uint8_t)0;
            break;
        }
        case MORLOC_BOOL: {
            bool val = TRY(match_json_boolean, json_ptr);
            *((uint8_t*)voidstar) = val ? (uint8_t)1  // true
                                        : (uint8_t)0; // false
            break;
        }

        case MORLOC_SINT8: {
            int64_t val = TRY(parse_json_int, json_ptr);
            RAISE_IF(val < INT8_MIN || val > INT8_MAX, "Value out of range for 8-bit signed integer");
            *((int8_t*)voidstar) = (int8_t)val;
            break;
        }

        case MORLOC_SINT16: {
            int64_t val = TRY(parse_json_int, json_ptr);
            RAISE_IF(val < INT16_MIN || val > INT16_MAX, "Value out of range for 16-bit signed integer");
            *((int16_t*)voidstar) = (int16_t)val;
            break;
        }

        case MORLOC_SINT32: {
            int64_t val = TRY(parse_json_int, json_ptr);
            RAISE_IF(val < INT32_MIN || val > INT32_MAX, "Value out of range for 32-bit signed integer");
            *((int32_t*)voidstar) = (int32_t)val;
            break;
        }

        case MORLOC_SINT64: {
            int64_t val = TRY(parse_json_int, json_ptr);
            *((int64_t*)voidstar) = val;
            break;
        }

        case MORLOC_UINT8: {
            uint64_t val = TRY(parse_json_uint, json_ptr);
            RAISE_IF(val > UINT8_MAX, "Value out of range for 8-bit unsigned integer");
            *((uint8_t*)voidstar) = (uint8_t)val;
            break;
        }

        case MORLOC_UINT16: {
            uint64_t val = TRY(parse_json_uint, json_ptr);
            RAISE_IF(val > UINT16_MAX, "Value out of range for 16-bit unsigned integer");
            *((uint16_t*)voidstar) = (uint16_t)val;
            break;
        }

        case MORLOC_UINT32: {
            uint64_t val = TRY(parse_json_uint, json_ptr);
            RAISE_IF(val > UINT32_MAX, "Value out of range for 32-bit unsigned integer");
            *((uint32_t*)voidstar) = (uint32_t)val;
            break;
        }

        case MORLOC_UINT64: {
            uint64_t val = TRY(parse_json_uint, json_ptr);
            *((uint64_t*)voidstar) = val;
            break;
        }

        case MORLOC_FLOAT32: {
            double val = TRY(parse_json_double, json_ptr);
            RAISE_IF((val < -FLT_MAX || val > FLT_MAX) && !isinf(val), "Value out of range for 32-bit float");
            *((float*)voidstar) = (float)val;
            break;
        }

        case MORLOC_FLOAT64: {
            double val = TRY(parse_json_double, json_ptr);
            *((double*)voidstar) = val;
            break;
        }

        case MORLOC_STRING: {
            size_t j_string_size = 0;
            size_t c_string_size = 0;

            Array* arr = (Array*)voidstar;
            arr->size = 0;
            arr->data = RELNULL;

            TRY(json_string_size, *json_ptr, &j_string_size, &c_string_size);

            if(c_string_size == 0){
                // eat the empty string and leave
                TRY(consume_char, '"', json_ptr);
                TRY(consume_char, '"', json_ptr);
                break;
            }

            absptr_t mlc_str = TRY(shcalloc, c_string_size, sizeof(char));

            // If the C and JSON strings are the same length, then there are
            // no special characters to consider and we can simply memcpy.
            if(c_string_size == j_string_size){
                // copy from the position AFTER the initial quote
                memcpy(mlc_str, (*json_ptr) + 1, c_string_size);
                // consume the string and quotes
                *json_ptr += j_string_size + 2;
            }
            // Otherwise we need to loop through the string and handle escapees
            else {
                TRY(write_json_string, json_ptr, (char*)mlc_str);
            }

            arr->size = c_string_size;
            arr->data = TRY(abs2rel, mlc_str)

            break;
        }

        case MORLOC_ARRAY: {
            size_t size = TRY(json_array_size, *json_ptr);

            Array* arr = (Array*)voidstar;
            arr->size = size;

            TRY(consume_char, '[', json_ptr);

            if(size == 0){
                arr->data = RELNULL; // handle empty array
            } else {
                absptr_t array_data = TRY(shcalloc, size, schema->parameters[0]->width);

                for(size_t element_idx = 0; element_idx < size; element_idx++){
                    uint8_t* element = (uint8_t*)array_data + element_idx * schema->parameters[0]->width;
                    TRY(read_json_with_schema_r, element, json_ptr, schema->parameters[0])
                    consume_whitespace(json_ptr);
                    if(element_idx < size - 1){
                        TRY(consume_char, ',', json_ptr);
                    }
                }
                arr->data = TRY(abs2rel, array_data);
            }

            TRY(consume_char, ']', json_ptr);

            break;
        }

        case MORLOC_TUPLE: {
            TRY(consume_char, '[', json_ptr);

            for(size_t element_idx = 0; element_idx < schema->size; element_idx++){
                uint8_t* element = voidstar + schema->offsets[element_idx];
                TRY(read_json_with_schema_r, element, json_ptr, schema->parameters[element_idx])
                if(element_idx < (schema->size - 1)){
                    TRY(consume_char, ',', json_ptr);
                }
            }

            TRY(consume_char, ']', json_ptr);

            break;
        }

        case MORLOC_MAP: {
            TRY(consume_char, '{', json_ptr);

            uint8_t* element_ptr = NULL;
            size_t nentries_complete = 0;
            while(**json_ptr != '}'){
                RAISE_IF(**json_ptr == '\0', "Unexpected end of JSON object")

                // match '<key><space>:<space>"
                consume_whitespace(json_ptr);
                char* key = TRY(read_json_key, json_ptr)
                TRY(consume_char, ':', json_ptr);

                // Search for key in schema and write to appropriate offset
                bool parse_failed = true;
                for(size_t element_idx = 0; element_idx < schema->size; element_idx++){
                    // if the key is matches, parse to the appropriate offset
                    if(strcmp(key, schema->keys[element_idx]) == 0){
                        FREE(key)
                        element_ptr = voidstar + schema->offsets[element_idx];
                        TRY(
                            read_json_with_schema_r,
                            element_ptr,
                            json_ptr,
                            schema->parameters[element_idx]
                        )
                        nentries_complete++;
                        parse_failed = false;
                        break;
                    }
                }
                RAISE_IF_WITH(parse_failed, free(key), "Could not find key %s in record", key)
                FREE(key);

                consume_whitespace(json_ptr);
                if(**json_ptr == ','){
                    (*json_ptr)++;
                }
            }
            RAISE_IF(nentries_complete != schema->size, "Expected %zu entries in JSON object, only found %zu", schema->size, nentries_complete)

            TRY(consume_char, '}', json_ptr);
            break;
        }

        default:
            RAISE("Unhandled schema type %c", **json_ptr)
            break;
    }

    return EXIT_PASS;
}

uint8_t* read_json_with_schema(
  uint8_t* voidstar, // the destination memory location, if NULL, allocate
  char* json_data,
  const Schema* schema,
  ERRMSG
){
    PTR_RETURN_SETUP(uint8_t)

    char* json_ptr = json_data;

    if(voidstar == NULL){
        voidstar = (uint8_t*)TRY(shcalloc, 1, schema->width);
    }
    TRY(read_json_with_schema_r, voidstar, &json_ptr, schema);

    return voidstar;
}

static bool print_voidstar_r(const void* voidstar, const Schema* schema, ERRMSG) {
    BOOL_RETURN_SETUP

    Array* array;
    const char* data;

    switch (schema->type) {
        case MORLOC_NIL:
            printf("null");
            break;
        case MORLOC_BOOL:
            printf(*(uint8_t*)voidstar ? "true" : "false");
            break;
        case MORLOC_UINT8:
            printf("%u", *(uint8_t*)voidstar);
            break;
        case MORLOC_UINT16:
            printf("%u", *(uint16_t*)voidstar);
            break;
        case MORLOC_UINT32:
            printf("%u", *(uint32_t*)voidstar);
            break;
        case MORLOC_UINT64:
            printf("%lu", *(uint64_t*)voidstar);
            break;
        case MORLOC_SINT8:
            printf("%d", *(int8_t*)voidstar);
            break;
        case MORLOC_SINT16:
            printf("%d", *(int16_t*)voidstar);
            break;
        case MORLOC_SINT32:
            printf("%d", *(int32_t*)voidstar);
            break;
        case MORLOC_SINT64:
            printf("%ld", *(int64_t*)voidstar);
            break;
        case MORLOC_FLOAT32:
            printf("%.7g", *(float*)voidstar);
            break;
        case MORLOC_FLOAT64:
            printf("%.15g", *(double*)voidstar);
            break;
        case MORLOC_STRING:
            {
                array = (Array*)voidstar;

                if(array->size == 0){
                    printf("\"\"");
                    break;
                }

                data = TRY((const char*)rel2abs, array->data);

                char* escaped_string = json_escape_string(data, array->size);
                if (escaped_string) {
                    printf("\"%s\"", escaped_string);
                    FREE(escaped_string);
                } else {
                    RAISE("Memory allocation failed for string escaping");
                }
            }
            break;
        case MORLOC_ARRAY:
            {
                array = (Array*)voidstar;

                if(array->size == 0){
                    printf("[]");
                    break;
                }

                data = TRY((const char*)rel2abs, array->data);

                printf("[");
                for (size_t i = 0; i < array->size; i++) {
                    if (i > 0) printf(",");
                    bool success = print_voidstar_r(data + i * schema->parameters[0]->width, schema->parameters[0], &CHILD_ERRMSG);
                    RAISE_IF(!success, "\n%s", CHILD_ERRMSG)
                }
                printf("]");
            }
            break;
        case MORLOC_TUPLE:
            {
                printf("[");
                for (size_t i = 0; i < schema->size; i++) {
                    if (i > 0) printf(",");
                    bool success = print_voidstar_r((char*)voidstar + schema->offsets[i], schema->parameters[i], &CHILD_ERRMSG);
                    RAISE_IF(!success, "\n%s", CHILD_ERRMSG)
                }
                printf("]");
            }
            break;
        case MORLOC_MAP:
            {
                printf("{");
                for (size_t i = 0; i < schema->size; i++) {
                    if (i > 0) printf(",");
                    printf("\"%s\":", schema->keys[i]);
                    bool success = print_voidstar_r((char*)voidstar + schema->offsets[i], schema->parameters[i], &CHILD_ERRMSG);
                    RAISE_IF(!success, "\n%s", CHILD_ERRMSG)
                }
                printf("}");
            }
            break;
        default:
            RAISE("Unexpected morloc type");
    }

    return true;
}

bool print_voidstar(const void* voidstar, const Schema* schema, ERRMSG) {
    BOOL_RETURN_SETUP

    // print JSON with no spaces
    bool success = print_voidstar_r(voidstar, schema, &CHILD_ERRMSG);
    RAISE_IF(!success, "\n%s", CHILD_ERRMSG)

    // add terminal newline
    // IMPORTANT: the newline distinguishes JSON from MessagePack for 0-9 values
    printf("\n");

    return success;
}

// ======================================================================
// JSON writer
// ======================================================================

#define JSON_BUF_INITIAL_CAP 256

static void json_buf_grow(json_buf_t* buf, size_t needed) {
    if (buf->len + needed < buf->cap) return;
    while (buf->len + needed >= buf->cap) {
        buf->cap *= 2;
    }
    buf->data = (char*)realloc(buf->data, buf->cap);
}

static void json_buf_append(json_buf_t* buf, const char* s, size_t n) {
    json_buf_grow(buf, n + 1);
    memcpy(buf->data + buf->len, s, n);
    buf->len += n;
    buf->data[buf->len] = '\0';
}

static void json_buf_append_str(json_buf_t* buf, const char* s) {
    json_buf_append(buf, s, strlen(s));
}

static void json_buf_maybe_comma(json_buf_t* buf) {
    if (buf->needs_comma) {
        json_buf_append(buf, ",", 1);
    }
}

json_buf_t* json_buf_new(void) {
    json_buf_t* buf = (json_buf_t*)calloc(1, sizeof(json_buf_t));
    buf->cap = JSON_BUF_INITIAL_CAP;
    buf->data = (char*)calloc(buf->cap, 1);
    buf->len = 0;
    buf->needs_comma = false;
    return buf;
}

void json_buf_free(json_buf_t* buf) {
    if (!buf) return;
    free(buf->data);
    free(buf);
}

char* json_buf_finish(json_buf_t* buf) {
    char* result = buf->data;
    buf->data = NULL;
    free(buf);
    return result;
}

void json_write_obj_start(json_buf_t* buf) {
    json_buf_maybe_comma(buf);
    json_buf_append(buf, "{", 1);
    buf->needs_comma = false;
}

void json_write_obj_end(json_buf_t* buf) {
    json_buf_append(buf, "}", 1);
    buf->needs_comma = true;
}

void json_write_arr_start(json_buf_t* buf) {
    json_buf_maybe_comma(buf);
    json_buf_append(buf, "[", 1);
    buf->needs_comma = false;
}

void json_write_arr_end(json_buf_t* buf) {
    json_buf_append(buf, "]", 1);
    buf->needs_comma = true;
}

void json_write_key(json_buf_t* buf, const char* key) {
    json_buf_maybe_comma(buf);
    json_buf_append(buf, "\"", 1);
    char* escaped = json_escape_string(key, strlen(key));
    if (escaped) {
        json_buf_append_str(buf, escaped);
        free(escaped);
    }
    json_buf_append(buf, "\":", 2);
    buf->needs_comma = false;
}

void json_write_string(json_buf_t* buf, const char* val) {
    json_buf_maybe_comma(buf);
    json_buf_append(buf, "\"", 1);
    if (val) {
        char* escaped = json_escape_string(val, strlen(val));
        if (escaped) {
            json_buf_append_str(buf, escaped);
            free(escaped);
        }
    }
    json_buf_append(buf, "\"", 1);
    buf->needs_comma = true;
}

void json_write_int(json_buf_t* buf, int val) {
    json_buf_maybe_comma(buf);
    char tmp[32];
    int n = snprintf(tmp, sizeof(tmp), "%d", val);
    json_buf_append(buf, tmp, (size_t)n);
    buf->needs_comma = true;
}

void json_write_uint(json_buf_t* buf, unsigned int val) {
    json_buf_maybe_comma(buf);
    char tmp[32];
    int n = snprintf(tmp, sizeof(tmp), "%u", val);
    json_buf_append(buf, tmp, (size_t)n);
    buf->needs_comma = true;
}

void json_write_bool(json_buf_t* buf, bool val) {
    json_buf_maybe_comma(buf);
    if (val) {
        json_buf_append(buf, "true", 4);
    } else {
        json_buf_append(buf, "false", 5);
    }
    buf->needs_comma = true;
}

void json_write_null(json_buf_t* buf) {
    json_buf_maybe_comma(buf);
    json_buf_append(buf, "null", 4);
    buf->needs_comma = true;
}

void json_write_raw(json_buf_t* buf, const char* raw) {
    json_buf_maybe_comma(buf);
    json_buf_append_str(buf, raw);
    buf->needs_comma = true;
}

// ======================================================================
// voidstar to JSON string (buffer-based, no stdout)
// ======================================================================

static bool voidstar_to_json_buf_r(json_buf_t* jb, const void* voidstar, const Schema* schema, ERRMSG) {
    BOOL_RETURN_SETUP

    Array* array;
    const char* data;

    switch (schema->type) {
        case MORLOC_NIL:
            json_buf_append(jb, "null", 4);
            break;
        case MORLOC_BOOL:
            if (*(uint8_t*)voidstar)
                json_buf_append(jb, "true", 4);
            else
                json_buf_append(jb, "false", 5);
            break;
        case MORLOC_UINT8: {
            char tmp[32];
            int n = snprintf(tmp, sizeof(tmp), "%u", *(uint8_t*)voidstar);
            json_buf_append(jb, tmp, (size_t)n);
            break;
        }
        case MORLOC_UINT16: {
            char tmp[32];
            int n = snprintf(tmp, sizeof(tmp), "%u", *(uint16_t*)voidstar);
            json_buf_append(jb, tmp, (size_t)n);
            break;
        }
        case MORLOC_UINT32: {
            char tmp[32];
            int n = snprintf(tmp, sizeof(tmp), "%u", *(uint32_t*)voidstar);
            json_buf_append(jb, tmp, (size_t)n);
            break;
        }
        case MORLOC_UINT64: {
            char tmp[32];
            int n = snprintf(tmp, sizeof(tmp), "%lu", *(uint64_t*)voidstar);
            json_buf_append(jb, tmp, (size_t)n);
            break;
        }
        case MORLOC_SINT8: {
            char tmp[32];
            int n = snprintf(tmp, sizeof(tmp), "%d", *(int8_t*)voidstar);
            json_buf_append(jb, tmp, (size_t)n);
            break;
        }
        case MORLOC_SINT16: {
            char tmp[32];
            int n = snprintf(tmp, sizeof(tmp), "%d", *(int16_t*)voidstar);
            json_buf_append(jb, tmp, (size_t)n);
            break;
        }
        case MORLOC_SINT32: {
            char tmp[32];
            int n = snprintf(tmp, sizeof(tmp), "%d", *(int32_t*)voidstar);
            json_buf_append(jb, tmp, (size_t)n);
            break;
        }
        case MORLOC_SINT64: {
            char tmp[32];
            int n = snprintf(tmp, sizeof(tmp), "%ld", *(int64_t*)voidstar);
            json_buf_append(jb, tmp, (size_t)n);
            break;
        }
        case MORLOC_FLOAT32: {
            char tmp[32];
            int n = snprintf(tmp, sizeof(tmp), "%.7g", *(float*)voidstar);
            json_buf_append(jb, tmp, (size_t)n);
            break;
        }
        case MORLOC_FLOAT64: {
            char tmp[32];
            int n = snprintf(tmp, sizeof(tmp), "%.15g", *(double*)voidstar);
            json_buf_append(jb, tmp, (size_t)n);
            break;
        }
        case MORLOC_STRING: {
            array = (Array*)voidstar;
            if (array->size == 0) {
                json_buf_append(jb, "\"\"", 2);
                break;
            }
            data = TRY((const char*)rel2abs, array->data);
            char* escaped = json_escape_string(data, array->size);
            if (escaped) {
                json_buf_append(jb, "\"", 1);
                json_buf_append_str(jb, escaped);
                json_buf_append(jb, "\"", 1);
                free(escaped);
            } else {
                RAISE("Memory allocation failed for string escaping");
            }
            break;
        }
        case MORLOC_ARRAY: {
            array = (Array*)voidstar;
            if (array->size == 0) {
                json_buf_append(jb, "[]", 2);
                break;
            }
            data = TRY((const char*)rel2abs, array->data);
            json_buf_append(jb, "[", 1);
            for (size_t i = 0; i < array->size; i++) {
                if (i > 0) json_buf_append(jb, ",", 1);
                TRY(voidstar_to_json_buf_r, jb,
                    data + i * schema->parameters[0]->width,
                    schema->parameters[0]);
            }
            json_buf_append(jb, "]", 1);
            break;
        }
        case MORLOC_TUPLE: {
            json_buf_append(jb, "[", 1);
            for (size_t i = 0; i < schema->size; i++) {
                if (i > 0) json_buf_append(jb, ",", 1);
                TRY(voidstar_to_json_buf_r, jb,
                    (char*)voidstar + schema->offsets[i],
                    schema->parameters[i]);
            }
            json_buf_append(jb, "]", 1);
            break;
        }
        case MORLOC_MAP: {
            json_buf_append(jb, "{", 1);
            for (size_t i = 0; i < schema->size; i++) {
                if (i > 0) json_buf_append(jb, ",", 1);
                // write key
                json_buf_append(jb, "\"", 1);
                char* escaped_key = json_escape_string(schema->keys[i], strlen(schema->keys[i]));
                if (escaped_key) {
                    json_buf_append_str(jb, escaped_key);
                    free(escaped_key);
                }
                json_buf_append(jb, "\":", 2);
                // write value
                TRY(voidstar_to_json_buf_r, jb,
                    (char*)voidstar + schema->offsets[i],
                    schema->parameters[i]);
            }
            json_buf_append(jb, "}", 1);
            break;
        }
        default:
            RAISE("Unexpected morloc type");
    }

    return true;
}

char* voidstar_to_json_string(const void* voidstar, const Schema* schema, ERRMSG) {
    PTR_RETURN_SETUP(char)

    json_buf_t* jb = json_buf_new();
    bool ok = voidstar_to_json_buf_r(jb, voidstar, schema, &CHILD_ERRMSG);
    if (!ok) {
        json_buf_free(jb);
        RAISE("Failed to serialize voidstar to JSON:\n%s", CHILD_ERRMSG);
    }

    return json_buf_finish(jb);
}
