#include "morloc.h"
#include "json.h"

// Forward declarations for voidstar flat format
int adjust_voidstar_relptrs(void* data, const Schema* schema, relptr_t base_rel, ERRMSG);
void* read_voidstar_binary(const uint8_t* blob, size_t blob_size, const Schema* schema, ERRMSG);

argument_t* initialize_positional(char* value){
  argument_t* arg = (argument_t*)calloc(1, sizeof(argument_t));
  arg->value = strdup(value);
  arg->size = 0;
  return arg;
}

argument_t* initialize_unrolled(size_t size, char* default_value, char** fields, char** default_fields){
  argument_t* arg = (argument_t*)calloc(1, sizeof(argument_t));
  if(default_value != NULL){
    arg->value = strdup(default_value);
  }
  arg->size = size;
  arg->fields = (char**)calloc(size, sizeof(char*));
  for(size_t i = 0; i < size; i++){
    if(fields[i] != NULL){
      arg->fields[i] = strdup(fields[i]);
    }
  }
  arg->default_fields = (char**)calloc(size, sizeof(char*));
  for(size_t i = 0; i < size; i++){
    if(default_fields[i] != NULL){
      arg->default_fields[i] = strdup(default_fields[i]);
    }
  }
  return arg;
}

void free_argument_t(argument_t* arg){
  FREE(arg->value)
  if(arg != NULL && arg->fields != NULL){
      for(size_t i = 0; i < arg->size; i++){
          FREE(arg->fields[i]);
      }
      free(arg->fields);
  }
  if(arg != NULL && arg->default_fields != NULL){
      for(size_t i = 0; i < arg->size; i++){
          FREE(arg->default_fields[i]);
      }
      free(arg->default_fields);
  }
  FREE(arg);
}

static int upload_packet(
  absptr_t dest,
  const uint8_t* data,
  size_t data_end, // value of data end pointer, used for checking
  const Schema* schema,
  ERRMSG
){
    INT_RETURN_SETUP

    switch(schema->type){
        case MORLOC_STRING:
        case MORLOC_ARRAY: {
            RAISE_IF(
                ((size_t)data + schema->width - 1) <= data_end,
                "Data is too small to store an array header"
            )
            memcpy(dest, data, schema->width);
            // cast the destination memory as an Array
            Array* arr = (Array*) dest;
            uint8_t* arr_data = (uint8_t*)data + arr->data;
            size_t arr_size = arr->size * schema->parameters[0]->width;

            // mutate relative pointer to array contents
            absptr_t data_ptr = TRY(shmemcpy, arr_data, arr_size);

            RAISE_IF(
                ((size_t)arr_data + arr_size - 1) > data_end,
                "Data is too small to contain the values pointed to by this array"
            )

            if(!schema_is_fixed_width(schema)){
                size_t width = schema->parameters[0]->width;
                for(size_t i = 0; i < arr->size; i++){
                    TRY( upload_packet,
                         (absptr_t)((char*)data_ptr + i * width),
                         arr_data + i * width,
                         data_end,
                         schema->parameters[0]
                       )
                }
            }

            arr->data = TRY(abs2rel, data_ptr)

        }; break;
        case MORLOC_TUPLE:
        case MORLOC_MAP: {
            for(size_t i = 0; i < schema->size; i++){
               TRY( upload_packet,
                    (absptr_t)((char*)dest + schema->offsets[i]),
                    data + schema->offsets[i],
                    data_end,
                    schema->parameters[i]
                  )
            }
        }; break;

        // in all other cases, the type is of fixed width (e.g., a primitive)
        default: {
            // die if the given data is not large enough
            if(((size_t)data + schema->width - 1) > data_end){
                RAISE("Given data packet is too small to contain the expected data")
            }
            // otherwise, copy the main object to the destination
            memcpy(dest, data, schema->width);
        }
    }

    return EXIT_PASS;
}

// Walk a voidstar blob and adjust all Array.data relptrs by adding base_rel.
// The blob was written with relptrs starting at 0; after copying into shared
// memory at offset base_rel, every relptr must be shifted.
int adjust_voidstar_relptrs(void* data, const Schema* schema, relptr_t base_rel, ERRMSG) {
    INT_RETURN_SETUP

    switch (schema->type) {
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            {
                Array* arr = (Array*)data;
                arr->data += base_rel;
                // Recurse into elements if they contain variable-width children
                if (!schema_is_fixed_width(schema->parameters[0])) {
                    absptr_t arr_data = TRY(rel2abs, arr->data);
                    for (size_t i = 0; i < arr->size; i++) {
                        void* elem = (void*)((char*)arr_data + i * schema->parameters[0]->width);
                        TRY(adjust_voidstar_relptrs, elem, schema->parameters[0], base_rel);
                    }
                }
            }
            break;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            for (size_t i = 0; i < schema->size; i++) {
                void* child = (void*)((char*)data + schema->offsets[i]);
                TRY(adjust_voidstar_relptrs, child, schema->parameters[i], base_rel);
            }
            break;
        case MORLOC_OPTIONAL:
            {
                uint8_t tag = *((uint8_t*)data);
                if (tag != 0) {
                    TRY(adjust_voidstar_relptrs, (char*)data + schema->offsets[0], schema->parameters[0], base_rel);
                }
            }
            break;
        case MORLOC_TENSOR:
            {
                Tensor* tensor = (Tensor*)data;
                if (tensor->total_elements > 0) {
                    tensor->shape += base_rel;
                    tensor->data += base_rel;
                }
            }
            break;
        default:
            break;
    }
    return EXIT_PASS;
}

// Read a flat voidstar binary blob into shared memory.
// The blob contains the serialized voidstar with relptrs starting at 0.
// We allocate shared memory, copy the blob, then adjust all relptrs.
void* read_voidstar_binary(const uint8_t* blob, size_t blob_size, const Schema* schema, ERRMSG) {
    PTR_RETURN_SETUP(void)

    void* base = TRY(shmalloc, blob_size);
    memcpy(base, blob, blob_size);

    char* cleanup_errmsg = NULL;
    relptr_t base_rel = abs2rel((absptr_t)base, &CHILD_ERRMSG);
    if (CHILD_ERRMSG != NULL) {
        shfree(base, &cleanup_errmsg);
        free(cleanup_errmsg);
        RAISE("%s", CHILD_ERRMSG)
    }

    if (adjust_voidstar_relptrs(base, schema, base_rel, &CHILD_ERRMSG) != 0) {
        shfree(base, &cleanup_errmsg);
        free(cleanup_errmsg);
        RAISE("%s", CHILD_ERRMSG)
    }

    return base;
}



// Load a data file in JSON, MessagePack or Voidstar
void* load_morloc_data_file(const char* path, uint8_t* data, size_t data_size, const Schema* schema, ERRMSG) {
    PTR_RETURN_SETUP(void)

    if(data_size == 0){
        RAISE("Cannot parse 0-length data")
    }

    void* result = NULL;

    // 1. Extension-based dispatch
    if (has_suffix(path, ".json")) {
        uint8_t* json_buf = (uint8_t*)realloc(data, data_size + 1);
        RAISE_IF_WITH(json_buf == NULL, free(data), "Failed to allocate for JSON parsing")
        json_buf[data_size] = '\0';
        result = (void*)read_json_with_schema(NULL, (char*)json_buf, schema, &CHILD_ERRMSG);
        RAISE_IF_WITH(CHILD_ERRMSG != NULL, free(json_buf), "Failed to read JSON file '%s': %s", path, CHILD_ERRMSG)
        free(json_buf);
        return result;
    }

    if (has_suffix(path, ".mpk") || has_suffix(path, ".msgpack")) {
        TRY_WITH(free(data), unpack_with_schema, (char*)data, data_size, schema, &result);
        free(data);
        return result;
    }

    // 2. Check for morloc packet header
    if (data_size >= sizeof(morloc_packet_header_t)) {
        uint32_t magic = *(uint32_t*)data;
        if (magic == MORLOC_PACKET_MAGIC) {
            morloc_packet_header_t* header = (morloc_packet_header_t*)data;
            RAISE_IF_WITH(
                header->command.cmd_type.type != PACKET_TYPE_DATA,
                free(data),
                "Expected a data packet in file '%s'", path
            )
            const uint8_t* payload = data + sizeof(morloc_packet_header_t) + header->offset;
            size_t payload_size = header->length;

            if (header->command.data.format == PACKET_FORMAT_VOIDSTAR) {
                result = TRY_WITH(free(data), read_voidstar_binary, payload, payload_size, schema);
                free(data);
                return result;
            } else if (header->command.data.format == PACKET_FORMAT_MSGPACK) {
                TRY_WITH(free(data), unpack_with_schema, (char*)payload, payload_size, schema, &result);
                free(data);
                return result;
            } else {
                free(data);
                RAISE("Unsupported packet format 0x%02x in file '%s'", header->command.data.format, path)
            }
        }
    }

    // Check for JSON/MessagePack overlap
    //
    // MessagePack uses the bytes 0x00 to 0x7f to encode the integers 0-127.
    // So for widths greater than 1, the first MessagePack character is always
    // greater than 0x7f, thus outside JSON range and unambiguous. The only
    // overlap can be in 1-byte data.
    //
    // The only valid 1-byte JSON values are the integers 0-9. So these are the
    // only overlap between JSON and MessagePack. The character values '0' to
    // '9' have byte values of 0x30 to 0x39 so MessagePack parses them as fixint
    // 48-57.
    //
    // Both JSON and raw ASCII data interpret bytes 0x30-0x39 as 0-9. Input
    // here is likely STDIN or some text file in the UNIX world, so the 0-9
    // interpretation is more likely.
    bool may_be_json_or_mpk = data_size == 1 && data[0] >= 0x30 && data[0] <= 0x39;

    bool may_be_json;
    switch (data[0]) {
        case '\'':
        case '"':
        case '[':
        case '{':
        case 't': // true
        case 'f': // false
        case 'n': // null
        // whitspace
        case '\t':
        case '\n':
        case '\r':
        case ' ':
        // numbers (MessagePack fixint overlap)
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
        case '-': // negation
          may_be_json = true;
          break;
        default:
          may_be_json = false;
    }

    if ((data_size > 1 && may_be_json) || may_be_json_or_mpk) {
        data = (uint8_t*)realloc(data, data_size + 1);
        RAISE_IF(data == NULL, "Failed to allocate for JSON parsing")
        data[data_size] = '\0';
        result = (void*)read_json_with_schema(NULL, (char*)data, schema, &CHILD_ERRMSG);
        if (CHILD_ERRMSG == NULL && result != NULL) {
            free(data);
            return result;
        }
        FREE(CHILD_ERRMSG)
    }

    if (data_size > 1 && data[0] <= 0x7f){
        RAISE("Cannot parse data, does not seem to be JSON, MessagePack or VoidStar")
    }

    // 4. Fall through to parse as MessagePack
    TRY(unpack_with_schema, (char*)data, data_size, schema, &result);

    return result;
}



static uint8_t* parse_cli_data_argument_singular(uint8_t* dest, char* arg, const Schema* schema, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)

    FILE* fd = NULL;

    // handle STDIN
    if(strcmp(arg, "/dev/stdin") == 0 || strcmp(arg, "-") == 0){
        fd = stdin;
    }
    // If the argument is a file, try to open it and parse it
    else if(file_exists(arg)){
        fd = fopen(arg, "rb");
        RAISE_IF(fd == NULL, "The argument '%s' is a filename, but it can't be read:\n%s", arg, strerror(errno));
    }

    if(fd == NULL){
        // The argument is literal data (JSON)
        if(dest == NULL){
            dest = (uint8_t*)TRY(shcalloc, 1, schema->width);
        }
        dest = read_json_with_schema(dest, arg, schema, &CHILD_ERRMSG);
        RAISE_IF(CHILD_ERRMSG != NULL, "Failed to read argument:\n%s", CHILD_ERRMSG)
        return dest;
    }

    // The argument is a file or stdin
    size_t data_size = 0;
    uint8_t* data = read_binary_fd(fd, &data_size, &CHILD_ERRMSG);
    if(fd != stdin){
        fclose(fd);
    }
    RAISE_IF_WITH(CHILD_ERRMSG != NULL, free(data), "\n%s", CHILD_ERRMSG)

    // Special case: RPTR packets (pool-to-pool arrow with data already in shm)
    if(data_size >= sizeof(morloc_packet_header_t)){
        uint32_t magic = *(uint32_t*)data;
        if(magic == MORLOC_PACKET_MAGIC){
            morloc_packet_header_t* header = (morloc_packet_header_t*)data;
            if(header->command.data.source == PACKET_SOURCE_RPTR
               && header->command.data.format == PACKET_FORMAT_VOIDSTAR){
                if(dest == NULL){
                    dest = (uint8_t*)TRY_WITH(free(data), shcalloc, 1, schema->width);
                }
                uint8_t* voidstar_ptr = data + sizeof(morloc_packet_header_t) + header->offset;
                TRY_WITH(free(data), upload_packet, dest, voidstar_ptr, (size_t)voidstar_ptr + data_size - 1, schema);
                free(data);
                return dest;
            }
        }
    }

    // All other formats: use canonical file loader (takes ownership of data)
    dest = (uint8_t*)TRY(load_morloc_data_file, arg, data, data_size, schema);
    return dest;
}

bool shfree_by_schema(absptr_t ptr, const Schema* schema, ERRMSG){
    BOOL_RETURN_SETUP

    switch(schema->type) {
        case MORLOC_STRING:
        case MORLOC_ARRAY: {
            Array* arr = (Array*)ptr;
            if(arr->data > 0){
              absptr_t arr_data = TRY(rel2abs, arr->data)
              if(!schema_is_fixed_width(schema->parameters[0])){
                for(size_t i = 0; i < arr->size; i++){
                  absptr_t element_ptr = (absptr_t)((char*)arr_data + i * schema->parameters[0]->width);
                  TRY(shfree_by_schema, element_ptr, schema->parameters[0])
                }
              }
              TRY(shfree, arr_data)
            }
          }
          break;
        case MORLOC_TUPLE:
        case MORLOC_MAP: {
          // free every element
            for(size_t i = 0; i < schema->size; i++){
              absptr_t element_ptr = (absptr_t)((char*)ptr + schema->offsets[i]);
              TRY(shfree_by_schema, element_ptr, schema->parameters[i])
            }
          }
          break;
        case MORLOC_TENSOR:
          // shape and data are inline in the same allocation (cursor pattern),
          // freed by the parent shfree
          break;
        default:
          // fixed-size types will directly over-written
          // no freeing needed
          break;
    }

    // zero location
    memset(ptr, 0, schema->width);

    return true;
}

static uint8_t* parse_cli_data_argument_unrolled(uint8_t* dest, char* default_value, char** fields, char** default_fields, const Schema* schema, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)

    bool using_record_default = false;
    if(dest == NULL){
        dest = (uint8_t*) TRY(shcalloc, 1, schema->width)
    }

    if(default_value != NULL){
        dest = TRY(parse_cli_data_argument_singular, dest, default_value, schema)
        using_record_default = true;
    }

    switch(schema->type) {
        case MORLOC_TUPLE:
        case MORLOC_MAP: {
            for(size_t i = 0; i < schema->size; i++){
                uint8_t* element_dest = dest + schema->offsets[i];

                // if the user provided a value, use it
                if(fields[i] != NULL){
                    // free any memory written in the default record for this field
                    TRY(shfree_by_schema, (absptr_t)element_dest, schema->parameters[i])

                    TRY(
                        parse_cli_data_argument_singular,
                        element_dest,
                        fields[i],
                        schema->parameters[i]
                    )
                // if the user provided no specific value, but specified a
                // default record, use that
                } else if (using_record_default) {
                    continue;
                // otherwise use the hard-coded default
                } else if (default_fields[i] != NULL) {
                    TRY(
                        parse_cli_data_argument_singular,
                        element_dest,
                        default_fields[i],
                        schema->parameters[i]
                    )
                // if there isn't even a hard-coded default (an issue the
                // compiler should catch), then die screaming
                } else {
                    RAISE("Field %zu missing with no default or default record", i)
                }
            }
        }; break;
        default: {
            RAISE("Only record an tuple types may be unrolled")
        }
    }
    return dest;
}

uint8_t* parse_cli_data_argument(uint8_t* dest, const argument_t* arg, const Schema* schema, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)

    // modify dest if it is not NULL, otherwise allocate it
    if(arg->fields == NULL) {
        dest = TRY(parse_cli_data_argument_singular, dest, arg->value, schema);
    } else {
        dest = TRY(parse_cli_data_argument_unrolled, dest, arg->value, arg->fields, arg->default_fields, schema);
    }

    relptr_t relptr = TRY(abs2rel, dest);
    uint8_t* packet_arg = make_standard_data_packet(relptr, schema);

    return packet_arg;
}

uint8_t* make_call_packet_from_cli(
    uint8_t* dest,
    uint32_t mid,
    argument_t** args, // NULL terminated array of arguments
    char** arg_schema_strs, // NULL terminated array of schema strings
    ERRMSG
){
    PTR_RETURN_SETUP(uint8_t)

    size_t nschemas = 0;
    for(size_t i = 0; arg_schema_strs[i] != NULL; i++){
        nschemas++;
    }

    const Schema** schemas = (const Schema**)calloc(nschemas + 1, sizeof(Schema*));
    RAISE_IF(schemas == NULL, "Failed to allocate memory for schemas")

    for(size_t i = 0; arg_schema_strs[i] != NULL; i++){
        schemas[i] = parse_schema(arg_schema_strs[i], &CHILD_ERRMSG);
        if(CHILD_ERRMSG != NULL){
            for(size_t j = 0; j < i; j++) free_schema((Schema*)schemas[j]);
            free(schemas);
            RAISE("Failed to parse argument %zu:\n%s", i, CHILD_ERRMSG)
        }
    }
    schemas[nschemas] = NULL;

    size_t nargs = 0;
    for(size_t i = 0; args[i] != NULL; i++){
        nargs++;
    }

    const uint8_t** packet_args = (const uint8_t**)calloc(nargs, sizeof(uint8_t*));
    if(packet_args == NULL){
        for(size_t j = 0; j < nschemas; j++) free_schema((Schema*)schemas[j]);
        free(schemas);
        RAISE("Failed to allocate packet_args");
    }

    for(size_t i = 0; i < nargs; i++){
        packet_args[i] = parse_cli_data_argument(dest, args[i], schemas[i], &CHILD_ERRMSG);
        if(CHILD_ERRMSG != NULL){
            for(size_t j = 0; j < i; j++) free((void*)packet_args[j]);
            for(size_t j = 0; j < nschemas; j++) free_schema((Schema*)schemas[j]);
            free(schemas);
            free(packet_args);
            RAISE("Failed to parse argument %zu\n%s", i, CHILD_ERRMSG);
        }
    }

    uint8_t* call_packet = make_morloc_local_call_packet(mid, packet_args, nargs, &CHILD_ERRMSG);

    for(size_t j = 0; j < nargs; j++) free((void*)packet_args[j]);
    for(size_t j = 0; j < nschemas; j++) free_schema((Schema*)schemas[j]);
    free(schemas);
    free(packet_args);

    RAISE_IF(CHILD_ERRMSG != NULL, "Failed to make call packet:\n%s", CHILD_ERRMSG);

    return call_packet;
}
