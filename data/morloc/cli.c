#include "morloc.h"
#include "json.h"

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

static bool maybe_msgpack(const uint8_t* data, size_t size) {
    if (size < 1) return false;

    // Check for MessagePack's initial byte patterns
    uint8_t c = data[0];

    return c > 0x7f || (c <= 0x7f && size == 1);
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

static uint8_t* parse_cli_data_argument_singular(uint8_t* dest, char* arg, const Schema* schema, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)

    FILE* fd;

    if(dest == NULL){
        dest = (uint8_t*)TRY(shcalloc, 1, schema->width);
    }

    // handle STDIN
    if(strcmp(arg, "/dev/stdin") == 0 || strcmp(arg, "-") == 0){
        fd = stdin;
    }
    // If the argument is a file, try to open it and parse it as JSON or MessagePack
    else if(file_exists(arg)){
        fd = fopen(arg, "rb");
        RAISE_IF(fd == NULL, "The argument '%s' is a filename, but it can't be read:\n%s", arg, strerror(errno));
    } else {
        fd = NULL; // this argument is not a file
    }

    if(fd == NULL){
        // The argument is the data
        // JSON is currently the only supported option
        dest = read_json_with_schema(dest, arg, schema, &CHILD_ERRMSG);
        RAISE_IF(CHILD_ERRMSG != NULL, "Failed to read argument:\n%s", CHILD_ERRMSG)
        return dest;
    } else {
        // The argument is a file
        size_t data_size = 0;
        char* data = (char*)read_binary_fd(fd, &data_size, &CHILD_ERRMSG);
        if(fd != stdin){
            fclose(fd);
        }
        RAISE_IF_WITH(CHILD_ERRMSG != NULL, free(data), "\n%s", CHILD_ERRMSG)

        if(has_suffix(arg, ".json")){
            dest = read_json_with_schema(dest, data, schema, &CHILD_ERRMSG);
            // If this isn't a JSON file, but you say it is, then either you are
            // confused or evil. In either case, I'll just play it safe and die.
            RAISE_IF_WITH(CHILD_ERRMSG != NULL, free(data), "Failed to read json argument file '%s':\n%s", arg, CHILD_ERRMSG)
            return dest;
        }

        else if(has_suffix(arg, ".mpk") || has_suffix(arg, ".msgpack")){
            unpack_with_schema(data, data_size, schema, (void**)(&dest), &CHILD_ERRMSG);
            RAISE_IF_WITH(
              CHILD_ERRMSG != NULL,
              free(data),
              "Failed to read MessagePack argument file '%s':\n%s",
              arg,
              CHILD_ERRMSG
            )
            return dest;
        }

        // If the extension is not recognized
        // First try to read it as a morloc voidstar packet if the data is at
        // least as large as a header
        if(data_size >= sizeof(morloc_packet_header_t)){
            morloc_packet_header_t* header = read_morloc_packet_header((uint8_t*)data, &CHILD_ERRMSG);
            if(CHILD_ERRMSG == NULL && header != NULL){
                uint8_t source = header->command.data.source;
                uint8_t format = header->command.data.format;

                if (source == PACKET_SOURCE_RPTR) {
                    if (format == PACKET_FORMAT_VOIDSTAR) {
                        // get a pointer to the payload start
                        uint8_t* voidstar_ptr = (uint8_t*)(data + sizeof(morloc_packet_header_t) + header->offset);
                        TRY( upload_packet,
                             dest,
                             voidstar_ptr,
                             (size_t)voidstar_ptr + data_size - 1,
                             schema
                        );
                        free(data);
                        return dest;
                    }
                    RAISE_WITH(free(data), "For RPTR source, expected voidstar format, found: 0x%02hhx", format);
                }
            }
        }

        // Next check if it is MessagePack
        if(maybe_msgpack((uint8_t*)data, data_size)){
            // Try to parse as MessagePack
            unpack_with_schema(data, data_size, schema, (void**)(&dest), &CHILD_ERRMSG);
            RAISE_IF_WITH(
              CHILD_ERRMSG != NULL,
              free(data),
              "Failed to read MessagePack argument:\n%s",
              CHILD_ERRMSG
            )
        }

        // Try to parse as JSON
        dest = read_json_with_schema(dest, data, schema, &CHILD_ERRMSG);

        RAISE_IF_WITH(CHILD_ERRMSG != NULL, free(data), "Failed to read json argument: %s", CHILD_ERRMSG)

        free(data);

        return dest;
    }
}

static bool shfree_by_schema(absptr_t ptr, const Schema* schema, ERRMSG){
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
        RAISE_IF_WITH(CHILD_ERRMSG != NULL, free(schemas), "Failed to parse argument %zu:\n%s", i, CHILD_ERRMSG)
    }
    schemas[nschemas] = NULL;

    size_t nargs = 0;
    for(size_t i = 0; args[i] != NULL; i++){
        nargs++;
    }

    const uint8_t** packet_args = (const uint8_t**)calloc(nargs, sizeof(uint8_t*));
    RAISE_IF_WITH(packet_args == NULL, free(schemas), "Failed to allocate packet_args");

    for(size_t i = 0; i < nargs; i++){
        packet_args[i] = parse_cli_data_argument(dest, args[i], schemas[i], &CHILD_ERRMSG);
        if(CHILD_ERRMSG != NULL){
            free(schemas);
            free(packet_args);
            RAISE("Failed to parse argument %zu\n%s", i, CHILD_ERRMSG);
        }
    }

    uint8_t* call_packet = make_morloc_local_call_packet(mid, packet_args, nargs, &CHILD_ERRMSG);
    if(CHILD_ERRMSG != NULL){
        free(schemas);
        free(packet_args);
        RAISE("Failed to make call packet:\n%s", CHILD_ERRMSG);
    }

    free(schemas);
    free(packet_args);
    return call_packet;
}
