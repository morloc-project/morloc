#include "morloc.h"
#include "mpack.h"

#define MPACK_TOKBUF_INITIAL_VALUE { { 0 }, { (mpack_token_type_t)0, 0, { .value = { 0 } } }, 0, 0, 0 }

// Static forward declarations
static size_t msg_size(const char* mgk, size_t mgk_size, const Schema* schema);
static size_t msg_size_r(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static size_t msg_size_bytes(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static size_t msg_size_array(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static size_t msg_size_tuple(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static EXIT_CODE parse_bool(        void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static EXIT_CODE parse_nil(         void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static EXIT_CODE parse_bytes(       void* mlc, void** cursor, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG);
static EXIT_CODE parse_int(    morloc_serial_type, void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG);
static EXIT_CODE parse_float(  morloc_serial_type, void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token);
static EXIT_CODE parse_array( void* mlc, const Schema* schema, void** cursor, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG);
static EXIT_CODE parse_tuple( void* mlc, const Schema* schema, void** cursor, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG);
static EXIT_CODE parse_obj(   void* mlc, const Schema* schema, void** cursor, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG);

static void upsize(
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
    char* new_data = (char*)realloc(*data, buffer_size);
    if (new_data == NULL) {
        fprintf(stderr, "Out of memory in upsize (requested %zu bytes)\n", buffer_size);
        abort();
    }

    *data = new_data;
    // point old pointer to the same offset in the new data
    *data_ptr = *data + used_size;
}

static void write_to_packet(
  const void* src,                // source data
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

static int dynamic_mpack_write(
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

static EXIT_CODE pack_data(
  const void* mlc,           // input data structure
  const Schema* schema,      // input data schema
  char** packet,             // a pointer to the messagepack data
  char** packet_ptr,         // the current position in the buffer
  size_t* packet_remaining,  // bytes from current position to the packet end
  mpack_tokbuf_t* tokbuf,
  ERRMSG
) {
    INT_RETURN_SETUP

    mpack_token_t token;
    Array* array;

    switch (schema->type) {
        case MORLOC_NIL:
            token = mpack_pack_nil();
            break;
        case MORLOC_BOOL:
            token = mpack_pack_boolean(*(uint8_t*)mlc != 0);
            break;
        case MORLOC_UINT8:
            token = mpack_pack_uint((uint64_t)*(uint8_t*)mlc);
            break;
        case MORLOC_UINT16:
            token = mpack_pack_uint((uint64_t)*(uint16_t*)mlc);
            break;
        case MORLOC_UINT32:
            token = mpack_pack_uint((uint64_t)*(uint32_t*)mlc);
            break;
        case MORLOC_UINT64:
            token = mpack_pack_uint(*(uint64_t*)mlc);
            break;
        case MORLOC_SINT8:
            token = mpack_pack_sint((int64_t)*(int8_t*)mlc);
            break;
        case MORLOC_SINT16:
            token = mpack_pack_sint((int64_t)*(int16_t*)mlc);
            break;
        case MORLOC_SINT32:
            token = mpack_pack_sint((int64_t)*(int32_t*)mlc);
            break;
        case MORLOC_SINT64:
            token = mpack_pack_sint(*(int64_t*)mlc);
            break;
        case MORLOC_FLOAT32:
            token = mpack_pack_float((double)*(float*)mlc);
            break;
        case MORLOC_FLOAT64:
            token = mpack_pack_float(*(double*)mlc);
            break;
        case MORLOC_STRING:
            token = mpack_pack_str(((Array*)mlc)->size);
            break;
        case MORLOC_ARRAY:
            array = (Array*)mlc;
            token = mpack_pack_array(array->size);
            break;
        case MORLOC_MAP:
        case MORLOC_TUPLE:
            token = mpack_pack_array(schema->size);
            break;
        default:
            RAISE("Unexpected morloc type")
    }

    dynamic_mpack_write(tokbuf, packet, packet_ptr, packet_remaining, &token, 0);

    size_t array_length;
    size_t array_width;
    void* voidstar_data = NULL;
    Schema* array_schema;

    switch(schema->type){
      case MORLOC_STRING:
        array = (Array*)mlc;

        voidstar_data = rel2abs(array->data, &CHILD_ERRMSG);
        RAISE_IF(voidstar_data == NULL, "\n%s", CHILD_ERRMSG)

        write_to_packet(voidstar_data, packet, packet_ptr, packet_remaining, array->size);
        break;
      case MORLOC_ARRAY:
        {
          array_length = ((Array*)mlc)->size;

          char* data = (char*)rel2abs(((Array*)mlc)->data, &CHILD_ERRMSG);
          RAISE_IF(data == NULL, "\n%s", CHILD_ERRMSG)

          array_schema = schema->parameters[0];
          array_width = array_schema->width;

          for (size_t i = 0; i < array_length; i++) {
              int exit_code = pack_data(
                  data + i * array_width,
                  array_schema,
                  packet,
                  packet_ptr,
                  packet_remaining,
                  tokbuf,
                  &CHILD_ERRMSG
              );
              RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
          }

        }
        break;
      case MORLOC_MAP:
      case MORLOC_TUPLE:
        {
            for (size_t i = 0; i < schema->size; i++) {
                int exit_code = pack_data(
                    (char*)mlc + schema->offsets[i],
                    schema->parameters[i],
                    packet,
                    packet_ptr,
                    packet_remaining,
                    tokbuf,
                    &CHILD_ERRMSG
                );
                RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
            }
        }
        break;
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
        // no further processing needed
        break;
    }

    return EXIT_PASS;
}

int pack_with_schema(const void* mlc, const Schema* schema, char** packet, size_t* packet_size, ERRMSG) {
    INT_RETURN_SETUP

    *packet_size = 0;

    *packet = (char*)calloc(BUFFER_SIZE, sizeof(char));
    RAISE_IF(*packet == NULL, "\n%s", "Empty packet")

    size_t packet_remaining = BUFFER_SIZE;
    char* packet_ptr = *packet;

    mpack_tokbuf_t tokbuf = MPACK_TOKBUF_INITIAL_VALUE;

    int pack_result = pack_data(mlc, schema, packet, &packet_ptr, &packet_remaining, &tokbuf, &CHILD_ERRMSG);
    if (pack_result == EXIT_FAIL) {
        free(*packet);
        *packet = NULL;
        RAISE("\n%s", CHILD_ERRMSG)
    }

    // mutate packet_size (will be used outside)
    *packet_size = packet_ptr - *packet;

    // Trim the output buffer to the exact size needed
    if (packet_remaining > 0) {
        char* tmp = (char*)realloc(*packet, *packet_size);
        if (tmp != NULL) {
            *packet = tmp;
        }
    }

    return pack_result;
}

int pack(const void* mlc, const char* schema_str, char** mpk, size_t* mpk_size, ERRMSG) {
    INT_RETURN_SETUP

    Schema* schema = parse_schema(schema_str, &CHILD_ERRMSG);
    RAISE_IF(schema == NULL, "\n%s", CHILD_ERRMSG)

    int exit_code = pack_with_schema(mlc, schema, mpk, mpk_size, &CHILD_ERRMSG);
    free_schema(schema);
    RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG);

    return exit_code;
}

static size_t msg_size_bytes(mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t array_size = token->length;

    size_t str_idx = 0;
    while((array_size - str_idx) > 0){
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);
        str_idx += token->length;
    }
    return array_size + sizeof(Array);
}

static size_t msg_size_array(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    size_t array_length = token->length;
    size_t size = sizeof(Array);
    for(size_t i = 0; i < array_length; i++){
        size += msg_size_r(schema, tokbuf, buf_ptr, buf_remaining, token);
    }
    return size;
}

static size_t msg_size_tuple(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    // parse the msgpack tuple
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    assert(token->length == schema->size);
    size_t size = 0;
    for(size_t i = 0; i < schema->size; i++){
        size += msg_size_r(schema->parameters[i], tokbuf, buf_ptr, buf_remaining, token);
    }
    return size;
}

static size_t msg_size_r(const Schema* schema, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    switch(schema->type){
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
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);
        return schema->width;
      case MORLOC_STRING:
        return msg_size_bytes(tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_ARRAY:
        return msg_size_array(schema->parameters[0], tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_MAP:
      case MORLOC_TUPLE:
        return msg_size_tuple(schema, tokbuf, buf_ptr, buf_remaining, token);
      default:
        return 0;
    }
}

static size_t msg_size(const char* mgk, size_t mgk_size, const Schema* schema) {
    size_t buf_remaining = mgk_size;

    mpack_tokbuf_t tokbuf = MPACK_TOKBUF_INITIAL_VALUE;
    mpack_token_t token;
    return msg_size_r(schema, &tokbuf, &mgk, &buf_remaining, &token);
}

static EXIT_CODE parse_nil(void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    *((int8_t*)mlc) = (int8_t)0;
    return EXIT_PASS;
}

static EXIT_CODE parse_bool(void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    // boolean here needs to be uint8 since the C `bool` type is not guaranteed
    // to be 1 byte, it is likely typedefed to `int`, which is 32 bit.
    *((uint8_t*)mlc) = (uint8_t) mpack_unpack_boolean(*token) ? 1 : 0;
    return EXIT_PASS;
}

static EXIT_CODE parse_int(morloc_serial_type schema_type, void* mlc, mpack_tokbuf_t* tokbuf, const char** buf_ptr, size_t* buf_remaining, mpack_token_t* token, ERRMSG){
    INT_RETURN_SETUP

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    switch(token->type){
      case MPACK_TOKEN_UINT:
        switch(schema_type){
          case MORLOC_UINT8:
            *((uint8_t*)mlc) = (uint8_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_UINT16:
            *((uint16_t*)mlc) = (uint16_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_UINT32:
            *((uint32_t*)mlc) = (uint32_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_UINT64:
            *((uint64_t*)mlc) = (uint64_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_SINT8:
            *((int8_t*)mlc) = (int8_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_SINT16:
            *((int16_t*)mlc) = (int16_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_SINT32:
            *((int32_t*)mlc) = (int32_t)mpack_unpack_uint(*token);
            break;
          case MORLOC_SINT64:
            *((int64_t*)mlc) = (int64_t)mpack_unpack_uint(*token);
            break;
          default:
            break;
        }
        break;
      case MPACK_TOKEN_SINT:
        switch(schema_type){
          case MORLOC_SINT8:
            *((int8_t*)mlc) = (int8_t)mpack_unpack_sint(*token);
            break;
          case MORLOC_SINT16:
            *((int16_t*)mlc) = (int16_t)mpack_unpack_sint(*token);
            break;
          case MORLOC_SINT32:
            *((int32_t*)mlc) = (int32_t)mpack_unpack_sint(*token);
            break;
          case MORLOC_SINT64:
            *((int64_t*)mlc) = (int64_t)mpack_unpack_sint(*token);
            break;
          default:
            break;
        }
        break;
      default:
        RAISE("Bad token %d", token->type);
    }
    return EXIT_PASS;
}

static EXIT_CODE parse_float(
    morloc_serial_type schema_type,
    void* mlc,
    mpack_tokbuf_t* tokbuf,
    const char** buf_ptr,
    size_t* buf_remaining,
    mpack_token_t* token
){
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    if(schema_type == MORLOC_FLOAT32){
      *(float*)mlc = (float)mpack_unpack_float(*token);
    } else {
      *(double*)mlc = (double)mpack_unpack_float(*token);
    }

    return EXIT_PASS;
}

static EXIT_CODE parse_bytes(
    void* mlc,
    void** cursor,
    mpack_tokbuf_t* tokbuf,
    const char** buf_ptr,
    size_t* buf_remaining,
    mpack_token_t* token,
    ERRMSG
){
    INT_RETURN_SETUP

    Array* result = (Array*) mlc;

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    result->size = token->length;

    result->data = abs2rel(*cursor, &CHILD_ERRMSG);
    RAISE_IF(result->data == RELNULL, "\n%s", CHILD_ERRMSG)

    *cursor = (char*)(*cursor) + result->size;

    size_t str_idx = 0;
    while((result->size - str_idx) > 0){
        mpack_read(tokbuf, buf_ptr, buf_remaining, token);

        void* voidstar = rel2abs(result->data + str_idx, &CHILD_ERRMSG);
        RAISE_IF(voidstar == NULL, "\n%s", CHILD_ERRMSG)

        memcpy(
          voidstar,
          token->data.chunk_ptr,
          token->length * sizeof(char)
        );
        str_idx += token->length;
    }
    return EXIT_PASS;
}

static EXIT_CODE parse_array(
    void* mlc,
    const Schema* schema,
    void** cursor,
    mpack_tokbuf_t* tokbuf,
    const char** buf_ptr,
    size_t* buf_remaining,
    mpack_token_t* token,
    ERRMSG
){
    INT_RETURN_SETUP

    Array* result = (Array*) mlc;

    size_t element_size = schema->width;
    mpack_read(tokbuf, buf_ptr, buf_remaining, token);
    result->size = token->length;

    result->data = abs2rel(*cursor, &CHILD_ERRMSG);
    RAISE_IF(result->data == RELNULL, "\n%s", CHILD_ERRMSG)

    *cursor = (char*)(*cursor) + result->size * element_size;

    for(size_t i = 0; i < result->size; i++){
        void* voidstar = rel2abs(result->data + i * element_size, &CHILD_ERRMSG);
        RAISE_IF(voidstar == NULL, "\n%s", CHILD_ERRMSG)

        int exit_code = parse_obj(voidstar, schema, cursor, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
    }
    return EXIT_PASS;
}

static EXIT_CODE parse_tuple(
    void* mlc,
    const Schema* schema,
    void** cursor,
    mpack_tokbuf_t* tokbuf,
    const char** buf_ptr,
    size_t* buf_remaining,
    mpack_token_t* token,
    ERRMSG
){
    INT_RETURN_SETUP

    size_t offset = 0;

    mpack_read(tokbuf, buf_ptr, buf_remaining, token);

    for(size_t i = 0; i < schema->size; i++){
        int exit_code = parse_obj((char*)mlc + offset, schema->parameters[i], cursor, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
        offset += schema->parameters[i]->width;
    }

    return EXIT_PASS;
}

static EXIT_CODE parse_obj(
    void* mlc,
    const Schema* schema,
    void** cursor,
    mpack_tokbuf_t* tokbuf,
    const char** buf_ptr,
    size_t* buf_remaining,
    mpack_token_t* token,
    ERRMSG
){
    INT_RETURN_SETUP

    int child_retcode;
    switch(schema->type){
      case MORLOC_NIL:
        return parse_nil(mlc, tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_BOOL:
        return parse_bool(mlc, tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_SINT8:
      case MORLOC_SINT16:
      case MORLOC_SINT32:
      case MORLOC_SINT64:
      case MORLOC_UINT8:
      case MORLOC_UINT16:
      case MORLOC_UINT32:
      case MORLOC_UINT64:
        child_retcode = parse_int(schema->type, mlc, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(child_retcode == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
        break;
      case MORLOC_FLOAT32:
      case MORLOC_FLOAT64:
        return parse_float(schema->type, mlc, tokbuf, buf_ptr, buf_remaining, token);
      case MORLOC_STRING:
        child_retcode = parse_bytes(mlc, cursor, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(child_retcode == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
        break;
      case MORLOC_ARRAY:
        child_retcode = parse_array(mlc, schema->parameters[0], cursor, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(child_retcode == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
        break;
      case MORLOC_MAP:
      case MORLOC_TUPLE:
        child_retcode = parse_tuple(mlc, schema, cursor, tokbuf, buf_ptr, buf_remaining, token, &CHILD_ERRMSG);
        RAISE_IF(child_retcode == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
        break;
      default:
        RAISE("Failed to parse morloc type %d", schema->type)
    }
    return EXIT_PASS;
}

int unpack_with_schema(const char* mgk, size_t mgk_size, const Schema* schema, void** mlcptr, ERRMSG) {
    INT_RETURN_SETUP

    // Pass once over the MessagePack data, calculating the allocation size
    size_t size = msg_size(mgk, mgk_size, schema);

    void* mlc = TRY((void*)shmalloc, size);

    // Use the existing unpack_with_schema function, but adapt it to the new prototype
    size_t buf_remaining = mgk_size;

    mpack_tokbuf_t tokbuf = MPACK_TOKBUF_INITIAL_VALUE;
    mpack_token_t token;

    void* cursor = (void*)((char*)mlc + schema->width);

    int exitcode = parse_obj(mlc, schema, &cursor, &tokbuf, &mgk, &buf_remaining, &token, &CHILD_ERRMSG);
    RAISE_IF(exitcode == EXIT_FAIL, "\n%s", CHILD_ERRMSG)

    *mlcptr = mlc;

    return exitcode;
}

int unpack(const char* mpk, size_t mpk_size, const char* schema_str, void** mlcptr, ERRMSG) {
    INT_RETURN_SETUP
    const Schema* schema = parse_schema(schema_str, &CHILD_ERRMSG);
    RAISE_IF(schema == NULL, "\n%s", CHILD_ERRMSG)
    int exit_code = unpack_with_schema(mpk, mpk_size, schema, mlcptr, &CHILD_ERRMSG);
    free_schema((Schema*)schema);
    RAISE_IF(exit_code == EXIT_FAIL, "\n%s", CHILD_ERRMSG)
    return exit_code;
}
