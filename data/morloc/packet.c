#include "morloc.h"

morloc_packet_header_t* read_morloc_packet_header(const uint8_t* msg, ERRMSG){
    PTR_RETURN_SETUP(morloc_packet_header_t)

    RAISE_IF(msg == NULL, "Cannot make packet from NULL pointer")

    morloc_packet_header_t* header = (morloc_packet_header_t*) msg;
    RAISE_IF(header->magic != MORLOC_PACKET_MAGIC, "Malformed morloc packet")

    return header;
}

bool packet_is_ping(const uint8_t* packet, ERRMSG){
    BOOL_RETURN_SETUP
    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);
    return header->command.cmd_type.type == PACKET_TYPE_PING;
}

bool packet_is_local_call(const uint8_t* packet, ERRMSG){
    BOOL_RETURN_SETUP
    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);
    return header->command.cmd_type.type == PACKET_TYPE_CALL &&
           header->command.call.entrypoint == PACKET_ENTRYPOINT_LOCAL;
}

bool packet_is_remote_call(const uint8_t* packet, ERRMSG){
    BOOL_RETURN_SETUP
    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);
    return header->command.cmd_type.type == PACKET_TYPE_CALL &&
           header->command.call.entrypoint == PACKET_ENTRYPOINT_REMOTE_SFS;
}

size_t morloc_packet_size_from_header(const morloc_packet_header_t* header){
    if(header == NULL){
      return 0;
    }
    return sizeof(morloc_packet_header_t) + header->offset + header->length;
}

size_t morloc_packet_size(const uint8_t* packet, ERRMSG){
    VAL_RETURN_SETUP(size_t, 0);

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);

    return sizeof(morloc_packet_header_t) + header->offset + header->length;
}

uint8_t* return_ping(const uint8_t* packet, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)
    TRY(packet_is_ping, packet);

    size_t size = TRY(morloc_packet_size, packet);

    uint8_t* pong = (uint8_t*)calloc(size, sizeof(uint8_t));
    memcpy(pong, packet, size);

    return pong;
}

static void set_morloc_packet_header(
    uint8_t* data,
    packet_command_t cmd,
    uint32_t offset,
    uint64_t length
){
    morloc_packet_header_t* header = (morloc_packet_header_t*) data;
    header->magic = MORLOC_PACKET_MAGIC;
    header->plain = THIS_PLAIN;
    header->version = THIS_VERSION;
    header->flavor = DEFAULT_FLAVOR;
    header->mode = DEFAULT_MODE;
    header->command = cmd;
    header->offset = offset;
    header->length = length;
}

static void set_morloc_metadata_header(uint8_t* metadata, uint8_t metadata_type, uint32_t metadata_length){
    morloc_metadata_header_t* metadata_header = (morloc_metadata_header_t*)metadata;
    metadata_header->magic[0] = 'm';
    metadata_header->magic[1] = 'm';
    metadata_header->magic[2] = 'h';
    metadata_header->type = metadata_type;
    metadata_header->size = (uint32_t)metadata_length;
}

uint8_t* make_ping_packet(){
    uint8_t* packet = (uint8_t*)calloc(1, sizeof(morloc_packet_header_t));

    packet_command_t cmd = {
        .ping = {
            PACKET_TYPE_PING,
            .padding = { 0, 0, 0, 0, 0, 0, 0 }
        }
    };

    set_morloc_packet_header(packet, cmd, 0, 0);

    return packet;
}

static uint8_t* make_morloc_data_packet(
    const uint8_t* data,
    size_t data_length,
    const uint8_t* metadata,
    size_t metadata_length,
    uint8_t src,
    uint8_t fmt,
    uint8_t cmpr,
    uint8_t encr,
    uint8_t status
){

  size_t packet_length = sizeof(morloc_packet_header_t) + metadata_length + data_length;
  uint8_t* packet = (uint8_t*)calloc(packet_length, sizeof(uint8_t));

  packet_command_t cmd = {
    .data = {
        .type = PACKET_TYPE_DATA,
        .source = src,
        .format = fmt,
        .compression = cmpr,
        .encryption = encr,
        .status = status,
        .padding = { 0, 0 }
    }
  };

  // generate the header
  set_morloc_packet_header(packet, cmd, metadata_length, data_length);

  if (metadata != NULL && metadata_length > 0) {
      memcpy(packet + sizeof(morloc_packet_header_t), metadata, metadata_length);
  }

  if (data != NULL && data_length > 0){
      memcpy(packet + sizeof(morloc_packet_header_t) + metadata_length, data, data_length);
  }

  return packet;
}

static uint8_t* make_morloc_data_packet_with_schema(
    const uint8_t* data,
    size_t data_length,
    const Schema* schema,
    uint8_t src,
    uint8_t fmt,
    uint8_t cmpr,
    uint8_t encr,
    uint8_t status
){
  if(schema == NULL){
    return make_morloc_data_packet(data, data_length, NULL, 0, src, fmt, cmpr, encr, status);
  }

  char* schema_str = schema_to_string(schema);
  size_t metadata_length = strlen(schema_str) + 1; // +1 for null byte
  size_t metadata_length_total = (1 + (sizeof(morloc_metadata_header_t) + metadata_length) / 32) * 32;
  uint8_t* metadata = (uint8_t*)calloc(metadata_length_total, sizeof(char));

  set_morloc_metadata_header(metadata, MORLOC_METADATA_TYPE_SCHEMA_STRING, metadata_length);
  memcpy(metadata + sizeof(morloc_metadata_header_t), schema_str, metadata_length);
  FREE(schema_str);

  return make_morloc_data_packet(data, data_length, metadata, metadata_length_total, src, fmt, cmpr, encr, status);
}

uint8_t* make_standard_data_packet(relptr_t ptr, const Schema* schema){
  uint8_t* packet = make_morloc_data_packet_with_schema(
        NULL, sizeof(relptr_t),
        schema,
        PACKET_SOURCE_RPTR,
        PACKET_FORMAT_VOIDSTAR,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_PASS
    );

  morloc_packet_header_t* header = (morloc_packet_header_t*)(packet);

  *((ssize_t*)(packet + sizeof(morloc_packet_header_t) + (size_t)header->offset)) = ptr;

  return packet;
}

uint8_t* make_mpk_data_packet(const char* mpk_filename, const Schema* schema){
    uint8_t* packet = make_morloc_data_packet_with_schema(
        (const uint8_t*)mpk_filename,
        strlen(mpk_filename),
        schema,
        PACKET_SOURCE_FILE,
        PACKET_FORMAT_MSGPACK,
        PACKET_COMPRESSION_NONE,
        PACKET_ENCRYPTION_NONE,
        PACKET_STATUS_PASS
    );
    return packet;
}

morloc_metadata_header_t* as_morloc_metadata_header(const uint8_t* ptr){

    morloc_metadata_header_t* metadata_header = (morloc_metadata_header_t*)(ptr);

    if(strncmp(metadata_header->magic, MORLOC_METADATA_HEADER_MAGIC, 3) != 0){
        return NULL;
    }

    return metadata_header;
}

char* read_schema_from_packet_meta(const uint8_t* packet, ERRMSG){
    PTR_RETURN_SETUP(char)

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);

    if(header->offset >= sizeof(morloc_metadata_header_t)){
        size_t offset = 0;
        do {
            morloc_metadata_header_t* metadata_header = as_morloc_metadata_header(packet + sizeof(morloc_packet_header_t));
            if(metadata_header == NULL){
                return NULL;
            }
            offset += sizeof(morloc_metadata_header_t);
            if(metadata_header != NULL && metadata_header->type == MORLOC_METADATA_TYPE_SCHEMA_STRING) {
                char* schema_str = (char*)packet + sizeof(morloc_packet_header_t) + offset;
                return schema_str;
            } else {
                offset += metadata_header->size;
            }
        } while(offset <= (header->offset - sizeof(morloc_metadata_header_t)));
    }

    return NULL;
}

uint8_t* make_fail_packet(const char* failure_message){
  return make_morloc_data_packet(
    (const uint8_t*)failure_message,
    strlen(failure_message),
    NULL, 0,
    PACKET_SOURCE_MESG,
    PACKET_FORMAT_TEXT,
    PACKET_COMPRESSION_NONE,
    PACKET_ENCRYPTION_NONE,
    PACKET_STATUS_FAIL
  );
}

char* get_morloc_data_packet_error_message(const uint8_t* data, ERRMSG){
    PTR_RETURN_SETUP(char)

    char* packet_err = NULL;

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, data);

    if (header->command.data.status == PACKET_STATUS_FAIL) {
        packet_err = (char*)calloc(header->length + 1, sizeof(char));
        RAISE_IF(packet_err == NULL, "Failed to allocate error message");

        char* packet_data = (char*)data + sizeof(morloc_packet_header_t) + (size_t)header->offset;
        void* copy_ptr = memcpy(packet_err, packet_data, header->length);
        RAISE_IF(copy_ptr == NULL, "Failed to copy error message to packet")

        return packet_err;
    }

    return packet_err;
}

uint8_t* get_morloc_data_packet_value(const uint8_t* data, const Schema* schema, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    uint8_t source;
    uint8_t format;

    void* voidstar = NULL;

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, data);

    RAISE_IF(header->command.cmd_type.type != PACKET_TYPE_DATA, "Expected a data packet");

    source = header->command.data.source;
    format = header->command.data.format;

    char* packet_error = TRY(get_morloc_data_packet_error_message, data);
    RAISE_IF_WITH(packet_error != NULL, free(packet_error), "\n%s", packet_error)

    switch (source) {
        case PACKET_SOURCE_MESG:
            if (format == PACKET_FORMAT_MSGPACK) {
                TRY(
                    unpack_with_schema,
                    (const char*)data + sizeof(morloc_packet_header_t) + header->offset,
                    header->length,
                    schema,
                    &voidstar
                );
            } else {
                RAISE("Invalid format from mesg: 0x%02hhx", format);
            }
            break;
        case PACKET_SOURCE_FILE:
            switch (format) {
                case PACKET_FORMAT_MSGPACK: {
                    char* filename = strndup((char*)data + sizeof(morloc_packet_header_t) + header->offset, MAX_FILENAME_SIZE);
                    size_t file_size;
                    uint8_t* msg = TRY(read_binary_file, filename, &file_size);
                    FREE(filename);
                    // Unpack the binary buffer using the schema
                    TRY(unpack_with_schema, (const char*)msg, file_size, schema, &voidstar);
                    FREE(msg);
                    break;
                }
                default:
                    RAISE("Invalid format from file: 0x%02hhx", format);
                    return NULL;
            }
            break;

        case PACKET_SOURCE_RPTR:
            if (format == PACKET_FORMAT_VOIDSTAR) {
                // This packet should contain a relative pointer as its payload
                size_t relptr = *(size_t*)(data + header->offset + sizeof(morloc_packet_header_t));
                voidstar = TRY(rel2abs, relptr);
            } else {
                RAISE("For RPTR source, expected voidstar format, found: 0x%02hhx", format);
                return NULL;
            }
            break;

        default:
            RAISE("Invalid source");
    }

    return (uint8_t*)voidstar;
}

static uint8_t* make_morloc_call_packet_gen(uint32_t midx, uint8_t entrypoint, const uint8_t** arg_packets, size_t nargs, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)

    size_t data_length = 0;
    uint32_t offset = 0;

    for(size_t i = 0; i < nargs; i++){
        morloc_packet_header_t* arg = TRY(read_morloc_packet_header, arg_packets[i]);

        data_length += sizeof(morloc_packet_header_t) + (size_t)arg->offset + (size_t)arg->length;
    }

    size_t packet_length = data_length + offset + sizeof(morloc_packet_header_t);

    uint8_t* data = (uint8_t*)calloc(packet_length, sizeof(uint8_t));

    packet_command_t cmd = {
      .call = {
        .type = PACKET_TYPE_CALL,
        .entrypoint = entrypoint,
        .padding = {0, 0},
        .midx = midx,
      }
    };

    set_morloc_packet_header(data, cmd, offset, data_length);

    size_t arg_start = sizeof(morloc_packet_header_t) + offset;
    for(size_t i = 0; i < nargs; i++){
      morloc_packet_header_t* arg = TRY(read_morloc_packet_header, arg_packets[i]);
      size_t arg_length = morloc_packet_size_from_header(arg);
      memcpy(data + arg_start, arg, arg_length);
      arg_start += arg_length;
    }

    return data;
}

uint8_t* make_morloc_local_call_packet(uint32_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)
    uint8_t* packet = TRY(make_morloc_call_packet_gen, midx, PACKET_ENTRYPOINT_LOCAL, arg_packets, nargs);
    return packet;
}

uint8_t* make_morloc_remote_call_packet(uint32_t midx, const uint8_t** arg_packets, size_t nargs, ERRMSG){
    PTR_RETURN_SETUP(uint8_t)
    uint8_t* packet = TRY(make_morloc_call_packet_gen, midx, PACKET_ENTRYPOINT_REMOTE_SFS, arg_packets, nargs);
    return packet;
}

morloc_call_t* read_morloc_call_packet(const uint8_t* packet, ERRMSG){
    PTR_RETURN_SETUP(morloc_call_t)

    morloc_call_t* call = (morloc_call_t*)calloc(1, sizeof(morloc_call_t));
    RAISE_IF(call == NULL, "calloc failed: %s", strerror(errno))

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);
    RAISE_IF(header->command.cmd_type.type != PACKET_TYPE_CALL, "Expected packet to be a call")

    call->midx = header->command.call.midx;
    call->nargs = 0;
    call->args = NULL;

    size_t start_pos = sizeof(morloc_packet_header_t) + header->offset;
    size_t end_pos = start_pos + header->length;
    size_t pos = start_pos;
    while (pos < end_pos) {
        pos += TRY(morloc_packet_size, packet + pos);
        call->nargs++;
    }

    call->args = (uint8_t**)calloc(call->nargs, sizeof(uint8_t*));
    pos = sizeof(morloc_packet_header_t) + header->offset;
    for(size_t i = 0; i < call->nargs; i++){
        morloc_packet_header_t* arg_header = read_morloc_packet_header(packet + pos, &CHILD_ERRMSG);
        RAISE_IF(CHILD_ERRMSG != NULL, "Failed to read call argument #%zu:\n%s", i, CHILD_ERRMSG)
        RAISE_IF(
            arg_header->command.cmd_type.type != PACKET_TYPE_DATA,
            "Argument #%zu is not a DATA packet (type=%d)",
            i,
            arg_header->command.cmd_type.type
        );

        size_t arg_packet_size = morloc_packet_size_from_header(arg_header);

        // Copy the argument
        // We alternatively we could avoid this copy and pass the pointer to the
        // original memory. This would improve performance, but would be less
        // safe. I'll leave this optimization for later.
        call->args[i] = (uint8_t*)calloc(arg_packet_size, sizeof(uint8_t));
        memcpy(call->args[i], packet + pos, arg_packet_size);

        pos += arg_packet_size;
    }

    return call;
}

static relptr_t print_voidstar_binary_binder_r(
    const void* data,
    const Schema* schema,
    relptr_t data_index,
    ERRMSG
){
    VAL_RETURN_SETUP(relptr_t, -1)

    switch (schema->type) {
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            {
                // copy the array struct
                Array array = *(Array*)data;
                // set the element pointer to the current data location
                array.data = data_index;
                // print just the array struct, the data will be printed later
                TRY(print_binary, (char*)(&array), sizeof(Array));
                // reserve space for the array and all its recursive contents
                size_t data_size = TRY(calculate_voidstar_size, data, schema);
                data_index += data_size - sizeof(Array);
            }
            break;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            {
                // recursively print every tuple element
                for(size_t i = 0; i < schema->size; i++){
                    void* child = (void*)((char*)data + schema->offsets[i]);
                    data_index = TRY(print_voidstar_binary_binder_r, child, schema->parameters[i], data_index);
                }
            }
            break;
        default:
            {
                // print primitives
                TRY(print_binary, (char*)data, schema->width);
            }
    }
    return data_index;
}

static relptr_t print_voidstar_binary_array_r(const void*, const Schema*, size_t, relptr_t, ERRMSG);

static relptr_t print_voidstar_binary_data_r(
    const void* data,
    const Schema* schema,
    relptr_t data_index, // pointer to start of the data for all arrays
    ERRMSG
){
    VAL_RETURN_SETUP(relptr_t, -1)

    switch (schema->type) {
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            {
                Array* array = (Array*)data;
                void* absptr = TRY(rel2abs, array->data);
                data_index = TRY(print_voidstar_binary_array_r, absptr, schema->parameters[0], array->size, data_index);
            }
            break;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            {
                // recursively print every tuple element
                for(size_t i = 0; i < schema->size; i++){
                    void* child = (void*)((char*)data + schema->offsets[i]);
                    data_index = TRY(print_voidstar_binary_data_r, child, schema->parameters[i], data_index);
                }
            }
            break;
        default:
            break;
    }
    return data_index;
}

static relptr_t print_voidstar_binary_array_r(
    const void* data, // pointer to the start of the array
    const Schema* schema, // schema for the element type
    size_t length, // number of elements in the array
    relptr_t data_index, // relative pointer to the start of the data block
    ERRMSG
){
    VAL_RETURN_SETUP(relptr_t, -1)

    // all the fixed-size elements will be printed first
    size_t fixed_array_size = schema->width * length;
    data_index += fixed_array_size;
    if (schema_is_fixed_width(schema)){
        TRY(print_binary, (char*)data, fixed_array_size);
    } else {
        // print fixed width array elements
        relptr_t binder_ptr = data_index; // for array pointers, don't reuse
        for(size_t i = 0; i < length; i++){
            void* child = (void*)((char*)data + i * schema->width);
            binder_ptr = TRY(print_voidstar_binary_binder_r, child, schema, binder_ptr);
        }
        // print array data
        for(size_t i = 0; i < length; i++){
            void* child = (void*)((char*)data + i * schema->width);
            data_index = TRY(print_voidstar_binary_data_r, child, schema, data_index);
        }
    }
    return data_index;
}

static relptr_t print_voidstar_binary(
    const void* data,
    const Schema* schema,
    ERRMSG
){
    VAL_RETURN_SETUP(relptr_t, -1)
    relptr_t data_index = (relptr_t)schema->width;
    TRY(print_voidstar_binary_binder_r, data, schema, data_index);
    data_index = TRY(print_voidstar_binary_data_r, data, schema, data_index);
    return data_index;
}

int print_morloc_data_packet(const uint8_t* packet, const Schema* schema, ERRMSG){
    INT_RETURN_SETUP

    uint8_t source;
    uint8_t format;

    morloc_packet_header_t* header = TRY(read_morloc_packet_header, packet);

    size_t packet_size = morloc_packet_size_from_header(header);

    RAISE_IF(header->command.cmd_type.type != PACKET_TYPE_DATA, "Expected a data packet");

    source = header->command.data.source;
    format = header->command.data.format;

    char* packet_error = TRY(get_morloc_data_packet_error_message, packet);
    RAISE_IF_WITH(packet_error != NULL, free(packet_error), "\n%s", packet_error)

    switch (source) {
        case PACKET_SOURCE_MESG:
        case PACKET_SOURCE_FILE:
            // verbatim print messages and files
            TRY(print_binary, (char*)packet, packet_size);
            break;
        case PACKET_SOURCE_RPTR:
            switch (format) {
                case PACKET_FORMAT_JSON:
                case PACKET_FORMAT_MSGPACK:
                case PACKET_FORMAT_TEXT:
                case PACKET_FORMAT_DATA:
                    TRY(print_binary, (char*)packet, packet_size);
                    break;
                case PACKET_FORMAT_VOIDSTAR:
                    {
                        size_t relptr = *(size_t*)(packet + header->offset + sizeof(morloc_packet_header_t));
                        void* voidstar_ptr = TRY(rel2abs, relptr);

                        morloc_packet_header_t new_header = *header;
                        new_header.command.data.format = PACKET_FORMAT_VOIDSTAR;
                        new_header.length = TRY(calculate_voidstar_size, voidstar_ptr, schema);

                        // print header
                        TRY(print_binary, (char*)&new_header, sizeof(morloc_packet_header_t));

                        // print metadata
                        if(new_header.offset > 0) {
                            TRY(print_binary, (char*)packet + sizeof(morloc_packet_header_t), new_header.offset);
                        }

                        // print flattened voidstar data
                        TRY(print_voidstar_binary, voidstar_ptr, schema);
                    }
                    break;
            }
            break;

        default:
            RAISE("Invalid source");
    }

    return EXIT_PASS;
}
