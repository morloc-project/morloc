#include "morloc.h"
#include "cache.h"

#define XXH_INLINE_ALL
#include "xxhash.h"

// Two prime numbers used in xxhash
static const uint64_t PRIME64_1 = 0x9E3779B185EBCA87;
static const uint64_t PRIME64_2 = 0xC2B2AE3D27D4EB4F;

// xxhash wrapper function
uint64_t morloc_xxh64(const void* input, size_t length, uint64_t seed) {
    return XXH64(input, length, seed);
}

uint64_t mix(uint64_t a, uint64_t b) {
    a ^= b * PRIME64_1;
    a = (a << 31) | (a >> (64 - 31)); // Rotate
    a *= PRIME64_2;
    return a;
}

uint64_t hash_voidstar(absptr_t data, const Schema* schema, uint64_t seed, ERRMSG){
    VAL_RETURN_SETUP(uint64_t, 0)

    uint64_t hash = seed;
    switch(schema->type){
        case MORLOC_STRING:
        case MORLOC_ARRAY:
            {
                Array* array = (Array*)data;
                size_t element_width = schema->parameters[0]->width;

                uint8_t* element_data = TRY((uint8_t*)rel2abs, array->data);

                if (schema_is_fixed_width(schema)){
                    size_t array_size = element_width * array->size;
                    hash = XXH64(element_data, array_size, seed);
                } else {
                    for(size_t i = 0; i < array->size; i++){
                        hash = TRY(hash_voidstar, element_data + i * element_width, schema->parameters[0], hash);
                    }
                }
            }
            break;
        case MORLOC_TUPLE:
        case MORLOC_MAP:
            {
                if (schema_is_fixed_width(schema)){
                    hash = XXH64(data, schema->width, seed);
                } else {
                    uint8_t* element_data = (uint8_t*)data;
                    for(size_t i = 0; i < schema->size; i++){
                        hash = TRY(hash_voidstar, element_data + schema->offsets[i], schema->parameters[i], hash);
                    }
                }
            }
            break;
        default:
            hash = XXH64((uint8_t*)data, schema->width, seed);
            break;
    }
    return hash;
}

bool hash_morloc_packet(const uint8_t* packet, const Schema* schema, uint64_t seed, uint64_t* hash, ERRMSG){
    BOOL_RETURN_SETUP

    morloc_packet_header_t* header = (morloc_packet_header_t*)packet;
    *hash = 0; // 0 softly represents a failed hash, though it could appear by chance
    uint8_t command_type = header->command.cmd_type.type;

    if (command_type == PACKET_TYPE_CALL){
        uint32_t midx = header->command.call.midx;
        *hash = mix(seed, (uint64_t)midx);
        const uint8_t* arg_data = packet + sizeof(morloc_packet_header_t) + (size_t)header->offset;
        size_t arg_start = 0;
        while(arg_start < header->length){
            uint64_t arg_hash = TRY(hash_voidstar, (void*)(arg_data + arg_start), schema, 0);
            *hash = mix(*hash, arg_hash);
        }
    } else if (command_type == PACKET_TYPE_DATA){
        uint8_t* voidstar = TRY(get_morloc_data_packet_value, packet + sizeof(morloc_packet_header_t) + header->offset, schema);
        *hash = TRY(hash_voidstar, (void*)voidstar, schema, seed);
    } else {
        RAISE("Cannot hash packet with command 0x%02hhxh", command_type)
    }

    return true; // success
}

char* make_cache_filename_ext(uint64_t key, const char* cache_path, const char* ext, ERRMSG) {
    PTR_RETURN_SETUP(char)

    char buffer[MAX_FILENAME_SIZE] = { '\0' };

    // Format the filename with the key in hexadecimal
    //  * It is always padded to 16 characters
    //  * The PRIx64 macro from inttypes.h ensures portability since the
    //    uint64_t type may be aliased to different 64 bit types on
    //    different systems (e.g., unsigned long or unsigned long long)
    int written = snprintf(buffer, sizeof(buffer), "%s/%016" PRIx64 "%s", cache_path, key, ext);

    RAISE_IF((size_t)written == 0, "Failed to create filename")

    return strndup(buffer, sizeof(buffer)-1);
}

char* make_cache_filename(uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(char)
    char packet_ext[] = ".packet";
    char* filename = TRY(make_cache_filename_ext, key, cache_path, packet_ext);
    return filename;
}

static char* make_cache_data_filename(uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(char)
    char dat_ext[] = ".dat";
    char* filename = TRY(make_cache_filename_ext, key, cache_path, dat_ext);
    return filename;
}

char* put_cache_packet(const uint8_t* voidstar, const Schema* schema, uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(char)

    // Generate the cache filename
    char* packet_filename = TRY(make_cache_filename, key, cache_path)

    // Generate the data filename
    char* data_filename = TRY(make_cache_data_filename, key, cache_path)

    uint8_t* data_packet = make_mpk_data_packet(data_filename, schema);

    size_t data_packet_size = TRY(morloc_packet_size, data_packet);

    // convert voidstar data to MessagePack
    char* mpk_data = NULL;
    size_t mpk_size = 0;
    TRY(pack_with_schema, voidstar, schema, &mpk_data, &mpk_size);

    // write packet
    TRY_WITH(free(data_packet), write_atomic, packet_filename, data_packet, data_packet_size);
    free(data_packet);

    TRY_WITH(free(mpk_data), write_atomic, data_filename, (uint8_t*)mpk_data, mpk_size);
    free(mpk_data);

    return strdup(packet_filename);
}

uint8_t* get_cache_packet(uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(uint8_t)

    char* filename = TRY(make_cache_filename, key, cache_path)

    // Read the binary file into memory
    size_t file_size;
    uint8_t* data = TRY(read_binary_file, filename, &file_size);

    return data;
}

bool del_cache_packet(uint64_t key, const char* cache_path, ERRMSG) {
    BOOL_RETURN_SETUP

    // Generate the cache filename
    char* filename = TRY(make_cache_filename, key, cache_path)

    // Attempt to delete the file
    RAISE_IF(
        unlink(filename) != 0,
        "Failed to delete cache file '%s'",
        filename
    )

    return true; // deletion success
}

char* check_cache_packet(uint64_t key, const char* cache_path, ERRMSG) {
    PTR_RETURN_SETUP(char)

    // Generate the cache filename
    char* filename = TRY(make_cache_filename, key, cache_path)

    // check if the file exists
    struct stat file_stat;
    if (stat(filename, &file_stat) == 0) {
        return strdup(filename); // File exists
    } else {
        // NOTE, this is NOT an error, it just means that no cache file exists
        return NULL;
    }
}
