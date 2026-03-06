#include "morloc.h"
#include "intrinsics.h"
#include "cache.h"
#include "json.h"

int mlc_save(const absptr_t data, const Schema* schema, const char* path, ERRMSG) {
    INT_RETURN_SETUP

    char* mpk = NULL;
    size_t mpk_size = 0;
    TRY(pack_with_schema, data, schema, &mpk, &mpk_size);

    TRY_WITH(free(mpk), write_atomic, path, (uint8_t*)mpk, mpk_size);
    free(mpk);
    return EXIT_PASS;
}

int mlc_save_json(const absptr_t data, const Schema* schema, const char* path, ERRMSG) {
    INT_RETURN_SETUP

    char* json = TRY(voidstar_to_json_string, data, schema);
    size_t json_size = strlen(json);
    TRY_WITH(free(json), write_atomic, path, (uint8_t*)json, json_size);
    free(json);
    return EXIT_PASS;
}

int mlc_save_voidstar(const absptr_t data, const Schema* schema, const char* path, ERRMSG) {
    INT_RETURN_SETUP

    char* file_dirpath = strdup(path);
    RAISE_IF(file_dirpath == NULL, "Failed to allocate path for mlc_save_voidstar")
    char* file_dirname_ptr = dirname(file_dirpath);

    char tmp_path[MAX_FILENAME_SIZE];
    snprintf(tmp_path, sizeof(tmp_path), "%s/morloc-tmp_XXXXXX", file_dirname_ptr);

    int fd = mkstemp(tmp_path);
    if (fd < 0) {
        free(file_dirpath);
        RAISE("Failed to create temp file for mlc_save_voidstar: %s", strerror(errno))
    }

    // Reserve space for the packet header (fill in after writing payload)
    morloc_packet_header_t header;
    memset(&header, 0, sizeof(header));
    if (write_binary_fd(fd, (char*)&header, sizeof(header), &CHILD_ERRMSG) != 0) {
        close(fd); unlink(tmp_path); free(file_dirpath);
        RAISE("%s", CHILD_ERRMSG)
    }

    // Write the voidstar binary payload
    relptr_t payload_size = write_voidstar_binary(fd, data, schema, &CHILD_ERRMSG);
    if (payload_size < 0) {
        close(fd); unlink(tmp_path); free(file_dirpath);
        RAISE("%s", CHILD_ERRMSG)
    }

    // Seek back and write the real packet header
    if (lseek(fd, 0, SEEK_SET) == (off_t)-1) {
        close(fd); unlink(tmp_path); free(file_dirpath);
        RAISE("Failed to seek in temp file: %s", strerror(errno))
    }

    header.magic = MORLOC_PACKET_MAGIC;
    header.plain = THIS_PLAIN;
    header.version = THIS_VERSION;
    header.flavor = DEFAULT_FLAVOR;
    header.mode = DEFAULT_MODE;
    header.command.data.type = PACKET_TYPE_DATA;
    header.command.data.source = PACKET_SOURCE_MESG;
    header.command.data.format = PACKET_FORMAT_VOIDSTAR;
    header.command.data.compression = PACKET_COMPRESSION_NONE;
    header.command.data.encryption = PACKET_ENCRYPTION_NONE;
    header.command.data.status = PACKET_STATUS_PASS;
    header.offset = 0;
    header.length = (uint64_t)payload_size;

    if (write_binary_fd(fd, (char*)&header, sizeof(header), &CHILD_ERRMSG) != 0) {
        close(fd); unlink(tmp_path); free(file_dirpath);
        RAISE("%s", CHILD_ERRMSG)
    }

    fsync(fd);
    close(fd);

    if (rename(tmp_path, path) != 0) {
        unlink(tmp_path); free(file_dirpath);
        RAISE("Failed to rename temp file to %s: %s", path, strerror(errno))
    }

    free(file_dirpath);
    return EXIT_PASS;
}

void* mlc_load(const char* path, const Schema* schema, ERRMSG) {
    PTR_RETURN_SETUP(void)

    if (!file_exists(path)) {
        return NULL;
    }

    size_t file_size;
    uint8_t* data = TRY(read_binary_file, path, &file_size);
    return TRY(load_morloc_data_file, path, data, file_size, schema);
}

char* mlc_hash(const absptr_t data, const Schema* schema, ERRMSG) {
    PTR_RETURN_SETUP(char)
    uint64_t hash = TRY(hash_voidstar, data, schema, DEFAULT_XXHASH_SEED);
    char* hex = (char*)malloc(17);
    RAISE_IF(hex == NULL, "Failed to allocate hash string")
    snprintf(hex, 17, "%016" PRIx64, hash);
    return hex;
}

char* mlc_show(const absptr_t data, const Schema* schema, ERRMSG) {
    PTR_RETURN_SETUP(char)
    return TRY(voidstar_to_json_string, data, schema);
}

void* mlc_read(const char* json_str, const Schema* schema, ERRMSG) {
    PTR_RETURN_SETUP(void)
    char* json_copy = strdup(json_str);
    RAISE_IF(json_copy == NULL, "Failed to allocate for mlc_read")
    char* child_errmsg = NULL;
    void* result = (void*)read_json_with_schema(NULL, json_copy, schema, &child_errmsg);
    free(json_copy);
    if (child_errmsg != NULL) {
        free(child_errmsg);
        return NULL;
    }
    return result;
}
