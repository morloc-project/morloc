#include "morloc.h"

size_t arrow_element_size(morloc_serial_type type) {
    switch (type) {
        case MORLOC_BOOL:    return 1; // stored as byte, not bit-packed
        case MORLOC_SINT8:   return 1;
        case MORLOC_UINT8:   return 1;
        case MORLOC_SINT16:  return 2;
        case MORLOC_UINT16:  return 2;
        case MORLOC_SINT32:  return 4;
        case MORLOC_UINT32:  return 4;
        case MORLOC_FLOAT32: return 4;
        case MORLOC_SINT64:  return 8;
        case MORLOC_UINT64:  return 8;
        case MORLOC_FLOAT64: return 8;
        default:             return 0; // variable-width or unsupported
    }
}

const char* arrow_format_string(morloc_serial_type type) {
    switch (type) {
        case MORLOC_BOOL:    return "b";
        case MORLOC_SINT8:   return "c";
        case MORLOC_UINT8:   return "C";
        case MORLOC_SINT16:  return "s";
        case MORLOC_UINT16:  return "S";
        case MORLOC_SINT32:  return "i";
        case MORLOC_UINT32:  return "I";
        case MORLOC_SINT64:  return "l";
        case MORLOC_UINT64:  return "L";
        case MORLOC_FLOAT32: return "f";
        case MORLOC_FLOAT64: return "g";
        case MORLOC_STRING:  return "u"; // utf-8
        default:             return NULL;
    }
}

morloc_serial_type arrow_format_to_type(const char* format) {
    if (!format || !format[0] || format[1] != '\0') return MORLOC_NIL;
    switch (format[0]) {
        case 'b': return MORLOC_BOOL;
        case 'c': return MORLOC_SINT8;
        case 'C': return MORLOC_UINT8;
        case 's': return MORLOC_SINT16;
        case 'S': return MORLOC_UINT16;
        case 'i': return MORLOC_SINT32;
        case 'I': return MORLOC_UINT32;
        case 'l': return MORLOC_SINT64;
        case 'L': return MORLOC_UINT64;
        case 'f': return MORLOC_FLOAT32;
        case 'g': return MORLOC_FLOAT64;
        case 'u': return MORLOC_STRING;
        default:  return MORLOC_NIL;
    }
}

const arrow_column_desc_t* arrow_column_desc(
    const arrow_shm_header_t* header,
    uint32_t col_index
) {
    if (!header || col_index >= header->n_columns) return NULL;
    const arrow_column_desc_t* descs =
        (const arrow_column_desc_t*)((const char*)header + sizeof(arrow_shm_header_t));
    return &descs[col_index];
}

const void* arrow_column_data(
    const arrow_shm_header_t* header,
    uint32_t col_index
) {
    const arrow_column_desc_t* desc = arrow_column_desc(header, col_index);
    if (!desc) return NULL;
    return (const char*)header + desc->data_offset;
}

const char* arrow_column_name(
    const arrow_shm_header_t* header,
    uint32_t col_index
) {
    const arrow_column_desc_t* desc = arrow_column_desc(header, col_index);
    if (!desc) return NULL;
    return (const char*)header + desc->name_offset;
}

relptr_t arrow_to_shm(
    const struct ArrowArray* array,
    const struct ArrowSchema* schema,
    ERRMSG
) {
    VAL_RETURN_SETUP(relptr_t, RELNULL)

    RAISE_IF(!array || !schema, "NULL array or schema")
    RAISE_IF(!schema->format || strcmp(schema->format, "+s") != 0,
             "Expected struct schema (format '+s'), got '%s'",
             schema->format ? schema->format : "NULL")

    int64_t n_cols = schema->n_children;
    int64_t n_rows = array->length;
    RAISE_IF(n_cols <= 0, "Arrow struct must have at least one column")
    RAISE_IF(array->n_children != n_cols, "Schema/array column count mismatch")

    // Calculate total size needed
    size_t header_size = sizeof(arrow_shm_header_t);
    size_t descs_size = (size_t)n_cols * sizeof(arrow_column_desc_t);
    size_t names_size = 0;
    for (int64_t i = 0; i < n_cols; i++) {
        const char* name = schema->children[i]->name;
        names_size += name ? strlen(name) : 0;
    }
    size_t data_start = ARROW_ALIGN_UP(header_size + descs_size + names_size);
    size_t total_size = data_start;

    for (int64_t i = 0; i < n_cols; i++) {
        morloc_serial_type col_type = arrow_format_to_type(schema->children[i]->format);
        size_t elem_size = arrow_element_size(col_type);
        if (col_type == MORLOC_STRING) {
            // String columns: offsets array (int32_t[n_rows+1]) + packed string data
            struct ArrowArray* child = array->children[i];
            const int32_t* offsets = (const int32_t*)child->buffers[1];
            size_t str_data_size = offsets ? (size_t)offsets[child->offset + n_rows] - (size_t)offsets[child->offset] : 0;
            total_size = ARROW_ALIGN_UP(total_size)
                       + ((size_t)n_rows + 1) * sizeof(int32_t)
                       + str_data_size;
        } else {
            RAISE_IF(elem_size == 0,
                     "Unsupported Arrow column type '%s' for column %lld",
                     schema->children[i]->format, (long long)i)
            total_size = ARROW_ALIGN_UP(total_size) + elem_size * (size_t)n_rows;
        }
    }

    // Allocate shared memory
    void* shm_ptr = TRY(shmalloc, total_size);
    memset(shm_ptr, 0, total_size);

    // Write header
    arrow_shm_header_t* header = (arrow_shm_header_t*)shm_ptr;
    header->magic = ARROW_SHM_MAGIC;
    header->n_columns = (uint32_t)n_cols;
    header->n_rows = (uint64_t)n_rows;
    header->total_size = (uint64_t)total_size;

    // Write column descriptors and names
    arrow_column_desc_t* descs =
        (arrow_column_desc_t*)((char*)shm_ptr + header_size);
    size_t name_cursor = header_size + descs_size;
    size_t data_cursor = data_start;

    for (int64_t i = 0; i < n_cols; i++) {
        struct ArrowSchema* child_schema = schema->children[i];
        struct ArrowArray* child_array = array->children[i];
        morloc_serial_type col_type = arrow_format_to_type(child_schema->format);

        data_cursor = ARROW_ALIGN_UP(data_cursor);

        const char* name = child_schema->name ? child_schema->name : "";
        size_t name_len = strlen(name);

        descs[i].type = col_type;
        descs[i].length = (uint64_t)n_rows;
        descs[i].null_count = (uint64_t)child_array->null_count;
        descs[i].name_offset = (uint32_t)name_cursor;
        descs[i].name_length = (uint16_t)name_len;
        descs[i].data_offset = (uint64_t)data_cursor;

        // Copy name
        if (name_len > 0) {
            memcpy((char*)shm_ptr + name_cursor, name, name_len);
        }
        name_cursor += name_len;

        if (col_type == MORLOC_STRING) {
            // String column: copy offsets (rebased to 0) then packed string data
            // Arrow string: buffers[1] = int32_t offsets, buffers[2] = char data
            const int32_t* src_offsets = (const int32_t*)child_array->buffers[1];
            const char* src_data = (const char*)child_array->buffers[2];
            int64_t arr_offset = child_array->offset;

            // Write rebased offsets
            int32_t* dst_offsets = (int32_t*)((char*)shm_ptr + data_cursor);
            int32_t base = src_offsets ? src_offsets[arr_offset] : 0;
            for (int64_t r = 0; r <= n_rows; r++) {
                dst_offsets[r] = src_offsets ? (src_offsets[arr_offset + r] - base) : 0;
            }
            size_t offsets_size = ((size_t)n_rows + 1) * sizeof(int32_t);

            // Copy string data
            size_t str_data_size = src_offsets ? (size_t)(src_offsets[arr_offset + n_rows] - base) : 0;
            if (str_data_size > 0 && src_data) {
                memcpy((char*)shm_ptr + data_cursor + offsets_size, src_data + base, str_data_size);
            }

            size_t buf_size = offsets_size + str_data_size;
            descs[i].data_size = (uint64_t)buf_size;
            data_cursor += buf_size;
        } else {
            // Fixed-width column
            size_t elem_size = arrow_element_size(col_type);
            size_t buf_size = elem_size * (size_t)n_rows;
            descs[i].data_size = (uint64_t)buf_size;

            // Arrow arrays: buffers[0] is validity bitmap (may be NULL),
            // buffers[1] is the data buffer for fixed-width types
            const void* src = NULL;
            if (child_array->n_buffers >= 2 && child_array->buffers[1]) {
                src = (const char*)child_array->buffers[1]
                      + (size_t)child_array->offset * elem_size;
            }
            if (src && buf_size > 0) {
                memcpy((char*)shm_ptr + data_cursor, src, buf_size);
            }
            data_cursor += buf_size;
        }
    }

    relptr_t relptr = TRY(abs2rel, shm_ptr);
    return relptr;
}

int arrow_validate(
    const arrow_shm_header_t* header,
    const Schema* schema,
    ERRMSG
) {
    BOOL_RETURN_SETUP

    RAISE_IF(!header, "NULL arrow header")
    RAISE_IF(header->magic != ARROW_SHM_MAGIC, "Invalid arrow SHM magic")
    RAISE_IF(!schema, "NULL schema for arrow validation")
    RAISE_IF(schema->type != MORLOC_MAP,
             "Expected MORLOC_MAP schema for arrow table")

    uint32_t n_cols = header->n_columns;
    RAISE_IF((size_t)n_cols != schema->size,
             "Column count mismatch: arrow has %u, schema has %zu",
             n_cols, schema->size)

    for (uint32_t i = 0; i < n_cols; i++) {
        const arrow_column_desc_t* desc = arrow_column_desc(header, i);
        RAISE_IF(!desc, "NULL column descriptor at index %u", i)

        // Check column type matches schema parameter type
        RAISE_IF(desc->type != schema->parameters[i]->type,
                 "Column %u type mismatch: arrow=%d, schema=%d",
                 i, desc->type, schema->parameters[i]->type)

        // Check column name matches schema key
        if (schema->keys && schema->keys[i]) {
            const char* shm_name = arrow_column_name(header, i);
            size_t key_len = strlen(schema->keys[i]);
            RAISE_IF(desc->name_length != (uint16_t)key_len ||
                     memcmp(shm_name, schema->keys[i], key_len) != 0,
                     "Column %u name mismatch", i)
        }
    }

    return 0;
}

// Release callback for child ArrowSchema (leaf nodes, no children of their own)
static void arrow_shm_child_schema_release(struct ArrowSchema* schema) {
    if (!schema) return;
    free((void*)schema->name);
    schema->name = NULL;
    schema->release = NULL;
}

// Release callback for child ArrowArray (leaf nodes, buffers live in shared memory)
static void arrow_shm_child_array_release(struct ArrowArray* array) {
    if (!array) return;
    free((void*)array->buffers);
    array->buffers = NULL;
    array->release = NULL;
}

// Release callback for parent ArrowSchema created by arrow_from_shm
static void arrow_shm_schema_release(struct ArrowSchema* schema) {
    if (!schema) return;
    for (int64_t i = 0; i < schema->n_children; i++) {
        if (schema->children[i]) {
            if (schema->children[i]->release) {
                schema->children[i]->release(schema->children[i]);
            }
            free((void*)schema->children[i]);
        }
    }
    free(schema->children);
    schema->children = NULL;
    schema->release = NULL;
}

// Release callback for parent ArrowArray created by arrow_from_shm
// Does NOT free the data buffers (they live in shared memory)
static void arrow_shm_array_release(struct ArrowArray* array) {
    if (!array) return;
    for (int64_t i = 0; i < array->n_children; i++) {
        if (array->children[i]) {
            if (array->children[i]->release) {
                array->children[i]->release(array->children[i]);
            }
            free(array->children[i]);
        }
    }
    free(array->children);
    array->children = NULL;
    free((void*)array->buffers);
    array->buffers = NULL;
    array->release = NULL;
}

int arrow_from_shm(
    const arrow_shm_header_t* header,
    struct ArrowSchema* out_schema,
    struct ArrowArray* out_array,
    ERRMSG
) {
    BOOL_RETURN_SETUP

    RAISE_IF(!header, "NULL arrow header")
    RAISE_IF(header->magic != ARROW_SHM_MAGIC, "Invalid arrow SHM magic")

    uint32_t n_cols = header->n_columns;
    int64_t n_rows = (int64_t)header->n_rows;

    // Build parent schema (struct type)
    memset(out_schema, 0, sizeof(*out_schema));
    out_schema->format = "+s";
    out_schema->name = NULL;
    out_schema->n_children = (int64_t)n_cols;
    out_schema->children = (struct ArrowSchema**)calloc(
        n_cols, sizeof(struct ArrowSchema*));
    RAISE_IF(!out_schema->children, "Failed to allocate schema children")
    out_schema->release = arrow_shm_schema_release;

    // Build parent array (struct type needs 1 buffer for validity bitmap)
    memset(out_array, 0, sizeof(*out_array));
    out_array->length = n_rows;
    out_array->null_count = 0;
    out_array->n_buffers = 1;
    out_array->buffers = (const void**)calloc(1, sizeof(void*));
    RAISE_IF(!out_array->buffers, "Failed to allocate parent buffers")
    out_array->buffers[0] = NULL; // no validity bitmap
    out_array->n_children = (int64_t)n_cols;
    out_array->children = (struct ArrowArray**)calloc(
        n_cols, sizeof(struct ArrowArray*));
    RAISE_IF(!out_array->children, "Failed to allocate array children")
    out_array->release = arrow_shm_array_release;

    for (uint32_t i = 0; i < n_cols; i++) {
        const arrow_column_desc_t* desc = arrow_column_desc(header, i);

        // Child schema - store immediately so release callbacks can clean up on failure
        struct ArrowSchema* child_s = (struct ArrowSchema*)calloc(
            1, sizeof(struct ArrowSchema));
        RAISE_IF_WITH(!child_s,
            (out_schema->release(out_schema), out_array->release(out_array)),
            "Failed to allocate child schema")
        child_s->release = arrow_shm_child_schema_release;
        out_schema->children[i] = child_s;

        child_s->format = arrow_format_string(desc->type);
        // Name must be null-terminated for Arrow C Data Interface
        const char* raw_name = arrow_column_name(header, i);
        uint16_t name_len = desc->name_length;
        char* name_copy = (char*)calloc(name_len + 1, 1);
        RAISE_IF_WITH(!name_copy,
            (out_schema->release(out_schema), out_array->release(out_array)),
            "Failed to allocate column name")
        if (raw_name && name_len > 0) memcpy(name_copy, raw_name, name_len);
        child_s->name = name_copy;
        child_s->n_children = 0;

        // Child array - store immediately so release callbacks can clean up on failure
        struct ArrowArray* child_a = (struct ArrowArray*)calloc(
            1, sizeof(struct ArrowArray));
        RAISE_IF_WITH(!child_a,
            (out_schema->release(out_schema), out_array->release(out_array)),
            "Failed to allocate child array")
        child_a->release = arrow_shm_child_array_release;
        out_array->children[i] = child_a;

        child_a->length = n_rows;
        child_a->null_count = (int64_t)desc->null_count;

        const void* col_buf = arrow_column_data(header, i);
        if (desc->type == MORLOC_STRING) {
            // String: offsets at col_buf, string data after offsets array
            child_a->n_buffers = 3;
            child_a->buffers = (const void**)calloc(3, sizeof(void*));
            RAISE_IF_WITH(!child_a->buffers,
                (out_schema->release(out_schema), out_array->release(out_array)),
                "Failed to allocate child buffers")
            child_a->buffers[0] = NULL; // no validity bitmap
            child_a->buffers[1] = col_buf; // int32_t offsets
            child_a->buffers[2] = (const char*)col_buf + ((size_t)n_rows + 1) * sizeof(int32_t); // string data
        } else {
            child_a->n_buffers = 2;
            child_a->buffers = (const void**)calloc(2, sizeof(void*));
            RAISE_IF_WITH(!child_a->buffers,
                (out_schema->release(out_schema), out_array->release(out_array)),
                "Failed to allocate child buffers")
            child_a->buffers[0] = NULL; // no validity bitmap
            child_a->buffers[1] = col_buf; // zero-copy!
        }
    }

    return 0;
}
