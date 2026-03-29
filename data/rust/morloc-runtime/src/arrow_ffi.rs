//! Arrow C Data Interface implementation.
//! Replaces arrow.c. Provides SHM ↔ Arrow conversion.

use std::ffi::{c_char, c_void};
use std::ptr;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::shm::{self, RelPtr};

const ARROW_SHM_MAGIC: u32 = 0xA770DA7A;
const ARROW_BUFFER_ALIGN: usize = 64;

fn arrow_align_up(x: usize) -> usize {
    (x + ARROW_BUFFER_ALIGN - 1) & !(ARROW_BUFFER_ALIGN - 1)
}

// ── Arrow C Data Interface structs (matching Apache spec) ────────────────────

#[repr(C)]
pub struct ArrowSchema {
    pub format: *const c_char,
    pub name: *const c_char,
    pub metadata: *const c_char,
    pub flags: i64,
    pub n_children: i64,
    pub children: *mut *mut ArrowSchema,
    pub dictionary: *mut ArrowSchema,
    pub release: Option<unsafe extern "C" fn(*mut ArrowSchema)>,
    pub private_data: *mut c_void,
}

#[repr(C)]
pub struct ArrowArray {
    pub length: i64,
    pub null_count: i64,
    pub offset: i64,
    pub n_buffers: i64,
    pub n_children: i64,
    pub buffers: *mut *const c_void,
    pub children: *mut *mut ArrowArray,
    pub dictionary: *mut ArrowArray,
    pub release: Option<unsafe extern "C" fn(*mut ArrowArray)>,
    pub private_data: *mut c_void,
}

// ── SHM header types ─────────────────────────────────────────────────────────

#[repr(C)]
pub struct ArrowColumnDesc {
    pub col_type: u32,       // morloc_serial_type
    pub length: u64,
    pub null_count: u64,
    pub name_offset: u32,
    pub name_length: u16,
    pub data_offset: u64,
    pub data_size: u64,
}

#[repr(C)]
pub struct ArrowShmHeader {
    pub magic: u32,
    pub n_columns: u32,
    pub n_rows: u64,
    pub total_size: u64,
}

// ── Type mapping ─────────────────────────────────────────────────────────────

// Serial type constants matching C enum
const MORLOC_NIL: u32 = 0;
const MORLOC_BOOL: u32 = 1;
const MORLOC_SINT8: u32 = 2;
const MORLOC_SINT16: u32 = 3;
const MORLOC_SINT32: u32 = 4;
const MORLOC_SINT64: u32 = 5;
const MORLOC_UINT8: u32 = 6;
const MORLOC_UINT16: u32 = 7;
const MORLOC_UINT32: u32 = 8;
const MORLOC_UINT64: u32 = 9;
const MORLOC_FLOAT32: u32 = 10;
const MORLOC_FLOAT64: u32 = 11;
const MORLOC_STRING: u32 = 12;

#[no_mangle]
pub extern "C" fn arrow_element_size(serial_type: u32) -> usize {
    match serial_type {
        MORLOC_BOOL | MORLOC_SINT8 | MORLOC_UINT8 => 1,
        MORLOC_SINT16 | MORLOC_UINT16 => 2,
        MORLOC_SINT32 | MORLOC_UINT32 | MORLOC_FLOAT32 => 4,
        MORLOC_SINT64 | MORLOC_UINT64 | MORLOC_FLOAT64 => 8,
        _ => 0,
    }
}

#[no_mangle]
pub extern "C" fn arrow_format_string(serial_type: u32) -> *const c_char {
    match serial_type {
        MORLOC_BOOL => b"b\0".as_ptr() as *const c_char,
        MORLOC_SINT8 => b"c\0".as_ptr() as *const c_char,
        MORLOC_UINT8 => b"C\0".as_ptr() as *const c_char,
        MORLOC_SINT16 => b"s\0".as_ptr() as *const c_char,
        MORLOC_UINT16 => b"S\0".as_ptr() as *const c_char,
        MORLOC_SINT32 => b"i\0".as_ptr() as *const c_char,
        MORLOC_UINT32 => b"I\0".as_ptr() as *const c_char,
        MORLOC_SINT64 => b"l\0".as_ptr() as *const c_char,
        MORLOC_UINT64 => b"L\0".as_ptr() as *const c_char,
        MORLOC_FLOAT32 => b"f\0".as_ptr() as *const c_char,
        MORLOC_FLOAT64 => b"g\0".as_ptr() as *const c_char,
        MORLOC_STRING => b"u\0".as_ptr() as *const c_char,
        _ => ptr::null(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn arrow_format_to_type(format: *const c_char) -> u32 {
    if format.is_null() || *format == 0 || *format.add(1) != 0 {
        return MORLOC_NIL;
    }
    match *format as u8 {
        b'b' => MORLOC_BOOL,
        b'c' => MORLOC_SINT8,
        b'C' => MORLOC_UINT8,
        b's' => MORLOC_SINT16,
        b'S' => MORLOC_UINT16,
        b'i' => MORLOC_SINT32,
        b'I' => MORLOC_UINT32,
        b'l' => MORLOC_SINT64,
        b'L' => MORLOC_UINT64,
        b'f' => MORLOC_FLOAT32,
        b'g' => MORLOC_FLOAT64,
        b'u' => MORLOC_STRING,
        _ => MORLOC_NIL,
    }
}

// ── Column accessors (used by arrow_json.c) ──────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn arrow_column_desc(
    header: *const ArrowShmHeader,
    col_index: u32,
) -> *const ArrowColumnDesc {
    if header.is_null() || col_index >= (*header).n_columns {
        return ptr::null();
    }
    let descs = (header as *const u8).add(std::mem::size_of::<ArrowShmHeader>()) as *const ArrowColumnDesc;
    descs.add(col_index as usize)
}

#[no_mangle]
pub unsafe extern "C" fn arrow_column_data(
    header: *const ArrowShmHeader,
    col_index: u32,
) -> *const c_void {
    let desc = arrow_column_desc(header, col_index);
    if desc.is_null() { return ptr::null(); }
    (header as *const u8).add((*desc).data_offset as usize) as *const c_void
}

#[no_mangle]
pub unsafe extern "C" fn arrow_column_name(
    header: *const ArrowShmHeader,
    col_index: u32,
) -> *const c_char {
    let desc = arrow_column_desc(header, col_index);
    if desc.is_null() { return ptr::null(); }
    (header as *const u8).add((*desc).name_offset as usize) as *const c_char
}

// ── arrow_to_shm ─────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn arrow_to_shm(
    array: *const ArrowArray,
    schema: *const ArrowSchema,
    errmsg: *mut *mut c_char,
) -> RelPtr {
    clear_errmsg(errmsg);

    if array.is_null() || schema.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("NULL array or schema".into()));
        return shm::RELNULL;
    }

    // Verify struct type
    if (*schema).format.is_null() || libc::strcmp((*schema).format, b"+s\0".as_ptr() as *const c_char) != 0 {
        set_errmsg(errmsg, &MorlocError::Other("Expected struct schema (format '+s')".into()));
        return shm::RELNULL;
    }

    let n_cols = (*schema).n_children as usize;
    let n_rows = (*array).length as usize;

    if n_cols == 0 {
        set_errmsg(errmsg, &MorlocError::Other("Arrow struct must have at least one column".into()));
        return shm::RELNULL;
    }

    let header_size = std::mem::size_of::<ArrowShmHeader>();
    let descs_size = n_cols * std::mem::size_of::<ArrowColumnDesc>();

    let mut names_size: usize = 0;
    for i in 0..n_cols {
        let child_schema = *(*schema).children.add(i);
        let name = (*child_schema).name;
        if !name.is_null() { names_size += libc::strlen(name); }
    }

    let data_start = arrow_align_up(header_size + descs_size + names_size);
    let mut total_size = data_start;

    for i in 0..n_cols {
        let child_schema = *(*schema).children.add(i);
        let col_type = arrow_format_to_type((*child_schema).format);
        let elem_size = arrow_element_size(col_type);
        if col_type == MORLOC_STRING {
            let child = *(*array).children.add(i);
            let offsets = if (*child).n_buffers >= 2 { *(*child).buffers.add(1) as *const i32 } else { ptr::null() };
            let str_data_size = if !offsets.is_null() {
                let off = (*child).offset as usize;
                (*offsets.add(off + n_rows) - *offsets.add(off)) as usize
            } else { 0 };
            total_size = arrow_align_up(total_size)
                + (n_rows + 1) * std::mem::size_of::<i32>()
                + str_data_size;
        } else {
            if elem_size == 0 {
                set_errmsg(errmsg, &MorlocError::Other(format!("Unsupported Arrow column type for column {}", i)));
                return shm::RELNULL;
            }
            total_size = arrow_align_up(total_size) + elem_size * n_rows;
        }
    }

    let shm_ptr = match shm::shmalloc(total_size) {
        Ok(p) => p,
        Err(e) => { set_errmsg(errmsg, &e); return shm::RELNULL; }
    };
    ptr::write_bytes(shm_ptr, 0, total_size);

    // Write header
    let header = &mut *(shm_ptr as *mut ArrowShmHeader);
    header.magic = ARROW_SHM_MAGIC;
    header.n_columns = n_cols as u32;
    header.n_rows = n_rows as u64;
    header.total_size = total_size as u64;

    let descs = shm_ptr.add(header_size) as *mut ArrowColumnDesc;
    let mut name_cursor = header_size + descs_size;
    let mut data_cursor = data_start;

    for i in 0..n_cols {
        let child_schema = *(*schema).children.add(i);
        let child_array = *(*array).children.add(i);
        let col_type = arrow_format_to_type((*child_schema).format);

        data_cursor = arrow_align_up(data_cursor);

        let name = if (*child_schema).name.is_null() { b"\0".as_ptr() as *const c_char } else { (*child_schema).name };
        let name_len = libc::strlen(name);

        let desc = &mut *descs.add(i);
        desc.col_type = col_type;
        desc.length = n_rows as u64;
        desc.null_count = (*child_array).null_count as u64;
        desc.name_offset = name_cursor as u32;
        desc.name_length = name_len as u16;
        desc.data_offset = data_cursor as u64;

        if name_len > 0 {
            ptr::copy_nonoverlapping(name as *const u8, shm_ptr.add(name_cursor), name_len);
        }
        name_cursor += name_len;

        if col_type == MORLOC_STRING {
            let src_offsets = if (*child_array).n_buffers >= 2 { *(*child_array).buffers.add(1) as *const i32 } else { ptr::null() };
            let src_data = if (*child_array).n_buffers >= 3 { *(*child_array).buffers.add(2) as *const u8 } else { ptr::null() };
            let arr_offset = (*child_array).offset as usize;

            let dst_offsets = shm_ptr.add(data_cursor) as *mut i32;
            let base = if !src_offsets.is_null() { *src_offsets.add(arr_offset) } else { 0 };
            for r in 0..=n_rows {
                *dst_offsets.add(r) = if !src_offsets.is_null() { *src_offsets.add(arr_offset + r) - base } else { 0 };
            }
            let offsets_size = (n_rows + 1) * std::mem::size_of::<i32>();

            let str_data_size = if !src_offsets.is_null() {
                (*src_offsets.add(arr_offset + n_rows) - base) as usize
            } else { 0 };
            if str_data_size > 0 && !src_data.is_null() {
                ptr::copy_nonoverlapping(src_data.add(base as usize), shm_ptr.add(data_cursor + offsets_size), str_data_size);
            }

            let buf_size = offsets_size + str_data_size;
            desc.data_size = buf_size as u64;
            data_cursor += buf_size;
        } else {
            let elem_size = arrow_element_size(col_type);
            let buf_size = elem_size * n_rows;
            desc.data_size = buf_size as u64;

            if (*child_array).n_buffers >= 2 && !(*(*child_array).buffers.add(1)).is_null() {
                let src = (*(*child_array).buffers.add(1) as *const u8).add((*child_array).offset as usize * elem_size);
                if buf_size > 0 {
                    ptr::copy_nonoverlapping(src, shm_ptr.add(data_cursor), buf_size);
                }
            }
            data_cursor += buf_size;
        }
    }

    match shm::abs2rel(shm_ptr) {
        Ok(r) => r,
        Err(e) => { set_errmsg(errmsg, &e); shm::RELNULL }
    }
}

// ── arrow_validate ───────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn arrow_validate(
    header: *const ArrowShmHeader,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    if header.is_null() { set_errmsg(errmsg, &MorlocError::Other("NULL arrow header".into())); return 1; }
    if (*header).magic != ARROW_SHM_MAGIC { set_errmsg(errmsg, &MorlocError::Other("Invalid arrow SHM magic".into())); return 1; }
    if schema.is_null() { set_errmsg(errmsg, &MorlocError::Other("NULL schema for arrow validation".into())); return 1; }
    // MORLOC_MAP = 16
    if (*schema).serial_type != crate::schema::SerialType::Map as u32 { set_errmsg(errmsg, &MorlocError::Other("Expected MORLOC_MAP schema for arrow table".into())); return 1; }

    let n_cols = (*header).n_columns as usize;
    if n_cols != (*schema).size {
        set_errmsg(errmsg, &MorlocError::Other(format!("Column count mismatch: arrow has {}, schema has {}", n_cols, (*schema).size)));
        return 1;
    }

    for i in 0..n_cols {
        let desc = arrow_column_desc(header, i as u32);
        if desc.is_null() {
            set_errmsg(errmsg, &MorlocError::Other(format!("NULL column descriptor at index {}", i)));
            return 1;
        }
        let param_schema = *(*schema).parameters.add(i);
        if (*desc).col_type != (*param_schema).serial_type as u32 {
            set_errmsg(errmsg, &MorlocError::Other(format!("Column {} type mismatch", i)));
            return 1;
        }
    }

    0
}

// ── Release callbacks for arrow_from_shm ─────────────────────────────────────

unsafe extern "C" fn arrow_shm_child_schema_release(schema: *mut ArrowSchema) {
    if schema.is_null() { return; }
    if !(*schema).name.is_null() { libc::free((*schema).name as *mut c_void); }
    (*schema).name = ptr::null();
    (*schema).release = None;
}

unsafe extern "C" fn arrow_shm_child_array_release(array: *mut ArrowArray) {
    if array.is_null() { return; }
    if !(*array).buffers.is_null() { libc::free((*array).buffers as *mut c_void); }
    (*array).buffers = ptr::null_mut();
    (*array).release = None;
}

unsafe extern "C" fn arrow_shm_schema_release(schema: *mut ArrowSchema) {
    if schema.is_null() { return; }
    for i in 0..(*schema).n_children as usize {
        let child = *(*schema).children.add(i);
        if !child.is_null() {
            if let Some(release) = (*child).release { release(child); }
            libc::free(child as *mut c_void);
        }
    }
    libc::free((*schema).children as *mut c_void);
    (*schema).children = ptr::null_mut();
    (*schema).release = None;
}

unsafe extern "C" fn arrow_shm_array_release(array: *mut ArrowArray) {
    if array.is_null() { return; }
    for i in 0..(*array).n_children as usize {
        let child = *(*array).children.add(i);
        if !child.is_null() {
            if let Some(release) = (*child).release { release(child); }
            libc::free(child as *mut c_void);
        }
    }
    libc::free((*array).children as *mut c_void);
    (*array).children = ptr::null_mut();
    if !(*array).buffers.is_null() { libc::free((*array).buffers as *mut c_void); }
    (*array).buffers = ptr::null_mut();
    (*array).release = None;
}

// ── arrow_from_shm ───────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn arrow_from_shm(
    header: *const ArrowShmHeader,
    out_schema: *mut ArrowSchema,
    out_array: *mut ArrowArray,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    if header.is_null() { set_errmsg(errmsg, &MorlocError::Other("NULL arrow header".into())); return 1; }
    if (*header).magic != ARROW_SHM_MAGIC { set_errmsg(errmsg, &MorlocError::Other("Invalid arrow SHM magic".into())); return 1; }

    let n_cols = (*header).n_columns as usize;
    let n_rows = (*header).n_rows as i64;

    // Parent schema (struct type)
    ptr::write_bytes(out_schema, 0, 1);
    (*out_schema).format = b"+s\0".as_ptr() as *const c_char;
    (*out_schema).n_children = n_cols as i64;
    (*out_schema).children = libc::calloc(n_cols, std::mem::size_of::<*mut ArrowSchema>()) as *mut *mut ArrowSchema;
    (*out_schema).release = Some(arrow_shm_schema_release);

    // Parent array
    ptr::write_bytes(out_array, 0, 1);
    (*out_array).length = n_rows;
    (*out_array).n_buffers = 1;
    (*out_array).buffers = libc::calloc(1, std::mem::size_of::<*const c_void>()) as *mut *const c_void;
    (*out_array).n_children = n_cols as i64;
    (*out_array).children = libc::calloc(n_cols, std::mem::size_of::<*mut ArrowArray>()) as *mut *mut ArrowArray;
    (*out_array).release = Some(arrow_shm_array_release);

    for i in 0..n_cols {
        let desc = arrow_column_desc(header, i as u32);

        // Child schema
        let child_s = libc::calloc(1, std::mem::size_of::<ArrowSchema>()) as *mut ArrowSchema;
        (*child_s).release = Some(arrow_shm_child_schema_release);
        *(*out_schema).children.add(i) = child_s;

        (*child_s).format = arrow_format_string((*desc).col_type);
        let raw_name = arrow_column_name(header, i as u32);
        let name_len = (*desc).name_length as usize;
        let name_copy = libc::calloc(name_len + 1, 1) as *mut c_char;
        if !raw_name.is_null() && name_len > 0 {
            ptr::copy_nonoverlapping(raw_name as *const u8, name_copy as *mut u8, name_len);
        }
        (*child_s).name = name_copy;

        // Child array
        let child_a = libc::calloc(1, std::mem::size_of::<ArrowArray>()) as *mut ArrowArray;
        (*child_a).release = Some(arrow_shm_child_array_release);
        *(*out_array).children.add(i) = child_a;

        (*child_a).length = n_rows;
        (*child_a).null_count = (*desc).null_count as i64;

        let col_buf = arrow_column_data(header, i as u32);
        if (*desc).col_type == MORLOC_STRING {
            (*child_a).n_buffers = 3;
            (*child_a).buffers = libc::calloc(3, std::mem::size_of::<*const c_void>()) as *mut *const c_void;
            *(*child_a).buffers.add(1) = col_buf; // offsets
            *(*child_a).buffers.add(2) = (col_buf as *const u8).add((n_rows as usize + 1) * std::mem::size_of::<i32>()) as *const c_void;
        } else {
            (*child_a).n_buffers = 2;
            (*child_a).buffers = libc::calloc(2, std::mem::size_of::<*const c_void>()) as *mut *const c_void;
            *(*child_a).buffers.add(1) = col_buf; // zero-copy data
        }
    }

    0
}

// ── Arrow JSON/Table output (replaces arrow_json.c) ──────────────────────────

unsafe fn print_arrow_value(desc: *const ArrowColumnDesc, col_data: *const u8, row: u64) {
    let row = row as usize;
    match (*desc).col_type {
        MORLOC_BOOL => {
            let v = *col_data.add(row);
            if v != 0 { libc::printf(b"true\0".as_ptr() as *const c_char); }
            else { libc::printf(b"false\0".as_ptr() as *const c_char); }
        }
        MORLOC_SINT8 => { libc::printf(b"%d\0".as_ptr() as *const c_char, *(col_data as *const i8).add(row) as i32); }
        MORLOC_SINT16 => { libc::printf(b"%d\0".as_ptr() as *const c_char, *(col_data as *const i16).add(row) as i32); }
        MORLOC_SINT32 => { libc::printf(b"%d\0".as_ptr() as *const c_char, *(col_data as *const i32).add(row)); }
        MORLOC_SINT64 => { libc::printf(b"%ld\0".as_ptr() as *const c_char, *(col_data as *const i64).add(row)); }
        MORLOC_UINT8 => { libc::printf(b"%u\0".as_ptr() as *const c_char, *col_data.add(row) as u32); }
        MORLOC_UINT16 => { libc::printf(b"%u\0".as_ptr() as *const c_char, *(col_data as *const u16).add(row) as u32); }
        MORLOC_UINT32 => { libc::printf(b"%u\0".as_ptr() as *const c_char, *(col_data as *const u32).add(row)); }
        MORLOC_UINT64 => { libc::printf(b"%lu\0".as_ptr() as *const c_char, *(col_data as *const u64).add(row)); }
        MORLOC_FLOAT32 => { libc::printf(b"%.7g\0".as_ptr() as *const c_char, *(col_data as *const f32).add(row) as f64); }
        MORLOC_FLOAT64 => { libc::printf(b"%.15g\0".as_ptr() as *const c_char, *(col_data as *const f64).add(row)); }
        MORLOC_STRING => {
            let offsets = col_data as *const i32;
            let str_data = offsets.add((*desc).length as usize + 1) as *const u8;
            let start = *offsets.add(row) as usize;
            let end = *offsets.add(row + 1) as usize;
            libc::putchar(b'"' as i32);
            for i in start..end {
                let c = *str_data.add(i);
                match c {
                    b'"' => { libc::printf(b"\\\"\0".as_ptr() as *const c_char); }
                    b'\\' => { libc::printf(b"\\\\\0".as_ptr() as *const c_char); }
                    b'\n' => { libc::printf(b"\\n\0".as_ptr() as *const c_char); }
                    b'\r' => { libc::printf(b"\\r\0".as_ptr() as *const c_char); }
                    b'\t' => { libc::printf(b"\\t\0".as_ptr() as *const c_char); }
                    _ if c < 32 => { libc::printf(b"\\u%04x\0".as_ptr() as *const c_char, c as u32); }
                    _ => { libc::putchar(c as i32); }
                }
            }
            libc::putchar(b'"' as i32);
        }
        _ => { libc::printf(b"null\0".as_ptr() as *const c_char); }
    }
}

#[no_mangle]
pub unsafe extern "C" fn print_arrow_as_json(
    data: *const c_void,
    errmsg: *mut *mut c_char,
) -> bool {
    use crate::error::{clear_errmsg, set_errmsg, MorlocError};
    clear_errmsg(errmsg);

    let header = data as *const ArrowShmHeader;
    if header.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("NULL arrow data".into()));
        return false;
    }
    if (*header).magic != ARROW_SHM_MAGIC {
        set_errmsg(errmsg, &MorlocError::Other(format!("Invalid arrow SHM magic: 0x{:08x}", (*header).magic)));
        return false;
    }

    let n_cols = (*header).n_columns;
    let n_rows = (*header).n_rows;

    libc::putchar(b'[' as i32);
    for r in 0..n_rows {
        if r > 0 { libc::putchar(b',' as i32); }
        libc::putchar(b'{' as i32);
        for c in 0..n_cols {
            if c > 0 { libc::putchar(b',' as i32); }
            let desc = arrow_column_desc(header, c);
            let name = arrow_column_name(header, c);
            let col_data = arrow_column_data(header, c);
            if !name.is_null() {
                libc::printf(b"\"%.*s\":\0".as_ptr() as *const c_char, (*desc).name_length as i32, name);
            }
            if !desc.is_null() && !col_data.is_null() {
                print_arrow_value(desc, col_data as *const u8, r);
            } else {
                libc::printf(b"null\0".as_ptr() as *const c_char);
            }
        }
        libc::putchar(b'}' as i32);
    }
    libc::printf(b"]\n\0".as_ptr() as *const c_char);
    true
}

#[no_mangle]
pub unsafe extern "C" fn print_arrow_as_table(
    data: *const c_void,
    errmsg: *mut *mut c_char,
) -> bool {
    use crate::error::{clear_errmsg, set_errmsg, MorlocError};
    clear_errmsg(errmsg);

    let header = data as *const ArrowShmHeader;
    if header.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("NULL arrow data".into()));
        return false;
    }
    if (*header).magic != ARROW_SHM_MAGIC {
        set_errmsg(errmsg, &MorlocError::Other(format!("Invalid arrow SHM magic: 0x{:08x}", (*header).magic)));
        return false;
    }

    let n_cols = (*header).n_columns;
    let n_rows = (*header).n_rows;

    for c in 0..n_cols {
        if c > 0 { libc::putchar(b'\t' as i32); }
        let desc = arrow_column_desc(header, c);
        let name = arrow_column_name(header, c);
        if !name.is_null() && !desc.is_null() {
            libc::printf(b"%.*s\0".as_ptr() as *const c_char, (*desc).name_length as i32, name);
        }
    }
    libc::putchar(b'\n' as i32);

    for r in 0..n_rows {
        for c in 0..n_cols {
            if c > 0 { libc::putchar(b'\t' as i32); }
            let desc = arrow_column_desc(header, c);
            let col_data = arrow_column_data(header, c);
            if !desc.is_null() && !col_data.is_null() {
                print_arrow_value(desc, col_data as *const u8, r);
            }
        }
        libc::putchar(b'\n' as i32);
    }
    true
}
