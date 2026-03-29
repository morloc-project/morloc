//! C ABI wrappers for SLURM job submission.
//! Replaces slurm.c.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, MorlocError};

const MAX_SLURM_COMMAND_LENGTH: usize = 1024;
const DEFAULT_XXHASH_SEED: u64 = 0;

// ── C-compatible types ───────────────────────────────────────────────────────

#[repr(C)]
pub struct Resources {
    pub memory: i32,  // in Gb
    pub time: i32,    // walltime in seconds
    pub cpus: i32,
    pub gpus: i32,
}

// ── parse_slurm_time ─────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn parse_slurm_time(
    time_str: *const c_char,
    errmsg: *mut *mut c_char,
) -> usize {
    clear_errmsg(errmsg);
    let s = CStr::from_ptr(time_str).to_string_lossy();

    let mut days: i32 = 0;
    let hours: i32;
    let minutes: i32;
    let seconds: i32;

    // Try D-HH:MM:SS format
    if let Some(dash_pos) = s.find('-') {
        days = match s[..dash_pos].parse() {
            Ok(d) => d,
            Err(_) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("Failed to scan slurm walltime string '{}'", s)));
                return 0;
            }
        };
        let rest = &s[dash_pos + 1..];
        let parts: Vec<&str> = rest.split(':').collect();
        if parts.len() != 3 {
            set_errmsg(errmsg, &MorlocError::Other(format!("Failed to scan slurm walltime string '{}'", s)));
            return 0;
        }
        hours = parts[0].parse().unwrap_or(-1);
        minutes = parts[1].parse().unwrap_or(-1);
        seconds = parts[2].parse().unwrap_or(-1);
    } else {
        // Try HH:MM:SS format
        let parts: Vec<&str> = s.split(':').collect();
        if parts.len() != 3 {
            set_errmsg(errmsg, &MorlocError::Other(format!("Failed to scan slurm walltime string '{}'", s)));
            return 0;
        }
        hours = parts[0].parse().unwrap_or(-1);
        minutes = parts[1].parse().unwrap_or(-1);
        seconds = parts[2].parse().unwrap_or(-1);
    }

    if days < 0 || hours < 0 || minutes < 0 || seconds < 0 {
        set_errmsg(errmsg, &MorlocError::Other(format!("Negative time component in '{}'", s)));
        return 0;
    }
    if hours > 23 || minutes > 59 || seconds > 59 {
        set_errmsg(errmsg, &MorlocError::Other(format!("Invalid time component in '{}' (HH<=23 MM<=59 SS<=59)", s)));
        return 0;
    }
    if days > 3650 {
        set_errmsg(errmsg, &MorlocError::Other("Do you really want to run this job for more than 10 years?".into()));
        return 0;
    }

    (seconds + 60 * minutes + 60 * 60 * hours + 60 * 60 * 24 * days) as usize
}

#[no_mangle]
pub unsafe extern "C" fn write_slurm_time(seconds: i32) -> *mut c_char {
    let mut rem = seconds;
    let days = rem / (60 * 60 * 24);
    rem -= days * 60 * 60 * 24;
    let hours = rem / (60 * 60);
    rem -= hours * 60 * 60;
    let minutes = rem / 60;
    rem -= minutes * 60;

    let s = format!("{}-{:02}:{:02}:{:02}", days, hours, minutes, rem);
    match CString::new(s) {
        Ok(cs) => cs.into_raw(),
        Err(_) => ptr::null_mut(),
    }
}

// ── parse_morloc_call_arguments ──────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn parse_morloc_call_arguments(
    packet: *mut u8,
    args: *mut *mut u8,
    nargs: *mut usize,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    *nargs = 0;

    let header = &*(packet as *const crate::packet::PacketHeader);
    let packet_size = 32 + header.offset as usize + header.length as usize;

    if header.command_type() != crate::packet::PACKET_TYPE_CALL {
        set_errmsg(errmsg, &MorlocError::Packet("Unexpected packet type (BUG)".into()));
        return false;
    }

    // First pass: count args
    let mut pos = 32 + header.offset as usize;
    while pos < packet_size {
        let arg_header = &*(packet.add(pos) as *const crate::packet::PacketHeader);
        pos += 32 + arg_header.offset as usize + arg_header.length as usize;
        *nargs += 1;
    }

    // Second pass: set pointers
    pos = 32 + header.offset as usize;
    for i in 0..*nargs {
        *args.add(i) = packet.add(pos);
        let arg_header = &*(packet.add(pos) as *const crate::packet::PacketHeader);
        pos += 32 + arg_header.offset as usize + arg_header.length as usize;
    }

    true
}

// ── slurm_job_is_complete ────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn slurm_job_is_complete(job_id: u32) -> bool {
    let cmd = format!("sacct -j {} --format=State --noheader\0", job_id);
    let sacct = libc::popen(cmd.as_ptr() as *const c_char, b"r\0".as_ptr() as *const c_char);
    if sacct.is_null() { return false; }

    let mut state = [0u8; 64];
    let mut done = false;
    while !libc::fgets(state.as_mut_ptr() as *mut c_char, 64, sacct).is_null() {
        let s = std::str::from_utf8(&state).unwrap_or("");
        if s.contains("COMPLETED") || s.contains("FAILED") || s.contains("CANCELLED") {
            done = true;
            break;
        }
    }
    libc::pclose(sacct);
    done
}

// ── shell_escape ─────────────────────────────────────────────────────────────

fn shell_escape(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 10);
    out.push('\'');
    for ch in input.chars() {
        if ch == '\'' {
            out.push_str("'\\''");
        } else {
            out.push(ch);
        }
    }
    out.push('\'');
    out
}

// ── submit_morloc_slurm_job ──────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn submit_morloc_slurm_job(
    nexus_path: *const c_char,
    socket_basename: *const c_char,
    call_packet_filename: *const c_char,
    result_cache_filename: *const c_char,
    output_filename: *const c_char,
    error_filename: *const c_char,
    resources: *const Resources,
    errmsg: *mut *mut c_char,
) -> u32 {
    clear_errmsg(errmsg);

    macro_rules! check_null {
        ($ptr:expr, $name:expr) => {
            if $ptr.is_null() {
                set_errmsg(errmsg, &MorlocError::Other(format!("{} undefined", $name)));
                return 0;
            }
        };
    }
    check_null!(nexus_path, "nexus path");
    check_null!(socket_basename, "socket basename");
    check_null!(call_packet_filename, "call packet filename");
    check_null!(result_cache_filename, "result cache filename");
    check_null!(output_filename, "slurm output filename");
    check_null!(error_filename, "slurm error filename");

    let res = &*resources;
    let nexus = CStr::from_ptr(nexus_path).to_string_lossy();
    let call = CStr::from_ptr(call_packet_filename).to_string_lossy();
    let socket = CStr::from_ptr(socket_basename).to_string_lossy();
    let result_cache = CStr::from_ptr(result_cache_filename).to_string_lossy();
    let output = CStr::from_ptr(output_filename).to_string_lossy();
    let error = CStr::from_ptr(error_filename).to_string_lossy();

    let time_str_raw = write_slurm_time(res.time);
    let time_str = CStr::from_ptr(time_str_raw).to_string_lossy().into_owned();
    libc::free(time_str_raw as *mut c_void);

    let mem_arg = format!("--mem={}G", res.memory);
    let time_arg = format!("--time={}", time_str);
    let cpus_arg = format!("--cpus-per-task={}", res.cpus);
    let gpus_arg = format!("--gres=gpu:{}", res.gpus);

    let esc_nexus = shell_escape(&nexus);
    let esc_call = shell_escape(&call);
    let esc_socket = shell_escape(&socket);
    let esc_result = shell_escape(&result_cache);

    let wrap_cmd = format!(
        "{} --call-packet {} --socket-base {} --output-file {} --output-form packet",
        esc_nexus, esc_call, esc_socket, esc_result
    );

    if wrap_cmd.len() >= MAX_SLURM_COMMAND_LENGTH {
        set_errmsg(errmsg, &MorlocError::Other("Wrap command too long".into()));
        return 0;
    }

    let wrap_arg = format!("--wrap={}", wrap_cmd);

    // Fork/exec sbatch
    let mut pipefd = [0i32; 2];
    if libc::pipe(pipefd.as_mut_ptr()) == -1 {
        set_errmsg(errmsg, &MorlocError::Other("Failed to create pipe for sbatch".into()));
        return 0;
    }

    let pid = libc::fork();
    if pid == -1 {
        libc::close(pipefd[0]);
        libc::close(pipefd[1]);
        set_errmsg(errmsg, &MorlocError::Other("Failed to fork for sbatch".into()));
        return 0;
    }

    if pid == 0 {
        // Child
        libc::close(pipefd[0]);
        libc::dup2(pipefd[1], libc::STDOUT_FILENO);
        libc::close(pipefd[1]);

        let sbatch = CString::new("sbatch").unwrap();
        let parsable = CString::new("--parsable").unwrap();
        let o_flag = CString::new("-o").unwrap();
        let e_flag = CString::new("-e").unwrap();
        let c_output = CString::new(output.as_ref()).unwrap();
        let c_error = CString::new(error.as_ref()).unwrap();
        let c_mem = CString::new(mem_arg).unwrap();
        let c_time = CString::new(time_arg).unwrap();
        let c_cpus = CString::new(cpus_arg).unwrap();
        let c_gpus = CString::new(gpus_arg).unwrap();
        let c_wrap = CString::new(wrap_arg).unwrap();

        libc::execlp(
            sbatch.as_ptr(),
            sbatch.as_ptr(),
            parsable.as_ptr(),
            o_flag.as_ptr(), c_output.as_ptr(),
            e_flag.as_ptr(), c_error.as_ptr(),
            c_mem.as_ptr(), c_time.as_ptr(), c_cpus.as_ptr(), c_gpus.as_ptr(),
            c_wrap.as_ptr(),
            ptr::null::<c_char>(),
        );
        libc::_exit(127);
    }

    // Parent
    libc::close(pipefd[1]);

    let mut buf = [0u8; 64];
    let nread = libc::read(pipefd[0], buf.as_mut_ptr() as *mut c_void, 63);
    libc::close(pipefd[0]);

    let mut status: i32 = 0;
    libc::waitpid(pid, &mut status, 0);

    if !libc::WIFEXITED(status) || libc::WEXITSTATUS(status) != 0 {
        set_errmsg(errmsg, &MorlocError::Other("sbatch exited with error".into()));
        return 0;
    }
    if nread <= 0 {
        set_errmsg(errmsg, &MorlocError::Other("Failed to read sbatch output".into()));
        return 0;
    }

    let output_str = std::str::from_utf8(&buf[..nread as usize]).unwrap_or("");
    match output_str.trim().parse::<u32>() {
        Ok(job_id) => job_id,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("Failed to parse job ID from sbatch output".into()));
            0
        }
    }
}

// ── remote_call ──────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn remote_call(
    midx: i32,
    socket_basename: *const c_char,
    cache_path: *const c_char,
    resources: *const Resources,
    arg_packets: *const *const u8,
    nargs: usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);

    // Use extern C declarations for functions from other modules
    extern "C" {
        fn read_schema_from_packet_meta(packet: *const u8, errmsg: *mut *mut c_char) -> *mut c_char;
        fn parse_schema(schema_str: *const c_char, errmsg: *mut *mut c_char) -> *mut crate::cschema::CSchema;
        fn free_schema(schema: *mut crate::cschema::CSchema);
        fn get_morloc_data_packet_value(data: *const u8, schema: *const crate::cschema::CSchema, errmsg: *mut *mut c_char) -> *mut u8;
        fn hash_voidstar(data: *const u8, schema: *const crate::cschema::CSchema, seed: u64, hash: *mut u64, errmsg: *mut *mut c_char) -> bool;
        fn mix(a: u64, b: u64) -> u64;
        fn mkdir_p(path: *const c_char, errmsg: *mut *mut c_char) -> i32;
        fn check_cache_packet(key: u64, cache_path: *const c_char, errmsg: *mut *mut c_char) -> *mut c_char;
        fn get_cache_packet(key: u64, cache_path: *const c_char, errmsg: *mut *mut c_char) -> *mut u8;
        fn put_cache_packet(data: *const u8, schema: *const crate::cschema::CSchema, key: u64, cache_path: *const c_char, errmsg: *mut *mut c_char) -> *mut c_char;
        fn make_cache_filename(hash: u64, cache_path: *const c_char, errmsg: *mut *mut c_char) -> *mut c_char;
        fn make_cache_filename_ext(hash: u64, cache_path: *const c_char, ext: *const c_char, errmsg: *mut *mut c_char) -> *mut c_char;
        fn make_morloc_remote_call_packet(midx: u32, arg_packets: *const *const u8, nargs: usize, errmsg: *mut *mut c_char) -> *mut u8;
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut c_char) -> usize;
        fn read_binary_file(filename: *const c_char, file_size: *mut usize, errmsg: *mut *mut c_char) -> *mut u8;
        fn write_atomic(filename: *const c_char, data: *const u8, size: usize, errmsg: *mut *mut c_char) -> i32;
        fn get_morloc_data_packet_error_message(data: *const u8, errmsg: *mut *mut c_char) -> *mut c_char;
    }

    let seed = midx as u64;
    let mut err: *mut c_char = ptr::null_mut();

    // Cleanup tracking
    let mut return_packet: *mut u8 = ptr::null_mut();
    let mut arg_hashes: Vec<u64> = vec![0; nargs];
    let mut arg_voidstars: Vec<*mut u8> = vec![ptr::null_mut(); nargs];
    let mut arg_schemas: Vec<*mut crate::cschema::CSchema> = vec![ptr::null_mut(); nargs];
    let mut cached_arg_filenames: Vec<*mut c_char> = vec![ptr::null_mut(); nargs];
    let mut cached_arg_packets: Vec<*mut u8> = vec![ptr::null_mut(); nargs];

    let mut function_hash = mix(seed, DEFAULT_XXHASH_SEED);

    // Hash each argument
    for i in 0..nargs {
        let schema_str = read_schema_from_packet_meta(*arg_packets.add(i), &mut err);
        if schema_str.is_null() || !err.is_null() { goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet); }

        arg_schemas[i] = parse_schema(schema_str, &mut err);
        if !err.is_null() { goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet); }

        arg_voidstars[i] = get_morloc_data_packet_value(*arg_packets.add(i), arg_schemas[i], &mut err);
        if !err.is_null() { goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet); }

        let mut h: u64 = 0;
        hash_voidstar(arg_voidstars[i], arg_schemas[i], DEFAULT_XXHASH_SEED, &mut h, &mut err);
        if !err.is_null() { goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet); }
        arg_hashes[i] = h;

        function_hash = mix(function_hash, h);
    }

    mkdir_p(cache_path, &mut err);
    if !err.is_null() { goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet); }

    // Check if result is cached
    let mut result_cache_filename = check_cache_packet(function_hash, cache_path, &mut err);
    if !err.is_null() { libc::free(err as *mut c_void); err = ptr::null_mut(); }

    if !result_cache_filename.is_null() {
        return_packet = get_cache_packet(function_hash, cache_path, &mut err);
        if !err.is_null() { goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet); }
    } else {
        result_cache_filename = make_cache_filename(function_hash, cache_path, &mut err);
        if !err.is_null() { goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet); }

        // Cache arguments
        for i in 0..nargs {
            cached_arg_filenames[i] = check_cache_packet(arg_hashes[i], cache_path, &mut err);
            if cached_arg_filenames[i].is_null() {
                if !err.is_null() { libc::free(err as *mut c_void); err = ptr::null_mut(); }
                cached_arg_filenames[i] = put_cache_packet(arg_voidstars[i], arg_schemas[i], arg_hashes[i], cache_path, &mut err);
                if !err.is_null() { goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet); }
            }
        }

        // Read cached arg packets
        for i in 0..nargs {
            let mut file_size: usize = 0;
            cached_arg_packets[i] = read_binary_file(cached_arg_filenames[i], &mut file_size, &mut err);
            if !err.is_null() { goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet); }
        }

        // Build call packet
        let cached_ptrs: Vec<*const u8> = cached_arg_packets.iter().map(|p| *p as *const u8).collect();
        let call_packet = make_morloc_remote_call_packet(midx as u32, cached_ptrs.as_ptr(), nargs, &mut err);
        if !err.is_null() {
            libc::free(call_packet as *mut c_void);
            goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet);
        }

        let call_packet_size = morloc_packet_size(call_packet, &mut err);

        // Hash call packet with xxhash
        let call_packet_hash = crate::hash::xxh64_with_seed(std::slice::from_raw_parts(call_packet, call_packet_size), DEFAULT_XXHASH_SEED);

        let call_ext = CString::new("-call.dat").unwrap();
        let call_packet_filename = make_cache_filename_ext(call_packet_hash, cache_path, call_ext.as_ptr(), &mut err);

        // Write call packet to disk
        write_atomic(call_packet_filename, call_packet, call_packet_size, &mut err);
        libc::free(call_packet as *mut c_void);

        let out_ext = CString::new(".out").unwrap();
        let err_ext = CString::new(".err").unwrap();
        let output_filename = make_cache_filename_ext(function_hash, cache_path, out_ext.as_ptr(), &mut err);
        let error_filename = make_cache_filename_ext(function_hash, cache_path, err_ext.as_ptr(), &mut err);

        // Submit SLURM job
        let nexus_c = CString::new("./nexus").unwrap();
        let pid = submit_morloc_slurm_job(
            nexus_c.as_ptr(), socket_basename, call_packet_filename,
            result_cache_filename, output_filename, error_filename,
            resources, &mut err,
        );

        libc::free(call_packet_filename as *mut c_void);
        libc::free(output_filename as *mut c_void);
        libc::free(error_filename as *mut c_void);

        if !err.is_null() { goto_cleanup!(errmsg, err, arg_schemas, cached_arg_filenames, cached_arg_packets, return_packet); }

        // Wait for job completion
        while !slurm_job_is_complete(pid) {
            libc::sleep(1);
        }

        let mut return_packet_size: usize = 0;
        return_packet = read_binary_file(result_cache_filename, &mut return_packet_size, &mut err);

        let failure = get_morloc_data_packet_error_message(return_packet, &mut err);
        if !failure.is_null() {
            libc::fprintf(
                libc::fdopen(libc::STDERR_FILENO, b"w\0".as_ptr() as *const c_char),
                b"Failed, deleting result %s\n\0".as_ptr() as *const c_char,
                result_cache_filename,
            );
            libc::unlink(result_cache_filename);
            libc::free(failure as *mut c_void);
        }
    }

    // Cleanup
    for i in 0..nargs {
        if !arg_schemas[i].is_null() { free_schema(arg_schemas[i]); }
        if !cached_arg_filenames[i].is_null() { libc::free(cached_arg_filenames[i] as *mut c_void); }
        if !cached_arg_packets[i].is_null() { libc::free(cached_arg_packets[i] as *mut c_void); }
    }
    if !result_cache_filename.is_null() { libc::free(result_cache_filename as *mut c_void); }

    return_packet
}

// Cleanup macro for goto-like pattern
macro_rules! goto_cleanup {
    ($errmsg:expr, $err:expr, $schemas:expr, $filenames:expr, $packets:expr, $return_packet:expr) => {{
        *$errmsg = $err;
        for i in 0..$schemas.len() {
            if !$schemas[i].is_null() {
                extern "C" { fn free_schema(s: *mut crate::cschema::CSchema); }
                free_schema($schemas[i]);
            }
            if !$filenames[i].is_null() { libc::free($filenames[i] as *mut c_void); }
            if !$packets[i].is_null() { libc::free($packets[i] as *mut c_void); }
        }
        return $return_packet;
    }};
}
use goto_cleanup;
