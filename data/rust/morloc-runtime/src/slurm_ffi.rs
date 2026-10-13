//! C ABI wrappers for SLURM job submission.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, take_reason, MorlocError};

// ── C-compatible types ───────────────────────────────────────────────────────

#[repr(C)]
pub struct Resources {
    pub memory: i32,  // in Gb
    pub time: i32,    // walltime in seconds
    pub cpus: i32,
    pub gpus: i32,
}

// ── sbatch_resource_args ─────────────────────────────────────────────────────

/// SLURM's `D-HH:MM:SS` for a walltime in seconds.
fn slurm_time(seconds: i32) -> String {
    let (d, rem) = (seconds / 86400, seconds % 86400);
    format!("{}-{:02}:{:02}:{:02}", d, rem / 3600, rem % 3600 / 60, rem % 60)
}

/// The sbatch options for a label's resources. A resource the label leaves
/// unset (-1) is not requested, so the cluster's default applies. Memory and
/// time of 0 are passed on, since SLURM reads them as all of a node's memory
/// and no limit; CPUs and GPUs of 0 are not requested. mim's bridge builds
/// the same options in `resource_args`.
pub(crate) fn sbatch_resource_args(res: &Resources) -> Vec<String> {
    let mut args = Vec::new();
    if res.memory >= 0 { args.push(format!("--mem={}G", res.memory)); }
    if res.time >= 0 { args.push(format!("--time={}", slurm_time(res.time))); }
    if res.cpus > 0 { args.push(format!("--cpus-per-task={}", res.cpus)); }
    if res.gpus > 0 { args.push(format!("--gres=gpu:{}", res.gpus)); }
    args
}

// ── slurm_job_end_state ──────────────────────────────────────────────────────

/// The state a job ended in, or `None` while it may still make progress.
pub(crate) unsafe fn slurm_job_end_state(job_id: u32) -> Option<String> {
    // Containerized path: ask the host-side bridge instead of shelling
    // out to sacct (which isn't on the container's PATH). Any transport
    // error here returns None so the caller keeps polling -- the
    // submit-time error path is the one that surfaces bridge problems
    // to the user.
    if let Some(sock) = crate::slurm_bridge::socket_from_env() {
        return match crate::slurm_bridge::status(&sock, job_id) {
            Ok(state) if state.is_terminal() => Some(state.name().to_string()),
            _ => None,
        };
    }

    let cmd = format!("sacct -j {} --format=State%30 --noheader\0", job_id);
    let sacct = morloc_runtime_types::fd::popen(cmd.as_ptr() as *const c_char, b"r\0".as_ptr() as *const c_char);
    if sacct.is_null() { return None; }

    let mut state = [0u8; 64];
    let mut end = None;
    while !libc::fgets(state.as_mut_ptr() as *mut c_char, 64, sacct).is_null() {
        let line = CStr::from_ptr(state.as_ptr() as *const c_char).to_string_lossy();
        if crate::slurm_bridge::state_is_terminal(&line) {
            end = line.split_whitespace().next().map(str::to_string);
            break;
        }
    }
    libc::pclose(sacct);
    end
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

pub(crate) unsafe fn submit_morloc_slurm_job(
    nexus_path: *const c_char,
    manifest_path: &str,
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

    // Build the inner command as an argv vector. The bridge takes this
    // as-is over JSON (no shell quoting on the wire); the direct-shell
    // path below shell-escapes it once for sbatch --wrap. Same source
    // of truth for both transports.
    let inner_argv: Vec<String> = vec![
        nexus.to_string(),
        "run".to_string(),
        "--call-packet".to_string(),
        call.to_string(),
        "--socket-base".to_string(),
        socket.to_string(),
        "--output-file".to_string(),
        result_cache.to_string(),
        "--output-form".to_string(),
        "packet".to_string(),
        manifest_path.to_string(),
    ];

    // Containerized path: send the structured argv to mim
    // via the UDS bridge. The bridge prepends `mim manager run --env
    // <name> --slurm-bridge --` and shell-escapes once for sbatch.
    if let Some(sock) = crate::slurm_bridge::socket_from_env() {
        let res_spec = crate::slurm_bridge::ResourceSpec {
            mem_gb: res.memory,
            time_s: res.time,
            cpus: res.cpus,
            gpus: res.gpus,
        };
        match crate::slurm_bridge::submit(
            &sock,
            &inner_argv,
            output.as_ref(),
            error.as_ref(),
            res_spec,
        ) {
            Ok(job_id) => return job_id,
            Err(msg) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("slurm bridge: {}", msg)));
                return 0;
            }
        }
    }

    // Direct (bare-metal) path: shell-escape the argv into the --wrap
    // string sbatch expects.
    let wrap_cmd: String = inner_argv
        .iter()
        .map(|a| shell_escape(a))
        .collect::<Vec<_>>()
        .join(" ");
    let wrap_arg = format!("--wrap={}", wrap_cmd);

    let resource_args = sbatch_resource_args(res);
    let args: Option<Vec<CString>> = ["sbatch", "--parsable", "-o", output.as_ref(), "-e", error.as_ref()]
        .into_iter()
        .chain(resource_args.iter().map(String::as_str))
        .chain(std::iter::once(wrap_arg.as_str()))
        .map(|a| CString::new(a).ok())
        .collect();
    let Some(args) = args else {
        set_errmsg(errmsg, &MorlocError::Other("sbatch argument contains a NUL byte".into()));
        return 0;
    };
    let argv: Vec<*const c_char> = args.iter().map(|a| a.as_ptr()).chain(std::iter::once(ptr::null())).collect();
    let (_env, envp) = morloc_runtime_types::spawn::current_environment();

    let mut pipefd = [0i32; 2];
    if morloc_runtime_types::fd::pipe(pipefd.as_mut_ptr()) == -1 {
        set_errmsg(errmsg, &MorlocError::Other("Failed to create pipe for sbatch".into()));
        return 0;
    }

    let started = morloc_runtime_types::spawn::Spawn::new().and_then(|mut spawn| {
        spawn.dup2(pipefd[1], libc::STDOUT_FILENO)?;
        spawn.close(pipefd[0])?;
        spawn.close(pipefd[1])?;
        spawn.run(&args[0], &argv, &envp, true)
    });
    let pid = match started {
        Ok(pid) => pid,
        Err(e) => {
            libc::close(pipefd[0]);
            libc::close(pipefd[1]);
            set_errmsg(errmsg, &MorlocError::Other(format!("cannot start sbatch: {e}")));
            return 0;
        }
    };

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

/// A variable the nexus exports to every pool it spawns.
fn nexus_env(name: &str) -> Result<String, String> {
    match std::env::var(name) {
        Ok(p) if !p.is_empty() => Ok(p),
        _ => Err(format!(
            "{name} is unset; remote dispatch requires the pool to be spawned by a morloc nexus"
        )),
    }
}

/// The start of a job's stderr file, to show with the job's failure.
fn job_stderr_head(path: &str) -> String {
    use std::io::Read;
    let mut buf = Vec::new();
    match std::fs::File::open(path) {
        Ok(f) => { let _ = f.take(4096).read_to_end(&mut buf); }
        Err(_) => return String::new(),
    }
    let text = String::from_utf8_lossy(&buf);
    let text = text.trim_end();
    if text.is_empty() { String::new() } else { format!(":\n{}", text) }
}

// ── remote_call ──────────────────────────────────────────────────────────────

pub(crate) unsafe fn remote_call(
    midx: i32,
    socket_basename: *const c_char,
    _cache_path: *const c_char,
    resources: *const Resources,
    arg_packets: *const *const u8,
    nargs: usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let args: Vec<*const u8> = (0..nargs).map(|i| *arg_packets.add(i)).collect();
    match run_remote_job(midx, socket_basename, resources, &args) {
        Ok(packet) => {
            let out = libc::malloc(packet.len().max(1)) as *mut u8;
            if out.is_null() {
                set_errmsg(errmsg, &MorlocError::Other("remote_call: malloc failed".into()));
                return ptr::null_mut();
            }
            ptr::copy_nonoverlapping(packet.as_ptr(), out, packet.len());
            out
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            ptr::null_mut()
        }
    }
}

/// A data packet holding an argument's value inline, readable by a nexus
/// on another node.
unsafe fn self_contained_arg(i: usize, packet: *const u8) -> Result<Vec<u8>, MorlocError> {
    use crate::packet_ffi::{get_morloc_data_packet_value, read_schema_from_packet_meta};
    let mut err: *mut c_char = ptr::null_mut();
    let schema_str = read_schema_from_packet_meta(packet, &mut err);
    if schema_str.is_null() || !err.is_null() { return Err(take_reason(err)); }
    let schema = crate::ffi::parse_schema(schema_str, &mut err);
    if !err.is_null() { return Err(take_reason(err)); }
    let result = (|| {
        let rs = crate::cschema::CSchema::to_rust(schema);
        for (kind, name) in [
            (crate::handle_scan::StreamFieldKind::IStream, "IStream"),
            (crate::handle_scan::StreamFieldKind::OStream, "OStream"),
        ] {
            if crate::handle_scan::schema_contains_kind(&rs, kind) {
                return Err(MorlocError::Other(format!(
                    "remote_call: argument {} contains {}, which cannot cross a nexus \
                     boundary; only IFile is supported in argument position for \
                     cross-nexus calls", i, name)));
            }
        }
        let mut err: *mut c_char = ptr::null_mut();
        let value = get_morloc_data_packet_value(packet, schema, &mut err);
        if !err.is_null() { return Err(take_reason(err)); }
        crate::cache::build_persistence_data_packet(value, schema, &mut err)
            .ok_or_else(|| take_reason(err))
    })();
    crate::ffi::free_schema(schema);
    result
}

/// A fresh directory under the shared cache base for one job's files.
unsafe fn make_job_dir() -> Result<std::path::PathBuf, MorlocError> {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let label = CString::new("_remote").unwrap();
    let mut err: *mut c_char = ptr::null_mut();
    let base = crate::cache::morloc_cache_path(label.as_ptr(), &mut err);
    if base.is_null() { return Err(take_reason(err)); }
    let base_path = std::path::PathBuf::from(CStr::from_ptr(base).to_string_lossy().into_owned());
    libc::free(base as *mut c_void);
    let mut buf = [0u8; 256];
    let host = if libc::gethostname(buf.as_mut_ptr() as *mut c_char, buf.len() - 1) == 0 {
        CStr::from_ptr(buf.as_ptr() as *const c_char).to_string_lossy().into_owned()
    } else {
        String::from("host")
    };
    loop {
        let nanos = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let name = format!("{}-{}-{}-{}", host, std::process::id(), NEXT.fetch_add(1, Ordering::Relaxed), nanos);
        let dir = base_path.join(name);
        match std::fs::create_dir(&dir) {
            Ok(()) => return Ok(dir),
            Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
            Err(e) => return Err(MorlocError::Other(format!("cannot create {}: {}", dir.display(), e))),
        }
    }
}

/// Why a remote call failed. A job that ran and failed keeps its directory,
/// so its stderr can be read.
enum JobError {
    Call(MorlocError),
    Job(MorlocError),
}

impl From<MorlocError> for JobError {
    fn from(e: MorlocError) -> Self { JobError::Call(e) }
}

/// Run one labeled call as a SLURM job and return its result packet. The
/// job's files live in a directory of their own, removed once the result
/// is read.
unsafe fn run_remote_job(
    midx: i32,
    socket_basename: *const c_char,
    resources: *const Resources,
    args: &[*const u8],
) -> Result<Vec<u8>, MorlocError> {
    let arg_bytes: Vec<Vec<u8>> = args
        .iter()
        .enumerate()
        .map(|(i, p)| self_contained_arg(i, *p))
        .collect::<Result<_, _>>()?;
    let arg_ptrs: Vec<*const u8> = arg_bytes.iter().map(|b| b.as_ptr()).collect();
    let mut err: *mut c_char = ptr::null_mut();
    let call = crate::packet_ffi::make_morloc_remote_call_packet(midx as u32, arg_ptrs.as_ptr(), args.len(), &mut err);
    if call.is_null() || !err.is_null() {
        if !call.is_null() { libc::free(call as *mut c_void); }
        return Err(take_reason(err));
    }
    let size = crate::packet_ffi::morloc_packet_size(call, &mut err);
    let call_bytes = std::slice::from_raw_parts(call, size).to_vec();
    libc::free(call as *mut c_void);
    if !err.is_null() { return Err(take_reason(err)); }

    let nexus = CString::new(nexus_env("MORLOC_NEXUS_PATH").map_err(MorlocError::Other)?)
        .map_err(|_| MorlocError::Other("MORLOC_NEXUS_PATH contains a NUL byte".into()))?;
    let manifest = nexus_env("MORLOC_MANIFEST_PATH").map_err(MorlocError::Other)?;

    let dir = make_job_dir()?;
    let result = submit_and_wait(&dir, &nexus, &manifest, socket_basename, resources, &call_bytes);
    match result {
        Ok(packet) => {
            let _ = std::fs::remove_dir_all(&dir);
            Ok(packet)
        }
        Err(JobError::Call(e)) => {
            let _ = std::fs::remove_dir_all(&dir);
            Err(e)
        }
        Err(JobError::Job(e)) => Err(e),
    }
}

unsafe fn submit_and_wait(
    dir: &std::path::Path,
    nexus: &CStr,
    manifest: &str,
    socket_basename: *const c_char,
    resources: *const Resources,
    call_bytes: &[u8],
) -> Result<Vec<u8>, JobError> {
    let c_path = |name: &str| {
        CString::new(dir.join(name).to_string_lossy().as_bytes())
            .map_err(|_| MorlocError::Other(format!("{} contains a NUL byte", dir.display())))
    };
    std::fs::write(dir.join("call.dat"), call_bytes)
        .map_err(|e| MorlocError::Other(format!("cannot write the call packet in {}: {}", dir.display(), e)))?;
    let (call_c, result_c, out_c, stderr_c) = (c_path("call.dat")?, c_path("result.dat")?, c_path("job.out")?, c_path("job.err")?);
    let mut err: *mut c_char = ptr::null_mut();
    let job = submit_morloc_slurm_job(
        nexus.as_ptr(), manifest, socket_basename, call_c.as_ptr(),
        result_c.as_ptr(), out_c.as_ptr(), stderr_c.as_ptr(), resources, &mut err,
    );
    if !err.is_null() { return Err(take_reason(err).into()); }

    let stderr = dir.join("job.err").to_string_lossy().into_owned();
    let end_state = loop {
        match slurm_job_end_state(job) {
            Some(state) => break state,
            None => { libc::sleep(1); }
        }
    };
    if end_state != "COMPLETED" {
        return Err(JobError::Job(MorlocError::Other(format!(
            "SLURM job {} ended {}; its stderr is {}{}",
            job, end_state, stderr, job_stderr_head(&stderr)))));
    }
    let packet = std::fs::read(dir.join("result.dat")).map_err(|_| JobError::Job(MorlocError::Other(format!(
        "SLURM job {} completed without writing its result; its stderr is {}", job, stderr))))?;
    if let Err(e) = crate::packet::validate_packet(&packet) {
        return Err(JobError::Job(MorlocError::Packet(format!(
            "result packet for SLURM job {} is malformed ({}); its stderr is {}", job, e, stderr))));
    }

    // A job's IStream result would name a cursor in the job's own nexus,
    // which is gone once the job ends. A packet without a readable schema
    // (a failure) is passed on for its reader to report.
    let mut err: *mut c_char = ptr::null_mut();
    let schema_str = crate::packet_ffi::read_schema_from_packet_meta(packet.as_ptr(), &mut err);
    if !schema_str.is_null() && err.is_null() {
        let schema = crate::ffi::parse_schema(schema_str, &mut err);
        if err.is_null() && !schema.is_null() {
            let rs = crate::cschema::CSchema::to_rust(schema);
            crate::ffi::free_schema(schema);
            if crate::handle_scan::schema_contains_kind(&rs, crate::handle_scan::StreamFieldKind::IStream) {
                return Err(MorlocError::Other(
                    "remote_call: result contains IStream, which cannot be returned from a \
                     cross-nexus call".into()).into());
            }
        }
    }
    if !err.is_null() { libc::free(err as *mut c_void); }
    Ok(packet)
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub unsafe extern "C" fn remote_call(midx: i32, socket_basename: *const c_char, cache_path: *const c_char, resources: *const Resources, arg_packets: *const *const u8, nargs: usize, errmsg: *mut *mut c_char) -> *mut u8 {
        super::remote_call(midx, socket_basename, cache_path, resources, arg_packets, nargs, errmsg)
    }
}

#[cfg(test)]
mod resource_tests {
    use super::*;

    #[test]
    fn an_unset_resource_is_left_to_the_cluster_default() {
        let none = Resources { memory: -1, time: -1, cpus: -1, gpus: 0 };
        assert!(sbatch_resource_args(&none).is_empty());
        let zero = Resources { memory: 0, time: 0, cpus: 0, gpus: 0 };
        assert_eq!(sbatch_resource_args(&zero), ["--mem=0G", "--time=0-00:00:00"]);
        let all = Resources { memory: 32, time: 90061, cpus: 8, gpus: 2 };
        assert_eq!(
            sbatch_resource_args(&all),
            ["--mem=32G", "--time=1-01:01:01", "--cpus-per-task=8", "--gres=gpu:2"]
        );
    }
}
