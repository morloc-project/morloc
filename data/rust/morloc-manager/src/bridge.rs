//! SLURM submission bridge for containerized morloc runs.
//!
//! `morloc-manager run --slurm-bridge` spawns this server on a Unix
//! domain socket on the host, bind-mounts the socket into the
//! container at `/run/morloc-bridge.sock`, and sets
//! `MORLOC_BRIDGE_SOCKET=/run/morloc-bridge.sock` inside. The
//! container's libmorloc.so (see `morloc-runtime/src/slurm_bridge.rs`
//! for the matching client) talks one JSON object per connection.
//!
//! # Bridge JSON RPC v1 -- canonical spec
//!
//! This is the source of truth for the wire contract; the client crate
//! mirrors it.
//!
//! ## Transport
//!
//! * Unix domain socket, owner-rw (0600).
//! * Path inside the driver container: `/run/morloc-bridge.sock`.
//! * Client locates it via env var `MORLOC_BRIDGE_SOCKET`. Absent or
//!   empty means "no bridge"; libmorloc falls back to the direct
//!   sbatch path.
//! * Connection-per-request: client connects, writes one request,
//!   reads one reply, closes.
//! * All payloads are UTF-8 JSON; the request and the reply are each
//!   terminated by a single `\n`.
//!
//! ## Request envelope
//!
//! Discriminated by the `op` string. Two ops are defined:
//!
//! * `"submit"` - submit a SLURM job, get back the job id.
//! * `"status"` - look up the state of a previously-submitted job.
//!
//! Unknown ops produce an `{"error": "..."}` reply.
//!
//! ## submit
//!
//! Request:
//!
//! ```json
//! {
//!   "op": "submit",
//!   "inner_argv": ["<nexus>", "--call-packet", "<pkt>",
//!                   "--socket-base", "<base>",
//!                   "--output-file", "<res>",
//!                   "--output-form", "packet"],
//!   "stdout":   "<string>",
//!   "stderr":   "<string>",
//!   "resources": {
//!     "mem_gb": <integer>,
//!     "time_s": <integer>,
//!     "cpus":   <integer>,
//!     "gpus":   <integer>
//!   }
//! }
//! ```
//!
//! Fields:
//!
//! * `inner_argv` - the logical command to run on the compute node,
//!   expressed as an argv list (one shell token per element). The
//!   bridge prepends `["<morloc-manager>", "run", "--slurm-bridge",
//!   "--"]` then shell-escapes the whole vector into a single string
//!   for sbatch --wrap. /bin/sh on the compute node parses the
//!   quoted tokens back into argv, morloc-manager hands everything
//!   after `--` to the in-container nexus. The runtime never
//!   shell-quotes anything; quoting is a bridge-side concern. The
//!   `--slurm-bridge` flag in the wrap is what enables nested
//!   remote dispatch from compute-node pools.
//! * `stdout`, `stderr` - absolute paths the bridge passes to sbatch
//!   as `-o <stdout> -e <stderr>`.
//! * `resources.mem_gb` - requested memory in GiB. Becomes
//!   `--mem=<N>G`.
//! * `resources.time_s` - wall time in seconds. Bridge formats as
//!   `D-HH:MM:SS` for `--time=`.
//! * `resources.cpus` - becomes `--cpus-per-task=<N>`. Floor of 1
//!   (a request for 0 cpus is reinterpreted as 1).
//! * `resources.gpus` - becomes `--gres=gpu:<N>` when `N > 0`. Zero
//!   omits the flag entirely; many SLURM installs reject
//!   `--gres=gpu:0` as an unknown resource when no gpu gres is
//!   defined on the cluster.
//!
//! Reply (success):
//!
//! ```json
//! { "job_id": <uint32> }
//! ```
//!
//! The job id comes from `sbatch --parsable` stdout.
//!
//! Reply (failure):
//!
//! ```json
//! { "error": "<string>" }
//! ```
//!
//! The string is intended for direct display to the user via the
//! morloc runtime's error path. Examples: `"sbatch exit 1: Invalid
//! partition"`, `"spawn sbatch: No such file or directory"`.
//!
//! Either `job_id` or `error` is present in a reply; never both,
//! never neither.
//!
//! ## status
//!
//! Request:
//!
//! ```json
//! { "op": "status", "job_id": <uint32> }
//! ```
//!
//! Reply (success):
//!
//! ```json
//! { "state": "<state>" }
//! ```
//!
//! Where `state` is exactly one of these five canonical strings:
//!
//! | state       | semantics                                          |
//! |-------------|----------------------------------------------------|
//! | `PENDING`   | submitted, not yet started                         |
//! | `RUNNING`   | executing on a compute node                        |
//! | `COMPLETED` | exited 0                                           |
//! | `FAILED`    | exited non-zero / TIMEOUT / NODE_FAIL / OOM        |
//! | `CANCELLED` | scancel'd or killed by an admin                    |
//!
//! Other sacct states (e.g. `SUSPENDED`, `REQUEUED`) are normalized
//! to `RUNNING` so the client keeps polling. The runtime treats
//! `COMPLETED`, `FAILED`, and `CANCELLED` identically as "terminal";
//! the result file (or its absence) is the next signal.
//!
//! Reply (failure):
//!
//! ```json
//! { "error": "<string>" }
//! ```
//!
//! Used when `sacct` itself fails (binary not found, exit != 0). The
//! client treats any error reply as "keep polling"; the submit path
//! is the place where bridge problems surface to the user.
//!
//! ## Versioning
//!
//! No schema version field. The client and server ship in the same
//! morloc release; cross-version compatibility is enforced upstream
//! by the manifest's `morloc_version` check.
//!
//! ## End-to-end example
//!
//! ```text
//! C -> {"op":"submit","inner_argv":["/opt/morloc/bin/nexus","--call-packet","/h/u/p/.morloc-cache/a8d-call.dat","--socket-base","py-pipe","--output-file","/h/u/p/.morloc-cache/a8d.dat","--output-form","packet"],"stdout":"/h/u/p/.morloc-cache/a8d.out","stderr":"/h/u/p/.morloc-cache/a8d.err","resources":{"mem_gb":32,"time_s":3600,"cpus":8,"gpus":0}}
//! S <- {"job_id":12345678}
//! ```
//!
//! The server then runs:
//!
//! ```sh
//! sbatch --parsable \
//!   -o /h/u/p/.morloc-cache/a8d.out \
//!   -e /h/u/p/.morloc-cache/a8d.err \
//!   --mem=32G --time=0-01:00:00 --cpus-per-task=8 \
//!   --wrap="'/home/u/.local/bin/morloc-manager' 'run' '--slurm-bridge' '--' '/opt/morloc/bin/nexus' '--call-packet' '/h/u/p/.morloc-cache/a8d-call.dat' '--socket-base' 'py-pipe' '--output-file' '/h/u/p/.morloc-cache/a8d.dat' '--output-form' 'packet'"
//! ```
//!
//! Status polling:
//!
//! ```text
//! C -> {"op":"status","job_id":12345678}
//! S <- {"state":"RUNNING"}
//!
//! ...   (client sleeps, polls again)
//!
//! C -> {"op":"status","job_id":12345678}
//! S <- {"state":"COMPLETED"}
//! ```
//!
//! The driver pool's `remote_call` then reads
//! `/h/u/p/.morloc-cache/a8d.dat` directly from the shared FS. The
//! bridge never touches the result data.

use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread::{self, JoinHandle};
use std::time::Duration;

use serde::{Deserialize, Serialize};

/// Static configuration the bridge needs to compose a SLURM wrap.
///
/// The remote container is brought up by `morloc-manager run` on the
/// compute node -- not by the bridge directly calling `apptainer exec`.
/// That keeps every nexus invocation (driver or compute-node) symmetric
/// with respect to env lookup, SIF resolution, $HOME mounts, and
/// MORLOC_HOME setup: morloc-manager owns all of that, and the bridge
/// just shells out to it. The only required information is the
/// absolute path to the morloc-manager binary (which must be reachable
/// at the same path on every compute node, typically via NFS-shared
/// $HOME).
#[derive(Debug, Clone)]
pub struct BridgeConfig {
    pub morloc_manager_exe: PathBuf,
}

/// Owns the listener thread + socket path. Drop tears the bridge down
/// cleanly so subsequent `morloc-manager run` invocations don't trip
/// over stale sockets.
pub struct BridgeHandle {
    sock_path: PathBuf,
    shutdown: Arc<AtomicBool>,
    listener_thread: Option<JoinHandle<()>>,
}

impl BridgeHandle {
    pub fn sock_path(&self) -> &Path {
        &self.sock_path
    }

    fn shutdown_inner(&mut self) {
        if self.shutdown.swap(true, Ordering::SeqCst) {
            return;
        }
        // Nudge the listener loop out of its accept poll by
        // self-connecting; the nonblocking listener will see the
        // shutdown flag on its next iteration.
        let _ = UnixStream::connect(&self.sock_path);
        if let Some(h) = self.listener_thread.take() {
            let _ = h.join();
        }
        let _ = std::fs::remove_file(&self.sock_path);
    }
}

impl Drop for BridgeHandle {
    fn drop(&mut self) {
        self.shutdown_inner();
    }
}

/// Start a bridge listener on `sock_path`. The socket is created with
/// 0600 perms (Unix sockets inherit umask; we tighten explicitly to
/// keep submission private to the calling user). Returns once the
/// listener is bound and ready to accept; the listener runs on a
/// background thread until the returned handle is dropped.
pub fn spawn_bridge(sock_path: &Path, cfg: BridgeConfig) -> Result<BridgeHandle, String> {
    // Best-effort cleanup of a stale socket from a crashed prior run.
    let _ = std::fs::remove_file(sock_path);

    let listener = UnixListener::bind(sock_path)
        .map_err(|e| format!("bind {}: {}", sock_path.display(), e))?;
    listener
        .set_nonblocking(true)
        .map_err(|e| format!("nonblocking: {}", e))?;

    // Restrict the socket to owner-rw so other users on the host
    // can't submit slurm jobs through our bridge.
    use std::os::unix::fs::PermissionsExt;
    let _ = std::fs::set_permissions(sock_path, std::fs::Permissions::from_mode(0o600));

    let shutdown = Arc::new(AtomicBool::new(false));
    let shutdown_t = shutdown.clone();
    let sock_owned = sock_path.to_path_buf();
    let cfg_owned = cfg;

    let listener_thread = thread::spawn(move || {
        run_listener(listener, shutdown_t, cfg_owned);
    });

    Ok(BridgeHandle {
        sock_path: sock_owned,
        shutdown,
        listener_thread: Some(listener_thread),
    })
}

fn run_listener(listener: UnixListener, shutdown: Arc<AtomicBool>, cfg: BridgeConfig) {
    while !shutdown.load(Ordering::SeqCst) {
        match listener.accept() {
            Ok((stream, _addr)) => {
                let cfg_h = cfg.clone();
                thread::spawn(move || {
                    handle_conn(stream, &cfg_h);
                });
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                thread::sleep(Duration::from_millis(50));
            }
            Err(e) => {
                eprintln!("[morloc-manager bridge] accept failed: {}", e);
                thread::sleep(Duration::from_millis(200));
            }
        }
    }
}

// ── Wire protocol ────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
#[serde(tag = "op", rename_all = "snake_case")]
enum Request {
    Submit {
        /// The inner command to run on the compute node, expressed as
        /// an argv list (one shell token per element). The bridge
        /// prepends `[<morloc-manager>, run, --slurm-bridge, --]` and
        /// shell-escapes once when composing the sbatch --wrap. The
        /// runtime stays out of shell-quoting entirely.
        inner_argv: Vec<String>,
        stdout: String,
        stderr: String,
        resources: ResourceSpec,
    },
    Status {
        job_id: u32,
    },
}

#[derive(Debug, Deserialize)]
struct ResourceSpec {
    mem_gb: i32,
    time_s: i32,
    cpus: i32,
    gpus: i32,
}

#[derive(Serialize)]
struct SubmitReply {
    #[serde(skip_serializing_if = "Option::is_none")]
    job_id: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

#[derive(Serialize)]
struct StatusReply {
    #[serde(skip_serializing_if = "Option::is_none")]
    state: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
}

fn handle_conn(stream: UnixStream, cfg: &BridgeConfig) {
    // Give each request a generous read window; submission can hang
    // briefly on a busy slurm controller.
    let _ = stream.set_read_timeout(Some(Duration::from_secs(120)));
    let _ = stream.set_write_timeout(Some(Duration::from_secs(30)));

    let reader = BufReader::new(stream.try_clone().expect("clone stream"));
    let mut writer = stream;

    let mut line = String::new();
    let mut lr = reader;
    if let Err(e) = lr.read_line(&mut line) {
        let _ = writeln!(writer, "{}", err_json(&format!("read: {}", e)));
        return;
    }
    if line.trim().is_empty() {
        return;
    }

    let reply = match serde_json::from_str::<Request>(line.trim()) {
        Ok(Request::Submit { inner_argv, stdout, stderr, resources }) => {
            handle_submit(cfg, &inner_argv, &stdout, &stderr, &resources)
        }
        Ok(Request::Status { job_id }) => handle_status(job_id),
        Err(e) => err_json(&format!("decode request '{}': {}", line.trim(), e)),
    };

    let _ = writeln!(writer, "{}", reply);
    let _ = writer.flush();
}

fn err_json(msg: &str) -> String {
    // Manual escape to avoid pulling json_macros for a single field.
    let escaped = msg.replace('\\', "\\\\").replace('"', "\\\"");
    format!("{{\"error\":\"{}\"}}", escaped)
}

// ── Submit ───────────────────────────────────────────────────────────

fn handle_submit(
    cfg: &BridgeConfig,
    inner_argv: &[String],
    stdout: &str,
    stderr: &str,
    res: &ResourceSpec,
) -> String {
    // Wrap the logical nexus invocation in `morloc-manager run
    // --slurm-bridge -- ...`. Each compute node thereby brings up the
    // active env's container (same .sif, same $HOME mount, same
    // MORLOC_HOME) AND spawns its own bridge UDS so the in-container
    // nexus can dispatch nested remote calls back to slurm on the
    // worker host. Without --slurm-bridge the leaf case still works
    // but the in-container nexus has no bridge to talk to and any
    // nested labeled call fails with "MORLOC_BRIDGE_SOCKET unset".
    //
    // The full argv `[morloc-manager, run, --slurm-bridge, --,
    // <inner_argv>...]` is shell-escaped once into a single string for
    // sbatch --wrap. /bin/sh on the compute node parses it back into
    // argv, morloc-manager hands everything after `--` to the
    // in-container nexus. One quoting boundary, end to end.
    let mut argv: Vec<String> = Vec::with_capacity(inner_argv.len() + 4);
    argv.push(cfg.morloc_manager_exe.to_string_lossy().into_owned());
    argv.push("run".to_string());
    argv.push("--slurm-bridge".to_string());
    argv.push("--".to_string());
    argv.extend(inner_argv.iter().cloned());

    let inner: String = argv
        .iter()
        .map(|a| shell_arg(a))
        .collect::<Vec<_>>()
        .join(" ");

    let time_arg = format!("--time={}", format_slurm_time(res.time_s));
    let mem_arg = format!("--mem={}G", res.mem_gb);
    let cpus_arg = format!("--cpus-per-task={}", res.cpus.max(1));
    let wrap_arg = format!("--wrap={}", inner);

    let mut cmd = Command::new("sbatch");
    cmd.arg("--parsable")
        .arg("-o").arg(stdout)
        .arg("-e").arg(stderr)
        .arg(&mem_arg)
        .arg(&time_arg)
        .arg(&cpus_arg);
    // Skip --gres=gpu:0 on clusters without any gpu gres definition;
    // many SLURM installs reject "gpu:0" as an unknown resource. A
    // request with no --gres flag means "no GPU allocation".
    if res.gpus > 0 {
        cmd.arg(format!("--gres=gpu:{}", res.gpus));
    }
    cmd.arg(&wrap_arg).stdin(Stdio::null());
    let output = cmd.output();

    let output = match output {
        Ok(o) => o,
        Err(e) => return err_json(&format!("spawn sbatch: {}", e)),
    };

    if !output.status.success() {
        let stderr_msg = String::from_utf8_lossy(&output.stderr);
        return err_json(&format!(
            "sbatch exit {}: {}",
            output.status.code().unwrap_or(-1),
            stderr_msg.trim()
        ));
    }

    let stdout_str = String::from_utf8_lossy(&output.stdout);
    let job_id = stdout_str.trim().split(';').next().unwrap_or("").trim();
    match job_id.parse::<u32>() {
        Ok(id) => serde_json::to_string(&SubmitReply {
            job_id: Some(id),
            error: None,
        })
        .unwrap_or_else(|_| err_json("encode reply")),
        Err(_) => err_json(&format!("unrecognized sbatch output: {}", stdout_str.trim())),
    }
}

// ── Status ───────────────────────────────────────────────────────────

fn handle_status(job_id: u32) -> String {
    let output = Command::new("sacct")
        .arg("-j")
        .arg(job_id.to_string())
        .arg("--format=State")
        .arg("--noheader")
        .stdin(Stdio::null())
        .output();

    let output = match output {
        Ok(o) => o,
        Err(e) => return err_json(&format!("spawn sacct: {}", e)),
    };

    if !output.status.success() {
        let stderr_msg = String::from_utf8_lossy(&output.stderr);
        return err_json(&format!(
            "sacct exit {}: {}",
            output.status.code().unwrap_or(-1),
            stderr_msg.trim()
        ));
    }

    let stdout_str = String::from_utf8_lossy(&output.stdout);
    // sacct emits one row per job step; the first non-empty token
    // captures the parent state. Anything sacct doesn't recognize
    // bubbles back to the client as "UNKNOWN" via the matched-state
    // fallthrough.
    let state = stdout_str
        .lines()
        .map(|l| l.trim())
        .find(|l| !l.is_empty())
        .map(|l| l.split('+').next().unwrap_or(l).trim().to_string())
        .unwrap_or_else(|| "PENDING".to_string());

    let canonical = match state.as_str() {
        s if s.starts_with("PENDING") => "PENDING",
        s if s.starts_with("RUNNING") => "RUNNING",
        s if s.starts_with("COMPLETED") => "COMPLETED",
        s if s.starts_with("FAILED") => "FAILED",
        s if s.starts_with("CANCELLED") => "CANCELLED",
        s if s.starts_with("TIMEOUT") => "FAILED",
        s if s.starts_with("NODE_FAIL") => "FAILED",
        s if s.starts_with("OUT_OF_MEMORY") => "FAILED",
        _ => "RUNNING",
    };

    serde_json::to_string(&StatusReply {
        state: Some(canonical.into()),
        error: None,
    })
    .unwrap_or_else(|_| err_json("encode reply"))
}

// ── Helpers ──────────────────────────────────────────────────────────

/// Wrap a string as a single shell-quoted token. The protocol carries
/// arbitrary user paths (cache filenames, SIF locations) so we never
/// trust the input to be space- or quote-free.
fn shell_arg(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('\'');
    for ch in s.chars() {
        if ch == '\'' {
            out.push_str("'\\''");
        } else {
            out.push(ch);
        }
    }
    out.push('\'');
    out
}

/// Format seconds as SLURM's D-HH:MM:SS walltime. Mirrors
/// libmorloc::write_slurm_time so the bridge stays a thin shell over
/// host sbatch without re-exporting C ABI.
fn format_slurm_time(seconds: i32) -> String {
    let mut rem = seconds.max(0);
    let days = rem / (60 * 60 * 24);
    rem -= days * 60 * 60 * 24;
    let hours = rem / (60 * 60);
    rem -= hours * 60 * 60;
    let minutes = rem / 60;
    rem -= minutes * 60;
    format!("{}-{:02}:{:02}:{:02}", days, hours, minutes, rem)
}
