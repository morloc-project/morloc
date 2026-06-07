//! Client side of the morloc-manager SLURM bridge.
//!
//! When a labeled `remote_call` happens inside a containerized morloc
//! program, the container has no `sbatch`/`sacct` on its PATH. The
//! container instead talks to a Unix domain socket bind-mounted into
//! it by morloc-manager (path in env `MORLOC_BRIDGE_SOCKET`), and
//! morloc-manager on the host translates each RPC into a real
//! `sbatch --parsable ...` / `sacct -j ID ...` invocation.
//!
//! See `morloc-manager/src/bridge.rs` for the canonical schema spec
//! (request / reply shapes, state enum, error format, examples). The
//! types in this module mirror that spec; if the two diverge, the
//! server is authoritative.
//!
//! Connection lifecycle: connect, write one JSON object terminated by
//! `\n`, read one JSON object terminated by `\n`, close. No keepalive,
//! no batching.

use std::io::{BufRead, BufReader, Write};
use std::os::unix::net::UnixStream;
use std::time::Duration;

use serde::{Deserialize, Serialize};

/// Reply states the bridge may report. Anything that isn't terminal
/// keeps the caller polling; the four terminal states are recognized
/// by `is_terminal`.
#[derive(Debug)]
pub enum JobState {
    Pending,
    Running,
    Completed,
    Failed,
    Cancelled,
    Unknown(String),
}

impl JobState {
    fn parse(s: &str) -> Self {
        let t = s.trim();
        match t {
            "PENDING" => JobState::Pending,
            "RUNNING" => JobState::Running,
            "COMPLETED" => JobState::Completed,
            "FAILED" => JobState::Failed,
            "CANCELLED" => JobState::Cancelled,
            other => JobState::Unknown(other.to_string()),
        }
    }

    /// True if no further progress is possible. Polling stops here.
    /// Bare-metal `slurm_job_is_complete` treats FAILED/CANCELLED the
    /// same as COMPLETED -- the result file (or its absence) is what
    /// the caller checks next.
    pub fn is_terminal(&self) -> bool {
        matches!(self, JobState::Completed | JobState::Failed | JobState::Cancelled)
    }
}

/// SLURM resource request packed into the submit RPC.
#[derive(Debug, Serialize)]
pub struct ResourceSpec {
    pub mem_gb: i32,
    pub time_s: i32,
    pub cpus: i32,
    pub gpus: i32,
}

#[derive(Serialize)]
struct SubmitReq<'a> {
    op: &'a str,
    /// Inner argv: the morloc-manager-/nexus-style command to run on
    /// the compute node, one element per shell token. The bridge
    /// prepends `["<morloc-manager>", "run", "--slurm-bridge", "--"]`
    /// before composing the sbatch wrap. Sending an argv list rather
    /// than a pre-escaped string keeps quoting out of the wire format
    /// and lets the bridge echo the exact attempted argv in error
    /// replies.
    inner_argv: &'a [String],
    stdout: &'a str,
    stderr: &'a str,
    resources: ResourceSpec,
}

#[derive(Serialize)]
struct StatusReq<'a> {
    op: &'a str,
    job_id: u32,
}

#[derive(Deserialize)]
struct SubmitReply {
    #[serde(default)]
    job_id: Option<u32>,
    #[serde(default)]
    error: Option<String>,
}

#[derive(Deserialize)]
struct StatusReply {
    #[serde(default)]
    state: Option<String>,
    #[serde(default)]
    error: Option<String>,
}

/// Open the bridge socket, send one JSON line, read one JSON line back.
/// The bridge is single-shot per connection; the manager spawns a fresh
/// handler thread for each connect.
fn rpc(sock_path: &str, req_json: &str) -> Result<String, String> {
    let mut stream = UnixStream::connect(sock_path)
        .map_err(|e| format!("connect {}: {}", sock_path, e))?;
    // Generous timeouts: sbatch may take a couple seconds under load,
    // and a stuck bridge should fail loudly rather than hang the pool.
    let _ = stream.set_read_timeout(Some(Duration::from_secs(60)));
    let _ = stream.set_write_timeout(Some(Duration::from_secs(10)));

    stream
        .write_all(req_json.as_bytes())
        .map_err(|e| format!("write: {}", e))?;
    if !req_json.ends_with('\n') {
        stream.write_all(b"\n").map_err(|e| format!("write: {}", e))?;
    }
    stream.flush().ok();

    let mut reader = BufReader::new(stream);
    let mut line = String::new();
    let n = reader
        .read_line(&mut line)
        .map_err(|e| format!("read: {}", e))?;
    if n == 0 {
        return Err("bridge closed connection without reply".into());
    }
    Ok(line)
}

/// Submit a job via the bridge. Returns the slurm job id assigned by
/// the host's sbatch.
pub fn submit(
    sock_path: &str,
    inner_argv: &[String],
    stdout: &str,
    stderr: &str,
    resources: ResourceSpec,
) -> Result<u32, String> {
    let req = SubmitReq {
        op: "submit",
        inner_argv,
        stdout,
        stderr,
        resources,
    };
    let req_json = serde_json::to_string(&req)
        .map_err(|e| format!("encode submit: {}", e))?;
    let reply_line = rpc(sock_path, &req_json)?;
    let reply: SubmitReply = serde_json::from_str(reply_line.trim())
        .map_err(|e| format!("decode submit reply '{}': {}", reply_line.trim(), e))?;
    if let Some(msg) = reply.error {
        return Err(msg);
    }
    reply.job_id.ok_or_else(|| "submit reply missing job_id".into())
}

/// Ask the bridge what state a previously-submitted job is in.
pub fn status(sock_path: &str, job_id: u32) -> Result<JobState, String> {
    let req = StatusReq { op: "status", job_id };
    let req_json = serde_json::to_string(&req)
        .map_err(|e| format!("encode status: {}", e))?;
    let reply_line = rpc(sock_path, &req_json)?;
    let reply: StatusReply = serde_json::from_str(reply_line.trim())
        .map_err(|e| format!("decode status reply '{}': {}", reply_line.trim(), e))?;
    if let Some(msg) = reply.error {
        return Err(msg);
    }
    let state = reply
        .state
        .ok_or_else(|| String::from("status reply missing 'state'"))?;
    Ok(JobState::parse(&state))
}

/// Return Some(path) if a bridge socket is configured for this process,
/// None otherwise. The runtime branches submit/status on this.
pub fn socket_from_env() -> Option<String> {
    match std::env::var("MORLOC_BRIDGE_SOCKET") {
        Ok(p) if !p.is_empty() => Some(p),
        _ => None,
    }
}
