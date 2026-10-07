use std::io::{Read, Write};
use std::os::fd::{AsRawFd, FromRawFd};
use std::os::unix::net::UnixStream;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};
use std::time::Duration;

use morloc_runtime_types::panic::{catch, install_hook, PANIC_EXIT_STATUS};

const PEER_FD: i32 = 3;
const CHILD_ENV: &str = "MORLOC_PANIC_TEST_CHILD";
const CHILD_OK: i32 = 42;

fn is_child() -> bool {
    std::env::var_os(CHILD_ENV).is_some()
}

fn child_ok() -> ! {
    unsafe { libc::_exit(CHILD_OK) }
}

fn run_child(name: &str) -> (std::process::Child, UnixStream) {
    let (ours, theirs) = UnixStream::pair().unwrap();
    let fd = theirs.as_raw_fd();
    let mut cmd = Command::new(std::env::current_exe().unwrap());
    cmd.args([&format!("panic_tests::{name}"), "--exact", "--ignored", "--nocapture", "--test-threads=1"])
        .env(CHILD_ENV, "1")
        .stdout(Stdio::null())
        .stderr(Stdio::null());
    unsafe {
        cmd.pre_exec(move || {
            if libc::dup2(fd, PEER_FD) < 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
    }
    let child = cmd.spawn().unwrap();
    drop(theirs);
    (child, ours)
}

fn peer() -> UnixStream {
    unsafe { UnixStream::from_raw_fd(PEER_FD) }
}

fn exit_code(mut child: std::process::Child) -> Option<i32> {
    child.wait().unwrap().code()
}

fn read_to_end(s: &mut impl Read) -> Vec<u8> {
    let mut out = Vec::new();
    let _ = s.read_to_end(&mut out);
    out
}

#[test]
fn a_panic_outside_a_catch_scope_exits_with_the_internal_error_status() {
    let (child, _peer) = run_child("child_panics_outside_a_scope");
    assert_eq!(exit_code(child), Some(PANIC_EXIT_STATUS));
}

#[test]
#[ignore]
fn child_panics_outside_a_scope() {
    if !is_child() {
        return;
    }
    install_hook(crate::process::panic_exit);
    panic!("outside");
}

#[test]
fn a_panic_inside_a_catch_scope_reaches_the_catch() {
    let (child, mut peer) = run_child("child_panics_inside_a_scope");
    assert_eq!(read_to_end(&mut peer), b"caught");
    assert_eq!(exit_code(child), Some(CHILD_OK));
}

#[test]
#[ignore]
fn child_panics_inside_a_scope() {
    if !is_child() {
        return;
    }
    install_hook(crate::process::panic_exit);
    if catch(|| panic!("inside")).is_err() {
        peer().write_all(b"caught").unwrap();
    }
    child_ok();
}

#[test]
fn a_panic_while_unwinding_exits_with_the_internal_error_status() {
    let (child, _peer) = run_child("child_panics_while_unwinding");
    assert_eq!(exit_code(child), Some(PANIC_EXIT_STATUS));
}

#[test]
#[ignore]
fn child_panics_while_unwinding() {
    if !is_child() {
        return;
    }
    struct PanicsOnDrop;
    impl Drop for PanicsOnDrop {
        fn drop(&mut self) {
            panic!("second");
        }
    }
    install_hook(crate::process::panic_exit);
    let _ = catch(|| {
        let _d = PanicsOnDrop;
        panic!("first");
    });
}

fn http(port: u16, path: &str) -> std::net::TcpStream {
    let mut s = std::net::TcpStream::connect(("127.0.0.1", port)).unwrap();
    write!(s, "GET {path} HTTP/1.1\r\nConnection: close\r\n\r\n").unwrap();
    s
}

fn status_line(s: &mut std::net::TcpStream) -> String {
    let text = String::from_utf8_lossy(&read_to_end(s)).into_owned();
    text.lines().next().unwrap_or("").to_string()
}

#[test]
fn an_http_request_that_panics_is_answered_500_and_the_server_exits_with_the_internal_error_status() {
    let (child, mut peer) = run_child("child_serves_http");
    let mut port = [0u8; 2];
    peer.read_exact(&mut port).unwrap();
    let port = u16::from_le_bytes(port);

    let mut slow = http(port, "/slow");
    std::thread::sleep(Duration::from_millis(300));
    let mut bad = http(port, "/panic");
    assert!(status_line(&mut bad).starts_with("HTTP/1.1 500"));
    let mut late = http(port, "/fast");
    assert!(status_line(&mut late).starts_with("HTTP/1.1 503"));
    assert!(status_line(&mut slow).starts_with("HTTP/1.1 200"));
    assert_eq!(exit_code(child), Some(PANIC_EXIT_STATUS));
}

#[test]
#[ignore]
fn child_serves_http() {
    if !is_child() {
        return;
    }
    install_hook(crate::process::panic_exit);
    let listener = std::net::TcpListener::bind(("127.0.0.1", 0)).unwrap();
    let port = listener.local_addr().unwrap().port();
    peer().write_all(&port.to_le_bytes()).unwrap();
    for stream in listener.incoming() {
        let stream = stream.unwrap();
        std::thread::spawn(move || {
            crate::mcp::serve_conn(stream, |req, keep_alive| {
                match req.path.as_str() {
                    "/panic" => panic!("handler"),
                    "/slow" => std::thread::sleep(Duration::from_secs(1)),
                    _ => {}
                }
                crate::mcp::http_json(200, b"{}", keep_alive)
            })
        });
    }
}

#[test]
fn a_jsonrpc_call_that_panics_is_answered_with_an_internal_error_and_the_process_exits() {
    let (child, mut peer) = run_child("child_answers_jsonrpc");
    let reply: serde_json::Value = serde_json::from_slice(&read_to_end(&mut peer)).unwrap();
    assert_eq!(reply["id"], 7);
    assert_eq!(reply["error"]["code"], -32603);
    assert_eq!(exit_code(child), Some(PANIC_EXIT_STATUS));
}

#[test]
#[ignore]
fn child_answers_jsonrpc() {
    if !is_child() {
        return;
    }
    install_hook(crate::process::panic_exit);
    let id = serde_json::json!(7);
    crate::mcp::answer_message(PEER_FD, Some(&id), || panic!("handler"));
}

#[test]
fn a_stdio_request_that_panics_is_answered_as_failed_and_the_process_exits() {
    let (child, mut peer) = run_child("child_serves_stdio_op");
    let reply = read_to_end(&mut peer);
    assert_eq!(reply.first(), Some(&morloc_runtime_types::stdio_proto::STATUS_ERR));
    assert_eq!(exit_code(child), Some(PANIC_EXIT_STATUS));
}

#[test]
#[ignore]
fn child_serves_stdio_op() {
    if !is_child() {
        return;
    }
    install_hook(crate::process::panic_exit);
    let mut stream = peer();
    let _ = crate::stdio_server::serve_op(&mut stream, |_| panic!("handler"));
}

#[test]
fn a_stdio_request_that_panics_in_the_daemon_fails_the_daemon_instead_of_exiting() {
    let (child, mut peer) = run_child("child_serves_stdio_op_in_the_daemon");
    let reply = read_to_end(&mut peer);
    assert_eq!(reply.first(), Some(&morloc_runtime_types::stdio_proto::STATUS_ERR));
    assert_eq!(exit_code(child), Some(CHILD_OK));
}

#[test]
#[ignore]
fn child_serves_stdio_op_in_the_daemon() {
    if !is_child() {
        return;
    }
    extern "C" {
        fn morloc_daemon_worker_panicked() -> bool;
    }
    install_hook(crate::process::panic_exit);
    crate::stdio_server::set_daemon_mode(true);
    let mut stream = peer();
    std::thread::spawn(move || {
        let _ = crate::stdio_server::serve_op(&mut stream, |_| panic!("handler"));
    });
    let deadline = std::time::Instant::now() + Duration::from_secs(10);
    while !unsafe { morloc_daemon_worker_panicked() } {
        assert!(std::time::Instant::now() < deadline);
        std::thread::sleep(Duration::from_millis(10));
    }
    child_ok();
}

#[test]
fn a_panic_in_a_forked_child_leaves_the_parents_run_alone() {
    let (child, mut peer) = run_child("child_forks_and_panics_in_the_grandchild");
    assert_eq!(read_to_end(&mut peer), b"intact");
    assert_eq!(exit_code(child), Some(CHILD_OK));
}

#[test]
#[ignore]
fn child_forks_and_panics_in_the_grandchild() {
    if !is_child() {
        return;
    }
    crate::process::record_nexus_process();
    let dir = std::env::temp_dir().join(format!("morloc-panic-fork-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    crate::sigrm::register(dir.to_str().unwrap()).unwrap();
    let pid = unsafe { libc::fork() };
    if pid == 0 {
        crate::process::panic_exit();
    }
    let mut status = 0;
    unsafe { libc::waitpid(pid, &mut status, 0) };
    if libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == PANIC_EXIT_STATUS && dir.exists() {
        peer().write_all(b"intact").unwrap();
    }
    let _ = std::fs::remove_dir_all(&dir);
    child_ok();
}
