//! Output stream costs as a pool sees them: a forked child opens through the
//! test custodian, as a pool opens through the nexus. Ignored by default; run
//! in release mode:
//!
//!   cargo test --release -p morloc-runtime --lib stream_bench -- \
//!       --ignored --nocapture --test-threads=1
//!
//! Each bench prints `MLCBENCH` rows (crate::alloc_bench::mlcbench).

use crate::alloc_bench::mlcbench;
use crate::compression::CompressionLevel;
use crate::schema::parse_schema;
use crate::stream::{shared_close_handle, shared_flush_buffer, shared_open_ostream_with_schema, shared_write_subpacket};
use std::path::PathBuf;
use std::time::Instant;

const REPS: usize = 5;

/// Run `work` in a forked child and return the per-operation seconds it
/// measured, one per repetition.
fn in_child(work: fn(&std::path::Path) -> Vec<f64>) -> Vec<f64> {
    let _shm = crate::own_test_registry();
    let dir: PathBuf = std::env::temp_dir().join(format!("stream-bench-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let mut fds = [0 as libc::c_int; 2];
    assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
    let pid = unsafe { libc::fork() };
    if pid == 0 {
        let out: Vec<u8> = work(&dir).iter().flat_map(|x| x.to_le_bytes()).collect();
        unsafe {
            libc::write(fds[1], out.as_ptr() as *const libc::c_void, out.len());
            libc::_exit(0);
        }
    }
    unsafe { libc::close(fds[1]) };
    let mut out = vec![0u8; 8 * REPS];
    let got = unsafe { libc::read(fds[0], out.as_mut_ptr() as *mut libc::c_void, out.len()) };
    let mut status = 0;
    unsafe {
        libc::close(fds[0]);
        libc::waitpid(pid, &mut status, 0);
    }
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(got as usize, out.len(), "bench child failed, status {status}");
    out.chunks(8).map(|c| f64::from_le_bytes(c.try_into().unwrap())).collect()
}

fn ints(json: &str) -> crate::shm_types::AbsPtr {
    crate::json::read_json_with_schema(json, &parse_schema("ai4").unwrap()).unwrap()
}

#[test]
#[ignore]
fn stream_bench_flush() {
    let s = in_child(|dir| {
        let lvl = CompressionLevel::from_u8(0).unwrap();
        let v = ints("[1]");
        (0..REPS)
            .map(|r| {
                let p = dir.join(format!("flush{r}.idx"));
                let h = shared_open_ostream_with_schema(p.to_str().unwrap(), "ai4").unwrap();
                let n = 3000;
                let t = Instant::now();
                for _ in 0..n {
                    shared_write_subpacket(h, lvl, v).unwrap();
                    shared_flush_buffer(h).unwrap();
                }
                let per = t.elapsed().as_secs_f64() / n as f64;
                shared_close_handle(h).unwrap();
                per
            })
            .collect()
    });
    mlcbench("stream", "write-flush", &s);
}

#[test]
#[ignore]
fn stream_bench_small() {
    let s = in_child(|dir| {
        let lvl = CompressionLevel::from_u8(0).unwrap();
        let v = ints("[1,2,3]");
        (0..REPS)
            .map(|r| {
                let n = 300;
                let t = Instant::now();
                for i in 0..n {
                    let p = dir.join(format!("s{r}-{i}.idx"));
                    let h = shared_open_ostream_with_schema(p.to_str().unwrap(), "ai4").unwrap();
                    shared_write_subpacket(h, lvl, v).unwrap();
                    shared_close_handle(h).unwrap();
                }
                t.elapsed().as_secs_f64() / n as f64
            })
            .collect()
    });
    mlcbench("stream", "open-write-close", &s);
}

/// Seconds per MiB written: 256 MiB of 16 KiB integer lists, closed.
fn bulk(dir: &std::path::Path, level: u8) -> Vec<f64> {
    let lvl = CompressionLevel::from_u8(level).unwrap();
    let body: Vec<String> = (0..4096).map(|i| (i * 7919 % 100003).to_string()).collect();
    let v = ints(&format!("[{}]", body.join(",")));
    let writes = 16384;
    (0..REPS)
        .map(|r| {
            let p = dir.join(format!("bulk{r}.idx"));
            let t = Instant::now();
            let h = shared_open_ostream_with_schema(p.to_str().unwrap(), "ai4").unwrap();
            for _ in 0..writes {
                shared_write_subpacket(h, lvl, v).unwrap();
            }
            shared_close_handle(h).unwrap();
            let per = t.elapsed().as_secs_f64() / 256.0;
            let _ = std::fs::remove_file(&p);
            per
        })
        .collect()
}

#[test]
#[ignore]
fn stream_bench_bulk_z0() {
    mlcbench("stream", "bulk-z0-s-per-mib", &in_child(|d| bulk(d, 0)));
}

#[test]
#[ignore]
fn stream_bench_bulk_z3() {
    mlcbench("stream", "bulk-z3-s-per-mib", &in_child(|d| bulk(d, 3)));
}

/// Seconds per batch through a channel: a producer thread queues one-element
/// batches as fast as the reader takes them.
#[test]
#[ignore]
fn stream_bench_channel() {
    let s = in_child(|_dir| {
        use crate::stream::{shared_next_frame, shared_open_channel, shared_settle_channel};
        const N: usize = 2000;
        (0..REPS)
            .map(|_| {
                let h = shared_open_channel("ai4").unwrap();
                let t = Instant::now();
                let producer = std::thread::spawn(move || {
                    let v = ints("[1]");
                    for _ in 0..N {
                        shared_write_subpacket(h, CompressionLevel::NONE, v).unwrap();
                        shared_flush_buffer(h).unwrap();
                    }
                    crate::shm::shfree(v).unwrap();
                    shared_close_handle(h).unwrap();
                });
                let mut n = 0;
                while let Some(p) = shared_next_frame(h).unwrap() {
                    crate::shm::shfree(p).unwrap();
                    n += 1;
                }
                producer.join().unwrap();
                assert_eq!(n, N);
                shared_settle_channel(h).unwrap();
                t.elapsed().as_secs_f64() / N as f64
            })
            .collect()
    });
    mlcbench("stream", "channel-batch", &s);
}

/// Seconds a reader takes, beyond the producer's own pace, to see each batch
/// of a channel whose producer pauses 200 us between batches.
#[test]
#[ignore]
fn stream_bench_channel_paced() {
    let s = in_child(|_dir| {
        use crate::stream::{shared_next_frame, shared_open_channel, shared_settle_channel};
        const N: usize = 200;
        const PACE: std::time::Duration = std::time::Duration::from_micros(200);
        (0..REPS)
            .map(|_| {
                let h = shared_open_channel("ai4").unwrap();
                let t = Instant::now();
                let producer = std::thread::spawn(move || {
                    let v = ints("[1]");
                    for _ in 0..N {
                        std::thread::sleep(PACE);
                        shared_write_subpacket(h, CompressionLevel::NONE, v).unwrap();
                        shared_flush_buffer(h).unwrap();
                    }
                    crate::shm::shfree(v).unwrap();
                    shared_close_handle(h).unwrap();
                });
                let mut n = 0;
                while let Some(p) = shared_next_frame(h).unwrap() {
                    crate::shm::shfree(p).unwrap();
                    n += 1;
                }
                producer.join().unwrap();
                assert_eq!(n, N);
                shared_settle_channel(h).unwrap();
                (t.elapsed().as_secs_f64() - N as f64 * PACE.as_secs_f64()) / N as f64
            })
            .collect()
    });
    mlcbench("stream", "channel-paced-lag", &s);
}
