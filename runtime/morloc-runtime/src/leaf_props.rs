//! Leaf elements written whole by the runtime (SLOT-36, SLOT-32, FMT-10):
//! byte identity with the copy path, refusals, and no stale byte from an
//! earlier stream on the same slot.

use crate::error::MorlocError;
use crate::json::read_json_with_schema;
use crate::shm;
use crate::shm_types::{AbsPtr, Array};
use crate::stream::{shared_close_handle, shared_open_ostream_with_schema, shared_write_subpacket};
use morloc_runtime_types::schema::parse_schema;

const CHILD_ENV: &str = "MORLOC_LEAF_PROPS_CHILD";

fn buffer_bytes(n: usize) -> impl Drop {
    struct Restore;
    impl Drop for Restore {
        fn drop(&mut self) {
            crate::stream::set_test_write_buffer_bytes(None);
        }
    }
    crate::stream::set_test_write_buffer_bytes(Some(n));
    Restore
}

fn temp_dir(tag: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("morloc_leaf_{tag}_{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// Write the list `value` of leaves as a binding does: one
/// `write_place_bytes` per element, from the element's own bytes; under
/// `nowait`, a placement that would block is waited for and made again.
/// `flags_at` gives one element other flags. True if written in place.
fn write_leaves(
    h: i64,
    level: crate::compression::CompressionLevel,
    value: AbsPtr,
    nowait: bool,
    flags_at: Option<(usize, u32)>,
) -> Result<bool, MorlocError> {
    use crate::stream::{write_abort, write_begin, write_commit, write_place_bytes, write_wait, Begun};
    if let Begun::Fallback = write_begin(h, level, nowait)? {
        return shared_write_subpacket(h, level, value).map(|()| false);
    }
    let r = (|| {
        let arr = unsafe { &*(value as *const Array) };
        let base = if arr.size == 0 { std::ptr::null_mut() } else { shm::rel2abs(arr.data)? };
        let mut reached: *const u8 = std::ptr::null();
        let mut i = 0;
        while i < arr.size {
            let e = unsafe { &*(base.add(i * 16) as *const Array) };
            let src = if e.size == 0 { std::ptr::null() } else { shm::rel2abs(e.data)? as *const u8 };
            let flags = flags_at.filter(|&(k, _)| k == i).map_or(0, |(_, f)| f);
            let mut bad = None;
            match write_place_bytes(src, e.size, flags, reached, &mut bad)? {
                None => {
                    assert!(nowait, "a waiting batch was told it would block");
                    write_wait(reached)?;
                }
                Some(end) => {
                    reached = end;
                    i += 1;
                }
            }
        }
        write_commit(reached)
    })();
    r.inspect_err(|_| write_abort()).map(|()| true)
}

/// The pieces of one list-of-leaves element at `e`: each leaf's bytes and
/// length.
fn pieces_of(e: &Array) -> Result<Vec<(*const u8, usize)>, MorlocError> {
    let base = if e.size == 0 { std::ptr::null_mut() } else { shm::rel2abs(e.data)? };
    (0..e.size)
        .map(|j| {
            let p = unsafe { &*(base.add(j * 16) as *const Array) };
            Ok((if p.size == 0 { std::ptr::null() } else { shm::rel2abs(p.data)? as *const u8 }, p.size))
        })
        .collect()
}

/// Write the list `value` of lists of leaves as a binding does: one
/// `write_place_pieces` per element, from its leaves' own bytes; under
/// `nowait`, a placement that would block is waited for and made again.
/// On a refusal, the element's index and the runtime's report of the bad
/// leaf and byte. True if written in place.
fn write_pieces(
    h: i64,
    level: crate::compression::CompressionLevel,
    value: AbsPtr,
    nowait: bool,
    flags: u32,
) -> Result<bool, (MorlocError, Option<(usize, Option<(usize, usize)>)>)> {
    use crate::stream::{write_abort, write_begin, write_commit, write_place_pieces, write_wait, Begun};
    if let Begun::Fallback = write_begin(h, level, nowait).map_err(|e| (e, None))? {
        return shared_write_subpacket(h, level, value).map(|()| false).map_err(|e| (e, None));
    }
    let r = (|| {
        let arr = unsafe { &*(value as *const Array) };
        let base = if arr.size == 0 { std::ptr::null_mut() } else { shm::rel2abs(arr.data).map_err(|e| (e, None))? };
        let mut reached: *const u8 = std::ptr::null();
        let mut i = 0;
        while i < arr.size {
            let e = unsafe { &*(base.add(i * 16) as *const Array) };
            let pieces = pieces_of(e).map_err(|e| (e, None))?;
            let mut bad = None;
            match write_place_pieces(&pieces, flags, reached, &mut bad).map_err(|e| (e, Some((i, bad))))? {
                None => {
                    assert!(nowait, "a waiting batch was told it would block");
                    write_wait(reached).map_err(|e| (e, None))?;
                }
                Some(end) => {
                    reached = end;
                    i += 1;
                }
            }
        }
        write_commit(reached).map_err(|e| (e, None))
    })();
    r.inspect_err(|_| write_abort()).map(|()| true)
}

/// The file's bytes before its footer, and its sub-packet index and count.
fn file_bytes(path: &std::path::Path) -> Result<Vec<u8>, MorlocError> {
    let (mp, sz) = crate::stream_format::mmap_file_readonly(path.to_str().unwrap())?;
    let footer = crate::stream_format::try_read_footer(mp, sz);
    unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize) };
    let footer = footer?.ok_or_else(|| MorlocError::Other("no footer".into()))?;
    let mut bytes = std::fs::read(path).unwrap();
    bytes.truncate(footer.footer_start as usize);
    bytes.extend(format!("{:?} {}", footer.subpacket_entries, footer.element_count).into_bytes());
    Ok(bytes)
}

fn lengths() -> Vec<usize> {
    let mut v: Vec<usize> = (0..=17).collect();
    for k in [5u32, 6, 7, 8, 10, 12] {
        v.extend([(1 << k) - 1, 1 << k, (1 << k) + 1]);
    }
    v
}

fn text_of(len: usize, tag: usize) -> String {
    let pieces = ["a", "\u{e9}", "\u{4e2d}", "\u{1F600}", "b", "c"];
    let mut s = String::new();
    let mut k = tag;
    while s.len() < len {
        let p = pieces[k % pieces.len()];
        k += 1;
        if s.len() + p.len() > len {
            s.push('x');
        } else {
            s.push_str(p);
        }
    }
    s
}

/// JSON batches of leaves of `schema`: the lengths, rotated per batch so that
/// each length follows different earlier elements, and one long element.
fn leaf_batches(schema: &str, long: usize) -> Vec<String> {
    let fixed = schema.strip_prefix("aa:").and_then(|t| t.split(|c: char| !c.is_ascii_digit()).next()).and_then(|d| d.parse::<usize>().ok());
    let ls = fixed.map_or_else(lengths, |len| vec![len; 30]);
    let mut out = Vec::new();
    for rot in 0..5 {
        let mut items = Vec::new();
        for (j, k) in (0..ls.len()).enumerate() {
            let n = ls[(k + rot * 3) % ls.len()];
            items.push(match schema {
                "as" => serde_json::to_string(&text_of(n, j + rot)).unwrap(),
                "aau1" => format!("[{}]", (0..n).map(|i| ((i * 7 + j) % 120).to_string()).collect::<Vec<_>>().join(",")),
                _ => format!("[{}]", (0..n).map(|i| ((i * 13 + j) % 100).to_string()).collect::<Vec<_>>().join(",")),
            });
        }
        out.push(format!("[{}]", items.join(",")));
    }
    let n = long;
    if fixed.is_some() {
        out.push("[]".into());
        return out;
    }
    out.push(match schema {
        "as" => format!("[{}, \"z\"]", serde_json::to_string(&text_of(n, 1)).unwrap()),
        _ => format!("[[{}], [1]]", (0..n / 8).map(|i| (i % 90).to_string()).collect::<Vec<_>>().join(",")),
    });
    out.push("[]".into());
    out
}

/// A leaf batch is laid out byte for byte as the copy path lays out the same
/// batch, sub-packet cuts, head pads, tails and empty elements included,
/// also when every placement that seals is first refused as one that would
/// block (SLOT-36, SLOT-37).
fn leaf_identity(tag: &str) -> Vec<String> {
    let _restore = buffer_bytes(4096);
    let dir = temp_dir(tag);
    let mut failures = Vec::new();
    for schema in ["as", "aau1", "aai1", "aai2", "aai4", "aai8", "aau2", "aau4", "aau8", "aaf4", "aaf8", "aa:5f8"] {
        let list = parse_schema(schema).unwrap();
        let batches = leaf_batches(schema, 300_000);
        for level in [0u8, 3] {
            let level = crate::compression::CompressionLevel::from_u8(level).unwrap();
            let write = |name: &str, leaves: bool, nowait: bool| -> Result<Vec<u8>, MorlocError> {
                let path = dir.join(format!("{name}.stream"));
                let h = shared_open_ostream_with_schema(path.to_str().unwrap(), schema)?;
                for b in &batches {
                    let v = read_json_with_schema(b, &list)?;
                    let w = if leaves {
                        crate::stream::FORCE_NO_ROOM.with(|f| f.set(nowait.then_some(true)));
                        let r = write_leaves(h, level, v, nowait, None);
                        crate::stream::FORCE_NO_ROOM.with(|f| f.set(None));
                        r.map(|placed| assert!(placed, "a leaf batch was not written in place"))
                    } else {
                        shared_write_subpacket(h, level, v)
                    };
                    let _ = shm::shfree(v);
                    w?;
                }
                shared_close_handle(h)?;
                file_bytes(&path)
            };
            let copy = write("copy", false, false);
            for (name, nowait) in [("leaf", false), ("wait", true)] {
                match (&copy, write(name, true, nowait)) {
                    (Ok(a), Ok(b)) if *a == b => {}
                    (Ok(a), Ok(b)) => {
                        let at = a.iter().zip(&b).position(|(x, y)| x != y).unwrap_or(a.len().min(b.len()));
                        failures.push(format!("{schema} {level:?} {name}: files differ at byte {at} ({} vs {} bytes)", a.len(), b.len()));
                    }
                    (a, b) => failures.push(format!("{schema} {level:?} {name}: copy {:?} leaf {:?}", a.as_ref().err(), b.err())),
                }
            }
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    failures
}

// SLOT-36
#[test]
fn a_leaf_batch_writes_the_bytes_the_copy_path_writes() {
    let _shm = crate::own_test_registry();
    let failures = leaf_identity("identity");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

fn status_of_child(name: &str) -> i32 {
    std::process::Command::new(std::env::current_exe().unwrap())
        .args([&format!("leaf_props::{name}"), "--exact", "--ignored", "--test-threads=1", "--nocapture"])
        .env(CHILD_ENV, "1")
        .env_remove("MORLOC_SKIP_UTF8_CHECK")
        .env_remove("MORLOC_PROGRAM_SKIPS_UTF8_CHECK")
        .env_remove(crate::origin::CODE_CHECKS_ENV)
        .status()
        .unwrap()
        .code()
        .unwrap_or(-1)
}

// SLOT-36, SLOT-32
#[test]
fn leaf_rooms_on_poisoned_spares_write_the_bytes_the_copy_path_writes() {
    assert_eq!(status_of_child("child_leaf_identity_on_poisoned_spares"), 0);
}

#[test]
#[ignore]
fn child_leaf_identity_on_poisoned_spares() {
    if std::env::var_os(CHILD_ENV).is_none() {
        return;
    }
    let _shm = crate::own_test_registry();
    crate::shm::set_poison_on_free(true);
    let failures = leaf_identity("poisoned");
    for f in &failures {
        eprintln!("{f}");
    }
    unsafe { libc::_exit(if failures.is_empty() { 0 } else { 1 }) };
}

fn count(bytes: &[u8], b: u8) -> usize {
    bytes.iter().filter(|&&x| x == b).count()
}

/// Stream A on a fresh slot: a committed batch of leaves holding 0xA5, then
/// a batch of such leaves that a Str that is not text aborts after its
/// rooms were written. The slot A used.
fn earlier_stream(dir: &std::path::Path, tag: usize, failures: &mut Vec<String>) -> usize {
    let level = crate::compression::CompressionLevel::NONE;
    let strs = parse_schema("as").unwrap();
    let a_path = dir.join(format!("a{tag}.stream"));
    let a = shared_open_ostream_with_schema(a_path.to_str().unwrap(), "as").unwrap();
    let sentinel = |n: usize| serde_json::to_string(&"\u{a5}".repeat(n)).unwrap();
    let kept = format!("[{}]", [100, 1000, 3000, 5000, 2047].map(sentinel).join(","));
    let v = read_json_with_schema(&kept, &strs).unwrap();
    write_leaves(a, level, v, false, None).unwrap();
    let _ = shm::shfree(v);
    let mut bytes = Vec::new();
    for n in [3000usize, 1500, 400_000] {
        bytes.push((0..n / 2).flat_map(|_| [0xC2u32, 0xA5]).collect::<Vec<_>>());
    }
    bytes.last_mut().unwrap().push(0xFF);
    let aborted = format!("[{}]", bytes.iter().map(|b| format!("{b:?}")).collect::<Vec<_>>().join(","));
    let v = read_json_with_schema(&aborted, &parse_schema("aau1").unwrap()).unwrap();
    match write_leaves(a, level, v, false, None) {
        Err(e) if e.to_string().contains("not valid UTF-8") => {}
        other => failures.push(format!("the bad batch was not refused as not text: {other:?}")),
    }
    let _ = shm::shfree(v);
    if crate::stream::write_open() {
        failures.push("a batch was still open after a leaf was refused".into());
    }
    shared_close_handle(a).unwrap();
    crate::stream::unpack_handle(a).1
}

/// JSON batches of lists of leaves of the element schema `leaf` (`s`, `au1`,
/// ...): pieces of the lengths, zero included, in lists of 0 to 6 pieces;
/// empty lists; and one element with a piece too large for a sub-packet.
fn piece_batches(leaf: &str, long: usize) -> Vec<String> {
    let ls = lengths();
    let piece = |n: usize, j: usize| match leaf {
        "s" => serde_json::to_string(&text_of(n, j)).unwrap(),
        "au1" => format!("[{}]", (0..n).map(|i| ((i * 7 + j) % 120).to_string()).collect::<Vec<_>>().join(",")),
        _ => format!("[{}]", (0..n).map(|i| ((i * 13 + j) % 100).to_string()).collect::<Vec<_>>().join(",")),
    };
    let mut out = Vec::new();
    for rot in 0..4 {
        let mut items = Vec::new();
        let mut k = rot * 5;
        for j in 0..24 {
            let count = (j + rot) % 7;
            let ps: Vec<String> = (0..count)
                .map(|q| {
                    k += 1;
                    piece(ls[(k + q) % ls.len()], j + q)
                })
                .collect();
            items.push(format!("[{}]", ps.join(",")));
        }
        out.push(format!("[{}]", items.join(",")));
    }
    out.push(format!("[[{}, {}], [{}], []]", piece(3, 0), piece(long, 1), piece(0, 2)));
    out.push("[]".into());
    out
}

/// A list-of-leaves batch is laid out byte for byte as the copy path lays
/// out the same batch: sub-packet cuts, header and body pads, empty leaves
/// and empty lists included, also when every placement that seals is first
/// refused as one that would block (SLOT-36, SLOT-37).
fn pieces_identity(tag: &str) -> Vec<String> {
    let _restore = buffer_bytes(4096);
    let dir = temp_dir(tag);
    let mut failures = Vec::new();
    for leaf in ["s", "au1", "ai1", "ai2", "ai4", "ai8", "au8", "af4", "af8"] {
        let schema = format!("aa{leaf}");
        let list = parse_schema(&schema).unwrap();
        let batches = piece_batches(leaf, if leaf == "s" || leaf == "au1" { 300_000 } else { 40_000 });
        for level in [0u8, 3] {
            let level = crate::compression::CompressionLevel::from_u8(level).unwrap();
            let write = |name: &str, pieces: bool, nowait: bool| -> Result<Vec<u8>, MorlocError> {
                let path = dir.join(format!("{name}.stream"));
                let h = shared_open_ostream_with_schema(path.to_str().unwrap(), &schema)?;
                for b in &batches {
                    let v = read_json_with_schema(b, &list)?;
                    let w = if pieces {
                        crate::stream::FORCE_NO_ROOM.with(|f| f.set(nowait.then_some(true)));
                        let r = write_pieces(h, level, v, nowait, 0);
                        crate::stream::FORCE_NO_ROOM.with(|f| f.set(None));
                        r.map_err(|(e, _)| e).map(|placed| assert!(placed, "a list-of-leaves batch was not written in place"))
                    } else {
                        shared_write_subpacket(h, level, v)
                    };
                    let _ = shm::shfree(v);
                    w?;
                }
                shared_close_handle(h)?;
                file_bytes(&path)
            };
            let copy = write("copy", false, false);
            for (name, nowait) in [("pieces", false), ("wait", true)] {
                match (&copy, write(name, true, nowait)) {
                    (Ok(a), Ok(b)) if *a == b => {}
                    (Ok(a), Ok(b)) => {
                        let at = a.iter().zip(&b).position(|(x, y)| x != y).unwrap_or(a.len().min(b.len()));
                        failures.push(format!("{schema} {level:?} {name}: files differ at byte {at} ({} vs {} bytes)", a.len(), b.len()));
                    }
                    (a, b) => failures.push(format!("{schema} {level:?} {name}: copy {:?} pieces {:?}", a.as_ref().err(), b.err())),
                }
            }
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    failures
}

// SLOT-36
#[test]
fn a_list_of_leaves_batch_writes_the_bytes_the_copy_path_writes() {
    let _shm = crate::own_test_registry();
    let failures = pieces_identity("pieces_identity");
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

// SLOT-36, SLOT-32
#[test]
fn list_of_leaves_rooms_on_poisoned_spares_write_the_bytes_the_copy_path_writes() {
    assert_eq!(status_of_child("child_pieces_identity_on_poisoned_spares"), 0);
}

#[test]
#[ignore]
fn child_pieces_identity_on_poisoned_spares() {
    if std::env::var_os(CHILD_ENV).is_none() {
        return;
    }
    let _shm = crate::own_test_registry();
    crate::shm::set_poison_on_free(true);
    let failures = pieces_identity("pieces_poisoned");
    for f in &failures {
        eprintln!("{f}");
    }
    unsafe { libc::_exit(if failures.is_empty() { 0 } else { 1 }) };
}

// SLOT-36, FMT-10: a list of leaves whose k-th leaf is not text aborts its
// batch, which is absent, naming the leaf and the byte; the stream takes
// later batches.
#[test]
fn a_list_of_leaves_with_a_leaf_that_is_not_text_aborts_its_batch() {
    let _shm = crate::own_test_registry();
    let dir = temp_dir("pieces_not_text");
    let strs = parse_schema("aas").unwrap();
    let bytes = parse_schema("aaau1").unwrap();
    // The leaf made bad, its byte made 0xFF, and the level.
    for (k, (which, at, level)) in [(0usize, 0usize, 0u8), (4, 299_999, 0), (3, 2, 3), (2, 0, 0)].into_iter().enumerate() {
        let level = crate::compression::CompressionLevel::from_u8(level).unwrap();
        let path = dir.join(format!("s{k}.stream"));
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "aas").unwrap();
        let v = read_json_with_schema("[[\"one\", \"\"], []]", &strs).unwrap();
        write_pieces(h, level, v, false, 0).map_err(|(e, _)| e).unwrap();
        let _ = shm::shfree(v);
        let mut ps: Vec<Vec<u32>> = vec![vec![116, 119, 111], vec![], vec![195, 169], vec![97, 98, 99, 100]];
        ps.push((0..300_000).map(|i| 97 + i % 26).collect());
        ps[which][at] = 0xFF;
        let ps: Vec<String> = ps.iter().map(|p| format!("{p:?}")).collect();
        let json = format!("[[[1]], [{}], [[2]]]", ps.join(","));
        let v = read_json_with_schema(&json, &bytes).unwrap();
        let r = write_pieces(h, level, v, false, 0);
        let _ = shm::shfree(v);
        match r {
            Err((e, Some((1, Some(bad))))) => {
                assert!(e.to_string().contains("not valid UTF-8"), "{k}: {e}");
                assert_eq!(bad, (which, at), "{k}: the bad leaf and byte");
            }
            other => panic!("{k}: a list holding a Str that is not text was not refused: {:?}", other.map_err(|(e, b)| (e.to_string(), b))),
        }
        assert!(!crate::stream::write_open(), "{k}: the batch stayed open");
        let v = read_json_with_schema("[[\"three\"]]", &strs).unwrap();
        write_pieces(h, level, v, false, 0).map_err(|(e, _)| e).unwrap();
        let _ = shm::shfree(v);
        shared_close_handle(h).unwrap();
        let got = crate::stream::shared_load_stream_file_as_array(path.to_str().unwrap(), Some(&strs)).unwrap();
        let json = crate::json::voidstar_to_json_string(got, &strs).unwrap();
        let _ = shm::shfree(got);
        assert_eq!(json, "[[\"one\",\"\"],[],[\"three\"]]", "{k}");
    }
    let _ = std::fs::remove_dir_all(&dir);
}

// SLOT-36: a list-of-leaves write refuses what is not one, or not its
// flags or lengths, naming the schema and aborting the batch.
#[test]
fn a_list_of_leaves_write_refuses_what_is_not_one() {
    use crate::stream::{write_begin, write_place_pieces};
    let _shm = crate::own_test_registry();
    let dir = temp_dir("pieces_refusals");
    let level = crate::compression::CompressionLevel::NONE;
    let abcd = b"abcd".as_ptr();
    let cases: [(&str, u32, &[(*const u8, usize)], &str); 7] = [
        ("as", 0, &[(abcd, 1)], "not a list of leaves"),
        ("aai4", 0, &[(abcd, 1)], "not a list of leaves"),
        ("aaat2si4", 0, &[(abcd, 1)], "not a list of leaves"),
        ("aaaai4", 0, &[(abcd, 1)], "not a list of leaves"),
        ("aaai4", morloc_runtime_types::MLC_PLACE_VALID_TEXT, &[(abcd, 1)], "not text"),
        ("aas", 8, &[(abcd, 1)], "unknown leaf flags"),
        ("aas", 0, &[(abcd, 1), (std::ptr::null(), 2)], "leaf 1 has no bytes"),
    ];
    for (k, (schema, flags, pieces, want)) in cases.iter().enumerate() {
        let path = dir.join(format!("r{k}.stream"));
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), schema).unwrap();
        write_begin(h, level, false).unwrap();
        let mut bad = None;
        let e = write_place_pieces(pieces, *flags, std::ptr::null(), &mut bad).expect_err(schema).to_string();
        assert!(e.contains(want) && e.contains(&schema[1..]), "{schema}: {e}");
        assert!(!crate::stream::write_open(), "{schema}: the batch stayed open");
        shared_close_handle(h).unwrap();
    }
    let _ = std::fs::remove_dir_all(&dir);
}

// SLOT-36, SLOT-32: no byte of an earlier stream on the slot reaches a
// later stream's file.
#[test]
fn a_later_stream_on_a_slot_holds_no_byte_of_an_earlier_one() {
    assert_eq!(status_of_child("child_later_stream_holds_no_earlier_byte"), 0);
}

#[test]
#[ignore]
fn child_later_stream_holds_no_earlier_byte() {
    if std::env::var_os(CHILD_ENV).is_none() {
        return;
    }
    let _shm = crate::own_test_registry();
    crate::shm::set_poison_on_free(true);
    let _restore = buffer_bytes(4096);
    let dir = temp_dir("sentinel");
    let level = crate::compression::CompressionLevel::NONE;
    let mut failures: Vec<String> = Vec::new();

    // 0: a leaf, 1: a list of leaves, 2: a node.
    let later: [(&str, &str, u8); 6] = [
        ("as", "[\"x\", \"yy\", \"\", \"abc\", \"0123456789abcdef0123\", \"q\"]", 0),
        ("aai4", "[[1,2,3], [], [4], [5,6,7,8,9,10,11,12,13], [14,15]]", 0),
        ("aau1", "[[1], [2,3,4,5,6,7,8,9], [], [10,11]]", 0),
        ("aas", "[[\"x\", \"\", \"yyy\"], [], [\"0123456789abcdef0\"], [\"\"], [\"a\", \"bc\", \"def\", \"ghij\", \"k\"]]", 1),
        ("aaau1", "[[[1], [], [2,3,4,5,6,7,8,9,10]], [], [[11,12]], [[], []]]", 1),
        ("at2si4", "[[\"a\", 1], [\"bcd\", 2], [\"\", 3], [\"efghijklmnop\", 4]]", 2),
    ];
    for (k, (schema, json, leaf)) in later.iter().enumerate() {
        let list = parse_schema(schema).unwrap();
        let elem = parse_schema(&schema[1..]).unwrap();
        let mut files = Vec::new();
        for in_place in [true, false] {
            let a_slot = if in_place { earlier_stream(&dir, k, &mut failures) } else { usize::MAX };
            let path = dir.join(format!("later-{k}-{in_place}.stream"));
            if in_place {
                crate::stream::PROBE_SLOT.with(|p| p.set(Some(a_slot)));
            }
            let h = shared_open_ostream_with_schema(path.to_str().unwrap(), schema).unwrap();
            if in_place && crate::stream::unpack_handle(h).1 != a_slot {
                failures.push(format!("{schema}: the later stream took slot {} not {a_slot}", crate::stream::unpack_handle(h).1));
            }
            for rep in 0..60 {
                let v = read_json_with_schema(json, &list).unwrap();
                let r = if !in_place {
                    shared_write_subpacket(h, level, v)
                } else if *leaf == 0 {
                    write_leaves(h, level, v, rep % 2 == 1, None).map(|_| ())
                } else if *leaf == 1 {
                    write_pieces(h, level, v, rep % 2 == 1, 0).map(|_| ()).map_err(|(e, _)| e)
                } else {
                    crate::layout_props::write_in_place(h, level, v, &elem, false, None).map(|_| ())
                };
                let _ = shm::shfree(v);
                r.unwrap();
            }
            shared_close_handle(h).unwrap();
            files.push(file_bytes(&path).unwrap());
        }
        let (placed, copied) = (&files[0], &files[1]);
        if placed != copied {
            failures.push(format!("{schema}: the file written after the earlier stream differs from the copy path's"));
        }
        for b in [0xA5u8, crate::shm::POISON_BYTE] {
            if count(placed, b) != count(copied, b) {
                failures.push(format!("{schema}: {} bytes {b:#x} against {} in the copy", count(placed, b), count(copied, b)));
            }
        }
    }
    let _ = std::fs::remove_dir_all(&dir);
    for f in &failures {
        eprintln!("{f}");
    }
    unsafe { libc::_exit(if failures.is_empty() { 0 } else { 1 }) };
}

// SLOT-36, FMT-10: a leaf Str that is not text aborts its batch, which is
// absent, and the stream takes later batches.
#[test]
fn a_leaf_str_that_is_not_text_aborts_its_batch() {
    let _shm = crate::own_test_registry();
    let dir = temp_dir("not_text");
    let path = dir.join("s.stream");
    let level = crate::compression::CompressionLevel::NONE;
    let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
    let strs = parse_schema("as").unwrap();
    let bytes = parse_schema("aau1").unwrap();
    let v = read_json_with_schema("[\"one\"]", &strs).unwrap();
    write_leaves(h, level, v, false, None).unwrap();
    let _ = shm::shfree(v);
    let v = read_json_with_schema("[[116,119,111], [97,255,98]]", &bytes).unwrap();
    let mut bad = None;
    let r = {
        use crate::stream::{write_begin, write_place_bytes};
        write_begin(h, level, false).unwrap();
        let arr = unsafe { &*(v as *const Array) };
        let base = shm::rel2abs(arr.data).unwrap();
        let first = unsafe { &*(base as *const Array) };
        let end = write_place_bytes(shm::rel2abs(first.data).unwrap(), first.size, 0, std::ptr::null(), &mut bad).unwrap().unwrap();
        let second = unsafe { &*(base.add(16) as *const Array) };
        write_place_bytes(shm::rel2abs(second.data).unwrap(), second.size, 0, end, &mut bad)
    };
    let _ = shm::shfree(v);
    assert!(r.is_err(), "a Str that is not text was written");
    assert_eq!(bad, Some(1));
    assert!(!crate::stream::write_open(), "the batch stayed open");
    let v = read_json_with_schema("[\"three\"]", &strs).unwrap();
    write_leaves(h, level, v, false, None).unwrap();
    let _ = shm::shfree(v);
    shared_close_handle(h).unwrap();
    let got = crate::stream::shared_load_stream_file_as_array(path.to_str().unwrap(), Some(&strs)).unwrap();
    let json = crate::json::voidstar_to_json_string(got, &strs).unwrap();
    let _ = shm::shfree(got);
    assert_eq!(json, "[\"one\",\"three\"]");
    let _ = std::fs::remove_dir_all(&dir);
}

// SLOT-36: refusals name the schema and abort the batch.
#[test]
fn a_leaf_write_refuses_what_is_not_a_leaf_or_not_its_flags() {
    use crate::stream::{write_begin, write_place_bytes};
    let _shm = crate::own_test_registry();
    let dir = temp_dir("refusals");
    let level = crate::compression::CompressionLevel::NONE;
    let cases: [(&str, u32, bool, &str); 6] = [
        ("at2si4", 0, false, "not a leaf"),
        ("aab", 0, false, "not a leaf"),
        ("aai4", morloc_runtime_types::MLC_PLACE_VALID_TEXT, false, "not text"),
        ("as", 8, false, "unknown leaf flags"),
        ("as", 0, true, "has no bytes"),
        ("aa:3i4", 0, false, "has length 1, not 3"),
    ];
    for (k, (schema, flags, null_src, want)) in cases.iter().enumerate() {
        let path = dir.join(format!("r{k}.stream"));
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), schema).unwrap();
        write_begin(h, level, false).unwrap();
        let src = if *null_src { std::ptr::null() } else { b"abcd".as_ptr() };
        let mut bad = None;
        let e = write_place_bytes(src, 1, *flags, std::ptr::null(), &mut bad).expect_err(schema).to_string();
        assert!(e.contains(want) && e.contains(&schema[1..]), "{schema}: {e}");
        assert!(!crate::stream::write_open(), "{schema}: the batch stayed open");
        shared_close_handle(h).unwrap();
    }
    let _ = std::fs::remove_dir_all(&dir);
}
