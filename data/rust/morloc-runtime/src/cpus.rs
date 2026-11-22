use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

// WPOOL-11
static CPUS: AtomicUsize = AtomicUsize::new(0);
static CPUS_READ_AT_NS: AtomicU64 = AtomicU64::new(0);
static CPUS_GENERATION: AtomicU64 = AtomicU64::new(0);
// WPOOL-11: the generation whose count a background thread keeps fresh, or
// u64::MAX for none.
static BACKGROUND_GENERATION: AtomicU64 = AtomicU64::new(u64::MAX);

const REREAD_AFTER_NS: u64 = 1_000_000_000;

fn coarse_now_ns() -> u64 {
    let mut ts = libc::timespec { tv_sec: 0, tv_nsec: 0 };
    #[cfg(target_os = "linux")]
    let clock = libc::CLOCK_MONOTONIC_COARSE;
    #[cfg(not(target_os = "linux"))]
    let clock = libc::CLOCK_MONOTONIC;
    // SAFETY: WPOOL-11: clock_gettime writes only the struct it is given.
    unsafe { libc::clock_gettime(clock, &mut ts) };
    (ts.tv_sec as u64).saturating_mul(1_000_000_000).saturating_add(ts.tv_nsec as u64).max(1)
}

// WPOOL-11: concurrent rereads each store a count they read, so any of them
// is a valid answer.
pub(crate) fn available_cpus() -> usize {
    let generation = crate::fork_policy::generation();
    let n = CPUS.load(Ordering::Acquire);
    if n > 0 && BACKGROUND_GENERATION.load(Ordering::Relaxed) == generation {
        return n;
    }
    let now = coarse_now_ns();
    let at = CPUS_READ_AT_NS.load(Ordering::Relaxed);
    if n > 0 && CPUS_GENERATION.load(Ordering::Relaxed) == generation && now.wrapping_sub(at) < REREAD_AFTER_NS {
        return n;
    }
    let n = read_available_cpus();
    CPUS_READ_AT_NS.store(now, Ordering::Relaxed);
    CPUS_GENERATION.store(generation, Ordering::Relaxed);
    CPUS.store(n, Ordering::Release);
    n
}

// WPOOL-11: a read by the thread that keeps the count fresh in the
// background, so callers in this generation never read it themselves.
pub(crate) fn refresh_in_background() {
    let generation = crate::fork_policy::generation();
    let n = read_available_cpus();
    CPUS_READ_AT_NS.store(coarse_now_ns(), Ordering::Relaxed);
    CPUS_GENERATION.store(generation, Ordering::Relaxed);
    CPUS.store(n, Ordering::Release);
    BACKGROUND_GENERATION.store(generation, Ordering::Relaxed);
}

// WPOOL-11: the last count read, without reading again unless none was.
pub(crate) fn available_cpus_cached() -> usize {
    match CPUS.load(Ordering::Acquire) {
        0 => available_cpus(),
        n => n,
    }
}

fn read_available_cpus() -> usize {
    let mut n = affinity_cpus();
    if n == 0 {
        n = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(1);
    }
    #[cfg(target_os = "linux")]
    if n > 1 {
        let quota = cgroup_cpu_limit("/sys/fs/cgroup", "/proc/self/cgroup");
        if quota > 0 && quota < n {
            n = quota;
        }
    }
    n.max(1)
}

#[cfg(target_os = "linux")]
fn affinity_cpus() -> usize {
    let mut ncpu: usize = 1024;
    while ncpu <= 1 << 20 {
        let words = ncpu / 64;
        let mut mask = vec![0u64; words];
        // SAFETY: WPOOL-11: `mask` holds `words * 8` writable bytes, the size passed.
        let rc = unsafe { libc::sched_getaffinity(0, words * 8, mask.as_mut_ptr() as *mut libc::cpu_set_t) };
        if rc == 0 {
            return mask.iter().map(|w| w.count_ones() as usize).sum();
        }
        // SAFETY: WPOOL-11: reads this thread's errno.
        if unsafe { crate::utility::errno_val() } != libc::EINVAL {
            return 0;
        }
        ncpu *= 2;
    }
    0
}

#[cfg(not(target_os = "linux"))]
fn affinity_cpus() -> usize {
    0
}

fn parse_count(text: &str) -> u64 {
    let t = text.trim_matches(' ');
    if t.is_empty() || !t.bytes().all(|c| c.is_ascii_digit()) {
        return 0;
    }
    t.parse::<i64>().map(|v| v as u64).unwrap_or(0)
}

#[cfg(target_os = "linux")]
fn read_small_file(path: &str) -> String {
    use std::io::Read;
    let mut buf = [0u8; 4096];
    let read = std::fs::File::open(path).and_then(|mut f| loop {
        match f.read(&mut buf) {
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
            r => break r,
        }
    });
    match read {
        Ok(k) => String::from_utf8_lossy(&buf[..k]).into_owned(),
        Err(_) => String::new(),
    }
}

#[cfg(target_os = "linux")]
fn quota_cpus(quota: u64, period: u64) -> i64 {
    if quota > 0 && period > 0 {
        quota.div_ceil(period) as i64
    } else {
        0
    }
}

// WPOOL-11: 0 for no limit, -1 when `cpu.max` cannot be read.
#[cfg(target_os = "linux")]
fn cgroup_v2_limit(dir: &str) -> i64 {
    let text = read_small_file(&format!("{dir}/cpu.max"));
    if text.is_empty() {
        return -1;
    }
    let text = text.strip_suffix('\n').unwrap_or(&text);
    match text.split_once(' ') {
        Some((quota, period)) => quota_cpus(parse_count(quota), parse_count(period)),
        None => 0,
    }
}

// WPOOL-11: 0 for no limit (a quota of -1 parses as 0), -1 when the quota
// cannot be read.
#[cfg(target_os = "linux")]
fn cgroup_v1_limit(dir: &str) -> i64 {
    let quota = read_small_file(&format!("{dir}/cpu.cfs_quota_us"));
    if quota.is_empty() {
        return -1;
    }
    let q = parse_count(quota.strip_suffix('\n').unwrap_or(&quota));
    if q == 0 {
        return 0;
    }
    let period = read_small_file(&format!("{dir}/cpu.cfs_period_us"));
    quota_cpus(q, parse_count(period.strip_suffix('\n').unwrap_or(&period)))
}

// WPOOL-11: the smallest limit over the cgroup at `rel` and its ancestors.
// The mount's own directory is read only for the namespace's root cgroup or
// when no level below it could be read; a path leaving the namespace or a
// deleted cgroup has no limit.
#[cfg(target_os = "linux")]
fn cgroup_walk(mount: &str, rel: &str, limit_of: fn(&str) -> i64) -> usize {
    if rel.ends_with(" (deleted)") || !rel.starts_with('/') || format!("{rel}/").contains("/../") {
        return 0;
    }
    let rel = rel.trim_end_matches('/');
    let mut limit = 0usize;
    let mut read_any = false;
    let mut dir = format!("{mount}{rel}");
    loop {
        let at_mount = dir.len() <= mount.len();
        if at_mount && !(rel.is_empty() || !read_any) {
            break;
        }
        let k = limit_of(&dir);
        read_any |= k >= 0;
        if k > 0 && (limit == 0 || (k as usize) < limit) {
            limit = k as usize;
        }
        if at_mount {
            break;
        }
        match dir.rfind('/') {
            Some(i) => dir.truncate(i),
            None => break,
        }
    }
    limit
}

// WPOOL-11: v2 (`cpu.max` under `mount`) and v1 (`cpu.cfs_quota_us` under
// `mount`/cpu) quotas named by `self_cgroup`, the smallest; 0 for none.
#[cfg(target_os = "linux")]
fn cgroup_cpu_limit(mount: &str, self_cgroup: &str) -> usize {
    let mut limit = 0usize;
    for line in read_small_file(self_cgroup).split('\n') {
        let mut parts = line.splitn(3, ':');
        let (Some(id), Some(controllers), Some(rel)) = (parts.next(), parts.next(), parts.next()) else { continue };
        let k = if id == "0" && controllers.is_empty() {
            cgroup_walk(mount, rel, cgroup_v2_limit)
        } else if controllers.split(',').any(|c| c == "cpu") {
            cgroup_walk(&format!("{mount}/cpu"), rel, cgroup_v1_limit)
        } else {
            continue;
        };
        if k > 0 && (limit == 0 || k < limit) {
            limit = k;
        }
    }
    limit
}

#[cfg(test)]
mod tests {
    use super::*;

    static CACHE_TESTS: std::sync::Mutex<()> = std::sync::Mutex::new(());

    #[cfg(target_os = "linux")]
    #[test]
    fn cgroup_cpu_limit_reads_v1_and_v2() {
        let root = std::env::temp_dir().join(format!("morloc-cgroup-{}", std::process::id()));
        let root = root.to_str().unwrap().to_string();
        std::fs::create_dir_all(format!("{root}/a/b")).unwrap();
        std::fs::create_dir_all(format!("{root}/cpu/x/y")).unwrap();
        let selff = format!("{root}/self");
        let write = |p: &str, t: &str| std::fs::write(p, t).unwrap();
        let limit = |lines: &str| {
            write(&selff, lines);
            cgroup_cpu_limit(&root, &selff)
        };
        let put = |rel: &str, text: &str| write(&format!("{root}/{rel}"), text);

        assert_eq!(limit("0::/a/b\n"), 0);
        put("a/b/cpu.max", "max 100000\n");
        assert_eq!(limit("0::/a/b\n"), 0);
        put("a/b/cpu.max", "150000 100000\n");
        assert_eq!(limit("0::/a/b\n"), 2);
        put("a/cpu.max", "50000 100000\n");
        assert_eq!(limit("0::/a/b\n"), 1);
        put("a/cpu.max", "800000 100000\n");
        put("cpu.max", "100000 100000\n");
        assert_eq!(limit("0::/a/b/\n"), 2);
        assert_eq!(limit("0::/\n"), 1);
        assert_eq!(limit("0::/elsewhere/leaf\n"), 1);
        assert_eq!(limit("0::/../..\n"), 0);
        assert_eq!(limit("0::/a/b (deleted)\n"), 0);

        put("cpu/x/y/cpu.cfs_quota_us", "-1\n");
        put("cpu/x/y/cpu.cfs_period_us", "100000\n");
        put("cpu/x/cpu.cfs_quota_us", "250000\n");
        put("cpu/x/cpu.cfs_period_us", "100000\n");
        assert_eq!(limit("4:cpu,cpuacct:/x/y\n"), 3);
        assert_eq!(limit("4:cpuacct,cpu:/x/y\n1:name=systemd:/x\n"), 3);
        assert_eq!(limit("4:cpu,cpuacct:/x/y\n0::/a/b\n"), 2);
        assert_eq!(limit("1:name=systemd:/x/y\n3:cpuacct:/x/y\n"), 0);
        assert_eq!(cgroup_cpu_limit(&root, &format!("{root}/missing")), 0);
        std::fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn the_available_cpus_follow_affinity_and_cgroup_quota() {
        let _one = CACHE_TESTS.lock().unwrap_or_else(|p| p.into_inner());
        let background = BACKGROUND_GENERATION.swap(u64::MAX, Ordering::Relaxed);
        let n = available_cpus();
        assert!(n >= 1);
        assert_eq!(n, read_available_cpus());
        let affinity = affinity_cpus();
        if affinity > 0 {
            assert!(n <= affinity);
        }
        CPUS.store(n + 1000, Ordering::Release);
        assert_eq!(available_cpus(), n + 1000);
        CPUS_READ_AT_NS.store(coarse_now_ns().saturating_sub(2 * REREAD_AFTER_NS), Ordering::Relaxed);
        assert_eq!(available_cpus(), n);
        refresh_in_background();
        CPUS.store(n + 1000, Ordering::Release);
        CPUS_READ_AT_NS.store(0, Ordering::Relaxed);
        assert_eq!(available_cpus(), n + 1000);
        CPUS.store(n, Ordering::Release);
        BACKGROUND_GENERATION.store(background, Ordering::Relaxed);
    }

    #[test]
    fn the_cpu_count_is_read_again_in_a_forked_child() {
        let _one = CACHE_TESTS.lock().unwrap_or_else(|p| p.into_inner());
        let background = BACKGROUND_GENERATION.swap(u64::MAX, Ordering::Relaxed);
        let n = available_cpus();
        CPUS.store(n + 1000, Ordering::Release);
        CPUS_READ_AT_NS.store(coarse_now_ns(), Ordering::Relaxed);
        // SAFETY: WPOOL-11: the child calls only what it needs and _exits.
        let pid = unsafe { libc::fork() };
        if pid == 0 {
            let ok = available_cpus() == read_available_cpus();
            unsafe { libc::_exit(if ok { 0 } else { 1 }) };
        }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        CPUS.store(0, Ordering::Release);
        BACKGROUND_GENERATION.store(background, Ordering::Relaxed);
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "status {status}");
    }
}

mod c_abi {
    // WPOOL-11
    #[no_mangle]
    pub extern "C" fn morloc_available_cpus() -> usize {
        super::available_cpus()
    }
}
