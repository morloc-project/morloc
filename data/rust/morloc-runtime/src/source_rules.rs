use quote::ToTokens;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use syn::visit::Visit;

const CRATES: &[&str] = &["morloc-runtime", "morloc-runtime-types", "rustmorloc", "morloc-nexus"];
const PREPARE_HANDLERS: &[&str] = &["prepare_fork"];
const MAX_DEVIATING_ROWS: usize = 34;
const MAX_ENV_READS: usize = 67;
const PID_READ_SITES: &[&str] = &[
    "morloc-runtime/cell.rs::proc_tag",
    "morloc-runtime/crash.rs::fatal",
    "morloc-runtime/lifeline.rs::create",
    "morloc-runtime/lifeline.rs::teardown",
    "morloc-runtime/log.rs::pool_pid",
    "morloc-runtime/packet_ffi.rs::make_file_data_packet_voidstar",
    "morloc-runtime/run.rs::init_run",
    "morloc-runtime/run.rs::gen_id",
    "morloc-runtime/stream.rs::with_process_local_slot",
    "morloc-runtime/stream.rs::allocate_slot_cas",
    "morloc-runtime/stream.rs::end_slot_locked",
    "morloc-runtime/stream.rs::read_pid_start_time",
    "morloc-runtime/stream.rs::try_reclaim_stale_stdio_claim",
    "morloc-runtime/stream.rs::verify_stdio_opener_pid",
    "morloc-runtime/stream.rs::shared_finalize_ostream_locked",
    "morloc-runtime/stream.rs::lock_for_write",
    "morloc-runtime/stream.rs::note_sealed",
    "morloc-runtime-types/process.rs::token",
    "morloc-runtime-types/recoverable_lock.rs::ensure_ready",
    "morloc-nexus/process.rs::make_tmpdir",
    "morloc-nexus/process.rs::make_job_hash",
    "morloc-nexus/runlog.rs::render",
    "morloc-nexus/stdio_server.rs::start",
    "morloc-nexus/stdio_server.rs::assert_nexus_pid",
    "morloc-nexus/main.rs::run_call_packet",
    "morloc-nexus/process.rs::init_shm",
    "morloc-nexus/mcp.rs::new_session_id",
    "morloc-runtime/ipc_ffi.rs::stream_from_client_wait",
    "morloc-runtime/ipc_ffi.rs::wait_for_client_with_timeout",
    "morloc-runtime/ipc_ffi.rs::try_answer_ping",
    "morloc-runtime/ipc_ffi.rs::close_socket",
    "morloc-runtime/ipc_ffi.rs::send_and_receive_over_socket_wait",
    "morloc-runtime/utility.rs::create_beside",
];
const BINDER_PID_READS: &[(&str, usize)] = &[("data/lang/r/rmorloc.c", 1)];
const CLASSES: &[&str] = &[
    "held", "reset", "unreachable", "exec-only", "startup", "lazy", "fork-scoped", "counter", "thread",
    "instance", "paired", "test-only",
];

fn rust_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_path_buf()
}

fn repo_root() -> PathBuf {
    rust_root().parent().unwrap().parent().unwrap().to_path_buf()
}

fn model_dir() -> PathBuf {
    repo_root().join("model")
}

fn walk(dir: &Path, exts: &[&str], skip: &[&str]) -> Vec<PathBuf> {
    fn go(dir: &Path, exts: &[&str], skip: &[&str], out: &mut Vec<PathBuf>) {
        for e in std::fs::read_dir(dir).unwrap() {
            let p = e.unwrap().path();
            if skip.iter().any(|k| p.to_string_lossy().contains(k)) {
                continue;
            }
            if p.is_dir() {
                go(&p, exts, skip, out);
            } else if p.extension().is_some_and(|x| exts.iter().any(|e| x == *e)) {
                out.push(p);
            }
        }
    }
    let mut out = Vec::new();
    go(dir, exts, skip, &mut out);
    out.sort();
    out
}

fn rust_sources() -> Vec<PathBuf> {
    CRATES.iter().flat_map(|k| walk(&rust_root().join(k).join("src"), &["rs"], &[])).collect()
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn last_ident(s: &str) -> Option<String> {
    let t = s.trim_end();
    let start = t.len() - t.chars().rev().take_while(|c| is_ident_char(*c)).map(char::len_utf8).sum::<usize>();
    let name = &t[start..];
    (!name.is_empty() && !name.starts_with(|c: char| c.is_ascii_digit())).then(|| name.to_string())
}

fn first_ident(s: &str) -> Option<String> {
    let name: String = s.trim_start().chars().take_while(|c| is_ident_char(*c)).collect();
    (!name.is_empty()).then_some(name)
}

fn item_id(text: &str) -> Option<&str> {
    let prefix = text.bytes().take_while(|b| b.is_ascii_uppercase()).count();
    if prefix == 0 || text.as_bytes().get(prefix) != Some(&b'-') {
        return None;
    }
    let digits = text[prefix + 1..].bytes().take_while(|b| b.is_ascii_digit()).count();
    (digits > 0).then(|| &text[..prefix + 1 + digits])
}

// -- model items --------------------------------------------------------------

struct Item {
    path: PathBuf,
    id: String,
    status: Option<String>,
    checks: Vec<String>,
}

fn model_items() -> Vec<Item> {
    let mut items = Vec::new();
    for path in walk(&model_dir(), &["md"], &["/tla/"]) {
        let text = std::fs::read_to_string(&path).unwrap();
        let mut current: Option<Item> = None;
        for line in text.lines().chain(std::iter::once("### END")) {
            if let Some(head) = line.strip_prefix("### ") {
                items.extend(current.take());
                current = item_id(head).map(|id| Item { path: path.clone(), id: id.to_string(), status: None, checks: Vec::new() });
            } else if let Some(item) = current.as_mut() {
                if let Some(s) = line.strip_prefix("Status: ") {
                    item.status = Some(s.trim().to_string());
                } else if let Some(c) = line.strip_prefix("Checked by: ") {
                    item.checks.extend(c.split(',').map(|t| t.trim().trim_matches('`').to_string()).filter(|t| !t.is_empty()));
                }
            }
        }
    }
    items
}

// -- Rust sources -------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Debug)]
enum Kind {
    Lock,
    Atomic,
    Cell,
    FieldLock,
    ThreadLocal,
    Template,
}

impl Kind {
    fn of(ty: &str) -> Kind {
        if is_lock(ty) {
            Kind::Lock
        } else if ty.contains("Atomic") {
            Kind::Atomic
        } else {
            Kind::Cell
        }
    }
}

#[derive(Debug, Clone)]
struct Found {
    id: String,
    kind: Kind,
    ty: String,
    init: String,
    test_only: bool,
    reads_pid: bool,
}

struct Call {
    site: String,
    path: String,
    args: String,
    test_only: bool,
}

#[derive(Default)]
struct RustScan {
    found: Vec<Found>,
    static_muts: Vec<String>,
    macro_statics: Vec<String>,
    calls: Vec<Call>,
    fn_names: HashSet<String>,
    prepare_bodies: String,
    pid_reads: Vec<String>,
    type_parts: HashMap<String, Vec<String>>,
    plain_statics: Vec<Found>,
}

fn is_held(ty: &str) -> bool {
    ty.starts_with("Held <") || ty.contains(":: Held <")
}

fn is_reset(ty: &str) -> bool {
    ty.starts_with("Reset <") || ty.contains(":: Reset <")
}

fn interior_mutable(ty: &str) -> bool {
    is_held(ty)
        || is_reset(ty)
        || (!ty.contains("Guard")
        && ["Mutex", "RwLock", "Condvar", "Atomic", "Once", "LazyLock", "Cell", "RefCell", "UnsafeCell"]
            .iter()
            .any(|w| ty.contains(w)))
}

fn is_lock(ty: &str) -> bool {
    is_held(ty) || is_reset(ty) || (!ty.contains("Guard") && !ty.contains("PhantomData") && ["Mutex", "RwLock", "Condvar"].iter().any(|w| ty.contains(w)))
}

fn takes_lock(body: &str, name: &str) -> bool {
    let call = format!("{name}.lock()");
    body.match_indices(&call).any(|(i, _)| {
        !body[..i].chars().next_back().is_some_and(|c| c.is_alphanumeric() || c == '_')
    })
}

#[test]
fn a_lock_name_inside_a_longer_name_is_not_a_take() {
    let body = "crate::stream::REGISTRY_SEGMENT.lock()";
    assert!(takes_lock(body, "REGISTRY_SEGMENT"));
    assert!(!takes_lock(body, "SEGMENT"));
}

fn has_test_attr(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| {
        let path = a.path().to_token_stream().to_string();
        let tokens = a.to_token_stream().to_string().replace(' ', "");
        path == "test" || (path == "cfg" && (tokens.contains("cfg(test)") || tokens.contains("cfg(all(test")))
    })
}

fn reads_pid(text: &str) -> bool {
    let t = text.replace(' ', "");
    t.contains("getpid(") || t.contains("process::id(")
}

struct Scan<'s, 'ast> {
    file: String,
    fns: Vec<(String, &'ast syn::Block)>,
    test_depth: usize,
    out: &'s mut RustScan,
}

impl<'ast> Scan<'_, 'ast> {
    fn scope(&self) -> String {
        self.fns.last().map_or("-".to_string(), |(n, _)| n.clone())
    }

    fn scoped<R>(&mut self, attrs: &[syn::Attribute], f: impl FnOnce(&mut Self) -> R) -> R {
        let t = has_test_attr(attrs) as usize;
        self.test_depth += t;
        let r = f(self);
        self.test_depth -= t;
        r
    }

    fn in_fn(&mut self, name: &syn::Ident, block: &'ast syn::Block, attrs: &[syn::Attribute], f: impl FnOnce(&mut Self)) {
        let name = name.to_string();
        self.out.fn_names.insert(name.clone());
        if PREPARE_HANDLERS.contains(&name.as_str()) {
            self.out.prepare_bodies.push_str(&block.to_token_stream().to_string().replace(' ', ""));
        }
        self.fns.push((name, block));
        self.scoped(attrs, f);
        self.fns.pop();
    }

    fn found(&self, name: &str, kind: Kind, ty: String, init: &str) -> Found {
        let body = self.fns.last().map(|(_, b)| b.to_token_stream().to_string()).unwrap_or_default();
        Found {
            id: format!("{}::{}::{}", self.file, self.scope(), name),
            kind,
            ty,
            init: init.replace(' ', ""),
            test_only: self.test_depth > 0,
            reads_pid: reads_pid(init) || reads_pid(&body),
        }
    }

    fn push(&mut self, name: &str, kind: Kind, ty: String, init: &str) {
        let f = self.found(name, kind, ty, init);
        self.out.found.push(f);
    }

    fn note_pid_read(&mut self) {
        if self.test_depth == 0 {
            let site = format!("{}::{}", self.file, self.scope());
            self.out.pid_reads.push(site);
        }
    }

    fn note_pid_tokens(&mut self, tokens: &str) {
        if tokens.contains("process :: id") || tokens.contains("getpid") {
            self.note_pid_read();
        }
    }

    fn push_thread_locals(&mut self, tokens: &str) {
        for part in tokens.split("static ").skip(1) {
            if let Some(name) = first_ident(part) {
                self.push(&name, Kind::ThreadLocal, String::new(), part);
            }
        }
    }
}

impl<'ast> Visit<'ast> for Scan<'_, 'ast> {
    fn visit_item_mod(&mut self, m: &'ast syn::ItemMod) {
        self.scoped(&m.attrs, |s| syn::visit::visit_item_mod(s, m));
    }
    fn visit_item_impl(&mut self, i: &'ast syn::ItemImpl) {
        self.scoped(&i.attrs, |s| syn::visit::visit_item_impl(s, i));
    }
    fn visit_item_fn(&mut self, f: &'ast syn::ItemFn) {
        self.in_fn(&f.sig.ident, &f.block, &f.attrs, |s| syn::visit::visit_item_fn(s, f));
    }
    fn visit_impl_item_fn(&mut self, f: &'ast syn::ImplItemFn) {
        self.in_fn(&f.sig.ident, &f.block, &f.attrs, |s| syn::visit::visit_impl_item_fn(s, f));
    }
    fn visit_item_static(&mut self, st: &'ast syn::ItemStatic) {
        let ty = st.ty.to_token_stream().to_string();
        let mutable = matches!(st.mutability, syn::StaticMutability::Mut(_));
        if mutable {
            self.out.static_muts.push(format!("{}::{}::{}", self.file, self.scope(), st.ident));
        }
        let init = st.expr.to_token_stream().to_string();
        if mutable || interior_mutable(&ty) {
            self.scoped(&st.attrs, |s| s.push(&st.ident.to_string(), Kind::of(&ty), ty.clone(), &init));
        } else {
            let f = self.scoped(&st.attrs, |s| s.found(&st.ident.to_string(), Kind::Cell, ty.clone(), &init));
            self.out.plain_statics.push(f);
        }
        syn::visit::visit_item_static(self, st);
    }
    fn visit_item_struct(&mut self, st: &'ast syn::ItemStruct) {
        let parts = st.fields.iter().map(|f| f.ty.to_token_stream().to_string());
        self.out.type_parts.entry(st.ident.to_string()).or_default().extend(parts);
        self.scoped(&st.attrs, |s| {
            for f in &st.fields {
                let ty = f.ty.to_token_stream().to_string();
                if is_lock(&ty) {
                    let field = f.ident.as_ref().map_or("0".to_string(), |i| i.to_string());
                    s.push(&format!("{}.{}", st.ident, field), Kind::FieldLock, ty, "");
                }
            }
        });
    }
    fn visit_macro(&mut self, m: &'ast syn::Macro) {
        self.note_pid_tokens(&m.tokens.to_string());
    }
    fn visit_expr_path(&mut self, p: &'ast syn::ExprPath) {
        let names: Vec<String> = p.path.segments.iter().rev().take(2).map(|s| s.ident.to_string()).collect();
        if names.first().is_some_and(|n| n == "getpid") || names == ["id", "process"] {
            self.note_pid_read();
        }
        syn::visit::visit_expr_path(self, p);
    }
    fn visit_item_type(&mut self, t: &'ast syn::ItemType) {
        let ty = t.ty.to_token_stream().to_string();
        self.out.type_parts.entry(t.ident.to_string()).or_default().push(ty);
        syn::visit::visit_item_type(self, t);
    }
    fn visit_item_macro(&mut self, m: &'ast syn::ItemMacro) {
        let path = m.mac.path.to_token_stream().to_string();
        let tokens = m.mac.tokens.to_string();
        self.note_pid_tokens(&tokens);
        if path.ends_with("thread_local") {
            self.scoped(&m.attrs, |s| s.push_thread_locals(&tokens));
        } else if path.ends_with("macro_rules") && tokens.split_whitespace().any(|t| t == "static") {
            let name = m.ident.as_ref().map_or("?".to_string(), |i| i.to_string());
            self.out.macro_statics.push(format!("{}::{}", self.file, name));
        }
    }
    fn visit_stmt_macro(&mut self, m: &'ast syn::StmtMacro) {
        let tokens = m.mac.tokens.to_string();
        self.note_pid_tokens(&tokens);
        if m.mac.path.to_token_stream().to_string().ends_with("thread_local") {
            self.push_thread_locals(&tokens);
        }
    }
    fn visit_expr_call(&mut self, c: &'ast syn::ExprCall) {
        if let syn::Expr::Path(p) = &*c.func {
            let call = Call {
                site: format!("{}::{}", self.file, self.scope()),
                path: p.path.to_token_stream().to_string().replace(' ', ""),
                args: c.args.to_token_stream().to_string(),
                test_only: self.test_depth > 0,
            };
            self.out.calls.push(call);
        }
        syn::visit::visit_expr_call(self, c);
    }
}

fn test_only_modules(src: &Path) -> HashSet<String> {
    ["lib.rs", "main.rs"]
        .iter()
        .filter_map(|f| std::fs::read_to_string(src.join(f)).ok())
        .flat_map(|text| syn::parse_file(&text).unwrap().items)
        .filter_map(|i| match i {
            syn::Item::Mod(m) if m.content.is_none() && has_test_attr(&m.attrs) => Some(m.ident.to_string()),
            _ => None,
        })
        .collect()
}

fn idents(ty: &str) -> Vec<&str> {
    ty.split(|c: char| !is_ident_char(c)).filter(|w| !w.is_empty()).collect()
}

fn interior_types(parts: &HashMap<String, Vec<String>>) -> HashSet<String> {
    let named: Vec<(&String, bool, Vec<&str>)> = parts
        .iter()
        .map(|(n, fs)| (n, fs.iter().any(|f| interior_mutable(f)), fs.iter().flat_map(|f| idents(f)).collect()))
        .collect();
    let mut interior: HashSet<String> = HashSet::new();
    loop {
        let more: Vec<String> = named
            .iter()
            .filter(|(n, own, used)| !interior.contains(*n) && (*own || used.iter().any(|i| interior.contains(*i))))
            .map(|(n, _, _)| n.to_string())
            .collect();
        if more.is_empty() {
            return interior;
        }
        interior.extend(more);
    }
}

#[test]
fn a_static_of_a_type_that_nests_a_lock_is_found() {
    let parts: HashMap<String, Vec<String>> = [
        ("Inner".to_string(), vec!["std :: sync :: atomic :: AtomicUsize".to_string()]),
        ("Outer".to_string(), vec!["Option < Inner >".to_string()]),
        ("Table".to_string(), vec!["[Outer ; 4]".to_string()]),
        ("Plain".to_string(), vec!["u32".to_string()]),
    ]
    .into_iter()
    .collect();
    let interior = interior_types(&parts);
    assert!(["Inner", "Outer", "Table"].iter().all(|n| interior.contains(*n)));
    assert!(!interior.contains("Plain"));
}

fn scan_rust() -> &'static RustScan {
    static SCAN: std::sync::OnceLock<RustScan> = std::sync::OnceLock::new();
    SCAN.get_or_init(scan_rust_sources)
}

fn scan_rust_sources() -> RustScan {
    let root = rust_root();
    let mut out = RustScan::default();
    for krate in CRATES {
        let src = root.join(krate).join("src");
        let test_mods = test_only_modules(&src);
        for f in walk(&src, &["rs"], &[]) {
            let text = std::fs::read_to_string(&f).unwrap();
            let parsed = syn::parse_file(&text).unwrap_or_else(|e| panic!("{}: {e}", f.display()));
            let stem = f.file_stem().unwrap().to_string_lossy().to_string();
            let mut scan = Scan {
                file: f.strip_prefix(&root).unwrap().to_string_lossy().replace("/src/", "/"),
                fns: Vec::new(),
                test_depth: test_mods.contains(&stem) as usize,
                out: &mut out,
            };
            scan.visit_file(&parsed);
        }
    }
    let interior = interior_types(&out.type_parts);
    let plain = std::mem::take(&mut out.plain_statics);
    for f in plain {
        if idents(&f.ty).iter().any(|i| interior.contains(*i)) {
            out.found.push(f);
        }
    }
    out
}

// -- binder sources and code templates ----------------------------------------

fn other_found(file: &Path, scope: &str, name: String, kind: Kind) -> Found {
    Found {
        id: format!("{}::{}::{}", file.strip_prefix(repo_root()).unwrap().display(), scope, name),
        kind,
        ty: String::new(),
        init: String::new(),
        test_only: false,
        reads_pid: false,
    }
}

fn scan_c(out: &mut Vec<Found>) {
    let locks = ["std::mutex", "std::once_flag", "pthread_mutex_t", "pthread_once_t", "std::condition_variable"];
    for f in walk(&repo_root().join("data/lang"), &["c", "cpp", "hpp", "h"], &["nanoarrow", "/julia"]) {
        let text = std::fs::read_to_string(&f).unwrap();
        let mut scope = "-".to_string();
        for line in text.lines() {
            let at_col0 = !line.starts_with(char::is_whitespace);
            let trimmed = line.trim();
            if at_col0 && line.contains('(') && !trimmed.ends_with(';') && !trimmed.starts_with('#') {
                if let Some(name) = last_ident(line.split('(').next().unwrap()) {
                    scope = name;
                }
            }
            if at_col0 && trimmed == "}" {
                scope = "-".to_string();
            }
            for lock in locks {
                if let Some(name) = trimmed.split(lock).nth(1).and_then(first_ident) {
                    out.push(other_found(&f, &scope, name, Kind::Lock));
                }
            }
            let decl = ["static ", "thread_local ", "__thread "].iter().any(|k| trimmed.starts_with(k));
            let constant = ["static const ", "static constexpr ", "static inline "].iter().any(|k| trimmed.starts_with(k));
            let head = trimmed.split(['=', ';', '[']).next().unwrap();
            if decl && !constant && !head.contains('(') {
                if let Some(name) = last_ident(head) {
                    let local = if at_col0 { "-".to_string() } else { scope.clone() };
                    let per_thread = trimmed.contains("__thread") || trimmed.contains("thread_local");
                    out.push(other_found(&f, &local, name, if per_thread { Kind::ThreadLocal } else { Kind::Cell }));
                }
            }
        }
    }
}

fn scan_py(out: &mut Vec<Found>) {
    let ctors = [
        "threading.Lock(", "threading.RLock(", "threading.Condition(", "threading.Event(",
        "threading.Semaphore(", "threading.BoundedSemaphore(", "queue.Queue(", "queue.SimpleQueue(",
    ];
    for f in walk(&repo_root().join("data/lang"), &["py"], &["/julia"]) {
        for line in std::fs::read_to_string(&f).unwrap().lines() {
            if let Some((lhs, rhs)) = line.split_once('=') {
                if ctors.iter().any(|k| rhs.trim_start().starts_with(k)) {
                    out.push(other_found(&f, "-", lhs.trim().to_string(), Kind::Lock));
                }
            }
        }
    }
}

fn scan_templates(out: &mut Vec<Found>) {
    for f in walk(&repo_root().join("library"), &["hs"], &[]) {
        for line in std::fs::read_to_string(&f).unwrap().lines() {
            let t = line.trim_start();
            if t.starts_with("--") {
                continue;
            }
            for part in t.split('"').skip(1).step_by(2) {
                for (at, _) in part.match_indices("static ") {
                    let before = part[..at].trim_end();
                    if !(before.is_empty() || before.ends_with('{') || before.ends_with(';')) {
                        continue;
                    }
                    let rest = &part[at + "static ".len()..];
                    let end = rest.find(['=', ';', '(']).unwrap_or(rest.len());
                    if rest[end..].starts_with('(') {
                        continue;
                    }
                    let head = &rest[..end];
                    let rust_style = head.contains(':') && !head.contains("::");
                    let name = if rust_style { last_ident(head.split(':').next().unwrap()) } else { last_ident(head) };
                    if let Some(name) = name {
                        out.push(other_found(&f, "-", name, Kind::Template));
                    }
                }
            }
        }
    }
}

fn all_found(scan: &RustScan) -> Vec<Found> {
    let mut found = scan.found.clone();
    scan_c(&mut found);
    scan_py(&mut found);
    scan_templates(&mut found);
    found
}

// -- the state registry -------------------------------------------------------

struct Row {
    id: String,
    class: String,
    rank: Option<u32>,
    cites: Vec<String>,
}

fn table_after(heading: &str) -> Vec<Vec<String>> {
    let text = std::fs::read_to_string(model_dir().join("state.md")).unwrap();
    let Some(start) = text.find(heading) else { return Vec::new() };
    text[start..]
        .lines()
        .skip(1)
        .skip_while(|l| !l.starts_with('|'))
        .take_while(|l| l.starts_with('|'))
        .skip(2)
        .map(|l| l.trim_matches('|').split('|').map(|c| c.trim().to_string()).collect())
        .collect()
}

const REGISTRY_HEADER: &str = "id\tclass\trank\tcites";

fn parse_registry(text: &str) -> Result<Vec<Row>, String> {
    let mut lines = text.lines().enumerate();
    match lines.next() {
        Some((_, REGISTRY_HEADER)) => {}
        other => return Err(format!("line 1: expected header {REGISTRY_HEADER:?}, found {:?}", other.map(|(_, l)| l))),
    }
    lines
        .map(|(i, line)| {
            let at = i + 1;
            let c: Vec<&str> = line.split('\t').collect();
            let [id, class, rank, cites] = c[..] else {
                return Err(format!("line {at}: expected 4 tab-separated fields, found {}", c.len()));
            };
            if id.is_empty() || class.is_empty() {
                return Err(format!("line {at}: id and class are required"));
            }
            let rank = if rank.is_empty() {
                None
            } else {
                Some(rank.parse().map_err(|_| format!("line {at}: rank {rank:?} is not a number"))?)
            };
            Ok(Row {
                id: id.to_string(),
                class: class.to_string(),
                rank,
                cites: cites.split(',').map(|s| s.trim().to_string()).filter(|s| !s.is_empty()).collect(),
            })
        })
        .collect()
}

fn rows() -> Vec<Row> {
    let text = std::fs::read_to_string(model_dir().join("registry.tsv")).unwrap();
    parse_registry(&text).unwrap_or_else(|e| panic!("model/registry.tsv: {e}"))
}

#[test]
fn the_registry_parser_rejects_malformed_rows() {
    let ok = format!("{REGISTRY_HEADER}\na::-::X\theld\t3\tFORK-5, FORK-10\nb::-::Y\tcounter\t\t\n");
    let rows = parse_registry(&ok).unwrap();
    assert_eq!((rows[0].rank, rows[0].cites.len(), rows[1].rank), (Some(3), 2, None));
    for bad in [
        "id class rank cites\na\theld\t\t\n".to_string(),
        format!("{REGISTRY_HEADER}\na\theld\t\n"),
        format!("{REGISTRY_HEADER}\na\theld\tx\t\n"),
        format!("{REGISTRY_HEADER}\n\theld\t\t\n"),
        format!("{REGISTRY_HEADER}\n\n"),
    ] {
        assert!(parse_registry(&bad).is_err(), "accepted {bad:?}");
    }
}

fn fork_sites() -> Vec<(String, String)> {
    table_after("## Fork sites").into_iter().map(|c| (c[0].clone(), c[1].clone())).collect()
}

fn production_calls<'a>(scan: &'a RustScan, path: &'a str) -> impl Iterator<Item = &'a Call> {
    scan.calls.iter().filter(move |c| !c.test_only && c.path == path)
}

// -- rules --------------------------------------------------------------------

#[test]
fn no_crate_declares_a_static_mut() {
    let scan = scan_rust();
    assert!(scan.static_muts.is_empty(), "static mut declared at:\n{}", scan.static_muts.join("\n"));
}

#[test]
fn every_descriptor_is_created_close_on_exec() {
    let raw = ["pipe", "socket", "socketpair", "accept", "dup", "mkstemp", "fopen", "popen"];
    let scan = scan_rust();
    let mut found = Vec::new();
    for c in scan.calls.iter().filter(|c| !c.site.starts_with("morloc-runtime-types/fd.rs")) {
        let Some(f) = c.path.strip_prefix("libc::") else { continue };
        if raw.contains(&f) {
            found.push(format!("{}: libc::{f}", c.site));
        } else if (f == "open" || f == "openat") && !c.args.contains("O_CLOEXEC") {
            found.push(format!("{}: libc::{f} without O_CLOEXEC", c.site));
        }
    }
    assert!(found.is_empty(), "descriptors not created close-on-exec:\n{}", found.join("\n"));
}

#[test]
fn model_items_are_checked() {
    let scan = scan_rust();
    let items = model_items();
    let repo = repo_root();
    let mut problems = Vec::new();
    let mut ids: HashMap<&str, &Path> = HashMap::new();
    for item in &items {
        let at = item.path.display();
        if let Some(prev) = ids.insert(&item.id, &item.path) {
            problems.push(format!("{} defined in {} and {at}", item.id, prev.display()));
        }
        match item.status.as_deref() {
            Some("implemented") => {
                if item.checks.is_empty() {
                    problems.push(format!("{} ({at}) is implemented but names no test", item.id));
                }
                for check in &item.checks {
                    let exists = if let Some(dir) = check.strip_prefix("golden:") {
                        repo.join("test-suite/golden-tests").join(dir).join("Makefile").exists()
                    } else if let Some(cfg) = check.strip_prefix("tla:") {
                        model_dir().join("tla").join(format!("{cfg}.cfg")).exists()
                    } else {
                        scan.fn_names.contains(check)
                    };
                    if !exists {
                        problems.push(format!("{} ({at}) names {check}, which does not exist", item.id));
                    }
                }
            }
            Some("deviation") | Some("retired") => {
                if !item.checks.is_empty() {
                    problems.push(format!("{} ({at}) is not implemented but names tests", item.id));
                }
            }
            other => problems.push(format!("{} ({at}) has status {other:?}", item.id)),
        }
    }
    for file in rust_sources() {
        let text = std::fs::read_to_string(&file).unwrap();
        for (i, line) in text.lines().enumerate() {
            let Some(comment) = line.trim_start().strip_prefix("// ") else { continue };
            let comment = comment.strip_prefix("SAFETY: ").unwrap_or(comment);
            if let Some(id) = item_id(comment) {
                if comment[id.len()..].starts_with(':') && !ids.contains_key(id) {
                    problems.push(format!("{}:{} cites {id}, which no model item defines", file.display(), i + 1));
                }
            }
        }
    }
    assert!(items.len() >= 20, "found only {} model items", items.len());
    assert!(problems.is_empty(), "model problems:\n{}", problems.join("\n"));
}

#[test]
fn every_process_wide_value_is_registered() {
    let rows = rows();
    let ids: HashSet<&str> = rows.iter().map(|r| r.id.as_str()).collect();
    let missing: Vec<String> = all_found(&scan_rust())
        .into_iter()
        .filter(|f| !ids.contains(f.id.as_str()))
        .map(|f| format!("{}\t?\t\t   ({:?}{})", f.id, f.kind, if f.test_only { ", test-only" } else { "" }))
        .collect();
    assert!(missing.is_empty(), "process-wide values missing from model/registry.tsv:\n{}", missing.join("\n"));
}

#[test]
fn every_registry_row_names_a_value_in_code() {
    let found: HashSet<String> = all_found(&scan_rust()).into_iter().map(|f| f.id).collect();
    let stale: Vec<String> = rows().into_iter().filter(|r| !found.contains(&r.id)).map(|r| r.id).collect();
    assert!(stale.is_empty(), "model/registry.tsv rows with no value in code:\n{}", stale.join("\n"));
}

#[test]
fn registry_rows_obey_their_class() {
    let scan = scan_rust();
    let found: HashMap<String, Found> = all_found(&scan).into_iter().map(|f| (f.id.clone(), f)).collect();
    let deviations: HashSet<String> =
        model_items().into_iter().filter(|i| i.status.as_deref() == Some("deviation")).map(|i| i.id).collect();
    let mut problems = Vec::new();
    let mut ranks = HashMap::new();
    for r in rows() {
        if !CLASSES.contains(&r.class.as_str()) {
            problems.push(format!("{}: unknown class {}", r.id, r.class));
        }
        for c in r.cites.iter().filter(|c| !deviations.contains(*c)) {
            problems.push(format!("{}: cites {c}, which is not a deviation item", r.id));
        }
        if (r.class == "held") != r.rank.is_some() {
            problems.push(format!("{}: a rank is given exactly for held rows", r.id));
        }
        if let Some(other) = r.rank.and_then(|rank| ranks.insert(rank, r.id.clone())) {
            problems.push(format!("{}: rank also used by {other}", r.id));
        }
        let Some(f) = found.get(&r.id) else { continue };
        if (r.class == "test-only") != f.test_only {
            problems.push(format!("{}: class {} but test-only in code is {}", r.id, r.class, f.test_only));
        }
        if f.kind == Kind::Cell && f.reads_pid && !["fork-scoped", "exec-only", "test-only"].contains(&r.class.as_str()) {
            problems.push(format!("{}: caches a value derived from the process id but is {}", r.id, r.class));
        }
        if r.class != "held" && is_held(&f.ty) {
            problems.push(format!("{}: declared Held but its class is {}", r.id, r.class));
        }
        if !["reset", "test-only"].contains(&r.class.as_str()) && is_reset(&f.ty) {
            problems.push(format!("{}: declared Reset but its class is {}", r.id, r.class));
        }
        if r.class == "reset" && !r.cites.iter().any(|c| c == "FORK-8") && !is_reset(&f.ty) {
            problems.push(format!("{}: reset but not declared Reset", r.id));
        }
        if r.class == "held" && !r.cites.iter().any(|c| c == "FORK-5") {
            let name = r.id.rsplit("::").next().unwrap();
            if !is_held(&f.ty) {
                problems.push(format!("{}: held but not declared Held", r.id));
            } else if !f
                .init
                .trim_start_matches("crate::fork_policy::")
                .starts_with(&format!("Held::new({},", r.rank.unwrap()))
            {
                problems.push(format!("{}: its Held rank differs from the registry's {}", r.id, r.rank.unwrap()));
            }
            if !takes_lock(&scan.prepare_bodies, name) {
                problems.push(format!("{}: held but no prepare handler takes it", r.id));
            }
        }
    }
    for m in &scan.macro_statics {
        problems.push(format!("{m}: a macro declares a static, which the registry cannot see"));
    }
    let sites = fork_sites();
    let listed: HashSet<&str> = sites.iter().map(|(s, _)| s.as_str()).collect();
    let forking: HashSet<&str> = production_calls(&scan, "libc::fork").map(|c| c.site.as_str()).collect();
    for site in forking.difference(&listed) {
        problems.push(format!("{site}: forks but is not in the fork sites table"));
    }
    for (site, child) in &sites {
        if !["exec", "signal-safe"].contains(&child.as_str()) {
            problems.push(format!("{site}: unknown child kind {child}"));
        }
        if !forking.contains(site.as_str()) {
            problems.push(format!("{site}: listed as a fork site but does not fork"));
        }
    }
    assert!(problems.is_empty(), "model/registry.tsv disagrees with the code:\n{}", problems.join("\n"));
}

#[test]
fn deviating_rows_only_decrease() {
    let deviating = rows().iter().filter(|r| !r.cites.is_empty()).count();
    assert!(deviating <= MAX_DEVIATING_ROWS, "{deviating} rows cite deviations; the limit is {MAX_DEVIATING_ROWS}");
    let scan = scan_rust();
    let env_reads = scan
        .calls
        .iter()
        .filter(|c| !c.test_only && (c.path.ends_with("env::var") || c.path.ends_with("env::var_os")))
        .count();
    assert!(env_reads <= MAX_ENV_READS, "{env_reads} environment reads outside tests; the limit is {MAX_ENV_READS}");
}

#[test]
fn only_the_runtime_library_uses_generation_keyed_state() {
    let banned = ["process::token", "owner_word", "OwnerWord", "fork_generation::", "shm_lock", "ShmLock", "recoverable_lock", "RecoverableLock"];
    let mut uses = Vec::new();
    for krate in ["rustmorloc", "morloc-nexus"] {
        for f in walk(&rust_root().join(krate).join("src"), &["rs"], &[]) {
            let text = std::fs::read_to_string(&f).unwrap();
            for b in banned.iter().filter(|b| text.contains(*b)) {
                uses.push(format!("{}: {b}", f.display()));
            }
        }
    }
    assert!(
        uses.is_empty(),
        "FORK-14: these crates link their own copy of the fork generation, which no fork handler changes:\n{}",
        uses.join("\n")
    );
}

#[test]
fn every_process_id_read_is_reviewed() {
    let scan = scan_rust();
    let reads: HashSet<&str> = scan.pid_reads.iter().map(|s| s.as_str()).collect();
    let mut binder_counts: HashMap<String, usize> = HashMap::new();
    for f in walk(&repo_root().join("data/lang"), &["c", "cpp", "hpp", "h", "py", "R", "rs"], &["/julia", "nanoarrow"]) {
        let text = std::fs::read_to_string(&f).unwrap();
        let n = text.lines().filter(|l| l.contains("getpid(") || l.contains("os.getpid") || l.contains("Sys.getpid")).count();
        if n > 0 {
            binder_counts.insert(f.strip_prefix(repo_root()).unwrap().display().to_string(), n);
        }
    }
    let expected: HashMap<String, usize> = BINDER_PID_READS.iter().map(|(f, n)| (f.to_string(), *n)).collect();
    assert_eq!(binder_counts, expected, "FORK-14: binder process-id reads changed");
    let allowed: HashSet<&str> = PID_READ_SITES.iter().copied().collect();
    let new: Vec<&&str> = reads.difference(&allowed).collect();
    let stale: Vec<&&str> = allowed.difference(&reads).collect();
    assert!(
        new.is_empty() && stale.is_empty(),
        "FORK-14: state inherited across fork is owned by fork generation; a process id names a process \
         to others (shared memory, files, logs). Unreviewed reads: {new:?}; listed but gone: {stale:?}"
    );
}
