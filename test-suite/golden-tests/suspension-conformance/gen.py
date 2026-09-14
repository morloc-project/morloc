#!/usr/bin/env python3
"""Generate the suspension conformance matrix.

Every cell is a program with a suspension whose computation lives in one
pool (home) and is forced three times from another (away). The
computation appends one byte to a counter file, so the count after the
last force is the number of runs: 3 in every position, 4 where a
suspension of a suspension is forced once and its inner thunk three
times. Positions cover the spec in /work/plans/suspension-crossing-protocol.md:

  rootResult    a suspension returned by a call across pools, forced by pool code
  rootArg       a suspension handed to a host, which runs it
  recordField   a suspension inside a record handed to a host
  listElem      a suspension inside a list handed to a host
  callback      a function with a suspension result handed to a host, which applies it
  pinned        a record of such functions built for a host, whose bodies call across
  relay         a suspension passed through the away pool and back to a home host
  closureResult a function with a suspension result returned across pools, applied by pool code
  nestedSusp    a suspension of a suspension returned across pools

The cells whose forcing side is pool code (rootResult, closureResult,
nestedSusp) leave the placement of that code to the realizer, which may run
the whole cell in the home pool; the host-forced cells always cross.

Run `python3 gen.py` to regenerate; the outputs are checked in.
"""

import os

LANGS = ["py", "cpp", "r", "rust"]
LANG_NAME = {"py": "Py", "cpp": "Cpp", "r": "R", "rust": "Rust"}
HOST_FILE = {"py": "host.py", "cpp": "host.hpp", "r": "host.R", "rust": "host.rs"}
RECORD_FORM = {
    "py": {"Holder": "dict", "Caller": "dict"},
    "r": {"Holder": "list", "Caller": "list"},
    "cpp": {"Holder": "Holder", "Caller": "Caller"},
    "rust": {"Holder": "Holder", "Caller": "Caller"},
}

CELLS = ["rootResult", "rootArg", "recordField", "listElem", "callback", "pinned", "relay"]
NESTED = ["closureResult", "nestedSusp"]
EXPECT = {c: 3 for c in CELLS + NESTED}
EXPECT["nestedSusp"] = 4

HOST_PY = '''import os

def tick(path):
    with open(path, "ab") as f:
        f.write(b"x")
    return os.path.getsize(path)

def mark(s):
    return s

def ident(x):
    return x

def take_thunk(t):
    t()
    t()
    return t()

def use_record(h):
    h["run"]()
    h["run"]()
    return h["run"]()

def use_list(xs):
    xs[0]()
    xs[0]()
    return xs[0]()

def use_callback(f, x):
    f(x)
    f(x)
    return f(x)

def use_pinned(c, x):
    c["inc"](x)
    c["inc"](x)
    return c["inc"](x)
'''

HOST_R = '''tick <- function(path) {
  cat("x", file = path, append = TRUE)
  as.integer(file.info(path)$size)
}

mark <- function(s) s

ident <- function(x) as.integer(x)

take_thunk <- function(t) {
  t()
  t()
  as.integer(t())
}

use_record <- function(h) {
  h[["run"]]()
  h[["run"]]()
  as.integer(h[["run"]]())
}

use_list <- function(xs) {
  xs[[1]]()
  xs[[1]]()
  as.integer(xs[[1]]())
}

use_callback <- function(f, x) {
  f(x)
  f(x)
  as.integer(f(x))
}

use_pinned <- function(c, x) {
  c[["inc"]](x)
  c[["inc"]](x)
  as.integer(c[["inc"]](x))
}
'''

HOST_CPP = '''#pragma once
#include <fstream>
#include <functional>
#include <string>
#include <sys/stat.h>
#include <vector>

struct Holder {
  std::function<int()> run;
};

struct Caller {
  std::function<int(int)> inc;
};

inline int tick(std::string path) {
  {
    std::ofstream f(path, std::ios::app | std::ios::binary);
    f << "x";
  }
  struct stat st;
  stat(path.c_str(), &st);
  return static_cast<int>(st.st_size);
}

inline std::string mark(std::string s) { return s; }

inline int ident(int x) { return x; }

inline int take_thunk(std::function<int()> t) {
  t();
  t();
  return t();
}

inline int use_record(Holder h) {
  h.run();
  h.run();
  return h.run();
}

inline int use_list(std::vector<std::function<int()>> xs) {
  xs[0]();
  xs[0]();
  return xs[0]();
}

inline int use_callback(std::function<int(int)> f, int x) {
  f(x);
  f(x);
  return f(x);
}

inline int use_pinned(Caller c, int x) {
  c.inc(x);
  c.inc(x);
  return c.inc(x);
}
'''

HOST_RS = '''#[derive(Clone)]
pub struct Holder {
    pub run: std::rc::Rc<dyn rustmorloc::MorlocFn0<i64>>,
}

#[derive(Clone)]
pub struct Caller {
    pub inc: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
}

pub fn tick(path: &String) -> i64 {
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(path)
        .unwrap();
    f.write_all(b"x").unwrap();
    std::fs::metadata(path).unwrap().len() as i64
}

pub fn mark(s: &String) -> String {
    s.clone()
}

pub fn ident(x: i64) -> i64 {
    x
}

pub fn take_thunk(t: impl rustmorloc::MorlocFn0<i64>) -> i64 {
    t.call0();
    t.call0();
    t.call0()
}

pub fn use_record(h: &Holder) -> i64 {
    h.run.call0();
    h.run.call0();
    h.run.call0()
}

pub fn use_list(xs: &Vec<std::rc::Rc<dyn rustmorloc::MorlocFn0<i64>>>) -> i64 {
    xs[0].call0();
    xs[0].call0();
    xs[0].call0()
}

pub fn use_callback(f: impl rustmorloc::MorlocFn1<i64, i64>, x: i64) -> i64 {
    f.call1(&x);
    f.call1(&x);
    f.call1(&x)
}

pub fn use_pinned(c: &Caller, x: i64) -> i64 {
    c.inc.call1(&x);
    c.inc.call1(&x);
    c.inc.call1(&x)
}
'''

HOSTS = {"py": HOST_PY, "r": HOST_R, "cpp": HOST_CPP, "rust": HOST_RS}


def header(home, away, cells):
    return f'''-- Suspension conformance: home={home} away={away}.
-- The computation (tickH, one byte appended to a counter file) lives in the
-- home pool; every cell forces it three times from the away side and prints
-- the count. Generated by gen.py; do not edit by hand.
module main ({", ".join(cells)})

'''


def imports(home, away):
    roots = sorted({home, away})
    return "".join(f"import root-{l}\n" for l in roots) + "\n"


def decls(home, away):
    H, A = LANG_NAME[home], LANG_NAME[away]
    out = []
    out.append("record Holder where\n  run :: <IO> Int\n\n")
    out.append("record Caller where\n  inc :: Int -> <IO> Int\n\n")
    for l in sorted({home, away}):
        for rec in ("Holder", "Caller"):
            out.append(f'record {LANG_NAME[l]} => {rec} = "{RECORD_FORM[l][rec]}"\n')
    out.append("\n")
    out.append(f'source {H} from "{HOST_FILE[home]}"\n'
               f'  ( "tick" as tickH\n  , "mark" as markH\n  , "ident" as identH\n  , "take_thunk" as takeThunkH\n  )\n')
    out.append(f'source {A} from "{HOST_FILE[away]}"\n'
               f'  ( "ident" as identA\n  , "take_thunk" as takeThunkA\n  , "use_record" as useRecordA\n'
               f'  , "use_list" as useListA\n  , "use_callback" as useCallbackA\n  , "use_pinned" as usePinnedA\n  )\n\n')
    out.append('''tickH :: Str -> <IO> Int
markH :: Str -> Str
identH :: Int -> Int
takeThunkH :: <IO> Int -> <IO> Int
identA :: Int -> Int
takeThunkA :: <IO> Int -> <IO> Int
useRecordA :: Holder -> <IO> Int
useListA :: [<IO> Int] -> <IO> Int
useCallbackA :: (Int -> <IO> Int) -> Int -> <IO> Int
usePinnedA :: Caller -> Int -> <IO> Int

-- the computation under test: morloc-defined, its body in the home pool
homeEff :: Str -> Int -> <IO> Int
homeEff p x = do
  n <- tickH p
  identH n

''')
    return "".join(out)


CELL_BODY = {
    "rootResult": '''rootResult :: Str -> <IO> Int
rootResult p = do
  let t = homeEff p 0
  _ <- t
  _ <- t
  c <- t
  identA c
''',
    "rootArg": '''rootArg :: Str -> <IO> Int
rootArg p = takeThunkA (homeEff p 0)
''',
    "recordField": '''recordField :: Str -> <IO> Int
recordField p = useRecordA { run = homeEff p 0 }
''',
    "listElem": '''listElem :: Str -> <IO> Int
listElem p = useListA [homeEff p 0]
''',
    "callback": '''callback :: Str -> <IO> Int
callback p = useCallbackA (\\x -> homeEff p x) 0
''',
    "pinned": '''pinned :: Str -> <IO> Int
pinned p = usePinnedA { inc = \\x -> homeEff p x } 0
''',
    "relay": '''-- the thunk goes home -> away pool code -> home host
awayPass :: <IO> Int -> <IO> Int
awayPass t = do
  let _z = identA 1
  t

relay :: Str -> <IO> Int
relay p = takeThunkH (awayPass (homeEff p 0))
''',
    "closureResult": '''-- a function value with a suspension result, built at home
mkF :: Str -> Int -> <IO> Int
mkF p = let q = markH p in \\x -> homeEff q x

closureResult :: Str -> <IO> Int
closureResult p = do
  let f = mkF p
  _ <- f 1
  _ <- f 1
  c <- f 1
  identA c
''',
    "nestedSusp": '''-- the outer run ticks once and yields the inner thunk
prepare :: Str -> <IO> (<IO> Int)
prepare p = do
  _ <- tickH p
  homeEff p 0

nestedSusp :: Str -> <IO> Int
nestedSusp p = do
  stmt <- prepare p
  _ <- stmt
  _ <- stmt
  c <- stmt
  identA c
''',
}


def program(home, away, cells):
    return header(home, away, cells) + imports(home, away) + decls(home, away) + "\n".join(CELL_BODY[c] for c in cells)


def main():
    here = os.path.dirname(os.path.abspath(__file__))
    for lang, src in HOSTS.items():
        with open(os.path.join(here, HOST_FILE[lang]), "w") as f:
            f.write(src)
    programs = []  # (name, cells)
    for home in LANGS:
        for away in LANGS:
            base = f"{home}-{away}"
            # The nested cells get their own program where Rust is involved,
            # so a build failure there cannot take the other cells down.
            if "rust" in (home, away):
                programs.append((f"pair-{base}", CELLS))
                programs.append((f"nested-{base}", NESTED))
            else:
                programs.append((f"pair-{base}", CELLS + NESTED))
    for name, cells in programs:
        home, away = name.split("-")[1:]
        with open(os.path.join(here, name + ".loc"), "w") as f:
            f.write(program(home, away, cells))
    with open(os.path.join(here, "Makefile"), "w") as f:
        f.write("PROGRAMS = " + " ".join(n for n, _ in programs) + "\n\n")
        f.write("all:\n\t: > obs.txt\n\trm -f *.err cnt-*\n")
        for name, cells in programs:
            f.write(f"\tmorloc make -o {name} {name}.loc 2>> build.err || echo \"--- {name} --- BUILD FAILED\" >> obs.txt\n")
            for c in cells:
                f.write(f"\techo \"--- {name} {c} ---\" >> obs.txt\n")
                f.write(f"\t[ -x {name} ] && ./{name} {c} $$PWD/cnt-{name}-{c} >> obs.txt 2>> obs.err || true\n")
        f.write("\nclean:\n\trm -rf $(PROGRAMS) __pycache__ *.err cnt-*\n\trm -rf *-build *build.tmp*\n")
    with open(os.path.join(here, "exp.txt"), "w") as f:
        for name, cells in programs:
            for c in cells:
                f.write(f"--- {name} {c} ---\n{EXPECT[c]}\n")


if __name__ == "__main__":
    main()
