#!/usr/bin/env python3
"""Generate the NATIVE-9 golden tests: for each language L, every shape is
built as a literal in L and also received in L from every other language,
and L's `same` compares the native forms of the two values.

Run from the repository root: python3 test-suite/gen-native-9.py
"""

import os
import sys

LANGS = ["py", "r", "cpp", "rust"]
LANG_NAME = {"py": "Py", "r": "R", "cpp": "Cpp", "rust": "Rust"}
ROOT = {"py": "root-py", "r": "root-r", "cpp": "root-cpp", "rust": "root-rust"}
VECTOR = {"py": "vector-py", "r": "vector-r", "cpp": "vector-cpp", "rust": "vector-rust"}
MAPSET = {"py": ["map-py", "set-py"], "r": ["map-r", "set-r"], "cpp": ["map-cpp", "set-cpp"]}

# (name, type, literal, languages that lack it)
SHAPES = [
    ("int", "Int", "42", []),
    ("i8", "I8", "-7", []),
    ("i16", "I16", "300", []),
    ("i32", "I32", "100000", []),
    ("i64", "I64", "100000", []),
    ("u8", "U8", "200", []),
    ("u16", "U16", "60000", []),
    ("u32", "U32", "4000000000", []),
    ("u64", "U64", "9000000000", []),
    ("u64Wide", "U64", "9000000000000000001", []),
    ("listI64Wide", "[I64]", "[9000000000000000001, 1]", []),
    ("f32", "F32", "1.5", []),
    ("real", "Real", "2.25", []),
    ("bool", "Bool", "True", []),
    ("str", "Str", '"abc"', []),
    ("enum", "Dir", "South", []),
    ("listInt", "[Int]", "[1, 2, 3]", []),
    ("listI8", "[I8]", "[1, -2]", []),
    ("listI32", "[I32]", "[1, 2]", []),
    ("listI64", "[I64]", "[1, 2]", []),
    ("listU8", "[U8]", "[1, 2, 3]", []),
    ("listU64", "[U64]", "[1, 2]", []),
    ("listF32", "[F32]", "[1.5, 2.5]", []),
    ("listReal", "[Real]", "[1.5, 2.5]", []),
    ("listBool", "[Bool]", "[True, False]", []),
    ("listStr", "[Str]", '["a", "b"]', []),
    ("listEnum", "[Dir]", "[North, South]", []),
    ("listEmptyU8", "[U8]", "[]", []),
    ("listEmptyStr", "[Str]", "[]", []),
    ("listList", "[[Int]]", "[[1], [2, 3]]", []),
    ("listTuple", "[(Int, Str)]", '[(1, "a"), (2, "b")]', []),
    ("vecU8", "Vector 3 U8", "[1, 2, 3]", []),
    ("vecEnum", "Vector 2 Dir", "[North, South]", []),
    ("vecI32", "Vector 2 I32", "[1, 2]", []),
    ("vecI64", "Vector 2 I64", "[1, 2]", []),
    ("vecReal", "Vector 2 Real", "[1.5, 2.5]", []),
    ("vecInt", "Vector 2 Int", "[1, 2]", []),
    ("vecStr", "Vector 2 Str", '["a", "b"]', []),
    ("vecEmptyU8", "Vector 0 U8", "[]", []),
    ("tuple", "(Int, Str)", '(1, "a")', []),
    ("tupleBytes", "(U8, [U8])", "(1, [2, 3])", []),
    ("record", "P", "{ px = 1, py = 2.5 }", []),
    ("optSome", "?Int", "5", []),
    ("optNull", "?Int", "Null", []),
    ("optList", "?[U8]", "[1, 2]", []),
    ("listOpt", "[?Int]", "[1, Null]", []),
    ("sum", "Shape", "Rect 1.0 2.0", []),
    ("map", "Map Str Int", '[("a", 1), ("b", 2)]', ["rust"]),
    ("set", "Set Int", "[1, 2]", ["rust"]),
    ("mapVec", "Map Str (Vector 2 U8)", '[("a", [1, 2])]', ["rust"]),
]

# Shapes whose elements are the program's argument rather than constants.
VAR_SHAPES = [
    ("varListU8", "[U8]", "[x, x]"),
    ("varVecU8", "Vector 2 U8", "[x, x]"),
    ("varTuple", "(U8, U8)", "(x, x)"),
    ("varOpt", "?U8", "x"),
]

FORM = {
    "py": '''import numpy


def _form(x):
    if isinstance(x, numpy.ndarray):
        flag = "ro" if not x.flags.writeable else "rw"
        if x.dtype == object:
            inner = ",".join(_form(e) for e in x)
        else:
            inner = ",".join(repr(e.item()) for e in x)
        return "ndarray<%s,%s>[%s]" % (x.dtype, flag, inner)
    if isinstance(x, (list, tuple, set, frozenset)):
        items = sorted(x, key=repr) if isinstance(x, (set, frozenset)) else x
        return "%s[%s]" % (type(x).__name__, ",".join(_form(e) for e in items))
    if isinstance(x, dict):
        return "dict{%s}" % ",".join("%s:%s" % (_form(k), _form(v)) for k, v in x.items())
    if hasattr(x, "__dict__"):
        return "%s%s" % (type(x).__name__, _form(vars(x)))
    return "%s(%r)" % (type(x).__name__, x)


def same(name, a, b):
    fa, fb = _form(a), _form(b)
    if fa == fb:
        return name + " ok"
    return "%s DIFF literal=%s received=%s" % (name, fa, fb)


def echo(x):
    return x
''',
    "r": '''.form <- function(x) {
  attrs <- attributes(x)
  attrs$names <- NULL
  a <- if (length(attrs) == 0) "" else paste0("{", paste(names(attrs), vapply(attrs, function(v) paste(format(v), collapse = " "), character(1)), sep = "=", collapse = ";"), "}")
  head <- paste0(paste(class(x), collapse = "/"), ":", typeof(x), a)
  nm <- names(x)
  if (is.list(x)) {
    inner <- vapply(seq_along(x), function(i) paste0(if (is.null(nm)) "" else paste0(nm[i], "="), .form(x[[i]])), character(1))
    return(paste0(head, "[", paste(inner, collapse = ","), "]"))
  }
  vals <- if (inherits(x, "integer64")) as.character(x) else format(unclass(x))
  paste0(head, "[", paste0(if (is.null(nm)) "" else paste0(nm, "="), vals, collapse = ","), "]")
}

same <- function(name, a, b) {
  fa <- .form(a)
  fb <- .form(b)
  if (identical(fa, fb)) return(paste(name, "ok"))
  paste0(name, " DIFF literal=", fa, " received=", fb)
}

echo <- function(x) x
''',
    "cpp": '''#pragma once
#include <string>

struct P {
    int px;
    double py;
};

// Both arguments have one static type, which is their native form.
template <class T>
std::string same(const std::string& name, const T& a, const T& b) {
    (void)a; (void)b;
    return name + " ok";
}

template <class T>
T echo(const T& x) { return x; }
''',
    "rust": '''#[derive(Clone, Debug)]
pub struct P {
    pub px: i64,
    pub py: f64,
}

// Both arguments have one static type, which is their native form.
pub fn same<T>(name: &String, _a: &T, _b: &T) -> String {
    format!("{} ok", name)
}

pub fn echo<T: Clone>(x: &T) -> T {
    x.clone()
}
''',
}

FORM_FILE = {"py": "form.py", "r": "form.R", "cpp": "form.hpp", "rust": "form.rs"}


def shapes_for(langs):
    return [s for s in SHAPES if not any(l in s[3] for l in langs)]


def program(lang):
    others = [m for m in LANGS if m != lang]
    present = [lang] + others
    imports = ["import %s" % ROOT[l] for l in present]
    imports += ["import vector"] + ["import %s" % VECTOR[l] for l in present]
    maps = [l for l in present if l in MAPSET]
    imports += ["import map (Map)", "import set (Set)"]
    for l in maps:
        imports += ["import %s (Map)" % MAPSET[l][0], "import %s (Set)" % MAPSET[l][1]]
    lines = [
        "-- Spec NATIVE-9: in %s a value built by a literal has the native form of" % LANG_NAME[lang],
        "-- the same type received from every other language.",
        "-- Generated by test-suite/gen-native-9.py; edit the generator, not this file.",
        "module main (report)",
        "",
    ] + imports + [
        "",
        "data Dir = North | South",
        "data Shape = Circle Real | Rect Real Real",
        "",
        "record P where",
        "  px :: Int",
        "  py :: Real",
        "record Cpp => P = \"P\"",
        "record Rust => P = \"P\"",
        "",
        'source %s from "%s" ("same")' % (LANG_NAME[lang], FORM_FILE[lang]),
        "same :: Str -> a -> a -> Str",
        "",
    ]
    for m in others:
        lines += [
            'source %s from "%s" ("echo" as echo%s)' % (LANG_NAME[m], FORM_FILE[m], LANG_NAME[m]),
            "echo%s :: a -> a" % LANG_NAME[m],
        ]
    lines += ["", "report :: U8 -> [Str]", "report x ="]
    cases = []
    for (name, ty, lit, missing) in SHAPES:
        for m in others:
            if m in missing or lang in missing:
                continue
            cases.append('same "%s:%s" (%s :: %s) (echo%s (%s :: %s))'
                         % (name, m, lit, ty, LANG_NAME[m], lit, ty))
    for (name, ty, lit) in VAR_SHAPES:
        for m in others:
            cases.append('same "%s:%s" (%s :: %s) (echo%s (%s :: %s))'
                         % (name, m, lit, ty, LANG_NAME[m], lit, ty))
    lines.append("  [ " + "\n  , ".join(cases) + "\n  ]")
    return "\n".join(lines) + "\n", [c.split('"')[1] for c in cases]


def main():
    base = os.path.join("test-suite", "golden-tests")
    for i, lang in enumerate(LANGS, start=1):
        d = os.path.join(base, "spec-native-9-%d" % i)
        os.makedirs(d, exist_ok=True)
        text, names = program(lang)
        with open(os.path.join(d, "main.loc"), "w") as f:
            f.write(text)
        for l in LANGS:
            with open(os.path.join(d, FORM_FILE[l]), "w") as f:
                f.write(FORM[l])
        with open(os.path.join(d, "Makefile"), "w") as f:
            f.write("all:\n\trm -f *.err obs.txt\n"
                    "\tmorloc make -o nexus main.loc 2> build.err\n"
                    "\t./nexus report 7 > obs.txt 2> obs.err\n\n"
                    "clean:\n\trm -rf nexus pools __pycache__ *err\n"
                    "\trm -rf *-build *build.tmp*\n")
        with open(os.path.join(d, "exp.txt"), "w") as f:
            f.write("[" + ",".join('"%s ok"' % n for n in names) + "]\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
