import os
import re
import subprocess

import manifold
from util import err

def add_manifold_line(manifold, row):
    cmd = row[0]
    try:
        if(cmd == "FUNC"):
            manifold.func = row[2]
        elif(cmd == "FAIL"):
            manifold.fail = row[2]
        elif(cmd == "CACH"):
            manifold.cache = row[2]
        elif(cmd == "MDOC"):
            manifold.mdoc = row[2]
        elif(cmd == "TYPE"):
            manifold.type = row[2]
        elif(cmd == "NARG"):
            manifold.narg = row[2]
        # platonic functions of four kinds
        elif(cmd == "INPM"):
            manifold.add_input('m', row[2], row[3], row[4])
        elif(cmd == "INPP"):
            manifold.add_input('p', row[2], row[3], row[4])
        elif(cmd == "INPF"):
            manifold.add_input('f', row[2], row[3], row[4])
        elif(cmd == "INPA"):
            manifold.add_input('a', row[2], row[3], row[4])
        # other list attributes
        elif(cmd == "CHEK"):
            manifold.add_check(row[2])
        elif(cmd == "HOOK"):
            manifold.add_hook(row[2], row[3])
        elif(cmd == "FARG"):
            manifold.add_farg(npos=row[2], key=row[3], value=row[4:])
    except IndexError:
        err("Malformed LIL, unexpected number of fields")

def compile_loc(
    loc_src,
    loc_path = "~/.loc/bin/loc",
    flags    = [],
    valgrind = False,
    memtest  = False
):
    lpath = os.path.expanduser(loc_path)

    cmds = []
    if valgrind or memtest:
        cmds = ['valgrind']
    if memtest:
        cmds.append('--leak-check=full')
    cmds.append(lpath)
    cmds += flags
    cmds.append(loc_src)

    result = subprocess.run(
        cmds,
        stderr=subprocess.PIPE,
        stdout=subprocess.PIPE,
        encoding='utf-8'
    )
    result.stdout = [s + "\n" for s in result.stdout.strip().split('\n')]
    return result

def typecheck(loc_src, loc_path="~/.loc/bin/loc"):
    return compile_loc(loc_src, loc_path, flags=['-c'])

def get_src(lil):
    src = {}
    lang = ""
    for l in lil:
        if(l[0:4] == "NSRC"):
            row = l.split('\t')
            lang = row[1]
            try:
                if not lang in src:
                    src[lang] = []
            except KeyError:
                err("Misformed LIL: source requires a language")
        elif(len(l) == 0 or l[0] == " "):
            try:
                src[lang].append(l[4:]) # remove first 4 spaces
            except KeyError:
                pass # skip whitespace lines preceding source sections
    src = {k:''.join(v) for k,v in src.items()}
    return src

def get_exports(lil):
    exports = {}
    for l in lil:
        r = l.strip().split('\t')
        if(r[0] == "EXPT"):
            try:
                exports[r[1]] = r[2]
            except IndexError:
                exports[r[1]] = r[1]
    return exports


def get_manifolds(lil):
    manifolds = {}
    for l in lil:
        row = l.strip().split('\t')
        if(row[0] == "EMIT"):
            manifolds[row[1]] = manifold.Manifold(row[1], row[2])

    for l in lil:
        row = l.strip().split('\t')
        try:
            mid = row[1]
            try:
                add_manifold_line(manifolds[mid], row)
            except KeyError:
                continue # everything is probably fine
        except IndexError:
            continue # no problem, this is just a source line

    return manifolds
