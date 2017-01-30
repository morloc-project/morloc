import subprocess
import os

import manifold
import my_util

def add_manifold_line(manifold, row):
    cmd = row[0]
    try:
        if(cmd == "FUNC"):
            manifold.func = row[2]
        elif(cmd == "FAIL"):
            manifold.fail = row[2]
        elif(cmd == "CACH"):
            manifold.cach = row[2]
        elif(cmd == "MDOC"):
            manifold.mdoc = row[2]
        elif(cmd == "TYPE"):
            manifold.type = row[2]
        elif(cmd == "NARG"):
            manifold.narg = row[2]
        # platonic functions of four kinds
        elif(cmd == "INPM"):
            manifold.add_input('m', row[2], row[3])
        elif(cmd == "INPP"):
            manifold.add_input('p', row[2], row[3])
        elif(cmd == "INPF"):
            manifold.add_input('f', row[2], row[3])
        elif(cmd == "INPA"):
            manifold.add_input('a', row[2], row[3])
        # other list attributes
        elif(cmd == "CHEK"):
            manifold.chek.append(row[2])
        elif(cmd == "FARG"):
            manifold.farg.append((row[2], row[3], row[4:]))
        elif(cmd == "HOOK"):
            manifold.hook.append((row[2], row[3]))
    except IndexError:
        err("Malformed LIL, unexpected number of fields")

def compile_loc(loc_src, loc_path="~/.loc/bin/locc"):
    # TODO - lose the kludge
    subprocess.call("{} {} > z.lil".format(loc_path, loc_src), shell=True)
    with open("z.lil", 'r') as f:
        lil = [s.strip() for s in f.readlines()]
    os.remove("z.lil")
    return lil

def get_src(lil):
    src = []
    for l in lil:
        if(l[0:4] == "NSRC"):
            try:
                src.append( (l.split('\t')[1], []) )
            except IndexError:
                err("Misformed LIL: source requires a language")
        elif(len(l) == 0 or l[0] == " "):
            try:
                src[-1][1].append(l)
            except IndexError:
                pass # skip whitespace lines preceding source sections
    return src

def get_exports(lil):
    exports = []
    for l in lil:
        r = l.split('\t')
        if(r[0] == "EXPT"):
            try:
                exports.append((r[1], r[2]))
            except IndexError:
                exports.append((r[1], None))
    return exports


def get_manifolds(lil):
    manifolds = {}
    for l in lil:
        row = l.split('\t')
        if(row[0] == "EMIT"):
            manifolds[row[1]] = manifold.Manifold(row[1], row[2])

    for l in lil:
        row = l.split('\t')
        try:
            mid = row[1]
            try:
                add_manifold_line(manifolds[mid], row)
            except KeyError:
                continue # everything is probably fine
        except IndexError:
            continue # no problem, this is just a source line

    return manifolds
