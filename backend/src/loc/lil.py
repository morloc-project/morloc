import subprocess
import os

class Manifold:
    def __init__(self, mid, lang):
        self.mid  = mid
        self.lang = lang
        self.func = ""
        self.inpt = [] # [mpfa]:value
        self.chek = []
        self.hook = [] # pos:mid
        self.farg = []
        self.marg = []
        self.fail = ""
        self.cach = ""
        self.mdoc = ""
        self.type = "*"
        self.narg = 0

    def add_line(self, row):
        cmd = row[0]
        try:
            if(cmd == "FUNC"):
                self.func = row[2]
            elif(cmd == "FAIL"):
                self.fail = row[2]
            elif(cmd == "CACH"):
                self.cach = row[2]
            elif(cmd == "MDOC"):
                self.mdoc = row[2]
            elif(cmd == "TYPE"):
                self.type = row[2]
            elif(cmd == "NARG"):
                self.narg = row[2]
            # platonic functions of four kinds
            elif(cmd == "INPM"):
                self.inpt.append(('m', row[2], row[3]))
            elif(cmd == "INPP"):
                self.inpt.append(('p', row[2], row[3]))
            elif(cmd == "INPF"):
                self.inpt.append(('f', row[2], row[3]))
            elif(cmd == "INPA"):
                self.inpt.append(('a', row[2], row[3]))
            # other list attributes
            elif(cmd == "CHEK"):
                self.chek.append(row[2])
            elif(cmd == "FARG"):
                self.farg.append((row[2], row[3], row[4:]))
            elif(cmd == "HOOK"):
                self.hook.append((row[2], row[3]))
        except IndexError:
            err("Malformed LIL, unexpected number of fields")

    def print(self):
        print("{}".format(self.mid))
        print("  {}<{}>:{}".format(self.func, self.type, self.lang))
        if self.cach:
            print("  cache:{}".format(self.cach))
        if self.mdoc:
            print("  mdoc:{}".format(self.mdoc))
        for r in self.chek:
            print("  check:{}".format(self.chek))
        if self.fail:
            print("  fail:{}".format(self.fail))
        for r in self.hook:
            print("  hook:{}".format(self.hook))
        for r in sorted(self.inpt, key=lambda x: x[1]):
            print("  input:{}:{}".format(r[0],r[2]))
        for r in sorted(self.farg, key=lambda x: x[0]):
            print("  arg:{}:[{}]".format(r[1],','.join(r[2])))


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
            manifolds[row[1]] = Manifold(row[1], row[2])

    for l in lil:
        row = l.split('\t')
        try:
            mid = row[1]
            try:
                manifolds[mid].add_line(row)
            except KeyError:
                continue # everything is probably fine
        except IndexError:
            continue # no problem, this is just a source line

    return manifolds
