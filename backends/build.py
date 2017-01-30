#!/usr/bin/env python3

import argparse
import subprocess
import sys

import etc.grammars

__version__ = '0.0.0'
__prog__ = 'loc'

def err(msg):
    sys.exit(msg)

class ManifoldNexus:
    def __init__(self):
        pass
        
class ManifoldPool:
    def __init__(self):
        pass

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
                self.farg.append((row[2], row[3]))
            elif(cmd == "HOOK"):
                self.hook.append((row[2], row[3]))
        except IndexError:
            err("Malformed LIL, unexpected number of fields")

    def print(self):
        print("{}".format(self.mid))

class Instructions:
    def __init__(self, loc_file):
        lil = self.read_lil(loc_file)
        self.source = self.get_src(lil)
        self.manifolds = self.load_manifolds(lil)
        self.exports = self.load_exports(lil)

    def read_lil(self, loc_file):
        # TODO - lose the kludge
        subprocess.call("loc {} > z.lil".format(loc_file), shell=True)
        with open("z.lil", 'r') as f:
            lil = f.readlines()
        return lil

    def get_src(self, lil):
        src = []
        for l in lil:
            if(l[0:4] == "NSRC"):
                src.append( (l.split('\t')[1], []) )
            elif(l[0] == " "):
                src[-1][1].append(l)
        return src

    def load_exports(self, lil):
        exports = []
        for l in lil:
            r = l.split('\t')
            if(r[0] == "EXPT"):
                try:
                    exports.append((r[1], r[2]))
                except IndexError:
                    exports.append((r[1], None))
        return exports

    def load_manifolds(self, lil):
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

    def print_lil(self, lil):
        for line in lil:
            print('\t'.join(line), end="")

    def print_src(self):
        for line in self.source:
            print(line[0])
            print(''.join(line[1]), end="")

    def print(self):
        for k,v in self.manifolds.items():
            v.print()

def parser():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--version',
        help='Display version',
        action='version',
        version='%(prog)s {}'.format(__version__)
    )
    parser.add_argument(
        'f',
        help="LOC source file"
    )
    args = parser.parse_args()
    return(args)

if __name__ == '__main__':
    args = parser()
    inst = Instructions(args.f)
    inst.print()
