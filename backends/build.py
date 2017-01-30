#!/usr/bin/env python3

import argparse
import subprocess

__version__ = '0.0.0'
__prog__ = 'loc'

class manifold_nexus:
    def __init__(self):
        pass
        
class manifold_pool:
    def __init__(self):
        pass

class manifold:
    def __init__(self):
        self.inpt = None

def print_lil(lil):
    for line in lil:
        print('\t'.join(line), end="")

def print_src(src):
    for line in src:
        print(line[0])
        print(''.join(line[1]), end="")

def read_lil(args):
    subprocess.call("loc {} > z.lil".format(args.f), shell=True)
    with open("z.lil", 'r') as f:
        lil = f.readlines()

    src = []
    for l in lil:
        if(l[0:4] == "NSRC"):
            src.append( (l.split('\t')[1], []) )
        elif(l[0] == " "):
            src[-1][1].append(l)

    manifolds = {}
    for l in lil:
        row = l.split('\t')
        mid = row[1]

        if(cmd == "EMIT"):
            manifolds[mid] = {}
            manifolds[mid]["name"] = row[2]
        elif(cmd == "FUNC"):
            manifolds[mid][cmd] = row[2]
        elif(cmd == "INPM"):
            manifolds[mid][cmd] = (row[2], row[3])
        elif(cmd == "INPP"):
            manifolds[mid][cmd] = (row[2], row[3])
        elif(cmd == "INPF"):
            manifolds[mid][cmd] = (row[2], row[3])
        elif(cmd == "INPA"):
            manifolds[mid][cmd] = (row[2], row[3:])
        elif(cmd == "NARG"):
            manifolds[mid][cmd] = row[2]
        elif(cmd == "CHEK"):
            manifolds[mid][cmd] = row[2]
        elif(cmd == "FAIL"):
            manifolds[mid][cmd] = row[2]
        elif(cmd == "CACH"):
            manifolds[mid][cmd] = row[2]
        elif(cmd == "MDOC"):
            manifolds[mid][cmd] = row[2]
        elif(cmd == "FARG"):
            manifolds[mid][cmd] = (row[2], row[3])
        elif(cmd == "HOOK"):
            manifolds[mid][cmd] = (row[2], row[3])
        elif(cmd == "TYPE"):
            manifolds[mid][cmd] = row[2]
        elif(cmd == "EXPT"):
            manifolds[mid][cmd] = row[2]

    print_src(src)


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
    read_lil(args)
