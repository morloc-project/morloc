#!/usr/bin/env python3

import argparse
import sys

import grammars
import lil

__version__ = '0.0.0'
__prog__ = 'loc'

def err(msg):
    sys.exit(msg)

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

    raw_lil = lil.compile_loc(args.f)
    exports = lil.get_exports(raw_lil)
    source = lil.get_src(raw_lil)
    manifolds = lil.get_manifolds(raw_lil)

    for k,v in manifolds.items():
        v.print()
