#!/usr/bin/env python3

import argparse
import sys
import os

import lil
import nexus
import pool
from util import err

__version__ = '0.0.0'
__prog__ = 'loc'

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
    parser.add_argument(
        '-m', '--print-manifolds',
        help="help",
        action='store_true',
        default=False
    )
    parser.add_argument(
        '-x', '--execution-path',
        help="help"
    )
    parser.add_argument(
        '-k', '--clobber',
        help="Overwrite execution path directory",
        action='store_true',
        default=False
    )
    args = parser.parse_args()
    return(args)

if __name__ == '__main__':
    args = parser()

    raw_lil = lil.compile_loc(args.f)
    exports = lil.get_exports(raw_lil)
    source = lil.get_src(raw_lil)
    manifolds = lil.get_manifolds(raw_lil)
    languages = set([l.lang for m,l in manifolds.items()])

    loc_home = os.path.expanduser("~/.loc")
    loc_tmp = "%s/tmp" % loc_home

    try:
        os.mkdir(loc_tmp)
    except FileExistsError:
        pass

    if args.execution_path:
        outdir=os.path.expanduser(args.execution_path)
        try:
            os.mkdir(outdir)
        except PermissionError:
            err("PermissionError: cannot mkdir '%s'" % outdir)
        except FileNotFoundError:
            err("Cannot create directory '%s'" % outdir)
        except FileExistsError:
            if not args.clobber:
                err("Directory '%s' already exists" % outdir)
    else:
        for i in range(100):
            try:
                outdir="{}/loc_{}".format(loc_tmp, i)
                os.mkdir(outdir)
                break
            except FileExistsError:
                pass
        else:
            err("Too many temporary directories")

    if(args.print_manifolds):
        for k,m in manifolds.items():
            m.print()

    manifold_nexus = nexus.build_manifold_nexus(
        languages = languages,
        exports   = exports,
        manifolds = manifolds,
        outdir    = outdir,
        home      = loc_home,
        version   = __version__,
        prog      = __prog__
    )

    for lang in languages:
        p = pool.build_pool(
            lang      = lang,
            source    = source,
            manifolds = manifolds,
            outdir    = outdir,
            home      = loc_home
        )
        pool_filename = "{}/call.{}".format(outdir, lang)
        with open(pool_filename, 'w') as f:
            print(p, file=f)
            os.chmod(pool_filename, 0o755)


    with open("manifold-nexus.py", 'w') as f:
        print(manifold_nexus, file=f)

    os.chmod("manifold-nexus.py", 0o755)
