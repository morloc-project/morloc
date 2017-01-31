import re

import manifold
from util import err,indent

nexus_template = '''\
#!/usr/bin/env python3

import argparse
import subprocess

def parser():
    parser = argparse.ArgumentParser(
        description="A program generated from a LOC script", 
        usage='manifold-nexus <mid>',
        prog="manifold-nexus"
    )
    parser.add_argument(
        '--version',
        help='Display version',
        action='version',
        version='{prog} {version}'
    )
    sub = parser.add_subparsers(
        help='.',
        metavar='[ for more help: manifold-nexus <subcommand> -h ]',
        title='manifolds'
    )

{manifold_parsers}

    args = parser.parse_args()

    return(args)

{manifold_calls}

if __name__ == '__main__':
    args = parser()
    args.func()
'''

parser_template = '''\

{mid}_parser = sub.add_parser(
    '{alias}',
    usage="manifold-nexus {alias}",
    description="{doc}",
    help="({lang}) {doc}"
)
{mid}_parser.set_defaults(func={mid})
'''

call_template = '''\
def {mid}():
    subprocess.call("{outdir}/call.{lang} {mid}", shell=True)
'''

def build_manifold_nexus(
    languages,
    exports,
    manifolds,
    outdir,
    home,
    version,
    prog
):
    mparsers = []
    mcalls = []

    for k,v in manifolds.items():

        try:
            alias = exports[k]
        except KeyError:
            continue

        if v.mdoc:
            doc = v.mdoc[1:-1]
        else:
            doc = ""

        parser_string = indent(
            parser_template.format(
                mid  = k,
                alias=alias,
                func = v.func,
                lang = v.lang,
                doc  = doc
            ),
            n=4
        )

        call_string = call_template.format(
            mid=k,
            lang=v.lang,
            outdir=outdir
        )
        mcalls.append(call_string)
        mparsers.append(parser_string)

    return nexus_template.format(
        prog=prog,
        version=version,
        manifold_parsers='\n'.join(mparsers),
        manifold_calls='\n'.join(mcalls)
    )
