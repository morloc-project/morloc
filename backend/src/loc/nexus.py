import manifold
import my_util
import re

nexus_template = '''\
#!/usr/bin/env python3

import argparse
import subprocess

def parser():
    parser = argparse.ArgumentParser(
        description="A program generated from a LOC script", 
        prog="manifold-nexus"
    )
    parser.add_argument(
        '--version',
        help='Display version',
        action='version',
        version='{prog} {version}'
    )
    sub = parser.add_subparsers(
        help='sub-command help'
    )
    {manifold_parsers}
    args = parser.parse_args()

    return(args)

{manifold_calls}

if __name__ == '__main__':
    args = parser()
    args.func(args)
'''

parser_template = '''\

{mid}_parser = sub.add_parser(
    '{mid}',
    usage="manifold-nexus {mid}",
    description="{doc}",
    help="({lang}) {func}"
)
{mid}_parser.set_defaults(func={mid})
'''

call_template = '''\
def {mid}():
    subprocess.call("./call.{lang} {mid}", shell=True)
'''

def indent(lines):
    return "\n".join(["    %s" % s for s in lines.split('\n')])

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
        if v.mdoc:
            doc = v.mdoc[1:-1]
        else:
            doc = ""

        parser_string = indent(parser_template.format(
            mid=k,
            func=v.func,
            lang=v.lang,
            doc=doc
        ))

        call_string = call_template.format(mid=k, lang=v.lang)
        mcalls.append(call_string)
        mparsers.append(parser_string)

    return nexus_template.format(
        prog=prog,
        version=version,
        manifold_parsers='\n'.join(mparsers),
        manifold_calls='\n'.join(mcalls)
    )
