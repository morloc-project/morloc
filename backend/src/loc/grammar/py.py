from grammar.base_grammar import Grammar

class PyGrammar(Grammar):
    def __init__(
        self,
        source,
        manifolds,
        outdir,
        home
    ):
        self.source    = source
        self.manifolds = manifolds
        self.outdir    = outdir
        self.home      = home
        self.lang      = "py"
        self.INDENT    = 4
        self.SEP       = ', '
        self.BIND      = '='
        self.AND       = ' and '
        self.LIST      = '[{values}]'
        self.POOL      = '''\
#!/usr/bin/env python3

import sys

outdir = "{outdir}"

{type_map}

{source}

{manifolds}

if __name__ == '__main__':
    args = sys.argv
    cmd_str = "{{function}}({{args}})"
    arg_str = ', '.join(args[2:])
    cmd = cmd_str.format(function=args[1], args=arg_str)
    x = eval(cmd)
    result = native_to_universal(x, output_type[args[1]], outdir)
    print(result)
'''
        self.TYPE_MAP         = '''output_type = {{\n{pairs}\n}}'''
        self.TYPE_ACCESS      = '''output_type[{key}]'''
        self.CAST_NAT2UNI     = '''natural_to_universal({key}, {type})'''
        self.CAST_UNI2NAT     = '''universal_to_natural({key}, {type})'''
        self.NATIVE_MANIFOLD = '''\
def {mid}({marg_uid}):
{blk}
'''
        self.NATIVE_MANIFOLD_BLK = '''\
{hook0}
{cache}
{hook1}
return b\
'''
        self.SIMPLE_MANIFOLD = '''\
def {mid}({marg_uid}):
{blk}
'''
        self.SIMPLE_MANIFOLD_BLK = '''\
return {function}({arguments})\
'''
        self.UID_WRAPPER = '''\
{mid}_uid = 0
def wrap_{mid}(*args, **kwargs):
{blk}
'''
        self.UID_WRAPPER_BLK = '''\
{mid}_uid = {mid}_uid + 1
{mid} (*args, **kwargs, uid={mid}_uid )\
'''
        self.UID = 'uid'
        self.MARG_UID = '{marg}, {uid}'
        self.WRAPPER_NAME = 'wrap_{mid}'
        self.FOREIGN_MANIFOLD = '''\
def {mid}({marg_uid}):
{blk}
'''
        self.FOREIGN_MANIFOLD_BLK = '''\
pass
# foreign_pool = file.path(outdir, "call.{foreign_lang}")
# fo = system2(
#   foreign_pool,
#   args=c({args}),
#   stdout=TRUE,
#   stderr=FALSE
# )
# universal_to_native(fo, {mid}_type)\
'''
        self.CACHE = '''\
if {cache}_chk("{mid}"{uid}{cache_args}):
{if_blk}
else:
{else_blk}
'''
        self.CACHE_IF = '''\
{hook8}
b = {cache}_get("{mid}"{uid}{cache_args})
{hook9}\
'''
        self.CACHE_ELSE = '''\
{hook2}
{validate}
{hook3}\
'''
        self.DATCACHE_ARGS = '''outdir="{outdir}"'''
        self.DO_VALIDATE = '''\
if {checks}:
{if_blk}
else:
{else_blk}
'''
        self.RUN_BLK = '''\
{hook4}
b = {function}({arguments})
{cache_put}
{hook5}
'''
        self.RUN_BLK_VOID = '''\
{hook4}
{function}({arguments})
b = None
{cache_put}
{hook5}
'''
        self.FAIL_BLK = '''\
{hook6}
b = {fail}
{cache_put}
{hook7}\
'''
        self.FAIL_BLK_VOID = '''\
{hook6}
{fail}
b = None
{cache_put}
{hook7}\
'''
        self.FAIL = '{fail}({marg_uid})'
        self.DEFAULT_FAIL = 'None'
        self.NO_VALIDATE = '''\
{hook4}
b = {function}({arguments})
{cache_put}
{hook5}
'''
        self.CACHE_PUT = '''\
{cache}_put("{mid}", b{other_args})
'''
        self.MARG          = 'x{i}'
        self.ARGUMENTS     = '{inputs}{sep}{fargs}'
        self.MANIFOLD_CALL = '{hmid}({marg_uid})'
        self.CHECK_CALL    = '{hmid}({marg_uid})'
        self.HOOK          = '{hmid}({marg_uid})'

    def make_foreign_manifold(self, m):
        arg_var = " %s" * (int(m.narg) + 1) if m.narg else ""

        arg_rep = ["'%s'" % m.mid]
        for i in range(int(m.narg)):
            a = self.MARG.format(i=str(i+1))
            s = 'native_to_universal(%s, %s_type, outdir)' % (a,a)
            arg_rep.append(s)
        if m.narg:
            arg_rep.append("uid")
        arg_rep = ', '.join(arg_rep)

        s = self.FOREIGN_MANIFOLD.format(
            mid=m.mid,
            args=arg_rep,
            marg_uid=self.make_marg_uid(m),
            foreign_lang=m.lang
        )
        return s

    def make_type_map(self):
        types = []
        for k,v in self.manifolds.items():
            types.append("    '%s' : '%s'" % (k, v.type))
            for k,n,m,t in v.input:
                if k == "a":
                    types.append("x%s : '%s'" % (m, t))
        return self.TYPE_MAP.format(pairs=',\n'.join(types))
