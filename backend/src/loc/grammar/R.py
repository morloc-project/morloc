from grammar.base_grammar import Grammar

class RGrammar(Grammar):
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
        self.lang      = "R"
        self.INDENT = 2                
        self.SEP    = ', '             
        self.BIND   = '='              
        self.AND    = ' && '           
        self.LIST   = 'list({values})' 
        self.POOL   = '''\
#!/usr/bin/Rscript --vanilla
library(readr)

outdir <- "{outdir}"

{type_map}

{source}

{manifolds}

args <- commandArgs(TRUE)
m <- args[1]

if(exists(m)){{
  f <- get(m)
  d <- do.call(f, as.list(args[-1]))
  u <- native_to_universal(d, types[m], outdir)
  write(u, file=stdout())
}} else {{
  quit(status=1)
}}'''
        self.TYPE_MAP         = '''types <- c({pairs})'''
        self.TYPE_ACCESS      = '''types["{key}"]'''
        self.CAST_NAT2UNI     = '''natural_to_universal({key}, {type})'''
        self.CAST_UNI2NAT     = '''universal_to_natural({key}, {type})'''
        self.NATIVE_MANIFOLD = '''\
{mid} = function ({marg_uid}){{
  {hook0}
{block}
  {hook1}
  return(b)
}}
'''
        self.SIMPLE_MANIFOLD = '''\
{mid} = function ({marg_uid}){{
  {function}({arguments})
}}
'''
        self.UID_WRAPPER = '''\
{mid}_uid = 0
wrap_{mid} <- function(...){{
    {mid}_uid <<- {mid}_uid + 1 
    {mid} (..., uid={mid}_uid )
}}
'''
        self.UID = 'uid'
        self.MARG_UID = '{marg}, {uid}'
        self.WRAPPER_NAME = 'wrap_{mid}'
        self.FOREIGN_MANIFOLD = '''\
{mid} <- function({marg_uid}){{
  foreign_pool <- file.path(outdir, "call.{foreign_lang}")
  fo <- system2(
    foreign_pool,
    args=c({args}),
    stdout=TRUE,
    stderr=FALSE
  )
  universal_to_native(fo, types["{mid}"])
}}
'''
        self.CACHE = '''\
if({cache}_chk("{mid}"{uid}{cache_args})){{
  {hook8}
  b <- {cache}_get("{mid}"{uid}{cache_args})
  {hook9}
}}
else{{
{process}
}}
'''
        self.DATCACHE_ARGS = '''outdir="{outdir}"'''
        self.PROCESS = '''\
{hook2}
{validate}
{hook3}
'''
        self.DO_VALIDATE = '''\
if( {checks} ){{
  {hook4}
  b <- {function}({arguments})
  {cache_put}
  {hook5}
}} else {{
  {hook6}
  b <- {fail}
  {cache_put}
  {hook7}
}}
'''
        self.FAIL = '{fail}({marg_uid})'
        self.DEFAULT_FAIL = 'NULL'
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
            s = 'native_to_universal(%s, types["%s"], outdir)' % (a,a)
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
            types.append("%s='%s'" % (k, v.type))
            for k,n,m,t in v.input:
                if k == "a":
                    types.append("x%s='%s'" % (m, t))
        return self.TYPE_MAP.format(pairs=', '.join(types))
