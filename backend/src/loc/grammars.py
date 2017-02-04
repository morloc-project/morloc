from util import err,indent,clean

class Grammar:
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
        self.lang      = None
        self.SEP              = ""
        self.BIND             = ""
        self.AND              = ""
        self.POOL             = ""
        self.SIMPLE_MANIFOLD  = ""
        self.NATIVE_MANIFOLD  = ""
        self.FOREIGN_MANIFOLD = ""
        self.CACHE            = ""
        self.DATCACHE_ARGS    = ""
        self.PROCESS          = ""
        self.DO_VALIDATE      = ""
        self.NO_VALIDATE      = ""
        self.ARGUMENTS        = ""
        self.MANIFOLD_CALL    = ""
        self.CHECK_CALL       = ""
        self.HOOK             = ""
        self.INDENT           = ""
        self.CACHE_PUT        = ""
        self.MARG             = ""
        self.LIST             = ""
        self.FAIL             = ""
        self.DEFAULT_FAIL     = ""
        self.UID_WRAPPER      = ""
        self.UID              = ""
        self.MARG_UID         = ""
        self.WRAPPER_NAME     = ""

    def make(self):

        p = self.POOL

        # Find the manifolds that require wrappers for adding
        # manifold arguments and ids
        wrappers = set()
        for mid,man in self.manifolds.items():
            for k,n,m in man.input:
                if k == "f":
                    wrappers.add(m)

        mtext = []
        for k,v in self.manifolds.items():
            if k in wrappers:
                w = self.make_uid_wrapper(v.mid)
                mtext.append(w)
            if v.lang == self.lang:
                if v.cache or v.check or v.hook:
                    s = self.native_manifold(v)
                else:
                    s = self.simple_manifold(v)
            else:
                s = self.foreign_manifold(v)
            s = clean(s)
            mtext.append(s)

        p = p.format(
            source=self.source,
            outdir=self.outdir,
            manifolds='\n\n'.join(mtext)
        )

        return p

    def make_uid_wrapper(self, mid):
        return self.UID_WRAPPER.format(mid=mid)


    def simple_manifold(self, m):
        return self.SIMPLE_MANIFOLD.format(
            mid       = m.mid,
            marg_uid  = self.get_marg_uid(m),
            function  = m.func,
            arguments = self.arguments(m)
        )

    def native_manifold(self, m):
        ind   = self.INDENT
        block = indent(self.cache(m), n=ind)
        margs = self.get_margs(m.narg)
        return self.NATIVE_MANIFOLD.format(
            mid         = m.mid,
            marg_uid    = self.get_marg_uid(m),
            hook0       = self.get_hook(m, 0),
            block       = block,
            hook1       = self.get_hook(m, 1)
        )

    def foreign_manifold(self, m):
        s = self.FOREIGN_MANIFOLD.format(
            mid          = m.mid,
            foreign_lang = m.lang,
            outdir       = self.outdir,
            marg_uid     = self.get_marg_uid(m)
        )
        return s

    def get_margs(self, n):
        ss = []
        try:
            n = int(n)
        except TypeError:
            err("nargs must be integrel")
        for i in range(n):
            ss.append(self.MARG.format(i=str(i+1)))
        margs = self.SEP.join(ss)
        return margs

    def get_uid(self, m):
        if m.narg:
            uid = self.UID.format(nth=str(int(m.narg)+1))
        else:
            uid = ""
        return uid

    def get_marg_uid(self, m):
        margs = self.get_margs(m.narg)
        uid = self.get_uid(m)
        if uid:
            s = self.MARG_UID.format(marg=margs, uid=uid)
        else:
            s = ""
        return s

    def get_cache_args(self, m):
        if(m.cache == "datcache"):
            args = self.DATCACHE_ARGS.format(outdir=self.outdir)
        else:
            args = ""
        return args

    def get_hook(self, m, kind):
        hooks = [h for h in m.hook if h.kind == kind]
        ss = []
        for h in hooks:
            ss.append( self.HOOK.format(
                hmid=h.mid,
                marg_uid=self.get_marg_uid(m)
            ) )
        return '\n'.join(ss)

    def checks(self, m):
        ss = []
        for c in m.check:
            ss.append(self.CHECK_CALL.format(
                hmid=c,
                marg_uid=self.get_marg_uid(m)
            ))
        s = self.SEP.join(ss)
        return s

    def arguments(self, m):
        if(m.input and m.farg):
            sep=self.SEP
        else:
            sep=""

        margs = self.get_margs(m.narg)

        inputs = []
        for k,n,v in m.input:
            if k == "m":
                inputs.append(self.MANIFOLD_CALL.format(
                    hmid=v,
                    marg_uid = self.get_marg_uid(m)
                ))
            elif k == "f":
                inputs.append(self.WRAPPER_NAME.format(mid=v))
            elif k == "a":
                inputs.append(self.MARG.format(i=v))
            else:
                inputs.append(v)
        inputs = self.SEP.join(inputs)

        fargs = []
        for n,k,vs in m.farg:
            v = self.SEP.join(vs)
            if len(vs) > 1:
                v = self.LIST.format(values=v)

            if k:
                fargs.append(self.BIND.join((k,v)))
            else:
                fargs.append(v)

        fargs = self.SEP.join(fargs)

        s = self.ARGUMENTS.format(
            inputs = inputs,
            sep    = sep,
            fargs  = fargs
        )
        return s

    def validate(self, m):
        uid = self.get_uid(m)
        uid = self.SEP + uid if uid else uid
        cache_args = self.get_cache_args(m)
        cache_args = self.SEP + cache_args if cache_args else cache_args
        if(m.cache):
            cache_put = self.CACHE_PUT.format(
                cache      = m.cache,
                mid        = m.mid,
                uid        = uid,
                cache_args = cache_args
            )
        else:
            cache_put = ""

        if(m.check):
            if m.fail:
                fail = self.FAIL.format(
                    fail=m.fail,
                    marg_uid=self.get_marg_uid(m)
                )
            else:
                fail = self.DEFAULT_FAIL
            s = self.DO_VALIDATE.format(
                checks    = self.checks(m),
                hook4     = self.get_hook(m, 4),
                hook5     = self.get_hook(m, 5),
                hook6     = self.get_hook(m, 6),
                hook7     = self.get_hook(m, 7),
                function  = m.func,
                arguments = self.arguments(m),
                mid       = m.mid,
                cache_put = cache_put,
                fail      = fail,
                marg      = self.get_margs(m.narg)
            )
        else:
            s = self.NO_VALIDATE.format(
                hook4     = self.get_hook(m, 4),
                hook5     = self.get_hook(m, 5),
                function  = m.func,
                arguments = self.arguments(m),
                mid       = m.mid,
                cache_put = cache_put
            )
        return s

    def process(self, m):
        s = self.PROCESS.format(
            hook2    = self.get_hook(m, 2),
            validate = self.validate(m),
            mid      = m.mid,
            hook3    = self.get_hook(m, 3)
        )
        return s

    def cache(self, m):
        uid = self.get_uid(m)
        cargs = self.get_cache_args(m)
        if(m.cache):
            s = self.CACHE.format(
                cache      = m.cache,
                mid        = m.mid,
                hook8      = self.get_hook(m, 8),
                hook9      = self.get_hook(m, 9),
                uid        = self.SEP + uid if uid else "",
                cache_args = self.SEP + cargs if cargs else "" ,
                process    = indent(self.process(m), n=self.INDENT)
            )
        else:
            s = self.process(m)
        return s


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

# outdir={outdir}

{source}

{manifolds}

args <- commandArgs(TRUE)
m <- args[1]

if(exists(m)){{
  f = get(m)
  d <- f()
  if(is.data.frame(d)){{
      write_tsv(d, path="/dev/stdout")
  }} else {{
      write_lines(d, path="/dev/stdout")
  }}
}} else {{
  quit(status=1)
}}'''
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
wrap_{mid} <- function( ... ){{
    {mid}_uid <<- {mid}_uid + 1 
    uid <- {mid}_uid
    {mid} ( ..., uid=uid )
}}
'''
        self.UID = 'uid'
        self.MARG_UID = '{marg}, {uid}'
        self.WRAPPER_NAME = 'wrap_{mid}'
        self.FOREIGN_MANIFOLD = '''\
{mid} <- function({marg_uid}){{
  d <- system("{outdir}/call.{foreign_lang} {mid} {marg_uid}", intern = TRUE)
  d <- read_tsv(d)
  if(ncol(d) == 1){{
    d <- d[[1]]
  }}
  return(d)
}}
'''
        self.CACHE = '''\
if({cache}_chk("{mid}"{uid}{cache_args})){{
  {hook8}
  b = {cache}_get("{mid}"{uid}{cache_args})
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
  b = {function}({arguments})
  {cache_put}
  {hook5}
}} else {{
  {hook6}
  b = {fail}
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
{cache}_put("{mid}", b{uid}{cache_args})
'''
        self.MARG          = 'x{i}'
        self.ARGUMENTS     = '{inputs}{sep}{fargs}'
        self.MANIFOLD_CALL = '{hmid}({marg_uid})'
        self.CHECK_CALL    = '{hmid}({marg_uid})'
        self.HOOK          = '{hmid}({marg_uid})'


# === sh grammar ===========================================

class ShGrammar(Grammar):
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
        self.lang      = "sh"
        self.INDENT = 4
        self.SEP    = ' '
        self.BIND   = ' '
        self.AND    = ' && '
        self.LIST   = ' {value} '
        self.POOL   = '''\
#!/usr/bin/env bash

outdir={outdir}

{source}

to_stderr () {{
    $@ 1>&2
}}

{manifolds}

manifold_exists() {{
    type $1 | grep -q function
}}
if manifold_exists $1
then
    if [[ -f $outdir/$1_tmp ]]
    then
        cat $outdir/$1_tmp
    else
        $@
    fi
else
    exit 1 
fi
'''
        self.NATIVE_MANIFOLD = '''\
{mid} () {{
    {hook0}
{block}
    {hook1}
}}
'''
        self.SIMPLE_MANIFOLD = '''
{mid} () {{
    {function} {arguments}
}}
'''
        self.UID_WRAPPER  = '''\
{mid}_uid=0
wrap_{mid} () {{
    {mid}_uid=$(( {mid}_uid + 1 ))
    {mid} $@ ${mid}_uid
}}
'''
        self.UID          = '${nth}'
        self.MARG_UID     = '{marg} {uid}'
        self.WRAPPER_NAME = 'wrap_{mid}'
        self.FOREIGN_MANIFOLD = '''\
{mid} () {{
    {outdir}/call.{foreign_lang} {mid} {marg_uid}
}}
'''
        self.CACHE = '''\
if {cache}_chk {mid}{uid}
then
    {hook8}
    {cache}_get {mid}{uid}
    {hook9}
else
    {process}
fi
'''
        self.DATCACHE_ARGS = ""
        self.PROCESS = '''\
{hook2}
{validate}
{hook3}
( cat $outdir/{mid}_tmp ; rm $outdir/{mid}_tmp )
'''
        self.DO_VALIDATE = '''\
if {checks}
then
    {hook4}
    {function} {arguments} > $outdir/{mid}_tmp
    {cache_put}
    {hook5}
else
    {hook6}
    {fail}> $outdir/{mid}_tmp
    {cache_put}
    {hook7}
fi
'''
        self.FAIL = '''{fail} {marg_uid} '''
        self.DEFAULT_FAIL = ""
        self.NO_VALIDATE = '''\
{hook4}
{function} {arguments} > $outdir/{mid}_tmp
{cache_put}
{hook5}
'''
        self.CACHE_PUT = '''\
{cache}_put {mid} $outdir/{mid}_tmp {uid} {cache_args}
'''
        self.MARG          = '${i}'
        self.ARGUMENTS     = '{fargs} {inputs}'
        self.MANIFOLD_CALL = '<({hmid} {marg_uid})'
        self.CHECK_CALL    = '{hmid} {marg_uid}'
        self.HOOK          = 'to_stderr {hmid} {marg_uid}'
