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
        self.TYPE_MAP         = ""
        self.TYPE_ACCESS      = ""
        self.CAST_NAT2UNI     = ""
        self.CAST_UNI2NAT     = ""
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

        p = self.make_pool()

        # Find the manifolds that require wrappers for adding
        # manifold arguments and ids
        wrappers = set()
        for mid,man in self.manifolds.items():
            for k,n,m,t in man.input:
                if k == "f":
                    wrappers.add(m)

        mtext = []
        for k,v in self.manifolds.items():
            if k in wrappers:
                w = self.make_uid_wrapper(v)
                mtext.append(w)
            if v.lang == self.lang:
                if v.cache or v.check or v.hook:
                    s = self.make_native_manifold(v)
                else:
                    s = self.make_simple_manifold(v)
            else:
                s = self.make_foreign_manifold(v)
            s = clean(s)
            mtext.append(s)

        p = p.format(
            source=self.source,
            outdir=self.outdir,
            manifolds='\n\n'.join(mtext)
        )

        return p



    def make_pool(self):
        return self.POOL

    def make_foreign_manifold(self, m):
        s = self.FOREIGN_MANIFOLD.format(
            mid          = m.mid,
            foreign_lang = m.lang,
            outdir       = self.outdir,
            uni_marg_uid = self.make_marg_uid(m, universal=True)
        )
        return s

    def make_simple_manifold(self, m):
        return self.SIMPLE_MANIFOLD.format(
            mid       = m.mid,
            marg_uid  = self.make_marg_uid(m),
            function  = m.func,
            arguments = self.make_arguments(m)
        )

    def make_native_manifold(self, m):
        block = indent(self.make_cache(m), n=self.INDENT)
        margs = self.make_marg(m)
        return self.NATIVE_MANIFOLD.format(
            mid         = m.mid,
            marg_uid    = self.make_marg_uid(m),
            hook0       = self.make_hook(m, 0),
            block       = block,
            hook1       = self.make_hook(m, 1)
        )

    def make_cache(self, m):
        uid = self.make_uid(m)
        cargs = self.make_cache_args(m)
        if(m.cache):
            s = self.CACHE.format(
                cache      = m.cache,
                mid        = m.mid,
                hook8      = self.make_hook(m, 8),
                hook9      = self.make_hook(m, 9),
                uid        = self.SEP + uid if uid else "",
                cache_args = self.SEP + cargs if cargs else "" ,
                process    = indent(self.make_process(m), n=self.INDENT)
            )
        else:
            s = self.make_process(m)
        return s

    def make_cache_put(self, m):
        if(m.cache):
            uid        = self.make_uid(m)
            cache_args = self.make_cache_args(m)
            other_args = ""
            if uid and cache_args:
                other_args = self.SEP.join((uid, cache_args))
            else:
                other_args = uid + cache_args
            if other_args:
                other_args = self.SEP + other_args
            cache_put = self.CACHE_PUT.format(
                cache      = m.cache,
                mid        = m.mid,
                other_args = other_args
            )
        else:
            cache_put = ""
        return cache_put

    def make_cache_args(self, m):
        if(m.cache == "datcache"):
            args = self.make_datcache_args()
        else:
            args = ""
        return args

    def make_datcache_args(self):
        return self.DATCACHE_ARGS.format(outdir=self.outdir)

    def make_process(self, m):
        s = self.PROCESS.format(
            hook2    = self.make_hook(m, 2),
            validate = self.make_validate(m),
            mid      = m.mid,
            hook3    = self.make_hook(m, 3)
        )
        return s

    def make_validate(self, m):
        if(m.check):
            s = self.make_do_validate(m)
        else:
            s = self.make_no_validate(m)
        return s

    def make_do_validate(self, m):
        return self.DO_VALIDATE.format(
            checks    = self.make_check(m),
            hook4     = self.make_hook(m, 4),
            hook5     = self.make_hook(m, 5),
            hook6     = self.make_hook(m, 6),
            hook7     = self.make_hook(m, 7),
            function  = m.func,
            arguments = self.make_arguments(m),
            mid       = m.mid,
            cache_put = self.make_cache_put(m),
            fail      = self.make_fail(m),
            marg      = self.make_marg(m)
        )

    def make_no_validate(self, m):
        return self.NO_VALIDATE.format(
            hook4     = self.make_hook(m, 4),
            hook5     = self.make_hook(m, 5),
            function  = m.func,
            arguments = self.make_arguments(m),
            mid       = m.mid,
            cache_put = self.make_cache_put(m)
        )

    def make_check(self, m):
        ss = []
        for c in m.check:
            ss.append(self.make_check_call(m, c))
        s = self.SEP.join(ss)
        return s

    def make_check_call(self, m, c):
        return self.CHECK_CALL.format(
            hmid=c,
            marg_uid=self.make_marg_uid(m)
        )

    def make_arguments(self, m):
        inputs = self.make_input(m)
        fargs = self.make_function_arguments(m)
        return self.ARGUMENTS.format(
            inputs = inputs,
            sep    = self.SEP if (inputs and fargs) else "",
            fargs  = fargs
        )

    def make_input(self, m):
        inputs = []
        for k,n,v,t in m.input: 
            if k == "m":
                inputs.append(self.make_input_manifold(m, v))
            elif k == "f":
                inputs.append(self.make_input_function(m, v))
            elif k == "a":
                inputs.append(self.make_input_argument(m, v))
            elif k == "p":
                inputs.append(self.make_input_positional(m, v))
            else:
                err("Unexpected argument type '%s'" % k)
        inputs = self.SEP.join(inputs)
        return inputs

    def make_input_manifold(self, m, value):
        return self.MANIFOLD_CALL.format(
            hmid=value,
            marg_uid = self.make_marg_uid(m)
        )

    def make_input_function(self, m, value):
        return self.WRAPPER_NAME.format(mid=value)

    def make_input_argument(self, m, value):
        return self.MARG.format(i=value)

    def make_input_positional(self, m, value):
        return value

    def make_function_arguments(self, m):
        fargs = []
        for n,k,vs in m.farg:
            if len(vs) > 1:
                v = self.make_list(vs)
            elif vs:
                v = vs[0]
            else:
                continue
            if k:
                fargs.append(self.BIND.join((k,v)))
            else:
                fargs.append(v)
        fargs = self.SEP.join(fargs)
        return fargs

    def make_hook(self, m, kind):
        hooks = [h for h in m.hook if h.kind == kind]
        ss = []
        for h in hooks:
            ss.append( self.HOOK.format(
                hmid=h.mid,
                marg_uid=self.make_marg_uid(m)
            ) )
        return '\n'.join(ss)

    def make_marg(self, m, universal=False):
        ss = []
        try:
            n = int(m.narg)
        except TypeError:
            err("nargs must be integrel")
        for i in range(n):
            arg = self.MARG.format(i=str(i+1))
            if universal:
                arg = self.make_cast_nat2uni(arg)
            ss.append(arg)
        margs = self.SEP.join(ss)
        return margs

    def make_cast_nat2uni(self, arg):
        return self.CAST_NAT2UNI.format(
            key=arg,
            type=make_type_access(arg)
        )

    def make_type_access(self, arg):
        return self.TYPE_ACCESS.format(key=arg)

    def make_list(self, xs):
        x = self.SEP.join(xs)
        x = self.LIST.format(values=x)
        return x

    def make_fail(self, m):
        if m.fail:
            fail = self.FAIL.format(
                fail=m.fail,
                marg_uid=self.make_marg_uid(m)
            )
        else:
            fail = self.make_default_fail()
        return fail

    def make_default_fail(self):
        return self.DEFAULT_FAIL

    def make_uid_wrapper(self, m):
        return self.UID_WRAPPER.format(mid=m.mid)

    def make_uid(self, m):
        if m.narg:
            uid = self.UID.format(nth=str(int(m.narg)+1))
        else:
            uid = ""
        return uid

    def make_marg_uid(self, m, universal=False):
        margs = self.make_marg(m, universal)
        uid = self.make_uid(m)
        if uid:
            s = self.MARG_UID.format(marg=margs, uid=uid)
        else:
            s = ""
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

outdir <- "{outdir}"

{source}

{manifolds}

args <- commandArgs(TRUE)
m <- args[1]

if(exists(m)){{
  f <- get(m)
  d <- f()
  if(is.data.frame(d)){{
      write_tsv(d, path="/dev/stdout")
  }} else {{
      write_lines(d, path="/dev/stdout")
  }}
}} else {{
  quit(status=1)
}}'''
        self.TYPE_MAP         = 'types <- c({pairs})'
        self.TYPE_ACCESS      = 'types["{key}"]'
        self.CAST_NAT2UNI     = 'natural_to_universal({key}, {type})'
        self.CAST_UNI2NAT     = 'universal_to_natural({key}, {type})'
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
  foreign_pool <- file.path(outdir, "call.{foreign_lang}")
  cmd <- sprintf("%s {mid} {uni_marg_uid}", foreign_pool)
  raw <- system(cmd, intern = TRUE)
  d <- universal_to_native(raw, types["{mid}"])
  return(d)
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
        self.TYPE_MAP         = '{pairs}'
        self.TYPE_ACCESS      = '${key}_type'
        self.CAST_NAT2UNI     = 'natural_to_universal {key} {type}'
        self.CAST_UNI2NAT     = 'universal_to_natural {key} {type}'
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
    {outdir}/call.{foreign_lang} {mid} {uni_marg_uid}
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
{cache}_put {mid} $outdir/{mid}_tmp{other_args}
'''
        self.MARG          = '${i}'
        self.ARGUMENTS     = '{fargs} {inputs}'
        self.MANIFOLD_CALL = '<({hmid} {marg_uid})'
        self.CHECK_CALL    = '{hmid} {marg_uid}'
        self.HOOK          = 'to_stderr {hmid} {marg_uid}'
