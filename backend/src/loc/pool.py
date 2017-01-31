from grammars import *
from util import err,indent,clean

def get_margs(n, lang):
    ss = []
    for i in range(n):
        ss.append(MARG[lang].format(i=i))
    margs = SEP[lang].join(ss)
    return margs

def get_uid(m):
    return ""

def get_cache_args(m, outdir):
    if(m.cache == "datcache"):
        args = DATCACHE_ARGS[m.lang].format(outdir=outdir)
    else:
        args = ""
    return args

def get_hook(m, kind):
    hooks = [h for h in m.hook if h.kind == kind]
    ss = []
    margs = get_margs(m.narg, m.lang)
    for h in hooks:
        ss.append( HOOK[m.lang].format(hmid=h.mid, margs=margs) )
    return '\n'.join(ss)

def checks(m):
    ss = []
    for c in m.check:
        ss.append(CHECK_CALL[m.lang].format(
            hmid=c,
            margs=get_margs(m.narg, m.lang)
        ))
    s = SEP[m.lang].join(ss)
    return s

def arguments(m):
    if(m.input and m.farg):
        sep=SEP[m.lang]
    else:
        sep=""

    margs = get_margs(m.narg, m.lang)

    inputs = []
    for k,n,v in m.input:
        if k == "m":
            inputs.append(MANIFOLD_CALL[m.lang].format(hmid=v, margs=margs))
        else:
            inputs.append(v)
    inputs = SEP[m.lang].join(inputs)

    fargs = []
    for n,k,vs in m.farg:
        v = SEP[m.lang].join(vs)
        if len(vs) > 1:
            v = LIST[m.lang].format(values=v)

        if k:
            fargs.append(BIND[m.lang].join((k,v)))
        else:
            fargs.append(v)

    fargs = SEP[m.lang].join(fargs)

    s = ARGUMENTS[m.lang]
    s = s.format(
        inputs=inputs,
        sep=sep,
        fargs=fargs
    )
    return s

def validate(m, outdir):
    if(m.cache):
        cache_put = CACHE_PUT[m.lang].format(
            cache=m.cache,
            mid=m.mid,
            uid_arg=get_uid(m),
            cache_args=get_cache_args(m, outdir)
        )
    else:
        cache_put = ""
    if(m.check):
        s = DO_VALIDATE[m.lang]
        s = s.format(
            checks = checks(m),
            hook4 = get_hook(m, 4),
            hook5 = get_hook(m, 5),
            hook6 = get_hook(m, 6),
            hook7 = get_hook(m, 7),
            function = m.func,
            arguments = arguments(m),
            mid = m.mid,
            cache_put = cache_put,
            margs = get_margs(m.narg, m.lang)
        )
    else:
        s = NO_VALIDATE[m.lang]
        s = s.format(
            hook4 = get_hook(m, 4),
            hook5 = get_hook(m, 5),
            function = m.func,
            arguments = arguments(m),
            mid = m.mid,
            cache_put = cache_put
        )
    return s

def process(m, outdir):
    s = PROCESS[m.lang]
    s = s.format(
        hook2=get_hook(m, 2),
        validate=validate(m, outdir),
        mid=m.mid,
        hook3=get_hook(m, 3)
    )
    return s

def cache(m, outdir):
    if(m.cache):
        s = CACHE[m.lang]
        s = s.format(
            cache      = m.cache,
            mid        = m.mid,
            hook8      = get_hook(m, 2),
            hook9      = get_hook(m, 2),
            uid_arg    = get_uid(m),
            cache_args = get_cache_args(m, outdir),
            process    = indent(process(m, outdir), n=INDENT[m.lang])
        )
    else:
        s = process(m, outdir)
    return s

def native_manifold(m, outdir):
    s = NATIVE_MANIFOLD[m.lang]

    ind = INDENT[m.lang]

    block = indent(cache(m, outdir))

    margs = get_margs(m.narg, m.lang)

    s = s.format(
        mid   = m.mid,
        marg  = margs,
        hook0 = get_hook(m, 0),
        block = block,
        hook1 = get_hook(m, 1)
    )
    return s

def foreign_manifold(m, lang, outdir, marg=""):
    margs = get_margs(m.narg, lang)
    s = FOREIGN_MANIFOLD[lang]
    s = s.format(
        mid=m.mid,
        foreign_lang=m.lang,
        outdir=outdir,
        marg=margs
    )
    return s

def build_pool(
    lang,
    source,
    manifolds,
    outdir,
    home
):
    try:
        p = POOL[lang]
    except KeyError:
        err("Language '%s' is not supported" % lang)

    try:
        src = source[lang]
    except KeyError:
        src = ""

    mtext = []
    for k,v in manifolds.items():
        if v.lang == lang:
            s = native_manifold(v, outdir)
        else:
            s = foreign_manifold(v, lang, outdir)
        s = clean(s)
        mtext.append(s)
    p = p.format(
        source=src,
        outdir=outdir,
        manifolds='\n\n'.join(mtext)
    )

    return p
