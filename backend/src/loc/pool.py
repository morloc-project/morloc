import grammars
from util import err,indent,clean

def get_margs(n, grm):
    ss = []
    try:
        n = int(n)
    except TypeError:
        err("nargs must be integrel")
    for i in range(n):
        ss.append(grm.MARG.format(i=str(i+1)))
    margs = grm.SEP.join(ss)
    return margs

def get_uid(m, grm):
    if m.narg:
        uid = grm.UID.format(nth=str(int(m.narg)+1))
    else:
        uid = ""
    return uid

def get_marg_uid(m, grm):
    margs = get_margs(m.narg, grm)
    uid = get_uid(m, grm)
    if uid:
        s = grm.MARG_UID.format(marg=margs, uid=uid)
    else:
        s = ""
    return s

def get_cache_args(m, outdir, grm):
    if(m.cache == "datcache"):
        args = grm.DATCACHE_ARGS.format(outdir=outdir)
    else:
        args = ""
    return args

def get_hook(m, kind, grm):
    hooks = [h for h in m.hook if h.kind == kind]
    ss = []
    for h in hooks:
        ss.append( grm.HOOK.format(
            hmid=h.mid,
            marg_uid=get_marg_uid(m, grm)
        ) )
    return '\n'.join(ss)

def checks(m, grm):
    ss = []
    for c in m.check:
        ss.append(grm.CHECK_CALL.format(
            hmid=c,
            marg_uid=get_marg_uid(m, grm)
        ))
    s = grm.SEP.join(ss)
    return s

def arguments(m, grm):
    if(m.input and m.farg):
        sep=grm.SEP
    else:
        sep=""

    margs = get_margs(m.narg, grm)

    inputs = []
    for k,n,v in m.input:
        if k == "m":
            inputs.append(grm.MANIFOLD_CALL.format(
                hmid=v,
                marg_uid = get_marg_uid(m, grm)
            ))
        elif k == "f":
            inputs.append(grm.WRAPPER_NAME.format(mid=v))
        elif k == "a":
            inputs.append(grm.MARG.format(i=v))
        else:
            inputs.append(v)
    inputs = grm.SEP.join(inputs)

    fargs = []
    for n,k,vs in m.farg:
        v = grm.SEP.join(vs)
        if len(vs) > 1:
            v = grm.LIST.format(values=v)

        if k:
            fargs.append(grm.BIND.join((k,v)))
        else:
            fargs.append(v)

    fargs = grm.SEP.join(fargs)

    s = grm.ARGUMENTS.format(
        inputs = inputs,
        sep    = sep,
        fargs  = fargs
    )
    return s

def validate(m, outdir, grm):
    uid = get_uid(m, grm)
    uid = grm.SEP + uid if uid else uid
    cache_args = get_cache_args(m, outdir, grm)
    cache_args = grm.SEP + cache_args if cache_args else cache_args
    if(m.cache):
        cache_put = grm.CACHE_PUT.format(
            cache      = m.cache,
            mid        = m.mid,
            uid        = uid,
            cache_args = cache_args
        )
    else:
        cache_put = ""

    if(m.check):
        if m.fail:
            fail = grm.FAIL.format(
                fail=m.fail,
                marg_uid=get_marg_uid(m, grm)
            )
        else:
            fail = grm.DEFAULT_FAIL
        s = grm.DO_VALIDATE.format(
            checks    = checks(m, grm),
            hook4     = get_hook(m, 4, grm),
            hook5     = get_hook(m, 5, grm),
            hook6     = get_hook(m, 6, grm),
            hook7     = get_hook(m, 7, grm),
            function  = m.func,
            arguments = arguments(m, grm),
            mid       = m.mid,
            cache_put = cache_put,
            fail      = fail,
            marg      = get_margs(m.narg, grm)
        )
    else:
        s = grm.NO_VALIDATE.format(
            hook4     = get_hook(m, 4, grm),
            hook5     = get_hook(m, 5, grm),
            function  = m.func,
            arguments = arguments(m, grm),
            mid       = m.mid,
            cache_put = cache_put
        )
    return s

def process(m, outdir, grm):
    s = grm.PROCESS.format(
        hook2    = get_hook(m, 2, grm),
        validate = validate(m, outdir, grm),
        mid      = m.mid,
        hook3    = get_hook(m, 3, grm)
    )
    return s

def cache(m, outdir, grm):
    uid = get_uid(m, grm)
    cargs = get_cache_args(m, outdir, grm)
    if(m.cache):
        s = grm.CACHE.format(
            cache      = m.cache,
            mid        = m.mid,
            hook8      = get_hook(m, 8, grm),
            hook9      = get_hook(m, 9, grm),
            uid        = grm.SEP + uid if uid else "",
            cache_args = grm.SEP + cargs if cargs else "" ,
            process    = indent(process(m, outdir, grm), n=grm.INDENT)
        )
    else:
        s = process(m, outdir, grm)
    return s

def native_manifold(m, outdir, grm):
    ind   = grm.INDENT
    block = indent(cache(m, outdir, grm), n=ind)
    margs = get_margs(m.narg, grm)
    return grm.NATIVE_MANIFOLD.format(
        mid         = m.mid,
        marg_uid    = get_marg_uid(m, grm),
        hook0       = get_hook(m, 0, grm),
        block       = block,
        hook1       = get_hook(m, 1, grm)
    )

def foreign_manifold(m, outdir, grm):
    s = grm.FOREIGN_MANIFOLD.format(
        mid          = m.mid,
        foreign_lang = m.lang,
        outdir       = outdir,
        marg_uid     = get_marg_uid(m, grm)
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
        grm = grammars.grammar[lang]
    except KeyError:
        err("'%s' is not a supported language" % lang)

    p = grm.POOL

    try:
        src = source[lang]
    except KeyError:
        src = ""

    wrappers = set()
    for mid,man in manifolds.items():
        for k,n,m in man.input:
            if k == "f":
                wrappers.add(m)

    mtext = []
    for k,v in manifolds.items():
        if k in wrappers:
            w = grm.UID_WRAPPER.format(mid=v.mid)
            mtext.append(w)
        if v.lang == lang:
            s = native_manifold(v, outdir, grm)
        else:
            s = foreign_manifold(v, outdir, grm)
        s = clean(s)
        mtext.append(s)

    p = p.format(
        source=src,
        outdir=outdir,
        manifolds='\n\n'.join(mtext)
    )

    return p
