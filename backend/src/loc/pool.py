from grammars import *
from util import err,indent,clean

def takes_function(m):
    has_wrapper = False
    for k,a,b in m.input:
        if k == "f":
            has_wrapper = True
    return has_wrapper

def get_margs(n, lang):
    ss = []
    try:
        n = int(n)
    except TypeError:
        err("nargs must be integrel")
    for i in range(n):
        ss.append(MARG[lang].format(i=str(i)))
    margs = SEP[lang].join(ss)
    return margs

def get_uid(m):
    if m.narg:
        uid = UID[m.lang]
    else:
        uid = ""
    return uid

def get_marg_uid(m):
    margs = get_margs(m.narg, m.lang)
    uid = get_uid(m)
    if uid:
        s = MARG_UID[m.lang].format(marg=margs, uid=uid)
    else:
        s = ""
    return s

def get_cache_args(m, outdir):
    if(m.cache == "datcache"):
        args = DATCACHE_ARGS[m.lang].format(outdir=outdir)
    else:
        args = ""
    return args

def get_hook(m, kind):
    hooks = [h for h in m.hook if h.kind == kind]
    ss = []
    for h in hooks:
        ss.append( HOOK[m.lang].format(
            hmid=h.mid,
            marg_uid=get_marg_uid(m)
        ) )
    return '\n'.join(ss)

def checks(m):
    ss = []
    for c in m.check:
        ss.append(CHECK_CALL[m.lang].format(
            hmid=c,
            marg_uid=get_marg_uid(m)
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
            inputs.append(MANIFOLD_CALL[m.lang].format(
                hmid=v,
                marg_uid = get_marg_uid(m)
            ))
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
    uid = get_uid(m)
    uid = SEP[m.lang] + uid if uid else uid
    cache_args = get_cache_args(m, outdir)
    cache_args = SEP[m.lang] + cache_args if cache_args else cache_args
    if(m.cache):
        cache_put = CACHE_PUT[m.lang].format(
            cache=m.cache,
            mid=m.mid,
            uid=uid,
            cache_args=cache_args
        )
    else:
        cache_put = ""

    if takes_function(m):
        function = WRAPPER_NAME[m.lang].format(mid=m.mid)
        args = SEP[m.lang].join([v for k,p,v in m.input if k == 'f'])
    else:
        function = m.func
        args = arguments(m)

    if(m.check):

        if m.fail:
            fail = FAIL[m.lang].format(
                fail=m.fail,
                marg_uid=get_marg_uid(m)
            )
        else:
            fail = DEFAULT_FAIL[m.lang]

        s = DO_VALIDATE[m.lang]
        s = s.format(
            checks    = checks(m),
            hook4     = get_hook(m, 4),
            hook5     = get_hook(m, 5),
            hook6     = get_hook(m, 6),
            hook7     = get_hook(m, 7),
            function  = function,
            arguments = args,
            mid       = m.mid,
            cache_put = cache_put,
            fail      = fail,
            marg      = get_margs(m.narg, m.lang)
        )
    else:
        s = NO_VALIDATE[m.lang]
        s = s.format(
            hook4 = get_hook(m, 4),
            hook5 = get_hook(m, 5),
            function  = function,
            arguments = args,
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
    uid = get_uid(m)
    cargs = get_cache_args(m, outdir)
    if(m.cache):
        s = CACHE[m.lang]
        s = s.format(
            cache      = m.cache,
            mid        = m.mid,
            hook8      = get_hook(m, 8),
            hook9      = get_hook(m, 9),
            uid        = SEP[m.lang] + uid if uid else "",
            cache_args = SEP[m.lang] + cargs if cargs else "" ,
            process    = indent(process(m, outdir), n=INDENT[m.lang])
        )
    else:
        s = process(m, outdir)
    return s

def uid_wrapper(m):
    has_wrapper = takes_function(m)

    if len(m.input) > 1:
        sep = SEP[m.lang]
    else:
        sep = ""


    if has_wrapper:
        wrapper = UID_WRAPPER[m.lang].format(
            mid=m.mid,
            deref=SEP[m.lang].join([v for k,p,v in m.input if k == 'f']),
            sep=sep,
            function=m.func,
            marg=arguments(m)
        )
    else:
        wrapper = ""
    return  wrapper

def native_manifold(m, outdir):
    s = NATIVE_MANIFOLD[m.lang]

    ind = INDENT[m.lang]

    block = indent(cache(m, outdir))

    margs = get_margs(m.narg, m.lang)

    s = s.format(
        uid_wrapper = uid_wrapper(m),
        mid         = m.mid,
        marg_uid    = get_marg_uid(m),
        hook0       = get_hook(m, 0),
        block       = block,
        hook1       = get_hook(m, 1)
    )
    return s

def foreign_manifold(m, lang, outdir, marg=""):
    s = FOREIGN_MANIFOLD[lang]
    s = s.format(
        mid=m.mid,
        foreign_lang=m.lang,
        outdir=outdir,
        marg_uid=get_marg_uid(m)
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
