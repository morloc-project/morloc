from grammars import *
from util import err,indent

def get_margs(n, lang):
    ss = []
    for i in range(n):
        ss.append(MARG[lang].format(i=i))
    margs = SEP[lang].join(ss)
    return margs

def get_hook(m, kind):
    hooks = [h for h in m.hook if h.kind == kind]
    ss = []
    margs = get_margs(m.narg, m.lang)
    for h in hooks:
        ss.append( HOOK[m.lang].format(mid=m.mid, margs=margs) )
    return '\n'.join(ss)

def process(m):
    return "process"

def validate(m):
    return "validate"

def cache(m):
    s = CACHE[m.lang]

def native_manifold(m):
    s = NATIVE_MANIFOLD[m.lang]

    ind = INDENT[m.lang]

    if(m.cach):
        block = indent(cache(m), n=ind)
    else:
        if(m.chek):
            block = indent(validate(m), n=ind)
        else:
            block = indent(process(m), n=ind)

    margs = get_margs(m.narg, m.lang)

    s = s.format(
        mid=m.mid,
        marg=margs,
        hook0=get_hook(m, 0),
        block=block,
        hook1=get_hook(m, 1)
    )
    return s

def foreign_manifold(m, lang, outdir, marg=""):
    margs = get_margs(m.narg, lang)
    s = FOREIGN_MANIFOLD[lang]
    s = s.format(mid=m.mid, lang=lang, outdir=outdir, marg=margs)
    return s

def build_pool(
    lang,
    manifolds,
    outdir,
    home
):
    try:
        p = POOL[lang]
    except KeyError:
        err("Language '%s' is not supported" % lang)

    mtext = []
    for k,v in manifolds.items():
        if v.lang == lang:
            s = native_manifold(v)
        else:
            s = foreign_manifold(v, lang, outdir)
        mtext.append(s)


    p = p.format(manifolds='\n'.join(mtext))

    return p
