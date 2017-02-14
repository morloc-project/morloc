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
        self.SEP                  = ""
        self.BIND                 = ""
        self.AND                  = ""
        self.POOL                 = ""
        self.TYPE_MAP             = ""
        self.TYPE_ACCESS          = ""
        self.CAST_NAT2UNI         = ""
        self.CAST_UNI2NAT         = ""
        self.SIMPLE_MANIFOLD      = ""
        self.SIMPLE_MANIFOLD_BLK  = ""
        self.NATIVE_MANIFOLD      = ""
        self.NATIVE_MANIFOLD_BLK  = ""
        self.FOREIGN_MANIFOLD     = ""
        self.FOREIGN_MANIFOLD_BLK = ""
        self.CACHE                = ""
        self.CACHE_IF             = ""
        self.CACHE_ELSE           = ""
        self.DATCACHE_ARGS        = ""
        self.CACHE_ELSE           = ""
        self.DO_VALIDATE          = ""
        self.DO_VALIDATE_IF       = ""
        self.DO_VALIDATE_ELSE     = ""
        self.NO_VALIDATE          = ""
        self.ARGUMENTS            = ""
        self.MANIFOLD_CALL        = ""
        self.CHECK_CALL           = ""
        self.HOOK                 = ""
        self.INDENT               = None
        self.CACHE_PUT            = ""
        self.MARG                 = ""
        self.LIST                 = ""
        self.FAIL                 = ""
        self.DEFAULT_FAIL         = ""
        self.UID_WRAPPER          = ""
        self.UID                  = ""
        self.MARG_UID             = ""
        self.WRAPPER_NAME         = ""


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
            type_map=self.make_type_map(),
            outdir=self.outdir,
            manifolds='\n\n'.join(mtext)
        )

        return p

    def make_pool(self):
        return self.POOL

    def make_type_map(self):
        NotImplemented

    def make_foreign_manifold(self, m):
        s = self.FOREIGN_MANIFOLD.format(
            mid      = m.mid,
            marg_uid = self.make_marg_uid(m),
            blk      = indent(self.make_foreign_manifold_blk(m), n=self.INDENT)
        )
        return s

    def make_foreign_manifold_blk(self, m):
        s = self.FOREIGN_MANIFOLD_BLK.format(
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
            blk       = indent(self.make_simple_manifold_blk(m), n=self.INDENT)
        )
    def make_simple_manifold_blk(self, m):
        return self.SIMPLE_MANIFOLD_BLK.format(
            function  = m.func,
            arguments = self.make_arguments(m)
        )

    def make_native_manifold(self, m):
        return self.NATIVE_MANIFOLD.format(
            mid      = m.mid,
            marg_uid = self.make_marg_uid(m),
            blk      = indent(self.make_native_manifold_blk(m), n=self.INDENT)
        )

    def make_native_manifold_blk(self, m):
        return self.NATIVE_MANIFOLD_BLK.format(
            hook0 = self.make_hook(m, 0),
            cache = self.make_cache(m),
            hook1 = self.make_hook(m, 1)
        )

    def make_cache(self, m):
        uid = self.make_uid(m)
        cargs = self.make_cache_args(m)
        if(m.cache):
            s = self.CACHE.format(
                cache      = m.cache,
                mid        = m.mid,
                uid        = self.SEP + uid if uid else "",
                cache_args = self.SEP + cargs if cargs else "" ,
                if_blk = indent(self.make_cache_if(m), n=self.INDENT),
                else_blk = indent(self.make_cache_else(m), n=self.INDENT)
            )
        else:
            s = self.make_cache_else(m)
        return s

    def make_cache_if(self, m):
        uid = self.make_uid(m)
        cargs = self.make_cache_args(m)
        return self.CACHE_IF.format(
            cache      = m.cache,
            mid        = m.mid,
            hook8      = self.make_hook(m, 8),
            hook9      = self.make_hook(m, 9),
            uid        = self.SEP + uid if uid else "",
            cache_args = self.SEP + cargs if cargs else "" ,
        )

    def make_cache_else(self, m):
        s = self.CACHE_ELSE.format(
            hook2    = self.make_hook(m, 2),
            validate = self.make_validate(m),
            mid      = m.mid,
            hook3    = self.make_hook(m, 3)
        )
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

    def make_validate(self, m):
        if(m.check):
            s = self.make_do_validate(m)
        else:
            s = self.make_no_validate(m)
        return s

    def make_do_validate(self, m):
        return self.DO_VALIDATE.format(
            checks   = self.make_check(m),
            if_blk   = indent(self.make_do_validate_if(m), n=self.INDENT),
            else_blk = indent(self.make_do_validate_else(m), n=self.INDENT)
        )

    def make_do_validate_if(self, m):
        return self.DO_VALIDATE_IF.format(
            hook4     = self.make_hook(m, 4),
            hook5     = self.make_hook(m, 5),
            function  = m.func,
            arguments = self.make_arguments(m),
            cache_put = self.make_cache_put(m)
        )

    def make_do_validate_else(self, m):
        return self.DO_VALIDATE_ELSE.format(
            hook6     = self.make_hook(m, 6),
            hook7     = self.make_hook(m, 7),
            fail      = self.make_fail(m),
            cache_put = self.make_cache_put(m),
        )

    def make_no_validate(self, m):
        return self.make_do_validate_if(m)

    def make_check(self, m):
        ss = []
        for c in m.check:
            ss.append(self.make_check_call(m, c))
        s = self.AND.join(ss)
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
                inputs.append(self.make_input_manifold(m, n, v, t))
            elif k == "f":
                inputs.append(self.make_input_function(m, n, v, t))
            elif k == "a":
                inputs.append(self.make_input_argument(m, n, v, t))
            elif k == "p":
                inputs.append(self.make_input_positional(m, n, v, t))
            else:
                err("Unexpected argument type '%s'" % k)
        inputs = self.SEP.join(inputs)
        return inputs

    def make_input_manifold(self, m, pos, val, typ):
        return self.MANIFOLD_CALL.format(
            hmid=val,
            marg_uid = self.make_marg_uid(m)
        )

    def make_input_function(self, m, pos, val, typ):
        return self.WRAPPER_NAME.format(mid=val)

    def make_input_argument(self, m, pos, val, typ):
        return self.MARG.format(i=val)

    def make_input_positional(self, m, pos, val, typ):
        return val

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
            type=self.make_type_access(arg)
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
        return self.UID_WRAPPER.format(
            mid = m.mid,
            blk = indent(self.make_uid_wrapper_blk(m), n=self.INDENT)
        )

    def make_uid_wrapper_blk(self, m):
        return self.UID_WRAPPER_BLK.format( mid = m.mid )

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
            s = margs
        return s
