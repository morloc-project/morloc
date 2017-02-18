from grammar.base_grammar import Grammar

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

{type_map}

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
    if [[ -f "$outdir/$1_tmp" ]]
    then
        cat "$outdir/$1_tmp"
    else
        vtype=${{typemap[$1]}}
        if [[ $vtype == "Int"    ||
              $vtype == "String" ||
              $vtype == "Num"    ||
              $vtype == "Bool"   ||
              $vtype == "File" ]]
        then
            d=$( native_to_universal "$( $@ )" "$vtype" "$outdir" )
        else
            d=$( native_to_universal <( $@ ) "$vtype" "$outdir" )
        fi
        echo $d
    fi
else
    exit 1 
fi'''
        self.TYPE_MAP         = '''\
declare -A typemap
{pairs}
'''
        self.TYPE_ACCESS      = '${{typemap[{mid}]}}'
        self.CAST_NAT2UNI     = 'natural_to_universal {key} {type}'
        self.CAST_UNI2NAT     = 'universal_to_natural {key} {type}'
        self.NATIVE_MANIFOLD = '''\
{mid} () {{
{blk}
}}
'''
        self.NATIVE_MANIFOLD_BLK = '''\
{hook0}
{cache}
{hook1}\
'''
        self.SIMPLE_MANIFOLD = '''
{mid} () {{
{blk}
}}
'''
        self.SIMPLE_MANIFOLD_BLK = '''\
{function} {arguments} {wrapper}\
'''
        self.UID_WRAPPER  = '''\
{mid}_uid=0
wrap_{mid} () {{
{blk}
}}
'''
        self.UID_WRAPPER_BLK  = '''\
{mid}_uid=$(( {mid}_uid + 1 ))
{mid} $@ ${mid}_uid\
'''
        self.UID          = '${nth}'
        self.MARG_UID     = '{marg} {uid}'
        self.WRAPPER_NAME = 'wrap_{mid}'
        self.FOREIGN_MANIFOLD = '''\
{mid} () {{
{blk}
}}
'''
        self.FOREIGN_MANIFOLD_BLK = '''\
u=$($outdir/call.{foreign_lang} {mid}{arg_rep})
universal_to_native "$u" ${{typemap[{mid}]}}\
'''
        self.CACHE = '''\
if {cache}_chk {mid}{uid}
then
{if_blk}
else
{else_blk}
fi
'''
        self.CACHE_IF = '''\
{hook8}
{cache}_get {mid}{uid}
{hook9}
'''
        self.CACHE_ELSE = '''\
{hook2}
{validate}
{hook3}
( cat "$outdir/{mid}_tmp" ; rm "$outdir/{mid}_tmp" )\
'''
        self.DATCACHE_ARGS = ""
        self.DO_VALIDATE = '''\
if [[ {checks} ]]
then
{if_blk}
else
{else_blk}
fi
'''
        self.RUN_BLK = '''\
{hook4}
{function} {arguments}{wrapper} > "$outdir/{mid}_tmp"
{cache_put}
{hook5}\
'''
        self.RUN_BLK_VOID = '''\
{hook4}
{function} {arguments}{wrapper} > /dev/null
> "$outdir/{mid}_tmp"
{cache_put}
{hook5}\
'''
        self.FAIL_BLK = '''\
{hook6}
{fail}> "$outdir/{mid}_tmp"
{cache_put}
{hook7}\
'''
        self.FAIL_BLK_VOID = '''\
{hook6}
{fail} > /dev/null
> "$outdir/{mid}_tmp"
{cache_put}
{hook7}\
'''
        self.FAIL = '''{fail} {marg_uid} '''
        self.DEFAULT_FAIL = ""
        self.NO_VALIDATE = '''\
{hook4}
{function} {arguments}{wrapper} > "$outdir/{mid}_tmp"
{cache_put}
{hook5}'''
        self.CACHE_PUT = '''\
{cache}_put {mid} "$outdir/{mid}_tmp"{other_args}
'''
        self.MARG          = '${i}'
        self.ARGUMENTS     = '{fargs} {inputs}'
        self.MANIFOLD_CALL = '{operator}({hmid} {marg_uid})'
        self.CHECK_CALL    = '$({hmid} {marg_uid}) -eq 1'
        self.HOOK          = 'to_stderr {hmid} {marg_uid}'

        self.BOOL_WRAPPER = '&> /dev/null && echo 1 || echo 0'

    def make_simple_manifold_blk(self, m):
        if m.type == "Bool":
            wrapper = self.BOOL_WRAPPER
        else:
            wrapper = ""
        return self.SIMPLE_MANIFOLD_BLK.format(
            function  = m.func,
            arguments = self.make_arguments(m),
            wrapper   = wrapper
        )

    def make_input_manifold(self, m, pos, val, typ):
        if(typ in ("Int", "Num", "String", "File", "Bool")):
            op = '$'
        else:
            op = '<'
        return self.MANIFOLD_CALL.format(
            hmid=val,
            operator=op,
            marg_uid = self.make_marg_uid(m)
        )

    def make_do_validate_if(self, m):
        if m.type == "void":
            template = self.RUN_BLK_VOID
        else:
            template = self.RUN_BLK

        if m.type == "Bool":
            wrapper = self.BOOL_WRAPPER
        else:
            wrapper = ""
        return template.format(
            mid       = m.mid,
            hook4     = self.make_hook(m, 4),
            hook5     = self.make_hook(m, 5),
            function  = m.func,
            wrapper   = wrapper,
            arguments = self.make_arguments(m),
            cache_put = self.make_cache_put(m)
        )

    def make_foreign_manifold_blk(self, m):
        arg_rep = ""
        for i in range(int(m.narg)):
            i_str = str(i+1)
            arg_rep += '\\\n    $(native_to_universal "$%s" "${typemap[x%s]}" "$outdir")' % (i_str,i_str)
        if m.narg:
            arg_rep += '\\\n    "$%s_uid"' % m.mid

        s = self.FOREIGN_MANIFOLD_BLK.format(
            foreign_lang=m.lang,
            mid=m.mid,
            arg_rep=arg_rep
        )
        return s

    def make_type_map(self):
        types = []
        for k,v in self.manifolds.items():
            types.append("typemap[%s]=%s" % (k, v.type))
            for k,n,m,t in v.input:
                if k == "a":
                    types.append("typemap[x%s]=%s" % (m, t))
        return self.TYPE_MAP.format(pairs='\n'.join(types))
