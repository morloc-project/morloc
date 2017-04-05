from mogrifier.base_mogrifier import Mogrifier
import sys

universal_to_atom = {
    "String" : "str({x})",
    "File"   : "str({x})",
    "Int"    : "int({x})",
    "Num"    : "float({x})",
    "Bool"   : "True if x == 'true' else False",
    "*"      : "str({x})",
    "Text"   : "read_text({x})",
    "Table"  : "read_table({x})",
    "Void"   : "None"
}

atom_to_universal = {
    "String" : "str({x})",
    "File"   : "str({x})",
    "Int"    : "str({x})",
    "Num"    : "str({x})",
    "Bool"   : "('true' if {x} else 'false')",
    "*"      : "str({x})",
    "Text"   : "write_text({x})",
    "Table"  : "write_table({x})",
    "Void"   : "None"
}

uni2nat_top = '''
import json

def read_table(xs):
    # ----- TODO remove the stupid kludge -----
    ## === BEGIN stupid kludge === ##
    n = len(xs[1])
    trans = [str] * n
    for i in range(n):
        try:
            x=int(xs[i])
            trans[i] = int
        except TypeError:
            try:
                x=float(xs[i])
                trans[i] = float
            except TypeError:
                pass
    ## === END stupid kludge === ##

    table = []
    for row in xs.rstrip().split("\\n"):
        table.append([t(x) for x,t in zip(row.split(),trans)])
    return table

def write_table(x):
    return '\\n'.join(['\\t'.join([str(s) for s in line]) for line in x])
'''

nat2uni_top = ''

universal_to_natural = '''
def read_{{mid}}(x):
    # {comment}
    {cast}
'''

natural_to_universal = '''
def show_{{mid}}(*args):
    # {comment}
    try:
        {cast}
    except Exception as e:
        print("failure in call.py::show_{{mid}}", file=sys.stderr)
        print("   %s" % e, file=sys.stderr)
        return None
'''

class PyMogrifier(Mogrifier):
    def __init__(self, manifolds):

        super().__init__(manifolds)

        self.manifolds = manifolds

        # key: name of atomic type
        # val: a function that maps between universal and native
        self.universal_to_atom = universal_to_atom
        self.atom_to_universal = atom_to_universal

        self.uni2nat_top = uni2nat_top
        self.nat2uni_top = nat2uni_top

        # templates for conversion functions
        self.universal_to_natural = universal_to_natural
        self.natural_to_universal = natural_to_universal

    def _universal_to_primitive(self, typ):
        if typ == 'Table':
            s = 'return read_table(x)'
        else:
            s = 'return json.loads(x) if x else "null"'
        return s

    def _universal_to_tuple(self, typ):
        return 'return json.loads(x) if x else "null"'

    def _universal_to_array(self, typ):
        return 'return json.loads(x) if x else "null"'

    def _universal_to_wtf(self, typ):
        return 'return json.loads(x) if x else "null"'

    def _wtf_to_universal(self, typ):
        return 'return json.dumps({mid}(*args))'

    def _primitive_to_universal(self, typ):
        if typ == 'Table':
            s = 'return write_table({mid}(*args))'
        else:
            s = 'return json.dumps({mid}(*args))'
        return s

    def _tuple_to_universal(self, typ):
        return 'return json.dumps(tuple({mid}(*args)))'

    def _array_to_universal(self, typ):
        return 'return json.dumps(list({mid}(*args)))'
