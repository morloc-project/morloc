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
    "void"   : "None"
}

atom_to_universal = {
    "String" : "str({x})",
    "File"   : "str({x})",
    "Int"    : "str({x})",
    "Num"    : "str({x})",
    "Bool"   : "('true' if {x} else 'false')",
    "*"      : "str({x})",
    "Text"   : "write_text({x})",
    "void"   : "None"
}

uni2nat_top = 'import json'

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
        return 'return json.loads(x) if x else "null"'

    def _universal_to_tuple(self, typ):
        return 'return json.loads(x) if x else "null"'

    def _universal_to_array(self, typ):
        return 'return json.loads(x) if x else "null"'

    def _universal_to_wtf(self, typ):
        return 'return json.loads(x) if x else "null"'

    def _wtf_to_universal(self, typ):
        return 'return json.dumps({mid}(*args))'

    def _primitive_to_universal(self, typ):
        return 'return json.dumps({mid}(*args))'

    def _tuple_to_universal(self, typ):
        return 'return json.dumps({mid}(*args))'

    def _array_to_universal(self, typ):
        return 'return json.dumps({mid}(*args))'
