from mogrifier.base_mogrifier import Mogrifier
import sys

universal_to_atom = {
    "String" : "str({x})",
    "File"   : "str({x})",
    "Int"    : "int({x})",
    "Num"    : "float({x})",
    "Bool"   : "bool({x})",
    "*"      : "str({x})",
    "Text"   : "read_text({x})",
    "void"   : "None"
}

atom_to_universal = {
    "String" : "str({x})",
    "File"   : "str({x})",
    "Int"    : "str({x})",
    "Num"    : "str({x})",
    "Bool"   : "str(int({x}))",
    "*"      : "str({x})",
    "Text"   : "write_text({x})",
    "void"   : "None"
}

uni2nat_top = ''

nat2uni_top = ''

universal_to_natural = '''
def read_{mid}():
    x = {mid}()
    {cast}
    return s
'''

natural_to_universal = '''
def show_{mid}():
    x = {mid}()
    {cast}
    return s 
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
        return "return 'ladida'"

    def _universal_to_tuple(self, typ):
        return "return 'ladida'"

    def _universal_to_array(self, typ):
        return "return 'ladida'"

    def _primitive_to_universal(self, typ):
        s = self.primitive_json_template % typ
        val = self.atom_to_universal[typ].format(x="x")
        return "s = '%s' %% %s" % (s, val)

    def _tuple_to_universal(self, typ, inner):
        return "return 'ladida'"

    def _array_to_universal(self, typ, inner):
        typ = '[%s]' % typ
        s = "s = '%s' %% val" % (self.json_template % typ)
        val = """val = '[{}]'.format(','.join('"%s"' % str(y) for y in x))"""
        s = val + "\n    " + s
        return s
