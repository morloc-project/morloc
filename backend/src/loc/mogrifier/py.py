from mogrifier.base_mogrifier import Mogrifier

universal_to_atom = {
    "String" : "str(%s)",
    "File"   : "str(%s)",
    "Int"    : "int(%s)",
    "Num"    : "float(%s)",
    "Bool"   : "bool(%s)",
    "*"      : "str(%s)",
    "Text"   : "read_text(%s)",
    "void"   : "None"
}

atom_to_universal = {
    "String" : "str(%s)",
    "File"   : "str(%s)",
    "Int"    : "str(%s)",
    "Num"    : "str(%s)",
    "Bool"   : "str(int(%s))",
    "*"      : "str(%s)",
    "Text"   : "write_text(%s)",
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
        pass

    def _universal_to_tuple(self, typ):
        pass

    def _universal_to_array(self, typ):
        pass

    def _primitive_to_universal(self, typ):
        val = self.atom_to_universal[typ] % "x"
        s = ("s = '%s'" % (self.json_template % typ)) + (" %% %s" % val)
        return s

    def _tuple_to_universal(self, typ, inner):
        val = "[" + ','.join(inner) + "]"
        return self.json_template % (typ, val)

    def _array_to_universal(self, typ):
        typ = "[" + ','.join(inner) + "]"
        return self.json_template % (typ, val)
