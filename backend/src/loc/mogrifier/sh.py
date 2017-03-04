from mogrifier.base_mogrifier import Mogrifier

universal_to_atom = {
    "Int"    : 'echo ${x}',
    "Num"    : 'echo ${x}',
    "String" : """printf '"%s"' ${x}""",
    "File"   : """printf '"%s"' ${x}""",
    "Bool"   : 'echo ${x}',
    "Text"   : 'cat "${x}"',
    "void"   : 'echo -n'
}

atom_to_universal = {
    "Int"    : 'echo ${x}',
    "Num"    : 'echo ${x}',
    "String" : """printf '"%s"' ${x}""",
    "File"   : """printf '"%s"' ${x}""",
    "Bool"   : 'echo ${x}',
    "Text"   : 'cat "${x}"',
    "void"   : 'echo -n'
}

uni2nat_top = ''
nat2uni_top = ''

universal_to_natural = '''
read_{mid} (){{
    x=$({mid})
    {cast}
}}
'''

natural_to_universal = '''
show_{mid} (){{
    x=$({mid})
    {cast}
}}
'''

class ShMogrifier(Mogrifier):
    def __init__(self, manifolds):

        super().__init__(manifolds) 

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
        return "echo 'ladida'"

    def _universal_to_tuple(self, typ):
        return "echo 'ladida'"

    def _universal_to_array(self, typ):
        return "echo 'ladida'"

    def _primitive_to_universal(self, typ):
        val = self.atom_to_universal[typ].format(x="x")
        s = '''printf '%%s' $(%s)''' % val
        return s

    def _tuple_to_universal(self, typ, inner):
        return "echo 'ladida'"

    def _array_to_universal(self, typ, inner):
        return '''
    echo -n '['
    echo -n $(echo $x | sed 's/\([^ ]\)/\\1/g' | tr ' ' ',')
    echo ']'
        '''
