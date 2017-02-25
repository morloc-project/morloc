from mogrifier.base_mogrifier import Mogrifier

universal_to_atom = dict()
atom_to_universal = dict()

uni2nat_top = ''
nat2uni_top = ''

universal_to_natural = '''
{name} (){{
    cat "$1" 2> /dev/null || echo "$1"
}}
'''

natural_to_universal = '''
{name} (){{
    cat "$1" 2> /dev/null || echo "$1"
}}
'''

class ShMogrifier(Mogrifier):
    def __init__(self, manifolds):

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
