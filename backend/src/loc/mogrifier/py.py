from mogrifier.base_mogrifier import Mogrifier

universal_to_atom = dict()
atom_to_universal = dict()

universal_to_natural = '''
def {name}(x):
    return x
'''

natural_to_universal = '''
def {name}(x):
    return x
'''


class PyMogrifier(Mogrifier):
    def __init__(self, manifolds):

        self.manifolds = manifolds

        # key: name of atomic type
        # val: a function that maps between universal and native
        self.universal_to_atom = universal_to_atom
        self.atom_to_universal = atom_to_universal

        # templates for conversion functions
        self.universal_to_natural = universal_to_natural
        self.natural_to_universal = natural_to_universal
