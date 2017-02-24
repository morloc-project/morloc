from util import err,indent,clean

class Mogrifier:
    def __init__(self, manifolds):
        # key: name of atomic type
        # val: a function that maps between universal and native
        self.universal_to_atom = dict()
        self.atom_to_universal = dict()

        # templates for conversion functions
        self.universal_to_natural = None
        self.natural_to_universal = None

    def build_uni2nat(self):
        raise NotImplemented

    def build_nat2uni(self):
        raise NotImplemented

    def _parse_type(self):
        raise NotImplemented

    def _category(self):
        raise NotImplemented

    def _universal_to_primitive(self, typ):
        raise NotImplemented

    def _primitive_to_universal(self, typ):
        raise NotImplemented

    def _universal_to_primitive_tuple(self, typ):
        raise NotImplemented

    def _primitive_tuple_to_universal(self, typ):
        raise NotImplemented

    def _tuple_to_universal(self, typ):
        raise NotImplemented

    def _universal_to_tuple(self, typ):
        raise NotImplemented

    def _universal_to_array(self, typ):
        raise NotImplemented

    def _array_to_universal(self, typ):
        raise NotImplemented
