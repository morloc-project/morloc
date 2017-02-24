from util import err,indent,clean
import sys

class Mogrifier:
    def __init__(self, manifolds):
        self.manifolds = manifolds

        # key: name of atomic type
        # val: a function that maps between universal and native
        self.universal_to_atom = dict()
        self.atom_to_universal = dict()

        # templates for conversion functions
        self.universal_to_natural = None
        self.natural_to_universal = None

    def build_uni2nat(self):
        out = []
        for m in self.manifolds.values():
            function_name = "read_" + m.mid
            s = self.universal_to_natural.format(
                name=function_name
            )
            out.append(s)
        return '\n'.join(out)

    def build_nat2uni(self):
        out = []
        for m in self.manifolds.values():
            function_name = "show_" + m.mid
            s = self.natural_to_universal.format(
                name=function_name
            )
            out.append(s)
        return '\n'.join(out)

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
