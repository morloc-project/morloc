from collections import namedtuple

from util import err

Input = namedtuple("Input", ["kind", "npos", "value", "type"])
Farg = namedtuple("Marg", ["npos", "key", "value"])
Hook = namedtuple("Hook", ["kind", "mid"])

class Manifold:
    def __init__(self, mid, lang):
        self.mid  = mid
        self.lang = lang
        self.func = ""
        self.input = [] # [mpfa]:value
        self.check = []
        self.hook = [] # pos:mid
        self.farg = []
        self.fail = ""
        self.cache = ""
        self.mdoc = ""
        self.type = "*"
        self.narg = 0

    def add_input(self, kind, npos, value, vtype):
        try:
            self.input.append(Input(kind, int(npos), value, vtype))
        except ValueError:
            err("Position must be an integer")

    def add_check(self, value):
        try:
            self.check.append(value)
        except ValueError:
            err("Position must be an integer")

    def add_farg(self, npos, key, value):
        try:
            self.farg.append(Farg(int(npos), key, value))
        except ValueError:
            err("Position must be an integer")

    def add_hook(self, kind, mid):
        try:
            self.hook.append(Hook(int(kind), mid))
        except ValueError:
            err("Hook kind must be integer")

    def print(self):
        print("{}".format(self.mid))
        print("  {}<{}>:{}".format(self.func, self.type, self.lang))
        if self.cache:
            print("  cache:{}".format(self.cache))
        if self.mdoc:
            print("  mdoc:{}".format(self.mdoc))
        for r in self.check:
            print("  check:{}".format(self.check))
        if self.fail:
            print("  fail:{}".format(self.fail))
        for r in self.hook:
            print("  hook:{}".format(self.hook))
        for r in sorted(self.input, key=lambda x: x[1]):
            print("  input:{}:{}".format(r[0],r[2]))
        for r in sorted(self.farg, key=lambda x: x[0]):
            print("  arg:{}:[{}]".format(r[1],','.join(r[2])))
