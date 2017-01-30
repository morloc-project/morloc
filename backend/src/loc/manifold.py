from collections import namedtuple

import my_util

Input = namedtuple("Input", ["kind", "npos", "value"])
Farg = namedtuple("Marg", ["npos", "key", "value"])
Check = namedtuple("Check", ["npos", "value"])
Hook = namedtuple("Hook", ["kind", "mid"])

class Manifold:
    def __init__(self, mid, lang):
        self.mid  = mid
        self.lang = lang
        self.func = ""
        self.inpt = [] # [mpfa]:value
        self.chek = []
        self.hook = [] # pos:mid
        self.farg = []
        self.fail = ""
        self.cach = ""
        self.mdoc = ""
        self.type = "*"
        self.narg = 0

    def add_input(self, kind, npos, value):
        try:
            self.inpt.append(Input(kind, int(npos), value))
        except ValueError:
            err("Position must be an integer")

    def add_check(self, npos, value):
        try:
            self.check.append(Check(int(npos), value))
        except ValueError:
            err("Position must be an integer")

    def add_farg(self, npos, key, value):
        try:
            self.farg.append(Farg(kind, int(npos), value))
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
        if self.cach:
            print("  cache:{}".format(self.cach))
        if self.mdoc:
            print("  mdoc:{}".format(self.mdoc))
        for r in self.chek:
            print("  check:{}".format(self.chek))
        if self.fail:
            print("  fail:{}".format(self.fail))
        for r in self.hook:
            print("  hook:{}".format(self.hook))
        for r in sorted(self.inpt, key=lambda x: x[1]):
            print("  input:{}:{}".format(r[0],r[2]))
        for r in sorted(self.farg, key=lambda x: x[0]):
            print("  arg:{}:[{}]".format(r[1],','.join(r[2])))
