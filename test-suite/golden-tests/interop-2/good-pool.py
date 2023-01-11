#!/usr/bin/env python

import sys
import subprocess
import json
from pymorlocinternals import (mlc_serialize, mlc_deserialize)
from collections import OrderedDict

sys.path = ["/home/z/.morloc/src"] + sys.path

from pybase.core import *
from pybase.data import *

def _morloc_foreign_call(args):
    try:
        sysObj = subprocess.run(
            args,
            stdout=subprocess.PIPE,
            check=True
        )
    except subprocess.CalledProcessError as e:
        sys.exit(str(e))

    return(sysObj.stdout.decode("ascii"))

def m17(x0):
    a1 = mlc_deserialize(x0, ("list",(("float",None))));
    a2 = mlc_map(m15, a1)
    a3 = mlc_serialize(a2, ("list",(("float",None))))
    return(a3)

#  def m15(x0, x1):
#      a0 = mlc_add(1.0, x1)
#      return(a0)
def m15(x1):
    a0 = mlc_add(1.0, x1)
    return(a0)

if __name__ == '__main__':
    try:
        cmdID = int(sys.argv[1])
    except IndexError:
        sys.exit("Internal error in {}: no manifold id found".format(sys.argv[0]))
    except ValueError:
        sys.exit("Internal error in {}: expected integer manifold id".format(sys.argv[0]))
    try:
        dispatch = {
            17: m17,
        }
        f = dispatch[cmdID]
    except KeyError:
        sys.exit("Internal error in {}: no manifold found with id={}".format(sys.argv[0], cmdID))

    result = f(*sys.argv[2:])

    print(result)
