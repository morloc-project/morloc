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

# NOTE: m286 has native args, it didn't deserialize them, why?
#  def m286(x0, x1):
#      a0 = mlc_add(x0, x1)
#      a1 = mlc_serialize(a0, ("float",None))
#      return(a1)
def m286(x0, x1):
    a0 = mlc_deserialize(x0, ("float",None))
    a1 = mlc_deserialize(x1, ("float",None))
    a2 = mlc_add(a0, a1)
    a3 = mlc_serialize(a2, ("float",None))
    return(a3)

def m18(x0):
    a1 = mlc_deserialize(x0, ("list",(("float",None))));
    a2 = mlc_map(m16, a1)
    a3 = mlc_serialize(a2, ("list",(("float",None))))
    return(a3)

# NOTE: extra argument
#  def m16(x0, x1):
#      a0 = mlc_add(1.0, x1)
#      return(a0)
def m16(x1):
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
            286: m286,
            18: m18,
        }
        f = dispatch[cmdID]
    except KeyError:
        sys.exit("Internal error in {}: no manifold found with id={}".format(sys.argv[0], cmdID))

    result = f(*sys.argv[2:])

    print(result)
