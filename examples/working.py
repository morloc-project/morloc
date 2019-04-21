#!/usr/bin/env python

import sys
import subprocess
import json

sys.path = ['/home/z/.morloc'] + sys.path
from lib.pybase.pybase import *
from lib.bio.bio import *

def _morloc_unpack(unpacker, jsonString, mid, filename):
    try:
        pyObj = unpacker(jsonString)
    except Exception:
        msg = "Error in %s::%s - JSON parse failure" % (filename, mid)
        if(len(jsonString) == 0):
            msg += ": empty document"
        else:
            msg += ", bad document:\n%s" % str(jsonString)
        sys.exit(msg)
    return(pyObj)

def _morloc_foreign_call(interpreter, pool, mid, args):
    try:
        sysObj = subprocess.run(
            [interpreter, pool, mid, *args],
            capture_output=True,
            check=True
        )
    except subprocess.CalledProcessError as e:
        sys.exit(str(e))
    
    jsonString = sysObj.stdout
    jsonLog = sysObj.stderr
    
    if(len(jsonLog) > 0):
        print(jsonLog, file=sys.stderr)
    
    return(jsonString)

# source manifold
# composition :: (String -> DataFrame)
def bio__bio_loc_2(x0):
    a0 = _morloc_unpack(unpackGeneric
                       ,x0
                       ,mid="bio__bio_loc_2"
                       ,filename="pool.py")
    return(composition(a0))



dispatch = dict(bio__bio_loc_2=bio__bio_loc_2)
dispatchGeneric = dict(bio__bio_loc_2=packDataFrame)

if __name__ == '__main__':
    script_name = sys.argv[0] 

    try:
        cmd = sys.argv[1]
    except IndexError:
        sys.exit("Internal error in {}: no manifold id found".format(script))

    try:
        function = dispatch[cmd]
    except KeyError:
        sys.exit("Internal error in {}: expected manifold id (e.g. m34), got {}".format(script, cmd))

    args = sys.argv[2:]

    print(unpackDataFrame(function(*args)))
