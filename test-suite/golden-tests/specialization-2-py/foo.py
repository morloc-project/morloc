def pid(x):
    return x

def changebytes(x):
    if isinstance(x, bytes):
        return x + b'!'
    else:
        raise TypeError(f"Expected bytes, found {type(x)!s}")

def changebytearray(x):
    if isinstance(x, bytearray):
        return x + b'!'
    else:
        raise TypeError(f"Expected bytearray, found {type(x)!s}")
