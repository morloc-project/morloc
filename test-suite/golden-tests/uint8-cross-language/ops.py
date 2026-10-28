"""
Python implementations for the uint8-cross-language golden test.

In Python a morloc list is a Python list whatever its elements (NATIVE-7),
so [U8] is a list of ints; compact bytes are spelled `Vector n U8`. Every
function below either produces a list or asserts that its input is one --
if the runtime ever delivers `bytes` instead, the echo and native functions
raise a TypeError that the test surfaces through obs.err.
"""


def py_encode(s):
    return list(s.encode('utf-8'))


def py_decode(b):
    if not isinstance(b, list):
        raise TypeError(
            "py_decode expected a list (the native form of [U8] in "
            "Python), got " + type(b).__name__
        )
    return bytes(b).decode('utf-8')


def py_echo(b):
    if not isinstance(b, list):
        raise TypeError(
            "py_echo expected a list, got " + type(b).__name__
        )
    return b


def py_native(b):
    return "python: " + type(b).__name__ + ", len=" + str(len(b))
