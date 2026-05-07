"""
Python implementations for the uint8-cross-language golden test.

The morloc type [UInt8] maps to Python `bytes` here (via root-py's
`type Py => (List UInt8) = "bytes" UInt8` specialisation). Every
function below either produces a `bytes` value or asserts that its
input is a `bytes` value -- if the runtime ever delivers a `list`
instead (e.g. because of a wire-deserialiser regression), the echo
and native functions raise a TypeError that the test will surface
through obs.err.
"""


def py_encode(s):
    return s.encode('utf-8')


def py_decode(b):
    if not isinstance(b, bytes):
        raise TypeError(
            "py_decode expected bytes (the contracted native form of "
            "[UInt8] in Python), got " + type(b).__name__
        )
    return b.decode('utf-8')


def py_echo(b):
    if not isinstance(b, bytes):
        raise TypeError(
            "py_echo expected bytes, got " + type(b).__name__
        )
    return b


def py_native(b):
    return "python: " + type(b).__name__ + ", len=" + str(len(b))
