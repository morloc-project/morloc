import os

# Conversion
def to_bytes(s):
    return s.encode("utf-8")

def from_bytes(b):
    return b.decode("utf-8")

# Filelike
def get_extension(path):
    return os.path.splitext(path)[1]

# Stringlike (Str)
def split_str(sep, s):
    return s.split(sep)

def trim_str(s):
    return s.strip()

# Stringlike (Bytes)
def split_bytes(sep, b):
    return b.split(sep.encode("utf-8"))

def trim_bytes(b):
    return b.strip()
