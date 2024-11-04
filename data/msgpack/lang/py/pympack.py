import ctypes
import sys
import os
from enum import IntEnum
from typing import Union, List, Dict, Tuple

# Load the shared library
lib = ctypes.CDLL(os.path.expanduser('~/.morloc/lib/libmlcmpack.so'))


class MorlocSerialType(IntEnum):
    MORLOC_NIL = 0
    MORLOC_BOOL = 1
    MORLOC_INT = 2
    MORLOC_FLOAT = 3
    MORLOC_STRING = 4
    MORLOC_BINARY = 5
    MORLOC_ARRAY = 6
    MORLOC_MAP = 7
    MORLOC_TUPLE = 8
    MORLOC_BOOL_ARRAY = 9
    MORLOC_INT_ARRAY = 10
    MORLOC_FLOAT_ARRAY = 11
    MORLOC_EXT = 12

class Anything(ctypes.Structure):
    pass

AnythingPtr = ctypes.POINTER(Anything)

class _Data(ctypes.Union):
    _fields_ = [
        ("nil_val", ctypes.c_char),
        ("bool_val", ctypes.c_bool),
        ("int_val", ctypes.c_int),
        ("double_val", ctypes.c_double),
        ("char_arr", ctypes.POINTER(ctypes.c_char)),
        ("bool_arr", ctypes.POINTER(ctypes.c_bool)),
        ("int_arr", ctypes.POINTER(ctypes.c_int)),
        ("float_arr", ctypes.POINTER(ctypes.c_double)),
        ("obj_arr", ctypes.POINTER(AnythingPtr))
    ]

Anything._fields_ = [
    ("type", ctypes.c_int),
    ("size", ctypes.c_size_t),
    ("key", ctypes.c_char_p),
    ("data", _Data)
]


def parse_schema_size(schema: str, index: int) -> tuple[int, int]:
    c = schema[index]
    if '0' <= c <= '9':
        size = ord(c) - ord('0')
    elif 'a' <= c <= 'z':
        size = ord(c) - ord('a') + 10
    elif 'A' <= c <= 'Z':
        size = ord(c) - ord('A') + 36
    elif c == '+':
        size = 62
    elif c == '/':
        size = 63
    else:
        raise ValueError(f"Invalid character for size: {c}")
    return size, index + 1

def parse_schema_key(schema: str, index: int) -> tuple[str, int]:
    key_size, index = parse_schema_size(schema, index)
    key = schema[index:index + key_size]
    return key, index + key_size

def parse_schema_r(schema: str, index: int = 0) -> tuple[list, int]:
    if index >= len(schema):
        return [], index

    c = schema[index]
    index += 1

    if c == 'a':  # SCHEMA_ARRAY
        sub_schema, index = parse_schema_r(schema, index)
        return ['a', sub_schema], index
    elif c == 't':  # SCHEMA_TUPLE
        size, index = parse_schema_size(schema, index)
        tuple_schema = ['t', []]
        for _ in range(size):
            sub_schema, index = parse_schema_r(schema, index)
            tuple_schema[1].append(sub_schema)
        return tuple_schema, index
    elif c == 'm':  # SCHEMA_MAP
        size, index = parse_schema_size(schema, index)
        map_schema = ['m', []]
        for _ in range(size):
            key, index = parse_schema_key(schema, index)
            value_schema, index = parse_schema_r(schema, index)
            map_schema[1].append([key, value_schema])
        return map_schema, index
    elif c == 'z':  # SCHEMA_NIL
        return ([c, []], index)
    elif c == 'b':  # SCHEMA_BOOL
        return [c, []], index
    elif c in ['i', 'u']:  # SCHEMA_SINT or SCHEMA_UINT
        _, index = parse_schema_size(schema, index)
        return [c, []], index
    elif c == 'f':  # SCHEMA_FLOAT
        _, index = parse_schema_size(schema, index)
        return ['f', []], index
    elif c == 's':  # SCHEMA_STRING
        return ['s', []], index
    elif c == 'r':  # SCHEMA_BINARY
        return ['r', []], index
    else:
        raise ValueError(f"Unrecognized schema type '{c}'")

def parse_schema(schema: str) -> list:
    result, _ = parse_schema_r(schema)
    return result

def python_to_parsed_data_r(data, schema, key = None) -> Anything:

    pd = Anything()

    if isinstance(key, str):
        pd.key = key.encode('utf-8')

    if schema[0] == "z":
        pd.size = 0
        pd.data.nil_val = b'\x00'
    elif schema[0] == "b":
        pd.size = 0
        pd.data.bool_val = bool(data)
    elif schema[0] == "i":
        pd.size = 0
        pd.data.int_val = int(data)
    elif schema[0] == "f":
        pd.size = 0
        pd.data.double_val = float(data)
    elif schema[0] == "s":
        encoded = data.encode('utf-8')
        pd.size = len(encoded)
        buffer = ctypes.create_string_buffer(encoded)
        pd.data.char_arr = ctypes.cast(buffer, ctypes.POINTER(ctypes.c_char))
    elif schema[0] == "r":
        pd.size = len(data)
        buffer = ctypes.create_string_buffer(data)
        pd.data.char_arr = ctypes.cast(buffer, ctypes.POINTER(ctypes.c_char))
    elif schema[0] == "a":
        array_schema = schema[1]
        if array_schema[0] == "b":
             pd.size = len(data)
             arr = (ctypes.c_bool * pd.size)(*data)
             pd.data.bool_arr = arr
        elif array_schema[0] == "i":
             pd.size = len(data)
             arr = (ctypes.c_int * pd.size)(*data)
             pd.data.int_arr = arr
        elif array_schema[0] == "f":
             pd.size = len(data)
             arr = (ctypes.c_double * pd.size)(*data)
             pd.data.float_arr = arr
        else:
             pd.size = len(data)
             arr = (AnythingPtr * pd.size)()
             for i, item in enumerate(data):
                 arr[i] = ctypes.pointer(python_to_parsed_data_r(item, array_schema))
             pd.data.obj_arr = arr
    elif schema[0] == "t":
        tuple_schemata = schema[1]
        pd.size = len(tuple_schemata)
        arr = (AnythingPtr * pd.size)()
        for i, (item, element_schema) in enumerate(zip(data, tuple_schemata)):
            arr[i] = ctypes.pointer(python_to_parsed_data_r(item, element_schema))
        pd.data.obj_arr = arr
    elif schema[0] == "m":
        kwargs_schemata = schema[1]
        pd.size = len(kwargs_schemata)
        arr = (AnythingPtr * pd.size)()
        for (i, (k, s)) in enumerate(kwargs_schemata):
            arr[i] = ctypes.pointer(python_to_parsed_data_r(data[k], s, key=k))
        pd.data.obj_arr = arr
    else:
        print(f"Bad schema: {str(schema)}", file=sys.stderr)

    return pd

def python_to_parsed_data(data, schema: str) -> Anything:
    return python_to_parsed_data_r(data, parse_schema(schema), key = None)

def parsed_data_to_python(pd: Anything) -> Union[None, bool, int, float, str, bytes, List, Dict, Tuple]:
    if pd.type == MorlocSerialType.MORLOC_NIL:
        return None
    elif pd.type == MorlocSerialType.MORLOC_BOOL:
        return pd.data.bool_val
    elif pd.type == MorlocSerialType.MORLOC_INT:
        return pd.data.int_val
    elif pd.type == MorlocSerialType.MORLOC_FLOAT:
        return pd.data.double_val
    elif pd.type == MorlocSerialType.MORLOC_STRING:
        return pd.data.char_arr[:pd.size].decode('utf-8')
    elif pd.type == MorlocSerialType.MORLOC_BINARY:
        return pd.data.char_arr[:pd.size]
    elif pd.type == MorlocSerialType.MORLOC_ARRAY:
        return [parsed_data_to_python(pd.data.obj_arr[i].contents) for i in range(pd.size)]
    elif pd.type == MorlocSerialType.MORLOC_MAP:
        return {pd.data.obj_arr[i].contents.key.decode('utf-8') if pd.data.obj_arr[i].contents.key else None:
                parsed_data_to_python(pd.data.obj_arr[i].contents) for i in range(pd.size)}
    elif pd.type == MorlocSerialType.MORLOC_TUPLE:
        return tuple(parsed_data_to_python(pd.data.obj_arr[i].contents) for i in range(pd.size))
    elif pd.type == MorlocSerialType.MORLOC_BOOL_ARRAY:
        return [pd.data.bool_arr[i] for i in range(pd.size)]
    elif pd.type == MorlocSerialType.MORLOC_INT_ARRAY:
        return [pd.data.int_arr[i] for i in range(pd.size)]
    elif pd.type == MorlocSerialType.MORLOC_FLOAT_ARRAY:
        return [pd.data.float_arr[i] for i in range(pd.size)]
    elif pd.type == MorlocSerialType.MORLOC_EXT:
        return bytes(pd.data.char_arr[:pd.size])
    else:
        raise ValueError(f"Unknown Anything type: {pd.type}")

# Update the function signatures
lib.pack.argtypes = [ctypes.POINTER(Anything), ctypes.c_char_p, ctypes.POINTER(ctypes.POINTER(ctypes.c_char)), ctypes.POINTER(ctypes.c_size_t)]
lib.pack.restype = ctypes.c_int

lib.unpack.argtypes = [ctypes.POINTER(ctypes.c_char), ctypes.c_size_t, ctypes.c_char_p, ctypes.POINTER(ctypes.POINTER(Anything))]
lib.unpack.restype = ctypes.c_int

def pack_data(data: Anything, schema: str) -> bytes:
    out_data = ctypes.POINTER(ctypes.c_char)()
    out_size = ctypes.c_size_t()

    # Convert Python string to bytes, then to c_char_p
    schema_bytes = schema.encode('utf-8')
    schema_c_str = ctypes.c_char_p(schema_bytes)

    result = lib.pack(ctypes.byref(data), schema_c_str, ctypes.byref(out_data), ctypes.byref(out_size))
    if result != 0:
        raise RuntimeError("Packing failed")

    packed_data = ctypes.string_at(out_data, out_size.value)

    return packed_data


def unpack_data(packed_data: bytes, schema: str) -> Anything:
    data_ptr = ctypes.cast(packed_data, ctypes.POINTER(ctypes.c_char))
    out_data = ctypes.POINTER(Anything)()

    # Convert Python string to bytes, then to c_char_p
    schema_bytes = schema.encode('utf-8')
    schema_c_str = ctypes.c_char_p(schema_bytes)

    result = lib.unpack(data_ptr, len(packed_data), schema_c_str, ctypes.byref(out_data))
    if result != 0:
        raise RuntimeError("Unpacking failed")

    # Create a deep copy of the Anything
    parsed_data = copy_parsed_data(out_data.contents)

    return parsed_data


def copy_parsed_data(data: Anything) -> Anything:
    new_data = Anything()
    new_data.type = data.type
    new_data.size = data.size
    new_data.key = ctypes.c_char_p(data.key) if data.key else None

    if data.type == MorlocSerialType.MORLOC_NIL:
        new_data.data.nil_val = data.data.nil_val
    elif data.type == MorlocSerialType.MORLOC_BOOL:
        new_data.data.bool_val = data.data.bool_val
    elif data.type == MorlocSerialType.MORLOC_INT:
        new_data.data.int_val = data.data.int_val
    elif data.type == MorlocSerialType.MORLOC_FLOAT:
        new_data.data.double_val = data.data.double_val
    elif data.type == MorlocSerialType.MORLOC_STRING:
        new_data.size = data.size  # Ensure the size is copied
        new_buffer = (ctypes.c_char * data.size)()
        ctypes.memmove(new_buffer, data.data.char_arr, data.size)
        new_data.data.char_arr = ctypes.cast(new_buffer, ctypes.POINTER(ctypes.c_char))
    elif data.type == MorlocSerialType.MORLOC_BINARY:
        new_data.size = data.size  # Ensure the size is copied
        new_buffer = (ctypes.c_char * data.size)()
        ctypes.memmove(new_buffer, data.data.char_arr, data.size)
        new_data.data.char_arr = ctypes.cast(new_buffer, ctypes.POINTER(ctypes.c_char))
    elif data.type in [MorlocSerialType.MORLOC_ARRAY, MorlocSerialType.MORLOC_MAP, MorlocSerialType.MORLOC_TUPLE]:
        new_arr = (AnythingPtr * data.size)()
        for i in range(data.size):
            new_arr[i] = ctypes.pointer(copy_parsed_data(data.data.obj_arr[i].contents))
        new_data.data.obj_arr = new_arr
    elif data.type == MorlocSerialType.MORLOC_BOOL_ARRAY:
        new_arr = (ctypes.c_bool * data.size)()
        ctypes.memmove(new_arr, data.data.bool_arr, data.size * ctypes.sizeof(ctypes.c_bool))
        new_data.data.bool_arr = new_arr
    elif data.type == MorlocSerialType.MORLOC_INT_ARRAY:
        new_arr = (ctypes.c_int * data.size)()
        ctypes.memmove(new_arr, data.data.int_arr, data.size * ctypes.sizeof(ctypes.c_int))
        new_data.data.int_arr = new_arr
    elif data.type == MorlocSerialType.MORLOC_FLOAT_ARRAY:
        new_arr = (ctypes.c_double * data.size)()
        ctypes.memmove(new_arr, data.data.float_arr, data.size * ctypes.sizeof(ctypes.c_double))
        new_data.data.float_arr = new_arr
    elif data.type == MorlocSerialType.MORLOC_EXT:
        new_data.data.char_arr = ctypes.c_char_p(ctypes.cast(data.data.char_arr, ctypes.c_char_p).value)
    else:
        raise ValueError(f"Unknown Anything type: {data.type}")

    return new_data



def pack(py_data, schema: str) -> bytes:
    # Convert Python data to Anything
    parsed_data = python_to_parsed_data(py_data, schema)

    # Translate to MessagePack
    return pack_data(parsed_data, schema)



def unpack(msgpack_data, schema: str):
    # Unpack the data
    unpacked_data = unpack_data(msgpack_data, schema)

    # Convert Anything back to Python
    return parsed_data_to_python(unpacked_data)
