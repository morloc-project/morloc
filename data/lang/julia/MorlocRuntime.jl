"""
    MorlocRuntime

Julia runtime module for morloc. Provides IPC (daemon lifecycle, packet I/O),
msgpack-based serialization, and foreign call support.

All heavy lifting is done by libmorloc via the thin C bridge (libjuliamorloc.so).
Julia handles only the msgpack <-> native type conversion using MsgPack.jl.
"""
module MorlocRuntime

using MsgPack

# Path to the bridge shared library (set during morloc init)
const LIB_PATH = Ref{String}("")

function __init__()
    # Look for libjuliamorloc.so relative to this file, or in standard locations
    candidates = [
        joinpath(dirname(@__FILE__), "libjuliamorloc.so"),
        joinpath(dirname(@__FILE__), "..", "lib", "libjuliamorloc.so"),
    ]
    # Also check the morloc home lib directory
    morloc_home = get(ENV, "MORLOC_HOME", joinpath(homedir(), ".local", "share", "morloc"))
    push!(candidates, joinpath(morloc_home, "lib", "libjuliamorloc.so"))

    for path in candidates
        if isfile(path)
            LIB_PATH[] = path
            return
        end
    end
    error("Cannot find libjuliamorloc.so. Run `morloc init` first.")
end

lib() = LIB_PATH[]

# -- Error handling --

function check_error(context::String)
    msg = unsafe_string(ccall((:jlmorloc_last_error, lib()), Cstring, ()))
    if !isempty(msg)
        error("$context: $msg")
    end
end

# -- Daemon lifecycle --

function start_daemon(socket_path::String, tmpdir::String,
                      shm_basename::String, shm_size::Integer)
    ptr = ccall((:jlmorloc_start_daemon, lib()), Ptr{Nothing},
                (Cstring, Cstring, Cstring, Csize_t),
                socket_path, tmpdir, shm_basename, UInt(shm_size))
    ptr == C_NULL && check_error("start_daemon")
    return ptr
end

function close_daemon(daemon::Ptr{Nothing})
    ccall((:jlmorloc_close_daemon, lib()), Nothing, (Ptr{Nothing},), daemon)
end

function wait_for_client(daemon::Ptr{Nothing})
    fd = ccall((:jlmorloc_wait_for_client, lib()), Cint, (Ptr{Nothing},), daemon)
    fd < 0 && check_error("wait_for_client")
    return fd
end

# -- Packet I/O --

function stream_from_client(client_fd)
    out_size = Ref{Csize_t}(0)
    ptr = ccall((:jlmorloc_stream_from_client, lib()), Ptr{UInt8},
                (Cint, Ref{Csize_t}), Int32(client_fd), out_size)
    ptr == C_NULL && check_error("stream_from_client")
    return ptr  # opaque packet pointer
end

function send_packet_to_foreign_server(client_fd, packet::Ptr{UInt8})
    rc = ccall((:jlmorloc_send_packet, lib()), Cint,
               (Cint, Ptr{UInt8}), Int32(client_fd), packet)
    rc != 0 && check_error("send_packet")
end

function close_socket(fd)
    ccall((:jlmorloc_close_socket, lib()), Nothing, (Cint,), Int32(fd))
end

# -- Packet classification --

function is_ping(packet::Ptr{UInt8})
    ccall((:jlmorloc_is_ping, lib()), Cint, (Ptr{UInt8},), packet) != 0
end

function is_local_call(packet::Ptr{UInt8})
    ccall((:jlmorloc_is_local_call, lib()), Cint, (Ptr{UInt8},), packet) != 0
end

function is_remote_call(packet::Ptr{UInt8})
    ccall((:jlmorloc_is_remote_call, lib()), Cint, (Ptr{UInt8},), packet) != 0
end

function pong(packet::Ptr{UInt8})
    result = ccall((:jlmorloc_pong, lib()), Ptr{UInt8}, (Ptr{UInt8},), packet)
    result == C_NULL && check_error("pong")
    return result
end

# -- Call packet parsing --

"""
    read_morloc_call_packet(packet) -> (mid, args)

Parse a call packet into a manifold index and a vector of argument packets.
"""
function read_morloc_call_packet(packet::Ptr{UInt8})
    out_mid = Ref{UInt32}(0)
    out_nargs = Ref{Csize_t}(0)
    call_ptr = ccall((:jlmorloc_read_call, lib()), Ptr{Nothing},
                     (Ptr{UInt8}, Ref{UInt32}, Ref{Csize_t}),
                     packet, out_mid, out_nargs)
    call_ptr == C_NULL && check_error("read_call")

    mid = Int(out_mid[])
    nargs = Int(out_nargs[])
    args = Vector{Ptr{UInt8}}(undef, nargs)
    for i in 1:nargs
        args[i] = ccall((:jlmorloc_call_arg, lib()), Ptr{UInt8},
                        (Ptr{Nothing}, Csize_t), call_ptr, UInt(i - 1))
    end

    ccall((:jlmorloc_free_call, lib()), Nothing, (Ptr{Nothing},), call_ptr)
    return (mid, args)
end

# -- Msgpack bridge: serialize/deserialize --

"""
Strip the `<TypeName>` prefix from schema strings like `"<Int64>i4"` -> `"i4"`.
"""
function strip_schema_prefix(schema_str::String)
    if !isempty(schema_str) && schema_str[1] == '<'
        i = findfirst('>', schema_str)
        if i !== nothing
            return schema_str[i+1:end]
        end
    end
    return schema_str
end

"""
    put_value(value, schema_str) -> Ptr{UInt8}

Serialize a Julia value to a morloc data packet via msgpack.
"""
function put_value(value, schema_str::String)
    schema = strip_schema_prefix(schema_str)
    mpk = MsgPack.pack(to_msgpack(value, schema))
    pkt = ccall((:jlmorloc_pack, lib()), Ptr{UInt8},
                (Ptr{UInt8}, Csize_t, Cstring),
                mpk, length(mpk), schema)
    pkt == C_NULL && check_error("pack")
    return pkt
end

"""
    get_value(packet, schema_str) -> Julia value

Deserialize a morloc data packet to a Julia value via msgpack.
"""
function get_value(packet::Ptr{UInt8}, schema_str::String)
    schema = strip_schema_prefix(schema_str)
    out_size = Ref{Csize_t}(0)
    mpk_ptr = ccall((:jlmorloc_unpack, lib()), Ptr{UInt8},
                    (Ptr{UInt8}, Cstring, Ref{Csize_t}),
                    packet, schema, out_size)
    mpk_ptr == C_NULL && check_error("unpack")

    mpk_bytes = unsafe_wrap(Array, mpk_ptr, out_size[]; own=true)
    raw = MsgPack.unpack(mpk_bytes)
    return from_msgpack(raw, schema)
end

# -- Error packet --

function make_fail_packet(msg::String)
    ccall((:jlmorloc_make_fail_packet, lib()), Ptr{UInt8}, (Cstring,), msg)
end

# -- Foreign call --

"""
    foreign_call(tmpdir, socket_name, mid, args) -> Ptr{UInt8}

Call another pool (cross-language IPC). args is a vector of packet pointers.
"""
function foreign_call(tmpdir::String, socket_name::String,
                      mid::Integer, args::Vector{Ptr{UInt8}})
    nargs = length(args)
    result = ccall((:jlmorloc_foreign_call, lib()), Ptr{UInt8},
                   (Cstring, Cstring, UInt32, Ptr{Ptr{UInt8}}, Csize_t),
                   tmpdir, socket_name, UInt32(mid), args, UInt(nargs))
    result == C_NULL && check_error("foreign_call")
    return result
end

# -- Type conversion helpers --

# Schema string format:
#   "b" = bool, "i4" = int32, "i8" = int64, "f8" = float64, "s" = string
#   "ai4" = array of int32, "t(i4f8s)" = tuple
#   "m{name:s,age:i4}" = record

"""
Convert a Julia value to a msgpack-friendly representation based on schema.
MsgPack.jl handles most types natively, but we need to ensure correct types
for the schema (e.g., Int32 vs Int64).
"""
function to_msgpack(value, schema::String)
    if startswith(schema, "a")
        elem_schema = schema[2:end]
        return [to_msgpack(v, elem_schema) for v in value]
    elseif startswith(schema, "t(")
        inner = schema[3:end-1]
        schemas = split_tuple_schema(inner)
        return [to_msgpack(value[i], schemas[i]) for i in 1:length(schemas)]
    elseif startswith(schema, "m{")
        inner = schema[3:end-1]
        fields = split_record_schema(inner)
        return Dict(k => to_msgpack(value[k], s) for (k, s) in fields)
    else
        return to_msgpack_scalar(value, schema)
    end
end

function to_msgpack_scalar(value, schema::String)
    if schema == "b"
        return Bool(value)
    elseif schema == "i4"
        return Int32(value)
    elseif schema == "i8"
        return Int64(value)
    elseif schema == "u4"
        return UInt32(value)
    elseif schema == "u8"
        return UInt64(value)
    elseif schema == "f4"
        return Float32(value)
    elseif schema == "f8"
        return Float64(value)
    elseif schema == "s"
        return String(value)
    elseif schema == "u"
        return nothing
    else
        return value
    end
end

"""
Convert a raw msgpack value to a Julia type based on schema.
"""
function from_msgpack(raw, schema::String)
    if startswith(schema, "a")
        elem_schema = schema[2:end]
        return [from_msgpack(v, elem_schema) for v in raw]
    elseif startswith(schema, "t(")
        inner = schema[3:end-1]
        schemas = split_tuple_schema(inner)
        return Tuple(from_msgpack(raw[i], schemas[i]) for i in 1:length(schemas))
    elseif startswith(schema, "m{")
        inner = schema[3:end-1]
        fields = split_record_schema(inner)
        return Dict(k => from_msgpack(raw[k], s) for (k, s) in fields)
    else
        return from_msgpack_scalar(raw, schema)
    end
end

function from_msgpack_scalar(raw, schema::String)
    if schema == "b"
        return Bool(raw)
    elseif schema == "i4"
        return Int32(raw)
    elseif schema == "i8"
        return Int64(raw)
    elseif schema == "u4"
        return UInt32(raw)
    elseif schema == "u8"
        return UInt64(raw)
    elseif schema == "f4"
        return Float32(raw)
    elseif schema == "f8"
        return Float64(raw)
    elseif schema == "s"
        return String(raw)
    elseif schema == "u"
        return nothing
    else
        return raw
    end
end

# -- Schema parsing helpers --

function split_tuple_schema(inner::String)
    schemas = String[]
    i = 1
    while i <= length(inner)
        s, i = parse_one_schema(inner, i)
        push!(schemas, s)
    end
    return schemas
end

function split_record_schema(inner::String)
    fields = Pair{String,String}[]
    i = 1
    while i <= length(inner)
        # parse field name
        colon = findnext(':', inner, i)
        name = inner[i:colon-1]
        i = colon + 1
        # parse field schema
        s, i = parse_one_schema(inner, i)
        push!(fields, name => s)
        if i <= length(inner) && inner[i] == ','
            i += 1
        end
    end
    return fields
end

function parse_one_schema(s::String, i::Int)
    if s[i] == 'a'
        inner, next_i = parse_one_schema(s, i + 1)
        return "a" * inner, next_i
    elseif s[i] == 't'
        # find matching ')'
        depth = 0
        j = i + 1
        while j <= length(s)
            if s[j] == '('; depth += 1; end
            if s[j] == ')'; depth -= 1; if depth == 0; break; end; end
            j += 1
        end
        return s[i:j], j + 1
    elseif s[i] == 'm'
        depth = 0
        j = i + 1
        while j <= length(s)
            if s[j] == '{'; depth += 1; end
            if s[j] == '}'; depth -= 1; if depth == 0; break; end; end
            j += 1
        end
        return s[i:j], j + 1
    elseif s[i] in ('i', 'u', 'f')
        # numeric: i4, i8, u4, u8, f4, f8
        return s[i:i+1], i + 2
    elseif s[i] == 's'
        return "s", i + 1
    elseif s[i] == 'b'
        return "b", i + 1
    else
        error("Unknown schema character: $(s[i]) at position $i in '$s'")
    end
end

end # module
