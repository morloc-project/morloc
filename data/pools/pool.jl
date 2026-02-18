# Morloc Julia pool template
# Single-threaded daemon: accepts one connection at a time.

# Add morloc runtime to load path
const MORLOC_HOME = get(ENV, "MORLOC_HOME", joinpath(homedir(), ".local", "share", "morloc"))
push!(LOAD_PATH, joinpath(MORLOC_HOME, "lang", "julia"))

# Global state accessible to manifolds (e.g., tmpdir for foreign calls)
global_state = Dict{String,String}()

# <<<BREAK>>>

using MorlocRuntime

# <<<BREAK>>>

# <<<BREAK>>>

function run_job(client_fd)
    try
        client_data = MorlocRuntime.stream_from_client(client_fd)

        if MorlocRuntime.is_local_call(client_data)
            (mid, args) = MorlocRuntime.read_morloc_call_packet(client_data)
            try
                result = dispatch[mid](args...)
            catch e
                result = MorlocRuntime.make_fail_packet(string(e))
            end

        elseif MorlocRuntime.is_remote_call(client_data)
            (mid, args) = MorlocRuntime.read_morloc_call_packet(client_data)
            try
                result = remote_dispatch[mid](args...)
            catch e
                result = MorlocRuntime.make_fail_packet(string(e))
            end

        elseif MorlocRuntime.is_ping(client_data)
            result = MorlocRuntime.pong(client_data)

        else
            error("Expected a ping or call type packet")
        end

        MorlocRuntime.send_packet_to_foreign_server(client_fd, result)

    catch e
        @error "job failed" exception=(e, catch_backtrace())
    finally
        MorlocRuntime.close_socket(client_fd)
    end
end

function main()
    socket_path = ARGS[1]
    tmpdir = ARGS[2]
    shm_basename = ARGS[3]

    global_state["tmpdir"] = tmpdir

    daemon = MorlocRuntime.start_daemon(socket_path, tmpdir, shm_basename, 0xffff)

    # Simple signal handling
    running = Ref(true)

    @async begin
        try
            while running[]
                sleep(0.01)
            end
        catch
        end
    end

    try
        while running[]
            client_fd = MorlocRuntime.wait_for_client(daemon)
            if client_fd > 0
                run_job(client_fd)
            end
        end
    catch e
        if !(e isa InterruptException)
            @error "Pool error" exception=(e, catch_backtrace())
        end
    finally
        MorlocRuntime.close_daemon(daemon)
    end
end

main()
