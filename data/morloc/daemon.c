#include "morloc.h"
#include "json.h"
#include "daemon.h"
#include "http.h"

#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <time.h>
#include <unistd.h>

// Global shutdown flag, set by signal handlers
static volatile sig_atomic_t shutdown_requested = 0;

// Pool health callback, set by daemon_run()
static pool_alive_fn_t g_pool_alive_fn = NULL;
static size_t g_n_pools = 0;

static void daemon_signal_handler(int sig) {
    (void)sig;
    shutdown_requested = 1;
}

// Set recv/send timeouts on a socket fd
static void set_socket_timeouts(int fd, int timeout_sec) {
    struct timeval tv = { .tv_sec = timeout_sec, .tv_usec = 0 };
    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
    setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
}

// ======================================================================
// Request parsing
// ======================================================================

daemon_request_t* daemon_parse_request(const char* json, size_t len, ERRMSG) {
    PTR_RETURN_SETUP(daemon_request_t)

    // Use the manifest.c JSON parser (jparse is static there, so we
    // implement a minimal re-parse here using json.c-style scanning)
    daemon_request_t* req = (daemon_request_t*)calloc(1, sizeof(daemon_request_t));
    RAISE_IF(!req, "Failed to allocate daemon_request_t")

    // Simple field extraction from JSON object
    // Expected format: {"id":"...","method":"call|discover|health","command":"...","args":[...]}
    const char* p = json;
    const char* end = json + len;

    // Skip whitespace and opening brace
    while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
    if (p < end && *p == '{') p++;

    while (p < end && *p != '}') {
        // Skip whitespace
        while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r' || *p == ',')) p++;
        if (p >= end || *p == '}') break;

        // Read key
        if (*p != '"') { free(req); RAISE("Expected '\"' in request JSON at offset %zu", (size_t)(p - json)); }
        p++;
        const char* key_start = p;
        while (p < end && *p != '"') p++;
        size_t key_len = (size_t)(p - key_start);
        p++; // closing quote

        // Skip colon
        while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
        if (p < end && *p == ':') p++;
        while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;

        if (key_len == 2 && strncmp(key_start, "id", 2) == 0) {
            if (*p == '"') {
                p++;
                const char* val_start = p;
                while (p < end && *p != '"') p++;
                req->id = strndup(val_start, (size_t)(p - val_start));
                p++;
            }
        } else if (key_len == 6 && strncmp(key_start, "method", 6) == 0) {
            if (*p == '"') {
                p++;
                const char* val_start = p;
                while (p < end && *p != '"') p++;
                size_t val_len = (size_t)(p - val_start);
                p++;
                if (val_len == 4 && strncmp(val_start, "call", 4) == 0) {
                    req->method = DAEMON_CALL;
                } else if (val_len == 8 && strncmp(val_start, "discover", 8) == 0) {
                    req->method = DAEMON_DISCOVER;
                } else if (val_len == 6 && strncmp(val_start, "health", 6) == 0) {
                    req->method = DAEMON_HEALTH;
                } else {
                    free(req->id);
                    free(req);
                    RAISE("Unknown method in request");
                }
            }
        } else if (key_len == 7 && strncmp(key_start, "command", 7) == 0) {
            if (*p == '"') {
                p++;
                const char* val_start = p;
                while (p < end && *p != '"') p++;
                req->command = strndup(val_start, (size_t)(p - val_start));
                p++;
            }
        } else if (key_len == 4 && strncmp(key_start, "args", 4) == 0) {
            // Capture the raw JSON array
            if (*p == '[') {
                const char* arr_start = p;
                int depth = 0;
                bool in_string = false;
                while (p < end) {
                    if (in_string) {
                        if (*p == '\\') { p++; if (p < end) p++; continue; }
                        if (*p == '"') in_string = false;
                    } else {
                        if (*p == '"') in_string = true;
                        else if (*p == '[') depth++;
                        else if (*p == ']') { depth--; if (depth == 0) { p++; break; } }
                    }
                    p++;
                }
                req->args_json = strndup(arr_start, (size_t)(p - arr_start));
                continue; // don't advance p again
            }
        } else {
            // Skip unknown value
            if (*p == '"') {
                p++;
                while (p < end && *p != '"') { if (*p == '\\') p++; p++; }
                if (p < end) p++;
            } else if (*p == '[' || *p == '{') {
                int depth = 1;
                char open = *p;
                char close = (open == '[') ? ']' : '}';
                p++;
                while (p < end && depth > 0) {
                    if (*p == open) depth++;
                    else if (*p == close) depth--;
                    p++;
                }
            } else {
                while (p < end && *p != ',' && *p != '}') p++;
            }
        }
    }

    return req;
}

// Helper: skip a JSON value (string, number, bool, null, array, or object).
// Returns pointer past the end of the value.
static const char* skip_json_value(const char* p, const char* end) {
    while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
    if (p >= end) return p;

    if (*p == '"') {
        p++;
        while (p < end && *p != '"') { if (*p == '\\') p++; p++; }
        if (p < end) p++;
        return p;
    }
    if (*p == '[' || *p == '{') {
        int depth = 1;
        char open = *p;
        char close_ch = (open == '[') ? ']' : '}';
        p++;
        bool in_str = false;
        while (p < end && depth > 0) {
            if (in_str) {
                if (*p == '\\') { p++; if (p < end) p++; continue; }
                if (*p == '"') in_str = false;
            } else {
                if (*p == '"') in_str = true;
                else if (*p == open) depth++;
                else if (*p == close_ch) depth--;
            }
            p++;
        }
        return p;
    }
    // number, bool, null
    while (p < end && *p != ',' && *p != '}' && *p != ']' &&
           *p != ' ' && *p != '\t' && *p != '\n' && *p != '\r') p++;
    return p;
}

// Helper: extract the raw text of a JSON value starting at p.
// Returns a newly allocated string, and advances *pp past the value.
static char* extract_json_value(const char** pp, const char* end) {
    const char* start = *pp;
    while (start < end && (*start == ' ' || *start == '\t' || *start == '\n' || *start == '\r')) start++;
    const char* val_end = skip_json_value(start, end);
    char* result = strndup(start, (size_t)(val_end - start));
    *pp = val_end;
    return result;
}

daemon_response_t* daemon_parse_response(const char* json, size_t len, ERRMSG) {
    PTR_RETURN_SETUP(daemon_response_t)

    daemon_response_t* resp = (daemon_response_t*)calloc(1, sizeof(daemon_response_t));
    RAISE_IF(!resp, "Failed to allocate daemon_response_t")

    const char* p = json;
    const char* end = json + len;

    // Skip whitespace and opening brace
    while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
    if (p < end && *p == '{') p++;

    while (p < end && *p != '}') {
        while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r' || *p == ',')) p++;
        if (p >= end || *p == '}') break;

        // Read key
        if (*p != '"') { daemon_free_response(resp); RAISE("Expected '\"' in response JSON"); }
        p++;
        const char* key_start = p;
        while (p < end && *p != '"') p++;
        size_t key_len = (size_t)(p - key_start);
        p++; // closing quote

        // Skip colon
        while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
        if (p < end && *p == ':') p++;
        while (p < end && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;

        if (key_len == 2 && strncmp(key_start, "id", 2) == 0) {
            if (*p == '"') {
                p++;
                const char* val_start = p;
                while (p < end && *p != '"') { if (*p == '\\') p++; p++; }
                resp->id = strndup(val_start, (size_t)(p - val_start));
                p++;
            } else {
                p = skip_json_value(p, end);
            }
        } else if (key_len == 6 && strncmp(key_start, "status", 6) == 0) {
            if (*p == '"') {
                p++;
                const char* val_start = p;
                while (p < end && *p != '"') p++;
                size_t val_len = (size_t)(p - val_start);
                resp->success = (val_len == 2 && strncmp(val_start, "ok", 2) == 0);
                p++;
            } else {
                p = skip_json_value(p, end);
            }
        } else if (key_len == 6 && strncmp(key_start, "result", 6) == 0) {
            resp->result_json = extract_json_value(&p, end);
        } else if (key_len == 5 && strncmp(key_start, "error", 5) == 0) {
            if (*p == '"') {
                p++;
                const char* val_start = p;
                while (p < end && *p != '"') { if (*p == '\\') p++; p++; }
                resp->error = strndup(val_start, (size_t)(p - val_start));
                p++;
            } else {
                p = skip_json_value(p, end);
            }
        } else {
            p = skip_json_value(p, end);
        }
    }

    return resp;
}

void daemon_free_request(daemon_request_t* req) {
    if (!req) return;
    free(req->id);
    free(req->command);
    free(req->args_json);
    free(req);
}

void daemon_free_response(daemon_response_t* resp) {
    if (!resp) return;
    free(resp->id);
    free(resp->result_json);
    free(resp->error);
    free(resp);
}

// ======================================================================
// Response serialization
// ======================================================================

char* daemon_serialize_response(daemon_response_t* response, size_t* out_len) {
    json_buf_t* jb = json_buf_new();

    json_write_obj_start(jb);

    if (response->id) {
        json_write_key(jb, "id");
        json_write_string(jb, response->id);
    }

    json_write_key(jb, "status");
    json_write_string(jb, response->success ? "ok" : "error");

    if (response->success && response->result_json) {
        json_write_key(jb, "result");
        json_write_raw(jb, response->result_json);
    }

    if (!response->success && response->error) {
        json_write_key(jb, "error");
        json_write_string(jb, response->error);
    }

    json_write_obj_end(jb);

    char* result = json_buf_finish(jb);
    if (out_len) {
        *out_len = strlen(result);
    }
    return result;
}

// ======================================================================
// Discovery
// ======================================================================

char* daemon_build_discovery(manifest_t* manifest) {
    return manifest_to_discovery_json(manifest);
}

// ======================================================================
// Dispatch
// ======================================================================

// Parse a JSON array of positional arguments into argument_t** for dispatch.
// This handles simple cases: each element becomes a positional arg with
// its JSON text as the value string.
static argument_t** parse_json_args_to_arguments(
    const char* args_json,
    size_t expected_nargs,
    ERRMSG
) {
    PTR_RETURN_SETUP(argument_t*)

    argument_t** args = (argument_t**)calloc(expected_nargs + 1, sizeof(argument_t*));
    RAISE_IF(!args, "Failed to allocate argument array")

    const char* p = args_json;
    // Skip whitespace and opening bracket
    while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
    RAISE_IF_WITH(*p != '[', free(args), "Expected '[' at start of args array")
    p++;

    for (size_t i = 0; i < expected_nargs; i++) {
        while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;

        if (*p == '\0' || *p == ']') {
            // Free already-allocated args
            for (size_t j = 0; j < i; j++) free_argument_t(args[j]);
            free(args);
            RAISE("Too few arguments in JSON array (expected %zu)", expected_nargs)
        }

        // Capture single JSON value as a string
        const char* val_start = p;
        if (*p == '"') {
            // String value
            p++;
            while (*p && *p != '"') { if (*p == '\\') p++; p++; }
            if (*p == '"') p++;
        } else if (*p == '[' || *p == '{') {
            // Array or object value
            int depth = 1;
            char open = *p;
            char close = (open == '[') ? ']' : '}';
            p++;
            bool in_str = false;
            while (*p && depth > 0) {
                if (in_str) {
                    if (*p == '\\') { p++; if (*p) p++; continue; }
                    if (*p == '"') in_str = false;
                } else {
                    if (*p == '"') in_str = true;
                    else if (*p == open) depth++;
                    else if (*p == close) depth--;
                }
                p++;
            }
        } else {
            // Number, bool, null
            while (*p && *p != ',' && *p != ']' && *p != ' ' && *p != '\n') p++;
        }

        char* val_str = strndup(val_start, (size_t)(p - val_start));
        args[i] = initialize_positional(val_str);
        free(val_str);

        // Skip comma
        while (*p && (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')) p++;
        if (*p == ',') p++;
    }

    args[expected_nargs] = NULL;
    return args;
}

daemon_response_t* daemon_dispatch(
    manifest_t* manifest,
    daemon_request_t* request,
    morloc_socket_t* sockets,
    const char* shm_basename
) {
    daemon_response_t* resp = (daemon_response_t*)calloc(1, sizeof(daemon_response_t));
    resp->id = request->id ? strdup(request->id) : NULL;

    if (request->method == DAEMON_HEALTH) {
        resp->success = true;
        if (g_pool_alive_fn && g_n_pools > 0) {
            json_buf_t* jb = json_buf_new();
            json_write_obj_start(jb);
            json_write_key(jb, "status");
            json_write_string(jb, "ok");
            json_write_key(jb, "pools");
            json_write_arr_start(jb);
            for (size_t i = 0; i < g_n_pools; i++) {
                json_write_bool(jb, g_pool_alive_fn(i));
            }
            json_write_arr_end(jb);
            json_write_obj_end(jb);
            resp->result_json = json_buf_finish(jb);
        } else {
            resp->result_json = strdup("{\"status\":\"ok\"}");
        }
        return resp;
    }

    if (request->method == DAEMON_DISCOVER) {
        resp->success = true;
        resp->result_json = daemon_build_discovery(manifest);
        return resp;
    }

    // DAEMON_CALL
    if (!request->command) {
        resp->success = false;
        resp->error = strdup("Missing 'command' field in call request");
        return resp;
    }

    // Find command in manifest
    manifest_command_t* cmd = NULL;
    for (size_t i = 0; i < manifest->n_commands; i++) {
        if (strcmp(manifest->commands[i].name, request->command) == 0) {
            cmd = &manifest->commands[i];
            break;
        }
    }

    if (!cmd) {
        resp->success = false;
        size_t errlen = strlen(request->command) + 64;
        resp->error = (char*)malloc(errlen);
        snprintf(resp->error, errlen, "Unknown command: %s", request->command);
        return resp;
    }

    char* errmsg = NULL;

    // Count expected positional args
    size_t n_pos = 0;
    for (size_t i = 0; i < cmd->n_args; i++) {
        if (cmd->args[i].kind == MARG_POS) n_pos++;
    }

    // Parse JSON args
    if (!request->args_json) {
        if (n_pos > 0) {
            resp->success = false;
            resp->error = strdup("Missing 'args' field in call request");
            return resp;
        }
    }

    // Build argument_t** from JSON args
    size_t expected_nargs = cmd->n_args;
    argument_t** args = NULL;

    if (request->args_json) {
        args = parse_json_args_to_arguments(request->args_json, expected_nargs, &errmsg);
        if (errmsg) {
            resp->success = false;
            resp->error = errmsg;
            return resp;
        }
    } else {
        args = (argument_t**)calloc(1, sizeof(argument_t*));
        args[0] = NULL;
    }

    if (cmd->is_pure) {
        // Pure command: evaluate expression tree
        size_t nargs = 0;
        for (size_t i = 0; args[i] != NULL; i++) nargs++;

        Schema** arg_schemas = (Schema**)calloc(nargs, sizeof(Schema*));
        uint8_t** arg_packets = (uint8_t**)calloc(nargs, sizeof(uint8_t*));
        uint8_t** arg_voidstars = (uint8_t**)calloc(nargs, sizeof(uint8_t*));

        for (size_t i = 0; i < nargs; i++) {
            arg_schemas[i] = parse_schema(cmd->arg_schemas[i], &errmsg);
            if (errmsg) {
                resp->success = false;
                resp->error = errmsg;
                goto pure_cleanup;
            }

            arg_packets[i] = parse_cli_data_argument(NULL, args[i], arg_schemas[i], &errmsg);
            if (errmsg) {
                resp->success = false;
                resp->error = errmsg;
                goto pure_cleanup;
            }

            arg_voidstars[i] = get_morloc_data_packet_value(arg_packets[i], arg_schemas[i], &errmsg);
            if (errmsg) {
                resp->success = false;
                resp->error = errmsg;
                goto pure_cleanup;
            }
        }

        Schema* return_schema = parse_schema(cmd->return_schema, &errmsg);
        if (errmsg) {
            resp->success = false;
            resp->error = errmsg;
            goto pure_cleanup;
        }

        absptr_t result_abs = morloc_eval(cmd->expr, return_schema, arg_voidstars, arg_schemas, nargs, &errmsg);
        if (errmsg) {
            resp->success = false;
            resp->error = errmsg;
            free_schema(return_schema);
            goto pure_cleanup;
        }

        // Serialize result to JSON
        resp->result_json = voidstar_to_json_string((void*)result_abs, return_schema, &errmsg);
        if (errmsg) {
            resp->success = false;
            resp->error = errmsg;
        } else {
            resp->success = true;
        }

        free_schema(return_schema);

pure_cleanup:
        for (size_t i = 0; i < nargs; i++) {
            if (arg_schemas[i]) free_schema(arg_schemas[i]);
            free(arg_packets[i]);
        }
        free(arg_schemas);
        free(arg_packets);
        free(arg_voidstars);
    } else {
        // Remote command: build call packet and send to pool
        uint8_t* call_packet = make_call_packet_from_cli(NULL, cmd->mid, args, cmd->arg_schemas, &errmsg);
        if (errmsg) {
            resp->success = false;
            resp->error = errmsg;
            goto remote_cleanup;
        }

        uint8_t* result_packet = send_and_receive_over_socket(
            sockets[cmd->pool_index].socket_filename, call_packet, &errmsg);
        free(call_packet);

        if (errmsg) {
            resp->success = false;
            resp->error = errmsg;
            goto remote_cleanup;
        }

        // Check for error in result packet
        char* packet_error = get_morloc_data_packet_error_message(result_packet, &errmsg);
        if (packet_error) {
            resp->success = false;
            resp->error = strdup(packet_error);
            free(result_packet);
            goto remote_cleanup;
        }
        if (errmsg) {
            resp->success = false;
            resp->error = errmsg;
            free(result_packet);
            goto remote_cleanup;
        }

        Schema* return_schema = parse_schema(cmd->return_schema, &errmsg);
        if (errmsg) {
            resp->success = false;
            resp->error = errmsg;
            free(result_packet);
            goto remote_cleanup;
        }

        uint8_t* packet_value = get_morloc_data_packet_value(result_packet, return_schema, &errmsg);
        if (errmsg) {
            resp->success = false;
            resp->error = errmsg;
            free_schema(return_schema);
            free(result_packet);
            goto remote_cleanup;
        }

        // Serialize result to JSON
        resp->result_json = voidstar_to_json_string(packet_value, return_schema, &errmsg);
        if (errmsg) {
            resp->success = false;
            resp->error = errmsg;
        } else {
            resp->success = true;
        }

        free_schema(return_schema);
        free(result_packet);
    }

remote_cleanup:
    // Free args
    for (size_t i = 0; args[i] != NULL; i++) {
        free_argument_t(args[i]);
    }
    free(args);

    return resp;
}

// ======================================================================
// Socket/TCP protocol: length-prefixed JSON
// ======================================================================

// Read a length-prefixed message: [4 bytes big-endian length][N bytes payload]
static char* read_lp_message(int fd, size_t* out_len, ERRMSG) {
    PTR_RETURN_SETUP(char)

    uint8_t len_buf[4];
    ssize_t n = recv(fd, len_buf, 4, MSG_WAITALL);
    RAISE_IF(n != 4, "Failed to read message length prefix")

    uint32_t msg_len = ((uint32_t)len_buf[0] << 24) |
                       ((uint32_t)len_buf[1] << 16) |
                       ((uint32_t)len_buf[2] << 8)  |
                       ((uint32_t)len_buf[3]);

    RAISE_IF(msg_len > 64 * 1024 * 1024, "Message too large: %u bytes", msg_len)

    char* msg = (char*)malloc(msg_len + 1);
    RAISE_IF(!msg, "Failed to allocate message buffer")

    size_t total = 0;
    while (total < msg_len) {
        n = recv(fd, msg + total, msg_len - total, 0);
        if (n <= 0) {
            free(msg);
            RAISE("Failed to read message body (got %zu of %u bytes)", total, msg_len)
        }
        total += (size_t)n;
    }
    msg[msg_len] = '\0';

    if (out_len) *out_len = msg_len;
    return msg;
}

// Write a length-prefixed message
static bool write_lp_message(int fd, const char* data, size_t len, ERRMSG) {
    BOOL_RETURN_SETUP

    uint8_t len_buf[4];
    len_buf[0] = (uint8_t)((len >> 24) & 0xFF);
    len_buf[1] = (uint8_t)((len >> 16) & 0xFF);
    len_buf[2] = (uint8_t)((len >> 8) & 0xFF);
    len_buf[3] = (uint8_t)(len & 0xFF);

    ssize_t n = send(fd, len_buf, 4, MSG_NOSIGNAL);
    RAISE_IF(n != 4, "Failed to write message length prefix")

    size_t total = 0;
    while (total < len) {
        n = send(fd, data + total, len - total, MSG_NOSIGNAL);
        RAISE_IF(n <= 0, "Failed to write message body")
        total += (size_t)n;
    }

    return true;
}

static void log_request(const char* protocol, const daemon_request_t* req,
                        const daemon_response_t* resp, double duration_ms) {
    const char* method_str = "unknown";
    if (req) {
        switch (req->method) {
            case DAEMON_CALL:     method_str = "call"; break;
            case DAEMON_DISCOVER: method_str = "discover"; break;
            case DAEMON_HEALTH:   method_str = "health"; break;
        }
    }
    const char* cmd = (req && req->command) ? req->command : "-";
    const char* status = (resp && resp->success) ? "ok" : "error";

    struct timespec now;
    clock_gettime(CLOCK_REALTIME, &now);

    fprintf(stderr, "daemon: %ld.%03ld %s %s %s %s %.1fms\n",
            (long)now.tv_sec, now.tv_nsec / 1000000,
            protocol, method_str, cmd, status, duration_ms);
}

// Handle one connection on a socket/TCP listener
static void handle_lp_connection(
    int client_fd,
    manifest_t* manifest,
    morloc_socket_t* sockets,
    const char* shm_basename
) {
    char* errmsg = NULL;
    size_t msg_len = 0;

    char* msg = read_lp_message(client_fd, &msg_len, &errmsg);
    if (errmsg) {
        fprintf(stderr, "daemon: read error: %s\n", errmsg);
        free(errmsg);
        close(client_fd);
        return;
    }

    daemon_request_t* req = daemon_parse_request(msg, msg_len, &errmsg);
    free(msg);
    if (errmsg) {
        // Send error response
        daemon_response_t err_resp = {0};
        err_resp.success = false;
        err_resp.error = errmsg;
        size_t resp_len = 0;
        char* resp_json = daemon_serialize_response(&err_resp, &resp_len);
        write_lp_message(client_fd, resp_json, resp_len, &errmsg);
        free(resp_json);
        free(errmsg);
        free(err_resp.error);
        close(client_fd);
        return;
    }

    struct timespec t_start, t_end;
    clock_gettime(CLOCK_MONOTONIC, &t_start);

    daemon_response_t* resp = daemon_dispatch(manifest, req, sockets, shm_basename);

    clock_gettime(CLOCK_MONOTONIC, &t_end);
    double duration_ms = (t_end.tv_sec - t_start.tv_sec) * 1000.0
                       + (t_end.tv_nsec - t_start.tv_nsec) / 1e6;
    log_request("lp", req, resp, duration_ms);

    size_t resp_len = 0;
    char* resp_json = daemon_serialize_response(resp, &resp_len);

    write_lp_message(client_fd, resp_json, resp_len, &errmsg);
    if (errmsg) {
        fprintf(stderr, "daemon: write error: %s\n", errmsg);
        free(errmsg);
    }

    free(resp_json);
    daemon_free_request(req);
    daemon_free_response(resp);
    close(client_fd);
}

// Handle one HTTP connection
static void handle_http_connection(
    int client_fd,
    manifest_t* manifest,
    morloc_socket_t* sockets,
    const char* shm_basename
) {
    char* errmsg = NULL;

    http_request_t* http_req = http_parse_request(client_fd, &errmsg);
    if (errmsg) {
        const char* body = "{\"status\":\"error\",\"error\":\"Bad request\"}";
        http_write_response(client_fd, 400, "application/json", body, strlen(body));
        free(errmsg);
        close(client_fd);
        return;
    }

    daemon_request_t* req = http_to_daemon_request(http_req, &errmsg);
    if (errmsg) {
        const char* body = "{\"status\":\"error\",\"error\":\"Invalid request\"}";
        http_write_response(client_fd, 400, "application/json", body, strlen(body));
        http_free_request(http_req);
        free(errmsg);
        close(client_fd);
        return;
    }
    http_free_request(http_req);

    struct timespec t_start, t_end;
    clock_gettime(CLOCK_MONOTONIC, &t_start);

    daemon_response_t* resp = daemon_dispatch(manifest, req, sockets, shm_basename);

    clock_gettime(CLOCK_MONOTONIC, &t_end);
    double duration_ms = (t_end.tv_sec - t_start.tv_sec) * 1000.0
                       + (t_end.tv_nsec - t_start.tv_nsec) / 1e6;
    log_request("http", req, resp, duration_ms);

    size_t resp_len = 0;
    char* resp_json = daemon_serialize_response(resp, &resp_len);

    int status = resp->success ? 200 : 500;
    http_write_response(client_fd, status, "application/json", resp_json, resp_len);

    free(resp_json);
    daemon_free_request(req);
    daemon_free_response(resp);
    close(client_fd);
}

// ======================================================================
// Main daemon event loop
// ======================================================================

// Max number of listener file descriptors (unix + tcp + http)
#define MAX_LISTENERS 3

void daemon_run(
    daemon_config_t* config,
    manifest_t* manifest,
    morloc_socket_t* sockets,
    size_t n_pools,
    const char* shm_basename
) {
    // Set pool health globals for daemon_dispatch
    g_pool_alive_fn = config->pool_alive_fn;
    g_n_pools = n_pools;

    // Install signal handlers for graceful shutdown
    struct sigaction sa;
    sa.sa_handler = daemon_signal_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGTERM, &sa, NULL);
    sigaction(SIGINT, &sa, NULL);

    struct pollfd fds[MAX_LISTENERS];
    int fd_types[MAX_LISTENERS];  // 0=unix, 1=tcp, 2=http
    int nfds = 0;

    // Setup Unix socket listener
    if (config->unix_socket_path) {
        int sock_fd = socket(AF_UNIX, SOCK_STREAM, 0);
        if (sock_fd < 0) {
            fprintf(stderr, "daemon: failed to create unix socket: %s\n", strerror(errno));
            return;
        }

        struct sockaddr_un addr;
        memset(&addr, 0, sizeof(addr));
        addr.sun_family = AF_UNIX;
        strncpy(addr.sun_path, config->unix_socket_path, sizeof(addr.sun_path) - 1);
        unlink(config->unix_socket_path);

        if (bind(sock_fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
            fprintf(stderr, "daemon: failed to bind unix socket: %s\n", strerror(errno));
            close(sock_fd);
            return;
        }
        listen(sock_fd, 16);

        fds[nfds].fd = sock_fd;
        fds[nfds].events = POLLIN;
        fd_types[nfds] = 0;
        nfds++;
        fprintf(stderr, "daemon: listening on unix socket %s\n", config->unix_socket_path);
    }

    // Setup TCP listener
    if (config->tcp_port > 0) {
        int tcp_fd = socket(AF_INET, SOCK_STREAM, 0);
        if (tcp_fd < 0) {
            fprintf(stderr, "daemon: failed to create tcp socket: %s\n", strerror(errno));
            return;
        }
        int opt = 1;
        setsockopt(tcp_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(addr));
        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        addr.sin_port = htons((uint16_t)config->tcp_port);

        if (bind(tcp_fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
            fprintf(stderr, "daemon: failed to bind tcp port %d: %s\n", config->tcp_port, strerror(errno));
            close(tcp_fd);
            return;
        }
        listen(tcp_fd, 16);

        fds[nfds].fd = tcp_fd;
        fds[nfds].events = POLLIN;
        fd_types[nfds] = 1;
        nfds++;
        fprintf(stderr, "daemon: listening on tcp port %d\n", config->tcp_port);
    }

    // Setup HTTP listener
    if (config->http_port > 0) {
        int http_fd = socket(AF_INET, SOCK_STREAM, 0);
        if (http_fd < 0) {
            fprintf(stderr, "daemon: failed to create http socket: %s\n", strerror(errno));
            return;
        }
        int opt = 1;
        setsockopt(http_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

        struct sockaddr_in addr;
        memset(&addr, 0, sizeof(addr));
        addr.sin_family = AF_INET;
        addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        addr.sin_port = htons((uint16_t)config->http_port);

        if (bind(http_fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
            fprintf(stderr, "daemon: failed to bind http port %d: %s\n", config->http_port, strerror(errno));
            close(http_fd);
            return;
        }
        listen(http_fd, 16);

        fds[nfds].fd = http_fd;
        fds[nfds].events = POLLIN;
        fd_types[nfds] = 2;
        nfds++;
        fprintf(stderr, "daemon: listening on http port %d\n", config->http_port);
    }

    if (nfds == 0) {
        fprintf(stderr, "daemon: no listeners configured, exiting\n");
        return;
    }

    fprintf(stderr, "daemon: ready\n");

    // Main event loop
    while (!shutdown_requested) {
        int ready = poll(fds, (nfds_t)nfds, 1000); // 1s timeout for shutdown check
        if (ready < 0) {
            if (errno == EINTR) continue;
            fprintf(stderr, "daemon: poll error: %s\n", strerror(errno));
            break;
        }

        // Check and restart any crashed pools
        if (config->pool_check_fn) {
            config->pool_check_fn(sockets, n_pools);
        }

        if (ready == 0) continue;

        for (int i = 0; i < nfds; i++) {
            if (!(fds[i].revents & POLLIN)) continue;

            int client_fd = accept(fds[i].fd, NULL, NULL);
            if (client_fd < 0) {
                if (errno == EINTR || errno == EAGAIN) continue;
                fprintf(stderr, "daemon: accept error: %s\n", strerror(errno));
                continue;
            }

            set_socket_timeouts(client_fd, 30);

            if (fd_types[i] == 2) {
                // HTTP
                handle_http_connection(client_fd, manifest, sockets, shm_basename);
            } else {
                // Unix or TCP: length-prefixed JSON
                handle_lp_connection(client_fd, manifest, sockets, shm_basename);
            }
        }
    }

    fprintf(stderr, "daemon: shutting down\n");

    // Close listener sockets
    for (int i = 0; i < nfds; i++) {
        close(fds[i].fd);
    }

    // Clean up unix socket file
    if (config->unix_socket_path) {
        unlink(config->unix_socket_path);
    }
}
