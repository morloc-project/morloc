#include "morloc.h"
#include "http.h"
#include "daemon.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

// Maximum HTTP request size (headers + body)
#define HTTP_MAX_REQUEST 4 * 1024 * 1024
// Maximum header section size
#define HTTP_MAX_HEADERS 8192

// ======================================================================
// HTTP request parser
// ======================================================================

http_request_t* http_parse_request(int fd, ERRMSG) {
    PTR_RETURN_SETUP(http_request_t)

    // Read headers (up to double CRLF)
    char header_buf[HTTP_MAX_HEADERS];
    size_t header_len = 0;
    char* header_end = NULL;

    while (header_len < HTTP_MAX_HEADERS - 1) {
        ssize_t n = recv(fd, header_buf + header_len, 1, 0);
        if (n <= 0) {
            RAISE("Connection closed while reading HTTP headers")
        }
        header_len++;
        header_buf[header_len] = '\0';

        // Check for end of headers: \r\n\r\n
        if (header_len >= 4) {
            header_end = strstr(header_buf, "\r\n\r\n");
            if (header_end) break;
        }
    }

    RAISE_IF(!header_end, "HTTP headers too large or malformed")

    // Parse request line
    http_request_t* req = (http_request_t*)calloc(1, sizeof(http_request_t));
    RAISE_IF(!req, "Failed to allocate http_request_t")

    // Method
    if (strncmp(header_buf, "GET ", 4) == 0) {
        req->method = HTTP_GET;
    } else if (strncmp(header_buf, "POST ", 5) == 0) {
        req->method = HTTP_POST;
    } else if (strncmp(header_buf, "OPTIONS ", 8) == 0) {
        req->method = HTTP_OPTIONS;
    } else {
        free(req);
        RAISE("Unsupported HTTP method")
    }

    // Path
    const char* path_start = strchr(header_buf, ' ');
    if (path_start) path_start++;
    const char* path_end = path_start ? strchr(path_start, ' ') : NULL;
    if (!path_start || !path_end) {
        free(req);
        RAISE("Malformed HTTP request line")
    }
    size_t path_len = (size_t)(path_end - path_start);
    if (path_len >= sizeof(req->path)) path_len = sizeof(req->path) - 1;
    memcpy(req->path, path_start, path_len);
    req->path[path_len] = '\0';

    // Strip query string
    char* qmark = strchr(req->path, '?');
    if (qmark) *qmark = '\0';

    // Find Content-Length header
    size_t content_length = 0;
    const char* cl_header = strcasestr(header_buf, "Content-Length:");
    if (!cl_header) cl_header = strcasestr(header_buf, "content-length:");
    if (cl_header) {
        cl_header += 15; // skip "Content-Length:"
        while (*cl_header == ' ') cl_header++;
        content_length = (size_t)strtoul(cl_header, NULL, 10);
    }

    // Read body if present
    if (content_length > 0) {
        RAISE_IF_WITH(content_length > HTTP_MAX_REQUEST, free(req),
                      "HTTP body too large: %zu bytes", content_length)

        req->body = (char*)malloc(content_length + 1);
        if (!req->body) {
            free(req);
            RAISE("Failed to allocate HTTP body buffer")
        }

        // Some body bytes may already be in header_buf after \r\n\r\n
        size_t already_read = header_len - (size_t)(header_end + 4 - header_buf);
        if (already_read > content_length) already_read = content_length;
        if (already_read > 0) {
            memcpy(req->body, header_end + 4, already_read);
        }

        size_t total = already_read;
        while (total < content_length) {
            ssize_t n = recv(fd, req->body + total, content_length - total, 0);
            if (n <= 0) {
                free(req->body);
                free(req);
                RAISE("Connection closed while reading HTTP body")
            }
            total += (size_t)n;
        }
        req->body[content_length] = '\0';
        req->body_len = content_length;
    }

    return req;
}

void http_free_request(http_request_t* req) {
    if (!req) return;
    free(req->body);
    free(req);
}

// ======================================================================
// HTTP response writer
// ======================================================================

static const char* http_status_text(int status) {
    switch (status) {
        case 200: return "OK";
        case 400: return "Bad Request";
        case 404: return "Not Found";
        case 405: return "Method Not Allowed";
        case 500: return "Internal Server Error";
        default: return "Unknown";
    }
}

bool http_write_response(int fd, int status, const char* content_type,
                         const char* body, size_t body_len) {
    char header[512];
    int hlen = snprintf(header, sizeof(header),
        "HTTP/1.1 %d %s\r\n"
        "Content-Type: %s\r\n"
        "Content-Length: %zu\r\n"
        "Connection: close\r\n"
        "Access-Control-Allow-Origin: *\r\n"
        "Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n"
        "Access-Control-Allow-Headers: Content-Type\r\n"
        "\r\n",
        status, http_status_text(status),
        content_type ? content_type : "application/json",
        body_len);

    // Send header
    ssize_t n = send(fd, header, (size_t)hlen, MSG_NOSIGNAL);
    if (n < 0) return false;

    // Send body
    if (body && body_len > 0) {
        size_t total = 0;
        while (total < body_len) {
            n = send(fd, body + total, body_len - total, MSG_NOSIGNAL);
            if (n <= 0) return false;
            total += (size_t)n;
        }
    }

    return true;
}

// ======================================================================
// HTTP to daemon request routing
// ======================================================================

daemon_request_t* http_to_daemon_request(http_request_t* req, ERRMSG) {
    PTR_RETURN_SETUP(daemon_request_t)

    daemon_request_t* dreq = (daemon_request_t*)calloc(1, sizeof(daemon_request_t));
    RAISE_IF(!dreq, "Failed to allocate daemon_request_t")

    // GET /health
    if (req->method == HTTP_GET && strcmp(req->path, "/health") == 0) {
        dreq->method = DAEMON_HEALTH;
        return dreq;
    }

    // GET /discover
    if (req->method == HTTP_GET && strcmp(req->path, "/discover") == 0) {
        dreq->method = DAEMON_DISCOVER;
        return dreq;
    }

    // POST /call/<command>
    if (req->method == HTTP_POST && strncmp(req->path, "/call/", 6) == 0) {
        const char* cmd_name = req->path + 6;
        if (cmd_name[0] == '\0') {
            free(dreq);
            RAISE("Missing command name in /call/ path")
        }
        dreq->method = DAEMON_CALL;
        dreq->command = strdup(cmd_name);

        // Parse body - can be {"args":[...]} or just [...]
        if (req->body && req->body_len > 0) {
            const char* p = req->body;
            while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r') p++;

            if (*p == '[') {
                // Raw array
                dreq->args_json = strdup(p);
            } else if (*p == '{') {
                // Object with "args" key - extract args array
                const char* args_key = strstr(p, "\"args\"");
                if (args_key) {
                    args_key += 6;
                    while (*args_key == ' ' || *args_key == ':') args_key++;
                    if (*args_key == '[') {
                        const char* arr_start = args_key;
                        int depth = 0;
                        bool in_string = false;
                        while (*args_key) {
                            if (in_string) {
                                if (*args_key == '\\') { args_key++; if (*args_key) args_key++; continue; }
                                if (*args_key == '"') in_string = false;
                            } else {
                                if (*args_key == '"') in_string = true;
                                else if (*args_key == '[') depth++;
                                else if (*args_key == ']') { depth--; if (depth == 0) { args_key++; break; } }
                            }
                            args_key++;
                        }
                        dreq->args_json = strndup(arr_start, (size_t)(args_key - arr_start));
                    }
                }
            }
        }
        return dreq;
    }

    // OPTIONS (CORS preflight)
    if (req->method == HTTP_OPTIONS) {
        dreq->method = DAEMON_HEALTH;
        return dreq;
    }

    free(dreq);
    RAISE("Unknown HTTP endpoint: %s %s",
          req->method == HTTP_GET ? "GET" : "POST", req->path)
}
