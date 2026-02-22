#ifndef __MORLOC_HTTP_H__
#define __MORLOC_HTTP_H__

#include "macros.h"
#include "daemon.h"

#include <stdbool.h>
#include <stddef.h>

typedef enum {
    HTTP_GET,
    HTTP_POST,
    HTTP_OPTIONS
} http_method_t;

typedef struct http_request_s {
    http_method_t method;
    char path[256];
    char* body;
    size_t body_len;
} http_request_t;

// Parse an HTTP/1.1 request from a connected socket fd
http_request_t* http_parse_request(int fd, ERRMSG);

// Write an HTTP response to a connected socket fd
bool http_write_response(int fd, int status, const char* content_type,
                         const char* body, size_t body_len);

// Route an HTTP request to a daemon_request_t
daemon_request_t* http_to_daemon_request(http_request_t* req, ERRMSG);

void http_free_request(http_request_t* req);

#endif // __MORLOC_HTTP_H__
