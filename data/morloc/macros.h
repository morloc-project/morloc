#ifndef __MORLOC_MACROS_H__
#define __MORLOC_MACROS_H__

#define EXIT_CODE int
#define EXIT_PASS 0
#define EXIT_FAIL 1

#define MAX_FILENAME_SIZE 128
#define MAX_ERRMSG_SIZE 1024

// With these parameters, the max wait time is around 6s
#define WAIT_RETY_INITIAL_TIME 0.001
#define WAIT_RETRY_MULTIPLIER 1.25
#define WAIT_RETRY_ATTEMPTS 24

#define BUFFER_SIZE 4096

#define FREE(ptr) \
    if(ptr != NULL){ \
        free(ptr); \
        ptr = NULL; \
    }

#define ERRMSG char** errmsg_
#define CHILD_ERRMSG child_errmsg_

#define ERROR_HANDLING_SETUP \
    char* CHILD_ERRMSG = NULL; \
    *errmsg_ = NULL; \
    char errmsg_buffer[MAX_ERRMSG_SIZE] = { 0 };

#define PTR_RETURN_SETUP(type) \
    ERROR_HANDLING_SETUP \
    type* fail_value_ = NULL;

#define INT_RETURN_SETUP \
    ERROR_HANDLING_SETUP \
    int fail_value_ = EXIT_FAIL;

#define VAL_RETURN_SETUP(type, value) \
    ERROR_HANDLING_SETUP \
    type fail_value_ = value;

#define BOOL_RETURN_SETUP \
    ERROR_HANDLING_SETUP \
    int fail_value_ = false;

#define IGNORE_ERROR \
    FREE(CHILD_ERRMSG);

#define RAISE(msg, ...) \
    snprintf(errmsg_buffer, MAX_ERRMSG_SIZE, "Error (%s:%d in %s): " msg, __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    *errmsg_ = strdup(errmsg_buffer); \
    if(child_errmsg_ != NULL){ \
        free(child_errmsg_); \
    } \
    return fail_value_;

#define RAISE_WITH(end, msg, ...) \
    snprintf(errmsg_buffer, MAX_ERRMSG_SIZE, "Error (%s:%d in %s): " msg, __FILE__, __LINE__, __func__, ##__VA_ARGS__); \
    *errmsg_ = strdup(errmsg_buffer); \
    end; \
    if(child_errmsg_ != NULL){ \
        free(child_errmsg_); \
    } \
    return fail_value_;

#define RAISE_IF(cond, msg, ...) \
    if((cond)){ \
        RAISE(msg, ##__VA_ARGS__) \
    }

#define RAISE_IF_WITH(cond, end, msg, ...) \
    if((cond)){ \
        RAISE_WITH(end, msg, ##__VA_ARGS__) \
    }

#define TRY_GOTO(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &CHILD_ERRMSG); \
    if(CHILD_ERRMSG != NULL){ \
        snprintf(errmsg_buffer, MAX_ERRMSG_SIZE, "Error (%s:%d in %s):\n%s", __FILE__, __LINE__, __func__, CHILD_ERRMSG); \
        *errmsg_ = strdup(errmsg_buffer); \
        FREE(CHILD_ERRMSG) \
        goto end; \
    }

#define TRY(fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &CHILD_ERRMSG); \
    RAISE_IF(CHILD_ERRMSG != NULL, "\n%s", CHILD_ERRMSG)

#define TRY_WITH(end, fun, ...) \
    fun(__VA_ARGS__ __VA_OPT__(,) &CHILD_ERRMSG); \
    RAISE_IF_WITH(CHILD_ERRMSG != NULL, end, "\n%s", CHILD_ERRMSG)

#define WAIT(code, cond, timeout_code) do { \
    double retry_time = WAIT_RETY_INITIAL_TIME; \
    int attempts = 0; \
    bool timed_out = false; \
    while(!timed_out){ \
        code; \
        if((cond)){ \
            break; \
        } \
        struct timespec sleep_time = { \
            .tv_sec = (time_t)retry_time, \
            .tv_nsec = (long)((retry_time - (time_t)retry_time) * 1e9) \
        }; \
        nanosleep(&sleep_time, NULL); \
        attempts++; \
        retry_time *= WAIT_RETRY_MULTIPLIER; \
        if(attempts > WAIT_RETRY_ATTEMPTS) { \
            timeout_code; \
        } \
    } \
    } while(0);

#endif
