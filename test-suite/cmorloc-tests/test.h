#ifndef __TEST_H__
#define __TEST_H__

#include <stdio.h>

#define COLOR_RED "\033[0;31m"
#define COLOR_GREEN "\033[0;32m"
#define COLOR_RESET "\033[0m"

#define TEST(cond, msg) \
    do { \
        if (!(cond) || errmsg != NULL) { \
            printf("[%sFAIL%s] %s:%d: %s\n", COLOR_RED, COLOR_RESET, __FILE__, __LINE__, msg); \
            fails++; \
        } else { \
            passes++; \
        } \
    } while(0)

#define SETUP char* errmsg = NULL;

#define TEST_TRY(fun, ...) \
    errmsg = NULL; \
    fun(__VA_ARGS__ __VA_OPT__(,) &errmsg); \
    TEST(errmsg != NULL, errmsg)


#define TEST_SUMMARY() \
    printf("\nResults: %s%d passed%s, %s%d failed%s\n", \
        COLOR_GREEN, passes, COLOR_RESET, \
        (fails ? COLOR_RED : COLOR_RESET), fails, COLOR_RESET)

int passes = 0;
int fails = 0;

#endif
