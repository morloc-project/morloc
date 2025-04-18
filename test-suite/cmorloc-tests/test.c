#include "test.h"
#include "../../data/morloc.h"

SETUP

void test_json() {
    path_t path[] = { { JSON_PATH_TYPE_KEY, {.key = "b"} } };
    char* element = access_json_by_path("{\"a\":69,\"b\":420}", path, 1, &errmsg);
    TEST( strcmp(element, "420") == 0, "access_json_by_path" );
}

int main() {
    test_json();
    
    TEST_SUMMARY();
    return fails ? 1 : 0;
}
