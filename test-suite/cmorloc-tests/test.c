#include "test.h"
#include "../../data/morloc.h"

SETUP

void test_json() {
    path_t path[] = { { JSON_PATH_TYPE_KEY, {.key = "b"} } };
    char* element = access_json_by_path("{\"a\":69,\"b\":420}", path, 1, &errmsg);
    TEST( strcmp(element, "420") == 0, "access_json_by_path" );
}

void test_json_string_size() {
    size_t json_size = 0;
    size_t c_size = 0;
    char* errmsg = NULL;
    char* json = "";
    json_string_size
}

int main() {
    test_json();
    test_json_string_size();
    
    TEST_SUMMARY();
    return fails ? 1 : 0;
}
