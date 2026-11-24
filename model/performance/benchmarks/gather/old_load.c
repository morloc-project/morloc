/* LD_PRELOAD control for ENC-11: refuses every IFile open, so @load of a
   stream file takes the general load (one whole-list block) instead of
   decoding each sub-packet in place. */
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

int64_t mlc_open_ifile(const char* schema, const char* path, char** errmsg) {
    (void)schema;
    (void)path;
    *errmsg = strdup("old_load.so: IFile open refused");
    return -1;
}
