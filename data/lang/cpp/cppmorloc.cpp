#include <stdexcept>
#include <string>
#include <cstdlib>

#include "morloc.h"

absptr_t cpp_rel2abs(relptr_t ptr){
    char* errmsg = NULL;
    absptr_t absptr = rel2abs(ptr, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return absptr;
}

relptr_t abs2rel_cpp(absptr_t ptr){
    char* errmsg = NULL;
    relptr_t relptr = abs2rel(ptr, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return relptr;
}

bool shfree_cpp(absptr_t ptr){
    char* errmsg = NULL;
    bool success = shfree(ptr, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return success;
}

Schema* parse_schema_cpp(const char* schema_ptr){
    char* errmsg = NULL;
    Schema* schema = parse_schema(schema_ptr, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return schema;
}

void* shmalloc_cpp(size_t size){
    char* errmsg = NULL;
    void* new_ptr = shmalloc(size, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return new_ptr;
}

shm_t* shinit_cpp(const char* shm_basename, size_t volume_index, size_t shm_size) {
    char* errmsg = NULL;
    shm_t* new_ptr = shinit(shm_basename, volume_index, shm_size, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return new_ptr;
}

int pack_with_schema_cpp(const void* mlc, const Schema* schema, char** mpk, size_t* mpk_size){
    char* errmsg = NULL;
    int exitcode = pack_with_schema(mlc, schema, mpk, mpk_size, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return exitcode;
}

int unpack_with_schema_cpp(const char* mgk, size_t mgk_size, const Schema* schema, void** mlcptr){
    char* errmsg = NULL;
    int exitcode = unpack_with_schema(mgk, mgk_size, schema, mlcptr, &errmsg);
    if(errmsg != NULL){
        std::string msg(errmsg); free(errmsg);
        throw std::runtime_error(msg);
    }
    return exitcode;
}
