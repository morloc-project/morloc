#pragma once
#include <string>
#include <thread>
#include <cstdlib>
#include <cstdint>
#include <sys/stat.h>

// The temp-file registry is reached through libmorloc's C ABI. User headers
// are spliced in before cppmorloc.hpp, so the entry points are declared here
// rather than included.
extern "C" char* mlc_tmpfile(char** errmsg);
extern "C" int32_t mlc_unlink_tmp(const char* path, char** errmsg);

namespace tmpthr {

inline std::string make(std::string& err) {
    char* e = NULL;
    char* p = mlc_tmpfile(&e);
    if (e != NULL) { err = e; return std::string(); }
    std::string path(p);
    free(p);
    return path;
}

inline std::string drop(const std::string& path) {
    char* e = NULL;
    mlc_unlink_tmp(path.c_str(), &e);
    return (e == NULL) ? "closed" : "close-REJECTED";
}

inline std::string presence(const std::string& path) {
    struct stat st;
    return (stat(path.c_str(), &st) == 0) ? "present" : "absent";
}

}  // namespace tmpthr

// Created on a spawned thread, closed on the thread that runs the manifold.
inline std::string closeHere(int64_t) {
    std::string err, path;
    std::thread t([&]() { path = tmpthr::make(err); });
    t.join();
    if (!err.empty()) return "make-REJECTED";
    std::string r = tmpthr::drop(path);
    return r + "|" + tmpthr::presence(path);
}

// Created on the manifold's thread, closed on a spawned thread.
inline std::string closeThere(int64_t) {
    std::string err;
    std::string path = tmpthr::make(err);
    if (!err.empty()) return "make-REJECTED";
    std::string r;
    std::thread t([&]() { r = tmpthr::drop(path); });
    t.join();
    return r + "|" + tmpthr::presence(path);
}

// A path the registry never issued must still be refused: @close is not a
// general file-removal tool. A path belonging to a different call is the same
// lookup, so this is also what keeps concurrent dispatches isolated.
inline std::string closeUnregistered(int64_t) {
    return tmpthr::drop("/tmp/morloc-tmpfile-worker-thread-never-registered");
}
