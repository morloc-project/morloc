#include <cstdlib>
#include <string>

static std::string cnode() {
    const char* n = std::getenv("MOCK_NODE");
    return n ? std::string(n) : std::string("parent");
}

std::string chere(int x) { return cnode() + ":" + std::to_string(x) + "c"; }

std::string ctag(std::string s) { return "ctag@" + cnode() + "[" + s + "]"; }
