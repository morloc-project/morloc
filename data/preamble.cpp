#include <string>
#include <string>
#include <string>
#include <string>
#include <iostream>
#include <sstream>
#include <functional>
#include <vector>
#include <algorithm> // for std::transform
#include <stdexcept>
#include <fstream>

// needed for foreign interface
#include <cstdlib>
#include <cstdio>
#include <cstdint>
#include <unistd.h>

// needed for interop
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/select.h>
#include <cmath>
#include <iomanip>

#include <fcntl.h>

#include <limits>
#include <tuple>
#include <utility>
#include <limits>

#define BUFFER_SIZE 4096

#define PACKET_TYPE_DATA  0x00
#define PACKET_TYPE_CALL  0x01
#define PACKET_TYPE_GET   0x02
#define PACKET_TYPE_PUT   0x03
#define PACKET_TYPE_PING  0x04

#define PACKET_SOURCE_MESG  0x00 // the message contains the data
#define PACKET_SOURCE_FILE  0x01 // the message is a path to a file of data
#define PACKET_SOURCE_NXDB  0x02 // the message is a key to the nexus uses to access the data

#define PACKET_FORMAT_JSON  0x00

#define PACKET_COMPRESSION_NONE  0x00 // uncompressed
#define PACKET_ENCRYPTION_NONE   0x00 // unencrypted

#define PACKET_STATUS_PASS  0x00
#define PACKET_STATUS_FAIL  0x01



using namespace std;

struct Message {
  char* data;
  size_t length;
};

struct Header {
    char command[8];
    int offset; // this is a 4 byte int, so I should use a short
    int length;
};

// Function to log messages
template <class M> 
void log_message(M message) {
    std::ofstream log_file("log", std::ios_base::app);
    log_file << "C: " << message << std::endl;
}

std::string show_hex(const char* input, int length) {
    std::stringstream ss;

    for(int i = 0; i < length; i++) {
        ss << std::hex << std::setw(2) << std::setfill('0') << (int)(unsigned char)input[i];
        if (i < length - 1) {
            ss << " ";
        }

    }

    return ss.str();
}
