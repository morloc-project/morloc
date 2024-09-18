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

#define BUFFER_SIZE 1024

using namespace std;
