#include <utility>

struct location_t {
  double latitude;
  double longitude;
  double altitude;
};

struct worker_t {
  location_t home;
  int age;
  std::tuple<bool, location_t> job;
};
