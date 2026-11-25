#include <vector>
#include <functional>
#include "mlccpptypes/prelude.hpp"

// A running tally the callbacks mutate as a side effect.
static int TALLY = 0;
inline void resetTally() { TALLY = 0; }
inline int totalSoFar() { return TALLY; }
inline void tap(int x) { TALLY += x; }
inline int add(int a, int b) { return a + b; }

// Invoke each callback in the list on x. The callbacks arrive already
// forced (function<Unit(int)>, not function<function<Unit()>(int)>): the
// source code invokes them as cb(x) and discards the return, so if the
// morloc side handed over unforced thunks the effect would never run.
inline void runList(std::vector<std::function<mlc::Unit(int)>> cbs, int x) {
    for (auto& cb : cbs) { cb(x); }
}
