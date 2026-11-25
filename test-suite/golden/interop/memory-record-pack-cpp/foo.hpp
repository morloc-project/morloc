#ifndef MORLOC_MEMORY_RECORD_PACK_HPP
#define MORLOC_MEMORY_RECORD_PACK_HPP

struct pair_br_t {
    bool flag;
    double value;
};

struct triple_bir_t {
    bool flag;
    int count;
    double value;
};

pair_br_t makePair(bool flag, double value) {
    return pair_br_t{flag, value};
}

bool getPairFlag(const pair_br_t& p) {
    return p.flag;
}

double getPairValue(const pair_br_t& p) {
    return p.value;
}

pair_br_t pairRoundTrip(const pair_br_t& p) {
    return pair_br_t{p.flag, p.value};
}

triple_bir_t makeTriple(bool flag, int count, double value) {
    return triple_bir_t{flag, count, value};
}

int getTripleCount(const triple_bir_t& t) {
    return t.count;
}

double getTripleValue(const triple_bir_t& t) {
    return t.value;
}

triple_bir_t tripleRoundTrip(const triple_bir_t& t) {
    return triple_bir_t{t.flag, t.count, t.value};
}

#endif
