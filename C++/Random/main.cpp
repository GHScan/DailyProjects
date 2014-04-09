#include "pch.h"

class RandomANSIC {
public:
    RandomANSIC(int seed) { srand(seed); }
    int next() { return rand(); }
    int max() const { return RAND_MAX; }
};

class RandomVC {
public:
    RandomVC(int seed): mSeed(seed << 15) { }
    int next() {
        mSeed *= 214013;
        mSeed += 2531011;
        return (mSeed >> 15) & 0x7fff;
    }
    int max() const { return 1 << 15; }
private:
    unsigned mSeed;
};

class RandomGCC {
public:
    RandomGCC(int seed): mSeed(seed << 15) { }
    int next() {
        mSeed *= 1103515245;
        mSeed += 12345;
        return (mSeed >> 15) & 0x7fff;
    }
    int max() const { return 1 << 15; }
private:
    unsigned mSeed;
};

template<typename RandomT>
static void testRandom(int seed) {
    RandomT random(seed);

    int lens[] = {256, 4 * 1024, 16 * 1024};
    vector<int> v;
    for (int len : lens) {
        v.clear();
        v.resize(len, 0);

        const int AVG = 4 * 1024;
        const double MISS_RATE = 0.1;
        for (int i = 0; i < AVG * (int)v.size(); ++i) {
            unsigned r = random.next();
            if(random.max() < v.size()) r = r * random.max() + random.next();
            ++v[r % v.size()];
        }

        for (int i = 0; i < (int)v.size(); ++i) {
            assert(fabs(v[i] / double(AVG) - 1) < MISS_RATE);
        }
    }
}

#define TEST(type, seed) puts("test: " #type); testRandom<type>(seed);

int main() {
    int seed = (int)time(nullptr);

    TEST(RandomANSIC, seed);
    TEST(RandomVC, seed);
    TEST(RandomGCC, seed);
}
