#include "pch.h"

#include <sstream>

#include "bigInteger2.h"

static void correctnessTest() {
    BigInteger big(1);
    for (BigInteger i = 37; i < 3997; ++i) big *= i;

#define CHECK_MOD(mod, value) assert((big % mod).toInt<int>() == value)
    CHECK_MOD(2137442214, 941586270);
    CHECK_MOD(1593476709, 239985834);
    CHECK_MOD(672706286, 422409064);
    CHECK_MOD(1232087163, 858076431);
    CHECK_MOD(1935794919, 1527939018);
    CHECK_MOD(1902954453, 1055560455);
    CHECK_MOD(602715690, 357563520);
    CHECK_MOD(254874191, 102339237);
    CHECK_MOD(127786646, 32322940);
    CHECK_MOD(1833255524, 832052676);
#undef CHECK_MOD

    for (int i = 10; i < 20; ++i) {
        BigInteger mod = big % (1<<i);
        BigInteger lo = big - 23 * mod;
        assert(lo + 11 * mod == big - 12 * mod);
    }

    for (int i = 3996; i >= 37; --i) {
        assert(big % i == 0);
        big /= i;
    }
    assert(big == 1);
}

static void benchmark_correctness() {
    clock_t start = clock();
    for (int i = 0; i < 20; ++i) correctnessTest();
    printf("%f\n", float(clock() - start) / CLOCKS_PER_SEC);
}

static void benchmark_pow() {
    BigInteger big(37);
    for (int i = 10; i < 23; ++i) {
        clock_t start = clock();
        BigInteger r = big.pow(1<<i);
        printf("%d=%d,%f\n", i, r.getBitCount(), float(clock() - start) / CLOCKS_PER_SEC);
    }
}

static void benchmark_mod() {
    BigInteger big(1);
    for (int i = 37; i < 3997; ++i) big *= i;

    for (int i = 10; i < 23; ++i) {
        clock_t start = clock();
        for (int j = 2; j < (1<<i); ++j) big % j;
        printf("%d=%f\n", i, float(clock() - start) / CLOCKS_PER_SEC);
    }
}

static void benchmark_toString() {
    BigInteger big(37);
    for (int i = 10; i < 23; ++i) {
        BigInteger r = big.pow(1<<i);
        clock_t start = clock();
        int len = (int)r.toString().size();
        printf("%d=%d,%f\n", i, len, float(clock() - start) / CLOCKS_PER_SEC);
    }
}

static void benchmark_fromString() {
    BigInteger big(37);
    for (int i = 10; i < 23; ++i) {
        BigInteger r = big.pow(1<<i);
        stringstream si;
        si << r;
        clock_t start = clock();
        BigInteger r2(0);
        si >> r2;
        printf("%d=%d,%f\n", i, r == r2, float(clock() - start) / CLOCKS_PER_SEC);
    }
}

int main() {
    correctnessTest();
    // benchmark_correctness();
    // benchmark_pow();
    // benchmark_mod();
    // benchmark_toString();
    // benchmark_fromString();
}
