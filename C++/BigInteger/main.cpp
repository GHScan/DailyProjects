#include "pch.h"

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
}

static void benchmark() {
    BigInteger big(37);
    for (int i = 10; i < 23; ++i) {
        clock_t start = clock();
        BigInteger r = big.pow(1<<i);
        printf("%d=%d,%f\n", i, r.getBitCount(), float(clock() - start) / CLOCKS_PER_SEC);
    }
}

static void benchmark2() {
    BigInteger big(1);
    for (int i = 37; i < 3997; ++i) big *= i;

    for (int i = 10; i < 23; ++i) {
        clock_t start = clock();
        for (int j = 2; j < (1<<i); ++j) big % j;
        printf("%d=%f\n", i, float(clock() - start) / CLOCKS_PER_SEC);
    }
}

static void benchmark3() {
    BigInteger big(37);
    for (int i = 10; i < 23; ++i) {
        BigInteger r = big.pow(1<<i);
        clock_t start = clock();
        int len = (int)r.toString().size();
        printf("%d=%d,%f\n", i, len, float(clock() - start) / CLOCKS_PER_SEC);
    }
}

int main() {
    correctnessTest();
}
