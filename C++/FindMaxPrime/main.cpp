#include "pch.h"

static long long largeRand(long long begin, long long end) {
    assert(begin < end);
    long long r = (long long)rand() * RAND_MAX + rand();
    return begin + r % (end - begin);
}

static long long powmod(long long a, long long b, long long c) {
    assert(a > 0 && b > 0 && c > 0);
    long long r = 1;
    long long exp2 = a % c;
    for (; b > 0; b >>= 1, exp2 = (exp2 * exp2) % c) {
        if (b & 1) r = (r * exp2) % c;
    }
    return r;
}
static bool isPrime(long long n) {
    for (int i = 0; i < 10; ++i) {
        long long k = largeRand(1, n);
        if (powmod(k, n - 1, n) != 1) return false;
    }
    return true;
}
static long long findMaxPrime(long long n) {
    while (!isPrime(n)) --n;
    return n;
}

int main() {
    srand(time(nullptr));

    for (int i = 1; i < 32; ++i) {
        long long n = (long long)1 << i;
        printf("%lld : %lld\n", n, findMaxPrime(n));
    }
}
