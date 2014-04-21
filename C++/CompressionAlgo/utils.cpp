#include "pch.h"
#include "utils.h"

int primeRounddown(int size) {
    static const int s_primes[] = {2, 3, 7, 13, 31, 61, 127, 251, 509, 1021, 2039, 4093, 8191, 16381, 32749, 65521, 131071, 262139, 524287, 1048573, 2097143, 4194301, 8388593, 16777213, 33554393, 67108859, 134217689, 268435399, 536870909, 1073741789, 2147483647,};
    int i = 1;
    for (; i < (int)ASIZE(s_primes) - 1 && s_primes[i] <= size; ++i);
    assert(s_primes[i - 1] <= size && size < s_primes[i]);
    return s_primes[i - 1];
}
