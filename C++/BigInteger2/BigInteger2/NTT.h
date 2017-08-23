#ifndef NTT_H
#define NTT_H


#include <cstdint>


#include <limits>
#include <vector>
#include <algorithm>


#include "Utility.h"


namespace RingArithmetic_NTT {

    uint64_t Multiply(uint64_t a, uint64_t b);
}


extern void NumberTheoreticTransform(
    uint64_t *dest, 
    uint64_t const *src,
    size_t size);

extern void InverseNumberTheoreticTransform(
    uint64_t *dest,
    uint64_t const *src,
    size_t size);


template<typename TIn, typename TOut, typename TInAdapter, typename TOutAdapter>
inline void Convolve_NTT(
    TOut *out, size_t outSize,
    TIn const *a, size_t aSize,
    TIn const *b, size_t bSize,
    TInAdapter inAdapter, TOutAdapter outAdapter) {

    static_assert(sizeof(TOut) >= sizeof(TIn) * 2, "");
    ASSERT(outSize >= (aSize + bSize - 1));

    auto nttSize = NextPowerOf2(aSize + bSize - 1);
    std::vector<uint64_t> bufVec(nttSize * 3);
    auto buf0 = &bufVec[0], buf1 = &bufVec[0] + nttSize, buf2 = &bufVec[0] + 2 * nttSize;

    std::transform(a, a + aSize, buf0, inAdapter);
    NumberTheoreticTransform(buf1, buf0, nttSize);
    if (b == a && bSize == aSize) {
        Memcpy(buf0, buf1, nttSize);
    } else {
        std::transform(b, b + bSize, buf2, inAdapter);
        NumberTheoreticTransform(buf0, buf2, nttSize);
    }

    for (size_t i = 0; i < nttSize; ++i)
        buf2[i] = RingArithmetic_NTT::Multiply(buf0[i], buf1[i]);

    InverseNumberTheoreticTransform(buf0, buf2, nttSize);
    std::transform(buf0, buf0 + aSize + bSize - 1, out, outAdapter);

    for (auto i = aSize + bSize - 1; i < nttSize; ++i)
        ASSERT(buf0[i] == 0);
}


#endif
