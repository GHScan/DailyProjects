#ifndef NTT_H
#define NTT_H


#include <cstdint>


#include <limits>
#include <vector>
#include <algorithm>


#include "Utility.h"


namespace NTTFieldArithmetic {

    constexpr uint64_t kPrime = 4179340454199820289ULL;
    constexpr uint64_t kHalfPrime = kPrime >> 1;
    constexpr long double kInvPrime = 2.3927220358300787503404874106038e-19;
    constexpr uint64_t kPrimitiveRoot = 3;

    inline uint64_t Add(uint64_t a, uint64_t b) {
        ASSERT(a < kPrime && b < kPrime);
        auto s = a + b;
        return s >= kPrime ? s - kPrime : s;
    }

    inline uint64_t Substract(uint64_t a, uint64_t b) {
        ASSERT(a < kPrime && b < kPrime);
        return a < b ? kPrime - b + a : a - b;
    }

#ifdef _MSC_BUILD
    inline uint64_t Multiply(uint64_t a, uint64_t b) {
        ASSERT(a < kPrime && b < kPrime);
        uint64_t r = 0;
        for (auto _ = 0; _ < 64; ++_) {
            r = r > kHalfPrime ? (r << 1) - kPrime : r << 1;
            r = a & 0x8000000000000000ULL ? r + b : r;
            r = r > kPrime ? r - kPrime : r;
            a <<= 1;
        }
        return r;
    }
#else
    inline uint64_t Multiply(uint64_t a, uint64_t b)
    {
        ASSERT(a < kPrime && b < kPrime);
        ASSERT(std::numeric_limits<long double>::digits >= 64);
        long double fa = a;
        auto q = static_cast<uint64_t>(fa * b * kInvPrime);
        auto r = static_cast<int64_t>(a * b - q * kPrime) % static_cast<int64_t>(kPrime);
        return r < 0 ? r + kPrime : r;
    }
#endif

    inline uint64_t Power(uint64_t a, int64_t b) {
        uint64_t r = 1;
        for (; b > 0; b >>= 1) {
            if (b & 1)
                r = Multiply(r, a);
            a = Multiply(a, a);
        }
        return r;
    }
}


extern void NTT(
    uint64_t *dest, 
    uint64_t const *src,
    size_t size);

extern void InverseNTT(
    uint64_t *dest,
    uint64_t const *src,
    size_t size);


template<typename TIn, typename TOut, typename TInAdapter, typename TOutAdapter>
inline void NTConvolve(
    TIn const *a, size_t aSize,
    TIn const *b, size_t bSize,
    TOut *out, size_t outSize,
    TInAdapter inAdapter, TOutAdapter outAdapter) {

    static_assert(sizeof(TOut) >= sizeof(TIn) * 2, "");
    ASSERT(outSize >= (aSize + bSize - 1));

    auto nttSize = NextPowerOf2(outSize);
    std::vector<uint64_t> buf(nttSize * 3);
    auto p0 = &buf[0], p1 = &buf[0] + nttSize, p2 = &buf[0] + 2 * nttSize;

    std::transform(a, a + aSize, p0, inAdapter);
    NTT(p1, p0, nttSize);
    std::transform(b, b + bSize, p2, inAdapter);
    NTT(p0, p2, nttSize);

    for (size_t i = 0; i < nttSize; ++i)
        p2[i] = NTTFieldArithmetic::Multiply(p0[i], p1[i]);

    InverseNTT(p0, p2, nttSize);
    std::transform(p0, p0 + aSize + bSize - 1, out, outAdapter);

    for (auto i = aSize + bSize - 1; i < nttSize; ++i)
        ASSERT(p0[i] == 0);
}


#endif