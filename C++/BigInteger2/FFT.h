#ifndef FFT_H
#define FFT_H


#include <complex>
#include <vector>
#include <algorithm>
#include <numeric>


#include "Utility.h"


// #define USE_RECURSIVE_FFT


extern void FastFourierTransform(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size);

extern void InverseFastFourierTransform(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size);


template<typename TIn>
inline bool CanConvolveIntegersAccurately(
    TIn const *a, size_t aSize,
    TIn const *b, size_t bSize) {
    
    auto sum1 = std::accumulate(a, a + aSize, 0ULL);
    auto sum2 = std::accumulate(b, b + bSize, 0ULL);

    auto constexpr kMaxIntegerAsDouble = 1ULL << 52;
    if (sum1 > kMaxIntegerAsDouble || sum2 > kMaxIntegerAsDouble)
        return false;

    return kMaxIntegerAsDouble % sum1 < kMaxIntegerAsDouble % sum2
        ? sum2 < kMaxIntegerAsDouble / sum1
        : sum1 < kMaxIntegerAsDouble / sum2;
}


template<typename TIn, typename TOut, typename TInAdapter, typename TOutAdapter>
inline void Convolve_FFT(
    TOut *out, size_t outSize,
    TIn const *a, size_t aSize,
    TIn const *b, size_t bSize,
    TInAdapter inAdapter, TOutAdapter outAdapter) {

    static_assert(sizeof(TOut) >= sizeof(TIn) * 2, "");
    ASSERT(outSize >= (aSize + bSize - 1));

    auto fftSize = NextPowerOf2(aSize + bSize - 1);
    std::vector<std::complex<double>> bufVec(fftSize * 3);
    auto buf0 = &bufVec[0], buf1 = &bufVec[0] + fftSize, buf2 = &bufVec[0] + 2 * fftSize;

    std::transform(a, a + aSize, buf0, inAdapter);
    FastFourierTransform(buf1, buf0, fftSize);
    if (b == a && bSize == aSize) {
        Memcpy(buf0, buf1, fftSize);
    } else {
        std::transform(b, b + bSize, buf2, inAdapter);
        FastFourierTransform(buf0, buf2, fftSize);
    }

    for (size_t i = 0; i < fftSize; ++i)
        buf2[i] = buf0[i] * buf1[i];

    InverseFastFourierTransform(buf0, buf2, fftSize);
    std::transform(buf0, buf0 + aSize + bSize - 1, out, outAdapter);

    for (auto i = aSize + bSize - 1; i < fftSize; ++i)
        ASSERT(llround(norm(buf0[i])) == 0);
}


template<typename TIn, typename TOut, typename TInAdapter, typename TOutAdapter>
inline void Convolve_Classic(
    TOut *out, size_t outSize,
    TIn const *a, size_t aSize,
    TIn const *b, size_t bSize,
    TInAdapter inAdapter, TOutAdapter outAdapter) {

    std::fill(out, out + outSize, 0);

    for (size_t i = 0; i < aSize; ++i) {
        auto va = outAdapter(inAdapter(a[i]));
        for (size_t j = 0; j < bSize; ++j) {
            out[i + j] += va * outAdapter(inAdapter(b[j]));
        }
    }
}

#endif