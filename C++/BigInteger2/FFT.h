#ifndef FFT_H
#define FFT_H


#include <complex>
#include <vector>
#include <algorithm>
#include <numeric>


#include "Utility.h"


extern void FFT(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size);

extern void InverseFFT(
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
inline void Convolve(
    TIn const *a, size_t aSize,
    TIn const *b, size_t bSize,
    TOut *out, size_t outSize,
    TInAdapter inAdapter, TOutAdapter outAdapter) {

    static_assert(sizeof(TOut) >= sizeof(TIn) * 2, "");
    ASSERT(outSize >= (aSize + bSize - 1));

    auto fftSize = NextPowerOf2(outSize);
    std::vector<std::complex<double>> buf(fftSize * 3);
    auto p0 = &buf[0], p1 = &buf[0] + fftSize, p2 = &buf[0] + 2 * fftSize;

    std::transform(a, a + aSize, p0, inAdapter);
    FFT(p1, p0, fftSize);
    std::transform(b, b + bSize, p2, inAdapter);
    FFT(p0, p2, fftSize);

    for (size_t i = 0; i < fftSize; ++i)
        p2[i] = p0[i] * p1[i];

    InverseFFT(p0, p2, fftSize);
    std::transform(p0, p0 + aSize + bSize - 1, out, outAdapter);

    for (auto i = aSize + bSize - 1; i < fftSize; ++i)
        ASSERT(llround(norm(p0[i])) == 0);
}


template<typename TIn, typename TOut, typename TInAdapter, typename TOutAdapter>
inline void ClassicConvolve(
    TIn const *a, size_t aSize,
    TIn const *b, size_t bSize,
    TOut *out, size_t outSize,
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