#ifndef FFT_H
#define FFT_H


#include <complex>
#include <vector>
#include <algorithm>
#include <numeric>


#include "Utility.h"


extern void FastFourierTransform(
    std::complex<FFTFloat> *dest,
    std::complex<FFTFloat> const *src,
    size_t size);

extern void InverseFastFourierTransform(
    std::complex<FFTFloat> *dest,
    std::complex<FFTFloat> const *src,
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


#if !USE_SCANFFT

template<typename TIn, typename TOut, typename TInAdapter, typename TOutAdapter>
inline void Convolve_FFT(
    TOut *out, size_t outSize,
    TIn const *a, size_t aSize,
    TIn const *b, size_t bSize,
    TInAdapter inAdapter, TOutAdapter outAdapter) {

    static_assert(sizeof(TOut) >= sizeof(TIn) * 2, "");
    ASSERT(outSize >= (aSize + bSize - 1));

    auto fftSize = NextPowerOf2(aSize + bSize - 1);
    std::vector<std::complex<FFTFloat>> bufVec(fftSize * 3);
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

#else

#include "ScanFFT.h"
#include "ScanFFT_ComplexV.h"

template<typename TIn, typename TOut, typename TInAdapter, typename TOutAdapter>
inline void Convolve_FFT(
    TOut *out, size_t outSize,
    TIn const *a, size_t aSize,
    TIn const *b, size_t bSize,
    TInAdapter inAdapter, TOutAdapter outAdapter) {

    static_assert(sizeof(TOut) >= sizeof(TIn) * 2, "");
    ASSERT(outSize >= (aSize + bSize - 1));

    auto fftSize = NextPowerOf2(aSize + bSize - 1);

    auto log2OfFFTSize = uint8_t(log2(fftSize));

    SCANFFT_ALLOC(vec, fftSize * 3);
    auto vec0Reals = &vecReals[0], vec1Reals = &vecReals[0] + fftSize, vec2Reals = &vecReals[0] + 2 * fftSize;
    auto vec0Imags = &vecImags[0], vec1Imags = &vecImags[0] + fftSize, vec2Imags = &vecImags[0] + 2 * fftSize;

    std::transform(a, a + aSize, vec0Reals, inAdapter);
    Memset(vec0Reals + aSize, 0, fftSize - aSize);
    Memset(vec0Imags, 0, fftSize);
    SCANFFT_TRANSFORM(vec1, vec0, log2OfFFTSize);

    if (b == a && bSize == aSize) {
        Memcpy(vec0Reals, vec1Reals, fftSize);
        Memcpy(vec0Imags, vec1Imags, fftSize);
    } else {
        std::transform(b, b + bSize, vec2Reals, inAdapter);
        Memset(vec2Reals + bSize, 0, fftSize - bSize);
        Memset(vec2Imags, 0, fftSize);
        SCANFFT_TRANSFORM(vec0, vec2, log2OfFFTSize);
    }

    {
        size_t i = 0, ei = fftSize / 2 + 1;
        for (; i < ei; i += SCANFFT_COMPLEXV_DIMENSION) {
            SCANFFT_COMPLEXV_LOAD(c0, vec0, i);
            SCANFFT_COMPLEXV_LOAD(c1, vec1, i);
            SCANFFT_COMPLEXV_MUL(c2, c0, c1);
            SCANFFT_COMPLEXV_STORE(vec2, i, c2);
        }
        for (; i < fftSize; ++i) {
            vec2Reals[i] = vec2Reals[fftSize - i];
            vec2Imags[i] = -vec2Imags[fftSize - i];
        }
    }

    SCANFFT_INVERSE_TRANSFORM(vec0, vec2, log2OfFFTSize);
    for (size_t i = 0; i < aSize + bSize - 1; ++i)
        out[i] = outAdapter(std::complex<FFTFloat>(vec0Reals[i], vec0Imags[i]));

    for (auto i = aSize + bSize - 1; i < fftSize; ++i)
        ASSERT(llround(vec0Imags[i]) == 0);

    SCANFFT_FREE(vec);
}

#endif

#endif