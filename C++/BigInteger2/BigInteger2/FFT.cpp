
#include <cassert>

#include <complex>


#if USE_SIMD
#include <pmmintrin.h>
#include <emmintrin.h>
#endif


#include "FFT.h"


static void RecursiveFFT(
    std::complex<FFTFloat> *dest,
    std::complex<FFTFloat> const *src,
    size_t size,
    size_t sstep,
    std::complex<FFTFloat> w,
    FFTFloat scale) {
    
    if (size == 1) {
        ASSERT(Equals(w.real(), 1.0));
        *dest = *src * scale;
    } else {
        auto halfSize = size / 2;

        auto w2 = w * w;
        RecursiveFFT(dest, src, halfSize, sstep * 2, w2, scale);
        RecursiveFFT(dest + halfSize, src + sstep, halfSize, sstep * 2, w2, scale);

        auto wi = std::complex<FFTFloat>(1);
        for (size_t i = 0; i < halfSize; ++i) {
            auto c0 = dest[i], c1 = dest[i + halfSize];
            auto wc1 = wi * c1;
            dest[i] = c0 + wc1;
            dest[i + halfSize] = c0 - wc1;
            wi *= w;
        }
    }
}


#if USE_SIMD
static void FFTCombine2_SSE(double *dest0, double *dest1, double *w) {
    auto r1 = _mm_loaddup_pd(dest1);
    auto i1 = _mm_loaddup_pd(dest1 + 1);
    auto cw = _mm_load_pd(w);
    auto revcw = _mm_shuffle_pd(cw, cw, 1);

    auto t0 = _mm_mul_pd(r1, cw);
    auto t1 = _mm_mul_pd(i1, revcw);

    auto wc1 = _mm_addsub_pd(t0, t1);

    auto c0 = _mm_load_pd(dest0);

    auto nc0 = _mm_add_pd(c0, wc1);
    auto nc1 = _mm_sub_pd(c0, wc1);

    _mm_store_pd(dest0, nc0);
    _mm_store_pd(dest1, nc1);
}
#endif 

static void FFT(
    std::complex<FFTFloat> *dest,
    std::complex<FFTFloat> const *src,
    size_t size,
    std::complex<FFTFloat> w,
    FFTFloat scale) {
    
    std::vector<std::complex<FFTFloat>> ws(size / 2);
    {
        auto wi = std::complex<FFTFloat>(1);
        for (auto &v : ws) {
            v = wi;
            wi *= w;
        }
    }

    if (scale == 1) {
        for (size_t i = 0, bc = static_cast<size_t>(log2(size)); i < size; ++i)
            dest[ReverseBits(i, bc)] = src[i];
    } else {
        for (size_t i = 0, bc = static_cast<size_t>(log2(size)); i < size; ++i)
            dest[ReverseBits(i, bc)] = src[i] * scale;
    }

    for (size_t s = 2; s <= size; s <<= 1) {
        size_t halfS = s / 2;
        size_t dw = size / s;
        for (size_t i = 0; i < size; i += s) {
            for (size_t j = 0, iw = 0; j < halfS; ++j, iw += dw) {
#if (USE_SIMD && !USE_SINGLE_FLOAT_FFT)
                FFTCombine2_SSE(
                    reinterpret_cast<double*>(dest + i + j),
                    reinterpret_cast<double*>(dest + i + j + halfS),
                    reinterpret_cast<double*>(&ws[iw]));
#else
                auto c0 = dest[i + j], c1 = dest[i + j + halfS];
                auto wc1 = ws[iw] * c1;
                dest[i + j] = c0 + wc1;
                dest[i + j + halfS] = c0 - wc1;
#endif
            }
        }
    }
}

extern void FastFourierTransform(
    std::complex<FFTFloat> *dest,
    std::complex<FFTFloat> const *src,
    size_t size) {

    ASSERT(IsPowerOf2(size));

#if USE_RECURSIVE_FFT
    RecursiveFFT(dest, src, size, 1, std::polar(1.0, 2 * kPi / size), 1);
#else
    FFT(dest, src, size, std::polar(1.0, 2 * kPi / size), 1);
#endif
}

extern void InverseFastFourierTransform(
    std::complex<FFTFloat> *dest,
    std::complex<FFTFloat> const *src,
    size_t size) {

    ASSERT(IsPowerOf2(size));

#if USE_RECURSIVE_FFT
    RecursiveFFT(dest, src, size, 1, std::polar(1.0, -2 * kPi / size), 1.0 / size);
#else
    FFT(dest, src, size, std::polar(1.0, -2 * kPi / size), FFTFloat(1.0) / size);
#endif
}