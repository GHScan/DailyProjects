#include "stdafx.h"

#include <cassert>

#include <complex>

#include "FFT.h"


#ifdef USE_SSE
#include "pmmintrin.h"
#include "emmintrin.h"
#endif


static void RecursiveFFTImpl(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size,
    size_t sstep,
    std::complex<double> w,
    double scale) {
    
    if (size == 1) {
        ASSERT(Equals(w.real(), 1.0));
        *dest = *src * scale;
    } else {
        auto halfSize = size / 2;

        auto w2 = w * w;
        RecursiveFFTImpl(dest, src, halfSize, sstep * 2, w2, scale);
        RecursiveFFTImpl(dest + halfSize, src + sstep, halfSize, sstep * 2, w2, scale);

        auto wi = std::complex<double>(1);
        for (size_t i = 0; i < halfSize; ++i) {
            auto c1 = dest[i], c2 = dest[i + halfSize];
            auto wc2 = wi * c2;
            dest[i] = c1 + wc2;
            dest[i + halfSize] = c1 - wc2;
            wi *= w;
        }
    }
}


#ifdef USE_SSE
static void FFTImpl2SSE(double *dest0, double *dest1, double *w) {
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

static void FFTImpl(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size,
    std::complex<double> w,
    double scale) {
    
    std::vector<std::complex<double>> ws(size / 2);
    {
        auto wi = std::complex<double>(1);
        for (auto &v : ws) {
            v = wi;
            wi *= w;
        }
    }

    if (scale == 1) {
        for (size_t i = 0, bc = BitCount(size) - 1; i < size; ++i)
            dest[BitReversal(i, bc)] = src[i];
    } else {
        for (size_t i = 0, bc = BitCount(size) - 1; i < size; ++i)
            dest[BitReversal(i, bc)] = src[i] * scale;
    }

    for (size_t s = 2; s <= size; s <<= 1) {
        size_t halfS = s / 2;
        size_t dw = size / s;
        for (size_t i = 0; i < size; i += s) {
            for (size_t j = 0, iw = 0; j < halfS; ++j, iw += dw) {
#ifdef USE_SSE
                FFTImpl2SSE(
                    reinterpret_cast<double*>(dest + i + j),
                    reinterpret_cast<double*>(dest + i + j + halfS),
                    reinterpret_cast<double*>(&ws[iw]));
#else
                auto c1 = dest[i + j], c2 = dest[i + j + halfS];
                auto wc2 = ws[iw] * c2;
                dest[i + j] = c1 + wc2;
                dest[i + j + halfS] = c1 - wc2;
#endif
            }
        }
    }
}

extern void FFT(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size) {

    ASSERT(IsPowerOf2(size));

#ifdef USE_RECURSIVE_FFT
    RecursiveFFTImpl(dest, src, size, 1, std::polar(1.0, 2 * kPi / size), 1);
#else
    FFTImpl(dest, src, size, std::polar(1.0, 2 * kPi / size), 1);
#endif
}

extern void InverseFFT(
    std::complex<double> *dest,
    std::complex<double> const *src,
    size_t size) {

    ASSERT(IsPowerOf2(size));

#ifdef USE_RECURSIVE_FFT
    RecursiveFFTImpl(dest, src, size, 1, std::polar(1.0, -2 * kPi / size), 1.0 / size);
#else
    FFTImpl(dest, src, size, std::polar(1.0, -2 * kPi / size), 1.0 / size);
#endif
}