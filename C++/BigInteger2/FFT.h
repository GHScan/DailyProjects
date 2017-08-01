#ifndef FFT_H
#define FFT_H


#include <cassert>


#include <complex>
#include <vector>
#include <algorithm>
#include <numeric>


extern int WAssert(char const* message, char const* fileName, size_t line);


#ifdef _DEBUG
#define ASSERT  assert
#else
#define ASSERT  assert
// #define ASSERT(b) (void)((!!(b)) || (WAssert(#b, __FILE__, __LINE__)))
#endif


#define USE_SSE
// #define USE_RECURSIVE_FFT


inline bool IsPowerOf2(size_t n) {
    return ((n - 1) & n) == 0;
}

inline size_t NextPowerOf2(size_t n) {
    auto c = 0;
    for (; n > 0; n >>= 1, ++c);
    return size_t(1) << c;
}

inline bool Equals(double f1, double f2, double epsilon = 1e-4) {
    return fabs(f1 - f2) < epsilon;
}

inline bool Equals(std::complex<double> c1, std::complex<double> c2, double epsilon = 1e-4) {
    return Equals(c1.real(), c2.real(), epsilon) && Equals(c1.imag(), c2.imag(), epsilon);
}


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

    static_assert(sizeof(TOut) > sizeof(TIn) * 2, "");
    ASSERT(outSize >= (aSize + bSize - 1));

    auto fftSize = NextPowerOf2(outSize);
    std::vector<std::complex<double>> buf(fftSize * 3);
    auto p0 = &buf[0], p1 = &buf[0] + fftSize, p2 = &buf[0] + 2 * fftSize;

    transform(a, a + aSize, p0, inAdapter);
    FFT(p1, p0, fftSize);
    transform(b, b + bSize, p2, inAdapter);
    FFT(p0, p2, fftSize);

    for (auto i = 0; i < fftSize; ++i)
        p2[i] = p0[i] * p1[i];

    InverseFFT(p0, p2, fftSize);
    transform(p0, p0 + aSize + bSize - 1, out, outAdapter);

    for (auto i = aSize + bSize - 1; i < fftSize; ++i)
        ASSERT(llround(norm(p0[i])) == 0);
}


#endif