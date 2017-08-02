#ifndef UTILITY
#define UTILITY


#include <cassert>


#include <complex>


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

inline size_t BitCount(size_t n) {
    auto c = 0;
    do {
        ++c;
        n >>= 1;
    } while (n > 0);
    return c;
}

inline size_t BitReversal(size_t i, size_t bitCount) {
    ASSERT(bitCount <= 32);

    extern size_t gReverseBytes[256];
    auto rev = (gReverseBytes[(i >> 0) & 0xff] << 24)
        | (gReverseBytes[(i >> 8) & 0xff] << 16)
        | (gReverseBytes[(i >> 16) & 0xff] << 8)
        | (gReverseBytes[(i >> 24) & 0xff] << 0);
    return rev >> (32 - bitCount);
}


inline bool Equals(double f1, double f2, double epsilon = 1e-4) {
    return fabs(f1 - f2) < epsilon;
}

inline bool Equals(std::complex<double> c1, std::complex<double> c2, double epsilon = 1e-4) {
    return Equals(c1.real(), c2.real(), epsilon) && Equals(c1.imag(), c2.imag(), epsilon);
}


static auto const kPi = std::acos(-1);


#endif