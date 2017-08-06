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


inline bool IsPowerOf2(size_t n) {
    return ((n - 1) & n) == 0;
}

inline size_t NextPowerOf2(size_t n) {
    if (IsPowerOf2(n)) return n;
    n |= n >> 1;
    n |= n >> 2;
    n |= n >> 4;
    n |= n >> 8;
    n |= n >> 16;
    return n + 1;
}

inline size_t BitReversal(size_t i, size_t bitCount) {
    ASSERT(bitCount <= 32);

    extern size_t gReversedBytes[256];
    auto rev = (gReversedBytes[(i >> 0) & 0xff] << 24)
        | (gReversedBytes[(i >> 8) & 0xff] << 16)
        | (gReversedBytes[(i >> 16) & 0xff] << 8)
        | (gReversedBytes[(i >> 24) & 0xff] << 0);
    return rev >> (32 - bitCount);
}


inline bool Equals(double f1, double f2, double epsilon = 1e-4) {
    return fabs(f1 - f2) < epsilon;
}

inline bool Equals(std::complex<double> c1, std::complex<double> c2, double epsilon = 1e-4) {
    return Equals(c1.real(), c2.real(), epsilon) && Equals(c1.imag(), c2.imag(), epsilon);
}


template<typename T>
void Memcpy(T *dest, T const *src, size_t n) {
    memcpy(dest, src, n * sizeof(T));
}

template<typename T>
void Memset(T *dest, uint8_t c, size_t n) {
    memset(dest, c, n * sizeof(T));
}


static auto const kPi = std::acos(-1);


#endif