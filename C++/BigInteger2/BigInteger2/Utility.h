#ifndef UTILITY
#define UTILITY


#include <cassert>


#include <complex>
#include <chrono>
#include <algorithm>


extern int WAssert(char const* message, char const* fileName, size_t line);
#ifdef _DEBUG
#define ASSERT  assert
#else
#define ASSERT  assert
// #define ASSERT(b) (void)((!!(b)) || (WAssert(#b, __FILE__, __LINE__)))
#endif


#define USE_SINGLE_FLOAT_FFT 0
#define USE_SIMD 1
#define USE_RECURSIVE_FFT 0
#define USE_SCANFFT 0


#if USE_SINGLE_FLOAT_FFT
using FFTFloat = float;
#else
using FFTFloat = double;
#endif


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

inline size_t ReverseBits(size_t i, size_t bitCount) {
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


template <typename TFunc>
static double Timing(TFunc func, int times = 3) {
    using namespace std::chrono;

    if (times > 1) 
        func();

    auto t = std::numeric_limits<double>::max();
    for (auto i = 0; i < times; ++i) {
        auto start = high_resolution_clock::now();
        func();
        auto end = high_resolution_clock::now();
        t = std::min(t, duration<double>(end - start).count());
    }

    return t;
}


#ifdef _MSC_VER
#define FORCEINLINE __forceinline
#else
#define FORCEINLINE __attribute__((always_inline))
#endif


#ifdef _MSC_VER
template<typename T>
inline T* AlignedAlloc(size_t size) {
    return static_cast<T*>(_aligned_malloc(size * sizeof(T), 32));
}
template<typename T>
inline void AlignedFree(T *p) {
    _aligned_free(p);
}
#else
template<typename T>
inline T* AlignedAlloc(size_t size) {
    return static_cast<T*>(aligned_alloc(32, size * sizeof(T)));
}
template<typename T>
inline void AlignedFree(T *p) {
    free(p);
}
#endif


#endif