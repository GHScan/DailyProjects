#ifndef SCANFFT_UTILS_H
#define SCANFFT_UTILS_H


#include <limits>
#include <chrono>
#include <algorithm>


#define SCANFFT_CONCAT_(a, b) a ## b
#define SCANFFT_CONCAT(a, b) SCANFFT_CONCAT_(a, b)


#ifdef _MSC_VER
#define SCANFFT_FORCEINLINE __forceinline
#else
#define SCANFFT_FORCEINLINE __attribute__((always_inline))
#endif


namespace ScanFFT {


#ifdef _MSC_VER
template<typename T>
inline T* Alloc(size_t size) {
    return static_cast<T*>(_aligned_malloc(size * sizeof(T), 64));
}
template<typename T>
inline void Free(T *p) {
    _aligned_free(p);
}
#else
template<typename T>
inline T* Alloc(size_t size) {
    return static_cast<T*>(aligned_alloc(64, size * sizeof(T)));
}
template<typename T>
inline void Free(T *p) {
    free(p);
}
#endif


inline bool FEquals(double f1, double f2, double epsilon = 1e-4) {
    return fabs(f1 - f2) < epsilon;
}


template <typename TFunc>
inline double Timing(TFunc &&func, int times = 3) {
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


extern size_t gReversedBytes[];


}


#endif