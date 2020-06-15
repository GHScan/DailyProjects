#ifndef UTILS_H
#define UTILS_H

#include <cassert>
#include <cstdint>
#include <cmath>

#include <limits>
#include <chrono>
#include <algorithm>
#include <type_traits>


#define kPI     3.14159265358979323846f


#define ARRAY_SIZE(a)   (sizeof(a) / sizeof((a)[0]))


template<typename T>
inline T DivideUp(T a, T b)
{
    return (a + b - 1) / b;
}

template<typename T>
inline T RoundUp(T a, T b)
{
    return DivideUp(a, b) * b;
}


template<typename TFunc>
inline double Time(TFunc const &f)
{
    using namespace std::chrono;

    auto start = high_resolution_clock::now();
    f();
    auto stop = high_resolution_clock::now();
    return duration<double>(stop - start).count();
}

template<typename TFunc>
inline double TimeInDuration(TFunc const &f, double duration = 0.25f)
{
    double estimateTime = Time([&]() { f(); });
    int loop = int(std::ceil(duration / estimateTime));

    double minTime = std::numeric_limits<double>::max();
    for (int i = 0; i < loop; ++i)
        minTime = std::min(minTime, Time([&]() { f(); }));

    return minTime;
}


template<
    typename TDst, 
    typename TSrc,
    std::enable_if_t<!(std::is_integral_v<TDst> && std::is_floating_point_v<TSrc>), int> = 0>
inline TDst Convert(TSrc src)
{
    return TDst(src);
}

template<
    typename TDst, 
    typename TSrc, 
    std::enable_if_t<std::is_integral_v<TDst> && std::is_floating_point_v<TSrc>, int> = 0>
inline TDst Convert(TSrc src)
{
    return TDst(std::round(src));
}


template<typename TDst, typename TSrc>
inline void Copy(TDst *dst, TSrc const *src, size_t n)
{
    for (size_t i = 0; i < n; ++i)
        dst[i] = Convert<TDst>(src[i]);
}


inline bool FloatEquals(float value, float expect, float percent = 1e-3)
{
    float diff = value - expect;
    return std::abs(expect) < 1e-4 
        ? std::abs(value) < 1e-4
        : std::abs(diff / expect) < percent;
}


template<typename T>
inline T const& Clamp(T const &v, T const &lo, T const &hi)
{
    assert(!(hi < lo));
    return (v < lo) ? lo : (hi < v) ? hi : v;
}


void DumpNumbers(float const *src, int n, int lineW = 8);
void DumpNumbers(uint8_t const *src, int n, int lineW = 8);
inline void DumpNumbers(int8_t const *src, int n, int lineW = 8)
{
    DumpNumbers(reinterpret_cast<uint8_t const *>(src), n, lineW);
}


#endif