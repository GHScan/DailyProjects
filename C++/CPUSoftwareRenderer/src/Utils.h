#ifndef UTIL_H
#define UTIL_H


#include <cassert>
#include <cmath>

#include <iostream>
#include <functional>
#include <algorithm>
#include <chrono>
#include <future>



template<typename TFunc>
inline double Time(TFunc const &f)
{
    using namespace std::chrono;
    auto start = steady_clock::now();
    f();
    auto end = steady_clock::now();
    return duration<double>(end - start).count();
}


inline int DivideUp(int a, int b)
{
    return (a + b - 1) / b;
}


inline int32_t ReverseBytes(uint32_t v)
{
    return ((v & 0xff) << 24) | 
        ((v & 0xff00) << 8) | 
        ((v & 0xff0000) >> 8) |
        ((v & 0xff000000) >> 24);
}


class ScopeGuard
{
public:
    explicit ScopeGuard(std::function<void()> onExitScope)
        : mOnExitScope(onExitScope)
    { 
    }

    ~ScopeGuard()
    {
        mOnExitScope();
    }

private:
    std::function<void()> mOnExitScope;
};
#define SCOPEGUARD_LINENAME_CAT(name, line) name ## line
#define SCOPEGUARD_LINENAME(name, line) SCOPEGUARD_LINENAME_CAT(name, line)

#define ON_SCOPE_EXIT(callback) ScopeGuard SCOPEGUARD_LINENAME(EXIT, __LINE__)(callback)



#define DAssert     assert
#if _DEBUG
#define RAssert     assert
#else
#define RAssert(b) \
    do \
    { \
        if (!(b)) \
        { \
            fprintf(stderr, "%s(%d): %s\n", __FILE__, __LINE__, #b); \
            exit(1);\
        } \
    } \
    while (false)
#endif


inline bool Equals(float a, float b)
{
    float diff = std::fabs(a - b);
    float min = std::min(std::fabs(a), std::fabs(b));
    return diff < 1e-5 || (diff < 1e-3 * min);
}


template<typename T>
inline T Read(std::istream& in)
{
    T v;
    in >> v;
    return v;
}


namespace Detail
{

template<size_t n>
struct Integer
{
    static constexpr size_t Val = n;
};

template <typename TFunc, size_t... ints>
inline void For(TFunc const &func, std::index_sequence<ints...>)
{
    using expander = int[];
    (void)expander
    {
        0, ((void)func(Integer<ints>{}), 0)...
    };
}

}

template <size_t n, typename TFunc>
inline void For(TFunc const &func)
{
    Detail::For(func, std::make_index_sequence<n>());
}


template<typename TFunc>
inline void ParallelFor(int td, TFunc const &f)
{
    std::vector<std::future<void>> futures;
    for (int tid = 0; tid < td; ++tid)
    {
        futures.emplace_back(std::async(std::launch::async, f, tid));
    }
    for (auto &future : futures)
    {
        future.wait();
    }
}


#endif
