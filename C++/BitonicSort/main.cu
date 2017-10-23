#include <cstdio>
#include <memory>
#include <algorithm>
#include <chrono>
#include <vector>
#include <numeric>
#include <iostream>

#include "device_launch_parameters.h"


#include "CUDAUtils.h"



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


inline bool IsPowerOf2(size_t n) {
    return n > 0 && ((n - 1) & n) == 0;
}


#define USE(v)  do { volatile auto _v2 = v; } while(0)


struct Bitonic_v0 {
    template<typename TIter>
    static void Kernel(TIter first, TIter mid, bool ascent) {
        for (auto p = first, q = mid; p != mid; ++p, ++q) {
            if ((*p > *q) == ascent)
                std::iter_swap(p, q);
        }
    }


    template<typename TIter>
    static void Bitonic_Sort(TIter first, TIter mid, TIter last, bool ascent) {
        if (first + 1 == last) return;

        Kernel(first, mid, ascent);
        Bitonic_Sort(first, (mid - first) / 2 + first, mid, ascent);
        Bitonic_Sort(mid, (last - mid) / 2 + mid, last, ascent);
    }


    template<typename TIter>
    static void Sort(TIter first, TIter last, bool ascent) {
        auto size = last - first;
        assert(IsPowerOf2(size));
        if (size == 1) return;
        auto mid = size / 2 + first;
        
        Sort(first, mid, true);
        Sort(mid, last, false);
        Bitonic_Sort(first, mid, last, ascent);
    }
};


struct Bitonic_v1 {
    template<typename TIter>
    static void Kernel(TIter first, TIter last, size_t span, bool ascent) {
        for (auto p = first; p != last; p += span) {
            for (auto q = p + span; p != q; ++p) {
                if ((p[0] > p[span]) == ascent)
                    std::iter_swap(p, p + span);
            }
        }
    }


    template<typename TIter>
    static void Bitonic_Sort(TIter first, TIter mid, TIter last, bool ascent) {
        for (auto span = mid - first; span >= 1; span /= 2) {
            Kernel(first, last, span, ascent);
        }
    }


    template<typename TIter>
    static void Sort(TIter first, TIter last, bool ascent) {
        auto size = last - first;
        assert(IsPowerOf2(size));
        if (size == 1) return;
        auto mid = size / 2 + first;

        Sort(first, mid, true);
        Sort(mid, last, false);
        Bitonic_Sort(first, mid, last, ascent);
    }
};


struct Bitonic_v2 {
    template<typename T>
    static void Sort(T *p, size_t size, bool ascent) {
        assert(IsPowerOf2(size));

        for (auto dirSpan = 2; dirSpan <= size; dirSpan *= 2) {
            for (auto span = size / 2; span >= 1; span /= 2) {
                for (size_t i = 0; i < size; ++i) {
                    if ((i / span) % 2 == 0) {
                        if ((p[i] > p[i + span]) == ((i / dirSpan) % 2 == 0 ? ascent : !ascent)) {
                            auto v = p[i]; p[i] = p[i + span]; p[i + span] = v;
                        }
                    }
                }
            }
        }
    }
};


template<typename T>
__global__ void Bitonic_v3_Kernel(T *ptr, size_t size, bool ascent) {
    auto tid = threadIdx.x;
    auto win = size / blockDim.x;
    auto first = tid * win;
    auto last = first + win;

    __shared__ T p[4096];
    for (auto i = first; i != last; ++i)
        p[i] = ptr[i];
    __syncthreads();
        

    for (auto dirSpan = 2; dirSpan <= size; dirSpan *= 2) {
        for (auto span = size / 2; span >= 1; span /= 2) {
#pragma unroll
            for (auto i = first; i != last; ++i) {
                if ((i / span) % 2 == 0) {
                    if ((p[i] > p[i + span]) == ((i / dirSpan) % 2 == 0 ? ascent : !ascent)) {
                        auto v = p[i]; p[i] = p[i + span]; p[i + span] = v;
                    }
                }
            }

            __syncthreads();
        }
    }

    for (auto i = first; i != last; ++i)
        ptr[i] = p[i];
}

struct Bitonic_v3 {
    template<typename T>
    static void Sort(std::shared_ptr<CUDAArray<T>> a, bool ascent, size_t maxThread) {
        assert(IsPowerOf2(a->Length));

        auto threadCount = std::min(maxThread, a->Length);
        Bitonic_v3_Kernel << < 1, threadCount >> > (a->Ptr, a->Length, ascent);

        a->Device->CheckLastError();
    }


    template<typename T>
    static void Sort(std::shared_ptr<CUDADevice> device, T *a, size_t size, bool ascent, size_t maxThread = 512) {
        auto da = device->Alloc<T>(size);
        device->Copy(da, a);
        Sort(da, ascent, maxThread);
        device->Copy(a, da);
    }
};


static void Test(std::shared_ptr<CUDADevice> device) {

    for (auto i = 0; i < 12; ++i) {
        std::vector<int> range(1 << i);
        iota(range.begin(), range.end(), 0);
        auto a(range);

        random_shuffle(a.begin(), a.end());
        Bitonic_v0::Sort(a.begin(), a.end(), true);
        assert(std::equal(a.begin(), a.end(), range.begin()));

        random_shuffle(a.begin(), a.end());
        Bitonic_v1::Sort(a.begin(), a.end(), true);
        assert(std::equal(a.begin(), a.end(), range.begin()));

        random_shuffle(a.begin(), a.end());
        Bitonic_v2::Sort(&a[0], a.size(), true);
        assert(std::equal(a.begin(), a.end(), range.begin()));

        random_shuffle(a.begin(), a.end());
        Bitonic_v3::Sort(device, &a[0], a.size(), true);
        assert(std::equal(a.begin(), a.end(), range.begin()));
    }
}


static void Benchmark(std::shared_ptr<CUDADevice> device) {
    constexpr size_t kSize = 1 << 12;
    constexpr size_t kLoop = 100;

    std::vector<int> a(kSize);
    iota(a.begin(), a.end(), 0);
    random_shuffle(a.begin(), a.end());
    {
        auto b(a);

        auto baseline = Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                b.assign(a.begin(), a.end());
                USE(b.front());
            }
        }) / kLoop;


        printf("%-24s=%f\n", "qsort", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                b.assign(a.begin(), a.end());
                sort(b.begin(), b.end());
                USE(b.front());
            }
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_0", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                b.assign(a.begin(), a.end());
                Bitonic_v0::Sort(b.begin(), b.end(), true);
                USE(b.front());
            }
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_1", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                b.assign(a.begin(), a.end());
                Bitonic_v1::Sort(b.begin(), b.end(), true);
                USE(b.front());
            }
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_2", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                b.assign(a.begin(), a.end());
                Bitonic_v2::Sort(&b[0], b.size(), true);
                USE(b.front());
            }
        }) / kLoop - baseline);
    }
    {
        auto da = device->Alloc<int>(kSize);
        device->Copy(da, &a[0]);
        auto db = device->Alloc<int>(kSize);

        auto baseline = Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(db, da);
            }
            device->Synchronize();
        }) / kLoop;

        printf("%-24s=%f\n", "bitonic_3,128", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(db, da);
                Bitonic_v3::Sort(db, true, 128);
            }
            device->Synchronize();
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_3,256", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(db, da);
                Bitonic_v3::Sort(db, true, 256);
            }
            device->Synchronize();
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_3,512", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(db, da);
                Bitonic_v3::Sort(db, true, 512);
            }
            device->Synchronize();
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_3,1024", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(db, da);
                Bitonic_v3::Sort(db, true, 1024);
            }
            device->Synchronize();
        }) / kLoop - baseline);
    }
}


int main() {
    try {
        auto device = std::make_shared<CUDADevice>();

        Test(device);
#ifdef NDEBUG
        Benchmark(device);
#endif
    } catch(std::exception const &e) {
        std::cerr << e.what() << std::endl;
    }


    return 0;
}