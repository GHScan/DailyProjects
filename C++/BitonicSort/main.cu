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


inline bool IsPowerOf2(size_t i) {
    return i > 0 && ((i - 1) & i) == 0;
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
    static void Sort(T *ptr, size_t size, bool ascent) {
        assert(IsPowerOf2(size));

        for (auto dirSpan = 2; dirSpan <= size; dirSpan *= 2) {
            for (auto span = size / 2; span >= 1; span /= 2) {
                for (size_t i = 0; i < size; ++i) {
                    if ((i / span) % 2 == 0) {
                        if ((ptr[i] > ptr[i + span]) == ((i / dirSpan) % 2 == 0 ? ascent : !ascent)) {
                            auto v = ptr[i]; ptr[i] = ptr[i + span]; ptr[i + span] = v;
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
    auto elemCountPerThread = size / blockDim.x;
    auto first = tid * elemCountPerThread;
    auto last = first + elemCountPerThread;

    __shared__ T localPtr[4096];
    for (auto i = first; i != last; ++i)
        localPtr[i] = ptr[i];
    __syncthreads();
        

    for (auto dirSpan = 2; dirSpan <= size; dirSpan *= 2) {
        for (auto span = size / 2; span >= 1; span /= 2) {
#pragma unroll
            for (auto i = first; i != last; ++i) {
                if ((i / span) % 2 == 0) {
                    if ((localPtr[i] > localPtr[i + span]) == ((i / dirSpan) % 2 == 0 ? ascent : !ascent)) {
                        auto v = localPtr[i]; localPtr[i] = localPtr[i + span]; localPtr[i + span] = v;
                    }
                }
            }

            __syncthreads();
        }
    }

    for (auto i = first; i != last; ++i)
        ptr[i] = localPtr[i];
}

struct Bitonic_v3 {
    template<typename T>
    static void Sort(std::shared_ptr<CUDAArray<T>> devPtr, bool ascent, size_t maxThread) {
        assert(IsPowerOf2(devPtr->Length));

        auto threadCount = std::min(maxThread, devPtr->Length);
        Bitonic_v3_Kernel << < 1, threadCount >> > (devPtr->Ptr, devPtr->Length, ascent);

        devPtr->Device->CheckLastError();
    }


    template<typename T>
    static void Sort(std::shared_ptr<CUDADevice> device, T *ptr, size_t size, bool ascent, size_t maxThread = 512) {
        auto devPtr = device->Alloc<T>(size);
        device->Copy(devPtr, ptr);
        Sort(devPtr, ascent, maxThread);
        device->Copy(ptr, devPtr);
    }
};


template<typename T>
__global__ void Bitonic_v4_Kernel(T *ptr, size_t size, size_t dirSpan, size_t span, bool ascent) {
    auto totalThreadCount = gridDim.x * blockDim.x;
    auto elemCountPerThread = totalThreadCount < size ? size / totalThreadCount : 1;
    auto tid = blockIdx.x * blockDim.x + threadIdx.x;
    auto first = tid * elemCountPerThread;
    auto last = first + elemCountPerThread;

    for (auto i = first; i != last; ++i) {
        if (i < size && (i / span) % 2 == 0) {
            if ((ptr[i] > ptr[i + span]) == ((i / dirSpan) % 2 == 0 ? ascent : !ascent)) {
                auto v = ptr[i]; ptr[i] = ptr[i + span]; ptr[i + span] = v;
            }
        }
    }
}

struct Bitonic_v4 {
    template<typename T>
    static void Sort(std::shared_ptr<CUDAArray<T>> devPtr, bool ascent) {
        assert(IsPowerOf2(devPtr->Length));

        size_t threadCount = 1024;
        size_t blockCount = std::max<size_t>(devPtr->Length / threadCount, 1);
        for (auto dirSpan = 2; dirSpan <= devPtr->Length; dirSpan *= 2) {
            for (auto span = devPtr->Length / 2; span >= 1; span /= 2) {
                Bitonic_v4_Kernel << < blockCount, threadCount >> > (devPtr->Ptr, devPtr->Length, dirSpan, span, ascent);
            }
        }

        devPtr->Device->CheckLastError();
    }


    template<typename T>
    static void Sort(std::shared_ptr<CUDADevice> device, T *ptr, size_t size, bool ascent) {
        auto devPtr = device->Alloc<T>(size);
        device->Copy(devPtr, ptr);
        Sort(devPtr, ascent);
        device->Copy(ptr, devPtr);
    }
};


static void Test(std::shared_ptr<CUDADevice> device) {

    for (auto i = 0; i < 12; ++i) {
        std::vector<int> range(1 << i);
        iota(range.begin(), range.end(), 0);
        auto temp(range);

        random_shuffle(temp.begin(), temp.end());
        Bitonic_v0::Sort(temp.begin(), temp.end(), true);
        assert(std::equal(temp.begin(), temp.end(), range.begin()));

        random_shuffle(temp.begin(), temp.end());
        Bitonic_v1::Sort(temp.begin(), temp.end(), true);
        assert(std::equal(temp.begin(), temp.end(), range.begin()));

        random_shuffle(temp.begin(), temp.end());
        Bitonic_v2::Sort(&temp[0], temp.size(), true);
        assert(std::equal(temp.begin(), temp.end(), range.begin()));

        random_shuffle(temp.begin(), temp.end());
        Bitonic_v3::Sort(device, &temp[0], temp.size(), true);
        assert(std::equal(temp.begin(), temp.end(), range.begin()));

        random_shuffle(temp.begin(), temp.end());
        Bitonic_v4::Sort(device, &temp[0], temp.size(), true);
        assert(std::equal(temp.begin(), temp.end(), range.begin()));
    }
}


static void Benchmark(std::shared_ptr<CUDADevice> device) {
    constexpr size_t kSize = 1 << 12;
    constexpr size_t kLoop = 100;

    std::vector<int> shuffled(kSize);
    iota(shuffled.begin(), shuffled.end(), 0);
    random_shuffle(shuffled.begin(), shuffled.end());
    {
        auto temp(shuffled);

        auto baseline = Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                temp.assign(shuffled.begin(), shuffled.end());
                USE(temp.front());
            }
        }) / kLoop;


        printf("%-24s=%f\n", "qsort", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                temp.assign(shuffled.begin(), shuffled.end());
                sort(temp.begin(), temp.end());
                USE(temp.front());
            }
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_0", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                temp.assign(shuffled.begin(), shuffled.end());
                Bitonic_v0::Sort(temp.begin(), temp.end(), true);
                USE(temp.front());
            }
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_1", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                temp.assign(shuffled.begin(), shuffled.end());
                Bitonic_v1::Sort(temp.begin(), temp.end(), true);
                USE(temp.front());
            }
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_2", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                temp.assign(shuffled.begin(), shuffled.end());
                Bitonic_v2::Sort(&temp[0], temp.size(), true);
                USE(temp.front());
            }
        }) / kLoop - baseline);
    }
    {
        auto devShuffled = device->Alloc<int>(kSize);
        device->Copy(devShuffled, &shuffled[0]);
        auto devTemp = device->Alloc<int>(kSize);

        auto baseline = Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(devTemp, devShuffled);
            }
            device->Synchronize();
        }) / kLoop;

        printf("%-24s=%f\n", "bitonic_3,128", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(devTemp, devShuffled);
                Bitonic_v3::Sort(devTemp, true, 128);
            }
            device->Synchronize();
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_3,1024", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(devTemp, devShuffled);
                Bitonic_v3::Sort(devTemp, true, 1024);
            }
            device->Synchronize();
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_4", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(devTemp, devShuffled);
                Bitonic_v4::Sort(devTemp, true);
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