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
inline double Timing(TFunc const &func, int times = 3) {
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


template<typename T>
__device__ inline void Swap(T &a, T& b) {
    auto c = a; 
    a = b;
    b = c;
}


#define MAX_CUDA_THREADS  1024


struct Bitonic_Naive {

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


struct Bitonic_Unroll1 {

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


struct Bitonic_Unroll2 {

    template<typename T>
    static void Sort(T *ptr, size_t size, bool ascent) {
        assert(IsPowerOf2(size));

        for (auto dirSpan = 2; dirSpan <= size; dirSpan *= 2) {
            for (auto span = size / 2; span >= 1; span /= 2) {
                for (size_t i = 0; i < size; ++i) {
                    if ((i / span) % 2 == 0) {
                        auto dir = (i / dirSpan) % 2 == 0 ? ascent : !ascent;
                        if ((ptr[i] > ptr[i + span]) == dir) {
                            std::iter_swap(ptr + i, ptr + i + span);
                        }
                    }
                }
            }
        }
    }
};


template<typename T>
__global__ void Bitonic_GPU1_Kernel(T *ptr, size_t size, bool ascent) {
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
                    auto dir = (i / dirSpan) % 2 == 0 ? ascent : !ascent;
                    if ((localPtr[i] > localPtr[i + span]) == dir) {
                        Swap(localPtr[i], localPtr[i + span]);
                    }
                }
            }

            __syncthreads();
        }
    }

    for (auto i = first; i != last; ++i)
        ptr[i] = localPtr[i];
}

struct Bitonic_GPU1 {

    template<typename T>
    static void Sort(CUDAArrayPtr<T> devPtr, bool ascent, size_t maxThread) {

        assert(IsPowerOf2(devPtr->Length));

        auto threadCount = std::min(maxThread, devPtr->Length);
        Bitonic_GPU1_Kernel << < 1, threadCount >> > (devPtr->Ptr, devPtr->Length, ascent);

        devPtr->Device->CheckLastError();
    }


    template<typename T>
    static void Sort(
        CUDADevicePtr device, 
        T *ptr, size_t size, bool ascent, size_t maxThread = 512) {

        auto devPtr = device->Alloc<T>(size);
        device->Copy(devPtr, ptr);
        Sort(devPtr, ascent, maxThread);
        device->Copy(ptr, devPtr);
    }
};


template<bool ascent, typename T>
__global__ void Bitonic_GPU2_Kernel1(T *ptr, int dirSpan, int span) {
    auto i = blockIdx.x * blockDim.x + threadIdx.x;
    if ((i & span) == 0) {
        auto dir = (i & dirSpan) == 0 ? ascent : !ascent;
        if ((ptr[i] > ptr[i + span]) == dir) {
            Swap(ptr[i], ptr[i + span]);
        }
    }
}

template<bool ascent, typename T>
__global__ void Bitonic_GPU2_Kernel2(T *ptr, int dirSpan, int initSpan) {

    auto i = blockIdx.x * blockDim.x + threadIdx.x;

    __shared__ T localPtr[MAX_CUDA_THREADS];
    localPtr[threadIdx.x] = ptr[i];
    __syncthreads();

    auto dir = (i & dirSpan) == 0 ? ascent : !ascent;
#pragma unroll
    for (auto span = initSpan; span >= 1; span >>= 1) {
        if ((i & span) == 0) {
            if ((localPtr[threadIdx.x] > localPtr[threadIdx.x + span]) == dir) {
                Swap(localPtr[threadIdx.x], localPtr[threadIdx.x + span]);
            }
        }

        __syncthreads();
    }

    ptr[i] = localPtr[threadIdx.x];
}

struct Bitonic_GPU2 {

    template<bool ascent, typename T>
    static void Sort(CUDAArrayPtr<T> devPtr) {
        assert(IsPowerOf2(devPtr->Length));

        auto size = int(devPtr->Length);
        for (auto dirSpan = 2; dirSpan <= size; dirSpan <<= 1) {
            auto span = size >> 1;

            for (; span >= MAX_CUDA_THREADS; span >>= 1) {

                Bitonic_GPU2_Kernel1 
                    <ascent, T> 
                    << < devPtr->Length / MAX_CUDA_THREADS, MAX_CUDA_THREADS >> > 
                    (devPtr->Ptr, dirSpan, span);

                devPtr->Device->CheckLastError();
            }

            {
                auto threadCount = std::min(size, MAX_CUDA_THREADS);
                auto blockCount = size / threadCount;

                Bitonic_GPU2_Kernel2 
                    <ascent, T> 
                    << <blockCount, threadCount >> >
                    (devPtr->Ptr, dirSpan, span);

                devPtr->Device->CheckLastError();
            }
        }
    }


    template<bool ascent, typename T>
    static void Sort(CUDADevicePtr device, T *ptr, size_t size) {
        auto devPtr = device->Alloc<T>(size);
        device->Copy(devPtr, ptr);
        Sort<ascent>(devPtr);
        device->Copy(ptr, devPtr);
    }
};



static void Test(CUDADevicePtr device) {
    for (auto i = 0; i < 12; ++i) {
        std::vector<int> range(1 << i);
        iota(range.begin(), range.end(), 0);
        auto temp(range);

        random_shuffle(temp.begin(), temp.end());
        Bitonic_Naive::Sort(temp.begin(), temp.end(), true);
        assert(std::equal(temp.begin(), temp.end(), range.begin()));

        random_shuffle(temp.begin(), temp.end());
        Bitonic_Unroll1::Sort(temp.begin(), temp.end(), true);
        assert(std::equal(temp.begin(), temp.end(), range.begin()));

        random_shuffle(temp.begin(), temp.end());
        Bitonic_Unroll2::Sort(&temp[0], temp.size(), true);
        assert(std::equal(temp.begin(), temp.end(), range.begin()));

        random_shuffle(temp.begin(), temp.end());
        Bitonic_GPU1::Sort(device, &temp[0], temp.size(), true);
        assert(std::equal(temp.begin(), temp.end(), range.begin()));

        random_shuffle(temp.begin(), temp.end());
        Bitonic_GPU2::Sort<true>(device, &temp[0], temp.size());
        assert(std::equal(temp.begin(), temp.end(), range.begin()));
    }
}


static void Benchmark(CUDADevicePtr device) {
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


        printf("%-24s=%f\n", "std::sort", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                temp.assign(shuffled.begin(), shuffled.end());
                sort(temp.begin(), temp.end());
                USE(temp.front());
            }
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_naive", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                temp.assign(shuffled.begin(), shuffled.end());
                Bitonic_Naive::Sort(temp.begin(), temp.end(), true);
                USE(temp.front());
            }
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_unroll1", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                temp.assign(shuffled.begin(), shuffled.end());
                Bitonic_Unroll1::Sort(temp.begin(), temp.end(), true);
                USE(temp.front());
            }
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_unroll2", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                temp.assign(shuffled.begin(), shuffled.end());
                Bitonic_Unroll2::Sort(&temp[0], temp.size(), true);
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

        printf("%-24s=%f\n", "bitonic_gpu1,128", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(devTemp, devShuffled);
                Bitonic_GPU1::Sort(devTemp, true, 128);
            }
            device->Synchronize();
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_gpu1,1024", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(devTemp, devShuffled);
                Bitonic_GPU1::Sort(devTemp, true, 1024);
            }
            device->Synchronize();
        }) / kLoop - baseline);

        printf("%-24s=%f\n", "bitonic_gpu2", Timing([&]() {
            for (size_t i = 0; i < kLoop; ++i) {
                device->Copy(devTemp, devShuffled);
                Bitonic_GPU2::Sort<true>(devTemp);
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
