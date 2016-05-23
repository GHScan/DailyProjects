#include "stdafx.h"

// refer to the StringLookup project for better implementation

#undef max
#define EXPORT_API extern "C" __declspec(dllexport)

#include <cstdint>
#include <cassert>
#include <array>
#include <chrono>
#include <iostream>

#include <emmintrin.h>
#include <immintrin.h>

EXPORT_API size_t IndexOf8(uint8_t *array, size_t length, uint8_t value) {
    for (size_t i = 0; i < length; ++i)
        if (array[i] == value) return i;
    return -1;
}

EXPORT_API size_t IndexOf8_SSE2(uint8_t *array, size_t length, uint8_t value) {

    if (length < 16) {
        return IndexOf8(array, length, value);
    }

    size_t i = 0;

    for (size_t count = (16 - reinterpret_cast<ptrdiff_t>(array) & 0xf) & 0xf; i < count; ++i) {
        if (array[i] == value) return i;
    }

    DWORD index;
    auto values = _mm_set1_epi8(value);
    for (; i <= length - 16; i += 16) {
        auto a = _mm_load_si128(reinterpret_cast<const __m128i *>(array + i));
        auto flag = _mm_movemask_epi8(_mm_cmpeq_epi8(a, values));
        if (_BitScanForward(&index, flag)) {
            return i + index;
        }
    }

    for (; i < length; ++i) {
        if (array[i] == value) return i;
    }

    return -1;
}

EXPORT_API size_t IndexOf8_AVX(uint8_t *array, size_t length, uint8_t value) {

    if (length < 32) {
        return IndexOf8(array, length, value);
    }

    size_t i = 0;
    for (size_t count = (32 - reinterpret_cast<ptrdiff_t>(array) & 0x1f) & 0x1f; i < count; ++i) {
        if (array[i] == value) return i;
    }

    DWORD index;
    auto values = _mm256_set1_epi8(value);
    for (; i <= length - 32; i += 32) {
        auto a = _mm256_load_si256(reinterpret_cast<const __m256i *>(array + i));
        auto flag = _mm256_movemask_epi8(_mm256_cmpeq_epi8(a, values));
        if (_BitScanForward(&index, flag)) {
            return i + index;
        }
    }

    for (; i < length; ++i) {
        if (array[i] == value) return i;
    }

    return -1;
}

EXPORT_API void TestIndexOf8() {

    std::array<uint8_t, 1024> array;
    for (size_t i = 0; i < array.size(); ++i)
        array[i] = static_cast<uint8_t>(i);

    for (size_t off = 0; off < array.size(); ++off) {
        for (size_t i = 0; i <= array.size(); ++i) {
            auto v = static_cast<uint8_t>(i);
            auto index = IndexOf8(&array[0] + off, array.size() - off, v);
            assert(index == IndexOf8_SSE2(&array[0] + off, array.size() - off, v));
            assert(index == IndexOf8_AVX(&array[0] + off, array.size() - off, v));
        }
    }

}

template<typename TFunc>
static void Timing(char const *name, int times, TFunc func) {
    if (times > 1) func();

    auto start = std::chrono::high_resolution_clock::now();
    for (auto i = 0; i < times; ++i) {
        func();
    }
    auto end = std::chrono::high_resolution_clock::now();

    std::cout << name << " : " << std::chrono::duration <double>(end - start).count() / times << " s" << std::endl;
}


EXPORT_API void BenchmarkIndexOf8() {
    std::array<uint8_t, 2048> array;
    for (auto i = 0; i < 256; ++i)
        array[i] = 0;
    for (auto i = 128; i < 512; ++i)
        array[i] = 1;
    for (size_t i = 512; i < array.size() - 512; ++i)
        array[i] = static_cast<uint8_t>(i);
    for (auto i = array.size() - 512; i < array.size(); ++i)
        array[i] = 2;

    const int kRepeat = 10;
    volatile size_t sideEffect = 0;

    size_t lens[] = { 1, 3, 5, 10, 20, 30, 45, 60, 100, 200, 300, 500, 1000, array.size() };
    for (auto len : lens) {
        std::cout << "Len - " << len << std::endl;

#define TIME(func) \
Timing("\t" #func, 3, [&]() \
{ \
    size_t counter = 0; \
    for (auto _ = 0; _ < kRepeat; ++_) { \
        for (size_t off = 0; off <= array.size() - len; ++off) { \
            for (size_t v = 0; v <= array.size(); ++v) { \
                counter += func(&array[0], len, static_cast<uint8_t>(v)); \
            } \
        } \
    } \
    sideEffect += counter; \
})

        TIME(IndexOf8);
        TIME(IndexOf8_SSE2);
        TIME(IndexOf8_AVX);

#undef TIME
    }
}