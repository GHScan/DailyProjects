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

EXPORT_API size_t IndexOf16(uint16_t* array, size_t length, uint16_t value) {
    for (size_t i = 0; i < length; ++i)
        if (array[i] == value) return i;
    return -1;
}

EXPORT_API size_t IndexOf16_SSE2(uint16_t* array, size_t length, uint16_t value) {

    if (length < 16) {
        return IndexOf16(array, length, value);
    }

    size_t i = 0;

    for (auto count = ((16 - reinterpret_cast<ptrdiff_t>(array) & 0xf) & 0xf) / sizeof(int16_t);
         i < count;
         ++i) {
        if (array[i] == value) return i;
    }

    DWORD index;
    auto values = _mm_set1_epi16(value);
    for (; i <= length - 16; i += 16) {
        auto a = _mm_load_si128(reinterpret_cast<const __m128i *>(array + i));
        auto a2 = _mm_load_si128(reinterpret_cast<const __m128i *>(array + i + 8));
        auto flag = _mm_movemask_epi8(_mm_packs_epi16(_mm_cmpeq_epi16(a, values), _mm_cmpeq_epi16(a2, values)));
        if (_BitScanForward(&index, flag)) {
            return i + index;
        }
    }

    for (; i < length; ++i) {
        if (array[i] == value) return i;
    }

    return -1;
}

EXPORT_API size_t IndexOf16_AVX(uint16_t* array, size_t length, uint16_t value) {
    static uint8_t sIndexMapping[] = {
        0, 1, 2, 3, 4, 5, 6, 7,
        16, 17, 18, 19, 20, 21, 22, 23,
        8, 9, 10, 11, 12, 13, 14, 15,
        24, 25, 26, 27, 28, 29, 30, 31
    };


    if (length < 32) {
        return IndexOf16(array, length, value);
    }

    size_t i = 0;
    for (auto count = ((32 - reinterpret_cast<ptrdiff_t>(array) & 0x1f) & 0x1f) / sizeof(int16_t);
         i < count;
         ++i) {
        if (array[i] == value) return i;
    }

    DWORD index;
    auto values = _mm256_set1_epi16(value);
    for (; i <= length - 32; i += 32) {
        auto a = _mm256_load_si256(reinterpret_cast<const __m256i *>(array + i));
        auto a2 = _mm256_load_si256(reinterpret_cast<const __m256i *>(array + i + 16));
        auto flag = _mm256_movemask_epi8(_mm256_packs_epi16(_mm256_cmpeq_epi16(a, values), _mm256_cmpeq_epi16(a2, values)));
        if (_BitScanForward(&index, flag)) {
            return i + sIndexMapping[index];
        }
    }

    for (; i < length; ++i) {
        if (array[i] == value) return i;
    }

    return -1;
}

EXPORT_API void TestIndexOf16() {

    std::array<uint16_t, 1024> array;
    for (size_t i = 0; i < array.size(); ++i)
        array[i] = static_cast<uint16_t>(i);

    for (size_t off = 0; off < array.size(); ++off) {
        for (size_t i = 0; i <= array.size(); ++i) {
            auto v = static_cast<uint16_t>(i);
            auto index = IndexOf16(&array[0] + off, array.size() - off, v);
            assert(index == IndexOf16_SSE2(&array[0] + off, array.size() - off, v));
            assert(index == IndexOf16_AVX(&array[0] + off, array.size() - off, v));
        }
    }

}

template <typename TFunc>
static void Timing(char const* name, int times, TFunc func) {
    if (times > 1) func();

    auto start = std::chrono::high_resolution_clock::now();
    for (auto i = 0; i < times; ++i) {
        func();
    }
    auto end = std::chrono::high_resolution_clock::now();

    std::cout << name << " : " << std::chrono::duration<double>(end - start).count() / times << " s" << std::endl;
}


EXPORT_API void BenchmarkIndexOf16() {
    std::array<uint16_t, 2048> array;
    for (auto i = 0; i < 256; ++i)
        array[i] = 0;
    for (auto i = 256; i < 512; ++i)
        array[i] = 1;
    for (size_t i = 512; i < array.size() - 512; ++i)
        array[i] = static_cast<uint16_t>(i);
    for (auto i = array.size() - 512; i < array.size(); ++i)
        array[i] = 2;

    const int kRepeat = 10;
    volatile size_t sideEffect = 0;

    size_t lens[] = {1, 3, 5, 10, 20, 30, 45, 60, 100, 200, 300, 500, 1000, array.size()};
    for (auto len : lens) {
        std::cout << "Len - " << len << std::endl;

#define TIME(func) \
Timing("\t" #func, 3, [&]() \
{ \
    size_t counter = 0; \
    for (auto _ = 0; _ < kRepeat; ++_) { \
        for (size_t off = 0; off <= array.size() - len; ++off) { \
            for (size_t v = 0; v <= array.size(); ++v) { \
                counter += func(&array[0], len, static_cast<uint16_t>(v)); \
            } \
        } \
    } \
    sideEffect += counter; \
})

        TIME(IndexOf16);
        TIME(IndexOf16_SSE2);
        TIME(IndexOf16_AVX);

#undef TIME
    }
}