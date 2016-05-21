// Cpp2015.cpp : 定义控制台应用程序的入口点。
//
#include "stdafx.h"

#include <cassert>
#include <vector>
#include <chrono>
#include <iostream>

#include "MemcpyImpl.h"
#include "MemcpyWyx.h"

static void testMemcpy() {
    std::vector<uint8_t> src(256);
    for (size_t i = 0; i < src.size(); ++i)
        src[i] = static_cast<uint8_t>(i);

    std::vector<uint8_t> dst;
    std::vector<uint8_t> dst2;

    void* (*funcs[])(void*, void const*, size_t) = {
        wyx::memcpy_fast, 
        scan::memcpy_for, 
        scan::memcpy_sse2,
        scan::memcpy_sse2_prefetch,
        scan::memcpy_sse2_prefetch_2line,
        scan::memcpy_sse2_stream,
        scan::memcpy_sse2_fast,
        scan::memcpy_avx_fast,
    };

    for (size_t len = 1; len < src.size(); ++len) {
        for (size_t off = 0; off <= src.size() - len; ++off) {
            dst.assign(len, 0);
            memcpy(&dst[0], &src[off], len);

            for (auto func : funcs) {
                dst2.assign(len, 0);
                func(&dst2[0], &src[off], len);
                assert(std::equal(dst.begin(), dst.end(), dst2.begin()));
            }
        }
    }
}

template <typename TFunc>
static void timing(char const* name, int times, TFunc func) {
    if (times > 1) func();

    auto start = std::chrono::high_resolution_clock::now();
    for (auto i = 0; i < times; ++i) {
        func();
    }
    auto end = std::chrono::high_resolution_clock::now();

    std::cout << name << " : " << std::chrono::duration<double>(end - start).count() / times << " s" << std::endl;
}


static void benchmarkMemcpy() {
    const char* alignStatus[] = {
        "\t\tdst(not aligned), src(not aligned)",
        "\t\tdst(aligned), src(not aligned)" ,
        "\t\tdst(not aligned), src(aligned)" ,
        "\t\tdst(aligned), src(aligned)"
    };
    const size_t alignment = 1 << 7;
    const size_t bufferSize = 1 << 26;

    auto buffer = static_cast<uint8_t*>(_aligned_malloc(bufferSize, alignment));

    std::vector<size_t> lens{ 5, 17, 26, 33, 63, 129, 256, 511, 1024, 4096, 16384, 32768, 262144, 1048576, 4194304, 8388608 };
    int randoms[] = { 13, 2, 11, 6, 4, 8, 7, 15, 9, 5, 10, 3, 14, 12, 0, 1, };

    auto bytesToCopy = (size_t(1) << 28);
    for (auto len : lens) {
        std::cout << "Len - " << len << std::endl;

#define TIME(func) \
        std::cout << "\t" << #func << std::endl; \
        for (auto alignFlag = 0; alignFlag < 4; ++alignFlag) { \
            timing(alignStatus[alignFlag], 3, [&]() { \
                size_t copied = 0; \
                size_t dstOff = 0, srcOff = bufferSize - len; \
                while (copied < bytesToCopy) { \
                    dstOff = dstOff + len + alignment > bufferSize ? 0 : dstOff; \
                    dstOff += (alignFlag & 1) == 1 ? (alignment - (dstOff & (alignment - 1))) : randoms[srcOff & 0xf]; \
                    srcOff = srcOff < len + alignment ? bufferSize - len : srcOff; \
                    srcOff -= (alignFlag & 2) == 2 ? (srcOff & (alignment - 1)) : randoms[dstOff & 0xf]; \
                    func(buffer + dstOff, buffer + srcOff, len); \
                    dstOff += len; \
                    srcOff -= len; \
                    copied += len; \
                } \
            }); \
        }

        TIME(wyx::memcpy_fast);
        TIME(memcpy);
        TIME(scan::memcpy_for);
        TIME(scan::memcpy_sse2);
        TIME(scan::memcpy_sse2_prefetch);
        TIME(scan::memcpy_sse2_prefetch_2line);
        TIME(scan::memcpy_sse2_stream);
        TIME(scan::memcpy_sse2_fast);
        TIME(scan::memcpy_avx_fast);

#undef TIME

    }

    _aligned_free(buffer);
}


int main() {

    testMemcpy();
    benchmarkMemcpy();

    return 0;
}