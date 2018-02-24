#include "stdafx.h"

#include <cassert>
#include <chrono>
#include <vector>

#include <immintrin.h>

template <typename TFunc>
inline double timing(TFunc&& func, int loop = 3) {
    using namespace std::chrono;

    if (loop > 1)
        func();

    double t = 0;
    for (auto i = 0; i < loop; ++i) {
        auto start = high_resolution_clock::now();
        func();
        auto end = high_resolution_clock::now();
        t += duration<double>(end - start).count();
    }

    return t / loop;
}

template<typename TFunc>
inline void timing(char const *name, TFunc const &f, int loop = 3) {
    printf("%30s : %.3f ms\n", name, timing(f, loop) * 1000);
}

#define DO_NOT_OPTIMIZE(a) { volatile float __v = a;}

static float sum8(__m256 x) {
    __m256 t1 = _mm256_hadd_ps(x, x);
    __m256 t2 = _mm256_hadd_ps(t1, t1);
    __m128 t3 = _mm256_extractf128_ps(t2, 1);
    __m128 t4 = _mm_add_ss(_mm256_castps256_ps128(t2), t3);
    return _mm_cvtss_f32(t4);
}

static float dot_naive(float const * __restrict a, float const * __restrict b, size_t len) {
    float sum0 = 0, sum1 = 0, sum2 = 0, sum3 = 0;
    float sum4 = 0, sum5 = 0, sum6 = 0, sum7 = 0;
    for (auto i = 0; i < len; i += 8) {
        sum0 += a[i + 0] * b[i + 0];
        sum1 += a[i + 1] * b[i + 1];
        sum2 += a[i + 2] * b[i + 2];
        sum3 += a[i + 3] * b[i + 3];
        sum4 += a[i + 4] * b[i + 4];
        sum5 += a[i + 5] * b[i + 5];
        sum6 += a[i + 6] * b[i + 6];
        sum7 += a[i + 7] * b[i + 7];
    }
    return sum0 + sum1 + sum2 + sum3 + sum4 + sum5 + sum6 + sum7;
}

static float dot_fma(float const * __restrict a, float const * __restrict b, size_t len) {
    __m256 sum = _mm256_setzero_ps();
    for (auto i = 0; i < len; i += 8) {
        auto av = _mm256_load_ps(&a[i]);
        auto bv = _mm256_load_ps(&b[i]);
        sum = _mm256_fmadd_ps(av, bv, sum);
    }
    return sum8(sum);
}

static float dot_parallel_fma(float const * __restrict a, float const * __restrict b, size_t len) {
    constexpr size_t block_size = 1 << 22;
    const size_t block_count = len / block_size;
    assert(len % block_size == 0);
    float sum = 0;
#pragma omp parallel for reduction(+:sum)
    for (auto i = 0; i < block_count; ++i) {
        auto off = i * block_size;
        sum += dot_fma(&a[off], &b[off], block_size);
    }
    return sum;
}

int main() {
    constexpr int loop = 1;
    constexpr int len = 1 << 27;
    auto a = (float*)_mm_malloc(len * sizeof(float), 64);
    auto b = (float*)_mm_malloc(len * sizeof(float), 64);
    std::fill(a, a + len, 0.f);
    std::fill(b, b + len, 0.f);

    timing("dot_naive", [=]()
    {
        for (auto i = 0; i < loop; ++i)
            DO_NOT_OPTIMIZE(dot_naive(a, b, len));
    });
    timing("dot_fma", [=]()
    {
        for (auto i = 0; i < loop; ++i)
            DO_NOT_OPTIMIZE(dot_fma(a, b, len));
    });
    timing("dot_parallel_fma", [=]()
    {
        for (auto i = 0; i < loop; ++i)
            DO_NOT_OPTIMIZE(dot_parallel_fma(a, b, len));
    });
}
