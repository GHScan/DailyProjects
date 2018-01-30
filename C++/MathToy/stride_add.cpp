#include <cassert>
#include <cstring>
#include <cstdio>
#include <chrono>
#include <numeric>
#include <algorithm>

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

inline bool is_power_of_2(int n) {
    return n > 0 && ((n - 1) & n) == 0;
}

/*
for (i from 0 to n - 1)
    A[i] = A[i] + B[f(i)]

where f(i) is an index mapping function that reads B at a stride of 8. (If for example, B was 16 elements long, then reading it at stride 8 would result in this access
pattern : f(i) = 0, 8, 1, 9, 2, 10, 3, 11, 4, 12, 5, 13, 6, 14, 7, 15).

(change 8 into 16 accordingly)
*/
static void add1(float* a, float* b, int len) {
    assert(is_power_of_2(len));
    auto mask = len - 1;
    auto shift = int(log2(len / 16));
    for (auto i = 0; i < len; ++i) {
        auto j = ((i * 16) & mask) + (i >> shift);
        a[i] += b[j];
    }
}

static void add2(float* a, float* b, int len) {
    assert(is_power_of_2(len));
    assert(len >= 256);

    auto stride = len / 16;
    auto base_idx0 = _mm256_set_epi32(112, 96, 80, 64, 48, 32, 16, 0);
    auto base_idx1 = _mm256_set_epi32(240, 224, 208, 192, 176, 160, 144, 128);
    auto idx_inc = _mm256_set1_epi32(1);
    for (auto b_off = 0, a_off = 0; b_off < len; b_off += 256, a_off += 16) {
        auto idx0 = base_idx0;
        auto idx1 = base_idx1;
        for (auto i = 0; i < 16; ++i) {
            auto av0 = _mm256_load_ps(&a[a_off + stride * i + 0]);
            auto av1 = _mm256_load_ps(&a[a_off + stride * i + 8]);
            auto bv0 = _mm256_i32gather_ps(&b[b_off], idx0, sizeof(float));
            auto bv1 = _mm256_i32gather_ps(&b[b_off], idx1, sizeof(float));
            idx0 = _mm256_add_epi32(idx0, idx_inc);
            idx1 = _mm256_add_epi32(idx1, idx_inc);
            _mm_prefetch((char*)&b[b_off + 256 + i * 16], _MM_HINT_NTA);
            av0 = _mm256_add_ps(av0, bv0);
            av1 = _mm256_add_ps(av1, bv1);
            _mm256_stream_ps(&a[a_off + stride * i + 0], av0);
            _mm256_stream_ps(&a[a_off + stride * i + 8], av1);
        }
    }

    _mm_sfence();
}

int main() {
    auto len = 1 << 23;
    auto a = static_cast<float*>(_mm_malloc(len * sizeof(float), 64));
    auto a2 = static_cast<float*>(_mm_malloc(len * sizeof(float), 64));
    auto b = static_cast<float*>(_mm_malloc(len * sizeof(float), 64));
    memset(a, 0, len * sizeof(float));
    memset(a2, 0, len * sizeof(float));
    std::iota(b, b + len, 0);

    add1(a, b, len);
    add2(a2, b, len);
    assert(std::equal(a, a + len, a2));

    double time;

    time = timing([&]()
    {
        add1(a, b, len);
    });
    printf("add1    :%f ms, %.3f GB/s\n", time * 1000, len * 4 * 3 * 1e-9 / time);

    time = timing([&]()
    {
        add2(a, b, len);
    });
    printf("add2    :%f ms, %.3f GB/s\n", time * 1000, len * 4 * 3 * 1e-9 / time);

    time = timing([&]()
    {
        memcpy(a, b, len * sizeof(float));
    });
    printf("memcpy  :%f ms, %.3f GB/s\n", time * 1000, len * 4 * 2 * 1e-9 / time);
}
