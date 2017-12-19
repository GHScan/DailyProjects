#include <cassert>
#include <cstdio>
#include <cmath>
#include <cstring>
#include <string>
#include <chrono>
#include <numeric>
#include <immintrin.h>

//------------------------------
constexpr int alignment = 32;

template <typename TFunc>
inline double time_it(TFunc const &f, int loop = 3) {
    using namespace std::chrono;

    if (loop > 1)
        f();

    auto t = 0.0;
    for (auto i = 0; i < loop; ++i) {
        auto start = high_resolution_clock::now();
        f();
        auto end = high_resolution_clock::now();
        t = t + duration<double>(end - start).count();
    }

    return t / loop;
}

static float l2_distance(float const *a, float const *b, int n) {
    float sqr_sum = 0;
    for (auto i = 0; i < n; ++i) {
        auto diff = a[i] - b[i];
        sqr_sum += diff * diff;
    }
    return sqrt(sqr_sum);
}

static void assert_array_equal(float const *a, float const *b, int n, float epsilon=0.01) {
    if (l2_distance(a, b, n) > epsilon) {

        puts("inequal arrays:");
        for (auto v : {a, b}) {
            printf("\t(");
            for (auto i = 0; i < 10; ++i)
                printf("%f,", v[i]);
            printf("...\n");
        }

        assert(0);
    }
}

//------------------------------
static void benchmark_dot(char const *name, int n, float(*fdot)(float const*, float const*, int)) {
    alignas(alignment) float buf[n * 2];
    std::iota(buf, buf + n * 2, 0);
    auto a = buf;
    auto b = buf + n;

    auto loop = 1024 * 1024 * 2;
    float volatile res[256] = {0};
    auto time = time_it([&](){
        for (auto i = 0; i < loop; ++i)
            res[i & 0xff] = fdot(a, b, n);
    });

    auto arith_perf = 2.0 * loop * n * 1E-9 / time;
    auto mem_perf = 8.0 * loop * n * 1E-9 / time;
    printf("%-16s: Arith=%8.3f GFLOPS, L1D Cache=%8.3f GB/S\n", name, arith_perf, mem_perf);
}

static float naive_dot(float const *a, float const *b, int n) {
    float ret = 0;
    for (auto i = 0; i < n; ++i) {
        ret += a[i] * b[i];
    }
    return ret;
}

extern "C" float fma_dot(float const *a, float const *b, int n);

//------------------------------
static void naive_gemm(float const *a, float const *b, float *c, int n) {
    for (auto i = 0; i < n; ++i) {
        for (auto j = 0; j < n; ++j) {
            for (auto k = 0; k < n; ++k) {
                c[i * n + j] += a[i * n + k] * b[k * n + j];
            }
        }
    }
}

extern "C" void fma_gemm48(float const *a, float const *b, float *c);
extern "C" void fma_gemm96(float const *a, float const *b, float *c);
extern "C" void gemm48_gen(float const *a, float const *b, float *c);
extern "C" void gemm96_gen(float const *a, float const *b, float *c);

template<typename TFunc>
void test_gemm(int n, TFunc const &fgemm) {
    for (auto i = 0; i < 4; ++i) {
        alignas(alignment) float buf[n * n * 4] = {0};
        std::iota(buf, buf + n * n * 2, i);
        auto a = buf;
        auto b = buf + n * n;
        auto c1 = buf + n * n * 2;
        auto c2 = buf + n * n * 3;

        naive_gemm(a, b, c1, n);
        fgemm(a, b, c2);
        assert_array_equal(c1, c2, n * n);

        naive_gemm(a, b, c1, n);
        fgemm(a, b, c2);
        assert_array_equal(c1, c2, n * n);
    }
}

template<typename TFunc>
static void benchmark_gemm(char const *name, int n, TFunc const &fgemm) {
    alignas(alignment) float buf[n * n * 3];
    std::iota(buf, buf + n * n * 3, 0);
    auto a = buf;
    auto b = buf + n * n;
    auto c = buf + n * n * 2;

    auto loop = int(1024 * 8 * pow(48.0 / n, 3));
    auto time = time_it([&](){
        for (auto i = 0; i < loop; ++i)
            fgemm(a, b, c);
    });

    auto arith_perf = 2.0 * loop * n * n * n * 1E-9 / time;
    auto mem_perf = 4.0 * loop * n * n * (n + 2) * 1E-9 / time;
    printf("%-16s: Arith=%8.3f GFLOPS, L1D Cache=%8.3f GB/S\n", name, arith_perf, mem_perf);
}

//------------------------------
static void naive_transform_points(float const *points, int n, float const *trans_mat, float *output_points, bool rows) {
    auto m = 4;
    memset(output_points, 0, n * m * sizeof(output_points[0]));

    if (rows) {
        for (auto i = 0; i < n; ++i) {
            for (auto j = 0; j < m; ++j) {
                for (auto k = 0; k < m; ++k) {
                    output_points[i * m + j] += points[i * m + k] * trans_mat[k * m + j];
                }
            }
        }
    } else {
        for (auto i = 0; i < m; ++i) {
            for (auto j = 0; j < n; ++j) {
                for (auto k = 0; k < m; ++k) {
                    output_points[i * n + j] += trans_mat[i * m + k] * points[k * n + j];
                }
            }
        }
    }
}

enum class WorkingSetScale {
    L1,
    L2,
    L3,
    Memory,
};
constexpr int L1_Threshold = 1024 * 2;
constexpr int L2_Threshold = 1024 * 16;
constexpr int L3_Threshold = 1024 * 512;

template<WorkingSetScale working_set>
static void _fma_transform_rows(float const *points, int n, float const *trans_mat, float *output_points) {
    auto r0 = _mm_load_ps(trans_mat + 0);
    auto r1 = _mm_load_ps(trans_mat + 4);
    auto r2 = _mm_load_ps(trans_mat + 8);
    auto r3 = _mm_load_ps(trans_mat + 12);
    for (auto i = 0, lasti = n << 2; i < lasti; i += 32) {
        if (working_set == WorkingSetScale::L1 || working_set == WorkingSetScale::L2) {
        } else if (working_set == WorkingSetScale::L3) {
            _mm_prefetch(&points[i + 32 * 4], _MM_HINT_NTA);
            _mm_prefetch(&points[i + 32 * 4 + 16], _MM_HINT_NTA);
        } else {
            _mm_prefetch(&points[i + 32 * 8], _MM_HINT_NTA);
            _mm_prefetch(&points[i + 32 * 8 + 16], _MM_HINT_NTA);
        }

        auto output0 = _mm_setzero_ps();
        auto output1 = _mm_setzero_ps();
        auto output2 = _mm_setzero_ps();
        auto output3 = _mm_setzero_ps();
        auto output4 = _mm_setzero_ps();
        auto output5 = _mm_setzero_ps();
        auto output6 = _mm_setzero_ps();
        auto output7 = _mm_setzero_ps();
        output0 = _mm_fmadd_ps(_mm_set1_ps(points[i + 0 * 4 + 0]), r0, output0);
        output1 = _mm_fmadd_ps(_mm_set1_ps(points[i + 1 * 4 + 0]), r0, output1);
        output2 = _mm_fmadd_ps(_mm_set1_ps(points[i + 2 * 4 + 0]), r0, output2);
        output3 = _mm_fmadd_ps(_mm_set1_ps(points[i + 3 * 4 + 0]), r0, output3);
        output4 = _mm_fmadd_ps(_mm_set1_ps(points[i + 4 * 4 + 0]), r0, output4);
        output5 = _mm_fmadd_ps(_mm_set1_ps(points[i + 5 * 4 + 0]), r0, output5);
        output6 = _mm_fmadd_ps(_mm_set1_ps(points[i + 6 * 4 + 0]), r0, output6);
        output7 = _mm_fmadd_ps(_mm_set1_ps(points[i + 7 * 4 + 0]), r0, output7);
        output0 = _mm_fmadd_ps(_mm_set1_ps(points[i + 0 * 4 + 1]), r1, output0);
        output1 = _mm_fmadd_ps(_mm_set1_ps(points[i + 1 * 4 + 1]), r1, output1);
        output2 = _mm_fmadd_ps(_mm_set1_ps(points[i + 2 * 4 + 1]), r1, output2);
        output3 = _mm_fmadd_ps(_mm_set1_ps(points[i + 3 * 4 + 1]), r1, output3);
        output4 = _mm_fmadd_ps(_mm_set1_ps(points[i + 4 * 4 + 1]), r1, output4);
        output5 = _mm_fmadd_ps(_mm_set1_ps(points[i + 5 * 4 + 1]), r1, output5);
        output6 = _mm_fmadd_ps(_mm_set1_ps(points[i + 6 * 4 + 1]), r1, output6);
        output7 = _mm_fmadd_ps(_mm_set1_ps(points[i + 7 * 4 + 1]), r1, output7);
        output0 = _mm_fmadd_ps(_mm_set1_ps(points[i + 0 * 4 + 2]), r2, output0);
        output1 = _mm_fmadd_ps(_mm_set1_ps(points[i + 1 * 4 + 2]), r2, output1);
        output2 = _mm_fmadd_ps(_mm_set1_ps(points[i + 2 * 4 + 2]), r2, output2);
        output3 = _mm_fmadd_ps(_mm_set1_ps(points[i + 3 * 4 + 2]), r2, output3);
        output4 = _mm_fmadd_ps(_mm_set1_ps(points[i + 4 * 4 + 2]), r2, output4);
        output5 = _mm_fmadd_ps(_mm_set1_ps(points[i + 5 * 4 + 2]), r2, output5);
        output6 = _mm_fmadd_ps(_mm_set1_ps(points[i + 6 * 4 + 2]), r2, output6);
        output7 = _mm_fmadd_ps(_mm_set1_ps(points[i + 7 * 4 + 2]), r2, output7);
        output0 = _mm_fmadd_ps(_mm_set1_ps(points[i + 0 * 4 + 3]), r3, output0);
        output1 = _mm_fmadd_ps(_mm_set1_ps(points[i + 1 * 4 + 3]), r3, output1);
        output2 = _mm_fmadd_ps(_mm_set1_ps(points[i + 2 * 4 + 3]), r3, output2);
        output3 = _mm_fmadd_ps(_mm_set1_ps(points[i + 3 * 4 + 3]), r3, output3);
        output4 = _mm_fmadd_ps(_mm_set1_ps(points[i + 4 * 4 + 3]), r3, output4);
        output5 = _mm_fmadd_ps(_mm_set1_ps(points[i + 5 * 4 + 3]), r3, output5);
        output6 = _mm_fmadd_ps(_mm_set1_ps(points[i + 6 * 4 + 3]), r3, output6);
        output7 = _mm_fmadd_ps(_mm_set1_ps(points[i + 7 * 4 + 3]), r3, output7);

        if (working_set == WorkingSetScale::L1) {
            _mm_store_ps(&output_points[i + 0 * 4], output0);
            _mm_store_ps(&output_points[i + 1 * 4], output1);
            _mm_store_ps(&output_points[i + 2 * 4], output2);
            _mm_store_ps(&output_points[i + 3 * 4], output3);
            _mm_store_ps(&output_points[i + 4 * 4], output4);
            _mm_store_ps(&output_points[i + 5 * 4], output5);
            _mm_store_ps(&output_points[i + 6 * 4], output6);
            _mm_store_ps(&output_points[i + 7 * 4], output7);
        } else {
            _mm_stream_ps(&output_points[i + 0 * 4], output0);
            _mm_stream_ps(&output_points[i + 1 * 4], output1);
            _mm_stream_ps(&output_points[i + 2 * 4], output2);
            _mm_stream_ps(&output_points[i + 3 * 4], output3);
            _mm_stream_ps(&output_points[i + 4 * 4], output4);
            _mm_stream_ps(&output_points[i + 5 * 4], output5);
            _mm_stream_ps(&output_points[i + 6 * 4], output6);
            _mm_stream_ps(&output_points[i + 7 * 4], output7);
        }
    }

    if (working_set != WorkingSetScale::L1) {
        _mm_sfence();
    }
}
static void fma_transform_rows(float const *points, int n, float const *trans_mat, float *output_points) {
    if (n < L1_Threshold) 
        _fma_transform_rows<WorkingSetScale::L1>(points, n, trans_mat, output_points);
    else if (n < L2_Threshold) 
        _fma_transform_rows<WorkingSetScale::L2>(points, n, trans_mat, output_points);
    else if (n < L3_Threshold) 
        _fma_transform_rows<WorkingSetScale::L3>(points, n, trans_mat, output_points);
    else 
        _fma_transform_rows<WorkingSetScale::Memory>(points, n, trans_mat, output_points);
}

extern "C" void _transform_cols_L1_gen(float const *trans_mat, float const *points, float *output_points, int n);
extern "C" void _transform_cols_L2_gen(float const *trans_mat, float const *points, float *output_points, int n);
extern "C" void _transform_cols_L3_gen(float const *trans_mat, float const *points, float *output_points, int n);
extern "C" void _transform_cols_Memory_gen(float const *trans_mat, float const *points, float *output_points, int n);
static void transform_cols_gen(float const *points, int n, float const *trans_mat, float *output_points) {
    if (n < L1_Threshold) 
        _transform_cols_L1_gen(trans_mat, points, output_points, n);
    else if (n < L2_Threshold) 
        _transform_cols_L2_gen(trans_mat, points, output_points, n);
    else if (n < L3_Threshold) 
        _transform_cols_L3_gen(trans_mat, points, output_points, n);
    else 
        _transform_cols_Memory_gen(trans_mat, points, output_points, n);
}

template<typename TFunc>
static void test_transform_points(TFunc const &ftransform, bool rows) {
    for (auto i = 0; i < 4; ++i) {
        auto n = 96;
        alignas(alignment) float trans_mat[4 * 4];
        alignas(alignment) float points[n * 4];
        alignas(alignment) float output_points1[n * 4];
        alignas(alignment) float output_points2[n * 4];
        std::iota(trans_mat, trans_mat + 4 * 4, i);
        std::iota(points, points + n * 4, i);

        naive_transform_points(points, n, trans_mat, output_points1, rows);
        ftransform(points, n, trans_mat, output_points2);
        assert_array_equal(output_points1, output_points2, n * 4);
    }
}

template<typename TFunc>
static void benchmark_transform_points(char const *name, TFunc const &ftransform) {
    auto cases = 4;
    int n_list[cases] = { 48 * 7, 48 * 7 * 65, 48 * 7 * 513, 48 * 7 * 9 * 1023 };
    char const *n_name_list[cases] = {"L1D Cache", "L2 Cache", "L3 Cache", "Memory"};

    printf("%s:\n", name);
    for (auto i = 0; i < cases; ++i) {
        auto n = n_list[i];
        auto n_name = n_name_list[i];

        alignas(alignment) float trans_mat[4 * 4];
        auto points = (float*)aligned_alloc(alignment, n * 4 * sizeof(float));
        auto output_points = (float*)aligned_alloc(alignment, n * 4 * sizeof(float));
        std::iota(trans_mat, trans_mat + 4 * 4, 0);
        std::iota(points, points + n * 4, 0);

        auto loop = n_list[cases - 1] * 64 / n;
        auto time = time_it([&](){  
            for (auto j = 0; j < loop; ++j) 
                ftransform(points, n, trans_mat, output_points);
        });

        free(points);
        free(output_points);

        auto arith_perf = 2.0 * loop * n * 4 * 4 * 1E-9 / time;
        auto mem_perf = i < cases - 1 
            ? 4.0 * loop * n * 4 * (4 + 1) * 1E-9 / time
            : 4.0 * loop * n * 8 * 1E-9 / time;

        printf("\tArith=%8.3f GFLOPS, %10s=%8.3f GB/S\n", arith_perf, n_name, mem_perf);
    }
}

//------------------------------

int main(int argc, char *argv[]) {
    if (argc == 1) {
        printf("%s dot/gemm48/gemm96/transform/all\n", argv[0]);
        return 1;
    }

    printf("The Peak Performance of Intel Xeon E3 1231 v3(Haswell-WS) + DDR3-1333(x2):\n");
    auto clock_freq = 3.4;
    auto dram_freq = 0.666;
    printf("    SSE - %.1f GFLOPS\n", clock_freq * 2 * 8);
    printf("    AVX - %.1f GFLOPS\n", clock_freq * 2 * 16);
    printf("    L1D Cache - %.1f GB/s\n", clock_freq * 1 * 64);
    printf("    L2  Cache - %.1f GB/s\n", clock_freq * 1 * 29);
    printf("    L3  Cache - %.1f GB/s\n", clock_freq * 1 * 18);
    printf("    Memory - %.1f GB/s\n", dram_freq * 2 * 8 * 2);
    printf("\n");

    for (auto i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "dot" || arg == "all") {
            benchmark_dot("fma_dot", 512, fma_dot);
            benchmark_dot("naive_dot", 512, naive_dot);
        }
        if (arg == "gemm48" || arg == "all") {
            test_gemm(48, fma_gemm48);
            test_gemm(48, gemm48_gen);
            benchmark_gemm("fma_gemm48", 48, fma_gemm48);
            benchmark_gemm("gemm48_gen", 48, gemm48_gen);
            benchmark_gemm("naive_gemm48", 48, [](auto a, auto b, auto c){ naive_gemm(a, b, c, 48); });
        }
        if (arg == "gemm96" || arg == "all") {
            test_gemm(96, fma_gemm96);
            test_gemm(96, gemm96_gen);
            benchmark_gemm("fma_gemm96", 96, fma_gemm96);
            benchmark_gemm("gemm96_gen", 96, gemm96_gen);
            benchmark_gemm("naive_gemm96", 96, [](auto a, auto b, auto c){ naive_gemm(a, b, c, 96); });
        }
        if (arg == "transform" || arg == "all") {
            test_transform_points(fma_transform_rows, true);
            test_transform_points(transform_cols_gen, false);
            benchmark_transform_points("fma_transform_rows", fma_transform_rows);
            benchmark_transform_points("transform_cols_gen", transform_cols_gen);
            benchmark_transform_points("naive_transform_rows", 
                    [](auto points, auto n, auto trans_mat, auto output_points){
                        naive_transform_points(points, n, trans_mat, output_points, true);
                    });
            benchmark_transform_points("naive_transform_cols", 
                    [](auto points, auto n, auto trans_mat, auto output_points){
                        naive_transform_points(points, n, trans_mat, output_points, false);
                    });
        }
    }
}
