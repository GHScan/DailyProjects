#include <cassert>
#include <cstdio>
#include <cmath>
#include <string>
#include <chrono>
#include <numeric>

//------------------------------
template <typename TFunc>
inline double time_it(TFunc&& f, int loop = 3) {
    using namespace std::chrono;

    if (loop > 1)
        f();

    auto t = std::numeric_limits<double>::max();
    for (auto i = 0; i < loop; ++i) {
        auto start = high_resolution_clock::now();
        f();
        auto end = high_resolution_clock::now();
        t = std::min(t, duration<double>(end - start).count());
    }

    return t;
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
    alignas(32) float buf[n * 2];
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
    printf("%20s: Arith=%8.3f GFLOPS, Cache=%8.3f GB/S\n", name, arith_perf, mem_perf);
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
void test_gemm(int n, TFunc &&fgemm) {
    alignas(32) float buf[n * n * 4] = {0};
    std::iota(buf, buf + n * n * 2, 0);
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

template<typename TFunc>
static void benchmark_gemm(char const *name, int n, TFunc &&fgemm) {
    alignas(32) float buf[n * n * 3];
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
    printf("%20s: Arith=%8.3f GFLOPS, Cache=%8.3f GB/S\n", name, arith_perf, mem_perf);
}

//------------------------------

int main(int argc, char *argv[]) {
    if (argc == 1) {
        printf("%s dot/gemm48/gemm96/all\n", argv[0]);
        return 1;
    }

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
    }
}
