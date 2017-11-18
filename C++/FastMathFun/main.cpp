#include <cassert>
#include <cstdio>
#include <cmath>
#include <vector>
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
static void benchmark_dot(char const *name, float(*fdot)(float const*, float const*, int)) {
    auto n = 512;
    alignas(32) float buffer[n * 2];
    std::iota(buffer, buffer + n * 2, 0);
    auto a = buffer;
    auto b = buffer + n;

    auto loop = 1024 * 1024 * 2;
    std::vector<float> res(256);
    auto time = time_it([&](){
        for (auto i = 0; i < loop; ++i)
            res[i & 0xff] = fdot(a, b, n);
    });

    auto arithPerf = loop * n * 2.0 * 1E-9 / time;
    auto memPerf = loop * n * 8.0 * 1E-9 / time;
    printf("%20s: Arith=%8.3f GFLOPS, Cache=%8.3f GB/S\n", name, arithPerf, memPerf);
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
static void benchmark_gemm48(char const *name, void(*fgemm)(float const*, float const*, float*)) {
    auto n = 48;
    alignas(32) float buffer[n * n * 3];
    std::iota(buffer, buffer + n * n * 3, 0);
    auto a = buffer;
    auto b = buffer + n * n;
    auto c = buffer + n * n * 2;

    auto loop = 1024 * 16;
    auto time = time_it([&](){
        for (auto i = 0; i < loop; ++i)
            fgemm(a, b, c);
    });

    auto arithPerf = loop * n * n * n * 2.0 * 1E-9 / time;
    auto memPerf = loop * n * n * (n + 2) * 4.0 * 1E-9 / time;
    printf("%20s: Arith=%8.3f GFLOPS, Cache=%8.3f GB/S\n", name, arithPerf, memPerf);
}

static void naive_gemm48(float const *a, float const *b, float *c) {
    auto n = 48;
    for (auto i = 0; i < n; ++i) {
        for (auto j = 0; j < n; ++j) {
            for (auto k = 0; k < n; ++k) {
                c[i * n + j] += a[i * n + k] * b[k * n + j];
            }
        }
    }
}

extern "C" void fma_gemm48(float const *a, float const *b, float *c);

void test_gemm48() {
    auto n = 48;
    alignas(32) float buffer[n * n * 4] = {0};
    std::iota(buffer, buffer + n * n * 2, 0);
    auto a = buffer;
    auto b = buffer + n * n;
    auto c1 = buffer + n * n * 2;
    auto c2 = buffer + n * n * 3;

    naive_gemm48(a, b, c1);
    fma_gemm48(a, b, c2);
    assert_array_equal(c1, c2, n * n);

    naive_gemm48(a, b, c1);
    fma_gemm48(a, b, c2);
    assert_array_equal(c1, c2, n * n);
}

//------------------------------

int main(int argc, char *argv[]) {
    if (argc == 1) {
        printf("%s dot/gemm48/all\n", argv[0]);
        return 1;
    }

    for (auto i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "dot" || arg == "all") {
            benchmark_dot("fma_dot", fma_dot);
            benchmark_dot("naive_dot", naive_dot);
        }
        if (arg == "gemm48" || arg == "all") {
            test_gemm48();
            benchmark_gemm48("fma_gemm48", fma_gemm48);
            benchmark_gemm48("naive_gemm48", naive_gemm48);
        }
    }
}
