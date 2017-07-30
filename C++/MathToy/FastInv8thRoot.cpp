#include "stdafx.h"

#include <cmath>
#include <cstdint>
#include <cstdio>

#include <chrono>
#include <iostream>
#include <algorithm>

template <typename TFunc>
static void Timing(char const* name, TFunc func, int times = 3) {
    using namespace std::chrono;

    if (times > 1) func();

    double t = 0;
    for (auto i = 0; i < times; ++i) {
        auto start = high_resolution_clock::now();
        func();
        auto end = high_resolution_clock::now();
        t = std::max(t, duration<double>(end - start).count());
    }

    std::cout << name << " : " << t << " s" << std::endl;
}


static float FastInv8thRoot(float y) {
    int32_t yu = reinterpret_cast<uint32_t&>(y);
    auto xu = 0x47698366U - (yu >> 3);
    auto x = reinterpret_cast<float&>(xu);

    auto x2 = x * x;
    auto x4 = x2 * x2;
    auto x8 = x4 * x4;
    x = x * (9.0f / 8 - x8 * 0.125f * y);

    /*x2 = x * x;
    x4 = x2 * x2;
    x8 = x4 * x4;
    x = x * (9.0 / 8 - x8 * 0.125 * y);*/

    /*x2 = x * x;
    x4 = x2 * x2;
    x8 = x4 * x4;
    x = x * (9.0 / 8 - x8 * 0.125 * y);*/

    return x;
}

/*

                    error(%)        default(s)     /fp:fast     /arch:avx2     /fp:fast/arch:avx2
dummy                               0.00463         0.00493     0.00472         0.00477
sqrt * 4                            0.43175         0.25407     0.35568         0.14386
power                               0.17621         0.17582     0.17635         0.17552
fastinv8throot      0.03~5          0.03257         0.03275     0.03109         0.02796
fastinv8throot+1    0.00005~0.01    0.09523         0.06561     0.09228         0.05649
fastinv8throot+2    0~0.000012      0.17713         0.11211     0.17925         0.09345


*/

int main() {

    float values[] = {9.22481e-5, 7.3285e-4, 5.632e-3, 3.24e-2, 0.1, 0.5, 1, 3, 21, 2.07e2, 5.103e3, 7.3285e4, 8.16325e5};
    for (auto v : values) {
        auto a = pow(v, -1.0f/8);
        auto b = FastInv8thRoot(v);
        auto e = fabs(a - b) / a * 100;
        printf("%f->%.7f%%\n", v, e);
    }


#define  kIterations  (1 << 20)
    Timing("dummy", [&values]()
    {
        for (auto _ = 0; _ < kIterations; ++_) {
            float volatile t = 0;
            for (auto v : values) {
                t = 1;
            }
        }
    });
    Timing("power", [&values]()
    {
        for (auto _ = 0; _ < kIterations; ++_) {
            float volatile t = 0;
            for (auto v : values) {
                t = pow(v, -1.0f/8);
            }
        }
    });
    Timing("sqrt", [&values]()
    {
        for (auto _ = 0; _ < kIterations; ++_) {
            float volatile t = 0;
            for (auto v : values) {
                t = 1.0f / sqrt(sqrt(sqrt(sqrt(v))));
            }
        }
    });
    Timing("fastinv", [&values]()
    {
        for (auto _ = 0; _ < kIterations; ++_) {
            float volatile t = 0;
            for (auto v : values) {
                t = FastInv8thRoot(v);
            }
        }
    });
}
