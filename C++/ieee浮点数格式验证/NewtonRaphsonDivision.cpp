#include "stdafx.h"

#include <cstdint>
#include <cstdio>

template<typename TDest, typename TSrc>
inline TDest UnsafeCast(TSrc v) {
    union {
        TSrc src;
        TDest dest;
    } u{ v };
    return u.dest;
}

constexpr uint32_t Log2(uint32_t n) {
    return n == 0 ? 0 : 1 + Log2(n >> 1);
}

static float Divide(float n, float d) {
    uint32_t constexpr s_mask = 1 << 31;
    uint32_t constexpr e_mask = 0xff << 23;
    uint32_t constexpr s_e_mask = s_mask | e_mask;
    uint32_t constexpr e_minus1 = 126 << 23;

    
    uint32_t u_d = UnsafeCast<uint32_t>(d);
    uint32_t u_n = UnsafeCast<uint32_t>(n);
    uint32_t s_d = u_d & s_mask;
    uint32_t e_d = u_d & e_mask;
    uint32_t e_n = u_n & e_mask;
    uint32_t e_shift = e_minus1 - e_d;
    uint32_t e_n2 = e_n + e_shift;


    uint32_t u_n2;
    if (e_n2 & s_mask) {
        u_n2 = e_shift & s_mask
            ? u_n & s_mask ^ s_d                // underflow
            : (u_n & s_mask | e_mask) ^ s_d;    // overflow
    } else {
        u_n2 = (u_n & ~e_mask | e_n2) ^ s_d;
    }
    auto n2 = UnsafeCast<float>(u_n2);
    auto d2 = UnsafeCast<float>(u_d & ~s_e_mask | e_minus1);


    uint32_t constexpr precision = 23;
    uint32_t constexpr steps = Log2((precision + Log2(17)) / Log2(17));

    float x = 48 / 17.0 - 32 / 17.0 * d2;
    for (auto _ = 0; _ < steps; ++_) {
        x = x + x * (1 - d2 * x);
    }


    return n2 * x;
}

int main() {
    float numbers[] = { -3e-22, 2e-10, 0.1, 0.6, 1, 7, 200, 300000, 5e8, -7e20 };
    for (auto a : numbers) {
        for (auto b : numbers) {
            auto c = Divide(a, b);
            auto d = a / b;
            if (c != d) {
                printf("%f/%f->%f (%f)\n", a, b, c, d);
            }
        }
    }
}
