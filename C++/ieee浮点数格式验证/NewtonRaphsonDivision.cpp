
#include <cstdint>

template<typename TDest, typename TSrc>
inline TDest UnsafeCast(TSrc v) {
    union {
        TSrc src;
        TDest dest;
    } u {v};
    return u.dest;
}

/*
    Newton–Raphson division:
    https://en.wikipedia.org/wiki/Division_algorithm#Newton.E2.80.93Raphson_division
 */
static float div(float n, float d) {    
    uint32_t e_minus1 = 126 << 23;
    uint32_t e_d = UnsafeCast<uint32_t>(d) & (0xff << 23);
    uint32_t e_n = UnsafeCast<uint32_t>(n) & (0xff << 23);
    uint32_t e_n2 = (e_n + e_minus1 - e_d) & (0xff << 23);

    auto d2 = UnsafeCast<float>((UnsafeCast<uint32_t>(d) & ~(0xff << 23)) | e_minus1);
    auto n2 = UnsafeCast<float>((UnsafeCast<uint32_t>(n) & ~(0xff << 23)) | e_n2);

    float x = 48 / 17.0 - 32 / 17.0 * d2;
    for (auto _ = 0; _ < 3; ++_) {
        x = x + x * (1 - d2 * x);
    }

    return n2 * x;
}

int main() {
    float numbers[] = { 1e-20, 0.1, 0.2, 0.7, 0.9, 1, 2, 4, 9, 300, 50000, 300000, 1e20 };
    for (auto a : numbers) {
        for (auto b : numbers) {
            printf("%f/%f->%f (%f)\n", a, b, div(a, b), a / b);
        }
    }
}
