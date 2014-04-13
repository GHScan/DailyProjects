
#include <assert.h>
#include <stdint.h>
#include <math.h>

static uint32_t mul(uint32_t a, uint32_t b) {
    uint32_t r = 0;
    for (; b > 0; b >>= 1, a += a) {
        if (b & 1) r += a;
    }
    return r;
}
static uint32_t div(uint32_t a, uint32_t b) {
    uint64_t a64 = a;
    uint64_t b64 = uint64_t(b) << 32;
    uint64_t r64 = 0;
    if (a64 > b64) {
        a64 -= b64;
        r64 = 1;
    }
    for (int i = 0; i < 32; ++i) {
        r64 <<= 1;
        b64 >>= 1;
        if (a64 > b64) {
            a64 -= b64;
            r64 |= 1;
        }
    }
    return (uint32_t)r64;
}
static uint32_t mod(uint32_t a, uint32_t b) {
    uint64_t a64 = a;
    uint64_t b64 = uint64_t(b) << 32;
    if (a64 > b64) {
        a64 -= b64;
    }
    for (int i = 0; i < 32; ++i) {
        b64 >>= 1;
        if (a64 > b64) {
            a64 -= b64;
        }
    }
    return (uint32_t)a64;
}
static uint32_t pow(uint32_t a, uint32_t b) {
    uint32_t r = 1;
    for (; b > 0; b >>= 1, a = mul(a, a)) {
        if (b & 1) r = mul(r, a);
    }
    return r;
}

static void correctnessTest() {
    for (int i = 0; i < 1000000; ++i) {
        uint32_t a = rand(), b = rand();
        assert(a * b == mul(a, b));
        assert(a / b == div(a, b));
        assert(a % b == mod(a, b));

        uint32_t ma = a % 32;
        uint32_t mb = b % 32;
        double fpow = pow(double(ma), double(mb));
        if (fpow >= 0 && fpow <= UINT32_MAX) {
            assert((uint32_t)fpow == pow(ma, mb));
        }
    }
}

int main() {
    srand(time(nullptr));

    correctnessTest();
}
