#include "pch.h" 

#include <stdint.h>
#include <math.h>
#include <string.h>

static void mul64_uint64(uint32_t result[2], uint32_t x, uint32_t y) {
    uint64_t _x = x;
    _x *= y;
    memcpy(result, &_x, sizeof(_x));
}

static void mul64_cpp(uint32_t result[2], uint32_t x, uint32_t y) {
    uint32_t lx = x & 0xffff, hx = x >> 16;
    uint32_t ly = y & 0xffff, hy = y >> 16;
    result[0] = lx * ly;
    result[1] = hx * hy;
    uint32_t t1 = lx * hy;
    uint32_t t2 = hx * ly + t1;
    uint32_t old_r0 = result[0];
    result[0] += (t2 & 0xffff) << 16;
    result[1] += t2 >> 16;
    if (t2 < t1) {
        result[1] += 1 << 16;
    }
    if (result[0] < old_r0) {
        result[1] += 1;
    }
}

static void mul64_asm(uint32_t result[2], uint32_t x, uint32_t y) {
}

double test(void(*f)(uint32_t[2], uint32_t, uint32_t), uint32_t *inputs1, uint32_t *inputs2, int n) {
    double ret = 0;
    uint64_t out = 0;
    for (int i = 0; i < n; ++i) {
        f((uint32_t*)&out, inputs1[i], inputs2[i]);
        ret += out;
    }
    return ret;
}
void genInputs(uint32_t *inputs, int n) {
    for (int i = 0; i < n; ++i) {
        inputs[i] = (rand() << 16) | rand();
    }
}

int main() {
    const int N = 1 << 10;

    double ret, ret2;

    srand(time(NULL));

    uint32_t *inputs1 = (uint32_t*)malloc(N * sizeof(uint32_t));
    uint32_t *inputs2 = (uint32_t*)malloc(N * sizeof(uint32_t));
    genInputs(inputs1, N);
    genInputs(inputs2, N);

    ret = test(mul64_uint64, inputs1, inputs2, N);

    ret2 = test(mul64_cpp, inputs1, inputs2, N);
    if (ret != ret2) printf("failed !: %f, %f\n", ret, ret2);

    return 0;
}
