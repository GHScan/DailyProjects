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
    uint32_t lx = x & 0xffff; 
    uint32_t hx = x >> 16;
    uint32_t ly = y & 0xffff; 
    uint32_t hy = y >> 16;

    uint32_t ll = lx * ly;
    uint32_t lh = lx * hy;
    uint32_t hl = hx * ly;
    uint32_t hh = hx * hy;

    result[0] = result[1] = 0;

    result[0] += ll & 0xffff;
    uint32_t t = (ll >> 16) + (lh & 0xffff) + (hl & 0xffff);
    result[0] += (t & 0xffff) << 16;
    result[1] += t >> 16;
    result[1] += (lh >> 16) + (hl >> 16) + hh;
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

    puts("mul64_uint64");
    ret = test(mul64_uint64, inputs1, inputs2, N);

    puts("mul64_cpp");
    ret2 = test(mul64_cpp, inputs1, inputs2, N);
    if (ret != ret2) printf("failed !: %f, %f\n", ret, ret2);

    return 0;
}
