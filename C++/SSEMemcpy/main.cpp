#include "pch.h" 

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#define TIMING(stmts) { clock_t start = clock(); stmts; printf("%f\n", float(clock() - start) / CLOCKS_PER_SEC);}

void initInputs(char *src, int n) {
    for (int i = 0; i < n; ++i) src[i] = i + (i & 1);
}
int getSumOfOutputs(const char *dest, int n) {
    int ret = 0;
    for (int i = 0; i < n; ++i) ret += dest[i];
    return ret;
}

void copy(char *dest, char *src, int n) {
    while (n-- > 0) *dest++ = *src++;
}

#ifdef __SSE__
#include <xmmintrin.h>
void copy2(char *dest, char *src, int n) {
    if (n < 16) {
        while (n-- > 0) *dest++ = *src++;
        return;
    }

    long m;

    m = 16 - (long)src & 0xf;
    n -= m;
    while (m-- > 0) *dest++ = *src++;

    m = n / 16;
    n %= 16;
    if ((long)dest & 0xf) {
        while (m-- > 0) {
            __m128 t = _mm_load_ps((float*)src);
            _mm_storeu_ps((float*)dest, t);
            src += 16;
            dest += 16;
        }
    } else {
        while (m-- > 0) {
            __m128 t = _mm_load_ps((float*)src);
            _mm_store_ps((float*)dest, t);
            src += 16;
            dest += 16;
        }
    }

    while (n-- > 0) *dest++ = *src++;
}
#endif

void copy3(char *dest, char *src, int n) {
    if (n < 16) {
        while (n-- > 0) *dest++ = *src++;
        return;
    }

    long m;

    m = 16 - (long)src & 0xf;
    n -= m;
    while (m-- > 0) *dest++ = *src++;

    m = n / 16;
    n %= 16;
    if ((long)dest & 0xf) {
        while (m-- > 0) {
            asm(
                "movaps (%0), %%xmm0;"
                "movups %%xmm0, (%1);"
                :
                :"r"(src), "r"(dest)
            );
            src += 16;
            dest += 16;
        }
    } else {
        while (m-- > 0) {
            asm(
                "movaps (%0), %%xmm0;"
                "movaps %%xmm0, (%1);"
                :
                :"r"(src), "r"(dest)
            );
            src += 16;
            dest += 16;
        }
    }

    while (n-- > 0) *dest++ = *src++;
}

void copy4(char *dest, char *src, int n) {
    memcpy(dest, src, n);
}

int main() {
    const int LOOP = 1 << 12;
    const int N = 1 << 20;

    char *dest = (char*)malloc(N), *src = (char*)malloc(N);
    int ret, ret2;

    puts("C++:");
    initInputs(src, N);
    memset(dest, 0, N);
    TIMING(
        for(int i = 0; i < LOOP; ++i) {
            copy(dest, src, N);
        }
    );
    ret = getSumOfOutputs(dest, N);

#ifdef __SSE__
    puts("xmmintrin:");
    initInputs(src, N);
    memset(dest, 0, N);
    TIMING(
        for(int i = 0; i < LOOP; ++i) {
            copy2(dest, src, N);
        }
    );
    ret2 = getSumOfOutputs(dest, N);
    if (ret != ret2) printf("failed: %d, %d\n", ret, ret2);
#endif

    puts("sse asm:");
    initInputs(src, N);
    memset(dest, 0, N);
    TIMING(
        for(int i = 0; i < LOOP; ++i) {
            copy3(dest, src, N);
        }
    );
    ret2 = getSumOfOutputs(dest, N);
    if (ret != ret2) printf("failed: %d, %d\n", ret, ret2);

    puts("memcpy:");
    initInputs(src, N);
    memset(dest, 0, N);
    TIMING(
        for(int i = 0; i < LOOP; ++i) {
            copy4(dest, src, N);
        }
    );
    ret2 = getSumOfOutputs(dest, N);
    if (ret != ret2) printf("failed: %d, %d\n", ret, ret2);
}
