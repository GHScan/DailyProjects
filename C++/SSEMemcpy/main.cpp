#include "pch.h" 

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#ifndef __GNUC__
#define __SSE__ 1
#pragma warning(disable : 4311)
#endif

void copy_cpp1(char *dest, const char *src, int n) {
    while (n-- > 0) *dest++ = *src++;
}

void copy_cpp2(char *dest, const char *src, int n) {
    int m;

    m = n / 8;
    n %= 8;
    while (m-- > 0) {
        *((long long*&)dest)++ = *((long long*&)src)++;
    }

    m = n / 4;
    n %= 4;
    while (m-- > 0) {
        *((int*&)dest)++ = *((int*&)src)++;
    }

    while (n-- > 0) *dest++ = *src++;
}

template<int N>
struct SizeN { 
    char buf[N];
    static void copy(char *&dest, const char *&src, int &n) {
        int m = n / N;
        n %= N;
        while (m-- > 0) {
            *((SizeN*&)dest)++ = *((SizeN*&)src)++;
        }
    }
};
void copy_cpp3(char *dest, const char *src, int n) {
    SizeN<16>::copy(dest, src, n);
    SizeN<8>::copy(dest, src, n);
    SizeN<4>::copy(dest, src, n);
    while (n-- > 0) *dest++ = *src++;
}

#ifdef __SSE__
#include <xmmintrin.h>
void copy_xmmintrin(char *dest, const char *src, int n) {
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

#ifdef __GNUC__
void copy_sse_asm(char *dest, const char *src, int n) {
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
#else
void copy_sse_asm(char *dest, const char *src, int n) {
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
        __asm {
            mov ecx, m
            mov esi, src
            mov edi, dest
__loop:
            movaps xmm0, [esi]
            movups [edi], xmm0
            add esi, 16
            add edi, 16
            dec ecx
            jnz __loop
            mov src, esi
            mov dest, edi
        }
    } else {
        __asm {
            mov ecx, m
            mov esi, src
            mov edi, dest
__loop2:
            movaps xmm0, [esi]
            movups [edi], xmm0
            add esi, 16
            add edi, 16
            dec ecx
            jnz __loop2
            mov src, esi
            mov dest, edi
        }
    }

    while (n-- > 0) *dest++ = *src++;
}
#endif

void copy_memcpy(char *dest, const char *src, int n) {
    memcpy(dest, src, n);
}

int benchmark(const char *funcName, void(*f)(char*, const char *, int), int loop, char *dest, char *src, int n) {
    for (int i = 0; i < n; ++i) src[i] = i + (i & 1);
    memset(dest, 0, n);

    clock_t start = clock();
    for (int i = 0; i < loop; ++i) {
        f(dest + (i & 0x7e), src + (i & 0x69), n - 256);
    }
    printf("%s : %f sec\n", funcName, float(clock() - start) / CLOCKS_PER_SEC);

    int ret = 0;
    for (int i = 0; i < n; ++i) ret += dest[i];
    return ret;
}

int main() {
    const int LOOP = 1 << 12;
    const int N = 1 << 20;

    char *dest = (char*)malloc(N), *src = (char*)malloc(N);
    int ret, ret2;

    ret = benchmark("cpp", copy_cpp1, LOOP, dest, src, N);

    ret2 = benchmark("cpp2", copy_cpp2, LOOP, dest, src, N);
    if (ret != ret2) printf("failed: %d, %d\n", ret, ret2);

    ret2 = benchmark("cpp3", copy_cpp3, LOOP, dest, src, N);
    if (ret != ret2) printf("failed: %d, %d\n", ret, ret2);

#ifdef __SSE__
    ret2 = benchmark("xmmintrin", copy_xmmintrin, LOOP, dest, src, N);
    if (ret != ret2) printf("failed: %d, %d\n", ret, ret2);
#endif

    ret2 = benchmark("sse asm", copy_sse_asm, LOOP, dest, src, N);
    if (ret != ret2) printf("failed: %d, %d\n", ret, ret2);

    ret2 = benchmark("memcpy", copy_memcpy, LOOP, dest, src, N);
    if (ret != ret2) printf("failed: %d, %d\n", ret, ret2);
}
