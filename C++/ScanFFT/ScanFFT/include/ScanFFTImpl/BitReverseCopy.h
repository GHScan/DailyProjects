#ifndef SCANFFTIMPL_BITREVERSECOPY_H
#define SCANFFTIMPL_BITREVERSECOPY_H


#include <ScanFFT_Config.h>
#include <ScanFFT_Utils.h>


#if SCANFFT_SIMD
#include <immintrin.h>
#include <emmintrin.h>
#endif


namespace ScanFFT {


#if !SCANFFT_SIMD


static void SplitEvenOdd(
    Float *destPtr, size_t d0, size_t d1, Float const *srcPtr, size_t s0, size_t s1, size_t n, Float scale) {
    for (size_t i = 0; i < n; ++i)
        destPtr[(i % 2 == 0 ? d0 : d1) + i / 2] = srcPtr[s0 + i] * scale;
    for (size_t i = 0; i < n; ++i)
        destPtr[(i % 2 == 0 ? d0 : d1) + n / 2 + i / 2] = srcPtr[s1 + i] * scale;
}

static void SplitMod4(
    Float *destPtr, size_t d0, size_t d1, size_t d2, size_t d3,
    Float const *srcPtr, size_t s0, size_t s1, size_t s2, size_t s3,
    size_t n, Float scale) {
    size_t destIndices[] = {d0, d2, d1, d3};
    for (size_t i = 0; i < n; ++i)
        destPtr[destIndices[i % 4] + i / 4] = srcPtr[s0 + i] * scale;
    for (size_t i = 0; i < n; ++i)
        destPtr[destIndices[i % 4] + n / 4 + i / 4] = srcPtr[s1 + i] * scale;
    for (size_t i = 0; i < n; ++i)
        destPtr[destIndices[i % 4] + n / 2 + i / 4] = srcPtr[s2 + i] * scale;
    for (size_t i = 0; i < n; ++i)
        destPtr[destIndices[i % 4] + n / 4 * 3 + i / 4] = srcPtr[s3 + i] * scale;
}

static void Transpose(
    Float *destPtr, std::initializer_list<size_t> destRows,
    Float const *srcPtr, std::initializer_list<size_t> srcRows,
    Float scale) {
    auto i = 0;
    for (auto srcRow : srcRows) {
        auto j = 0;
        for (auto destRow : destRows)
            destPtr[destRow + i] = srcPtr[srcRow + j++] * scale;
        ++i;
    }
}


#if SCANFFT_SINGLE_PRECISION_FLOAT


#define CreateScaler_8PS(scale) scale
#define SplitEvenOdd_8PS(destPtr, d0, d1, srcPtr, s0, s1) SplitEvenOdd(destPtr, d0, d1, srcPtr, s0, s1, 8, 1)
#define SplitEvenOdd_Scaled8PS(destPtr, d0, d1, srcPtr, s0, s1, scale) SplitEvenOdd(destPtr, d0, d1, srcPtr, s0, s1, 8, scale)
#define SplitMod4_8PS(destPtr, d0, d1, d2, d3, srcPtr, s0, s1, s2, s3) SplitMod4(destPtr, d0, d1, d2, d3, srcPtr, s0, s1, s2, s3, 8, 1)
#define SplitMod4_Scaled8PS(destPtr, d0, d1, d2, d3, srcPtr, s0, s1, s2, s3, scale) SplitMod4(destPtr, d0, d1, d2, d3, srcPtr, s0, s1, s2, s3, 8, scale)
#define Transpose_8PS(destPtr, d0, d1, d2, d3, d4, d5, d6, d7, srcPtr, s0, s1, s2, s3, s4, s5, s6, s7) Transpose(destPtr, {d0, d1, d2, d3, d4, d5, d6, d7}, srcPtr, {s0, s1, s2, s3, s4, s5, s6, s7}, 1)
#define Transpose_Scaled8PS(destPtr, d0, d1, d2, d3, d4, d5, d6, d7, srcPtr, s0, s1, s2, s3, s4, s5, s6, s7, scale) Transpose(destPtr, {d0, d1, d2, d3, d4, d5, d6, d7}, srcPtr, {s0, s1, s2, s3, s4, s5, s6, s7}, scale)
#include <ScanFFTImpl\Generated\BitReverseCopy_Single.h>


#else // SCANFFT_SINGLE_PRECISION_FLOAT


#define CreateScaler_4PD(scale) scale
#define SplitEvenOdd_4PD(destPtr, d0, d1, srcPtr, s0, s1)  SplitEvenOdd(destPtr, d0, d1, srcPtr, s0, s1, 4, 1)
#define SplitEvenOdd_Scaled4PD(destPtr, d0, d1, srcPtr, s0, s1, scale) SplitEvenOdd(destPtr, d0, d1, srcPtr, s0, s1, 4, scale)
#define Transpose_4PD(destPtr, d0, d1, d2, d3, srcPtr, s0, s1, s2, s3) Transpose(destPtr, {d0, d1, d2, d3}, srcPtr, {s0, s1, s2, s3}, 1)
#define Transpose_Scaled4PD(destPtr, d0, d1, d2, d3, srcPtr, s0, s1, s2, s3, scale) Transpose(destPtr, {d0, d1, d2, d3}, srcPtr, {s0, s1, s2, s3}, scale)
#include <ScanFFTImpl\Generated\BitReverseCopy_Double.h>


#endif


#endif


#if SCANFFT_SIMD && SCANFFT_SINGLE_PRECISION_FLOAT


SCANFFT_FORCEINLINE static __m256 CreateScaler_8PS(float scale) {
    return _mm256_set1_ps(scale);
}


SCANFFT_FORCEINLINE static void SplitEvenOdd_8PS(
    float *destPtr, size_t d0, size_t d1, float const *srcPtr, size_t s0, size_t s1) {
    auto row0 = _mm256_load_ps(srcPtr + s0);
    auto row1 = _mm256_load_ps(srcPtr + s1);
    auto even = _mm256_permutevar8x32_ps(_mm256_shuffle_ps(row0, row1, 0x88), gF32Q2Q3SwapIdx);
    auto odd = _mm256_permutevar8x32_ps(_mm256_shuffle_ps(row0, row1, 0xdd), gF32Q2Q3SwapIdx);
    _mm256_store_ps(destPtr + d0, even);
    _mm256_store_ps(destPtr + d1, odd);
}


SCANFFT_FORCEINLINE static void SplitEvenOdd_Scaled8PS(
    float *destPtr, size_t d0, size_t d1, float const *srcPtr, size_t s0, size_t s1, __m256 scale) {
    auto row0 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s0), scale);
    auto row1 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s1), scale);
    auto even = _mm256_permutevar8x32_ps(_mm256_shuffle_ps(row0, row1, 0x88), gF32Q2Q3SwapIdx);
    auto odd = _mm256_permutevar8x32_ps(_mm256_shuffle_ps(row0, row1, 0xdd), gF32Q2Q3SwapIdx);
    _mm256_store_ps(destPtr + d0, even);
    _mm256_store_ps(destPtr + d1, odd);
}


static auto gMod4PermIndex = _mm256_set_epi32(7, 3, 5, 1, 6, 2, 4, 0);

SCANFFT_FORCEINLINE static void SplitMod4_8PS(
    float *destPtr, size_t d0, size_t d1, size_t d2, size_t d3, 
    float const *srcPtr, size_t s0, size_t s1, size_t s2, size_t s3) {
    auto row0 = _mm256_load_ps(srcPtr + s0);
    auto row1 = _mm256_load_ps(srcPtr + s1);
    auto row2 = _mm256_load_ps(srcPtr + s2);
    auto row3 = _mm256_load_ps(srcPtr + s3);

    auto row01Lo = _mm256_unpacklo_ps(row0, row1);
    auto row01Hi = _mm256_unpackhi_ps(row0, row1);
    auto row23Lo = _mm256_unpacklo_ps(row2, row3);
    auto row23Hi = _mm256_unpackhi_ps(row2, row3);
    auto lololo = _mm256_unpacklo_ps(row01Lo, row23Lo);
    auto lolohi = _mm256_unpackhi_ps(row01Lo, row23Lo);
    auto hihilo = _mm256_unpacklo_ps(row01Hi, row23Hi);
    auto hihihi = _mm256_unpackhi_ps(row01Hi, row23Hi);
    row0 = _mm256_permutevar8x32_ps(lololo, gMod4PermIndex);
    row1 = _mm256_permutevar8x32_ps(hihilo, gMod4PermIndex);
    row2 = _mm256_permutevar8x32_ps(lolohi, gMod4PermIndex);
    row3 = _mm256_permutevar8x32_ps(hihihi, gMod4PermIndex);

    _mm256_store_ps(destPtr + d0, row0);
    _mm256_store_ps(destPtr + d1, row1);
    _mm256_store_ps(destPtr + d2, row2);
    _mm256_store_ps(destPtr + d3, row3);
}


SCANFFT_FORCEINLINE static void SplitMod4_Scaled8PS(
    float *destPtr, size_t d0, size_t d1, size_t d2, size_t d3,
    float const *srcPtr, size_t s0, size_t s1, size_t s2, size_t s3,
    __m256 scale) {
    auto row0 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s0), scale);
    auto row1 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s1), scale);
    auto row2 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s2), scale);
    auto row3 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s3), scale);

    auto row01Lo = _mm256_unpacklo_ps(row0, row1);
    auto row01Hi = _mm256_unpackhi_ps(row0, row1);
    auto row23Lo = _mm256_unpacklo_ps(row2, row3);
    auto row23Hi = _mm256_unpackhi_ps(row2, row3);
    auto lololo = _mm256_unpacklo_ps(row01Lo, row23Lo);
    auto lolohi = _mm256_unpackhi_ps(row01Lo, row23Lo);
    auto hihilo = _mm256_unpacklo_ps(row01Hi, row23Hi);
    auto hihihi = _mm256_unpackhi_ps(row01Hi, row23Hi);
    row0 = _mm256_permutevar8x32_ps(lololo, gMod4PermIndex);
    row1 = _mm256_permutevar8x32_ps(hihilo, gMod4PermIndex);
    row2 = _mm256_permutevar8x32_ps(lolohi, gMod4PermIndex);
    row3 = _mm256_permutevar8x32_ps(hihihi, gMod4PermIndex);

    _mm256_store_ps(destPtr + d0, row0);
    _mm256_store_ps(destPtr + d1, row1);
    _mm256_store_ps(destPtr + d2, row2);
    _mm256_store_ps(destPtr + d3, row3);
}


#define ___MM256_TRANSPOSE8_PS(in0, in1, in2, in3, in4, in5, in6, in7, out0, out1, out2, out3, out4, out5, out6, out7, __in0, __in1, __in2, __in3, __in4, __in5, __in6, __in7, __out0, __out1, __out2, __out3, __out4, __out5, __out6, __out7, __tmp0, __tmp1, __tmp2, __tmp3, __tmp4, __tmp5, __tmp6, __tmp7, __tmpp0, __tmpp1, __tmpp2, __tmpp3, __tmpp4, __tmpp5, __tmpp6, __tmpp7) \
  do { \
    __m256 __in0 = (in0), __in1 = (in1), __in2 = (in2), __in3 = (in3), __in4 = (in4), __in5 = (in5), __in6 = (in6), __in7 = (in7); \
    __m256 __tmp0, __tmp1, __tmp2, __tmp3, __tmp4, __tmp5, __tmp6, __tmp7; \
    __m256 __tmpp0, __tmpp1, __tmpp2, __tmpp3, __tmpp4, __tmpp5, __tmpp6, __tmpp7; \
    __m256 __out0, __out1, __out2, __out3, __out4, __out5, __out6, __out7; \
    __tmp0  = _mm256_unpacklo_ps(__in0, __in1); \
    __tmp1  = _mm256_unpackhi_ps(__in0, __in1); \
    __tmp2  = _mm256_unpacklo_ps(__in2, __in3); \
    __tmp3  = _mm256_unpackhi_ps(__in2, __in3); \
    __tmp4  = _mm256_unpacklo_ps(__in4, __in5); \
    __tmp5  = _mm256_unpackhi_ps(__in4, __in5); \
    __tmp6  = _mm256_unpacklo_ps(__in6, __in7); \
    __tmp7  = _mm256_unpackhi_ps(__in6, __in7); \
    __tmpp0 = _mm256_shuffle_ps(__tmp0, __tmp2, 0x44); \
    __tmpp1 = _mm256_shuffle_ps(__tmp0, __tmp2, 0xEE); \
    __tmpp2 = _mm256_shuffle_ps(__tmp1, __tmp3, 0x44); \
    __tmpp3 = _mm256_shuffle_ps(__tmp1, __tmp3, 0xEE); \
    __tmpp4 = _mm256_shuffle_ps(__tmp4, __tmp6, 0x44); \
    __tmpp5 = _mm256_shuffle_ps(__tmp4, __tmp6, 0xEE); \
    __tmpp6 = _mm256_shuffle_ps(__tmp5, __tmp7, 0x44); \
    __tmpp7 = _mm256_shuffle_ps(__tmp5, __tmp7, 0xEE); \
    __out0  = _mm256_permute2f128_ps(__tmpp0, __tmpp4, 0x20); \
    __out1  = _mm256_permute2f128_ps(__tmpp1, __tmpp5, 0x20); \
    __out2  = _mm256_permute2f128_ps(__tmpp2, __tmpp6, 0x20); \
    __out3  = _mm256_permute2f128_ps(__tmpp3, __tmpp7, 0x20); \
    __out4  = _mm256_permute2f128_ps(__tmpp0, __tmpp4, 0x31); \
    __out5  = _mm256_permute2f128_ps(__tmpp1, __tmpp5, 0x31); \
    __out6  = _mm256_permute2f128_ps(__tmpp2, __tmpp6, 0x31); \
    __out7  = _mm256_permute2f128_ps(__tmpp3, __tmpp7, 0x31); \
    (out0)  = __out0, (out1) = __out1, (out2) = __out2, (out3) = __out3, (out4) = __out4, (out5) = __out5, (out6) = __out6, (out7) = __out7; \
  } while (0)

#define _MM256_TRANSPOSE8_PS(in0, in1, in2, in3, in4, in5, in6, in7) \
    ___MM256_TRANSPOSE8_PS(in0, in1, in2, in3, in4, in5, in6, in7, in0, in1, in2, in3, in4, in5, in6, in7, \
        __in0##__LINE__, __in1##__LINE__, __in2##__LINE__, __in3##__LINE__, __in4##__LINE__, __in5##__LINE__, __in6##__LINE__, __in7##__LINE__, \
        __out0##__LINE__, __out1##__LINE__, __out2##__LINE__, __out3##__LINE__, __out4##__LINE__, __out5##__LINE__, __out6##__LINE__, __out7##__LINE__, \
        __tmp0##__LINE__, __tmp1##__LINE__, __tmp2##__LINE__, __tmp3##__LINE__, __tmp4##__LINE__, __tmp5##__LINE__, __tmp6##__LINE__, __tmp7##__LINE__, \
        __tmpp0##__LINE__, __tmpp1##__LINE__, __tmpp2##__LINE__, __tmpp3##__LINE__, __tmpp4##__LINE__, __tmpp5##__LINE__, __tmpp6##__LINE__, __tmpp7##__LINE__)


SCANFFT_FORCEINLINE static void Transpose_8PS(
    float *destPtr, size_t d0, size_t d1, size_t d2, size_t d3, size_t d4, size_t d5, size_t d6, size_t d7,
    float const *srcPtr, size_t s0, size_t s1, size_t s2, size_t s3, size_t s4, size_t s5, size_t s6, size_t s7) {
    auto row0 = _mm256_load_ps(srcPtr + s0);
    auto row1 = _mm256_load_ps(srcPtr + s1);
    auto row2 = _mm256_load_ps(srcPtr + s2);
    auto row3 = _mm256_load_ps(srcPtr + s3);
    auto row4 = _mm256_load_ps(srcPtr + s4);
    auto row5 = _mm256_load_ps(srcPtr + s5);
    auto row6 = _mm256_load_ps(srcPtr + s6);
    auto row7 = _mm256_load_ps(srcPtr + s7);
    _MM256_TRANSPOSE8_PS(row0, row1, row2, row3, row4, row5, row6, row7);
    _mm256_store_ps(destPtr + d0, row0);
    _mm256_store_ps(destPtr + d1, row1);
    _mm256_store_ps(destPtr + d2, row2);
    _mm256_store_ps(destPtr + d3, row3);
    _mm256_store_ps(destPtr + d4, row4);
    _mm256_store_ps(destPtr + d5, row5);
    _mm256_store_ps(destPtr + d6, row6);
    _mm256_store_ps(destPtr + d7, row7);
}


SCANFFT_FORCEINLINE static void Transpose_Scaled8PS(
    float *destPtr, size_t d0, size_t d1, size_t d2, size_t d3, size_t d4, size_t d5, size_t d6, size_t d7,
    float const *srcPtr, size_t s0, size_t s1, size_t s2, size_t s3, size_t s4, size_t s5, size_t s6, size_t s7,
    __m256 scale) {
    auto row0 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s0), scale);
    auto row1 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s1), scale);
    auto row2 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s2), scale);
    auto row3 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s3), scale);
    auto row4 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s4), scale);
    auto row5 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s5), scale);
    auto row6 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s6), scale);
    auto row7 = _mm256_mul_ps(_mm256_load_ps(srcPtr + s7), scale);
    _MM256_TRANSPOSE8_PS(row0, row1, row2, row3, row4, row5, row6, row7);
    _mm256_store_ps(destPtr + d0, row0);
    _mm256_store_ps(destPtr + d1, row1);
    _mm256_store_ps(destPtr + d2, row2);
    _mm256_store_ps(destPtr + d3, row3);
    _mm256_store_ps(destPtr + d4, row4);
    _mm256_store_ps(destPtr + d5, row5);
    _mm256_store_ps(destPtr + d6, row6);
    _mm256_store_ps(destPtr + d7, row7);
}


#include <ScanFFTImpl\Generated\BitReverseCopy_Single.h>


#endif


#if SCANFFT_SIMD && !SCANFFT_SINGLE_PRECISION_FLOAT


SCANFFT_FORCEINLINE static __m256d CreateScaler_4PD(double scale) {
    return _mm256_set1_pd(scale);
}


SCANFFT_FORCEINLINE static void SplitEvenOdd_4PD(
    double *destPtr, size_t d0, size_t d1, double const *srcPtr, size_t s0, size_t s1) {
    auto row0 = _mm256_load_pd(srcPtr + s0);
    auto row1 = _mm256_load_pd(srcPtr + s1);
    auto even = _mm256_permute4x64_pd(_mm256_unpacklo_pd(row0, row1), 0xd8);
    auto odd = _mm256_permute4x64_pd(_mm256_unpackhi_pd(row0, row1), 0xd8);
    _mm256_store_pd(destPtr + d0, even);
    _mm256_store_pd(destPtr + d1, odd);
}


SCANFFT_FORCEINLINE static void SplitEvenOdd_Scaled4PD(
    double *destPtr, size_t d0, size_t d1, double const *srcPtr, size_t s0, size_t s1, __m256d scale) {
    auto row0 = _mm256_mul_pd(_mm256_load_pd(srcPtr + s0), scale);
    auto row1 = _mm256_mul_pd(_mm256_load_pd(srcPtr + s1), scale);
    auto even = _mm256_permute4x64_pd(_mm256_unpacklo_pd(row0, row1), 0xd8);
    auto odd = _mm256_permute4x64_pd(_mm256_unpackhi_pd(row0, row1), 0xd8);
    _mm256_store_pd(destPtr + d0, even);
    _mm256_store_pd(destPtr + d1, odd);
}


#define ___MM256_TRANSPOSE4_PD(in0, in1, in2, in3, out0, out1, out2, out3, __in0, __in1, __in2, __in3, __out0, __out1, __out2, __out3, __tmp0, __tmp1, __tmp2, __tmp3) \
  do { \
    __m256d __in0 = (in0), __in1 = (in1), __in2 = (in2), __in3 = (in3); \
    __m256d __tmp0, __tmp1, __tmp2, __tmp3; \
    __m256d __out0, __out1, __out2, __out3; \
    __tmp0 = _mm256_shuffle_pd(__in0, __in1, 0x0); \
    __tmp1 = _mm256_shuffle_pd(__in0, __in1, 0xf); \
    __tmp2 = _mm256_shuffle_pd(__in2, __in3, 0x0); \
    __tmp3 = _mm256_shuffle_pd(__in2, __in3, 0xf); \
    __out0 = _mm256_permute2f128_pd(__tmp0, __tmp2, 0x20); \
    __out1 = _mm256_permute2f128_pd(__tmp1, __tmp3, 0x20); \
    __out2 = _mm256_permute2f128_pd(__tmp0, __tmp2, 0x31); \
    __out3 = _mm256_permute2f128_pd(__tmp1, __tmp3, 0x31); \
    (out0) = __out0, (out1) = __out1, (out2) = __out2, (out3) = __out3; \
  } while (0)

#define _MM256_TRANSPOSE4_PD(in0, in1, in2, in3) \
    ___MM256_TRANSPOSE4_PD(in0, in1, in2, in3, in0, in1, in2, in3, \
        __in0##__LINE__, __in1##__LINE__, __in2##__LINE__, __in3##__LINE__, \
        __out0##__LINE__, __out1##__LINE__, __out2##__LINE__, __out3##__LINE__, \
        __tmp0##__LINE__, __tmp1##__LINE__, __tmp2##__LINE__, __tmp3##__LINE__)


SCANFFT_FORCEINLINE static void Transpose_4PD(
    double *destPtr, size_t d0, size_t d1, size_t d2, size_t d3,
    double const *srcPtr, size_t s0, size_t s1, size_t s2, size_t s3) {
    auto row0 = _mm256_load_pd(srcPtr + s0);
    auto row1 = _mm256_load_pd(srcPtr + s1);
    auto row2 = _mm256_load_pd(srcPtr + s2);
    auto row3 = _mm256_load_pd(srcPtr + s3);
    _MM256_TRANSPOSE4_PD(row0, row1, row2, row3);
    _mm256_store_pd(destPtr + d0, row0);
    _mm256_store_pd(destPtr + d1, row1);
    _mm256_store_pd(destPtr + d2, row2);
    _mm256_store_pd(destPtr + d3, row3);
}


SCANFFT_FORCEINLINE static void Transpose_Scaled4PD(
    double *destPtr, size_t d0, size_t d1, size_t d2, size_t d3,
    double const *srcPtr, size_t s0, size_t s1, size_t s2, size_t s3,
    __m256d scale) {
    auto row0 = _mm256_mul_pd(_mm256_load_pd(srcPtr + s0), scale);
    auto row1 = _mm256_mul_pd(_mm256_load_pd(srcPtr + s1), scale);
    auto row2 = _mm256_mul_pd(_mm256_load_pd(srcPtr + s2), scale);
    auto row3 = _mm256_mul_pd(_mm256_load_pd(srcPtr + s3), scale);
    _MM256_TRANSPOSE4_PD(row0, row1, row2, row3);
    _mm256_store_pd(destPtr + d0, row0);
    _mm256_store_pd(destPtr + d1, row1);
    _mm256_store_pd(destPtr + d2, row2);
    _mm256_store_pd(destPtr + d3, row3);
}


#include <ScanFFTImpl\Generated\BitReverseCopy_Double.h>


#endif


}

#endif