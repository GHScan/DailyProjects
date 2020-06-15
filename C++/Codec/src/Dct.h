#ifndef DCT_H
#define DCT_H

void Dct1D(
    float *dst, size_t dstInc,
    float const *src, size_t srcInc,
    int n);

void IDct1D(
    float *dst, size_t dstInc,
    float const *src, size_t srcInc,
    int n);

void Dct2D(
    float *dst, size_t dstStride,
    float const *src, size_t srcStride,
    int h, int w);

void IDct2D(
    float *dst, size_t dstStride,
    float const *src, size_t srcStride,
    int h, int w);

#endif
