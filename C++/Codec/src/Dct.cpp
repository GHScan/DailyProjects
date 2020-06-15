#include <cmath>

#include <vector>

#include "Utils.h"
#include "Dct.h"

void Dct1D(
    float *dst, size_t dstInc,
    float const *src, size_t srcInc,
    int n)
{
    for (int i = 0; i < n; ++i)
    {
        float sum = 0;
        for (int j = 0; j < n; ++j)
            sum += src[j * srcInc] * std::cos(kPI * i * (j + 0.5f) / n);

        float factor = std::sqrt(0.5f * (i == 0 ? 0.5f : 1.f) / n);

        dst[i * dstInc] = 2.0f * factor * sum;
    }
}

void IDct1D(
    float *dst, size_t dstInc,
    float const *src, size_t srcInc,
    int n)
{
    for (int i = 0; i < n; ++i)
    {
        float sum = 0;
        for (int j = 1; j < n; ++j)
            sum += src[j * srcInc] * std::cos(kPI * j * (i + 0.5f) / n);

        float factor = std::sqrt(2.f / n);

        dst[i * dstInc] = src[0] / std::sqrt(float(n)) + factor * sum;
    }
}

void Dct2D(
    float *dst, size_t dstStride,
    float const *src, size_t srcStride,
    int h, int w)
{
    size_t tmpStride = w;
    std::vector<float> tmp(h * tmpStride);

    for (int h0 = 0; h0 < h; ++h0)
    {
        Dct1D(tmp.data() + h0 * tmpStride, 1, src + h0 * srcStride, 1, w);
    }
    for (int w0 = 0; w0 < w; ++w0)
    {
        Dct1D(dst + w0, dstStride, tmp.data() + w0, w, h);
    }
}

void IDct2D(
    float *dst, size_t dstStride,
    float const *src, size_t srcStride,
    int h, int w)
{
    size_t tmpStride = w;
    std::vector<float> tmp(h * tmpStride);

    for (int h0 = 0; h0 < h; ++h0)
    {
        IDct1D(tmp.data() + h0 * tmpStride, 1, src + h0 * srcStride, 1, w);
    }
    for (int w0 = 0; w0 < w; ++w0)
    {
        IDct1D(dst + w0, dstStride, tmp.data() + w0, w, h);
    }
}