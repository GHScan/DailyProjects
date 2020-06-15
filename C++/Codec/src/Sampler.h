#ifndef SAMPLER_H
#define SAMPLER_H

#include "Utils.h"


enum class SampleMethodKind
{
    Nearest,
    Linear,
};

template<typename TDst, typename TSrc>
inline void Sample2D(
    TDst *dst, int dstH, int dstW, size_t dstStride,
    TSrc const *src, int srcH, int srcW, size_t srcStride,
    SampleMethodKind sampleMethod)
{
    assert(!(dstH == srcH && dstW == srcW));

    if (sampleMethod == SampleMethodKind::Nearest)
    {
        for (int dh0 = 0; dh0 < dstH; ++dh0)
        {
            int sh0 = dh0 * srcH / dstH;
            for (int dw0 = 0; dw0 < dstW; ++dw0)
            {
                int sw0 = dw0 * srcW / dstW;
                dst[dh0 * dstStride + dw0] = Convert<TDst>(src[sh0 * srcStride + sw0]);
            }
        }
    }
    else
    {
        assert(false);
    }
}


#endif