#include "Utils.h"
#include "ColorSpace.h"

void ConvertRGB2YUV(
    int8_t *dstY, int8_t *dstU, int8_t *dstV,
    uint8_t const *src, size_t srcStride,
    int h, int w)
{
    for (int h0 = 0; h0 < h; ++h0)
    {
        for (int w0 = 0; w0 < w; ++w0)
        {
            float r = src[w0 * 3 + 0];
            float g = src[w0 * 3 + 1];
            float b = src[w0 * 3 + 2];

            float y = 0.299f * r + 0.587f * g + 0.114f * b - 128.f;
            float u = -0.1687f * r - 0.3313f * g + 0.5f * b;
            float v = 0.5f * r - 0.4187f * g - 0.0813f * b;

            *dstY++ = Convert<int8_t>(y);
            *dstU++ = Convert<int8_t>(u);
            *dstV++ = Convert<int8_t>(v);
        }

        src += srcStride;
    }
}

void ConvertYUV2RGB(
    uint8_t *dst, size_t dstStride,
    int8_t const *srcY, int8_t const *srcU, int8_t const *srcV,
    int h, int w)
{
    for (int h0 = 0; h0 < h; ++h0)
    {
        for (int w0 = 0; w0 < w; ++w0)
        {
            float y = *srcY++;
            float u = *srcU++;
            float v = *srcV++;

            y += 128.f;

            float r = y + 1.402f * v;
            float g = y - 0.344f * u - 0.714f * v;
            float b = y + 1.772f * u;

            dst[w0 * 3 + 0] = Convert<uint8_t>(Clamp(r, 0.f, 255.f));
            dst[w0 * 3 + 1] = Convert<uint8_t>(Clamp(g, 0.f, 255.f));
            dst[w0 * 3 + 2] = Convert<uint8_t>(Clamp(b, 0.f, 255.f));
        }

        dst += dstStride;
    }
}
