#ifndef COLOR_SPACE
#define COLOR_SPACE

#include <cstdint>

void ConvertRGB2YUV(
    int8_t *dstY, int8_t *dstU, int8_t *dstV,
    uint8_t const *src, size_t srcStride,
    int h, int w);

void ConvertYUV2RGB(
    uint8_t *dst, size_t dstStride,
    int8_t const *srcY, int8_t const *srcU, int8_t const *srcV,
    int h, int w);

#endif