#ifndef COLOR_H
#define COLOR_H


#include <cstdint>

#include <immintrin.h>

#include "Vector.h"
#include "Math.h"


inline int32_t EncodeRGBA32(Vector4 const &clr)
{
    auto ivRgba = _mm_cvtps_epi32(_mm_mul_ps(clr.SIMD, _mm_set1_ps(255.f)));
    ivRgba = _mm_shuffle_epi32(ivRgba, _MM_SHUFFLE(0, 1, 2, 3));
    ivRgba = _mm_packs_epi32(ivRgba, ivRgba);
    ivRgba = _mm_packus_epi16(ivRgba, ivRgba);
    return _mm_cvtsi128_si32(ivRgba);
}

inline Vector4 DecodeRGBA32(uint32_t rgba)
{
    auto fvRgba = _mm_cvtepi32_ps(_mm_cvtepu8_epi32(_mm_cvtsi32_si128(rgba)));
    fvRgba = _mm_shuffle_ps(fvRgba, fvRgba, _MM_SHUFFLE(0, 1, 2, 3));
    return Vector4(_mm_mul_ps(fvRgba, _mm_set1_ps(1 / 255.f)));
}



#define COMMON_GAMMA    2.2f

inline Vector4 GammaEncode(Vector4 const &clr, float gamma)
{
    return Vector4(Pow(clr.SIMD, _mm_set1_ps(1 / gamma)));
}

inline Vector4 GammaDecode(Vector4 const &clr, float gamma)
{
    Vector4 res;
    for (int i = 0; i < 4; ++i)
        res.Val[i] = Pow(clr.Val[i], gamma);
    return res;
}

inline Vector4 ExposureToneMapping(Vector4 const &clr, float exposure)
{
    return Vector4(1) - Vector4(Exp(((Vector4(0) - clr) * exposure).SIMD));
}


#endif
