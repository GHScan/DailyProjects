#ifndef TEXTURE_H
#define TEXTURE_H


#include <cstdint>

#include <vector>
#include <memory>

#include "Vector.h"



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


struct Texture
{
    std::vector<uint32_t> Data;
    int w, h;

    Texture(int w_, int h_, uint32_t const *data, float gamma)
        : Data(w_ * h_)
        , w (w_)
        , h(h_)
    {
        RAssert(h > 0 && w > 0);

        for (int i = 0; i < w_ * h_; ++i)
        {
            auto clr = DecodeRGBA32(ReverseBytes(data[i]));
            clr = GammaDecode(clr, gamma);
            Data[i] = EncodeRGBA32(clr);
        }
    }

    Vector4 Sample(Vector2 const &uv)
    {
        // WARP mode : REPEAT
        float u = uv.Val[0] - std::floor(uv.Val[0]);
        float v = uv.Val[1] - std::floor(uv.Val[1]);
        float x = u * w;
        float y = v * h;

        // BILINEAR
        int x0 = Clamp(int(std::floor(x)), 0, w - 1);
        int x1 = std::min(x0 + 1, w - 1);
        int y0 = Clamp(int(std::floor(y)), 0, h - 1);
        int y1 = std::min(y0 + 1, h - 1);
        
        float weightX = x - x0;
        float weightY = y - y0;

        auto p = Data.data();

        // decode online to save bandwidth
        Vector4 clrY0 = Lerp(
            DecodeRGBA32(p[y0 * w + x0]),
            DecodeRGBA32(p[y0 * w + x1]),
            weightX);
        Vector4 clrY1 = Lerp(
            DecodeRGBA32(p[y1 * w + x0]),
            DecodeRGBA32(p[y1 * w + x1]),
            weightX);

        return Lerp(clrY0, clrY1, weightY);
    }
};

using TexturePtr = std::shared_ptr<Texture>;




#endif
