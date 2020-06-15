#ifndef TEXTURE_H
#define TEXTURE_H


#include <cstdint>
#include <cmath>

#include <vector>
#include <memory>
#include <algorithm>

#define STB_IMAGE_IMPLEMENTATION
#include <stb/stb_image.h>

#include "Color.h"


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

    Vector4 Sample(Vector2 const &uv) const
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




struct Cubemap
{
    TexturePtr Maps[6];

    bool Load(std::string const &dir, float gamma)
    {
        for (int i = 0; i < 6; ++i)
        {
            auto path = dir + "/" + std::to_string(i) + ".jpg";

            int w, h;
            uint8_t *data = stbi_load(path.c_str(), &w, &h, nullptr, 4);
            if (data != nullptr)
            {
                Maps[i] = std::make_shared<Texture>(w, h, (uint32_t*)data, gamma);
                stbi_image_free(data);
            }
            else
            {
                fprintf(stderr, "failed to load : %s\n", path.c_str());
                return false;
            }
        }

        return true;
    }


    float kScale = std::sqrtf(2.f) / 2.f;

    Vector4 Sample(Vector4 const &dir) const
    {
        float xI = std::abs(dir.Val[0]);
        float yI = std::abs(dir.Val[1]);
        float zI = std::abs(dir.Val[2]);
        if (xI > std::max(yI, zI))
        {
            Vector2 uv(dir.Val[2], dir.Val[1]);
            if (dir.Val[0] < 0)
                return Maps[0]->Sample(uv * Vector2(-kScale, -kScale) + 0.5f);
            else
                return Maps[1]->Sample(uv * Vector2(kScale, -kScale) + 0.5f);
        }
        else if (yI > zI)
        {
            Vector2 uv(dir.Val[0], dir.Val[2]);
            if (dir.Val[1] < 0)
                return Maps[2]->Sample(uv * Vector2(kScale, kScale) + 0.5f);
            else
                return Maps[3]->Sample(uv * Vector2(kScale, -kScale) + 0.5f);
        }
        else
        {
            Vector2 uv(dir.Val[0], dir.Val[1]);
            if (dir.Val[2] < 0)
                return Maps[4]->Sample(uv * Vector2(kScale, -kScale) + 0.5f);
            else
                return Maps[5]->Sample(uv * Vector2(-kScale, -kScale) + 0.5f);
        }
    }
};




#endif
