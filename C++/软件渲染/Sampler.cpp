// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>

#include "Sampler.h"
#include "Vector.h"
#include "Texture.h"

struct AddressingMethod_Clamp
{
    void operator () (int &a, int r) const
    {
        if (a < 0) a = 0;
        if (a >= r) a = r - 1;
    }
};
struct AddressingMethod_Repeat
{
    void operator () (int &a, int r) const
    {
        if (a < 0) {
            a = a % r + r - 1;
        }
        else {
            if (a >= r) {
                a %= r;
            }
        }
    }
};
struct AddressingMethod_Mirror
{
    void operator () (int &a, int r) const
    {
        if (a < 0) {
            int b = a % r;
            if ((a / r) & 1) {
                a = -b;
            }
            else a = r - 1 + b;
        }
        else 
        {
            if (a >= r) {
                int b = a % r;
                if ((a / r) & 1) {
                    a = r - 1 - b;
                }
                else a = b;
            }
        }
    }
};
template<typename UAddressingMethod, typename VAddressingMethod>
class PointFilterMethod:
    public ITextureFilterMethod
{
public:
    PointFilterMethod(const float *data, int w, int h): m_data(data), m_w(w), m_h(h){}
    virtual Vector3 sample(const Vector2& uv) const
    {
        assert(m_data != NULL);
        int u = (int)uv.x;
        int v = (int)uv.y;
        m_uAddressingMethod(u, m_w);
        m_vAddressingMethod(v, m_h);
        assert(u >= 0 && u < m_w);
        assert(v >= 0 && v < m_h);
        return Vector3(m_data + (v * m_w + u) * 4 + 1);
    }
    virtual Vector2 textureSize() const
    {
        return Vector2(float(m_w), float(m_h));
    }
private:
    const float *m_data;
    int m_w, m_h;
    UAddressingMethod m_uAddressingMethod;
    VAddressingMethod m_vAddressingMethod;
};
template<typename UAddressingMethod, typename VAddressingMethod>
class BilinearFilterMethod:
    public ITextureFilterMethod
{
public:
    BilinearFilterMethod(const float *data, int w, int h):
        m_w(w), m_h(h), m_data(data) {}
    virtual Vector3 sample(const Vector2& uv) const
    {
        assert(m_data != NULL);
        int _u0 = (int)uv.x, _v0 = (int)uv.y;
        int u0 = _u0, v0 = _v0;
        m_uAddressingMethod(u0, m_w);
        m_vAddressingMethod(v0, m_h);
        assert(u0 >= 0 && u0 < m_w);
        assert(v0 >= 0 && v0 < m_h);

        const float *p00 = m_data + (v0 * m_w + u0) * 4 + 1;
        const float *p10 = p00;
        if (v0 + 1 < m_h) p10 += m_w * 4;
        const float *p01 = p00, *p11 = p10;
        if (u0 + 1 < m_w) p01 += 4, p11 += 4;

        float xRatio = fabs(uv.x - _u0), yRatio = fabs(uv.y - _v0);
        assert(xRatio >= 0 && xRatio <= 1);
        assert(yRatio >= 0 && yRatio <= 1);
        float invXRatio = 1 - xRatio, invYRatio = 1 - yRatio;
        float r00 = invXRatio * invYRatio, r01 = xRatio * invYRatio;
        float r10 = invXRatio * yRatio, r11 = xRatio * yRatio;
        Vector3 r(p00);
        r *= r00;
        r += Vector3(p01) *= r01;
        r += Vector3(p10) *= r10;
        r += Vector3(p11) *= r11;
        return r;
    }
    virtual Vector2 textureSize() const
    {
        return Vector2(float(m_w), float(m_h));
    }
    const float *m_data;
    int m_w, m_h;
    UAddressingMethod m_uAddressingMethod;
    VAddressingMethod m_vAddressingMethod;
};
template<typename UAddressingMethod, typename VAddressingMethod>
class TrilinearFilterMethod:
    public ITextureFilterMethod
{
public:
    TrilinearFilterMethod(const float *data0, const float *data1, int w, int h, float weight0):
        m_data0(data0), m_data1(data1), m_w0(w), m_h0(h), m_weight0(weight0)
    {
        m_w1 = m_w0 >> 1; m_h1 = m_h0 >> 1;
        assert(m_w1 > 0 && m_h1 > 0);
        assert(m_weight0 >= 0 && m_weight0 <= 1);
    }
    virtual Vector3 sample(const Vector2& uv) const
    {
        assert(m_data0 != NULL && m_data1 != NULL);
        int _u0 = (int)uv.x, _v0 = (int)uv.y;
        int u0 = _u0, v0 = _v0;
        m_uAddressingMethod(u0, m_w0);
        m_vAddressingMethod(v0, m_h0);
        assert(u0 >= 0 && u0 < m_w0);
        assert(v0 >= 0 && v0 < m_h0);

        const float *p00 = m_data0 + (v0 * m_w0 + u0) * 4 + 1;
        const float *p10 = p00;
        if (v0 + 1 < m_h0) p10 += m_w0 * 4;
        const float *p01 = p00, *p11 = p10;
        if (u0 + 1 < m_w0) p01 += 4, p11 += 4;

        float xRatio = fabs(uv.x - _u0), yRatio = fabs(uv.y - _v0);
        assert(xRatio >= 0 && xRatio <= 1);
        assert(yRatio >= 0 && yRatio <= 1);
        float invXRatio = 1 - xRatio, invYRatio = 1 - yRatio;
        float r00 = invXRatio * invYRatio, r01 = xRatio * invYRatio;
        float r10 = invXRatio * yRatio, r11 = xRatio * yRatio;

        Vector3 clr0(p00);
        clr0 *= r00;
        clr0 += Vector3(p01) *= r01;
        clr0 += Vector3(p10) *= r10;
        clr0 += Vector3(p11) *= r11;

        u0 >>= 1, v0 >>= 1;
        if (u0 + 1 > m_w1) u0 = m_w1 - 1;
        if (v0 + 1 > m_h1) v0 = m_h1 - 1;
        assert(u0 >= 0 && u0 < m_w1);
        assert(v0 >= 0 && v0 < m_h1);

        p00 = m_data1 + (v0 * m_w1 + u0) * 4 + 1;
        p10 = p00;
        if (v0 + 1 < m_h1) p10 += m_w1 * 4;
        p01 = p00, p11 = p10;
        if (u0 + 1 < m_w1) p01 += 4, p11 += 4;

        Vector3 clr1(p00);
        clr1 *= r00;
        clr1 += Vector3(p01) *= r01;
        clr1 += Vector3(p10) *= r10;
        clr1 += Vector3(p11) *= r11;

        clr0 *= m_weight0;
        clr1 *= 1 - m_weight0;
        return clr0 += clr1;
    }
    virtual Vector2 textureSize() const
    {
        return Vector2(float(m_w0), float(m_h0));
    }
private:
    const float *m_data0, *m_data1;
    int m_w0, m_h0, m_w1, m_h1;
    float m_weight0;
    UAddressingMethod m_uAddressingMethod;
    VAddressingMethod m_vAddressingMethod;
};

class ConstColorFilterMethod:
    public ITextureFilterMethod
{
public:
    ConstColorFilterMethod(const Vector3& clr): m_color(clr) {}
    virtual Vector3 sample(const Vector2& uv) const
    {
        return m_color;
    }
    virtual Vector2 textureSize() const
    {
        return Vector2::ZERO;
    }
private:
    Vector3 m_color;
};

Sampler::Sampler():
    m_tex(NULL), m_minFilterType(ETFT_point), m_maxFilterType(ETFT_point),
    m_uAddressingMode(ETAM_clamp), m_vAddressingMode(ETAM_clamp),
    m_filterMethod(NULL)
{
}
void Sampler::setTexture(const Texture* tex) { m_tex = tex; }
void Sampler::setMinFilterType(E_TextureFilterType t) { m_minFilterType = t; }
void Sampler::setMagFilterType(E_TextureFilterType t) { m_maxFilterType = t; }
void Sampler::setUAddressingMode(E_TextureAddressingMode mode) { m_uAddressingMode = mode; }
void Sampler::setVAddressingMode(E_TextureAddressingMode mode) { m_vAddressingMode = mode; }

Vector2 Sampler::maxTextureSize() const
{
    return Vector2(float(m_tex->width(0)), float(m_tex->height(0)));
}

void Sampler::beginSample(
        const Vector2& p0, const Vector2& uv0,
        const Vector2& p1, const Vector2& uv1,
        const Vector2& p2, const Vector2& uv2)
{
    assert(m_tex != NULL);
    
    static char s_objCacheBuf[512];

    int filterMethodType = 0; // 1-point, 2-bilinear, 3-trilinear, 4-mipmapdbg
    const float *data0 = NULL, *data1 = NULL;
    int w = 0, h = 0;
    float weight0 = 0;

    float level = 0;
    {
        float screenArea = 0;
        {
            Vector2 v0 = p2 - p1;
            Vector2 v1 = p0 - p1;
            screenArea = fabs(v0.x * v1.y - v0.y * v1.x);
        }
        float texArea = 0;
        {
            Vector2 v0 = uv2 - uv1;
            Vector2 v1 = uv0 - uv1;
            texArea = fabs(v0.x * v1.y - v0.y * v1.x);
        }
        float r = fequal(screenArea, 0) ? MAX_FLOAT : texArea / screenArea;
        level = log(r) / log(4.f);
    }

    if (m_tex->hasMipmap()) {
        if (level < 0) { // 放大过滤
            data0 = m_tex->data(0);
            w = m_tex->width(0), h = m_tex->height(0);
            if (m_maxFilterType == ETFT_point) {
                filterMethodType = 1;
            }
            else {
                filterMethodType = 2;
            }
        }
        else { // 缩小过滤
            int maxLevel = m_tex->maxMipmapLevel();
            int ilevel = (int)level;
            if (ilevel >= maxLevel) {
                data0 = m_tex->data(maxLevel);
                w = m_tex->width(maxLevel), h = m_tex->height(maxLevel);
                filterMethodType = 1;
            }
            else {
                data0 = m_tex->data(ilevel);
                w = m_tex->width(ilevel), h = m_tex->height(ilevel);
                switch (m_minFilterType) {
                    case ETFT_point: filterMethodType = 1; break;
                    case ETFT_bilinear: filterMethodType = 2; break;
                    case ETFT_trilinear:
                        {
                            if (fequal(level, float(ilevel))) {
                                filterMethodType = 2;
                            }
                            else {
                                filterMethodType = 3;
                                data1 = m_tex->data(ilevel + 1);
                                weight0 = 1 - (level - ilevel);
                            }
                        }
                        break;
                    case ETFT_mipmapDbg: break;
                    default:
                        assert(0);
                        break;
                }
            }
        }
    }
    else {
        data0 = m_tex->data(0);
        w = m_tex->width(0), h = m_tex->height(0);
        if (level < 0) { // 放大过滤
            filterMethodType = m_maxFilterType == ETFT_point ? 1 : 2;
        }
        else { // 缩小过滤
            filterMethodType = m_minFilterType == ETFT_point ? 1 : 2;
        }
    }

    if (m_minFilterType == ETFT_mipmapDbg) { // 调试mipmap
        filterMethodType = 4;
    }

    if (filterMethodType == 1) {
        if (m_uAddressingMode == ETAM_clamp) {
            if (m_vAddressingMode == ETAM_clamp) {
                m_filterMethod = new (s_objCacheBuf) 
                    PointFilterMethod<AddressingMethod_Clamp, AddressingMethod_Clamp>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_repeat) {
                m_filterMethod = new (s_objCacheBuf) 
                    PointFilterMethod<AddressingMethod_Clamp, AddressingMethod_Repeat>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_mirror) {
                m_filterMethod = new (s_objCacheBuf) 
                    PointFilterMethod<AddressingMethod_Clamp, AddressingMethod_Mirror>(data0, w, h);
            }
            else assert(0);
        }
        else if (m_uAddressingMode == ETAM_repeat) {
            if (m_vAddressingMode == ETAM_clamp) {
                m_filterMethod = new (s_objCacheBuf) 
                    PointFilterMethod<AddressingMethod_Repeat, AddressingMethod_Clamp>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_repeat) {
                m_filterMethod = new (s_objCacheBuf) 
                    PointFilterMethod<AddressingMethod_Repeat, AddressingMethod_Repeat>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_mirror) {
                m_filterMethod = new (s_objCacheBuf) 
                    PointFilterMethod<AddressingMethod_Repeat, AddressingMethod_Mirror>(data0, w, h);
            }
            else assert(0);
        }
        else if (m_uAddressingMode == ETAM_mirror) {
            if (m_vAddressingMode == ETAM_clamp) {
                m_filterMethod = new (s_objCacheBuf) 
                    PointFilterMethod<AddressingMethod_Mirror, AddressingMethod_Clamp>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_repeat) {
                m_filterMethod = new (s_objCacheBuf) 
                    PointFilterMethod<AddressingMethod_Mirror, AddressingMethod_Repeat>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_mirror) {
                m_filterMethod = new (s_objCacheBuf) 
                    PointFilterMethod<AddressingMethod_Mirror, AddressingMethod_Mirror>(data0, w, h);
            }
            else assert(0);
        }
    }
    else if (filterMethodType == 2) {
        if (m_uAddressingMode == ETAM_clamp) {
            if (m_vAddressingMode == ETAM_clamp) {
                m_filterMethod = new (s_objCacheBuf) 
                    BilinearFilterMethod<AddressingMethod_Clamp, AddressingMethod_Clamp>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_repeat) {
                m_filterMethod = new (s_objCacheBuf) 
                    BilinearFilterMethod<AddressingMethod_Clamp, AddressingMethod_Repeat>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_mirror) {
                m_filterMethod = new (s_objCacheBuf) 
                    BilinearFilterMethod<AddressingMethod_Clamp, AddressingMethod_Mirror>(data0, w, h);
            }
            else assert(0);
        }
        else if (m_uAddressingMode == ETAM_repeat) {
            if (m_vAddressingMode == ETAM_clamp) {
                m_filterMethod = new (s_objCacheBuf) 
                    BilinearFilterMethod<AddressingMethod_Repeat, AddressingMethod_Clamp>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_repeat) {
                m_filterMethod = new (s_objCacheBuf) 
                    BilinearFilterMethod<AddressingMethod_Repeat, AddressingMethod_Repeat>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_mirror) {
                m_filterMethod = new (s_objCacheBuf) 
                    BilinearFilterMethod<AddressingMethod_Repeat, AddressingMethod_Mirror>(data0, w, h);
            }
            else assert(0);
        }
        else if (m_uAddressingMode == ETAM_mirror) {
            if (m_vAddressingMode == ETAM_clamp) {
                m_filterMethod = new (s_objCacheBuf) 
                    BilinearFilterMethod<AddressingMethod_Mirror, AddressingMethod_Clamp>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_repeat) {
                m_filterMethod = new (s_objCacheBuf) 
                    BilinearFilterMethod<AddressingMethod_Mirror, AddressingMethod_Repeat>(data0, w, h);
            }
            else if (m_vAddressingMode == ETAM_mirror) {
                m_filterMethod = new (s_objCacheBuf) 
                    BilinearFilterMethod<AddressingMethod_Mirror, AddressingMethod_Mirror>(data0, w, h);
            }
            else assert(0);
        }
    }
    else if (filterMethodType == 3) {
        if (m_uAddressingMode == ETAM_clamp) {
            if (m_vAddressingMode == ETAM_clamp) {
                m_filterMethod = new (s_objCacheBuf) 
                    TrilinearFilterMethod<AddressingMethod_Clamp, AddressingMethod_Clamp>(data0, data1, w, h, weight0);
            }
            else if (m_vAddressingMode == ETAM_repeat) {
                m_filterMethod = new (s_objCacheBuf) 
                    TrilinearFilterMethod<AddressingMethod_Clamp, AddressingMethod_Repeat>(data0, data1, w, h, weight0);
            }
            else if (m_vAddressingMode == ETAM_mirror) {
                m_filterMethod = new (s_objCacheBuf) 
                    TrilinearFilterMethod<AddressingMethod_Clamp, AddressingMethod_Mirror>(data0, data1, w, h, weight0);
            }
            else assert(0);
        }
        else if (m_uAddressingMode == ETAM_repeat) {
            if (m_vAddressingMode == ETAM_clamp) {
                m_filterMethod = new (s_objCacheBuf) 
                    TrilinearFilterMethod<AddressingMethod_Repeat, AddressingMethod_Clamp>(data0, data1, w, h, weight0);
            }
            else if (m_vAddressingMode == ETAM_repeat) {
                m_filterMethod = new (s_objCacheBuf) 
                    TrilinearFilterMethod<AddressingMethod_Repeat, AddressingMethod_Repeat>(data0, data1, w, h, weight0);
            }
            else if (m_vAddressingMode == ETAM_mirror) {
                m_filterMethod = new (s_objCacheBuf) 
                    TrilinearFilterMethod<AddressingMethod_Repeat, AddressingMethod_Mirror>(data0, data1, w, h, weight0);
            }
            else assert(0);
        }
        else if (m_uAddressingMode == ETAM_mirror) {
            if (m_vAddressingMode == ETAM_clamp) {
                m_filterMethod = new (s_objCacheBuf) 
                    TrilinearFilterMethod<AddressingMethod_Mirror, AddressingMethod_Clamp>(data0, data1, w, h, weight0);
            }
            else if (m_vAddressingMode == ETAM_repeat) {
                m_filterMethod = new (s_objCacheBuf) 
                    TrilinearFilterMethod<AddressingMethod_Mirror, AddressingMethod_Repeat>(data0, data1, w, h, weight0);
            }
            else if (m_vAddressingMode == ETAM_mirror) {
                m_filterMethod = new (s_objCacheBuf) 
                    TrilinearFilterMethod<AddressingMethod_Mirror, AddressingMethod_Mirror>(data0, data1, w, h, weight0);
            }
            else assert(0);
        }
    }
    else if (filterMethodType == 4) {
        static const Vector3 constColorTable[] = {
            Vector3(1, 0, 0), Vector3(0, 1, 0), Vector3(0, 0, 1), Vector3(0, 1, 1), 
            Vector3(1, 0, 1), Vector3(1, 1, 0), Vector3(0.5f, 0, 0), Vector3(0, 0.5f, 0), 
            Vector3(0, 0, 0.5f), Vector3(0, 0.5f, 0.5f), Vector3(0.5f, 0, 0.5f), Vector3(0.5f, 0.5f, 0), 
        };
        int idx = std::min(std::max((int)level, 0), arraySize(constColorTable) - 1);
        m_filterMethod = new (s_objCacheBuf) ConstColorFilterMethod(constColorTable[idx]);
    }
    else assert(0);

    assert(m_filterMethod != NULL);
}
Vector2 Sampler::textureSize() const
{
    return m_filterMethod->textureSize();
}
void Sampler::endSample()
{
    m_filterMethod->~ITextureFilterMethod();
    m_filterMethod = NULL;
}
