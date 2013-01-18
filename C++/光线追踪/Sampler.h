// vim: fileencoding=gbk

#ifndef SAMPLER_H
#define SAMPLER_H

#include "RenderStateEnums.h"
#include "Vector.h"
#include "Texture.h"

struct ITextureFilterMethod
{
    virtual ~ITextureFilterMethod() = 0 {};
    virtual Vector3 sample(const Vector2& uv) const = 0;
    virtual Vector2 textureSize() const = 0;
};

class Sampler
{
public:
    Sampler();

    void setTexture(const Texture* tex);
    void setMinFilterType(E_TextureFilterType t);
    void setMagFilterType(E_TextureFilterType t);
    void setFilterType(E_TextureFilterType t);
    void setUAddressingMode(E_TextureAddressingMode mode);
    void setVAddressingMode(E_TextureAddressingMode mode);
    void setAddressingMode(E_TextureAddressingMode mode);

    Vector2 maxTextureSize() const;
    void beginSample(
            const Vector2& p0, const Vector2& uv0,
            const Vector2& p1, const Vector2& uv1,
            const Vector2& p2, const Vector2& vu2);
    void beginSample();
    Vector2 textureSize() const;
    Vector3 sample(const Vector2& uv) const;
    void endSample();

private:
    const Texture* m_tex;
    E_TextureFilterType m_minFilterType;
    E_TextureFilterType m_maxFilterType;
    E_TextureAddressingMode m_uAddressingMode;
    E_TextureAddressingMode m_vAddressingMode;

    ITextureFilterMethod *m_filterMethod;
    char m_memBuf[64];
};

inline Vector3 Sampler::sample(const Vector2& uv) const
{
    assert(m_filterMethod != NULL);
    return m_filterMethod->sample(uv);
}

inline void Sampler::setTexture(const Texture* tex) { m_tex = tex; }
inline void Sampler::setMinFilterType(E_TextureFilterType t) { m_minFilterType = t; }
inline void Sampler::setMagFilterType(E_TextureFilterType t) { m_maxFilterType = t; }
inline void Sampler::setUAddressingMode(E_TextureAddressingMode mode) { m_uAddressingMode = mode; }
inline void Sampler::setVAddressingMode(E_TextureAddressingMode mode) { m_vAddressingMode = mode; }

inline Vector2 Sampler::maxTextureSize() const
{
    return Vector2(float(m_tex->width(0)), float(m_tex->height(0)));
}
inline Vector2 Sampler::textureSize() const
{
    return m_filterMethod->textureSize();
}
inline void Sampler::endSample()
{
    m_filterMethod->~ITextureFilterMethod();
    m_filterMethod = NULL;
}
inline void Sampler::setFilterType(E_TextureFilterType t)
{
    setMinFilterType(t);
    setMagFilterType(t);
}
inline void Sampler::setAddressingMode(E_TextureAddressingMode mode)
{
    setUAddressingMode(mode);
    setVAddressingMode(mode);
}
#endif // #ifndef SAMPLER_H
