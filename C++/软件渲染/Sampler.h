// vim: fileencoding=gbk

#ifndef SAMPLER_H
#define SAMPLER_H

#include "RenderStateEnums.h"
#include "Vector.h"

class Texture;

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
    void setUAddressingMode(E_TextureAddressingMode mode);
    void setVAddressingMode(E_TextureAddressingMode mode);

    Vector2 maxTextureSize() const;
    void beginSample(
            const Vector2& p0, const Vector2& uv0,
            const Vector2& p1, const Vector2& uv1,
            const Vector2& p2, const Vector2& vu2);
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
};

inline Vector3 Sampler::sample(const Vector2& uv) const
{
    assert(m_filterMethod != NULL);
    return m_filterMethod->sample(uv);
}
#endif // #ifndef SAMPLER_H
