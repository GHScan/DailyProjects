// vim: fileencoding=gbk

#ifndef RENDERER_H
#define RENDERER_H

#include "RenderStateEnums.h"

class SceneManager;
struct Plane;

class Renderer
{
public:
    Renderer(const SceneManager* sceneMgr);
    ~Renderer();

    void render(char *buf, int w, int h, int pitch);

    E_CullFace getCullFace() const;
    void setCullFace(E_CullFace face);

    E_TextureFilterType getTextureFilterType() const;
    void setTextureFilterType(E_TextureFilterType t);

    E_TextureAddressingMode getTextureAddressingMode() const;
    void setTextureAddressingMode(E_TextureAddressingMode t);

    E_ZBufferType getZbufferType() const;
    void setZbufferType(E_ZBufferType t);

    E_ShadeMode getShadeMode() const;
    void setShadeMode(E_ShadeMode mode);

    E_ZSortType getZSortType() const;
    void setZSortType(E_ZSortType t);

    E_SpecularLight getSpecularLight() const;
    void setSpecularLight(E_SpecularLight t);

    void addClipPlane(const Plane& p);
    void clearClipPlane();
private:
    class Renderer_Impl *m_impl;
};

#endif // #ifndef RENDERER_H
