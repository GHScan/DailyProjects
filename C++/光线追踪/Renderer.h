// vim: fileencoding=gbk

#ifndef RENDERER_H
#define RENDERER_H

#include <iostream>

#include "Vector.h"

class SceneManager;
class CameraRayCache;
class Sampler;
struct Ray;

class Renderer
{
public:
    Renderer();
    ~Renderer();
    void render(SceneManager* sceneMgr, char *buf, int w, int h, int pitch);

    void enableSkybox(bool b);
    void enableSpecular(bool b);
    void setMaxTraceDepth(int maxDepth);
    void setShadowLevel(int shadowLvl);
    void setAOLevel(int aoLvl);

    void printStream(std::ostream& fo) const;
    void scanStream(std::istream& fi);

public:
    Vector3 getColorViaRay(
            const Ray& r, SceneManager *sceneMgr, Sampler& sampler, int depth);

private:
    CameraRayCache *m_cameraRayCache;
    bool m_enableSkybox;
    bool m_enableSpecular;
    int m_maxTraceDepth;
    int m_shadowLevel;
    int m_aoLevel;
    float m_shadowRayBias, m_shadowShade;
    float m_refractRayBias;
    float m_aoInnerRadius, m_aoOuterRadius;
    float m_aoRayBias, m_aoShadeBias;
};

#endif // #ifndef RENDERER_H
