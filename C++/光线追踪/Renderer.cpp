// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>
#include <cmath>

#include <omp.h>

#include "Renderer.h"
#include "SceneManager.h"
#include "Camera.h"
#include "Traceable.h"
#include "Entity.h"
#include "Util.h"
#include "Vector.h"
#include "Light.h"
#include "Sampler.h"
#include "Texture.h"
#include "Serialize.h"

#define USE_OMP 1

static Vector3 getSkyColor(
        const Ray& r, SceneManager* sceneMgr, Sampler& sampler)
{
    Entity* skybox = sceneMgr->getSkybox();
    if (skybox != NULL) {
        float _t = MAX_FLOAT;
        TraceFragment frag;
        if (skybox->getTraceable()->intersect(r, EIC_front, _t, frag)) {
            const Texture * tex = TextureManager::instance()->find(
                    frag.mat->texture.c_str());
            assert(tex != NULL && tex->valid());
            sampler.setTexture(tex);
            sampler.setAddressingMode(ETAM_clamp);
            sampler.setFilterType(ETFT_point);
            sampler.beginSample();
            Vector3 clr = sampler.sample(frag.uv.multiplyInplace(
                        sampler.textureSize()));
            sampler.endSample();
            return clr;
        }
    }
    return Vector3::ZERO;
}

static float getShadowShade(
        int shadowLevel, float shadowShade, float shadowRayBias, 
        const TraceFragment& frag, SceneManager* sceneMgr)
{
    if (shadowLevel < 1) return 1;
    const Light *l = sceneMgr->getLight();
    Ray newRay(frag.pos, l->getLightDirection(frag.pos));
    if (frag.norm.dotProduct(newRay.dir) > 0) {
        newRay.pt = newRay.getPoint(shadowRayBias);
        if (sceneMgr->intersectTest(newRay)) {
            return shadowShade;
        }
    }
    return 1;
}

static void adjustNormalWithBumpMap(
        TraceFragment& frag, Sampler& sampler)
{
    if (frag.mat->bumpTexture.empty()) return;
    const Texture *bumpTex = TextureManager::instance()->find(
            frag.mat->bumpTexture.c_str());
    if (bumpTex == NULL) return;
    if (!bumpTex->valid()) return;

    sampler.setTexture(bumpTex);
    assert(frag.mat->texFilter != ETFT_null);
    sampler.setFilterType(frag.mat->texFilter);
    sampler.setAddressingMode(frag.mat->texAddressMode);
    sampler.beginSample();
    Vector3 tsScale = sampler.sample(
            Vector2(frag.uv).multiplyInplace(sampler.textureSize()));
    sampler.endSample();

    tsScale *= 2;
    tsScale -= Vector3::UNIT_SCALE;
    std::swap(tsScale.y, tsScale.z);

    frag.tangentSpace[0] *= tsScale.x;
    frag.tangentSpace[1] *= tsScale.y;
    frag.tangentSpace[2] *= tsScale.z;
    frag.norm = frag.tangentSpace[0];
    frag.norm += frag.tangentSpace[1];
    frag.norm += frag.tangentSpace[2];
}

static Vector3 getLightColor(
        bool specular, float shadowShade, 
        const TraceFragment& frag, SceneManager* sceneMgr, Sampler& sampler)
{
    const Light* l = sceneMgr->getLight();
    const Texture* tex = NULL;
    if (!frag.mat->texture.empty()) {
        tex = TextureManager::instance()->find(frag.mat->texture.c_str());
        if (!tex->valid()) tex = NULL;
    }
    if (tex != NULL) {
        sampler.setTexture(tex);
        assert(frag.mat->texFilter != ETFT_null);
        sampler.setFilterType(frag.mat->texFilter);
        sampler.setAddressingMode(frag.mat->texAddressMode);
        sampler.beginSample();
    }

    int lightingLevel = frag.mat->lightingLevel;
    if (lightingLevel == 3 && !specular) lightingLevel = 2;
    if (lightingLevel == 3 && shadowShade < 1) lightingLevel = 2;

    Vector3 clr;
    switch (lightingLevel) {
        case 0:
            if (tex == NULL) {
                clr = Vector3::UNIT_SCALE;
            }
            else {
                clr = sampler.sample(
                        Vector2(frag.uv).multiplyInplace(sampler.textureSize()));
            }
            break;
        case 1:
            clr = frag.mat->ambientClr;
            if (tex != NULL) {
                clr.multiplyInplace(
                        sampler.sample(
                            Vector2(frag.uv).multiplyInplace(sampler.textureSize())));
            }
            break;
        case 2:
            clr = l->illuminate(*frag.mat, frag.pos, frag.norm);
            if (tex != NULL) {
                clr.multiplyInplace(
                        sampler.sample(
                            Vector2(frag.uv).multiplyInplace(sampler.textureSize())));
            }
            break;
        case 3:
            if (tex == NULL) {
                clr = l->illuminateWithSpecular(*frag.mat, frag.pos, frag.norm);
            }
            else {
                Vector3 spec(Vector3::ZERO);
                clr = l->illuminateWithSeparateSpecular(
                        *frag.mat, frag.pos, frag.norm, spec);
                clr.multiplyInplace(
                        sampler.sample(
                            Vector2(frag.uv).multiplyInplace(sampler.textureSize())));
                clr += spec;
            }
            break;
        default:
            assert(0);
            break;
    }

    if (tex != NULL) {
        sampler.endSample();
    }
    return clr;
}

static void addReflectAndRefractColor(
        Vector3& clr, Renderer *renderer,
        float refractRayBias, int maxDepth, int depth,
        const Ray& r, SceneManager* sceneMgr, 
        const TraceFragment& frag, ITraceable* tr, Sampler& sampler)
{
    if (depth > maxDepth) return;

    // 反射
    bool reflectClrValid = false;
    Vector3 reflectClr;
    if (frag.mat->reflectible) {
        Ray newRay(frag.pos, r.dir);
        if (reflect(newRay.dir, frag.norm)) {
            reflectClr = renderer->getColorViaRay(newRay, sceneMgr, sampler, depth + 1);
            reflectClrValid = true;
        }
    }

    // 折射
    bool refractClrValid = false;
    Vector3 refractClr;
    if (frag.mat->refractable) {
        Ray newRay(frag.pos, r.dir);
        if (refract(newRay.dir, frag.norm, frag.mat->refractIndex)) {
            newRay.pt = newRay.getPoint(refractRayBias);

            float _t = MAX_FLOAT;
            TraceFragment frag2;
            if (tr->intersect(newRay, EIC_back, _t, frag2)) {
                Ray newRay2(frag2.pos, newRay.dir);
                if (refract(newRay2.dir, -frag2.norm, frag2.mat->_refractInvIndex)) {
                    newRay2.pt = newRay2.getPoint(refractRayBias);
                    refractClr = renderer->getColorViaRay(newRay2, sceneMgr, sampler, depth + 1);
                    refractClrValid = true;
                }
            }
        }
    }

    if (reflectClrValid) {
        if (refractClrValid) {
            float f = (-frag.pos).normalize().dotProduct(frag.norm);
            if (f > 0) {
                f = frag.mat->fresnelBias + pow(1 - f, frag.mat->fresnelPower);
                if (f > 1) f = 1;
                refractClr *= 1 - f;
                reflectClr *= f;
                refractClr += reflectClr;
                clr *= 1 - frag.mat->reflectAndRefractWeight;
                clr += refractClr * frag.mat->reflectAndRefractWeight;
            }
        }
        else {
            clr *= 1 - frag.mat->reflectAndRefractWeight;
            clr += reflectClr * frag.mat->reflectAndRefractWeight;
        }
    }
    else {
        if (refractClrValid) {
            clr *= 1 - frag.mat->reflectAndRefractWeight;
            clr += refractClr * frag.mat->reflectAndRefractWeight;
        }
        else {}
    }
}

static float getAOShade(
        int aoLevel, float aoRayBias, float aoShadeBias, float aoInnerRadius, float aoOuterRadius,
        const TraceFragment& frag, SceneManager* sceneMgr, ITraceable* tr, Entity *ent)
{
    if (aoLevel < 1) return 1;
    assert(frag.norm.isUnit());
    Vector3 x(frag.norm.crossProduct(Vector3::AXIS_Y));
    float xLen = x.length();
    if (xLen > 0) {
        float hit = 0;
        int testCnt = 0;
        x /= xLen;
        Vector3 z(x.crossProduct(frag.norm));
        float step0(PI / (2 * (aoLevel + 1)));
        float angle0 = 0;
        Ray aoRay(frag.pos + frag.norm * aoRayBias, Vector3::AXIS_X);
        for (int i = 0; i < aoLevel; ++i) {
            angle0 += step0;
            float ptY = sin(angle0);
            float radius = cos(angle0);
            float step1 = PI2 / (aoLevel * 4);
            float angle1 = 0;
            for (int j = 0; j < aoLevel * 4; ++j) {
                angle1 += step1;
                float ptX = cos(angle1) * radius;
                float ptZ = sin(angle1) * radius;
                aoRay.dir = x;
                aoRay.dir *= ptX;
                aoRay.dir += Vector3(frag.norm) *= ptY;
                aoRay.dir += Vector3(z) *= ptZ;
                assert(aoRay.dir.isUnit() && aoRay.dir.dotProduct(frag.norm) > 0);
                ++testCnt;
                // 先检测frag所在的物体，再检测场景
                float radius = aoOuterRadius;
                if (tr->intersectSimply(aoRay, EIC_front, radius)) {
                    hit += smoothStep(aoOuterRadius, aoInnerRadius, radius);
                }
                else {
                    if (sceneMgr->intersectSimply(aoRay, EIC_front, radius, ent)) {
                        hit += smoothStep(aoOuterRadius, aoInnerRadius, radius);
                    }
                }
            }
        }

        // 测试法线
        {
            ++testCnt;
            Ray aoRay(frag.pos, frag.norm);
            aoRay.pt = aoRay.getPoint(aoRayBias);
            // 先检测frag所在的物体，再检测场景
            float radius = aoOuterRadius;
            if (tr->intersectSimply(aoRay, EIC_front, radius)) {
                hit += smoothStep(aoOuterRadius, aoInnerRadius, radius);
            }
            else {
                if (sceneMgr->intersectSimply(aoRay, EIC_front, radius, ent)) {
                    hit += smoothStep(aoOuterRadius, aoInnerRadius, radius);
                }
            }
            assert(testCnt > 0 && testCnt == (1 + 4 * aoLevel * aoLevel));
            assert(hit <= testCnt);
            return std::min(1.f,
                    (1 - hit / testCnt) * (1 - aoShadeBias) + aoShadeBias);
        }
    }
    return 1;
}

Vector3 Renderer::getColorViaRay(
        const Ray& r, SceneManager *sceneMgr, Sampler& sampler, int depth)
{
    TraceFragment frag;
    Entity *ent = sceneMgr->pickClosestEntity(r, EIC_front, frag);
    if (ent == NULL) {
        if (m_enableSkybox) return getSkyColor(r, sceneMgr, sampler);
        else return Vector3::ZERO;
    }
    ITraceable* tr = ent->getTraceable();

    // 阴影
    float shadowShade = getShadowShade(
            m_shadowLevel, m_shadowShade, m_shadowRayBias, 
            frag, sceneMgr);
    // 凹凸贴图
    adjustNormalWithBumpMap(frag, sampler);
    // 光照和贴图
    Vector3 clr = getLightColor(
            m_enableSpecular, shadowShade, frag, sceneMgr, sampler);
    // 折射反射
    addReflectAndRefractColor(
            clr, this,
            m_refractRayBias, m_maxTraceDepth, depth, 
            r, sceneMgr, frag, tr, sampler);
    // ao
    float aoShade = getAOShade(
            m_aoLevel, m_aoRayBias, m_aoShadeBias, m_aoInnerRadius, m_aoOuterRadius,
            frag, sceneMgr, tr, ent);
    clr *= shadowShade * aoShade;
    return clr;
}

class CameraRayCache
{
public:
    CameraRayCache();
    ~CameraRayCache();
    void update(const Camera *camera, int w, int h);
    const Ray& get(int x, int y);
private:
    void buildRays();
    void clearRays();
private:
    std::vector<Ray*> m_rayLines;
    std::vector<Ray> m_rayBuf;
    const Camera* m_camera;
    int m_w, m_h;
};
CameraRayCache::CameraRayCache():
    m_camera(NULL), m_w(0), m_h(0)
{
}
CameraRayCache::~CameraRayCache()
{
    clearRays();
}
void CameraRayCache::update(const Camera *camera, int w, int h)
{
    if (m_camera == camera && m_w == w && m_h == h) return;
    clearRays();
    m_w = w, m_h = h;
    m_camera = camera;
    buildRays();
}
inline const Ray& CameraRayCache::get(int x, int y)
{
    return m_rayLines[y][x];
}
void CameraRayCache::buildRays()
{
    assert(m_camera != NULL && m_w > 0 && m_h > 0);
    m_rayBuf.resize(m_w * m_h, Ray(Vector3::ZERO, Vector3(1, 0, 0)));
    for (int y = 0; y < m_h; ++y) {
        Ray *rline = &m_rayBuf[y * m_w];
        for (int x = 0; x < m_w; ++x) {
            rline[x] = m_camera->getRayFromViewport(x / float(m_w), y / float(m_h));
        }
        m_rayLines.push_back(rline);
    }
}
void CameraRayCache::clearRays()
{
    m_rayLines.clear();
    m_rayBuf.clear();
    m_camera = NULL;
    m_w = m_h = 0;
}

int color2Int(const Vector3& color) 
{
    assert(color.x >= 0 && color.y >= 0 && color.z >= 0);
    Vector3 t(color);
    t *= 255;
    return 
        ((t.x >= 255.f ? 255 : (int)t.x) << 16) | 
        ((t.y >= 255.f ? 255 : (int)t.y) << 8) |
        ((t.z >= 255.f ? 255 : (int)t.z));
}

Renderer::Renderer():
    m_cameraRayCache(new CameraRayCache()),
    m_enableSpecular(false), 
    m_maxTraceDepth(0),
    m_shadowLevel(0), m_aoLevel(0),
    m_enableSkybox(false), 
    m_aoInnerRadius(0.45f), m_aoOuterRadius(0.6f),
    m_aoRayBias(0.001f), m_aoShadeBias(0.3f),
    m_shadowRayBias(0.01f), m_shadowShade(0.75f),
    m_refractRayBias(0.01f)
{
}
Renderer::~Renderer()
{
    sdelete(m_cameraRayCache);
}

void Renderer::render(SceneManager* sceneMgr, char *buf, int w, int h, int pitch)
{
    m_cameraRayCache->update(sceneMgr->getCamera(), w, h);

#if USE_OMP
#pragma omp parallel for
#endif
    for (int y = 0; y < h; ++y) {
        Sampler sampler;
        int *lineBuf = (int*)(buf + y * pitch);

        for (int x = 0; x < w; ++x) {
            lineBuf[x] = color2Int(
                    getColorViaRay(
                        m_cameraRayCache->get(x, y),
                        sceneMgr,
                        sampler,
                        1));
        }
    }
}

void Renderer::printStream(std::ostream& fo) const
{
    StreamBlockWriter w("Renderer", fo);
    w.write("enableSkybox", m_enableSkybox);
    w.write("enableSpecular", m_enableSpecular);
    w.write("maxTraceDepth", m_maxTraceDepth);
    w.write("shadowLevel", m_shadowLevel);
    w.write("aoLevel", m_aoLevel);
    w.write("aoInnerRadius", m_aoInnerRadius);
    w.write("aoOuterRadius", m_aoOuterRadius);
    w.write("aoRayBias", m_aoRayBias);
    w.write("aoShadeBias", m_aoShadeBias);
    w.write("shadowRayBias", m_shadowRayBias);
    w.write("shadowShade", m_shadowShade);
    w.write("refractRayBias", m_refractRayBias);
}

void Renderer::scanStream(std::istream& fi)
{
    StreamBlockReader r("Renderer", fi);
    if (!r.read("enableSkybox", &m_enableSkybox)) assert(0);
    if (!r.read("enableSpecular", &m_enableSpecular)) assert(0);
    if (!r.read("maxTraceDepth", &m_maxTraceDepth)) assert(0);
    if (!r.read("shadowLevel", &m_shadowLevel)) assert(0);
    if (!r.read("aoLevel", &m_aoLevel)) assert(0);
    if (!r.read("aoInnerRadius", &m_aoInnerRadius)) assert(0);
    if (!r.read("aoOuterRadius", &m_aoOuterRadius)) assert(0);
    if (!r.read("aoRayBias", &m_aoRayBias)) assert(0);
    if (!r.read("aoShadeBias", &m_aoShadeBias)) assert(0);
    if (!r.read("shadowRayBias", &m_shadowRayBias)) assert(0);
    if (!r.read("shadowShade", &m_shadowShade)) assert(0);
    if (!r.read("refractRayBias", &m_refractRayBias)) assert(0);
}

void Renderer::enableSkybox(bool b)
{
    cout << "Render state changed [enable skybox] : " 
        << m_enableSkybox << "->" << b << endl;
    m_enableSkybox = b;
}
void Renderer::enableSpecular(bool b)
{
    cout << "Render state changed [enable specular] : " 
        << m_enableSpecular << "->" << b << endl;
    m_enableSpecular = b;
}
void Renderer::setMaxTraceDepth(int maxDepth)
{
    cout << "Render state changed [max trace depth] : " 
        << m_maxTraceDepth << "->" << maxDepth << endl;
    m_maxTraceDepth = maxDepth;
}
void Renderer::setShadowLevel(int shadowLvl)
{
    cout << "Render state changed [shadow level] : " 
        << m_shadowLevel << "->" << shadowLvl << endl;
    m_shadowLevel = shadowLvl;
}
void Renderer::setAOLevel(int aoLvl)
{
    cout << "Render state changed [ao level] : " 
        << m_aoLevel << "->" << aoLvl << endl;
    m_aoLevel = aoLvl;
}
