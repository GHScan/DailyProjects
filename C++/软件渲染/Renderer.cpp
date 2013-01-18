// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>
#include <cstdio>

#include <vector>

#include "Log.h"
#include "Texture.h"
#include "Renderer.h"
#include "Matrix.h"
#include "Vector.h"
#include "Entity.h"
#include "Camera.h"
#include "Light.h"
#include "VectorT.h"
#include "Geometry.h"
#include "Rasterizer.h"
#include "Mesh.h"
#include "SceneManager.h"

#define RGB(r, g, b) ((r << 16) | (g << 8) | b)

template<typename T>
struct PairFirstLess
{
    bool operator () (const T& a, const T& b) const
    {
        return a.first < b.first;
    }
};
template<typename T>
struct PairFirstGreater
{
    bool operator () (const T& a, const T& b) const
    {
        return a.first > b.first;
    }
};

void calcEntityZInfo(std::vector<std::pair<float, Entity*> > &zinfo, 
        const std::vector<Entity*> &ents,
        const Camera* camera)
{
    zinfo.clear();
    Matrix4x4 viewMat = camera->getViewMatrix();
    for (int i = 0; i < (int)ents.size(); ++i) {
        Entity *ent = ents[i];
        Matrix4x4 mat = ent->getSceneNode()->getWorldMatrix() * viewMat;
        AABB taabb(ent->getBoundAABB());
        transform(taabb, mat);
        zinfo.push_back(std::pair<float, Entity*>(taabb.minPt.z, ent));
    }
}

enum E_VertexCullType
{
    EVCT_clipPlane = 1,
    EVCT_frustumLeft = 0x100,
    EVCT_frustumRight = 0x200,
    EVCT_frustumTop = 0x400,
    EVCT_frustumBottom = 0x800,
    EVCT_frustumNear = 0x1000,
    EVCT_frustumFar = 0x2000,
    EVCT_frustumMask = 0xff00,
};
enum E_TriangleCullType
{
    ETCT_faceDirection = 1,
    ETCT_clipPlane = 2,
    ETCT_frustum = 4,
};

class PipelineCache
{
public:
    void notifyVertexCount(int vCnt);
    void notifyTriangleCount(int tCnt);

    Vector4* positionBuffer() { return &m_posBuf[0]; }
    Vector4* normalBuffer() { return &m_normBuf[0]; }
    Vector3* colorBuffer() { return &m_clrBuf[0]; }
    Vector3* colorBuffer2() { return &m_clrBuf2[0];}
    int* vertexRefBuffer() { return &m_vertexRefBuf[0]; }
    int* vertexCullBuffer() { return &m_vertexCullBuf[0];}
    int* triangleCullBuffer() { return &m_triangleCullBuf[0];}
    std::pair<float, int>* triangleZDis2IdxBuffer() { return &m_triangleZDis2IdxBuf[0]; }
private:
    std::vector<Vector4> m_posBuf;
    std::vector<Vector4> m_normBuf;
    std::vector<Vector3> m_clrBuf;
    std::vector<Vector3> m_clrBuf2;
    std::vector<int> m_vertexRefBuf;
    std::vector<int> m_vertexCullBuf;
    std::vector<int> m_triangleCullBuf;
    std::vector<std::pair<float, int> > m_triangleZDis2IdxBuf;
};
void PipelineCache::notifyVertexCount(int vCnt)
{
    m_posBuf.resize(vCnt);
    m_normBuf.resize(vCnt);
    m_clrBuf.resize(vCnt);
    m_clrBuf2.resize(vCnt);
    m_vertexRefBuf.resize(vCnt);
    m_vertexCullBuf.resize(vCnt);
}
void PipelineCache::notifyTriangleCount(int tCnt)
{
    m_triangleCullBuf.resize(tCnt);
    m_triangleZDis2IdxBuf.resize(tCnt);
}

class Renderer_Impl
{
public:
    Renderer_Impl(const SceneManager* sceneMgr);
    ~Renderer_Impl();

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
    void renderSubMesh(Rasterizer &rasterizer,
            const SubMesh& sub, const Matrix4x4& worldMat);
    void drawTriangle(
            Rasterizer &rasterizer, Sampler* sampler, const Light* l, int tid,
            const IndexTriangle& tri,
            const Vector4* posBuf, const Vector3* clrBuf, const Vector2* texBuf,
            const Vector3* srcPosBuf, const Vector3* srcNormBuf,
            const Vector3* clrBuf2);

private:
    const SceneManager *m_sceneMgr;
    PipelineCache   *m_pipeCache;
    Sampler m_sampler;
    std::vector<float> m_zbuf;
    std::vector<Plane> m_clipPlanes;

    E_CullFace m_cullFace;
    E_ZBufferType m_zbufType;
    E_TextureFilterType m_texFilterType;
    E_TextureAddressingMode m_texAddressingMode;
    E_ShadeMode m_shadeMode;
    E_ZSortType m_zsortType;
    E_SpecularLight m_specularLight;
};
Renderer_Impl::Renderer_Impl(const SceneManager* sceneMgr):
    m_sceneMgr(sceneMgr), m_pipeCache(new PipelineCache()),
    m_cullFace(ECF_back), m_texFilterType(ETFT_null), m_zbufType(EZBT_null),
    m_shadeMode(ESM_frame), m_zsortType(EZST_null), m_specularLight(ESL_disable),
    m_texAddressingMode(ETAM_clamp)
{
}
Renderer_Impl::~Renderer_Impl()
{
    sdelete(m_pipeCache);
}

E_CullFace Renderer_Impl::getCullFace() const { return m_cullFace; }
void Renderer_Impl::setCullFace(E_CullFace face) { m_cullFace = face; }
E_TextureFilterType Renderer_Impl::getTextureFilterType() const { return m_texFilterType; }
void Renderer_Impl::setTextureFilterType(E_TextureFilterType t) { m_texFilterType = t; }
E_ZBufferType Renderer_Impl::getZbufferType() const { return m_zbufType; }
void Renderer_Impl::setZbufferType(E_ZBufferType t) { m_zbufType = t; }
E_ShadeMode Renderer_Impl::getShadeMode() const { return m_shadeMode; }
void Renderer_Impl::setShadeMode(E_ShadeMode mode) { m_shadeMode = mode; }
E_ZSortType Renderer_Impl::getZSortType() const { return m_zsortType; }
void Renderer_Impl::setZSortType(E_ZSortType t) { m_zsortType = t;}
E_SpecularLight Renderer_Impl::getSpecularLight() const { return m_specularLight; }
void Renderer_Impl::setSpecularLight(E_SpecularLight t) { m_specularLight = t; }
E_TextureAddressingMode Renderer_Impl::getTextureAddressingMode() const { return m_texAddressingMode;}
void Renderer_Impl::setTextureAddressingMode(E_TextureAddressingMode t) { m_texAddressingMode = t;}

void Renderer_Impl::addClipPlane(const Plane& p) { m_clipPlanes.push_back(p); }
void Renderer_Impl::clearClipPlane() { m_clipPlanes.clear(); }

void Renderer_Impl::render(char *buf, int w, int h, int pitch)
{
    if (m_zbufType == EZBT_zbuf) {
        m_zbuf.assign(w * h, 1);
    }
    if (m_zbufType == EZBT_1zbuf) {
        m_zbuf.resize(w * h);
        memset(&m_zbuf[0], 0, w * h * sizeof(m_zbuf[0]));
    }

    Rasterizer rasterizer(buf, w, h, pitch, 
            m_zbufType == EZBT_null ? NULL : &m_zbuf[0],
            m_texFilterType == ETFT_null ? NULL : &m_sampler);

    std::vector<Entity*> visibleEntitys = m_sceneMgr->getVisibleEntityList();

    // 将物体根据z排序
    std::vector<std::pair<float, Entity*> > zinfo;
    if (m_zsortType == EZST_near2Far) {
        calcEntityZInfo(zinfo, visibleEntitys, m_sceneMgr->getCamera());
        std::sort(zinfo.begin(), zinfo.end(), PairFirstLess<std::pair<float, Entity*> >());
    }
    else if (m_zsortType == EZST_far2Near) {
        calcEntityZInfo(zinfo, visibleEntitys, m_sceneMgr->getCamera());
        std::sort(zinfo.begin(), zinfo.end(), PairFirstGreater<std::pair<float, Entity*> >());
    }
    else {}
    if (!zinfo.empty()) {
        visibleEntitys.clear();
        for (int i = 0; i < (int)zinfo.size(); ++i) {
            visibleEntitys.push_back(zinfo[i].second);
        }
    }

    for (int i = 0; i < (int)visibleEntitys.size(); ++i) {
        if (visibleEntitys[i]->getObjType() == ESOT_staticEntity) {
            StaticEntity *ent = (StaticEntity*)visibleEntitys[i];
            const Mesh* m = ent->getMesh();
            for (int i = 0; i < m->getSubCount(); ++i) {
                renderSubMesh(
                        rasterizer,
                        *m->sub(i), ent->getSceneNode()->getWorldMatrix());
            }
        }
        else assert(0);
    }
}
void Renderer_Impl::renderSubMesh(Rasterizer &rasterizer,
        const SubMesh& sub, const Matrix4x4& worldMat)
{
    const Light* light = m_sceneMgr->getLight();
    const Camera* camera = m_sceneMgr->getCamera();

    const bool useTexture = 
        m_texFilterType != ETFT_null && 
        (sub.vertexBuffer.getVertexType() & EVET_texCoord) &&
        !sub.mat.texture.empty() &&
        TextureManager::instance()->find(sub.mat.texture.c_str())->valid();
    const bool useLighting = 
        m_shadeMode != ESM_frame && m_shadeMode != ESM_const &&
        light != NULL &&
        (sub.vertexBuffer.getVertexType() & EVET_normal);

    Matrix4x4 invWorldMat = worldMat.inverse();
    Matrix4x4 worldViewMat = worldMat * camera->getViewMatrix();
    Matrix4x4 worldViewProjMat = worldViewMat * camera->getProjectionMatrix();
    Matrix4x4 camera2ModelSpaceMat = camera->getSceneNode()->getWorldMatrix() * invWorldMat;

    Vector3 cameraPosInMS; // 物体空间的摄像机
    Vector3 cameraDirInMS;
    {
        Vector4 pos4(camera->getSceneNode()->getPosition());
        transform(pos4, invWorldMat);
        assert(fequal(pos4.w, 1));
        cameraPosInMS = pos4.divW();

        Vector4 dir4(0, 0, 1, 0);
        transform(dir4, camera2ModelSpaceMat);
        assert(fequal(dir4.w, 0));
        cameraDirInMS = (Vector3&)dir4;
    }

    if (useTexture) {
        m_sampler.setTexture(
                TextureManager::instance()->find(sub.mat.texture.c_str()));
        m_sampler.setMinFilterType(m_texFilterType);
        m_sampler.setMagFilterType(m_texFilterType);
        m_sampler.setUAddressingMode(m_texAddressingMode);
        m_sampler.setVAddressingMode(m_texAddressingMode);
    }
    if (useLighting) {
        light->beginLighting(invWorldMat, sub.mat, cameraPosInMS);
    }

    int vCnt = sub.vertexBuffer.getVertexCount();
    int tCnt = sub.indexBuffer.getTriangleCount();
    const int rawVCnt = vCnt, rawTCnt = tCnt;
    assert(vCnt > 0 && tCnt > 0);

    m_pipeCache->notifyVertexCount(vCnt);
    m_pipeCache->notifyTriangleCount(tCnt);
    Vector4 *posBuf = m_pipeCache->positionBuffer();
    Vector3 *clrBuf = (m_shadeMode != ESM_frame && m_shadeMode != ESM_const) || 
        (sub.vertexBuffer.getVertexType() & EVET_color) ? 
        m_pipeCache->colorBuffer() : NULL;
    Vector3 *clrBuf2 = NULL;
    const Vector3 *srcPosBuf = &sub.vertexBuffer.getElementList3<EVEI_position>()[0];
    const Vector3 *srcNormBuf = useLighting ? 
        &sub.vertexBuffer.getElementList3<EVEI_normal>()[0] : NULL;
    const Vector2 *texBuf = useTexture ? 
        &sub.vertexBuffer.getElementList2<EVEI_texCoord>()[0] : NULL;
    const IndexTriangle *triBuf = &sub.indexBuffer.getTriangleList()[0];
    int* vertexRefBuf = m_pipeCache->vertexRefBuffer();
    int* triCullBuf = m_pipeCache->triangleCullBuffer();
    int* vertexCullBuf = m_pipeCache->vertexCullBuffer();

    for (int i = 0; i < vCnt; ++i) {
        posBuf[i] = Vector4(sub.vertexBuffer.element3<EVEI_position>(i));
    }
    if (clrBuf != NULL) {
        if (sub.vertexBuffer.getVertexType() & EVET_color) {
            memcpy(clrBuf, &sub.vertexBuffer.getElementList3<EVEI_color>()[0], vCnt * 3 * sizeof(float));
        }
        else {
            Vector3 v(1, 1, 1);
            for (int i = 0; i < vCnt; ++i) clrBuf[i] = v;
        }
    }
    memset(vertexRefBuf, 0, vCnt * sizeof(int));
    for (int i = 0; i < tCnt; ++i) {
        ++vertexRefBuf[triBuf[i].v0];
        ++vertexRefBuf[triBuf[i].v1];
        ++vertexRefBuf[triBuf[i].v2];
    }
    memset(triCullBuf, 0, tCnt * sizeof(int));
    memset(vertexCullBuf, 0, vCnt * sizeof(int));

    // 背面裁剪
    if (m_cullFace != ECF_null) {

        sub.genTriangleNormals();

        int backCull = 0;

        if (camera->getObjType() == ESOT_perspectiveCamera) {
            for (int i = 0; i < tCnt; ++i) {
                assert(triCullBuf[i] == 0);
                const IndexTriangle& tri = triBuf[i];
                Vector3 vertex2Camera = cameraPosInMS - (Vector3&)posBuf[tri.v0];
                float cosA = vertex2Camera.dotProduct(sub.triangleNormals[i]);
                if ((m_cullFace == ECF_back && cosA <= 0) || // 裁剪背面
                        (m_cullFace == ECF_front && cosA >= 0)) { // 裁剪正面
                    triCullBuf[i] |= ETCT_faceDirection;
                    --vertexRefBuf[tri.v0];
                    --vertexRefBuf[tri.v1];
                    --vertexRefBuf[tri.v2];
                    ++backCull;
                }
            }
        }
        else if (camera->getObjType() == ESOT_orthoCamera) {
            Vector3 invCameraDirInMS = -cameraDirInMS;
            for (int i = 0; i < tCnt; ++i) {
                assert(triCullBuf[i] == 0);
                const IndexTriangle& tri = triBuf[i];
                float cosA = invCameraDirInMS.dotProduct(sub.triangleNormals[i]);
                if ((m_cullFace == ECF_back && cosA <= 0) || // 裁剪背面
                        (m_cullFace == ECF_front && cosA >= 0)) { // 裁剪正面
                    triCullBuf[i] |= ETCT_faceDirection;
                    --vertexRefBuf[tri.v0];
                    --vertexRefBuf[tri.v1];
                    --vertexRefBuf[tri.v2];
                    ++backCull;
                }
            }
        }

        Log::instance()->addMsg("back triangle cull : %d/%d", backCull, tCnt);
    }

    // 平面裁剪
    {
        std::vector<Plane> planes;
        for (int i = 0; i < (int)m_clipPlanes.size(); ++i) {
            Plane p(m_clipPlanes[i]);
            transform(p, invWorldMat);
            planes.push_back(p);
        }
        // 添加nearZ面
        {
            Plane p = camera->getVolumePlanes(EPID_front).planes[0];
            Matrix4x4 camera2ModelMat= 
                camera->getSceneNode()->getWorldMatrix() * invWorldMat;
            transform(p, camera2ModelMat);
            planes.push_back(p);
        }

        for (int pid = 0; pid < (int)planes.size(); ++pid) {
            const Plane& p(planes[pid]);
            // 裁剪
            for (int i = 0; i < vCnt; ++i) {
                if (vertexRefBuf[i] == 0) continue;
                if (distance(p, (Vector3&)posBuf[i]) > 0) {
                    vertexCullBuf[i] |= EVCT_clipPlane;
                }
            }
            int planeCull = 0, triCut = 0, triAdd = 0;
            int newTCnt = tCnt, newVCnt = vCnt;
            for (int i = 0; i < tCnt; ++i) {
                if (triCullBuf[i]) continue;
                const IndexTriangle& tri = triBuf[i];
                int clipVertexCnt = 
                    (vertexCullBuf[tri.v0] != 0) + (vertexCullBuf[tri.v1] != 0) + (vertexCullBuf[tri.v2] != 0);
                if (clipVertexCnt == 0) continue;

                triCullBuf[i] |= ETCT_clipPlane;
                --vertexRefBuf[tri.v0];
                --vertexRefBuf[tri.v1];
                --vertexRefBuf[tri.v2];

                if (clipVertexCnt == 3) {
                    ++planeCull;
                    continue;
                }
                // 三角形分割
                ++triCut;

                int oldTri[3] = {tri.v0, tri.v1, tri.v2};
                int enterPt = 0;
                for (int i = 0; i < 3; ++i) {
                    if (vertexCullBuf[oldTri[i]] == 0 && vertexCullBuf[oldTri[(i + 1) % 3]]) {
                        enterPt = i;
                        break;
                    }
                }
                if (enterPt == 1) { // 循环左移一个
                    std::swap(oldTri[0], oldTri[1]);
                    std::swap(oldTri[1], oldTri[2]);
                }
                else if (enterPt == 2) { // 循环左移二个
                    std::swap(oldTri[0], oldTri[1]);
                    std::swap(oldTri[0], oldTri[2]);
                }
                else assert(enterPt == 0);

                m_pipeCache->notifyVertexCount(newVCnt += 2);
                m_pipeCache->notifyTriangleCount(
                        newTCnt += (clipVertexCnt == 1) ? 2 : 1);
                sub.vertexBuffer.resizeElementList(newVCnt);
                sub.indexBuffer.resizeTriangleList(newTCnt);

                posBuf = m_pipeCache->positionBuffer();
                if (clrBuf != NULL) clrBuf = m_pipeCache->colorBuffer();
                srcPosBuf = &sub.vertexBuffer.getElementList3<EVEI_position>()[0];
                if (srcNormBuf != NULL) srcNormBuf = 
                    &sub.vertexBuffer.getElementList3<EVEI_normal>()[0];
                if (texBuf != NULL) texBuf = &sub.vertexBuffer.getElementList2<EVEI_texCoord>()[0];
                triBuf = &sub.indexBuffer.getTriangleList()[0];
                vertexRefBuf = m_pipeCache->vertexRefBuffer();
                triCullBuf = m_pipeCache->triangleCullBuffer();
                vertexCullBuf = m_pipeCache->vertexCullBuffer();

                if (clipVertexCnt == 1) {
                    ++triAdd;

                    int t1 = newTCnt - 2, t2 = newTCnt - 1;
                    int v3 = newVCnt - 2, v4 = newVCnt - 1;
                    (IndexTriangle&)triBuf[t1] = IndexTriangle(
                            oldTri[0], v3, oldTri[2]);
                    (IndexTriangle&)triBuf[t2] = IndexTriangle(
                            v3, v4, oldTri[2]);
                    triCullBuf[t1] = 0;
                    triCullBuf[t2] = 0;
                    vertexRefBuf[oldTri[0]] += 1;
                    vertexRefBuf[oldTri[2]] += 2;
                    vertexRefBuf[v3] = 2;
                    vertexRefBuf[v4] = 1;
                    vertexCullBuf[v3] = 0;
                    vertexCullBuf[v4] = 0;
                    // 插值
                    float dis0 = fabs(distance(p, (Vector3&)posBuf[oldTri[0]]));
                    float dis1 = fabs(distance(p, (Vector3&)posBuf[oldTri[1]]));
                    float dis2 = fabs(distance(p, (Vector3&)posBuf[oldTri[2]]));
                    {
                        // 插值v3
                        float r1 = dis0 / (dis0 + dis1);
                        float r0 = 1 - r1;
                        posBuf[v3] = posBuf[oldTri[0]] * r0 + posBuf[oldTri[1]] * r1;
                        assert(fequal(posBuf[oldTri[0]].w, 1) && fequal(posBuf[oldTri[1]].w, 1));
                        posBuf[v3].w = 1; // 避免误差
                        if (clrBuf != NULL) {
                            clrBuf[v3] = clrBuf[oldTri[0]] * r0 + clrBuf[oldTri[1]] * r1;
                        }
                        if (srcPosBuf != NULL) {
                            (Vector3&)srcPosBuf[v3] = srcPosBuf[oldTri[0]] * r0 + srcPosBuf[oldTri[1]] * r1;
                        }
                        if (srcNormBuf != NULL) {
                            (Vector3&)srcNormBuf[v3] = srcNormBuf[oldTri[0]] * r0 + srcNormBuf[oldTri[1]] * r1;
                        }
                        if (texBuf != NULL) {
                            (Vector2&)texBuf[v3] = texBuf[oldTri[0]] * r0 + texBuf[oldTri[1]] * r1;
                        }
                    }
                    {
                        // 插值v4
                        float r1 = dis2 / (dis1 + dis2);
                        float r2 = 1 - r1;
                        posBuf[v4] = posBuf[oldTri[2]] * r2 + posBuf[oldTri[1]] * r1;
                        assert(fequal(posBuf[oldTri[2]].w, 1) && fequal(posBuf[oldTri[1]].w, 1));
                        posBuf[v4].w = 1; // 避免误差
                        if (clrBuf != NULL) {
                            clrBuf[v4] = clrBuf[oldTri[2]] * r2 + clrBuf[oldTri[1]] * r1;
                        }
                        if (srcPosBuf != NULL) {
                            (Vector3&)srcPosBuf[v4] = srcPosBuf[oldTri[2]] * r2 + srcPosBuf[oldTri[1]] * r1;
                        }
                        if (srcNormBuf != NULL) {
                            (Vector3&)srcNormBuf[v4] = srcNormBuf[oldTri[2]] * r2 + srcNormBuf[oldTri[1]] * r1;
                        }
                        if (texBuf != NULL) {
                            (Vector2&)texBuf[v4] = texBuf[oldTri[2]] * r2 + texBuf[oldTri[1]] * r1;
                        }
                    }
                }
                else 
                {
                    assert(clipVertexCnt == 2);
                    // 增加两个顶点, 三角形数不变
                    IndexTriangle& tri1 = (IndexTriangle&)triBuf[newTCnt - 1];
                    tri1 = IndexTriangle(oldTri[0], newVCnt - 2, newVCnt - 1);
                    triCullBuf[newTCnt - 1] = 0;
                    ++vertexRefBuf[tri1.v0];
                    vertexRefBuf[tri1.v1] = 1;
                    vertexRefBuf[tri1.v2] = 1;
                    vertexCullBuf[tri1.v1] = 0;
                    vertexCullBuf[tri1.v2] = 0;
                    // 插值
                    float dis0 = fabs(distance(p, (Vector3&)posBuf[oldTri[0]]));
                    float dis1 = fabs(distance(p, (Vector3&)posBuf[oldTri[1]]));
                    float dis2 = fabs(distance(p, (Vector3&)posBuf[oldTri[2]]));
                    {
                        // 插值v1
                        float r1 = dis0 / (dis0 + dis1);
                        float r0 = 1 - r1;
                        posBuf[tri1.v1] = posBuf[oldTri[0]] * r0 + posBuf[oldTri[1]] * r1;
                        assert(fequal(posBuf[oldTri[0]].w, 1) && fequal(posBuf[oldTri[1]].w, 1));
                        posBuf[tri1.v1].w = 1; // 避免误差
                        if (clrBuf != NULL) {
                            clrBuf[tri1.v1] = clrBuf[oldTri[0]] * r0 + clrBuf[oldTri[1]] * r1;
                        }
                        if (srcPosBuf != NULL) {
                            (Vector3&)srcPosBuf[tri1.v1] = srcPosBuf[oldTri[0]] * r0 + srcPosBuf[oldTri[1]] * r1;
                        }
                        if (srcNormBuf != NULL) {
                            (Vector3&)srcNormBuf[tri1.v1] = srcNormBuf[oldTri[0]] * r0 + srcNormBuf[oldTri[1]] * r1;
                        }
                        if (texBuf != NULL) {
                            (Vector2&)texBuf[tri1.v1] = texBuf[oldTri[0]] * r0 + texBuf[oldTri[1]] * r1;
                        }
                    }
                    {
                        // 插值v2
                        float r2 = dis0 / (dis0 + dis2);
                        float r0 = 1 - r2;
                        posBuf[tri1.v2] = posBuf[oldTri[0]] * r0 + posBuf[oldTri[2]] * r2;
                        assert(fequal(posBuf[oldTri[0]].w, 1) && fequal(posBuf[oldTri[2]].w, 1));
                        posBuf[tri1.v2].w = 1; // 避免误差
                        if (clrBuf != NULL) {
                            clrBuf[tri1.v2] = clrBuf[oldTri[0]] * r0 + clrBuf[oldTri[2]] * r2;
                        }
                        if (srcPosBuf != NULL) {
                            (Vector3&)srcPosBuf[tri1.v2] = srcPosBuf[oldTri[0]] * r0 + srcPosBuf[oldTri[2]] * r2;
                        }
                        if (srcNormBuf != NULL) {
                            (Vector3&)srcNormBuf[tri1.v2] = srcNormBuf[oldTri[0]] * r0 + srcNormBuf[oldTri[2]] * r2;
                        }
                        if (texBuf != NULL) {
                            (Vector2&)texBuf[tri1.v2] = texBuf[oldTri[0]] * r0 + texBuf[oldTri[2]] * r2;
                        }
                    }
                }
            }
            Log::instance()->addMsg("clip plane cull triangles : %d/%d", planeCull, tCnt);
            Log::instance()->addMsg("clip plane cut triangles: %d", triCut);
            Log::instance()->addMsg("clip plane increase triangles : %d", triAdd);

            tCnt = newTCnt, vCnt = newVCnt;
        }
    }

    // 世界视图投影变换
    for (int i = 0; i < vCnt; ++i) {
        if (vertexRefBuf[i] == 0) continue;
        transform(posBuf[i], worldViewProjMat);
    }

    // 投影裁剪
    for (int i = 0; i < vCnt; ++i) {
        if (vertexRefBuf[i] == 0) continue;
        Vector4& p = posBuf[i];
        assert(!fequal(p.w, 0));
        p /= p.w; // 透视除法
        if (p.x < -1) vertexCullBuf[i] |= EVCT_frustumLeft;
        if (p.x > 1) vertexCullBuf[i] |= EVCT_frustumRight;
        if (p.y < -1) vertexCullBuf[i] |= EVCT_frustumBottom;
        if (p.y > 1) vertexCullBuf[i] |= EVCT_frustumTop;
        if (p.z < 0) vertexCullBuf[i] |= EVCT_frustumNear;
        if (p.z > 1) vertexCullBuf[i] |= EVCT_frustumFar;
    }
    int projCull = 0;
    for (int i = 0; i < tCnt; ++i) {
        if (triCullBuf[i]) continue;
        const IndexTriangle& tri = triBuf[i];
        // 三个点都被裁剪
        if ((vertexCullBuf[tri.v0] & vertexCullBuf[tri.v1] & vertexCullBuf[tri.v2]) & 
                EVCT_frustumMask) {
            triCullBuf[i] |= ETCT_frustum;
            --vertexRefBuf[tri.v0];
            --vertexRefBuf[tri.v1];
            --vertexRefBuf[tri.v2];
            ++projCull;
        }
    }
    Log::instance()->addMsg("projection triangle cull : %d/%d", projCull, tCnt);

    if (m_shadeMode == ESM_flat || m_shadeMode == ESM_gouraud) {
        if (m_specularLight == ESL_disable) {
            for (int i = 0; i < vCnt; ++i) {
                if (vertexRefBuf[i] == 0) continue;
                clrBuf[i] = clrBuf[i].multiply(light->illuminate(
                            srcPosBuf[i], srcNormBuf[i]));
            }
        }
        else {
            if (!useTexture) {
                for (int i = 0; i < vCnt; ++i) {
                    if (vertexRefBuf[i] == 0) continue;
                    clrBuf[i] = clrBuf[i].multiply(light->illuminateWithSpecular(
                                srcPosBuf[i], srcNormBuf[i]));
                }
            }
            else {
                assert(useTexture);
                clrBuf2 = m_pipeCache->colorBuffer2();
                for (int i = 0; i < vCnt; ++i) {
                    if (vertexRefBuf[i] == 0) continue;
                    clrBuf2[i] = Vector3::ZERO;
                    clrBuf[i] = clrBuf[i].multiply(light->illuminateWithSeparateSpecular(
                                srcPosBuf[i], srcNormBuf[i], clrBuf2[i]));
                }
            }
        }
    }

    // 视口变换
    int viewportW = rasterizer.width(), viewportH = rasterizer.height();
    for (int i = 0; i < vCnt; ++i) {
        if (vertexRefBuf[i] == 0) continue;
        posBuf[i].x = (posBuf[i].x + 1) * viewportW / 2;
        posBuf[i].y = (1 - posBuf[i].y) * viewportH / 2;
    }

    if (m_zbufType == EZBT_1zbuf) { // 将透视z还原为摄像机空间的z
        Vector4 vz(worldViewMat.transpose()[2]);
        for (int i = 0; i < vCnt; ++i) {
            if (vertexRefBuf[i] == 0) continue;
            posBuf[i].z = Vector4(srcPosBuf[i]).dotProduct(vz);
        }
    }

    // 绘制三角形
    if (m_zsortType == EZST_null) { // 不进行z排序
        for (int i = 0; i < tCnt; ++i) {
            if (triCullBuf[i]) continue;
            const IndexTriangle& tri(triBuf[i]);
            drawTriangle(
                    rasterizer, &m_sampler, light, i, 
                    tri,
                    posBuf, clrBuf, texBuf, srcPosBuf, srcNormBuf, clrBuf2);
        }
    }
    else 
    { // z排序
        std::pair<float, int> *triZ2IdxBuf = m_pipeCache->triangleZDis2IdxBuffer();
        int tri2IdxBufLen = 0;
        for (int i = 0; i < tCnt; ++i) {
            if (triCullBuf[i]) continue;
            const IndexTriangle& tri = triBuf[i];
            float minZ = std::min(posBuf[tri.v2].z,
                    std::min(posBuf[tri.v0].z, posBuf[tri.v1].z));
            triZ2IdxBuf[tri2IdxBufLen++] = std::pair<float, int>(minZ, i);
        }
        if (m_zsortType == EZST_near2Far) {
            std::sort(triZ2IdxBuf, triZ2IdxBuf + tri2IdxBufLen, PairFirstLess<std::pair<float, int> >());
        }
        else {
            assert(m_zsortType == EZST_far2Near);
            std::sort(triZ2IdxBuf, triZ2IdxBuf + tri2IdxBufLen, PairFirstGreater<std::pair<float, int> >());
        }
        for (int i = 0; i < tri2IdxBufLen; ++i) {
            const IndexTriangle& tri(triBuf[triZ2IdxBuf[i].second]);
            drawTriangle(
                    rasterizer, &m_sampler, light, i, 
                    tri,
                    posBuf, clrBuf, texBuf, srcPosBuf, srcNormBuf, clrBuf2);
        }
    }

    if (useLighting) {
        light->endLighting();
    }
    if (useTexture) {
        m_sampler.setTexture(NULL);
    }

    // 恢复因三角形分割可能造成的裁剪
    sub.vertexBuffer.resizeElementList(rawVCnt); 
    sub.indexBuffer.resizeTriangleList(rawTCnt);

    {
        int vAlive = 0, tAlive = 0;
        for (int i = 0; i < vCnt; ++i) {
            if (vertexRefBuf[i] > 0) ++vAlive;
        }
        for (int i = 0; i < tCnt; ++i) {
            if (triCullBuf[i] == 0) ++tAlive;
        }
        Log::instance()->addMsg("Vertex alive: %d/%d, Triangle alive : %d/%d", 
                vAlive, vCnt, tAlive, tCnt);
    }
}

void Renderer_Impl::drawTriangle(
        Rasterizer &rasterizer, Sampler* sampler, const Light *l, int tid,
        const IndexTriangle& tri,
        const Vector4* posBuf, const Vector3* clrBuf,
        const Vector2* texBuf,
        const Vector3* srcPosBuf, const Vector3* srcNormBuf,
        const Vector3* clrBuf2)
{
    const Vector3& p0 = *(const Vector3*)&posBuf[tri.v0];
    const Vector3& p1 = *(const Vector3*)&posBuf[tri.v1];
    const Vector3& p2 = *(const Vector3*)&posBuf[tri.v2];
    Vector2 uv0, uv1, uv2;

    bool useTexture = texBuf != NULL;

    if (useTexture) {
        assert(m_texFilterType != ETFT_null);

        Vector2 maxTexSize(sampler->maxTextureSize());
        sampler->beginSample(
                (Vector2&)p0, texBuf[tri.v0].multiply(maxTexSize),
                (Vector2&)p1, texBuf[tri.v1].multiply(maxTexSize),
                (Vector2&)p2, texBuf[tri.v2].multiply(maxTexSize));

        Vector2 texSize(sampler->textureSize());
        uv0 = texBuf[tri.v0].multiply(texSize);
        uv1 = texBuf[tri.v1].multiply(texSize);
        uv2 = texBuf[tri.v2].multiply(texSize);
    }

    // 线框模式，不考虑纹理和zbuf
    if (m_shadeMode == ESM_frame) {
        rasterizer.drawFrameTriangle(
                (int)p0.x, (int)p0.y, (int)p1.x, (int)p1.y, (int)p2.x, (int)p2.y,
                0xff << 16);
    }

    if (m_shadeMode == ESM_const) {
        if (!useTexture) {
            int clrs[] = {
                RGB(255, 0, 0), RGB(0, 255, 0), RGB(0, 0, 255), RGB(64, 128, 255), 
                RGB(255, 128, 64), RGB(64, 192, 64), RGB(255, 0, 255), RGB(255, 255, 0),
            };
            if (m_zbufType == EZBT_null) {
                rasterizer.drawTriangle<EZBT_null, PixelShader_Flat>(
                        p0, 0.f, p1, 0.f, p2, 0.f, clrs[tid & 0x7]);
            }
            else if (m_zbufType == EZBT_zbuf){
                rasterizer.drawTriangle<EZBT_zbuf, PixelShader_Flat>(
                        p0, 0.f, p1, 0.f, p2, 0.f, clrs[tid & 0x7]);
            }
            else if (m_zbufType == EZBT_1zbuf) {
                rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_Flat>(
                        p0, 0.f, p1, 0.f, p2, 0.f, clrs[tid & 0x7]);
            }
        }
        else {
            Vector3 clr(255);
            if (m_zbufType == EZBT_null) {
                rasterizer.drawTriangle<EZBT_null, PixelShader_TexturedFlat>(
                        p0, uv0, p1, uv1, p2, uv2, clr);
            }
            else if (m_zbufType == EZBT_zbuf) {
                rasterizer.drawTriangle<EZBT_zbuf, PixelShader_TexturedFlat>(
                        p0, uv0, p1, uv1, p2, uv2, clr);
            }
            else if (m_zbufType == EZBT_1zbuf) {
                rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_TexturedFlat>(
                        p0, uv0, p1, uv1, p2, uv2, clr);
            }
        }
    }

    if (m_shadeMode == ESM_flat) {
        Vector3 clr(clrBuf[tri.v0] + clrBuf[tri.v1] + clrBuf[tri.v2]);
        clr *= 255.f / 3;
        if (!useTexture) {
            if (m_zbufType == EZBT_null) {
                rasterizer.drawTriangle<EZBT_null, PixelShader_Flat>(
                        p0, 0.f, p1, 0.f, p2, 0.f, color255Vector2Int(clr));
            }
            else if (m_zbufType == EZBT_zbuf){
                rasterizer.drawTriangle<EZBT_zbuf, PixelShader_Flat>(
                        p0, 0.f, p1, 0.f, p2, 0.f, color255Vector2Int(clr));
            }
            else if (m_zbufType == EZBT_1zbuf){
                rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_Flat>(
                        p0, 0.f, p1, 0.f, p2, 0.f, color255Vector2Int(clr));
            }
        }
        else {
            if (m_specularLight == ESL_disable) {
                if (m_zbufType == EZBT_null) {
                    rasterizer.drawTriangle<EZBT_null, PixelShader_TexturedFlat>(
                            p0, uv0, p1, uv1, p2, uv2, clr);
                }
                else if (m_zbufType == EZBT_zbuf) {
                    rasterizer.drawTriangle<EZBT_zbuf, PixelShader_TexturedFlat>(
                            p0, uv0, p1, uv1, p2, uv2, clr);
                }
                else if (m_zbufType == EZBT_1zbuf) {
                    rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_TexturedFlat>(
                            p0, uv0, p1, uv1, p2, uv2, clr);
                }
            }
            else {
                assert(m_specularLight == ESL_enbale);
                Vector3 specClr(clrBuf2[tri.v0] + clrBuf2[tri.v1] + clrBuf2[tri.v2]);
                specClr *= 255.f / 3;
                std::pair<Vector3, Vector3> clrs(clr, specClr);
                if (m_zbufType == EZBT_null) {
                    rasterizer.drawTriangle<EZBT_null, PixelShader_TexturedFlatWithSpecular>(
                            p0, uv0, p1, uv1, p2, uv2, clrs);
                }
                else if (m_zbufType == EZBT_zbuf) {
                    rasterizer.drawTriangle<EZBT_zbuf, PixelShader_TexturedFlatWithSpecular>(
                            p0, uv0, p1, uv1, p2, uv2, clrs);
                }
                else if (m_zbufType == EZBT_1zbuf) {
                    rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_TexturedFlatWithSpecular>(
                            p0, uv0, p1, uv1, p2, uv2, clrs);
                }
            }
        }
    }

    if (m_shadeMode == ESM_gouraud) {
        Vector3 clr0(clrBuf[tri.v0] * 255); 
        Vector3 clr1(clrBuf[tri.v1] * 255); 
        Vector3 clr2(clrBuf[tri.v2] * 255);
        if (!useTexture) {
            if (m_zbufType == EZBT_null) {
                rasterizer.drawTriangle<EZBT_null, PixelShader_Gouraud>(
                        p0, clr0, p1, clr1, p2, clr2, 0);
            }
            else if(m_zbufType == EZBT_zbuf) {
                rasterizer.drawTriangle<EZBT_zbuf, PixelShader_Gouraud>(
                        p0, clr0, p1, clr1, p2, clr2, 0);
            }
            else if(m_zbufType == EZBT_1zbuf) {
                rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_Gouraud>(
                        p0, clr0, p1, clr1, p2, clr2, 0);
            }
        }
        else {
            if (m_specularLight == ESL_disable) {
                Vector2T<Vector3, Vector2> d0(clr0, uv0);
                Vector2T<Vector3, Vector2> d1(clr1, uv1);
                Vector2T<Vector3, Vector2> d2(clr2, uv2);
                if (m_zbufType == EZBT_null) {
                    rasterizer.drawTriangle<EZBT_null, PixelShader_TexturedGouraud>(
                            p0, d0, p1, d1, p2, d2, 0);
                }
                else if (m_zbufType == EZBT_zbuf) {
                    rasterizer.drawTriangle<EZBT_zbuf, PixelShader_TexturedGouraud>(
                            p0, d0, p1, d1, p2, d2, 0);
                }
                else if (m_zbufType == EZBT_1zbuf) {
                    rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_TexturedGouraud>(
                            p0, d0, p1, d1, p2, d2, 0);
                }
            }
            else {
                Vector3T<Vector3, Vector3, Vector2> d0(clr0, clrBuf2[tri.v0] * 255, uv0);
                Vector3T<Vector3, Vector3, Vector2> d1(clr1, clrBuf2[tri.v1] * 255, uv1);
                Vector3T<Vector3, Vector3, Vector2> d2(clr2, clrBuf2[tri.v2] * 255, uv2);
                if (m_zbufType == EZBT_null) {
                    rasterizer.drawTriangle<EZBT_null, PixelShader_TexturedGouraudWithSpecular>(
                            p0, d0, p1, d1, p2, d2, 0);
                }
                else if (m_zbufType == EZBT_zbuf) {
                    rasterizer.drawTriangle<EZBT_zbuf, PixelShader_TexturedGouraudWithSpecular>(
                            p0, d0, p1, d1, p2, d2, 0);
                }
                else if (m_zbufType == EZBT_1zbuf) {
                    rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_TexturedGouraudWithSpecular>(
                            p0, d0, p1, d1, p2, d2, 0);
                }
            }
        }
    }

    if (m_shadeMode == ESM_phong) {
        Vector3 srcPos0(srcPosBuf[tri.v0]), srcPos1(srcPosBuf[tri.v1]), srcPos2(srcPosBuf[tri.v2]);
        Vector3 srcNorm0(srcNormBuf[tri.v0]), srcNorm1(srcNormBuf[tri.v1]), srcNorm2(srcNormBuf[tri.v2]);
        if (!useTexture) {
            Vector2T<Vector3, Vector3> d0(srcPos0, srcNorm0), d1(srcPos1, srcNorm1), d2(srcPos2, srcNorm2);
            if (m_specularLight == ESL_disable) {
                if (m_zbufType == EZBT_null) {
                    rasterizer.drawTriangle<EZBT_null, PixelShader_Phong>(
                            p0, d0, p1, d1, p2, d2, l);
                }
                else if (m_zbufType == EZBT_zbuf) {
                    rasterizer.drawTriangle<EZBT_zbuf, PixelShader_Phong>(
                            p0, d0, p1, d1, p2, d2, l);
                }
                else if (m_zbufType == EZBT_1zbuf) {
                    rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_Phong>(
                            p0, d0, p1, d1, p2, d2, l);
                }
            }
            else {
                assert(m_specularLight == ESL_enbale);
                if (m_zbufType == EZBT_null) {
                    rasterizer.drawTriangle<EZBT_null, PixelShader_PhongWithSpecular>(
                            p0, d0, p1, d1, p2, d2, l);
                }
                else if (m_zbufType == EZBT_zbuf) {
                    rasterizer.drawTriangle<EZBT_zbuf, PixelShader_PhongWithSpecular>(
                            p0, d0, p1, d1, p2, d2, l);
                }
                else if (m_zbufType == EZBT_1zbuf) {
                    rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_PhongWithSpecular>(
                            p0, d0, p1, d1, p2, d2, l);
                }
            }
        }
        else {
            assert(useTexture);
            Vector3T<Vector3, Vector3, Vector2> d0(srcPosBuf[tri.v0], srcNormBuf[tri.v0], uv0);
            Vector3T<Vector3, Vector3, Vector2> d1(srcPosBuf[tri.v1], srcNormBuf[tri.v1], uv1);
            Vector3T<Vector3, Vector3, Vector2> d2(srcPosBuf[tri.v2], srcNormBuf[tri.v2], uv2);
            if (m_specularLight == ESL_disable) {
                if (m_zbufType == EZBT_null) {
                    rasterizer.drawTriangle<EZBT_null, PixelShader_TexturedPhong>(
                            p0, d0, p1, d1, p2, d2, l);
                }
                else if(m_zbufType == EZBT_zbuf) {
                    rasterizer.drawTriangle<EZBT_zbuf, PixelShader_TexturedPhong>(
                            p0, d0, p1, d1, p2, d2, l);
                }
                else if(m_zbufType == EZBT_1zbuf) {
                    rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_TexturedPhong>(
                            p0, d0, p1, d1, p2, d2, l);
                }
            }
            else {
                assert(m_specularLight == ESL_enbale);
                if (m_zbufType == EZBT_null) {
                    rasterizer.drawTriangle<EZBT_null, PixelShader_TexturedPhongWithSpecular>(
                            p0, d0, p1, d1, p2, d2, l);
                }
                else if(m_zbufType == EZBT_zbuf) {
                    rasterizer.drawTriangle<EZBT_zbuf, PixelShader_TexturedPhongWithSpecular>(
                            p0, d0, p1, d1, p2, d2, l);
                }
                else if(m_zbufType == EZBT_1zbuf) {
                    rasterizer.drawTriangle<EZBT_1zbuf, PixelShader_TexturedPhongWithSpecular>(
                            p0, d0, p1, d1, p2, d2, l);
                }
            }
        }
    }

    if (useTexture) {
        assert(m_texFilterType != ETFT_null);
        sampler->endSample();
    }
}
//----------------------------------------
// Renderer
//----------------------------------------
Renderer::Renderer(const SceneManager* sceneMgr):
    m_impl(new Renderer_Impl(sceneMgr))
{
}
Renderer::~Renderer()
{
    delete m_impl;
}
void Renderer::render(char *buf, int w, int h, int pitch)
{
    m_impl->render(buf, w, h, pitch);
}

E_CullFace Renderer::getCullFace() const { return m_impl->getCullFace(); }
void Renderer::setCullFace(E_CullFace face) 
{
    printf("change render state: %s -> %s\n", enum2Str(getCullFace()), enum2Str(face));
    m_impl->setCullFace(face); 
}

E_TextureFilterType Renderer::getTextureFilterType() const { return m_impl->getTextureFilterType(); }
void Renderer::setTextureFilterType(E_TextureFilterType t) 
{ 
    printf("change render state: %s -> %s\n", enum2Str(getTextureFilterType()), enum2Str(t));
    m_impl->setTextureFilterType(t); 
}

E_TextureAddressingMode Renderer::getTextureAddressingMode() const { return m_impl->getTextureAddressingMode();}
void Renderer::setTextureAddressingMode(E_TextureAddressingMode t)
{
    printf("change render state: %s -> %s\n", enum2Str(getTextureAddressingMode()), enum2Str(t));
    m_impl->setTextureAddressingMode(t);
}

E_ZBufferType Renderer::getZbufferType() const { return m_impl->getZbufferType(); }
void Renderer::setZbufferType(E_ZBufferType t) 
{ 
    printf("change render state: %s -> %s\n", enum2Str(getZbufferType()), enum2Str(t));
    m_impl->setZbufferType(t); 
}

E_ShadeMode Renderer::getShadeMode() const { return m_impl->getShadeMode(); }
void Renderer::setShadeMode(E_ShadeMode mode) 
{ 
    printf("change render state: %s -> %s\n", enum2Str(getShadeMode()), enum2Str(mode));
    m_impl->setShadeMode(mode); 
}

E_ZSortType Renderer::getZSortType() const { return m_impl->getZSortType(); }
void Renderer::setZSortType(E_ZSortType t)
{
    printf("change render state: %s -> %s\n", enum2Str(getZSortType()), enum2Str(t));
    m_impl->setZSortType(t);
}
E_SpecularLight Renderer::getSpecularLight() const { return m_impl->getSpecularLight();}
void Renderer::setSpecularLight(E_SpecularLight t)
{
    printf("change render state: %s -> %s\n", enum2Str(getSpecularLight()), enum2Str(t));
    m_impl->setSpecularLight(t);
}

void Renderer::addClipPlane(const Plane& p) { m_impl->addClipPlane(p); }
void Renderer::clearClipPlane() { m_impl->clearClipPlane(); }
