// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>

#include <string>

#include "Serialize.h"
#include "TriTraceAccelerator.h"
#include "TriTraceAccelerator_KDTree.h"
#include "Mesh.h"
#include "Matrix.h"
#include "Vector.h"
#include "Geometry.h"
#include "Traceable.h"

static std::string getTriTraceAcceleratorType(const TriTraceAccelerator_Base* p)
{
    if (dynamic_cast<const TriTraceAccelerator_KDTree*>(p)) {
        return "KDTree";
    }
    if (dynamic_cast<const TriTraceAccelerator_Base*>(p)) {
        return "Base";
    }
    assert(0);
    return "";
}
static TriTraceAccelerator_Base* createTriTraceAcceleratorByType(
        const std::string& s)
{
    if (s == "Base") return new TriTraceAccelerator_Base();
    if (s == "KDTree") return new TriTraceAccelerator_KDTree();
    assert(0);
    return NULL;
}


std::ostream& operator << (std::ostream& so, const TriTraceAccelerator_Base* p)
{
    StreamBlockWriter w("TriTraceAccelerator", so);
    w.write("type", getTriTraceAcceleratorType(p).c_str());
    p->printStream(so);
    return so;
}
std::istream& operator >> (std::istream& si, TriTraceAccelerator_Base*& p)
{
    StreamBlockReader r("TriTraceAccelerator", si);
    std::string type;
    if (!r.read("type", type)) assert(0);
    assert(p == NULL);
    p = createTriTraceAcceleratorByType(type);
    p->scanStream(si);
    return si;
}

TriTraceAccelerator_Base::TriTraceAccelerator_Base():
    m_aabb(Vector3::ZERO, Vector3::ZERO)
{
}
TriTraceAccelerator_Base::~TriTraceAccelerator_Base()
{
}
void TriTraceAccelerator_Base::rebuild(const SubMesh* sub, const Matrix4x4& worldView)
{
    int tCnt = sub->indexBuffer.getTriangleCount();
    m_tris.resize(tCnt);
    const Vector3* pbuf = &sub->vertexBuffer.getElementList3<EVEI_position>()[0];
    const Vector3* nbuf = &sub->vertexBuffer.getElementList3<EVEI_normal>()[0];
    const Vector2* uvBuf = (sub->vertexBuffer.getVertexType() & EVET_texCoord) ? 
        &sub->vertexBuffer.getElementList2<EVEI_texCoord>()[0]: NULL;
    Vector4 v4;
    for (int i = 0; i < tCnt; ++i) {
        const IndexTriangle& itri = sub->indexBuffer.triangle(i);
        Triangle& tri = m_tris[i];

        // pos
        tri.p0 = pbuf[itri.v0];
        transformPoint(tri.p0, worldView);
        tri.p1 = pbuf[itri.v1];
        transformPoint(tri.p1, worldView);
        tri.p2 = pbuf[itri.v2];
        transformPoint(tri.p2, worldView);

        // norm
        tri.n0 = nbuf[itri.v0];
        transformDirection(tri.n0, worldView);
        tri.n1 = nbuf[itri.v1];
        transformDirection(tri.n1, worldView);
        tri.n2 = nbuf[itri.v2];
        transformDirection(tri.n2, worldView);

        // uv
        if (uvBuf != NULL) {
            tri.uv0 = uvBuf[itri.v0];
            tri.uv1 = uvBuf[itri.v1];
            tri.uv2 = uvBuf[itri.v2];
        }

        tri.plane = Plane(tri.p0, tri.p1, tri.p2);
    }
    m_mat = &sub->mat;

    m_aabb.minPt = m_aabb.maxPt = m_tris[0].p0;
    for (int i = 0; i < (int)m_tris.size(); ++i) {
        const Triangle& tri = m_tris[i];
        m_aabb.merge(tri.p0);
        m_aabb.merge(tri.p1);
        m_aabb.merge(tri.p2);
    }
    m_aabb.minPt -= Vector3(EPSILON * 2);
    m_aabb.maxPt += Vector3(EPSILON * 2);
}
bool TriTraceAccelerator_Base::intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag)
{
    float ts[2] = {0};
    if (!rayIntersectAABB(r, m_aabb, ts)) return false;

    int tid = -1;
    Vector2 ab;
    for (int i = 0; i < (int)m_tris.size(); ++i) {
        const Triangle& tri = m_tris[i];
        float _t;
        E_IntersectCase ic = ::intersect(r, tri.plane, _t);
        if (intersectFace & ic) {
            if (_t >= ts[0] && _t <= ts[1] && _t < t) {
                Vector3 pt = r.getPoint(_t);
                Vector2 _ab;
                if (isPointInTriangle(pt, tri, _ab)) {
                    t = _t;
                    tid = i;
                    ab = _ab;
                    frag.pos = pt;
                }
            }
        }
    }
    if (tid != -1) {
        frag.mat = m_mat;
        interpolateNormUV(frag, m_tris[tid], ab);
        updateTangentSpace(frag, m_tris[tid]);
        return true;
    }
    return false;
}
bool TriTraceAccelerator_Base::intersectSimply(const Ray& r, int intersectFace, float &t)
{
    float ts[2] = {0};
    if (!rayIntersectAABB(r, m_aabb, ts)) return false;

    bool b = false;
    for (int i = 0; i < (int)m_tris.size(); ++i) {
        const Triangle& tri = m_tris[i];
        float _t;
        E_IntersectCase ic = ::intersect(r, tri.plane, _t);
        if (intersectFace & ic) {
            if (_t >= ts[0] && _t <= ts[1] && _t <= t) {
                Vector3 pt = r.getPoint(_t);
                Vector2 _ab;
                if (isPointInTriangle(pt, tri, _ab)) {
                    t = _t;
                    b = true;
                }
            }
        }
    }
    return b;
}
bool TriTraceAccelerator_Base::intersectTest(const Ray& r)
{
    float ts[2] = {0};
    if (!rayIntersectAABB(r, m_aabb, ts)) return false;

    for (int i = 0; i < (int)m_tris.size(); ++i) {
        const Triangle& tri = m_tris[i];
        float _t;
        if (::intersect(r, tri.plane, _t) != EIC_none) {
            if (_t >= ts[0] && _t <= ts[1]) {
                Vector3 pt = r.getPoint(_t);
                Vector2 _ab;
                if (isPointInTriangle(pt, tri, _ab)) {
                    return true;
                }
            }
        }
    }
    return false;
}
TriTraceAccelerator_Base* TriTraceAccelerator_Base::clone() const
{
    TriTraceAccelerator_Base *p = new TriTraceAccelerator_Base();
    p->m_tris = m_tris;
    p->m_mat = m_mat;
    return p;
}
void TriTraceAccelerator_Base::printStream(std::ostream& so) const
{
}
void TriTraceAccelerator_Base::scanStream(std::istream& si)
{
}
