r// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>
#include <cmath>

#include "Traceable.h"
#include "Serialize.h"
#include "Mesh.h"
#include "TriTraceAccelerator.h"
#include "Matrix.h"

static std::string getTraceableType(const ITraceable* p) 
{
    if (dynamic_cast<const Traceable_Sphere*>(p)) {
        return "Sphere";
    }
    if (dynamic_cast<const Traceable_Box*>(p)) {
        return "Box";
    }
    if (dynamic_cast<const Traceable_Cylinder*>(p)) {
        return "Cylinder";
    }
    if (dynamic_cast<const Traceable_Mesh*>(p)) {
        return "Mesh";
    }
    assert(0);
    return "";
}
static ITraceable* createTraceableByType(const std::string& type)
{
    if (type == "Sphere") return new Traceable_Sphere();
    if (type == "Box") return new Traceable_Box();
    if (type == "Cylinder") return new Traceable_Cylinder();
    if (type == "Mesh") return new Traceable_Mesh();
    assert(0);
    return NULL;
}

std::istream& operator >> (std::istream& si, ITraceable*& p)
{
    StreamBlockReader r("Traceable", si);
    std::string type;
    if (!r.read("type", type)) assert(0);
    assert(p == NULL);
    p = createTraceableByType(type);
    p->scanStream(si);
    return si;
}
std::ostream& operator << (std::ostream& so, const ITraceable* p)
{
    StreamBlockWriter w("Traceable", so);
    w.write("type", getTraceableType(p).c_str());
    p->printStream(so);
    return so;
}

//----------------------------------------
// Traceable_Sphere
//----------------------------------------
Traceable_Sphere::Traceable_Sphere():
    m_sphere(Vector3::ZERO, 1) { }
Traceable_Sphere::Traceable_Sphere(const Material& mat):
    m_mat(mat), m_sphere(Vector3::ZERO, 1) { }

bool Traceable_Sphere::intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag)
{
    float _t;
    E_IntersectCase ic = ::intersect(r, m_sphere, _t);
    if (_t < t && (intersectFace & ic)) {
        t = _t;

        frag.pos = r.getPoint(t);
        frag.norm = (frag.pos - m_sphere.center).normalize();
        frag.mat = &m_mat;

        if (!m_mat.texture.empty() || !m_mat.bumpTexture.empty()) 
        {
            float nDotY = frag.norm.dotProduct(m_xyz[1]);
            Vector3 xzProj(m_xyz[1]);
            xzProj *= -nDotY;
            xzProj += frag.norm;
            float xzProjLen = xzProj.length();
            frag.uv.y = acos(nDotY) / PI;
            if (!fequal(xzProjLen, 0)) {
                xzProj /= xzProjLen;
                frag.uv.x = (atan2(
                            xzProj.dotProduct(m_xyz[2]), 
                            xzProj.dotProduct(m_xyz[0])) + PI) / PI2;
            }
            else {
                frag.uv.x = 0;
            }
        }

        if (!m_mat.bumpTexture.empty()) {
            frag.tangentSpace[1] = frag.norm;
            frag.tangentSpace[0] = frag.norm.crossProduct(m_xyz[1]);
            frag.tangentSpace[0] /= frag.tangentSpace[0].length();
            frag.tangentSpace[2] = frag.tangentSpace[1].crossProduct(
                    frag.tangentSpace[0]);
        }

        return true;
    }
    return false;
}
bool Traceable_Sphere::intersectSimply(const Ray& r, int intersectFace, float &t)
{
    float _t;
    E_IntersectCase ic = ::intersect(r, m_sphere, _t);
    if (_t < t && (intersectFace & ic)) {
        t = _t;
        return true;
    }
    return false;
}
bool Traceable_Sphere::intersectTest(const Ray& r)
{
    float _t;
    return ::intersect(r, m_sphere, _t) != EIC_none;
}

void Traceable_Sphere::applyTransform(const Matrix4x4& mat)
{
    m_sphere = Sphere(Vector3::ZERO, 1);
    transform(m_sphere, mat);

    m_xyz[0] = Vector3::AXIS_X;
    m_xyz[1] = Vector3::AXIS_Y;
    m_xyz[2] = Vector3::AXIS_Z;
    transformDirection(m_xyz[0], mat);
    transformDirection(m_xyz[1], mat);
    transformDirection(m_xyz[2], mat);
}
void Traceable_Sphere::printStream(std::ostream& so) const
{
    StreamBlockWriter w("Sphere", so);
    so << m_mat;
}
void Traceable_Sphere::scanStream(std::istream& si)
{
    StreamBlockReader r("Sphere", si);
    si >> m_mat;
}
//----------------------------------------
// Traceable_Box
//----------------------------------------

Traceable_Box::Traceable_Box():
    m_aabb(Vector3::ZERO, Vector3(1.f)),
    m_invTrans(Matrix4x4::IDENTITY), m_trans(Matrix4x4::IDENTITY)
{
}
Traceable_Box::Traceable_Box(const Material& mat):
    m_aabb(Vector3::ZERO, Vector3(1.f)), 
    m_mat(mat), m_invTrans(Matrix4x4::IDENTITY), m_trans(Matrix4x4::IDENTITY)
{
}
bool Traceable_Box::intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag)
{
    Ray _r(r);
    transform(_r, m_invTrans);
    float ts[2] = {0};
    if (!rayIntersectAABB(_r, m_aabb, ts)) return false;

    float _t = 0;
    if (ts[0] > EPSILON) { // 交正面
        if (!(intersectFace & EIC_front)) return false;
        _t = ts[0];
    }
    else { // 交背面
        if (!(intersectFace & EIC_back)) return false;
        _t = ts[1];
    }

    Vector3 pt(_r.getPoint(_t));
    Vector3 realPt(pt);
    transformPoint(realPt, m_trans);
    _t = r.getT(realPt);
    if (_t >= t) return false;
    t = _t;

    frag.pos = realPt;
    frag.mat = &m_mat;
    bool bumpMap = !m_mat.bumpTexture.empty();

    if (fequal(pt.x, 1)) {
        frag.norm = Vector3::AXIS_X;
        frag.uv = Vector2(pt.z, 1 - pt.y);
        if (bumpMap) {
            frag.tangentSpace[0] = Vector3::AXIS_Z;
            frag.tangentSpace[1] = Vector3::AXIS_X;
            frag.tangentSpace[2] = Vector3::AXIS_NEGATIVE_Y;
        }
    }
    else if (fequal(pt.x, 0)) {
        frag.norm = Vector3::AXIS_NEGATIVE_X;
        frag.uv = Vector2(1 - pt.z, 1 - pt.y);
        if (bumpMap) {
            frag.tangentSpace[0] = Vector3::AXIS_NEGATIVE_Z;
            frag.tangentSpace[1] = Vector3::AXIS_NEGATIVE_X;
            frag.tangentSpace[2] = Vector3::AXIS_NEGATIVE_Y;
        }
    }
    else if (fequal(pt.y, 1)) {
        frag.norm = Vector3::AXIS_Y;
        frag.uv = Vector2(pt.x, 1 - pt.z);
        if (bumpMap) {
            frag.tangentSpace[0] = Vector3::AXIS_X;
            frag.tangentSpace[1] = Vector3::AXIS_Y;
            frag.tangentSpace[2] = Vector3::AXIS_NEGATIVE_Z;
        }
    }
    else if (fequal(pt.y, 0)) {
        frag.norm = Vector3::AXIS_NEGATIVE_Y;
        frag.uv = Vector2(1 - pt.x, 1 - pt.z);
        if (bumpMap) {
            frag.tangentSpace[0] = Vector3::AXIS_NEGATIVE_X;
            frag.tangentSpace[1] = Vector3::AXIS_NEGATIVE_Y;
            frag.tangentSpace[2] = Vector3::AXIS_NEGATIVE_Z;
        }
    }
    else if (fequal(pt.z, 1)) {
        frag.norm = Vector3::AXIS_Z;
        frag.uv = Vector2(1 - pt.x, 1 - pt.y);
        if (bumpMap) {
            frag.tangentSpace[0] = Vector3::AXIS_NEGATIVE_X;
            frag.tangentSpace[1] = Vector3::AXIS_Z;
            frag.tangentSpace[2] = Vector3::AXIS_NEGATIVE_Y;
        }
    }
    else if (fequal(pt.z, 0)) {
        frag.norm = Vector3::AXIS_NEGATIVE_Z;
        frag.uv = Vector2(pt.x, 1 - pt.y);
        if (bumpMap) {
            frag.tangentSpace[0] = Vector3::AXIS_X;
            frag.tangentSpace[1] = Vector3::AXIS_NEGATIVE_Z;
            frag.tangentSpace[2] = Vector3::AXIS_NEGATIVE_Y;
        }
    }
    else assert(0);

    transformDirection(frag.norm, m_trans);
    if (bumpMap) {
        transformDirection(frag.tangentSpace[0], m_trans);
        transformDirection(frag.tangentSpace[1], m_trans);
        transformDirection(frag.tangentSpace[2], m_trans);
    }

    return true;
}
bool Traceable_Box::intersectSimply(const Ray& r, int intersectFace, float &t)
{
    Ray _r(r);
    transform(_r, m_invTrans);
    float ts[2] = {0};
    if (!rayIntersectAABB(_r, m_aabb, ts)) return false;

    float _t = 0;
    if (ts[0] > EPSILON) { // 交正面
        if (!(intersectFace & EIC_front)) return false;
        _t = ts[0];
    }
    else { // 交背面
        if (!(intersectFace & EIC_back)) return false;
        _t = ts[1];
    }

    Vector3 pt(_r.getPoint(_t));
    Vector3 realPt(pt);
    transformPoint(realPt, m_trans);
    _t = r.getT(realPt);
    if (_t > t) return false;
    t = _t;
    return true;
}
bool Traceable_Box::intersectTest(const Ray& r)
{
    Ray _r(r);
    transform(_r, m_invTrans);
    float ts[2] = {0};
    if (!rayIntersectAABB(_r, m_aabb, ts)) return false;
    return true;
}

void Traceable_Box::applyTransform(const Matrix4x4& mat)
{
    // 就可以围绕中心转了
    m_trans = Matrix4x4::fromTranslate(-0.5f, -0.5f, -0.5f); 
    m_trans *= mat;
    m_invTrans = m_trans.inverse();
}
void Traceable_Box::printStream(std::ostream& so) const
{
    StreamBlockWriter w("Box", so);
    so << m_mat;
}
void Traceable_Box::scanStream(std::istream& si)
{
    StreamBlockReader r("Box", si);
    si >> m_mat;
}
//----------------------------------------
// Traceable_Cylinder
//----------------------------------------
Traceable_Cylinder::Traceable_Cylinder():
    m_trans(Matrix4x4::IDENTITY), m_invTrans(Matrix4x4::IDENTITY)
{
}
Traceable_Cylinder::Traceable_Cylinder(const Material& mat):
    m_mat(mat),
    m_trans(Matrix4x4::IDENTITY), m_invTrans(Matrix4x4::IDENTITY)
{
}

bool Traceable_Cylinder::intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag)
{
    Ray _r(r);
    transform(_r, m_invTrans);
    float _t;
    E_IntersectCase ic = intersectStandardCylinder(_r, _t);
    if (!(intersectFace & ic)) return false;

    Vector3 pt(_r.getPoint(_t));
    Vector3 realPt(pt);
    transformPoint(realPt, m_trans);
    _t = r.getT(realPt);
    if (_t >= t) return false;
    t = _t;

    frag.pos = realPt;
    frag.mat = &m_mat;
    if ((pt.x * pt.x + pt.z * pt.z <= 1 - EPSILON) &&
            (fequal(pt.y, -1) || fequal(pt.y, 1))) {
        // 顶或者底
        frag.norm = pt.y > 0 ? Vector3::AXIS_Y : Vector3::AXIS_NEGATIVE_Y;
        frag.uv = Vector2::ZERO;

        frag.tangentSpace[0] = Vector3::AXIS_X;
        frag.tangentSpace[1] = Vector3::AXIS_Y;
        frag.tangentSpace[2] = Vector3::AXIS_Z;
    }
    else {
        frag.norm = Vector3(pt.x, 0, pt.z);
        frag.norm /= frag.norm.length();

        if (!m_mat.texture.empty() || !m_mat.bumpTexture.empty()) {
            frag.uv.y = (1 - pt.y) * 0.5f;
            frag.uv.x = atan2(pt.z, pt.x) / PI2 + 0.5f;
        }

        if (!m_mat.bumpTexture.empty()) {
            frag.tangentSpace[2] = Vector3::AXIS_Y;
            frag.tangentSpace[1] = frag.norm;
            frag.tangentSpace[0] = 
                frag.tangentSpace[1].crossProduct(frag.tangentSpace[2]);
        }
    }

    transformDirection(frag.norm, m_trans);
    if (!m_mat.bumpTexture.empty()) {
        transformDirection(frag.tangentSpace[0], m_trans);
        transformDirection(frag.tangentSpace[1], m_trans);
        transformDirection(frag.tangentSpace[2], m_trans);
    }

    return true;
}
bool Traceable_Cylinder::intersectSimply(const Ray& r, int intersectFace, float &t)
{
    Ray _r(r);
    transform(_r, m_invTrans);
    float _t;
    E_IntersectCase ic = intersectStandardCylinder(_r, _t);
    if (!(intersectFace & ic)) return false;

    Vector3 pt(_r.getPoint(_t));
    Vector3 realPt(pt);
    transformPoint(realPt, m_trans);
    _t = r.getT(realPt);
    if (_t > t) return false;
    t = _t;
    return true;
}
bool Traceable_Cylinder::intersectTest(const Ray& r)
{
    Ray _r(r);
    transform(_r, m_invTrans);
    float _t;
    return intersectStandardCylinder(_r, _t) != EIC_none;
}
void Traceable_Cylinder::applyTransform(const Matrix4x4& mat)
{
    m_trans = mat;
    m_invTrans = m_trans.inverse();
}
void Traceable_Cylinder::printStream(std::ostream& so) const
{
    StreamBlockWriter w("Cylinder", so);
    so << m_mat;
}
void Traceable_Cylinder::scanStream(std::istream& si)
{
    StreamBlockReader r("Cylinder", si);
    si >> m_mat;
}

//----------------------------------------
// Traceable_Mesh
//----------------------------------------
Traceable_Mesh::Traceable_Mesh():
    m_mesh(NULL)
{
}
Traceable_Mesh::Traceable_Mesh(const char *meshName):
    m_mesh(NULL)
{
    m_mesh = MeshManager::instance()->getMesh(meshName);
    for (int i = 0; i < m_mesh->getSubCount(); ++i) {
        m_accelerators.push_back(new TriTraceAccelerator_Base());
    }
}
Traceable_Mesh::~Traceable_Mesh()
{
}
bool Traceable_Mesh::intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag)
{
    bool b = false;
    for (int i = 0; i < (int)m_accelerators.size(); ++i) {
        if (m_accelerators[i]->intersect(r, intersectFace, t, frag)) {
            b = true;
        }
    }
    return b;
}
bool Traceable_Mesh::intersectSimply(const Ray& r, int intersectFace, float &t)
{
    bool b = false;
    for (int i = 0; i < (int)m_accelerators.size(); ++i) {
        if (m_accelerators[i]->intersectSimply(r, intersectFace, t)) {
            b = true;
        }
    }
    return b;
}
bool Traceable_Mesh::intersectTest(const Ray& r)
{
    for (int i = 0; i < m_mesh->getSubCount(); ++i) {
        if (m_accelerators[i]->intersectTest(r)) return true;
    }
    return false;
}
void Traceable_Mesh::applyTransform(const Matrix4x4& mat)
{
    for (int i = 0; i < m_mesh->getSubCount(); ++i) {
        m_accelerators[i]->rebuild(m_mesh->sub(i), mat);
    }
}
void Traceable_Mesh::printStream(std::ostream& so) const
{
    StreamBlockWriter w("Mesh", so);
    w.write("name", m_mesh->getFileName().c_str());
    so << m_accelerators[0];
}
void Traceable_Mesh::scanStream(std::istream& si)
{
    StreamBlockReader r("Mesh", si);

    std::string name;
    if (!r.read("name", name)) assert(0);
    m_mesh = MeshManager::instance()->getMesh(name);

    for (int i = 0; i < (int)m_accelerators.size(); ++i) {
        sdelete(m_accelerators[i]);
    }
    m_accelerators.resize(m_mesh->getSubCount(), 0);
    si >> m_accelerators[0];
    for (int i = 1; i < (int)m_accelerators.size(); ++i) {
        m_accelerators[i] = m_accelerators[0]->clone();
    }
}
