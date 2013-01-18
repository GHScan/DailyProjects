// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>
#include <cmath>

#include <fstream>

#include "Mesh.h"
#include "Serialize.h"
#include "Matrix.h"
#include "Geometry.h"

void VertexBuffer::resizeElementList(int n) const
{
    ((VertexBuffer*)this)->_resizeElementList(n);
}
void VertexBuffer::_resizeElementList(int n)
{
    for (int i = EVEI_FRONT; i <= EVEI_BACK; ++i) {
        if (!m_v2sArray[i].empty()) {
            m_v2sArray[i].resize(n);
        }
        if (!m_v3sArray[i].empty()) {
            m_v3sArray[i].resize(n);
        }
    }
}

void IndexBuffer::resizeTriangleList(int n) const
{
    ((IndexBuffer*)this)->_resizeTriangleList(n);
}
void IndexBuffer::_resizeTriangleList(int n)
{
    m_triangles.resize(n);
}

void SubMesh::genTriangleNormals() const
{
    if (!triangleNormals.empty()) return;
    assert(indexBuffer.getTriangleCount() > 0);

    const std::vector<Vector3>& posBuf = 
        vertexBuffer.getElementList3<EVEI_position>();
    for (int i = 0; i < indexBuffer.getTriangleCount(); ++i) {
        const IndexTriangle& tri = indexBuffer.triangle(i);
        Vector3 norm = (posBuf[tri.v2] - posBuf[tri.v1]).crossProduct(
                (posBuf[tri.v0] - posBuf[tri.v1]));
        triangleNormals.push_back(norm.normalize());
    }
}

void SubMesh::genVertexNormals()
{
    if (vertexBuffer.getVertexType() & EVET_normal) return;
    assert(indexBuffer.getTriangleCount() > 0);

    std::vector<Vector3> normals(vertexBuffer.getVertexCount(), Vector3::ZERO);

    const std::vector<Vector3>& posBuf = 
        vertexBuffer.getElementList3<EVEI_position>();
    for (int i = 0; i < indexBuffer.getTriangleCount(); ++i) {
        const IndexTriangle& tri = indexBuffer.triangle(i);
        Vector3 norm = (posBuf[tri.v2] - posBuf[tri.v1]).crossProduct(
                (posBuf[tri.v0] - posBuf[tri.v1]));
        // 故意不规范化, 加上的是法线的面积权重向量
        normals[tri.v0] += norm;
        normals[tri.v1] += norm;
        normals[tri.v2] += norm;
    }

    for (int i = 0; i < (int)normals.size(); ++i) {
        vertexBuffer.addElement<EVEI_normal>(normals[i].normalize());
    }
    vertexBuffer.setVertexType(vertexBuffer.getVertexType() | EVET_normal);
}

void SubMesh::genTexcoords(const Matrix4x4& mat)
{
    if (vertexBuffer.getVertexType() & EVET_texCoord) return;
    for (int i = 0; i < vertexBuffer.getVertexCount(); ++i) {
        Vector4 v(vertexBuffer.element3<EVEI_position>(i));
        transform(v, mat);
        vertexBuffer.addElement<EVEI_texCoord>((Vector2&)v);
    }
    vertexBuffer.setVertexType(vertexBuffer.getVertexType() | EVET_texCoord);
}

Mesh::Mesh():
    m_boundAABB(NULL), m_boundSphere(NULL)
{}

Mesh::Mesh(const std::string& fname):
    m_boundAABB(NULL), m_boundSphere(NULL)
{
    bool b = load(fname);
    assert(b);
}
Mesh::~Mesh() { clear(); }

const std::string Mesh::getFileName() const { return m_fname; }
bool Mesh::load(const std::string& fname)
{
    std::ifstream fi(fname.c_str());
    assert(fi);
    int subCnt = 0;
    {
        StreamBlockReader r("MeshDescription", fi);
        if (!r.read("subCount", &subCnt)) assert(0);
    }
    while (subCnt-- > 0) {
        SubMesh *sub = new SubMesh();
        fi >> *sub;
        sub->genVertexNormals();
        sub->genTriangleNormals();
        addSubMesh(sub);
    }
    m_fname = fname;
    return !!fi;
}
void Mesh::save(const std::string& fname) const
{
    std::ofstream fo(fname.c_str());
    {
        StreamBlockWriter w("MeshDescription", fo);
        w.write("subCount", getSubCount());
    }
    for (int i = 0; i < getSubCount(); ++i) {
        fo << *sub(i);
    }
}
int Mesh::getSubCount() const
{
    return (int)m_subMeshs.size();
}
const SubMesh* Mesh::sub(int i) const
{
    return m_subMeshs[i];
}
void Mesh::addSubMesh(SubMesh* sub)
{
    return m_subMeshs.push_back(sub);
}
void Mesh::clear()
{
    for (int i = 0; i < (int)m_subMeshs.size(); ++i) {
        delete m_subMeshs[i];
    }
    m_subMeshs.clear();
    m_fname.clear();
    sdelete(m_boundAABB);
    sdelete(m_boundSphere);
}
void Mesh::setBoundAABB(const AABB& aabb)
{
    sdelete(m_boundAABB);
    m_boundAABB = new AABB(aabb);
}
AABB Mesh::getBoundAABB() const
{
    if (m_boundAABB == NULL) calcBoundAABB();
    return *m_boundAABB;
}
void Mesh::setBoundSphere(const Sphere& sphere)
{
    sdelete(m_boundSphere);
    m_boundSphere = new Sphere(sphere);
}
Sphere Mesh::getBoundSphere() const
{
    if (m_boundSphere == NULL) calcBoundSphere();
    return *m_boundSphere;
}
void Mesh::calcBoundAABB() const
{
    if (m_subMeshs.empty()) return;
    sdelete(m_boundAABB);

    for (int i = 0; i < (int)m_subMeshs.size(); ++i) {
        const SubMesh* sub = m_subMeshs[i];
        int vCnt = sub->vertexBuffer.getVertexCount();
        assert(vCnt > 0);
        const Vector3* p = &sub->vertexBuffer.getElementList3<EVEI_position>()[0];
        if (m_boundAABB == NULL) {
            m_boundAABB = new AABB(AABB::fromPoints(p, p + vCnt));
        }
        else m_boundAABB->merge(AABB::fromPoints(p, p + vCnt));
    }
}
void Mesh::calcBoundSphere() const
{
    if (m_subMeshs.empty()) return;
    sdelete(m_boundSphere);

    for (int i = 0; i < (int)m_subMeshs.size(); ++i) {
        const SubMesh* sub = m_subMeshs[i];
        int vCnt = sub->vertexBuffer.getVertexCount();
        assert(vCnt > 0);
        const Vector3* p = &sub->vertexBuffer.getElementList3<EVEI_position>()[0];
        if (m_boundSphere == NULL) {
            m_boundSphere = new Sphere(Sphere::fromPoints(p, p + vCnt));
        }
        else m_boundSphere->merge(Sphere::fromPoints(p, p + vCnt));
    }
}

MeshManager* MeshManager::instance()
{
    static MeshManager s_mgr;
    return &s_mgr;
}
MeshManager::MeshManager()
{
}
MeshManager::~MeshManager()
{
    for (MeshMap::iterator iter = m_meshMap.begin();
            iter != m_meshMap.end(); ++iter) {
        delete iter->second;
    }
    m_meshMap.clear();
}

const Mesh* MeshManager::getMesh(const std::string& fname)
{
    assert(!fname.empty());
    MeshMap::const_iterator iter = m_meshMap.find(fname);
    if (iter != m_meshMap.end()) {
        return iter->second;
    }
    Mesh *p = new Mesh(fname);
    return m_meshMap[fname] = p;
}
const KeyFrameMesh* MeshManager::getKeyFrameMesh(const std::string& fname)
{
    return NULL;
}
const SkeletonMesh* MeshManager::getSkeletonMesh(const std::string& fname)
{
    return NULL;
}

//----------------------------------------
// 序列化相关
//----------------------------------------
namespace 
{

void printList(std::ostream& so, const char *name, const std::vector<Vector2>& v)
{
    StreamBlockWriter w(name, so);
    w.write("length", (int)v.size());
    for (int i = 0; i < (int)v.size(); ++i) {
        const Vector2& _v = v[i];
        w.write("v", _v.data(), _v.data() + 2);
    }
}
void printList(std::ostream& so, const char* name, const std::vector<Vector3>& v)
{
    StreamBlockWriter w(name, so);
    w.write("length", (int)v.size());
    for (int i = 0; i < (int)v.size(); ++i) {
        const Vector3& _v = v[i];
        w.write("v", _v.data(), _v.data() + 3);
    }
}
void scanList(std::istream& si, const char *name, std::vector<Vector2>& v)
{
    v.clear();
    StreamBlockReader r(name, si);
    int len;
    if (!r.read("length", &len)) assert(0);
    for (int i = 0; i < len; ++i) {
        Vector2 _v;
        if (!r.read("v", _v.data(), _v.data() + 2)) assert(0);
        v.push_back(_v);
    }
}
void scanList(std::istream& si, const char *name, std::vector<Vector3>& v)
{
    v.clear();
    StreamBlockReader r(name, si);
    int len;
    if (!r.read("length", &len)) assert(0);
    for (int i = 0; i < len; ++i) {
        Vector3 _v;
        if (!r.read("v", _v.data(), _v.data() + 3)) assert(0);
        v.push_back(_v);
    }
}

}

std::ostream& operator << (std::ostream& so, const VertexBuffer& vBuffer)
{
    StreamBlockWriter w("VertexBuffer", so);
    w.write("vertexType", vBuffer.getVertexType());
    if (vBuffer.getVertexType() & EVET_position)
    printList(so, "position", vBuffer.getElementList3<EVEI_position>());
    if (vBuffer.getVertexType() & EVET_normal)
    printList(so, "normal", vBuffer.getElementList3<EVEI_normal>());
    if (vBuffer.getVertexType() & EVET_color)
    printList(so, "color", vBuffer.getElementList3<EVEI_color>());
    if (vBuffer.getVertexType() & EVET_texCoord)
    printList(so, "texCoord", vBuffer.getElementList2<EVEI_texCoord>());
    return so;
}
std::istream& operator >> (std::istream& si, VertexBuffer& vBuffer)
{
    StreamBlockReader r("VertexBuffer", si);
    int vt;
    if (!r.read("vertexType", &vt)) assert(0);
    assert(vt & EVET_position);
    vBuffer.setVertexType(vt);

    std::vector<Vector3> t3;
    std::vector<Vector2> t2;
    if (vBuffer.getVertexType() & EVET_position) {
        scanList(si, "position", t3);
        vBuffer.addElementList<EVEI_position>(&t3[0], &t3[0] + t3.size());
    }
    if (vBuffer.getVertexType() & EVET_normal) {
        scanList(si, "normal", t3);
        vBuffer.addElementList<EVEI_normal>(&t3[0], &t3[0] + t3.size());
    }
    if (vBuffer.getVertexType() & EVET_color) {
        scanList(si, "color", t3);
        vBuffer.addElementList<EVEI_color>(&t3[0], &t3[0] + t3.size());
    }
    if (vBuffer.getVertexType() & EVET_texCoord) {
        scanList(si, "texCoord", t2);
        vBuffer.addElementList<EVEI_texCoord>(&t2[0], &t2[0] + t2.size());
    }
    return si;
}
std::ostream& operator << (std::ostream& so, const IndexBuffer& idxBuffer)
{
    StreamBlockWriter w("IndexBuffer", so);
    w.write("length", idxBuffer.getTriangleCount());
    for (int i = 0; i < idxBuffer.getTriangleCount(); ++i) {
        IndexTriangle tri = idxBuffer.triangle(i);
        w.write("t", tri.data(), tri.data() + 3);
    }
    return so;
}
std::istream& operator >> (std::istream& si, IndexBuffer& idxBuffer)
{
    StreamBlockReader r("IndexBuffer", si);
    int len;
    if (!r.read("length", &len)) assert(0);
    for (int i = 0; i < len; ++i) {
        IndexTriangle tri;
        if (!r.read("t", tri.data(), tri.data() + 3)) assert(0);
        idxBuffer.addTriangle(tri);
    }
    return si;
}
std::ostream& operator << (std::ostream& so, const Material& m)
{
    StreamBlockWriter w("Material", so);
    w.write("ambient", m.ambientClr.data(), m.ambientClr.data() + 3);
    w.write("diffuse", m.diffuseClr.data(), m.diffuseClr.data() + 3);
    w.write("specular", m.specularClr.data(), m.specularClr.data() + 3);
    w.write("emissive", m.emissiveClr.data(), m.emissiveClr.data() + 3);
    w.write("power", m.power);
    w.write("texture", m.texture.c_str());
    return so;
}
std::istream& operator >> (std::istream& si, Material& m)
{
    StreamBlockReader r("Material", si);
    if (!r.read("ambient", m.ambientClr.data(), m.ambientClr.data() + 3)) assert(0);
    if (!r.read("diffuse", m.diffuseClr.data(), m.diffuseClr.data() + 3)) assert(0);
    if (!r.read("specular", m.specularClr.data(), m.specularClr.data() + 3)) assert(0);
    if (!r.read("emissive", m.emissiveClr.data(), m.emissiveClr.data() + 3)) assert(0);
    if (!r.read("power", &m.power)) assert(0);
    if (!r.read("texture", m.texture)) assert(0);
    return si;
}
std::ostream& operator << (std::ostream& so, const SubMesh& sub)
{
    StreamBlockWriter w("SubMesh", so);
    so << sub.mat;
    so << sub.vertexBuffer;
    so << sub.indexBuffer;
    return so;
}
std::istream& operator >> (std::istream& si, SubMesh& sub)
{
    StreamBlockReader r("SubMesh", si);
    si >> sub.mat;
    si >> sub.vertexBuffer;
    si >> sub.indexBuffer;
    return si;
}
