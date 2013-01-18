// vim: fileencoding=gbk

#ifndef MODEL_H
#define MODEL_H

#include <cassert>

#include <vector>
#include <string>
#include <iostream>
#include <map>

#include "Vector.h"

struct Sphere;
struct AABB;
struct Matrix4x4;

//----------------------------------------
// 顶点数据
//----------------------------------------
enum E_VertexElementIndex
{
    EVEI_position,
    EVEI_normal,
    EVEI_color,
    EVEI_texCoord,
    EVEI_COUNT,
    EVEI_FRONT = EVEI_position,
    EVEI_BACK = EVEI_texCoord,
};

enum E_VertexElementType
{
    EVET_position = 1,
    EVET_normal = 2,
    EVET_color = 4,
    EVET_texCoord = 8,
};

class VertexBuffer
{
public:
    explicit VertexBuffer(int vertexType = EVET_position);
    void setVertexType(int vertexType);
    int getVertexType() const;

    int getVertexCount() const;

    template<E_VertexElementIndex eIdx>
    void addElement(const Vector2& val);
    template<E_VertexElementIndex eIdx>
    void addElement(const Vector3& val);

    template<E_VertexElementIndex eIdx>
    void addElementList(const Vector2* begin, const Vector2* end);
    template<E_VertexElementIndex eIdx>
    void addElementList(const Vector3* begin, const Vector3* end);

    void clear();
    template<E_VertexElementIndex eIdx>
    void clear();

    template<E_VertexElementIndex eIdx>
    const Vector2& element2(int i) const;
    template<E_VertexElementIndex eIdx>
    const Vector3& element3(int i) const;

    template<E_VertexElementIndex eIdx>
    const std::vector<Vector2>& getElementList2() const;
    template<E_VertexElementIndex eIdx>
    const std::vector<Vector3>& getElementList3() const;

    void resizeElementList(int n) const; // 一个特殊接口；因为性能存在
private:
    void _resizeElementList(int n);

private:
    std::vector<Vector2> m_v2sArray[EVEI_COUNT];
    std::vector<Vector3> m_v3sArray[EVEI_COUNT];
    int m_vertexType;
};
std::ostream& operator << (std::ostream& so, const VertexBuffer& vBuffer);
std::istream& operator >> (std::istream& si, VertexBuffer& vBuffer);

struct IndexTriangle
{
    int v0, v1, v2;
    IndexTriangle(int _v0, int _v1, int _v2): v0(_v0), v1(_v1), v2(_v2){}
    IndexTriangle(): v0(0), v1(0), v2(0){}
    const int* data() const { return &v0; }
    int* data() { return (int*)((const IndexTriangle*)this)->data(); }
};

class IndexBuffer
{
public:
    int getTriangleCount() const;

    void addTriangle(const IndexTriangle& tri);
    void addTriangleList(const IndexTriangle* begin, const IndexTriangle* end);

    void clear();

    const IndexTriangle& triangle(int i) const;
    const std::vector<IndexTriangle>& getTriangleList() const;

    void resizeTriangleList(int n) const; // 为性能存在的接口
private:
    void _resizeTriangleList(int n);

private:
    std::vector<IndexTriangle> m_triangles;
};
std::ostream& operator << (std::ostream& so, const IndexBuffer& idxBuffer);
std::istream& operator >> (std::istream& si, IndexBuffer& idxBuffer);


inline VertexBuffer::VertexBuffer(int vertexType): m_vertexType(vertexType){}
inline void VertexBuffer::setVertexType(int vertexType) { m_vertexType = vertexType; }
inline int VertexBuffer::getVertexType() const { return m_vertexType;}

inline int VertexBuffer::getVertexCount() const { return (int)m_v3sArray[EVEI_position].size(); }

template<E_VertexElementIndex eIdx>
inline void VertexBuffer::addElement(const Vector2& val) { m_v2sArray[eIdx].push_back(val);}
template<E_VertexElementIndex eIdx>
inline void VertexBuffer::addElement(const Vector3& val) { m_v3sArray[eIdx].push_back(val);}

template<E_VertexElementIndex eIdx>
inline void VertexBuffer::addElementList(const Vector2* begin, const Vector2* end)
{
    m_v2sArray[eIdx].insert(m_v2sArray[eIdx].end(), begin, end);
}
template<E_VertexElementIndex eIdx>
inline void VertexBuffer::addElementList(const Vector3* begin, const Vector3* end)
{
    m_v3sArray[eIdx].insert(m_v3sArray[eIdx].end(), begin, end);
}

inline void VertexBuffer::clear() 
{
    for (int i = EVEI_FRONT; i <= EVEI_BACK; ++i) {
        m_v2sArray[i].clear();
        m_v3sArray[i].clear();
    }
}
template<E_VertexElementIndex eIdx>
inline void VertexBuffer::clear()
{
    m_v2sArray[eIdx].clear();
    m_v3sArray[eIdx].clear();
}

template<E_VertexElementIndex eIdx>
inline const Vector2& VertexBuffer::element2(int i) const { return m_v2sArray[eIdx][i];}
template<E_VertexElementIndex eIdx>
inline const Vector3& VertexBuffer::element3(int i) const  { return m_v3sArray[eIdx][i];}
template<E_VertexElementIndex eIdx>
inline const std::vector<Vector2>& VertexBuffer::getElementList2() const { return m_v2sArray[eIdx];}
template<E_VertexElementIndex eIdx>
inline const std::vector<Vector3>& VertexBuffer::getElementList3() const { return m_v3sArray[eIdx];}

inline int IndexBuffer::getTriangleCount() const { return (int)m_triangles.size(); }

inline void IndexBuffer::addTriangle(const IndexTriangle& tri) { m_triangles.push_back(tri); }
inline void IndexBuffer::addTriangleList(const IndexTriangle* begin, const IndexTriangle* end)
{
    m_triangles.insert(m_triangles.end(), begin, end);
}

inline void IndexBuffer::clear() { m_triangles.clear(); }

inline const IndexTriangle& IndexBuffer::triangle(int i) const { return m_triangles[i]; }
inline const std::vector<IndexTriangle>& IndexBuffer::getTriangleList() const { return m_triangles; }
//----------------------------------------
// 模型数据
//----------------------------------------

struct Material
{
    Vector3 ambientClr;
    Vector3 diffuseClr;
    Vector3 specularClr;
    Vector3 emissiveClr;
    float power;
    std::string texture;

    Material():
        ambientClr(0.0), diffuseClr(0.0), specularClr(0.0), emissiveClr(0.0), power(0){}
};
std::ostream& operator << (std::ostream& so, const Material& material);
std::istream& operator >> (std::istream& si, Material& material);

struct SubMesh
{
    Material mat;
    VertexBuffer vertexBuffer;
    IndexBuffer indexBuffer;
    mutable std::vector<Vector3> triangleNormals;
    void genTriangleNormals() const;
    void genVertexNormals();
    void genTexcoords(const Matrix4x4& mat);
};
std::ostream& operator << (std::ostream& so, const SubMesh& sub);
std::istream& operator >> (std::istream& si, SubMesh& sub);

class Mesh
{
public:
    Mesh();
    Mesh(const std::string& fname);
    ~Mesh();

    const std::string getFileName() const;
    bool load(const std::string& fname);
    void save(const std::string& fname) const;

    int getSubCount() const;
    const SubMesh* sub(int i) const;
    void addSubMesh(SubMesh* sub);
    void clear();

    void setBoundAABB(const AABB& aabb);
    AABB getBoundAABB() const;
    void setBoundSphere(const Sphere& sphere);
    Sphere getBoundSphere() const;

private:
    void calcBoundAABB() const;
    void calcBoundSphere() const;

private:
    std::vector<SubMesh*> m_subMeshs;
    std::string m_fname;
    mutable AABB *m_boundAABB;
    mutable Sphere* m_boundSphere;
};

class KeyFrameMesh
{
public:
    KeyFrameMesh();
    KeyFrameMesh(const std::string& fname);

    const std::string getFileName() const;
    bool load(const std::string& fname);
    void save(const std::string& fname) const;

    int getAnimationCount() const;
    std::string getAnimationName(int animIdx) const;
    int getAniamtionFrameCount(int animIdx) const;
    const Mesh* getKeyFrameMesh(int animIdx, int frameId) const;

private:
    std::string m_fname;
};

class SkeletonAnimation{};
class SkeletonAnimationList{};

class SkeletonMesh
{
public:
    SkeletonMesh();
    SkeletonMesh(const std::string& fname);

    const std::string getFileName() const;
    bool load(const std::string& fname);
    void save(const std::string& fname) const;

    const Mesh* getMesh() const;

    int getAnimationCount() const;
    std::string getAnimationName(int animIdx) const;
    const SkeletonAnimation* getAnimation(int animIdx) const;

private:
    Mesh    m_mesh;
    SkeletonAnimationList m_animList;
};

class MeshManager
{
public:
    static MeshManager* instance();
    MeshManager();
    ~MeshManager();

    const Mesh* getMesh(const std::string& fname);
    const KeyFrameMesh* getKeyFrameMesh(const std::string& fname);
    const SkeletonMesh* getSkeletonMesh(const std::string& fname);
private:
    typedef std::map<std::string, Mesh*> MeshMap;
    typedef std::map<std::string, KeyFrameMesh*> KeyFrameMeshMap;
    typedef std::map<std::string, SkeletonMesh*> SkeletonMeshMap;
private:
    MeshMap m_meshMap;
};


#endif // #ifndef MODEL_H
