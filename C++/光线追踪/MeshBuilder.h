// vim: fileencoding=gbk

#ifndef MESHBUILDER_H
#define MESHBUILDER_H

#include <string>
#include <map>

class Mesh;
struct Material;
struct SubMesh;

class MeshBuilder
{
public:
    MeshBuilder(const std::string& name);
    ~MeshBuilder();

    void beginSubMesh(const Material& mat);
    void normal(float x, float y, float z);
    void texcoord(float u, float v);
    void color(float r, float g, float b);
    void position(float x, float y, float z);
    // 可以用负数索引，表示vCnt - i
    void triangle(int i, int j, int k);
    void endSubMesh();

    const Mesh* mesh() const;
    Mesh* detachMesh();

private:
    Mesh *m_mesh;
    SubMesh* m_sub;
    float m_lastNorm[3];
    float m_lastClr[3];
    float m_lastUV[2];
};

class BuildinMeshManager
{
public:
    static BuildinMeshManager* instance();
    Mesh* getMesh(const std::string& name);

private:
    BuildinMeshManager();
    ~BuildinMeshManager();

private:
    std::map<std::string, Mesh*> m_map;
    std::map<std::string, Mesh*(*)()> m_meshCreateFuncMap;
};

#endif // #ifndef MESHBUILDER_H
