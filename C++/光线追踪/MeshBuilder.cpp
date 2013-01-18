// vim: fileencoding=gbk
#include "pch.h"

#include <cmath>

#include <fstream>

#include "MeshBuilder.h"
#include "Mesh.h"
#include "Geometry.h"
#include "Serialize.h"

//----------------------------------------
// MeshBuilder
//----------------------------------------
MeshBuilder::MeshBuilder(const std::string& name):
    m_mesh(new Mesh()), m_sub(NULL)
{
    m_mesh->setFileName(name);
}
MeshBuilder::~MeshBuilder()
{
    assert(m_sub == NULL && m_mesh == NULL);
}

void MeshBuilder::beginSubMesh(const Material& mat)
{
    assert(m_sub == NULL);
    m_sub = new SubMesh();
    m_sub->mat = mat;
}
void MeshBuilder::normal(float x, float y, float z)
{
    m_lastNorm[0] = x; m_lastNorm[1] = y; m_lastNorm[2] = z;
    m_sub->vertexBuffer.addElement<EVEI_normal>(Vector3(x, y, z));
    m_sub->vertexBuffer.setVertexType(
            m_sub->vertexBuffer.getVertexType() | EVET_normal);
}
void MeshBuilder::texcoord(float u, float v)
{
    m_lastUV[0] = u; m_lastUV[1] = v;
    m_sub->vertexBuffer.addElement<EVEI_texCoord>(Vector2(u, v));
    m_sub->vertexBuffer.setVertexType(
            m_sub->vertexBuffer.getVertexType() | EVET_texCoord);
}
void MeshBuilder::color(float r, float g, float b)
{
    m_lastClr[0] = r; m_lastClr[1] = g; m_lastClr[2] = b;
    m_sub->vertexBuffer.addElement<EVEI_color>(Vector3(r, g, b));
    m_sub->vertexBuffer.setVertexType(
            m_sub->vertexBuffer.getVertexType() | EVET_color);
}
void MeshBuilder::position(float x, float y, float z)
{
    m_sub->vertexBuffer.addElement<EVEI_position>(Vector3(x, y, z));
    m_sub->vertexBuffer.setVertexType(
            m_sub->vertexBuffer.getVertexType() | EVET_position);

    int vCnt = m_sub->vertexBuffer.getVertexCount();
    if (m_sub->vertexBuffer.getVertexType() & EVET_normal &&
            (int)m_sub->vertexBuffer.getElementList3<EVEI_normal>().size() < vCnt) {
        normal(m_lastNorm[0], m_lastNorm[1], m_lastNorm[2]);
    }
    if (m_sub->vertexBuffer.getVertexType() & EVET_color &&
            (int)m_sub->vertexBuffer.getElementList3<EVEI_color>().size() < vCnt) {
        color(m_lastClr[0], m_lastClr[1], m_lastClr[2]);
    }
    if (m_sub->vertexBuffer.getVertexType() & EVET_texCoord &&
            (int)m_sub->vertexBuffer.getElementList2<EVEI_texCoord>().size() < vCnt) {
        texcoord(m_lastUV[0], m_lastUV[1]);
    }
}
void MeshBuilder::triangle(int i, int j, int k)
{
    int vCnt = m_sub->vertexBuffer.getVertexCount();
    if (i < 0) i = vCnt + i;
    if (j < 0) j = vCnt + j;
    if (k < 0) k = vCnt + k;
    m_sub->indexBuffer.addTriangle(IndexTriangle(i, j, k));
}
void MeshBuilder::endSubMesh()
{
    int vCnt = m_sub->vertexBuffer.getVertexCount();
    // 当没有索引时自动生成
    if (m_sub->indexBuffer.getTriangleCount() == 0 && vCnt > 0) {
        assert(vCnt % 3 == 0);
        for (int i = 0; i < vCnt; i += 3) {
            m_sub->indexBuffer.addTriangle(
                    IndexTriangle(i, i + 1, i + 2));
        }
    }
    m_mesh->addSubMesh(m_sub);
    m_sub = NULL;
}
const Mesh* MeshBuilder::mesh() const
{
    return m_mesh;
}

Mesh* MeshBuilder::detachMesh()
{
    assert(m_sub == NULL && m_mesh != NULL);
    Mesh* r = m_mesh;
    m_mesh = NULL;
    return r;
}
//----------------------------------------
// MeshCreateFuncs
//----------------------------------------
// 这个sideMask纯粹是用来优化三角形数量的，保证只创建必须的面
void _mengerSpongeInitSideMasks(int sideMasks[3][3][3], int parentSideMask)
{
    int sideOff[][3] = {
        {1, 0, 0}, {-1, 0, 0}, {0, 1, 0}, {0, -1, 0}, {0, 0, 1}, {0, 0, -1},
    };
    for (int x = 0; x < 3; ++x) {
        for (int y = 0; y < 3; ++y) {
            for (int z = 0; z < 3; ++z) {
                for (int i = 0; i < 6; ++i) {
                    int _x = x + sideOff[i][0];
                    int _y = y + sideOff[i][1];
                    int _z = z + sideOff[i][2];
                    int bitMask = 0;
                    if (_x < 0 || _x > 2 || 
                            _y < 0 || _y > 2 || 
                            _z < 0 || _z > 2) {
                        bitMask = (parentSideMask >> i) & 1;
                    }
                    else {
                        bitMask = ((_x == 1) + (_y == 1) + (_z == 1)) >= 2;
                    }
                    if (bitMask) sideMasks[x][y][z] |= 1 << i;
                    else sideMasks[x][y][z] &= ~(1 << i);
                }
            }
        }
    }
}
void _mengerSpongeBuild(MeshBuilder* builder, 
        int maxDepth, int depth, const AABB& aabb, int parentSideMask)
{
    if (depth >= maxDepth) {
        // +x
        if (parentSideMask & 1) {
            builder->normal(1, 0, 0);
            builder->position(aabb.maxPt.x, aabb.minPt.y, aabb.minPt.z);
            builder->position(aabb.maxPt.x, aabb.maxPt.y, aabb.minPt.z);
            builder->position(aabb.maxPt.x, aabb.maxPt.y, aabb.maxPt.z);
            builder->position(aabb.maxPt.x, aabb.minPt.y, aabb.maxPt.z);
            builder->triangle(-4, -3, -2);
            builder->triangle(-4, -2, -1);
        }
        // -x
        if (parentSideMask & 2) {
            builder->normal(-1, 0, 0);
            builder->position(aabb.minPt.x, aabb.minPt.y, aabb.minPt.z);
            builder->position(aabb.minPt.x, aabb.maxPt.y, aabb.minPt.z);
            builder->position(aabb.minPt.x, aabb.maxPt.y, aabb.maxPt.z);
            builder->position(aabb.minPt.x, aabb.minPt.y, aabb.maxPt.z);
            builder->triangle(-4, -2, -3);
            builder->triangle(-4, -1, -2);
        }
        // +y
        if (parentSideMask & 4) {
            builder->normal(0, 1, 0);
            builder->position(aabb.minPt.x, aabb.maxPt.y, aabb.minPt.z);
            builder->position(aabb.minPt.x, aabb.maxPt.y, aabb.maxPt.z);
            builder->position(aabb.maxPt.x, aabb.maxPt.y, aabb.maxPt.z);
            builder->position(aabb.maxPt.x, aabb.maxPt.y, aabb.minPt.z);
            builder->triangle(-4, -3, -2);
            builder->triangle(-4, -2, -1);
        }
        // -y
        if (parentSideMask & 8) {
            builder->normal(0, -1, 0);
            builder->position(aabb.minPt.x, aabb.minPt.y, aabb.minPt.z);
            builder->position(aabb.minPt.x, aabb.minPt.y, aabb.maxPt.z);
            builder->position(aabb.maxPt.x, aabb.minPt.y, aabb.maxPt.z);
            builder->position(aabb.maxPt.x, aabb.minPt.y, aabb.minPt.z);
            builder->triangle(-4, -2, -3);
            builder->triangle(-4, -1, -2);
        }
        // +z
        if (parentSideMask & 16) {
            builder->normal(0, 0, 1);
            builder->position(aabb.minPt.x, aabb.minPt.y, aabb.maxPt.z);
            builder->position(aabb.minPt.x, aabb.maxPt.y, aabb.maxPt.z);
            builder->position(aabb.maxPt.x, aabb.maxPt.y, aabb.maxPt.z);
            builder->position(aabb.maxPt.x, aabb.minPt.y, aabb.maxPt.z);
            builder->triangle(-4, -2, -3);
            builder->triangle(-4, -1, -2);
        }
        // -z
        if (parentSideMask & 32) {
            builder->normal(0, 0, -1);
            builder->position(aabb.minPt.x, aabb.minPt.y, aabb.minPt.z);
            builder->position(aabb.minPt.x, aabb.maxPt.y, aabb.minPt.z);
            builder->position(aabb.maxPt.x, aabb.maxPt.y, aabb.minPt.z);
            builder->position(aabb.maxPt.x, aabb.minPt.y, aabb.minPt.z);
            builder->triangle(-4, -3, -2);
            builder->triangle(-4, -2, -1);
        }
        return ;
    }
    float subSize = (aabb.maxPt.x - aabb.minPt.x) / 3;
    int sideMasks[3][3][3] = {0};
    _mengerSpongeInitSideMasks(sideMasks, parentSideMask);
    for (int x = 0; x < 3; ++x) {
        for (int y = 0; y < 3; ++y) {
            for (int z = 0; z < 3; ++z) {
                if (((x == 1) + (y == 1) + (z == 1)) < 2) {
                    Vector3 minPt(aabb.minPt);
                    minPt += Vector3(x * subSize, y * subSize, z * subSize);
                    _mengerSpongeBuild(
                            builder, maxDepth, depth + 1, 
                            AABB(minPt, minPt + Vector3(subSize)), sideMasks[x][y][z]);
                }
            }
        }
    }
}

Mesh* meshCreater_MengerSponge()
{
    int maxDepth = 1;
    Material mat;
    {
        std::ifstream fi("meshConfig_MengerSponge.txt");
        assert(fi && "meshConfig_MengerSponge.txt 文件不存在！！");
        StreamBlockReader r("MengerSponge", fi);
        {
            if (!r.read("maxDepth", &maxDepth)) assert(0);
            fi >> mat;
        }
    }

    MeshBuilder builder("MengerSponge");
    builder.beginSubMesh(mat);
    assert(maxDepth >= 1);
    float size = pow(3.f, maxDepth - 1);
    _mengerSpongeBuild(&builder, maxDepth, 0, 
            AABB(Vector3(-1) * size, Vector3(1) * size), 63);
    builder.endSubMesh();
    Mesh *p = builder.detachMesh();
    p->printInfo(stdout);
    return p;
}

//----------------------------------------
// BuildinMeshManager
//----------------------------------------

BuildinMeshManager* BuildinMeshManager::instance()
{
    static BuildinMeshManager s_ins;
    return &s_ins;
}
Mesh* BuildinMeshManager::getMesh(const std::string& name)
{
    if (m_map.count(name)) {
        return m_map[name];
    }
    if (!m_meshCreateFuncMap.count(name)) return NULL;
    Mesh *m = m_meshCreateFuncMap[name]();
    return m_map[name] = m;
}

BuildinMeshManager::BuildinMeshManager()
{
    m_meshCreateFuncMap["MengerSponge"] = &meshCreater_MengerSponge;
}
BuildinMeshManager::~BuildinMeshManager()
{
    while (!m_map.empty()) {
        delete m_map.begin()->second;
        m_map.erase(m_map.begin());
    }
    m_meshCreateFuncMap.clear();
}
