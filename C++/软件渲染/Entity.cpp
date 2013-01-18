// vim: fileencoding=gbk
#include "pch.h"

#include "Entity.h"
#include "Serialize.h"
#include "Mesh.h"
//----------------------------------------
// Entity
//----------------------------------------
Entity::Entity(E_SceneObjType t, const std::string& name):
    m_name(name), SceneObj(t)
{
}
const std::string Entity::name() const { return m_name; }

StaticEntity::StaticEntity(const std::string& name, const std::string& meshFile):
    Entity(ESOT_staticEntity, name), m_mesh(NULL)
{
    if (!meshFile.empty()) {
        m_mesh = MeshManager::instance()->getMesh(meshFile);
    }
}
const Mesh* StaticEntity::getMesh() const { return m_mesh; }
AABB StaticEntity::getBoundAABB() const { return m_mesh->getBoundAABB(); }
Sphere StaticEntity::getBoundSphere() const { return m_mesh->getBoundSphere(); }


void Entity::printStream(std::ostream& so) const
{
    StreamBlockWriter w(sceneObjType2Str(ESOT_entity).c_str(), so);
    w.write("name", m_name.c_str());
}
void Entity::scanStream(std::istream& si)
{
    StreamBlockReader r(sceneObjType2Str(ESOT_entity).c_str(), si);
    if(!r.read("name", m_name)) assert(0);
}
void StaticEntity::printStream(std::ostream& so) const
{
    StreamBlockWriter w(sceneObjType2Str(ESOT_staticEntity).c_str(), so);
    w.write("mesh", m_mesh == NULL ? "" : m_mesh->getFileName().c_str());
    Entity::printStream(so);
}
void StaticEntity::scanStream(std::istream& si)
{
    StreamBlockReader r(sceneObjType2Str(ESOT_staticEntity).c_str(), si);
    std::string fname;
    if (!r.read("mesh", fname)) assert(0);
    if (fname.empty()) m_mesh = NULL;
    else m_mesh = MeshManager::instance()->getMesh(fname);
    Entity::scanStream(si);
}
