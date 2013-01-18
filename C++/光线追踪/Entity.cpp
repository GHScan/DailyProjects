// vim: fileencoding=gbk
#include "pch.h"

#include "Entity.h"
#include "Serialize.h"
#include "Traceable.h"
//----------------------------------------
// Entity
//----------------------------------------
Entity::Entity(E_SceneObjType t, const std::string& name):
    m_name(name), SceneObj(t)
{
}
const std::string Entity::name() const { return m_name; }

StaticEntity::StaticEntity(const std::string& name, ITraceable* traceable):
    Entity(ESOT_staticEntity, name), m_traceable(traceable)
{
}
StaticEntity::~StaticEntity()
{
    sdelete(m_traceable);
}
void StaticEntity::notifyCameraSpaceChanged(const Matrix4x4& viewMat)
{
    m_traceable->applyTransform(
            getSceneNode()->getWorldMatrix() * viewMat);
}
ITraceable* StaticEntity::getTraceable() { return m_traceable;}
const ITraceable* StaticEntity::getTraceable() const { return m_traceable;}


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
    assert(m_traceable != NULL);
    so << m_traceable;
    Entity::printStream(so);
}
void StaticEntity::scanStream(std::istream& si)
{
    StreamBlockReader r(sceneObjType2Str(ESOT_staticEntity).c_str(), si);
    sdelete(m_traceable);
    si >> m_traceable;
    Entity::scanStream(si);
}
