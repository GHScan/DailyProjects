// vim: fileencoding=gbk

#ifndef ENTITY_H
#define ENTITY_H

#include <string>
#include <iostream>

#include "SceneObj.h"
#include "Geometry.h"

struct ITraceable;
class KeyFrameMesh;
class SkeletonMesh;

//----------------------------------------
// Entity
//----------------------------------------
class Entity:
    public SceneObj
{
public:
    Entity(E_SceneObjType t, const std::string& name);
    virtual ~Entity() = 0{}

    const std::string name() const;

    virtual ITraceable* getTraceable() = 0;
    virtual const ITraceable* getTraceable() const = 0;
    virtual void notifyCameraSpaceChanged(const Matrix4x4& viewMat) = 0;
    
    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    std::string m_name;
};

class StaticEntity:
    public Entity
{
public:
    StaticEntity(const std::string& name, ITraceable* traceable);
    ~StaticEntity();

    virtual ITraceable* getTraceable();
    virtual const ITraceable* getTraceable() const;
    virtual void notifyCameraSpaceChanged(const Matrix4x4& viewMat);

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    ITraceable *m_traceable;
};

#endif // #ifndef ENTITY_H
