// vim: fileencoding=gbk

#ifndef ENTITY_H
#define ENTITY_H

#include <string>
#include <iostream>

#include "SceneObj.h"
#include "Geometry.h"

class Mesh;
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
    const std::string name() const;

    virtual ~Entity() = 0{}
    virtual AABB getBoundAABB() const = 0;
    virtual Sphere getBoundSphere() const = 0;
    
    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    std::string m_name;
};

class StaticEntity:
    public Entity
{
public:
    StaticEntity(const std::string& name, const std::string& meshFile);
    const Mesh* getMesh() const;
    virtual AABB getBoundAABB() const;
    virtual Sphere getBoundSphere() const;

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    const Mesh* m_mesh;
};

class KeyFrameEntity:
    public Entity
{
public:
    KeyFrameEntity(const std::string& name, const KeyFrameMesh* mesh);
    const KeyFrameMesh* getMesh() const;
    virtual AABB getBoundAABB() const;
    virtual Sphere getBoundSphere() const;

    void toggleAnimation(int animIdx);
    void updateAnimation(float f);

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    KeyFrameMesh    *m_mesh;
};

class SkeletonEntity:
    public Entity
{
public:
    SkeletonEntity(const std::string& name, const SkeletonMesh *mesh);
    const SkeletonMesh* getMesh() const;
    virtual AABB getBoundAABB() const;
    virtual Sphere getBoundSphere() const;

    void toggleAnimation(int animIdx);
    void updateAnimation(float f);

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    const SkeletonMesh    *m_mesh;
};


#endif // #ifndef ENTITY_H
