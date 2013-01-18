// vim: fileencoding=gbk

#ifndef SCENEOBJ_H
#define SCENEOBJ_H

#include <vector>
#include <string>
#include <iostream>

#include "Matrix.h"
#include "Vector.h"

class SceneObj;

//----------------------------------------
// SceneNode
//----------------------------------------
enum E_TransformSpace
{
    ETS_local,
    ETS_world,
};

class SceneNode
{
public:
    SceneNode();
    void translate(float x, float y, float z, E_TransformSpace space = ETS_world);
    void rotateX(float degree, E_TransformSpace space = ETS_local);
    void rotateY(float degree, E_TransformSpace space = ETS_local);
    void rotateZ(float degree, E_TransformSpace space = ETS_local);
    void scale(float x, float y, float z, E_TransformSpace space = ETS_local);

    void setWorldMatrix(const Matrix4x4& mat);
    Matrix4x4 getWorldMatrix() const;
    Vector3 getPosition() const;

    void attachSceneObj(SceneObj* obj);
    void detachSceneObj(SceneObj* obj);

    const std::vector<SceneObj*>& getAttachedObjs() const;
private:
    std::vector<SceneObj*> m_objs;
    Matrix4x4 m_worldMat;
};

//----------------------------------------
// SceneObj
//----------------------------------------
enum E_SceneObjType
{
    ESOT_entity = 0x10000,
    ESOT_camera = 0x20000,
    ESOT_light = 0x30000,

    ESOT_staticEntity = 0x10001,
    ESOT_keyFrameEntity = 0x10002,
    ESOT_skeletonEntity = 0x10003,

    ESOT_commonCamera = 0x20001,
    ESOT_perspectiveCamera = 0x20002,
    ESOT_orthoCamera = 0x20003,

    ESOT_pointLight = 0x30001,
    ESOT_directionLight = 0x30002,
    ESOT_spotLight = 0x30003,
};
inline bool isEntity(E_SceneObjType t) { return (t & 0xffff0000) == ESOT_entity; }
inline bool isCamera(E_SceneObjType t) { return (t & 0xffff0000) == ESOT_camera; }
inline bool isLight(E_SceneObjType t) { return (t & 0xffff0000) == ESOT_light; }

class SceneObj
{
public:
    SceneObj(E_SceneObjType t);
    virtual ~SceneObj() = 0;
    void notifyAttached(SceneNode* node);
    SceneNode* getSceneNode();
    const SceneNode* getSceneNode() const;
    E_SceneObjType getObjType() const;

    virtual void printStream(std::ostream& so) const = 0;
    virtual void scanStream(std::istream& si) = 0;
    static SceneObj* create(E_SceneObjType t);

private:
    SceneNode* m_node;
    E_SceneObjType m_objType;
};

//----------------------------------------
// 序列化相关
//----------------------------------------
std::ostream& operator << (std::ostream& so, const SceneNode& o);
std::istream& operator >> (std::istream& si, SceneNode& o);

std::string sceneObjType2Str(E_SceneObjType t);
E_SceneObjType str2SceneObjType(const std::string& s);

std::ostream& operator << (std::ostream& so, const SceneObj* obj);
std::istream& operator >> (std::istream& si, SceneObj*& obj);

#endif // #ifndef SCENEOBJ_H
