// vim: fileencoding=gbk
#include "pch.h"

#include <algorithm>

#include "SceneObj.h"
#include "Serialize.h"
#include "Light.h"
#include "Camera.h"
#include "Entity.h"
//----------------------------------------
// SceneNode
//----------------------------------------
SceneNode::SceneNode():
    m_parent(NULL), m_worldMat(Matrix4x4::IDENTITY)
{
}
SceneNode::SceneNode(const std::string &name):
    m_name(name), m_worldMat(Matrix4x4::IDENTITY), m_parent(NULL)
{
}
const char* SceneNode::name() const { return m_name.c_str(); }
const char* SceneNode::parentName() const { return m_parentName.c_str(); }
void SceneNode::setName(const char *name) { m_name = name; }
void SceneNode::setParentName(const char *name) { m_parentName = name; }
void SceneNode::setParent(SceneNode *parent) { m_parent = parent; }

void SceneNode::translate(float x, float y, float z, E_TransformSpace space)
{
    transform(Matrix4x4::fromTranslate(x, y, z), space);
}
void SceneNode::rotateX(float degree, E_TransformSpace space)
{
    transform(Matrix4x4::fromRotateX(degree), space);
}
void SceneNode::rotateY(float degree, E_TransformSpace space)
{
    transform(Matrix4x4::fromRotateY(degree), space);
}
void SceneNode::rotateZ(float degree, E_TransformSpace space)
{
    transform(Matrix4x4::fromRotateZ(degree), space);
}
void SceneNode::scale(float x, float y, float z, E_TransformSpace space)
{
    transform(Matrix4x4::fromScale(x, y, z), space);
}

void SceneNode::transform(const Matrix4x4& mat, E_TransformSpace space)
{
    switch (space) {
        case ETS_local:
            m_worldMat = mat * m_worldMat;
            break;
        case ETS_parent:
            m_worldMat *= mat;
            break;
        case ETS_world:
            setWorldMatrix(getWorldMatrix() * mat);
            break;
        default:
            assert(0);
            break;
    }
}
Vector3 SceneNode::getWorldPosition() const 
{ 
    return Vector3(getWorldMatrix()[3]);
}
Vector3 SceneNode::getScale() const
{
    Vector4 x(Vector3::AXIS_X, 0);
    ::transform(x, m_worldMat);
    Vector4 y(Vector3::AXIS_Y, 0);
    ::transform(y, m_worldMat);
    Vector4 z(Vector3::AXIS_Z, 0);
    ::transform(z, m_worldMat);
    assert(fequal(x.w, 0) && fequal(y.w, 0) && fequal(z.w, 0));
    return Vector3(x.length(), y.length(), z.length());
}
void SceneNode::setWorldMatrix(const Matrix4x4& mat) 
{
    if (m_parent == NULL) m_worldMat = mat;
    else {
        m_worldMat = mat * getParentWorldMatrixWithoutScale().inverse();
    }
}
Matrix4x4 SceneNode::getWorldMatrix() const 
{ 
    if (m_parent == NULL) return m_worldMat;
    else {
        return m_worldMat * getParentWorldMatrixWithoutScale();
    }
}
Matrix4x4 SceneNode::getParentWorldMatrixWithoutScale() const
{
    assert(m_parent != NULL);
    Vector3 scale(m_parent->getScale());
    assert(!fequal(scale.x, 0) && !fequal(scale.y, 0) && !fequal(scale.z, 0));
    return Matrix4x4::fromScale(
            1 / scale.x, 1 / scale.y, 1 / scale.x) * m_parent->getWorldMatrix();
}
const Matrix4x4& SceneNode::getNodeWorldMatrix() const { return m_worldMat; }
void SceneNode::setNodeWorldMatrix(const Matrix4x4& mat) { m_worldMat = mat;}

void SceneNode::attachSceneObj(SceneObj* obj)
{
    obj->notifyAttached(this);
    m_objs.push_back(obj);
}
void SceneNode::detachSceneObj(SceneObj* obj)
{
    assert(obj->getSceneNode() == this);
    obj->notifyAttached(NULL);
	m_objs.erase(std::find(m_objs.begin(), m_objs.end(), obj));
}

const std::vector<SceneObj*>& SceneNode::getAttachedObjs() const { return m_objs; }
//----------------------------------------
// SceneObj
//----------------------------------------

SceneObj::SceneObj(E_SceneObjType t):
    m_node(NULL), m_objType(t)
{}
SceneObj::~SceneObj()
{
}
void SceneObj::notifyAttached(SceneNode* node) { m_node = node; }
SceneNode* SceneObj::getSceneNode() { return m_node; }
const SceneNode* SceneObj::getSceneNode() const { return m_node; }
E_SceneObjType SceneObj::getObjType() const { return m_objType; }

//----------------------------------------
// 序列化相关
//----------------------------------------
std::ostream& operator << (std::ostream& so, const SceneNode& o)
{
    StreamBlockWriter w("SceneNode", so);
    w.write("name", o.name());
    w.write("parentName", o.parentName());
    w.write("describMatrix", 1);
    Matrix4x4 mat = o.getNodeWorldMatrix();
    w.write("matrix", mat[0], mat[0] + 16);
    return so;
}
std::istream& operator >> (std::istream& si, SceneNode& o)
{
    StreamBlockReader r("SceneNode", si);
    {
        std::string s;
        if (!r.read("name", s)) assert(0);
        o.setName(s.c_str());
        if (!r.read("parentName", s)) assert(0);
        o.setParentName(s.c_str());
    }
    int describMatrix;
    if (!r.read("describMatrix", &describMatrix)) assert(0);
    if (describMatrix) {
        Matrix4x4 mat;
        if (!r.read("matrix", mat[0], mat[0] + 16)) assert(0);
        o.setNodeWorldMatrix(mat);
    }
    else {
        Vector3 scale, pos, yawPitchRoll;
        if (!r.read("scale", scale.data(), scale.data() + 3)) assert(0);
        if (!r.read("position", pos.data(), pos.data() + 3)) assert(0);
        if (!r.read("yawPitchRoll", yawPitchRoll.data(), yawPitchRoll.data() + 3)) assert(0);
        Matrix4x4 mat = 
            Matrix4x4::fromScale(scale.x, scale.y, scale.z) *
            Matrix4x4::fromYawPitchRoll(yawPitchRoll.x, yawPitchRoll.y, yawPitchRoll.z) *
            Matrix4x4::fromTranslate(pos.x, pos.y, pos.z);
        o.setNodeWorldMatrix(mat);
    }
    return si;
}

std::string sceneObjType2Str(E_SceneObjType t)
{
    switch (t) {
        case ESOT_entity: return "Entity";
        case ESOT_camera: return "Camera";
        case ESOT_light: return "Light";
        case ESOT_staticEntity: return "StaticEntity";
        case ESOT_commonCamera: return "CommonCamera";
        case ESOT_perspectiveCamera: return "PerspectiveCamera";
        case ESOT_orthoCamera: return "OrthoCamera";
        case ESOT_pointLight: return "PointLight";
        case ESOT_directionLight: return "DirectionLight";
        case ESOT_spotLight: return "SpotLight";
        default: break;
    }
    assert(0);
    return "";
}
E_SceneObjType str2SceneObjType(const std::string& s)
{
    if (s == "Entity") return ESOT_entity;
    else if (s == "Camera") return ESOT_camera;
    else if (s == "Light") return ESOT_light;
    else if (s == "StaticEntity") return ESOT_staticEntity;
    else if (s == "CommonCamera") return ESOT_commonCamera;
    else if (s == "PerspectiveCamera") return ESOT_perspectiveCamera;
    else if (s == "OrthoCamera") return ESOT_orthoCamera;
    else if (s == "PointLight") return ESOT_pointLight;
    else if (s == "DirectionLight") return ESOT_directionLight;
    else if (s == "SpotLight") return ESOT_spotLight;
    else assert(0);
    return ESOT_light;
}

SceneObj* SceneObj::create(E_SceneObjType t)
{
    switch (t) {
        case ESOT_staticEntity: 
            return new StaticEntity("", NULL);
        case ESOT_commonCamera: 
            return new CommonCamera(Matrix4x4::ZERO);
        case ESOT_perspectiveCamera: 
            return new PerspectiveCamera(0, 0, 0, 0);
        case ESOT_orthoCamera: 
            return new OrthoCamera(0, 0, 0, 0);
        case ESOT_pointLight: 
            return new PointLight(
                Vector3::ZERO, 
                Vector3::ZERO, 
                Vector3::ZERO, 
                Vector3::ZERO, 
                Vector3::ZERO, 
                0);
        case ESOT_directionLight: 
            return new DirectionLight(
                Vector3(1, 0, 0), 
                Vector3::ZERO, 
                Vector3::ZERO, 
                Vector3::ZERO, 
                Vector3::ZERO, 
                0);
        case ESOT_spotLight: 
            return new SpotLight(
                Vector3::ZERO, Vector3(1, 0, 0), 
                0, 0, 0, 
                Vector3::ZERO, 
                Vector3::ZERO, 
                Vector3::ZERO, 
                Vector3::ZERO, 
                0);
        default:break;
    }
    assert(0);
    return NULL;
}

std::ostream& operator << (std::ostream& so, const SceneObj* obj)
{
    assert(obj != NULL);
    {
        StreamBlockWriter w("SceneObjDescription", so);
        w.write("type", sceneObjType2Str(obj->getObjType()).c_str());
    }
    obj->printStream(so);
    return so;
}
std::istream& operator >> (std::istream& si, SceneObj*& obj)
{
    assert(obj == NULL);
    {
        StreamBlockReader r("SceneObjDescription", si);
        std::string name;
        if (!r.read("type", name)) assert(0);
        obj = SceneObj::create(str2SceneObjType(name));
    }
    assert(obj != NULL);
    obj->scanStream(si);
    return si;
}

