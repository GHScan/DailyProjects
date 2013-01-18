// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>

#include <fstream>
#include <algorithm>

#include "Log.h"
#include "Mesh.h"
#include "SceneManager.h"
#include "SceneObj.h"
#include "Matrix.h"
#include "CameraController.h"
#include "EntityController.h"
#include "Terrain.h"
#include "Serialize.h"
#include "Entity.h"
#include "Camera.h"
#include "Light.h"
#include "Geometry.h"

SceneManager::SceneManager(const std::string& fname):
    m_light(NULL), m_camera(NULL), m_cameraController(NULL), m_terrain(NULL),
    m_entController(NULL)
{
    bool b = load(fname);
    assert(b);
}
SceneManager::~SceneManager()
{
    clear();
}
void SceneManager::clear()
{
    sdelete(m_terrain);
    sdelete(m_cameraController);
    sdelete(m_entController);

    for (int i = 0; i < (int)m_nodes.size(); ++i) {
        delete m_nodes[i];
    }
    m_entitys.clear();

    for (int i = 0; i < (int)m_entitys.size(); ++i) {
        delete m_entitys[i];
    }
    m_entitys.clear();
    sdelete(m_camera);
    sdelete(m_light);
}
bool SceneManager::load(const std::string& fname)
{
    clear();

    std::ifstream fi(fname.c_str());

    int sceneObjCnt = 0;
    {
        StreamBlockReader r("SceneDescription", fi);
        if (!r.read("sceneObjCount", &sceneObjCnt)) assert(0);
    }
    while (sceneObjCnt-- > 0) {
        SceneNode *node = new SceneNode();
        fi >> *node;
        SceneObj* obj = NULL;
        fi >> obj;
        assert(obj != NULL);
        node->attachSceneObj(obj);
        m_nodes.push_back(node);
        if (isEntity(obj->getObjType())) {
            m_entitys.push_back((Entity*)obj);
        }
        else if (isCamera(obj->getObjType())) {
            assert(m_camera == NULL);
            m_camera = (Camera*)obj;
        }
        else if (isLight(obj->getObjType())) {
            assert(m_light == NULL);
            m_light = (Light*)obj;
        }
        else assert(0);
    }

    assert(m_camera != NULL);
    assert(m_light != NULL);
    assert(!m_entitys.empty());

    m_terrain = new FlatTerrain(0);
    m_cameraController = new FPSCameraController(m_camera, m_terrain);
    m_entController = new EntityController_Rotator(this);

    return !!fi;
}

void SceneManager::onKeyDown(int k)
{
    if (m_cameraController != NULL) m_cameraController->onKeyDown(k);
}
void SceneManager::onKeyUp(int k)
{
    if (m_cameraController != NULL) m_cameraController->onKeyUp(k);
}
void SceneManager::onMouseButtonDown(int btn, float x, float y)
{
    if (m_cameraController != NULL) m_cameraController->onMouseButtonDown(btn, x, y);
    if (m_entController != NULL) m_entController->onMouseButtonDown(btn, x, y);
}
void SceneManager::onMouseButtonUp(int btn, float x, float y)
{
    if (m_cameraController != NULL) m_cameraController->onMouseButtonUp(btn, x, y);
    if (m_entController != NULL) m_entController->onMouseButtonUp(btn, x, y);
}
void SceneManager::onMouseMove(float x, float y)
{
    if (m_cameraController != NULL) m_cameraController->onMouseMove(x, y);
    if (m_entController != NULL) m_entController->onMouseMove(x, y);
}
void SceneManager::onUpdate(float elapse)
{
    if (m_cameraController != NULL) m_cameraController->onUpdate(elapse);
}

const Camera* SceneManager::getCamera() const
{
    return m_camera;
}
const Light* SceneManager::getLight() const
{
    return m_light;
}
const ITerrain* SceneManager::getTerrain() const
{
    return m_terrain;
}

SceneManager::SceneManager():
    m_light(NULL), m_camera(NULL), m_cameraController(NULL), m_terrain(NULL)
{
}
void SceneManager::save(const std::string& fname) const
{
    std::ofstream fo(fname.c_str());
    assert(fo);

    int sceneObjCnt = (int)m_entitys.size() + 2;
    {
        StreamBlockWriter w("SceneDescription", fo);
        w.write("sceneObjCount", sceneObjCnt);
    }
    for (int i = 0; i < (int)m_nodes.size(); ++i) {
        SceneNode *node = m_nodes[i];
        for (int j = 0; j < (int)node->getAttachedObjs().size(); ++j) { 
            SceneObj* obj = node->getAttachedObjs()[j];
            fo << *node;
            fo << obj;
            --sceneObjCnt;
        }
    }
    assert(sceneObjCnt == 0);
}
void SceneManager::setTerrain(ITerrain *terrain)
{
    sdelete(m_terrain);
    m_terrain = terrain;
}
void SceneManager::setCameraController(ICameraController *controller)
{
    sdelete(m_cameraController);
    m_cameraController = controller;
}
void SceneManager::addSceneNode(SceneNode *node)
{
    assert(node != NULL);
    assert(find(m_nodes.begin(), m_nodes.end(), node) == m_nodes.end());
    m_nodes.push_back(node);
    for (int i = 0; i < (int)node->getAttachedObjs().size(); ++i) {
        SceneObj* obj = node->getAttachedObjs()[i];
        if (isCamera(obj->getObjType())) {
            assert(m_camera == NULL);
            m_camera = (Camera*)obj;
        }
        else if (isLight(obj->getObjType())) {
            assert(m_light == NULL);
            m_light = (Light*)obj;
        }
        else if (isEntity(obj->getObjType())) {
            assert(find(m_entitys.begin(), m_entitys.end(), obj) == m_entitys.end());
            m_entitys.push_back((Entity*)obj);
        }
        else assert(0);
    }
}
void SceneManager::removeSceneNode(SceneNode *node)
{
    assert(node != NULL);
    {
        std::vector<SceneNode*>::iterator iter = 
            std::find(m_nodes.begin(), m_nodes.end(), node);
        if (iter == m_nodes.end()) return;
        m_nodes.erase(iter);
    }
    for (int i = 0; i < (int)node->getAttachedObjs().size(); ++i) {
        SceneObj* obj = node->getAttachedObjs()[i];
        if (isCamera(obj->getObjType())) {
            assert(m_camera == obj);
            m_camera = NULL;
        }
        else if (isLight(obj->getObjType())) {
            assert(m_light == obj);
            m_light = NULL;
        }
        else if (isEntity(obj->getObjType())) {
			m_entitys.erase(std::find(m_entitys.begin(), m_entitys.end(), obj));
        }
        else assert(0);
    }
}

std::vector<Entity*> SceneManager::getVisibleEntityList() const
{
    return 
        clipEntityWithFrustum(
                clipEntityWithPortal(
                    clipEntityWithPVS(
                        clipEntityWithOrtree(m_entitys))));
}

std::vector<Entity*> SceneManager::clipEntityWithOrtree(
        const std::vector<Entity*>& ents) const
{
    return ents;
}
std::vector<Entity*> SceneManager::clipEntityWithPVS(
        const std::vector<Entity*>& ents) const
{
    return ents;
}
std::vector<Entity*> SceneManager::clipEntityWithPortal(
        const std::vector<Entity*>& ents) const
{
    return ents;
}
std::vector<Entity*> SceneManager::clipEntityWithFrustum(
        const std::vector<Entity*>& ents) const
{
    PlaneList frustumPlanes = getCamera()->getVolumePlanes(EPID_all);

    std::vector<Entity*> rents;
    for (int i = 0; i < (int)ents.size(); ++i) {
        Entity* ent = ents[i];
        Matrix4x4 worldViewMat = ent->getSceneNode()->getWorldMatrix() * getCamera()->getViewMatrix();

        AABB aabb = ent->getBoundAABB();
        transform(aabb, worldViewMat);
        if (intersect(frustumPlanes, aabb.getCorners()) == EIC_front) continue;

        Sphere sphere = ent->getBoundSphere();
        transform(sphere, worldViewMat);
        if (intersect(frustumPlanes, sphere) == EIC_front) continue;

        rents.push_back(ent);
    }

    Log::instance()->addMsg("frustum clip : %d/%d", ents.size() - rents.size(), ents.size());

    return rents;
}

Entity* SceneManager::pickEntity(
        const Ray& r, E_PickLevel level)
{
    assert(level == EPL_boundingSphere);
    Entity *nearestEnt = NULL;
    float t = 0;
    for (int i = 0; i < (int)m_entitys.size(); ++i) {
        Entity* ent = m_entitys[i];
        Sphere s(ent->getBoundSphere());
        transform(s, ent->getSceneNode()->getWorldMatrix());
        float _t;
        if (intersect(r, s, _t)) {
            if (nearestEnt == NULL) {
                nearestEnt = ent;
                t = _t;
            }
            else {
                if (_t < t) {
                    t = _t;
                    nearestEnt = ent;
                }
            }
        }
    }
    return nearestEnt;
}
