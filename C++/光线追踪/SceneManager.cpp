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
#include "Traceable.h"

SceneManager::SceneManager(std::istream& fi):
    m_light(NULL), m_camera(NULL), m_cameraController(NULL), m_terrain(NULL),
    m_entController(NULL), m_skybox(NULL)
{
    bool b = load(fi);
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
    m_nodes.clear();

    for (int i = 0; i < (int)m_entitys.size(); ++i) {
        delete m_entitys[i];
    }
    m_entitys.clear();
    sdelete(m_skybox);

    sdelete(m_camera);
    sdelete(m_light);
}
bool SceneManager::load(std::istream& fi)
{
    clear();

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
            Entity *ent = (Entity*)obj;
            if (ent->name() == "skybox") {
                assert(m_skybox == NULL);
                m_skybox = ent;
            }
            else {
                m_entitys.push_back(ent);
            }
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

    // 将父子关联上
    for (int i = 0; i < (int)m_nodes.size(); ++i) {
        for (int j = 0; j < (int)m_nodes.size(); ++j) {
            if (m_nodes[i]->parentName()[0] != 0 && 
                std::string(m_nodes[i]->parentName()) == m_nodes[j]->name()) {
                m_nodes[i]->setParent(m_nodes[j]);
                break;
            }
        }
    }

    assert(m_camera != NULL);
    assert(m_light != NULL);
    assert(!m_entitys.empty());

    m_terrain = new FlatTerrain(0);
    m_cameraController = new BirdCameraController(this, 30);
    m_entController = new EntityController_Rotator(this);

    notifyCameraSpaceChanged();

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

const Camera* SceneManager::getCamera() const { return m_camera; }
Camera* SceneManager::getCamera(){ return m_camera; }
const Light* SceneManager::getLight() const { return m_light; }
const ITerrain* SceneManager::getTerrain() const { return m_terrain; }

SceneManager::SceneManager():
    m_light(NULL), m_camera(NULL), m_cameraController(NULL), m_terrain(NULL),
    m_entController(NULL), m_skybox(NULL)
{
}
void SceneManager::save(std::ostream& fo) const
{
    int sceneObjCnt = (int)m_entitys.size() + 2;
    if (m_skybox != NULL) sceneObjCnt += 1;
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
            Entity *ent = (Entity*)obj;
            if (ent->name() == "skybox") {
                assert(m_skybox == NULL);
                m_skybox = ent;
            }
            else {
                m_entitys.push_back(ent);
            }
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
            Entity *ent = (Entity*)obj;
            if (ent == m_skybox) {
                m_skybox = NULL;
            }
            else {
                m_entitys.erase(std::find(m_entitys.begin(), m_entitys.end(), obj));
            }
        }
        else assert(0);
    }
}

Entity* SceneManager::pickClosestEntity(const Ray& r, int intersectFace, TraceFragment& frag)
{
    float t = MAX_FLOAT;
    Entity *ent = NULL;
    for (int i = 0; i < (int)m_entitys.size(); ++i) {
        ITraceable *tr = m_entitys[i]->getTraceable();
        if (tr->intersect(r, intersectFace, t, frag)) {
            ent = m_entitys[i];
        }
    }
    return ent;
}

bool SceneManager::intersectTest(const Ray& r) const
{
    for (int i = 0; i < (int)m_entitys.size(); ++i) {
        if (m_entitys[i]->getTraceable()->intersectTest(r)) return true;
    }
    return false;
}
bool SceneManager::intersectSimply(const Ray& r, int intersectFace, float &t, Entity* exclude)
{
    bool b = false;
    for (int i = 0; i < (int)m_entitys.size(); ++i) {
        if (m_entitys[i] == exclude) continue;
        if (m_entitys[i]->getTraceable()->intersectSimply(r, intersectFace, t)) b = true;
    }
    return b;
}

void SceneManager::notifyCameraSpaceChanged()
{
    Matrix4x4 viewMat = m_camera->getViewMatrix();
    for (int i = 0; i < (int)m_entitys.size(); ++i) {
        m_entitys[i]->notifyCameraSpaceChanged(viewMat);
    }
    m_light->notifyCameraSpaceChanged(viewMat);
    if (m_skybox != NULL) m_skybox->notifyCameraSpaceChanged(viewMat);
}
Entity* SceneManager::getSkybox()
{
    return m_skybox;
}
