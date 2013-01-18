#include "pch.h"

#include "SceneManager.h"
#include "Camera.h"
#include "Light.h"
#include "Entity.h"

int main()
{
    SceneManager mgr;

    DirectionLight *dir = new DirectionLight(
            Vector3(-1, -1, 0), 
            Vector3::ZERO,
            Vector3::ZERO,
            Vector3::ZERO,
            Vector3::ZERO,
            1000);
    SceneNode *node = new SceneNode();
    node->attachSceneObj(dir);
    mgr.addSceneNode(node);

    PerspectiveCamera *camera = new PerspectiveCamera(
            90, 1.33f, 0.1f, 100.f);
    node = new SceneNode();
    node->attachSceneObj(camera);
    mgr.addSceneNode(node);

    StaticSceneEntity *entity = new StaticSceneEntity(
            "testEntity", "destMesh.txt");
    node = new SceneNode();
    node->translate(0, 0, 10);
    node->attachSceneObj(entity);
    mgr.addSceneNode(node);

    mgr.save("Scene.txt");
}
