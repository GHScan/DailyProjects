// vim: fileencoding=gbk

#ifndef SCENEMANAGER_H
#define SCENEMANAGER_H

#include <vector>
#include <string>
#include <iostream>

class SceneNode;
class Camera;
class Entity;
class Light;
struct ICameraController;
struct ITerrain;
struct IEntityController;
struct Ray;
struct ITraceable;
struct TraceFragment;

class SceneManager
{
public:
    SceneManager(std::istream& fi);
    ~SceneManager();
    bool load(std::istream& fi);

    void onKeyDown(int k);
    void onKeyUp(int k);
    void onMouseButtonDown(int btn, float x, float y);
    void onMouseButtonUp(int btn, float x, float y);
    void onMouseMove(float x, float y);
    void onUpdate(float elapse);

    const Camera* getCamera() const;
    Camera* getCamera();
    const Light* getLight() const;
    const ITerrain* getTerrain() const;
    Entity* getSkybox();

    void clear();

    Entity* pickClosestEntity(
            const Ray& r, int intersectFace, TraceFragment& frag);
    bool intersectTest(const Ray& r) const;
    bool intersectSimply(const Ray& r, int intersectFace, float &t, Entity *exclude);

    void notifyCameraSpaceChanged();

public:
    SceneManager();
    void save(std::ostream& fo) const;

    void setTerrain(ITerrain *terrain);
    void setCameraController(ICameraController *controller);
    void addSceneNode(SceneNode *node);
    void removeSceneNode(SceneNode *node);

private:
    std::vector<SceneNode*>     m_nodes;
    std::vector<Entity*>   m_entitys;
    Light*  m_light;
    Camera* m_camera;
    ICameraController *m_cameraController;
    IEntityController *m_entController;
    ITerrain    *m_terrain;
    Entity *m_skybox;
};

#endif // #ifndef SCENEMANAGER_H
