// vim: fileencoding=gbk

#ifndef SCENEMANAGER_H
#define SCENEMANAGER_H

#include <vector>
#include <string>

class SceneNode;
class Camera;
class Entity;
class Light;
struct ICameraController;
struct ITerrain;
struct IEntityController;
struct Ray;

enum E_PickLevel
{
    EPL_boundingSphere = 1,
    EPL_triangle,
};

class SceneManager
{
public:
    SceneManager(const std::string& fname);
    ~SceneManager();
    bool load(const std::string& fname);

    void onKeyDown(int k);
    void onKeyUp(int k);
    void onMouseButtonDown(int btn, float x, float y);
    void onMouseButtonUp(int btn, float x, float y);
    void onMouseMove(float x, float y);
    void onUpdate(float elapse);

    const Camera* getCamera() const;
    const Light* getLight() const;
    const ITerrain* getTerrain() const;

    void clear();

    std::vector<Entity*> getVisibleEntityList() const;
    Entity* pickEntity(
            const Ray& r, E_PickLevel level = EPL_boundingSphere);

public:
    SceneManager();
    void save(const std::string& fname) const;

    void setTerrain(ITerrain *terrain);
    void setCameraController(ICameraController *controller);
    void addSceneNode(SceneNode *node);
    void removeSceneNode(SceneNode *node);

private:
    std::vector<Entity*> clipEntityWithOrtree(
            const std::vector<Entity*>& ents) const;
    std::vector<Entity*> clipEntityWithPVS(
            const std::vector<Entity*>& ents) const;
    std::vector<Entity*> clipEntityWithPortal(
            const std::vector<Entity*>& ents) const;
    std::vector<Entity*> clipEntityWithFrustum(
            const std::vector<Entity*>& ents) const;

private:
    std::vector<SceneNode*>     m_nodes;
    std::vector<Entity*>   m_entitys;
    Light*  m_light;
    Camera* m_camera;
    ICameraController *m_cameraController;
    IEntityController *m_entController;
    ITerrain    *m_terrain;
};

#endif // #ifndef SCENEMANAGER_H
