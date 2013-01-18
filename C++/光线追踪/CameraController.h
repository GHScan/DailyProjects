// vim: fileencoding=gbk

#ifndef CAMERACONTROLLER_H
#define CAMERACONTROLLER_H

#include "Vector.h"
#include "Matrix.h"

class SceneManager;
struct ITerrain;

struct ICameraController
{
    virtual ~ICameraController() = 0 {}
    virtual void onUpdate(float elapse) = 0;
    virtual void onKeyDown(int k) = 0;
    virtual void onKeyUp(int k) = 0;
    virtual void onMouseButtonDown(int btn, float x, float y) = 0;
    virtual void onMouseButtonUp(int btn, float x, float y) = 0;
    virtual void onMouseMove(float x, float y) = 0;
};

class FPSCameraController:
    public ICameraController
{
public:
    FPSCameraController(
            SceneManager* sceneMgr, const ITerrain *terrain, float speed);
    virtual void onUpdate(float elapse);
    virtual void onKeyDown(int k);
    virtual void onKeyUp(int k);
    virtual void onMouseButtonDown(int btn, float x, float y);
    virtual void onMouseButtonUp(int btn, float x, float y);
    virtual void onMouseMove(float x, float y);
private:
    SceneManager *m_sceneMgr;
    const ITerrain *m_terrain;
    float m_speed;
    Vector3 m_velocity;
    float m_oldMouseX;
    Matrix4x4 m_oldCameraWorldMat;
};

class BirdCameraController:
    public ICameraController
{
public:
    BirdCameraController(SceneManager* sceneMgr, float speed);
    virtual void onUpdate(float elapse);
    virtual void onKeyDown(int k);
    virtual void onKeyUp(int k);
    virtual void onMouseButtonDown(int btn, float x, float y);
    virtual void onMouseButtonUp(int btn, float x, float y);
    virtual void onMouseMove(float x, float y);
private:
    void applyCameraWorldMatrix();
private:
    SceneManager *m_sceneMgr;
    float m_speed;
    Vector3 m_velocity;
    Vector3 m_pos;
    Matrix4x4 m_rot;
    Matrix4x4 m_oldRot;
    float m_oldMousePos[2];
};

#endif // #ifndef CAMERACONTROLLER_H
