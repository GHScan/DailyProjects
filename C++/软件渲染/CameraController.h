// vim: fileencoding=gbk

#ifndef CAMERACONTROLLER_H
#define CAMERACONTROLLER_H

#include "Vector.h"
#include "Matrix.h"

class Camera;
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
            Camera *camera, const ITerrain *terrain = NULL, float speed = 1);
    virtual void onUpdate(float elapse);
    virtual void onKeyDown(int k);
    virtual void onKeyUp(int k);
    virtual void onMouseButtonDown(int btn, float x, float y);
    virtual void onMouseButtonUp(int btn, float x, float y);
    virtual void onMouseMove(float x, float y);
private:
    Camera  *m_camera;
    const ITerrain *m_terrain;
    float m_speed;
    Vector3 m_velocity;
    float m_oldMouseX;
    Matrix4x4 m_oldCameraWorldMat;
};

class SpaceShipCameraController:
    public ICameraController
{
public:
    SpaceShipCameraController(Camera *camera, float speed = 1);
    virtual void onUpdate(float elapse);
    virtual void onKeyDown(int k);
    virtual void onKeyUp(int k);
    virtual void onMouseButtonDown(int btn, float x, float y);
    virtual void onMouseButtonUp(int btn, float x, float y);
    virtual void onMouseMove(float x, float y);
private:
    Camera *m_camera;
    float m_speed;
};

#endif // #ifndef CAMERACONTROLLER_H
