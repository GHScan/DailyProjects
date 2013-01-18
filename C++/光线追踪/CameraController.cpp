// vim: fileencoding=gbk
#include "pch.h"

#include <cmath>

#include "CameraController.h"
#include "Camera.h"
#include "SceneManager.h"
#include "Terrain.h"
#include "VirtualPlatform.h"

FPSCameraController::FPSCameraController(
        SceneManager* sceneMgr, const ITerrain *terrain, float speed):
    m_sceneMgr(sceneMgr), m_speed(speed), m_terrain(terrain), m_velocity(0.0), 
    m_oldMouseX(-1), m_oldCameraWorldMat(Matrix4x4::IDENTITY)
{
}
void FPSCameraController::onUpdate(float elapse)
{
    if (m_velocity != Vector3::ZERO) {
        Camera *camera = m_sceneMgr->getCamera();
        Vector3 oldPos = camera->getSceneNode()->getWorldPosition();
        float xOff = m_velocity.x * elapse, zOff = m_velocity.z * elapse;
        float yOff = m_terrain != NULL ? m_terrain->getHeight(
                oldPos.x + xOff, oldPos.z + zOff) - oldPos.y : 0;
        camera->getSceneNode()->translate(
                m_velocity.x * elapse, yOff, m_velocity.z * elapse, ETS_local);
        m_sceneMgr->notifyCameraSpaceChanged();
    }

}
void FPSCameraController::onMouseButtonDown(int btn, float x, float y)
{
    if (btn == MOUSE_RBUTTON) { 
        Camera *camera = m_sceneMgr->getCamera();
        m_oldMouseX = x;
        m_oldCameraWorldMat = camera->getSceneNode()->getWorldMatrix();
    }
}
void FPSCameraController::onMouseButtonUp(int btn, float x, float y)
{
    if (btn == MOUSE_RBUTTON) { 
        m_oldMouseX = -1;
    }
}
void FPSCameraController::onMouseMove(float x, float y)
{
    if (m_oldMouseX >= 0) {
        Camera *camera = m_sceneMgr->getCamera();
        Matrix4x4 mat = Matrix4x4::fromRotateY(
                (x - m_oldMouseX) * 120) * m_oldCameraWorldMat;
        camera->getSceneNode()->setWorldMatrix(mat);
        m_sceneMgr->notifyCameraSpaceChanged();
    }
}
void FPSCameraController::onKeyDown(int k)
{
    switch (k) {
        case K_UP: m_velocity.z = m_speed; break;
        case K_DOWN: m_velocity.z = -m_speed; break;
        case K_LEFT: m_velocity.x = -m_speed; break;
        case K_RIGHT: m_velocity.x = m_speed; break;
        default:
            break;
    }
}
void FPSCameraController::onKeyUp(int k)
{
    switch (k) {
        case K_UP: m_velocity.z = 0; break;
        case K_DOWN: m_velocity.z = 0; break;
        case K_LEFT: m_velocity.x = 0; break;
        case K_RIGHT: m_velocity.x = 0; break;
        default:
            break;
    }
}

//----------------------------------------
// BirdCameraController
//----------------------------------------
BirdCameraController::BirdCameraController(SceneManager* sceneMgr, float speed):
    m_sceneMgr(sceneMgr), m_speed(speed), m_velocity(Vector3::ZERO),
    m_pos(Vector3::ZERO), m_rot(Matrix4x4::IDENTITY), m_oldRot(Matrix4x4::IDENTITY)
{
    m_oldMousePos[0] = m_oldMousePos[1] = -1;

    Camera *c = sceneMgr->getCamera();
    Matrix4x4 mat = c->getSceneNode()->getWorldMatrix();
    m_pos = Vector3(mat[3]);
    *(Vector3*)m_rot[0] = Vector3(mat[0]);
    *(Vector3*)m_rot[1] = Vector3(mat[1]);
    *(Vector3*)m_rot[2] = Vector3(mat[2]);
}
void BirdCameraController::onUpdate(float elapse)
{
    if (m_velocity != Vector3::ZERO) {
        Vector3 vel(m_velocity.normalize());
        transformDirection(vel, m_rot);
        m_pos += vel * (elapse * m_speed);
        applyCameraWorldMatrix();
    }
}
void BirdCameraController::onKeyDown(int k)
{
    switch (k) {
        case K_UP: m_velocity.z = m_speed; break;
        case K_DOWN: m_velocity.z = -m_speed; break;
        case K_LEFT: m_velocity.x = -m_speed; break;
        case K_RIGHT: m_velocity.x = m_speed; break;
        default:
            break;
    }
}
void BirdCameraController::onKeyUp(int k)
{
    switch (k) {
        case K_UP: m_velocity.z = 0; break;
        case K_DOWN: m_velocity.z = 0; break;
        case K_LEFT: m_velocity.x = 0; break;
        case K_RIGHT: m_velocity.x = 0; break;
        default:
            break;
    }
}
void BirdCameraController::onMouseButtonDown(int btn, float x, float y)
{
    if (btn == MOUSE_RBUTTON) {
        m_oldMousePos[0] = x;
        m_oldMousePos[1] = y;
        m_oldRot = m_rot;
    }
}
void BirdCameraController::onMouseButtonUp(int btn, float x, float y)
{
    if (btn == MOUSE_RBUTTON) {
        m_oldMousePos[0] = m_oldMousePos[1] = -1;
    }
}
void BirdCameraController::onMouseMove(float x, float y)
{
    if (m_oldMousePos[0] > 0) {
        float yaw = (x - m_oldMousePos[0]) * 120;
        float pitch = (y - m_oldMousePos[1]) * 120;
        m_rot = 
            Matrix4x4::fromRotateX(pitch) * 
            Matrix4x4::fromRotateY(yaw) * 
            m_oldRot;
        Vector3 xyz[3] = {
            Vector3(m_rot[0]), Vector3(m_rot[1]), Vector3(m_rot[2]),
        };
        if (!fequal(xyz[0].y, 0)) {
            xyz[0].y = 0;
            xyz[0] = xyz[0].normalize();
            xyz[2] = xyz[2].normalize();
            xyz[1] = xyz[2].crossProduct(xyz[0]);
            m_rot = Matrix4x4(Matrix3x3(xyz[0].data()));
        }
        applyCameraWorldMatrix();
    }
}
void BirdCameraController::applyCameraWorldMatrix()
{
    Camera *camera = m_sceneMgr->getCamera();
    camera->getSceneNode()->setWorldMatrix(
            m_rot * 
            Matrix4x4::fromTranslate(m_pos.x, m_pos.y, m_pos.z));
    m_sceneMgr->notifyCameraSpaceChanged();
}
