// vim: fileencoding=gbk
#include "pch.h"

#include "CameraController.h"
#include "Camera.h"
#include "Terrain.h"
#include "VirtualPlatform.h"

FPSCameraController::FPSCameraController(
        Camera *camera, const ITerrain *terrain, float speed):
    m_camera(camera), m_speed(speed), m_terrain(terrain), m_velocity(0.0), 
    m_oldMouseX(-1), m_oldCameraWorldMat(Matrix4x4::IDENTITY)
{
}
void FPSCameraController::onUpdate(float elapse)
{
    if (m_velocity != Vector3::ZERO) {
        Vector3 oldPos = m_camera->getSceneNode()->getPosition();
        float xOff = m_velocity.x * elapse, zOff = m_velocity.z * elapse;
        float yOff = m_terrain != NULL ? m_terrain->getHeight(
                oldPos.x + xOff, oldPos.z + zOff) - oldPos.y : 0;
        m_camera->getSceneNode()->translate(
                m_velocity.x * elapse, yOff, m_velocity.z * elapse, ETS_local);
    }

}
void FPSCameraController::onMouseButtonDown(int btn, float x, float y)
{
    if (btn == MOUSE_RBUTTON) { 
        m_oldMouseX = x;
        m_oldCameraWorldMat = m_camera->getSceneNode()->getWorldMatrix();
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
        Matrix4x4 mat = Matrix4x4::fromRotateY(
                (x - m_oldMouseX) * 120) * m_oldCameraWorldMat;
        m_camera->getSceneNode()->setWorldMatrix(mat);
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

SpaceShipCameraController::SpaceShipCameraController(Camera *camera, float speed):
    m_camera(camera), m_speed(speed)
{
}
void SpaceShipCameraController::onUpdate(float elapse)
{
}
void SpaceShipCameraController::onKeyDown(int k)
{
}
void SpaceShipCameraController::onKeyUp(int k)
{
}
void SpaceShipCameraController::onMouseButtonDown(int btn, float x, float y)
{
}
void SpaceShipCameraController::onMouseButtonUp(int btn, float x, float y)
{
}
void SpaceShipCameraController::onMouseMove(float x, float y)
{
}
