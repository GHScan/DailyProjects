// vim: fileencoding=gbk
#include "pch.h"

#include <cmath>

#include "Camera.h"
#include "Util.h"
#include "Serialize.h"
//----------------------------------------
// Camera
//----------------------------------------
Camera::Camera(E_SceneObjType t):
    SceneObj(t)
{}
Matrix4x4 Camera::getViewMatrix() const
{
    const SceneNode *node = getSceneNode();
    assert(node != NULL);
    return node->getWorldMatrix().inverse();
}

//----------------------------------------
// CommonCamera
//----------------------------------------
CommonCamera::CommonCamera(const Matrix4x4& projMat):
    m_projMat(projMat), Camera(ESOT_commonCamera)
{
}
Matrix4x4 CommonCamera::getProjectionMatrix() const { return m_projMat; }
Ray CommonCamera::getRayFromViewport(float x, float y) const
{
    assert(0);
    return Ray(Vector3::ZERO, Vector3::ZERO);
}
PlaneList CommonCamera::getVolumePlanes(E_PlaneID pids) const
{
    assert(0);
    return PlaneList();
}

//----------------------------------------
// PerspectiveCamera
//----------------------------------------
PerspectiveCamera::PerspectiveCamera(float fovY, float aspect, float nearZ, float farZ):
    Camera(ESOT_perspectiveCamera), m_fovY(fovY), m_aspect(aspect), m_nearZ(nearZ), m_farZ(farZ)
{}
Matrix4x4 PerspectiveCamera::getProjectionMatrix() const 
{ 
    return Matrix4x4::fromPerspectiveProj(m_fovY, m_aspect, m_nearZ, m_farZ);
}
Ray PerspectiveCamera::getRayFromViewport(float x, float y) const
{
    float tanY = tan(degree2Radian(m_fovY) / 2);
    float nearY = tanY * m_nearZ;
    float nearX = m_aspect * nearY;
    Vector3 nearPt(x * 2 * nearX - nearX, nearY - y * 2 * nearY, m_nearZ);
    return Ray(nearPt, nearPt.normalize());
}
PlaneList PerspectiveCamera::getVolumePlanes(E_PlaneID pids) const
{
    return getFrustum().getPlanes(pids);
}
Frustum PerspectiveCamera::getFrustum() const
{
    return Frustum(m_fovY, m_aspect, m_nearZ, m_farZ);
}
float PerspectiveCamera::fovY() const { return m_fovY; }
float PerspectiveCamera::aspect() const { return m_aspect; }
float PerspectiveCamera::nearZ() const { return m_nearZ; }
float PerspectiveCamera::farZ() const { return m_farZ; }

//----------------------------------------
// OrthoCamera
//----------------------------------------
OrthoCamera::OrthoCamera(float w, float h, float nearZ, float farZ):
    Camera(ESOT_orthoCamera), m_w(w), m_h(h), m_nearZ(nearZ), m_farZ(farZ)
{}
Matrix4x4 OrthoCamera::getProjectionMatrix() const 
{
    return Matrix4x4::fromOrthoProj(m_w, m_h, m_nearZ, m_farZ);
}
Ray OrthoCamera::getRayFromViewport(float x, float y) const
{
    return Ray(Vector3(x * m_w - m_w / 2, m_h / 2 - y * m_h, m_nearZ), 
            Vector3(0, 0, 1));
}
PlaneList OrthoCamera::getVolumePlanes(E_PlaneID pids) const
{
    return getAABB().getPlanes(pids);
}
AABB OrthoCamera::getAABB() const
{
    return AABB(
            Vector3(-m_w / 2, -m_h / 2, m_nearZ), 
            Vector3(m_w / 2, m_h / 2, m_farZ));
}
float OrthoCamera::width() const { return m_w; }
float OrthoCamera::height() const { return m_h; }
float OrthoCamera::nearZ() const { return m_nearZ; }
float OrthoCamera::farZ() const { return m_farZ; }

//----------------------------------------
// 序列化
//----------------------------------------
void CommonCamera::printStream(std::ostream& so) const
{
    StreamBlockWriter w(sceneObjType2Str(ESOT_commonCamera).c_str(), so);
    w.write("projMatrix", m_projMat[0], m_projMat[0] + 16);
}
void CommonCamera::scanStream(std::istream& si)
{
    StreamBlockReader r(sceneObjType2Str(ESOT_commonCamera).c_str(), si);
    if (!r.read("projMatrix", m_projMat[0], m_projMat[0] + 16)) assert(0);
}
void PerspectiveCamera::printStream(std::ostream& so) const
{
    StreamBlockWriter w(sceneObjType2Str(ESOT_perspectiveCamera).c_str(), so);
    w.write("fovY", m_fovY);
    w.write("aspect", m_aspect);
    w.write("nearZ", m_nearZ);
    w.write("farZ", m_farZ);
}
void PerspectiveCamera::scanStream(std::istream& si)
{
    StreamBlockReader r(sceneObjType2Str(ESOT_perspectiveCamera).c_str(), si);
    if (!r.read("fovY", &m_fovY)) assert(0);
    if (!r.read("aspect", &m_aspect)) assert(0);
    if (!r.read("nearZ", &m_nearZ)) assert(0);
    if (!r.read("farZ", &m_farZ)) assert(0);
}
void OrthoCamera::printStream(std::ostream& so) const
{
    StreamBlockWriter w(sceneObjType2Str(ESOT_orthoCamera).c_str(), so);
    w.write("width", m_w);
    w.write("height", m_h);
    w.write("nearZ", m_nearZ);
    w.write("farZ", m_farZ);
}
void OrthoCamera::scanStream(std::istream& si)
{
    StreamBlockReader r(sceneObjType2Str(ESOT_orthoCamera).c_str(), si);
    if (!r.read("width", &m_w)) assert(0);
    if (!r.read("height", &m_h)) assert(0);
    if (!r.read("nearZ", &m_nearZ)) assert(0);
    if (!r.read("farZ", &m_farZ)) assert(0);
}
