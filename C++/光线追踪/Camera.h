// vim: fileencoding=gbk

#ifndef CAMERA_H
#define CAMERA_H

#include "SceneObj.h"
#include "Matrix.h"
#include "Geometry.h"
//----------------------------------------
// Camera
//----------------------------------------

class Camera:
    public SceneObj
{
public:
    Camera(E_SceneObjType t);
    Matrix4x4 getViewMatrix() const;

    virtual Matrix4x4 getProjectionMatrix() const = 0;
    virtual Ray getRayFromViewport(float x, float y) const = 0; // x:0~1, y:0~1
    virtual PlaneList getVolumePlanes(E_PlaneID pids) const = 0;
    virtual void notifyCameraSpaceChanged(const Matrix4x4& mat) {}
private:
};

class CommonCamera:
    public Camera
{
public:
    CommonCamera(const Matrix4x4& projMat);

    virtual Matrix4x4 getProjectionMatrix() const;
    virtual Ray getRayFromViewport(float x, float y) const; // x:0~1, y:0~1
    virtual PlaneList getVolumePlanes(E_PlaneID pids) const;
     
    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    Matrix4x4   m_projMat;
};

class PerspectiveCamera:
    public Camera
{
public:
    PerspectiveCamera(float fovY, float aspect, float nearZ, float farZ);
    float fovY() const;
    float aspect() const;
    float nearZ() const;
    float farZ() const;
    Frustum getFrustum() const;

    virtual Matrix4x4 getProjectionMatrix() const;
    virtual Ray getRayFromViewport(float x, float y) const;
    virtual PlaneList getVolumePlanes(E_PlaneID pids) const;

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    float m_fovY, m_aspect;
    float m_nearZ, m_farZ;
};
class OrthoCamera:
    public Camera
{
public:
    OrthoCamera(float w, float h, float nearZ, float farZ);
    float width() const;
    float height() const;
    float nearZ() const;
    float farZ() const;
    AABB getAABB() const;

    virtual Matrix4x4 getProjectionMatrix() const;
    virtual Ray getRayFromViewport(float x, float y) const; // x:0~1, y:0~1
    virtual PlaneList getVolumePlanes(E_PlaneID pids) const;

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    float m_w, m_h;
    float m_nearZ, m_farZ;
};


#endif // #ifndef CAMERA_H
