// vim: fileencoding=gbk

#ifndef LIGHT_H
#define LIGHT_H

#include <iostream>

#include "SceneObj.h"
#include "Vector.h"

struct Material;
//----------------------------------------
// Light
//----------------------------------------
class Light:
    public SceneObj
{
public:
    Light(
            E_SceneObjType t, 
            const Vector3& ambient, 
            const Vector3& diffuse, 
            const Vector3& specular,
            const Vector3& attenuation,
            float range);
    virtual ~Light() = 0 {}

    virtual void beginLighting(
            const Matrix4x4& modelSpaceMat,
            const Material& materail, 
            const Vector3& cameraPosInMS) const = 0;
    virtual Vector3 illuminate(
            const Vector3& pos, const Vector3& norm) const = 0;
    virtual Vector3 illuminateWithSpecular(
            const Vector3& pos, const Vector3& norm) const = 0;
    virtual Vector3 illuminateWithSeparateSpecular(
            const Vector3& pos, const Vector3& norm, Vector3& specular) const = 0;
    virtual void endLighting() const = 0;

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
protected:
    Vector3 m_ambient, m_diffuse, m_specular;
    Vector3 m_attenuation;
    float m_range;
};

class PointLight:
    public Light
{
public:
    PointLight(
            const Vector3& pos,
            const Vector3& ambient, 
            const Vector3& diffuse, 
            const Vector3& specular,
            const Vector3& attenuation,
            float range);

    virtual void beginLighting(
            const Matrix4x4& modelSpaceMat, 
            const Material& materail,
            const Vector3& cameraPosInMS) const;
    virtual Vector3 illuminate(
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSpecular(
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSeparateSpecular(
            const Vector3& pos, const Vector3& norm, Vector3& specular) const;
    virtual void endLighting() const;

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    Vector3 m_pos;
    mutable Vector3 m_modelSpacePos;
    mutable const Material* m_mat;
    mutable Vector3 m_cameraPosInMS; // 物体空间的摄像机位置
};

class DirectionLight:
    public Light
{
public:
    DirectionLight(
            const Vector3& dir,
            const Vector3& ambient, 
            const Vector3& diffuse, 
            const Vector3& specular,
            const Vector3& attenuation,
            float range);

    virtual void beginLighting(
            const Matrix4x4& modelSpaceMat, 
            const Material& materail,
            const Vector3& cameraPosInMS) const;
    virtual Vector3 illuminate(
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSpecular(
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSeparateSpecular(
            const Vector3& pos, const Vector3& norm, Vector3& specular) const;
    virtual void endLighting() const;

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    Vector3 m_dir;
    mutable Vector3 m_modelSpaceInvDir;
    mutable const Material* m_mat;
    mutable Vector3 m_cameraPosInMS; // 物体空间的摄像机位置
};

class SpotLight:
    public Light
{
public:
    SpotLight(
            Vector3 pos, Vector3 dir,
            float inner, float outter, float falloff,
            const Vector3& ambient, 
            const Vector3& diffuse, 
            const Vector3& specular,
            const Vector3& attenuation,
            float range);

    virtual void beginLighting(
            const Matrix4x4& modelSpaceMat,
            const Material& materail,
            const Vector3& cameraPosInMS) const;
    virtual Vector3 illuminate(
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSpecular(
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSeparateSpecular(
            const Vector3& pos, const Vector3& norm, Vector3& specular) const;
    virtual void endLighting() const;

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    float m_inner, m_outter;
    float m_falloff;
    Vector3 m_pos, m_dir;
    mutable Vector3 m_modelSpacePos, m_modelSpaceInvDir;
    mutable float m_cosInner, m_cosOutter;
    mutable float m_invCosInnerSubOutter;
    mutable const Material* m_mat;
    mutable Vector3 m_cameraPosInMS; // 物体空间的摄像机位置
};


#endif // #ifndef LIGHT_H
