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

    virtual Vector3 getLightDirection(const Vector3& point) const = 0;
    virtual void notifyCameraSpaceChanged(const Matrix4x4& viewMat) = 0;
    virtual Vector3 illuminatePro(const Material& mat,
            const Vector3& pos, const Vector3& norm) const = 0;
    virtual Vector3 illuminateWithSpecularPro(const Material& mat,
            const Vector3& pos, const Vector3& norm) const = 0;
    virtual Vector3 illuminateWithSeparateSpecularPro(const Material& mat,
            const Vector3& pos, const Vector3& norm, Vector3& specular) const = 0;
    // 简化光照模型呢，即，将灯光简化为白光
    virtual Vector3 illuminate(const Material& mat,
            const Vector3& pos, const Vector3& norm) const = 0;
    virtual Vector3 illuminateWithSpecular(const Material& mat,
            const Vector3& pos, const Vector3& norm) const = 0;
    virtual Vector3 illuminateWithSeparateSpecular(const Material& mat,
            const Vector3& pos, const Vector3& norm, Vector3& specular) const = 0;

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

    virtual Vector3 getLightDirection(const Vector3& point) const;
    virtual void notifyCameraSpaceChanged(const Matrix4x4& viewMat);
    virtual Vector3 illuminatePro(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSpecularPro(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSeparateSpecularPro(const Material& mat,
            const Vector3& pos, const Vector3& norm, Vector3& specular) const;

    virtual Vector3 illuminate(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSpecular(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSeparateSpecular(const Material& mat,
            const Vector3& pos, const Vector3& norm, Vector3& specular) const;

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    Vector3 m_pos;
    Vector3 m_posInCS;
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

    virtual Vector3 getLightDirection(const Vector3& point) const;
    virtual void notifyCameraSpaceChanged(const Matrix4x4& viewMat);
    virtual Vector3 illuminatePro(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSpecularPro(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSeparateSpecularPro(const Material& mat,
            const Vector3& pos, const Vector3& norm, Vector3& specular) const;

    virtual Vector3 illuminate(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSpecular(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSeparateSpecular(const Material& mat,
            const Vector3& pos, const Vector3& norm, Vector3& specular) const;

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    Vector3 m_dir;
    Vector3 m_invDirInCS;
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

    virtual Vector3 getLightDirection(const Vector3& point) const;
    virtual void notifyCameraSpaceChanged(const Matrix4x4& viewMat);
    virtual Vector3 illuminatePro(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSpecularPro(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSeparateSpecularPro(const Material& mat,
            const Vector3& pos, const Vector3& norm, Vector3& specular) const;

    virtual Vector3 illuminate(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSpecular(const Material& mat,
            const Vector3& pos, const Vector3& norm) const;
    virtual Vector3 illuminateWithSeparateSpecular(const Material& mat,
            const Vector3& pos, const Vector3& norm, Vector3& specular) const;

    virtual void printStream(std::ostream& so) const;
    virtual void scanStream(std::istream& si);
private:
    float m_inner, m_outter;
    float m_falloff;
    Vector3 m_pos, m_dir;
    Vector3 m_posInCS, m_invDirInCS;
    float m_cosInner, m_cosOutter;
    float m_invCosInnerSubOutter;
};


#endif // #ifndef LIGHT_H
