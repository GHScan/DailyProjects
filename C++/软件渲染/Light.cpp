// vim: fileencoding=gbk
#include "pch.h"

#include <cmath>

#include "Light.h"
#include "Serialize.h"
#include "Matrix.h"
#include "Mesh.h"

//----------------------------------------
// Light
//----------------------------------------
Light::Light(
        E_SceneObjType t, 
        const Vector3& ambient, 
        const Vector3& diffuse, 
        const Vector3& specular,
        const Vector3& attenuation,
        float range):
    m_ambient(ambient), m_diffuse(diffuse), m_specular(specular), 
    m_attenuation(attenuation), m_range(range),
    SceneObj(t)
{}

PointLight::PointLight(
        const Vector3& pos,
        const Vector3& ambient, 
        const Vector3& diffuse, 
        const Vector3& specular,
        const Vector3& attenuation,
        float range):
    m_pos(pos), Light(ESOT_pointLight, ambient, diffuse, specular, attenuation, range)
{
}
void PointLight::beginLighting(
        const Matrix4x4& modelSpaceMat,
        const Material& material,
        const Vector3& cameraPosInMS) const
{
    Vector4 v4(m_pos);
    transform(v4, modelSpaceMat);
    assert(fequal(v4.w, 1));
    m_modelSpacePos = v4.divW();
    m_mat = &material;
    m_cameraPosInMS = cameraPosInMS;
}
Vector3 PointLight::illuminate(
        const Vector3& pos, const Vector3& norm) const
{
    assert(m_mat != NULL);
    Vector3 r(m_mat->ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = m_modelSpacePos - pos;
    float dot = pt2Light.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        dot /= pt2Light.length();
        r += Vector3(m_mat->diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
    }
    // 衰减
    // ....
    return r;
}
Vector3 PointLight::illuminateWithSpecular(
        const Vector3& pos, const Vector3& norm) const
{
    assert(m_mat != NULL);
    Vector3 r(m_mat->ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = m_modelSpacePos - pos;
    float dot = pt2Light.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        {
            float invLen = 1 / pt2Light.length();
            dot *= invLen;
            pt2Light *= invLen;
        }
        r += Vector3(m_mat->diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (m_cameraPosInMS - pos).normalize();
        Vector3 I = pt2Camera + pt2Light;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, m_mat->power);
            if (_pow > EPSILON)
            r += Vector3(m_mat->specularClr).multiplyInplace(m_specular) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
}
Vector3 PointLight::illuminateWithSeparateSpecular(
        const Vector3& pos, const Vector3& norm, Vector3& specular) const
{
    assert(m_mat != NULL);
    Vector3 r(m_mat->ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = m_modelSpacePos - pos;
    float dot = pt2Light.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        {
            float invLen = 1 / pt2Light.length();
            dot *= invLen;
            pt2Light *= invLen;
        }
        r += Vector3(m_mat->diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (m_cameraPosInMS - pos).normalize();
        Vector3 I = pt2Camera + pt2Light;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, m_mat->power);
            if (_pow > EPSILON)
            specular = Vector3(m_mat->specularClr).multiplyInplace(m_specular) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
}
void PointLight::endLighting() const
{
    m_modelSpacePos = Vector3::ZERO;
    m_mat = NULL;
}

DirectionLight::DirectionLight(
        const Vector3& dir,
        const Vector3& ambient, 
        const Vector3& diffuse, 
        const Vector3& specular,
        const Vector3& attenuation,
        float range):
    m_dir(dir), Light(ESOT_directionLight, ambient, diffuse, specular, attenuation, range)
{
    assert(dir.isUnit());
}
void DirectionLight::beginLighting(
        const Matrix4x4& modelSpaceMat,
        const Material& materail,
        const Vector3& cameraPosInMS) const
{
    assert(m_dir.isUnit());
    Vector4 v4(m_dir, 0);
    transform(v4, modelSpaceMat);
    assert(fequal(v4.w, 0));
    m_modelSpaceInvDir = -(Vector3&)v4;
    m_mat = &materail;
    m_cameraPosInMS = cameraPosInMS;
}
Vector3 DirectionLight::illuminate(
        const Vector3& pos, const Vector3& norm) const
{
    assert(m_mat != NULL);
    Vector3 r(m_mat->ambientClr);
    r.multiplyInplace(m_ambient);

    float dot = m_modelSpaceInvDir.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        r += Vector3(m_mat->diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
    }
    // 衰减
    // ....
    return r;
}
Vector3 DirectionLight::illuminateWithSpecular(
        const Vector3& pos, const Vector3& norm) const
{
    assert(m_mat != NULL);
    Vector3 r(m_mat->ambientClr);
    r.multiplyInplace(m_ambient);

    float dot = m_modelSpaceInvDir.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        r += Vector3(m_mat->diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (m_cameraPosInMS - pos).normalize();
        Vector3 I = pt2Camera + m_modelSpaceInvDir;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, m_mat->power);
            if (_pow > EPSILON)
            r += Vector3(m_mat->specularClr).multiplyInplace(m_specular) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
}
Vector3 DirectionLight::illuminateWithSeparateSpecular(
        const Vector3& pos, const Vector3& norm, Vector3& specular) const
{
    assert(m_mat != NULL);
    Vector3 r(m_mat->ambientClr);
    r.multiplyInplace(m_ambient);

    float dot = m_modelSpaceInvDir.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        r += Vector3(m_mat->diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (m_cameraPosInMS - pos).normalize();
        Vector3 I = pt2Camera + m_modelSpaceInvDir;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, m_mat->power);
            if (_pow > EPSILON)
            specular = Vector3(m_mat->specularClr).multiplyInplace(m_specular) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
}
void DirectionLight::endLighting() const
{
    m_modelSpaceInvDir = Vector3::ZERO;
    m_mat = NULL;
}

SpotLight::SpotLight(
        Vector3 pos, Vector3 dir,
        float inner, float outter, float falloff,
        const Vector3& ambient, 
        const Vector3& diffuse, 
        const Vector3& specular,
        const Vector3& attenuation,
        float range):
    m_inner(inner), m_outter(outter), m_falloff(falloff), 
    m_pos(pos), m_dir(dir),
    Light(ESOT_spotLight, ambient, diffuse, specular, attenuation, range)
{
    assert(dir.isUnit());
}
void SpotLight::beginLighting(
        const Matrix4x4& modelSpaceMat, 
        const Material& material,
        const Vector3& cameraPosInMS) const
{
    m_cosInner = cos(degree2Radian(m_inner));
    m_cosOutter = cos(degree2Radian(m_outter));
    assert(!fequal(m_cosInner - m_cosOutter, 0));
    m_invCosInnerSubOutter = 1 / (m_cosInner - m_cosOutter);

    Vector4 v4(m_pos);
    transform(v4, modelSpaceMat);
    assert(fequal(v4.w, 1));
    m_modelSpacePos = v4.divW();

    assert(m_dir.isUnit());
    v4 = Vector4(m_dir, 0);
    transform(v4, modelSpaceMat);
    assert(fequal(v4.w, 0));
    m_modelSpaceInvDir = -(Vector3&)v4;

    m_mat = &material;
    m_cameraPosInMS = cameraPosInMS;
}
Vector3 SpotLight::illuminate(
        const Vector3& pos, const Vector3& norm) const
{
    assert(0); // 还未优化

    assert(m_mat != NULL);
    Vector3 r(m_mat->ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = (m_modelSpacePos - pos).normalize();
    float spotDot = pt2Light.dotProduct(m_modelSpaceInvDir);
    if (spotDot >= m_cosOutter) {
        float spotRatio = spotDot > m_cosInner ? 1 :
            (m_cosInner - spotDot) * m_invCosInnerSubOutter;
        float dot = pt2Light.dotProduct(norm);
        if (dot > EPSILON) {
            // 漫反射
            r += Vector3(m_mat->diffuseClr).multiplyInplace(m_diffuse) *= (dot * spotRatio);
            // 镜面反射
        }
        // 衰减
        // ....
    }
    return r;
}
Vector3 SpotLight::illuminateWithSpecular(
        const Vector3& pos, const Vector3& norm) const
{
    assert(0); // 还未优化

    assert(m_mat != NULL);
    Vector3 r(m_mat->ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = (m_modelSpacePos - pos).normalize();
    float spotDot = pt2Light.dotProduct(m_modelSpaceInvDir);
    if (spotDot >= m_cosOutter) {
        float spotRatio = spotDot > m_cosInner ? 1 :
            (m_cosInner - spotDot) * m_invCosInnerSubOutter;
        float dot = pt2Light.dotProduct(norm);
        if (dot > EPSILON) {
            // 漫反射
            Vector3 diffAndSpec(m_mat->diffuseClr);
            diffAndSpec.multiplyInplace(m_diffuse) *= dot;
            // 镜面反射
            Vector3 pt2Camera = (m_cameraPosInMS - pos).normalize();
            float specRatio = (pt2Camera + pt2Light).normalize().dotProduct(norm);
            if (specRatio > EPSILON) {
                float _pow = pow(specRatio, m_mat->power);
                if (_pow > EPSILON)
                diffAndSpec += Vector3(m_mat->specularClr).multiplyInplace(m_specular) *= _pow;
            }

            r += diffAndSpec * spotRatio;
        }
        // 衰减
        // ....
    }
    return r;
}
Vector3 SpotLight::illuminateWithSeparateSpecular(
        const Vector3& pos, const Vector3& norm, Vector3& specular) const
{
    assert(0); // 还未优化

    assert(m_mat != NULL);
    Vector3 r(m_mat->ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = (m_modelSpacePos - pos).normalize();
    float spotDot = pt2Light.dotProduct(m_modelSpaceInvDir);
    if (spotDot >= m_cosOutter) {
        float spotRatio = spotDot > m_cosInner ? 1 :
            (m_cosInner - spotDot) * m_invCosInnerSubOutter;
        float dot = pt2Light.dotProduct(norm);
        if (dot > EPSILON) {
            // 漫反射
            r += Vector3(m_mat->diffuseClr).multiplyInplace(m_diffuse) *= (dot * spotRatio);
            // 镜面反射
            Vector3 pt2Camera = (m_cameraPosInMS - pos).normalize();
            float specRatio = (pt2Camera + pt2Light).normalize().dotProduct(norm);
            if (specRatio > EPSILON) {
                float _pow = pow(specRatio, m_mat->power);
                if (_pow > EPSILON)
                specular = Vector3(m_mat->specularClr).multiplyInplace(m_specular) *= 
                    (_pow * spotRatio);
            }
        }
        // 衰减
        // ....
    }
    return r;
}
void SpotLight::endLighting() const
{
    m_modelSpacePos = Vector3::ZERO;
    m_modelSpaceInvDir = Vector3::ZERO;
    m_mat = NULL;
}

void Light::printStream(std::ostream& so) const
{
    StreamBlockWriter w(sceneObjType2Str(ESOT_light).c_str(), so);
    w.write("ambient", m_ambient.data(), m_ambient.data() + 3);
    w.write("diffuse", m_diffuse.data(), m_diffuse.data() + 3);
    w.write("specular", m_specular.data(), m_specular.data() + 3);
    w.write("attenuation", m_attenuation.data(), m_attenuation.data() + 3);
    w.write("range", m_range);
}
void Light::scanStream(std::istream& si)
{
    StreamBlockReader r(sceneObjType2Str(ESOT_light).c_str(), si);
    if (!r.read("ambient", m_ambient.data(), m_ambient.data() + 3)) assert(0);
    if (!r.read("diffuse", m_diffuse.data(), m_diffuse.data() + 3)) assert(0);
    if (!r.read("specular", m_specular.data(), m_specular.data() + 3)) assert(0);
    if (!r.read("attenuation", m_attenuation.data(), m_attenuation.data() + 3)) assert(0);
    if (!r.read("range", &m_range)) assert(0);
}
void PointLight::printStream(std::ostream& so) const
{
    StreamBlockWriter w(sceneObjType2Str(ESOT_pointLight).c_str(), so);
    w.write("position", m_pos.data(), m_pos.data() + 3);
    Light::printStream(so);
}
void PointLight::scanStream(std::istream& si)
{
    StreamBlockReader r(sceneObjType2Str(ESOT_pointLight).c_str(), si);
    if (!r.read("position", m_pos.data(), m_pos.data() + 3)) assert(0);
    Light::scanStream(si);
}
void DirectionLight::printStream(std::ostream& so) const
{
    StreamBlockWriter w(sceneObjType2Str(ESOT_directionLight).c_str(), so);
    w.write("direction", m_dir.data(), m_dir.data() + 3);
    Light::printStream(so);
}
void DirectionLight::scanStream(std::istream& si)
{
    StreamBlockReader r(sceneObjType2Str(ESOT_directionLight).c_str(), si);
    if (!r.read("direction", m_dir.data(), m_dir.data() + 3)) assert(0);
    m_dir = m_dir.normalize();
    Light::scanStream(si);
}
void SpotLight::printStream(std::ostream& so) const
{
    StreamBlockWriter w(sceneObjType2Str(ESOT_spotLight).c_str(), so);
    w.write("position", m_pos.data(), m_pos.data() + 3);
    w.write("direction", m_dir.data(), m_dir.data() + 3);
    w.write("inner", m_inner);
    w.write("outter", m_outter);
    w.write("falloff", m_falloff);
    Light::printStream(so);
}
void SpotLight::scanStream(std::istream& si)
{
    StreamBlockReader r(sceneObjType2Str(ESOT_spotLight).c_str(), si);
    if (!r.read("position", m_pos.data(), m_pos.data() + 3)) assert(0);
    if (!r.read("direction", m_dir.data(), m_dir.data() + 3)) assert(0);
    if (!r.read("inner", &m_inner)) assert(0);
    if (!r.read("outter", &m_outter)) assert(0);
    if (!r.read("falloff", &m_falloff)) assert(0);
    m_dir = m_dir.normalize();
    Light::scanStream(si);
}
