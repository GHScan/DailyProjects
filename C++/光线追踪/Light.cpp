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
Vector3 PointLight::getLightDirection(const Vector3& point) const
{
    return (m_posInCS - point).normalize();
}
void PointLight::notifyCameraSpaceChanged(const Matrix4x4& viewMat)
{
    m_posInCS = m_pos;
    transformPoint(m_posInCS, getSceneNode()->getWorldMatrix() * viewMat);
}
Vector3 PointLight::illuminatePro(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    Vector3 r(mat.ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = m_posInCS - pos;
    float dot = pt2Light.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        dot /= pt2Light.length();
        r += Vector3(mat.diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
    }
    // 衰减
    // ....
    return r;
}
Vector3 PointLight::illuminateWithSpecularPro(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    Vector3 r(mat.ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = m_posInCS - pos;
    float dot = pt2Light.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        {
            float invLen = 1 / pt2Light.length();
            dot *= invLen;
            pt2Light *= invLen;
        }
        r += Vector3(mat.diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (- pos).normalize();
        Vector3 I = pt2Camera + pt2Light;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, mat.power);
            if (_pow > EPSILON)
            r += Vector3(mat.specularClr).multiplyInplace(m_specular) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
}
Vector3 PointLight::illuminateWithSeparateSpecularPro(const Material& mat,
        const Vector3& pos, const Vector3& norm, Vector3& specular) const
{
    Vector3 r(mat.ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = m_posInCS - pos;
    float dot = pt2Light.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        {
            float invLen = 1 / pt2Light.length();
            dot *= invLen;
            pt2Light *= invLen;
        }
        r += Vector3(mat.diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (- pos).normalize();
        Vector3 I = pt2Camera + pt2Light;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, mat.power);
            if (_pow > EPSILON)
            specular = Vector3(mat.specularClr).multiplyInplace(m_specular) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
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
Vector3 DirectionLight::getLightDirection(const Vector3& point) const
{
    return m_invDirInCS;
}
void DirectionLight::notifyCameraSpaceChanged(const Matrix4x4& viewMat)
{
    assert(m_dir.isUnit());
    m_invDirInCS = -m_dir;
    transformDirection(m_invDirInCS, getSceneNode()->getWorldMatrix() * viewMat);
}
Vector3 DirectionLight::illuminatePro(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    Vector3 r(mat.ambientClr);
    r.multiplyInplace(m_ambient);

    float dot = m_invDirInCS.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        r += Vector3(mat.diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
    }
    // 衰减
    // ....
    return r;
}
Vector3 DirectionLight::illuminateWithSpecularPro(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    Vector3 r(mat.ambientClr);
    r.multiplyInplace(m_ambient);

    float dot = m_invDirInCS.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        r += Vector3(mat.diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (- pos).normalize();
        Vector3 I = pt2Camera + m_invDirInCS;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, mat.power);
            if (_pow > EPSILON)
            r += Vector3(mat.specularClr).multiplyInplace(m_specular) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
}
Vector3 DirectionLight::illuminateWithSeparateSpecularPro(const Material& mat,
        const Vector3& pos, const Vector3& norm, Vector3& specular) const
{
    Vector3 r(mat.ambientClr);
    r.multiplyInplace(m_ambient);

    float dot = m_invDirInCS.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        r += Vector3(mat.diffuseClr).multiplyInplace(m_diffuse) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (- pos).normalize();
        Vector3 I = pt2Camera + m_invDirInCS;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, mat.power);
            if (_pow > EPSILON)
            specular = Vector3(mat.specularClr).multiplyInplace(m_specular) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
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
Vector3 SpotLight::getLightDirection(const Vector3& point) const
{
    return (m_posInCS - point).normalize();
}
void SpotLight::notifyCameraSpaceChanged(const Matrix4x4& viewMat)
{
    m_cosInner = cos(degree2Radian(m_inner));
    m_cosOutter = cos(degree2Radian(m_outter));
    assert(!fequal(m_cosInner - m_cosOutter, 0));
    m_invCosInnerSubOutter = 1 / (m_cosInner - m_cosOutter);

    Matrix4x4 worldView(getSceneNode()->getWorldMatrix() * viewMat);

    m_posInCS = m_pos;
    transformPoint(m_posInCS, worldView);

    m_invDirInCS = -m_dir;
    transformDirection(m_invDirInCS, viewMat);
}
Vector3 SpotLight::illuminatePro(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    assert(0); // 还未优化

    Vector3 r(mat.ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = (m_posInCS - pos).normalize();
    float spotDot = pt2Light.dotProduct(m_invDirInCS);
    if (spotDot >= m_cosOutter) {
        float spotRatio = spotDot > m_cosInner ? 1 :
            (m_cosInner - spotDot) * m_invCosInnerSubOutter;
        float dot = pt2Light.dotProduct(norm);
        if (dot > EPSILON) {
            // 漫反射
            r += Vector3(mat.diffuseClr).multiplyInplace(m_diffuse) *= (dot * spotRatio);
            // 镜面反射
        }
        // 衰减
        // ....
    }
    return r;
}
Vector3 SpotLight::illuminateWithSpecularPro(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    assert(0); // 还未优化

    Vector3 r(mat.ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = (m_posInCS - pos).normalize();
    float spotDot = pt2Light.dotProduct(m_invDirInCS);
    if (spotDot >= m_cosOutter) {
        float spotRatio = spotDot > m_cosInner ? 1 :
            (m_cosInner - spotDot) * m_invCosInnerSubOutter;
        float dot = pt2Light.dotProduct(norm);
        if (dot > EPSILON) {
            // 漫反射
            Vector3 diffAndSpec(mat.diffuseClr);
            diffAndSpec.multiplyInplace(m_diffuse) *= dot;
            // 镜面反射
            Vector3 pt2Camera = (- pos).normalize();
            float specRatio = (pt2Camera + pt2Light).normalize().dotProduct(norm);
            if (specRatio > EPSILON) {
                float _pow = pow(specRatio, mat.power);
                if (_pow > EPSILON)
                diffAndSpec += Vector3(mat.specularClr).multiplyInplace(m_specular) *= _pow;
            }

            r += diffAndSpec * spotRatio;
        }
        // 衰减
        // ....
    }
    return r;
}
Vector3 SpotLight::illuminateWithSeparateSpecularPro(const Material& mat,
        const Vector3& pos, const Vector3& norm, Vector3& specular) const
{
    assert(0); // 还未优化

    Vector3 r(mat.ambientClr);
    r.multiplyInplace(m_ambient);

    Vector3 pt2Light = (m_posInCS - pos).normalize();
    float spotDot = pt2Light.dotProduct(m_invDirInCS);
    if (spotDot >= m_cosOutter) {
        float spotRatio = spotDot > m_cosInner ? 1 :
            (m_cosInner - spotDot) * m_invCosInnerSubOutter;
        float dot = pt2Light.dotProduct(norm);
        if (dot > EPSILON) {
            // 漫反射
            r += Vector3(mat.diffuseClr).multiplyInplace(m_diffuse) *= (dot * spotRatio);
            // 镜面反射
            Vector3 pt2Camera = (- pos).normalize();
            float specRatio = (pt2Camera + pt2Light).normalize().dotProduct(norm);
            if (specRatio > EPSILON) {
                float _pow = pow(specRatio, mat.power);
                if (_pow > EPSILON)
                specular = Vector3(mat.specularClr).multiplyInplace(m_specular) *= 
                    (_pow * spotRatio);
            }
        }
        // 衰减
        // ....
    }
    return r;
}
//----------------------------------------
// 序列化
//----------------------------------------
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
//----------------------------------------
// 简化光照模型
//----------------------------------------
Vector3 PointLight::illuminate(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    Vector3 r(mat.ambientClr);

    Vector3 pt2Light = m_posInCS - pos;
    float dot = pt2Light.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        dot /= pt2Light.length();
        r += Vector3(mat.diffuseClr) *= dot;
        // 镜面反射
    }
    // 衰减
    // ....
    return r;
}
Vector3 PointLight::illuminateWithSpecular(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    Vector3 r(mat.ambientClr);

    Vector3 pt2Light = m_posInCS - pos;
    float dot = pt2Light.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        {
            float invLen = 1 / pt2Light.length();
            dot *= invLen;
            pt2Light *= invLen;
        }
        r += Vector3(mat.diffuseClr) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (- pos).normalize();
        Vector3 I = pt2Camera + pt2Light;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, mat.power);
            if (_pow > EPSILON)
                r += Vector3(mat.specularClr) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
}
Vector3 PointLight::illuminateWithSeparateSpecular(const Material& mat,
        const Vector3& pos, const Vector3& norm, Vector3& specular) const
{
    Vector3 r(mat.ambientClr);

    Vector3 pt2Light = m_posInCS - pos;
    float dot = pt2Light.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        {
            float invLen = 1 / pt2Light.length();
            dot *= invLen;
            pt2Light *= invLen;
        }
        r += Vector3(mat.diffuseClr) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (- pos).normalize();
        Vector3 I = pt2Camera + pt2Light;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, mat.power);
            if (_pow > EPSILON)
                specular = Vector3(mat.specularClr) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
}

Vector3 DirectionLight::illuminate(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    Vector3 r(mat.ambientClr);

    float dot = m_invDirInCS.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        r += Vector3(mat.diffuseClr) *= dot;
        // 镜面反射
    }
    // 衰减
    // ....
    return r;
}
Vector3 DirectionLight::illuminateWithSpecular(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    Vector3 r(mat.ambientClr);

    float dot = m_invDirInCS.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        r += Vector3(mat.diffuseClr) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (- pos).normalize();
        Vector3 I = pt2Camera + m_invDirInCS;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, mat.power);
            if (_pow > EPSILON)
                r += Vector3(mat.specularClr) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
}
Vector3 DirectionLight::illuminateWithSeparateSpecular(const Material& mat,
        const Vector3& pos, const Vector3& norm, Vector3& specular) const
{
    Vector3 r(mat.ambientClr);

    float dot = m_invDirInCS.dotProduct(norm);
    if (dot > EPSILON) {
        // 漫反射
        r += Vector3(mat.diffuseClr) *= dot;
        // 镜面反射
        Vector3 pt2Camera = (- pos).normalize();
        Vector3 I = pt2Camera + m_invDirInCS;
        float specRatio = I.dotProduct(norm);
        if (specRatio > EPSILON) {
            specRatio /= I.length();
            float _pow = pow(specRatio, mat.power);
            if (_pow > EPSILON)
                specular = Vector3(mat.specularClr) *= _pow;
        }
    }
    // 衰减
    // ....
    return r;
}
Vector3 SpotLight::illuminate(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    assert(0); // 还未优化

    Vector3 r(mat.ambientClr);

    Vector3 pt2Light = (m_posInCS - pos).normalize();
    float spotDot = pt2Light.dotProduct(m_invDirInCS);
    if (spotDot >= m_cosOutter) {
        float spotRatio = spotDot > m_cosInner ? 1 :
            (m_cosInner - spotDot) * m_invCosInnerSubOutter;
        float dot = pt2Light.dotProduct(norm);
        if (dot > EPSILON) {
            // 漫反射
            r += Vector3(mat.diffuseClr) *= (dot * spotRatio);
            // 镜面反射
        }
        // 衰减
        // ....
    }
    return r;
}
Vector3 SpotLight::illuminateWithSpecular(const Material& mat,
        const Vector3& pos, const Vector3& norm) const
{
    assert(0); // 还未优化

    Vector3 r(mat.ambientClr);

    Vector3 pt2Light = (m_posInCS - pos).normalize();
    float spotDot = pt2Light.dotProduct(m_invDirInCS);
    if (spotDot >= m_cosOutter) {
        float spotRatio = spotDot > m_cosInner ? 1 :
            (m_cosInner - spotDot) * m_invCosInnerSubOutter;
        float dot = pt2Light.dotProduct(norm);
        if (dot > EPSILON) {
            // 漫反射
            Vector3 diffAndSpec(mat.diffuseClr);
            diffAndSpec *= dot;
            // 镜面反射
            Vector3 pt2Camera = (- pos).normalize();
            float specRatio = (pt2Camera + pt2Light).normalize().dotProduct(norm);
            if (specRatio > EPSILON) {
                float _pow = pow(specRatio, mat.power);
                if (_pow > EPSILON)
                    diffAndSpec += Vector3(mat.specularClr) *= _pow;
            }
            r += diffAndSpec * spotRatio;
        }
        // 衰减
        // ....
    }
    return r;
}
Vector3 SpotLight::illuminateWithSeparateSpecular(const Material& mat,
        const Vector3& pos, const Vector3& norm, Vector3& specular) const
{
    assert(0); // 还未优化

    Vector3 r(mat.ambientClr);

    Vector3 pt2Light = (m_posInCS - pos).normalize();
    float spotDot = pt2Light.dotProduct(m_invDirInCS);
    if (spotDot >= m_cosOutter) {
        float spotRatio = spotDot > m_cosInner ? 1 :
            (m_cosInner - spotDot) * m_invCosInnerSubOutter;
        float dot = pt2Light.dotProduct(norm);
        if (dot > EPSILON) {
            // 漫反射
            r += Vector3(mat.diffuseClr) *= (dot * spotRatio);
            // 镜面反射
            Vector3 pt2Camera = (- pos).normalize();
            float specRatio = (pt2Camera + pt2Light).normalize().dotProduct(norm);
            if (specRatio > EPSILON) {
                float _pow = pow(specRatio, mat.power);
                if (_pow > EPSILON)
                    specular = Vector3(mat.specularClr) *= (_pow * spotRatio);
            }
        }
        // 衰减
        // ....
    }
    return r;
}
